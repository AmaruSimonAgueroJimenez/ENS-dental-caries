"""Verify the original Andrea Correa DTA against the supplemental ENS SAV.

Original questionnaire values are compared by participant ID after documented
code harmonization. This module does not change the supplied analytical frame.
Only the returned legacy-frequency table contains identifiers, for an in-memory
sensitivity analysis; the three other outputs contain aggregate information.
"""

from __future__ import annotations

import hashlib
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
import pyreadstat

from .data import (
    DIET_FEATURES, NUMERIC_FEATURES, RAW_COLUMNS, SOCIO_FEATURES,
    derive_caries_data,
)


DIRECT_COLUMNS = {
    "IdEncuesta": "IdEncuesta", "Edad": "Edad", "region": "Region",
    "zona": "Zona", "sexo": "Sexo", "c1": "c1", "Estrato": "Estrato",
    **{name: name for name in [
        "m5p3", "m5p5", "m5p6", "m5p8", "die1a", "die2", "die3", "die4",
        "die5", "die6", "die7", "die8", "die9", "die10_a", "die10_b",
        "die10_c", "die10_d", "die10_e", "die10_f", "die11", "die14",
        "die12_cantidad", "die12_unidad", "die13_cantidad", "die13_unidad",
    ]},
}
DTA_COLUMNS = list(DIRECT_COLUMNS) + [
    "c6", "nedu", "indigena", "remanentes", "cariados", "cariesnt",
    "frecsembebida", "frecsemjugo",
]


def _numeric(series: pd.Series, name: str) -> pd.Series:
    """Normalize documented numeric fields without accepting arbitrary text."""
    result = pd.to_numeric(series, errors="coerce").astype(float)
    bad = series.notna() & result.isna()
    if bad.any():
        raise ValueError(f"Unexpected nonnumeric values in original source field {name}")
    if np.isinf(result).any():
        raise ValueError(f"Nonfinite numeric value in original source field {name}")
    # All negative questionnaire codes denote special missing responses.
    return result.mask(result.lt(0))


def _equal(left, right, numeric=True) -> np.ndarray:
    if numeric:
        return np.isclose(np.asarray(left, float), np.asarray(right, float),
                          atol=1e-5, rtol=1e-6, equal_nan=True)
    a, b = pd.Series(np.asarray(left, object)), pd.Series(np.asarray(right, object))
    return (a.eq(b) | (a.isna() & b.isna())).to_numpy(bool)


def _assert_equal(left, right, name: str, numeric=True):
    matching = _equal(left, right, numeric)
    if not matching.all():
        raise ValueError(f"Unexpected source discrepancy for {name}: {int((~matching).sum())} participants")


def _ids(series: pd.Series) -> pd.Series:
    if series.isna().any() or series.mod(1).ne(0).any() or series.duplicated().any():
        raise ValueError("Original source participant IDs must be unique observed integers")
    return series.map(lambda value: str(int(value)))


def _categories(series: pd.Series, allowed, name: str):
    if not series.dropna().isin(allowed).all():
        raise ValueError(f"Unexpected category code in original source field {name}")


def _weekly(quantity: pd.Series, unit: pd.Series) -> pd.Series:
    _categories(unit, [1, 2, 3, 4], "beverage frequency unit")
    # These are the original DTA's observed rules, not a calendar approximation.
    return (quantity * unit.map({1: 7.0, 2: 1.0, 3: .25})).mask(unit.eq(4), 0.)


def _compare_sources(
    sav_raw: pd.DataFrame, dta_raw: pd.DataFrame, frame: pd.DataFrame,
) -> tuple[dict[str, Any], pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """Core comparison, separately testable with small synthetic questionnaires."""
    for source, required, name in [(sav_raw, RAW_COLUMNS, "SAV"), (dta_raw, DTA_COLUMNS, "DTA")]:
        missing = sorted(set(required) - set(source))
        if missing:
            raise ValueError(f"{name} source is missing required numeric columns: {missing}")
    s = sav_raw[RAW_COLUMNS].apply(lambda col: _numeric(col, col.name))
    d = dta_raw[DTA_COLUMNS].apply(lambda col: _numeric(col, col.name))
    s.index, d.index = _ids(s.IdEncuesta), _ids(d.IdEncuesta)
    s.index.name = d.index.name = "participant_id"
    if frame.participant_id.isna().any() or frame.participant_id.duplicated().any():
        raise ValueError("Analytical frame participant IDs must be unique and observed")
    current = frame.set_index("participant_id", drop=False)
    examined = s.m5p3.notna() & s.m5p6.notna() & s.m5p5.notna() & s.m5p8.notna()
    if set(d.index) != set(s.index[examined]):
        raise ValueError("Original DTA IDs do not exactly match the complete SAV examination cohort")
    if not set(d.index).issubset(current.index):
        raise ValueError("Original DTA IDs are missing from the supplied analytical frame")
    paired, canonical = s.loc[d.index].copy(), current.loc[d.index]
    raw_checks = []
    for dta_name, sav_name in DIRECT_COLUMNS.items():
        _assert_equal(d[dta_name], paired[sav_name], f"{dta_name} vs {sav_name}")
        raw_checks.append({"dta_variable": dta_name, "sav_variable": sav_name,
                           "n_compared": len(d), "n_discrepant": 0})
    _categories(d.nedu, [0, 1, 2], "nedu")
    _categories(d.indigena, [0, 1], "indigena")
    _categories(d.c6, range(10), "c6")
    _categories(d.cariesnt, [0, 1], "cariesnt")
    # In the original DTA c6=0 represents the SAV's c6=10 nonindigenous response.
    recoded_c6 = d.c6.mask(d.c6.eq(0), 10.)
    _assert_equal(recoded_c6, paired.c6, "indigenous questionnaire categories")
    _assert_equal(d.indigena, paired.c6.isin(range(1, 10)).astype(float), "indigenous indicator")
    _assert_equal(3 - d.nedu, paired.NEDU1_MINSAL_1, "decoded education")
    _assert_equal(d.remanentes, paired.m5p3 + paired.m5p6, "remaining teeth")
    _assert_equal(d.cariados, paired.m5p5 + paired.m5p8, "decayed teeth")
    _assert_equal(d.cariesnt, (paired.m5p5 + paired.m5p8).gt(0).astype(float), "caries outcome")
    _assert_equal(d.cariesnt, canonical.caries, "analytical-frame outcome")
    _assert_equal(d.remanentes, canonical.teeth_remaining, "analytical-frame remaining teeth")

    # Construct a harmonized physical-value frame from DTA questionnaire fields,
    # supplementing only fields absent from DTA with matched SAV information.
    harmonized = paired.copy()
    for dta_name, sav_name in DIRECT_COLUMNS.items():
        harmonized[sav_name] = d[dta_name]
    harmonized["c6"] = recoded_c6
    harmonized["NEDU1_MINSAL_1"] = 3 - d.nedu
    dta_frame, _, _ = derive_caries_data(harmonized.reset_index(drop=True))
    dta_frame = dta_frame.set_index("participant_id", drop=False).loc[d.index]
    legacy = pd.DataFrame({"participant_id": d.index}, index=d.index)
    unit_differences = {}
    for feature, source_name, number in [
        ("soda_frequency", "frecsembebida", 12),
        ("juice_frequency", "frecsemjugo", 13),
    ]:
        quantity, unit = d[f"die{number}_cantidad"], d[f"die{number}_unidad"]
        _assert_equal(d[source_name], _weekly(quantity, unit), f"original {source_name} conversion rule")
        legacy[feature] = d[source_name] / 7.
        differs = ~_equal(legacy[feature], canonical[feature])
        if (differs & ~unit.eq(3)).any():
            raise ValueError(f"Undeclared beverage conversion difference outside monthly reports: {feature}")
        unit_differences[feature] = {
            "n_different_examined": int(differs.sum()),
            "n_different_dentate": int((differs & d.remanentes.gt(0)).sum()),
            "maximum_absolute_difference_times_per_day": float((legacy[feature] - canonical[feature]).abs().max()),
        }

    comparison_rows = []
    for feature in SOCIO_FEATURES + DIET_FEATURES:
        _assert_equal(dta_frame[feature], canonical[feature], f"harmonized predictor {feature}",
                      numeric=feature in NUMERIC_FEATURES)
        comparison_rows.append({
            "feature": feature, "n_compared": len(d), "n_equal_primary": len(d),
            "n_discrepant_primary": 0,
            "n_different_legacy_unit_examined": unit_differences.get(feature, {}).get("n_different_examined", 0),
            "n_different_legacy_unit_dentate": unit_differences.get(feature, {}).get("n_different_dentate", 0),
            "primary_rule": "Recalculate from original quantity/unit using 30 days per month" if feature in unit_differences else
                            "Original reported glasses/day, not R factor indices" if feature == "water" else
                            "Decode education: DTA0 high,1 intermediate,2 low" if feature == "education" else
                            "Decode indigenous indicator" if feature == "indigenous" else "Same physical questionnaire values; structural skips harmonized",
            "legacy_rule": "DTA weekly frequency /7; monthly frequency uses four weeks (28 days)" if feature in unit_differences else
                           "as.numeric(droplevels(die11)) after labelled-to-factor conversion" if feature == "water" else "Equivalent after documented category decoding",
        })

    # Reproduce the actual original QMD preprocessing and complete-case domain.
    original_predictors = dta_frame[SOCIO_FEATURES + DIET_FEATURES].copy()
    original_predictors["dairy_type"] = d.die3.fillna(0).to_numpy()
    original_predictors["fruit_portions"] = d.die7.fillna(0).to_numpy()
    original_predictors["vegetable_portions"] = d.die9.fillna(0).to_numpy()
    original_predictors["soda_frequency"] = d.frecsembebida.to_numpy()
    original_predictors["juice_frequency"] = d.frecsemjugo.to_numpy()
    original_complete = d.remanentes.gt(0) & d.cariesnt.notna() & original_predictors.notna().all(axis=1)
    if set(d.index[original_complete]) != set(current.index[current.paper_complete]):
        raise ValueError("Original R complete-case cohort IDs differ from analytical paper_complete domain")
    if set(d.index[d.remanentes.gt(0)]) != set(current.index[current.eligible]):
        raise ValueError("Original dentate cohort IDs differ from analytical eligible domain")
    observed_water = sorted(d.loc[d.remanentes.gt(0), "die11"].dropna().unique())
    water_coding = pd.DataFrame({
        "reported_glasses_per_day": observed_water,
        "original_r_factor_index": np.arange(1, len(observed_water) + 1),
        "n_dentate_before_complete_case_deletion": [int((d.remanentes.gt(0) & d.die11.eq(value)).sum()) for value in observed_water],
        "level_selection": "Observed among dentate participants before drop_na, as in original QMD",
    })
    for feature in ["dairy_type", "fruit_portions", "vegetable_portions"]:
        source = {"dairy_type": d.die3.fillna(0), "fruit_portions": d.die7.fillna(0),
                  "vegetable_portions": d.die9.fillna(0)}[feature]
        corrected = {"dairy_type": d.die3.mask(d.die2.eq(7), 0),
                     "fruit_portions": d.die7.mask(d.die6.eq(0), 0),
                     "vegetable_portions": d.die9.mask(d.die8.eq(0), 0)}[feature]
        _assert_equal(source, corrected, f"observed original/conditional structural-skip handling for {feature}")
    summary = {
        "status": "verified",
        "identity": {"key": "IdEncuesta", "n_original_participants": len(d),
                     "original_ids_unique": True, "unmatched_original_ids": 0,
                     "ids_equal_complete_sav_examination_cohort": True,
                     "row_order_equal_to_sav": bool(np.array_equal(d.index, s.index[examined])),
                     "join_requirement": "Match by IdEncuesta; original row order differs from SAV"},
        "cohorts": {"examined": len(d), "dentate": int(d.remanentes.gt(0).sum()),
                    "original_complete_cases": int(original_complete.sum()),
                    "original_complete_case_events": int(d.loc[original_complete, "cariesnt"].sum()),
                    "original_complete_case_ids_equal_python": True},
        "outcome": {"caries_discrepancies": 0, "remaining_teeth_discrepancies": 0, "decayed_teeth_discrepancies": 0},
        "raw_numeric_checks": raw_checks,
        "primary_predictors": {"n": 27, "n_matching_after_declared_harmonization": 27,
                               "n_matching_without_beverage_month_harmonization": 25,
                               "unexpected_discrepancies": 0},
        "category_harmonization": {"education": "DTA nedu0=high,1=intermediate,2=low; SAV education3=high,2=intermediate,1=low",
                                   "indigenous": "DTA c6=0 replaces SAV c6=10; DTA indigena0/1 equals decoded SAV indicator"},
        "beverage_month_conversion": {"original": "Quantity *7 daily; quantity weekly; quantity /4 monthly; zero for nonconsumers. Divide weekly result by7 for legacy daily sensitivity.",
                                      "primary": "Quantity daily; quantity /7 weekly; quantity /30 monthly; zero for nonconsumers.",
                                      "interpretation": "Original monthly conversion implies28 days; primary conversion uses30 days. Source quantity/unit values are identical.",
                                      "differences": unit_differences},
        "water_coding": {"reported_glasses_match": True, "original_r_uses_factor_indices": True,
                         "observed_levels_before_complete_case_deletion": len(observed_water),
                         "note": "An ordered factor index preserves ranking but changes cardinal spacing and the meaning of a unit increase."},
        "structural_skip_handling": "Original unconditional and corrected questionnaire-conditional recoding give identical values in this observed dataset; validation rejects unexplained missing responses.",
        "aggregate_outputs_contain_identifiers": False,
        "legacy_frequency_table": "Contains participant_id for in-memory joining only; do not export as a public result.",
    }
    return summary, pd.DataFrame(comparison_rows), water_coding, legacy.reset_index(drop=True)


def reconcile_original_data(
    sav_path: str | Path, dta_path: str | Path, frame: pd.DataFrame,
) -> tuple[dict[str, Any], pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """Return aggregate summary, 27-feature comparison, water mapping, legacy rates.

    The final table contains participant IDs and must remain private/in memory.
    Original data and the caller's frame are never modified. Unexpected source,
    cohort, coding, or physical-value discrepancies stop the analysis.
    """
    sav_path, dta_path = Path(sav_path), Path(dta_path)
    sav, sav_meta = pyreadstat.read_sav(
        str(sav_path), usecols=RAW_COLUMNS, apply_value_formats=False,
        user_missing=False, disable_datetime_conversion=True,
    )
    # The supplied DTA contains a truncated UTF-8 label. Latin-1 reads the original
    # numeric bytes without failure; no free-text participant responses are read.
    dta, dta_meta = pyreadstat.read_dta(
        str(dta_path), usecols=DTA_COLUMNS, encoding="latin1", apply_value_formats=False,
        user_missing=False, disable_datetime_conversion=True,
    )
    summary, comparison, water_coding, legacy = _compare_sources(sav, dta, frame)
    expected = {"examined": 5520, "dentate": 5036, "original_complete_cases": 4994,
                "original_complete_case_events": 2664}
    for name, value in expected.items():
        if summary["cohorts"][name] != value:
            raise ValueError(f"Original study cohort changed: expected {name}={value}, observed {summary['cohorts'][name]}")
    def digest(path):
        with path.open("rb") as stream:
            return hashlib.file_digest(stream, "sha256").hexdigest()
    summary["sources"] = {
        "original_dta": {"file": dta_path.name, "sha256": digest(dta_path),
                         "size_bytes": dta_path.stat().st_size, "rows": dta_meta.number_rows,
                         "numeric_columns_read": DTA_COLUMNS, "read_encoding": "latin1",
                         "encoding_note": "Fallback for a truncated UTF-8 source label; numeric values unchanged. Labels interpreted through an explicit dictionary, not mojibake text."},
        "supplemental_sav": {"file": sav_path.name, "sha256": digest(sav_path),
                             "size_bytes": sav_path.stat().st_size, "rows": sav_meta.number_rows,
                             "role": "Survey weight/PSU and additional water context absent from original DTA; ID-matched supplementation"},
        "original_code": {"repository": "https://github.com/AmaruSimonAgueroJimenez/ENS-dental-caries",
                          "commit": "77c107feb56d3f825cd4ad26354b672f8bfebe38", "file": "docs/index.qmd"},
    }
    summary["sav_sha256"] = summary["sources"]["supplemental_sav"]["sha256"]
    summary["original_dta_sha256"] = summary["sources"]["original_dta"]["sha256"]
    return summary, comparison, water_coding, legacy
