"""Reconstruct the original paper's 90-column R treatment-contrast matrix.

This historical representation is separate from the modern 27-predictor
preparation. It reproduces complete-case deletion, single-column factor-index
water, categorical fruit/vegetable days, weekly beverage frequencies (28-day
month convention), and the original factor reference levels. No R runtime,
model fitting, imputation or statistical transformation is performed here.

X and y have private participant identifiers as their index. Metadata contain
only the schema, hashes and aggregate checks and can be exported publicly.
"""
from __future__ import annotations

import hashlib
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
import pyreadstat

from .data import CATEGORY_MAPS

HISTORICAL_FEATURE_ORDER = ['die1a',
 'die2',
 'die3',
 'die4',
 'die5',
 'die6',
 'die7',
 'die8',
 'die9',
 'die10_a',
 'die10_b',
 'die10_c',
 'die10_d',
 'die10_e',
 'die10_f',
 'die11',
 'die14',
 'frecsembebida',
 'frecsemjugo',
 'sexo',
 'zona',
 'nedu',
 'region',
 'indigena',
 'c1',
 'remanentes',
 'Edad']

CATEGORY_LEVELS = {'die1a': [(1, 'mas_de_una_vez_a_la_semana'),
           (2, '1_vez_a_la_semana'),
           (3, 'menos_de_3_veces_al_mes'),
           (4, 'menos_de_1_vez_al_mes_o_nunca')],
 'die2': [(1, 'tres_o_mas_veces_al_dia'),
          (2, 'menos_de_tres_veces_al_dia'),
          (3, 'una_vez_al_dia'),
          (4, 'dia_por_medio'),
          (5, 'al_menos_una_vez_por_semana'),
          (6, 'al_menos_una_vez_por_mes'),
          (7, 'nunca')],
 'die3': [(1, 'semi_descremados,_descremados_o_bajos_en_grasas'),
          (2, 'enteros'),
          (None, 'no_consume_lacteos')],
 'die4': [(1, 'mas_de_una_vez_por_dia'),
          (2, 'a_diario'),
          (3, 'dia_por_medio'),
          (4, 'al_menos_una_vez_por_semana'),
          (5, 'al_menos_una_vez_por_mes'),
          (6, 'nunca')],
 'die5': [(1, 'dos_o_mas_veces_por_semana'),
          (2, 'al_menos_una_vez_por_semana'),
          (3, 'entre_una_y_tres_veces_al_mes'),
          (4, 'menos_de_una_vez_al_mes_o_nunca')],
 'die6': [(0, '0'), (1, '1'), (2, '2'), (3, '3'), (4, '4'), (5, '5'), (6, '6'), (7, '7')],
 'die8': [(0, '0'), (1, '1'), (2, '2'), (3, '3'), (4, '4'), (5, '5'), (6, '6'), (7, '7')],
 'die10_a': [(1, 'siempre'),
             (2, 'casi_siempre'),
             (3, 'algunas_veces'),
             (4, 'rara_vez'),
             (5, 'nunca')],
 'die10_b': [(1, 'siempre'),
             (2, 'casi_siempre'),
             (3, 'algunas_veces'),
             (4, 'rara_vez'),
             (5, 'nunca')],
 'die10_c': [(1, 'siempre'),
             (2, 'casi_siempre'),
             (3, 'algunas_veces'),
             (4, 'rara_vez'),
             (5, 'nunca')],
 'die10_d': [(1, 'siempre'),
             (2, 'casi_siempre'),
             (3, 'algunas_veces'),
             (4, 'rara_vez'),
             (5, 'nunca')],
 'die10_e': [(1, 'siempre'),
             (2, 'casi_siempre'),
             (3, 'algunas_veces'),
             (4, 'rara_vez'),
             (5, 'nunca')],
 'die10_f': [(1, 'siempre'),
             (2, 'casi_siempre'),
             (3, 'algunas_veces'),
             (4, 'rara_vez'),
             (5, 'nunca')],
 'die14': [(0, 'ninguno'),
           (1, 'aceite_vegetal_(maravilla,_maiz,_pepa_de_uva)'),
           (2, 'aceite_de_oliva'),
           (3, 'manteca'),
           (4, 'mantequilla'),
           (5, 'margarina'),
           (6, 'otro._especifique:')],
 'sexo': [(1, 'hombre'), (2, 'mujer')],
 'zona': [(1, 'urbana'), (2, 'rural')],
 'nedu': [(0, '13_o_mas'), (1, '8-12'), (2, '<8')],
 'region': [(1, 'xv._arica_y_parinacota'),
            (2, 'i._tarapaca'),
            (3, 'ii._antofagasta'),
            (4, 'iii._atacama'),
            (5, 'iv._coquimbo'),
            (6, 'v._valparaiso'),
            (7, 'xiii._metropolitana'),
            (8, 'vi._l._bdo._ohiggins'),
            (9, 'vii._maule'),
            (10, 'viii._biobio'),
            (11, 'ix._la_araucania'),
            (12, 'xiv._los_rios'),
            (13, 'x._los_lagos'),
            (14, 'xi._aysen'),
            (15, 'xii._magallanes_y_antartica')],
 'indigena': [(0, 'no'), (1, 'si')],
 'c1': [(1, 'chile'), (2, 'otro_(?cual?)')]}

# Numeric codes, normalized labels and level order were verified by R
# preprocessing against the saved 4,994-row 2024 analysis table (all.equal TRUE).
# None denotes the original explicit factor level for missing dairy type.
PREDICTOR_NAMES = {
    "die1a": "fish", "die2": "dairy_frequency", "die3": "dairy_type",
    "die4": "wholegrain", "die5": "legumes", "die6": "fruit_days",
    "die7": "fruit_portions", "die8": "vegetable_days",
    "die9": "vegetable_portions", "die11": "water", "die14": "oil",
    "frecsembebida": "soda_frequency", "frecsemjugo": "juice_frequency",
    "sexo": "sex", "zona": "rural", "nedu": "education", "region": "region",
    "indigena": "indigenous", "c1": "born_chile", "remanentes": "teeth_remaining",
    "Edad": "age",
    **dict(zip([f"die10_{letter}" for letter in "abcdef"],
               ["label_ingredients", "label_nutrition", "label_warnings",
                "label_health", "label_brand", "label_discounts"], strict=True)),
}
HISTORICAL_NUMERIC_FEATURES = [
    name for name in HISTORICAL_FEATURE_ORDER if name not in CATEGORY_LEVELS
]
REQUIRED_COLUMNS = ["IdEncuesta", "cariesnt", *HISTORICAL_FEATURE_ORDER,
                    "die12_cantidad", "die12_unidad", "die13_cantidad", "die13_unidad"]
AUDITED_WATER_LEVELS = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                        16, 18, 20, 24, 25, 28, 30, 32, 40]
AUDITED_CACHE_SHA256 = "6711aa2744a91740332a099c88e8f92bbe53f4827bf95146754d401b3e42b28e"


def _numeric(values: pd.Series, name: str) -> pd.Series:
    result = pd.to_numeric(values, errors="coerce").astype(float)
    if (values.notna() & result.isna()).any() or np.isinf(result).any():
        raise ValueError(f"Invalid numeric value in historical field {name}")
    return result.mask(result.lt(0))


def _identifiers(values: pd.Series) -> pd.Index:
    numeric = _numeric(values, "IdEncuesta")
    if numeric.isna().any() or numeric.mod(1).ne(0).any() or numeric.duplicated().any():
        raise ValueError("Historical participant IDs must be unique observed integers")
    return pd.Index(numeric.map(lambda v: str(int(v))), name="participant_id")


def _validate_category(values: pd.Series, codes, name: str) -> None:
    if not values.dropna().isin(codes).all():
        raise ValueError(f"Unknown category code in historical field {name}")


def _weekly(quantity: pd.Series, unit: pd.Series) -> pd.Series:
    _validate_category(unit, [1, 2, 3, 4], "beverage unit")
    return (quantity * unit.map({1: 7., 2: 1., 3: .25})).mask(unit.eq(4), 0.)


def historical_column_map() -> list[dict[str, Any]]:
    """Return the original ordered 90 terms mapped to 27 modern raw predictors."""
    result = []
    for raw in HISTORICAL_FEATURE_ORDER:
        if raw in CATEGORY_LEVELS:
            levels = CATEGORY_LEVELS[raw]
            for code, level in levels[1:]:
                result.append({"encoded_column": raw + level,
                               "predictor": PREDICTOR_NAMES[raw], "raw_variable": raw,
                               "kind": "category_indicator", "raw_code": code,
                               "level": level, "reference_level": levels[0][1]})
        else:
            result.append({"encoded_column": raw, "predictor": PREDICTOR_NAMES[raw],
                           "raw_variable": raw, "kind": "numeric", "raw_code": None,
                           "level": None, "reference_level": None})
    return result


def _build_historical_matrix(raw: pd.DataFrame) -> tuple[pd.DataFrame, pd.Series, dict]:
    """Pure numeric reconstruction; small fixtures can exercise consequential rules."""
    missing = sorted(set(REQUIRED_COLUMNS) - set(raw))
    if missing:
        raise ValueError(f"Historical source is missing required fields: {missing}")
    d = raw[REQUIRED_COLUMNS].apply(lambda c: _numeric(c, c.name))
    d.index = _identifiers(raw.IdEncuesta)
    _validate_category(d.cariesnt, [0, 1], "cariesnt")
    for name, levels in CATEGORY_LEVELS.items():
        _validate_category(d[name], [code for code, _ in levels if code is not None], name)
    for name, number in [("frecsembebida", 12), ("frecsemjugo", 13)]:
        expected = _weekly(d[f"die{number}_cantidad"], d[f"die{number}_unidad"])
        if not np.isclose(d[name], expected, atol=1e-5, rtol=1e-6, equal_nan=True).all():
            raise ValueError(f"Original weekly beverage conversion mismatch: {name}")

    dentate = d.remanentes.gt(0)
    # R drops unobserved WATER levels after removing edentulous participants,
    # before complete-case deletion. Other factor levels remain source-defined.
    water_levels = sorted(d.loc[dentate, "die11"].dropna().unique().tolist())
    if not d.die11.dropna().between(0, 50).all():
        raise ValueError("Unquantified or unexpected historical water value")
    water_lookup = {value: rank for rank, value in enumerate(water_levels, 1)}
    selected = d.loc[dentate, ["cariesnt", *HISTORICAL_FEATURE_ORDER]].copy()
    selected["die11"] = selected.die11.map(water_lookup)
    # These unconditional rules intentionally reproduce the original QMD.
    # Their empirical equivalence to conditional structural rules is audited
    # by source reconciliation; this function does not extend that assumption.
    selected["die7"] = selected.die7.fillna(0.)
    selected["die9"] = selected.die9.fillna(0.)
    columns_for_completeness = [n for n in selected if n != "die3"]
    complete = selected[columns_for_completeness].notna().all(axis=1)
    selected = selected.loc[complete]
    columns = {}
    for name in HISTORICAL_FEATURE_ORDER:
        if name in CATEGORY_LEVELS:
            for code, level in CATEGORY_LEVELS[name][1:]:
                values = selected[name].isna() if code is None else selected[name].eq(code)
                columns[name + level] = values.to_numpy(dtype=float)
        else:
            columns[name] = selected[name].to_numpy(dtype=float)
    X = pd.DataFrame(columns, index=selected.index)
    y = selected.cariesnt.astype(np.int64).rename("caries")
    mapping = historical_column_map()
    expected_order = [entry["encoded_column"] for entry in mapping]
    if X.columns.tolist() != expected_order or X.shape[1] != 90 or not np.isfinite(X).all().all():
        raise ValueError("Historical matrix violates the finite 90-column contract")
    metadata = {
        "status": "constructed", "schema_version": 1,
        "n_source": len(d), "n_dentate": int(dentate.sum()), "n_complete": len(X),
        "n_cases": int(y.sum()), "n_columns": X.shape[1], "n_raw_predictors": 27,
        "n_numeric_columns": 7, "n_category_indicators": 83,
        "source_feature_order": HISTORICAL_FEATURE_ORDER.copy(),
        "column_order": X.columns.tolist(), "column_map": mapping,
        "category_levels": {
            name: [{"code": code, "label": label} for code, label in levels]
            for name, levels in CATEGORY_LEVELS.items()
        },
        "water_mapping": [{"reported_glasses_per_day": value,
                           "original_r_factor_index": rank}
                          for value, rank in water_lookup.items()],
        "contrast": "Unordered treatment contrasts; first original factor level is reference; no intercept column",
        "outcome": "caries=1, libre_de_caries=0; original factor level order libre_de_caries then caries",
        "beverage_units": "Weekly frequencies retained from DTA: daily quantity*7; weekly quantity; monthly quantity/4; never=0 (28-day month)",
        "fruit_vegetable_days": "Unordered factors with levels 0..7; reference zero; seven indicators each",
        "missingness": "Dentate restriction, missing dairy type assigned no_consume_lacteos, missing die7/die9 set zero, then complete-case deletion",
        "water_support_scope": "Observed original dentate data before complete-case deletion, reproducing historical droplevels; not estimated independently within each training fold",
        "index_privacy": "X/y index contains participant_id and must remain private; no IDs occur in this metadata",
        "numeric_preprocessing": "No scaling, centering or imputation; reproduces formula-derived model matrix only",
    }
    return X, y, metadata


def _validate_frame(raw: pd.DataFrame, X: pd.DataFrame, y: pd.Series,
                    full_frame: pd.DataFrame) -> None:
    required = {"participant_id", "paper_complete", "caries", "weight", "psu", "stratum", *PREDICTOR_NAMES.values()}
    if required - set(full_frame):
        raise ValueError("Analytical frame is missing historical alignment fields")
    if full_frame.participant_id.isna().any() or full_frame.participant_id.duplicated().any():
        raise ValueError("Analytical participant IDs must be unique and observed")
    frame = full_frame.set_index("participant_id", drop=False)
    if set(X.index) != set(frame.index[frame.paper_complete]):
        raise ValueError("Historical complete-case IDs differ from paper_complete domain")
    paired = frame.loc[X.index]
    if not np.array_equal(y.to_numpy(), paired.caries.to_numpy()):
        raise ValueError("Historical outcome differs after participant-ID alignment")
    if paired[["weight", "psu", "stratum"]].isna().any().any() or not paired.weight.gt(0).all():
        raise ValueError("Historical cohort requires observed positive weights and survey design")
    d = raw[REQUIRED_COLUMNS].apply(lambda c: _numeric(c, c.name))
    d.index = _identifiers(raw.IdEncuesta)
    d = d.loc[X.index]
    for name, predictor in PREDICTOR_NAMES.items():
        values = d[name]
        if name == "die3":
            values = values.map(CATEGORY_MAPS[predictor]).fillna("No dairy")
        elif name in ["die7", "die9"]:
            values = values.fillna(0.)
        elif name == "nedu":
            values = (3 - values).map(CATEGORY_MAPS[predictor])
        elif name == "indigena":
            values = values.map({0: "No", 1: "Yes"})
        elif name in ["frecsembebida", "frecsemjugo"]:
            # Compare the common source quantity/unit in the modern 30-day units.
            number = 12 if name == "frecsembebida" else 13
            unit = d[f"die{number}_unidad"]
            values = (d[f"die{number}_cantidad"] / unit.map({1: 1., 2: 7., 3: 30.})).mask(unit.eq(4), 0.)
        elif predictor in CATEGORY_MAPS:
            values = values.map(CATEGORY_MAPS[predictor])
        current = paired[predictor]
        if pd.api.types.is_numeric_dtype(values):
            equal = np.isclose(values, current, atol=1e-5, rtol=1e-6, equal_nan=True)
        else:
            equal = (values.eq(current) | (values.isna() & current.isna())).to_numpy()
        if not equal.all():
            raise ValueError(f"Historical source differs from analytical frame for {predictor}")


def load_historical_matrix(original_dta_path: str | Path, full_frame: pd.DataFrame
                           ) -> tuple[pd.DataFrame, pd.Series, dict[str, Any]]:
    """Return private X/y by participant ID and an identifier-free audited schema.

    Exactly reconstructs the original 4,994-participant, 90-column representation.
    Join weights/design with full_frame.set_index('participant_id').loc[X.index].
    Physical modern feature values are checked by ID after declared unit/category
    reconciliation. A mismatch fails; this function never filters silently to the
    intersection or changes the supplied frame. Production does not require R.
    """
    path = Path(original_dta_path)
    raw, _ = pyreadstat.read_dta(str(path), usecols=REQUIRED_COLUMNS, encoding="latin1",
                               apply_value_formats=False, user_missing=False,
                               disable_datetime_conversion=True)
    X, y, metadata = _build_historical_matrix(raw)
    expected = {"n_source": 5520, "n_dentate": 5036, "n_complete": 4994, "n_cases": 2664}
    if any(metadata[key] != value for key, value in expected.items()):
        raise ValueError("Original historical study cohort/counts changed")
    if [entry["reported_glasses_per_day"] for entry in metadata["water_mapping"]] != AUDITED_WATER_LEVELS:
        raise ValueError("Original historical water factor support changed")
    _validate_frame(raw, X, y, full_frame)
    with path.open("rb") as stream:
        digest = hashlib.file_digest(stream, "sha256").hexdigest()
    metadata.update({"status": "verified", "source_dta_file": path.name,
                     "source_dta_sha256": digest,
                     "read_encoding": "latin1 numeric-only fallback for truncated source label; labels come from the audited fixed schema",
                     "analytical_frame_id_outcome_predictor_design_checks": "passed",
                     "historical_cached_frame_sha256": AUDITED_CACHE_SHA256,
                     "reference_validation": "R preprocessing was all.equal to the saved 2024 4994x28 frame; Python's 90-column matrix is independently tested against that R model.matrix reference when privately available"})
    return X, y, metadata
