"""Reconstruct the caries/food-intake cohort from the ENS 2016–2017 SAV.

This module does not impute predictors or fit data-dependent transformations.
Missing dental examinations remain missing outcomes. Structural questionnaire
skips are resolved only when the preceding response identifies a nonconsumer.
"""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
import pyreadstat


SOCIO_FEATURES = [
    "age", "sex", "region", "rural", "education", "indigenous", "born_chile",
    "teeth_remaining",
]
DIET_FEATURES = [
    "fish", "dairy_frequency", "dairy_type", "wholegrain", "legumes",
    "fruit_days", "fruit_portions", "vegetable_days", "vegetable_portions",
    "label_ingredients", "label_nutrition", "label_warnings", "label_health",
    "label_brand", "label_discounts", "water", "soda_frequency",
    "juice_frequency", "oil",
]
# These are additions to, rather than a replacement for, the primary features.
EXTENDED_FEATURES = ["household_water_source", "rural_water_consumption", "dental_visit"]
NUMERIC_FEATURES = [
    "age", "teeth_remaining", "fruit_days", "fruit_portions", "vegetable_days",
    "vegetable_portions", "water", "soda_frequency", "juice_frequency",
]
CATEGORICAL_FEATURES = [
    feature for feature in SOCIO_FEATURES + DIET_FEATURES + EXTENDED_FEATURES
    if feature not in NUMERIC_FEATURES
]

SOURCES: dict[str, tuple[str, ...]] = {
    "participant_id": ("IdEncuesta",), "age": ("Edad",), "sex": ("Sexo",),
    "region": ("Region",), "rural": ("Zona",),
    "education": ("NEDU1_MINSAL_1",), "indigenous": ("c6",), "born_chile": ("c1",),
    "teeth_remaining": ("m5p3", "m5p6"), "decayed_teeth": ("m5p5", "m5p8"),
    "caries": ("m5p5", "m5p8"), "weight": ("Fexp_F1F2p_Corr",),
    "stratum": ("Estrato",), "psu": ("Conglomerado",),
    "eligible": ("m5p3", "m5p6"),
    "paper_complete": ("m5p3", "m5p6", "NEDU1_MINSAL_1", "die11"),
    "fish": ("die1a",), "dairy_frequency": ("die2",),
    "dairy_type": ("die2", "die3"), "wholegrain": ("die4",), "legumes": ("die5",),
    "fruit_days": ("die6",), "fruit_portions": ("die6", "die7"),
    "vegetable_days": ("die8",), "vegetable_portions": ("die8", "die9"),
    **{name: (f"die10_{letter}",) for name, letter in zip(
        ["label_ingredients", "label_nutrition", "label_warnings", "label_health",
         "label_brand", "label_discounts"], "abcdef", strict=True)},
    "water": ("die11",),
    "soda_frequency": ("die12_cantidad", "die12_unidad"),
    "juice_frequency": ("die13_cantidad", "die13_unidad"), "oil": ("die14",),
    "household_water_source": ("as29",), "rural_water_consumption": ("m8p13",),
    "dental_visit": ("sb2",),
    "soda_glasses_daily": ("die12_cantidad", "die12_unidad", "die12e"),
    "juice_glasses_daily": ("die13_cantidad", "die13_unidad", "die13e"),
}
RAW_COLUMNS = list(dict.fromkeys(
    [column for columns in SOURCES.values() for column in columns] + ["Fexp_F2p_Corr"]
))

LABEL_FREQUENCY = {1: "Always", 2: "Almost always", 3: "Sometimes", 4: "Rarely", 5: "Never"}
CATEGORY_MAPS: dict[str, dict[int, str]] = {
    "sex": {1: "Male", 2: "Female"}, "rural": {1: "Urban", 2: "Rural"},
    "education": {1: "Low", 2: "Intermediate", 3: "High"},
    "born_chile": {1: "Yes", 2: "No"},
    "indigenous": {**dict.fromkeys(range(1, 10), "Yes"), 10: "No"},
    "region": {i: f"{i:02d} {name}" for i, name in enumerate([
        "Arica and Parinacota", "Tarapaca", "Antofagasta", "Atacama", "Coquimbo",
        "Valparaiso", "Metropolitan", "O'Higgins", "Maule", "Biobio",
        "La Araucania", "Los Rios", "Los Lagos", "Aysen", "Magallanes and Antarctica",
    ], 1)},
    "fish": {1: "More than once/week", 2: "Once/week", 3: "Less than three times/month", 4: "Less than once/month or never"},
    "dairy_frequency": {1: "At least three times/day", 2: "Less than three times/day", 3: "Once/day", 4: "Every other day", 5: "At least once/week", 6: "At least once/month", 7: "Never"},
    "dairy_type": {1: "Reduced fat", 2: "Full fat"},
    "wholegrain": {1: "More than once/day", 2: "Daily", 3: "Every other day", 4: "At least once/week", 5: "At least once/month", 6: "Never"},
    "legumes": {1: "At least twice/week", 2: "At least once/week", 3: "One to three times/month", 4: "Less than once/month or never"},
    **{f"label_{name}": LABEL_FREQUENCY for name in ["ingredients", "nutrition", "warnings", "health", "brand", "discounts"]},
    "oil": {0: "None", 1: "Vegetable oil", 2: "Olive oil", 3: "Lard", 4: "Butter", 5: "Margarine", 6: "Other"},
    "household_water_source": {1: "Public network", 2: "Public network", 3: "Public network", 4: "Other", 5: "Other", 6: "Other", 7: "Other"},
    "rural_water_consumption": {1: "Yes", 2: "No"},
    "dental_visit": {1: "Less than 6 months", 2: "6–12 months", 3: "More than 1 to less than 2 years", 4: "2 to less than 5 years", 5: "At least 5 years", 6: "Never"},
}

DESCRIPTIONS = {
    "participant_id": "Unique survey participant identifier; excluded from predictors",
    "age": "Age at survey", "sex": "Recorded sex", "region": "Survey region (2016 boundaries)",
    "rural": "Urban/rural residence", "education": "Low <8, intermediate 8–12, high >=13 years of schooling",
    "indigenous": "Self-reported membership/ancestry of a recognized indigenous people",
    "born_chile": "Born in Chile", "teeth_remaining": "Total remaining teeth, upper plus lower jaw",
    "decayed_teeth": "Total clinically observed decayed teeth, upper plus lower jaw",
    "caries": "Prevalent cavitated caries: at least one clinically observed decayed tooth",
    "weight": "Joint F1/F2 calibrated expansion weight, supplied by ENS",
    "stratum": "Survey sampling stratum", "psu": "Primary sampling unit (Conglomerado)",
    "eligible": "Dentate participant with both remaining-tooth counts observed",
    "paper_complete": "Eligible participant with observed education and water intake; manuscript cohort reconstruction",
    "fish": "Fish/seafood consumption frequency", "dairy_frequency": "Dairy consumption frequency",
    "dairy_type": "Usual dairy fat content, with structural category for nonconsumers",
    "wholegrain": "Wholegrain consumption frequency", "legumes": "Legume consumption frequency",
    "fruit_days": "Fruit consumption days in a typical week",
    "fruit_portions": "Fruit portions on a consumption day; zero for nonconsumers",
    "vegetable_days": "Vegetable consumption days in a typical week",
    "vegetable_portions": "Vegetable portions on a consumption day; zero for nonconsumers",
    **{f"label_{name}": f"Frequency of checking/considering package {name}" for name in ["ingredients", "nutrition", "warnings", "health", "brand", "discounts"]},
    "water": "Self-reported glasses of water per day; water source/fluoride exposure not identified",
    "soda_frequency": "Sugar-sweetened soda consumption frequency converted to times/day",
    "juice_frequency": "Sugar-sweetened juice consumption frequency converted to times/day",
    "oil": "Most frequently used cooking oil/fat",
    "household_water_source": "Household supply: public network vs other; not a measure of fluoride exposure",
    "rural_water_consumption": "Consumed well, spring or rural drinking water during the previous week for drinking/cooking; not bottled water or fluoride concentration",
    "dental_visit": "Time since last dental visit; possible reverse causation, extended sensitivity only",
    "soda_glasses_daily": "Soda frequency times reported glasses/occasion; ancillary exposure with extreme-value audit",
    "juice_glasses_daily": "Juice frequency times reported glasses/occasion; ancillary exposure with extreme-value audit",
}


def _nonnegative(values: pd.Series) -> pd.Series:
    """All negative questionnaire values are missing, never genuine zero."""
    values = pd.to_numeric(values, errors="raise").astype(float)
    return values.mask(values.lt(0) | ~np.isfinite(values))


def _categorical(values: pd.Series, mapping: dict[int, str], name: str) -> pd.Series:
    unknown = sorted(values.dropna().loc[lambda s: ~s.isin(mapping)].unique().tolist())
    if unknown:
        raise ValueError(f"Unexpected positive category codes for {name}: {unknown}")
    # Object strings plus numpy NaN are compatible with fold-fitted sklearn imputation.
    return values.map(mapping).astype(object)


def _identifier(values: pd.Series, name: str) -> pd.Series:
    if (values.dropna() % 1 != 0).any():
        raise ValueError(f"Noninteger {name}")
    return values.map(lambda x: str(int(x)) if pd.notna(x) else np.nan).astype(object)


def _daily_frequency(quantity: pd.Series, unit: pd.Series) -> pd.Series:
    """Unit 1=daily, 2=weekly, 3=monthly (30 days), 4=nonconsumer."""
    unknown = unit.notna() & ~unit.isin([1, 2, 3, 4])
    if unknown.any():
        raise ValueError(f"Unexpected beverage frequency units: {unit[unknown].unique()}")
    result = quantity / unit.map({1: 1.0, 2: 7.0, 3: 30.0})
    return result.mask(unit.eq(4), 0.0)


def _dictionary(labels: dict[str, str] | None, value_labels: dict | None) -> pd.DataFrame:
    labels, value_labels = labels or {}, value_labels or {}
    units = {"age": "years", "teeth_remaining": "teeth", "decayed_teeth": "teeth",
             "fruit_days": "days/week", "vegetable_days": "days/week",
             "fruit_portions": "portions/consumption day", "vegetable_portions": "portions/consumption day",
             "water": "glasses/day", "soda_frequency": "times/day", "juice_frequency": "times/day",
             "soda_glasses_daily": "glasses/day", "juice_glasses_daily": "glasses/day",
             "weight": "represented persons", "caries": "binary 0/1"}
    rules = {
        "teeth_remaining": "Add both jaw counts only if both observed; each jaw must be integer 0–16.",
        "decayed_teeth": "Add both jaw counts only if both observed; verify decay <= remaining teeth in each jaw.",
        "caries": "1 if decayed_teeth >0, 0 if zero, missing if either jaw outcome missing. No outcome imputation.",
        "dairy_type": "If die2=7 set 'No dairy'; otherwise missing die3 remains missing.",
        "fruit_portions": "If die6=0 set 0; otherwise preserve missing die7. Do not clip large values.",
        "vegetable_portions": "If die8=0 set 0; otherwise preserve missing die9. Do not clip large values.",
        "water": "Preserve 0–50 glasses; code 51='other value, specify' is unquantified and missing; no clipping.",
        "soda_frequency": "Quantity / (1,7,30) for unit (1,2,3); unit4 nonconsumer=0, unknown/missing unit remains missing.",
        "juice_frequency": "Quantity / (1,7,30) for unit (1,2,3); unit4 nonconsumer=0, unknown/missing unit remains missing.",
        "soda_glasses_daily": "Daily frequency * die12e; nonconsumer=0 even if glasses/occasion missing; no clipping.",
        "juice_glasses_daily": "Daily frequency * die13e; nonconsumer=0 even if glasses/occasion missing; no clipping.",
        "eligible": "teeth_remaining >0; missing examination is not eligible. Outcome completeness checked in audit.",
        "paper_complete": "eligible AND education observed AND water observed. Retain other predictor missingness.",
    }
    rows = []
    for name, raw_columns in SOURCES.items():
        mapping = CATEGORY_MAPS.get(name, {})
        if name == "dairy_type":
            mapping = {**mapping, "structural": "No dairy"}
        rows.append({
            "variable": name, "description": DESCRIPTIONS[name],
            "raw_variables": " + ".join(raw_columns),
            "raw_labels": " | ".join(f"{col}: {labels.get(col, '')}" for col in raw_columns),
            "raw_value_labels": json.dumps({col: value_labels.get(col, {}) for col in raw_columns}, ensure_ascii=False),
            "units": units.get(name, "category" if name in CATEGORY_MAPS else "identifier/indicator"),
            "english_categories": json.dumps(mapping, ensure_ascii=False),
            "missing_and_derivation_rule": "Negative special codes and nonfinite values become missing. " + rules.get(name, "Missing values retained; categorical codes mapped without imposing linear spacing."),
            "predictor_role": "primary" if name in SOCIO_FEATURES + DIET_FEATURES else "extended sensitivity" if name in EXTENDED_FEATURES else "ancillary" if name.endswith("glasses_daily") else "outcome/design/cohort",
        })
    return pd.DataFrame(rows)


def derive_caries_data(
    raw: pd.DataFrame,
    source_labels: dict[str, str] | None = None,
    source_value_labels: dict | None = None,
) -> tuple[pd.DataFrame, pd.DataFrame, dict[str, Any]]:
    """Deterministically derive variables; separately exposed for meaningful tests."""
    missing_columns = sorted(set(RAW_COLUMNS) - set(raw.columns))
    if missing_columns:
        raise ValueError(f"Missing required numeric ENS columns: {missing_columns}")
    clean = raw[RAW_COLUMNS].apply(_nonnegative)
    out = pd.DataFrame(index=raw.index)
    for name in ["participant_id", "stratum", "psu"]:
        out[name] = _identifier(clean[SOURCES[name][0]], name)
    if out.participant_id.isna().any() or out.participant_id.duplicated().any():
        raise ValueError("Participant identifiers must be observed and unique")
    for name in ["age", "weight", "fruit_days", "vegetable_days", "water"]:
        out[name] = clean[SOURCES[name][0]]
    for name in CATEGORY_MAPS:
        # Dairy type is the second source; the first source identifies structural skips.
        source = SOURCES[name][-1] if name == "dairy_type" else SOURCES[name][0]
        out[name] = _categorical(clean[source], CATEGORY_MAPS[name], name)
    out["dairy_type"] = out.dairy_type.mask(clean.die2.eq(7), "No dairy")
    for name, days, portions in [("fruit_portions", "die6", "die7"), ("vegetable_portions", "die8", "die9")]:
        out[name] = clean[portions].mask(clean[days].eq(0), 0.0)
    for name in ["fruit_days", "vegetable_days"]:
        if not out[name].dropna().isin(range(8)).all():
            raise ValueError(f"{name} must be an integer between 0 and 7")
    if out.water.gt(51).any():
        raise ValueError("Unexpected water response above the documented coding range 0–51")
    out["water"] = out.water.mask(out.water.eq(51))
    for drink, number in [("soda", 12), ("juice", 13)]:
        quantity, unit = clean[f"die{number}_cantidad"], clean[f"die{number}_unidad"]
        out[f"{drink}_frequency"] = _daily_frequency(quantity, unit)
        out[f"{drink}_glasses_daily"] = (out[f"{drink}_frequency"] * clean[f"die{number}e"]).mask(unit.eq(4), 0.0)
    for remaining, decay in [("m5p3", "m5p5"), ("m5p6", "m5p8")]:
        for name in [remaining, decay]:
            values = clean[name].dropna()
            if (values.gt(16) | values.mod(1).ne(0)).any():
                raise ValueError(f"Impossible dental counts in {name}: expected integer 0–16")
        if clean[decay].gt(clean[remaining]).any():
            raise ValueError(f"Impossible dental counts: {decay} exceeds {remaining}")
    # Arithmetic addition intentionally propagates a missing jaw, unlike sum(skipna=True).
    out["teeth_remaining"] = clean.m5p3 + clean.m5p6
    out["decayed_teeth"] = clean.m5p5 + clean.m5p8
    out["caries"] = out.decayed_teeth.gt(0).astype(float).mask(out.decayed_teeth.isna())
    out["eligible"] = out.teeth_remaining.gt(0)
    out["paper_complete"] = out.eligible & out.education.notna() & out.water.notna()
    examined = out.teeth_remaining.notna() & out.decayed_teeth.notna()
    eligible = out.eligible
    if (examined & (out.weight.isna() | out.weight.le(0))).any():
        raise ValueError("Examined participants require a positive F1/F2 survey weight")
    if out[["stratum", "psu"]].isna().any().any():
        raise ValueError("Survey stratum and primary sampling unit must be observed")
    if out.groupby("psu").stratum.nunique().gt(1).any():
        raise ValueError("Primary sampling units unexpectedly span sampling strata")

    sample_flow = {
        "all_participants": len(out), "dental_examination": int(examined.sum()),
        "no_complete_dental_examination": int((~examined).sum()),
        "edentulous": int(out.teeth_remaining.eq(0).sum()),
        "dentate_eligible": int(eligible.sum()),
        "dentate_outcome_missing": int((eligible & out.caries.isna()).sum()),
        "dentate_cases": int(out.loc[eligible, "caries"].sum()),
        "dentate_non_cases": int(out.loc[eligible, "caries"].eq(0).sum()),
        "dentate_missing_education": int((eligible & out.education.isna()).sum()),
        "dentate_missing_water": int((eligible & out.water.isna()).sum()),
        "dentate_missing_both_education_water": int((eligible & out.education.isna() & out.water.isna()).sum()),
        "paper_complete": int(out.paper_complete.sum()),
        "paper_cases": int(out.loc[out.paper_complete, "caries"].sum()),
        "paper_non_cases": int(out.loc[out.paper_complete, "caries"].eq(0).sum()),
    }
    thresholds = {"water": 20, "fruit_portions": 10, "vegetable_portions": 10,
                  "soda_frequency": 10, "juice_frequency": 10,
                  "soda_glasses_daily": 20, "juice_glasses_daily": 20}
    extremes = {}
    for name, threshold in thresholds.items():
        values = out.loc[eligible, name].dropna()
        extremes[name] = {
            "n_observed": len(values), "min": float(values.min()) if len(values) else None,
            "median": float(values.median()) if len(values) else None,
            "p99": float(values.quantile(.99)) if len(values) else None,
            "max": float(values.max()) if len(values) else None,
            "audit_threshold": threshold, "n_above_threshold": int(values.gt(threshold).sum()),
            "action": "Retained; threshold is a review flag, not a validated cutoff or exclusion",
        }
    psus_per_stratum = out.groupby("stratum").psu.nunique()
    eligible_psus = out.loc[eligible].groupby("stratum").psu.nunique()
    weights = out.loc[eligible, "weight"]
    audit: dict[str, Any] = {
        "sample_flow": sample_flow,
        "structural_skips": {
            "dairy_nonconsumers": int(clean.die2.eq(7).sum()),
            "dairy_missing_recoded_no_dairy": int((clean.die2.eq(7) & clean.die3.isna()).sum()),
            "fruit_nonconsumers": int(clean.die6.eq(0).sum()),
            "fruit_missing_portions_recoded_zero": int((clean.die6.eq(0) & clean.die7.isna()).sum()),
            "vegetable_nonconsumers": int(clean.die8.eq(0).sum()),
            "vegetable_missing_portions_recoded_zero": int((clean.die8.eq(0) & clean.die9.isna()).sum()),
            "soda_nonconsumers": int(clean.die12_unidad.eq(4).sum()),
            "juice_nonconsumers": int(clean.die13_unidad.eq(4).sum()),
        },
        "raw_negative_special_codes": {col: int(pd.to_numeric(raw[col]).lt(0).sum()) for col in RAW_COLUMNS if pd.to_numeric(raw[col]).lt(0).any()},
        "raw_missing_counts": {col: int(raw[col].isna().sum()) for col in RAW_COLUMNS},
        "derived_missing_all": {col: int(out[col].isna().sum()) for col in out},
        "derived_missing_dentate": {col: int(out.loc[eligible, col].isna().sum()) for col in out},
        "unquantified_water_code_51": int(clean.die11.eq(51).sum()),
        "extreme_values_dentate": extremes,
        "survey_design": {
            "weight_variable": "Fexp_F1F2p_Corr", "weight_identical_to_F2": bool(clean.Fexp_F1F2p_Corr.equals(clean.Fexp_F2p_Corr)),
            "n_strata_full": int(out.stratum.nunique()), "n_psu_full": int(out.psu.nunique()),
            "n_strata_dentate": int(out.loc[eligible, "stratum"].nunique()), "n_psu_dentate": int(out.loc[eligible, "psu"].nunique()),
            "singleton_strata_full": psus_per_stratum[psus_per_stratum.eq(1)].index.tolist(),
            "singleton_strata_dentate": eligible_psus[eligible_psus.eq(1)].index.tolist(),
            "dentate_weight_sum": float(weights.sum()),
            "dentate_kish_effective_n": float(weights.sum() ** 2 / (weights ** 2).sum()) if len(weights) else None,
        },
        "integrity": {"participant_ids_unique": True, "impossible_dental_counts": 0,
                      "outcomes_imputed": 0, "predictors_statistically_imputed": 0,
                      "extreme_values_clipped": 0},
        "interpretation_notes": [
            "Cross-sectional prevalent caries, not incident caries or causal protection.",
            "The main dentate cohort retains predictor missingness for training-fold-only imputation.",
            "The reconstructed manuscript cohort requires observed education and water only.",
            "Beverage quantities are frequency reports; glasses/day additionally use glasses/occasion.",
            "Household supply and rural-water consumption do not measure fluoridation or bottled-water intake.",
            "Raw free-text/string columns are intentionally not imported or exported.",
        ],
    }
    # Stable output order makes the provenance and exported frame easy to compare.
    out = out[list(SOURCES)]
    return out, _dictionary(source_labels, source_value_labels), audit


def load_caries_data(path: str | Path) -> tuple[pd.DataFrame, pd.DataFrame, dict[str, Any]]:
    """Read only documented numeric fields and return full cohort, dictionary, audit.

    The explicit usecols list avoids malformed encoding in unrelated free-text
    responses in this SAV. No local participant-level cache is created.
    """
    path = Path(path)
    raw, metadata = pyreadstat.read_sav(
        str(path), usecols=RAW_COLUMNS, apply_value_formats=False,
        user_missing=True, disable_datetime_conversion=True,
    )
    frame, dictionary, audit = derive_caries_data(
        raw, metadata.column_names_to_labels, metadata.variable_value_labels,
    )
    with path.open("rb") as source:
        audit["source_sha256"] = hashlib.file_digest(source, "sha256").hexdigest()
    audit.update({"source_file": path.name, "source_size_bytes": path.stat().st_size,
                  "source_encoding": metadata.file_encoding,
                  "source_numeric_columns_read": RAW_COLUMNS})
    return frame, dictionary, audit
