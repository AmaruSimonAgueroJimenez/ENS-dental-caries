"""Deterministic additional predictors for the separate dietary contribution study.

The original loader and its outcomes are retained. Questionnaire variables are
joined by participant ID, never by row position. No fitted preprocessing, outcome
selection, clipping, or participant-level export takes place in this module.
"""

from __future__ import annotations

from copy import deepcopy
import json
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
import pyreadstat

from .data import (
    DIET_FEATURES, NUMERIC_FEATURES as BASE_NUMERIC_FEATURES, SOCIO_FEATURES,
    load_caries_data,
)


CONTEXT_FEATURES = SOCIO_FEATURES + [
    "income_band", "health_insurance", "smoking_status", "diabetes_reported",
    "dental_visit", "dental_visit_reason", "dental_access",
    "household_water_source", "rural_water_consumption",
]
SYMPTOM_FEATURES = [
    "oral_health_selfrating", "oral_pain", "oral_speech", "oral_eating",
    "oral_daily_activities", "oral_social",
]
LABEL_FEATURES = [name for name in DIET_FEATURES if name.startswith("label_")]
INTAKE_FEATURES = [name for name in DIET_FEATURES if name not in LABEL_FEATURES] + [
    "soda_glasses_daily", "juice_glasses_daily",
]
ALL_FEATURES = CONTEXT_FEATURES + SYMPTOM_FEATURES + INTAKE_FEATURES + LABEL_FEATURES
NUMERIC_FEATURES = BASE_NUMERIC_FEATURES + ["soda_glasses_daily", "juice_glasses_daily"]
EXPANDED_NUMERIC_FEATURES = NUMERIC_FEATURES
CATEGORICAL_FEATURES = [name for name in ALL_FEATURES if name not in NUMERIC_FEATURES]

SOURCES = {
    "income_band": ("as27", "as28"), "health_insurance": ("as5_1",),
    "smoking_status": ("ta3",), "diabetes_reported": ("di3",),
    "dental_visit_reason": ("sb2", "sb3"), "dental_access": ("sb4",),
    "oral_health_selfrating": ("sb1",), "oral_pain": ("sb5_2",),
    "oral_speech": ("sb5_1",), "oral_eating": ("sb5_3",),
    "oral_daily_activities": ("sb5_4",), "oral_social": ("sb5_5",),
}
RAW_COLUMNS = ["IdEncuesta"] + list(dict.fromkeys(c for cols in SOURCES.values() for c in cols))

# Exact pesos use contiguous intervals. At every listed lower bound, including
# 1,573,000, the SAV's supplied as28 band agrees with the upper interval. This
# resolves the loose endpoint wording in the first/last Spanish value labels.
INCOME_LOWER_BOUNDS = [78000, 135000, 218000, 296000, 384000, 481000, 608000,
                       765000, 1030000, 1573000]
INCOME_LABELS = {
    1: "01 <78,000 CLP/month", 2: "02 78,000–134,999 CLP/month",
    3: "03 135,000–217,999 CLP/month", 4: "04 218,000–295,999 CLP/month",
    5: "05 296,000–383,999 CLP/month", 6: "06 384,000–480,999 CLP/month",
    7: "07 481,000–607,999 CLP/month", 8: "08 608,000–764,999 CLP/month",
    9: "09 765,000–1,029,999 CLP/month", 10: "10 1,030,000–1,572,999 CLP/month",
    11: "11 >=1,573,000 CLP/month",
}
FREQUENCY_LABELS = {1: "Never", 2: "Almost never", 3: "Sometimes", 4: "Almost always", 5: "Always"}
CATEGORY_MAPS = {
    "income_band": INCOME_LABELS,
    "health_insurance": {1: "Public FONASA A", 2: "Public FONASA B", 3: "Public FONASA C",
                         4: "Public FONASA D", 5: "Public FONASA, group unknown",
                         6: "Armed forces or police", 7: "Private ISAPRE", 8: "None", 9: "Other"},
    "smoking_status": {1: "Current daily", 2: "Current occasional", 3: "Former", 4: "Never"},
    "diabetes_reported": {1: "Yes", 2: "No"},
    "dental_visit_reason": {1: "Pain or discomfort", 2: "Treatment or continuation",
                            3: "Routine check-up"},
    "dental_access": {1: "Received care", 2: "Appointment obtained, unable to attend",
                      3: "Requested care, not provided", 4: "Needed care, did not request",
                      5: "Did not need care"},
    "oral_health_selfrating": {1: "Very good", 2: "Good", 3: "Fair", 4: "Poor", 5: "Very poor"},
    **{name: FREQUENCY_LABELS for name in SYMPTOM_FEATURES[1:]},
}
DESCRIPTIONS = {
    "income_band": "Monthly net income of the entire household, in questionnaire bands",
    "health_insurance": "Participant's health insurance system, including public insurance group",
    "smoking_status": "Self-reported current daily, occasional, former or never cigarette smoking",
    "diabetes_reported": "Ever told by a health professional that the participant had diabetes",
    "dental_visit_reason": "Main reason for the most recent dental visit",
    "dental_access": "Dental care received or unmet need during the previous six months",
    "oral_health_selfrating": "Overall self-rated oral health",
    "oral_pain": "Frequency of suffering or pain attributed to teeth or dentures",
    "oral_speech": "Frequency of discomfort from teeth or dentures when speaking",
    "oral_eating": "Frequency of discomfort from teeth or dentures when eating",
    "oral_daily_activities": "Frequency of interference with daily activities from teeth or dentures",
    "oral_social": "Frequency of interference with social relationships from teeth or dentures",
}
RULES = {
    "income_band": "Use nonnegative as27 in CLP/month and the ten fixed lower bounds; otherwise use as28 codes 1–11. Bounds are left-inclusive, including 1,573,000 in band11. No interval midpoints or estimated income. Exact amount takes precedence; disagreements are audited.",
    "diabetes_reported": "di3=3 (does not remember) is missing, although its numeric code is positive.",
    "dental_visit_reason": "sb2=6 establishes structural 'Never visited'; otherwise unknown/missing sb3 stays missing. A substantive sb3 response with sb2=6 is an inconsistent questionnaire route and raises an error.",
    "dental_access": "All three negative special codes, including -7777 (not applicable), remain missing; do not interpret missing as no care.",
}


def _nonnegative(values: pd.Series) -> pd.Series:
    values = pd.to_numeric(values, errors="raise").astype(float)
    return values.mask(~np.isfinite(values) | values.lt(0))


def _validated_codes(values: pd.Series, allowed: Any, name: str) -> pd.Series:
    invalid = values.notna() & ~values.isin(allowed)
    if invalid.any():
        raise ValueError(f"Unexpected positive category code for {name}")
    return values


def _raw_ids(values: pd.Series) -> pd.Series:
    numeric = pd.to_numeric(values, errors="raise").astype(float)
    if (~np.isfinite(numeric) | numeric.le(0) | numeric.mod(1).ne(0)).any():
        raise ValueError("Participant IDs must be observed positive integers")
    ids = numeric.map(lambda x: str(int(x))).astype(object)
    if ids.duplicated().any():
        raise ValueError("Duplicate participant IDs in additional questionnaire data")
    return ids


def _align_raw(frame: pd.DataFrame, raw: pd.DataFrame) -> pd.DataFrame:
    missing = sorted(set(RAW_COLUMNS) - set(raw.columns))
    if missing:
        raise ValueError(f"Missing required additional ENS columns: {missing}")
    if "participant_id" not in frame or frame.participant_id.isna().any() or frame.participant_id.duplicated().any():
        raise ValueError("Base frame requires observed unique participant IDs")
    ids = _raw_ids(raw.IdEncuesta)
    if set(ids) != set(frame.participant_id):
        raise ValueError("Base and additional questionnaire participant ID sets differ")
    aligned = raw.set_index(pd.Index(ids)).loc[frame.participant_id, RAW_COLUMNS].copy()
    aligned.index = frame.index
    return aligned


def derive_expanded_data(
    frame: pd.DataFrame,
    raw: pd.DataFrame,
    dictionary: pd.DataFrame,
    base_audit: dict[str, Any],
    source_labels: dict[str, str] | None = None,
    source_value_labels: dict | None = None,
) -> tuple[pd.DataFrame, pd.DataFrame, dict[str, Any]]:
    """Join and derive additional predictors, with no mutation of caller inputs.

    Kept separate from file loading for synthetic ID/skip/boundary tests. Outcomes
    are read only for cohort audit, never for derivation or statistical imputation.
    """
    if set(SOURCES) & set(frame):
        raise ValueError("Base frame already contains additional predictor columns")
    aligned = _align_raw(frame, raw)
    clean = aligned[RAW_COLUMNS[1:]].apply(_nonnegative)
    out, audit = frame.copy(deep=True), deepcopy(base_audit)
    labels, value_labels = source_labels or {}, source_value_labels or {}

    bands = _validated_codes(clean.as28, range(1, 12), "as28")
    amount_bands = pd.Series(np.searchsorted(INCOME_LOWER_BOUNDS, clean.as27, side="right") + 1,
                             index=frame.index, dtype=float).mask(clean.as27.isna())
    out["income_band"] = amount_bands.fillna(bands).map(INCOME_LABELS).astype(object)
    for name, mapping in CATEGORY_MAPS.items():
        if name == "income_band":
            continue
        values = clean[SOURCES[name][-1]].copy()
        allowed = [1, 2, 3] if name == "diabetes_reported" else mapping
        _validated_codes(values, allowed, SOURCES[name][-1])
        if name == "diabetes_reported":
            values = values.mask(values.eq(3))
        out[name] = values.map(mapping).astype(object)

    _validated_codes(clean.sb2, range(1, 7), "sb2")
    never_visited = clean.sb2.eq(6)
    if (never_visited & clean.sb3.notna()).any():
        raise ValueError("Inconsistent dental route: never visited with substantive visit reason")
    if "dental_visit" in frame and not frame.dental_visit.eq("Never").equals(never_visited):
        raise ValueError("Dental visit history disagrees after participant ID alignment")
    out["dental_visit_reason"] = out.dental_visit_reason.mask(never_visited, "Never visited")

    roles = {name: role for role, names in [
        ("context", CONTEXT_FEATURES), ("oral symptoms", SYMPTOM_FEATURES),
        ("dietary intake", INTAKE_FEATURES), ("label-related behaviour", LABEL_FEATURES),
    ] for name in names}
    additions = []
    for name, sources in SOURCES.items():
        categories = dict(CATEGORY_MAPS[name])
        if name == "dental_visit_reason":
            categories["structural"] = "Never visited"
        additions.append({
            "variable": name, "description": DESCRIPTIONS[name],
            "raw_variables": " + ".join(sources),
            "raw_labels": " | ".join(f"{c}: {labels.get(c, '')}" for c in sources),
            "raw_value_labels": json.dumps({c: value_labels.get(c, {}) for c in sources}, ensure_ascii=False),
            "units": "household CLP/month, category" if name == "income_band" else "category",
            "english_categories": json.dumps(categories, ensure_ascii=False),
            "missing_and_derivation_rule": "Negative special codes and nonfinite values are missing. " + RULES.get(name, "Preserve categories and unexplained missingness; no fitted transformation."),
            "predictor_role": roles[name],
        })
    dictionary_out = pd.concat([dictionary.copy(deep=True), pd.DataFrame(additions)], ignore_index=True)
    dictionary_out["expanded_predictor_role"] = dictionary_out.variable.map(roles).fillna("outcome/design/cohort or ancillary")

    eligible = out.eligible.astype(bool)
    both = amount_bands.notna() & bands.notna()
    audit["expanded_feature_blocks"] = {"context": CONTEXT_FEATURES.copy(), "oral_symptoms": SYMPTOM_FEATURES.copy(),
                                        "dietary_intake": INTAKE_FEATURES.copy(), "label_behaviour": LABEL_FEATURES.copy()}
    audit["expanded_income_coding"] = {
        "lower_bounds_clp": INCOME_LOWER_BOUNDS.copy(), "midpoints_used": False,
        "exact_amount_available_all": int(amount_bands.notna().sum()),
        "exact_amount_available_dentate": int((eligible & amount_bands.notna()).sum()),
        "fallback_band_used_all": int((amount_bands.isna() & bands.notna()).sum()),
        "fallback_band_used_dentate": int((eligible & amount_bands.isna() & bands.notna()).sum()),
        "amount_and_band_observed_all": int(both.sum()),
        "amount_band_disagreements_all": int((both & amount_bands.ne(bands)).sum()),
        "amount_band_disagreements_dentate": int((eligible & both & amount_bands.ne(bands)).sum()),
        "boundary_rule": "Each fixed lower bound belongs to the upper band; >=1,573,000 is band11. Exact amount takes precedence over supplied band.",
    }
    audit.setdefault("structural_skips", {}).update({
        "dental_never_visited_all": int(never_visited.sum()),
        "dental_never_visited_dentate": int((eligible & never_visited).sum()),
    })
    audit["expanded_positive_unknown_diabetes"] = {"all": int(clean.di3.eq(3).sum()),
                                                   "dentate": int((eligible & clean.di3.eq(3)).sum())}
    audit["expanded_raw_special_codes"] = {
        c: {str(int(code)): int(n) for code, n in aligned[c].loc[aligned[c].lt(0)].value_counts().sort_index().items()}
        for c in RAW_COLUMNS[1:] if aligned[c].lt(0).any()
    }
    audit["expanded_raw_missing_counts"] = {c: int(aligned[c].isna().sum()) for c in RAW_COLUMNS[1:]}
    audit["derived_missing_all"] = {c: int(out[c].isna().sum()) for c in out}
    audit["derived_missing_dentate"] = {c: int(out.loc[eligible, c].isna().sum()) for c in out}
    audit["expanded_coverage_dentate"] = {
        c: {"observed": int(out.loc[eligible, c].notna().sum()), "missing": int(out.loc[eligible, c].isna().sum())}
        for c in ALL_FEATURES
    }
    audit.setdefault("integrity", {}).update({"expanded_id_join_one_to_one": True,
                                              "expanded_derivations_use_outcome": False,
                                              "expanded_predictors_statistically_imputed": 0})
    audit.setdefault("interpretation_notes", []).extend([
        "Additional predictors form a separate dietary contribution proposal; the original27 analysis is unchanged.",
        "Oral self-rating, symptoms and dental care are concurrent reports and may reflect existing disease or treatment, not temporally established causes.",
        "The loader retains raw derived beverage volumes without clipping or log transformation. Any log1p or p99 sensitivity is applied by the model pipeline, with p99 learned from training observations only.",
        "Extreme-value quantiles in this audit are descriptive only, not preprocessing thresholds.",
        "No brushing, toothpaste/fluoride, floss or xerostomia measures were found in this SAV's variable labels.",
    ])
    return out, dictionary_out, audit


def load_expanded_data(sav_path: str | Path) -> tuple[pd.DataFrame, pd.DataFrame, dict[str, Any]]:
    """Load the full ENS frame plus additional predictors; retain all 6233 rows.

    The eligible analysis domain must reproduce 5036 participants and 2689 cases.
    Numerical-only reads avoid malformed encoding in unrelated free-text fields.
    """
    frame, dictionary, base_audit = load_caries_data(sav_path)
    raw, metadata = pyreadstat.read_sav(
        str(sav_path), usecols=RAW_COLUMNS, apply_value_formats=False,
        user_missing=True, disable_datetime_conversion=True,
    )
    out, dictionary_out, audit = derive_expanded_data(
        frame, raw, dictionary, base_audit,
        metadata.column_names_to_labels, metadata.variable_value_labels,
    )
    observed = {"all_participants": len(out), "dentate_eligible": int(out.eligible.sum()),
                "dentate_cases": int(out.loc[out.eligible, "caries"].sum()),
                "dentate_outcome_missing": int(out.loc[out.eligible, "caries"].isna().sum())}
    expected = {"all_participants": 6233, "dentate_eligible": 5036,
                "dentate_cases": 2689, "dentate_outcome_missing": 0}
    if observed != expected:
        raise ValueError(f"Expanded ENS cohort does not match the audited source counts: {observed}")
    audit["expanded_cohort_validation"] = {"status": "verified", **observed}
    audit["source_numeric_columns_read"] = list(dict.fromkeys(audit["source_numeric_columns_read"] + RAW_COLUMNS))
    return out, dictionary_out, audit
