"""Survey-weighted predictor distributions by observed caries status.

``descriptive_profile(frame)`` accepts the complete frame from
``load_caries_data`` and returns an aggregate long DataFrame and JSON-safe
metadata. Positive examination-phase weights define the survey design;
dentate/outcome domains do not remove PSUs before variance estimation.
No model is fitted, no predictor is imputed, and no hypothesis tests are run.
"""
from __future__ import annotations

import warnings

import numpy as np
import pandas as pd
from scipy.special import expit, logit
from scipy.stats import t

from .data import CATEGORY_MAPS, DESCRIPTIONS, DIET_FEATURES, NUMERIC_FEATURES, SOCIO_FEATURES
from .survey import SurveyWarning, design_covariance


PRIMARY_VARIABLES = tuple(SOCIO_FEATURES + DIET_FEATURES)
AGE_LABELS = ("15–24", "25–44", "45–64", "65+")
WATER_LABELS = ("0", ">0–2", ">2–4", ">4–6", ">6–10", ">10")
DOMAINS = ("No caries", "Caries", "Total")
NUMERIC_UNITS = {
    "age": "years", "teeth_remaining": "teeth", "decayed_teeth": "teeth",
    "water": "glasses/day", "soda_frequency": "times/day", "juice_frequency": "times/day",
    "fruit_days": "days/week", "vegetable_days": "days/week",
    "fruit_portions": "portions/consumption day", "vegetable_portions": "portions/consumption day",
}
CATEGORY_ORDER = {
    "sex": ("Male", "Female"), "education": ("High", "Intermediate", "Low"),
    "rural": ("Urban", "Rural"), "indigenous": ("No", "Yes"),
    "born_chile": ("Yes", "No"), "age_group": AGE_LABELS, "water_group": WATER_LABELS,
}


def _levels(variable: str, values: pd.Series) -> list[str]:
    """Keep questionnaire categories, including zero-count categories."""
    levels = list(CATEGORY_ORDER.get(variable, dict.fromkeys(CATEGORY_MAPS.get(variable, {}).values())))
    if variable == "dairy_type":
        levels.append("No dairy")
    unknown = set(values.dropna().unique()) - set(levels)
    if unknown:
        raise ValueError(f"Unexpected derived category for {variable}: {sorted(unknown, key=str)}")
    return levels


def descriptive_profile(frame: pd.DataFrame) -> tuple[pd.DataFrame, dict]:
    """Return all 27 predictors plus three explicitly ancillary descriptions.

    Additional variables are ``age_group``, ``decayed_teeth`` (an outcome
    description, never an additional predictor) and ``water_group`` (the
    existing descriptive water cutoffs, never a model recoding).

    Category rows report unweighted ``n`` and survey-weighted column percent
    ``estimate``/``se``/``ci_low``/``ci_high`` on a 0–100 scale. Their
    denominator is nonmissing observations for that variable and outcome
    domain, not the sample in the corresponding predictor category. Mean
    rows use original units, normal-scale t intervals and weighted empirical
    SD, sqrt(sum(w * (x - mean)**2) / sum(w)); SD is not the SE of the mean.
    Every variable/domain also has a count-only missing row. Boundary 0/100%
    proportions have unavailable logit intervals, explicitly flagged as NaN.
    All variances use the complete phase design, including zero-score PSUs.

    The input must retain the complete examination-phase survey frame. This
    function cannot reconstruct PSUs removed by a caller before invocation.
    Returned objects contain aggregate summaries only, without identifiers.
    """
    required = set(PRIMARY_VARIABLES) | {"decayed_teeth", "caries", "eligible", "weight", "stratum", "psu"}
    absent = required - set(frame.columns)
    if absent:
        raise ValueError(f"Missing descriptive-profile columns: {sorted(absent)}")
    if frame.eligible.isna().any() or not frame.eligible.isin([True, False]).all():
        raise ValueError("eligible must contain nonmissing Boolean values")
    weights = pd.to_numeric(frame.weight, errors="raise").to_numpy(dtype=float, na_value=np.nan)
    if np.isinf(weights).any() or (weights[np.isfinite(weights)] < 0).any():
        raise ValueError("Examination weights must be nonnegative and finite or missing")
    phase = np.isfinite(weights) & (weights > 0)
    if np.any(frame.eligible.to_numpy(dtype=bool) & ~phase):
        raise ValueError("Every eligible participant must have a positive examination weight")
    design = frame.loc[phase].reset_index(drop=True).copy()
    if design.empty or design[["stratum", "psu"]].isna().any().any():
        raise ValueError("The full examination design must have nonmissing strata and PSUs")
    eligible = design.eligible.to_numpy(dtype=bool)
    outcome = pd.to_numeric(design.caries, errors="raise").to_numpy(dtype=float, na_value=np.nan)
    if not eligible.any() or not np.isin(outcome[eligible], [0, 1]).all():
        raise ValueError("Eligible dentate participants require observed binary caries outcomes")
    weight = design.weight.to_numpy(dtype=float)
    sizes = design.groupby("stratum", observed=True, sort=False).psu.nunique()
    n_strata, n_psu = len(sizes), int(sizes.sum())
    df = n_psu - n_strata
    if df <= 0:
        raise ValueError("Descriptive confidence intervals require positive PSU-minus-strata df")
    critical = float(t.ppf(.975, df))
    numeric = set(NUMERIC_FEATURES) | {"decayed_teeth"}
    for variable in numeric:
        design[variable] = pd.to_numeric(design[variable], errors="raise").astype(float)
        if np.isinf(design[variable]).any() or design.loc[eligible, variable].lt(0).any():
            raise ValueError(f"Invalid nonfinite or negative derived values in {variable}")
    if design.loc[eligible, "age"].lt(15).any():
        raise ValueError("The eligible ENS target population begins at age 15")
    design["age_group"] = pd.cut(design.age, [14, 24, 44, 64, np.inf], labels=AGE_LABELS)
    design["water_group"] = pd.cut(design.water, [-.1, 0, 2, 4, 6, 10, np.inf], labels=WATER_LABELS)
    variables = [*PRIMARY_VARIABLES, "age_group", "decayed_teeth", "water_group"]
    masks = {"No caries": eligible & (outcome == 0), "Caries": eligible & (outcome == 1), "Total": eligible}
    rows, scores, scored_rows = [], [], []
    for variable in variables:
        values = design[variable]
        is_numeric = variable in numeric
        levels = [] if is_numeric else _levels(variable, values)
        role = "primary" if variable in PRIMARY_VARIABLES else (
            "additional clinical summary" if variable == "decayed_teeth" else "secondary descriptive grouping")
        description = DESCRIPTIONS.get(variable, {
            "age_group": "Age at survey, using the original manuscript age boundaries",
            "water_group": "Reported water intake, using the existing descriptive group boundaries",
        }.get(variable, variable))
        for domain, mask in masks.items():
            observed = mask & values.notna().to_numpy()
            observed_n, domain_n = int(observed.sum()), int(mask.sum())
            missing_n = domain_n - observed_n
            denominator_weight = float(weight[observed].sum())
            base = {
                "domain": domain, "variable": variable, "description": description,
                "variable_role": role, "kind": "numeric" if is_numeric else "categorical",
                "units": NUMERIC_UNITS[variable] if is_numeric else "percent",
                "domain_n": domain_n, "observed_n": observed_n, "missing_n": missing_n,
                "denominator_n": observed_n, "weighted_denominator": denominator_weight,
                "df": df, "confidence": .95,
            }
            for level in (["Mean"] if is_numeric else levels):
                selected = observed if is_numeric else observed & values.eq(level).fillna(False).to_numpy(dtype=bool)
                count = int(selected.sum())
                row = {
                    **base, "row_type": "mean" if is_numeric else "category", "level": level,
                    "n": count, "unweighted_percent": 100 * count / observed_n if observed_n and not is_numeric else np.nan,
                    "unweighted_mean": float(values[observed].mean()) if observed_n and is_numeric else np.nan,
                    "estimate": np.nan, "se": np.nan, "ci_low": np.nan, "ci_high": np.nan,
                    "weighted_sd": np.nan, "ci_method": "linearized_t" if is_numeric else "logit_t",
                    "ci_status": "empty_domain" if not domain_n else "no_observed_values",
                }
                if observed_n:
                    y = values[observed].to_numpy(dtype=float) if is_numeric else selected[observed].astype(float)
                    estimate = float(np.dot(weight[observed], y) / denominator_weight)
                    scale = 1.0 if is_numeric else 100.0
                    influence = np.zeros(len(design))
                    influence[observed] = weight[observed] * (y - estimate) / denominator_weight * scale
                    row["estimate"] = estimate * scale
                    if is_numeric:
                        row["weighted_sd"] = float(np.sqrt(np.dot(weight[observed], (y - estimate)**2) / denominator_weight))
                    row["ci_status"] = "estimated" if is_numeric or 0 < estimate < 1 else "boundary_undefined"
                    scores.append(influence)
                    scored_rows.append(len(rows))
                rows.append(row)
            rows.append({
                **base, "row_type": "missing", "level": "Missing", "units": "count",
                "n": missing_n, "denominator_n": domain_n,
                "weighted_denominator": float(weight[mask].sum()),
                "unweighted_percent": 100 * missing_n / domain_n if domain_n else np.nan,
                "unweighted_mean": np.nan, "estimate": np.nan, "se": np.nan,
                "ci_low": np.nan, "ci_high": np.nan, "weighted_sd": np.nan,
                "ci_method": "not_applicable_missing_count", "ci_status": "not_applicable",
            })
    covariance = design_covariance(np.column_stack(scores), design.stratum, design.psu, singleton="average")
    for position, variance in zip(scored_rows, np.diag(covariance), strict=True):
        row = rows[position]
        row["se"] = float(np.sqrt(max(0.0, variance)))
        if row["ci_status"] == "estimated":
            if row["row_type"] == "mean":
                low, high = row["estimate"] + np.array([-1, 1]) * critical * row["se"]
            else:
                proportion = row["estimate"] / 100
                half = critical * (row["se"] / 100) / (proportion * (1 - proportion))
                low, high = expit(logit(proportion) + np.array([-half, half])) * 100
            row["ci_low"], row["ci_high"] = float(low), float(high)
    table = pd.DataFrame(rows)
    boundary_n = int(table.ci_status.eq("boundary_undefined").sum())
    messages = []
    if boundary_n:
        message = f"{boundary_n} categorical estimates at 0 or 100% have undefined logit-linearized confidence intervals; limits are NaN, not zero-width intervals."
        warnings.warn(message, SurveyWarning, stacklevel=2)
        messages.append(message)
    singleton_n = int(sizes.eq(1).sum())
    if singleton_n:
        messages.append(f"{singleton_n} singleton stratum/strata use the average variance contribution of non-singleton strata; original strata are preserved.")
    metadata = {
        "cohort": "Primary eligible dentate cohort, with no predictor complete-case restriction",
        "input_rows": len(frame), "design_n": len(design), "n_strata": n_strata, "n_psu": n_psu,
        "df": df, "singleton_strata": singleton_n, "singleton_method": "average",
        "domain_counts": {name: int(mask.sum()) for name, mask in masks.items()},
        "primary_predictor_count": len(PRIMARY_VARIABLES), "primary_predictors": list(PRIMARY_VARIABLES),
        "additional_variables": {"age_group": "Secondary descriptive grouping, not an additional predictor",
                                 "decayed_teeth": "Outcome description, never a predictor",
                                 "water_group": "Secondary descriptive grouping, not a recoding used by the models"},
        "age_group_boundaries": [15, 24, 44, 64], "age_group_labels": list(AGE_LABELS),
        "water_group_labels": list(WATER_LABELS), "water_model_units_unchanged": "reported glasses/day",
        "category_denominator": "Observed predictor values within each outcome domain; column composition, not caries prevalence within a predictor category",
        "missing_rows": "Separate raw missing counts; missing-row unweighted percent uses the full outcome-domain denominator",
        "numeric_sd_definition": "Weighted empirical population SD: sqrt(sum(w*(x-weighted_mean)^2)/sum(w)); not an SE or a design-based uncertainty interval",
        "variance_method": "Full examination-design ratio linearization, stratum-centred PSU totals, with-replacement first-stage approximation, no finite-population correction",
        "confidence": .95, "categorical_ci": "Logit-scale t intervals, reported in percent; boundary 0/100% intervals unavailable",
        "numeric_ci": "Original-scale t intervals for the weighted mean; design df includes zero-score PSUs outside the domain",
        "weight_note": "Supplied examination-phase weight remains the previously documented working choice pending confirmation against the original ENS manual",
        "hypothesis_tests": False, "imputation": False, "model_refitting": False,
        "boundary_ci_rows": boundary_n, "warnings": messages,
    }
    return table, metadata
