"""Taylor-linearized, with-replacement complex-survey estimates.

PSUs are nested within the supplied *original* strata. No finite-population
correction is applied. Keep all sampled design rows when using ``domain``:
rows outside the analysis have zero influence but remain in PSU totals and
design degrees of freedom. These are design-based associations, not causal
effects. Choice of the appropriate ENS phase weight remains an analyst task.
"""

from __future__ import annotations

import warnings
from typing import Any

import numpy as np
import pandas as pd
import patsy
import statsmodels.api as sm
from scipy.special import expit, logit
from scipy.stats import t
from statsmodels.tools.sm_exceptions import PerfectSeparationWarning


class SurveyWarning(UserWarning):
    """An explicit survey-estimation limitation or variance adjustment."""


def _vector(values: Any, name: str, n: int | None = None) -> np.ndarray:
    array = np.asarray(values)
    if array.ndim != 1 or (n is not None and len(array) != n):
        raise ValueError(f"{name} must be a one-dimensional vector of length {n}.")
    return array


def _numeric(values: Any, name: str, n: int | None = None) -> np.ndarray:
    array = _vector(values, name, n)
    try:
        return pd.to_numeric(pd.Series(array), errors="raise").to_numpy(
            dtype=float, na_value=np.nan
        )
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{name} must contain numeric values or missing values.") from exc


def _domain_mask(domain: Any, n: int) -> np.ndarray:
    if domain is None:
        return np.ones(n, dtype=bool)
    values = _vector(domain, "domain", n)
    if pd.isna(values).any() or not pd.Series(values).isin([True, False]).all():
        raise ValueError("domain must contain only nonmissing Boolean values.")
    return values.astype(bool)


def _design(strata: Any, psu: Any, n: int) -> tuple[list, dict]:
    strata_values = _vector(strata, "strata", n)
    psu_values = _vector(psu, "psu", n)
    if n == 0 or pd.isna(strata_values).any() or pd.isna(psu_values).any():
        raise ValueError("The full design must be nonempty and have no missing strata/PSUs.")
    frame = pd.DataFrame({"stratum": strata_values, "psu": psu_values})
    groups = []
    for label, stratum in frame.groupby("stratum", sort=False, observed=True):
        groups.append(
            (label, [part.index.to_numpy() for _, part in stratum.groupby(
                "psu", sort=False, observed=True
            )])
        )
    n_psu = sum(len(parts) for _, parts in groups)
    return groups, {
        "n_strata": len(groups),
        "n_psu": n_psu,
        "n_singleton_strata": sum(len(parts) == 1 for _, parts in groups),
        "df": n_psu - len(groups),
    }


def design_covariance(
    scores: Any, strata: Any, psu: Any, singleton: str = "average"
) -> np.ndarray:
    """Return covariance of a sum of already-weighted influence/score rows.

    Sum scores within each PSU; center PSU totals within stratum h and use
    ``m_h / (m_h - 1) * sum((U_hi - mean(U_h)) outer itself)``. There is no
    further division by the sample size: callers supply the required ratio
    denominator or GLM bread. The output is always a 2-D square matrix.

    ``singleton='average'`` replaces each singleton-stratum contribution by
    the average contribution of non-singleton strata and emits SurveyWarning.
    ``'fail'`` raises; ``'certainty'`` contributes zero and warns that this
    requires an actual certainty-PSU assumption. No adjustment renames strata.
    """
    if singleton not in {"average", "fail", "certainty"}:
        raise ValueError("singleton must be 'average', 'fail', or 'certainty'.")
    values = np.asarray(scores, dtype=float)
    if values.ndim == 1:
        values = values[:, None]
    if values.ndim != 2 or values.shape[1] == 0 or not np.isfinite(values).all():
        raise ValueError("scores must be a finite numeric vector or matrix.")
    groups, info = _design(strata, psu, len(values))
    contributions = []
    for _, parts in groups:
        m = len(parts)
        if m > 1:
            totals = np.stack([values[rows].sum(axis=0) for rows in parts])
            centered = totals - totals.mean(axis=0)
            contributions.append(m / (m - 1) * (centered.T @ centered))
    covariance = np.zeros((values.shape[1], values.shape[1]))
    if contributions:
        covariance = np.sum(contributions, axis=0)
    lonely = info["n_singleton_strata"]
    if lonely:
        if singleton == "fail":
            raise ValueError(f"The design contains {lonely} singleton stratum/strata.")
        if singleton == "average":
            if not contributions:
                raise ValueError("Singleton averaging needs at least one non-singleton stratum.")
            covariance += lonely * np.mean(contributions, axis=0)
            message = (
                f"{lonely} singleton stratum/strata: variance contribution replaced "
                "by the average of non-singleton strata; original strata preserved."
            )
        else:
            message = (
                f"{lonely} singleton stratum/strata: zero variance contribution under "
                "the explicitly requested certainty-PSU assumption."
            )
        warnings.warn(message, SurveyWarning, stacklevel=2)
    return (covariance + covariance.T) / 2


def _critical(confidence: float, df: int) -> float:
    if not 0 < confidence < 1:
        raise ValueError("confidence must be strictly between zero and one.")
    if df <= 0:
        raise ValueError("Design t inference requires positive PSU-minus-strata degrees of freedom.")
    return float(t.ppf((1 + confidence) / 2, df))


def survey_mean(
    y: Any,
    weights: Any,
    strata: Any,
    psu: Any,
    domain: Any = None,
    *,
    proportion: bool | None = None,
    confidence: float = 0.95,
    singleton: str = "average",
) -> dict:
    """Estimate sum(w*y)/sum(w) with full-design ratio linearization.

    Missing outcomes and missing/zero weights have zero influence. Negative
    or infinite weights raise. ``n`` counts eligible domain observations;
    ``df`` always uses the complete supplied design, including zero-score PSUs.
    If ``proportion`` is omitted, binary outcomes select a logit-scale t CI.
    Other means use a t CI on the original scale. At proportion 0 or 1 a
    finite logit CI is unavailable and is returned as NaN with a warning.
    """
    outcomes = _numeric(y, "y")
    n_full = len(outcomes)
    weight = _numeric(weights, "weights", n_full)
    requested = _domain_mask(domain, n_full)
    _, info = _design(strata, psu, n_full)
    critical = _critical(confidence, info["df"])
    if np.isinf(weight).any() or np.any(weight[np.isfinite(weight)] < 0):
        raise ValueError("weights must be nonnegative and finite, or missing.")
    eligible = requested & np.isfinite(outcomes) & np.isfinite(weight) & (weight > 0)
    if not eligible.any():
        raise ValueError("The requested domain has no observations with outcome and positive weight.")
    weight_sum = float(weight[eligible].sum())
    estimate = float(np.dot(weight[eligible], outcomes[eligible]) / weight_sum)
    influence = np.zeros(n_full)
    influence[eligible] = weight[eligible] * (outcomes[eligible] - estimate) / weight_sum
    variance = float(design_covariance(influence, strata, psu, singleton)[0, 0])
    se = float(np.sqrt(max(0.0, variance)))
    is_binary = bool(np.isin(outcomes[eligible], [0, 1]).all())
    is_proportion = is_binary if proportion is None else bool(proportion)
    if is_proportion and not np.all((outcomes[eligible] >= 0) & (outcomes[eligible] <= 1)):
        raise ValueError("A proportion outcome must lie in [0, 1].")
    if is_proportion:
        if 0 < estimate < 1:
            half_width = critical * se / (estimate * (1 - estimate))
            bounds = expit(logit(estimate) + np.array([-half_width, half_width]))
        else:
            warnings.warn(
                "A proportion at 0 or 1 has no finite logit-linearized confidence interval.",
                SurveyWarning,
                stacklevel=2,
            )
            bounds = [np.nan, np.nan]
    else:
        bounds = [estimate - critical * se, estimate + critical * se]
    return {
        "estimate": estimate,
        "se": se,
        "variance": variance,
        "ci_low": float(bounds[0]),
        "ci_high": float(bounds[1]),
        "confidence": confidence,
        "n": int(eligible.sum()),
        "n_full": n_full,
        "n_domain_requested": int(requested.sum()),
        "weight_sum": weight_sum,
        "is_proportion": is_proportion,
        "analysis_mask": eligible,
        **info,
    }


def survey_glm(
    formula: str,
    frame: pd.DataFrame,
    weight_col: str,
    strata_col: str,
    psu_col: str,
    family: str = "binomial",
    domain: Any = None,
    *,
    confidence: float = 0.95,
    singleton: str = "average",
    maxiter: int = 200,
    tol: float = 1e-9,
) -> dict:
    """Fit a weighted canonical-link GLM and replace model-based inference.

    ``family`` is binomial/logit or Poisson/log. ``formula`` is a Patsy formula
    whose response must produce one numeric column. Weights are normalized
    to mean one among complete domain rows; this scaling cancels in the
    sandwich. Score = normalized_weight * x * (y-mu); bread is the inverse
    expected information. Scores outside those rows are zero in the full
    design, so domain restriction does not discard design PSUs.

    Returns a dict containing a ``coefficients`` DataFrame (term, coef, se,
    ci_low, ci_high, p_value), covariance, design df/counts, rank, converged,
    analysis_mask, the fitted model and Patsy design_info. ``predicted`` and
    ``linear_predictor`` align positionally with frame.index and are NaN
    outside analysis rows. The embedded statsmodels model's *own* covariance,
    p-values, and confidence intervals are not survey-adjusted: use the
    returned table/covariance instead. Coefficient inference is unavailable
    for deficient rank, failed convergence, or detected perfect separation.
    """
    if family not in {"binomial", "poisson"}:
        raise ValueError("family must be 'binomial' or 'poisson'.")
    working = frame.reset_index(drop=True).copy()
    n_full = len(working)
    strata = working[strata_col].to_numpy()
    psu = working[psu_col].to_numpy()
    _, info = _design(strata, psu, n_full)
    critical = _critical(confidence, info["df"])
    weight = _numeric(working[weight_col], "weights", n_full)
    if np.isinf(weight).any() or np.any(weight[np.isfinite(weight)] < 0):
        raise ValueError("weights must be nonnegative and finite, or missing.")
    requested = _domain_mask(domain, n_full)
    candidates = requested & np.isfinite(weight) & (weight > 0)
    if not candidates.any():
        raise ValueError("The requested domain has no observations with positive weight.")
    response, matrix = patsy.dmatrices(
        formula, working.loc[candidates], return_type="dataframe", NA_action="drop"
    )
    if response.shape[1] != 1 or len(matrix) == 0:
        raise ValueError("The formula needs one numeric response and at least one complete row.")
    values = matrix.to_numpy(dtype=float)
    outcomes = response.to_numpy(dtype=float).ravel()
    if not np.isfinite(values).all() or not np.isfinite(outcomes).all():
        raise ValueError("Formula values must be finite after dropping missing observations.")
    if np.any(outcomes < 0) or (family == "binomial" and np.any(outcomes > 1)):
        raise ValueError("Binomial responses must lie in [0,1]; Poisson responses must be nonnegative.")
    rows = matrix.index.to_numpy(dtype=int)
    fit_weight = weight[rows] / weight[rows].mean()
    glm_family = sm.families.Binomial() if family == "binomial" else sm.families.Poisson()
    with warnings.catch_warnings(record=True) as fit_warnings:
        warnings.simplefilter("always")
        model = sm.GLM(outcomes, matrix, family=glm_family, freq_weights=fit_weight)
        fitted = model.fit(maxiter=maxiter, tol=tol)
    separated = any(issubclass(item.category, PerfectSeparationWarning) for item in fit_warnings)
    # Re-emit each distinct fitting warning once, retaining its category.
    seen_warnings = set()
    for item in fit_warnings:
        key = (item.category, str(item.message))
        if key not in seen_warnings:
            warnings.warn(str(item.message), item.category, stacklevel=2)
            seen_warnings.add(key)
    params = np.asarray(fitted.params)
    means = np.asarray(fitted.fittedvalues)
    rank = int(np.linalg.matrix_rank(values))
    converged = bool(fitted.converged)
    variance_function = means * (1 - means) if family == "binomial" else means
    information = values.T @ ((fit_weight * variance_function)[:, None] * values)
    bread = np.linalg.pinv(information)
    scores = np.zeros((n_full, values.shape[1]))
    scores[rows] = values * (fit_weight * (outcomes - means))[:, None]
    meat = design_covariance(scores, strata, psu, singleton=singleton)
    covariance = bread @ meat @ bread.T
    covariance = (covariance + covariance.T) / 2
    valid_inference = rank == values.shape[1] and converged and not separated
    if not valid_inference:
        warnings.warn(
            "Coefficient inference is unavailable: check rank, convergence and perfect_separation.",
            SurveyWarning,
            stacklevel=2,
        )
        covariance[:] = np.nan
    se = np.sqrt(np.maximum(0.0, np.diag(covariance)))
    test_stat = np.divide(params, se, out=np.full_like(params, np.nan), where=se > 0)
    table = pd.DataFrame({
        "term": matrix.columns,
        "coef": params,
        "se": se,
        "ci_low": params - critical * se,
        "ci_high": params + critical * se,
        "p_value": 2 * t.sf(np.abs(test_stat), info["df"]),
    })
    analysis_mask = np.zeros(n_full, dtype=bool)
    analysis_mask[rows] = True
    predicted = np.full(n_full, np.nan)
    predicted[rows] = means
    linear_predictor = np.full(n_full, np.nan)
    linear_predictor[rows] = values @ params
    return {
        "coefficients": table,
        "covariance": covariance,
        "predicted": pd.Series(predicted, index=frame.index, name="predicted"),
        "linear_predictor": pd.Series(linear_predictor, index=frame.index, name="linear_predictor"),
        "analysis_mask": analysis_mask,
        "n": len(rows),
        "n_full": n_full,
        "n_domain_requested": int(requested.sum()),
        "weight_sum": float(weight[rows].sum()),
        "rank": rank,
        "n_parameters": values.shape[1],
        "converged": converged,
        "perfect_separation": separated,
        "valid_inference": valid_inference,
        "family": family,
        "confidence": confidence,
        "formula": formula,
        "model": fitted,
        "design_info": matrix.design_info,
        **info,
    }
