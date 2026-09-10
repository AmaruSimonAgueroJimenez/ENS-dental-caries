"""Analytical reference cases for the shared survey-inference module."""

import numpy as np
import pandas as pd
import pytest
from scipy.special import expit, logit
from scipy.stats import t

from ens_analysis.survey import (
    SurveyWarning,
    design_covariance,
    survey_glm,
    survey_mean,
)


def test_covariance_totals_people_within_psu_before_centering():
    # PSU totals are 3 and 7, so the variance of their sum is (7-3)^2 = 16.
    covariance = design_covariance([1, 2, 3, 4], [1, 1, 1, 1], [10, 10, 20, 20])
    np.testing.assert_allclose(covariance, [[16]])


def test_repeated_psu_identifiers_are_nested_in_original_strata():
    # For two PSUs per stratum, the contribution is outer(difference, difference).
    scores = [[1, 2], [3, 6], [2, 1], [4, 3]]
    result = design_covariance(scores, [1, 1, 2, 2], [10, 20, 10, 20])
    np.testing.assert_allclose(result, [[8, 12], [12, 20]])
    assert np.linalg.eigvalsh(result).min() >= 0


def test_singleton_average_is_explicit_and_does_not_merge_strata():
    # Ordinary contributions 4 and 16; singleton replacement = average 10.
    with pytest.warns(SurveyWarning, match="1 singleton"):
        result = design_covariance([0, 2, 0, 4, 999], [1, 1, 2, 2, 22], [1, 2, 3, 4, 5])
    np.testing.assert_allclose(result, [[30]])
    with pytest.raises(ValueError, match="singleton"):
        design_covariance([0, 2, 99], [1, 1, 22], [1, 2, 3], singleton="fail")
    with pytest.raises(ValueError, match="at least one non-singleton"):
        design_covariance([0, 1], [1, 2], [1, 2])


def test_mean_reduces_to_srs_sample_variance_over_n_and_t_interval():
    y = np.array([1.0, 2.0, 3.0, 4.0])
    result = survey_mean(y, np.ones(4), [1] * 4, np.arange(4))
    expected_se = np.sqrt(y.var(ddof=1) / len(y))
    assert result["estimate"] == pytest.approx(2.5)
    assert result["se"] == pytest.approx(expected_se)
    assert result["df"] == 3
    assert result["ci_low"] == pytest.approx(2.5 - t.ppf(0.975, 3) * expected_se)
    assert result["ci_high"] == pytest.approx(2.5 + t.ppf(0.975, 3) * expected_se)
    assert not result["is_proportion"]


def test_domain_uses_zero_scores_and_retains_outside_psus():
    result = survey_mean(
        [1, 3, 100, np.nan], [1, 1, 1, 1], [1] * 4, [1, 2, 3, 4],
        domain=[True, True, False, False],
    )
    # Domain influence [-1/2, 1/2, 0, 0], with four PSUs, gives variance 2/3.
    assert result["estimate"] == pytest.approx(2)
    assert result["variance"] == pytest.approx(2 / 3)
    assert result["df"] == 3
    assert result["n"] == 2
    assert result["n_psu"] == 4
    np.testing.assert_array_equal(result["analysis_mask"], [True, True, False, False])


def test_proportion_interval_is_logit_t_and_weight_scale_invariant():
    args = ([0, 1, 0, 1], [1, 2, 3, 4], [1] * 4, [1, 2, 3, 4])
    result = survey_mean(*args)
    scaled = survey_mean(args[0], np.array(args[1]) * 35, args[2], args[3])
    for field in ["estimate", "se", "ci_low", "ci_high"]:
        assert result[field] == pytest.approx(scaled[field])
    p = result["estimate"]
    half = t.ppf(0.975, 3) * result["se"] / (p * (1 - p))
    np.testing.assert_allclose([result["ci_low"], result["ci_high"]], expit([logit(p) - half, logit(p) + half]))
    assert 0 < result["ci_low"] < result["ci_high"] < 1


def test_boundary_proportion_has_no_spurious_precise_logit_interval():
    with pytest.warns(SurveyWarning, match="no finite logit"):
        result = survey_mean([0, 0, 0], [1, 1, 1], [1, 1, 1], [1, 2, 3])
    assert result["estimate"] == 0
    assert np.isnan(result["ci_low"])
    assert np.isnan(result["ci_high"])


def test_intercept_logit_matches_ratio_proportion_in_a_domain():
    frame = pd.DataFrame({
        "y": [0, 1, 1, 0, 1, 0], "w": [1, 2, 3, 4, 5, 6],
        "stratum": [1] * 6, "psu": [1, 2, 3, 4, 5, 6],
    }, index=["duplicate"] * 6)
    domain = [True, True, True, True, False, False]
    mean = survey_mean(frame.y, frame.w, frame.stratum, frame.psu, domain)
    fitted = survey_glm("y ~ 1", frame, "w", "stratum", "psu", domain=domain)
    intercept = fitted["coefficients"].iloc[0]
    p = mean["estimate"]
    assert intercept["coef"] == pytest.approx(logit(p), abs=1e-8)
    assert intercept["se"] == pytest.approx(mean["se"] / (p * (1 - p)))
    np.testing.assert_allclose(
        expit(intercept[["ci_low", "ci_high"]].to_numpy(dtype=float)),
        [mean["ci_low"], mean["ci_high"]],
    )
    assert fitted["df"] == 5
    assert fitted["n"] == 4
    assert fitted["rank"] == 1
    assert fitted["converged"] and fitted["valid_inference"]
    assert fitted["predicted"].index.equals(frame.index)
    assert fitted["predicted"].iloc[4:].isna().all()


def test_intercept_poisson_matches_log_of_ratio_mean():
    frame = pd.DataFrame({
        "y": [0, 1, 2, 5], "w": [1, 2, 3, 2],
        "stratum": [1, 1, 1, 1], "psu": [1, 2, 3, 4],
    })
    mean = survey_mean(frame.y, frame.w, frame.stratum, frame.psu)
    fitted = survey_glm("y ~ 1", frame, "w", "stratum", "psu", family="poisson")
    intercept = fitted["coefficients"].iloc[0]
    assert intercept["coef"] == pytest.approx(np.log(mean["estimate"]), abs=1e-8)
    assert intercept["se"] == pytest.approx(mean["se"] / mean["estimate"])
    expected_p = 2 * t.sf(abs(intercept["coef"] / intercept["se"]), 3)
    assert intercept["p_value"] == pytest.approx(expected_p)


def test_glm_weights_can_be_rescaled_and_missing_rows_retain_design():
    frame = pd.DataFrame({
        "y": [0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1],
        "x": [-1, -1, 0, 0, 1, 1, -1, -1, 0, 0, 1, np.nan],
        "w": [1, 2, 3, 2, 1, 2, 3, 4, 2, 1, 2, 3],
        "stratum": [1] * 6 + [2] * 6,
        "psu": [1, 1, 2, 2, 3, 3] * 2,
    })
    first = survey_glm("y ~ x", frame, "w", "stratum", "psu")
    second = survey_glm("y ~ x", frame.assign(w=frame.w * 1000), "w", "stratum", "psu")
    np.testing.assert_allclose(
        first["coefficients"].drop(columns="term"),
        second["coefficients"].drop(columns="term"),
    )
    assert first["n"] == 11
    assert first["n_psu"] == 6
    assert first["n_strata"] == 2
    assert first["df"] == 4
    assert np.isnan(first["predicted"].iloc[-1])


def test_rank_deficiency_does_not_report_identified_coefficient_inference():
    frame = pd.DataFrame({
        "y": [0, 1, 1, 0, 0, 1, 1, 0], "x": [-1, -1, 0, 0, 1, 1, 2, 2],
        "w": [1] * 8, "stratum": [1] * 8, "psu": range(8),
    })
    frame["duplicate_x"] = 2 * frame.x
    with pytest.warns(SurveyWarning, match="inference is unavailable"):
        result = survey_glm("y ~ x + duplicate_x", frame, "w", "stratum", "psu")
    assert result["rank"] == 2
    assert result["n_parameters"] == 3
    assert not result["valid_inference"]
    assert result["coefficients"]["se"].isna().all()
    assert result["coefficients"]["p_value"].isna().all()


def test_bad_design_and_weights_fail_explicitly():
    with pytest.raises(ValueError, match="no missing strata"):
        design_covariance([1, 2], [1, np.nan], [1, 2])
    with pytest.raises(ValueError, match="nonnegative"):
        survey_mean([0, 1], [1, -1], [1, 1], [1, 2])
    with pytest.raises(ValueError, match="positive PSU-minus-strata"):
        survey_mean([0, 1], [1, 1], [1, 2], [1, 2])
    with pytest.raises(ValueError, match="Boolean"):
        survey_mean([0, 1], [1, 1], [1, 1], [1, 2], domain=[True, None])
