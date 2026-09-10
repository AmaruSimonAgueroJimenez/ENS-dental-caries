"""Known survey totals, outcome domains, and observed denominators."""
from pathlib import Path

import numpy as np
import pandas as pd
import pytest
from scipy.special import expit, logit
from scipy.stats import t

from ens_analysis.data import CATEGORY_MAPS, NUMERIC_FEATURES, load_caries_data
from ens_analysis.descriptive_profile import PRIMARY_VARIABLES, descriptive_profile
from ens_analysis.survey import SurveyWarning


def synthetic_frame():
    frame = pd.DataFrame({"eligible": [True] * 8 + [False] * 2,
                          "caries": [0.] * 4 + [1.] * 4 + [0., 0.],
                          "weight": np.ones(10), "stratum": [1] * 10,
                          "psu": list(range(10)), "decayed_teeth": [0.] * 4 + [1.] * 4 + [0., 0.]})
    for variable in PRIMARY_VARIABLES:
        frame[variable] = 1.0 if variable in NUMERIC_FEATURES else next(iter(CATEGORY_MAPS[variable].values()))
    frame["age"] = 35.
    frame["teeth_remaining"] = 20.
    frame["water"] = [1., 2., 3., np.nan, 2., 4., 6., 8., 50., 50.]
    frame["sex"] = ["Male", "Female"] * 5
    frame["education"] = ["High", "Low", "Intermediate", "High", "Low", "High", np.nan, "Intermediate", "Low", "Low"]
    return frame


def select(table, variable, domain, level="Mean", row_type="mean"):
    found = table.loc[table.variable.eq(variable) & table.domain.eq(domain) & table.level.eq(level) & table.row_type.eq(row_type)]
    assert len(found) == 1
    return found.iloc[0]


def test_srs_mean_keeps_outside_domain_psus_and_observed_denominator():
    frame = synthetic_frame()
    original = frame.copy(deep=True)
    with pytest.warns(SurveyWarning, match="undefined logit"):
        table, meta = descriptive_profile(frame)
    pd.testing.assert_frame_equal(frame, original)
    row = select(table, "water", "No caries")
    # Three observed values [1,2,3]. Seven rows have zero domain influence.
    variance = 10 / 9 * (1 + 0 + 1) / 3**2
    assert row.estimate == pytest.approx(2.)
    assert row.se**2 == pytest.approx(variance)
    assert row.weighted_sd == pytest.approx(np.sqrt(2 / 3))
    assert row.ci_low == pytest.approx(2 - t.ppf(.975, 9) * np.sqrt(variance))
    assert (row.domain_n, row.observed_n, row.missing_n, row.n) == (4, 3, 1, 3)
    assert row.df == 9 and meta["design_n"] == 10
    assert meta["domain_counts"] == {"No caries": 4, "Caries": 4, "Total": 8}
    assert meta["primary_predictor_count"] == 27
    assert set(meta["primary_predictors"]) == set(PRIMARY_VARIABLES)
    assert "decayed_teeth" not in meta["primary_predictors"]
    missing = select(table, "water", "No caries", "Missing", "missing")
    assert missing.n == 1 and missing.unweighted_percent == 25
    assert np.isnan(missing.estimate)


def test_weighted_column_percentage_matches_known_ratio_and_logit_interval():
    frame = synthetic_frame()
    frame["weight"] = np.arange(1., 11.)
    with pytest.warns(SurveyWarning):
        table, _ = descriptive_profile(frame)
    row = select(table, "sex", "No caries", "Male", "category")
    # Male weights 1+3, all observed outcome-domain weights 1+2+3+4.
    p = 4 / 10
    score = np.array([1, 2, 3, 4]) * (np.array([1, 0, 1, 0]) - p) / 10
    variance = 10 / 9 * np.square(score).sum()
    half = t.ppf(.975, 9) * np.sqrt(variance) / (p * (1 - p))
    assert row.estimate == pytest.approx(40.)
    assert row.unweighted_percent == 50.
    assert row.se == pytest.approx(100 * np.sqrt(variance))
    assert row.ci_low == pytest.approx(100 * expit(logit(p) - half))
    assert row.ci_high == pytest.approx(100 * expit(logit(p) + half))
    assert row.weighted_denominator == 10
    # Education missingness affects only its own observed denominator.
    edu = table.loc[table.variable.eq("education") & table.domain.eq("Caries") & table.row_type.eq("category")]
    assert edu.n.sum() == 3 and edu.estimate.sum() == pytest.approx(100.)
    assert (edu.observed_n == 3).all() and (edu.missing_n == 1).all()


def test_clustered_variance_uses_psu_totals_not_individual_rows():
    frame = synthetic_frame()
    frame["psu"] = [1, 1, 2, 2, 3, 3, 1, 1, 2, 2]
    frame["stratum"] = [1] * 6 + [2] * 4
    with pytest.warns(SurveyWarning):
        table, meta = descriptive_profile(frame)
    row = select(table, "water", "No caries")
    # Stratum 1 PSU scores are [-1/3, 1/3, 0]; stratum 2 scores are both 0.
    expected_variance = 3 / 2 * (2 / 9)
    assert row.se**2 == pytest.approx(expected_variance)
    assert row.df == 3 and meta["n_psu"] == 5 and meta["n_strata"] == 2


def test_boundary_and_missing_values_remain_explicit_and_invalid_design_fails():
    frame = synthetic_frame()
    with pytest.warns(SurveyWarning):
        table, meta = descriptive_profile(frame)
    zero = select(table, "oil", "Total", "Other", "category")
    assert zero.n == 0 and zero.estimate == 0
    assert zero.ci_status == "boundary_undefined" and np.isnan(zero.ci_high)
    assert meta["boundary_ci_rows"] > 0
    invalid = frame.copy(); invalid.loc[0, "weight"] = np.nan
    with pytest.raises(ValueError, match="Every eligible participant"):
        descriptive_profile(invalid)
    invalid = frame.copy(); invalid.loc[0, "caries"] = np.nan
    with pytest.raises(ValueError, match="observed binary"):
        descriptive_profile(invalid)


def test_real_cohort_category_partitions_and_missing_counts():
    source = Path(__file__).resolve().parents[1] / "data" / "data.sav"
    if not source.exists():
        pytest.skip("Private ENS source is not distributed with the repository")
    frame, _, _ = load_caries_data(source)
    with pytest.warns(SurveyWarning):
        table, meta = descriptive_profile(frame)
    assert meta["design_n"] == 5520 and meta["n_psu"] == 1075 and meta["df"] == 1045
    assert meta["singleton_strata"] == 1
    assert meta["domain_counts"] == {"No caries": 2347, "Caries": 2689, "Total": 5036}
    for (variable, domain), block in table.query("row_type == 'category'").groupby(["variable", "domain"]):
        assert block.n.sum() == block.observed_n.iloc[0], (variable, domain)
        assert block.estimate.sum() == pytest.approx(100.), (variable, domain)
    for domain, observed, missing in [("Total", 5032, 4), ("No caries", 2346, 1), ("Caries", 2686, 3)]:
        row = select(table, "water", domain)
        assert (row.observed_n, row.missing_n) == (observed, missing)
    edu = select(table, "education", "Total", "High", "category")
    assert edu.observed_n == 4998 and edu.missing_n == 38
    zero_decay = select(table, "decayed_teeth", "No caries")
    assert zero_decay.estimate == zero_decay.ci_low == zero_decay.ci_high == 0
    assert zero_decay.ci_status == "estimated"
    assert not {"participant_id", "psu", "stratum"}.intersection(table.columns)
