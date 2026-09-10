"""Independent numerical checks of dietary gains and survey descriptions."""

import numpy as np
import pandas as pd
import pytest
from scipy.special import expit, logit
from scipy.stats import t

from ens_analysis import dietary_results as dr
from ens_analysis.expanded_data import ALL_FEATURES, NUMERIC_FEATURES


def direct_metrics(y, p, weight):
    """Closed-form probability losses and weighted case/control AUC."""
    y, p, w = np.asarray(y), np.asarray(p), np.asarray(weight)
    positive, negative = y == 1, y == 0
    pairs = w[positive, None] * w[None, negative]
    order = p[positive, None] - p[None, negative]
    auc = np.sum(pairs * ((order > 0) + 0.5 * (order == 0))) / pairs.sum()
    clipped = np.clip(p, 1e-7, 1 - 1e-7)
    return {"auc": auc, "brier": np.sum(w * (y - p)**2) / w.sum(),
            "log_loss": -np.sum(w * (y * np.log(clipped) + (1-y) * np.log1p(-clipped))) / w.sum()}


@pytest.fixture(scope="module")
def evaluated():
    n = 48
    frame = pd.DataFrame({
        "participant_id": [f"p{i}" for i in range(n)],
        "psu": np.repeat([f"g{i}" for i in range(12)], 4),
        "stratum": np.repeat([f"s{i // 3}" for i in range(12)], 4),
        "caries": np.tile([0, 1, 1, 0], 12),
        "weight": 1 + np.arange(n) % 7 / 3,
        "eligible": np.arange(n) < 40,
        "age": 15 + np.arange(n),
        "sex": np.tile(["Male", "Female", "Male", "Female"], 12),
        "rural": np.tile(["Urban", "Urban", "Rural", "Rural"], 12),
        "education": np.tile(["Low", "High", "Low", "High"], 12),
    }, index=np.arange(1000, 1000 + n))
    cohort = frame.loc[frame.eligible]
    levels = {"basic": .06, "context": .10, "context_diet": .18,
              "context_diet_labels": .14, "symptoms": .12,
              "symptoms_diet": .20, "symptoms_diet_labels": .17}
    strength = {f"{family}__{feature_set}": amount + family_index * .005
                for family_index, family in enumerate(dr.FAMILIES)
                for feature_set, amount in levels.items()}
    strength.update(spline_logistic__context_diet__winsor99=.22,
                    spline_logistic__symptoms_diet__winsor99=.24)
    oof = []
    noise = .24 * np.sin(np.arange(len(cohort)) * 2.1)
    for name, amount in strength.items():
        part = cohort[["participant_id", "psu", "stratum", "caries", "weight"]].rename(columns={"caries": "y"}).copy()
        part.insert(0, "row_index", part.index)
        part["model"] = name
        part["fold"] = np.repeat(np.arange(10) % 5 + 1, 4)
        part["p"] = .5 + amount * (2 * part.y - 1) + noise
        oof.append(part.reset_index(drop=True))
    raw = pd.DataFrame([
        {"model": "spline_logistic__context_diet_labels", "fold": fold,
         "feature": feature, "repeat": repeat, "brier_increase": value,
         "auc_drop": value * 2, "test_weight_sum": weight}
        for fold, weight in [(1, 1.), (2, 3.)]
        for feature, value in [("water", -.01 * fold), ("age", .01 * fold)]
        for repeat in [1, 2]
    ])
    grouped = raw.rename(columns={"feature": "group"}).copy()
    grouped["group"] = grouped["group"].replace({"water": "intake", "age": "context"})
    grouped["n_features"] = grouped.group.map({"intake": 15, "context": 17})
    results = {"oof": pd.concat(oof, ignore_index=True), "importance": raw,
               "grouped_importance": grouped, "manifest": {"models": {name: {} for name in strength}}}
    return frame, results, dr.evaluate_dietary(frame, results, replicates=31)


def test_all_paired_gains_and_percentile_limits_have_correct_sign(evaluated):
    _, results, evaluated = evaluated
    contrasts = evaluated["paired_contrasts"]
    assert len(contrasts) == 75  # 21 primary/secondary pairs plus 4 p99 pairs.
    assert contrasts.role.eq("primary_incremental_contrast").sum() == 1
    primary = contrasts.loc[contrasts.role.eq("primary_incremental_contrast")].iloc[0]
    assert primary.model == "spline_logistic__context_diet"
    assert primary.reference == "spline_logistic__context"
    assert primary.metric == "brier" and primary.difference > 0
    assert {"diet_context_winsor", "diet_symptoms_winsor"}.issubset(contrasts.contrast_id)
    direct = {}
    for model, block in results["oof"].groupby("model"):
        direct[model] = direct_metrics(block.y, block.p, block.weight)
    for row in contrasts.itertuples():
        direction = 1 if row.metric == "auc" else -1
        raw = direct[row.model][row.metric] - direct[row.reference][row.metric]
        assert row.difference == pytest.approx(direction * raw, abs=1e-14)
        assert row.unoriented_difference == pytest.approx(raw, abs=1e-14)
        column = {"auc": 0, "brier": 1, "log_loss": 2}[row.metric]
        draws = direction * (evaluated["_bootstrap"][row.model][:, column]
                             - evaluated["_bootstrap"][row.reference][:, column])
        bounds = np.quantile(draws, [.025, .975])
        np.testing.assert_allclose([row.ci_low, row.ci_high], bounds, rtol=0, atol=1e-14)
        assert row.ci_low <= row.ci_high
        if row.metric != "auc":
            assert row.relative_loss_reduction_percent == pytest.approx(100 * direction * raw / direct[row.reference][row.metric])
        else:
            assert np.isnan(row.relative_loss_reduction_percent)
    # The synthetic label extension deliberately worsens probability accuracy:
    # a loss is retained as negative, never clipped or given the wrong sign.
    negative = contrasts.query("contrast_id == 'labels_context' and metric == 'brier'")
    assert negative.difference.lt(0).all()


def test_prevalence_is_training_only_and_group_api_normalizes_intake(evaluated):
    _, results, evaluated = evaluated
    wide = evaluated["_wide"]
    for fold, heldout in wide.groupby("fold"):
        training = wide.fold.ne(fold)
        expected = np.average(wide.loc[training, "y"], weights=wide.loc[training, "weight"])
        np.testing.assert_allclose(heldout.prevalence_only, expected)
    assert len(wide) == 40
    baseline = evaluated["model_performance"].query("model == 'prevalence_only'")
    assert baseline.calibration_slope.isna().all()
    grouped = evaluated["grouped_importance_stability"]
    assert set(grouped.block) == {"context", "diet"}
    assert "group" not in grouped
    diet = grouped.set_index("block").loc["diet"]
    assert diet.brier_increase == pytest.approx(-.0175)
    assert diet.minimum_fold_brier_increase == pytest.approx(-.02)
    assert diet.maximum_fold_brier_increase == pytest.approx(-.01)
    assert diet.median_rank == 2
    # Evaluation does not mutate the private raw engine output.
    assert set(results["grouped_importance"].group) == {"context", "intake"}


def test_importance_averages_repeats_before_weighting_folds():
    raw = pd.DataFrame([
        {"model": "m", "fold": 1, "feature": "x", "brier_increase": .1, "auc_drop": .2, "test_weight_sum": 1},
        {"model": "m", "fold": 1, "feature": "x", "brier_increase": .3, "auc_drop": .4, "test_weight_sum": 1},
        {"model": "m", "fold": 2, "feature": "x", "brier_increase": -.1, "auc_drop": -.2, "test_weight_sum": 3},
    ])
    summary = dr.summarize_importance(raw).iloc[0]
    assert summary.brier_increase == pytest.approx((.2 - .3) / 4)
    assert summary.auc_drop == pytest.approx((.3 - .6) / 4)
    assert summary.minimum_fold_brier_increase == -.1
    assert summary.maximum_fold_brier_increase == pytest.approx(.2)


def test_profile_full_design_denominators_income_missing_and_boundary_ci():
    frame = pd.DataFrame({
        "weight": [1., 2., 1., 1., 3., 1., 2., 2.],
        "stratum": ["a"] * 4 + ["b"] * 4,
        "psu": np.repeat(["p1", "p2", "p3", "p4"], 2),
        "eligible": [True, True, True, False, True, True, False, False],
        "caries": [0, 1, 0, 0, 1, 0, 0, 0],
        "soda_glasses_daily": [0., 2., np.nan, 999., 10., 4., 999., 999.],
        "income_band": ["Low", "High", np.nan, "High", "Low", "None", "High", "High"],
    })
    dictionary = pd.DataFrame({"variable": ["soda_glasses_daily", "income_band"],
                               "description": ["Soda quantity", "Income bands"],
                               "units": ["glasses/day", "category"]})
    profile, metadata = dr.expanded_profile(frame, dictionary, {"soda_glasses_daily"},
                                            ["soda_glasses_daily", "income_band"])
    assert metadata["design_n"] == 8
    assert metadata["n_psu"] == 4 and metadata["n_strata"] == 2 and metadata["df"] == 2
    assert metadata["domain_counts"] == {"No caries": 3, "Caries": 2, "Total": 5}
    mean = profile.query("domain == 'Total' and variable == 'soda_glasses_daily' and row_type == 'mean'").iloc[0]
    assert mean.observed_n == 4 and mean.missing_n == 1 and mean.domain_n == 5
    assert mean.estimate == pytest.approx(38 / 7)  # Original, unlogged scale.
    # PSU influence totals are [-86/49, 0, +86/49, 0]. The two
    # out-of-domain-only PSU totals remain in the full-design centring.
    expected_se = np.sqrt(2) * 86 / 49
    assert mean.se == pytest.approx(expected_se)
    np.testing.assert_allclose([mean.ci_low, mean.ci_high],
                               38 / 7 + np.array([-1, 1]) * t.ppf(.975, 2) * expected_se)
    categories = profile.query("domain == 'Total' and variable == 'income_band' and row_type == 'category'").set_index("level")
    assert set(categories.index) == {"Low", "High", "None"}
    assert categories.kind.eq("categorical").all()
    assert categories.observed_n.eq(4).all() and categories.missing_n.eq(1).all()
    assert categories["n"].sum() == 4 and categories.estimate.sum() == pytest.approx(100)
    assert categories.loc["Low", "estimate"] == pytest.approx(100 * 4 / 7)
    assert categories.loc["High", "estimate"] == pytest.approx(100 * 2 / 7)
    assert categories.loc["None", "estimate"] == pytest.approx(100 / 7)
    p, cat_se = 4 / 7, np.sqrt(2) * 500 / 49
    expected = 100 * expit(logit(p) + np.array([-1, 1]) * t.ppf(.975, 2) * cat_se / 100 / (p * (1-p)))
    np.testing.assert_allclose(categories.loc["Low", ["ci_low", "ci_high"]].to_numpy(float), expected)
    missing = profile.query("domain == 'Total' and variable == 'income_band' and row_type == 'missing'").iloc[0]
    assert missing.n == 1 and np.isnan(missing.estimate)
    boundary = profile.query("domain == 'Caries' and variable == 'income_band' and level == 'None'").iloc[0]
    assert boundary.estimate == 0 and np.isnan(boundary.ci_low) and np.isnan(boundary.ci_high)


def test_profile_covers_all_44_predictors_without_numeric_income():
    n = 24
    frame = pd.DataFrame({"weight": np.ones(n), "stratum": np.repeat(["a", "b"], 12),
                          "psu": np.repeat(np.arange(12), 2), "eligible": True,
                          "caries": np.tile([0, 1], 12)})
    for feature in ALL_FEATURES:
        frame[feature] = (np.arange(n, dtype=float) if feature in NUMERIC_FEATURES
                          else np.tile(["Yes", "No"], 12).astype(object))
    frame["income_band"] = np.asarray([f"Band {i % 11 + 1}" for i in range(n)], dtype=object)
    frame.loc[23, "income_band"] = np.nan
    dictionary = pd.DataFrame({"variable": ALL_FEATURES, "description": ALL_FEATURES,
                               "units": ["numeric unit" if f in NUMERIC_FEATURES else "category" for f in ALL_FEATURES]})
    profile, metadata = dr.expanded_profile(frame, dictionary, NUMERIC_FEATURES, ALL_FEATURES)
    assert metadata["predictors"] == 44 and set(profile.variable) == set(ALL_FEATURES)
    assert profile.loc[profile.variable.eq("income_band"), "kind"].eq("categorical").all()
    income = profile.query("variable == 'income_band' and domain == 'Total' and row_type == 'category'")
    assert len(income) == 11 and income.observed_n.eq(23).all()
    assert income.estimate.sum() == pytest.approx(100)
    for volume in ["soda_glasses_daily", "juice_glasses_daily"]:
        assert profile.query("variable == @volume and row_type == 'mean'").shape[0] == 3
