"""Historical coding comparisons must preserve cohort, pairing and source state."""
from copy import deepcopy
import json

import numpy as np
import pandas as pd
import pytest

from ens_analysis import evaluation, historical_comparison as comparison, models


def test_joint_predictor_permutation_preserves_indicator_combinations_and_pairing():
    modern = pd.DataFrame({"water": [0., 1., 2., 0., 1., 2., 0., 1.],
                           "category": ["A", "B", "C", "A", "B", "C", "A", "B"]})
    historical = pd.DataFrame({"water_old": modern.water,
                               "is_B": modern.category.eq("B").astype(float),
                               "is_C": modern.category.eq("C").astype(float)})
    supplied_rows = []

    class Predictor:
        def __init__(self, historical=False):
            self.historical = historical

        def predict_proba(self, frame):
            if self.historical:
                assert set(map(tuple, frame[["is_B", "is_C"]].to_numpy())) <= {(0, 0), (1, 0), (0, 1)}
                water = frame.water_old.to_numpy()
                category = frame.is_B.to_numpy() + 2 * frame.is_C.to_numpy()
                supplied_rows.append(frame.copy())
            else:
                water = frame.water.to_numpy()
                category = frame.category.map({"A": 0, "B": 1, "C": 2}).to_numpy()
            p = .1 + .15 * water + .1 * category
            return np.column_stack([1 - p, p])

    y, w = np.array([0, 0, 1, 0, 1, 1, 0, 1]), np.arange(1., 9.)
    a, b = Predictor(), Predictor(True)
    pa, pb = models._probabilities(a, modern), models._probabilities(b, historical)
    before = historical.copy(deep=True)
    left = comparison._permutations(a, modern, y, w, pa,
        {"water": ["water"], "category": ["category"]}, "same", 2, 99)
    right = comparison._permutations(b, historical, y, w, pb,
        {"water": ["water_old"], "category": ["is_B", "is_C"]}, "same", 2, 99)
    for m, h in zip(left, right, strict=True):
        assert m["brier_increase"] == pytest.approx(h["brier_increase"])
        assert m["auc_drop"] == pytest.approx(h["auc_drop"])
    pd.testing.assert_frame_equal(before, historical)
    assert len(supplied_rows) == 1 + 2 * comparison.PERMUTATION_REPEATS


def test_mdi_mapping_sums_actual_modern_columns_and_retains_constants():
    x = pd.DataFrame({"age": np.tile([20., 40., 60., 80.], 12),
                      "water": np.tile([0., 1., 2., 3.], 12),
                      "sex": np.tile(["Female", "Male"], 24)})
    y = (x.age > 40).to_numpy(int)
    spec = {"kind": "random_forest", "features": list(x), "weighted_training": True}
    fitted = models._fit_estimator(spec, {"max_features": 1., "min_samples_leaf": 2}, x, y,
                                   np.linspace(1., 3., len(x)), np.arange(len(x)), 32, 1)
    columns, groups = comparison._mdi_records(fitted, comparison.MODERN, None, list(x), 1, x, 10.)
    columns, groups = pd.DataFrame(columns), pd.DataFrame(groups)
    assert len(columns) == 6  # two numeric, two explicit missing, two sex indicators
    assert set(columns.feature) == set(x)
    assert columns.training_constant.sum() == 2
    assert columns.loc[columns.training_constant, "mdi"].eq(0).all()
    assert columns.mdi.sum() == pytest.approx(1.)
    assert groups.mdi.sum() == pytest.approx(1.)
    for feature, group in columns.groupby("feature"):
        assert groups.set_index("feature").loc[feature, "mdi"] == pytest.approx(group.mdi.sum())
    np.testing.assert_array_equal(comparison._scale_100([0., 0., 0.]), [0., 0., 0.])


@pytest.fixture
def completed(tmp_path, monkeypatch):
    rng = np.random.default_rng(143)
    n = 80
    frame = pd.DataFrame({
        "participant_id": [f"p{i}" for i in range(n)],
        "psu": [f"u{i//4}" for i in range(n)], "stratum": "s1",
        "caries": np.tile([0, 1, 1, 0], n//4), "weight": rng.uniform(.5, 2., n),
        "age": rng.uniform(20, 80, n), "water": rng.choice([0., 1., 2., 18.], n),
        "sex": np.tile(["Female", "Male"], n//2),
        "eligible": np.arange(n) < 72, "paper_complete": np.arange(n) < 64,
    }, index=np.arange(n) + 1000)
    # Deliberately incomplete rows remain in original training folds, but are
    # excluded from BOTH new complete-cohort fits.
    frame.loc[frame.index[64:68], "water"] = np.nan
    specs = {"random_forest": {"kind": "random_forest", "features": ["age", "water", "sex"],
        "candidates": [{"max_features": 1.0, "min_samples_leaf": 2}],
        "weighted_training": True, "role": "algorithm_comparison"}}
    monkeypatch.setattr(models, "_model_specs", lambda: specs)
    results = models.run_nested_models(frame.loc[frame.eligible], tmp_path / "primary", seed=13,
        outer_folds=2, inner_folds=2, n_jobs=1, model_names=["random_forest"], permutation_repeats=0)
    cohort = frame.loc[frame.paper_complete].set_index("participant_id")
    x = pd.DataFrame({"Age": cohort.age, "water_index": cohort.water.map({0.: 1., 1.: 2., 2.: 3., 18.: 4.}),
                      "sexFemale": cohort.sex.eq("Female").astype(float)})
    mapping = pd.DataFrame({"encoded_column": list(x), "predictor": ["age", "water", "sex"],
                            "kind": ["numeric", "numeric", "indicator"]})
    return frame, results, x.iloc[::-1], mapping, tmp_path / "comparison"


def test_same_complete_cohort_folds_pairing_privacy_and_cache_resume(completed, monkeypatch):
    frame, results, x, mapping, cache = completed
    before_frame, before_x, before_oof = frame.copy(deep=True), x.copy(deep=True), results["oof"].copy(deep=True)
    calls, actual = [], comparison._fit_variant

    def counted(name, spec, params, values, y, weight, psu, seed, n_jobs):
        calls.append((name, values.index.tolist(), seed, dict(params)))
        assert set(values.index) <= set(frame.index[frame.paper_complete])
        np.testing.assert_array_equal(weight, frame.loc[values.index, "weight"])
        return actual(name, spec, params, values, y, weight, psu, seed, n_jobs)

    monkeypatch.setattr(comparison, "_fit_variant", counted)
    run = comparison.run_historical_comparison(frame, results, x, mapping, cache,
                                                bootstrap_replicates=20, n_jobs=1)
    assert len(calls) == 4
    for fold in [1, 2]:
        a, b = [call for call in calls if call[2] == 13 + fold * 1000]
        assert a[1:] == b[1:]
    manifest = run["manifest"]
    assert manifest["n_original_eligible"] == 72 and manifest["n_participants"] == 64
    assert manifest["n_full_design_psu"] == 20 and manifest["n_psu"] == 16
    assert manifest["new_hyperparameter_searches"] == 0
    assert manifest["outer_fits_this_call"] == 4
    assert len(run["importance27"]) == 2 * 2 * 3 * 5
    assert len(run["mdi27"]) == 2 * 2 * 3
    assert len(run["mdi27_summary"]) == len(run["importance27_summary"]) == 6
    assert len(run["oof"]) == 128
    assert run["oof"].groupby(["model", "participant_id"]).size().eq(1).all()
    original_assignment = results["oof"].set_index("participant_id").fold
    assert all(row.fold == original_assignment.loc[row.participant_id] for row in run["oof"].itertuples())
    assert run["foldscores"].psu_overlap.eq(0).all()
    # Re-evaluate shared full-design draws to verify the returned paired bounds.
    points, draws, _ = evaluation.evaluate_oof(run["oof"], frame[["stratum", "psu"]], 20, 13)
    expected = evaluation.paired_differences(draws, points,
        [(comparison.HISTORICAL, comparison.MODERN, "Historical coding minus modern coding, same complete cohort")])
    pd.testing.assert_frame_equal(run["comparisons"][expected.columns], expected)
    assert all(v == 20 for metrics in manifest["bootstrap_valid_replicates"].values()
               for k, v in metrics.items() if k in ["auc", "brier", "log_loss"])
    forbidden = {"participant_id", "row_index", "psu", "stratum", "y", "p", "weight"}
    for key in ["performance", "comparisons", "foldscores", "importance27", "importance27_summary",
                "mdi_columns", "mdi27", "mdi27_summary"]:
        assert not forbidden.intersection(run[key].columns)
    # Different incoming DTA row order must align to IDs and reuse every cache.
    resumed = comparison.run_historical_comparison(frame, results, x.iloc[::-1], mapping, cache,
                                                    bootstrap_replicates=20, n_jobs=1)
    assert len(calls) == 4 and resumed["manifest"]["cache_hits_this_call"] == 4
    for key in ["performance", "comparisons", "oof", "mdi27_summary", "importance27"]:
        pd.testing.assert_frame_equal(run[key], resumed[key])
    path = cache / comparison.HISTORICAL / "outer_1.json"
    payload = json.loads(path.read_text())
    payload["probabilities"][0] = .999
    path.write_text(json.dumps(payload))
    repaired = comparison.run_historical_comparison(frame, results, x, mapping, cache,
                                                     bootstrap_replicates=20, n_jobs=1)
    assert len(calls) == 5 and repaired["manifest"]["outer_fits_this_call"] == 1
    pd.testing.assert_frame_equal(repaired["oof"], run["oof"])
    pd.testing.assert_frame_equal(before_frame, frame)
    pd.testing.assert_frame_equal(before_x, x)
    pd.testing.assert_frame_equal(before_oof, results["oof"])


def test_invalid_source_fold_and_matrix_fail_before_fitting(completed, monkeypatch):
    frame, results, x, mapping, cache = completed
    monkeypatch.setattr(comparison, "_fit_variant", lambda *a, **k: pytest.fail("Fit before validation"))
    changed = frame.copy()
    changed.loc[changed.index[0], "weight"] *= 2
    with pytest.raises(ValueError, match="fingerprint"):
        comparison.run_historical_comparison(changed, results, x, mapping, cache, bootstrap_replicates=5)
    changed_result = deepcopy(results)
    changed_result["oof"].loc[0, "fold"] = 99
    with pytest.raises(ValueError, match="fold assignments"):
        comparison.run_historical_comparison(frame, changed_result, x, mapping, cache, bootstrap_replicates=5)
    changed_result = deepcopy(results)
    changed_result["oof"].loc[0, "p"] = 1 - changed_result["oof"].loc[0, "p"]
    # Avoid the accidental invariant p=.5 when deliberately corrupting input.
    changed_result["oof"].loc[0, "p"] = .999
    with pytest.raises(ValueError, match="metrics disagree"):
        comparison.run_historical_comparison(frame, changed_result, x, mapping, cache, bootstrap_replicates=5)
    with pytest.raises(ValueError, match="participant IDs"):
        comparison.run_historical_comparison(frame, results, x.iloc[:-1], mapping, cache, bootstrap_replicates=5)
    with pytest.raises(ValueError, match="every encoded column"):
        comparison.run_historical_comparison(frame, results, x, mapping.iloc[:-1], cache, bootstrap_replicates=5)


def test_mdi_summary_weights_folds_and_does_not_call_fold_range_a_ci():
    raw = pd.DataFrame({"model": ["m"] * 4, "feature": ["a", "b", "a", "b"],
                        "fold": [1, 1, 2, 2], "mdi": [.8, .2, .4, .6],
                        "rank": [1., 2., 2., 1.], "test_weight_sum": [1., 1., 3., 3.],
                        "n_encoded_columns": [1, 2, 1, 3]})
    result = comparison._summarize_mdi(raw).set_index("feature")
    assert result.loc["a", "mdi_mean"] == pytest.approx(.5)
    assert result.loc["b", "mdi_mean"] == pytest.approx(.5)
    assert result.loc["a", "minimum_fold_mdi"] == .4
    assert result.loc["b", "min_encoded_columns"] == 2
    assert result.loc["b", "max_encoded_columns"] == 3
    assert not any("ci" in col.lower() for col in result.columns)
