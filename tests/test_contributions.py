"""Contribution checks focus on joint permutation and reconstruction boundaries."""
import copy

import numpy as np
import pandas as pd
import pytest

from ens_analysis import contributions as c, models
from ens_analysis.data import SOCIO_FEATURES, DIET_FEATURES


def test_groups_cover_all_predictors_without_overlap():
    c.validate_groups(SOCIO_FEATURES + DIET_FEATURES, c.PREDICTOR_GROUPS)
    for groups in [{"a": ["age", "age"]}, {"a": ["age"]}, {"a": []}]:
        with pytest.raises(ValueError):
            c.validate_groups(["age", "water"], groups)


def test_joint_permutation_preserves_within_group_values_and_input():
    x = pd.DataFrame({"a": np.arange(20.), "b": np.arange(20.) * 2, "z": np.arange(20.) % 2})
    original = x.copy(deep=True)
    visited = []

    class RecordingModel:
        def predict_proba(self, supplied):
            np.testing.assert_array_equal(supplied.b, supplied.a * 2)
            visited.append(supplied.copy())
            p = np.where(supplied.a > 9, .8, .2)
            return np.column_stack([1 - p, p])

    y = (x.a > 9).to_numpy(int)
    p = np.where(y, .8, .2)
    rows = c.grouped_permutation(RecordingModel(), x, y, np.arange(1., 21.), p,
                                {"pair": ["a", "b"], "other": ["z"]}, "model", 1, 2, 2026)
    pd.testing.assert_frame_equal(x, original)
    pd.testing.assert_series_equal(visited[0].z, x.z)
    np.testing.assert_array_equal(visited[-1][["a", "b"]], x[["a", "b"]])
    assert rows[0]["brier_increase"] > 0
    assert rows[-1]["brier_increase"] == pytest.approx(0)
    assert not {"participant_id", "y", "p", "weight"}.intersection(rows[0])


def test_stability_summary_keeps_negative_importance_and_uses_fold_weights():
    rows = pd.DataFrame([
        dict(model="m", block="g", fold=1, repeat=1, n_features=2,
             brier_increase=-.1, auc_drop=-.2, test_weight_sum=1),
        dict(model="m", block="g", fold=1, repeat=2, n_features=2,
             brier_increase=-.3, auc_drop=-.4, test_weight_sum=1),
        dict(model="m", block="g", fold=2, repeat=1, n_features=2,
             brier_increase=.1, auc_drop=.2, test_weight_sum=3),
    ])
    summary = c.summarize_grouped_importance(rows).iloc[0]
    assert summary.brier_increase == pytest.approx(.025)
    assert summary.auc_drop == pytest.approx(.075)
    assert summary.minimum_fold_brier_increase == pytest.approx(-.2)
    assert summary.maximum_fold_brier_increase == pytest.approx(.1)


@pytest.fixture
def completed(monkeypatch, tmp_path):
    rng = np.random.default_rng(77)
    n = 120
    frame = pd.DataFrame({
        "participant_id": np.arange(n) + 1000,
        "psu": np.repeat(np.arange(30), 4), "stratum": np.repeat(np.arange(30) % 3, 4),
        "caries": np.tile([0, 1, 1, 0], 30), "weight": rng.uniform(.5, 3, n),
        "age": rng.uniform(15, 85, n), "water": rng.integers(0, 12, n).astype(float),
        "sex": np.where(np.arange(n) % 2, "Female", "Male"),
    }, index=np.arange(n) + 2000)
    specs = {"logistic": {"kind": "logistic", "features": ["age", "water", "sex"],
                           "candidates": [{"C": .1}], "weighted_training": True,
                           "role": "algorithm_comparison"}}
    monkeypatch.setattr(models, "_model_specs", lambda: specs)
    result = models.run_nested_models(frame, tmp_path, seed=8, outer_folds=2, inner_folds=2,
                                     n_jobs=1, model_names=["logistic"], permutation_repeats=2)
    return frame, result, {"context": ["age", "sex"], "beverage": ["water"]}


def test_reconstruction_matches_saved_oof_and_exports_only_aggregates(completed):
    frame, result, groups = completed
    raw, summary, manifest = c.predictor_contributions(frame, result, 1, groups, ["logistic"])
    assert len(raw) == 2 * 2 * 2 and len(summary) == 2
    assert len(manifest["refit_checks"]) == 2
    assert max(x["max_absolute_probability_difference"] for x in manifest["refit_checks"]) < 1e-10
    assert manifest["model_fingerprints"]["logistic"] == result["manifest"]["models"]["logistic"]["fingerprint"]
    assert not {"participant_id", "row_index", "psu", "y", "p"}.intersection(raw.columns)


def test_reconstruction_rejects_changed_predictions(completed):
    frame, result, groups = completed
    bad = copy.deepcopy(result)
    bad["oof"].loc[0, "p"] += .01
    with pytest.raises(ValueError, match="predictions differ"):
        c.predictor_contributions(frame, bad, 1, groups, ["logistic"])


def test_reconstruction_rejects_changed_source_and_folds(completed):
    frame, result, groups = completed
    bad_frame = frame.copy()
    bad_frame.loc[bad_frame.index[0], "weight"] *= 2
    with pytest.raises(ValueError, match="fingerprint"):
        c.predictor_contributions(bad_frame, result, 1, groups, ["logistic"])
    bad_result = copy.deepcopy(result)
    bad_result["oof"].loc[0, "fold"] = 999
    with pytest.raises(ValueError, match="folds changed"):
        c.predictor_contributions(frame, bad_result, 1, groups, ["logistic"])
