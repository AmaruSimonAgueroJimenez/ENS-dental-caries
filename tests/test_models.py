"""Checks of leakage boundaries, grouped calibration, and resumable outputs."""

import json

import numpy as np
import pandas as pd
import pytest

from ens_analysis import models


@pytest.fixture
def frame():
    rng = np.random.default_rng(7)
    n = 160
    return pd.DataFrame({
        "participant_id": [f"p{i}" for i in range(n)],
        "psu": np.repeat([f"g{i}" for i in range(n // 4)], 4),
        "stratum": np.repeat([f"s{i % 4}" for i in range(n // 4)], 4),
        "caries": np.tile([0, 1, 1, 0], n // 4),
        "weight": rng.uniform(0.2, 5.0, n),
        "age": rng.uniform(18, 80, n),
        "water": rng.integers(0, 12, n).astype(float),
        "sex": np.where(np.arange(n) % 2, "Female", "Male"),
    }, index=np.arange(1000, 1000 + n))


@pytest.fixture
def small_specs(monkeypatch):
    specs = {
        "logistic": {"kind": "logistic", "features": ["age", "water", "sex"],
                     "candidates": [{"C": 0.1}, {"C": 1.0}],
                     "weighted_training": True, "role": "algorithm_comparison"},
        "random_forest": {"kind": "random_forest", "features": ["age", "water", "sex"],
                          "candidates": [{"min_samples_leaf": 5, "max_features": "sqrt"}],
                          "weighted_training": True, "role": "algorithm_comparison"},
    }
    monkeypatch.setattr(models, "_model_specs", lambda: specs)
    return specs


def test_training_only_preprocessing_and_unseen_category():
    spec = {"kind": "logistic", "features": ["age", "water", "sex"], "weighted_training": True}
    train = pd.DataFrame({"age": [20., 30., 40., 50.], "water": [1., 3., np.nan, 5.],
                          "sex": ["Female", "Male", "Female", np.nan]})
    fitted = models._fit_pipeline(models._pipeline(spec, {"C": 1.}, 1, 1), train,
                                  np.array([0, 1, 0, 1]), np.ones(4), True)
    imputer = fitted.named_steps["preprocess"].named_transformers_["numeric"].named_steps["impute"]
    np.testing.assert_allclose(imputer.statistics_, [35., 3.])
    test = pd.DataFrame({"age": [999999., np.nan], "water": [np.nan, 100000.],
                        "sex": ["Unseen category", np.nan]})
    p = models._probabilities(fitted, test)
    assert np.isfinite(p).all()
    # Applying a extreme/missing test set cannot alter the fitted medians/categories.
    np.testing.assert_allclose(imputer.statistics_, [35., 3.])
    encoder = fitted.named_steps["preprocess"].named_transformers_["categorical"].named_steps["onehot"]
    assert "Unseen category" not in encoder.categories_[0]
    assert "Missing" in encoder.categories_[0]


def test_group_splits_are_disjoint_and_cover_each_row(frame):
    splits = models._group_splits(frame.caries.to_numpy(), frame.psu.to_numpy(), 5, 9, "test")
    heldout = []
    for train, test in splits:
        assert not set(frame.iloc[train].psu).intersection(frame.iloc[test].psu)
        heldout.extend(test)
    assert sorted(heldout) == list(range(len(frame)))


def test_probability_clipping_applies_only_to_log_loss():
    result = models._metrics(np.array([0, 1]), np.array([1., 0.]), np.array([1., 1.]))
    assert result["log_loss"] == pytest.approx(-np.log(1e-7))
    assert result["brier"] == 1.
    assert result["auc"] == 0.


def test_nested_outputs_share_folds_and_resume_without_refitting(frame, small_specs, tmp_path, monkeypatch):
    frame.loc[frame.index[:7], "water"] = np.nan
    result = models.run_nested_models(frame, tmp_path, seed=2, outer_folds=2, inner_folds=2,
                                      n_jobs=1, model_names=list(small_specs), permutation_repeats=1)
    oof = result["oof"]
    assert len(oof) == 2 * len(frame)
    assert oof.groupby("participant_id").fold.nunique().eq(1).all()
    assert oof.groupby(["model", "participant_id"]).size().eq(1).all()
    assert set(oof.row_index) == set(frame.index)
    assert result["split_qc"].psu_overlap.eq(0).all()
    assert len(result["importance"]) == 2 * 3
    assert result["tuning"].groupby(["model", "fold"]).selected.sum().eq(1).all()
    for name in small_specs:
        for fold in [1, 2]:
            payload = json.loads((tmp_path / "cache" / name / f"outer_{fold}.json").read_text())
            assert len(payload["probabilities"]) == len(payload["test_positions"])

    def unexpected_fit(*args, **kwargs):
        raise AssertionError("A valid completed cache should prevent fitting")

    monkeypatch.setattr(models, "_fit_estimator", unexpected_fit)
    resumed = models.run_nested_models(frame, tmp_path, seed=2, outer_folds=2, inner_folds=2,
                                       n_jobs=1, model_names=list(small_specs), permutation_repeats=1)
    pd.testing.assert_frame_equal(resumed["oof"], result["oof"])
    assert resumed["manifest"]["cache_hits"] == 4
    changed = frame.copy()
    changed.loc[changed.index[0], "weight"] *= 2
    with pytest.raises(RuntimeError, match="No participants or candidates were silently excluded"):
        models.run_nested_models(changed, tmp_path, seed=2, outer_folds=2, inner_folds=2,
                                  n_jobs=1, model_names=list(small_specs), permutation_repeats=1)


def test_invalid_outcome_weights_and_non_nested_psus_fail(frame, small_specs, tmp_path):
    for column, value, message in [("weight", 0., "strictly positive"),
                                    ("caries", np.nan, "Missing required")]:
        bad = frame.copy()
        bad.loc[bad.index[0], column] = value
        with pytest.raises(ValueError, match=message):
            models.run_nested_models(bad, tmp_path, outer_folds=2, inner_folds=2,
                                      model_names=["logistic"])
    bad = frame.copy()
    bad.loc[bad.index[0], "stratum"] = "unexpected stratum"
    with pytest.raises(ValueError, match="span multiple strata"):
        models.run_nested_models(bad, tmp_path, model_names=["logistic"])


def test_svm_uses_grouped_training_calibration(frame, monkeypatch):
    calls = []
    original_splitter = models._group_splits

    def track_splits(y, groups, n_splits, seed, label):
        splits = original_splitter(y, groups, n_splits, seed, label)
        calls.append((label, len(y), splits))
        return splits

    monkeypatch.setattr(models, "_group_splits", track_splits)
    spec = {"kind": "svm", "features": ["age", "water", "sex"], "weighted_training": True}
    train, heldout = frame.iloc[:120], frame.iloc[120:]
    fit = models._fit_estimator(spec, {"C": 1.}, models._feature_frame(train, spec["features"]),
                                train.caries.to_numpy(), train.weight.to_numpy(),
                                train.psu.to_numpy(), 11, 1)
    assert calls[0][0:2] == ("SVM calibration", len(train))
    for a, b in calls[0][2]:
        assert not set(train.iloc[a].psu).intersection(train.iloc[b].psu)
        assert len(a) < len(train) and len(b) < len(train)
    assert np.isfinite(models._probabilities(fit, models._feature_frame(heldout, spec["features"]))).all()
    assert fit.estimator.named_steps["model"].probability in {False, "deprecated"}


@pytest.mark.parametrize("kind,params", [
    ("spline_logistic", {"C": 1.}),
    ("hist_gradient_boosting", {"max_leaf_nodes": 7, "l2_regularization": 1.}),
    ("decision_tree", {"min_samples_leaf": 20, "max_depth": 3}),
    ("knn", {"n_neighbors": 25}),
])
def test_estimator_families_accept_missing_predictors(frame, kind, params):
    frame.loc[frame.index[:10], "water"] = np.nan
    frame.loc[frame.index[10:20], "sex"] = np.nan
    spec = {"kind": kind, "features": ["age", "water", "sex"], "weighted_training": kind != "knn"}
    x = models._feature_frame(frame, spec["features"])
    fit = models._fit_estimator(spec, params, x.iloc[:120], frame.caries.to_numpy()[:120],
                                frame.weight.to_numpy()[:120], frame.psu.to_numpy()[:120], 5, 1)
    p = models._probabilities(fit, x.iloc[120:])
    assert len(p) == 40 and np.isfinite(p).all()
