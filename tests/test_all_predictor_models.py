"""Synthetic safeguards for the exploratory full-information neural benchmark."""
from copy import deepcopy
import json

import numpy as np
import pandas as pd
import pytest

from ens_analysis import all_predictor_models as am, dietary_models as dm, models
from ens_analysis.expanded_data import ALL_FEATURES, NUMERIC_FEATURES


@pytest.fixture(scope="module")
def source(tmp_path_factory):
    rng = np.random.default_rng(71)
    n = 120
    frame = pd.DataFrame({
        "participant_id": [f"person_{i}" for i in range(n)],
        "psu": np.repeat([f"g{i}" for i in range(n // 4)], 4),
        "stratum": np.repeat([f"s{i % 3}" for i in range(n // 4)], 4),
        "caries": np.tile([0, 1, 1, 0], n // 4), "weight": rng.uniform(.3, 4., n),
        "eligible": True,
    }, index=np.arange(1000, 1000 + n))
    for f in ALL_FEATURES:
        frame[f] = rng.uniform(0, 10, n) if f in NUMERIC_FEATURES else rng.choice(["A", "B"], n)
    frame.loc[frame.index[:4], "soda_glasses_daily"] = np.nan
    # Restrict synthetic source grids, while retaining genuine fitted OOF and
    # genuine fingerprints. Production grids and data are never modified.
    specs = {name: deepcopy(dm._model_specs()[name]) for name in am._source_names()}
    for spec in specs.values():
        spec["candidates"] = spec["candidates"][:1]
    patch = pytest.MonkeyPatch()
    patch.setattr(dm, "_model_specs", lambda: specs)
    results = dm.run_dietary_models(frame, tmp_path_factory.mktemp("dietary_source"), n_jobs=1)
    yield frame, results
    patch.undo()


def test_grid_has_two_hidden_layers_and_two_prespecified_scenarios():
    specs = am._mlp_specs()
    assert set(specs) == {"mlp__context_diet_labels", "mlp__symptoms_diet_labels"}
    assert sorted(len(s["features"]) for s in specs.values()) == [38, 44]
    assert len(am.MLP_CANDIDATES) == 6
    assert {tuple(p["hidden_layer_sizes"]) for p in am.MLP_CANDIDATES} == {(32, 16), (64, 32)}
    assert {(p["alpha"], p["epochs"]) for p in am.MLP_CANDIDATES} == {(1., 100), (10., 100), (10., 250)}
    for s in specs.values():
        assert not set(s["features"]) & dm.FORBIDDEN_PREDICTORS
        assert [f for g in s["groups"].values() for f in g] != []
        assert sorted(f for g in s["groups"].values() for f in g) == sorted(s["features"])


def test_fixed_budget_weighted_fit_and_training_only_transformations(monkeypatch):
    rng = np.random.default_rng(1)
    x = pd.DataFrame({"age": [20., 30., np.nan, 50.] * 8,
                      "soda_glasses_daily": [0., 1., 4., np.nan] * 8,
                      "income_band": ["A", "B", np.nan, "A"] * 8})
    spec = {"features": list(x.columns)}
    params = {"hidden_layer_sizes": [4, 2], "alpha": 1., "epochs": 3}
    y = rng.integers(0, 2, len(x)); weight = np.linspace(.5, 4, len(x))
    original_fit = am.MLPClassifier.fit
    seen = []
    def weighted_fit(self, X, y, sample_weight=None):
        seen.append(np.asarray(sample_weight).copy())
        return original_fit(self, X, y, sample_weight=sample_weight)
    monkeypatch.setattr(am.MLPClassifier, "fit", weighted_fit)
    fitted, diagnostics = am._fit_mlp(spec, params, x, y, weight, 12, 1)
    np.testing.assert_allclose(seen[0], weight / weight.mean())
    model = fitted.named_steps["model"]
    assert not model.early_stopping and model.n_iter_no_change > model.max_iter and model.tol == 0
    assert model.n_iter_ == diagnostics["epochs_completed"] == 3
    assert diagnostics["optimizer_status"] == "fixed_epoch_budget_completed"
    assert "No tolerance-based convergence claim" in diagnostics["convergence_claim"]
    prep = fitted.named_steps["preprocess"]
    numeric = prep.named_transformers_["numeric"]
    medians = numeric.named_steps["impute"].statistics_.copy()
    mean = numeric.named_steps["scale"].mean_.copy()
    assert medians[1] == pytest.approx(np.log1p(1.))
    unseen = pd.DataFrame({"age": [1e8], "soda_glasses_daily": [1e9], "income_band": ["unseen"]})
    assert np.isfinite(models._probabilities(fitted, unseen)).all()
    np.testing.assert_array_equal(medians, numeric.named_steps["impute"].statistics_)
    np.testing.assert_array_equal(mean, numeric.named_steps["scale"].mean_)
    assert "unseen" not in prep.named_transformers_["categorical"].named_steps["onehot"].categories_[0]
    assert fitted.named_steps["beverages"].caps_ == {}


def test_source_authenticates_ids_design_parameters_and_both_split_levels(source):
    frame, results = source
    domain, _, outer, inner, _, _ = am._validate_source(frame, results)
    assert domain.index.equals(frame.index)
    for fold, (train, test) in enumerate(outer, 1):
        assert not set(frame.psu.iloc[train]) & set(frame.psu.iloc[test])
        for a, b in inner[fold]:
            assert not set(frame.psu.iloc[train[a]]) & set(frame.psu.iloc[train[b]])
    changed = frame.copy(); changed.loc[changed.index[0], "weight"] *= 2
    with pytest.raises(ValueError, match="fingerprint"):
        am._validate_source(changed, results)
    changed = deepcopy(results); changed["oof"].loc[0, "participant_id"] = "wrong"
    with pytest.raises(ValueError, match="identity"):
        am._validate_source(frame, changed)
    changed = deepcopy(results); changed["splits"]["inner"][1][0][0][0] += 1
    with pytest.raises(ValueError, match="inner PSU split"):
        am._validate_source(frame, changed)
    changed = deepcopy(results); changed["foldscores"].loc[0, "selected_params"] = '{"C":99999}'
    with pytest.raises(ValueError, match="parameters"):
        am._validate_source(frame, changed)


def test_eight_variants_preserve_source_refits_and_resume(source, monkeypatch, tmp_path):
    frame, previous = source
    specs = am._mlp_specs()
    for spec in specs.values():
        spec["candidates"] = [{"hidden_layer_sizes": [4, 2], "alpha": 1., "epochs": 2}]
    monkeypatch.setattr(am, "_mlp_specs", lambda: specs)
    plan = tmp_path / "plan.md"; plan.write_text("Synthetic fixed benchmark plan\n")
    fits = []
    original_fit = am._fit_mlp
    def tracked_fit(spec, params, x, y, weight, seed, n_jobs):
        fits.append((spec["feature_set"], seed, set(x.index)))
        return original_fit(spec, params, x, y, weight, seed, n_jobs)
    monkeypatch.setattr(am, "_fit_mlp", tracked_fit)
    result = am.run_all_predictor_models(frame, tmp_path / "cache", previous, 1, plan)
    assert len(result["manifest"]["models"]) == 8
    assert result["manifest"]["fitted_mlp_model_folds"] == 10
    assert result["manifest"]["reconstructed_boosting_model_folds"] == 10
    assert len(result["training_diagnostics"]) == len(fits) == 40
    assert result["reconstruction_checks"].maximum_absolute_probability_difference.le(1e-12).all()
    for key in ["oof", "tuning", "foldscores"]:
        original = previous[key].loc[previous[key].model.isin(am._source_names())].reset_index(drop=True)
        actual = result[key].loc[result[key].model.isin(am._source_names()), original.columns].reset_index(drop=True)
        pd.testing.assert_frame_equal(actual, original, check_dtype=False)
    for fold, (train, _) in enumerate(result["splits"]["outer"], 1):
        for spec in specs.values():
            assert (spec["feature_set"], am.SEED + fold * 1000, set(frame.index[train])) in fits
            for i, (a, _) in enumerate(result["splits"]["inner"][fold], 1):
                assert (spec["feature_set"], am.SEED + fold * 1000 + i, set(frame.index[train[a]])) in fits
    assert result["oof"].groupby(["model", "participant_id"]).size().eq(1).all()
    assert result["oof"].groupby("participant_id").fold.nunique().eq(1).all()
    assert result["split_qc"].psu_overlap.eq(0).all()
    assert len(result["importance"]) == (38 + 44) * 4 * 5 * 5
    assert len(result["grouped_importance"]) == (3 + 4) * 4 * 5 * 5
    def forbidden(*args, **kwargs):
        raise AssertionError("Cached run must not refit")
    monkeypatch.setattr(am, "_fit_mlp", forbidden)
    monkeypatch.setattr(dm, "_fit_estimator", forbidden)
    resumed = am.run_all_predictor_models(frame, tmp_path / "cache", previous, 1, plan)
    assert resumed["manifest"]["cache_hits"] == 20
    pd.testing.assert_frame_equal(resumed["oof"], result["oof"])
    cache = tmp_path / "cache/mlp__context_diet_labels/outer_1.json"
    envelope = json.loads(cache.read_text()); envelope["payload"]["probabilities"][0] = .123456
    cache.write_text(json.dumps(envelope))
    with pytest.raises(AssertionError, match="must not refit"):
        am.run_all_predictor_models(frame, tmp_path / "cache", previous, 1, plan)


def test_boosting_reconstruction_mismatch_fails_before_permutation(source, monkeypatch, tmp_path):
    frame, previous = source
    plan = tmp_path / "plan.md"; plan.write_text("Synthetic\n")
    class Wrong:
        def predict_proba(self, x):
            return np.tile([.99, .01], (len(x), 1))
    monkeypatch.setattr(dm, "_fit_estimator", lambda *a, **k: Wrong())
    def unexpected(*args, **kwargs):
        raise AssertionError("Do not permute a reconstruction that fails")
    monkeypatch.setattr(dm, "_permutation_tables", unexpected)
    with pytest.raises(RuntimeError, match="reconstruction failed"):
        am.run_all_predictor_models(frame, tmp_path / "cache", previous, 1, plan)
