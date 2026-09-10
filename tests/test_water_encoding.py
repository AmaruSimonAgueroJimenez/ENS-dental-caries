"""Representation sensitivity must preserve reference predictions and validation."""
from copy import deepcopy
import json

import numpy as np
import pandas as pd
import pytest

from ens_analysis import models, water_encoding as encoding


def mapping():
    return pd.DataFrame({"reported_glasses_per_day": [0., 1., 2., 18.],
                         "original_r_factor_index": [1, 2, 3, 4]})


def test_historical_mapping_and_nominal_alias_preserve_inputs_and_missing():
    frame = pd.DataFrame({"age": [20., 40., 60.], "water": [0., 18., np.nan]})
    original = frame.copy(deep=True)
    spec = {"kind": "spline_logistic", "features": ["age", "water"]}
    original_spec = deepcopy(spec)
    mapped, index_spec = encoding._encode_water(frame, spec, "index", encoding._water_mapping(mapping()))
    assert mapped.water.iloc[:2].tolist() == [1., 4.]
    assert pd.isna(mapped.water.iloc[2])
    nominal, nominal_spec = encoding._encode_water(frame, spec, "nominal", encoding._water_mapping(mapping()))
    assert nominal_spec["features"] == ["age", "water_nominal"]
    assert nominal.water_nominal.iloc[:2].tolist() == ["water=0", "water=18"]
    assert pd.isna(nominal.water_nominal.iloc[2])
    assert spec == original_spec == index_spec
    pd.testing.assert_frame_equal(frame, original)
    assert "water_nominal" not in models.NUMERIC_FEATURES
    assert "water" in models.SPLINE_FEATURES
    with pytest.raises(ValueError, match="absent from"):
        encoding._encode_water(frame.assign(water=[0., 17., np.nan]), spec, "index", encoding._water_mapping(mapping()))
    with pytest.raises(ValueError, match="ordered indices"):
        encoding._water_mapping(mapping().assign(original_r_factor_index=[1, 2, 3, 5]))


def test_nominal_levels_are_learned_only_in_training():
    frame = pd.DataFrame({"age": [20., 30., 40., 50., 60., 70.], "water": [0., 1., 2., np.nan, 0., 1.]})
    encoded, spec = encoding._encode_water(frame, {"features": ["age", "water"]}, "nominal", encoding._water_mapping(mapping()))
    x = models._feature_frame(encoded, spec["features"])
    transformer = models._preprocessor(spec["features"], spline=True, scale=True).fit(x)
    cat = transformer.named_transformers_["categorical"]
    assert "water=18" not in cat.named_steps["onehot"].categories_[0]
    unknown = x.iloc[[0]].assign(water_nominal="water=18")
    assert np.array_equal(cat.transform(unknown[["water_nominal"]]), np.zeros((1, 4)))
    assert np.isfinite(transformer.transform(unknown)).all()


def test_water_only_permutation_matches_original_feature_position_and_seed():
    x = pd.DataFrame({"age": [20., 30., 40., 50.], "water": [0., 1., 2., np.nan]})
    class Predictor:
        def predict_proba(self, values):
            p = .2 + values.water.fillna(0).to_numpy()*.15
            return np.column_stack([1-p, p])
    fitted = Predictor()
    y, w = np.array([0, 1, 0, 1]), np.array([1., 2., 3., 4.])
    p = models._probabilities(fitted, x)
    original = pd.DataFrame(models._permutation_importance(fitted, x, y, w, p, "test", 3, 5, 42))
    water = pd.DataFrame(encoding._water_permutation(fitted, x, y, w, p, "test", 3, 5, 42, 1))
    pd.testing.assert_frame_equal(water, original.loc[original.feature.eq("water")].reset_index(drop=True))


def test_stability_uses_weighted_fold_means_and_keeps_negative_values():
    records = pd.DataFrame({"model": ["RF"]*4, "fold": [1, 1, 2, 2],
                            "brier_increase": [-.3, -.1, .1, .3],
                            "auc_drop": [-.1, -.1, .05, .05],
                            "test_weight_sum": [3., 3., 1., 1.]})
    result = encoding.summarize_water_importance(records).iloc[0]
    assert result.brier_increase == pytest.approx(-.1)
    assert result.minimum_fold_brier_increase == pytest.approx(-.2)
    assert result.maximum_fold_brier_increase == pytest.approx(.2)
    assert "rank" not in result.index


@pytest.fixture
def completed(tmp_path, monkeypatch):
    rng = np.random.default_rng(73)
    n = 80
    frame = pd.DataFrame({
        "participant_id": [f"p{i}" for i in range(n)], "psu": [f"u{i//4}" for i in range(n)],
        "stratum": "s1", "caries": np.tile([0, 1, 1, 0], n//4),
        "weight": rng.uniform(.5, 2., n), "age": rng.uniform(20, 80, n),
        "water": rng.choice([0., 1., 2., 18.], n), "eligible": np.arange(n) < 72})
    frame.loc[4, "water"] = np.nan
    specs = models._model_specs()
    for name in encoding.BASE_MODELS:
        specs[name]["features"] = ["age", "water"]
        specs[name]["candidates"] = ([{"C": .01}] if name == "spline_logistic"
                                      else [{"max_features": "sqrt", "min_samples_leaf": 2}])
    monkeypatch.setattr(models, "_model_specs", lambda: specs)
    results = models.run_nested_models(frame.loc[frame.eligible], tmp_path / "original", seed=23,
        outer_folds=2, inner_folds=2, n_jobs=1, model_names=list(encoding.BASE_MODELS), permutation_repeats=2)
    return frame, results, tmp_path / "alternative"


def test_complete_sensitivity_reuses_physical_and_resumes_without_new_fits(completed, monkeypatch):
    frame, results, cache = completed
    original_frame, original_oof = frame.copy(deep=True), results["oof"].copy(deep=True)
    calls, actual_fit = [], models._fit_estimator
    def counted(spec, params, x, y, weight, groups, seed, n_jobs):
        calls.append((spec["kind"], tuple(x.columns), seed, dict(params)))
        return actual_fit(spec, params, x, y, weight, groups, seed, n_jobs)
    monkeypatch.setattr(models, "_fit_estimator", counted)
    run = encoding.run_water_encoding_sensitivity(frame, results, mapping(), cache,
        bootstrap_replicates=20, permutation_repeats=2, n_jobs=1)
    assert len(calls) == 8  # 2 alternatives x 2 algorithms x 2 outer folds
    assert run["manifest"]["new_hyperparameter_searches"] == 0
    assert run["manifest"]["n_full_design_psu"] == 20
    assert run["manifest"]["n_psu"] == 18
    assert run["manifest"]["physical_outer_fits_reused"] == 4
    assert len(run["performance"]) == 12
    assert len(run["comparisons"]) == 12
    assert len(run["oof"]) == 6*72
    assert len(run["water_importance"]) == 6*2*2
    assert len(run["water_importance_stability"]) == 6
    assert run["comparisons"].reference_encoding.eq("physical").all()
    assert set(call[2] for call in calls) == {1023, 2023}
    source = results["oof"].sort_values(["model", "row_index"]).reset_index(drop=True)
    reused = run["oof"].loc[run["oof"].encoding.eq("physical"), source.columns].sort_values(["model", "row_index"]).reset_index(drop=True)
    pd.testing.assert_frame_equal(source, reused)
    pd.testing.assert_frame_equal(frame, original_frame)
    pd.testing.assert_frame_equal(results["oof"], original_oof)
    resumed = encoding.run_water_encoding_sensitivity(frame, results, mapping(), cache,
        bootstrap_replicates=20, permutation_repeats=2, n_jobs=1)
    assert len(calls) == 8
    assert resumed["manifest"]["alternative_outer_fits_this_call"] == 0
    assert resumed["manifest"]["cache_hits_this_call"] == 8
    for key in ["performance", "comparisons", "water_importance", "water_importance_stability", "oof"]:
        pd.testing.assert_frame_equal(run[key], resumed[key])
    payload_path = next(cache.rglob("outer_1.json"))
    payload = json.loads(payload_path.read_text())
    payload["probabilities"][0] = .999
    payload_path.write_text(json.dumps(payload))
    assert encoding._read_cache(payload_path, payload["fingerprint"], np.array(payload["test_positions"]), 2) is None


def test_stale_physical_inputs_or_folds_fail_before_fitting(completed, monkeypatch):
    frame, results, cache = completed
    monkeypatch.setattr(models, "_fit_estimator", lambda *args, **kwargs: pytest.fail("Unexpected fit before source validation"))
    changed = frame.copy()
    changed.loc[0, "age"] += 1
    with pytest.raises(ValueError, match="Stale physical"):
        encoding.run_water_encoding_sensitivity(changed, results, mapping(), cache,
            bootstrap_replicates=20, permutation_repeats=2, n_jobs=1)
    changed_results = deepcopy(results)
    changed_results["oof"].loc[0, "fold"] = 7
    with pytest.raises(ValueError, match="fold assignments"):
        encoding.run_water_encoding_sensitivity(frame, changed_results, mapping(), cache,
            bootstrap_replicates=20, permutation_repeats=2, n_jobs=1)
