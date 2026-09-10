"""Safeguards for the fixed dietary-increment protocol (synthetic data only)."""

from copy import deepcopy
import json

import numpy as np
import pandas as pd
import pytest

from ens_analysis import dietary_models as dm, models
from ens_analysis.expanded_data import ALL_FEATURES, NUMERIC_FEATURES


@pytest.fixture
def frame():
    rng = np.random.default_rng(213)
    n = 120
    frame = pd.DataFrame({
        "participant_id": [f"p{i}" for i in range(n)],
        "psu": np.repeat([f"g{i}" for i in range(n // 4)], 4),
        "stratum": np.repeat([f"s{i % 3}" for i in range(n // 4)], 4),
        "caries": np.tile([0, 1, 1, 0], n // 4),
        "weight": rng.uniform(0.2, 5., n), "eligible": True,
    }, index=np.arange(1000, 1000 + n))
    for feature in ALL_FEATURES:
        frame[feature] = (rng.uniform(0, 12, n) if feature in NUMERIC_FEATURES
                          else rng.choice(["Low", "High"], n).astype(object))
    frame["age"] = rng.uniform(18, 90, n)
    frame["teeth_remaining"] = rng.integers(1, 33, n).astype(float)
    frame.loc[frame.index[:5], "soda_glasses_daily"] = np.nan
    return frame


def test_fixed_sets_disjoint_blocks_and_unchanged_tuning_grids():
    specs = dm._model_specs()
    assert len(specs) == 23
    counts = {"basic": 8, "context": 17, "context_diet": 32,
              "context_diet_labels": 38, "symptoms": 23,
              "symptoms_diet": 38, "symptoms_diet_labels": 44}
    for family in dm.FAMILIES:
        for feature_set, count in counts.items():
            spec = specs[f"{family}__{feature_set}"]
            assert len(spec["features"]) == count
            assert spec["candidates"] == models._model_specs()[family]["candidates"]
            assert not set(spec["features"]) & dm.FORBIDDEN_PREDICTORS
    assert sum(s["compute_importance"] for s in specs.values()) == 4
    assert {n for n, s in specs.items() if s["winsor_positive_quantile"] is not None} == {
        "spline_logistic__context_diet__winsor99", "spline_logistic__symptoms_diet__winsor99",
    }
    for spec in specs.values():
        if spec["compute_importance"]:
            grouped = [f for block in spec["groups"].values() for f in block]
            assert len(grouped) == len(set(grouped)) == len(spec["features"])
            assert set(grouped) == set(spec["features"])
    dm._validate_specs(specs)


def test_positive_training_quantile_keeps_zeros_missing_and_fixed_caps():
    train = pd.DataFrame({"soda_frequency": [0., np.nan, 1., 10., 100.],
                          "soda_glasses_daily": [0., 0., 0., np.nan, 0.],
                          "water": [0., 1., 2., 3., 4.]})
    transform = dm.BeverageTransform(0.99).fit(train)
    assert transform.caps_["soda_frequency"] == pytest.approx(98.2)
    assert transform.caps_["soda_glasses_daily"] is None
    heldout = pd.DataFrame({"soda_frequency": [0., np.nan, 1e9],
                            "soda_glasses_daily": [1000., 0., np.nan],
                            "water": [100., 0., np.nan]})
    changed = transform.transform(heldout)
    np.testing.assert_allclose(changed.soda_frequency, [0., np.nan, np.log1p(98.2)], equal_nan=True)
    np.testing.assert_allclose(changed.soda_glasses_daily, [np.log1p(1000), 0., np.nan], equal_nan=True)
    pd.testing.assert_series_equal(changed.water, heldout.water)
    assert transform.caps_["soda_frequency"] == pytest.approx(98.2)
    assert heldout.soda_frequency.iloc[-1] == 1e9
    primary = dm.BeverageTransform().fit(train).transform(heldout)
    assert primary.soda_frequency.iloc[-1] == pytest.approx(np.log1p(1e9))
    for bad in [-1., np.inf]:
        with pytest.raises(ValueError, match="nonnegative"):
            dm.BeverageTransform().fit(pd.DataFrame({"juice_frequency": [bad]}))


def test_new_volumes_are_numeric_and_training_transformations_do_not_use_holdout(frame):
    features = ["age", "water", "soda_frequency", "soda_glasses_daily", "income_band"]
    spec = {"kind": "spline_logistic", "features": features,
            "winsor_positive_quantile": 0.99, "weighted_training": True}
    x = dm._feature_frame(frame, features)
    assert x.soda_glasses_daily.dtype.kind == "f"
    assert x.income_band.dtype == object
    fitted = dm._fit_estimator(spec, {"C": 0.1}, x.iloc[:90], frame.caries.to_numpy()[:90],
                               frame.weight.to_numpy()[:90], 2, 1)
    prep = fitted.named_steps["preprocess"]
    transformer_columns = {name: list(columns) for name, _, columns in prep.transformers_ if name != "remainder"}
    assert transformer_columns["numeric"] == ["soda_frequency", "soda_glasses_daily"]
    assert transformer_columns["categorical"] == ["income_band"]
    assert transformer_columns["numeric_missing"] == ["age", "water", "soda_frequency", "soda_glasses_daily"]
    medians = prep.named_transformers_["numeric"].named_steps["impute"].statistics_.copy()
    scaler_mean = prep.named_transformers_["numeric"].named_steps["scale"].mean_.copy()
    knots = [b.t.copy() for b in prep.named_transformers_["splines"].named_steps["spline"].bsplines_]
    caps = fitted.named_steps["beverages"].caps_.copy()
    expected_cap = np.quantile(x.soda_glasses_daily.iloc[:90].dropna(), 0.99)
    assert caps["soda_glasses_daily"] == pytest.approx(expected_cap)
    heldout = x.iloc[90:].copy()
    heldout.loc[:, "soda_glasses_daily"] = 1e12
    heldout.loc[:, "income_band"] = "Held-out-only category"
    assert np.isfinite(models._probabilities(fitted, heldout)).all()
    np.testing.assert_array_equal(medians, prep.named_transformers_["numeric"].named_steps["impute"].statistics_)
    np.testing.assert_array_equal(scaler_mean, prep.named_transformers_["numeric"].named_steps["scale"].mean_)
    for a, b in zip(knots, prep.named_transformers_["splines"].named_steps["spline"].bsplines_, strict=True):
        np.testing.assert_array_equal(a, b.t)
    assert caps == fitted.named_steps["beverages"].caps_
    assert "Held-out-only category" not in prep.named_transformers_["categorical"].named_steps["onehot"].categories_[0]


def test_basic_pipeline_matches_original_family_exactly(frame):
    spec = dm._model_specs()["spline_logistic__basic"]
    x = dm._feature_frame(frame, spec["features"])
    old_x = models._feature_frame(frame, spec["features"])
    first = dm._fit_estimator(spec, {"C": 0.1}, x.iloc[:90], frame.caries.to_numpy()[:90],
                              frame.weight.to_numpy()[:90], 5, 1)
    original = models._fit_estimator(spec, {"C": 0.1}, old_x.iloc[:90], frame.caries.to_numpy()[:90],
                                     frame.weight.to_numpy()[:90], frame.psu.to_numpy()[:90], 5, 1)
    np.testing.assert_allclose(models._probabilities(first, x.iloc[90:]),
                               models._probabilities(original, old_x.iloc[90:]), rtol=0, atol=1e-14)


def test_permutations_preserve_group_dependencies_and_recipient_weights():
    # Perfect prediction from a binary signal, with a deterministic second
    # column: independent permutation breaks it, joint permutation preserves it.
    x = pd.DataFrame({"a": np.tile([0., 1.], 50), "b": np.tile([0., 2.], 50)})
    y = x.a.to_numpy(dtype=int)
    weight = np.linspace(1., 4., len(x))
    observations = []

    class Fitted:
        def predict_proba(self, frame):
            observations.append(frame.copy())
            p = 0.1 + 0.8 * frame.a.to_numpy()
            return np.c_[1 - p, p]

    individual, grouped = dm._permutation_tables(Fitted(), x, y, weight,
                                                  0.1 + 0.8 * y, "test", 1,
                                                  {"intake": ["a", "b"]}, repeats=2)
    assert len(individual) == 4 and len(grouped) == 2
    assert all(row["brier_increase"] > 0 for row in individual if row["feature"] == "a")
    assert all(row["brier_increase"] == pytest.approx(0) for row in individual if row["feature"] == "b")
    for perturbed in observations[-2:]:
        np.testing.assert_array_equal(perturbed.b, 2 * perturbed.a)
        assert sorted(perturbed.a) == sorted(x.a)
    for perturbed, row in zip(observations[-2:], grouped, strict=True):
        expected = np.average((y - (0.1 + 0.8 * perturbed.a.to_numpy())) ** 2, weights=weight) - 0.01
        assert row["brier_increase"] == pytest.approx(expected)
        assert row["test_weight_sum"] == pytest.approx(weight.sum())


def test_nested_domain_original_splits_training_boundaries_and_resume(frame, monkeypatch, tmp_path):
    specs = dm._model_specs()
    selected = {k: deepcopy(specs[k]) for k in ["spline_logistic__basic", "spline_logistic__context_diet__winsor99"]}
    for spec in selected.values():
        spec["candidates"] = [{"C": 0.1}]
    monkeypatch.setattr(dm, "_model_specs", lambda: selected)
    # An ineligible row with unavailable examination data must not become a
    # modelling participant or change the order/indices of the eligible cohort.
    excluded = frame.iloc[[0]].copy()
    excluded.index = [999]
    excluded["participant_id"] = "excluded"
    excluded["eligible"] = False
    excluded["caries"] = np.nan
    excluded["weight"] = np.nan
    full = pd.concat([excluded, frame])
    fits = []
    original_fit = dm._fit_estimator

    def tracked_fit(spec, params, x, y, weight, seed, n_jobs):
        fits.append((spec["feature_set"], seed, set(x.index)))
        return original_fit(spec, params, x, y, weight, seed, n_jobs)

    monkeypatch.setattr(dm, "_fit_estimator", tracked_fit)
    result = dm.run_dietary_models(full, tmp_path, n_jobs=1)
    oof = result["oof"]
    assert len(oof) == 2 * len(frame)
    assert "excluded" not in set(oof.participant_id)
    assert set(oof.row_index) == set(frame.index)
    assert oof.groupby("participant_id").fold.nunique().eq(1).all()
    assert oof.groupby(["model", "participant_id"]).size().eq(1).all()
    assert result["split_qc"].psu_overlap.eq(0).all()
    assert len(result["split_qc"]) == 20
    reference = models._group_splits(frame.caries.to_numpy(), frame.psu.to_numpy(), 5, 20260910, "reference")
    for fold, ((train, test), (a, b)) in enumerate(zip(result["splits"]["outer"], reference, strict=True), start=1):
        np.testing.assert_array_equal(train, a)
        np.testing.assert_array_equal(test, b)
        expected_inner = models._group_splits(frame.caries.to_numpy()[train], frame.psu.to_numpy()[train],
                                               3, 20260910 + fold * 101, "inner reference")
        for (it, iv), (rt, rv) in zip(result["splits"]["inner"][fold], expected_inner, strict=True):
            np.testing.assert_array_equal(it, rt)
            np.testing.assert_array_equal(iv, rv)
        for feature_set in [s["feature_set"] for s in selected.values()]:
            for inner_fold, (it, _) in enumerate(expected_inner, start=1):
                assert (feature_set, 20260910 + fold * 1000 + inner_fold, set(frame.index[train[it]])) in fits
            assert (feature_set, 20260910 + fold * 1000, set(frame.index[train])) in fits
    assert len(fits) == 2 * 5 * (3 + 1)
    assert result["tuning"].groupby(["model", "fold"]).selected.sum().eq(1).all()
    assert result["manifest"]["fitted_model_folds"] == 10
    assert set(result["manifest"]["source_hashes"]) == {
        "data.py", "expanded_data.py", "models.py", "dietary_models.py",
    }
    def unexpected_fit(*args, **kwargs):
        raise AssertionError("Cache should prevent fitting")

    monkeypatch.setattr(dm, "_fit_estimator", unexpected_fit)
    resumed = dm.run_dietary_models(full, tmp_path, n_jobs=1)
    pd.testing.assert_frame_equal(resumed["oof"], oof)
    assert resumed["manifest"]["cache_hits"] == 10
    assert resumed["manifest"]["fitted_model_folds"] == 0
    corrupted = tmp_path / "spline_logistic__basic" / "outer_1.json"
    cache = json.loads(corrupted.read_text())
    cache["payload"]["probabilities"][0] = 0.123456
    corrupted.write_text(json.dumps(cache))
    with pytest.raises(RuntimeError, match="Cache should prevent fitting"):
        dm.run_dietary_models(full, tmp_path, n_jobs=1)


def test_cache_fingerprint_rejects_data_or_source_changes(frame, monkeypatch):
    spec = dm._model_specs()["spline_logistic__basic"]
    settings = {"source_hashes": dm._source_hashes(), "software": {"sklearn": dm.sklearn.__version__}}
    baseline = dm._fingerprint(frame, spec, settings)
    changed = frame.copy()
    changed.loc[changed.index[0], "weight"] *= 1.5
    assert dm._fingerprint(changed, spec, settings) != baseline
    changed_settings = deepcopy(settings)
    changed_settings["source_hashes"]["expanded_data.py"] = "changed"
    assert dm._fingerprint(frame, spec, changed_settings) != baseline
    changed = frame.copy()
    changed["decayed_teeth"] = changed.caries * 20
    # An unused outcome component cannot enter preprocessing or fingerprints.
    assert dm._fingerprint(changed, spec, settings) == baseline
    pd.testing.assert_frame_equal(dm._feature_frame(changed, spec["features"]),
                                   dm._feature_frame(frame, spec["features"]))


def test_outcome_leakage_invalid_design_and_negative_beverages_fail(frame, monkeypatch, tmp_path):
    spec = deepcopy(dm._model_specs()["spline_logistic__basic"])
    for forbidden in ["decayed_teeth", "DMFT", "caries", "m5p5"]:
        bad = deepcopy(spec)
        bad["features"].append(forbidden)
        with pytest.raises(ValueError, match="Outcome/design leakage"):
            dm._validate_specs({"invalid": bad})
    monkeypatch.setattr(dm, "_model_specs", lambda: {"basic": spec})
    for column, value, message in [("weight", 0., "strictly positive"),
                                    ("caries", np.nan, "Missing required"),
                                    ("stratum", "different", "span multiple strata")]:
        bad = frame.copy()
        bad.loc[bad.index[0], column] = value
        with pytest.raises(ValueError, match=message):
            dm.run_dietary_models(bad, tmp_path, n_jobs=1)
    with pytest.raises(ValueError, match="nonnegative"):
        dm._feature_frame(pd.DataFrame({"soda_glasses_daily": [-1., 0.]}), ["soda_glasses_daily"])
