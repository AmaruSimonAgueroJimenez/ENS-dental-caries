"""Exploratory full-information benchmark with a regularized tabular MLP.

Six previously fitted variants are authenticated and retained. Boosting is
reconstructed solely to obtain held-out permutation importance. Only the two
MLP variants undergo a new, bounded nested search. All participant-level return
values and caches must remain private; this module writes no public tables.
"""
from __future__ import annotations

from collections import Counter
from copy import deepcopy
from hashlib import sha256
import inspect
import json
from pathlib import Path
import time
import warnings

import numpy as np
import pandas as pd
import scipy
import sklearn
from sklearn.compose import ColumnTransformer
from sklearn.exceptions import ConvergenceWarning
from sklearn.impute import MissingIndicator, SimpleImputer
from sklearn.neural_network import MLPClassifier
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler
from threadpoolctl import threadpool_limits

from . import dietary_models as dm, models


SEED = dm.SEED
FEATURE_SETS = ("context_diet_labels", "symptoms_diet_labels")
SOURCE_FAMILIES = dm.FAMILIES
MLP_FAMILY = "mlp"
CACHE_VERSION = 1
REPRODUCTION_ATOL = 1e-12
PERMUTATION_REPEATS = dm.PERMUTATION_REPEATS
MLP_CANDIDATES = [
    {"hidden_layer_sizes": list(layers), "alpha": alpha, "epochs": epochs}
    for layers in [(32, 16), (64, 32)]
    for alpha, epochs in [(1.0, 100), (10.0, 100), (10.0, 250)]
]


def _source_names():
    return [f"{family}__{setting}" for family in SOURCE_FAMILIES for setting in FEATURE_SETS]


def _mlp_specs():
    sets, blocks = dm._feature_sets()
    return {f"{MLP_FAMILY}__{setting}": {
        "kind": MLP_FAMILY, "feature_set": setting, "features": sets[setting],
        "groups": {g: list(fs) for g, fs in blocks.items() if set(fs).issubset(sets[setting])},
        "candidates": deepcopy(MLP_CANDIDATES), "weighted_training": True,
        "winsor_positive_quantile": None, "compute_importance": True,
        "role": "exploratory_neural_network_benchmark",
    } for setting in FEATURE_SETS}


def _source_hashes():
    return dm._source_hashes() | {Path(__file__).name: sha256(Path(__file__).read_bytes()).hexdigest()}


def _preprocessor(features):
    numeric = [f for f in features if f in dm._numeric_features()]
    categorical = [f for f in features if f not in numeric]
    transformers = []
    if numeric:
        transformers.extend([
            ("numeric", Pipeline([
                ("impute", SimpleImputer(strategy="median", keep_empty_features=True)),
                ("scale", StandardScaler()),
            ]), numeric),
            ("numeric_missing", MissingIndicator(features="all", error_on_new=False), numeric),
        ])
    if categorical:
        transformers.append(("categorical", Pipeline([
            ("impute", SimpleImputer(strategy="constant", fill_value="Missing", keep_empty_features=True)),
            ("onehot", OneHotEncoder(handle_unknown="ignore", sparse_output=False)),
        ]), categorical))
    return ColumnTransformer(transformers, remainder="drop", sparse_threshold=0.0)


def _fit_mlp(spec, params, x, y, weight, seed, n_jobs):
    """Execute a fixed epoch budget; neither random validation nor early stop."""
    if "sample_weight" not in inspect.signature(MLPClassifier.fit).parameters:
        raise RuntimeError("This benchmark requires native MLP sample_weight support")
    epochs = int(params["epochs"])
    if epochs < 1 or len(params["hidden_layer_sizes"]) < 2 or params["alpha"] <= 0:
        raise ValueError("The MLP requires at least two hidden layers, positive regularization and epochs")
    estimator = MLPClassifier(
        hidden_layer_sizes=tuple(params["hidden_layer_sizes"]), activation="relu",
        solver="adam", alpha=params["alpha"], batch_size=min(128, len(x)),
        learning_rate_init=0.001, max_iter=epochs, shuffle=True,
        early_stopping=False, tol=0.0, n_iter_no_change=epochs + 1,
        random_state=seed,
    )
    fitted = Pipeline([
        ("beverages", dm.BeverageTransform()),
        ("preprocess", _preprocessor(spec["features"])), ("model", estimator),
    ])
    started = time.monotonic()
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always", ConvergenceWarning)
        fitted.fit(x, y, model__sample_weight=np.asarray(weight, float) / np.mean(weight))
    curve = np.asarray(estimator.loss_curve_, float)
    if (estimator.n_iter_ != epochs or len(curve) != epochs or not np.isfinite(curve).all()
            or any(not np.isfinite(a).all() for a in estimator.coefs_ + estimator.intercepts_)):
        raise RuntimeError("MLP did not complete its fixed budget with finite parameters and losses")
    messages = [{"category": type(w.message).__name__, "message": str(w.message)} for w in caught]
    unexpected = [m for m in messages if m["category"] != "ConvergenceWarning"
                  or "Maximum iterations" not in m["message"]]
    if unexpected:
        raise RuntimeError(f"Unexpected MLP fitting warning: {unexpected}")
    diagnostic = {
        "epochs_requested": epochs, "epochs_completed": int(estimator.n_iter_),
        "optimizer_loss_initial": float(curve[0]), "optimizer_loss_final": float(curve[-1]),
        "optimizer_loss_minimum": float(curve.min()),
        "last_10_epoch_loss_change": float(curve[-1] - curve[max(0, len(curve) - 11)]),
        "optimizer_loss_curve": json.dumps(curve.tolist()),
        "optimizer_status": "fixed_epoch_budget_completed",
        "convergence_claim": "No tolerance-based convergence claim; epochs selected by inner validation",
        "convergence_warning_count": len(messages), "warnings": json.dumps(messages, sort_keys=True),
        "n_input_columns": int(estimator.n_features_in_),
        "n_parameters": int(sum(a.size for a in estimator.coefs_ + estimator.intercepts_)),
        "fit_seconds": time.monotonic() - started,
    }
    return fitted, diagnostic


def _validate_importance(table, name, spec, grouped=False):
    key, units = ("group", spec["groups"]) if grouped else ("feature", spec["features"])
    rows = table.loc[table.model.eq(name)]
    expected = Counter((fold, unit, repeat) for fold in range(1, dm.OUTER_FOLDS + 1)
                       for unit in units for repeat in range(1, PERMUTATION_REPEATS + 1))
    if (Counter(zip(rows.fold, rows[key], rows["repeat"])) != expected
            or not np.isfinite(rows[["brier_increase", "auc_drop", "test_weight_sum"]]).all().all()
            or rows.test_weight_sum.le(0).any()):
        raise ValueError(f"Incomplete or invalid retained permutation importance: {name}")


def _validate_source(frame, results):
    """Validate all inherited inputs, candidate choices, probabilities and splits."""
    source_specs = dm._model_specs()
    names = _source_names()
    all_features = list(dict.fromkeys(f for n in names for f in source_specs[n]["features"]))
    domain = dm._prepare_domain(frame, all_features)
    manifest = results["manifest"]
    settings = manifest["settings"]
    if (settings["source_hashes"] != dm._source_hashes()
            or settings["seed"] != SEED or settings["outer_folds"] != dm.OUTER_FOLDS
            or settings["inner_folds"] != dm.INNER_FOLDS
            or manifest["n_participants"] != len(domain)
            or manifest["n_events"] != int(domain.caries.sum())):
        raise ValueError("Dietary source hashes, cohort or validation settings changed")
    software = {"sklearn": sklearn.__version__, "pandas": pd.__version__,
                "numpy": np.__version__, "scipy": scipy.__version__}
    if manifest["software"] != software:
        raise ValueError("Source reconstruction requires the original software versions")
    y, psu = domain.caries.to_numpy(int), domain.psu.to_numpy()
    outer = models._group_splits(y, psu, dm.OUTER_FOLDS, SEED, "all-predictor source outer")
    inner = {}
    for fold, (train, test) in enumerate(outer, 1):
        stored = results["splits"]["outer"][fold - 1]
        if not all(np.array_equal(a, b) for a, b in zip((train, test), stored, strict=True)):
            raise ValueError("Original outer PSU split changed")
        inner[fold] = models._group_splits(y[train], psu[train], dm.INNER_FOLDS,
                                          SEED + fold * 101, "all-predictor source inner")
        for actual, saved in zip(inner[fold], results["splits"]["inner"][fold], strict=True):
            if not all(np.array_equal(a, b) for a, b in zip(actual, saved, strict=True)):
                raise ValueError("Original inner PSU split changed")
    saved_oof = {}
    for name in names:
        recorded = manifest["models"][name]
        spec = {k: v for k, v in recorded.items() if k != "fingerprint"}
        if spec != source_specs[name] or dm._fingerprint(domain, spec, settings) != recorded["fingerprint"]:
            raise ValueError(f"Stale dietary input/specification fingerprint: {name}")
        saved = results["oof"].loc[results["oof"].model.eq(name)].copy()
        if saved.row_index.duplicated().any() or set(saved.row_index) != set(domain.index):
            raise ValueError("Retained OOF rows must uniquely cover the eligible domain")
        saved = saved.set_index("row_index").loc[domain.index]
        for a, b in [("participant_id", "participant_id"), ("y", "caries"),
                     ("weight", "weight"), ("psu", "psu"), ("stratum", "stratum")]:
            if not np.array_equal(saved[a].to_numpy(), domain[b].to_numpy()):
                raise ValueError("Retained OOF identity, outcome or survey design mismatch")
        if not np.isfinite(saved.p).all() or not saved.p.between(0, 1).all():
            raise ValueError("Invalid retained OOF probabilities")
        scores = results["foldscores"].loc[results["foldscores"].model.eq(name)]
        if len(scores) != dm.OUTER_FOLDS or scores.fold.duplicated().any():
            raise ValueError("Expected one retained score per model and outer fold")
        for fold, (train, test) in enumerate(outer, 1):
            if not saved.iloc[test].fold.eq(fold).all():
                raise ValueError("Retained OOF fold assignments changed")
            row = scores.loc[scores.fold.eq(fold)].iloc[0]
            tuning = results["tuning"].loc[results["tuning"].model.eq(name) & results["tuning"].fold.eq(fold)]
            if (len(tuning) != len(spec["candidates"]) or tuning.selected.sum() != 1
                    or not np.isfinite(tuning.inner_weighted_log_loss).all()):
                raise ValueError("Invalid retained inner candidate search")
            selected = json.loads(row.selected_params)
            winner = tuning.loc[tuning.selected].iloc[0]
            if (selected not in spec["candidates"] or selected != json.loads(winner.params)
                    or winner.inner_weighted_log_loss != tuning.inner_weighted_log_loss.min()):
                raise ValueError("Retained parameters were not selected by inner validation")
            metrics = models._metrics(y[test], saved.p.to_numpy()[test], domain.weight.to_numpy()[test])
            if (any(not np.isclose(value, row[k], rtol=0, atol=1e-10) for k, value in metrics.items())
                    or row.n_train != len(train) or row.n_test != len(test)
                    or not np.isclose(row.test_weight_sum, domain.weight.iloc[test].sum(), rtol=0, atol=1e-8)):
                raise ValueError("Retained OOF probabilities and fold metrics disagree")
        if spec["compute_importance"]:
            _validate_importance(results["importance"], name, spec)
            _validate_importance(results["grouped_importance"], name, spec, True)
        saved_oof[name] = saved
    # Bind inherited predictions and selected settings into each new cache.
    reference_digest = sha256()
    for key in ["oof", "tuning", "foldscores", "importance", "grouped_importance"]:
        table = results[key].loc[results[key].model.isin(names)]
        reference_digest.update(pd.util.hash_pandas_object(table, index=True).to_numpy().tobytes())
    return domain, source_specs, outer, inner, saved_oof, reference_digest.hexdigest()


def _load_cache(path, fingerprint, test, name, fold, spec, mlp):
    try:
        envelope = json.loads(path.read_text())
        payload = envelope["payload"]
        if envelope["sha256"] != dm._payload_checksum(payload):
            return None
        p = np.asarray(payload["probabilities"], float)
        if (payload["fingerprint"] != fingerprint or payload["test_positions"] != test.tolist()
                or payload["model"] != name or payload["fold"] != fold
                or p.shape != (len(test),) or not np.isfinite(p).all() or ((p < 0) | (p > 1)).any()):
            return None
        for grouped, field in [(False, "importance"), (True, "grouped_importance")]:
            table = pd.DataFrame(payload[field])
            key, units = ("group", spec["groups"]) if grouped else ("feature", spec["features"])
            expected = Counter((unit, repeat) for unit in units for repeat in range(1, PERMUTATION_REPEATS + 1))
            if (Counter(zip(table[key], table["repeat"])) != expected or not table.model.eq(name).all()
                    or not table.fold.eq(fold).all() or not np.isfinite(table[["brier_increase", "auc_drop"]]).all().all()):
                return None
        if mlp and (len(payload["tuning"]) != len(spec["candidates"])
                    or len(payload["diagnostics"]) != len(spec["candidates"]) * dm.INNER_FOLDS + 1):
            return None
        return payload
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return None


def run_all_predictor_models(frame, cache_dir, dietary_results, n_jobs=3, plan_path=None):
    """Return eight paired full-information variants, with all-model importance.

    The caller must verify its private dietary result-file checksum before
    unpickling. This routine additionally authenticates its inputs, source code,
    software, model specifications, inner choices, OOF identities and splits.
    It never writes to dietary outputs or serializes a fitted estimator.
    """
    if n_jobs < 1:
        raise ValueError("n_jobs must be positive")
    started = time.monotonic()
    plan_path = Path(plan_path) if plan_path else Path(__file__).resolve().parents[2] / "docs/all-predictor-analysis-plan.md"
    if not plan_path.is_file():
        raise ValueError("Freeze the all-predictor analysis plan before fitting")
    plan_hash = sha256(plan_path.read_bytes()).hexdigest()
    domain, source_specs, outer, inner, saved_oof, reference_hash = _validate_source(frame, dietary_results)
    mlp_specs = _mlp_specs()
    settings = {
        "seed": SEED, "outer_folds": dm.OUTER_FOLDS, "inner_folds": dm.INNER_FOLDS,
        "permutation_repeats": PERMUTATION_REPEATS, "cache_version": CACHE_VERSION,
        "plan_sha256": plan_hash, "numerical_thread_limit": n_jobs,
        "selection": "pooled_inner_validation_weighted_log_loss", "source_hashes": _source_hashes(),
        "software": dietary_results["manifest"]["software"], "reference_results_sha256": reference_hash,
        "beverage_log1p": list(dm.BEVERAGE_FEATURES), "early_stopping": False,
        "optimizer": "Adam", "batch_size": 128, "learning_rate_init": 0.001,
        "epoch_stopping": "Fixed budget; tol=0 and n_iter_no_change=epochs+1",
        "reproduction_absolute_tolerance": REPRODUCTION_ATOL,
        "preprocessing": "Training-only median imputation and standardization of numeric predictors; all numeric missing indicators; categorical Missing plus one-hot, unseen levels ignored",
        "weighting": "Training weights normalized to mean one; pooled validation log loss and held-out metrics use survey weights",
    }
    names = _source_names()
    result = {key: dietary_results[key].loc[dietary_results[key].model.isin(names)].copy()
              for key in ["oof", "tuning", "foldscores", "importance", "grouped_importance"]}
    for key in ["split_qc", "fold_assignments"]:
        result[key] = dietary_results[key].copy()
    result["splits"] = {"outer": deepcopy(outer), "inner": deepcopy(inner)}
    specs = {name: deepcopy(dietary_results["manifest"]["models"][name]) for name in names}
    y, weight = domain.caries.to_numpy(int), domain.weight.to_numpy(float)
    destination = Path(cache_dir)
    destination.mkdir(parents=True, exist_ok=True)
    diagnostics, recovery, appended = [], [], {k: [] for k in result if isinstance(result[k], pd.DataFrame)}
    cache_hits = fitted_mlp_folds = recovered_boosting_folds = 0
    work = {name: source_specs[name] for name in names if source_specs[name]["kind"] == "hist_gradient_boosting"} | mlp_specs
    with threadpool_limits(limits=n_jobs):
        for name, spec in work.items():
            is_mlp = spec["kind"] == MLP_FAMILY
            x = dm._feature_frame(domain, spec["features"])
            fingerprint = dm._fingerprint(domain, spec, settings)
            if is_mlp:
                specs[name] = {**deepcopy(spec), "fingerprint": fingerprint}
            for fold, (train, test) in enumerate(outer, 1):
                path = destination / name / f"outer_{fold}.json"
                payload = _load_cache(path, fingerprint, test, name, fold, spec, is_mlp)
                loaded = payload is not None
                if loaded:
                    cache_hits += 1
                    if not is_mlp:
                        actual = saved_oof[name].p.to_numpy()[test]
                        if (not np.array_equal(actual, np.asarray(payload["probabilities"]))
                                or payload["reconstruction"]["status"] != "verified"
                                or payload["reconstruction"]["maximum_absolute_probability_difference"] > REPRODUCTION_ATOL):
                            raise ValueError("Cached boosting reconstruction differs from authenticated OOF")
                else:
                    fold_started = time.monotonic()
                    tuning, training_records = [], []
                    if is_mlp:
                        for candidate, params in enumerate(spec["candidates"]):
                            inner_p = np.full(len(train), np.nan)
                            losses = []
                            for inner_fold, (a, b) in enumerate(inner[fold], 1):
                                fitted, diagnostic = _fit_mlp(spec, params, x.iloc[train[a]], y[train[a]],
                                    weight[train[a]], SEED + fold * 1000 + inner_fold, n_jobs)
                                inner_p[b] = models._probabilities(fitted, x.iloc[train[b]])
                                losses.append(models._metrics(y[train[b]], inner_p[b], weight[train[b]])["log_loss"])
                                training_records.append({**diagnostic, "model": name, "fold": fold,
                                    "candidate": candidate, "inner_fold": inner_fold, "phase": "inner",
                                    "params": json.dumps(params, sort_keys=True), "n_train": len(a)})
                            loss = models._metrics(y[train], inner_p, weight[train])["log_loss"]
                            tuning.append({"model": name, "fold": fold, "candidate": candidate,
                                "params": json.dumps(params, sort_keys=True), "inner_weighted_log_loss": loss,
                                "inner_fold_log_losses": json.dumps(losses)})
                        winner = min(range(len(tuning)), key=lambda i: tuning[i]["inner_weighted_log_loss"])
                        for i, row in enumerate(tuning):
                            row["selected"] = i == winner
                        selected = spec["candidates"][winner]
                        fitted, diagnostic = _fit_mlp(spec, selected, x.iloc[train], y[train], weight[train],
                                                      SEED + fold * 1000, n_jobs)
                        training_records.append({**diagnostic, "model": name, "fold": fold,
                            "candidate": winner, "inner_fold": None, "phase": "outer",
                            "params": json.dumps(selected, sort_keys=True), "n_train": len(train)})
                        p = models._probabilities(fitted, x.iloc[test])
                        score = {"model": name, "fold": fold, "family": MLP_FAMILY,
                            "feature_set": spec["feature_set"], "n_features": len(spec["features"]),
                            **models._metrics(y[test], p, weight[test]), "n_train": len(train), "n_test": len(test),
                            "test_weight_sum": float(weight[test].sum()), "selected_params": json.dumps(selected, sort_keys=True),
                            "weighted_training": True, "converged": None,
                            "optimizer_status": diagnostic["optimizer_status"],
                            "epochs_completed": diagnostic["epochs_completed"],
                            "beverage_training_caps": "{}", "elapsed_seconds": time.monotonic() - fold_started}
                        reproduced = None
                        fitted_mlp_folds += 1
                    else:
                        saved_score = result["foldscores"].loc[result["foldscores"].model.eq(name)
                                                               & result["foldscores"].fold.eq(fold)].iloc[0]
                        selected = json.loads(saved_score.selected_params)
                        fitted = dm._fit_estimator(spec, selected, x.iloc[train], y[train], weight[train],
                                                   SEED + fold * 1000, n_jobs)
                        recreated = models._probabilities(fitted, x.iloc[test])
                        p = saved_oof[name].p.to_numpy()[test]
                        maximum_error = float(np.max(np.abs(recreated - p)))
                        if maximum_error > REPRODUCTION_ATOL:
                            raise RuntimeError(f"Boosting OOF reconstruction failed for {name}, fold {fold}: {maximum_error}")
                        reproduced = {"model": name, "fold": fold, "maximum_absolute_probability_difference": maximum_error,
                            "tolerance": REPRODUCTION_ATOL, "n_test": len(test), "params": json.dumps(selected, sort_keys=True),
                            "retuned": False, "status": "verified", "reconstruction_seconds": time.monotonic() - fold_started}
                        score = None
                        recovered_boosting_folds += 1
                    individual, grouped = dm._permutation_tables(fitted, x.iloc[test], y[test], weight[test],
                                                                 p, name, fold, spec["groups"])
                    payload = {"fingerprint": fingerprint, "model": name, "fold": fold,
                        "test_positions": test.tolist(), "probabilities": p.tolist(), "tuning": tuning,
                        "scores": score, "importance": individual, "grouped_importance": grouped,
                        "diagnostics": training_records, "reconstruction": reproduced}
                    models._json_write(path, {"payload": payload, "sha256": dm._payload_checksum(payload)})
                if is_mlp:
                    oof = domain.iloc[test][dm.DESIGN_COLUMNS].copy()
                    oof.insert(0, "row_index", domain.index.to_numpy()[test])
                    oof = oof.rename(columns={"caries": "y"})
                    oof["model"], oof["fold"], oof["p"] = name, fold, payload["probabilities"]
                    appended["oof"].append(oof.reset_index(drop=True))
                    appended["tuning"].append(pd.DataFrame(payload["tuning"]))
                    appended["foldscores"].append(pd.DataFrame([payload["scores"]]))
                    diagnostics.extend(payload["diagnostics"])
                else:
                    recovery.append(payload["reconstruction"])
                appended["importance"].append(pd.DataFrame(payload["importance"]))
                appended["grouped_importance"].append(pd.DataFrame(payload["grouped_importance"]))
                print(f"{name}: outer {fold}/{dm.OUTER_FOLDS} complete" + (" (cache)" if loaded else ""), flush=True)
    for key, tables in appended.items():
        if tables:
            result[key] = pd.concat([result[key], *tables], ignore_index=True)
    if (len(result["oof"]) != len(domain) * len(specs)
            or result["oof"].groupby(["model", "participant_id"]).size().ne(1).any()
            or result["oof"].groupby("participant_id").fold.nunique().ne(1).any()):
        raise AssertionError("Each model must cover each eligible participant once on common folds")
    for name, spec in specs.items():
        _validate_importance(result["importance"], name, spec)
        _validate_importance(result["grouped_importance"], name, spec, True)
    if _source_hashes() != settings["source_hashes"] or sha256(plan_path.read_bytes()).hexdigest() != plan_hash:
        raise RuntimeError("Model source or analysis plan changed during the benchmark")
    result["training_diagnostics"] = pd.DataFrame(diagnostics)
    result["reconstruction_checks"] = pd.DataFrame(recovery)
    result["manifest"] = {
        "settings": settings, "models": specs, "n_participants": len(domain), "n_events": int(y.sum()),
        "n_psu": int(domain.psu.nunique()), "n_strata": int(domain.stratum.nunique()),
        "source_hashes": settings["source_hashes"], "software": settings["software"],
        "cache_hits": cache_hits, "fitted_mlp_model_folds": fitted_mlp_folds,
        "reconstructed_boosting_model_folds": recovered_boosting_folds,
        "retained_variants": names, "new_variants": list(mlp_specs),
        "expected_new_model_folds": len(work) * dm.OUTER_FOLDS,
        "elapsed_seconds": time.monotonic() - started,
        "notes": [
            "Exploratory, dataset-informed benchmark; the reused folds are not a new external validation sample.",
            "All six retained variants preserve original OOF probabilities, tuning, scores and sample weights.",
            "Boosting is reconstructed with its original selected parameters and seed only for held-out importance; OOF reproduction is checked before permutation.",
            "The MLP has two hidden ReLU layers; six bounded architecture/regularization/epoch candidates are selected by pooled inner survey-weighted log loss.",
            "Numeric medians and unweighted standardization, categorical levels and Missing encoding are learned within each training split; no spline expansion is applied to the MLP.",
            "MLP training uses mean-one survey weights with Adam mini-batches; no resampling, balancing or class-weight adjustment.",
            "Epochs are fixed within each candidate; there is no early stopping or random held-out validation within a training fit. Budget warnings do not establish failed optimization or convergence.",
            "Optimizer losses include regularization and are training diagnostics, not validation performance; finite coefficients and all requested epochs are required.",
            "All four families have individual and joint-block raw held-out permutation importance with identical deterministic donor seeds and five repeats.",
            "Importance is fitted-model reliance, not a causal effect; marginal shuffling can break predictor dependence. Fold ranges are not confidence intervals.",
            "The 44-construct symptom scenario classifies concurrent disease using information potentially affected by disease; it does not predict future incidence.",
            "No target-dependent feature screening or post-result expansion of the fixed grid is performed.",
        ],
    }
    models._json_write(destination / "model_manifest.json", result["manifest"])
    return result
