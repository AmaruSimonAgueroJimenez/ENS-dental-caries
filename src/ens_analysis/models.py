"""Grouped nested validation of concurrent caries classification models.

All preprocessing, tuning, and SVM probability calibration are confined to
training participants. Survey weights affect fitting where supported, selection,
and scoring; survey strata are retained for downstream uncertainty estimation.
"""

from __future__ import annotations

from dataclasses import dataclass
from hashlib import sha256
import json
import os
from pathlib import Path
import time
from typing import Any

import numpy as np
import pandas as pd
import sklearn
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import HistGradientBoostingClassifier, RandomForestClassifier
from sklearn.exceptions import ConvergenceWarning
from sklearn.impute import MissingIndicator, SimpleImputer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import brier_score_loss, log_loss, roc_auc_score
from sklearn.model_selection import ParameterGrid, StratifiedGroupKFold
from sklearn.neighbors import KNeighborsClassifier
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, SplineTransformer, StandardScaler
from sklearn.svm import SVC
from sklearn.tree import DecisionTreeClassifier
from threadpoolctl import threadpool_limits
import warnings


NUMERIC_FEATURES = {
    "age", "teeth_remaining", "fruit_days", "fruit_portions",
    "vegetable_days", "vegetable_portions", "water", "soda_frequency",
    "juice_frequency",
}
SPLINE_FEATURES = {"age", "teeth_remaining", "water"}
EXTRA_FEATURES = ["household_water_source", "rural_water_consumption", "dental_visit"]
DEFAULT_MODEL_NAMES = [
    "logistic", "spline_logistic", "random_forest", "hist_gradient_boosting",
    "svm", "decision_tree", "knn", "spline_base", "spline_no_water",
    "rf_no_water", "rf_base", "spline_extended", "spline_no_teeth",
]
_ALIASES = {
    "rf": "random_forest", "RF": "random_forest", "histboost": "hist_gradient_boosting",
    "hist_boost": "hist_gradient_boosting", "SVM": "svm", "KNN": "knn",
    "RF_no_water": "rf_no_water", "RF_base": "rf_base",
    "spline_full": "spline_logistic", "no_teeth": "spline_no_teeth",
}
_CACHE_VERSION = 1
PROBABILITY_LOG_EPSILON = 1e-7


def _feature_sets() -> tuple[list[str], list[str]]:
    from .data import DIET_FEATURES, SOCIO_FEATURES

    base = list(SOCIO_FEATURES)
    # Accommodate either a diet-only list or a full feature list without repeats.
    full = list(dict.fromkeys(base + list(DIET_FEATURES)))
    return base, full


def _model_specs() -> dict[str, dict[str, Any]]:
    base, full = _feature_sets()
    specs: dict[str, dict[str, Any]] = {}
    for name in DEFAULT_MODEL_NAMES:
        kind = {
            "spline_base": "spline_logistic", "spline_no_water": "spline_logistic",
            "rf_no_water": "random_forest", "rf_base": "random_forest",
            "spline_extended": "spline_logistic", "spline_no_teeth": "spline_logistic",
        }.get(name, name)
        features = list(full)
        if name in {"spline_base", "rf_base"}:
            features = list(base)
        elif name in {"spline_no_water", "rf_no_water"}:
            features = [f for f in full if f != "water"]
        elif name == "spline_extended":
            features = list(dict.fromkeys(full + EXTRA_FEATURES))
        elif name == "spline_no_teeth":
            features = [f for f in full if f != "teeth_remaining"]
        grids = {
            "logistic": {"C": [0.0001, 0.001, 0.01, 0.1, 1.0, 10.0]},
            "spline_logistic": {"C": [0.0001, 0.001, 0.01, 0.1, 1.0, 10.0]},
            "random_forest": {"min_samples_leaf": [5, 20, 50, 100], "max_features": ["sqrt", 0.7]},
            "hist_gradient_boosting": {"max_leaf_nodes": [3, 7, 15], "l2_regularization": [1.0, 10.0, 100.0]},
            "svm": {"C": [0.1, 1.0, 10.0]},
            "decision_tree": {"min_samples_leaf": [20, 50, 100], "max_depth": [1, 2, 3, 6]},
            "knn": {"n_neighbors": [25, 75, 150, 300]},
        }
        specs[name] = {
            "kind": kind, "features": features, "candidates": list(ParameterGrid(grids[kind])),
            "weighted_training": kind != "knn",
            "role": "algorithm_comparison" if name in DEFAULT_MODEL_NAMES[:7] else "ablation_or_sensitivity",
        }
    return specs


def _feature_frame(frame: pd.DataFrame, features: list[str]) -> pd.DataFrame:
    """Apply deterministic type conversion, with no learned preprocessing."""
    x = frame.loc[:, features].copy()
    for name in features:
        if name in NUMERIC_FEATURES:
            x[name] = pd.to_numeric(x[name], errors="raise").astype(float)
            if np.isinf(x[name].to_numpy()).any():
                raise ValueError(f"Infinite numeric predictor: {name}")
        else:
            # Ensure categories never mix numeric and string objects in sklearn.
            x[name] = x[name].map(lambda v: np.nan if pd.isna(v) else str(v)).astype(object)
    return x


def _preprocessor(features: list[str], spline: bool, scale: bool) -> ColumnTransformer:
    numeric = [f for f in features if f in NUMERIC_FEATURES]
    splines = [f for f in numeric if spline and f in SPLINE_FEATURES]
    linear = [f for f in numeric if f not in splines]
    categorical = [f for f in features if f not in numeric]
    transformers: list[tuple] = []
    if linear:
        steps = [("impute", SimpleImputer(strategy="median", keep_empty_features=True))]
        if scale:
            steps.append(("scale", StandardScaler()))
        transformers.append(("numeric", Pipeline(steps), linear))
    if splines:
        steps = [
            ("impute", SimpleImputer(strategy="median", keep_empty_features=True)),
            ("spline", SplineTransformer(n_knots=3, degree=3, knots="quantile",
                                          include_bias=False, extrapolation="linear")),
            ("scale", StandardScaler()),
        ]
        transformers.append(("splines", Pipeline(steps), splines))
    if numeric:
        # One explicit indicator per numeric variable, also for newly missing values.
        transformers.append(("numeric_missing", MissingIndicator(features="all", error_on_new=False), numeric))
    if categorical:
        transformers.append(("categorical", Pipeline([
            ("impute", SimpleImputer(strategy="constant", fill_value="Missing", keep_empty_features=True)),
            ("onehot", OneHotEncoder(handle_unknown="ignore", sparse_output=False)),
        ]), categorical))
    return ColumnTransformer(transformers, remainder="drop", sparse_threshold=0.0)


def _pipeline(spec: dict, params: dict, seed: int, n_jobs: int) -> Pipeline:
    kind = spec["kind"]
    if kind in {"logistic", "spline_logistic"}:
        estimator = LogisticRegression(max_iter=3000, solver="lbfgs", random_state=seed, **params)
    elif kind == "random_forest":
        estimator = RandomForestClassifier(n_estimators=200, n_jobs=n_jobs, random_state=seed, **params)
    elif kind == "hist_gradient_boosting":
        estimator = HistGradientBoostingClassifier(max_iter=150, early_stopping=False,
                                                   random_state=seed, **params)
    elif kind == "svm":
        estimator = SVC(kernel="rbf", gamma="scale", random_state=seed,
                        cache_size=512, **params)
    elif kind == "decision_tree":
        estimator = DecisionTreeClassifier(random_state=seed, **params)
    elif kind == "knn":
        estimator = KNeighborsClassifier(weights="distance", n_jobs=n_jobs, **params)
    else:
        raise ValueError(f"Unsupported estimator kind {kind}")
    return Pipeline([
        ("preprocess", _preprocessor(spec["features"], spline=kind == "spline_logistic",
                                     scale=kind in {"logistic", "spline_logistic", "svm", "knn"})),
        ("model", estimator),
    ])


def _group_splits(y: np.ndarray, groups: np.ndarray, n_splits: int, seed: int,
                  label: str) -> list[tuple[np.ndarray, np.ndarray]]:
    if n_splits < 2 or len(np.unique(groups)) < n_splits:
        raise ValueError(f"{label}: need at least {n_splits} distinct PSUs and at least two folds")
    splitter = StratifiedGroupKFold(n_splits=n_splits, shuffle=True, random_state=seed)
    splits = list(splitter.split(np.zeros(len(y)), y, groups))
    seen = np.zeros(len(y), dtype=int)
    for fold, (train, test) in enumerate(splits):
        overlap = set(groups[train]).intersection(groups[test])
        if overlap:
            raise AssertionError(f"{label} fold {fold}: PSU leakage: {sorted(overlap, key=str)[:5]}")
        if len(np.unique(y[train])) != 2 or len(np.unique(y[test])) != 2:
            raise ValueError(f"{label} fold {fold}: both outcome classes required in train and validation; "
                             f"train={np.bincount(y[train], minlength=2).tolist()}, "
                             f"validation={np.bincount(y[test], minlength=2).tolist()}")
        seen[test] += 1
    if not np.all(seen == 1):
        raise AssertionError(f"{label}: each participant must be validated exactly once")
    return splits


def _fit_pipeline(pipeline: Pipeline, x: pd.DataFrame, y: np.ndarray,
                  weight: np.ndarray, weighted: bool) -> Pipeline:
    kwargs = {"model__sample_weight": weight / weight.mean()} if weighted else {}
    with warnings.catch_warnings():
        # A non-converged model must not quietly enter the final OOF predictions.
        warnings.simplefilter("error", ConvergenceWarning)
        pipeline.fit(x, y, **kwargs)
    return pipeline


@dataclass
class _PlattModel:
    estimator: Pipeline
    calibrator: LogisticRegression

    def predict_proba(self, x: pd.DataFrame) -> np.ndarray:
        score = self.estimator.decision_function(x).reshape(-1, 1)
        return self.calibrator.predict_proba(score)


def _fit_estimator(spec: dict, params: dict, x: pd.DataFrame, y: np.ndarray,
                   weight: np.ndarray, groups: np.ndarray, seed: int, n_jobs: int) -> Any:
    if spec["kind"] != "svm":
        return _fit_pipeline(_pipeline(spec, params, seed, n_jobs), x, y, weight,
                             spec["weighted_training"])
    # Platt calibration uses PSU-disjoint OOF margins only inside this training set.
    n_calibration = min(3, len(np.unique(groups)))
    splits = _group_splits(y, groups, n_calibration, seed + 7919, "SVM calibration")
    margins = np.full(len(y), np.nan)
    for fold, (train, heldout) in enumerate(splits):
        fitted = _fit_pipeline(_pipeline(spec, params, seed + fold, n_jobs), x.iloc[train],
                               y[train], weight[train], True)
        margins[heldout] = fitted.decision_function(x.iloc[heldout])
    if not np.isfinite(margins).all():
        raise ValueError("SVM calibration produced nonfinite OOF margins")
    calibrator = LogisticRegression(C=1e6, solver="lbfgs", max_iter=3000, random_state=seed)
    with warnings.catch_warnings():
        warnings.simplefilter("error", ConvergenceWarning)
        calibrator.fit(margins.reshape(-1, 1), y, sample_weight=weight / weight.mean())
    fitted = _fit_pipeline(_pipeline(spec, params, seed, n_jobs), x, y, weight, True)
    return _PlattModel(fitted, calibrator)


def _probabilities(fitted: Any, x: pd.DataFrame) -> np.ndarray:
    p = np.asarray(fitted.predict_proba(x)[:, 1], dtype=float)
    if not np.isfinite(p).all() or ((p < 0) | (p > 1)).any():
        raise ValueError("Nonfinite or out-of-range model probabilities")
    return p


def _metrics(y: np.ndarray, p: np.ndarray, weight: np.ndarray) -> dict[str, float]:
    return {
        "auc": float(roc_auc_score(y, p, sample_weight=weight)),
        "brier": float(brier_score_loss(y, p, sample_weight=weight)),
        "log_loss": float(log_loss(y, np.clip(p, PROBABILITY_LOG_EPSILON, 1 - PROBABILITY_LOG_EPSILON),
                                    sample_weight=weight, labels=[0, 1])),
    }


def _json_write(path: Path, payload: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f"{path.name}.{os.getpid()}.tmp")
    temporary.write_text(json.dumps(payload, indent=2, allow_nan=False), encoding="utf-8")
    temporary.replace(path)


def _fingerprint(frame: pd.DataFrame, features: list[str], settings: dict) -> str:
    data = frame[["participant_id", "psu", "stratum", "caries", "weight"] + features]
    digest = sha256()
    digest.update(pd.util.hash_pandas_object(data, index=True).values.tobytes())
    digest.update(json.dumps(settings, sort_keys=True).encode())
    digest.update(Path(__file__).read_bytes())
    digest.update(f"{_CACHE_VERSION}/{sklearn.__version__}/{pd.__version__}/{np.__version__}".encode())
    return digest.hexdigest()


def _split_record(frame: pd.DataFrame, train: np.ndarray, test: np.ndarray,
                   level: str, outer_fold: int, inner_fold: int | None) -> dict:
    a, b = frame.iloc[train], frame.iloc[test]
    return {
        "level": level, "outer_fold": outer_fold, "inner_fold": inner_fold,
        "n_train": len(train), "n_test": len(test),
        "psu_train": int(a.psu.nunique()), "psu_test": int(b.psu.nunique()),
        "psu_overlap": len(set(a.psu).intersection(b.psu)),
        "n_events_train": int(a.caries.sum()), "n_events_test": int(b.caries.sum()),
    }


def _permutation_importance(fitted: Any, x: pd.DataFrame, y: np.ndarray,
                             weight: np.ndarray, p: np.ndarray, name: str,
                             fold: int, repeats: int, seed: int) -> list[dict]:
    baseline = _metrics(y, p, weight)
    records = []
    for feature_index, feature in enumerate(x.columns):
        for repeat in range(repeats):
            rng = np.random.default_rng(seed + fold * 10007 + feature_index * 101 + repeat)
            permuted = x.copy()
            permuted[feature] = x[feature].to_numpy()[rng.permutation(len(x))]
            result = _metrics(y, _probabilities(fitted, permuted), weight)
            records.append({
                "model": name, "fold": fold, "feature": feature, "repeat": repeat + 1,
                "brier_increase": result["brier"] - baseline["brier"],
                "auc_drop": baseline["auc"] - result["auc"],
                "n_test": len(y), "test_weight_sum": float(weight.sum()),
            })
    return records


def run_nested_models(
    frame: pd.DataFrame,
    output_dir: str | Path,
    seed: int = 20260910,
    outer_folds: int = 5,
    inner_folds: int = 3,
    n_jobs: int = 3,
    model_names: list[str] | None = None,
    permutation_repeats: int = 5,
) -> dict[str, Any]:
    """Return OOF predictions, tuning, scores, importance, and auditable metadata.

    ``frame`` must already be the intended study domain. Required columns are
    ``participant_id, psu, stratum, caries, weight`` plus selected predictors.
    Missing predictors are allowed; missing outcomes, weights, or identifiers
    fail. The frame index is retained as ``row_index`` in the output.

    Completed model/fold results resume from JSON caches only when data, code,
    feature sets, tuning grids, software versions, and settings fingerprints
    match. No trained estimator or participant predictors are persisted.
    """
    started = time.monotonic()
    if n_jobs < 1 or permutation_repeats < 0:
        raise ValueError("n_jobs must be positive and permutation_repeats nonnegative")
    specifications = _model_specs()
    names = [_ALIASES.get(n, n) for n in (model_names or DEFAULT_MODEL_NAMES)]
    if len(names) != len(set(names)):
        raise ValueError("model_names contains duplicates or duplicate aliases")
    unknown = set(names).difference(specifications)
    if unknown:
        raise ValueError(f"Unknown models: {sorted(unknown)}")
    all_features = list(dict.fromkeys(f for name in names for f in specifications[name]["features"]))
    required = ["participant_id", "psu", "stratum", "caries", "weight"] + all_features
    missing = set(required).difference(frame.columns)
    if missing:
        raise ValueError(f"Missing input columns: {sorted(missing)}")
    if not frame.index.is_unique or frame.participant_id.duplicated().any():
        raise ValueError("Frame index and participant_id must both be unique")
    metadata = frame[["participant_id", "psu", "stratum", "caries", "weight"]]
    if metadata.isna().any().any():
        raise ValueError(f"Missing required outcome/design values: {metadata.isna().sum().to_dict()}")
    if set(frame.caries.unique()) != {0, 1}:
        raise ValueError("caries must contain exactly binary classes 0 and 1")
    weight = frame.weight.to_numpy(dtype=float)
    if not np.isfinite(weight).all() or (weight <= 0).any():
        raise ValueError("Survey weights must be finite and strictly positive")
    if (frame.groupby("psu").stratum.nunique() > 1).any():
        raise ValueError("PSU identifiers span multiple strata; use globally unique nested PSU identifiers")
    y = frame.caries.to_numpy(dtype=int)
    groups = frame.psu.to_numpy()
    outer = _group_splits(y, groups, outer_folds, seed, "outer validation")
    inner: dict[int, list[tuple[np.ndarray, np.ndarray]]] = {}
    split_qc = []
    assignment = np.zeros(len(frame), dtype=int)
    for fold, (train, test) in enumerate(outer, start=1):
        assignment[test] = fold
        split_qc.append(_split_record(frame, train, test, "outer", fold, None))
        inner[fold] = _group_splits(y[train], groups[train], inner_folds,
                                    seed + fold * 101, f"outer {fold} inner validation")
        for inner_fold, (a, b) in enumerate(inner[fold], start=1):
            split_qc.append(_split_record(frame, train[a], train[b], "inner", fold, inner_fold))
    settings = {
        "seed": seed, "outer_folds": outer_folds, "inner_folds": inner_folds,
        "permutation_repeats": permutation_repeats,
        "selection": "pooled_inner_validation_weighted_log_loss",
        "probability_log_epsilon": PROBABILITY_LOG_EPSILON,
        "numeric_features": sorted(NUMERIC_FEATURES), "spline_features": sorted(SPLINE_FEATURES),
    }
    destination = Path(output_dir)
    destination.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(split_qc).to_csv(destination / "model_split_qc.csv", index=False)
    frame[["participant_id", "psu", "stratum"]].assign(fold=assignment).to_csv(
        destination / "model_fold_assignments.csv", index_label="row_index")
    all_oof, all_tuning, all_scores, all_importance = [], [], [], []
    cache_hits = 0
    manifest_models = {}
    with threadpool_limits(limits=n_jobs):
        for name in names:
            spec = specifications[name]
            x = _feature_frame(frame, spec["features"])
            fingerprint = _fingerprint(frame, spec["features"], {**settings, "model": name, "spec": spec})
            manifest_models[name] = {**spec, "fingerprint": fingerprint}
            for fold, (train, test) in enumerate(outer, start=1):
                cache_path = destination / "cache" / name / f"outer_{fold}.json"
                payload = None
                if cache_path.exists():
                    try:
                        candidate_cache = json.loads(cache_path.read_text(encoding="utf-8"))
                        cp = np.asarray(candidate_cache.get("probabilities", []), dtype=float)
                        if (candidate_cache.get("fingerprint") == fingerprint
                                and candidate_cache.get("test_positions") == test.tolist()
                                and len(cp) == len(test) and np.isfinite(cp).all()
                                and ((cp >= 0) & (cp <= 1)).all()
                                and all(k in candidate_cache for k in ["tuning", "scores", "importance"])):
                            payload = candidate_cache
                            cache_hits += 1
                    except (OSError, ValueError, TypeError):
                        payload = None
                loaded_from_cache = payload is not None
                if payload is None:
                    fold_started = time.monotonic()
                    tuning = []
                    try:
                        for candidate_index, params in enumerate(spec["candidates"]):
                            internal_p = np.full(len(train), np.nan)
                            scores = []
                            for inner_fold, (a, b) in enumerate(inner[fold], start=1):
                                fitted = _fit_estimator(spec, params, x.iloc[train[a]], y[train[a]],
                                                        weight[train[a]], groups[train[a]],
                                                        seed + fold * 1000 + inner_fold, n_jobs)
                                internal_p[b] = _probabilities(fitted, x.iloc[train[b]])
                                scores.append(_metrics(y[train[b]], internal_p[b], weight[train[b]])["log_loss"])
                            candidate_loss = _metrics(y[train], internal_p, weight[train])["log_loss"]
                            tuning.append({
                                "model": name, "fold": fold, "candidate": candidate_index,
                                "params": json.dumps(params, sort_keys=True),
                                "inner_weighted_log_loss": candidate_loss,
                                "inner_fold_log_losses": json.dumps(scores),
                            })
                        winner = min(range(len(tuning)), key=lambda i: tuning[i]["inner_weighted_log_loss"])
                        selected = spec["candidates"][winner]
                        for i, row in enumerate(tuning):
                            row["selected"] = i == winner
                        final_fit = _fit_estimator(spec, selected, x.iloc[train], y[train], weight[train],
                                                  groups[train], seed + fold * 1000, n_jobs)
                        p = _probabilities(final_fit, x.iloc[test])
                        importance = []
                        if name in {"random_forest", "spline_logistic"} and permutation_repeats:
                            importance = _permutation_importance(final_fit, x.iloc[test], y[test], weight[test],
                                                                 p, name, fold, permutation_repeats, seed)
                        score = {"model": name, "fold": fold, **_metrics(y[test], p, weight[test]),
                                 "n_train": len(train), "n_test": len(test),
                                 "test_weight_sum": float(weight[test].sum()),
                                 "selected_params": json.dumps(selected, sort_keys=True),
                                 "weighted_training": spec["weighted_training"],
                                 "elapsed_seconds": time.monotonic() - fold_started}
                        payload = {"fingerprint": fingerprint, "test_positions": test.tolist(),
                                   "probabilities": p.tolist(), "tuning": tuning,
                                   "scores": score, "importance": importance}
                        _json_write(cache_path, payload)
                    except Exception as exc:
                        diagnostic = {"model": name, "outer_fold": fold,
                                      "fingerprint": fingerprint, "error_type": type(exc).__name__,
                                      "error": str(exc), "completed_tuning": tuning}
                        _json_write(destination / "cache" / name / f"outer_{fold}_failure.json", diagnostic)
                        raise RuntimeError(f"Nested validation failed: model={name}, outer fold={fold}. "
                                           f"No participants or candidates were silently excluded. {exc}") from exc
                oof = frame.iloc[test][["participant_id", "psu", "stratum", "caries", "weight"]].copy()
                oof.insert(0, "row_index", frame.index.to_numpy()[test])
                oof = oof.rename(columns={"caries": "y"})
                oof["fold"], oof["model"], oof["p"] = fold, name, payload["probabilities"]
                all_oof.append(oof.reset_index(drop=True))
                all_tuning.extend(payload["tuning"])
                all_scores.append(payload["scores"])
                all_importance.extend(payload["importance"])
                print(f"{name}: outer fold {fold}/{outer_folds} complete "
                      f"(AUC={payload['scores']['auc']:.4f}, "
                      f"Brier={payload['scores']['brier']:.4f}, "
                      f"log loss={payload['scores']['log_loss']:.4f}" +
                      (", cache)" if loaded_from_cache else ")"), flush=True)
                _json_write(destination / "cache" / "checkpoint.json", {
                    "last_completed_model": name, "last_completed_fold": fold,
                    "completed_model_folds_this_call": len(all_scores),
                    "expected_model_folds": len(names) * outer_folds,
                    "cache_hits_this_call": cache_hits,
                })
    oof = pd.concat(all_oof, ignore_index=True)
    if oof.groupby(["model", "participant_id"]).size().ne(1).any():
        raise AssertionError("OOF output is not exactly one prediction per participant and model")
    manifest = {
        "settings": settings, "models": manifest_models, "n_participants": len(frame),
        "n_psu": int(frame.psu.nunique()), "n_strata": int(frame.stratum.nunique()),
        "software": {"sklearn": sklearn.__version__, "pandas": pd.__version__, "numpy": np.__version__},
        "cache_hits": cache_hits, "elapsed_seconds": time.monotonic() - started,
        "notes": [
            "The supplied analytical domain is not changed by the model engine.",
            "All algorithms use identical outer and inner PSU-disjoint folds.",
            "Fitting weights are normalized to mean one within each training sample.",
            "KNN does not support sample weights and is an unweighted-training benchmark; selection and scoring remain weighted.",
            "SVM uses three-fold grouped OOF Platt calibration within each training sample, then refits its SVC on that training sample.",
            "Spline knots and median imputation are learned only within the applicable training fold.",
            "Permutation importance permutes raw held-out variables; it has no causal or directional interpretation.",
            "No class balancing, synthetic oversampling, or outcome-based predictor selection is used.",
            "Fold scores are descriptive; survey-aware uncertainty should use the OOF table and original design.",
        ],
    }
    _json_write(destination / "model_manifest.json", manifest)
    return {
        "oof": oof,
        "tuning": pd.DataFrame(all_tuning),
        "foldscores": pd.DataFrame(all_scores),
        "importance": pd.DataFrame(all_importance, columns=[
            "model", "fold", "feature", "repeat", "brier_increase", "auc_drop", "n_test", "test_weight_sum",
        ]),
        "manifest": manifest,
        "split_qc": pd.DataFrame(split_qc),
    }
