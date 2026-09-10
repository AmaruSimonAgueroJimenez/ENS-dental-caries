"""Fixed dietary-increment comparisons with grouped, nested validation.

The primary family and feature-set comparisons are declared before this new
analysis. They do not replace or modify the earlier caries model caches.
``run_dietary_models`` returns the old results-table contract plus grouped
importance and private fold assignments. The supplied cache directory must be
private: its JSON files contain participant-level held-out probabilities.
"""

from __future__ import annotations

from copy import deepcopy
from hashlib import sha256
import json
from pathlib import Path
import time
from typing import Any

import numpy as np
import pandas as pd
import scipy
import sklearn
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.compose import ColumnTransformer
from sklearn.impute import MissingIndicator, SimpleImputer
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, SplineTransformer, StandardScaler
from sklearn.utils.validation import check_is_fitted
from threadpoolctl import threadpool_limits

from . import models as original_models


SEED = 20260910
OUTER_FOLDS = 5
INNER_FOLDS = 3
PERMUTATION_REPEATS = 5
CACHE_VERSION = 1
FAMILIES = ("spline_logistic", "random_forest", "hist_gradient_boosting")
BEVERAGE_FEATURES = (
    "soda_frequency", "juice_frequency", "soda_glasses_daily", "juice_glasses_daily",
)
IMPORTANCE_SETS = {"context_diet_labels", "symptoms_diet_labels"}
DESIGN_COLUMNS = ["participant_id", "psu", "stratum", "caries", "weight"]
FORBIDDEN_PREDICTORS = {
    "caries", "decayed_teeth", "dmft", "dmfs", "cpod", "m5p5", "m5p8",
    "participant_id", "psu", "stratum", "weight", "eligible", "paper_complete",
}
IMPORTANCE_COLUMNS = [
    "model", "fold", "feature", "repeat", "brier_increase", "auc_drop",
    "n_test", "test_weight_sum",
]
GROUP_IMPORTANCE_COLUMNS = [
    "model", "fold", "group", "n_features", "repeat", "brier_increase",
    "auc_drop", "n_test", "test_weight_sum",
]


def _numeric_features() -> set[str]:
    from .expanded_data import NUMERIC_FEATURES

    return set(NUMERIC_FEATURES)


def _feature_sets() -> tuple[dict[str, list[str]], dict[str, list[str]]]:
    from .data import SOCIO_FEATURES
    from .expanded_data import (
        CONTEXT_FEATURES, INTAKE_FEATURES, LABEL_FEATURES, SYMPTOM_FEATURES,
    )

    blocks = {
        "context": list(CONTEXT_FEATURES), "intake": list(INTAKE_FEATURES),
        "labels": list(LABEL_FEATURES), "symptoms": list(SYMPTOM_FEATURES),
    }
    expected = {"context": 17, "intake": 15, "labels": 6, "symptoms": 6}
    for name, block in blocks.items():
        if len(block) != expected[name] or len(set(block)) != len(block):
            raise ValueError(f"Incorrect fixed {name} block: expected {expected[name]} distinct predictors")
    all_features = [feature for block in blocks.values() for feature in block]
    if len(set(all_features)) != len(all_features):
        raise ValueError("Context, intake, labels and symptom blocks must be disjoint")
    if len(SOCIO_FEATURES) != 8 or not set(SOCIO_FEATURES).issubset(blocks["context"]):
        raise ValueError("The eight original basic predictors must be contained in context")
    c, d, l, s = (blocks[k] for k in ["context", "intake", "labels", "symptoms"])
    sets = {
        "basic": list(SOCIO_FEATURES), "context": c,
        "context_diet": c + d, "context_diet_labels": c + d + l,
        "symptoms": c + s, "symptoms_diet": c + s + d,
        "symptoms_diet_labels": c + s + d + l,
    }
    return sets, blocks


def _model_specs() -> dict[str, dict[str, Any]]:
    sets, blocks = _feature_sets()
    # Reuse the frozen existing candidate grids, without widening their search.
    originals = original_models._model_specs()
    specs: dict[str, dict[str, Any]] = {}
    for family in FAMILIES:
        for feature_set, features in sets.items():
            groups = {key: list(values) for key, values in blocks.items()
                      if set(values).issubset(features)}
            specs[f"{family}__{feature_set}"] = {
                "kind": family, "feature_set": feature_set,
                "features": list(features), "groups": groups,
                "candidates": deepcopy(originals[family]["candidates"]),
                "weighted_training": True, "winsor_positive_quantile": None,
                "role": "primary_family" if family == "spline_logistic" else "robustness_family",
                "compute_importance": family in {"spline_logistic", "random_forest"}
                and feature_set in IMPORTANCE_SETS,
            }
    for feature_set in ["context_diet", "symptoms_diet"]:
        spec = deepcopy(specs[f"spline_logistic__{feature_set}"])
        spec.update(winsor_positive_quantile=0.99, role="beverage_extreme_sensitivity",
                    compute_importance=False)
        specs[f"spline_logistic__{feature_set}__winsor99"] = spec
    return specs


def _validate_specs(specs: dict[str, dict]) -> None:
    if not specs:
        raise ValueError("No dietary model specifications")
    for name, spec in specs.items():
        features = spec["features"]
        forbidden = {f for f in features if f.lower() in FORBIDDEN_PREDICTORS}
        if forbidden:
            raise ValueError(f"Outcome/design leakage in {name}: {sorted(forbidden)}")
        if not features or len(set(features)) != len(features):
            raise ValueError(f"Predictors must be nonempty and unique: {name}")
        if spec["kind"] not in FAMILIES or not spec["candidates"]:
            raise ValueError(f"Unsupported family or empty candidate grid: {name}")
        if spec.get("compute_importance"):
            grouped = [f for group in spec["groups"].values() for f in group]
            if sorted(grouped) != sorted(features) or len(set(grouped)) != len(grouped):
                raise ValueError(f"Importance blocks must partition every predictor exactly once: {name}")


class BeverageTransform(TransformerMixin, BaseEstimator):
    """Optional training-positive p99 cap, then log1p, preserving zeros/NAs.

    The quantile is unweighted and uses strictly positive observed training
    values with linear interpolation. With no positive training observations,
    that column has no estimable cap and is only log-transformed. The subsequent
    preprocessing pipeline learns imputations from this transformed training
    sample. This estimator never examines outcomes or validation values in fit.
    """

    def __init__(self, positive_quantile: float | None = None):
        self.positive_quantile = positive_quantile

    @staticmethod
    def _validate(x: pd.DataFrame) -> None:
        if not isinstance(x, pd.DataFrame):
            raise TypeError("BeverageTransform requires a named pandas DataFrame")
        for feature in BEVERAGE_FEATURES:
            if feature in x:
                values = x[feature].to_numpy(dtype=float)
                if np.isinf(values).any() or np.any(values < 0):
                    raise ValueError(f"Beverage predictor must be nonnegative or missing: {feature}")

    def fit(self, x: pd.DataFrame, y: Any = None) -> BeverageTransform:
        self._validate(x)
        if self.positive_quantile is not None and not 0 < self.positive_quantile <= 1:
            raise ValueError("positive_quantile must lie in (0, 1]")
        self.feature_names_in_ = np.asarray(x.columns, dtype=object)
        self.n_features_in_ = len(x.columns)
        self.caps_: dict[str, float | None] = {}
        if self.positive_quantile is not None:
            for feature in BEVERAGE_FEATURES:
                if feature in x:
                    values = x[feature].to_numpy(dtype=float)
                    positive = values[np.isfinite(values) & (values > 0)]
                    self.caps_[feature] = (float(np.quantile(positive, self.positive_quantile,
                                                           method="linear"))
                                           if len(positive) else None)
        return self

    def transform(self, x: pd.DataFrame) -> pd.DataFrame:
        check_is_fitted(self, "caps_")
        self._validate(x)
        if list(x.columns) != list(self.feature_names_in_):
            raise ValueError("Predictor names/order changed after beverage-transform fitting")
        out = x.copy()
        for feature in BEVERAGE_FEATURES:
            if feature in out:
                cap = self.caps_.get(feature)
                if cap is not None:
                    out[feature] = out[feature].clip(upper=cap)
                out[feature] = np.log1p(out[feature])
        return out

    def get_feature_names_out(self, input_features: Any = None) -> np.ndarray:
        check_is_fitted(self, "feature_names_in_")
        return self.feature_names_in_.copy()


def _feature_frame(frame: pd.DataFrame, features: list[str]) -> pd.DataFrame:
    x = frame.loc[:, features].copy()
    numeric = _numeric_features()
    for feature in features:
        if feature in numeric:
            x[feature] = pd.to_numeric(x[feature], errors="raise").astype(float)
            if np.isinf(x[feature].to_numpy()).any():
                raise ValueError(f"Infinite numeric predictor: {feature}")
        else:
            x[feature] = x[feature].map(lambda v: np.nan if pd.isna(v) else str(v)).astype(object)
    BeverageTransform._validate(x)
    return x


def _preprocessor(features: list[str], spline: bool) -> ColumnTransformer:
    numeric = [f for f in features if f in _numeric_features()]
    splines = [f for f in numeric if spline and f in original_models.SPLINE_FEATURES]
    linear = [f for f in numeric if f not in splines]
    categorical = [f for f in features if f not in numeric]
    transformers = []
    if linear:
        steps = [("impute", SimpleImputer(strategy="median", keep_empty_features=True))]
        if spline:
            steps.append(("scale", StandardScaler()))
        transformers.append(("numeric", Pipeline(steps), linear))
    if splines:
        transformers.append(("splines", Pipeline([
            ("impute", SimpleImputer(strategy="median", keep_empty_features=True)),
            ("spline", SplineTransformer(n_knots=3, degree=3, knots="quantile",
                                         include_bias=False, extrapolation="linear")),
            ("scale", StandardScaler()),
        ]), splines))
    if numeric:
        transformers.append(("numeric_missing", MissingIndicator(features="all", error_on_new=False), numeric))
    if categorical:
        transformers.append(("categorical", Pipeline([
            ("impute", SimpleImputer(strategy="constant", fill_value="Missing", keep_empty_features=True)),
            ("onehot", OneHotEncoder(handle_unknown="ignore", sparse_output=False)),
        ]), categorical))
    return ColumnTransformer(transformers, remainder="drop", sparse_threshold=0.0)


def _pipeline(spec: dict, params: dict, seed: int, n_jobs: int) -> Pipeline:
    estimator = original_models._pipeline(spec, params, seed, n_jobs).named_steps["model"]
    return Pipeline([
        ("beverages", BeverageTransform(spec.get("winsor_positive_quantile"))),
        ("preprocess", _preprocessor(spec["features"], spec["kind"] == "spline_logistic")),
        ("model", estimator),
    ])


def _fit_estimator(spec: dict, params: dict, x: pd.DataFrame, y: np.ndarray,
                   weight: np.ndarray, seed: int, n_jobs: int) -> Pipeline:
    return original_models._fit_pipeline(_pipeline(spec, params, seed, n_jobs), x, y,
                                         weight, weighted=True)


def _permutation_tables(fitted: Any, x: pd.DataFrame, y: np.ndarray, weight: np.ndarray,
                        p: np.ndarray, name: str, fold: int, groups: dict[str, list[str]],
                        repeats: int = PERMUTATION_REPEATS) -> tuple[list[dict], list[dict]]:
    baseline = original_models._metrics(y, p, weight)
    tables: list[list[dict]] = [[], []]
    for table_index, units in enumerate([{f: [f] for f in x.columns}, groups]):
        for unit, columns in units.items():
            # A common donor permutation across models/folds of the same unit;
            # keep all columns in a group together, without moving y or weights.
            unit_seed = int.from_bytes(sha256(f"{table_index}/{unit}".encode()).digest()[:4], "little")
            for repeat in range(repeats):
                rng = np.random.default_rng(SEED + fold * 10007 + unit_seed + repeat)
                perturbed = x.copy()
                order = rng.permutation(len(x))
                perturbed.loc[:, columns] = x.loc[:, columns].iloc[order].to_numpy()
                metrics = original_models._metrics(y, original_models._probabilities(fitted, perturbed), weight)
                row = {
                    "model": name, "fold": fold, "repeat": repeat + 1,
                    "brier_increase": metrics["brier"] - baseline["brier"],
                    "auc_drop": baseline["auc"] - metrics["auc"],
                    "n_test": len(y), "test_weight_sum": float(weight.sum()),
                }
                if table_index:
                    row.update(group=unit, n_features=len(columns))
                else:
                    row["feature"] = unit
                tables[table_index].append(row)
    return tables[0], tables[1]


def _source_hashes() -> dict[str, str]:
    from . import data, expanded_data

    return {Path(module.__file__).name: sha256(Path(module.__file__).read_bytes()).hexdigest()
            for module in [original_models, data, expanded_data]} | {
                Path(__file__).name: sha256(Path(__file__).read_bytes()).hexdigest(),
            }


def _fingerprint(frame: pd.DataFrame, spec: dict, settings: dict) -> str:
    digest = sha256(pd.util.hash_pandas_object(
        frame[DESIGN_COLUMNS + spec["features"]], index=True).to_numpy().tobytes())
    digest.update(json.dumps({"spec": spec, "settings": settings}, sort_keys=True,
                             allow_nan=False).encode())
    return digest.hexdigest()


def _payload_checksum(payload: dict) -> str:
    return sha256(json.dumps(payload, sort_keys=True, separators=(",", ":"),
                             allow_nan=False).encode()).hexdigest()


def _load_cache(path: Path, fingerprint: str, test: np.ndarray, spec: dict,
                model: str, fold: int) -> dict | None:
    if not path.exists():
        return None
    try:
        envelope = json.loads(path.read_text())
        payload = envelope["payload"]
        if envelope["sha256"] != _payload_checksum(payload):
            return None
        probabilities = np.asarray(payload["probabilities"], dtype=float)
        if (payload["fingerprint"] != fingerprint or payload["test_positions"] != test.tolist()
                or len(probabilities) != len(test) or not np.isfinite(probabilities).all()
                or np.any((probabilities < 0) | (probabilities > 1))):
            return None
        if payload["scores"]["model"] != model or payload["scores"]["fold"] != fold:
            return None
        if len(payload["tuning"]) != len(spec["candidates"]):
            return None
        expected_individual = len(spec["features"]) * PERMUTATION_REPEATS if spec["compute_importance"] else 0
        expected_groups = len(spec["groups"]) * PERMUTATION_REPEATS if spec["compute_importance"] else 0
        if len(payload["importance"]) != expected_individual or len(payload["grouped_importance"]) != expected_groups:
            return None
        return payload
    except (OSError, ValueError, TypeError, KeyError):
        return None


def _prepare_domain(frame: pd.DataFrame, features: list[str]) -> pd.DataFrame:
    if not frame.index.is_unique or frame.columns.duplicated().any():
        raise ValueError("Frame index and columns must be unique")
    if "eligible" in frame:
        if frame.eligible.isna().any() or not frame.eligible.isin([True, False]).all():
            raise ValueError("eligible must contain only nonmissing booleans")
        domain = frame.loc[frame.eligible.astype(bool)].copy()
    else:
        domain = frame.copy()
    missing = set(DESIGN_COLUMNS + features).difference(domain.columns)
    if missing:
        raise ValueError(f"Missing input columns: {sorted(missing)}")
    if domain[DESIGN_COLUMNS].isna().any().any():
        raise ValueError("Missing required outcome/design values in eligible domain")
    if domain.participant_id.duplicated().any():
        raise ValueError("participant_id must be unique")
    if set(domain.caries.unique()) != {0, 1}:
        raise ValueError("caries must contain exactly binary classes 0 and 1")
    weight = domain.weight.to_numpy(dtype=float)
    if not np.isfinite(weight).all() or np.any(weight <= 0):
        raise ValueError("Survey weights must be finite and strictly positive")
    if domain.groupby("psu").stratum.nunique().gt(1).any():
        raise ValueError("PSU identifiers span multiple strata")
    return domain


def run_dietary_models(frame: pd.DataFrame, cache_dir: str | Path,
                       n_jobs: int = 3) -> dict[str, Any]:
    """Fit the fixed 21 comparisons and two positive-p99 sensitivities.

    ``frame`` may be the complete expanded survey frame (``eligible`` selects
    the dentate domain) or an already restricted domain. Row order and original
    indices are preserved; the original seed/group splitter therefore recreate
    the preceding five outer and three inner PSU splits on the same cohort.

    Return keys: ``oof``, ``tuning``, ``foldscores``, ``importance``,
    ``grouped_importance``, ``split_qc``, ``fold_assignments``, ``splits`` and
    ``manifest``. OOF, fold assignments and split positions are PRIVATE.
    Importance uses only the final outer-training fit, never a full-data fit.
    Caches resume only with matching inputs, parameters, module/software hashes
    and a valid payload checksum. No fitted estimators are serialized.
    """
    if n_jobs < 1:
        raise ValueError("n_jobs must be positive")
    started = time.monotonic()
    specs = _model_specs()
    _validate_specs(specs)
    features = list(dict.fromkeys(f for spec in specs.values() for f in spec["features"]))
    domain = _prepare_domain(frame, features)
    y = domain.caries.to_numpy(dtype=int)
    weight = domain.weight.to_numpy(dtype=float)
    psu = domain.psu.to_numpy()
    outer = original_models._group_splits(y, psu, OUTER_FOLDS, SEED, "dietary outer validation")
    inner, split_qc = {}, []
    assignments = np.zeros(len(domain), dtype=int)
    for fold, (train, test) in enumerate(outer, start=1):
        assignments[test] = fold
        split_qc.append(original_models._split_record(domain, train, test, "outer", fold, None))
        inner[fold] = original_models._group_splits(y[train], psu[train], INNER_FOLDS,
                                                   SEED + fold * 101, f"dietary outer {fold} inner validation")
        for inner_fold, (a, b) in enumerate(inner[fold], start=1):
            split_qc.append(original_models._split_record(domain, train[a], train[b], "inner", fold, inner_fold))
    software = {"sklearn": sklearn.__version__, "pandas": pd.__version__,
                "numpy": np.__version__, "scipy": scipy.__version__}
    settings = {
        "seed": SEED, "outer_folds": OUTER_FOLDS, "inner_folds": INNER_FOLDS,
        "permutation_repeats": PERMUTATION_REPEATS, "cache_version": CACHE_VERSION,
        "selection": "pooled_inner_validation_weighted_log_loss",
        "primary_family": "spline_logistic", "primary_comparison": "context_diet versus context",
        "primary_increment_metric": "reference_Brier_minus_expanded_Brier",
        "probability_log_epsilon": original_models.PROBABILITY_LOG_EPSILON,
        "numeric_features": sorted(_numeric_features()),
        "spline_features": sorted(original_models.SPLINE_FEATURES),
        "beverage_log1p": list(BEVERAGE_FEATURES),
        "winsor_quantile_population": "strictly positive observed training values",
        "source_hashes": _source_hashes(), "software": software,
    }
    destination = Path(cache_dir)
    destination.mkdir(parents=True, exist_ok=True)
    oof_tables, tuning, scores, importance, grouped_importance = [], [], [], [], []
    manifest_models = {}
    cache_hits, fitted_model_folds = 0, 0
    with threadpool_limits(limits=n_jobs):
        for name, spec in specs.items():
            x = _feature_frame(domain, spec["features"])
            fingerprint = _fingerprint(domain, spec, settings)
            manifest_models[name] = {**spec, "fingerprint": fingerprint}
            for fold, (train, test) in enumerate(outer, start=1):
                cache_path = destination / name / f"outer_{fold}.json"
                payload = _load_cache(cache_path, fingerprint, test, spec, name, fold)
                loaded = payload is not None
                if loaded:
                    cache_hits += 1
                else:
                    fold_started = time.monotonic()
                    fold_tuning = []
                    try:
                        for candidate, params in enumerate(spec["candidates"]):
                            inner_p = np.full(len(train), np.nan)
                            inner_losses = []
                            for inner_fold, (a, b) in enumerate(inner[fold], start=1):
                                fitted = _fit_estimator(spec, params, x.iloc[train[a]], y[train[a]],
                                                        weight[train[a]], SEED + fold * 1000 + inner_fold, n_jobs)
                                inner_p[b] = original_models._probabilities(fitted, x.iloc[train[b]])
                                inner_losses.append(original_models._metrics(y[train[b]], inner_p[b],
                                                                            weight[train[b]])["log_loss"])
                            loss = original_models._metrics(y[train], inner_p, weight[train])["log_loss"]
                            fold_tuning.append({"model": name, "fold": fold, "candidate": candidate,
                                                "params": json.dumps(params, sort_keys=True),
                                                "inner_weighted_log_loss": loss,
                                                "inner_fold_log_losses": json.dumps(inner_losses)})
                        winner = min(range(len(fold_tuning)), key=lambda i: fold_tuning[i]["inner_weighted_log_loss"])
                        selected = spec["candidates"][winner]
                        for i, row in enumerate(fold_tuning):
                            row["selected"] = i == winner
                        fitted = _fit_estimator(spec, selected, x.iloc[train], y[train], weight[train],
                                                SEED + fold * 1000, n_jobs)
                        p = original_models._probabilities(fitted, x.iloc[test])
                        individual, grouped = [], []
                        if spec["compute_importance"]:
                            individual, grouped = _permutation_tables(
                                fitted, x.iloc[test], y[test], weight[test], p, name, fold, spec["groups"])
                        score = {
                            "model": name, "fold": fold, "family": spec["kind"],
                            "feature_set": spec["feature_set"], "n_features": len(spec["features"]),
                            **original_models._metrics(y[test], p, weight[test]),
                            "n_train": len(train), "n_test": len(test),
                            "test_weight_sum": float(weight[test].sum()),
                            "selected_params": json.dumps(selected, sort_keys=True),
                            "weighted_training": True, "converged": True,
                            "elapsed_seconds": time.monotonic() - fold_started,
                            "beverage_training_caps": json.dumps(fitted.named_steps["beverages"].caps_, sort_keys=True),
                        }
                        payload = {"fingerprint": fingerprint, "test_positions": test.tolist(),
                                   "probabilities": p.tolist(), "tuning": fold_tuning, "scores": score,
                                   "importance": individual, "grouped_importance": grouped}
                        original_models._json_write(cache_path, {"payload": payload, "sha256": _payload_checksum(payload)})
                        fitted_model_folds += 1
                    except Exception as exc:
                        original_models._json_write(cache_path.with_name(f"outer_{fold}_failure.json"), {
                            "model": name, "fold": fold, "fingerprint": fingerprint,
                            "error_type": type(exc).__name__, "error": str(exc),
                            "completed_tuning": fold_tuning,
                        })
                        raise RuntimeError(f"Dietary nested validation failed: model={name}, fold={fold}. "
                                           f"No participants or candidates were silently excluded. {exc}") from exc
                oof = domain.iloc[test][DESIGN_COLUMNS].copy()
                oof.insert(0, "row_index", domain.index.to_numpy()[test])
                oof = oof.rename(columns={"caries": "y"})
                oof["model"], oof["fold"], oof["p"] = name, fold, payload["probabilities"]
                oof_tables.append(oof.reset_index(drop=True))
                tuning.extend(payload["tuning"])
                scores.append(payload["scores"])
                importance.extend(payload["importance"])
                grouped_importance.extend(payload["grouped_importance"])
                print(f"{name}: outer fold {fold}/{OUTER_FOLDS} complete"
                      + (" (cache)" if loaded else ""), flush=True)
                original_models._json_write(destination / "checkpoint.json", {
                    "last_completed_model": name, "last_completed_fold": fold,
                    "completed_model_folds_this_call": len(scores),
                    "expected_model_folds": len(specs) * OUTER_FOLDS,
                    "cache_hits_this_call": cache_hits,
                })
    oof = pd.concat(oof_tables, ignore_index=True)
    if (len(oof) != len(domain) * len(specs)
            or oof.groupby(["model", "participant_id"]).size().ne(1).any()
            or oof.groupby("participant_id").fold.nunique().ne(1).any()):
        raise AssertionError("OOF rows must cover each participant/model once with common outer folds")
    fold_assignments = domain[["participant_id", "psu", "stratum"]].copy()
    fold_assignments.insert(0, "row_index", domain.index.to_numpy())
    fold_assignments["fold"] = assignments
    manifest = {
        "settings": settings, "models": manifest_models, "n_participants": len(domain),
        "n_events": int(y.sum()), "n_psu": int(domain.psu.nunique()),
        "n_strata": int(domain.stratum.nunique()), "software": software,
        "source_hashes": settings["source_hashes"], "cache_hits": cache_hits,
        "fitted_model_folds": fitted_model_folds, "expected_model_folds": len(specs) * OUTER_FOLDS,
        "elapsed_seconds": time.monotonic() - started,
        "notes": [
            "The eligible domain preserves original participant order and index; no predictor complete-case exclusion.",
            "All variants share the original five outer and three inner PSU-disjoint folds and random seeds.",
            "All fitted models use training sample weights normalized to mean one; tuning uses pooled inner weighted log loss.",
            "Four beverage fields undergo log1p; only two declared sensitivity variants cap positive extremes at training p99 first.",
            "Zero and missing beverage values remain unchanged by capping; numeric imputation, scaling and spline knots are fitted only in training.",
            "No balancing, oversampling, outcome-derived predictors or outcome-based screening of individual predictors.",
            "Importance is raw held-out marginal permutation, with five repeats; each disjoint block is jointly permuted using one donor row.",
            "Individual permutations can break dependence between beverage frequency and derived volume; the intact intake block provides complementary context.",
            "Importance does not establish causality or protective direction; fold ranges are stability descriptions, not confidence intervals.",
            "Symptom models address concurrent classification with information that may reflect existing disease, not prediction of incidence.",
            "Cached probabilities and returned OOF, fold assignments and split positions are private participant-level artifacts.",
        ],
    }
    original_models._json_write(destination / "model_manifest.json", manifest)
    return {
        "oof": oof, "tuning": pd.DataFrame(tuning), "foldscores": pd.DataFrame(scores),
        "importance": pd.DataFrame(importance, columns=IMPORTANCE_COLUMNS),
        "grouped_importance": pd.DataFrame(grouped_importance, columns=GROUP_IMPORTANCE_COLUMNS),
        "split_qc": pd.DataFrame(split_qc), "fold_assignments": fold_assignments,
        "splits": {"outer": outer, "inner": inner}, "manifest": manifest,
    }
