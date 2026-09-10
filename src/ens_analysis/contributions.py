"""Exploratory contribution of predictor families to held-out classification.

Joint permutation preserves observed relationships inside each family. It
perturbs relationships with other families and is neither a causal effect nor
the incremental gain from fitting a model with that family removed.
"""
from __future__ import annotations

import json
from collections import Counter

import numpy as np
import pandas as pd
from threadpoolctl import threadpool_limits

from . import models
from .data import DIET_FEATURES, SOCIO_FEATURES


PREDICTOR_GROUPS = {
    "Sociodemographic": SOCIO_FEATURES[:-1],
    "Remaining teeth": ["teeth_remaining"],
    "Food intake": DIET_FEATURES[:9],
    "Label-related behaviours": [f for f in DIET_FEATURES if f.startswith("label_")],
    "Beverages": ["water", "soda_frequency", "juice_frequency"],
    "Cooking fat": ["oil"],
}


def validate_groups(features, groups):
    """Require a disjoint, exhaustive partition; fail rather than omit a field."""
    members = [feature for block in groups.values() for feature in block]
    if not groups or any(not block for block in groups.values()):
        raise ValueError("Predictor groups must be nonempty")
    if Counter(members) != Counter(features) or len(set(members)) != len(members):
        raise ValueError("Groups must cover every predictor exactly once")


def grouped_permutation(fitted, x, y, weight, baseline_probability, groups,
                        name, fold, repeats, seed):
    """Permute all columns of a family with the same uniform row permutation.

    Outcomes and scoring weights stay fixed. The donor distribution is the
    unweighted observed held-out sample; performance metrics use survey weights.
    The same donor permutation is used across models on identical held-out rows.
    """
    validate_groups(list(x.columns), groups)
    if repeats < 1:
        raise ValueError("At least one permutation repeat is required")
    baseline = models._metrics(y, baseline_probability, weight)
    records = []
    for block_index, (block, features) in enumerate(groups.items()):
        for repeat in range(repeats):
            rng = np.random.default_rng(seed + 199999 + fold * 10007 + block_index * 101 + repeat)
            order = rng.permutation(len(x))
            permuted = x.copy()
            for feature in features:
                permuted[feature] = x[feature].to_numpy()[order]
            score = models._metrics(y, models._probabilities(fitted, permuted), weight)
            records.append({
                "model": name, "fold": fold, "block": block, "repeat": repeat + 1,
                "n_features": len(features), "brier_increase": score["brier"] - baseline["brier"],
                "auc_drop": baseline["auc"] - score["auc"], "n_test": len(y),
                "test_weight_sum": float(np.sum(weight)),
            })
    return records


def summarize_grouped_importance(records):
    """Average repeats within folds, then use held-out weight totals across folds.

    Fold minima/maxima describe stability, not confidence intervals. In
    particular the mean fold AUC drop is not a pooled OOF AUC contrast.
    """
    folds = records.groupby(["model", "fold", "block"], as_index=False).agg(
        brier_increase=("brier_increase", "mean"), auc_drop=("auc_drop", "mean"),
        test_weight_sum=("test_weight_sum", "first"), n_features=("n_features", "first"))
    folds["rank"] = folds.groupby(["model", "fold"]).brier_increase.rank(ascending=False, method="min")
    output = []
    for (model, block), rows in folds.groupby(["model", "block"], sort=False):
        output.append({
            "model": model, "block": block, "n_features": int(rows.n_features.iloc[0]),
            "brier_increase": float(np.average(rows.brier_increase, weights=rows.test_weight_sum)),
            "auc_drop": float(np.average(rows.auc_drop, weights=rows.test_weight_sum)),
            "median_rank": float(rows["rank"].median()), "best_rank": float(rows["rank"].min()),
            "worst_rank": float(rows["rank"].max()), "fraction_top3": float(rows["rank"].le(3).mean()),
            "minimum_fold_brier_increase": float(rows.brier_increase.min()),
            "maximum_fold_brier_increase": float(rows.brier_increase.max()),
            "minimum_fold_auc_drop": float(rows.auc_drop.min()),
            "maximum_fold_auc_drop": float(rows.auc_drop.max()),
        })
    return pd.DataFrame(output)


def predictor_contributions(frame, results, n_jobs=3, groups=None,
                            model_names=("random_forest", "spline_logistic")):
    """Reconstruct the original outer fits and verify predictions before reuse.

    Original models persist predictions and selected parameters, not estimators.
    No hyperparameters are retuned here. A source fingerprint mismatch, changed
    fold assignment or a prediction mismatch aborts the contribution analysis.
    Returns aggregate tables and metadata only; never participant records.
    """
    groups = PREDICTOR_GROUPS if groups is None else groups
    settings = results["manifest"]["settings"]
    seed = int(settings["seed"])
    repeats = int(settings["permutation_repeats"])
    y = frame.caries.to_numpy(dtype=int)
    weight = frame.weight.to_numpy(dtype=float)
    psu = frame.psu.to_numpy()
    splits = models._group_splits(y, psu, settings["outer_folds"], seed, "contribution outer validation")
    records, checks, fingerprints = [], [], {}
    with threadpool_limits(limits=n_jobs):
        for name in model_names:
            original = results["manifest"]["models"][name]
            spec = {k: v for k, v in original.items() if k != "fingerprint"}
            fingerprint = models._fingerprint(frame, spec["features"], {**settings, "model": name, "spec": spec})
            if fingerprint != original["fingerprint"]:
                raise ValueError("Stale original model fingerprint in predictor contribution analysis")
            fingerprints[name] = fingerprint
            validate_groups(spec["features"], groups)
            x = models._feature_frame(frame, spec["features"])
            saved = results["oof"].loc[results["oof"].model.eq(name)].copy()
            if saved.row_index.duplicated().any() or set(saved.row_index) != set(frame.index):
                raise ValueError("Original held-out rows do not match the contribution cohort")
            saved = saved.set_index("row_index")
            for fold, (train, test) in enumerate(splits, start=1):
                test_frame = frame.iloc[test]
                expected = saved.loc[test_frame.index]
                if (not expected.fold.eq(fold).all()
                        or not np.array_equal(expected.participant_id.to_numpy(), test_frame.participant_id.to_numpy())
                        or not np.array_equal(expected.y.to_numpy(), y[test])
                        or not np.array_equal(expected.weight.to_numpy(), weight[test])):
                    raise ValueError("Original outcome, weights, identifiers or folds changed")
                selected = results["foldscores"].loc[
                    results["foldscores"].model.eq(name) & results["foldscores"].fold.eq(fold)]
                if len(selected) != 1:
                    raise ValueError("Exactly one saved parameter selection per model/fold is required")
                params = json.loads(selected.selected_params.iloc[0])
                if params not in spec["candidates"]:
                    raise ValueError("Saved parameters are outside the original candidate grid")
                fitted = models._fit_estimator(spec, params, x.iloc[train], y[train], weight[train],
                                               psu[train], seed + fold * 1000, n_jobs)
                p = models._probabilities(fitted, x.iloc[test])
                max_difference = float(np.max(np.abs(p - expected.p.to_numpy())))
                if not np.isfinite(max_difference) or max_difference > 1e-10:
                    raise ValueError(f"Reconstructed {name} fold {fold} predictions differ from original run")
                checks.append({"model": name, "fold": fold, "n_test": len(test),
                               "max_absolute_probability_difference": max_difference})
                records.extend(grouped_permutation(fitted, x.iloc[test], y[test], weight[test], p,
                                                    groups, name, fold, repeats, seed))
                print(f"Predictor families: {name}, verified outer fold {fold}/{len(splits)}", flush=True)
    raw = pd.DataFrame(records)
    manifest = {
        "status": "complete", "scope": "Exploratory predictor-contribution analysis",
        "seed": seed, "permutation_repeats": repeats, "model_names": list(model_names),
        "n_participants": len(frame), "groups": groups, "model_fingerprints": fingerprints,
        "refit_checks": checks, "prediction_match_tolerance": 1e-10,
        "grouping_status": "Conceptual grouping added after initial model results, 2026-09-10",
        "method": "Joint uniform held-out row permutation within predictor families, fixed outcomes and survey scoring weights",
        "aggregation": "Mean over repeats within fold, then mean over folds weighted by held-out expansion-weight totals",
        "uncertainty": "Minimum to maximum fold means are stability ranges, not confidence intervals",
        "interpretation": "Model-specific marginal perturbation; not a causal effect, conditional importance, refitted feature-ablation contrast, or additive percentage of explained disease",
    }
    return raw, summarize_grouped_importance(raw), manifest
