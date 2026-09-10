"""Compare historical and modern coding in the same complete-case RF cohort.

Both representations are refitted with the original outer-training-selected
parameters. This is a post hoc representation-package comparison, not an exact
reproduction of the historical caret fit or a new hyperparameter search.
"""
from __future__ import annotations

from collections import Counter
from hashlib import sha256
from pathlib import Path
import json
import time

import numpy as np
import pandas as pd
import sklearn
from threadpoolctl import threadpool_limits

from . import contributions, evaluation, models


MODERN = "rf_complete_modern"
HISTORICAL = "rf_complete_historical"
REPRESENTATIONS = {
    MODERN: "Modern coding, complete cohort",
    HISTORICAL: "Historical 90-term coding, complete cohort",
}
PERMUTATION_REPEATS = 5


def _digest(value):
    return sha256(json.dumps(value, sort_keys=True, allow_nan=False).encode()).hexdigest()


def _hash_frame(frame):
    digest = sha256(pd.util.hash_pandas_object(frame, index=True).values.tobytes())
    digest.update(json.dumps(list(frame.columns)).encode())
    digest.update(str(frame.dtypes.to_dict()).encode())
    return digest.hexdigest()


def _validate_original(frame, results):
    """Authenticate source inputs and inherit, rather than redraw, outer folds."""
    for flag in ("eligible", "paper_complete"):
        if flag not in frame or frame[flag].isna().any() or not frame[flag].isin([True, False]).all():
            raise ValueError(f"A complete boolean {flag} flag is required")
    if (frame.paper_complete & ~frame.eligible).any():
        raise ValueError("The complete cohort must be a subset of the eligible cohort")
    if not frame.index.is_unique or frame.participant_id.isna().any() or frame.participant_id.duplicated().any():
        raise ValueError("Unique row indices and participant identifiers are required")
    source = frame.loc[frame.eligible.astype(bool)].copy()
    cohort = frame.loc[frame.paper_complete.astype(bool)].copy()
    if (source.empty or cohort.empty
            or source[["psu", "stratum", "caries", "weight"]].isna().any().any()
            or not np.isfinite(source.weight).all() or source.weight.le(0).any()
            or set(source.caries.unique()) != {0, 1}):
        raise ValueError("Observed binary outcomes and positive finite survey weights are required")
    design = frame.loc[frame.weight.gt(0), ["stratum", "psu"]].copy()
    if design.isna().any().any() or (design.groupby("psu").stratum.nunique() > 1).any():
        raise ValueError("Invalid full positive-weight survey design")
    settings = results["manifest"]["settings"]
    original = results["manifest"]["models"]["random_forest"]
    spec = {k: v for k, v in original.items() if k != "fingerprint"}
    if spec["kind"] != "random_forest" or not spec["weighted_training"]:
        raise ValueError("The weighted primary random forest is required")
    fingerprint = models._fingerprint(source, spec["features"],
                                     {**settings, "model": "random_forest", "spec": spec})
    if fingerprint != original["fingerprint"] or results["manifest"]["n_participants"] != len(source):
        raise ValueError("Stale primary random-forest source fingerprint")
    saved = results["oof"].loc[results["oof"].model.eq("random_forest")].copy()
    if saved.row_index.duplicated().any() or set(saved.row_index) != set(source.index):
        raise ValueError("Primary OOF rows do not match the full eligible cohort")
    saved = saved.set_index("row_index").loc[source.index]
    if (not np.array_equal(saved.participant_id.to_numpy(), source.participant_id.to_numpy())
            or not np.array_equal(saved.y.to_numpy(), source.caries.to_numpy())
            or not np.array_equal(saved.weight.to_numpy(), source.weight.to_numpy())
            or not np.array_equal(saved.psu.to_numpy(), source.psu.to_numpy())
            or not np.array_equal(saved.stratum.to_numpy(), source.stratum.to_numpy())
            or not np.isfinite(saved.p).all() or not saved.p.between(0, 1).all()):
        raise ValueError("Primary OOF identifiers, outcomes, design or probabilities changed")
    seed, n_folds = int(settings["seed"]), int(settings["outer_folds"])
    outer = models._group_splits(source.caries.to_numpy(int), source.psu.to_numpy(),
                                 n_folds, seed, "historical comparison source folds")
    scores = results["foldscores"].loc[results["foldscores"].model.eq("random_forest")].copy()
    if len(scores) != n_folds or scores.fold.duplicated().any():
        raise ValueError("Exactly one primary parameter selection per outer fold is required")
    selections = {}
    for fold, (_, test) in enumerate(outer, 1):
        expected = saved.iloc[test]
        if not expected.fold.eq(fold).all():
            raise ValueError("Primary outer fold assignments changed")
        row = scores.loc[scores.fold.eq(fold)]
        if len(row) != 1:
            raise ValueError("Missing primary outer-fold parameter selection")
        params = json.loads(row.selected_params.iloc[0])
        if params not in spec["candidates"]:
            raise ValueError("Saved parameters are outside the original candidate grid")
        actual = models._metrics(expected.y.to_numpy(), expected.p.to_numpy(), expected.weight.to_numpy())
        if any(not np.isclose(actual[k], row[k].iloc[0], rtol=0, atol=1e-10) for k in actual):
            raise ValueError("Primary probabilities and saved fold metrics disagree")
        selections[fold] = params
    assignment = saved.loc[cohort.index, "fold"].to_numpy(int)
    splits = []
    for fold in range(1, n_folds + 1):
        train, test = np.flatnonzero(assignment != fold), np.flatnonzero(assignment == fold)
        if (set(cohort.psu.iloc[train]).intersection(cohort.psu.iloc[test])
                or set(cohort.caries.iloc[train].unique()) != {0, 1}
                or set(cohort.caries.iloc[test].unique()) != {0, 1}):
            raise ValueError("Restricted folds require two classes and disjoint PSUs")
        splits.append((train, test))
    if cohort[spec["features"]].isna().any().any():
        raise ValueError("The historical complete cohort must have all predictors observed")
    return source, cohort, design, spec, settings, selections, splits, saved, scores


def _align_historical(cohort, historical_X, column_map, features):
    """Use participant IDs, never the Stata or SAV row order, for alignment."""
    if (not isinstance(historical_X, pd.DataFrame) or not historical_X.index.is_unique
            or historical_X.index.isna().any() or not historical_X.columns.is_unique
            or set(historical_X.index) != set(cohort.participant_id)):
        raise ValueError("Historical matrix must uniquely match complete-cohort participant IDs")
    mapping = pd.DataFrame(column_map).copy()
    if (not {"encoded_column", "predictor"}.issubset(mapping)
            or mapping[["encoded_column", "predictor"]].isna().any().any()
            or mapping.encoded_column.duplicated().any()
            or set(mapping.encoded_column) != set(historical_X.columns)
            or set(mapping.predictor) != set(features)):
        raise ValueError("Column map must cover every encoded column and original predictor exactly")
    mapping = mapping.set_index("encoded_column").loc[historical_X.columns].rename_axis("encoded_column").reset_index()
    x = historical_X.loc[cohort.participant_id].apply(pd.to_numeric, errors="raise").astype(float)
    if not np.isfinite(x.to_numpy()).all():
        raise ValueError("Historical matrix must contain finite complete numeric values")
    x.index = cohort.index
    groups = {feature: mapping.loc[mapping.predictor.eq(feature), "encoded_column"].tolist()
              for feature in features}
    contributions.validate_groups(list(x.columns), groups)
    return x, mapping, groups


def _fit_variant(name, spec, params, x, y, weight, psu, seed, n_jobs):
    if name == MODERN:
        return models._fit_estimator(spec, params, x, y, weight, psu, seed, n_jobs)
    # Reuse the same estimator constructor; the already verified historical
    # matrix needs neither modern imputation nor another categorical encoder.
    fitted = models._pipeline(spec, params, seed, n_jobs).named_steps["model"]
    return fitted.fit(x, y, sample_weight=weight / weight.mean())


def _modern_columns(fitted):
    """Map transformed columns without guessing from underscore-split labels."""
    pre = fitted.named_steps["preprocess"]
    names = list(pre.get_feature_names_out())
    predictors = []
    for name, transformer, columns in pre.transformers_:
        if name == "remainder":
            continue
        if name == "numeric":
            predictors.extend(columns)
        elif name == "numeric_missing":
            predictors.extend([columns[i] for i in transformer.features_])
        elif name == "categorical":
            encoder = transformer.named_steps["onehot"]
            if encoder.drop is not None:
                raise ValueError("Unexpected reference dropping in modern encoder")
            for column, levels in zip(columns, encoder.categories_, strict=True):
                predictors.extend([column] * len(levels))
        else:
            raise ValueError(f"Unexpected modern RF transformer: {name}")
    if len(names) != len(predictors) or len(set(names)) != len(names):
        raise ValueError("Transformed modern column map is ambiguous")
    return pd.DataFrame({"encoded_column": names, "predictor": predictors})


def _scale_100(values):
    shifted = np.asarray(values, float) - np.min(values)
    return shifted / shifted.max() * 100 if shifted.max() > 0 else np.zeros(len(shifted))


def _mdi_records(fitted, name, mapping, features, fold, x_train, test_weight_sum):
    if name == MODERN:
        mapping = _modern_columns(fitted)
        estimator = fitted.named_steps["model"]
        transformed = fitted.named_steps["preprocess"].transform(x_train)
    else:
        estimator = fitted
        transformed = x_train.to_numpy()
    values = np.asarray(estimator.feature_importances_, float)
    if (len(values) != len(mapping) or not np.isfinite(values).all() or (values < 0).any()
            or not (np.isclose(values.sum(), 1) or np.isclose(values.sum(), 0))):
        raise ValueError("Invalid normalized impurity importance or column map")
    columns = mapping[["encoded_column", "predictor"]].rename(columns={"predictor": "feature"}).copy()
    columns["mdi"] = values
    columns["training_constant"] = np.ptp(transformed, axis=0) == 0
    if columns.loc[columns.training_constant, "mdi"].gt(1e-12).any():
        raise ValueError("Constant training columns have nonzero impurity importance")
    columns["mdi_scaled"] = _scale_100(values)
    columns["rank"] = columns.mdi.rank(ascending=False, method="min")
    grouped = columns.groupby("feature", sort=False).agg(
        mdi=("mdi", "sum"), n_encoded_columns=("encoded_column", "size")).reindex(features).reset_index()
    if grouped.mdi.isna().any() or not np.isclose(grouped.mdi.sum(), values.sum()):
        raise ValueError("Original-predictor impurity sums do not conserve column importance")
    grouped["mdi_scaled"] = _scale_100(grouped.mdi)
    grouped["rank"] = grouped.mdi.rank(ascending=False, method="min")
    for table in (columns, grouped):
        table["model"], table["fold"], table["test_weight_sum"] = name, fold, test_weight_sum
    return columns.to_dict("records"), grouped.to_dict("records")


def _permutations(fitted, x, y, weight, p, groups, name, fold, seed):
    raw = contributions.grouped_permutation(fitted, x, y, weight, p, groups, name, fold,
                                            PERMUTATION_REPEATS, seed)
    return [{**{k: v for k, v in row.items() if k not in ("block", "n_features")},
             "feature": row["block"], "n_input_columns": row["n_features"]} for row in raw]


def _summarize_mdi(records):
    output = []
    for (name, feature), rows in records.groupby(["model", "feature"], sort=False):
        output.append({"model": name, "feature": feature,
                       "mdi": float(np.average(rows.mdi, weights=rows.test_weight_sum)),
                       "minimum_fold_mdi": float(rows.mdi.min()),
                       "maximum_fold_mdi": float(rows.mdi.max()),
                       "median_rank": float(rows["rank"].median()),
                       "best_rank": float(rows["rank"].min()), "worst_rank": float(rows["rank"].max()),
                       "fraction_top3": float(rows["rank"].le(3).mean()),
                       "min_encoded_columns": int(rows.n_encoded_columns.min()),
                       "max_encoded_columns": int(rows.n_encoded_columns.max())})
    summary = pd.DataFrame(output)
    summary["mdi_scaled"] = summary.groupby("model").mdi.transform(_scale_100)
    summary["mean_rank"] = summary.groupby("model").mdi.rank(ascending=False, method="min")
    summary["mdi_mean"] = summary.mdi
    summary["rank"] = summary.mean_rank
    return summary


def _read_cache(path, fingerprint, test, features):
    try:
        payload = json.loads(path.read_text())
        checksum = payload.pop("payload_sha256", None)
        if checksum != _digest(payload) or payload["fingerprint"] != fingerprint:
            return None
        p = np.asarray(payload["probabilities"], float)
        importance = pd.DataFrame(payload["importance27"])
        mdi = pd.DataFrame(payload["mdi27"])
        columns = pd.DataFrame(payload["mdi_columns"])
        expected = Counter((f, r) for f in features for r in range(1, PERMUTATION_REPEATS + 1))
        if (payload["test_positions"] != test.tolist() or p.shape != (len(test),)
                or not np.isfinite(p).all() or ((p < 0) | (p > 1)).any()
                or Counter(zip(importance.feature, importance["repeat"])) != expected
                or not np.isfinite(importance[["brier_increase", "auc_drop"]]).all().all()
                or Counter(mdi.feature) != Counter(features)
                or columns.encoded_column.duplicated().any()
                or set(columns.feature) != set(features)
                or not np.isfinite(mdi.mdi).all() or not np.isfinite(columns.mdi).all()
                or (mdi.mdi < 0).any() or (columns.mdi < 0).any()
                or not np.isclose(mdi.mdi.sum(), columns.mdi.sum())):
            return None
        return payload
    except (OSError, ValueError, TypeError, KeyError, AttributeError):
        return None


def run_historical_comparison(frame, results, historical_X, column_map, cache_dir, *,
                              bootstrap_replicates=1000, n_jobs=3):
    """Return aggregate comparisons and ``oof`` records that MUST remain private.

    ``frame`` is the full survey frame, with boolean ``eligible`` and
    ``paper_complete`` flags. ``results`` is the saved primary model-results
    dictionary. ``historical_X`` has one row per complete participant, indexed
    by participant ID; its row order may differ from ``frame``. ``column_map``
    is a DataFrame or record list containing ``encoded_column`` and ``predictor``
    (modern predictor name), optionally with other historical-schema metadata.
    It comes from the separately validated historical-coding module.

    Both RF variants are trained in the complete cohort, using restrictions of
    the existing outer folds and their original selected RF parameters. Only
    private, checksummed fold caches are written; no estimator is persisted.
    """
    started = time.monotonic()
    if bootstrap_replicates < 1 or n_jobs < 1:
        raise ValueError("Positive bootstrap and job counts are required")
    source, cohort, design, spec, settings, selections, splits, saved, scores = _validate_original(frame, results)
    features = spec["features"]
    historical, mapping, historical_groups = _align_historical(cohort, historical_X, column_map, features)
    modern = models._feature_frame(cohort, features)
    seed = int(settings["seed"])
    y, weight, psu = cohort.caries.to_numpy(int), cohort.weight.to_numpy(float), cohort.psu.to_numpy()
    sources = {Path(module.__file__).name: sha256(Path(module.__file__).read_bytes()).hexdigest()
               for module in (models, evaluation, contributions)}
    sources[Path(__file__).name] = sha256(Path(__file__).read_bytes()).hexdigest()
    inputs = {
        "source_sha256": sources, "full_frame_sha256": _hash_frame(frame),
        "historical_matrix_sha256": _hash_frame(historical), "column_map_sha256": _hash_frame(mapping),
        "original_oof_sha256": _hash_frame(saved), "original_foldscores_sha256": _hash_frame(scores),
        "original_model_fingerprint": results["manifest"]["models"]["random_forest"]["fingerprint"],
        "selected_params_by_fold": selections, "seed": seed, "permutation_repeats": PERMUTATION_REPEATS,
        "bootstrap_replicates": bootstrap_replicates,
        "software": {"sklearn": sklearn.__version__, "pandas": pd.__version__, "numpy": np.__version__},
    }
    analysis_fingerprint = _digest(inputs)
    oof_parts, foldscores, importance, mdi_columns, mdi27 = [], [], [], [], []
    fits, cache_hits = 0, 0
    with threadpool_limits(limits=n_jobs):
        for name, x, groups in [(MODERN, modern, {f: [f] for f in features}),
                                (HISTORICAL, historical, historical_groups)]:
            for fold, (train, test) in enumerate(splits, 1):
                params = selections[fold]
                fold_fingerprint = _digest({"analysis": analysis_fingerprint, "model": name,
                    "fold": fold, "params": params, "train_positions": train.tolist(), "test_positions": test.tolist()})
                path = Path(cache_dir) / name / f"outer_{fold}.json"
                payload = _read_cache(path, fold_fingerprint, test, features)
                if payload is None:
                    fitted = _fit_variant(name, spec, params, x.iloc[train], y[train], weight[train],
                                          psu[train], seed + fold * 1000, n_jobs)
                    p = models._probabilities(fitted, x.iloc[test])
                    columns, sums = _mdi_records(fitted, name, mapping, features, fold, x.iloc[train],
                                                 float(weight[test].sum()))
                    perms = _permutations(fitted, x.iloc[test], y[test], weight[test], p, groups, name, fold, seed)
                    score = {"model": name, "fold": fold, **models._metrics(y[test], p, weight[test]),
                             "n_train": len(train), "n_test": len(test),
                             "n_psu_train": len(np.unique(psu[train])), "n_psu_test": len(np.unique(psu[test])),
                             "psu_overlap": 0, "test_weight_sum": float(weight[test].sum()),
                             "selected_params": json.dumps(params, sort_keys=True), "weighted_training": True,
                             "n_encoded_columns": len(columns),
                             "n_constant_training_columns": sum(row["training_constant"] for row in columns)}
                    payload = {"fingerprint": fold_fingerprint, "test_positions": test.tolist(),
                               "probabilities": p.tolist(), "scores": score, "importance27": perms,
                               "mdi_columns": columns, "mdi27": sums}
                    models._json_write(path, {**payload, "payload_sha256": _digest(payload)})
                    fits += 1
                else:
                    cache_hits += 1
                check = models._metrics(y[test], np.asarray(payload["probabilities"]), weight[test])
                if any(not np.isclose(check[k], payload["scores"][k], atol=1e-12, rtol=0) for k in check):
                    raise ValueError("Cached historical-comparison metrics disagree with probabilities")
                part = cohort.iloc[test][["participant_id", "psu", "stratum", "caries", "weight"]].rename(columns={"caries": "y"})
                part.insert(0, "row_index", cohort.index.to_numpy()[test])
                oof_parts.append(part.assign(model=name, fold=fold, p=payload["probabilities"]).reset_index(drop=True))
                foldscores.append(payload["scores"])
                importance.extend(payload["importance27"])
                mdi_columns.extend(payload["mdi_columns"])
                mdi27.extend(payload["mdi27"])
                print(f"Historical coding: {name}, outer fold {fold}/{len(splits)} complete", flush=True)
    oof = pd.concat(oof_parts, ignore_index=True)
    if len(oof) != 2 * len(cohort) or oof.groupby(["model", "participant_id"]).size().ne(1).any():
        raise AssertionError("Both representations must evaluate every complete participant once")
    performance, bootstrap, _ = evaluation.evaluate_oof(oof, design, bootstrap_replicates, seed)
    comparisons = evaluation.paired_differences(bootstrap, performance, [
        (HISTORICAL, MODERN, "Historical coding minus modern coding, same complete cohort")])
    raw = pd.DataFrame(importance)
    summary = contributions.summarize_grouped_importance(
        raw.rename(columns={"feature": "block", "n_input_columns": "n_features"})).rename(
            columns={"block": "feature", "n_features": "n_input_columns"})
    grouped_mdi = pd.DataFrame(mdi27)
    add_label = lambda table: table.assign(representation=table.model.map(REPRESENTATIONS))
    manifest = {
        "status": "complete", "scope": "Post hoc historical-versus-modern coding in the same complete cohort",
        "analysis_fingerprint": analysis_fingerprint, **inputs,
        "n_original_eligible": len(source), "n_participants": len(cohort), "n_events": int(y.sum()),
        "n_predictors": len(features), "n_historical_columns": historical.shape[1],
        "n_psu": int(cohort.psu.nunique()), "n_strata": int(cohort.stratum.nunique()),
        "n_full_design_psu": int(design.psu.nunique()), "n_full_design_strata": int(design.stratum.nunique()),
        "outer_folds": len(splits), "outer_fits_required": 2 * len(splits),
        "outer_fits_this_call": fits, "cache_hits_this_call": cache_hits, "new_hyperparameter_searches": 0,
        "n_oof_rows_private": len(oof), "n_permutation_rows": len(raw), "model_names": list(REPRESENTATIONS),
        "bootstrap_valid_replicates": {name: {metric: int(np.isfinite(draws[:, j]).sum())
            for j, metric in enumerate(evaluation.BOOTSTRAP_METRICS)} for name, draws in bootstrap.items()},
        "elapsed_seconds": time.monotonic() - started,
        "notes": [
            "Both variants are newly fitted in the identical complete cohort. Primary probabilities and fitted models are not used as the modern complete-cohort reference.",
            "Outer folds are restrictions of the original eligible-cohort PSU assignments; no participant or PSU enters both training and validation in a fold.",
            "Parameters were selected using each original outer training cohort, which can include additional incomplete participants, and are transported unchanged to both representations. No outer validation outcomes inform that selection and no retuning is performed.",
            "Historical column vocabulary and reference contrasts are fixed source artifacts, verified separately against the original R matrix. They are not claimed to be learned within modern training folds.",
            "The historical coding package changes water indices, categorical fruit/vegetable days, reference contrasts and original beverage units/conversion. The comparison does not isolate each component or reproduce the historical unweighted caret forest.",
            "The modern pipeline learns its categorical vocabulary and preprocessing within training folds, retaining its explicit missing indicators, including constant ones in this complete cohort.",
            "Both forests use the same estimator settings and normalized survey fitting weights. Different input dimensionality changes feature-subsampling behaviour even when max_features is unchanged.",
            "MDI is normalized sklearn fitted-model impurity importance; its 0–100 min–max rescaling is descriptive and is not an exact reproduction of the original R score. Tied values share the minimum rank; an all-zero vector receives scaled zeros.",
            "MDI sums over all encoded columns of an original predictor conserve total MDI. They are not whole-predictor permutation importance, causal contributions or percentages of disease explained.",
            "Whole-predictor importance uses five common uniform donor-row permutations per outer test fold. All indicator columns of a predictor move together; outcomes and survey scoring weights stay fixed.",
            "Importance summaries average repeats within folds, then weight fold means by held-out expansion-weight totals. Fold ranges and ranks describe stability and are not confidence intervals. Negative permutation values are retained.",
            "Paired percentile intervals use shared stratified rescaled PSU bootstrap draws from the full positive-weight design, conditional on fixed fitted forests, parameters and folds. Singleton PSUs remain fixed; threshold metrics use 0.5.",
            "Only aggregate tables and this manifest are public outputs. OOF records, matrices, row positions and fold caches must remain private.",
        ],
    }
    return {"performance": add_label(performance), "comparisons": add_label(comparisons),
            "foldscores": add_label(pd.DataFrame(foldscores)), "importance27": add_label(raw),
            "importance27_summary": add_label(summary), "mdi_columns": add_label(pd.DataFrame(mdi_columns)),
            "mdi27": add_label(grouped_mdi), "mdi27_summary": add_label(_summarize_mdi(grouped_mdi)),
            "manifest": manifest, "oof": add_label(oof)}
