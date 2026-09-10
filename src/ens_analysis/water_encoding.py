"""Post hoc water-encoding sensitivity with fixed physical-model parameters.

The physical-reference predictions and water permutations are reused unchanged.
Only the two alternative encodings are fitted, on the same outer training folds.
This isolates representation under fixed hyperparameters; it is neither an
independently tuned comparison nor a reproduction of the historical R analysis.
"""
from __future__ import annotations

from copy import deepcopy
from hashlib import sha256
import json
from pathlib import Path
import time

import numpy as np
import pandas as pd
from threadpoolctl import threadpool_limits

from . import evaluation, models


BASE_MODELS = ("random_forest", "spline_logistic")
ENCODING_LABELS = {
    "physical": "Reported glasses/day",
    "index": "Historical factor index",
    "nominal": "Nominal water categories",
}
NOMINAL_FEATURE = "water_nominal"


def _hash_frame(frame):
    return sha256(pd.util.hash_pandas_object(frame, index=True).values.tobytes()).hexdigest()


def _digest(value):
    return sha256(json.dumps(value, sort_keys=True, allow_nan=False).encode()).hexdigest()


def _water_mapping(water_coding):
    """Validate the frozen historical mapping; never infer bins or new levels."""
    required = ["reported_glasses_per_day", "original_r_factor_index"]
    if not set(required).issubset(water_coding):
        raise ValueError("Historical water coding must contain physical values and factor indices")
    table = water_coding[required].apply(pd.to_numeric, errors="raise").sort_values(required[0])
    if (table.empty or not np.isfinite(table.to_numpy()).all()
            or table[required[0]].lt(0).any() or table[required[0]].duplicated().any()
            or not np.array_equal(table[required[1]].to_numpy(), np.arange(1, len(table)+1))):
        raise ValueError("Historical water mapping must have unique nonnegative levels and ordered indices 1..K")
    return dict(zip(table[required[0]].astype(float), table[required[1]].astype(float)))


def _encode_water(frame, original_spec, encoding, mapping):
    """Copy inputs, preserving water's raw-column position for shared permutations."""
    spec = deepcopy(original_spec)
    if spec["features"].count("water") != 1 or NOMINAL_FEATURE in frame:
        raise ValueError("Exactly one original water feature and no reserved nominal alias are required")
    values = pd.to_numeric(frame.water, errors="raise").astype(float)
    unknown = sorted(set(values.dropna()).difference(mapping))
    if unknown:
        raise ValueError(f"Water values absent from the frozen historical mapping: {unknown}")
    encoded = frame.copy()
    if encoding == "index":
        encoded["water"] = values.map(mapping)
    elif encoding == "nominal":
        # Naming is deterministic, not a learned vocabulary. The one-hot encoder
        # learns observed levels only from each outer training sample.
        encoded[NOMINAL_FEATURE] = values.map(lambda value: np.nan if pd.isna(value) else f"water={value:g}")
        spec["features"] = [NOMINAL_FEATURE if feature == "water" else feature for feature in spec["features"]]
    elif encoding != "physical":
        raise ValueError(f"Unknown water encoding: {encoding}")
    return encoded, spec


def _water_permutation(fitted, x, y, weight, p, name, fold, repeats, seed, water_position):
    """Use the original raw-water row permutation, including its missing values."""
    feature = x.columns[water_position]
    baseline = models._metrics(y, p, weight)
    records = []
    for repeat in range(repeats):
        rng = np.random.default_rng(seed + fold * 10007 + water_position * 101 + repeat)
        permuted = x.copy()
        permuted[feature] = x[feature].to_numpy()[rng.permutation(len(x))]
        score = models._metrics(y, models._probabilities(fitted, permuted), weight)
        records.append({
            "model": name, "fold": fold, "feature": "water", "repeat": repeat + 1,
            "brier_increase": score["brier"] - baseline["brier"],
            "auc_drop": baseline["auc"] - score["auc"], "n_test": len(y),
            "test_weight_sum": float(weight.sum()),
        })
    return records


def summarize_water_importance(records):
    """Weight fold means by held-out weight totals; ranges are not confidence intervals."""
    folds = records.groupby(["model", "fold"], as_index=False).agg(
        brier_increase=("brier_increase", "mean"), auc_drop=("auc_drop", "mean"),
        test_weight_sum=("test_weight_sum", "first"))
    output = []
    for name, rows in folds.groupby("model", sort=False):
        output.append({"model": name, "feature": "water", "n_folds": len(rows),
            **{metric: float(np.average(rows[metric], weights=rows.test_weight_sum))
               for metric in ("brier_increase", "auc_drop")},
            **{f"{bound}_fold_{metric}": float(getattr(rows[metric], operation)())
               for metric in ("brier_increase", "auc_drop")
               for bound, operation in (("minimum", "min"), ("maximum", "max"))}})
    return pd.DataFrame(output)


def _read_cache(path, fingerprint, positions, repeats):
    if not path.exists():
        return None
    try:
        payload = json.loads(path.read_text())
        checksum = payload.pop("payload_sha256")
        probabilities = np.asarray(payload["probabilities"], float)
        if (checksum != _digest(payload) or payload["fingerprint"] != fingerprint
                or payload["test_positions"] != positions.tolist()
                or probabilities.shape != (len(positions),)
                or not np.isfinite(probabilities).all()
                or ((probabilities < 0) | (probabilities > 1)).any()
                or len(payload["importance"]) != repeats
                or [row["repeat"] for row in payload["importance"]] != list(range(1, repeats+1))):
            return None
        return payload
    except (OSError, ValueError, KeyError, TypeError):
        return None


def _validate_physical(analytical, results, splits, settings, repeats):
    """Fail stale source results before any alternative model is fitted."""
    required = ["participant_id", "psu", "stratum", "caries", "weight"]
    originals, saved_oof, saved_scores, saved_importance = {}, [], [], []
    for name in BASE_MODELS:
        original = results["manifest"]["models"][name]
        spec = {key: value for key, value in original.items() if key != "fingerprint"}
        fingerprint = models._fingerprint(analytical, spec["features"], {**settings, "model": name, "spec": spec})
        if fingerprint != original["fingerprint"]:
            raise ValueError(f"Stale physical model fingerprint: {name}")
        originals[name] = spec
        saved = results["oof"].loc[results["oof"].model.eq(name)].copy()
        if saved.row_index.duplicated().any() or set(saved.row_index) != set(analytical.index):
            raise ValueError("Physical OOF participants must exactly match the dentate domain")
        saved = saved.set_index("row_index").loc[analytical.index]
        observed = saved.rename(columns={"y": "caries"})[required]
        if not all(np.array_equal(observed[column].to_numpy(), analytical[column].to_numpy()) for column in required):
            raise ValueError("Physical OOF identifiers, outcomes or design values have changed")
        if not np.isfinite(saved.p).all() or not saved.p.between(0, 1).all():
            raise ValueError("Physical OOF probabilities are invalid")
        scores = results["foldscores"].loc[results["foldscores"].model.eq(name)].copy()
        importance = results["importance"].loc[
            results["importance"].model.eq(name) & results["importance"].feature.eq("water")].copy()
        if len(scores) != len(splits) or len(importance) != len(splits)*repeats:
            raise ValueError("Incomplete physical fold scores or water permutations")
        for fold, (train, test) in enumerate(splits, 1):
            if not saved.iloc[test].fold.eq(fold).all():
                raise ValueError("Physical outer fold assignments changed")
            score = scores.loc[scores.fold.eq(fold)]
            imp = importance.loc[importance.fold.eq(fold)].sort_values("repeat")
            if (len(score) != 1 or len(imp) != repeats
                    or imp.repeat.tolist() != list(range(1, repeats+1))):
                raise ValueError("Physical fold results are duplicated or incomplete")
            row = score.iloc[0]
            expected = models._metrics(analytical.caries.iloc[test], saved.p.iloc[test], analytical.weight.iloc[test])
            if (not np.allclose([row[key] for key in expected], list(expected.values()), rtol=0, atol=1e-12)
                    or int(row.n_train) != len(train) or int(row.n_test) != len(test)
                    or not imp.n_test.eq(len(test)).all()
                    or not np.isfinite(imp[["brier_increase", "auc_drop"]]).all().all()
                    or not np.allclose(imp.test_weight_sum, analytical.weight.iloc[test].sum(), rtol=1e-13)):
                raise ValueError("Physical fold metrics or water-permutation metadata changed")
            if json.loads(row.selected_params) not in spec["candidates"]:
                raise ValueError("Physical selected parameters are outside the recorded candidate grid")
        saved_oof.append(saved.reset_index(names="row_index"))
        saved_scores.append(scores)
        saved_importance.append(importance)
    return originals, pd.concat(saved_oof, ignore_index=True), pd.concat(saved_scores, ignore_index=True), pd.concat(saved_importance, ignore_index=True)


def run_water_encoding_sensitivity(frame, results, water_coding, cache_dir, *,
                                   bootstrap_replicates=1000, permutation_repeats=5, n_jobs=3):
    """Return six-variant aggregates and PRIVATE OOF records; write private caches only.

    Pass the full survey frame, the original model-results object and the frozen
    ``legacy_water_coding.csv`` DataFrame. The explicit domain is ``eligible``;
    bootstrap design retains every positive-weight survey participant. Exactly
    four alternative variants x original outer folds are fitted when uncached.
    Caches fingerprint inputs, original predictions, folds, selected parameters,
    mapping, source code, software and bootstrap/permutation settings.
    """
    started = time.monotonic()
    if bootstrap_replicates < 1 or permutation_repeats < 1 or n_jobs < 1:
        raise ValueError("Positive bootstrap, permutation and job counts are required")
    if "eligible" not in frame or frame.eligible.isna().any() or not frame.eligible.isin([True, False]).all():
        raise ValueError("A complete boolean eligibility flag is required")
    analytical = frame.loc[frame.eligible.astype(bool)].copy()
    if (analytical.empty or not frame.index.is_unique or frame.participant_id.duplicated().any()
            or analytical[["participant_id", "psu", "stratum", "caries", "weight"]].isna().any().any()
            or not np.isfinite(analytical.weight).all() or analytical.weight.le(0).any()
            or set(analytical.caries.unique()) != {0, 1}):
        raise ValueError("Unique participants, binary observed outcomes and positive design weights are required")
    design = frame.loc[frame.weight.gt(0), ["stratum", "psu"]].copy()
    if design.isna().any().any() or (design.groupby("psu").stratum.nunique() > 1).any():
        raise ValueError("The full positive-weight survey design has invalid PSU/stratum identifiers")
    settings = results["manifest"]["settings"]
    seed, n_folds = int(settings["seed"]), int(settings["outer_folds"])
    if int(settings["permutation_repeats"]) != permutation_repeats:
        raise ValueError("Permutation repeats must equal the cached physical analysis")
    if results["manifest"]["n_participants"] != len(analytical):
        raise ValueError("The original and encoding-sensitivity domains differ")
    mapping = _water_mapping(water_coding)
    y, weight, psu = analytical.caries.to_numpy(int), analytical.weight.to_numpy(float), analytical.psu.to_numpy()
    splits = models._group_splits(y, psu, n_folds, seed, "water encoding outer validation")
    specs, physical_oof, physical_scores, physical_importance = _validate_physical(
        analytical, results, splits, settings, permutation_repeats)
    for spec in specs.values():
        _encode_water(analytical, spec, "physical", mapping)
    source_hashes = {"water_encoding.py": sha256(Path(__file__).read_bytes()).hexdigest(),
                     "models.py": sha256(Path(models.__file__).read_bytes()).hexdigest(),
                     "evaluation.py": sha256(Path(evaluation.__file__).read_bytes()).hexdigest()}
    input_manifest = {
        "source_sha256": source_hashes, "full_frame_sha256": _hash_frame(frame),
        "physical_oof_sha256": _hash_frame(physical_oof),
        "physical_scores_sha256": _hash_frame(physical_scores),
        "physical_water_importance_sha256": _hash_frame(physical_importance),
        "water_mapping_sha256": _hash_frame(water_coding),
        "water_mapping": [{"reported_glasses_per_day": k, "original_r_factor_index": int(v)} for k, v in mapping.items()],
        "original_model_fingerprints": {name: results["manifest"]["models"][name]["fingerprint"] for name in BASE_MODELS},
        "software": results["manifest"]["software"], "seed": seed,
        "bootstrap_replicates": bootstrap_replicates, "permutation_repeats": permutation_repeats,
        "outer_folds": n_folds, "n_jobs": n_jobs,
    }
    analysis_fingerprint = _digest(input_manifest)
    oof_parts, score_parts, importance_parts = [physical_oof], [physical_scores], [physical_importance]
    cache_hits, fits = 0, 0
    variants = {name: {"base_model": name, "encoding": "physical", "encoding_label": ENCODING_LABELS["physical"]} for name in BASE_MODELS}
    with threadpool_limits(limits=n_jobs):
        for base in BASE_MODELS:
            for encoding in ("index", "nominal"):
                name = f"{base}_water_{encoding}"
                variants[name] = {"base_model": base, "encoding": encoding, "encoding_label": ENCODING_LABELS[encoding]}
                encoded, spec = _encode_water(analytical, specs[base], encoding, mapping)
                x = models._feature_frame(encoded, spec["features"])
                water_position = specs[base]["features"].index("water")
                for fold, (train, test) in enumerate(splits, 1):
                    params = json.loads(physical_scores.loc[physical_scores.model.eq(base) & physical_scores.fold.eq(fold), "selected_params"].iloc[0])
                    fold_fingerprint = _digest({"analysis": analysis_fingerprint, "model": name, "spec": spec,
                        "params": params, "fold": fold, "train_positions": train.tolist(), "test_positions": test.tolist()})
                    path = Path(cache_dir) / name / f"outer_{fold}.json"
                    payload = _read_cache(path, fold_fingerprint, test, permutation_repeats)
                    if payload is None:
                        fold_started = time.monotonic()
                        fitted = models._fit_estimator(spec, params, x.iloc[train], y[train], weight[train], psu[train], seed + fold*1000, n_jobs)
                        p = models._probabilities(fitted, x.iloc[test])
                        importance = _water_permutation(fitted, x.iloc[test], y[test], weight[test], p, name,
                                                        fold, permutation_repeats, seed, water_position)
                        unseen = 0
                        if encoding == "nominal":
                            # Missing is handled as its own imputed category; count
                            # unseen nonmissing physical levels separately.
                            unseen = int((x.iloc[test][NOMINAL_FEATURE].notna() &
                                          ~x.iloc[test][NOMINAL_FEATURE].isin(x.iloc[train][NOMINAL_FEATURE].dropna())).sum())
                        score = {"model": name, "fold": fold, **models._metrics(y[test], p, weight[test]),
                            "n_train": len(train), "n_test": len(test), "test_weight_sum": float(weight[test].sum()),
                            "selected_params": json.dumps(params, sort_keys=True), "weighted_training": True,
                            "elapsed_seconds": time.monotonic()-fold_started,
                            "unseen_nonmissing_water_categories_n": unseen}
                        payload = {"fingerprint": fold_fingerprint, "test_positions": test.tolist(),
                                   "probabilities": p.tolist(), "scores": score, "importance": importance}
                        models._json_write(path, {**payload, "payload_sha256": _digest(payload)})
                        fits += 1
                    else:
                        cache_hits += 1
                    part = analytical.iloc[test][["participant_id", "psu", "stratum", "caries", "weight"]].rename(columns={"caries": "y"})
                    part.insert(0, "row_index", analytical.index.to_numpy()[test])
                    part = part.assign(fold=fold, model=name, p=payload["probabilities"])
                    oof_parts.append(part.reset_index(drop=True))
                    score_parts.append(pd.DataFrame([payload["scores"]]))
                    importance_parts.append(pd.DataFrame(payload["importance"]))
                    print(f"Water encoding: {name}, outer fold {fold}/{n_folds} complete", flush=True)
    oof = pd.concat(oof_parts, ignore_index=True)
    if len(oof) != 6*len(analytical) or oof.groupby(["model", "participant_id"]).size().ne(1).any():
        raise AssertionError("Every encoding must evaluate each participant exactly once")
    performance, bootstrap, _ = evaluation.evaluate_oof(oof, design, bootstrap_replicates, seed)
    comparisons = evaluation.paired_differences(bootstrap, performance, [
        (f"{base}_water_{encoding}", base, f"{base}: {encoding} minus physical")
        for base in BASE_MODELS for encoding in ("index", "nominal")])
    comparisons["reference_encoding"] = "physical"
    importance = pd.concat(importance_parts, ignore_index=True)
    fold_scores = pd.concat(score_parts, ignore_index=True)
    fold_scores["unseen_nonmissing_water_categories_n"] = fold_scores.unseen_nonmissing_water_categories_n.fillna(0).astype(int)
    metadata = pd.DataFrame.from_dict(variants, orient="index").rename_axis("model").reset_index()
    attach = lambda table: table.merge(metadata, on="model", how="left", validate="many_to_one")
    manifest = {
        "status": "complete", "scope": "Post hoc water-encoding sensitivity with fixed physical-model hyperparameters",
        "analysis_fingerprint": analysis_fingerprint, **input_manifest,
        "n_participants": len(analytical), "n_events": int(y.sum()),
        "n_psu": int(analytical.psu.nunique()), "n_strata": int(analytical.stratum.nunique()),
        "n_full_design_psu": int(design.psu.nunique()), "n_full_design_strata": int(design.stratum.nunique()),
        "physical_outer_fits_reused": 2*n_folds, "alternative_outer_fits_required": 4*n_folds,
        "alternative_outer_fits_this_call": fits, "cache_hits_this_call": cache_hits,
        "new_hyperparameter_searches": 0, "n_oof_rows_private": len(oof),
        "n_water_permutation_rows": len(importance), "variants": variants,
        "bootstrap_valid_replicates": {name: {metric: int(np.isfinite(draws[:, j]).sum())
             for j, metric in enumerate(evaluation.BOOTSTRAP_METRICS)} for name, draws in bootstrap.items()},
        "elapsed_seconds": time.monotonic()-started,
        "notes": [
            "Physical OOF predictions, selected hyperparameters and held-out water permutations are reused unchanged; no physical model is refitted.",
            "Alternative encodings use the identical outer training/test participants, original model seeds and selected physical-model hyperparameters; no retuning is performed.",
            "The historical factor-index mapping is a fixed, data-derived source artifact: observed dentate levels before original complete-case deletion. It is not a training-only learned transformation or a set of water-intake bins.",
            "All statistical imputation, spline knots, scaling and nominal one-hot vocabulary are learned only in the outer training sample. Unseen nominal levels are ignored by the trained one-hot encoder.",
            "Nominal water replaces the numeric/spline water input; age and remaining-teeth splines remain. Encoding changes dimensionality and may change RF feature-subsampling behaviour under the fixed hyperparameters.",
            "Nominal one-hot encoding is not an exact reproduction of native R factor handling; this is not an independently tuned algorithm comparison or an exact reconstruction of the historical pipeline.",
            "The same five uniform held-out donor-row permutations (including missing water) are used for each encoding; outcomes and survey scoring weights stay fixed.",
            "Water-only permutation provides no rank relative to other predictors in alternative encodings. Weighted fold means and fold minima/maxima describe stability; the ranges are not confidence intervals.",
            "Paired percentile confidence intervals use shared rescaled stratified PSU bootstrap draws from the full positive-weight survey design, conditional on fixed fits, hyperparameters and outer folds; singleton PSUs remain fixed.",
            "Paired AUC differences use pooled OOF probabilities; mean fold permutation AUC drops are a different estimand. Negative permutation importance is retained.",
        ],
    }
    return {"performance": attach(performance), "comparisons": attach(comparisons),
            "water_importance": attach(importance),
            "water_importance_stability": attach(summarize_water_importance(importance)),
            "fold_scores": attach(fold_scores), "oof": attach(oof), "manifest": manifest}
