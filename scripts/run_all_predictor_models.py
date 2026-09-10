#!/usr/bin/env python3
"""Fit only the new full-information benchmark and its private caches."""
from __future__ import annotations

import argparse
from datetime import datetime, timezone
from hashlib import sha256
import json
from pathlib import Path
import pickle
import sys

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--data", type=Path, default=ROOT / "data/data.sav")
    parser.add_argument("--jobs", type=int, default=3)
    args = parser.parse_args()
    from ens_analysis.expanded_data import load_expanded_data
    from ens_analysis import all_predictor_models as am, models

    private = ROOT / "outputs/private/all_predictors"
    tables = ROOT / "outputs/all_predictors/tables"
    source = ROOT / "outputs/private/dietary/results.pkl"
    integrity = source.with_name("results_integrity.json")
    expected = json.loads(integrity.read_text())["sha256"]
    if sha256(source.read_bytes()).hexdigest() != expected:
        raise RuntimeError("Dietary private result-file integrity check failed")
    # Unpickle only the already authenticated local analysis artifact.
    dietary = pickle.loads(source.read_bytes())
    input_hash = sha256(args.data.read_bytes()).hexdigest()
    old_provenance = dietary["_training_provenance"]
    if old_provenance["input_sha256"] != input_hash:
        raise RuntimeError("Expanded frame must use the same original survey file")
    old_plan = ROOT / "docs/dietary-incremental-analysis-plan.md"
    if sha256(old_plan.read_bytes()).hexdigest() != old_provenance["plan_sha256"]:
        raise RuntimeError("Original dietary plan changed")
    source_hashes = am._source_hashes()
    for name, expected_hash in old_provenance["model_source_sha256"].items():
        if sha256((ROOT / name).read_bytes()).hexdigest() != expected_hash:
            raise RuntimeError("Original dietary fitting source changed")
    plan = ROOT / "docs/all-predictor-analysis-plan.md"
    plan_hash = sha256(plan.read_bytes()).hexdigest()
    runner_hash = sha256(Path(__file__).read_bytes()).hexdigest()
    frame, _, audit = load_expanded_data(args.data)
    private.mkdir(parents=True, exist_ok=True)
    result = am.run_all_predictor_models(frame, private / "models", dietary, args.jobs, plan)
    if (source_hashes != am._source_hashes()
            or sha256(plan.read_bytes()).hexdigest() != plan_hash
            or sha256(Path(__file__).read_bytes()).hexdigest() != runner_hash
            or sha256(args.data.read_bytes()).hexdigest() != input_hash
            or sha256(source.read_bytes()).hexdigest() != expected):
        raise RuntimeError("Analysis source, plan or input changed during fitting")
    result["_training_provenance"] = {
        "input_sha256": input_hash, "model_source_sha256": source_hashes,
        "runner_sha256": runner_hash, "plan_sha256": plan_hash,
        "dietary_results_sha256": expected,
        "completed_utc": datetime.now(timezone.utc).isoformat(),
    }
    target = private / "results.pkl"
    temporary = target.with_suffix(".tmp")
    temporary.write_bytes(pickle.dumps(result, protocol=pickle.HIGHEST_PROTOCOL))
    temporary.replace(target)
    models._json_write(private / "results_integrity.json", {"sha256": sha256(target.read_bytes()).hexdigest()})
    # Public summaries contain no IDs, OOF probabilities or individual records.
    models._json_write(tables / "model_manifest.json", result["manifest"])
    print(json.dumps({"status": "models_complete", "participants": int(frame.eligible.sum()),
                      "events": audit.get("events", result["manifest"]["n_events"]),
                      "variants": len(result["manifest"]["models"]),
                      "private_results": str(target.relative_to(ROOT)),
                      "result_sha256": sha256(target.read_bytes()).hexdigest()}), flush=True)


if __name__ == "__main__":
    main()
