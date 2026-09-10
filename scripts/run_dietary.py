#!/usr/bin/env python3
"""Run the independent proposal on incremental information from dietary variables."""
from __future__ import annotations

import argparse
from datetime import datetime, timezone
from hashlib import sha256
import json
from pathlib import Path
import pickle
import sys

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'src'))


def write_json(path, payload):
    def convert(value):
        if isinstance(value, np.generic): return value.item()
        if isinstance(value, np.ndarray): return value.tolist()
        if isinstance(value, Path): return str(value)
        raise TypeError(type(value).__name__)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2, ensure_ascii=False, default=convert) + '\n')


def hashes(paths):
    return {str(p.relative_to(ROOT)): sha256(p.read_bytes()).hexdigest() for p in paths}


def model_source_hashes():
    return hashes([ROOT / 'src/ens_analysis' / f for f in
                   ['data.py', 'models.py', 'expanded_data.py', 'dietary_models.py']])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--stage', choices=['prepare', 'models', 'summarize', 'all'], default='all')
    parser.add_argument('--data', type=Path, default=ROOT / 'data/data.sav')
    parser.add_argument('--jobs', type=int, default=3)
    parser.add_argument('--bootstrap', type=int, default=1000)
    args = parser.parse_args()
    from ens_analysis.expanded_data import (load_expanded_data, CONTEXT_FEATURES,
                                            SYMPTOM_FEATURES, INTAKE_FEATURES, LABEL_FEATURES)
    from ens_analysis.dietary_results import expanded_profile, evaluate_dietary
    from ens_analysis.models import NUMERIC_FEATURES
    destination = ROOT / 'outputs/dietary'
    tables, figures = destination / 'tables', destination / 'figures'
    private = ROOT / 'outputs/private/dietary'
    for directory in [tables, figures, private]: directory.mkdir(parents=True, exist_ok=True)
    write_json(tables / 'run_manifest.json', {'status': 'in_progress', 'stage': args.stage,
                                              'started_utc': datetime.now(timezone.utc).isoformat()})
    frame, dictionary, audit = load_expanded_data(args.data)
    input_hash = sha256(args.data.read_bytes()).hexdigest()
    features = list(dict.fromkeys(CONTEXT_FEATURES + SYMPTOM_FEATURES + INTAKE_FEATURES + LABEL_FEATURES))
    numeric = set(NUMERIC_FEATURES) | {'soda_glasses_daily', 'juice_glasses_daily'}
    if args.stage in ['prepare', 'summarize', 'all']:
        audit['source_file'] = 'data/data.sav'
        write_json(tables / 'data_audit.json', audit)
        dictionary.to_csv(tables / 'variable_dictionary.csv', index=False)
        missing = [{'variable': name, 'n': int(frame.eligible.sum()),
                    'missing': int(frame.loc[frame.eligible, name].isna().sum()),
                    'percent_missing': float(100 * frame.loc[frame.eligible, name].isna().mean())}
                   for name in features]
        pd.DataFrame(missing).to_csv(tables / 'predictor_missingness.csv', index=False)
        profile, profile_meta = expanded_profile(frame, dictionary, numeric, features)
        profile.to_csv(tables / 'descriptive_profile.csv', index=False)
        write_json(tables / 'descriptive_profile_metadata.json', profile_meta)
        write_json(tables / 'feature_blocks.json', {'context': CONTEXT_FEATURES, 'symptoms': SYMPTOM_FEATURES,
                                                   'diet': INTAKE_FEATURES, 'labels': LABEL_FEATURES})
        print(f'Prepared {int(frame.eligible.sum())} participants and {len(features)} predictors', flush=True)
    if args.stage in ['models', 'all']:
        from ens_analysis.dietary_models import run_dietary_models
        initial = model_source_hashes()
        plan = ROOT / 'docs/dietary-incremental-analysis-plan.md'
        if not plan.exists(): raise RuntimeError('Freeze the new analysis plan before fitting')
        plan_hash = sha256(plan.read_bytes()).hexdigest()
        results = run_dietary_models(frame, private / 'models', n_jobs=args.jobs)
        if initial != model_source_hashes() or sha256(plan.read_bytes()).hexdigest() != plan_hash:
            raise RuntimeError('Model code or analysis plan changed during fitting; rerun models')
        results['_training_provenance'] = {'input_sha256': input_hash, 'model_source_sha256': initial,
                                           'plan_sha256': plan_hash, 'completed_utc': datetime.now(timezone.utc).isoformat()}
        target = private / 'results.pkl'
        target.write_bytes(pickle.dumps(results))
        write_json(private / 'results_integrity.json', {'sha256': sha256(target.read_bytes()).hexdigest()})
        print('New model fits and private held-out predictions saved', flush=True)
    if args.stage in ['summarize', 'all']:
        target = private / 'results.pkl'
        integrity = json.loads((private / 'results_integrity.json').read_text())
        if sha256(target.read_bytes()).hexdigest() != integrity['sha256']:
            raise RuntimeError('Private result checksum mismatch')
        results = pickle.loads(target.read_bytes())
        provenance = results['_training_provenance']
        if provenance['input_sha256'] != input_hash or provenance['model_source_sha256'] != model_source_hashes():
            raise RuntimeError('Model results stale; rerun models before summarizing')
        plan = ROOT / 'docs/dietary-incremental-analysis-plan.md'
        if sha256(plan.read_bytes()).hexdigest() != provenance['plan_sha256']:
            raise RuntimeError('Analysis plan differs from its pre-fit version')
        sources = sorted((ROOT / 'src').rglob('*.py')) + [Path(__file__)]
        code = hashes(sources)
        print(f'Evaluating new models with {args.bootstrap} shared PSU-bootstrap replicates', flush=True)
        aggregate = evaluate_dietary(frame, results, args.bootstrap)
        for key, value in aggregate.items():
            if not key.startswith('_'): value.to_csv(tables / f'{key}.csv', index=False)
        for key, filename in [('tuning', 'hyperparameter_search'), ('foldscores', 'outer_fold_performance'),
                              ('importance', 'permutation_importance'), ('grouped_importance', 'grouped_permutation_importance'),
                              ('split_qc', 'validation_split_checks')]:
            value = results[key]
            if key == 'grouped_importance':
                value = value.rename(columns={'group': 'block'}).copy()
                value['block'] = value['block'].replace({'intake': 'diet'})
            value.to_csv(tables / f'{filename}.csv', index=False)
        write_json(tables / 'model_manifest.json', results['manifest'])
        results['oof'].to_csv(private / 'oof.csv', index=False)
        with (private / 'bootstrap.pkl').open('wb') as stream: pickle.dump(aggregate['_bootstrap'], stream)
        from ens_analysis.dietary_figures import make_dietary_figures
        make_dietary_figures(tables, figures)
        if code != hashes(sources): raise RuntimeError('Analysis source changed during summary; rerun summarize')
        run = {'status': 'complete', 'completed_utc': datetime.now(timezone.utc).isoformat(),
               'input_sha256': input_hash, 'code_sha256': code, 'training_provenance': provenance,
               'n_participants': int(frame.eligible.sum()), 'events': int(frame.loc[frame.eligible, 'caries'].sum()),
               'n_predictors': len(features), 'n_model_variants': len(results['manifest']['models']),
               'bootstrap_replicates': args.bootstrap, 'model_manifest': results['manifest'],
               'primary_contrast': 'Spline Brier(context) minus Brier(context + dietary intake)',
               'primary_hypothesis': 'Dietary information improves held-out probability accuracy beyond expanded non-dietary context',
               'scope': 'Independent dietary incremental-value proposal; previous Andrea revision is separate',
               'development_status': 'Exploratory, dataset-informed extension with plan fixed before these new fits',
               'weight_confirmation': 'Working examination-phase weight; official manual confirmation outstanding'}
        write_json(tables / 'run_manifest.json', run)
        print(f'Completed independent dietary proposal: {len(results["manifest"]["models"])} fitted variants', flush=True)


if __name__ == '__main__': main()
