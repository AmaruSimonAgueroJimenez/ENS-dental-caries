#!/usr/bin/env python3
"""Build aggregate results and figures for the all-predictor proposal."""
from __future__ import annotations

import argparse
from copy import deepcopy
from datetime import datetime, timezone
from hashlib import sha256
import json
from pathlib import Path
import pickle
import shutil
import sys

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'src'))


def digest(path):
    return sha256(Path(path).read_bytes()).hexdigest()


def write_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, ensure_ascii=False) + '\n')


def current_sources():
    # Enumerate only the analysis dependencies, so unrelated modules can evolve.
    names = ['data.py', 'expanded_data.py', 'models.py', 'dietary_models.py', 'evaluation.py',
             'survey.py', 'dietary_results.py', 'figures.py', 'dietary_figures.py',
             'all_predictor_models.py', 'all_predictor_results.py', 'all_predictor_figures.py']
    files = [ROOT/'src/ens_analysis'/n for n in names]
    files += [ROOT/'scripts/run_all_predictor_models.py', Path(__file__)]
    return {str(p.relative_to(ROOT)): digest(p) for p in files}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--data', type=Path, default=ROOT/'data/data.sav')
    parser.add_argument('--bootstrap', type=int, default=1000)
    args = parser.parse_args()
    if args.bootstrap < 1: raise ValueError('Bootstrap count must be positive')
    from ens_analysis import all_predictor_models as am
    from ens_analysis.expanded_data import load_expanded_data
    from ens_analysis.all_predictor_results import evaluate_all_predictors, MODELS
    from ens_analysis.all_predictor_figures import make_all_predictor_figures
    import pandas as pd

    private = ROOT/'outputs/private/all_predictors'
    source = private/'results.pkl'
    integrity = json.loads((private/'results_integrity.json').read_text())
    if digest(source) != integrity['sha256']: raise RuntimeError('Private combined result checksum mismatch')
    results = pickle.loads(source.read_bytes())
    provenance = results['_training_provenance']
    plan = ROOT/'docs/all-predictor-analysis-plan.md'
    if (digest(args.data) != provenance['input_sha256'] or digest(plan) != provenance['plan_sha256']
            or am._source_hashes() != provenance['model_source_sha256']
            or digest(ROOT/'scripts/run_all_predictor_models.py') != provenance['runner_sha256']):
        raise RuntimeError('New model inputs, plan or fitting sources changed')
    if digest(ROOT/'outputs/private/dietary/results.pkl') != provenance['dietary_results_sha256']:
        raise RuntimeError('Preserved classical source changed')
    if set(results['manifest']['models']) != set(MODELS): raise RuntimeError('Eight fixed models required')
    sources = current_sources()
    previous = ROOT/'outputs/dietary/tables'
    prior_run = json.loads((previous/'run_manifest.json').read_text())
    if prior_run['status'] != 'complete' or prior_run['input_sha256'] != provenance['input_sha256']:
        raise RuntimeError('The preserved dietary summary has a different population source')
    for name, value in prior_run['code_sha256'].items():
        if digest(ROOT/name) != value: raise RuntimeError(f'Preserved classical source changed: {name}')
    frame, _, audit = load_expanded_data(args.data)
    tables, figures = ROOT/'outputs/all_predictors/tables', ROOT/'outputs/all_predictors/figures'
    tables.mkdir(parents=True, exist_ok=True)
    write_json(tables/'run_manifest.json', {'status': 'in_progress', 'started_utc': datetime.now(timezone.utc).isoformat()})
    # Descriptive aggregates are preserved, with three clearly documented role labels updated.
    descriptive = ['data_audit.json', 'descriptive_profile.csv', 'descriptive_profile_metadata.json',
                   'feature_blocks.json', 'predictor_missingness.csv', 'variable_dictionary.csv']
    reused = {}
    for name in descriptive:
        reused[name] = {'source': f'outputs/dietary/tables/{name}', 'source_sha256': digest(previous/name)}
        shutil.copy2(previous/name, tables/name)
    dictionary = pd.read_csv(tables/'variable_dictionary.csv', keep_default_na=False, na_values=[''])
    updates = {'dental_visit': 'Time since last dental visit; potentially responsive to existing disease or treatment',
               'soda_glasses_daily': 'Estimated daily soda volume in glasses; extreme reported values retained and audited',
               'juice_glasses_daily': 'Estimated daily juice volume in glasses; extreme reported values retained and audited'}
    for name, description in updates.items(): dictionary.loc[dictionary.variable.eq(name), 'description'] = description
    dictionary.to_csv(tables/'variable_dictionary.csv', index=False)
    reused['variable_dictionary.csv']['description_only_overrides'] = updates
    for name in reused: reused[name]['output_sha256'] = digest(tables/name)
    write_json(tables/'descriptive_provenance.json', reused)
    print(f'Evaluating eight model variants with {args.bootstrap} shared PSU-bootstrap draws', flush=True)
    summaries = evaluate_all_predictors(frame, results, args.bootstrap)
    for key, value in summaries.items():
        if not key.startswith('_'): value.to_csv(tables/(key+'.csv'), index=False)
    spread = []
    for (name, fold), block in results['oof'].groupby(['model', 'fold']):
        training = results['oof'].loc[results['oof'].model.eq(name) & results['oof'].fold.ne(fold)]
        spread.append({'model': name, 'fold': int(fold), 'n_test': len(block),
                       'min_probability': float(block.p.min()), 'max_probability': float(block.p.max()),
                       'probability_range': float(block.p.max()-block.p.min()),
                       'probability_sd': float(block.p.std(ddof=0)),
                       'distinct_probabilities_at_six_decimals': int(block.p.round(6).nunique()),
                       'weighted_mean_probability': float((block.p*block.weight).sum()/block.weight.sum()),
                       'training_prevalence': float((training.y*training.weight).sum()/training.weight.sum())})
    pd.DataFrame(spread).to_csv(tables/'prediction_spread.csv', index=False)
    copies = {'tuning': 'hyperparameter_search', 'foldscores': 'outer_fold_performance',
              'importance': 'permutation_importance', 'grouped_importance': 'grouped_permutation_importance',
              'split_qc': 'validation_split_checks', 'training_diagnostics': 'training_diagnostics',
              'reconstruction_checks': 'reconstruction_checks'}
    for key, name in copies.items():
        table = results[key].copy()
        if key == 'grouped_importance':
            table = table.rename(columns={'group': 'block'})
            table['block'] = table.block.replace({'intake': 'diet'})
        table.to_csv(tables/(name+'.csv'), index=False)
    public_model_manifest = deepcopy(results['manifest'])
    public_model_manifest['all_predictor_importance'] = {
        name: {'available': True,
               'origin': ('New neural-network outer fits' if name.startswith('mlp__') else
                          'Reconstructed selected outer fits with verified identical probabilities'
                          if name.startswith('hist_gradient_boosting__') else
                          'Preserved earlier held-out permutation outputs'),
               'individual_rows': int(results['importance'].model.eq(name).sum()),
               'grouped_rows': int(results['grouped_importance'].model.eq(name).sum())}
        for name in MODELS}
    public_model_manifest['inherited_specification_note'] = (
        'Six classical model specifications retain their original fingerprints and flags. '
        'In particular, the historical compute_importance=False flag for boosting describes the '
        'earlier run. The new complete importance outputs and their provenance are recorded '
        'separately in all_predictor_importance and reconstruction_checks.csv.')
    write_json(tables/'model_manifest.json', public_model_manifest)
    with (private/'bootstrap.pkl').open('wb') as stream: pickle.dump(summaries['_bootstrap'], stream)
    with (private/'evaluated_wide.pkl').open('wb') as stream: pickle.dump(summaries['_wide'], stream)
    make_all_predictor_figures(tables, figures)
    if sources != current_sources(): raise RuntimeError('Analysis code changed during summary; rerun this command')
    manifest = {'status': 'complete', 'completed_utc': datetime.now(timezone.utc).isoformat(),
                'input_sha256': provenance['input_sha256'], 'plan_sha256': digest(plan),
                'code_sha256': sources, 'training_provenance': provenance,
                'reused_classical_run_sha256': digest(previous/'run_manifest.json'),
                'n_participants': int(frame.eligible.sum()), 'events': int(frame.loc[frame.eligible, 'caries'].sum()),
                'n_predictors': 44, 'n_model_variants': len(MODELS), 'bootstrap_replicates': args.bootstrap,
                'model_manifest': public_model_manifest, 'n_figures': 9,
                'scope': 'Exploratory all-predictor importance and regularized neural-network comparison; prior dietary proposal and Andrea correction preserved',
                'development_status': 'Focus changed after examining prior results; neural search fixed before neural fits; no prospective preregistration',
                'weight_confirmation': 'Working examination-phase weight; official manual confirmation outstanding',
                'importance_interpretation': 'Predictive reliance under held-out marginal permutation, not causation; fold ranges are not confidence intervals'}
    manifest['importance_presentation_rule'] = {
        'absolute_brier_display_tolerance': 1e-8,
        'scope': 'Presentation only; no statistical significance or model selection threshold',
        'timing': 'Reporting decision after observing near-constant neural predictions; no models or search settings changed',
        'rule': 'If all absolute mean individual Brier importances in a model are below 1e-8, do not interpret its variable ranks; retain every raw signed value',
        'additional_caution': 'Permutation AUC and positive-fold fractions may be sensitive to tiny probability differences or floating-point order; interpret cross-model agreement using the separately provided classical-only columns'}
    write_json(tables/'run_manifest.json', manifest)
    print(json.dumps({'status': 'complete', 'variants': len(MODELS), 'participants': manifest['n_participants'],
                      'aggregate_files': len(list(tables.glob('*'))), 'figures': 9}), flush=True)


if __name__ == '__main__': main()
