"""Evaluate the eight fixed all-predictor model variants on common held-out rows."""
from __future__ import annotations

import numpy as np
import pandas as pd
from sklearn.metrics import roc_curve

from .evaluation import evaluate_oof, paired_differences, calibration_bins, probability_metrics

FAMILIES = ('spline_logistic', 'random_forest', 'hist_gradient_boosting', 'mlp')
SCENARIOS = ('context_diet_labels', 'symptoms_diet_labels')
MODELS = tuple(f'{family}__{scenario}' for family in FAMILIES for scenario in SCENARIOS)


def importance_summary(raw, feature_column='feature'):
    """Signed, weighted means and unweighted descriptions of five-fold stability.

    Repeat perturbations are first averaged within fold. Fold ranges and top-ten
    frequencies describe variation; they are not sampling confidence intervals.
    """
    required = {'model', 'fold', feature_column, 'repeat', 'brier_increase', 'auc_drop',
                'test_weight_sum'}
    if not required.issubset(raw):
        raise ValueError(f'Missing importance columns: {sorted(required.difference(raw.columns))}')
    if raw.empty or raw[['model', 'fold', feature_column, 'repeat']].isna().any().any():
        raise ValueError('Importance requires nonempty, observed model/fold/variable/repeat keys')
    if raw.duplicated(['model', 'fold', feature_column, 'repeat']).any():
        raise ValueError('Duplicate importance repeat')
    if not np.isfinite(raw[['brier_increase', 'auc_drop', 'test_weight_sum']].to_numpy(float)).all():
        raise ValueError('Importance must have finite metrics and weights')
    if raw.test_weight_sum.le(0).any():
        raise ValueError('Importance weights must be positive')
    if raw.groupby(['model', 'fold']).test_weight_sum.nunique().ne(1).any():
        raise ValueError('Importance weights must be constant within each model/fold')
    for _, model_rows in raw.groupby('model'):
        expected = set(zip(model_rows.fold, model_rows['repeat']))
        for _, feature_rows in model_rows.groupby(feature_column):
            if set(zip(feature_rows.fold, feature_rows['repeat'])) != expected:
                raise ValueError('Each variable requires the same folds and repeats within a model')
    folds = raw.groupby(['model', 'fold', feature_column], as_index=False).agg(
        brier_increase=('brier_increase', 'mean'), auc_drop=('auc_drop', 'mean'),
        test_weight_sum=('test_weight_sum', 'first'), n_repeats=('repeat', 'nunique'))
    folds['rank'] = folds.groupby(['model', 'fold']).brier_increase.rank(ascending=False, method='min')
    records = []
    for (model, feature), block in folds.groupby(['model', feature_column], sort=False):
        record = {'model': model, feature_column: feature,
                  'brier_increase': np.average(block.brier_increase, weights=block.test_weight_sum),
                  'auc_drop': np.average(block.auc_drop, weights=block.test_weight_sum),
                  'minimum_fold_brier_increase': block.brier_increase.min(),
                  'maximum_fold_brier_increase': block.brier_increase.max(),
                  'minimum_fold_auc_drop': block.auc_drop.min(),
                  'maximum_fold_auc_drop': block.auc_drop.max(),
                  'median_rank': block['rank'].median(), 'best_rank': block['rank'].min(),
                  'worst_rank': block['rank'].max(),
                  'fraction_top3': block['rank'].le(3).mean(),
                  'fraction_top10': block['rank'].le(10).mean(),
                  'fraction_positive_folds': block.brier_increase.gt(0).mean(),
                  'folds': block.fold.nunique(), 'repeats_per_fold': int(block.n_repeats.min())}
        if block.n_repeats.nunique() != 1:
            raise ValueError('Unequal repeat counts across importance folds')
        records.append(record)
    summary = pd.DataFrame(records)
    summary['overall_rank'] = summary.groupby('model').brier_increase.rank(ascending=False, method='min')
    return summary


def agreement_table(importance):
    """Describe agreement without selecting a universally important predictor."""
    expected = {f'{family}__symptoms_diet_labels' for family in FAMILIES}
    complete = importance.loc[importance.model.str.endswith('__symptoms_diet_labels')].copy()
    if set(complete.model) != expected or complete.duplicated(['model', 'feature']).any():
        raise ValueError('Agreement requires unique results from the four specified full models')
    if complete.groupby('feature').model.nunique().ne(len(FAMILIES)).any():
        raise ValueError('Agreement requires complete results in all four full models')
    rows = []
    for feature, block in complete.groupby('feature'):
        classical = block.loc[~block.model.str.startswith('mlp__')]
        rows.append({'feature': feature, 'models': len(block),
                     'models_with_positive_mean': int(block.brier_increase.gt(0).sum()),
                     'models_positive_in_at_least_four_folds': int(block.fraction_positive_folds.ge(.8).sum()),
                     'models_in_top10': int(block.overall_rank.le(10).sum()),
                     'median_overall_rank': float(block.overall_rank.median()),
                     'best_overall_rank': float(block.overall_rank.min()),
                     'worst_overall_rank': float(block.overall_rank.max()),
                     'classical_models': len(classical),
                     'classical_models_with_positive_mean': int(classical.brier_increase.gt(0).sum()),
                     'classical_models_positive_in_at_least_four_folds': int(classical.fraction_positive_folds.ge(.8).sum()),
                     'classical_models_in_top10': int(classical.overall_rank.le(10).sum()),
                     'classical_median_overall_rank': float(classical.overall_rank.median()),
                     'classical_best_overall_rank': float(classical.overall_rank.min()),
                     'classical_worst_overall_rank': float(classical.overall_rank.max())})
    # Both inventories are retained. The explicit classical-only view is useful
    # when the neural benchmark's near-zero effects make its numerical ranks
    # uninterpretable; it is not a data-driven selection of a winning family.
    return pd.DataFrame(rows).sort_values(['classical_median_overall_rank', 'feature']).reset_index(drop=True)


def evaluate_all_predictors(frame, results, replicates=1000):
    oof = results['oof'].copy()
    if set(oof.model) != set(MODELS):
        raise ValueError('Expected exactly four families in two fixed information scenarios')
    if oof.duplicated(['participant_id', 'model']).any():
        raise ValueError('Repeated held-out participant within a model')
    if oof.groupby('participant_id').fold.nunique().ne(1).any():
        raise ValueError('Models must share the exact same outer folds')
    baseline = oof.loc[oof.model.eq(MODELS[0])].copy()
    baseline['model'] = 'prevalence_only'
    for fold in baseline.fold.unique():
        training = baseline.fold.ne(fold)
        baseline.loc[~training, 'p'] = np.average(baseline.loc[training, 'y'],
                                                  weights=baseline.loc[training, 'weight'])
    combined = pd.concat([oof, baseline], ignore_index=True)
    design = frame.loc[frame.weight.notna() & frame.weight.gt(0)]
    metrics, bootstrap, wide = evaluate_oof(combined, design, replicates)
    base_rows = metrics.model.eq('prevalence_only')
    metrics.loc[base_rows, ['calibration_slope', 'joint_calibration_intercept']] = np.nan
    metrics.loc[base_rows, 'calibration_status'] = 'Within-fold constant benchmark; pooled slope not interpretable'
    comparisons, codes = [], {}
    for scenario in SCENARIOS:
        for family in FAMILIES[:-1]:
            left, right = f'mlp__{scenario}', f'{family}__{scenario}'
            comparisons.append((left, right, f'Neural network versus {family} in {scenario}'))
            codes[(left, right)] = f'neural_vs_{family}__{scenario}'
    for family in FAMILIES:
        left, right = f'{family}__{SCENARIOS[1]}', f'{family}__{SCENARIOS[0]}'
        comparisons.append((left, right, f'Concurrent oral-status information in {family}'))
        codes[(left, right)] = f'oral_status__{family}'
    pairs = paired_differences(bootstrap, metrics, comparisons)
    pairs['contrast_id'] = [codes[(a, b)] for a, b in zip(pairs.model, pairs.reference)]
    losses = pairs.metric.isin(['brier', 'log_loss'])
    pairs.loc[losses, 'difference'] *= -1
    low, high = pairs.loc[losses, 'ci_low'].copy(), pairs.loc[losses, 'ci_high'].copy()
    pairs.loc[losses, 'ci_low'], pairs.loc[losses, 'ci_high'] = -high, -low
    pairs['direction'] = 'Positive favours the compared model over the reference'
    pairs['role'] = 'exploratory_conditional_comparison'
    calibration, roc = [], []
    for name in MODELS:
        bins = calibration_bins(wide.y, wide[name], wide.weight)
        bins['model'] = name; calibration.append(bins)
        fpr, tpr, _ = roc_curve(wide.y, wide[name], sample_weight=wide.weight)
        roc.append(pd.DataFrame({'model': name, 'fpr': fpr, 'tpr': tpr}))
    grouped = results['grouped_importance'].rename(columns={'group': 'block'}).copy()
    grouped['block'] = grouped.block.replace({'intake': 'diet'})
    importance = importance_summary(results['importance'])
    group_summary = importance_summary(grouped, 'block')
    covariates = frame.reset_index(names='row_index')[['row_index', 'age', 'sex', 'rural', 'education']]
    wide = wide.merge(covariates, on='row_index', validate='one_to_one')
    wide['age_group'] = pd.cut(wide.age, [14, 24, 44, 64, np.inf], labels=['15–24', '25–44', '45–64', '65+'])
    subgroup = []
    for name in MODELS:
        for variable in ('sex', 'age_group', 'rural', 'education'):
            for level, block in wide.groupby(variable, observed=True):
                subgroup.append({'model': name, 'variable': variable, 'level': str(level),
                                 **probability_metrics(block.y, block[name], block.weight)})
    return {'model_performance': metrics, 'paired_contrasts': pairs,
            'importance_stability': importance, 'grouped_importance_stability': group_summary,
            'cross_model_agreement': agreement_table(importance),
            'calibration': pd.concat(calibration, ignore_index=True),
            'roc': pd.concat(roc, ignore_index=True), 'subgroup_performance': pd.DataFrame(subgroup),
            '_bootstrap': bootstrap, '_wide': wide}
