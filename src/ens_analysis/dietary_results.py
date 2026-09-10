"""Aggregate evaluation for the independent dietary incremental-value proposal."""
from __future__ import annotations

import numpy as np
import pandas as pd
from scipy.special import expit, logit
from scipy.stats import t
from sklearn.metrics import roc_curve

from .evaluation import evaluate_oof, paired_differences, calibration_bins, probability_metrics
from .survey import design_covariance

FAMILIES = ('spline_logistic', 'random_forest', 'hist_gradient_boosting')
SET_ORDER = ('basic', 'context', 'context_diet', 'context_diet_labels', 'symptoms',
             'symptoms_diet', 'symptoms_diet_labels')
CONTRASTS = (
    ('context_diet', 'context', 'Diet beyond context', 'diet_context'),
    ('symptoms_diet', 'symptoms', 'Diet beyond context and symptoms', 'diet_symptoms'),
    ('context_diet_labels', 'context_diet', 'Label behaviours beyond context and diet', 'labels_context'),
    ('symptoms_diet_labels', 'symptoms_diet', 'Label behaviours beyond symptoms and diet', 'labels_symptoms'),
    ('context', 'basic', 'Expanded context beyond basic', 'context_basic'),
    ('symptoms', 'context', 'Symptoms beyond context', 'symptoms_context'),
    ('symptoms_diet_labels', 'context_diet_labels', 'Symptoms beyond context, diet and labels', 'symptoms_full'),
)


def summarize_importance(raw: pd.DataFrame, feature_column='feature') -> pd.DataFrame:
    if raw.empty:
        return pd.DataFrame()
    folds = raw.groupby(['model', 'fold', feature_column], as_index=False).agg(
        brier_increase=('brier_increase', 'mean'), auc_drop=('auc_drop', 'mean'),
        test_weight_sum=('test_weight_sum', 'first'))
    folds['rank'] = folds.groupby(['model', 'fold']).brier_increase.rank(ascending=False, method='min')
    rows = []
    for (model, feature), block in folds.groupby(['model', feature_column]):
        rows.append({'model': model, feature_column: feature,
                     'brier_increase': np.average(block.brier_increase, weights=block.test_weight_sum),
                     'auc_drop': np.average(block.auc_drop, weights=block.test_weight_sum),
                     'minimum_fold_brier_increase': block.brier_increase.min(),
                     'maximum_fold_brier_increase': block.brier_increase.max(),
                     'median_rank': block['rank'].median(), 'best_rank': block['rank'].min(),
                     'worst_rank': block['rank'].max(), 'fraction_top3': block['rank'].le(3).mean()})
    return pd.DataFrame(rows)


def evaluate_dietary(frame, results, replicates=1000):
    """Use shared full-design PSU draws and orient every contrast toward gain."""
    oof = results['oof'].copy()
    first = oof.model.iloc[0]
    baseline = oof.loc[oof.model.eq(first)].copy()
    baseline['model'] = 'prevalence_only'
    for fold in baseline.fold.unique():
        training = baseline.fold.ne(fold)
        baseline.loc[~training, 'p'] = np.average(baseline.loc[training, 'y'],
                                                  weights=baseline.loc[training, 'weight'])
    oof = pd.concat([oof, baseline], ignore_index=True)
    design = frame.loc[frame.weight.notna() & frame.weight.gt(0)]
    metrics, boot, wide = evaluate_oof(oof, design, replicates)
    is_baseline = metrics.model.eq('prevalence_only')
    metrics.loc[is_baseline, ['calibration_slope', 'joint_calibration_intercept']] = np.nan
    metrics.loc[is_baseline, 'calibration_status'] = 'Within-fold constant benchmark; pooled slope not interpretable'
    pairs, pair_codes = [], {}
    for family in FAMILIES:
        for larger, smaller, label, code in CONTRASTS:
            model, reference = f'{family}__{larger}', f'{family}__{smaller}'
            pairs.append((model, reference, label))
            pair_codes[(model, reference)] = code
    for feature_set in ('context_diet', 'symptoms_diet'):
        model = f'spline_logistic__{feature_set}__winsor99'
        reference = f'spline_logistic__{feature_set}'
        pairs.append((model, reference, 'Training-positive-p99 beverage sensitivity'))
        pair_codes[(model, reference)] = f'winsor_{feature_set}'
        baseline_set = 'context' if feature_set == 'context_diet' else 'symptoms'
        reference = f'spline_logistic__{baseline_set}'
        pairs.append((model, reference, 'Dietary increment under training-positive-p99 sensitivity'))
        pair_codes[(model, reference)] = f'diet_{baseline_set}_winsor'
    contrasts = paired_differences(boot, metrics, pairs)
    contrasts['contrast_id'] = [pair_codes[(a, b)] for a, b in zip(contrasts.model, contrasts.reference)]
    contrasts['family'] = contrasts.model.str.split('__').str[0]
    contrasts['unoriented_difference'] = contrasts.difference
    losses = contrasts.metric.isin(['brier', 'log_loss'])
    contrasts.loc[losses, 'difference'] *= -1
    low, high = contrasts.loc[losses, 'ci_low'].copy(), contrasts.loc[losses, 'ci_high'].copy()
    contrasts.loc[losses, 'ci_low'] = -high
    contrasts.loc[losses, 'ci_high'] = -low
    contrasts['direction'] = 'Positive favours added variables or sensitivity'
    contrasts['role'] = 'secondary_exploratory'
    primary = (contrasts.family.eq('spline_logistic') & contrasts.contrast_id.eq('diet_context')
               & contrasts.metric.eq('brier'))
    contrasts.loc[primary, 'role'] = 'primary_incremental_contrast'
    points = metrics.query("weighting == 'Survey weighted'").set_index('model')
    contrasts['relative_loss_reduction_percent'] = [
        100 * r.difference / points.loc[r.reference, r.metric] if r.metric in ('brier', 'log_loss') else np.nan
        for r in contrasts.itertuples()]
    calibration, rocs = [], []
    for name in results['manifest']['models']:
        b = calibration_bins(wide.y, wide[name], wide.weight); b['model'] = name; calibration.append(b)
        fpr, tpr, _ = roc_curve(wide.y, wide[name], sample_weight=wide.weight)
        rocs.append(pd.DataFrame({'model': name, 'fpr': fpr, 'tpr': tpr}))
    covariates = frame.reset_index(names='row_index')[['row_index', 'age', 'sex', 'rural', 'education']]
    wide = wide.merge(covariates, on='row_index', validate='one_to_one')
    wide['age_group'] = pd.cut(wide.age, [14, 24, 44, 64, np.inf], labels=['15–24', '25–44', '45–64', '65+'])
    subgroup = []
    for feature_set in ('context', 'context_diet', 'symptoms', 'symptoms_diet'):
        name = 'spline_logistic__' + feature_set
        for variable in ('sex', 'age_group', 'rural', 'education'):
            for level, block in wide.groupby(variable, observed=True):
                subgroup.append({'model': name, 'variable': variable, 'level': str(level),
                                 **probability_metrics(block.y, block[name], block.weight)})
    grouped = results['grouped_importance'].copy()
    if 'block' not in grouped and 'group' in grouped:
        grouped = grouped.rename(columns={'group': 'block'})
    if 'block' not in grouped and 'feature' in grouped:
        grouped = grouped.rename(columns={'feature': 'block'})
    grouped['block'] = grouped['block'].replace({'intake': 'diet'})
    return {'model_performance': metrics, 'paired_contrasts': contrasts,
            'importance_stability': summarize_importance(results['importance']),
            'grouped_importance_stability': summarize_importance(grouped, 'block'),
            'calibration': pd.concat(calibration, ignore_index=True), 'roc': pd.concat(rocs, ignore_index=True),
            'subgroup_performance': pd.DataFrame(subgroup), '_bootstrap': boot, '_wide': wide}


def expanded_profile(frame, dictionary, numeric_features, variables):
    """Describe all new predictors on the original scale with full-design variance."""
    design = frame.loc[frame.weight.notna() & frame.weight.gt(0)].reset_index(drop=True)
    w = design.weight.to_numpy(float)
    eligible = design.eligible.to_numpy(bool)
    domains = {'No caries': eligible & design.caries.eq(0).to_numpy(),
               'Caries': eligible & design.caries.eq(1).to_numpy(), 'Total': eligible}
    psus = design.groupby('stratum', sort=False).psu.nunique()
    df = int(psus.sum() - len(psus)); critical = t.ppf(.975, df)
    meta = dictionary.drop_duplicates('variable').set_index('variable')
    scores, positions, rows = [], [], []
    for variable in variables:
        values = design[variable]
        numeric = variable in numeric_features
        levels = ['Mean'] if numeric else sorted(values.dropna().unique(), key=str)
        for domain, mask in domains.items():
            observed = mask & values.notna().to_numpy()
            n, denominator = int(observed.sum()), float(w[observed].sum())
            common = {'domain': domain, 'variable': variable, 'description': meta.loc[variable, 'description'],
                      'kind': 'numeric' if numeric else 'categorical', 'domain_n': int(mask.sum()),
                      'observed_n': n, 'missing_n': int(mask.sum()) - n, 'df': df,
                      'units': meta.loc[variable, 'units'] if numeric else 'percent'}
            for level in levels:
                selected = observed if numeric else observed & values.eq(level).fillna(False).to_numpy(bool)
                record = {**common, 'row_type': 'mean' if numeric else 'category', 'level': level,
                          'n': int(selected.sum()), 'estimate': np.nan, 'weighted_sd': np.nan,
                          'ci_low': np.nan, 'ci_high': np.nan, 'se': np.nan}
                if n:
                    y = values[observed].to_numpy(float) if numeric else selected[observed].astype(float)
                    mean = np.average(y, weights=w[observed]); scale = 1 if numeric else 100
                    record['estimate'] = mean * scale
                    if numeric:
                        record['weighted_sd'] = np.sqrt(np.average((y - mean)**2, weights=w[observed]))
                    score = np.zeros(len(design)); score[observed] = w[observed] * (y - mean) / denominator * scale
                    scores.append(score); positions.append(len(rows))
                rows.append(record)
            rows.append({**common, 'row_type': 'missing', 'level': 'Missing', 'n': common['missing_n'],
                         'estimate': np.nan, 'weighted_sd': np.nan, 'se': np.nan, 'ci_low': np.nan, 'ci_high': np.nan})
    variance = np.diag(design_covariance(np.column_stack(scores), design.stratum, design.psu, singleton='average'))
    for position, value in zip(positions, variance, strict=True):
        r = rows[position]; se = np.sqrt(max(0, value)); r['se'] = se
        if r['kind'] == 'numeric':
            r['ci_low'], r['ci_high'] = r['estimate'] + np.array([-1, 1]) * critical * se
        elif 0 < r['estimate'] < 100:
            p = r['estimate'] / 100; half = critical * se / 100 / (p * (1-p))
            r['ci_low'], r['ci_high'] = 100 * expit(logit(p) + np.array([-half, half]))
    return pd.DataFrame(rows), {'design_n': len(design), 'n_psu': int(psus.sum()), 'n_strata': len(psus),
                               'df': df, 'singleton_strata': int(psus.eq(1).sum()), 'predictors': len(variables),
                               'domain_counts': {k: int(v.sum()) for k, v in domains.items()},
                               'variance': 'Full-design ratio linearization; average variance for singleton stratum',
                               'intervals': 'Design-t mean intervals and logit-t categorical intervals; boundary limits unavailable',
                               'imputation': False, 'numeric_scale': 'Original untransformed units'}
