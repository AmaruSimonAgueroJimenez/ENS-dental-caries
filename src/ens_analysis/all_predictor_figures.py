"""Composite figures preserving the earlier manuscript's model/importance sequence."""
from __future__ import annotations

import json
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.colors import Normalize
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
import numpy as np
import pandas as pd

from .all_predictor_results import FAMILIES, SCENARIOS
from .dietary_figures import LABELS, BLUE, style, interval, shared_limits, save

NAMES = {'spline_logistic': 'Spline logistic', 'random_forest': 'Random forest',
         'hist_gradient_boosting': 'Gradient boosting', 'mlp': 'Neural network'}
SHORT = {'spline_logistic': 'Spline', 'random_forest': 'Forest',
         'hist_gradient_boosting': 'Boosting', 'mlp': 'Neural'}
COLOURS = {'spline_logistic': '#176B87', 'random_forest': BLUE,
           'hist_gradient_boosting': '#A45A32', 'mlp': '#7851A9'}
MARKERS = ['o', 's', '^', 'D']
MODE = {'context_diet_labels': '38 predictors', 'symptoms_diet_labels': '44 predictors'}
# A reporting resolution, never a model-selection or significance threshold.
DISPLAY_TOLERANCE = 1e-8


def read(tables, name):
    return pd.read_csv(tables / (name + '.csv'), keep_default_na=False, na_values=[''])


def performance(tables, target, metadata):
    data = read(tables, 'model_performance').query("weighting == 'Survey weighted'").set_index('model')
    fig, axes = plt.subplots(2, 2, figsize=(180/25.4, 171/25.4))
    metrics = ['auc', 'brier', 'log_loss', 'calibration_slope']
    titles = ['Discrimination', 'Probability error', 'Probability loss', 'Calibration slope']
    for panel, (ax, metric, title) in enumerate(zip(axes.flat, metrics, titles)):
        style(ax, f'{chr(97+panel)}  {title}', 'y')
        for i, family in enumerate(FAMILIES):
            for s, scenario in enumerate(SCENARIOS):
                row = data.loc[f'{family}__{scenario}']; x = i + (s-.5)*.35
                if metric == 'calibration_slope':
                    ax.scatter(x, row[metric], marker=['o', 's'][s], s=26,
                               facecolor='white' if s == 0 else COLOURS[family],
                               edgecolor=COLOURS[family], zorder=5)
                else:
                    ax.bar(x, row[metric], width=.32, color=COLOURS[family],
                           hatch='//' if s == 0 else '', edgecolor='#333333', linewidth=.3)
                    ax.vlines(x, row[metric+'_low'], row[metric+'_high'], color='#222222', lw=.7)
        ax.set_xticks(range(4), [SHORT[f] for f in FAMILIES], rotation=30, ha='right')
        ax.set_ylabel({'auc': 'AUC', 'brier': 'Brier score', 'log_loss': 'Log loss',
                       'calibration_slope': 'Slope (point estimate)'}[metric])
        if metric == 'auc': ax.set_ylim(0, 1); ax.axhline(.5, color='#888888', ls='--', lw=.7)
        elif metric != 'calibration_slope': ax.set_ylim(bottom=0)
        else: ax.axhline(1, color='#888888', ls='--', lw=.7)
    fig.legend([Patch(facecolor='#BBBBBB', hatch='//', edgecolor='#333333'),
                Patch(facecolor='#BBBBBB', edgecolor='#333333')],
               ['38 predictors: without oral-status block', '44 predictors: complete model'],
               loc='upper center', ncol=1, frameon=False, fontsize=7)
    fig.subplots_adjust(left=.12, right=.97, top=.88, bottom=.12, hspace=.5, wspace=.4)
    caption = ('Comparison of four model families in 5,036 dentate participants. Hatched bars use 38 context, dietary and label constructs; '
               'solid bars add six concurrent oral-status indicators (44 constructs). Panels a–c show weighted held-out estimates and conditional '
               '95% intervals from 1,000 shared stratified PSU-bootstrap draws. Panel d shows calibration-slope estimates without intervals, '
               'with open circles for 38 predictors and filled squares for 44. Reference lines are AUC 0.5 and slope 1. Higher AUC and lower '
               'Brier/log loss are desirable; a slope near one indicates less calibration distortion. These are exploratory internally validated '
               'classification results, with all preprocessing and tuning inside training partitions. Neural pooled slopes are sensitive to '
               'small between-fold shifts because the fitted neural probabilities are nearly constant within folds. No outer results were used to choose a model.')
    save(fig, 'figure1_performance', target, metadata, caption, ['model_performance.csv'], 4, tables)


def importance_panel(ax, rows, features, title, limits):
    style(ax, title)
    block = rows.set_index('feature').loc[features]
    y = np.arange(len(features))
    ax.barh(y, block.brier_increase, color=BLUE, edgecolor='#333333', lw=.25, height=.66)
    for i, row in enumerate(block.itertuples()):
        interval(ax, i, row.minimum_fold_brier_increase, row.maximum_fold_brier_increase)
    ax.set_yticks(y, [LABELS.get(f, f.replace('_', ' ')) for f in features])
    ax.invert_yaxis(); ax.set_xlim(limits); ax.axvline(0, color='#555555', lw=.6)
    ax.set_xlabel('Held-out Brier increase'); ax.locator_params(axis='x', nbins=3)
    ax.tick_params(axis='y', labelsize=6.5)


def importance_figure(tables, target, metadata, scenario, name, features=None, title_kind='leading'):
    data = read(tables, 'importance_stability')
    subset = data.loc[data.model.str.endswith('__'+scenario)]
    if features is not None: subset = subset.loc[subset.feature.isin(features)]
    ordered = {}
    for family in FAMILIES:
        rows = subset.loc[subset.model.eq(f'{family}__{scenario}')]
        ordered[family] = (list(features) if features is not None else
                           rows.sort_values(['brier_increase', 'feature'], ascending=[False, True]).feature.head(12).tolist())
    shown = pd.concat([subset.loc[subset.model.eq(f'{family}__{scenario}') & subset.feature.isin(ordered[family])]
                       for family in FAMILIES])
    limits = shared_limits(shown[['minimum_fold_brier_increase', 'maximum_fold_brier_increase']].to_numpy().ravel())
    height = 221 if max(map(len, ordered.values())) >= 15 else 199
    fig, axes = plt.subplots(2, 2, figsize=(180/25.4, height/25.4))
    for i, (ax, family) in enumerate(zip(axes.flat, FAMILIES)):
        rows = subset.loc[subset.model.eq(f'{family}__{scenario}')]
        if features is None and rows.brier_increase.abs().max() < DISPLAY_TOLERANCE:
            style(ax, f'{chr(97+i)}  {NAMES[family]}')
            ax.set_xlim(limits); ax.set_yticks([]); ax.set_ylim(0, 1)
            ax.axvline(0, color='#555555', lw=.6); ax.set_xlabel('Held-out Brier increase')
            ax.locator_params(axis='x', nbins=3)
            ax.text(.56, .65, 'No interpretable ranking\n\nAll '+str(len(rows))+' absolute mean\nBrier increases below 10⁻⁸\n\nNearly constant predictions',
                    transform=ax.transAxes, ha='center', va='center', fontsize=7, color='#444444',
                    bbox={'facecolor':'white', 'edgecolor':'none', 'alpha':.95})
        else:
            importance_panel(ax, rows, ordered[family], f'{chr(97+i)}  {NAMES[family]}', limits)
            if rows.brier_increase.abs().max() < DISPLAY_TOLERANCE:
                ax.text(.96, .97, 'All |mean ΔBrier| < 10⁻⁸', transform=ax.transAxes, ha='right', va='top',
                        fontsize=6.3, color='#444444', bbox={'facecolor':'white', 'edgecolor':'none', 'alpha':.95})
    fig.subplots_adjust(left=.235, right=.97, top=.94, bottom=.075, hspace=.38, wspace=1.12)
    selection = ('The 12 largest signed mean importances are shown separately within each family; their selection is descriptive and is not predictor screening. '
                 'All 44 constructs remain fitted and their complete signed results are tabulated.' if title_kind == 'leading' and scenario == SCENARIOS[1]
                 else 'The 12 largest signed mean importances are shown for each 38-predictor model; all 38 constructs remain fitted.'
                 if features is None else 'Every construct in the indicated block is shown in questionnaire order, including zero and negative values.')
    caption = (f'Individual predictor importance in the {MODE[scenario]} scenario. {selection} '
               'Blue horizontal bars preserve the visual line of the earlier manuscript. Bars are means of held-out Brier increases after marginal '
               'permutation of each raw variable, with five perturbations per outer fold and weighting by fold expansion totals. Whiskers are the '
               'minimum and maximum fold means, not confidence intervals. A positive value indicates deterioration after perturbation; a negative '
               'value indicates improvement. Correlated predictors may share information, and permutation can create unusual combinations. '
               'The scores describe reliance of these fitted classifiers, not causal effects, protective directions or percentages of disease explained. '
               'Neural results below absolute mean Brier 10⁻⁸ are marked as numerically near zero; this reporting rule was adopted after observing '
               'nearly constant predictions and is not a significance threshold. Neural ranks, sign fractions and AUC permutation scores are not interpreted; '
               'all raw signed values remain available in the supplementary results.')
    save(fig, name, target, metadata, caption, ['importance_stability.csv'], 4, tables)


def rank_matrix(tables, target, metadata):
    data = read(tables, 'importance_stability')
    blocks = json.loads((tables/'feature_blocks.json').read_text())
    fig, axes = plt.subplots(2, 2, figsize=(180/25.4, 222/25.4), gridspec_kw={'height_ratios': [1.6, 1]})
    cmap = plt.get_cmap('Blues_r').copy(); cmap.set_bad('#E6E6E6'); norm = Normalize(1, 44)
    near_zero = [data.loc[data.model.eq(f'{f}__{SCENARIOS[1]}'), 'brier_increase'].abs().max() < DISPLAY_TOLERANCE
                 for f in FAMILIES]
    for i, (ax, block, title) in enumerate(zip(axes.flat, ['context', 'diet', 'labels', 'symptoms'],
                                            ['Context', 'Dietary intake', 'Label behaviours', 'Concurrent oral status'])):
        features = blocks[block]
        values = np.column_stack([data.loc[data.model.eq(f'{f}__{SCENARIOS[1]}')].set_index('feature').loc[features, 'overall_rank']
                                  for f in FAMILIES])
        shown = values.copy()
        for column, suppressed in enumerate(near_zero):
            if suppressed: shown[:, column] = np.nan
        pic = ax.imshow(shown, aspect='auto', cmap=cmap, norm=norm)
        ax.set_title(f'{chr(97+i)}  {title}', loc='left', fontsize=8, fontweight='bold', pad=8)
        ax.set_yticks(range(len(features)), [LABELS.get(f, f.replace('_', ' ')) for f in features], fontsize=6.5)
        ax.set_xticks(range(4), [SHORT[f] for f in FAMILIES], fontsize=6.5, rotation=30, ha='right')
        ax.tick_params(length=0)
        for row in range(len(features)):
            for col in range(4):
                if near_zero[col]:
                    ax.text(col, row, '≈0', ha='center', va='center', fontsize=6.5, color='#555555')
                    continue
                value = values[row, col]; rgb = np.asarray(cmap(norm(value))[:3])
                linear = np.where(rgb <= .04045, rgb/12.92, ((rgb+.055)/1.055)**2.4)
                luminance = linear @ np.array([.2126, .7152, .0722])
                ax.text(col, row, str(int(value)), ha='center', va='center', fontsize=6.5,
                        color='white' if luminance < .179 else '#111111')
        for spine in ax.spines.values(): spine.set_visible(False)
    fig.subplots_adjust(left=.235, right=.97, top=.95, bottom=.17, hspace=.42, wspace=1.1)
    cbax = fig.add_axes([.28, .080, .58, .018]); cb = fig.colorbar(pic, cax=cbax, orientation='horizontal')
    cb.set_ticks([1, 10, 20, 30, 44]); cb.ax.tick_params(labelsize=7)
    cb.set_label('Rank within the complete model (1 = largest mean Brier increase)', fontsize=7)
    fig.text(.56, .009, 'Grey ≈0: all absolute mean Brier increases in the model are below 10⁻⁸', ha='center', fontsize=6.3)
    caption = ('Ranks of all 44 predictors in the complete models, displayed by questionnaire block. Each column ranks the same 44 raw constructs '
               'within one family using weighted mean held-out Brier permutation increase; rank 1 is the largest signed score and ties receive '
               'their minimum rank. The common 1–44 scale is ordinal: differences in rank are not equal differences in predictive information. '
               'A rank does not establish a positive importance or statistical significance. Signed magnitudes are provided in Table 3 and complete '
               'fold-stability results in the supplement. Grey cells mark models whose absolute mean Brier importances are all below 10⁻⁸; '
               'ranks at that scale are not interpreted. This is a presentation rule applied after observing the near-constant neural predictions, '
               'not a significance threshold or a model-selection rule. The raw signed scores and computed ranks remain in the aggregate tables. '
               'Water, label behaviours and concurrent symptoms remain visible irrespective of their rank.')
    save(fig, 'figure3_all_predictor_ranks', target, metadata, caption,
         ['importance_stability.csv', 'feature_blocks.json'], 4, tables)


def block_figure(tables, target, metadata):
    data = read(tables, 'grouped_importance_stability')
    limits = shared_limits(data[['minimum_fold_brier_increase', 'maximum_fold_brier_increase']].to_numpy().ravel())
    fig, axes = plt.subplots(2, 2, figsize=(180/25.4, 166/25.4))
    blocks = ['context', 'diet', 'labels', 'symptoms']
    labels = ['Context (17)', 'Diet (15)', 'Labels (6)', 'Oral status (6)']
    for i, (ax, family) in enumerate(zip(axes.flat, FAMILIES)):
        style(ax, f'{chr(97+i)}  {NAMES[family]}')
        for s, scenario in enumerate(SCENARIOS):
            rows = data.loc[data.model.eq(f'{family}__{scenario}')].set_index('block')
            for j, block in enumerate(blocks):
                if block not in rows.index: continue
                row = rows.loc[block]; y = j + (s-.5)*.34
                ax.barh(y, row.brier_increase, height=.3, color=BLUE, hatch='//' if s == 0 else '',
                        edgecolor='#333333', linewidth=.3)
                interval(ax, y, row.minimum_fold_brier_increase, row.maximum_fold_brier_increase)
        ax.set_yticks(range(4), labels); ax.invert_yaxis(); ax.set_xlim(limits)
        ax.axvline(0, color='#555555', lw=.7); ax.set_xlabel('Joint held-out Brier increase')
        ax.locator_params(axis='x', nbins=3)
    fig.legend([Patch(facecolor=BLUE, hatch='//', edgecolor='#333333'), Patch(facecolor=BLUE, edgecolor='#333333')],
               ['38 predictors', '44 predictors'], loc='upper center', frameon=False, ncol=2, fontsize=7)
    fig.subplots_adjust(left=.17, right=.97, top=.91, bottom=.095, hspace=.45, wspace=.65)
    caption = ('Joint importance of the four predictor blocks in each model family. A single donor-row permutation moves all raw variables of a '
               'block together, preserving internal combinations. Hatched bars use 38 predictors and solid bars use 44; oral-status information '
               'is absent from the 38-predictor scenario. Bars are weighted averages of fold means and whiskers span minimum–maximum fold means '
               'over five outer folds, with five perturbations per block per fold. Ranges are not confidence intervals. The common signed axis '
               'preserves negative estimates. Block sizes differ; scores are not additive, are not percentages explained, and do not measure '
               'improvement from refitting after removing a block.')
    save(fig, 'figure4_blocks', target, metadata, caption, ['grouped_importance_stability.csv'], 4, tables)


def validation(tables, target, metadata):
    roc, cal, pairs = [read(tables, n) for n in ['roc', 'calibration', 'paired_contrasts']]
    fig, axes = plt.subplots(3, 2, figsize=(180/25.4, 221/25.4))
    for row, scenario in enumerate(reversed(SCENARIOS)):
        a, b = axes[row]
        for i, family in enumerate(FAMILIES):
            model = f'{family}__{scenario}'
            r = roc.loc[roc.model.eq(model)]; c = cal.loc[cal.model.eq(model)]
            a.plot(r.fpr, r.tpr, color=COLOURS[family], ls=['-', '--', '-.', ':'][i], lw=1)
            b.plot(c.predicted, c.observed, color=COLOURS[family], marker=MARKERS[i], ms=3, lw=.8,
                   ls=['-', '--', '-.', ':'][i])
        style(a, f'{chr(97+row*2)}  ROC / {MODE[scenario]}', 'both')
        style(b, f'{chr(98+row*2)}  Calibration / {MODE[scenario]}', 'both')
        for ax in (a, b):
            ax.plot([0, 1], [0, 1], color='#999999', ls=':', lw=.7); ax.set_xlim(0, 1); ax.set_ylim(0, 1)
        a.set_xlabel('False-positive rate'); a.set_ylabel('True-positive rate')
        b.set_xlabel('Mean predicted probability'); b.set_ylabel('Observed proportion')
    for ax, metric, title in zip(axes[2], ['auc', 'brier'], ['e  Neural network: AUC gain', 'f  Neural network: Brier gain']):
        style(ax, title)
        rows = []
        for s, scenario in enumerate(SCENARIOS):
            for i, family in enumerate(FAMILIES[:-1]):
                r = pairs.loc[pairs.contrast_id.eq(f'neural_vs_{family}__{scenario}') & pairs.metric.eq(metric)].iloc[0]
                y = s*3+i; rows.append(r)
                ax.scatter(r.difference, y, color=COLOURS[family], marker=['o', 's'][s], s=22, zorder=5)
                interval(ax, y, r.ci_low, r.ci_high)
        limits = shared_limits([x for r in rows for x in (r.ci_low, r.ci_high)], .18)
        ax.set_xlim(limits); ax.axvline(0, color='#555555', lw=.7)
        ax.set_yticks(range(6), [SHORT[f]+' / '+str(n) for n in [38, 44] for f in FAMILIES[:-1]])
        ax.invert_yaxis(); ax.axhline(2.5, color='#BBBBBB', lw=.6); ax.set_xlabel('Positive favours neural network')
        ax.locator_params(axis='x', nbins=3); ax.tick_params(axis='y', labelsize=6.5)
    fig.legend([Line2D([], [], color=COLOURS[f], marker=MARKERS[i], ls=['-', '--', '-.', ':'][i], lw=.9, ms=3)
                for i, f in enumerate(FAMILIES)], [SHORT[f] for f in FAMILIES], loc='upper center',
               ncol=4, fontsize=7, frameon=False)
    fig.subplots_adjust(left=.16, right=.97, top=.93, bottom=.075, hspace=.65, wspace=.48)
    caption = ('ROC and calibration displays for the complete 44-predictor models (a–b) and the 38-predictor complementary models (c–d). '
               'Calibration points use deciles of held-out predictions with weighted mean predicted and observed proportions; lines are descriptive '
               'and do not carry intervals. The diagonal is the reference for chance discrimination or ideal calibration. Panels e–f compare the '
               'neural network with each classical family on identical held-out participants. Labels identify the reference family and number of '
               'constructs. Positive AUC gain or reference-minus-neural Brier gain favours the neural model. Whiskers are paired conditional '
               '95% intervals from 1,000 shared PSU-bootstrap draws, with fitted models and tuning decisions held fixed. All comparisons are exploratory.')
    save(fig, 'figure5_validation', target, metadata, caption, ['roc.csv', 'calibration.csv', 'paired_contrasts.csv'], 6, tables)


def make_all_predictor_figures(tables, target):
    tables, target = Path(tables), Path(target); target.mkdir(parents=True, exist_ok=True)
    blocks = json.loads((tables/'feature_blocks.json').read_text())
    metadata = {'scope': 'All-predictor proposal: classical models and a regularized neural benchmark', 'figures': []}
    with plt.rc_context({'font.family': 'DejaVu Sans', 'pdf.fonttype': 42, 'ps.fonttype': 42,
                         'svg.fonttype': 'none', 'axes.unicode_minus': True}):
        performance(tables, target, metadata)
        importance_figure(tables, target, metadata, SCENARIOS[1], 'figure2_leading_predictors')
        rank_matrix(tables, target, metadata)
        block_figure(tables, target, metadata)
        validation(tables, target, metadata)
        for name, features in [('supplementary1_context_importance', blocks['context']),
                               ('supplementary2_diet_importance', blocks['diet']),
                               ('supplementary3_labels_symptoms', blocks['labels']+blocks['symptoms'])]:
            importance_figure(tables, target, metadata, SCENARIOS[1], name, features, 'complete_block')
        importance_figure(tables, target, metadata, SCENARIOS[0], 'supplementary4_non_symptom_leaders')
    (target/'figure_metadata.json').write_text(json.dumps(metadata, indent=2, ensure_ascii=False)+'\n')
    return metadata
