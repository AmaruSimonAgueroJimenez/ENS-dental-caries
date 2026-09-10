"""Publication figures drawn only from the new dietary-proposal aggregates."""
from __future__ import annotations

from pathlib import Path
import hashlib
import json

import matplotlib.pyplot as plt
from matplotlib.patches import Patch
import numpy as np
import pandas as pd

from .dietary_results import FAMILIES, SET_ORDER
from .figures import FEATURE_LABELS

BLUE = '#4472C4'
COLOURS = {'spline_logistic': '#176B87', 'random_forest': BLUE, 'hist_gradient_boosting': '#A45A32'}
NAMES = {'spline_logistic': 'Spline logistic', 'random_forest': 'Random forest', 'hist_gradient_boosting': 'Gradient boosting'}
SHORT = ['B', 'C', 'C+D', 'C+D+L', 'C+S', 'C+S+D', 'C+S+D+L']
LABELS = {**FEATURE_LABELS,
          'soda_glasses_daily': 'Soda volume', 'juice_glasses_daily': 'Juice volume',
          'income_band': 'Household income', 'health_insurance': 'Health insurance',
          'smoking_status': 'Smoking status', 'diabetes_reported': 'Reported diabetes',
          'dental_visit': 'Last dental visit', 'dental_visit_reason': 'Reason for dental visit',
          'dental_access': 'Dental access / need', 'household_water_source': 'Household water supply',
          'rural_water_consumption': 'Rural / well water', 'oral_health_selfrating': 'Self-rated oral health',
          'oral_pain': 'Oral pain', 'oral_speech': 'Speech discomfort', 'oral_eating': 'Eating discomfort',
          'oral_daily_activities': 'Daily activity impact', 'oral_social': 'Social impact'}


def style(ax, title, axis='x'):
    ax.set_title(title, loc='left', fontsize=8, fontweight='bold', pad=8)
    ax.tick_params(labelsize=7, length=2)
    ax.grid(axis=axis, color='#E7E7E7', linewidth=.5); ax.set_axisbelow(True)
    ax.spines[['top', 'right']].set_visible(False)
    ax.xaxis.label.set_size(7); ax.yaxis.label.set_size(7)


def interval(ax, y, lo, hi):
    ax.hlines(y, lo, hi, color='#222222', linewidth=.7, zorder=5)
    ax.vlines([lo, hi], y-.08, y+.08, color='#222222', linewidth=.7, zorder=5)


def shared_limits(values, pad=.12):
    finite = np.asarray(values, float); finite = finite[np.isfinite(finite)]
    low, high = min(0., finite.min()), max(0., finite.max())
    span = max(high-low, .0001)
    return low-span*pad, high+span*pad


def save(fig, name, target, metadata, caption, sources, panels, source_dir):
    paths = []
    for ext in ('png', 'pdf', 'svg'):
        p = target / f'{name}.{ext}'
        options = {'metadata': {'Date': None}} if ext == 'svg' else {'metadata': {'CreationDate': None, 'ModDate': None}} if ext == 'pdf' else {}
        fig.savefig(p, dpi=300, facecolor='white', **options); paths.append(p)
    metadata['figures'].append({'name': name, 'panels': panels, 'caption': caption, 'alt_text': caption,
                               'width_mm': float(fig.get_figwidth()*25.4), 'height_mm': float(fig.get_figheight()*25.4),
                               'dpi': 300, 'source_tables': sources,
                               'source_sha256': {s: hashlib.sha256((source_dir/s).read_bytes()).hexdigest() for s in sources},
                               'files': {p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in paths}})
    plt.close(fig)


def performance(tables, target, metadata):
    data = pd.read_csv(tables/'model_performance.csv').query("weighting == 'Survey weighted'").set_index('model')
    fig, axes = plt.subplots(2, 2, figsize=(180/25.4, 172/25.4))
    for ax, metric, title in zip(axes.flat, ['auc', 'brier', 'log_loss', 'calibration_slope'],
                               ['a  Discrimination', 'b  Probability error', 'c  Probability loss', 'd  Calibration slope']):
        style(ax, title, 'y')
        for j, family in enumerate(FAMILIES):
            rows = data.loc[[family+'__'+s for s in SET_ORDER]]
            x = np.arange(7)+(j-1)*.24
            if metric == 'calibration_slope':
                ax.plot(x, rows[metric], ['o', 's', '^'][j], color=COLOURS[family], ms=3.5)
            else:
                ax.bar(x, rows[metric], width=.22, color=COLOURS[family], edgecolor='#333333', linewidth=.3, hatch=['', '//', '..'][j])
                ax.vlines(x, rows[metric+'_low'], rows[metric+'_high'], color='#222222', linewidth=.6)
            ax.set_xticks(range(7), SHORT, rotation=55, ha='right')
        ax.set_ylabel({'auc': 'AUC', 'brier': 'Brier score', 'log_loss': 'Log loss', 'calibration_slope': 'Slope (point estimate)'}[metric])
        if metric == 'auc': ax.set_ylim(0, 1); ax.axhline(.5, color='#777777', ls='--', lw=.6)
        elif metric != 'calibration_slope': ax.set_ylim(bottom=0)
        else: ax.axhline(1, color='#777777', ls='--', lw=.6)
    fig.legend([Patch(facecolor=COLOURS[f], hatch=['', '//', '..'][i], edgecolor='#333333') for i,f in enumerate(FAMILIES)],
               [NAMES[f] for f in FAMILIES], loc='upper center', ncol=3, fontsize=7, frameon=False)
    fig.subplots_adjust(left=.11, right=.97, top=.91, bottom=.13, hspace=.6, wspace=.38)
    caption = ('Performance of 21 newly developed model variants in 5,036 dentate participants. B, eight basic predictors; C, 17 context predictors; '
               'D, 15 direct-consumption indicators; L, six label behaviours; S, six concurrent oral-status indicators. Panels a–c show survey-weighted '
               'held-out estimates and conditional 95% intervals from 1,000 shared stratified PSU-bootstrap draws. Bars begin at zero. Panel d shows '
               'calibration-slope point estimates without confidence intervals. Dashed references are AUC 0.5 and calibration slope 1. '
               'Higher AUC, lower probability losses and slopes near one are desirable. No model is selected from these outer-validation results.')
    save(fig, 'figure1_performance', target, metadata, caption, ['model_performance.csv'], 4, tables)


def gains(tables, target, metadata):
    data = pd.read_csv(tables/'paired_contrasts.csv')
    fig, axes = plt.subplots(2, 3, figsize=(180/25.4, 151/25.4))
    metrics = ['brier', 'auc', 'log_loss']
    for col, metric in enumerate(metrics):
        selection = data.loc[data.metric.eq(metric) & data.contrast_id.isin(['diet_context', 'diet_symptoms'])]
        limits = shared_limits(selection[['ci_low', 'ci_high']].to_numpy().ravel(), .2)
        for row, contrast in enumerate(['diet_context', 'diet_symptoms']):
            ax = axes[row, col]; block = selection.loc[selection.contrast_id.eq(contrast)].set_index('family').loc[list(FAMILIES)]
            style(ax, f'{chr(97+row*3+col)}  '+['Brier gain', 'AUC gain', 'Log-loss gain'][col]
                  + ('\nBeyond context' if row == 0 else '\nBeyond context + symptoms'))
            for i, family in enumerate(FAMILIES):
                r = block.loc[family]; ax.scatter(r.difference, i, color=COLOURS[family], marker=['o', 's', '^'][i], s=24, zorder=6)
                interval(ax, i, r.ci_low, r.ci_high)
            ax.set_yticks(range(3), ['Spline', 'RF', 'Boost']); ax.invert_yaxis(); ax.set_xlim(limits)
            ax.axvline(0, color='#666666', lw=.8); ax.set_xlabel('Positive = improvement')
            ax.ticklabel_format(axis='x', style='plain', useOffset=False); ax.locator_params(axis='x', nbins=3)
    fig.subplots_adjust(left=.09, right=.97, top=.88, bottom=.11, wspace=.58, hspace=.7)
    caption = ('Incremental contribution of the 15 direct dietary indicators beyond context (a–c) and beyond context plus concurrent oral status (d–f). '
               'Brier and log-loss gains subtract the extended model loss from the reference loss; AUC gain subtracts reference AUC from extended AUC. '
               'Positive values uniformly favour adding diet. Whiskers are paired conditional 95% PSU-bootstrap intervals with shared draws. '
               'The spline Brier gain in panel a is the predeclared primary comparison for this dataset-informed extension; all other comparisons are complementary or exploratory. '
               'Zero-crossing intervals do not establish equivalence or the absence of a biological dietary role.')
    save(fig, 'figure2_dietary_gain', target, metadata, caption, ['paired_contrasts.csv'], 6, tables)


def importance_plot(ax, block, features, title, limits):
    style(ax, title)
    ordered = block.set_index('feature').loc[features]
    y = np.arange(len(features))
    ax.barh(y, ordered.brier_increase, color=BLUE, edgecolor='#333333', linewidth=.25, height=.65)
    for i, r in enumerate(ordered.itertuples()): interval(ax, i, r.minimum_fold_brier_increase, r.maximum_fold_brier_increase)
    ax.set_yticks(y, [LABELS.get(f, f.replace('_', ' ')) for f in features]); ax.invert_yaxis()
    ax.set_xlim(limits); ax.axvline(0, color='#444444', linewidth=.6); ax.set_xlabel('Held-out Brier increase')
    ax.locator_params(axis='x', nbins=4)


def diet_importance(tables, target, metadata):
    data = pd.read_csv(tables/'importance_stability.csv')
    features = json.loads((tables/'feature_blocks.json').read_text())['diet']
    subset = data.loc[data.feature.isin(features)]
    limits = shared_limits(subset[['minimum_fold_brier_increase', 'maximum_fold_brier_increase']].to_numpy().ravel())
    fig, axes = plt.subplots(2, 2, figsize=(180/25.4, 218/25.4))
    for row, scenario in enumerate(['context_diet_labels', 'symptoms_diet_labels']):
        for col, family in enumerate(FAMILIES[:2]):
            model = family+'__'+scenario; title = f'{chr(97+row*2+col)}  '+NAMES[family]+('\nContext + diet + labels' if row == 0 else '\nContext + symptoms\n+ diet + labels')
            importance_plot(axes[row, col], subset.loc[subset.model.eq(model)], features, title, limits)
    fig.subplots_adjust(left=.23, right=.97, top=.94, bottom=.07, hspace=.4, wspace=1.1)
    caption = ('All 15 direct dietary indicators in questionnaire order, retaining the blue horizontal bars of the original manuscript. '
               'Panels a–b use the context, diet and label models; panels c–d add concurrent oral-status indicators. Each variable is permuted before '
               'the trained transformations, five times per held-out fold. Bars are weighted averages of fold mean Brier increases; whiskers are '
               'minimum–maximum fold means, not confidence intervals. The signed axis is shared across panels and negative values are retained. '
               'Frequency and volume indicators are dependent, so isolated-variable perturbations may create unusual combinations. These scores describe '
               'model reliance and do not establish causal direction, dietary effects or fractions of disease explained. Water remains explicitly displayed.')
    save(fig, 'figure3_dietary_importance', target, metadata, caption, ['importance_stability.csv', 'feature_blocks.json'], 4, tables)


def blocks(tables, target, metadata):
    data = pd.read_csv(tables/'grouped_importance_stability.csv')
    raw = pd.read_csv(tables/'grouped_permutation_importance.csv')
    if 'block' not in raw and 'group' in raw: raw = raw.rename(columns={'group': 'block'})
    if 'block' not in raw: raw = raw.rename(columns={'feature': 'block'})
    fold = raw.groupby(['model', 'fold', 'block'], as_index=False).agg(auc_drop=('auc_drop', 'mean'))
    bounds = fold.groupby(['model', 'block']).auc_drop.agg(['min', 'max'])
    counts = {k: len(v) for k,v in json.loads((tables/'feature_blocks.json').read_text()).items()}
    fig, axes = plt.subplots(2, 2, figsize=(180/25.4, 167/25.4))
    labels = ['Context (17)', 'Diet (15)', 'Labels (6)', 'Context (17)', 'Symptoms (6)', 'Diet (15)', 'Labels (6)']
    block_order = ['context', 'diet', 'labels', 'context', 'symptoms', 'diet', 'labels']
    for col, family in enumerate(FAMILIES[:2]):
        models = [family+'__context_diet_labels']*3+[family+'__symptoms_diet_labels']*4
        for row, metric in enumerate(['brier_increase', 'auc_drop']):
            ax = axes[row, col]; style(ax, f'{chr(97+row*2+col)}  {NAMES[family]} / '+('Brier' if row == 0 else 'AUC'))
            for i, (model, block) in enumerate(zip(models, block_order)):
                r = data.loc[data.model.eq(model) & data.block.eq(block)].iloc[0]
                lo, hi = (r.minimum_fold_brier_increase, r.maximum_fold_brier_increase) if row == 0 else bounds.loc[(model, block)]
                ax.barh(i, r[metric], color=BLUE, hatch='' if i < 3 else '//', edgecolor='#333333', linewidth=.3)
                interval(ax, i, lo, hi)
            ax.set_yticks(range(7), labels); ax.invert_yaxis(); ax.axvline(0, color='#444444', lw=.7)
            ax.axhline(2.5, color='#AAAAAA', lw=.6); ax.set_xlabel('Brier increase' if row == 0 else 'AUC decrease')
            ax.locator_params(axis='x', nbins=4)
    for row in range(2):
        limits = shared_limits([v for ax in axes[row] for v in ax.get_xlim()]); [ax.set_xlim(limits) for ax in axes[row]]
    fig.legend([Patch(facecolor=BLUE, edgecolor='#333333'), Patch(facecolor=BLUE, edgecolor='#333333', hatch='//')],
               ['Without oral-status block', 'With oral-status block'], ncol=2, loc='upper center', fontsize=7, frameon=False)
    fig.subplots_adjust(left=.19, right=.97, top=.9, bottom=.08, hspace=.45, wspace=.7)
    caption = ('Joint permutation of complete predictor blocks in spline and random forest, shown as Brier increases (a–b) and AUC decreases (c–d). '
               'Unhatched bars correspond to C+D+L; hatched bars correspond to C+S+D+L. A single common donor permutation moves all raw variables in a block together, '
               'preserving their internal combinations. Counts of constructs are shown in labels. Bars combine fold means using held-out expansion totals; whiskers '
               'are minimum–maximum fold means and are not confidence intervals. Joint scores cannot be obtained by adding individual importances, and blocks of different '
               'sizes are not comparable percentages of explained disease. The refitted dietary comparison in Figure 2 addresses a different question.')
    save(fig, 'figure4_blocks', target, metadata, caption, ['grouped_importance_stability.csv', 'grouped_permutation_importance.csv'], 4, tables)


def validation(tables, target, metadata):
    roc = pd.read_csv(tables/'roc.csv'); cal = pd.read_csv(tables/'calibration.csv'); gain = pd.read_csv(tables/'paired_contrasts.csv')
    fig, axes = plt.subplots(3, 2, figsize=(180/25.4, 220/25.4))
    for row, pair in enumerate([('context', 'context_diet'), ('symptoms', 'symptoms_diet')]):
        ax, bx = axes[row]; scenario = 'Context' if row == 0 else 'Context + symptoms'
        style(ax, f'{chr(97+row*2)}  ROC: {scenario}', 'both'); style(bx, f'{chr(98+row*2)}  Calibration: {scenario}', 'both')
        for j, feature_set in enumerate(pair):
            model = 'spline_logistic__'+feature_set; r = roc.loc[roc.model.eq(model)]; b = cal.loc[cal.model.eq(model)]
            ax.plot(r.fpr, r.tpr, color=['#555555', BLUE][j], ls=['--', '-'][j], lw=1, label=['Reference', '+ Diet'][j])
            bx.plot(b.predicted, b.observed, color=['#555555', BLUE][j], ls=['--', '-'][j], marker=['s', 'o'][j], ms=3, lw=.8)
        for a in [ax, bx]: a.plot([0,1], [0,1], color='#BBBBBB', ls=':', lw=.7); a.set_xlim(0,1); a.set_ylim(0,1)
        ax.set_xlabel('False-positive rate'); ax.set_ylabel('True-positive rate'); ax.legend(fontsize=6, frameon=False, loc='lower right')
        bx.set_xlabel('Mean predicted probability'); bx.set_ylabel('Observed proportion')
    for col, metric in enumerate(['brier', 'auc']):
        ax = axes[2,col]; style(ax, f'{chr(101+col)}  Tail sensitivity: '+('Brier gain' if col == 0 else 'AUC gain'))
        selection = gain.loc[gain.metric.eq(metric) & gain.contrast_id.str.startswith('winsor_')].set_index('contrast_id')
        for i, s in enumerate(['context_diet', 'symptoms_diet']):
            r = selection.loc['winsor_'+s]; ax.scatter(r.difference, i, color=BLUE, marker=['o', 's'][i], s=24, zorder=6); interval(ax, i, r.ci_low, r.ci_high)
        ax.set_yticks([0,1], ['Context + diet', 'Symptoms + diet']); ax.invert_yaxis(); ax.axvline(0,color='#666666',lw=.7)
        ax.set_xlim(shared_limits(selection[['ci_low','ci_high']].to_numpy().ravel(), .2)); ax.set_xlabel('Capped minus uncapped performance gain'); ax.locator_params(axis='x',nbins=3)
    fig.subplots_adjust(left=.16, right=.97, top=.95, bottom=.065, hspace=.63, wspace=.53)
    caption = ('Validation of the primary spline information scenarios: ROC and binned calibration without oral status (a–b), and with oral status (c–d). '
               'Calibration bins summarize held-out predictions and do not have confidence intervals. Panels e–f compare the two additional spline sensitivities '
               'with their corresponding uncapped diet models. Sensitivities cap each beverage frequency/volume at its 99th percentile among positive training values '
               'before log1p; the cutoff and all other transformations are learned within each inner training set and outer refit. Paired conditional 95% intervals '
               'use the same 1,000 PSU-bootstrap draws. Positive gains favour the sensitivity. Neither validation display is an independent external validation.')
    save(fig, 'figure5_validation', target, metadata, caption, ['roc.csv','calibration.csv','paired_contrasts.csv'], 6, tables)


def supplementary(tables, target, metadata):
    importance = pd.read_csv(tables/'importance_stability.csv'); missing = pd.read_csv(tables/'predictor_missingness.csv').set_index('variable')
    definitions = json.loads((tables/'feature_blocks.json').read_text()); cal = pd.read_csv(tables/'calibration.csv')
    for scenario, name in [('context_diet_labels', 'supplement1_context_importance'), ('symptoms_diet_labels', 'supplement2_symptom_importance')]:
        groups = [definitions['context']+(definitions['symptoms'] if scenario.startswith('symptoms') else []), definitions['diet']+definitions['labels']]
        subset = importance.loc[importance.model.str.endswith('__'+scenario)]
        limits = shared_limits(subset[['minimum_fold_brier_increase','maximum_fold_brier_increase']].to_numpy().ravel())
        fig, axes = plt.subplots(2,2,figsize=(180/25.4,223/25.4))
        for row, family in enumerate(FAMILIES[:2]):
            for col, features in enumerate(groups):
                title = f'{chr(97+row*2+col)}  {NAMES[family]}\n'+('Non-dietary information' if col == 0 else 'Diet and label behaviours')
                importance_plot(axes[row,col], subset.loc[subset.model.eq(family+'__'+scenario)], features, title, limits)
                axes[row,col].tick_params(axis='y',labelsize=6.5)
        fig.subplots_adjust(left=.24,right=.97,top=.94,bottom=.055,hspace=.35,wspace=1.25)
        caption = ('Complete individual contributions for the '+('symptom-aware 44-predictor' if scenario.startswith('symptoms') else '38-predictor context')+
                   ' scenario. Every predictor appears once per model. Columns divide the display into non-dietary and diet/label information without changing '
                   'the fitted models. Signed bars are weighted fold-mean Brier increases; whiskers are minimum–maximum fold means, not confidence intervals. '
                   'Five held-out permutations per predictor and fold are used; shared scales retain negative values. These are marginal, model-specific reliance measures.')
        save(fig,name,target,metadata,caption,['importance_stability.csv','feature_blocks.json'],4,tables)
    fig,axes=plt.subplots(2,2,figsize=(180/25.4,206/25.4))
    for ax,(i,(block,features)) in zip(axes.flat,enumerate(definitions.items())):
        style(ax,f'{chr(97+i)}  {block.capitalize()} ({len(features)})')
        values=missing.loc[features,'percent_missing'];ax.barh(range(len(features)),values,color=BLUE)
        ax.set_yticks(range(len(features)),[LABELS.get(f,f.replace('_',' ')) for f in features]);ax.invert_yaxis();ax.set_xlim(0,max(16,missing.percent_missing.max()*1.12));ax.set_xlabel('Missing (%)')
        for j,v in enumerate(values):ax.text(v+.2,j,f'{v:.1f}',va='center',fontsize=6)
    fig.subplots_adjust(left=.24,right=.97,top=.94,bottom=.065,hspace=.3,wspace=1.25)
    save(fig,'supplement3_missingness',target,metadata,'Observed predictor missingness in the 5,036-person analytical cohort, before any statistical imputation. All 44 predictors are shown by predefined block. Structural categories and confirmed nonconsumption have already been resolved; zero bars denote fully observed derived predictors. Numbers are unweighted percentages rounded to one decimal place; a displayed 0.0 may represent a very small nonzero proportion. No participant is excluded because of these missing predictors.',['predictor_missingness.csv','feature_blocks.json'],4,tables)
    fig,axes=plt.subplots(2,3,figsize=(180/25.4,150/25.4))
    for row,pair in enumerate([('context','context_diet'),('symptoms','symptoms_diet')]):
        for col,family in enumerate(FAMILIES):
            ax=axes[row,col];style(ax,f'{chr(97+row*3+col)}  '+['Spline','Random forest','Boosting'][col],'both')
            for j,s in enumerate(pair):
                b=cal.loc[cal.model.eq(family+'__'+s)];ax.plot(b.predicted,b.observed,color=['#555555',BLUE][j],ls=['--','-'][j],marker=['s','o'][j],ms=3,lw=.8,label=['Reference','+ Diet'][j])
            ax.plot([0,1],[0,1],color='#BBBBBB',ls=':',lw=.6);ax.set_xlim(0,1);ax.set_ylim(0,1);ax.set_xlabel('Predicted');ax.set_ylabel('Observed')
            ax.legend(fontsize=6,frameon=False,loc='upper left');ax.text(.98,.02,'Context' if row==0 else 'Context + symptoms',ha='right',fontsize=6,transform=ax.transAxes)
    fig.subplots_adjust(left=.1,right=.97,top=.93,bottom=.11,hspace=.55,wspace=.5)
    save(fig,'supplement4_calibration',target,metadata,'Held-out calibration summaries for the three model families without and with direct dietary indicators. The first row uses context references; the second adds oral-status indicators. Points are survey-weighted observed proportions in prediction-quantile bins. Lines connect bin summaries and have no confidence intervals; the dotted diagonal marks perfect calibration.',['calibration.csv'],6,tables)


def make_dietary_figures(tables, destination):
    tables, destination = Path(tables), Path(destination); destination.mkdir(parents=True, exist_ok=True)
    metadata={'scope':'New dietary incremental-value proposal only','figures':[]}
    with plt.rc_context({'font.family':'Arial','font.size':7,'svg.fonttype':'none','svg.hashsalt':'dietary-2026',
                         'pdf.fonttype':42,'axes.unicode_minus':True,'figure.facecolor':'white'}):
        for function in [performance,gains,diet_importance,blocks,validation,supplementary]:
            function(tables,destination,metadata)
    (destination/'figure_metadata.json').write_text(json.dumps(metadata,indent=2,ensure_ascii=False)+'\n')
    return metadata
