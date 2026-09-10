"""Compound figures retaining the supplied Correa manuscript's bar-chart language.

The two embedded original performance rasters and native horizontal Word chart
were inspected as visual references. All plotted values come from the completed
Python reanalysis; old numerical rankings and resampling labels are not reused.
"""
from __future__ import annotations

from pathlib import Path
import hashlib
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.patches import Patch
from matplotlib.ticker import MaxNLocator

from .figures import _save, _weighted, _importance_summary, _common_limits, FEATURE_LABELS

MODEL_ORDER = ['decision_tree', 'knn', 'logistic', 'random_forest', 'svm', 'spline_logistic', 'hist_gradient_boosting']
MODEL_COLOURS = dict(zip(MODEL_ORDER, ['#1F77B4','#2CA02C','#D62728','#9467BD','#FF7F0E','#168A8A','#7F7F7F'], strict=True))
MODEL_NAMES = ['Decision tree','KNN','Logistic regression','Random forest','SVM','Spline logistic','Gradient boosting']
MODEL_SHORT = ['DT','KNN','LR','RF','SVM','Spline','HGB']
HATCHES = ['', '//', '..', '\\\\', 'xx', '--', '++']
BLUE = '#4472C4'


def panel(ax, name, *, axis='y'):
    ax.set_title(name,loc='left',fontsize=8,fontweight='bold',pad=8,color='black')
    ax.tick_params(labelsize=7,length=2)
    ax.grid(axis=axis,color='#E5E5E5',linewidth=.65)
    ax.set_axisbelow(True)
    ax.spines['top'].set_visible(False);ax.spines['right'].set_visible(False)
    ax.spines['left'].set_color('#AAAAAA');ax.spines['bottom'].set_color('#AAAAAA')
    ax.xaxis.label.set_size(7);ax.yaxis.label.set_size(7)


def vertical_interval(ax,x,low,high,width=.05):
    if np.isfinite(low) and np.isfinite(high):
        ax.vlines(x,low,high,color='black',linewidth=.65,zorder=5)
        ax.hlines([low,high],x-width,x+width,color='black',linewidth=.65,zorder=5)


def horizontal_interval(ax,y,low,high):
    ax.hlines(y,low,high,color='black',linewidth=.7,zorder=5)
    ax.vlines([low,high],y-.1,y+.1,color='black',linewidth=.7,zorder=5)


def performance(table,destination,metadata):
    data=_weighted(table).set_index('model').loc[MODEL_ORDER]
    metrics=['sensitivity','specificity','ppv','accuracy','auc','brier','log_loss']
    required=[f'{m}_{bound}' for m in metrics for bound in ['low','high']]
    if not set(required).issubset(data):
        raise ValueError('Correa-style performance panels require shared PSU-bootstrap intervals for all displayed metrics')
    fig=plt.figure(figsize=(180/25.4,176/25.4))
    grid=fig.add_gridspec(2,3,height_ratios=[1.1,1],hspace=.43,wspace=.38)
    ax=fig.add_subplot(grid[0,:]);panel(ax,'a  Classification metrics at probability threshold 0.5')
    width=.102
    for i,model in enumerate(MODEL_ORDER):
        row=data.loc[model]
        for j,metric in enumerate(metrics[:4]):
            x=j+(i-3)*width
            ax.bar(x,row[metric],width=.091,color=MODEL_COLOURS[model],edgecolor='#333333',linewidth=.3,hatch=HATCHES[i],zorder=3)
            vertical_interval(ax,x,row[f'{metric}_low'],row[f'{metric}_high'],.024)
            ax.text(x,row[f'{metric}_high']+.019,f'{row[metric]:.3f}',ha='center',va='bottom',rotation=90,fontsize=7,color='black')
    ax.set_xticks(np.arange(4),['Sensitivity','Specificity','Precision (PPV)','Accuracy'])
    ax.set_xlim(-.48,3.48);ax.set_ylim(0,1.13);ax.set_yticks([0,.25,.5,.75,1]);ax.set_ylabel('Proportion')
    for col,(metric,title,ylabel,limit) in enumerate([
        ('auc','b  Discrimination','AUC',1.1),('brier','c  Probability error','Brier score',.35),('log_loss','d  Probability loss','Log loss',1.04)]):
        ax=fig.add_subplot(grid[1,col]);panel(ax,title)
        for i,model in enumerate(MODEL_ORDER):
            row=data.loc[model]
            ax.bar(i,row[metric],width=.72,color=MODEL_COLOURS[model],edgecolor='#333333',linewidth=.3,hatch=HATCHES[i],zorder=3)
            vertical_interval(ax,i,row[f'{metric}_low'],row[f'{metric}_high'],.15)
            ax.text(i,row[f'{metric}_high']+limit*.02,f'{row[metric]:.3f}',ha='center',va='bottom',rotation=90,fontsize=7,color='black')
        ax.set_xticks(range(7),MODEL_SHORT,rotation=90);ax.set_ylabel(ylabel);ax.set_ylim(0,limit)
        ax.yaxis.set_major_locator(MaxNLocator(5))
        if metric=='auc':ax.axhline(.5,color='#666666',linewidth=.7,linestyle='--')
    fig.legend([Patch(facecolor=MODEL_COLOURS[m],edgecolor='#333333',linewidth=.3,hatch=HATCHES[i]) for i,m in enumerate(MODEL_ORDER)],MODEL_NAMES,
               loc='upper center',bbox_to_anchor=(.52,.997),ncol=4,frameon=False,fontsize=7,handlelength=1.5,columnspacing=1.2)
    fig.subplots_adjust(left=.09,right=.985,top=.885,bottom=.085)
    caption=('Performance metrics for the seven full-feature algorithms, retaining the grouped-bar presentation of the original manuscript. '
             'Panel a shows sensitivity, specificity, precision (positive predictive value) and accuracy at a fixed threshold of 0.5; '
             'panels b–d show AUC, Brier score and log loss. All bars start at zero and use survey-weighted held-out predictions. '
             'Whiskers are 95% intervals from the same 1,000 stratified rescaled PSU-bootstrap replicates, conditional on trained models and validation folds. '
             'All replicates had valid denominators for the displayed metrics. The dashed AUC line denotes chance discrimination. '
             'Higher AUC and lower Brier score/log loss indicate better performance. KNN fitting is unweighted, with weighted tuning and evaluation. '
             'DT, decision tree; LR, logistic regression; RF, random forest; SVM, support vector machine; HGB, histogram gradient boosting. '
             'Nested cross-validation and the conditional bootstrap serve different purposes and are not presented as independent validation experiments.')
    return _save(fig,'correa_model_performance',destination,metadata,caption,['model_performance.csv'],preserve_size=True)


def importance(table,destination,metadata):
    """Display every raw predictor in both models, without rank-based selection."""
    from .data import SOCIO_FEATURES, DIET_FEATURES

    summary=_importance_summary(table)
    models=['random_forest','spline_logistic']
    groups=[('Demographic and dental',list(SOCIO_FEATURES)),
            ('Food intake and cooking fat',DIET_FEATURES[:9]+['oil']),
            ('Beverages and label behaviours',['water','soda_frequency','juice_frequency']+
             [f for f in DIET_FEATURES if f.startswith('label_')])]
    expected=[f for _, features in groups for f in features]
    if len(expected)!=27 or len(set(expected))!=27:
        raise ValueError('The display must partition all 27 predictors exactly once')
    for model in models:
        actual=summary.loc[summary.model.eq(model),'feature'].tolist()
        if len(actual)!=27 or set(actual)!=set(expected):
            raise ValueError('All 27 predictors are required for each displayed model')
    selected=summary.loc[summary.model.isin(models)]
    limits=_common_limits(selected[['mean','low','high']])
    fig,axes=plt.subplots(3,2,figsize=(180/25.4,215/25.4),
                          gridspec_kw={'height_ratios':[8,10,9]})
    displayed={m:[] for m in models}
    for row,(group,features) in enumerate(groups):
        # Shared order preserves direct comparison; it is fixed by the declared
        # conceptual groups, never by statistical significance or rank.
        ys=np.arange(len(features))
        for col,model in enumerate(models):
            values=summary.loc[summary.model.eq(model)].set_index('feature').loc[features]
            ax=axes[row,col]
            panel(ax,f'{chr(97+row*2+col)}  '+('Random forest' if col==0 else 'Spline logistic'),axis='x')
            ax.barh(ys,values['mean'],height=.52,color=BLUE,edgecolor='#333333',linewidth=.25,zorder=3)
            for y,item in zip(ys,values.itertuples(),strict=True):horizontal_interval(ax,y,item.low,item.high)
            ax.set_yticks(ys,[FEATURE_LABELS[f] for f in features]);ax.invert_yaxis()
            ax.set_xlim(limits);ax.axvline(0,color='#555555',linewidth=.75)
            ax.set_xlabel('Increase in Brier score');ax.xaxis.set_major_locator(MaxNLocator(3))
            displayed[model].extend(features)
    fig.subplots_adjust(left=.245,right=.985,bottom=.055,top=.96,wspace=1.42,hspace=.46)
    caption=('All 27 predictors in random forest and spline logistic regression, retaining the original blue horizontal-bar style. '
             'Panels a–b contain eight demographic/dental predictors; c–d contain nine food-intake variables and cooking fat; '
             'e–f contain water, sweetened-drink frequencies and six label behaviours. Within each row, both models have the same fixed '
             'conceptual order; no variable is selected or omitted on its importance. All six panels use the same numerical axis and '
             'zero bar baseline, preserving null and negative results. Bars show mean held-out Brier-score increases after raw-variable '
             'permutation. Five repeats are averaged within each fold, then weighted by held-out expansion-weight totals. '
             'Whiskers span minimum to maximum fold means, not confidence intervals. Water is shown in the beverage panels regardless '
             'of rank. Complete numerical ranks and stability remain in the supplementary tables and Figure S7. Scores are unscaled '
             'Brier changes, not the original rescaled importance score; importance is neither causal nor directional.')
    paths=_save(fig,'correa_predictor_importance',destination,metadata,caption,['permutation_importance.csv'],preserve_size=True,
                alt_text='Six aligned blue bar plots display all 27 predictors in both models, including water, with fold ranges and a common scale.')
    metadata['figures'][-1].update(displayed_predictors=displayed,panel_count=6,
                                 display_groups={name:features for name,features in groups})
    return paths


def groups_and_comparisons(groups,pairs,destination,metadata):
    models=['random_forest','spline_logistic'];colours=[BLUE,'#A0A0A0']
    order=['Sociodemographic','Remaining teeth','Food intake','Label-related behaviours','Beverages','Cooking fat']
    comparison_order=['Spline versus linear logistic','Random forest versus linear logistic',
        'Adding dietary variables to spline logistic','Adding dietary variables to random forest',
        'Adding remaining teeth','Adding water context and dental attendance',
        'Adding water to spline logistic','Adding water to random forest']
    short=['Spline vs linear logistic','RF vs linear logistic','Adding diet to spline','Adding diet to RF',
           'Adding remaining teeth','Adding context and attendance','Adding water to spline','Adding water to RF']
    fig,axes=plt.subplots(2,2,figsize=(180/25.4,210/25.4),gridspec_kw={'height_ratios':[1,1.15]})
    for col,metric in enumerate(['brier_increase','auc_drop']):
        ax=axes[0,col];panel(ax,('a  Joint Brier contribution' if col==0 else 'b  Joint AUC contribution'),axis='x')
        low='minimum_fold_'+metric;high='maximum_fold_'+metric
        for i,model in enumerate(models):
            rows=groups.loc[groups.model.eq(model)].set_index('block').loc[order]
            ys=np.arange(6)+(i-.5)*.3
            ax.barh(ys,rows[metric],height=.28,color=colours[i],hatch='' if i==0 else '///',edgecolor='#333333',linewidth=.25,zorder=3)
            for y,row in zip(ys,rows.itertuples(),strict=True):horizontal_interval(ax,y,getattr(row,low),getattr(row,high))
        ax.set_yticks(range(6),['Sociodemographic (7)','Remaining teeth (1)','Food intake (9)','Label behaviours (6)','Beverages (3)','Cooking fat (1)'])
        ax.invert_yaxis();ax.set_xlim(_common_limits(groups[[metric,low,high]]));ax.axvline(0,color='#555555',linewidth=.7)
        ax.set_xlabel('Increase in Brier score' if col==0 else 'Decrease in AUC');ax.xaxis.set_major_locator(MaxNLocator(3))
    for col,metric in enumerate(['auc','brier']):
        ax=axes[1,col];panel(ax,('c  Refitted AUC difference' if col==0 else 'd  Refitted Brier difference'),axis='x')
        rows=pairs.loc[pairs.metric.eq(metric)].set_index('comparison').loc[comparison_order]
        ax.barh(range(8),rows.difference,color=BLUE,edgecolor='#333333',linewidth=.25,height=.54,zorder=3)
        for y,row in enumerate(rows.itertuples()):horizontal_interval(ax,y,row.ci_low,row.ci_high)
        ax.set_yticks(range(8),short);ax.invert_yaxis();ax.set_xlim(_common_limits(rows[['difference','ci_low','ci_high']]))
        ax.axvline(0,color='#555555',linewidth=.7);ax.xaxis.set_major_locator(MaxNLocator(3))
        ax.set_xlabel('Difference in AUC' if col==0 else 'Difference in Brier score')
    fig.legend([Patch(facecolor=colours[0],edgecolor='#333333'),Patch(facecolor=colours[1],edgecolor='#333333',hatch='///')],
               ['Random forest','Spline logistic'],loc='upper center',bbox_to_anchor=(.59,.995),ncol=2,fontsize=7,frameon=False)
    fig.subplots_adjust(left=.275,right=.985,top=.915,bottom=.065,wspace=1.6,hspace=.26)
    caption=('Predictor-family contributions and added value after refitting. Panels a and b show joint held-out permutation of six exhaustive groups, '
             'with numbers of predictors in parentheses. Groups were specified after inspection of the initial results. Whiskers span fold-mean ranges, not confidence intervals. '
             'Within-group relationships are preserved through joint row permutation. Panels c and d show paired differences after separately redeveloping the specified model variants, '
             'with 95% conditional intervals from 1,000 shared stratified PSU-bootstrap replicates. Positive AUC differences and negative Brier-score differences favour the first or expanded model. '
             'The contextual extension adds household water supply, recent rural/well/spring water use and dental-attendance recency together. '
             'Permutation contributions and refitted comparisons answer different questions and are not interchangeable or causal. Group sizes differ; group contributions are not additive shares of disease explained. '
             'All comparisons are exploratory without multiplicity adjustment.')
    return _save(fig,'correa_predictor_groups',destination,metadata,caption,['grouped_importance_stability.csv','paired_comparisons.csv'],preserve_size=True)


def make_correa_figures(source:Path,destination:Path,metadata:dict):
    paths=[]
    names=['model_performance.csv','permutation_importance.csv','grouped_importance_stability.csv','paired_comparisons.csv']
    data={name:pd.read_csv(source/name) for name in names}
    with plt.rc_context({'text.color':'black','axes.labelcolor':'black','xtick.color':'black','ytick.color':'black','hatch.linewidth':.3}):
        paths+=performance(data['model_performance.csv'],destination,metadata)
        paths+=importance(data['permutation_importance.csv'],destination,metadata)
        paths+=groups_and_comparisons(data['grouped_importance_stability.csv'],data['paired_comparisons.csv'],destination,metadata)
    metadata['source_table_sha256'].update({name:hashlib.sha256((source/name).read_bytes()).hexdigest() for name in names})
    metadata['correa_presentation']={'reference':'Figures embedded in the user-supplied Correa manuscript',
        'style_preserved':['Grouped vertical model-performance bars','Blue horizontal predictor-importance bars','White background and light grey grids'],
        'methodological_changes':['Shared conditional PSU-bootstrap intervals','Explicit metric definitions','Held-out permutation','Visible fold stability'],
        'selection':'All 27 predictors shown in both models without importance-based selection; six panels preserve conceptual groups',
        'model_colours':MODEL_COLOURS}
    return paths
