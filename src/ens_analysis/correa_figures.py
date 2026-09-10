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
    summary=_importance_summary(table)
    fold=table.groupby(['model','fold','feature'],sort=False).brier_increase.mean().reset_index()
    fold['rank']=fold.groupby(['model','fold']).brier_increase.rank(ascending=False,method='min')
    models=['random_forest','spline_logistic']
    chosen={m:summary.loc[summary.model.eq(m)].sort_values(['mean','feature'],ascending=[False,True]).head(10) for m in models}
    selected=pd.concat(chosen.values());limits=_common_limits(selected[['mean','low','high']])
    fig,axes=plt.subplots(2,2,figsize=(180/25.4,215/25.4),gridspec_kw={'height_ratios':[1,1]})
    for col,model in enumerate(models):
        rows=chosen[model];features=rows.feature.tolist();names=[FEATURE_LABELS[f] for f in features];ys=np.arange(10)
        ax=axes[0,col];panel(ax,f'{chr(97+col)}  '+('Random forest' if col==0 else 'Spline logistic'),axis='x')
        ax.barh(ys,rows['mean'],height=.55,color=BLUE,edgecolor='#333333',linewidth=.25,zorder=3)
        for y,row in zip(ys,rows.itertuples(),strict=True):horizontal_interval(ax,y,row.low,row.high)
        ax.set_yticks(ys,names);ax.invert_yaxis();ax.set_xlim(limits);ax.axvline(0,color='#555555',linewidth=.75)
        ax.set_xlabel('Increase in Brier score');ax.xaxis.set_major_locator(MaxNLocator(3))
        ax=axes[1,col]
        matrix=fold.loc[fold.model.eq(model)].pivot(index='feature',columns='fold',values='rank').loc[features].sort_index(axis=1)
        im=ax.imshow(matrix.to_numpy(),cmap='Blues_r',vmin=1,vmax=27,aspect='auto',interpolation='nearest')
        panel(ax,f'{chr(99+col)}  '+('Random forest ranks' if col==0 else 'Spline logistic ranks'),axis='x');ax.grid(False)
        ax.set_yticks(ys,names);ax.set_xticks(range(len(matrix.columns)),[str(x) for x in matrix.columns]);ax.set_xlabel('Held-out fold')
        for row in range(10):
            for c in range(len(matrix.columns)):
                value=matrix.iloc[row,c]
                ax.text(c,row,f'{value:g}',ha='center',va='center',fontsize=7,color='white' if value<13 else 'black')
    fig.subplots_adjust(left=.24,right=.985,bottom=.055,top=.95,wspace=1.4,hspace=.28)
    caption=('Leading predictors and their stability, retaining the blue horizontal-bar presentation of the original manuscript. '
             'Panels a and b show the ten predictors with the largest mean held-out Brier-score increases in random forest and spline logistic regression, respectively. '
             'Selection uses each model’s own mean contribution; the two lists need not be identical. Bar lengths begin at zero on a common scale, and negative endpoints remain visible. '
             'Whiskers span minimum to maximum fold-mean contributions, not 95% confidence intervals. Five raw-variable permutations are averaged within each fold, '
             'then fold means are weighted by test-set expansion-weight totals. Panels c and d show ranks among all 27 predictors in each held-out fold for those same displayed predictors; '
             '1 is highest and darker blue indicates a higher rank. Full results for all 27 predictors, including null and negative contributions, are retained in the supplement. '
             'The score is an increase in Brier error, not the original rescaled mean-decrease-in-accuracy score. Importance has no causal or directional interpretation.')
    paths=_save(fig,'correa_predictor_importance',destination,metadata,caption,['permutation_importance.csv'],preserve_size=True)
    metadata['figures'][-1]['displayed_predictors']={m:chosen[m].feature.tolist() for m in models}
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
        'selection':'Top ten per model shown compactly; all 27 remain available in the full figure and tables',
        'model_colours':MODEL_COLOURS}
    return paths
