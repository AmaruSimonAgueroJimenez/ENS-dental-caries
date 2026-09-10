#!/usr/bin/env python3
"""Run or resume the manuscript-informed ENS caries reanalysis."""
from __future__ import annotations

import argparse
from datetime import datetime, timezone
from hashlib import sha256
from importlib.metadata import version
import json
from pathlib import Path
import pickle
import platform
import sys
import warnings

import numpy as np
import pandas as pd
from sklearn.metrics import roc_curve

ROOT=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT/'src'))
from ens_analysis.data import load_caries_data, SOCIO_FEATURES, DIET_FEATURES, EXTENDED_FEATURES
from ens_analysis.inference import descriptive_tables, run_associations
from ens_analysis.reconciliation import reconcile_original_data
from ens_analysis.evaluation import probability_metrics, evaluate_oof, paired_differences, calibration_bins


def write_json(path,value):
    def convert(obj):
        if isinstance(obj,np.generic): return obj.item()
        if isinstance(obj,np.ndarray): return obj.tolist()
        if isinstance(obj,Path): return str(obj)
        raise TypeError(type(obj).__name__)
    path.write_text(json.dumps(value,indent=2,default=convert,ensure_ascii=False),encoding='utf-8')


def source_hashes(root):
    """Hash analysis/orchestration source without including private data paths."""
    paths = sorted(path for directory in ['scripts', 'src']
                   for path in (root/directory).rglob('*.py'))
    return {path.relative_to(root).as_posix(): sha256(path.read_bytes()).hexdigest()
            for path in paths}


def prepare(frame,dictionary,audit,tables,legacy_frequencies=None):
    audit=dict(audit);audit['source_file']='data/data.sav'
    write_json(tables/'data_audit.json',audit)
    dictionary.to_csv(tables/'variable_dictionary.csv',index=False)
    pd.DataFrame([{'stage':k,'n':v} for k,v in audit['sample_flow'].items()]).to_csv(tables/'sample_flow.csv',index=False)
    missing=[]
    for name in SOCIO_FEATURES+DIET_FEATURES+EXTENDED_FEATURES:
        a=frame.loc[frame.eligible,name]
        missing.append({'variable':name,'n':len(a),'missing':int(a.isna().sum()),'percent_missing':100*a.isna().mean()})
    pd.DataFrame(missing).to_csv(tables/'predictor_missingness.csv',index=False)
    with warnings.catch_warnings():
        warnings.simplefilter('ignore',UserWarning)
        prevalence,numeric=descriptive_tables(frame)
    prevalence.to_csv(tables/'descriptive_prevalence.csv',index=False)
    numeric.to_csv(tables/'descriptive_numeric.csv',index=False)
    print('Fitting survey-adjusted association and sensitivity models',flush=True)
    association=run_associations(frame,legacy_frequencies=legacy_frequencies)
    for key,value in association.items():
        if isinstance(value,pd.DataFrame):
            value.to_csv(tables/(key+'.csv'),index=False)
    write_json(tables/'association_metadata.json',{k:v for k,v in association.items() if not isinstance(v,pd.DataFrame)})
    return association


def summarize(frame,results,tables,bootstrap):
    # Prevent a report from combining a changed input, feature derivation, model
    # implementation or search specification with obsolete cached predictions.
    from ens_analysis.models import _fingerprint
    settings=results['manifest']['settings']
    analytical=frame.loc[frame.eligible]
    for name,record in results['manifest']['models'].items():
        spec={k:v for k,v in record.items() if k!='fingerprint'}
        expected=_fingerprint(analytical,spec['features'],{**settings,'model':name,'spec':spec})
        if expected!=record['fingerprint']:
            raise ValueError('Cached predictions are stale; rerun --stage models before summarizing')
    oof=results['oof']
    base=oof.loc[oof.model==oof.model.iloc[0]].copy()
    base['model']='prevalence_only'
    for fold in base.fold.unique():
        train=base.fold.ne(fold)
        base.loc[~train,'p']=np.average(base.loc[train,'y'],weights=base.loc[train,'weight'])
    oof=pd.concat([oof,base],ignore_index=True)
    design=frame.loc[frame.weight.notna() & frame.weight.gt(0)].copy()
    print(f'Evaluating held-out probabilities with {bootstrap} shared PSU bootstrap replicates',flush=True)
    metrics,replicates,wide=evaluate_oof(oof,design,bootstrap)
    metrics.loc[metrics.model.eq('prevalence_only'),'calibration_slope']=np.nan
    metrics.loc[metrics.model.eq('prevalence_only'),'joint_calibration_intercept']=np.nan
    metrics.loc[metrics.model.eq('prevalence_only'),'calibration_status']='Within-fold constant benchmark; pooled slope not interpretable'
    metrics.to_csv(tables/'model_performance.csv',index=False)
    pairs=[
        ('random_forest','logistic','Random forest versus linear logistic'),
        ('spline_logistic','logistic','Spline versus linear logistic'),
        ('random_forest','rf_no_water','Adding water to random forest'),
        ('spline_logistic','spline_no_water','Adding water to spline logistic'),
        ('random_forest','rf_base','Adding dietary variables to random forest'),
        ('spline_logistic','spline_base','Adding dietary variables to spline logistic'),
        ('spline_extended','spline_logistic','Adding water context and dental attendance'),
        ('spline_logistic','spline_no_teeth','Adding remaining teeth'),
    ]
    paired_differences(replicates,metrics,pairs).to_csv(tables/'paired_comparisons.csv',index=False)
    for source,name in [('tuning','hyperparameter_search'),('foldscores','outer_fold_performance'),
                        ('importance','permutation_importance'),('split_qc','validation_split_checks')]:
        results[source].to_csv(tables/(name+'.csv'),index=False)
    write_json(tables/'model_manifest.json',results['manifest'])
    important=results['importance']
    if len(important):
        folds=important.groupby(['model','fold','feature'],as_index=False).agg(
            brier_increase=('brier_increase','mean'),auc_drop=('auc_drop','mean'),
            test_weight_sum=('test_weight_sum','first'))
        folds['rank']=folds.groupby(['model','fold']).brier_increase.rank(ascending=False,method='min')
        rows=[]
        for (model,feature),block in folds.groupby(['model','feature']):
            rows.append({'model':model,'feature':feature,
                         'brier_increase':np.average(block.brier_increase,weights=block.test_weight_sum),
                         'auc_drop':np.average(block.auc_drop,weights=block.test_weight_sum),
                         'median_rank':block['rank'].median(),'best_rank':block['rank'].min(),
                         'worst_rank':block['rank'].max(),'fraction_top3':block['rank'].le(3).mean(),
                         'minimum_fold_brier_increase':block.brier_increase.min(),
                         'maximum_fold_brier_increase':block.brier_increase.max()})
        pd.DataFrame(rows).to_csv(tables/'importance_stability.csv',index=False)
    covariates=frame.reset_index(names='row_index')[['row_index','sex','age','education','rural','paper_complete']]
    wide=wide.merge(covariates,on='row_index',validate='one_to_one')
    wide['age_group']=pd.cut(wide.age,[14,24,44,64,np.inf],labels=['15–24','25–44','45–64','65+'])
    subgroup=[]
    for name in ['logistic','spline_logistic','random_forest']:
        if name not in wide: continue
        for variable in ['sex','age_group','education','rural']:
            for level,block in wide.groupby(variable,observed=True):
                subgroup.append({'model':name,'variable':variable,'level':str(level),
                                 **probability_metrics(block.y,block[name],block.weight)})
    pd.DataFrame(subgroup).to_csv(tables/'subgroup_performance.csv',index=False)
    sensitivity=[]
    for name in results['manifest']['models']:
        for label,mask in [('Exact manuscript subset',wide.paper_complete),('Age at least 18',wide.age.ge(18))]:
            block=wide.loc[mask]
            sensitivity.append({'model':name,'subset':label,**probability_metrics(block.y,block[name],block.weight)})
    pd.DataFrame(sensitivity).to_csv(tables/'prediction_subset_sensitivity.csv',index=False)
    calibration=[];rocs=[]
    for name in results['manifest']['models']:
        b=calibration_bins(wide.y,wide[name],wide.weight);b['model']=name;calibration.append(b)
        fpr,tpr,_=roc_curve(wide.y,wide[name],sample_weight=wide.weight)
        rocs.append(pd.DataFrame({'model':name,'fpr':fpr,'tpr':tpr}))
    pd.concat(calibration,ignore_index=True).to_csv(tables/'calibration.csv',index=False)
    pd.concat(rocs,ignore_index=True).to_csv(tables/'roc.csv',index=False)
    return metrics


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--stage',choices=['all','prepare','models','summarize'],default='all')
    parser.add_argument('--data',type=Path,default=ROOT/'data/data.sav')
    parser.add_argument('--original-data',type=Path,
                        default=ROOT/'data/original/23.07.26_base_ens_5520_R_enviar.dta')
    parser.add_argument('--bootstrap',type=int,default=1000)
    parser.add_argument('--jobs',type=int,default=3)
    args=parser.parse_args()
    tables=ROOT/'outputs/tables';private=ROOT/'outputs/private'
    tables.mkdir(parents=True,exist_ok=True);private.mkdir(parents=True,exist_ok=True)
    # Invalidate an earlier completed report before even reading a new input.
    # A failed prepare/load must never leave that earlier completion in place.
    started_utc=datetime.now(timezone.utc).isoformat()
    write_json(tables/'run_manifest.json',{
        'status':'in_progress','stage':args.stage,'started_utc':started_utc,
        'reason':'Analysis stage started; earlier aggregate completion invalidated',
    })
    code_sha256=source_hashes(ROOT)
    frame,dictionary,audit=load_caries_data(args.data)
    reconciliation,comparison,water_coding,legacy_frequencies=reconcile_original_data(
        args.data,args.original_data,frame)
    original_sha256=sha256(args.original_data.read_bytes()).hexdigest()
    write_json(tables/'source_reconciliation.json',reconciliation)
    comparison.to_csv(tables/'source_comparison.csv',index=False)
    water_coding.to_csv(tables/'legacy_water_coding.csv',index=False)
    if args.stage in ['all','prepare']:
        prepare(frame,dictionary,audit,tables,legacy_frequencies)
    if args.stage in ['all','models']:
        from ens_analysis.models import run_nested_models
        results=run_nested_models(frame.loc[frame.eligible],private/'nested',n_jobs=args.jobs)
        (private/'model_results.pkl').write_bytes(pickle.dumps(results))
    if args.stage in ['all','summarize']:
        # Cached predictions have their own fingerprint checks. Descriptives
        # and association tables must also belong to the current input/code.
        # The all stage has already prepared them above in this invocation.
        if args.stage=='summarize':
            prepare(frame,dictionary,audit,tables,legacy_frequencies)
        results=pickle.loads((private/'model_results.pkl').read_bytes())
        # Private caches are produced locally by this pipeline; never load an
        # untrusted downloaded pickle into this process.
        metrics=summarize(frame,results,tables,args.bootstrap)
        from ens_analysis.contributions import predictor_contributions
        grouped,grouped_stability,contribution_manifest=predictor_contributions(
            frame.loc[frame.eligible],results,n_jobs=args.jobs)
        grouped.to_csv(tables/'grouped_permutation_importance.csv',index=False)
        grouped_stability.to_csv(tables/'grouped_importance_stability.csv',index=False)
        write_json(tables/'predictor_contributions_manifest.json',contribution_manifest)
        from ens_analysis.figures import make_figures
        make_figures(tables,ROOT/'outputs/figures')
        if source_hashes(ROOT)!=code_sha256:
            raise RuntimeError('Analysis source changed during execution; rerun summarize before rendering')
        if (sha256(args.data.read_bytes()).hexdigest()!=audit['source_sha256']
                or sha256(args.original_data.read_bytes()).hexdigest()!=original_sha256):
            raise RuntimeError('Source data changed during execution; rerun the analysis')
        manifest={
            'status':'complete','stage':args.stage,'started_utc':started_utc,
            'completed_utc':datetime.now(timezone.utc).isoformat(),
            'input_sha256':audit['source_sha256'],'bootstrap_replicates':args.bootstrap,
            'code_sha256':code_sha256,
            'original_input_sha256':original_sha256,
            'source_reconciliation':reconciliation,
            'analysis_origin':'Python reanalysis with original Stata cohort and source variables reconciled against the full ENS release; documented methodological corrections intentionally change the original R analysis',
            'python':platform.python_version(),'analysis_seed':20260910,
            'packages':{p:version(p) for p in ['numpy','pandas','scipy','scikit-learn','statsmodels','pyreadstat','matplotlib','patsy']},
            'n_primary':int(frame.eligible.sum()),'n_manuscript':int(frame.paper_complete.sum()),
            'model_names':list(results['manifest']['models']),
            'predictor_contributions':contribution_manifest,
            'outer_folds':results['manifest']['settings']['outer_folds'],
            'inner_folds':results['manifest']['settings']['inner_folds'],
            'weight_confirmation':'Working phase-based choice; official manual confirmation outstanding',
            'uncertainty':'PSU bootstrap intervals conditional on fixed fitted models and CV splits; singleton fixed',
        }
        write_json(tables/'run_manifest.json',manifest)
        print(metrics.query("weighting == 'Survey weighted'")[['model','auc','brier','log_loss']].to_string(index=False))
        print('Completed aggregate tables and figures. Render docs/caries_python.qmd with Quarto.',flush=True)
    else:
        # Preparing associations or fitting models alone is a successful
        # partial stage, never a completed aggregate report.
        write_json(tables/'run_manifest.json',{
            'status':'prepared' if args.stage=='prepare' else 'models_ready',
            'stage':args.stage,'started_utc':started_utc,
            'completed_utc':datetime.now(timezone.utc).isoformat(),
            'input_sha256':audit['source_sha256'],'code_sha256':code_sha256,
            'reason':'Run summarize to regenerate and validate all aggregate outputs',
        })


if __name__=='__main__':
    main()
