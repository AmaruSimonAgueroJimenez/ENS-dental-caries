"""Independent numerical and completeness checks of all-predictor summaries."""
import numpy as np
import pandas as pd
import pytest

from ens_analysis import all_predictor_results as ar


def raw_importance():
    rows = []
    for fold, weight in [(1, 1.), (2, 3.)]:
        for feature in ['water'] + [f'x{i}' for i in range(11)]:
            mean = (.2 if fold == 1 else -.1) if feature == 'water' else .01
            for repeat, offset in [(1, -.02), (2, .02)]:
                rows.append(dict(model='m', fold=fold, feature=feature, repeat=repeat,
                                 brier_increase=mean + offset, auc_drop=2*(mean + offset),
                                 test_weight_sum=weight))
    return pd.DataFrame(rows)


def test_importance_signed_fold_weighting_ranges_and_stability():
    summary = ar.importance_summary(raw_importance()).set_index('feature')
    row = summary.loc['water']
    assert row.brier_increase == pytest.approx(-.025)
    assert row.auc_drop == pytest.approx(-.05)
    assert row.minimum_fold_brier_increase == pytest.approx(-.1)
    assert row.maximum_fold_brier_increase == pytest.approx(.2)
    assert row.minimum_fold_auc_drop == pytest.approx(-.2)
    assert row.maximum_fold_auc_drop == pytest.approx(.4)
    assert row.best_rank == 1 and row.worst_rank == 12 and row.median_rank == 6.5
    assert row.fraction_top3 == row.fraction_top10 == row.fraction_positive_folds == .5
    assert row.overall_rank == 12
    assert row.folds == 2 and row.repeats_per_fold == 2
    # Ties use the disclosed minimum-rank convention, without arbitrary jitter.
    assert summary.loc['x0', 'overall_rank'] == summary.loc['x1', 'overall_rank'] == 1


@pytest.mark.parametrize('problem', ['duplicate', 'weight', 'negative_weight', 'missing_cell', 'nan_key', 'nan_value', 'empty'])
def test_importance_rejects_incomplete_or_inconsistent_inputs(problem):
    raw = raw_importance()
    if problem == 'duplicate': raw = pd.concat([raw, raw.iloc[[0]]], ignore_index=True)
    elif problem == 'weight': raw.loc[0, 'test_weight_sum'] = 9
    elif problem == 'negative_weight': raw.loc[0, 'test_weight_sum'] = -1
    elif problem == 'missing_cell': raw = raw.drop(index=0)
    elif problem == 'nan_key': raw.loc[0, 'feature'] = np.nan
    elif problem == 'nan_value': raw.loc[0, 'auc_drop'] = np.nan
    elif problem == 'empty': raw = raw.iloc[:0]
    with pytest.raises(ValueError): ar.importance_summary(raw)


def agreement_input():
    rows = []
    for i, family in enumerate(ar.FAMILIES):
        for feature in ['water', 'age']:
            rows.append(dict(model=f'{family}__symptoms_diet_labels', feature=feature,
                             brier_increase=[-.01, .02, 0, .04][i] if feature == 'water' else .03,
                             fraction_positive_folds=[0, .8, .4, 1][i] if feature == 'water' else 1,
                             overall_rank=[44, 8, 20, 5][i] if feature == 'water' else 2))
    # Lower-information models are intentionally excluded from full-model agreement.
    rows.append(dict(model='mlp__context_diet_labels', feature='water', brier_increase=100,
                     fraction_positive_folds=1, overall_rank=1))
    return pd.DataFrame(rows)


def test_agreement_retains_weak_variables_and_all_four_models():
    before = agreement_input()
    out = ar.agreement_table(before).set_index('feature')
    assert out.index.tolist() == ['age', 'water']
    water = out.loc['water']
    assert water.models == 4 and water.models_with_positive_mean == 2
    assert water.models_positive_in_at_least_four_folds == 2 and water.models_in_top10 == 2
    assert water.median_overall_rank == 14 and water.best_overall_rank == 5 and water.worst_overall_rank == 44
    assert water.classical_models == 3 and water.classical_models_with_positive_mean == 1
    assert water.classical_models_positive_in_at_least_four_folds == 1 and water.classical_models_in_top10 == 1
    assert water.classical_median_overall_rank == 20
    assert water.classical_best_overall_rank == 8 and water.classical_worst_overall_rank == 44
    pd.testing.assert_frame_equal(before, agreement_input())


@pytest.mark.parametrize('problem', ['duplicate', 'missing', 'unknown', 'empty'])
def test_agreement_checks_identity_and_completeness(problem):
    table = agreement_input()
    if problem == 'duplicate': table = pd.concat([table, table.iloc[[0]]], ignore_index=True)
    elif problem == 'missing': table = table.drop(index=0)
    elif problem == 'unknown': table['model'] = table.model.str.replace('random_forest', 'unknown', regex=False)
    elif problem == 'empty': table = table.iloc[:0]
    with pytest.raises(ValueError): ar.agreement_table(table)


@pytest.fixture(scope='module')
def evaluated():
    n = 48
    frame = pd.DataFrame(dict(participant_id=[f'p{i}' for i in range(n)],
                              psu=np.repeat([f'g{i}' for i in range(12)], 4),
                              stratum=np.repeat([f's{i // 3}' for i in range(12)], 4),
                              caries=np.tile([0, 1, 1, 0], 12), weight=1+np.arange(n)%7/3,
                              eligible=np.arange(n)<40, age=15+np.arange(n),
                              sex=np.tile(['Male', 'Female', 'Male', 'Female'], 12),
                              rural=np.tile(['Urban', 'Urban', 'Rural', 'Rural'], 12),
                              education=np.tile(['Low', 'High', 'Low', 'High'], 12)),
                         index=np.arange(1000, 1000+n))
    cohort=frame.loc[frame.eligible]
    oof, raw = [], []
    for i, model in enumerate(ar.MODELS):
        part=cohort[['participant_id','psu','stratum','caries','weight']].rename(columns={'caries':'y'}).copy()
        part.insert(0, 'row_index', part.index)
        part['model']=model
        part['fold']=np.repeat(np.arange(10)%5+1,4)
        amount=.1 + .012*i
        part['p']=.5 + amount*(2*part.y-1) + .24*np.sin(np.arange(len(part))*2.1)
        oof.append(part.reset_index(drop=True))
        for fold in range(1,6):
            for feature in ['age', 'water']:
                for repeat in range(5):
                    raw.append(dict(model=model,fold=fold,feature=feature,repeat=repeat,
                                    brier_increase=.01*fold if feature=='age' else -.001*fold,
                                    auc_drop=.02*fold if feature=='age' else -.002*fold,
                                    test_weight_sum=float(part.loc[part.fold.eq(fold),'weight'].sum())))
    raw=pd.DataFrame(raw)
    groups=raw.rename(columns={'feature':'group'}).replace({'group':{'water':'intake','age':'context'}})
    results={'oof':pd.concat(oof,ignore_index=True),'importance':raw,'grouped_importance':groups}
    return frame,results,ar.evaluate_all_predictors(frame,results,replicates=19)


def direct_metrics(y,p,w):
    y,p,w=np.asarray(y),np.asarray(p),np.asarray(w)
    pos,neg=y==1,y==0
    pairweights=w[pos,None]*w[None,neg]
    order=p[pos,None]-p[None,neg]
    return dict(auc=np.sum(pairweights*((order>0)+.5*(order==0)))/pairweights.sum(),
                brier=np.sum(w*(y-p)**2)/w.sum(),
                log_loss=-np.sum(w*(y*np.log(p)+(1-y)*np.log1p(-p)))/w.sum())


def test_all_ten_pairs_have_correct_gain_and_percentile_interval(evaluated):
    _,results,ev=evaluated
    assert len(ev['paired_contrasts']) == 30
    assert ev['paired_contrasts'].contrast_id.nunique()==10
    assert ev['paired_contrasts'].role.eq('exploratory_conditional_comparison').all()
    metrics={name:direct_metrics(b.y,b.p,b.weight) for name,b in results['oof'].groupby('model')}
    for row in ev['paired_contrasts'].itertuples():
        sign=1 if row.metric=='auc' else -1
        assert row.difference==pytest.approx(sign*(metrics[row.model][row.metric]-metrics[row.reference][row.metric]),abs=1e-14)
        col={'auc':0,'brier':1,'log_loss':2}[row.metric]
        draws=sign*(ev['_bootstrap'][row.model][:,col]-ev['_bootstrap'][row.reference][:,col])
        np.testing.assert_allclose([row.ci_low,row.ci_high],np.quantile(draws,[.025,.975]),rtol=0,atol=1e-14)
    assert set(ev['grouped_importance_stability'].block)=={'context','diet'}
    assert set(results['grouped_importance'].group)=={'context','intake'}
    assert len(ev['cross_model_agreement'])==2


def test_prevalence_training_only_and_identical_outer_participants(evaluated):
    _,results,ev=evaluated
    wide=ev['_wide']
    assert len(wide)==40 and set(ev['model_performance'].model)==set(ar.MODELS)|{'prevalence_only'}
    for fold,part in wide.groupby('fold'):
        train=wide.loc[wide.fold.ne(fold)]
        np.testing.assert_allclose(part.prevalence_only,np.average(train.y,weights=train.weight))
    baseline=ev['model_performance'].query("model == 'prevalence_only'")
    assert baseline.calibration_slope.isna().all()
    assert baseline.joint_calibration_intercept.isna().all()
    assert ev['subgroup_performance'].model.nunique()==8


@pytest.mark.parametrize('problem', ['missing_model', 'duplicate_person', 'different_fold', 'missing_person'])
def test_oof_rejects_unpaired_models(evaluated,problem):
    frame,results,_=evaluated
    modified={**results,'oof':results['oof'].copy()}
    d=modified['oof']
    if problem=='missing_model': modified['oof']=d.loc[d.model.ne(ar.MODELS[0])]
    elif problem=='duplicate_person': modified['oof']=pd.concat([d,d.iloc[[0]]],ignore_index=True)
    elif problem=='different_fold': modified['oof'].loc[0,'fold']=99
    elif problem=='missing_person': modified['oof']=d.drop(index=0)
    with pytest.raises(ValueError): ar.evaluate_all_predictors(frame,modified,replicates=2)
