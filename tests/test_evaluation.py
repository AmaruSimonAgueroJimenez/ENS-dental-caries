import numpy as np
import pandas as pd
from ens_analysis.evaluation import (
    BOOTSTRAP_METRICS, evaluate_oof, paired_differences,
    probability_metrics, replicate_multipliers,
)


def test_confusion_denominators_and_weighted_brier():
    m=probability_metrics([1,1,0,0],[.9,.4,.6,.1],[1,2,3,4])
    assert np.isclose(m['sensitivity'],1/3)
    assert np.isclose(m['specificity'],4/7)
    assert np.isclose(m['ppv'],1/4)
    assert np.isclose(m['accuracy'],.5)
    assert np.isclose(m['brier'],(.01+2*.36+3*.36+4*.01)/10)


def test_bootstrap_preserves_whole_clusters_and_singleton():
    frame=pd.DataFrame({'stratum':['a','a','a','b'], 'psu':['1','1','2','3']})
    draws=replicate_multipliers(frame,frame,100,3)
    assert np.array_equal(draws[:,0],draws[:,1])
    assert np.all(draws[:,3]==1)
    assert np.all(draws[:,0]+draws[:,2]==2)


def test_calibration_slope_not_fabricated_for_constant_predictions():
    m=probability_metrics([0,1,0,1],[.5,.5,.5,.5])
    assert np.isnan(m['calibration_slope'])
    assert np.isclose(m['calibration_intercept'],0)
    assert 'not identifiable' in m['calibration_status']


def test_single_class_calibration_unavailable():
    m=probability_metrics([1,1,1],[.2,.4,.9])
    assert np.isnan(m['calibration_slope'])
    assert np.isnan(m['calibration_intercept'])


def _clustered_oof():
    # Both outcomes occur in each PSU, but errors and unequal survey weights
    # differ between PSUs, so the cluster bootstrap has nontrivial uncertainty.
    frame = pd.DataFrame({
        'row_index':np.arange(8), 'participant_id':[str(i) for i in range(8)],
        'psu':np.repeat(['1','2','3','4'],2),
        'stratum':np.repeat(['a','b'],4), 'y':[0,1]*4,
        'weight':np.arange(1,9,dtype=float), 'fold':[0,0,1,1,0,0,1,1],
    })
    first=frame.assign(model='first',p=[.1,.5,.7,.9,.6,.3,.2,.8])
    second=frame.assign(model='second',p=[.6,.4,.2,.7,.3,.8,.7,.5])
    return pd.concat([first,second],ignore_index=True), frame


def test_shared_psu_classification_intervals_match_weighted_confusion_counts():
    oof, design = _clustered_oof()
    metrics, bootstrap, wide = evaluate_oof(oof,design,replicates=40,seed=17)
    draws = replicate_multipliers(design,wide,replicates=40,seed=17)
    y, w = wide.y.to_numpy(), wide.weight.to_numpy()
    for name in ['first','second']:
        pred = wide[name].to_numpy() >= .5  # Values exactly .5 are positive.
        expected=[]
        for draw in draws:
            weight = w*draw
            tp = weight[(y==1)&pred].sum()
            tn = weight[(y==0)&~pred].sum()
            fp = weight[(y==0)&pred].sum()
            fn = weight[(y==1)&~pred].sum()
            expected.append([tp/(tp+fn),tn/(tn+fp),tp/(tp+fp),(tp+tn)/weight.sum()])
        expected=np.asarray(expected)
        np.testing.assert_allclose(bootstrap[name][:,3:],expected)
        point=metrics.loc[metrics.model.eq(name)&metrics.weighting.eq('Survey weighted')].iloc[0]
        for j,metric in enumerate(['sensitivity','specificity','ppv','accuracy']):
            np.testing.assert_allclose([point[metric+'_low'],point[metric+'_high']],
                                       np.quantile(expected[:,j],[.025,.975]))
            assert point[metric+'_bootstrap_valid']==40
        assert point.classification_threshold==.5
        assert point.bootstrap_replicates==40
    assert BOOTSTRAP_METRICS[:3]==('auc','brier','log_loss')
    paired=paired_differences(bootstrap,metrics,[('first','second','Comparison')])
    assert paired.metric.tolist()==['auc','brier','log_loss']
    assert metrics.loc[metrics.weighting.eq('Unweighted'),'sensitivity_low'].isna().all()


def test_undefined_ppv_is_not_replaced_with_fabricated_intervals():
    oof, design = _clustered_oof()
    oof=oof.loc[oof.model.eq('first')].assign(p=.1)
    metrics, bootstrap, _ = evaluate_oof(oof,design,replicates=20,seed=3)
    point=metrics.loc[metrics.weighting.eq('Survey weighted')].iloc[0]
    assert np.isnan(point.ppv)
    assert np.isnan(point.ppv_low) and np.isnan(point.ppv_high)
    assert point.ppv_bootstrap_valid==0
    assert np.isnan(bootstrap['first'][:,BOOTSTRAP_METRICS.index('ppv')]).all()
    assert point.specificity_low==1 and point.specificity_high==1
    assert point.sensitivity_low==0 and point.sensitivity_high==0
