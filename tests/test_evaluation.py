import numpy as np
import pandas as pd
from ens_analysis.evaluation import probability_metrics, replicate_multipliers


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
