"""Known-odds spline contrast and real-cohort association integration checks."""

from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from ens_analysis.data import load_caries_data
from ens_analysis.inference import CURVE_CI_METHOD, run_associations, water_spline_contrast
from ens_analysis.survey import survey_glm


def test_spline_same_covariates_contrast_recovers_known_group_odds():
    # Five unique water levels and five spline/intercept parameters saturate
    # the group logits. At 2 glasses p=4/20; at 6 glasses p=12/20: OR=6.
    water = np.repeat([0, 2, 4, 6, 8], 20)
    outcome = np.concatenate([
        np.r_[np.ones(events), np.zeros(20 - events)]
        for events in [2, 4, 8, 12, 16]
    ])
    frame = pd.DataFrame({
        'water': water, 'caries': outcome, 'weight': np.ones(100),
        'stratum': np.ones(100), 'psu': np.arange(100),
    })
    result = survey_glm(
        'caries ~ bs(water, df=4, degree=3)', frame, 'weight', 'stratum', 'psu'
    )
    contrast = water_spline_contrast(result, frame)
    assert contrast['supported']
    assert contrast['estimate'] == pytest.approx(6.0, rel=1e-8)
    assert contrast['log_odds_ratio'] == pytest.approx(np.log(6), rel=1e-8)
    # The SRS Taylor correction is 100/99 times the four-cell log-OR variance.
    known_variance = (1/4 + 1/16 + 1/12 + 1/8) * 100/99
    assert contrast['se_log_odds_ratio'] ** 2 == pytest.approx(known_variance)
    assert contrast['n_water_2'] == 20 and contrast['n_water_6'] == 20
    assert contrast['ci_low'] < 6 < contrast['ci_high']

    unsupported = water_spline_contrast(result, frame, reference=2, comparison=7)
    assert not unsupported['supported']
    assert unsupported['n_at_comparison'] == 0
    assert np.isnan(unsupported['estimate'])


def test_real_association_cohort_rank_and_spline_missing_value_handling():
    path = Path(__file__).resolve().parents[1] / 'data' / 'data.sav'
    if not path.exists():
        pytest.skip('Local ENS input is not included in the repository.')
    frame, _, _ = load_caries_data(path)
    # These missing inputs previously poisoned Patsy's stateful spline knots
    # before NA_action could remove rows. Keep all design rows for variance.
    assert frame.loc[frame.eligible, 'water'].isna().sum() == 4
    assert frame.loc[frame.eligible, 'education'].isna().sum() == 38
    output = run_associations(frame)
    diagnostics = output['diagnostics'].set_index('model')
    assert len(diagnostics) == 7
    assert diagnostics.converged.all()
    assert (diagnostics['rank'] == diagnostics.parameters).all()
    assert diagnostics.loc['Primary linear water', 'n'] == 4994
    assert diagnostics.loc['Water spline', 'n'] == 4994
    assert diagnostics.loc['Water spline', 'df'] == 1045
    assert diagnostics.loc['Water spline', 'rank'] == 39
    contrast = output['water_contrasts'].iloc[0]
    analytical = frame.loc[frame.paper_complete]
    assert contrast['n'] == 4994
    assert contrast['n_water_2'] == int(analytical.water.eq(2).sum())
    assert contrast['n_water_6'] == int(analytical.water.eq(6).sum())
    assert contrast['supported']
    assert np.isfinite(contrast[['estimate', 'ci_low', 'ci_high', 'p_value']].to_numpy(dtype=float)).all()
    assert output['water_curve_ci_method'] == CURVE_CI_METHOD
    assert 'fixed_observed_covariate_composition' in CURVE_CI_METHOD
    assert output['water_curve'][['adjusted_prevalence', 'ci_low', 'ci_high']].notna().all().all()


def test_original_frequencies_match_by_identifier_and_preserve_primary_models():
    path = Path(__file__).resolve().parents[1] / 'data' / 'data.sav'
    if not path.exists():
        pytest.skip('Local ENS input is not included in the repository.')
    frame, _, _ = load_caries_data(path)
    original_frame = frame.copy(deep=True)
    # Same values in deliberately reversed order: a positional merge would
    # change the sensitivity fit, while a valid ID join reproduces the primary.
    legacy = frame.loc[frame.weight.notna() & frame.weight.gt(0),
                       ['participant_id', 'soda_frequency', 'juice_frequency']].iloc[::-1].copy()
    output = run_associations(frame, legacy_frequencies=legacy)
    pd.testing.assert_frame_equal(frame, original_frame)
    diagnostics = output['diagnostics'].set_index('model')
    name = 'Original 28-day beverage conversion'
    assert len(diagnostics) == 8
    assert diagnostics.loc[name, 'n'] == 4994
    assert diagnostics.loc[name, 'df'] == 1045
    assert diagnostics.loc[name, 'rank'] == diagnostics.loc[name, 'parameters']
    assert diagnostics.loc[name, 'converged']
    table = output['coefficients']
    primary = table.loc[table.model.eq('Primary linear water')].reset_index(drop=True)
    sensitivity = table.loc[table.model.eq(name)].reset_index(drop=True)
    assert primary.term.tolist() == sensitivity.term.tolist()
    columns = ['coef', 'se', 'ci_low', 'ci_high', 'p_value']
    np.testing.assert_allclose(primary[columns], sensitivity[columns], equal_nan=True)
    assert len(output['water_effects']) == 7
    assert len(output['water_contrasts']) == 1

    with pytest.raises(ValueError, match='unique nonmissing participant IDs'):
        run_associations(frame, legacy_frequencies=pd.concat([legacy, legacy.iloc[:1]]))
    with pytest.raises(ValueError, match='cover every examination-design participant'):
        run_associations(frame, legacy_frequencies=legacy.iloc[1:])
