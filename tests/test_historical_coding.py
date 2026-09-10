"""Independent coding invariants and optional full R-reference reconciliation."""
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from ens_analysis.historical_coding import (
    CATEGORY_LEVELS, REQUIRED_COLUMNS, _build_historical_matrix,
    historical_column_map, load_historical_matrix,
)
from ens_analysis.data import load_caries_data


@pytest.fixture
def raw():
    result = pd.DataFrame(1., index=range(7), columns=REQUIRED_COLUMNS)
    for name, levels in CATEGORY_LEVELS.items():
        result[name] = float(levels[0][0])
    result['IdEncuesta'] = np.arange(101, 108)
    result['cariesnt'] = [0., 1., 1., 0., 0., 0., 1.]
    result['remanentes'] = [20., 20., 20., 20., 0., 20., 20.]
    result['Edad'] = [20., 30., 40., 50., 60., 70., 80.]
    result['die11'] = [0., 2., 18., 10., 50., 40., -9999.]
    result.loc[3, 'nedu'] = np.nan
    result.loc[1, 'die2'] = 7.
    result.loc[1, 'die3'] = np.nan
    result.loc[0, ['die7', 'die9']] = np.nan
    result.loc[2, ['die6', 'die8']] = 7.
    result['die12_cantidad'] = 4.
    result['die12_unidad'] = 3.
    result['frecsembebida'] = 1.
    result['die13_cantidad'] = 2.
    result['die13_unidad'] = 1.
    result['frecsemjugo'] = 14.
    return result


def test_original_support_before_complete_cases_and_weekly_units(raw):
    before = raw.copy(deep=True)
    X, y, metadata = _build_historical_matrix(raw)
    assert X.index.tolist() == ['101', '102', '103', '106']
    assert y.tolist() == [0, 1, 1, 0]
    # Ten glasses occurs only in the education-missing row but remains a level.
    # Edentulous fifty glasses is excluded before water levels are dropped.
    assert [m['reported_glasses_per_day'] for m in metadata['water_mapping']] == [0, 2, 10, 18, 40]
    assert X.die11.tolist() == [1., 2., 4., 5.]
    assert X.frecsembebida.eq(1.).all()   # 4/month -> 1/week, not 4/28 per day.
    assert X.frecsemjugo.eq(14.).all()  # 2/day -> 14/week.
    pd.testing.assert_frame_equal(before, raw)


def test_fixed_treatment_contrasts_zero_days_and_structural_skips(raw):
    X, _, _ = _build_historical_matrix(raw)
    assert X.shape == (4, 90)
    assert X.columns[:3].tolist() == [
        'die1a1_vez_a_la_semana', 'die1amenos_de_3_veces_al_mes',
        'die1amenos_de_1_vez_al_mes_o_nunca',
    ]
    assert X.columns[-2:].tolist() == ['remanentes', 'Edad']
    assert X.loc['101', ['die7', 'die9']].eq(0).all()
    assert X.loc['102', 'die3no_consume_lacteos'] == 1.
    assert X.loc['103', 'die67'] == X.loc['103', 'die87'] == 1.
    assert X.loc['101', [f'die6{i}' for i in range(1, 8)]].eq(0).all()
    assert 'die60' not in X and 'die80' not in X and '(Intercept)' not in X
    assert 'edad_cat' not in X and sum(c == 'die11' for c in X) == 1
    # Unobserved levels are retained as zero columns from the audited codebook.
    assert X['regioni._tarapaca'].eq(0).all()
    mapping = historical_column_map()
    assert sum(m['kind'] == 'numeric' for m in mapping) == 7
    assert sum(m['kind'] == 'category_indicator' for m in mapping) == 83
    assert len({m['predictor'] for m in mapping}) == 27
    assert next(m for m in mapping if m['encoded_column'] == 'die67')['predictor'] == 'fruit_days'


@pytest.mark.parametrize('column,value,match', [
    ('die6', 8., 'Unknown category'),
    ('cariesnt', 2., 'Unknown category'),
    ('die11', 51., 'water value'),
    ('frecsembebida', 4/28, 'weekly beverage'),
    ('die13_unidad', 5., 'Unknown category'),
    ('Edad', np.inf, 'Invalid numeric'),
])
def test_unknown_or_changed_rules_fail(raw, column, value, match):
    raw.loc[0, column] = value
    with pytest.raises(ValueError, match=match):
        _build_historical_matrix(raw)


def test_invalid_identifier_fails_instead_of_merging_people(raw):
    raw.loc[1, 'IdEncuesta'] = raw.loc[0, 'IdEncuesta']
    with pytest.raises(ValueError, match='participant IDs'):
        _build_historical_matrix(raw)


def test_exact_private_r_matrix_when_sources_available():
    root = Path(__file__).resolve().parents[1]
    dta = root/'data/original/23.07.26_base_ens_5520_R_enviar.dta'
    sav = root/'data/data.sav'
    reference_path = root/'outputs/private/historical_reference/r_matrix.csv'
    if not (dta.exists() and sav.exists() and reference_path.exists()):
        pytest.skip('Private original sources and R reference not available')
    frame, _, _ = load_caries_data(sav)
    # Deliberately reverse the frame: matching must use IDs, never row position.
    X, y, metadata = load_historical_matrix(dta, frame.iloc[::-1])
    reference = pd.read_csv(reference_path, dtype={'participant_id': str}).set_index('participant_id')
    assert X.shape == (4994, 90) and int(y.sum()) == 2664
    assert X.index.equals(reference.index)
    pd.testing.assert_series_equal(y, reference.pop('caries'), check_dtype=False)
    assert X.columns.tolist() == reference.columns.tolist()
    np.testing.assert_array_equal(X.to_numpy(), reference.to_numpy())
    assert metadata['status'] == 'verified'
    assert metadata['n_numeric_columns'] == 7
    assert len(metadata['column_map']) == 90
    assert metadata['source_dta_sha256'] == '754c2d32bafd002b861221d9d468f74a703de4ea9dd6c492cf848f042f3b0966'
    changed = frame.copy()
    changed.loc[changed.participant_id.eq(X.index[0]), 'caries'] = 1-y.iloc[0]
    with pytest.raises(ValueError, match='outcome differs'):
        load_historical_matrix(dta, changed)
