"""Consequential source-identity, coding, and original-preprocessing checks."""

from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from ens_analysis.data import RAW_COLUMNS, derive_caries_data, load_caries_data
from ens_analysis.reconciliation import DIRECT_COLUMNS, _compare_sources, _numeric, _weekly, reconcile_original_data


@pytest.fixture
def sources():
    sav = pd.DataFrame(1.0, index=range(6), columns=RAW_COLUMNS)
    sav["IdEncuesta"] = np.arange(101, 107)
    sav["Edad"] = 40.
    sav["c6"] = [10, 1, 4, 10, 10, 10]
    sav["m5p3"] = sav["m5p6"] = 12.
    sav["m5p5"] = [0, 1, 2, 0, 0, np.nan]
    sav["m5p8"] = 0.
    sav.loc[4, ["m5p3", "m5p6"]] = 0
    sav.loc[5, ["m5p3", "m5p6", "m5p5", "m5p8", "Fexp_F1F2p_Corr", "Fexp_F2p_Corr"]] = np.nan
    sav["die11"] = [0, 2, 8, 40, 50, 1]
    sav.loc[3, "NEDU1_MINSAL_1"] = np.nan
    sav["die12_cantidad"] = 4.
    sav["die12_unidad"] = 3.
    original = sav.iloc[:5].copy()
    dta = pd.DataFrame({dk: original[sk] for dk, sk in DIRECT_COLUMNS.items()})
    dta["c6"] = original.c6.mask(original.c6.eq(10), 0)
    dta["indigena"] = original.c6.isin(range(1, 10)).astype(float)
    dta["nedu"] = 3 - original.NEDU1_MINSAL_1
    dta["remanentes"] = original.m5p3 + original.m5p6
    dta["cariados"] = original.m5p5 + original.m5p8
    dta["cariesnt"] = dta.cariados.gt(0).astype(float)
    dta["frecsembebida"] = _weekly(dta.die12_cantidad, dta.die12_unidad)
    dta["frecsemjugo"] = _weekly(dta.die13_cantidad, dta.die13_unidad)
    frame, _, _ = derive_caries_data(sav)
    return sav, dta, frame


def test_id_join_category_decoding_and_declared_month_conversion(sources):
    sav, dta, frame = sources
    before = frame.copy(deep=True)
    summary, comparison, water, legacy = _compare_sources(sav, dta.iloc[::-1], frame)
    assert not summary["identity"]["row_order_equal_to_sav"]
    assert summary["cohorts"]["original_complete_cases"] == 3
    assert summary["cohorts"]["original_complete_case_events"] == 2
    assert comparison.n_discrepant_primary.sum() == 0
    assert len(comparison) == 27
    np.testing.assert_allclose(legacy.soda_frequency, 4 / 28)
    assert summary["beverage_month_conversion"]["differences"]["soda_frequency"]["n_different_dentate"] == 4
    pd.testing.assert_frame_equal(before, frame)


def test_water_factor_levels_are_selected_before_complete_case_deletion(sources):
    _, _, mapping, _ = _compare_sources(*sources)
    # 40 remains a factor level despite missing education; edentulous 50 is absent.
    assert mapping.reported_glasses_per_day.tolist() == [0, 2, 8, 40]
    assert mapping.original_r_factor_index.tolist() == [1, 2, 3, 4]


@pytest.mark.parametrize("column,value", [("cariesnt", 1), ("remanentes", 23), ("nedu", 0), ("die11", 99), ("frecsembebida", 2)])
def test_unexpected_original_discrepancy_stops_analysis(sources, column, value):
    sav, dta, frame = sources
    dta.loc[0, column] = value
    with pytest.raises(ValueError, match="discrepancy"):
        _compare_sources(sav, dta, frame)


def test_wrong_participant_and_unknown_category_are_rejected(sources):
    sav, dta, frame = sources
    wrong = dta.copy(); wrong.loc[0, "IdEncuesta"] = 999
    with pytest.raises(ValueError, match="IDs"):
        _compare_sources(sav, wrong, frame)
    dta.loc[0, "nedu"] = 8
    with pytest.raises(ValueError, match="category"):
        _compare_sources(sav, dta, frame)


def test_negative_special_codes_missing_and_arbitrary_text():
    result = _numeric(pd.Series([0, -9999, -8888, np.nan, 4]), "water")
    np.testing.assert_allclose(result, [0, np.nan, np.nan, np.nan, 4], equal_nan=True)
    with pytest.raises(ValueError, match="nonnumeric"):
        _numeric(pd.Series(["unknown"]), "water")
    with pytest.raises(ValueError, match="category"):
        _weekly(pd.Series([1.]), pd.Series([5.]))


def test_real_original_attachment_reconciliation_when_available():
    root = Path(__file__).resolve().parents[1]
    sav = root / "data/data.sav"
    dta = root / "data/original/23.07.26_base_ens_5520_R_enviar.dta"
    if not sav.exists() or not dta.exists():
        pytest.skip("Private original source files not available")
    frame, _, _ = load_caries_data(sav)
    summary, comparison, water, legacy = reconcile_original_data(sav, dta, frame)
    assert summary["cohorts"]["examined"] == 5520
    assert summary["cohorts"]["original_complete_cases"] == 4994
    assert summary["cohorts"]["original_complete_case_events"] == 2664
    assert comparison.n_discrepant_primary.sum() == 0
    assert summary["beverage_month_conversion"]["differences"]["soda_frequency"]["n_different_dentate"] == 520
    assert summary["beverage_month_conversion"]["differences"]["juice_frequency"]["n_different_dentate"] == 286
    assert water.set_index("reported_glasses_per_day").loc[40, "original_r_factor_index"] == 25
    assert len(legacy) == 5520
    assert summary["sources"]["original_dta"]["sha256"] == "754c2d32bafd002b861221d9d468f74a703de4ea9dd6c492cf848f042f3b0966"
