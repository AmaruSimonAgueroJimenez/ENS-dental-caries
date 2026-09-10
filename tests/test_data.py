"""Tests target consequential cohort/outcome and questionnaire skip errors."""

from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from ens_analysis.data import DIET_FEATURES, RAW_COLUMNS, derive_caries_data, load_caries_data


@pytest.fixture
def raw():
    frame = pd.DataFrame(1.0, index=range(5), columns=RAW_COLUMNS)
    frame["IdEncuesta"] = np.arange(1, 6)
    frame["Edad"] = 40.0
    frame["m5p3"] = 12.0
    frame["m5p6"] = 12.0
    frame["m5p5"] = 0.0
    frame["m5p8"] = 0.0
    frame["die11"] = 4.0
    return frame


def test_missing_exam_is_not_caries_free_and_edentulous_excluded(raw):
    raw.loc[0, ["m5p3", "m5p6", "m5p5", "m5p8"]] = np.nan
    raw.loc[1, ["m5p3", "m5p6", "m5p5", "m5p8"]] = 0
    raw.loc[2, "m5p8"] = np.nan
    raw.loc[3, "m5p5"] = 2
    data, _, audit = derive_caries_data(raw)
    assert np.isnan(data.loc[0, "caries"])
    assert not data.loc[0, "eligible"]
    assert data.loc[1, "caries"] == 0
    assert not data.loc[1, "eligible"]
    assert np.isnan(data.loc[2, "decayed_teeth"])
    assert np.isnan(data.loc[2, "caries"])
    assert data.loc[3, "caries"] == 1
    assert audit["sample_flow"]["dentate_outcome_missing"] == 1


def test_structural_skips_do_not_impute_unexplained_missingness(raw):
    raw.loc[0, ["die6", "die8"]] = 0
    raw.loc[0, "die2"] = 7
    raw.loc[[0, 1], ["die7", "die9", "die3"]] = np.nan
    data, _, audit = derive_caries_data(raw)
    assert data.loc[0, "fruit_portions"] == 0
    assert data.loc[0, "vegetable_portions"] == 0
    assert data.loc[0, "dairy_type"] == "No dairy"
    assert data.loc[1, ["fruit_portions", "vegetable_portions", "dairy_type"]].isna().all()
    assert audit["structural_skips"]["dairy_missing_recoded_no_dairy"] == 1


def test_daily_beverage_conversion_and_structural_zero(raw):
    raw["die12_cantidad"] = [3, 14, 60, np.nan, 10]
    raw["die12_unidad"] = [1, 2, 3, 4, np.nan]
    raw.loc[3, "die12e"] = np.nan
    data, _, _ = derive_caries_data(raw)
    np.testing.assert_allclose(data.soda_frequency, [3, 2, 2, 0, np.nan], equal_nan=True)
    assert data.loc[3, "soda_glasses_daily"] == 0
    assert np.isnan(data.loc[4, "soda_glasses_daily"])


def test_special_missing_and_extremes_are_not_silently_clipped(raw):
    raw["die11"] = [-9999, -8888, 40, 51, 0]
    raw.loc[2, "die7"] = 62.5
    data, _, audit = derive_caries_data(raw)
    assert data.loc[[0, 1, 3], "water"].isna().all()
    assert data.loc[2, "water"] == 40
    assert data.loc[4, "water"] == 0
    assert data.loc[2, "fruit_portions"] == 62.5
    assert audit["extreme_values_dentate"]["water"]["n_above_threshold"] == 1
    assert audit["unquantified_water_code_51"] == 1


@pytest.mark.parametrize("column,value", [("m5p3", 17), ("m5p8", 13), ("m5p5", 0.5)])
def test_impossible_dental_counts_fail_loudly(raw, column, value):
    raw.loc[0, column] = value
    with pytest.raises(ValueError, match="Impossible dental counts"):
        derive_caries_data(raw)


def test_paper_complete_does_not_remove_missingness_in_other_predictors(raw):
    raw.loc[0, "NEDU1_MINSAL_1"] = np.nan
    raw.loc[1, "die11"] = np.nan
    raw.loc[2, "die3"] = np.nan
    data, dictionary, _ = derive_caries_data(raw)
    assert data.eligible.sum() == 5
    assert data.paper_complete.sum() == 3
    assert data.loc[2, "paper_complete"]
    assert len(DIET_FEATURES) == 19
    assert set(dictionary.variable) == set(data.columns)


def test_local_sav_reconstructs_manuscript_and_main_cohort():
    path = Path(__file__).resolve().parents[1] / "data" / "data.sav"
    if not path.exists():
        pytest.skip("Private ENS source is not available; synthetic integrity tests still run")
    data, dictionary, audit = load_caries_data(path)
    assert len(data) == 6233
    assert data.caries.notna().sum() == 5520
    assert data.eligible.sum() == 5036
    assert data.paper_complete.sum() == 4994
    assert data.loc[data.paper_complete, "caries"].sum() == 2664
    assert data.loc[data.eligible, "caries"].notna().all()
    assert data.loc[data.eligible, "weight"].gt(0).all()
    assert audit["survey_design"]["weight_identical_to_F2"]
    # The rural stratum 22 has one PSU; downstream variance estimation must
    # handle it explicitly rather than silently dropping its contribution.
    assert audit["survey_design"]["singleton_strata_full"] == ["22"]
    assert audit["integrity"]["impossible_dental_counts"] == 0
    assert len(audit["source_sha256"]) == 64
    assert dictionary.raw_labels.str.len().gt(0).all()
