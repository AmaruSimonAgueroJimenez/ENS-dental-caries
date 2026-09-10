"""Guard ID alignment, fixed bands, structural skips and absence of target use."""

from copy import deepcopy
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from ens_analysis.data import RAW_COLUMNS as BASE_RAW_COLUMNS, derive_caries_data, load_caries_data
from ens_analysis.expanded_data import (
    ALL_FEATURES, CATEGORICAL_FEATURES, CONTEXT_FEATURES, INCOME_LABELS,
    INCOME_LOWER_BOUNDS, INTAKE_FEATURES, LABEL_FEATURES, NUMERIC_FEATURES,
    RAW_COLUMNS, SOURCES, SYMPTOM_FEATURES, derive_expanded_data, load_expanded_data,
)


@pytest.fixture
def make_inputs():
    def make(n=6):
        base_raw = pd.DataFrame(1.0, index=range(n), columns=BASE_RAW_COLUMNS)
        base_raw["IdEncuesta"] = np.arange(1, n + 1)
        base_raw["Edad"] = 40.0
        base_raw[["m5p3", "m5p6"]] = 12.0
        base_raw[["m5p5", "m5p8"]] = 0.0
        base, dictionary, audit = derive_caries_data(base_raw)
        raw = pd.DataFrame(1.0, index=range(n), columns=RAW_COLUMNS)
        raw["IdEncuesta"] = np.arange(1, n + 1, dtype=float)
        raw["as27"] = 100000.0
        raw["as28"] = 2.0
        return base, raw, dictionary, audit
    return make


def test_prespecified_blocks_are_disjoint_and_complete():
    assert [len(x) for x in [CONTEXT_FEATURES, SYMPTOM_FEATURES, INTAKE_FEATURES, LABEL_FEATURES]] == [17, 6, 15, 6]
    assert len(ALL_FEATURES) == len(set(ALL_FEATURES)) == 44
    assert len(NUMERIC_FEATURES) == 11
    assert set(NUMERIC_FEATURES).isdisjoint(CATEGORICAL_FEATURES)
    assert set(NUMERIC_FEATURES) | set(CATEGORICAL_FEATURES) == set(ALL_FEATURES)
    assert "caries" not in ALL_FEATURES
    assert {"soda_glasses_daily", "juice_glasses_daily"}.issubset(INTAKE_FEATURES)


def test_all_income_boundaries_and_no_midpoints(make_inputs):
    amounts = [0, *[x for b in INCOME_LOWER_BOUNDS for x in [b - 1, b]], 16000000]
    expected = [1, *[x for band in range(1, 11) for x in [band, band + 1]], 11]
    base, raw, dictionary, audit = make_inputs(len(amounts))
    raw["as27"] = amounts
    raw["as28"] = np.nan
    out, _, result_audit = derive_expanded_data(base, raw, dictionary, audit)
    assert out.income_band.tolist() == [INCOME_LABELS[x] for x in expected]
    assert out.income_band.dtype == object
    assert not result_audit["expanded_income_coding"]["midpoints_used"]


def test_income_fallback_and_exact_amount_precedence(make_inputs):
    base, raw, dictionary, audit = make_inputs()
    raw["as27"] = [np.nan, -9999, 100000, np.inf, -8888, 0]
    raw["as28"] = [4, 5, 11, 6, -9999, np.nan]
    out, _, result_audit = derive_expanded_data(base, raw, dictionary, audit)
    assert out.loc[[0, 1, 2, 3, 5], "income_band"].tolist() == [INCOME_LABELS[x] for x in [4, 5, 2, 6, 1]]
    assert pd.isna(out.loc[4, "income_band"])
    assert result_audit["expanded_income_coding"]["amount_band_disagreements_all"] == 1
    assert result_audit["expanded_income_coding"]["fallback_band_used_all"] == 3


def test_dental_never_visit_is_distinct_from_unknown_reason(make_inputs):
    base, raw, dictionary, audit = make_inputs()
    raw["sb3"] = [np.nan, -9999, -8888, 1, 2, 3]
    raw.loc[0, "sb2"] = 6
    base.loc[0, "dental_visit"] = "Never"
    out, _, result_audit = derive_expanded_data(base, raw, dictionary, audit)
    assert out.loc[0, "dental_visit_reason"] == "Never visited"
    assert out.loc[[1, 2], "dental_visit_reason"].isna().all()
    assert out.loc[3:, "dental_visit_reason"].tolist() == ["Pain or discomfort", "Treatment or continuation", "Routine check-up"]
    assert result_audit["structural_skips"]["dental_never_visited_dentate"] == 1


def test_inconsistent_dental_route_is_not_silently_overwritten(make_inputs):
    base, raw, dictionary, audit = make_inputs()
    raw.loc[0, "sb2"] = 6
    base.loc[0, "dental_visit"] = "Never"
    with pytest.raises(ValueError, match="Inconsistent dental route"):
        derive_expanded_data(base, raw, dictionary, audit)


def test_positive_unknown_diabetes_and_access_not_applicable_are_missing(make_inputs):
    base, raw, dictionary, audit = make_inputs()
    raw["di3"] = [1, 2, 3, -8888, -9999, np.nan]
    raw["sb4"] = [1, -7777, -8888, -9999, np.nan, 5]
    out, _, result_audit = derive_expanded_data(base, raw, dictionary, audit)
    assert out.diabetes_reported.iloc[:2].tolist() == ["Yes", "No"]
    assert out.diabetes_reported.iloc[2:].isna().all()
    assert out.dental_access.iloc[1:5].isna().all()
    assert out.dental_access.iloc[5] == "Did not need care"
    assert result_audit["expanded_positive_unknown_diabetes"]["all"] == 1
    assert result_audit["expanded_raw_special_codes"]["sb4"]["-7777"] == 1


@pytest.mark.parametrize("column,value", [("as28", 12), ("as5_1", 10), ("ta3", 0),
                                          ("di3", 4), ("sb3", 4), ("sb4", 6),
                                          ("sb1", 2.5), ("sb5_2", 7)])
def test_unrecognized_positive_codes_fail_even_if_not_used_as_fallback(make_inputs, column, value):
    base, raw, dictionary, audit = make_inputs()
    raw.loc[0, column] = value
    with pytest.raises(ValueError, match="Unexpected positive category code"):
        derive_expanded_data(base, raw, dictionary, audit)


def test_join_is_by_id_not_order_and_inputs_are_immutable(make_inputs):
    base, raw, dictionary, audit = make_inputs()
    base.index = pd.Index([30, 10, 20, 50, 60, 40], name="original_row")
    raw["ta3"] = [1, 2, 3, 4, 2, 1]
    before = [base.copy(deep=True), raw.copy(deep=True), dictionary.copy(deep=True), deepcopy(audit)]
    expected, _, _ = derive_expanded_data(base, raw, dictionary, audit)
    reordered, _, _ = derive_expanded_data(base, raw.iloc[::-1], dictionary, audit)
    pd.testing.assert_frame_equal(expected, reordered)
    pd.testing.assert_frame_equal(expected[base.columns], base)
    for original, untouched in zip([base, raw, dictionary], before[:3], strict=True):
        pd.testing.assert_frame_equal(original, untouched)
    assert audit == before[3]
    assert expected.smoking_status.tolist() == ["Current daily", "Current occasional", "Former", "Never", "Current occasional", "Current daily"]


@pytest.mark.parametrize("bad_id", [np.nan, 0, 1.5, 2, 999])
def test_missing_duplicate_noninteger_or_mismatched_ids_fail(make_inputs, bad_id):
    base, raw, dictionary, audit = make_inputs()
    raw.loc[0, "IdEncuesta"] = bad_id
    with pytest.raises(ValueError, match="[Ii][Dd]"):
        derive_expanded_data(base, raw, dictionary, audit)


def test_derivations_do_not_use_outcome_and_preserve_extreme_volumes(make_inputs):
    base, raw, dictionary, audit = make_inputs()
    base.loc[0, "soda_glasses_daily"] = 700.0
    expected, _, _ = derive_expanded_data(base, raw, dictionary, audit)
    changed_outcome = base.copy(deep=True)
    changed_outcome["caries"] = [1, 0, np.nan, 1, np.nan, 1]
    changed_outcome["decayed_teeth"] = [2, 0, np.nan, 4, np.nan, 5]
    actual, _, _ = derive_expanded_data(changed_outcome, raw, dictionary, audit)
    pd.testing.assert_frame_equal(expected[list(SOURCES)], actual[list(SOURCES)])
    assert actual.loc[0, "soda_glasses_daily"] == 700.0


def test_local_sav_cohort_coverage_and_no_change_to_original_fields():
    path = Path(__file__).resolve().parents[1] / "data" / "data.sav"
    if not path.exists():
        pytest.skip("Private ENS source is not available")
    base, _, _ = load_caries_data(path)
    out, dictionary, audit = load_expanded_data(path)
    pd.testing.assert_frame_equal(out[base.columns], base)
    assert len(out) == 6233
    assert out.eligible.sum() == 5036
    assert out.loc[out.eligible, "caries"].sum() == 2689
    assert out.participant_id.tolist() == base.participant_id.tolist()
    assert set(dictionary.variable) == set(out.columns)
    assert not dictionary.variable.duplicated().any()
    d = out.loc[out.eligible]
    expected_observed = {"income_band": 4281, "health_insurance": 4984,
                         "smoking_status": 5036, "diabetes_reported": 4993,
                         "dental_visit_reason": 4843, "dental_access": 4840,
                         **dict.fromkeys(SYMPTOM_FEATURES, 5036)}
    assert d[list(expected_observed)].notna().sum().to_dict() == expected_observed
    assert audit["expanded_income_coding"]["amount_and_band_observed_all"] == 5100
    assert audit["expanded_income_coding"]["amount_band_disagreements_all"] == 0
    assert audit["expanded_cohort_validation"]["status"] == "verified"
    assert audit["extreme_values_dentate"]["soda_glasses_daily"]["max"] == d.soda_glasses_daily.max()
    assert audit["integrity"]["extreme_values_clipped"] == 0
    assert dictionary.loc[dictionary.variable.isin(SOURCES), "raw_labels"].str.len().gt(0).all()
