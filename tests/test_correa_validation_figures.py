"""Preserve the sensitivity figure's six panels and distinct uncertainty types."""
from hashlib import sha256

import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection, PathCollection
import numpy as np
import pandas as pd
import pytest

from ens_analysis import correa_validation_figures as cvf


def sensitivity_tables():
    performance = pd.DataFrame([
        dict(model=model, weighting="Survey weighted", n=5036,
             auc=.58 + .01 * index, brier=.25 - .005 * index)
        for index, model in enumerate(cvf.SENSITIVITY_MODELS)
    ])
    subset = performance.drop(columns="weighting").assign(
        subset="Exact manuscript subset", n=4994, auc=lambda x:x.auc-.001)
    subgroups = pd.DataFrame([
        dict(model=model, variable=variable, level=level, n=100,
             auc=.55 + .003 * group, brier=.22 + .001 * group)
        for model in cvf.SENSITIVITY_MODELS
        for group, (variable, level, _) in enumerate(cvf.SUBGROUPS)
    ])
    comparisons, importance = [], []
    for model in cvf.WATER_MODELS:
        for index, encoding in enumerate(cvf.WATER_ENCODINGS):
            identifier = model if encoding == "physical" else model + "_water_" + encoding
            importance.append(dict(
                model=identifier, base_model=model, encoding=encoding, feature="water", n_folds=5,
                brier_increase=-.0002 * (index + 1), minimum_fold_brier_increase=-.001,
                maximum_fold_brier_increase=.0003,
            ))
            if encoding != "physical":
                zero = model == "random_forest" and encoding == "index"
                comparisons.append(dict(
                    model=identifier, base_model=model, encoding=encoding, reference_encoding="physical",
                    metric="auc", difference=0 if zero else -.002,
                    ci_low=0 if zero else -.005, ci_high=0 if zero else .001,
                ))
    return {
        "model_performance.csv": performance,
        "prediction_subset_sensitivity.csv": subset,
        "subgroup_performance.csv": subgroups,
        "water_encoding_comparisons.csv": pd.DataFrame(comparisons),
        "water_encoding_importance_stability.csv": pd.DataFrame(importance),
    }


def capture_figure(monkeypatch):
    captured = {}

    def capture(fig, name, destination, metadata, caption, sources, alt_text):
        captured["fig"] = fig
        metadata["figures"].append(dict(name=name, caption=caption, source_tables=sources))
        return []

    monkeypatch.setattr(cvf, "_save_and_record", capture)
    return captured


def test_six_panels_keep_old_metrics_and_show_zero_and_negative_water(tmp_path, monkeypatch):
    captured = capture_figure(monkeypatch)
    tables = sensitivity_tables()
    metadata = {"figures": []}
    with plt.rc_context(cvf.STYLE):
        cvf._sensitivity(tables, tmp_path, metadata)
    fig = captured["fig"]
    try:
        assert len(fig.axes) == 6
        assert [len(ax.patches) for ax in fig.axes[:4]] == [6, 6, 33, 33]
        for axis in fig.axes[:2]:
            assert axis.get_ylim()[0] == 0
        assert fig.axes[2].get_xlim()[0] == fig.axes[3].get_xlim()[0] == 0
        np.testing.assert_allclose(
            [patch.get_height() for patch in fig.axes[0].patches[:3]],
            tables["model_performance.csv"].auc,
        )
        e, f = fig.axes[4:]
        assert len(e.get_yticklabels()) == 4
        assert len(f.get_yticklabels()) == 6
        e_points = [item for item in e.collections if isinstance(item, PathCollection)]
        f_points = [item for item in f.collections if isinstance(item, PathCollection)]
        assert len(e_points) == 4 and len(f_points) == 6
        assert e_points[0].get_offsets()[0, 0] == 0
        first_interval = next(item for item in e.collections if isinstance(item, LineCollection))
        np.testing.assert_array_equal(first_interval.get_segments()[0], [[0, 0], [0, 0]])
        np.testing.assert_allclose(
            [item.get_offsets()[0, 0] for item in f_points],
            tables["water_encoding_importance_stability.csv"].brier_increase,
        )
        assert all(item.get_offsets()[0, 0] < 0 for item in f_points)
        assert "95% confidence" in e.get_xlabel()
        assert "range of fold means" in f.get_xlabel()
        entry = metadata["figures"][0]
        assert entry["panel_count"] == 6
        assert len(entry["displayed_water_models"]) == 6
        assert entry["water_comparison_reference"] == "physical"
        assert "not confidence intervals" in entry["panel_uncertainty"]["f"]
        np.testing.assert_allclose(fig.get_size_inches() * 25.4, [180, 220])
    finally:
        plt.close(fig)


def test_missing_encoding_fails_instead_of_silently_omitting_it(tmp_path):
    tables = sensitivity_tables()
    tables["water_encoding_importance_stability.csv"] = tables[
        "water_encoding_importance_stability.csv"].iloc[:-1]
    with pytest.raises(ValueError, match="every displayed model/coding combination"):
        cvf._sensitivity(tables, tmp_path, {"figures": []})


def test_encoding_sources_are_required_and_hashed(tmp_path, monkeypatch):
    tables = sensitivity_tables()
    for name, table in tables.items():
        table.to_csv(tmp_path / name, index=False)
    captured = capture_figure(monkeypatch)
    metadata = {}
    cvf.make_correa_validation_figures(tmp_path, tmp_path / "figures", metadata)
    try:
        for name in ["water_encoding_comparisons.csv", "water_encoding_importance_stability.csv"]:
            assert metadata["source_table_sha256"][name] == sha256((tmp_path / name).read_bytes()).hexdigest()
            assert name in metadata["figures"][0]["source_tables"]
    finally:
        plt.close(captured["fig"])
    (tmp_path / "water_encoding_comparisons.csv").unlink()
    missing_metadata = {}
    assert cvf.make_correa_validation_figures(tmp_path, tmp_path / "figures", missing_metadata) == []
    assert missing_metadata["figures"] == []
    assert "water_encoding_comparisons.csv" in missing_metadata["missing_or_empty_source_tables"]
