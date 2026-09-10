"""Scientific display checks: fold ranges, complete predictors and vector exports."""

import json
import xml.etree.ElementTree as ET

import matplotlib.pyplot as plt
from PIL import Image
import numpy as np
import pandas as pd
import pytest

from ens_analysis import figures


def test_importance_averages_repeats_before_fold_stability():
    # Unequal fold weights and large within-fold variation make the distinction
    # between repeat-level noise and fold-level stability observable.
    rows = []
    for fold, weight, age_values, water_values in [
        (1, 1, [.1, .3], [-.4, .2]),
        (2, 3, [.02, .06], [.08, .12]),
    ]:
        for feature, values in [("age", age_values), ("water", water_values)]:
            for repeat, value in enumerate(values):
                rows.append(dict(model="random_forest", feature=feature, fold=fold,
                                 repeat=repeat, brier_increase=value, test_weight_sum=weight))
    result = figures._importance_summary(pd.DataFrame(rows)).set_index("feature")
    assert result.loc["age", "mean"] == pytest.approx(.08)
    assert result.loc["age", "low"] == pytest.approx(.04)
    assert result.loc["age", "high"] == pytest.approx(.20)
    assert result.loc["water", "low"] == pytest.approx(-.10)
    assert result.loc["water", "high"] == pytest.approx(.10)
    assert result.loc["water", "n_folds"] == 2
    assert result.loc["water", "median_rank"] == 1.5
    assert result.loc["water", "best_rank"] == 1
    assert result.loc["water", "worst_rank"] == 2


def _all_predictor_rows():
    rows = []
    for model in figures.MODEL_STYLE:
        for fold in [1, 2]:
            for i, feature in enumerate(figures.FEATURE_LABELS):
                value = -.01 * fold if feature == "water" else (27 - i) * .001 * fold
                rows.append(dict(model=model, fold=fold, feature=feature,
                                 brier_increase=value, test_weight_sum=fold))
    return pd.DataFrame(rows)


def test_all_predictors_retained_and_water_not_selected_or_highlighted(tmp_path, monkeypatch):
    captured = {}

    def capture(fig, *args, **kwargs):
        captured["fig"] = fig
        return []

    monkeypatch.setattr(figures, "_save", capture)
    with plt.rc_context(figures.STYLE):
        figures._importance(_all_predictor_rows(), tmp_path, {"figures": []}, {})
    fig = captured["fig"]
    try:
        axes = fig.axes
        labels = [text.get_text() for text in axes[0].get_yticklabels()]
        assert set(labels) == set(figures.FEATURE_LABELS.values())
        assert len(labels) == 27
        assert labels[-1] == "Water intake"
        assert axes[0].get_xlim() == axes[1].get_xlim()
        assert axes[0].get_xlim()[0] < -.02
        for ax, model in zip(axes[:2], figures.MODEL_STYLE, strict=True):
            point_colours = [collection.get_facecolors()[0] for collection in ax.collections
                             if isinstance(collection, figures.matplotlib.collections.PathCollection)]
            assert len(point_colours) == 27
            assert np.unique(np.round(point_colours, 6), axis=0).shape[0] == 1
        assert fig.get_size_inches() * 25.4 == pytest.approx([180, 235])
    finally:
        plt.close(fig)


def _group_rows():
    rows = []
    for model in figures.MODEL_STYLE:
        for i, (block, n_features) in enumerate(zip(figures.PREDICTOR_BLOCKS, [7, 1, 9, 6, 3, 1], strict=True)):
            rows.append(dict(model=model, block=block, n_features=n_features,
                             brier_increase=.002 * i, auc_drop=.003 * i,
                             minimum_fold_brier_increase=-.005, maximum_fold_brier_increase=.02,
                             minimum_fold_auc_drop=-.01, maximum_fold_auc_drop=.04))
    return pd.DataFrame(rows)


def test_group_export_preserves_physical_size_and_negative_ranges(tmp_path, monkeypatch):
    real_save = figures._save

    def check_then_save(fig, *args, **kwargs):
        assert fig.axes[0].get_xlim()[0] < -.005
        assert fig.axes[0].get_xlim()[1] > .02
        assert fig.axes[1].get_xlim()[0] < -.01
        assert fig.axes[1].get_xlim()[1] > .04
        return real_save(fig, *args, **kwargs)

    monkeypatch.setattr(figures, "_save", check_then_save)
    metadata = {"figures": []}
    with plt.rc_context(figures.STYLE):
        paths = figures._group_contributions(_group_rows(), tmp_path, metadata)
    assert {path.suffix for path in paths} == {".png", ".svg", ".pdf"}
    assert all(path.stat().st_size > 1000 for path in paths)
    with Image.open(tmp_path / "predictor_group_contributions.png") as image:
        assert image.width == pytest.approx(180 / 25.4 * 300, abs=1)
        assert image.height == pytest.approx(116 / 25.4 * 300, abs=1)
        assert image.info["dpi"] == pytest.approx([300, 300], abs=.1)
    svg = ET.parse(tmp_path / "predictor_group_contributions.svg").getroot()
    assert float(svg.attrib["width"].removesuffix("pt")) == pytest.approx(180 / 25.4 * 72)
    svg_text = " ".join(svg.itertext())
    assert "Food intake" in svg_text
    entry = metadata["figures"][0]
    assert entry["physical_canvas_preserved"] is True
    assert "not confidence intervals" in entry["caption"]
    assert "not sums" in entry["caption"]
    assert entry["source_tables"] == ["grouped_importance_stability.csv"]


def test_group_missing_family_fails_instead_of_silently_omitting(tmp_path):
    with pytest.raises(ValueError, match="all six predefined blocks"):
        figures._group_contributions(_group_rows().iloc[1:], tmp_path, {"figures": []})


def test_make_figures_accepts_absent_optional_group_table(tmp_path):
    source = tmp_path / "tables"
    source.mkdir()
    destination = tmp_path / "figures"
    assert figures.make_figures(source, destination) == []
    metadata = json.loads((destination / "figure_metadata.json").read_text())
    assert "grouped_importance_stability.csv" in metadata["missing_or_empty_source_tables"]
    assert metadata["private_predictions_read"] is False
