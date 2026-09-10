"""Guard complete predictor coverage in the manuscript's six-panel display."""
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pytest

from ens_analysis import correa_figures, figures


def records():
    return pd.DataFrame([
        dict(model=model, fold=fold, feature=feature,
             brier_increase=(-.005 if feature == 'water' else .001 * i) * fold,
             test_weight_sum=fold)
        for model in ['random_forest', 'spline_logistic']
        for fold in [1, 2]
        for i, feature in enumerate(figures.FEATURE_LABELS)
    ])


def test_six_panels_retain_all_predictors_including_negative_water(tmp_path, monkeypatch):
    captured = {}

    def capture(fig, name, destination, metadata, *args, **kwargs):
        captured['fig'] = fig
        metadata['figures'].append({'name': name})
        return []

    monkeypatch.setattr(correa_figures, '_save', capture)
    metadata = {'figures': []}
    with plt.rc_context(figures.STYLE):
        correa_figures.importance(records(), tmp_path, metadata)
    fig = captured['fig']
    try:
        assert len(fig.axes) == 6
        for column in range(2):
            axes = fig.axes[column::2]
            labels = [label.get_text() for ax in axes for label in ax.get_yticklabels()]
            assert len(labels) == 27
            assert set(labels) == set(figures.FEATURE_LABELS.values())
            assert labels.count('Water intake') == 1
            assert sum(len(ax.patches) for ax in axes) == 27
        assert len({ax.get_xlim() for ax in fig.axes}) == 1
        assert fig.axes[0].get_xlim()[0] < -.01
        assert metadata['figures'][0]['panel_count'] == 6
        for features in metadata['figures'][0]['displayed_predictors'].values():
            assert len(features) == len(set(features)) == 27
        np.testing.assert_allclose(fig.get_size_inches() * 25.4, [180, 215])
    finally:
        plt.close(fig)


def test_missing_water_fails_instead_of_silently_omitting_it(tmp_path):
    data = records().query("feature != 'water'")
    with pytest.raises(ValueError, match='All 27 predictors'):
        correa_figures.importance(data, tmp_path, {'figures': []})
