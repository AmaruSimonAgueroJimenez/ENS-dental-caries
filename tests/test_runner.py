"""Guard against completed manifests accompanying stale/mixed report inputs."""

from hashlib import sha256
import importlib.util
import json
from pathlib import Path
import pickle
import sys

import pandas as pd
import pytest


@pytest.fixture
def runner(monkeypatch, tmp_path):
    script = Path(__file__).resolve().parents[1] / 'scripts' / 'run_caries.py'
    spec = importlib.util.spec_from_file_location('caries_runner_under_test', script)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    monkeypatch.setattr(module, 'ROOT', tmp_path)
    tables = tmp_path / 'outputs' / 'tables'
    tables.mkdir(parents=True)
    (tables / 'run_manifest.json').write_text('{"status": "complete", "old": true}')
    return module, tmp_path, tables


@pytest.mark.parametrize('stage', ['all', 'prepare', 'models', 'summarize'])
def test_every_stage_invalidates_old_completion_before_failed_input_load(runner, monkeypatch, stage):
    module, root, tables = runner
    monkeypatch.setattr(sys, 'argv', ['run_caries.py', '--stage', stage])

    def failing_load(_):
        manifest = json.loads((tables / 'run_manifest.json').read_text())
        assert manifest['status'] == 'in_progress'
        assert manifest['stage'] == stage
        assert 'old' not in manifest
        raise ValueError('Synthetic unavailable input')

    monkeypatch.setattr(module, 'load_caries_data', failing_load)
    with pytest.raises(ValueError, match='unavailable input'):
        module.main()
    assert json.loads((tables / 'run_manifest.json').read_text())['status'] != 'complete'


@pytest.mark.parametrize('change_source_during_summary', [False, True])
def test_summarize_refreshes_associations_and_requires_stable_source_hashes(
    runner, monkeypatch, change_source_during_summary
):
    module, root, tables = runner
    (tables / 'water_effects.csv').write_text('stale association result')
    source = root / 'src' / 'ens_analysis' / 'inference.py'
    source.parent.mkdir(parents=True)
    source.write_text('# example analysis source\n')
    script = root / 'scripts' / 'run_caries.py'
    script.parent.mkdir(parents=True)
    script.write_text('# example orchestration source\n')
    frame = pd.DataFrame({'eligible': [True, True], 'paper_complete': [True, False]})
    survey = root / 'data' / 'data.sav'
    original = root / 'data' / 'original' / '23.07.26_base_ens_5520_R_enviar.dta'
    original.parent.mkdir(parents=True)
    survey.write_bytes(b'synthetic survey source')
    original.write_bytes(b'synthetic original source')
    audit = {'source_sha256': sha256(survey.read_bytes()).hexdigest()}
    monkeypatch.setattr(module, 'load_caries_data', lambda _: (frame, pd.DataFrame(), audit))
    monkeypatch.setattr(module, 'reconcile_original_data', lambda *_: (
        {'status': 'verified'}, pd.DataFrame(), pd.DataFrame(), pd.DataFrame()))
    prepared = []

    def prepare(*args):
        prepared.append(True)
        (tables / 'water_effects.csv').write_text('fresh association result')

    def summarize(*args):
        assert prepared == [True]
        assert (tables / 'water_effects.csv').read_text() == 'fresh association result'
        if change_source_during_summary:
            source.write_text('# changed during summary\n')
        return pd.DataFrame([{
            'model': 'fixture', 'weighting': 'Survey weighted',
            'auc': 0.6, 'brier': 0.2, 'log_loss': 0.6,
        }])

    monkeypatch.setattr(module, 'prepare', prepare)
    monkeypatch.setattr(module, 'summarize', summarize)
    monkeypatch.setattr(module, 'version', lambda _: 'test-version')
    import ens_analysis.figures
    monkeypatch.setattr(ens_analysis.figures, 'make_figures', lambda *_: None)
    private = root / 'outputs' / 'private'
    private.mkdir(parents=True)
    (private / 'model_results.pkl').write_bytes(pickle.dumps({
        'manifest': {'models': {'fixture': {}}, 'settings': {'outer_folds': 5, 'inner_folds': 3}},
    }))
    monkeypatch.setattr(sys, 'argv', ['run_caries.py', '--stage', 'summarize', '--bootstrap', '5'])
    if change_source_during_summary:
        with pytest.raises(RuntimeError, match='source changed during execution'):
            module.main()
        assert json.loads((tables / 'run_manifest.json').read_text())['status'] != 'complete'
        return
    module.main()
    manifest = json.loads((tables / 'run_manifest.json').read_text())
    assert manifest['status'] == 'complete'
    assert manifest['input_sha256'] == sha256(survey.read_bytes()).hexdigest()
    assert manifest['original_input_sha256'] == sha256(original.read_bytes()).hexdigest()
    assert manifest['code_sha256'] == {
        'scripts/run_caries.py': sha256(script.read_bytes()).hexdigest(),
        'src/ens_analysis/inference.py': sha256(source.read_bytes()).hexdigest(),
    }
    assert manifest['source_reconciliation']['status'] == 'verified'
    assert prepared == [True]
