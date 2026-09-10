#!/usr/bin/env python3
"""Package the reviewed all-predictor manuscript, aggregates and figure sources."""
from __future__ import annotations

from datetime import datetime, timezone
from hashlib import sha256
import json
from pathlib import Path
import shutil
import subprocess
import zipfile

ROOT = Path(__file__).resolve().parents[1]
DRAFT = ROOT/'manuscript_drafts/all_predictor_proposal'
DOCUMENTS = ('All_predictor_caries_proposal.docx', 'Supplementary_material_all_predictors.docx',
             'STROBE_reporting_map.docx', 'TRIPOD_AI_reporting_map.docx')
AGGREGATES = ('data_audit.json', 'descriptive_profile.csv', 'descriptive_profile_metadata.json',
              'feature_blocks.json', 'predictor_missingness.csv', 'variable_dictionary.csv',
              'descriptive_provenance.json', 'model_performance.csv', 'paired_contrasts.csv',
              'importance_stability.csv', 'grouped_importance_stability.csv', 'cross_model_agreement.csv',
              'calibration.csv', 'roc.csv', 'subgroup_performance.csv', 'hyperparameter_search.csv',
              'outer_fold_performance.csv', 'permutation_importance.csv', 'grouped_permutation_importance.csv',
              'validation_split_checks.csv', 'training_diagnostics.csv', 'reconstruction_checks.csv', 'prediction_spread.csv',
              'model_manifest.json', 'run_manifest.json')


def digest(path): return sha256(Path(path).read_bytes()).hexdigest()
def read(path): return json.loads(Path(path).read_text())


def main():
    tables, figures = ROOT/'outputs/all_predictors/tables', ROOT/'outputs/all_predictors/figures'
    run = read(tables/'run_manifest.json')
    assert run['status'] == 'complete' and run['n_model_variants'] == 8 and run['n_predictors'] == 44
    assert run['bootstrap_replicates'] == 1000
    for name, expected in run['code_sha256'].items(): assert digest(ROOT/name) == expected, name
    assert digest(ROOT/'docs/all-predictor-analysis-plan.md') == run['plan_sha256']
    reviews = read(DRAFT/'document_qa.json')
    assert reviews['status'] == 'PASS'
    documents = {r['file']: r for r in reviews['documents']}
    assert set(documents) == set(DOCUMENTS)
    for name, record in documents.items():
        assert record['status'] == 'PASS' and record['all_pages_viewed'] and record['pages'] > 0
        assert record['sha256'] == digest(DRAFT/name), f'Stale document review: {name}'
    manuscript = read(DRAFT/'manuscript_source.json')
    assert manuscript['status'] == 'complete'
    assert manuscript['provenance']['run_manifest_sha256'] == digest(tables/'run_manifest.json')
    assert manuscript['provenance']['source_table_sha256']
    for name, expected in manuscript['provenance']['source_table_sha256'].items():
        assert digest(tables/name) == expected, f'Stale manuscript table: {name}'
    maps = read(DRAFT/'reporting_maps_source.json')
    assert maps['status'] == 'complete' and maps['manuscript_source_sha256'] == digest(DRAFT/'manuscript_source.json')
    metadata = read(figures/'figure_metadata.json')
    assert len(metadata['figures']) == 9
    figure_files = {'figure_metadata.json'}
    for figure in metadata['figures']:
        assert 4 <= figure['panels'] <= 6
        for name, expected in figure['files'].items():
            assert digest(figures/name) == expected; figure_files.add(name)
        for name, expected in figure['source_sha256'].items(): assert digest(tables/name) == expected
    assert len(figure_files) == 28
    report = read(DRAFT/'report_qa.json')
    assert report['status'].lower() in ('pass', 'passed') and report['self_contained']
    assert report['html_sha256'] == digest(ROOT/'docs/all_predictors.html')
    assert report['qmd_sha256'] == digest(ROOT/'docs/all_predictors.qmd')
    assert report['run_manifest_sha256'] == digest(tables/'run_manifest.json')
    assert report['quarto_execution']['completed_cells'] == 12
    assert report['embedded_png_count'] == 9
    assert sorted(report['embedded_png_sha256']) == sorted(digest(figures/(f['name']+'.png')) for f in metadata['figures'])
    assert {p.name for p in tables.iterdir() if p.is_file()} == set(AGGREGATES)
    assert {p.name for p in figures.iterdir() if p.is_file()} == figure_files

    selected = {name: DRAFT/name for name in DOCUMENTS}
    selected.update({'aggregate_results/'+name: tables/name for name in AGGREGATES})
    selected.update({'figures/'+name: figures/name for name in figure_files})
    for name in ('manuscript_source.json', 'reporting_maps_source.json', 'references.json',
                 'build_documents.py', 'author_notes.md'):
        selected['editorial_sources/'+name] = DRAFT/name
    for name in ('all_predictors.qmd', 'all_predictors.html', 'all-predictor-analysis-plan.md', 'caries-report.css'):
        selected['report/'+name] = ROOT/'docs'/name
    assert not any(token in name.lower() for name in selected for token in ('private', '.sav', '.pkl', 'correa_revised'))
    stage = DRAFT/'package_contents'; stage.mkdir(exist_ok=True)
    expected_files = set(selected) | {'README.md', 'package_manifest.json'}
    for path in stage.rglob('*'):
        if path.is_file() and str(path.relative_to(stage)) not in expected_files: path.unlink()
    for name, source in selected.items():
        target = stage/name; target.parent.mkdir(parents=True, exist_ok=True); shutil.copy2(source, target)
    (stage/'README.md').write_text(
        '# All-predictor caries proposal\n\n'
        'English author-review proposal centred on the importance of all available variables, '
        'with the earlier manuscript sequence of participant profile, model performance and predictor importance. '
        'Four model families are evaluated with 44 constructs and a complementary 38-construct scenario. '
        'The added neural model is a regularized two-hidden-layer tabular MLP, evaluated under a fixed bounded search.\n\n'
        'The main Word contains three editable tables and five composite figures. The supplement contains '
        'the full new combined results and four additional composite figures. All nine figures are supplied '
        'as PNG (300 dpi), editable SVG and PDF. The reporting maps identify both completed and outstanding items.\n\n'
        'Read editorial_sources/author_notes.md and the executed report for the actual results and limitations. '
        'Importance is model reliance under held-out permutation, not causation. Concurrent symptoms can reflect '
        'existing disease. Conditional bootstrap intervals do not encompass model redevelopment. '
        'The official examination-weight documentation and author declarations require confirmation.\n\n'
        'This package is separate from the prior dietary-increment proposal and Andrea’s earlier correction. '
        'It has not been submitted to a journal or deposited as a public preprint. '
        'Individual records, held-out predictions, fold assignments and model caches are excluded.\n\n'
        'Open report/all_predictors.html for the self-contained report. To reproduce the analysis from the '
        'repository, run scripts/run_all_predictor_models.py, scripts/summarize_all_predictors.py and '
        'scripts/render_all_predictors.py with its Python environment and locally authorized source data. '
        'The six classical variants are authenticated from the preserved dietary results. To rebuild Word, '
        'restore editorial_sources into manuscript_drafts/all_predictor_proposal within the repository and '
        'run build_documents.py using Python with python-docx and pandas.\n')
    commit = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip()
    manifest = {'status': 'complete', 'scope': run['scope'], 'created_utc': datetime.now(timezone.utc).isoformat(),
                'analysis_commit': commit, 'repository': 'https://github.com/AmaruSimonAgueroJimenez/ENS-dental-caries',
                'run_manifest_sha256': digest(tables/'run_manifest.json'),
                'document_review_sha256': digest(DRAFT/'document_qa.json'),
                'report_review_sha256': digest(DRAFT/'report_qa.json'),
                'documents': 4, 'figures': 9,
                'files': {str(p.relative_to(stage)): digest(p) for p in sorted(stage.rglob('*'))
                          if p.is_file() and p.name != 'package_manifest.json'}}
    (stage/'package_manifest.json').write_text(json.dumps(manifest, indent=2, ensure_ascii=False)+'\n')
    output = DRAFT/'All_predictor_caries_proposal_package.zip'
    with zipfile.ZipFile(output, 'w', zipfile.ZIP_DEFLATED) as archive:
        for path in sorted(stage.rglob('*')):
            if path.is_file(): archive.write(path, path.relative_to(stage))
    with zipfile.ZipFile(output) as archive:
        assert archive.testzip() is None and set(archive.namelist()) == expected_files
        for name, expected in manifest['files'].items(): assert sha256(archive.read(name)).hexdigest() == expected
    print(json.dumps({'package': str(output), 'files': len(expected_files), 'bytes': output.stat().st_size,
                      'sha256': digest(output), 'analysis_commit': commit}, indent=2))


if __name__ == '__main__': main()
