"""Package only the independent dietary proposal after document and result QA.

The editorial files and final ZIP remain under the ignored manuscript directory.
The package contains aggregate outputs, never individual records or model caches.
"""
from __future__ import annotations

from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import zipfile

ROOT = Path(__file__).resolve().parents[1]
DRAFT = ROOT / 'manuscript_drafts/dietary_incremental_proposal'
DOCUMENTS = [
    'Dietary_information_caries_proposal.docx', 'Supplementary_material_dietary.docx',
    'STROBE_reporting_map.docx', 'TRIPOD_AI_reporting_map.docx',
    'tables/Table_1.docx', 'tables/Table_2.docx', 'tables/Table_3.docx',
]
AGGREGATES = (
    'calibration.csv', 'data_audit.json', 'descriptive_profile.csv', 'descriptive_profile_metadata.json',
    'feature_blocks.json', 'grouped_importance_stability.csv', 'grouped_permutation_importance.csv',
    'hyperparameter_search.csv', 'importance_stability.csv', 'model_manifest.json', 'model_performance.csv',
    'outer_fold_performance.csv', 'paired_contrasts.csv', 'permutation_importance.csv', 'predictor_missingness.csv',
    'roc.csv', 'run_manifest.json', 'subgroup_performance.csv', 'validation_split_checks.csv', 'variable_dictionary.csv',
)


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def read(path):
    return json.loads(Path(path).read_text())


def main():
    tables = ROOT / 'outputs/dietary/tables'
    figures = ROOT / 'outputs/dietary/figures'
    run = read(tables / 'run_manifest.json')
    assert run['status'] == 'complete', 'The new analysis is incomplete'
    for name, expected in run['code_sha256'].items():
        assert sha(ROOT / name) == expected, f'Stale analysis: {name}'
    qa = read(DRAFT / 'document_qa.json')
    assert qa['status'] == 'PASS'
    reviews = {item['file']: item for item in qa['documents']}
    assert set(reviews) == set(DOCUMENTS)
    for name in DOCUMENTS:
        item = reviews[name]
        assert item['status'] == 'PASS' and item['all_pages_viewed']
        assert item['pages'] > 0 and item['sha256'] == sha(DRAFT / name), f'Stale document review: {name}'
    content = read(DRAFT / 'manuscript_source.json')
    assert content['status'] == 'complete'
    provenance = content['provenance']
    assert provenance['run_manifest_sha256'] == sha(tables / 'run_manifest.json')
    assert provenance['pre_fit_plan_sha256'] == sha(ROOT / 'docs/dietary-incremental-analysis-plan.md')
    assert provenance['source_table_sha256'], 'Manuscript has no result provenance'
    for name, expected in provenance['source_table_sha256'].items():
        assert sha(tables / name) == expected, f'Stale manuscript result source: {name}'
    maps = read(DRAFT / 'reporting_maps_source.json')
    assert maps['status'] == 'complete' and maps['manuscript_source_sha256'] == sha(DRAFT / 'manuscript_source.json')
    metadata = read(figures / 'figure_metadata.json')
    assert len(metadata['figures']) == 9
    for figure in metadata['figures']:
        for name, expected in figure['files'].items():
            assert sha(figures / name) == expected
        for name, expected in figure['source_sha256'].items():
            assert sha(tables / name) == expected
    report_qa = read(DRAFT / 'report_qa.json')
    assert report_qa['status'] == 'pass' and report_qa['quarto_execution']['completed_cells'] == 16
    assert report_qa['html_sha256'] == sha(ROOT / 'docs/dietary_incremental.html')
    assert report_qa['qmd_sha256'] == sha(ROOT / 'docs/dietary_incremental.qmd')
    assert report_qa['run_manifest_sha256'] == sha(tables / 'run_manifest.json')
    assert report_qa['self_contained'] and report_qa['embedded_png_count'] == 9
    assert sorted(report_qa['embedded_png_sha256']) == sorted(
        sha(figures / (f['name'] + '.png')) for f in metadata['figures'])

    # A dedicated staging tree cannot accidentally include adjacent Andrea files.
    stage = DRAFT / 'package_contents'
    stage.mkdir(exist_ok=True)
    selected = {}
    for name in DOCUMENTS:
        selected[name] = DRAFT / name
    assert {p.name for p in tables.iterdir() if p.is_file()} == set(AGGREGATES)
    for name in AGGREGATES:
        selected['aggregate_results/' + name] = tables / name
    figure_files = {f['name'] + '.' + ext for f in metadata['figures'] for ext in ('png', 'svg', 'pdf')}
    figure_files.add('figure_metadata.json')
    assert len(figure_files) == 28
    assert {p.name for p in figures.iterdir() if p.is_file()} == figure_files
    for name in sorted(figure_files):
        selected['figures/' + name] = figures / name
    for name in ['manuscript_source.json', 'reporting_maps_source.json', 'references.json',
                 'build_documents.py', 'author_notes.md']:
        selected['editorial_sources/' + name] = DRAFT / name
    for name in ['dietary_incremental.html', 'dietary_incremental.qmd',
                 'dietary-incremental-analysis-plan.md', 'caries-report.css']:
        selected['report/' + name] = ROOT / 'docs' / name
    for name in selected:
        assert not any(token in name.lower() for token in ['private', '.sav', '.dta', '.pkl', 'correa_revised'])
    expected = set(selected) | {'README.md', 'package_manifest.json'}
    for old in stage.rglob('*'):
        if old.is_file() and str(old.relative_to(stage)) not in expected:
            old.unlink()
    for name, source in selected.items():
        target = stage / name
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, target)
    readme = (
        '# Independent dietary incremental-value proposal\n\n'
        'English manuscript and supplement for author review, based only on the new 44-predictor analysis. '
        'This package does not revise or include Andrea’s previous correction.\n\n'
        'The manuscript embeds three editable tables and five composite figures. The supplement contains '
        'new analytical detail and four supplementary composite figures; the aggregate CSV files provide '
        'the full machine-readable results. All nine figures are supplied as PNG (300 dpi), editable SVG and PDF.\n\n'
        'Main result: adding 15 dietary indicators did not demonstrate improvement in the three model families. '
        'The primary spline Brier gain is −0.00439 (conditional 95% interval −0.00716 to −0.00162). '
        'A negative gain represents worse probability prediction. This does not estimate a causal dietary effect.\n\n'
        'Read editorial_sources/author_notes.md for source confirmation and author-review requirements. '
        'These files are a proposal, not a submitted article or a public preprint.\n\n'
        'Open report/dietary_incremental.html for the self-contained executed report. The QMD is provided '
        'as source; rerun it from the repository using scripts/render_dietary.py after completing the analysis. '
        'No individual records, predictions, fold assignments or fitted caches are included.\n'
    )
    (stage / 'README.md').write_text(readme)
    commit = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip()
    manifest = {
        'status': 'complete', 'scope': 'Independent dietary proposal; Andrea correction excluded',
        'created_utc': datetime.now(timezone.utc).isoformat(), 'analysis_commit': commit,
        'repository': 'https://github.com/AmaruSimonAgueroJimenez/ENS-dental-caries',
        'run_manifest_sha256': sha(tables / 'run_manifest.json'),
        'manuscript_source_sha256': sha(DRAFT / 'manuscript_source.json'),
        'document_review_sha256': sha(DRAFT / 'document_qa.json'),
        'report_review_sha256': sha(DRAFT / 'report_qa.json'),
        'document_count': len(DOCUMENTS), 'figure_count': 9,
        'files': {str(p.relative_to(stage)): sha(p) for p in sorted(stage.rglob('*'))
                  if p.is_file() and p.name != 'package_manifest.json'},
    }
    (stage / 'package_manifest.json').write_text(json.dumps(manifest, indent=2, ensure_ascii=False) + '\n')
    destination = DRAFT / 'Dietary_incremental_value_proposal_package.zip'
    with zipfile.ZipFile(destination, 'w', zipfile.ZIP_DEFLATED) as archive:
        for path in sorted(stage.rglob('*')):
            if path.is_file():
                archive.write(path, path.relative_to(stage))
    with zipfile.ZipFile(destination) as archive:
        assert archive.testzip() is None
        assert set(archive.namelist()) == expected
        for name, expected_sha in manifest['files'].items():
            assert hashlib.sha256(archive.read(name)).hexdigest() == expected_sha
    print(json.dumps({'package': str(destination), 'files': len(expected), 'bytes': destination.stat().st_size,
                      'sha256': sha(destination), 'analysis_commit': commit}, indent=2))


if __name__ == '__main__':
    main()
