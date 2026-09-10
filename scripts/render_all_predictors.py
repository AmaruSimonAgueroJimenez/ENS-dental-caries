#!/usr/bin/env python3
"""Render the all-predictor report from an authenticated completed summary."""
import json
import os
from hashlib import sha256
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
manifest = ROOT / 'outputs/all_predictors/tables/run_manifest.json'
if not manifest.is_file():
    raise SystemExit('Complete the all-predictor summary before rendering.')
record = json.loads(manifest.read_text())
if record.get('status') != 'complete':
    raise SystemExit('The all-predictor analysis is not complete.')
# Only the exact analysis dependencies declared by this run are relevant.
# Unrelated additions to src/ must not invalidate preserved analysis artifacts.
if not record.get('code_sha256'):
    raise SystemExit('Missing analysis-source provenance.')
for relative, expected in record['code_sha256'].items():
    source = (ROOT / relative).resolve()
    if (not source.is_relative_to(ROOT) or not source.is_file()
            or sha256(source.read_bytes()).hexdigest() != expected):
        raise SystemExit(f'Analysis source changed or is missing: {relative}. Regenerate the summary.')
plan = ROOT / 'docs/all-predictor-analysis-plan.md'
if sha256(plan.read_bytes()).hexdigest() != record['plan_sha256']:
    raise SystemExit('The fixed extension plan changed. Regenerate the summary.')
env = os.environ.copy()
env['QUARTO_PYTHON'] = sys.executable
env['JUPYTER_PATH'] = str(Path(sys.prefix) / 'share/jupyter') + os.pathsep + env.get('JUPYTER_PATH', '')
subprocess.run(['/usr/local/bin/quarto', 'render', str(ROOT / 'docs/all_predictors.qmd'), '--to', 'html'],
               cwd=ROOT, env=env, check=True)
