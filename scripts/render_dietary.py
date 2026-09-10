#!/usr/bin/env python3
"""Render the new dietary report with the verified analysis interpreter."""
import json
import os
from pathlib import Path
import subprocess
import sys

from run_dietary import hashes

root = Path(__file__).resolve().parents[1]
manifest = root / 'outputs/dietary/tables/run_manifest.json'
if not manifest.exists(): raise SystemExit('Complete run_dietary.py before rendering.')
record = json.loads(manifest.read_text())
if record.get('status') != 'complete': raise SystemExit('The new dietary analysis is not complete.')
sources = sorted((root / 'src').rglob('*.py')) + [root / 'scripts/run_dietary.py']
if record['code_sha256'] != hashes(sources):
    raise SystemExit('Analysis sources changed; run dietary --stage summarize first.')
env = os.environ.copy()
env['QUARTO_PYTHON'] = sys.executable
env['JUPYTER_PATH'] = str(Path(sys.prefix) / 'share/jupyter') + os.pathsep + env.get('JUPYTER_PATH', '')
subprocess.run(['/usr/local/bin/quarto', 'render', str(root / 'docs/dietary_incremental.qmd'), '--to', 'html'],
               cwd=root, env=env, check=True)
