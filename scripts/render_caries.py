#!/usr/bin/env python3
"""Render the English Quarto report using the active Python environment."""
import json
import os
from pathlib import Path
import subprocess
import sys
from run_caries import source_hashes

root=Path(__file__).resolve().parents[1]
manifest=root/'outputs/tables/run_manifest.json'
if not manifest.exists() or json.loads(manifest.read_text()).get('status')!='complete':
    raise SystemExit('Run scripts/run_caries.py through the summarize stage before rendering.')
record=json.loads(manifest.read_text())
if record.get('code_sha256')!=source_hashes(root):
    raise SystemExit('Analysis code changed after the recorded run. Run --stage summarize before rendering.')
if not record.get('original_input_sha256') or not record.get('source_reconciliation'):
    raise SystemExit('Reconcile the original Stata data before rendering the completed report.')
env=os.environ.copy()
env['QUARTO_PYTHON']=sys.executable
env['JUPYTER_PATH']=str(Path(sys.prefix)/'share/jupyter')+os.pathsep+env.get('JUPYTER_PATH','')
subprocess.run(['quarto','render',str(root/'docs/caries_python.qmd'),'--to','html'],
               cwd=root,env=env,check=True)
