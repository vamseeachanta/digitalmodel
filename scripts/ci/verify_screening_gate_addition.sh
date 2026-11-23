#!/usr/bin/env bash
# Verify that YAML reformatting preserves the screening-only gate addition.
set -euo pipefail
uv run --no-sync --no-sources python - "${1:?Usage: bash scripts/ci/verify_screening_gate_addition.sh BASE}" <<'PY'
import pathlib
import subprocess
import sys

import yaml

path = '.claude/quality-gates.yaml'
before = yaml.safe_load(subprocess.check_output(['git', 'show', f'{sys.argv[1]}:{path}']))
after = yaml.safe_load(pathlib.Path(path).read_text(encoding='utf-8'))
gate = before['gates']['tests-subsea']
folder = 'tests/riser_configuration_screen/'
if folder in gate['test_roots'] or folder in gate['command']:
    raise SystemExit('Baseline already includes the screening suite')
gate['test_roots'].append(folder)
needle = 'python -m pytest tests/subsea/'
if gate['command'].count(needle) != 1:
    raise SystemExit('Expected subsea pytest command was not found exactly once')
gate['command'] = gate['command'].replace(needle, f'python -m pytest {folder} tests/subsea/')
if before != after:
    raise SystemExit('Gate values differ beyond the two screening additions')
print('PASS: exactly two screening additions; all other YAML values unchanged')
PY
