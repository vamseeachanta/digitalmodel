"""Optimization must preserve export and source-integrity refusal.

No engineering values are used. The independent oracle is literal JSON
inequality (synthetic versus tampered) and reference identity membership.
Python Language Reference, section 7.3, specifies assert removal with -O;
the command-line documentation specifies that -OO additionally drops docstrings.
"""

import json
import os
from pathlib import Path
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[2]
MODES = [(), ("-O",), ("-OO",)]

EXPORT_PROGRAM = r'''
import json
from pathlib import Path
import sys
from unittest.mock import patch
from digitalmodel.workflows import installation_monitoring_demo as module

directory, fault = Path(sys.argv[1]), sys.argv[2]
output = directory / 'output.json'
arguments = ['demo']
for name in ('summary', 'criteria', 'demo-config'):
    path = directory / (name + '.json')
    path.write_text('{}', encoding='utf-8')
    arguments.extend(['--' + name, str(path)])
arguments.extend(['--output', str(output)])
original_write = Path.write_text
if fault == 'existing':
    output.write_text('prior evidence', encoding='utf-8')

def write(path, text, *args, **kwargs):
    if path == output and fault == 'changed':
        text = text.replace('synthetic', 'tampered')
    return original_write(path, text, *args, **kwargs)

with patch.object(sys, 'argv', arguments), patch.object(
    module, 'prepare_payload', return_value={'status': 'synthetic'}
), patch.object(Path, 'write_text', write):
    try:
        module.main()
    except (ValueError, FileExistsError) as error:
        print(json.dumps({'status': 'refused', 'reason': str(error)}))
    else:
        print(json.dumps({'status': 'accepted'}))
'''

REFERENCE_PROGRAM = r'''
from hashlib import sha256
import json
from pathlib import Path
import sys
from digitalmodel.ansys.analysis_records import verify_reference

directory, fault = Path(sys.argv[1]), sys.argv[2]
path = directory / 'source.bin'
original = b'original synthetic evidence'
path.write_bytes(original)
reference = {'id': 'source', 'sha256': sha256(original).hexdigest()}
resolver = {'source': path}
if fault == 'changed':
    path.write_bytes(b'tampered synthetic evidence')
elif fault == 'identity':
    reference['id'] = 'unknown'
elif fault == 'malformed':
    reference['sha256'] = 'not-a-digest'
elif fault == 'missing':
    resolver['source'] = directory / 'absent.bin'
try:
    received = verify_reference(reference, resolver)
except ValueError as error:
    print(json.dumps({'status': 'refused', 'reason': str(error)}))
else:
    print(json.dumps({'status': 'accepted', 'unchanged': received == original}))
'''


def run_child(program, mode, tmp_path, fault):
    env = os.environ.copy()
    for key in ("PYTHONOPTIMIZE", "GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(key, None)
    env["PYTHONPATH"] = str(ROOT / "src")
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    result = subprocess.run(
        [sys.executable, *mode, "-c", program, str(tmp_path), fault],
        cwd=ROOT, env=env, capture_output=True, text=True, check=False,
    )
    assert result.returncode == 0, result.stderr
    return json.loads(result.stdout)


@pytest.mark.parametrize("mode", MODES, ids=["normal", "optimized", "docstrings_removed"])
@pytest.mark.parametrize("fault", ["none", "changed", "existing"])
def test_export_readback_refuses_changed_payload_and_preserves_prior_file(mode, fault, tmp_path):
    result = run_child(EXPORT_PROGRAM, mode, tmp_path, fault)
    if fault == "none":
        assert result == {"status": "accepted"}
        assert json.loads((tmp_path / "output.json").read_text())["status"] == "synthetic"
    else:
        assert result["status"] == "refused"
        if fault == "changed":
            assert result["reason"] == "Monitoring payload changed during export"
        else:
            assert (tmp_path / "output.json").read_text() == "prior evidence"


@pytest.mark.parametrize("mode", MODES, ids=["normal", "optimized", "docstrings_removed"])
@pytest.mark.parametrize("fault", ["none", "changed", "identity", "malformed", "missing"])
def test_reference_digest_identity_and_availability_remain_enforced(mode, fault, tmp_path):
    result = run_child(REFERENCE_PROGRAM, mode, tmp_path, fault)
    if fault == "none":
        assert result == {"status": "accepted", "unchanged": True}
    else:
        assert result["status"] == "refused"
        assert result["reason"] == {
            "changed": "evidence changed: source",
            "identity": "evidence unavailable: unknown",
            "malformed": "invalid evidence digest",
            "missing": "evidence unavailable: source",
        }[fault]
