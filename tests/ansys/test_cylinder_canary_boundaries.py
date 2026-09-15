"""Regression evidence for prelaunch identity and durable callback refusals."""
import json

import pytest

from digitalmodel.ansys.analysis_records import digest_bytes
from digitalmodel.ansys.cylinder_canary import run_canary
from tests.ansys.test_cylinder_canary import ORDER, protocol, reapprove_manifest


def test_approved_hash_does_not_authorize_nonfrozen_command_stream(protocol):
    bundle, root, approval, callbacks, calls = protocol
    manifest = json.loads((bundle / 'manifest.json').read_bytes())
    name = manifest['cases'][0]['deck']
    raw = (bundle / name).read_bytes() + b'\n/SYS,unexpected\n'
    (bundle / name).write_bytes(raw)
    next(a for a in manifest['artifacts'] if a['path'] == name)['sha256'] = digest_bytes(raw)
    reapprove_manifest(bundle, approval, manifest)
    result = run_canary(bundle, root, approval, **callbacks)
    assert result['status'] == 'INCOMPLETE'
    assert not calls and result['attempted'] == []
    assert not list(root.glob('attempt-*.json'))


@pytest.mark.parametrize('boundary', ['extract', 'assess', 'adjudicate'])
def test_noncanonical_callback_retains_incomplete_receipt(protocol, boundary):
    bundle, root, approval, callbacks, calls = protocol
    original = callbacks[boundary]
    callbacks[boundary] = lambda *args: {**original(*args), 'invalid': object()}
    result = run_canary(bundle, root, approval, **callbacks)
    assert result['status'] == 'INCOMPLETE'
    assert json.loads((root / 'outcome.json').read_bytes()) == result
    assert len(calls) == (4 if boundary == 'adjudicate' else 1)
    assert result['attempted'] == ORDER[:len(calls)]


def test_assessment_cannot_mutate_retained_records(protocol):
    bundle, root, approval, callbacks, calls = protocol
    def assess(records):
        records[0]['invalid'] = object()
        return {'status': 'PASS', 'checks': []}
    callbacks['assess'] = assess
    result = run_canary(bundle, root, approval, **callbacks)
    assert result['status'] == 'PASS'
    assert all('invalid' not in row for row in result['records'])
