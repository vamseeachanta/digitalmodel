"""Explicit structure channels must not dispatch jumper-only object names."""
from types import SimpleNamespace

import numpy as np
import pytest

from digitalmodel.workflows import installation_trace_extract as traces


def test_structure_profile_dispatches_explicit_channels(monkeypatch):
    calls = []
    monkeypatch.setattr(traces, '_add_channel', lambda *a: calls.append(a))
    monkeypatch.setattr(traces, '_geometry_channels', lambda *a, **k: calls.append(k))
    api = SimpleNamespace(oeEndA='A', oeEndB='B')
    profile = {'schema_version': 1, 'channels': [
        {'object': 'rig', 'variable': 'Effective tension', 'units': 'kN', 'position': 'End A'},
        {'object': 'body', 'variable': 'Z', 'units': 'm'}], 'geometry_lines': ['rig']}
    traces._profile_channels({}, api, {'time': np.arange(3)}, {}, None, profile)
    assert calls[0][5:8] == ('rig', 'Effective tension', 'kN')
    assert calls[0][9] == 'A'
    assert calls[1][5:8] == ('body', 'Z', 'm')
    assert calls[1][9] is None
    assert calls[2] == {'names': ['rig']}


@pytest.mark.parametrize('profile', [
    {}, {'channels': []}, {'channels': [{'object': 'x'}]},
    {'channels': [{'object': 'x', 'variable': 'Z', 'units': 'm', 'position': 'mid'}]},
    {'channels': [{'object': 'x', 'variable': 'Z', 'units': 'm'}] * 2},
    {'channels': [{'object': 'x', 'variable': 'Z', 'units': 'm'}], 'geometry_lines': ['x', 'x']},
])
def test_invalid_profile_rejected_before_native_access(profile):
    profile.setdefault('schema_version', 1)
    with pytest.raises(ValueError):
        traces._profile_channels({}, None, {'time': np.arange(3)}, {}, None, profile)


def test_profile_override_cannot_replace_embedded_profile(tmp_path):
    import hashlib
    import json
    request = tmp_path / 'request.yml'
    request.write_text(json.dumps({'extraction': {'supplemental_profile': {'schema_version': 1}}}))
    receipt = {'request_sha256': hashlib.sha256(request.read_bytes()).hexdigest()}
    with pytest.raises(ValueError, match='conflict'):
        traces._read_profile(tmp_path, receipt, {'schema_version': 2})
    request.write_text('{}')
    with pytest.raises(ValueError, match='identity'):
        traces._read_profile(tmp_path, receipt, None)


def test_saved_profile_reuse_rejects_missing_or_wrong_digest(tmp_path):
    import hashlib
    import json
    profile = {'schema_version': 1, 'channels': [{'object': 'body', 'variable': 'Z', 'units': 'm'}]}
    request = tmp_path / 'request.yml'
    request.write_text(json.dumps({'extraction': {'supplemental_profile': profile}}))
    receipt = {'request_sha256': hashlib.sha256(request.read_bytes()).hexdigest()}
    with pytest.raises(ValueError, match='differs'):
        traces.verify_profile_metadata(tmp_path, receipt, {})
    metadata = {'supplemental_profile': profile, 'request_sha256': receipt['request_sha256'],
                'supplemental_profile_sha256': 'bad'}
    with pytest.raises(ValueError, match='provenance'):
        traces.verify_profile_metadata(tmp_path, receipt, metadata)
    metadata['supplemental_profile_sha256'] = traces.profile_digest(profile)
    with pytest.raises(ValueError, match='wave'):
        traces.verify_profile_metadata(tmp_path, receipt, metadata)
    metadata['channels'] = {'wave_elevation': {}, 'profile_000': profile['channels'][0]}
    traces.verify_profile_metadata(tmp_path, receipt, metadata)
    metadata['channels']['profile_000'] = {}
    with pytest.raises(ValueError, match='coverage'):
        traces.verify_profile_metadata(tmp_path, receipt, metadata)


def test_profile_arrays_reject_missing_and_nonfinite(tmp_path):
    path = tmp_path / 'traces.npz'
    receipt = {'simulation_stop': 2, 'actual_logging_interval': 1}
    metadata = {'supplemental_profile': {}, 'channels': {'wave_elevation': {}}}
    np.savez(path, time=[0, 1, 2])
    with pytest.raises(ValueError, match='coverage'):
        traces.verify_profile_arrays(path, receipt, metadata)
    np.savez(path, time=[0, 1, 2], wave_elevation=[0, np.nan, 1])
    with pytest.raises(ValueError, match='Invalid'):
        traces.verify_profile_arrays(path, receipt, metadata)
