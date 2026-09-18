import hashlib
import json

import numpy as np
import pytest

from digitalmodel.workflows.installation_response_metrics import tension_event_metrics


@pytest.mark.parametrize('defect', ['missing', 'mismatch', 'none'])
def test_audit_events_requires_every_expected_channel(tmp_path, defect):
    from digitalmodel.workflows.installation_event_audit import audit_case
    directory = tmp_path / 'installation_traces'
    directory.mkdir()
    t, y = np.array([0., 1., 2.]), np.array([1., -1., 2.])
    path = directory / 'traces.npz'
    np.savez(path, time=t, tension=y)
    event = tension_event_metrics(t, y, units='kN')
    if defect == 'mismatch':
        event['low_tension']['event_count'] = 42
    channels = {} if defect == 'missing' else {'tension': dict(variable='Effective tension', units='kN', events=event)}
    metadata = directory / 'metadata.json'
    digest = hashlib.sha256(path.read_bytes()).hexdigest()
    metadata.write_text(json.dumps(dict(channels=channels, trace_sha256=digest, simulation_sha256='sim')))
    row = dict(index=0, run_dir=str(tmp_path), metadata_sha256=hashlib.sha256(metadata.read_bytes()).hexdigest(),
               trace_sha256=digest, simulation_sha256='sim')
    result = audit_case(row, expected_channels=('tension',))
    assert result['status'] == ('VERIFIED' if defect == 'none' else 'FAILED')
    assert result['channels_verified'] == (1 if defect == 'none' else 0)


def test_truncated_report_is_rejected(tmp_path):
    from digitalmodel.workflows.installation_event_audit import generate_audit
    report = tmp_path / 'report.json'
    report.write_text(json.dumps({'cases': [{'index': 0, 'status': 'VERIFIED'}],
        'campaign_snapshot': {'cases': [{'index': 0}, {'index': 1}]}}))
    with pytest.raises(ValueError, match='coverage'):
        generate_audit(report, tmp_path / 'audit.json', workers=3)
    assert not (tmp_path / 'audit.json').exists()


@pytest.mark.parametrize('field', ['trace_sha256', 'simulation_sha256'])
def test_digest_link_to_verified_row_required(tmp_path, field):
    from digitalmodel.workflows.installation_event_audit import audit_case
    directory = tmp_path / 'installation_traces'
    directory.mkdir()
    trace = directory / 'traces.npz'
    np.savez(trace, time=[0., 1.])
    digest = hashlib.sha256(trace.read_bytes()).hexdigest()
    metadata = directory / 'metadata.json'
    metadata.write_text(json.dumps(dict(channels={}, trace_sha256=digest, simulation_sha256='sim')))
    row = dict(index=0, run_dir=str(tmp_path), metadata_sha256=hashlib.sha256(metadata.read_bytes()).hexdigest(),
               trace_sha256=digest, simulation_sha256='sim')
    row[field] = 'incorrect'
    result = audit_case(row, expected_channels=())
    assert result['status'] == 'FAILED'


@pytest.mark.parametrize('state', ['VERIFIED', 'MISSING', None])
def test_report_issue_label_tracks_complete_verification(state):
    from digitalmodel.workflows.installation_partial_report import render_html
    cases = [] if state is None else [dict(index=0, status=state, hs_m=1, tp_s=8,
        run_dir='case', channels={}, simulation_sha256='a', trace_sha256='b')]
    summary = dict(created_utc='now', counts={'VERIFIED': 1}, cases=cases,
                   envelopes=[], matrix_sha256='a', campaign_sha256='b')
    assert ('Complete run-demand snapshot' in render_html(summary, {})) == (state == 'VERIFIED')
