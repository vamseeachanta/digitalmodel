import pytest
import json
from hashlib import sha256
from digitalmodel.workflows import vessel_capability_report as report

from digitalmodel.workflows.vessel_capability_report import critical_periods, render_html, generate_report


def test_critical_periods_excludes_unverified_and_discloses_coverage():
    rows = [dict(index=0, hs_m=1, tp_s=4, status='VERIFIED', peak_tension_kN=10,
                 maximum_low_tension_duration_s=2),
            dict(index=1, hs_m=1, tp_s=5, status='FAILED', peak_tension_kN=999),
            dict(index=2, hs_m=1, tp_s=6, status='VERIFIED', peak_tension_kN=20,
                 maximum_low_tension_duration_s=1)]
    result = critical_periods(rows)[0]
    assert result['verified_periods'] == [4, 6]
    assert result['complete_tp_row'] is False
    assert result['peak_tension']['tp_s'] == 6
    assert result['low_duration']['tp_s'] == 4


def test_render_preserves_limitations_and_escapes_basis():
    summary = dict(created_utc='now', counts={}, cases=[], envelopes=[],
                   critical_periods=[], design_basis={'vessel': '<test>'},
                   matrix_sha256='a', campaign_sha256='b')
    html = render_html(summary)
    for phrase in ('Introduction', 'Summary and conclusions', 'Design data',
                   'Analysis methodology', 'Results', 'Appendix',
                   'No operating window has been established', 'not physical slack'):
        assert phrase in html
    assert '&lt;test&gt;' in html and '<test>' not in html
    assert 'Jumper' not in html


def test_refuses_overwrite(tmp_path):
    output = tmp_path / 'report.html'
    output.write_text('preserve')
    with pytest.raises(FileExistsError):
        generate_report('absent', 'absent', output, {})
    assert output.read_text() == 'preserve'


def test_event_audit_failure_excludes_case_from_envelope(monkeypatch, tmp_path):
    matrix = tmp_path / 'matrix.json'
    matrix.write_text(json.dumps({'cases': []}))
    campaign = tmp_path / 'campaign.json'
    campaign.write_text(json.dumps({'matrix_sha256': sha256(matrix.read_bytes()).hexdigest()}))
    row = dict(index=0, hs_m=1, tp_s=8, status='VERIFIED', channels={
        'profile_000': dict(variable='Effective tension', units='kN', object='wire')})
    monkeypatch.setattr(report, 'collect_cases', lambda *a, **k: [row])
    monkeypatch.setattr(report, '_audit_profile', lambda *a, **k: {'status': 'FAILED'})
    result = report.generate_report(campaign, matrix, tmp_path / 'report.html', {})
    assert result['counts'] == {'FAILED': 1}
    assert result['envelopes'] == [] and result['critical_periods'] == []
    assert 'channels' not in result['cases'][0]


def test_missing_profile_channel_fails_before_event_audit(monkeypatch, tmp_path):
    traces = tmp_path / 'installation_traces'
    traces.mkdir()
    (tmp_path / 'run.json').write_text('{}')
    metadata = traces / 'metadata.json'
    metadata.write_text('{}')
    row = dict(index=0, run_dir=str(tmp_path), metadata_sha256=sha256(metadata.read_bytes()).hexdigest())
    def reject(*args):
        raise ValueError('Supplemental channel coverage mismatch')
    monkeypatch.setattr(report, 'verify_profile_metadata', reject)
    monkeypatch.setattr(report, 'audit_case', lambda *a, **k: pytest.fail('Must reject missing coverage first'))
    result = report._audit_profile(row)
    assert result['status'] == 'FAILED'
    assert 'coverage mismatch' in result['errors'][0]


def test_status_table_uses_snapshots_instead_of_preparation_labels():
    baseline = {'status': 'paused', 'cases': [{'status': 'COMPLETED'}, {'status': 'MISSING'}]}
    sensitivity = {'captured_utc': '2026-09-18T22:00:00Z', 'snapshot': {
        'status': 'completed', 'cases': [{'status': 'completed'}] * 5}}
    text = report._pending({'dry_mass_t': 5.97}, baseline, sensitivity)
    assert 'paused' in text and 'completed: 1' in text and 'missing: 1' in text
    assert 'completed: 5' in text and '2026-09-18T22:00:00Z' in text
    assert 'results pending' not in text and 'in progress' not in text


@pytest.mark.parametrize('status', ['failed', 'waiting', 'running', 'unexpected'])
def test_status_does_not_promote_incomplete_or_unknown(status):
    text = report._pending({}, {'status': status, 'cases': [{'status': status}]}, None)
    assert status in text
    assert 'No campaign snapshot supplied' in text
    assert 'completed:' not in text


def test_generate_requires_pinned_sensitivity_before_output(monkeypatch, tmp_path):
    matrix = tmp_path / 'matrix.json'
    matrix.write_text('{"cases": []}')
    campaign = tmp_path / 'campaign.json'
    campaign.write_text(json.dumps({'matrix_sha256': sha256(matrix.read_bytes()).hexdigest(),
                                    'status': 'paused', 'cases': []}))
    sensitivity = tmp_path / 'sensitivity.json'
    sensitivity.write_text('{"status": "completed", "cases": [{"status": "completed"}]}')
    monkeypatch.setattr(report, 'collect_cases', lambda *a, **k: [])
    with pytest.raises(ValueError, match='Pinned sensitivity'):
        report.generate_report(campaign, matrix, tmp_path / 'report.html', {},
                               sensitivity_campaign=sensitivity)
    assert not (tmp_path / 'report.html').exists()


def test_inconsistent_completed_snapshot_is_not_reported_as_completion():
    text = report._campaign_status({'status': 'completed', 'cases': [{'status': 'failed'}]})
    assert 'Inconsistent snapshot' in text and 'failed: 1' in text


def _sensitivity_fixture(tmp_path):
    source = tmp_path / 'source'
    source.mkdir()
    (source / 'master.yml').write_text('model')
    master_sha = sha256((source / 'master.yml').read_bytes()).hexdigest()
    model_sha, request_sha = 'b' * 64, 'c' * 64
    original = source / 'manifest.json'
    original.write_text(json.dumps({'files': [
        {'path': 'master.yml', 'sha256': master_sha},
        {'path': 'matched-pilot/pilot/model.yml', 'sha256': model_sha}]}))
    matrix = tmp_path / 'matched-pilot.json'
    matrix.write_text(json.dumps({'cases': [{'id': 'pilot', 'model_sha256': model_sha}],
        'source_manifest': 'source/manifest.json',
        'source_manifest_sha256': sha256(original.read_bytes()).hexdigest()}))
    matrix_sha = sha256(matrix.read_bytes()).hexdigest()
    expected = [dict(id='pilot', model_sha256=model_sha, request_sha256=request_sha)]
    campaign = tmp_path / 'campaign.json'
    campaign.write_text(json.dumps({'status': 'completed', 'cases': expected,
        'pinned_inputs': {'manifest_sha256': matrix_sha, 'artifact_manifest': 'artifact.json',
                          'artifact_manifest_sha256': 'a' * 64}}))
    return campaign, matrix, expected


def test_sensitivity_binds_own_matrix_and_source_master(monkeypatch, tmp_path):
    campaign, matrix, expected = _sensitivity_fixture(tmp_path)
    from digitalmodel.workflows import installation_priority_requests
    monkeypatch.setattr(installation_priority_requests, '_inputs', lambda *a: expected)
    result = report._capture_sensitivity(campaign, matrix,
        sha256(campaign.read_bytes()).hexdigest(), sha256(matrix.read_bytes()).hexdigest())
    assert result['matrix_sha256'] == sha256(matrix.read_bytes()).hexdigest()
    assert result['source_master_sha256'] == sha256(b'model').hexdigest()
    assert 'captured_utc' not in result and 'read_utc' in result
    assert 'read ' in report._campaign_status(result)


@pytest.mark.parametrize('defect', ['campaign_hash', 'matrix_hash', 'case_model', 'source_master'])
def test_sensitivity_rejects_unbound_snapshot(monkeypatch, tmp_path, defect):
    campaign, matrix, expected = _sensitivity_fixture(tmp_path)
    from digitalmodel.workflows import installation_priority_requests
    monkeypatch.setattr(installation_priority_requests, '_inputs', lambda *a: expected)
    if defect == 'case_model':
        data = json.loads(campaign.read_bytes())
        data['cases'][0]['model_sha256'] = 'd' * 64
        campaign.write_text(json.dumps(data))
    if defect == 'source_master': (tmp_path / 'source/master.yml').write_text('changed')
    campaign_sha = '0' * 64 if defect == 'campaign_hash' else sha256(campaign.read_bytes()).hexdigest()
    matrix_sha = '0' * 64 if defect == 'matrix_hash' else sha256(matrix.read_bytes()).hexdigest()
    with pytest.raises(ValueError):
        report._capture_sensitivity(campaign, matrix, campaign_sha, matrix_sha)
