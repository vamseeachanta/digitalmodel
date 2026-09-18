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
