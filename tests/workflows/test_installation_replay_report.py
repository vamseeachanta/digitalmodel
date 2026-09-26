import copy
import json

import pytest

from digitalmodel.workflows import installation_replay_report as reports


@pytest.mark.parametrize('field', ['master', 'matrix'])
def test_reference_bound_to_same_master_and_matrix(field):
    fresh = {'campaign_snapshot': {'master_sha256': 'master'}, 'matrix_sha256': 'matrix'}
    reference = copy.deepcopy(fresh)
    if field == 'master':
        reference['campaign_snapshot']['master_sha256'] = 'different'
    else:
        reference['matrix_sha256'] = 'different'
    with pytest.raises(ValueError, match='master|matrix'):
        reports._reference_identity(fresh, reference)


def test_campaign_digest_does_not_depend_on_output_path():
    row = {'index': 97, 'hs_m': 2, 'tp_s': 10, 'seed': 1, 'run_dir': 'first', 'generation_file': 'first'}
    matrix = {'master_sha256': 'master', 'cases': [{}] * 97 + [{'change_sha256': 'change'}]}
    original = reports._campaign_identity(row, matrix)
    row.update(run_dir='different', generation_file='different')
    assert reports._campaign_identity(row, matrix) == original


def test_failed_comparison_retains_fresh_summary(tmp_path, monkeypatch):
    row = {'index': 97, 'status': 'VERIFIED', 'hs_m': 2, 'tp_s': 10, 'seed': 1,
           'heading_degrees': 0, 'settings': {},
           'channels': {'load': {'units': 'kN', 'minimum': 1, 'maximum': 2}}}
    fresh = {'cases': [row], 'campaign_snapshot': {'master_sha256': 'master'}, 'matrix_sha256': 'matrix'}
    reference = copy.deepcopy(fresh)
    reference['cases'][0]['channels']['load']['maximum'] = 3
    path = tmp_path / 'reference.json'
    path.write_text(json.dumps(reference))
    monkeypatch.setattr(reports, '_summary', lambda *args: fresh)
    with pytest.raises(ValueError, match='tolerances'):
        reports.build_reports(tmp_path, {'reference_summary': path}, {},
                              {'case_index': 97, 'comparison': {'absolute_tolerance': 0, 'relative_tolerance': 0}})
    assert json.loads((tmp_path / 'summary.json').read_bytes()) == fresh
    assert not json.loads((tmp_path / 'comparison.json').read_bytes())['passed']


def test_successful_replay_renders_inherited_findings_as_historical(tmp_path, monkeypatch):
    from pypdf import PdfReader
    row = {'index': 0, 'status': 'VERIFIED', 'hs_m': 2, 'tp_s': 10, 'seed': 1, 'heading_degrees': 0,
           'settings': {'duration_s': 600}, 'peak_tension_kN': 100.0,
           'channels': {'load': {'units': 'kN', 'minimum': 1, 'maximum': 2}}}
    fresh = {'cases': [row], 'counts': {'VERIFIED': 1}, 'envelopes': [],
             'campaign_snapshot': {'master_sha256': 'master'}, 'matrix_sha256': 'matrix',
             'engineering_acceptance': 'NOT EVALUATED'}
    frozen = {}
    for name, value in [('reference_summary', copy.deepcopy(fresh)),
                        ('demo_config', {'scenarios': [{'hs_m': 2, 'tp_s': 10}], 'snapshot': {}}),
                        ('criteria', {'checks': []}),
                        ('report_config', {'summary_findings': ['Campaign peak 151 kN'], 'decisions': ['D'],
                                           'supplements': ['S'], 'disclosures': ['X']})]:
        frozen[name] = tmp_path / f'{name}.json'
        frozen[name].write_text(json.dumps(value))
    for name in ('run/installation_traces/metadata.json', 'run/installation_traces/traces.npz',
                 'run/run.json', 'prepared/model.yml'):
        (tmp_path / name).parent.mkdir(parents=True, exist_ok=True)
        (tmp_path / name).write_text('x')
    channel = dict(id='load', label='Load', units='kN', assumed_limit=None,
                   history=dict(times=[240, 360], values=[0, 1]),
                   forecast=dict(times=[361, 480], values=[1, 0]), fit_status='fitted')
    payload = dict(created_utc='now', criteria=[], limitations=[],
                   cases=[dict(index=0, hs_m=2, tp_s=10, status='NOT_EVALUATED', checks=[])],
                   demo=dict(scenarios=[dict(case_index=0, hs_m=2, tp_s=10, source_label='SIMULATED',
                                             frames=[dict(now_s=360, channels=[channel])])]))
    monkeypatch.setattr(reports, '_summary', lambda *args: fresh)
    monkeypatch.setattr(reports, 'prepare_payload', lambda *args: copy.deepcopy(payload))
    def fake_html(summary, payload_path, output, config_path):
        output.write_text('html'); output.with_suffix('.json').write_text('{}')
    monkeypatch.setattr(reports, 'generate_report', fake_html)
    reports.build_reports(tmp_path, frozen, {}, {'case_index': 0, 'pilot_limitations': [],
                          'comparison': {'absolute_tolerance': 0, 'relative_tolerance': 0}})
    prefix = 'Historical full-campaign context; not fresh pilot findings: '
    config = json.loads((tmp_path / 'pilot-config.json').read_text())
    assert config['summary_findings'] == [prefix + 'Campaign peak 151 kN']
    assert config['decisions'] == [prefix + 'D'] and config['supplements'] == [prefix + 'S']
    text = ' '.join(' '.join(p.extract_text() for p in PdfReader(tmp_path / 'pilot-report.pdf').pages).split())
    assert prefix + 'Campaign peak 151 kN' in text
    assert 'Campaign peak 151 kN' not in text.replace(prefix + 'Campaign peak 151 kN', '')
