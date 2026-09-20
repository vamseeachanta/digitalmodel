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
