"""Synthetic compositor proof and event-index regression tests; no solver calls."""
import copy
from hashlib import sha256
import json

import pytest

from digitalmodel.workflows.installation_composite_summary import build_composite
from tests.workflows.test_mudmat_sensitivity_reporting import _summaries, _sub


def _save(path, value):
    path.write_text(json.dumps(value), encoding='utf-8')


def _compose(paths, sub=None):
    return build_composite(paths['base'], sha256(paths['base'].read_bytes()).hexdigest(),
                           [sub or _sub(paths)])


def _failed_base(paths):
    base = json.loads(paths['base'].read_bytes())
    base['cases'][1].pop('settings')
    matrix = dict(settings={k: v for k, v in base['cases'][0]['settings'].items() if k not in ('hs_m', 'tp_s', 'seed')},
                  cases=[dict(hs_m=1., tp_s=8, seed=7), dict(hs_m=1., tp_s=10, seed=7)])
    path = paths['base'].with_name('matrix.json')
    _save(path, matrix)
    digest = sha256(path.read_bytes()).hexdigest()
    base['matrix_sha256'] = base['campaign_snapshot']['matrix_sha256'] = digest
    variant = json.loads(paths['variant'].read_bytes())
    variant['source_matrix_sha256'] = digest
    _save(paths['base'], base)
    _save(paths['variant'], variant)
    return path


@pytest.mark.parametrize('field', ['duration_s', 'gamma', 'sample_interval_s'])
def test_failed_row_uses_pinned_base_settings(tmp_path, field):
    paths = _summaries(tmp_path)
    matrix = _failed_base(paths)
    other = json.loads(paths['supplement'].read_bytes())
    other['cases'][1]['settings'][field] = 900
    _save(paths['supplement'], other)
    with pytest.raises(ValueError, match='settings'):
        _compose(paths, dict(_sub(paths), base_matrix=matrix))


@pytest.mark.parametrize('target', ['source_matrix_sha256', 'matrix_sha256'])
def test_variant_matrix_hash_is_bound_to_summary(tmp_path, target):
    paths = _summaries(tmp_path)
    variant = json.loads(paths['variant'].read_bytes())
    variant[target] = 'wrong-matrix'
    _save(paths['variant'], variant)
    with pytest.raises(ValueError, match='matrix'):
        _compose(paths)


def test_failed_row_requires_frozen_settings_evidence(tmp_path):
    paths = _summaries(tmp_path)
    _failed_base(paths)
    with pytest.raises(ValueError, match='base_matrix'):
        _compose(paths)


@pytest.mark.parametrize('field', ['tp_s', 'run_dir'])
def test_selected_campaign_row_matches_audited_result(tmp_path, field):
    paths = _summaries(tmp_path)
    other = json.loads(paths['supplement'].read_bytes())
    other['campaign_snapshot']['cases'][1][field] = 'different'
    _save(paths['supplement'], other)
    with pytest.raises(ValueError, match='campaign row'):
        _compose(paths)


@pytest.mark.parametrize('defect', ['missing', 'duplicate', 'failed', 'trace', 'metadata', 'errors'])
def test_supplemental_event_audit_must_bind_result(tmp_path, defect):
    paths = _summaries(tmp_path)
    other = json.loads(paths['supplement'].read_bytes())
    audit = other['event_audits'][0]
    if defect == 'missing': other['event_audits'] = []
    if defect == 'duplicate': other['event_audits'].append(copy.deepcopy(audit))
    if defect == 'failed': audit['status'] = 'FAILED'
    if defect == 'trace': audit['trace_sha256'] = 'old-trace'
    if defect == 'metadata': audit['metadata_sha256'] = 'old-metadata'
    if defect == 'errors': audit['errors'] = ['unverified channel']
    _save(paths['supplement'], other)
    with pytest.raises(ValueError, match='audit'):
        _compose(paths)


def test_failed_substitution_adds_event_audit(tmp_path):
    paths = _summaries(tmp_path)
    result = _compose(paths)
    assert [r['index'] for r in result['event_audits']] == [0, 1]
    assert result['event_audits'][1]['trace_sha256'] == result['cases'][1]['trace_sha256']


def test_verified_substitution_replaces_event_audit(tmp_path):
    paths = _summaries(tmp_path)
    base = json.loads(paths['base'].read_bytes())
    base['cases'][1]['status'] = 'VERIFIED'
    base['event_audits'].append(dict(index=1, status='VERIFIED', trace_sha256='old', metadata_sha256='old'))
    _save(paths['base'], base)
    result = _compose(paths)
    selected = [r for r in result['event_audits'] if r['index'] == 1]
    assert len(selected) == 1 and selected[0]['trace_sha256'] == 'b'


def test_seven_failed_cells_use_frozen_base_and_gain_audits(tmp_path):
    paths = _summaries(tmp_path)
    base = json.loads(paths['base'].read_bytes())
    other = json.loads(paths['supplement'].read_bytes())
    base['cases'] = [dict(index=i, status='FAILED', hs_m=1., tp_s=10, seed=0) for i in range(7)]
    other['cases'] = [dict(copy.deepcopy(other['cases'][1]), index=i, seed=0) for i in range(7)]
    for row in other['cases']:
        row['settings']['seed'] = 0
    for data, status in ((base, 'FAILED'), (other, 'COMPLETED')):
        data['campaign_snapshot']['cases'] = [dict(index=i, hs_m=1., tp_s=10, seed=0,
                                                  run_dir='quarter', status=status) for i in range(7)]
    base['event_audits'] = []
    other['event_audits'] = [dict(copy.deepcopy(other['event_audits'][0]), index=i) for i in range(7)]
    matrix = paths['base'].with_name('frozen-matrix.json')
    common = {k: v for k, v in other['cases'][0]['settings'].items() if k not in ('hs_m', 'tp_s', 'seed')}
    common['fixed_time_step_s'] = .05
    _save(matrix, dict(settings=common,
                       cases=[dict(hs_m=1., tp_s=10, seed=0) for i in range(7)]))
    digest = sha256(matrix.read_bytes()).hexdigest()
    base['matrix_sha256'] = base['campaign_snapshot']['matrix_sha256'] = digest
    variant = json.loads(paths['variant'].read_bytes())
    variant['source_matrix_sha256'] = digest
    for key, value in (('base', base), ('supplement', other), ('variant', variant)):
        _save(paths[key], value)
    subs = [dict(_sub(paths), index=i, base_matrix=matrix) for i in range(7)]
    result = build_composite(paths['base'], sha256(paths['base'].read_bytes()).hexdigest(), subs)
    assert result['counts'] == {'VERIFIED': 7}
    assert [r['index'] for r in result['event_audits']] == list(range(7))
    assert len(result['composite']['substitutions']) == 7
    assert json.loads(paths['base'].read_bytes()) == base


@pytest.mark.parametrize('defect', ['emptied', 'trace', 'metadata', 'duplicate', 'failed'])
def test_every_verified_case_needs_one_bound_audit_after_substitution(tmp_path, defect):
    """Base audits for cells that were not substituted are checked as strictly as substituted ones."""
    paths = _summaries(tmp_path)
    base = json.loads(paths['base'].read_bytes())
    audit = base['event_audits'][0]
    if defect == 'emptied': base['event_audits'] = []
    if defect == 'trace': audit['trace_sha256'] = 'old-trace'
    if defect == 'metadata': audit['metadata_sha256'] = 'old-metadata'
    if defect == 'duplicate': base['event_audits'].append(copy.deepcopy(audit))
    if defect == 'failed': audit['status'] = 'FAILED'
    _save(paths['base'], base)
    with pytest.raises(ValueError, match='audit'):
        _compose(paths)


@pytest.mark.parametrize('defect', ['missing_both', 'missing_variant', 'nan', 'boolean'])
def test_time_step_before_proof_requires_finite_number(tmp_path, defect):
    """None == None (or a non-number) must not satisfy the variant time-step proof."""
    paths = _summaries(tmp_path)
    variant = json.loads(paths['variant'].read_bytes())
    delta = variant['master_deltas']['General.ImplicitConstantTimeStep']
    if defect in ('missing_both', 'missing_variant'):
        delta.pop('before')
    if defect == 'missing_both':
        base = json.loads(paths['base'].read_bytes())
        base['cases'][1]['settings'].pop('fixed_time_step_s')
        _save(paths['base'], base)
    if defect == 'nan':
        delta['before'] = float('nan')
        base = json.loads(paths['base'].read_bytes())
        base['cases'][1]['settings']['fixed_time_step_s'] = float('nan')
        _save(paths['base'], base)
    if defect == 'boolean':
        delta['before'] = True
        base = json.loads(paths['base'].read_bytes())
        base['cases'][1]['settings']['fixed_time_step_s'] = True
        _save(paths['base'], base)
    _save(paths['variant'], variant)
    with pytest.raises(ValueError, match='time-step'):
        _compose(paths)


def test_supplemental_audit_must_cover_every_channel(tmp_path):
    paths = _summaries(tmp_path)
    other = json.loads(paths['supplement'].read_bytes())
    other['event_audits'][0]['channels_verified'] = len(other['cases'][1]['channels']) + 15
    _save(paths['supplement'], other)
    with pytest.raises(ValueError, match='audit'):
        _compose(paths)


def test_missing_snapshot_row_raises_value_error(tmp_path):
    paths = _summaries(tmp_path)
    base = json.loads(paths['base'].read_bytes())
    base['campaign_snapshot']['cases'] = [r for r in base['campaign_snapshot']['cases'] if r['index'] != 1]
    _save(paths['base'], base)
    with pytest.raises(ValueError, match='campaign snapshot'):
        _compose(paths)


@pytest.mark.parametrize('target', ['base', 'supplement'])
def test_campaign_matrix_hash_must_match_summary(tmp_path, target):
    paths = _summaries(tmp_path)
    data = json.loads(paths[target].read_bytes())
    data['campaign_snapshot']['matrix_sha256'] = 'different'
    _save(paths[target], data)
    with pytest.raises(ValueError, match='matrix'):
        _compose(paths)


def test_failed_row_rejects_tampered_frozen_matrix(tmp_path):
    paths = _summaries(tmp_path)
    matrix = _failed_base(paths)
    matrix.write_text('{}', encoding='utf-8')
    with pytest.raises(ValueError, match='Digest mismatch'):
        _compose(paths, dict(_sub(paths), base_matrix=matrix))


@pytest.mark.parametrize('defect', ['missing_frozen_duration', 'missing_replacement_coordinates'])
def test_frozen_settings_do_not_infer_equivalence(tmp_path, defect):
    paths = _summaries(tmp_path)
    matrix = _failed_base(paths)
    if defect == 'missing_frozen_duration':
        data = json.loads(matrix.read_bytes())
        data['settings'].pop('duration_s')
        _save(matrix, data)
        digest = sha256(matrix.read_bytes()).hexdigest()
        base = json.loads(paths['base'].read_bytes())
        base['matrix_sha256'] = base['campaign_snapshot']['matrix_sha256'] = digest
        variant = json.loads(paths['variant'].read_bytes())
        variant['source_matrix_sha256'] = digest
        _save(paths['base'], base)
        _save(paths['variant'], variant)
    else:
        other = json.loads(paths['supplement'].read_bytes())
        for key in ('hs_m', 'tp_s', 'seed'):
            other['cases'][1]['settings'].pop(key)
        _save(paths['supplement'], other)
    with pytest.raises(ValueError, match='settings'):
        _compose(paths, dict(_sub(paths), base_matrix=matrix))
