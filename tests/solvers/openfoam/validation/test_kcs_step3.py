# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Analytical, offline Case 2.10 controls; no solver or external data access."""
import copy
import json
import math
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.validation import kcs_step3 as qa

ROOT = Path(__file__).resolve().parents[4]
FIXTURE = ROOT / 'tests/fixtures/test_vectors/marine/kcs_head_wave_efd.json'
POLICY = ROOT / 'config/cfd/kcs_step3_qa.json'


def inputs():
    reference = json.loads(FIXTURE.read_text())
    policy = json.loads(POLICY.read_text())
    condition = reference['reference_rows'][1]
    rows = []
    # Exactly 15 encounter periods, 32 samples per period, no duplicate endpoint.
    # Every harmonic is nonzero; mean=1, amplitude=0.1/n, phase=0.2*n.
    for i in range(480):
        t = i / 32
        wave_phase = 0.4
        row = {'time_over_Te': t}
        for quantity in qa.SIGNALS:
            row[quantity] = 1 + sum(0.1 / n * math.cos(2 * math.pi * n * t + n * wave_phase + 0.2 * n) for n in range(1, 5))
        row['wave_over_lpp'] = 0.1 * math.cos(2 * math.pi * t + wave_phase)
        rows.append(row)
    for key, cell in condition['responses'].items():
        if key == 'wave_over_lpp_a1':
            value = 0.1
        elif '_phase' in key:
            value = 0.2 * int(key[-1])
        else:
            n = int(key[-1])
            value = 2.0 if n == 0 else 0.1 / n
        cell.update(value=value, status='verified', sheet='synthetic worksheet', cell=key)
    condition['readback'] = {'status': 'synthetic', 'source_sha256': None, 'table_sha256': qa.response_table_digest(condition)}
    meta = {'mesh': 'coarse', 'evidence_kind': 'synthetic', 'condition_id': condition['id'],
            'reference_sha256': qa.digest(condition), 'criteria_sha256': qa.digest(policy),
            'conditions': condition['conditions'], 'units': qa.UNITS,
            'conventions': reference['conventions']}
    return rows, meta, condition, policy


def test_reference_has_six_conditions_and_no_invented_experimental_responses():
    reference = qa.load_reference(FIXTURE)
    assert len(reference['reference_rows']) == 6
    for condition in reference['reference_rows']:
        assert all(c['value'] is None and c['status'] == 'NOT_EVALUATED' for c in condition['responses'].values())
        assert not qa.reference_is_qualified(condition)
        for cell in condition['responses'].values():
            cell = {**condition.get('response_provenance', {}), **cell}
            assert cell['source_document'] and cell['page'] and cell['published_data_file']
            assert cell['units'] and cell['licence_note'] and cell['froude'] == .261


def test_analytical_fourier_conventions_and_synthetic_nonqualification():
    rows, meta, condition, policy = inputs()
    report = qa.compare_rows(rows, meta, condition, policy)
    assert report['comparison_passed'] is None
    assert report['coarse_smoke_passed'] is None
    assert report['status'] == 'PENDING_THRESHOLD_DECISION'
    assert all(q['passed'] is None for q in report['quantities'].values())
    assert all(q['status'] == 'PENDING_THRESHOLD_DECISION' for q in report['quantities'].values())
    assert report['history_fit']['passed'] is None
    assert not report['qualified']
    for quantity in qa.SIGNALS:
        assert report['quantities'][quantity + '_a0']['computed'] == pytest.approx(2)
        assert report['quantities'][quantity + '_phase3']['computed'] == pytest.approx(.6)


@pytest.mark.parametrize('problem', ['missing', 'duplicate', 'nan', 'ragged', 'short', 'irregular', 'drift', 'perturbed'])
def test_bad_history_refuses_or_fails(problem, monkeypatch):
    rows, meta, condition, policy = inputs()
    record_synthetic_decisions(policy, monkeypatch)
    meta['criteria_sha256'] = qa.digest(policy)
    if problem == 'missing':
        del rows[0]['ct_x1000']
    elif problem == 'duplicate':
        rows[2]['time_over_Te'] = rows[1]['time_over_Te']
    elif problem == 'nan':
        rows[0]['ct_x1000'] = float('nan')
    elif problem == 'ragged':
        rows[0]['extra'] = 1
    elif problem == 'short':
        rows = rows[:100]
    elif problem == 'irregular':
        rows[20]['time_over_Te'] += .001
    elif problem == 'drift':
        for row in rows[320:]:
            row['ct_x1000'] += .02
    else:
        for row in rows:
            row['ct_x1000'] += 1
    try:
        report = qa.compare_rows(rows, meta, condition, policy)
    except ValueError:
        return
    assert not report['comparison_passed']


@pytest.mark.parametrize('problem', ['missing_response', 'null_response', 'source_digest', 'wrong_case', 'criteria', 'units', 'conditions'])
def test_incomplete_or_stale_evidence(problem):
    rows, meta, condition, policy = inputs()
    if problem == 'missing_response':
        del condition['responses']['ct_x1000_a1']
        meta['reference_sha256'] = qa.digest(condition)
    elif problem == 'null_response':
        condition['responses']['ct_x1000_a1']['value'] = None
        meta['reference_sha256'] = qa.digest(condition)
    elif problem == 'source_digest':
        meta['reference_sha256'] = '0' * 64
    elif problem == 'wrong_case':
        meta['condition_id'] = 'C0'
    elif problem == 'criteria':
        policy['tolerances']['ct_x1000_a1']['limit'] = 100
        meta['criteria_sha256'] = qa.digest(policy)
    elif problem == 'units':
        meta['units'] = {}
    else:
        meta['conditions'] = {**meta['conditions'], 'froude': .26}
    with pytest.raises(ValueError):
        qa.compare_rows(rows, meta, condition, policy)


def test_zero_wave_condition_refuses_normalized_wave_readback():
    rows, meta, condition, policy = inputs()
    condition['id'] = 'C0'
    meta['condition_id'] = 'C0'
    meta['reference_sha256'] = qa.digest(condition)
    with pytest.raises(ValueError, match='C0'):
        qa.compare_rows(rows, meta, condition, policy)


def test_phase_below_floor_is_not_evaluated_and_cannot_pass(monkeypatch):
    rows, meta, condition, policy = inputs()
    record_synthetic_decisions(policy, monkeypatch)
    meta['criteria_sha256'] = qa.digest(policy)
    for row in rows:
        row['heave_over_a'] = 1
    report = qa.compare_rows(rows, meta, condition, policy)
    assert report['quantities']['heave_over_a_phase1']['status'] == 'NOT_EVALUATED'
    assert not report['comparison_passed']


@pytest.mark.parametrize('mesh', ['medium', 'fine'])
@pytest.mark.parametrize('problem', ['reference', 'criteria', 'smoke'])
def test_step1_dispatch_refuses_missing_step3_prerequisites(mesh, problem, monkeypatch):
    from digitalmodel.solvers.openfoam.validation import kcs_step1
    rows, meta, condition, policy = inputs()
    record_synthetic_decisions(policy, monkeypatch)
    reference = {'reference_rows': [condition]}
    monkeypatch.setattr(qa, 'load_reference', lambda path: reference if problem != 'reference' else (_ for _ in ()).throw(ValueError('reference missing')))
    monkeypatch.setattr(qa, 'reference_is_qualified', lambda row: True)
    monkeypatch.setattr(qa, 'load_policy', lambda path: policy if problem != 'criteria' else (_ for _ in ()).throw(ValueError('criteria missing')))
    with pytest.raises(ValueError, match=problem):
        kcs_step1.require_run_admission(mesh, Path('reference'), Path('criteria'), case='2.10', condition_id='C1')


def test_existing_gci_is_reused_for_known_second_order_series():
    result = qa.amplitude_gci([1, 2, 4], [1.001, 1.004, 1.016])
    assert result['p'] == pytest.approx(2)
    assert result['gci_fine_pct'] == pytest.approx(100 * 1.25 * .001 / 1.001)


@pytest.mark.parametrize('values', [[1, 1, 1], [1, 2, 1], [0, 1, 2]])
def test_gci_rejects_degenerate_or_nonpositive_amplitudes(values):
    with pytest.raises(ValueError):
        qa.amplitude_gci([1, 2, 4], values)


def test_native_readback_and_admission_bind_current_files(tmp_path):
    import hashlib
    rows, meta, condition, policy = inputs()
    condition['readback']['status'] = 'verified'
    condition['readback']['source_sha256'] = 'a' * 64
    meta.update(evidence_kind='native', reference_sha256=qa.digest(condition),
                residuals={'p_rgh': 1e-5, 'U': 1e-6},
                y_plus={'min': .1, 'mean': .5, 'max': 1},
                free_surface={'dx_m': .01, 'dz_m': .001})
    csv = ','.join(qa.UNITS) + '\n' + '\n'.join(','.join(str(r[k]) for k in qa.UNITS) for r in rows) + '\n'
    data = csv.encode()
    meta['history_sha256'] = hashlib.sha256(data).hexdigest()
    history = tmp_path / 'history.csv'
    history.write_bytes(data)
    metadata_path = tmp_path / 'metadata.json'
    metadata_path.write_text(json.dumps(meta))
    reference = qa.load_reference(FIXTURE)
    reference['reference_rows'][1] = condition
    reference_path = tmp_path / 'reference.json'
    reference_path.write_text(json.dumps(reference))
    report = qa.compare_run(history, meta, condition, policy)
    assert report['coarse_smoke_passed'] is None
    assert report['comparison_passed'] is None
    assert report['status'] == 'PENDING_THRESHOLD_DECISION'
    for mesh in ('medium', 'fine'):
        with pytest.raises(ValueError, match='PENDING_THRESHOLD_DECISION'):
            qa.require_run_admission(mesh, reference_path, POLICY, history, metadata_path, condition_id='C1')
    meta['history_sha256'] = '0' * 64
    metadata_path.write_text(json.dumps(meta))
    with pytest.raises(ValueError, match='history'):
        qa.compare_run(history, meta, condition, policy)


@pytest.mark.parametrize('field', ['source_sha256', 'table_sha256', 'status'])
def test_reference_readback_evidence_is_required(field):
    _, _, condition, _ = inputs()
    condition['readback'].update(status='verified', source_sha256='a' * 64)
    assert qa.reference_is_qualified(condition)
    condition['readback'][field] = None
    assert not qa.reference_is_qualified(condition)


def test_three_grid_report_uses_systematic_evidence(monkeypatch):
    _, meta, condition, policy = inputs()
    metas = []
    for mesh, h in [('fine', 1), ('medium', 2), ('coarse', 4)]:
        item = copy.deepcopy(meta)
        item.update(mesh=mesh, h_m=h, mesh_family='analytic', numerics='identical', refinement_axes='xyz', free_surface={'dx_m':h, 'dz_m':h})
        metas.append(item)
    def compare(path, metadata, row, criteria):
        return {'comparison_passed': True, 'quantities': {key: {'computed': 1 + .001 * metadata['h_m'] ** 2} for key in qa.QUANTITIES}}
    monkeypatch.setattr(qa, 'compare_run', compare)
    report = qa.compare_triplet([Path('f'), Path('m'), Path('c')], metas, condition, policy)
    assert report['amplitude_gci_passed'] is None
    assert report['status'] == 'PENDING_THRESHOLD_DECISION'
    assert not report['qualified']
    assert report['gci']['ct_x1000_a1']['p'] == pytest.approx(2)
    metas[-1]['numerics'] = 'different'
    with pytest.raises(ValueError):
        qa.compare_triplet([Path('f'), Path('m'), Path('c')], metas, condition, policy)


def test_wrong_source_conditions_cannot_admit_coarse(monkeypatch):
    _, _, condition, policy = inputs()
    condition['readback'].update(status='verified', source_sha256='a' * 64)
    condition['conditions']['speed_m_s'] = 100
    monkeypatch.setattr(qa, 'load_reference', lambda path: {'reference_rows': [condition]})
    monkeypatch.setattr(qa, 'load_policy', lambda path: policy)
    with pytest.raises(ValueError, match='condition'):
        qa.require_run_admission('coarse', Path('ref'), Path('policy'), condition_id='C1')


@pytest.mark.parametrize('decisions_recorded', [False, True])
def test_growth_outside_retained_harmonics_fails_history_convergence(decisions_recorded, monkeypatch):
    rows, meta, condition, policy = inputs()
    if decisions_recorded:
        record_synthetic_decisions(policy, monkeypatch)
        meta['criteria_sha256'] = qa.digest(policy)
    for row in rows[320:]:
        row['heave_over_a'] += 10 * math.sin(10 * math.pi * row['time_over_Te'])
    report = qa.compare_rows(rows, meta, condition, policy)
    assert not report['comparison_passed']
    assert report['history_fit']['passed'] is (False if decisions_recorded else None)
    assert report['history_fit']['maximum_window_rms']['heave_over_a'] > 1


def test_cli_reports_pending_reference_without_writing_worktree(monkeypatch):
    import importlib.util
    spec = importlib.util.spec_from_file_location('head_wave_cli', ROOT / 'scripts/cfd/readback_kcs_step3.py')
    cli = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(cli)
    captured = {}
    monkeypatch.setattr(Path, 'write_text', lambda self, text, **kwargs: captured.update(report=json.loads(text)))
    rc = cli.main(['admit', '--next-mesh', 'coarse', '--condition', 'C1', '--reference', str(FIXTURE), '--criteria', str(POLICY), '--output', 'unused.json'])
    assert rc == 1
    assert not captured['report']['admitted']
    assert 'reference' in captured['report']['reason']


def test_mixed_condition_reference_cannot_qualify_even_with_updated_digest():
    _, _, condition, _ = inputs()
    condition['readback'].update(status='verified', source_sha256='a' * 64)
    condition['responses']['ct_x1000_a1']['condition_id'] = 'C2'
    condition['responses']['ct_x1000_a1']['wave_conditions'] = 'C2: see conditions'
    condition['readback']['table_sha256'] = qa.response_table_digest(condition)
    assert not qa.reference_is_qualified(condition)


def analytic_triplet(tmp_path, condition, policy, phases='negative'):
    import hashlib
    histories, metadata = [], []
    for mesh, scale in [('fine', 1), ('medium', 2), ('coarse', 4)]:
        rows, meta, _, _ = inputs()
        offset = {1: -.005, 2: .005, 4: 0}[scale]
        for row in rows:
            t = row['time_over_Te']
            for quantity in qa.SIGNALS:
                row[quantity] = 1 + sum(
                    (.1 / n + .00001 * scale ** 2) * math.cos(
                        2 * math.pi * n * t + n * .4
                        + (-.2 * n if phases == 'negative' else .2 * n + offset))
                    for n in range(1, 5))
            row['wave_over_lpp'] = (.1 + .00001 * scale ** 2) * math.cos(2 * math.pi * t + .4)
        data = (','.join(qa.UNITS) + '\n' + '\n'.join(
            ','.join(str(row[k]) for k in qa.UNITS) for row in rows) + '\n').encode()
        history = tmp_path / f'{mesh}.csv'
        history.write_bytes(data)
        meta.update(mesh=mesh, h_m=.001 * scale, mesh_family='analytic', numerics='identical',
                    refinement_axes='xyz', free_surface={'dx_m': .01 * scale, 'dz_m': .001 * scale},
                    residuals={'p_rgh': 1e-5, 'U': 1e-6}, y_plus={'min': .1, 'mean': .5, 'max': 1},
                    reference_sha256=qa.digest(condition), criteria_sha256=qa.digest(policy),
                    history_sha256=hashlib.sha256(data).hexdigest())
        histories.append(history)
        metadata.append(meta)
    return histories, metadata


@pytest.mark.parametrize('phases', ['negative', 'nonmonotone'])
def test_real_triplet_excludes_phases_from_thirteen_amplitude_gcis(tmp_path, phases):
    _, _, condition, policy = inputs()
    if phases == 'negative':
        for key, cell in condition['responses'].items():
            if '_phase' in key:
                cell['value'] = -.2 * int(key[-1])
    histories, metadata = analytic_triplet(tmp_path, condition, policy, phases)
    report = qa.compare_triplet(histories, metadata, condition, policy)
    assert set(report['gci']) == {
        'ct_x1000_a1', 'ct_x1000_a2', 'ct_x1000_a3', 'ct_x1000_a4',
        'heave_over_a_a1', 'heave_over_a_a2', 'heave_over_a_a3', 'heave_over_a_a4',
        'pitch_over_ka_a1', 'pitch_over_ka_a2', 'pitch_over_ka_a3', 'pitch_over_ka_a4',
        'wave_over_lpp_a1',
    }
    assert report['amplitude_gci_passed'] is None
    assert report['status'] == 'PENDING_THRESHOLD_DECISION'
    assert not report['qualified']
    assert report['gci']['ct_x1000_a1']['p'] == pytest.approx(2)


@pytest.mark.parametrize('limits,reason', [
    ({'minimum_order': 2.1}, 'order'),
    ({'maximum_order': 1.9}, 'order'),
])
def test_amplitude_gci_enforces_declared_bounds(limits, reason):
    with pytest.raises(ValueError, match=reason):
        qa.amplitude_gci([1, 2, 4], [1.001, 1.004, 1.016], **limits)


def test_triplet_forwards_all_policy_gci_bounds(tmp_path, monkeypatch):
    _, _, condition, policy = inputs()
    histories, metadata = analytic_triplet(tmp_path, condition, policy, 'nonmonotone')
    original = qa.amplitude_gci

    def enforce_bounds(h, values, **limits):
        assert limits == {'minimum_order': .5, 'maximum_order': 3.,
                          'maximum_asymptotic_deviation': .05}
        return original(h, values, **limits)

    monkeypatch.setattr(qa, 'amplitude_gci', enforce_bounds)
    report = qa.compare_triplet(histories, metadata, condition, policy)
    assert report['amplitude_gci_passed'] is None
    assert len(report['gci']) == 13


def test_shared_response_provenance_preserves_qualification_and_cell_overrides():
    _, _, condition, _ = inputs()
    fields = ('froude', 'wave_conditions', 'source_document', 'source_url', 'page',
              'published_data_file', 'licence_note', 'condition_id')
    cell_provenance = {**condition.get('response_provenance', {}),
                       **condition['responses']['ct_x1000_a1']}
    condition['response_provenance'] = {
        key: cell_provenance[key] for key in fields}
    for cell in condition['responses'].values():
        for key in fields:
            cell.pop(key, None)
    condition['readback'].update(status='verified', source_sha256='a' * 64,
                                 table_sha256=qa.response_table_digest(condition))
    assert qa.reference_is_qualified(condition)
    condition['responses']['ct_x1000_a1']['condition_id'] = 'C2'
    condition['readback']['table_sha256'] = qa.response_table_digest(condition)
    assert not qa.reference_is_qualified(condition)


@pytest.mark.parametrize('field', ['source_url', 'page'])
def test_shared_source_edit_invalidates_verified_readback(field):
    _, _, condition, _ = inputs()
    condition['readback'].update(status='verified', source_sha256='a' * 64)
    assert qa.reference_is_qualified(condition)
    condition['response_provenance'][field] = 'different source locator'
    assert not qa.reference_is_qualified(condition)


@pytest.mark.parametrize('command', ['compare', 'triplet'])
@pytest.mark.parametrize('decisions_recorded', [False, True])
def test_cli_reads_real_evidence_and_writes_verdict(tmp_path, command, decisions_recorded, monkeypatch, capsys):
    import importlib.util
    spec = importlib.util.spec_from_file_location('head_wave_cli', ROOT / 'scripts/cfd/readback_kcs_step3.py')
    cli = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(cli)
    _, _, condition, policy = inputs()
    criteria_path = POLICY
    if decisions_recorded:
        record_synthetic_decisions(policy, monkeypatch)
        criteria_path = tmp_path / 'criteria.json'
        criteria_path.write_text(json.dumps(policy))
    histories, metadata = analytic_triplet(tmp_path, condition, policy, 'nonmonotone')
    reference = qa.load_reference(FIXTURE)
    reference['reference_rows'][1] = condition
    reference_path = tmp_path / 'reference.json'
    reference_path.write_text(json.dumps(reference))
    output = tmp_path / 'report.json'
    args = [command, '--condition', 'C1', '--reference', str(reference_path),
            '--criteria', str(criteria_path), '--output', str(output)]
    count = 3 if command == 'triplet' else 1
    for history, meta in zip(histories[:count], metadata[:count]):
        metadata_path = tmp_path / f'{meta["mesh"]}.json'
        metadata_path.write_text(json.dumps(meta))
        args.extend(['--history', str(history), '--metadata', str(metadata_path)])
    assert cli.main(args) == (0 if decisions_recorded else 1)
    assert capsys.readouterr().out.strip() == ('PASS' if decisions_recorded else 'PENDING_THRESHOLD_DECISION')
    report = json.loads(output.read_text())
    assert report['amplitude_gci_passed' if command == 'triplet' else 'comparison_passed'] is (True if decisions_recorded else None)
    if not decisions_recorded:
        assert report['status'] == 'PENDING_THRESHOLD_DECISION'
    assert not report['qualified']


def record_synthetic_decisions(policy, monkeypatch):
    """Model a separate policy revision for analytical controls only."""
    for group in policy['threshold_basis'].values():
        if 'decision' in group:
            group['status'] = 'recorded'
            group['decision'] = '2000-01-01: synthetic decision for analytical controls only'
    monkeypatch.setattr(qa, 'POLICY_SHA256', qa.digest(policy))


@pytest.mark.parametrize('pending_group', [
    None, 'harmonic_comparison', 'window_drift', 'sampling', 'windows',
    'phase_amplitude_floor', 'fit_residual',
])
def test_each_threshold_decision_is_required_for_comparison(pending_group, monkeypatch):
    rows, meta, condition, policy = inputs()
    record_synthetic_decisions(policy, monkeypatch)
    if pending_group is not None:
        policy['threshold_basis'][pending_group]['decision'] = None
        monkeypatch.setattr(qa, 'POLICY_SHA256', qa.digest(policy))
    meta['criteria_sha256'] = qa.digest(policy)
    report = qa.compare_rows(rows, meta, condition, policy)
    expected = True if pending_group is None else None
    assert report['comparison_passed'] is expected
    assert report['history_fit']['passed'] is expected
    assert all(q['passed'] is expected for q in report['quantities'].values())
    if pending_group is not None:
        assert report['status'] == 'PENDING_THRESHOLD_DECISION'
    for row in rows:
        row['ct_x1000'] += 1
    failed = qa.compare_rows(rows, meta, condition, policy)
    assert failed['comparison_passed'] is (False if pending_group is None else None)
    assert failed['quantities']['ct_x1000_a0']['absolute_error'] == pytest.approx(2)


def test_recorded_decisions_require_a_reviewed_policy_pin():
    rows, meta, condition, policy = inputs()
    policy['threshold_basis']['harmonic_comparison']['decision'] = '2000-01-01: synthetic control'
    meta['criteria_sha256'] = qa.digest(policy)
    with pytest.raises(ValueError, match='reviewed revision'):
        qa.compare_rows(rows, meta, condition, policy)


def test_recorded_decisions_restore_file_verdicts_and_admission(tmp_path, monkeypatch):
    _, _, condition, policy = inputs()
    record_synthetic_decisions(policy, monkeypatch)
    condition['readback'].update(status='verified', source_sha256='a' * 64)
    histories, metadata = analytic_triplet(tmp_path, condition, policy, 'nonmonotone')
    for meta in metadata:
        meta['evidence_kind'] = 'native'
    reference = qa.load_reference(FIXTURE)
    reference['reference_rows'][1] = condition
    reference_path, criteria_path = tmp_path / 'reference.json', tmp_path / 'criteria.json'
    reference_path.write_text(json.dumps(reference))
    criteria_path.write_text(json.dumps(policy))
    metadata_path = tmp_path / 'coarse.json'
    metadata_path.write_text(json.dumps(metadata[-1]))
    report = qa.compare_run(histories[-1], metadata[-1], condition, policy)
    assert report['comparison_passed'] is True
    assert report['coarse_smoke_passed'] is True
    assert qa.compare_triplet(histories, metadata, condition, policy)['amplitude_gci_passed'] is True
    for mesh in ('medium', 'fine'):
        assert qa.require_run_admission(mesh, reference_path, criteria_path, histories[-1],
                                        metadata_path, condition_id='C1')['admitted'] is True
    metadata[-1]['history_sha256'] = '0' * 64
    metadata_path.write_text(json.dumps(metadata[-1]))
    with pytest.raises(ValueError, match='history'):
        qa.require_run_admission('medium', reference_path, criteria_path, histories[-1],
                                 metadata_path, condition_id='C1')


@pytest.mark.parametrize('status,decision', [
    ('recorded', None),
    ('recorded', ''),
    ('recorded', '   '),
    ('recorded', 'TBD'),
    ('recorded', 'synthetic decision without a date'),
    ('recorded', '2000-02-30: invalid calendar date'),
    ('recorded', '2000-01-01'),
    ('recorded', '2000-01-01:   '),
    ('recorded', 1),
    (None, '2000-01-01: synthetic decision'),
    ('', '2000-01-01: synthetic decision'),
    ('assumed_pending_maintainer_decision', '2000-01-01: synthetic decision'),
])
def test_incomplete_decisions_keep_comparison_and_admission_pending(
        status, decision, tmp_path, monkeypatch):
    rows, meta, condition, policy = inputs()
    record_synthetic_decisions(policy, monkeypatch)
    group = policy['threshold_basis']['harmonic_comparison']
    for key, value in (('status', status), ('decision', decision)):
        if value is None:
            group.pop(key)
        else:
            group[key] = value
    monkeypatch.setattr(qa, 'POLICY_SHA256', qa.digest(policy))
    meta['criteria_sha256'] = qa.digest(policy)
    report = qa.compare_rows(rows, meta, condition, policy)
    assert report['comparison_passed'] is None
    assert report['status'] == 'PENDING_THRESHOLD_DECISION'
    condition['readback'].update(status='verified', source_sha256='a' * 64)
    reference = qa.load_reference(FIXTURE)
    reference['reference_rows'][1] = condition
    reference_path, criteria_path = tmp_path / 'reference.json', tmp_path / 'criteria.json'
    reference_path.write_text(json.dumps(reference))
    criteria_path.write_text(json.dumps(policy))
    for mesh in ('medium', 'fine'):
        with pytest.raises(ValueError, match='PENDING_THRESHOLD_DECISION'):
            qa.require_run_admission(mesh, reference_path, criteria_path, condition_id='C1')


@pytest.mark.parametrize('decisions_recorded', [False, True])
@pytest.mark.parametrize('grid', [0, 1, 2])
@pytest.mark.parametrize('diagnostic', ['p_rgh', 'U', 'y_plus', 'dx_m', 'dz_m'])
def test_inherited_diagnostic_failures_block_comparison_and_triplet(
        tmp_path, decisions_recorded, grid, diagnostic, monkeypatch):
    _, _, condition, policy = inputs()
    if decisions_recorded:
        record_synthetic_decisions(policy, monkeypatch)
    histories, metadata = analytic_triplet(tmp_path, condition, policy, 'nonmonotone')
    meta = metadata[grid]
    if diagnostic in ('p_rgh', 'U'):
        meta['residuals'][diagnostic] = 2 * policy['residual_limits'][diagnostic]
    elif diagnostic == 'y_plus':
        meta['y_plus']['max'] = 2 * policy['mesh_limits']['maximum_y_plus']
    else:
        limit = policy['mesh_limits']['maximum_d' + diagnostic[1] + '_over_lpp']
        meta['free_surface'][diagnostic] = 2 * limit * condition['conditions']['lpp_m']
    report = qa.compare_run(histories[grid], meta, condition, policy)
    assert report['diagnostics']['passed'] is False
    assert report['comparison_passed'] is False
    assert report['coarse_smoke_passed'] is False
    if not decisions_recorded:
        assert report['status'] == 'PENDING_THRESHOLD_DECISION'
        assert report['history_fit']['passed'] is None
        assert all(q['passed'] is None for q in report['quantities'].values())
    # Keep refinement systematic so the grid verdict is the failing prerequisite.
    for item in metadata:
        for key in ('dx_m', 'dz_m'):
            item['free_surface'][key] = meta['free_surface'][key] * item['h_m'] / meta['h_m']
    with pytest.raises(ValueError, match='every grid requires passing comparison and convergence'):
        qa.compare_triplet(histories, metadata, condition, policy)


@pytest.mark.parametrize('command', ['compare', 'triplet'])
def test_cli_prints_fail_for_inherited_failure_while_thresholds_pending(tmp_path, command, capsys):
    import importlib.util
    spec = importlib.util.spec_from_file_location('head_wave_cli', ROOT / 'scripts/cfd/readback_kcs_step3.py')
    cli = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(cli)
    _, _, condition, policy = inputs()
    histories, metadata = analytic_triplet(tmp_path, condition, policy, 'nonmonotone')
    metadata[0]['residuals']['U'] = 2 * policy['residual_limits']['U']
    reference = qa.load_reference(FIXTURE)
    reference['reference_rows'][1] = condition
    reference_path, output = tmp_path / 'reference.json', tmp_path / 'report.json'
    reference_path.write_text(json.dumps(reference))
    args = [command, '--condition', 'C1', '--reference', str(reference_path),
            '--criteria', str(POLICY), '--output', str(output)]
    count = 3 if command == 'triplet' else 1
    for history, meta in zip(histories[:count], metadata[:count]):
        metadata_path = tmp_path / f'{meta["mesh"]}.json'
        metadata_path.write_text(json.dumps(meta))
        args.extend(['--history', str(history), '--metadata', str(metadata_path)])
    assert cli.main(args) == 1
    assert capsys.readouterr().out.strip() == 'FAIL'
    report = json.loads(output.read_text())
    if command == 'compare':
        assert report['comparison_passed'] is False
    else:
        assert 'every grid requires passing comparison and convergence' in report['reason']
