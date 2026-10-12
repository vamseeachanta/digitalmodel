"""Synthetic refusal tests plus mandatory-on-publication private N4 regression."""
from copy import deepcopy
import os
from pathlib import Path

import pytest

from digitalmodel.ansys import cylinder_refinement_replay as replay
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes, parse_json
from digitalmodel.ansys.cylinder_criteria import EXPECTED_KEYS, evaluate_attempt
from digitalmodel.ansys.cylinder_results_validation import REQUIRED


@pytest.fixture
def synthetic(tmp_path, monkeypatch):
    """Fake bytes and validator, never native evidence or engineering results."""
    dataset, runtime = tmp_path / 'dataset', tmp_path / 'runtime'
    dataset.mkdir(); runtime.mkdir(); (runtime / 'prepared').mkdir()
    values = {key: '0' for key in EXPECTED_KEYS}
    case = dict(case_id='ocv-t60-p10-n4', pressure_mpa='10')
    reference = dict(rows=[dict(station_id=s, pressure_mpa='10',
        values={q: v for (sid, q), v in values.items() if sid == s})
        for s in sorted({key[0] for key in values})])
    case_raw, ref_raw = canonical_bytes(case), canonical_bytes(reference)
    (runtime / 'prepared/ocv-t60-p10-n4.json').write_bytes(case_raw)
    (runtime / 'reference.json').write_bytes(ref_raw)
    artifacts = [dict(path=p, sha256=digest_bytes(b), bytes=len(b)) for p, b in
        [('prepared/ocv-t60-p10-n4.json', case_raw), ('reference.json', ref_raw)]]
    runtime_raw = canonical_bytes(dict(artifacts=artifacts))
    (runtime / 'manifest.json').write_bytes(runtime_raw)
    source = 'src/digitalmodel/ansys/cylinder_criteria.py'
    root = Path(replay.__file__).resolve().parents[3]
    evidence = {}
    for role in REQUIRED:
        raw = b'' if role in ('stdout', 'stderr') else b'synthetic ' + role.encode()
        (dataset / role).write_bytes(raw)
        evidence[role] = dict(owner_relative_path=role, sha256=digest_bytes(raw),
            expected_sha256=digest_bytes(raw), bytes=len(raw), verified=True)
    recovered = dict(status='COMPLETE', errors=[], values=values, rfy_sum='0')
    assessment = evaluate_attempt(case['case_id'], values, values, '0', [])
    rows = [dict(station_id=s, quantity=q, value=v,
        unit='mm' if q.startswith('u_') else 'MPa',
        origin='derived_von_mises' if q == 'sigma_vm' else 'native_export')
        for (s, q), v in sorted(values.items())]
    report = dict(schema='ansys-diagnostic-recovery-observation-1', case_id=case['case_id'],
        run_id='synthetic-only', source_evidence=evidence,
        source_files={source: digest_bytes((root / source).read_bytes())},
        runtime_sha256=digest_bytes(runtime_raw), case_metadata_sha256=digest_bytes(case_raw),
        reference_sha256=digest_bytes(ref_raw), recovery_status='COMPLETE', recovery_errors=[],
        values=rows, support_rfy_sum=dict(value='0', unit='N'), assessment=assessment,
        engineering_qualified=False, limits=['Synthetic only'], matrix_adoption='NOT_ADOPTED')
    observation = tmp_path / 'observation.json'
    def repin():
        raw = canonical_bytes(report); observation.write_bytes(raw)
        monkeypatch.setattr(replay, 'OBSERVATION_SHA256', digest_bytes(raw))
    repin()
    monkeypatch.setattr(replay, 'validate_native_evidence', lambda *a: deepcopy(recovered))
    return dict(dataset=dataset, runtime=runtime, observation=observation,
                report=report, recovered=recovered, repin=repin)


def run(fixture):
    return replay.replay_n4_predecessor(fixture['dataset'], fixture['runtime'], fixture['observation'])


def test_synthetic_contract_does_not_grant_admission(synthetic):
    result = run(synthetic)
    assert result['replay_status'] == 'COMPLETE'
    assert result['native_admission'] == 'NOT_EVALUATED'
    assert result['engineering_qualified'] is False
    assert result['native_launches'] == 0
    assert result['assessment'] == synthetic['report']['assessment']
    assert result['values'] == synthetic['report']['values']
    assert result['non_discriminating_roles'] == ['stderr', 'stdout']
    assert result['failed_checks'] == []
    assert result['historical_limits'] == ['Synthetic only']
    assert result['historical_matrix_adoption'] == 'NOT_ADOPTED'
    assert 'wrapper-assigned' in result['label_basis']


@pytest.mark.parametrize('target', ['observation', 'manifest.json', 'reference.json',
    'prepared/ocv-t60-p10-n4.json', *REQUIRED])
def test_changed_bytes_refuse(synthetic, target):
    path = (synthetic['observation'] if target == 'observation' else
        synthetic['dataset'] / target if target in REQUIRED else synthetic['runtime'] / target)
    path.write_bytes(path.read_bytes() + b' ')
    with pytest.raises(ValueError):
        run(synthetic)


@pytest.mark.parametrize('locator', ['../escape', '/absolute', 'a//b', 'a\\b', './a'])
def test_unsafe_raw_locator_refuses_even_with_reanchored_synthetic_report(synthetic, locator):
    synthetic['report']['source_evidence']['native.out']['owner_relative_path'] = locator
    synthetic['repin']()
    with pytest.raises(ValueError):
        run(synthetic)


@pytest.mark.parametrize('damage', ['missing_role', 'extra_role', 'size', 'source', 'false_verified'])
def test_bad_attribution_refuses(synthetic, damage):
    report = synthetic['report']
    if damage == 'missing_role': report['source_evidence'].pop('stdout')
    elif damage == 'extra_role': report['source_evidence']['extra'] = {}
    elif damage == 'size': report['source_evidence']['stdout']['bytes'] = False
    elif damage == 'source': report['source_files'][next(iter(report['source_files']))] = '0' * 64
    else: report['source_evidence']['stdout']['verified'] = False
    synthetic['repin']()
    with pytest.raises(ValueError):
        run(synthetic)


@pytest.mark.parametrize('damage', ['incomplete', 'errors', 'value', 'missing_value', 'reaction', 'assessment'])
def test_changed_recovery_refuses(synthetic, monkeypatch, damage):
    result = synthetic['recovered']
    if damage == 'incomplete': result['status'] = 'INCOMPLETE'
    elif damage == 'errors': result['errors'] = ['evidence mismatch']
    elif damage == 'value': result['values'][next(iter(result['values']))] = '1'
    elif damage == 'missing_value': result['values'].pop(next(iter(result['values'])))
    elif damage == 'reaction': result['rfy_sum'] = '1'
    else: monkeypatch.setattr(replay, 'evaluate_attempt', lambda *a: {'status': 'CONTINUE'})
    with pytest.raises(ValueError):
        run(synthetic)


@pytest.mark.parametrize('kind', ['hardlink', 'symlink'])
def test_aliased_evidence_refuses(synthetic, kind):
    original = synthetic['dataset'] / 'native.out'
    alias = synthetic['dataset'] / 'alias'
    if kind == 'hardlink': os.link(original, alias)
    else:
        try: alias.symlink_to(original)
        except OSError as error: pytest.skip(f'symlink creation unavailable: {error}')
    synthetic['report']['source_evidence']['native.out']['owner_relative_path'] = 'alias'
    synthetic['repin']()
    with pytest.raises(ValueError):
        run(synthetic)


def test_loaded_source_origin_mismatch_refuses(synthetic, monkeypatch, tmp_path):
    import sys
    from types import SimpleNamespace
    monkeypatch.setitem(sys.modules, 'digitalmodel.ansys.cylinder_criteria',
        SimpleNamespace(__file__=str(tmp_path / 'foreign.py')))
    with pytest.raises(ValueError, match='origin'):
        run(synthetic)


@pytest.mark.parametrize('key,value', [('schema', 'wrong'), ('case_id', 'ocv-t60-p10-n8'),
    ('recovery_status', 'INCOMPLETE'), ('recovery_errors', ['bad']), ('engineering_qualified', True)])
def test_historical_nonqualification_contract(synthetic, key, value):
    synthetic['report'][key] = value
    synthetic['repin']()
    with pytest.raises(ValueError):
        run(synthetic)


def test_missing_historical_field_is_refusal(synthetic):
    synthetic['report'].pop('source_files')
    synthetic['repin']()
    with pytest.raises(ValueError):
        run(synthetic)


def test_missing_frozen_reference_entry_is_refusal(synthetic):
    path = synthetic['runtime'] / 'manifest.json'
    manifest = parse_json(path.read_bytes())
    manifest['artifacts'] = manifest['artifacts'][:1]
    raw = canonical_bytes(manifest); path.write_bytes(raw)
    synthetic['report']['runtime_sha256'] = digest_bytes(raw)
    synthetic['repin']()
    with pytest.raises(ValueError):
        run(synthetic)


def test_retained_n4_exact_replay():
    names = ('ANSYS_REPLAY_DATASET', 'ANSYS_REPLAY_RUNTIME', 'ANSYS_REPLAY_OBSERVATION')
    paths = [os.environ.get(name) for name in names]
    if not all(paths):
        if os.environ.get('ANSYS_REPLAY_REQUIRE_CORPUS') == '1':
            pytest.fail('strict publication mode requires all retained corpus locations')
        pytest.skip('private retained corpus not configured; insufficient for publication')
    roots = [Path(p) for p in paths]
    files = [p for root in roots for p in (root.rglob('*') if root.is_dir() else [root]) if p.is_file()]
    before = {p: digest_bytes(p.read_bytes()) for p in files}
    result = replay.replay_n4_predecessor(*roots)
    assert result['replay_status'] == 'COMPLETE'
    assert len(result['values']) == 63
    assert result['assessment']['status'] == 'CONTINUE'
    assert digest_bytes(canonical_bytes(result['values'])) == '4b7ec695e293790fa099efc4df5e9bdff625dec75f3d80a49f200d0c4a6260a3'
    assert digest_bytes(canonical_bytes(result['assessment'])) == '87d1d311216b9735563494f8fb4e63fde74133ddb413b4f0ad5a17e481053dbc'
    assert result['support_rfy_sum'] == {'value': '-0.00100716186836650647', 'unit': 'N'}
    assert result['failed_checks'] == [dict(criterion='expected_zero',
        response=['outer_y120', 'sigma_r'], residual='0.01059709996930703', limit='0.010', passed=False)]
    checks = result['assessment']['checks']
    assert len(checks) == 22 and sum(row['passed'] for row in checks) == 21
    assert result['native_launches'] == 0 and result['engineering_qualified'] is False
    assert result['observation_sha256'] == 'd38703f10f5cdd190802b57d1374ddf9f7b81d628e36ae77030a0e1418881c5f'
    matrix_raw = (roots[0] / 'derived/r6/matrix.json').read_bytes()
    assert digest_bytes(matrix_raw) == '5b2d08289e0e2affea908883c73967f9ab5afa6af4a864aceb04ef68bb400e59'
    n4 = next(row for row in parse_json(matrix_raw)['cases'] if row['case_id'] == 'ocv-t60-p10-n4')
    assert len(n4['responses']) == 64 and all(row['value'] is None for row in n4['responses'])
    after = {p: digest_bytes(p.read_bytes()) for root in roots
        for p in (root.rglob('*') if root.is_dir() else [root]) if p.is_file()}
    assert after == before
