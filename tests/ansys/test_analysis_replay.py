"""Synthetic offline replay contracts; no native execution or qualification."""
from copy import deepcopy
from decimal import Decimal
import pytest

from digitalmodel.ansys.analysis_replay import canonical_measurement, make_check_receipt
from digitalmodel.ansys.analysis_replay_inputs import validate_mapping, validate_review
from digitalmodel.ansys.cylinder_criteria import STATIONS


def mapping_fixture():
    rows = []
    for station in STATIONS:
        for quantity in ('sigma_r', 'sigma_theta', 'sigma_z', 'tau_rz', 'sigma_vm', 'u_r', 'u_z'):
            rows.append(dict(key=[station, quantity], name=f'{station}.{quantity}',
                             definition='fixed definition', location='fixed location',
                             unit='mm' if quantity.startswith('u_') else 'MPa'))
    rows.append(dict(key='RFY', name='support.RFY', definition='reaction sum',
                     location='bottom', unit='N'))
    return {'schema': 'zero-replay-mapping-1', 'responses': rows}


@pytest.mark.parametrize('raw,expected', [('0E-16', '0'), ('-0.000', '0'),
    ('1.234567890123456789', '1.234567890123456789'), ('1E-8', '0.00000001')])
def test_measurement_normalization_is_exact(raw, expected):
    assert canonical_measurement(raw) == expected
    assert Decimal(raw) == Decimal(expected)


@pytest.mark.parametrize('raw', ['NaN', 'Infinity', '1e10000', 0.0, 'x', ' 0'])
def test_invalid_or_unbounded_measurements_refuse(raw):
    with pytest.raises(ValueError):
        canonical_measurement(raw)


def test_mapping_is_bijective_and_matches_response_metadata():
    mapping = mapping_fixture()
    responses = [{k: v for k, v in row.items() if k != 'key'} for row in mapping['responses']]
    assert len(validate_mapping(mapping, responses)) == 64


@pytest.mark.parametrize('fault', ['duplicate', 'unit', 'name', 'extra'])
def test_mapping_tamper_refuses(fault):
    mapping = mapping_fixture()
    responses = [{k: v for k, v in row.items() if k != 'key'} for row in mapping['responses']]
    if fault == 'duplicate':
        mapping['responses'][1]['key'] = mapping['responses'][0]['key']
    elif fault == 'extra':
        mapping['responses'][0]['new'] = 'value'
    else:
        mapping['responses'][0][fault] = 'changed'
    with pytest.raises(ValueError):
        validate_mapping(mapping, responses)


def review_fixture():
    return {'status': 'REVIEW_RECEIVED', 'bundle_sha256': 'b' * 64,
            'files': [{'path': 'src/digitalmodel/ansys/example.py', 'sha256': 'a' * 64}],
            'review': {'bundle_sha256': 'b' * 64, 'verdict': 'MINOR', 'findings': []}}


def test_review_requires_current_exact_source_coverage():
    inventory = {'src/digitalmodel/ansys/example.py': 'a' * 64}
    validate_review(review_fixture(), inventory)
    with pytest.raises(ValueError):
        validate_review(review_fixture(), {**inventory, 'missing.py': 'c' * 64})


@pytest.mark.parametrize('fault', ['major', 'unavailable', 'digest', 'duplicate'])
def test_blocking_or_ambiguous_review_refuses(fault):
    review = review_fixture()
    if fault == 'major':
        review['review']['verdict'] = 'MAJOR'
    elif fault == 'unavailable':
        review['status'] = 'UNAVAILABLE'
    elif fault == 'duplicate':
        review['files'].append(deepcopy(review['files'][0]))
    else:
        review['review']['bundle_sha256'] = 'c' * 64
    with pytest.raises(ValueError):
        validate_review(review, {'src/digitalmodel/ansys/example.py': 'a' * 64})


def test_check_receipt_preserves_raw_text_and_reaction_taxonomy():
    checks = [{'response': ['inner_y60', 'sigma_r'], 'criterion': 'zero_control',
               'limit': '1E-8', 'residual': '0E-16', 'passed': True}]
    values = {('inner_y60', 'sigma_r'): '0E-16'}
    result = make_check_receipt('a' * 64, [], checks, values)
    assert result['checks'][0]['measured_raw'] == '0E-16'
    assert result['checks'][0]['measured_canonical'] == '0'
    assert 'observed_utc' not in result
    assert result == make_check_receipt('a' * 64, [], checks, values)


def test_failed_check_never_produces_receipt():
    with pytest.raises(ValueError):
        make_check_receipt('a' * 64, [], [{'response': 'RFY', 'criterion': 'axial_equilibrium',
            'limit': '0.000001', 'residual': '1', 'passed': False}], {'RFY': '1'})


def replay_case_fixture():
    mapping = mapping_fixture()
    refs = {name: dict(id=name, sha256='a' * 64, required=True)
            for name in ('replay_reference', 'check_reference', 'observation_reference')}
    rows = [dict(name=row['name'], value='0', calculation_status='computed',
                 limitations=['diagnostic-only']) for row in mapping['responses']]
    return dict(case_id='ocv-zero-t60-n16', attempt_consumed=True, native_attempt_count=1,
        source_kind='native', execution_status='completed', assessment_status='complete',
        author='SOLVERS', author_status='recorded', capture_role='diagnostic_replay',
        engineering_qualified=False, responses=rows, evidence=list(refs.values()), **refs)


def test_standalone_replay_schema_validates_without_io():
    from digitalmodel.ansys.analysis_replay import validate_replay_record
    validate_replay_record(replay_case_fixture())


@pytest.mark.parametrize('fault', ['count', 'role', 'assessment', 'duplicate', 'null', 'reference', 'qualified'])
def test_standalone_replay_schema_rejects_forged_state(fault):
    from digitalmodel.ansys.analysis_replay import validate_replay_record
    row = replay_case_fixture()
    if fault == 'count':
        row['native_attempt_count'] = True
    elif fault == 'role':
        row['capture_role'] = 'computed'
    elif fault == 'assessment':
        row['assessment_status'] = 'incomplete'
    elif fault == 'duplicate':
        row['responses'][1] = deepcopy(row['responses'][0])
    elif fault == 'null':
        row['responses'][0]['value'] = None
    elif fault == 'reference':
        row['evidence'].pop()
    else:
        row['engineering_qualified'] = True
    with pytest.raises(ValueError):
        validate_replay_record(row)


def synthetic_evaluation_inputs():
    from pathlib import Path
    from digitalmodel.ansys.analysis_replay_inputs import CASE_ID
    from tests.ansys.cylinder_synthetic_protocol import synthetic_protocol
    case, artifacts = synthetic_protocol(CASE_ID)
    names = {'native.out': CASE_ID + '.out', 'jobname.err': 'file.err',
             'stdout': 'stdout.bin', 'stderr': 'stderr.bin'}
    raw = {CASE_ID + '/' + names.get(k, k): v for k, v in artifacts.items()}
    raw[CASE_ID + '/' + CASE_ID + '.inp'] = case['deck_bytes']
    repo = Path(__file__).resolve().parents[2]
    reference = (repo/'examples/ansys/cylinder-benchmark/reference.json').read_bytes()
    from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
    reference_hash = digest_bytes(reference)
    manifest = canonical_bytes(dict(reference='reference.json', artifacts=[dict(
        path='reference.json', sha256=reference_hash, bytes=len(reference))]))
    raw['outcome.json'] = canonical_bytes(dict(records=[dict(
        case_id=CASE_ID, reference_sha256=reference_hash)]))
    return {'raw': raw, 'documents': {'reference': reference, 'runtime_manifest': manifest}}


def test_existing_parser_and_criteria_produce_all_64_synthetic_checks():
    from digitalmodel.ansys.analysis_replay import _evaluate
    values, checks = _evaluate(synthetic_evaluation_inputs())
    assert len(values) == len(checks) == 64
    assert sum(c['criterion'] == 'zero_control' for c in checks) == 63
    assert checks[-1]['criterion'] == 'axial_equilibrium'
    assert checks[-1]['response'] == 'RFY'


@pytest.mark.parametrize('fault', ['deck', 'missing', 'state'])
def test_real_parser_refuses_synthetic_tamper(fault):
    from digitalmodel.ansys.analysis_replay import _evaluate
    from digitalmodel.ansys.analysis_replay_inputs import CASE_ID
    inputs = synthetic_evaluation_inputs()
    raw = inputs['raw']
    if fault == 'deck':
        raw[CASE_ID + '/' + CASE_ID + '.inp'] += b'!changed'
    elif fault == 'missing':
        raw.pop(CASE_ID + '/station_values.txt')
    else:
        raw[CASE_ID + '/state_values.txt'] = b'corrupt'
    with pytest.raises((ValueError, KeyError)):
        _evaluate(inputs)
