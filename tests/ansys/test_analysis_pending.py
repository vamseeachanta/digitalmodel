"""Synthetic pending-matrix boundary tests; no native execution or qualification."""
import copy

import pytest

from digitalmodel.ansys.analysis_evidence import build_package, validate_package
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys.analysis_pending import build_pending_package
from digitalmodel.ansys.analysis_lookup import lookup


IDS = ['ocv-zero-t60-n16', 'ocv-t60-p10-n4', 'ocv-t60-p10-n8', 'ocv-t60-p10-n16']


def response(name, unit='MPa'):
    return dict(name=name, definition='synthetic protocol definition', location=name,
                unit=unit, value=None, calculation_status='not_evaluated',
                limitations=['native-not-attempted'], evidence_ids=['fixture'],
                inherited_findings=[])


@pytest.fixture
def records(tmp_path):
    path = tmp_path / 'fixture.txt'
    path.write_bytes(b'explicit synthetic evidence')
    ref = dict(id='fixture', sha256=digest_bytes(path.read_bytes()))
    case = dict(case_id='old-0', component_id='fixture', model_revision='fixture',
                parameters={'index': '0'}, author='unknown', author_status='unknown',
                source_kind='unverified', execution_status='unknown',
                retention_rights='approved', use_rights='unresolved',
                generated_at='2026-09-14T00:00:00Z', superseded_by=[], capture_role='fixture',
                input_descriptor=dict(load_basis='fixture', source_revision='fixture',
                                      solver='none', frame='fixture', dependencies=['fixture']),
                evidence=[dict(ref, role='input', required=True)], responses=[])
    old = []
    for i, count in enumerate([6, 6, 7, 7, 16, 16, 18, 18]):
        item = copy.deepcopy(case)
        item.update(case_id=f'old-{i}', parameters={'index': str(i)},
                    responses=[response(f'old-{j}') for j in range(count)])
        old.append(item)
    study = dict(analysis_id='fixture', dataset_id='ansys-retained-evidence', revision='r1',
                 criteria_revision='fixture', code_revision='old-code', method_revision='fixture',
                 expected_cases=[c['case_id'] for c in old], intended_uses=['diagnostic'],
                 cases=old, finding_ledger=[], criteria_reference=ref, intake_reference=ref,
                 review_sources=[])
    pending = []
    for i, name in enumerate(IDS):
        item = copy.deepcopy(case)
        item.update(case_id=name, parameters={'index': str(8+i)}, capture_role='pending_native',
                    author='unverified', author_status='unverified', attempt_consumed=False,
                    native_attempt_count=0)
        item['input_descriptor']['matrix_preparer'] = 'Codex'
        item['responses'] = [response(f'{r}_y{y}.{q}', 'mm' if q.startswith('u_') else 'MPa')
                             for r in ['inner', 'middle', 'outer'] for y in [60, 120, 180]
                             for q in ['sigma_r', 'sigma_theta', 'sigma_z', 'tau_rz',
                                       'sigma_vm', 'u_r', 'u_z']] + [response('support.RFY', 'N')]
        pending.append(item)
    resolver = {'fixture': path}
    return build_package(study, resolver), pending, resolver


def build(records):
    old, pending, resolver = records
    return build_pending_package(old, pending, resolver, revision='r2', code_revision='new-code',
                                 source_revision='new-source')


def test_preserves_all_historical_bytes_and_adds_only_pending(records):
    before = canonical_bytes(records[0])
    pending_before = canonical_bytes(records[1])
    result = build(records)
    validate_package(result)
    assert canonical_bytes(result['cases'][:8]) == canonical_bytes(records[0]['cases'])
    assert canonical_bytes(records[0]) == before
    assert canonical_bytes(records[1]) == pending_before
    assert len(result['cases']) == 12
    assert sum(len(c['responses']) for c in result['cases']) == 350
    assert all(r['value'] is None for c in result['cases'][8:] for r in c['responses'])
    assert result['finding_ledger'] == records[0]['finding_ledger']
    assert result['previous_package_hash'] == records[0]['package_hash']
    assert all('attempt_consumed' not in c for c in result['cases'][:8])


def test_current_code_inventory_is_not_inherited_as_new_evidence(records):
    old = records[0]
    old['code_files'] = [{'path': 'old.py', 'sha256': 'a' * 64}]
    old.pop('package_hash')
    old['package_hash'] = digest_bytes(canonical_bytes(old))
    new_files = [{'path': 'new.py', 'sha256': 'b' * 64}]
    result = build_pending_package(old, records[1], records[2], revision='r2',
                                   code_revision='new-code', code_files=new_files,
                                   method_revision='pending-v1', source_revision='new-source')
    assert result['baseline_code_files'] == old['code_files']
    assert result['code_files'] == new_files
    assert result['method_revision'] == 'pending-v1'
    assert 'code_files' not in build(records)


def test_source_revision_replaces_stale_package_identity(records):
    old = records[0]
    old['source_revision'] = 'old-source'
    old.pop('package_hash')
    old['package_hash'] = digest_bytes(canonical_bytes(old))
    result = build(records)
    assert result['source_revision'] == 'new-source'
    assert result['baseline_source_revision'] == 'old-source'
    with pytest.raises(TypeError):
        build_pending_package(old, records[1], records[2], revision='r2', code_revision='new-code')
    with pytest.raises(ValueError):
        build_pending_package(old, records[1], records[2], revision='r2',
                              code_revision='new-code', source_revision='')


def test_pending_lookup_remains_diagnostic_and_refuses_engineering(records):
    package = build(records)
    case = package['cases'][8]
    row = case['responses'][0]
    query = {key: package[key] for key in ('dataset_id', 'revision', 'analysis_id')}
    query.update({key: case[key] for key in ('component_id', 'model_revision', 'parameters')})
    query.update({key: row[key] for key in ('definition', 'location', 'unit')})
    query.update(response_name=row['name'], intended_use='diagnostic')
    result = lookup(package, query, records[2], diagnostic=True)
    assert result['value'] is None
    assert result['qualified'] is False
    assert result['source_kind'] == 'unverified'
    with pytest.raises(ValueError):
        lookup(package, query, records[2])


def test_pending_coverage_updates_without_inventing_baseline_qualification(records):
    old = records[0]
    old['coverage'] = {'native_captures': 6, 'qualified_responses': 0, 'custom': 'retained'}
    old['method_revision'] = 'offline-digest-import-1'
    old.pop('package_hash')
    old['package_hash'] = digest_bytes(canonical_bytes(old))
    result = build(records)
    assert result['coverage'] == dict(old['coverage'], pending_cases=4,
                                      pending_responses=256, total_cases=12, total_responses=350)
    assert result['method_revision'] == 'pending-native-matrix-1'
    assert result['baseline_method_revision'] == 'offline-digest-import-1'
    old['coverage']['qualified_responses'] = 3
    old.pop('package_hash')
    old['package_hash'] = digest_bytes(canonical_bytes(old))
    assert build(records)['coverage']['qualified_responses'] == 3


@pytest.mark.parametrize('count,consumed', [(0, False), (1, True)])
def test_shared_validator_accepts_consistent_nonpending_attempt_metadata(records, count, consumed):
    package = copy.deepcopy(records[0])
    case = package['cases'][0]
    case.update(native_attempt_count=count, attempt_consumed=consumed)
    case.pop('row_hash')
    case['row_hash'] = digest_bytes(canonical_bytes(case))
    package.pop('package_hash')
    package['package_hash'] = digest_bytes(canonical_bytes(package))
    validate_package(package)


@pytest.mark.parametrize('field,value', [('source_kind','native'), ('execution_status','completed'),
    ('attempt_consumed',True), ('attempt_consumed',0), ('native_attempt_count',False),
    ('native_attempt_count',1), ('native_attempt_count',-1), ('author','Codex'),
    ('author_status','recorded'), ('capture_role','native')])
def test_builder_refuses_nonpending_case(records, field, value):
    records[1][0][field] = value
    with pytest.raises(ValueError):
        build(records)


@pytest.mark.parametrize('damage', ['number','observed','computed','limitation','unit',
                                  'duplicate','missing','renamed','evidence','attempt_field'])
def test_builder_refuses_incomplete_responses(records, damage):
    case = records[1][0]
    row = case['responses'][0]
    if damage == 'number': row['value'] = '0'
    elif damage == 'observed': row['observed_value'] = None
    elif damage == 'computed': row['calculation_status'] = 'computed'
    elif damage == 'limitation': row['limitations'] = ['other']
    elif damage == 'unit': row['unit'] = 'Pa'
    elif damage == 'duplicate': case['responses'][-1] = copy.deepcopy(row)
    elif damage == 'missing': case['responses'].pop()
    elif damage == 'renamed': row['name'] = 'unexpected'
    elif damage == 'evidence': row['evidence_ids'] = []
    else: del case['attempt_consumed']
    with pytest.raises(ValueError): build(records)


@pytest.mark.parametrize('damage', ['membership','identity','missing_ref','changed_ref','old_hash','same_revision'])
def test_builder_refuses_membership_or_evidence_damage(records, damage):
    if damage == 'membership': records[1][0]['case_id'] = IDS[1]
    elif damage == 'identity': records[1][0]['parameters'] = records[0]['cases'][0]['parameters']
    elif damage == 'missing_ref': records[2].clear()
    elif damage == 'changed_ref': records[2]['fixture'].write_bytes(b'changed')
    elif damage == 'old_hash': records[0]['cases'][0]['author'] = 'changed'
    elif damage == 'same_revision': records[0]['revision'] = 'r2'
    with pytest.raises(ValueError): build(records)


@pytest.mark.parametrize('damage', ['count_bool','unpaired','count_negative','false_positive',
                                  'native','author','observed','reason'])
def test_shared_validation_refuses_rehashed_invalid_pending(records, damage):
    package = build(records)
    case = package['cases'][8]
    if damage == 'count_bool': case['native_attempt_count'] = False
    elif damage == 'unpaired': del case['attempt_consumed']
    elif damage == 'count_negative': case['native_attempt_count'] = -1
    elif damage == 'false_positive': case['native_attempt_count'] = 1
    elif damage == 'native': case['source_kind'] = 'native'
    elif damage == 'author': case['author_status'] = 'recorded'
    elif damage == 'observed': case['responses'][0]['observed_value'] = '2'
    else: case['responses'][0]['limitations'] = []
    case.pop('row_hash')
    case['row_hash'] = digest_bytes(canonical_bytes(case))
    package.pop('package_hash')
    package['package_hash'] = digest_bytes(canonical_bytes(package))
    with pytest.raises(ValueError): validate_package(package)
