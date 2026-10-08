"""RED metadata transition contracts; synthetic replicas, no native qualification.

Primary replica matches retained execution: one launch, rc0, 21.219 s, followed
by Unsupported CDB command OMEGA extraction refusal. No native bytes are copied.
"""
import copy
import json

import pytest

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes, validate_case
from digitalmodel.ansys.analysis_evidence import build_package, publish_package
from digitalmodel.ansys.analysis_matrix_publish import publish_matrix
from digitalmodel.ansys.analysis_observed import build_observed_package


def reference(tmp_path, resolver, identity, data):
    path = tmp_path / (identity + '.json')
    path.write_bytes(canonical_bytes(data))
    resolver[identity] = path
    return dict(id=identity, sha256=digest_bytes(path.read_bytes()))


def fixture_baseline(tmp_path, resolver):
    ref = reference(tmp_path, resolver, 'fixture', {'synthetic_protocol': True})
    cases = []
    original = dict(id='repository/examples/ansys/cylinder-benchmark/manifest.json', sha256=ref['sha256'])
    ids = [f'old-{i}' for i in range(8)] + [
        'ocv-zero-t60-n16', 'ocv-t60-p10-n4', 'ocv-t60-p10-n8', 'ocv-t60-p10-n16']
    for index, count in enumerate([6, 6, 7, 7, 16, 16, 18, 18, 64, 64, 64, 64]):
        pending = index >= 8
        rows = [dict(name=f'quantity-{n}', definition=f'fixture-{n}', location=f'node-{n}',
                     unit='MPa', value=None, calculation_status='not_evaluated',
                     limitations=['native-not-attempted'], evidence_ids=['fixture'],
                     inherited_findings=[]) for n in range(count)]
        case = dict(case_id=ids[index], component_id='fixture', model_revision='fixture',
            parameters={'index': str(index)}, author='unverified', author_status='unverified',
            source_kind='unverified', execution_status='unknown', retention_rights='approved',
            use_rights='unresolved', generated_at='2026-09-14T00:00:00Z', superseded_by=[],
            capture_role='pending_native' if pending else 'fixture',
            input_descriptor=dict(load_basis='fixture', source_revision='fixture', solver='none',
                                  frame='fixture', dependencies=['fixture'],
                                  benchmark_manifest_reference=original),
            evidence=[dict(ref, role='input_basis', required=True)], responses=rows)
        if pending:
            case.update(attempt_consumed=False, native_attempt_count=0)
        cases.append(case)
    return build_package(dict(analysis_id='fixture', dataset_id='ansys-retained-evidence',
        revision='r2', criteria_revision='fixture', code_revision='fixture',
        method_revision='pending-native-matrix-1', expected_cases=ids, intended_uses=['diagnostic'],
        cases=cases, finding_ledger=[], criteria_reference=ref, intake_reference=ref,
        review_sources=[], coverage=dict(total_cases=12, total_responses=350,
            pending_cases=4, pending_responses=256, qualified_responses=0)), resolver)


@pytest.fixture
def transition(tmp_path):
    resolver = {}
    baseline = fixture_baseline(tmp_path, resolver)
    execution = reference(tmp_path, resolver, 'native-execution-1', dict(return_code=0,
        duration_seconds='21.219000000040978', streams_finalized=True, timed_out=False,
        owned_processes_remaining=0, evidence_complete=True,
        stdout_sha256='a'*64, stderr_sha256='b'*64))
    outcome = reference(tmp_path, resolver, 'native-outcome-1', dict(status='INCOMPLETE',
        attempted=['ocv-zero-t60-n16'], records=[dict(case_id='ocv-zero-t60-n16',
        values={}, engineering_qualified=False, reference_sha256='c'*64,
        artifacts={'ocv-zero-t60-n16.inp': {'sha256': 'd'*64},
                   'stdout.bin': {'sha256': 'a'*64}, 'stderr.bin': {'sha256': 'b'*64}}, evidence_errors=['Unsupported CDB command OMEGA'])]))
    metadata = reference(tmp_path, resolver, 'bound-source-1', {'synthetic_protocol': True})
    runtime = reference(tmp_path, resolver, 'runtime-1', dict(case_order=['ocv-zero-t60-n16'],
        runtime_lineage={'original_manifest_sha256': baseline['cases'][8]['input_descriptor']['benchmark_manifest_reference']['sha256']},
        artifacts=[{'path': 'prepared/ocv-zero-t60-n16.inp', 'sha256': 'd'*64},
                   {'path': 'reference.json', 'sha256': 'c'*64}]))
    config = reference(tmp_path, resolver, 'config-1', dict(operator_id='SOLVERS',
        campaign_id='fixture', scope=dict(case_ids=['ocv-zero-t60-n16'], max_attempts=1,
        qualification='diagnostic_only'), execution_binding=dict(manifest_sha256=runtime['sha256'],
        profile={'cores': 1})))
    approval = reference(tmp_path, resolver, 'approval-1', dict(approval_id='fixture',
        operator_id='SOLVERS', manifest_sha256=runtime['sha256'],
        config_sha256=config['sha256'], review_receipt_sha256=metadata['sha256'],
        campaign_id='fixture', profile={'cores': 1}))
    claim = reference(tmp_path, resolver, 'claim-1', dict(approval_id='fixture',
                                                         approval_sha256=approval['sha256']))
    receipt = dict(schema='zero-diagnostic-intake-2', case_id='ocv-zero-t60-n16', case_index=8,
        previous_case_hash=baseline['cases'][8]['row_hash'], attempt_consumed=True,
        native_attempt_count=1, execution_status='completed', assessment_status='incomplete',
        source_kind='native', author='SOLVERS', author_status='recorded', return_code=0,
        duration_seconds='21.219000000040978', reason_code='unsupported-cdb-omega',
        sources=dict(execution=execution, outcome=outcome, claim=claim,
                     runtime_manifest=runtime, approval=approval, config=config, review=metadata))
    return dict(baseline=baseline, receipt=receipt, resolver=resolver, tmp_path=tmp_path)


def bind(data):
    return reference(data['tmp_path'], data['resolver'], 'observed-receipt-1', data['receipt'])


def build(data, receipt_ref=None):
    return build_observed_package(data['baseline'], data['resolver'],
        observation_reference=receipt_ref or bind(data), revision='r3',
        code_revision='fixture-code', source_revision='fixture-source')


def test_completed_native_execution_with_incomplete_assessment_preserves_nulls(transition):
    result = build(transition)
    case = result['cases'][8]
    assert (case['attempt_consumed'], case['native_attempt_count']) == (True, 1)
    assert (case['source_kind'], case['execution_status'], case['assessment_status']) == (
        'native', 'completed', 'incomplete')
    assert (case['author'], case['author_status']) == ('SOLVERS', 'recorded')
    assert all(r['value'] is None and r['calculation_status'] == 'failed'
               and 'unsupported-cdb-omega' in r['limitations'] for r in case['responses'])
    validate_case(case)
    before = transition['baseline']
    for index in [*range(8), 9, 10, 11]:
        assert canonical_bytes(result['cases'][index]) == canonical_bytes(before['cases'][index])
    for key in ('case_id', 'component_id', 'model_revision', 'parameters', 'input_descriptor'):
        assert case[key] == before['cases'][8][key]
    assert result['coverage']['pending_cases'] == 3
    assert result['coverage']['pending_responses'] == 192
    assert result['coverage']['assessment_incomplete_cases'] == 1
    assert result['coverage']['assessment_failed_responses'] == 64
    assert result['coverage']['qualified_responses'] == 0


@pytest.mark.parametrize('field,value,reason', [
    ('case_index', 9, 'case index'), ('case_id', 'ocv-t60-p10-n4', 'case identity'),
    ('previous_case_hash', '0'*64, 'baseline row'), ('source_kind', 'synthetic', 'native source'),
    ('native_attempt_count', 0, 'execution evidence'),
    ('values', {'q': '0'}, 'receipt fields'), ('host', 'private-host', 'receipt fields'),
])
def test_specific_receipt_contradictions_refuse(transition, field, value, reason):
    transition['receipt'][field] = value
    with pytest.raises(ValueError, match=reason):
        build(transition)


@pytest.mark.parametrize('count', [0, None])
def test_consumed_without_proven_count_requires_specific_evidence(transition, count):
    transition['receipt'].update(native_attempt_count=count, source_kind='unverified',
                                 execution_status='unknown', return_code=None, duration_seconds=None,
                                 reason_code='prelaunch-refusal' if count == 0 else 'launch-settlement-unknown')
    transition['receipt']['sources'].pop('execution')
    transition['receipt']['sources'].pop('outcome')
    with pytest.raises(ValueError, match='claim or uncertainty evidence'):
        build(transition)


@pytest.mark.parametrize('count,state', [(0, 'not_started'), (None, 'unknown')])
def test_specific_launch_boundary_retains_zero_or_unknown_count(transition, count, state):
    receipt = transition['receipt']
    boundary = reference(transition['tmp_path'], transition['resolver'], 'launch-boundary-1',
        dict(schema='launch-boundary-1', claim_consumed=True, launch_state=state,
             process_created=False if count == 0 else None,
             reason_code='prelaunch-refusal' if count == 0 else 'launch-settlement-unknown'))
    receipt.update(native_attempt_count=count, source_kind='unverified',
        execution_status='unknown', return_code=None, duration_seconds=None,
        reason_code='prelaunch-refusal' if count == 0 else 'launch-settlement-unknown')
    receipt['sources'].pop('execution')
    receipt['sources'].pop('outcome')
    receipt['sources']['launch_boundary'] = boundary
    package = build(transition)
    assert package['coverage']['observed_transition_native_attempts'] is count
    assert package['coverage']['assessment_incomplete_cases'] == 1
    assert package['coverage']['assessment_failed_responses'] == 64
    result = package['cases'][8]
    assert result['assessment_status'] == 'incomplete'
    assert all(r['calculation_status'] == 'failed' for r in result['responses'])
    assert result['native_attempt_count'] is count
    assert result['attempt_consumed'] is True
    assert result['source_kind'] == 'unverified'
    assert all(r['value'] is None for r in result['responses'])
    validate_case(result)


def publication_setup(data):
    owner = data['tmp_path'] / 'owner'
    publish_package(data['baseline'], owner)
    canonical = data['tmp_path'] / 'manifest.json'
    canonical.write_bytes(canonical_bytes(data['baseline']))
    return owner, canonical


def test_publish_rechecks_tampered_receipt_before_writing(transition):
    result = build(transition)
    owner, canonical = publication_setup(transition)
    original = canonical.read_bytes()
    transition['resolver']['observed-receipt-1'].write_bytes(b'tampered')
    with pytest.raises(ValueError, match='evidence changed'):
        publish_matrix(result, transition['baseline'], canonical, owner, transition['resolver'])
    assert canonical.read_bytes() == original


def test_publish_refuses_second_transition_even_with_rebound_package_hash(transition):
    first = build(transition)
    owner, canonical = publication_setup(transition)
    publish_matrix(first, transition['baseline'], canonical, owner, transition['resolver'])
    second = copy.deepcopy(first)
    second['revision'] = 'r4'
    second['previous_package_hash'] = first['package_hash']
    second['cases'][8]['assessment_status'] = 'failed'
    for case in second['cases']:
        case.pop('row_hash')
    second.pop('package_hash')
    second = build_package(second, transition['resolver'])
    with pytest.raises(ValueError, match='pending_native transition'):
        publish_matrix(second, first, canonical, owner, transition['resolver'])


def test_rehashed_claim_with_unrelated_approval_refuses(transition):
    transition['receipt']['sources']['claim'] = reference(transition['tmp_path'],
        transition['resolver'], 'claim-other', dict(approval_id='fixture', approval_sha256='0'*64))
    with pytest.raises(ValueError, match='execution authority binding'):
        build(transition)


def test_rehashed_execution_cannot_change_exact_duration(transition):
    transition['receipt']['duration_seconds'] = '21.219'
    with pytest.raises(ValueError, match='execution evidence'):
        build(transition)


def test_deterministic_build_retains_baseline_time_and_descriptor(transition):
    first = build(transition)
    second = build(transition)
    assert canonical_bytes(first) == canonical_bytes(second)
    assert first['cases'][8]['generated_at'] == transition['baseline']['cases'][8]['generated_at']
    assert first['cases'][8]['input_descriptor_scope'] == 'historical-prelaunch-basis'


@pytest.mark.parametrize('damage', ['other-case', 'selected-physics', 'qualification-count'])
def test_rehashed_publication_cannot_change_preserved_contract(transition, damage):
    result = build(transition)
    owner, canonical = publication_setup(transition)
    if damage == 'other-case':
        result['cases'][9]['parameters']['index'] = '999'
        reason = 'other historical case changed'
    elif damage == 'selected-physics':
        result['cases'][8]['input_descriptor']['frame'] = 'changed'
        reason = 'deterministic receipt derivation'
    else:
        result['coverage']['qualified_responses'] = 1
        reason = 'observation coverage differs'
    for case in result['cases']:
        case.pop('row_hash')
    result.pop('package_hash')
    result = build_package(result, transition['resolver'])
    with pytest.raises(ValueError, match=reason):
        publish_matrix(result, transition['baseline'], canonical, owner, transition['resolver'])


def rebind_source(data, role, document):
    sources = data['receipt']['sources']
    sources[role] = reference(data['tmp_path'], data['resolver'], role + '-changed', document)
    if role in ('config', 'runtime_manifest'):
        approval = json.loads(data['resolver'][sources['approval']['id']].read_bytes())
        approval['config_sha256' if role == 'config' else 'manifest_sha256'] = sources[role]['sha256']
        rebind_source(data, 'approval', approval)
    if role == 'approval':
        rebind_source(data, 'claim', dict(approval_id=document['approval_id'],
                                        approval_sha256=sources[role]['sha256']))


def source_doc(data, role):
    return json.loads(data['resolver'][data['receipt']['sources'][role]['id']].read_bytes())


@pytest.mark.parametrize('role,field', [
    ('config', 'manifest'), ('config', 'scope'), ('config', 'profile'),
    ('outcome', 'deck'), ('outcome', 'reference'), ('execution', 'stdout'),
    ('execution', 'stderr'), ('runtime_manifest', 'lineage'),
    ('runtime_manifest', 'artifact'),
])
def test_rehashed_sources_must_cross_bind(transition, role, field):
    doc = source_doc(transition, role)
    if field == 'manifest':
        doc['execution_binding']['manifest_sha256'] = '0'*64
    elif field == 'scope':
        doc['scope']['max_attempts'] = 2
    elif field == 'profile':
        doc['execution_binding']['profile'] = {'cores': 99}
    elif field == 'deck':
        doc['records'][0]['artifacts']['ocv-zero-t60-n16.inp']['sha256'] = '0'*64
    elif field == 'reference':
        doc['records'][0]['reference_sha256'] = '0'*64
    elif field in ('stdout', 'stderr'):
        doc[field + '_sha256'] = '0'*64
    elif field == 'lineage':
        doc['runtime_lineage']['original_manifest_sha256'] = '0'*64
    else:
        doc['artifacts'][0]['sha256'] = '0'*64
    rebind_source(transition, role, doc)
    with pytest.raises(ValueError, match='binding|lineage|artifact'):
        build(transition)


def test_missing_physical_manifest_refuses(transition):
    baseline = transition['baseline']
    baseline['cases'][8]['input_descriptor'].pop('benchmark_manifest_reference')
    baseline.pop('package_hash')
    for case in baseline['cases']:
        case.pop('row_hash')
    transition['baseline'] = build_package(baseline, transition['resolver'])
    transition['receipt']['previous_case_hash'] = transition['baseline']['cases'][8]['row_hash']
    with pytest.raises(ValueError, match='physical input'):
        build(transition)


@pytest.mark.parametrize('field,value', [
    ('observed_solver_profile', {'build': '26.1'}),
    ('observed_solver_profile_status', 'observed'),
    ('input_descriptor_scope', 'current'), ('observed_execution', {'return_code': False}),
    ('observed_retention', {}),
])
def test_rehashed_record_metadata_refuses(transition, field, value):
    case = build(transition)['cases'][8]
    case[field] = value
    with pytest.raises(ValueError, match='observed metadata'):
        validate_case(case)


def test_coverage_counts_actual_remaining_pending_cases(transition):
    baseline = transition['baseline']
    baseline['cases'][9]['capture_role'] = 'historical_unresolved'
    baseline.pop('package_hash')
    for case in baseline['cases']:
        case.pop('row_hash')
    transition['baseline'] = build_package(baseline, transition['resolver'])
    result = build(transition)
    assert result['coverage']['pending_cases'] == 2
    assert result['coverage']['pending_responses'] == 128


@pytest.mark.parametrize('matches', [True, False])
def test_baseline_physical_artifact_mapping(transition, matches):
    baseline = transition['baseline']
    identity = 'repository/examples/ansys/cylinder-benchmark/prepared/ocv-zero-t60-n16.inp'
    path = transition['tmp_path'] / 'synthetic-deck.inp'
    path.write_bytes(b'synthetic fixture deck')
    transition['resolver'][identity] = path
    sha = digest_bytes(path.read_bytes())
    baseline['cases'][8]['evidence'].append(dict(id=identity, sha256=sha,
                                               role='input_basis', required=True))
    baseline.pop('package_hash')
    for case in baseline['cases']:
        case.pop('row_hash')
    transition['baseline'] = build_package(baseline, transition['resolver'])
    transition['receipt']['previous_case_hash'] = transition['baseline']['cases'][8]['row_hash']
    if matches:
        runtime = source_doc(transition, 'runtime_manifest')
        runtime['artifacts'][0]['sha256'] = sha
        rebind_source(transition, 'runtime_manifest', runtime)
        config = source_doc(transition, 'config')
        config['execution_binding']['manifest_sha256'] = transition['receipt']['sources']['runtime_manifest']['sha256']
        rebind_source(transition, 'config', config)
        outcome = source_doc(transition, 'outcome')
        outcome['records'][0]['artifacts']['ocv-zero-t60-n16.inp']['sha256'] = sha
        rebind_source(transition, 'outcome', outcome)
        assert build(transition)['cases'][8]['native_attempt_count'] == 1
    else:
        with pytest.raises(ValueError, match='runtime input artifact differs'):
            build(transition)


def test_coverage_includes_historical_failed_responses(transition):
    baseline = transition['baseline']
    baseline.pop('package_hash')
    for case in baseline['cases']:
        case.pop('row_hash')
    for response in baseline['cases'][0]['responses']:
        response.update(calculation_status='failed', limitations=['historical-parse-failure'])
    transition['baseline'] = build_package(baseline, transition['resolver'])
    result = build(transition)
    assert result['coverage']['assessment_failed_responses'] == 70
    assert result['coverage']['assessment_incomplete_cases'] == 1
    assert result['cases'][0] == transition['baseline']['cases'][0]
