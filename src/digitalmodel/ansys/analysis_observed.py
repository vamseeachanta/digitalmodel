"""One deterministic metadata-only pending-to-observed transition; never a solve."""
from copy import deepcopy
import re

from digitalmodel.ansys.analysis_records import (
    canonical_bytes, decimal_text, digest_bytes, nonempty, parse_json, verify_reference,
)
from digitalmodel.ansys.analysis_evidence import build_package, validate_package

CASE_ID = 'ocv-zero-t60-n16'
FIELDS = {'schema', 'case_id', 'case_index', 'previous_case_hash', 'attempt_consumed',
          'native_attempt_count', 'execution_status', 'assessment_status', 'source_kind',
          'author', 'author_status', 'return_code', 'duration_seconds', 'reason_code', 'sources'}
COMMON_SOURCES = {'claim', 'runtime_manifest', 'approval', 'config', 'review'}


def _reference(reference):
    if not isinstance(reference, dict) or set(reference) != {'id', 'sha256'}:
        raise ValueError('opaque evidence reference required')
    if (not isinstance(reference['id'], str)
            or not re.fullmatch(r'[A-Za-z0-9][A-Za-z0-9_-]{0,99}', reference['id'])):
        raise ValueError('opaque evidence id required')
    if not isinstance(reference['sha256'], str) or not re.fullmatch(r'[0-9a-f]{64}', reference['sha256']):
        raise ValueError('evidence digest required')


def _state(receipt):
    count = receipt['native_attempt_count']
    if receipt['attempt_consumed'] is not True or (count is not None and type(count) is not int):
        raise ValueError('invalid typed attempt metadata')
    if count not in (0, 1, None):
        raise ValueError('unsupported native attempt count')
    if (receipt['assessment_status'] != 'incomplete' or receipt['author'] != 'SOLVERS'
            or receipt['author_status'] != 'recorded'):
        raise ValueError('diagnostic assessment or author differs')
    if count == 1:
        if receipt['source_kind'] != 'native':
            raise ValueError('native source required for observed execution')
        if (receipt['execution_status'] != 'completed' or type(receipt['return_code']) is not int
                or receipt['return_code'] != 0 or receipt['reason_code'] != 'unsupported-cdb-omega'
                or decimal_text(receipt['duration_seconds']) != receipt['duration_seconds']):
            raise ValueError('execution evidence state differs')
        return COMMON_SOURCES | {'execution', 'outcome'}
    reason = 'prelaunch-refusal' if count == 0 else 'launch-settlement-unknown'
    if (receipt['source_kind'] != 'unverified' or receipt['execution_status'] != 'unknown'
            or receipt['return_code'] is not None or receipt['duration_seconds'] is not None
            or receipt['reason_code'] != reason):
        raise ValueError('execution evidence contradicts zero or unknown count')
    if 'launch_boundary' not in receipt['sources']:
        raise ValueError('claim or uncertainty evidence required')
    return COMMON_SOURCES | {'launch_boundary'}


def _execution(receipt, documents):
    execution, outcome = documents['execution'], documents['outcome']
    if (type(execution.get('return_code')) is not int or execution['return_code'] != 0
            or execution.get('timed_out') is not False
            or execution.get('streams_finalized') is not True
            or type(execution.get('owned_processes_remaining')) is not int
            or execution['owned_processes_remaining'] != 0
            or execution.get('settlement_required', False) is not False
            or execution.get('duration_seconds') != receipt['duration_seconds']):
        raise ValueError('execution evidence contradicts observed completion')
    rows = outcome.get('records')
    if (outcome.get('status') != 'INCOMPLETE' or outcome.get('attempted') != [CASE_ID]
            or not isinstance(rows, list) or len(rows) != 1):
        raise ValueError('outcome execution evidence differs')
    row = rows[0]
    if (row.get('case_id') != CASE_ID or row.get('values') != {}
            or row.get('engineering_qualified') is not False
            or row.get('evidence_errors') != ['Unsupported CDB command OMEGA']):
        raise ValueError('original incomplete assessment evidence differs')
    _result_binding(documents, row)


def _result_binding(documents, row):
    artifacts = {r['path']: r['sha256'] for r in documents['runtime_manifest'].get('artifacts', [])}
    captured = row.get('artifacts', {})
    if (not artifacts.get('prepared/' + CASE_ID + '.inp')
            or captured.get(CASE_ID + '.inp', {}).get('sha256') != artifacts['prepared/' + CASE_ID + '.inp']
            or not artifacts.get('reference.json')
            or row.get('reference_sha256') != artifacts['reference.json']):
        raise ValueError('outcome input artifact binding differs')
    for stream in ('stdout', 'stderr'):
        expected = captured.get(stream + '.bin', {}).get('sha256')
        if not expected or documents['execution'].get(stream + '_sha256') != expected:
            raise ValueError('execution stream artifact binding differs')


def _config_binding(documents):
    config, approval = documents['config'], documents['approval']
    scope = config.get('scope')
    if (scope != dict(case_ids=[CASE_ID], max_attempts=1, qualification='diagnostic_only')
            or type(scope.get('max_attempts')) is not int):
        raise ValueError('config scope binding differs')
    binding = config.get('execution_binding')
    if (not isinstance(binding, dict) or 'manifest_sha256' not in binding
            or any(approval.get(key) != value for key, value in binding.items())
            or config.get('operator_id') != approval.get('operator_id')
            or config.get('campaign_id') != approval.get('approval_id')):
        raise ValueError('config execution binding differs')


def _boundary(receipt, document):
    count = receipt['native_attempt_count']
    expected = dict(schema='launch-boundary-1', claim_consumed=True,
                    launch_state='not_started' if count == 0 else 'unknown',
                    process_created=False if count == 0 else None,
                    reason_code=receipt['reason_code'])
    if canonical_bytes(document) != canonical_bytes(expected):
        raise ValueError('claim or uncertainty evidence differs')


def _authority(receipt, documents, baseline_case):
    sources = receipt['sources']
    claim, approval = documents['claim'], documents['approval']
    expected = {'manifest_sha256': sources['runtime_manifest']['sha256'],
                'config_sha256': sources['config']['sha256'],
                'review_receipt_sha256': sources['review']['sha256']}
    if (claim.get('approval_sha256') != sources['approval']['sha256']
            or not approval.get('approval_id') or claim.get('approval_id') != approval['approval_id']
            or approval.get('operator_id') != 'SOLVERS'
            or any(approval.get(k) != v for k, v in expected.items())):
        raise ValueError('execution authority binding differs')
    _config_binding(documents)
    runtime = documents['runtime_manifest']
    if not runtime.get('case_order') or runtime['case_order'][0] != CASE_ID:
        raise ValueError('executed runtime case identity differs')
    descriptor = baseline_case['input_descriptor']
    original = descriptor.get('benchmark_manifest_reference')
    if not isinstance(original, dict) or not original.get('sha256'):
        raise ValueError('production physical input manifest required')
    if runtime.get('runtime_lineage', {}).get('original_manifest_sha256') != original['sha256']:
        raise ValueError('executed runtime physical input lineage differs')
    if original:
        actual = {r['path']: r['sha256'] for r in runtime.get('artifacts', [])}
        for ref in baseline_case['evidence']:
            prefix = 'repository/examples/ansys/cylinder-benchmark/'
            if ref['id'].startswith(prefix) and not ref['id'].endswith('/manifest.json'):
                if actual.get(ref['id'][len(prefix):]) != ref['sha256']:
                    raise ValueError('executed runtime input artifact differs')


def _receipt(baseline_case, reference, resolver):
    _reference(reference)
    receipt = parse_json(verify_reference(reference, resolver))
    if set(receipt) != FIELDS or receipt['schema'] != 'zero-diagnostic-intake-2':
        raise ValueError('unsupported receipt fields')
    if type(receipt['case_index']) is not int or receipt['case_index'] != 8:
        raise ValueError('observation case index differs')
    if receipt['case_id'] != CASE_ID:
        raise ValueError('observation case identity differs')
    if receipt['previous_case_hash'] != baseline_case['row_hash']:
        raise ValueError('baseline row hash differs')
    if not isinstance(receipt['sources'], dict):
        raise ValueError('receipt sources required')
    required = _state(receipt)
    if set(receipt['sources']) != required:
        raise ValueError('receipt source roles differ')
    documents = {}
    for role, ref in receipt['sources'].items():
        _reference(ref)
        documents[role] = parse_json(verify_reference(ref, resolver))
    _authority(receipt, documents, baseline_case)
    if receipt['native_attempt_count'] == 1:
        _execution(receipt, documents)
    else:
        _boundary(receipt, documents['launch_boundary'])
    return receipt, documents


def _baseline(baseline):
    validate_package(baseline)
    if (baseline['dataset_id'] != 'ansys-retained-evidence' or len(baseline['cases']) != 12
            or sum(len(c['responses']) for c in baseline['cases']) != 350):
        raise ValueError('expected twelve-case 350-response baseline')
    case = baseline['cases'][8]
    if (case['case_id'] != CASE_ID or case['capture_role'] != 'pending_native'
            or case.get('attempt_consumed') is not False or case.get('native_attempt_count') != 0
            or len(case['responses']) != 64):
        raise ValueError('one pending_native transition required')
    return case


def derive_observed_case(baseline, reference, resolver):
    old = _baseline(baseline)
    receipt, documents = _receipt(old, reference, resolver)
    case = deepcopy(old)
    case.pop('row_hash')
    for key in ('attempt_consumed', 'native_attempt_count', 'execution_status',
                'assessment_status', 'source_kind', 'author', 'author_status'):
        case[key] = receipt[key]
    case.update(capture_role='diagnostic_observation', observation_reference=deepcopy(reference),
        input_descriptor_scope='historical-prelaunch-basis',
        observed_retention=dict(status='retained-local-source-evidence',
                                private_git_backup='not-established'),
        observed_solver_profile=None,
        observed_solver_profile_status='not-established-from-native-header',
        observed_execution=dict(return_code=receipt['return_code'],
                                duration_seconds=receipt['duration_seconds']))
    evidence = {r['id']: r for r in case['evidence']}
    for role, ref in dict(diagnostic_intake=reference, **receipt['sources']).items():
        item = dict(ref, role='approval-reconstructed' if role == 'approval' else role, required=True)
        if ref['id'] in evidence and evidence[ref['id']]['sha256'] != ref['sha256']:
            raise ValueError('observation evidence identity collision')
        if ref['id'] not in evidence:
            evidence[ref['id']] = item
    case['evidence'] = list(evidence.values())
    if receipt['native_attempt_count'] != 1:
        case['attempt_boundary_reference'] = deepcopy(receipt['sources']['launch_boundary'])
    for row in case['responses']:
        row.update(value=None, calculation_status='failed',
            limitations=list(dict.fromkeys([x for x in row['limitations']
                if x != 'native-not-attempted'] + ['diagnostic-only', receipt['reason_code']])),
            evidence_ids=list(dict.fromkeys(row['evidence_ids'] + [reference['id']])))
    case['row_hash'] = digest_bytes(canonical_bytes(case))
    return case


def validate_observed_transition(package, baseline, resolver):
    _baseline(baseline)
    if len(package['cases']) != 12:
        raise ValueError('observed transition case count differs')
    for index in [*range(8), 9, 10, 11]:
        if canonical_bytes(package['cases'][index]) != canonical_bytes(baseline['cases'][index]):
            raise ValueError('other historical case changed')
    case = package['cases'][8]
    reference = case.get('observation_reference')
    if not reference:
        raise ValueError('observed transition receipt required')
    expected = derive_observed_case(baseline, reference, resolver)
    if canonical_bytes(case) != canonical_bytes(expected):
        raise ValueError('observed case differs from deterministic receipt derivation')
    if package.get('coverage') != _coverage(baseline, case):
        raise ValueError('observation coverage differs')


def _coverage(baseline, case):
    coverage = deepcopy(baseline.get('coverage', {}))
    cases = list(baseline['cases'])
    cases[8] = case
    pending = [row for row in cases if row['capture_role'] == 'pending_native']
    coverage.update(total_cases=len(cases), total_responses=sum(len(c['responses']) for c in cases),
        pending_cases=len(pending), pending_responses=sum(len(c['responses']) for c in pending),
        assessment_incomplete_cases=sum(c.get('assessment_status') == 'incomplete' for c in cases),
        assessment_failed_responses=sum(r['calculation_status'] == 'failed'
                                        for c in cases for r in c['responses']), qualified_responses=0,
        observed_transition_native_attempts=case['native_attempt_count'])
    return coverage


def build_observed_package(baseline, resolver, *, observation_reference, revision,
                           code_revision, source_revision):
    case = derive_observed_case(baseline, observation_reference, resolver)
    if revision == baseline['revision']:
        raise ValueError('new explicit revision required')
    study = deepcopy(baseline)
    study.pop('package_hash')
    study.update(previous_package_hash=baseline['package_hash'], revision=revision,
                 code_revision=nonempty(code_revision, 'code revision'),
                 source_revision=nonempty(source_revision, 'source revision'),
                 method_revision='observed-native-metadata-1')
    study['cases'][8] = case
    study['coverage'] = _coverage(baseline, case)
    for row in study['cases']:
        row.pop('row_hash')
    package = build_package(study, resolver)
    validate_observed_transition(package, baseline, resolver)
    return package
