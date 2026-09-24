"""One offline zero-case replay transition; no native execution or qualification."""
from copy import deepcopy
from decimal import Decimal, InvalidOperation
import re

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes, parse_json, nonempty, verify_reference
from digitalmodel.ansys.analysis_evidence import build_package, validate_package
from digitalmodel.ansys.analysis_replay_inputs import (
    CASE_ID, REFERENCE_HASH, load_replay_inputs, opaque_reference, validate_mapping, observed_reference_hash, verify_original_artifacts,
)
from digitalmodel.ansys.cylinder_benchmark import build_case
from digitalmodel.ansys.cylinder_criteria import EXPECTED_KEYS, evaluate_attempt, evaluate_canary
from digitalmodel.ansys.cylinder_results_validation import validate_native_evidence


def canonical_measurement(raw):
    if (not isinstance(raw, str) or len(raw) > 128
            or not re.fullmatch(r'[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[Ee][+-]?\d{1,3})?', raw)):
        raise ValueError('invalid bounded measured text')
    try:
        value = Decimal(raw)
    except InvalidOperation as error:
        raise ValueError('invalid measured decimal') from error
    if not value.is_finite() or abs(value.as_tuple().exponent) > 128 or len(value.as_tuple().digits) > 64:
        raise ValueError('measured decimal exceeds exact bounds')
    result = '0' if value.is_zero() else format(value, 'f')
    if '.' in result:
        result = result.rstrip('0').rstrip('.')
    if Decimal(result) != value:
        raise ValueError('numeric normalization changed value')
    return result


def make_check_receipt(previous_hash, inventory, checks, measured):
    rows = []
    for check in checks:
        key = tuple(check['response']) if isinstance(check['response'], list) else check['response']
        if check['passed'] is not True:
            raise ValueError('failed fixed check refuses numerical intake')
        raw = measured[key]
        rows.append(dict(response=check['response'], criterion=check['criterion'],
            comparator='abs(residual)<=limit', limit=canonical_measurement(check['limit']),
            residual=canonical_measurement(check['residual']), measured_raw=raw,
            measured_canonical=canonical_measurement(raw), passed=True))
    return dict(schema='zero-replay-checks-1', case_id=CASE_ID, previous_case_hash=previous_hash,
                evidence=deepcopy(inventory), checks=rows)


def validate_replay_record(case):
    expected = dict(case_id=CASE_ID, attempt_consumed=True, native_attempt_count=1,
        source_kind='native', execution_status='completed', assessment_status='complete',
        author='SOLVERS', author_status='recorded', capture_role='diagnostic_replay')
    if any(type(case.get(k)) is not type(v) or case.get(k) != v for k, v in expected.items()):
        raise ValueError('diagnostic replay state differs')
    for role in ('replay_reference', 'check_reference', 'observation_reference'):
        ref = case.get(role)
        if not isinstance(ref, dict) or not ref.get('id') or not ref.get('sha256'):
            raise ValueError('diagnostic replay references required')
        if not any(r.get('id') == ref['id'] and r.get('sha256') == ref['sha256']
                   and r.get('required') is True for r in case.get('evidence', [])):
            raise ValueError('diagnostic replay evidence reference absent')
    rows = case.get('responses', [])
    names = {s + '.' + q for s, q in EXPECTED_KEYS} | {'support.RFY'}
    if len(rows) != 64 or {r.get('name') for r in rows} != names:
        raise ValueError('diagnostic replay requires 64 unique computed responses')
    for row in rows:
        if (row.get('calculation_status') != 'computed' or row.get('value') is None
                or canonical_measurement(row['value']) != row['value']
                or 'diagnostic-only' not in row.get('limitations', [])):
            raise ValueError('diagnostic replay response differs')
    if case.get('engineering_qualified') is not False:
        raise ValueError('diagnostic replay cannot qualify engineering')


def _baseline(baseline):
    validate_package(baseline)
    if (baseline['dataset_id'] != 'ansys-retained-evidence' or len(baseline['cases']) != 12
            or sum(len(c['responses']) for c in baseline['cases']) != 350):
        raise ValueError('expected fixed twelve-case matrix')
    case = baseline['cases'][8]
    if (case['case_id'] != CASE_ID or case['capture_role'] != 'diagnostic_observation'
            or case.get('assessment_status') != 'incomplete' or len(case['responses']) != 64
            or case.get('attempt_consumed') is not True or type(case.get('native_attempt_count')) is not int
            or case['native_attempt_count'] != 1
            or any(r['value'] is not None or r['calculation_status'] != 'failed' for r in case['responses'])):
        raise ValueError('one incomplete observed baseline required')
    return case


def _original(raw, old, resolver, documents):
    outcome = parse_json(raw['outcome.json'])
    if (outcome.get('status') != 'INCOMPLETE' or outcome.get('attempted') != [CASE_ID]
            or len(outcome.get('records', [])) != 1
            or outcome['records'][0].get('evidence_errors') != ['Unsupported CDB command OMEGA']
            or outcome['records'][0].get('values') != {}):
        raise ValueError('original incomplete outcome differs')
    observation = parse_json(verify_reference(old['observation_reference'], resolver))
    refs = observation['sources']
    for role, ref in refs.items():
        received = verify_reference(ref, resolver)
        if role in ('approval', 'config', 'runtime_manifest') and received != documents[role]:
            raise ValueError('replay document differs from observed authority')
    for role, path in [('outcome', 'outcome.json'), ('execution', CASE_ID + '/execution.json')]:
        if refs.get(role, {}).get('sha256') != digest_bytes(raw[path]):
            raise ValueError('original observation source differs')
    verify_original_artifacts(raw, outcome)
    return outcome


def _evaluate(resolved):
    observed_hash = observed_reference_hash(resolved)
    raw = resolved['raw']
    artifacts = {name.split('/')[-1]: value for name, value in raw.items() if name.startswith(CASE_ID + '/')}
    for old, new in [(CASE_ID + '.out', 'native.out'), ('file.err', 'jobname.err'),
                     ('stdout.bin', 'stdout'), ('stderr.bin', 'stderr')]:
        artifacts[new] = artifacts[old]
    case = build_case(CASE_ID)
    if artifacts[CASE_ID + '.inp'] != case['deck_bytes']:
        raise ValueError('executed deck differs from fixed zero case')
    result = validate_native_evidence(case, artifacts, REFERENCE_HASH, observed_hash)
    if result['status'] != 'COMPLETE' or result['errors']:
        raise ValueError('native replay extraction incomplete')
    measured = {key: str(value) for key, value in result['values'].items()}
    measured['RFY'] = str(result['rfy_sum'])
    table = parse_json(resolved['documents']['reference'])
    reference = {(row['station_id'], q): Decimal(v) for row in table['rows']
                 if row['pressure_mpa'] == '10' for q, v in row['values'].items()}
    attempt = dict(case_id=CASE_ID, values={k: Decimal(v) for k, v in measured.items() if k != 'RFY'},
                   rfy_sum=Decimal(measured['RFY']), evidence_errors=[])
    assessment = evaluate_attempt(**attempt, reference=reference)
    campaign = evaluate_canary([attempt], reference)
    if assessment['status'] != 'PASS' or len(assessment['checks']) != 64:
        raise ValueError('fixed zero-case assessment failed or incomplete')
    if campaign['status'] != 'INCOMPLETE' or len(campaign['unattempted']) != 3:
        raise ValueError('campaign attempt accounting differs')
    return measured, assessment['checks']


def derive_replay_case(baseline, replay_reference, resolver, *, review_sha256):
    old = _baseline(baseline)
    receipt, resolved, evidence = load_replay_inputs(replay_reference, resolver, old['row_hash'], review_sha256)
    _original(resolved['raw'], old, resolver, resolved['documents'])
    mapping = validate_mapping(parse_json(resolved['documents']['mapping']), old['responses'])
    measured, checks = _evaluate(resolved)
    check = make_check_receipt(old['row_hash'], evidence, checks, measured)
    check_hash = digest_bytes(canonical_bytes(check))
    case = deepcopy(old)
    case.pop('row_hash')
    case.update(capture_role='diagnostic_replay', assessment_status='complete', engineering_qualified=False,
        replay_reference=deepcopy(replay_reference), check_reference=dict(id='zero-check-' + check_hash[:32],
        sha256=check_hash, required=True), replay_checks=check,
        original_assessment=dict(status='INCOMPLETE', reason='unsupported-cdb-omega',
            current_disposition='superseded-by-reviewed-offline-replay'), campaign_assessment_status='INCOMPLETE')
    additions = [dict(replay_reference, role='replay'), dict(case['check_reference'], role='replay_checks'), *evidence]
    existing = {r['id']: r for r in case['evidence']}
    for ref in additions:
        if ref['id'] in existing and existing[ref['id']]['sha256'] != ref['sha256']:
            raise ValueError('replay evidence identity collision')
        existing[ref['id']] = ref
    case['evidence'] = list(existing.values())
    by_name = {v['name']: k for k, v in mapping.items()}
    for row in case['responses']:
        row.update(value=canonical_measurement(measured[by_name[row['name']]]), calculation_status='computed',
            limitations=[v for v in row['limitations'] if v != 'unsupported-cdb-omega'],
            evidence_ids=list(dict.fromkeys(row['evidence_ids'] + [replay_reference['id'], case['check_reference']['id']])))
    validate_replay_record(case)
    case['row_hash'] = digest_bytes(canonical_bytes(case))
    return case


def _coverage(baseline, case):
    cases = [case if i == 8 else c for i, c in enumerate(baseline['cases'])]
    coverage = deepcopy(baseline['coverage'])
    coverage.update(assessment_incomplete_cases=sum(c.get('assessment_status') == 'incomplete' for c in cases),
        assessment_failed_responses=sum(r['calculation_status'] == 'failed' for c in cases for r in c['responses']))
    # Historical failures remain visible; the replaced case is validated separately.
    return coverage


def validate_replay_transition(package, baseline, resolver, *, review_sha256):
    if len(package['cases']) != 12:
        raise ValueError('replay case count differs')
    for i in [*range(8), 9, 10, 11]:
        if canonical_bytes(package['cases'][i]) != canonical_bytes(baseline['cases'][i]):
            raise ValueError('other historical case changed')
    actual = package['cases'][8]
    expected = derive_replay_case(baseline, actual['replay_reference'], resolver, review_sha256=review_sha256)
    if canonical_bytes(actual) != canonical_bytes(expected) or package['coverage'] != _coverage(baseline, expected):
        raise ValueError('replay transition differs from deterministic derivation')


def build_replay_package(baseline, resolver, *, replay_reference, revision, code_revision, review_sha256):
    case = derive_replay_case(baseline, replay_reference, resolver, review_sha256=review_sha256)
    if revision == baseline['revision']:
        raise ValueError('new immutable revision required')
    study = deepcopy(baseline)
    study.pop('package_hash')
    study.update(revision=revision, previous_package_hash=baseline['package_hash'],
                 code_revision=nonempty(code_revision, 'code revision'), method_revision='zero-diagnostic-replay-1')
    study['cases'][8], study['coverage'] = case, _coverage(baseline, case)
    for row in study['cases']:
        row.pop('row_hash')
    package = build_package(study, resolver)
    validate_replay_transition(package, baseline, resolver, review_sha256=review_sha256)
    return package
