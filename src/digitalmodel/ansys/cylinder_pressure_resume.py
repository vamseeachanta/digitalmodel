"""Single selected pressure diagnostic capture; injected adapters are explicit trust boundaries."""
from copy import deepcopy
import os
from pathlib import Path
import time

from digitalmodel.ansys.analysis_records import digest_bytes
from digitalmodel.ansys.cylinder_benchmark import validate_deck
from digitalmodel.ansys.cylinder_canary import _manifest, _record_execution
from digitalmodel.ansys.cylinder_criteria import EXPECTED_KEYS
from digitalmodel.ansys.cylinder_pressure_journal import (
    InvocationClock, PressureJournal, owned_path, write_exclusive,
)
from digitalmodel.ansys.cylinder_pressure_resources import validate_observation_ages
from digitalmodel.ansys.cylinder_pressure_scope import pressure_step
from digitalmodel.ansys.cylinder_intermediate_lineage import replay_coarse_predecessor
from digitalmodel.ansys.cylinder_preparation_streams import verify_streams

CASE_ID = 'ocv-t60-p10-n4'
_write_exclusive = write_exclusive


def _empty(ordinal=2, case_id=CASE_ID):
    return dict(schema='cylinder-pressure-resume-1', case_id=case_id,
                terminal_reason='PRECLAIM_REFUSAL', consumed_count=ordinal - 1,
                native_launch_count=0, launch_adapter_calls=0,
                launch_scope='this invocation only; null means unestablished',
                recording_boundary='terminal/outcome writes follow reservation release',
                accepted_values={}, assessment_status='NOT_EVALUATED',
                assessment_reason=('PRESSURE_PARSER_NOT_VALIDATED' if ordinal == 2
                    else 'PRESSURE_NUMERICAL_ASSESSMENT_NOT_EVALUATED'),
                campaign_status='INCOMPLETE', engineering_qualified=False,
                no_owned_processes_established=True, reservation_released=False)


def _scope(config):
    return pressure_step(config)


def _prefix(case):
    receipt = case.get('replay_checks', {})
    checks = receipt.get('checks')
    if (case.get('case_id') != 'ocv-zero-t60-n16'
            or case.get('assessment_status') != 'complete'
            or receipt.get('schema') != 'zero-replay-checks-1'
            or receipt.get('case_id') != 'ocv-zero-t60-n16'
            or not isinstance(checks, list) or len(checks) != 64
            or any(row.get('passed') is not True for row in checks)):
        raise ValueError('64 successful freshly derived zero checks required')
    responses = []
    for row in checks:
        value = row.get('response')
        if isinstance(value, list) and len(value) == 2 and all(type(v) is str for v in value):
            value = tuple(value)
        elif value != 'RFY':
            raise ValueError('zero check identity format differs')
        responses.append(value)
    if set(responses) != EXPECTED_KEYS | {'RFY'} or len(set(responses)) != 64:
        raise ValueError('zero check identities missing, foreign or duplicated')


def _verify(admission, approval):
    if admission['verify_authority'](deepcopy(approval)) is not True:
        raise ValueError('current independent authority not established')


def _prepare_case(config, approval, output):
    bundle = owned_path(config['operational']['bundle'])
    manifest = _manifest(bundle, approval)
    _, case_id = pressure_step(config)
    matches = [row for row in manifest['cases'] if row['case_id'] == case_id]
    if len(matches) != 1:
        raise ValueError('selected pressure case missing or duplicated')
    case = matches[0]
    directory = output / case_id
    directory.mkdir(exist_ok=False)
    raw = (bundle / case['deck']).read_bytes()
    validate_deck(case_id, raw)
    target = directory / Path(case['deck']).name
    with target.open('xb') as stream:
        stream.write(raw)
        stream.flush()
        os.fsync(stream.fileno())
    if target.read_bytes() != raw:
        raise OSError('pressure deck copy differs')
    return case, directory


def _settled(execution):
    return (type(execution.get('owned_processes_remaining')) is int
            and execution['owned_processes_remaining'] == 0
            and execution.get('streams_finalized') is True
            and not execution.get('retained_supervisor_token'))


def _successful_execution(execution):
    return (_settled(execution) and execution.get('return_code') == 0
            and execution.get('timed_out') is False
            and execution.get('containment_verified') is True
            and execution.get('evidence_complete') is True
            and execution.get('settlement_required') is False
            and execution.get('error') == [])


def _observe(state, adapters):
    approval, output = state['approval'], state['output']
    _verify(adapters['admission'], approval)
    case = adapters['replay_prefix'](deepcopy(approval))
    _prefix(case)
    _write_exclusive(output / 'prefix-replay.json', case)
    if state['ordinal'] == 3:
        coarse = replay_coarse_predecessor(state['config'])
        _write_exclusive(output / 'coarse-prefix-replay.json', coarse)
    adapters['preflight'](deepcopy(approval))
    evidence = adapters['preflight'].before_launch()
    _write_exclusive(output / 'preflight.json', evidence)
    return evidence


def _storage(adapters):
    evidence = adapters['account_storage']()
    if not isinstance(evidence, dict) or evidence.get('status') != 'PASS':
        raise ValueError('cumulative storage allowance or reserve failed')
    return evidence


def _ages(state, preflight, maximum):
    now_ns = state['clock'].time_ns()
    now = str(now_ns // 1_000_000_000) + '.' + f'{now_ns % 1_000_000_000:09d}'
    return validate_observation_ages(preflight.last_evidence, preflight._ready_at, now, maximum)


def _probe_before_claim(state, adapters):
    if state['ordinal'] != 3:
        return
    preflight = adapters['preflight']
    record = dict(status='REFUSED', maximum_postclaim_ns=5_000_000_000,
        multiplier=2, unmeasured_write_reserve_ns=1_000_000_000,
        limitation='Measured scheduling heuristic; final hard stop still applies.',
        pre_probe_evidence=deepcopy(preflight.last_evidence))
    state['result']['preclaim_probe'] = record
    ready, licence = preflight._ready_at, deepcopy(preflight.last_evidence['license_observation'])
    _ages(state, preflight, 20)
    started = state['clock'].monotonic_ns()
    try:
        callback = adapters.get('preclaim_recheck')
        if not callable(callback):
            raise ValueError('intermediate preclaim production probe required')
        record['checked_evidence'] = callback(deepcopy(state['approval']))
        if (not isinstance(record['checked_evidence'], dict)
                or record['checked_evidence'].get('status') != 'PASS'):
            raise ValueError('preclaim resource probe did not pass')
        record['storage_evidence'] = _storage(adapters)
    finally:
        record['elapsed_ns'] = state['clock'].monotonic_ns() - started
    elapsed = record['elapsed_ns']
    if type(elapsed) is not int or not 0 <= elapsed <= 2_000_000_000:
        raise ValueError('preclaim measured workload exceeds scheduling allowance')
    if preflight._ready_at != ready or preflight.last_evidence['license_observation'] != licence:
        raise ValueError('preclaim probe changed observation authority')
    _ages(state, preflight, 20)
    record['status'] = 'PASS'


def _prepare_launch(state, adapters):
    state['case'], state['directory'] = _prepare_case(
        state['config'], state['approval'], state['output'])
    state['result']['preclaim_storage'] = _storage(adapters)
    _probe_before_claim(state, adapters)
    _ages(state, adapters['preflight'], 20)
    if state['window'].remaining_seconds() < 365:
        raise ValueError('less than 365 seconds before successor invocation')


def _claim_and_launch(state, adapters):
    journal, window, result = state['journal'], state['window'], state['result']
    case, directory, preflight = state['case'], state['directory'], adapters['preflight']
    _ages(state, preflight, 20)
    if window.remaining_seconds() < 365:
        raise ValueError('less than 365 seconds before successor claim')
    claim_start = state['clock'].monotonic_ns()
    attempt = dict(ordinal=state['ordinal'], case_id=state['case_id'], state='attempt_consumed',
        parent_sha256=journal.parent_sha256, output=str(state['output']),
        input_sha256=digest_bytes((directory / Path(case['deck']).name).read_bytes()),
        config_sha256=state['approval']['config_sha256'],
        review_receipt_sha256=state['approval']['review_receipt_sha256'])
    journal.claim(attempt)
    result['consumed_count'] = state['ordinal']
    _write_exclusive(state['output'] / f"attempt-{state['ordinal']}.json", attempt)
    result['phase2_evidence'] = adapters['phase2_recheck'](deepcopy(state['approval']))
    result['postclaim_storage'] = _storage(adapters)
    _ages(state, preflight, 30)
    remaining = window.remaining_seconds()
    elapsed = state['clock'].monotonic_ns() - claim_start
    if not 0 <= elapsed <= 5_000_000_000 or remaining < 360:
        raise ValueError('postclaim interval or launcher allowance exceeded')
    result['no_owned_processes_established'] = False
    result['launch_adapter_calls'] = 1
    result['native_launch_count'] = None
    execution = adapters['launch'](case, directory, 300)
    if execution.get('containment_verified') is True:
        result['native_launch_count'] = 1
    result['no_owned_processes_established'] = _settled(execution)
    state['execution'] = execution
    state['directory'] = directory
    state['case'] = case
    return execution


def _retain(state, adapters, execution):
    result, directory = state['result'], state['directory']
    try:
        _record_execution(directory, execution)
    except Exception as error:
        result['execution_retention_error'] = f'{type(error).__name__}: {error}'
    capture = adapters['capture'](state['case'], directory, execution)
    result['capture'] = capture
    _write_exclusive(directory / 'capture.json', capture)
    remaining = state['window'].remaining_seconds()
    reasons = []
    if not _successful_execution(execution):
        reasons.append('EXECUTION_INCOMPLETE')
    if result.get('execution_retention_error'):
        reasons.append('RETENTION_INCOMPLETE')
    if not (capture.get('retention_status') == 'COMPLETE'
          and capture.get('independent_check_status') == 'COMPLETE'
          and capture.get('accepted_values') == {}
          and capture.get('engineering_qualified') is False
          and capture.get('capture_status') == 'INCOMPLETE'):
        reasons.append('CAPTURE_CHECKS_INCOMPLETE')
    if state['ordinal'] == 3 and (capture.get('case_id') != state['case_id']
            or capture.get('numerical_assessment') != 'NOT_EVALUATED'
            or capture.get('reason') != 'PRESSURE_NUMERICAL_ASSESSMENT_NOT_EVALUATED'):
        reasons.append('CAPTURE_CHECKS_INCOMPLETE')
    if remaining < 0:
        reasons.append('DEADLINE_EXCEEDED')
    result['terminal_reasons'] = list(dict.fromkeys(reasons)) or ['PLANNED_SCOPE_STOP']
    result['terminal_reason'] = result['terminal_reasons'][0]


def _release(state, reservation):
    result = state['result']
    try:
        result['reservation_released'] = reservation.release(
            no_owned_processes=result['no_owned_processes_established'])
    except Exception as error:
        result['reservation_release_error'] = f'{type(error).__name__}: {error}'


def _additional_reason(result, reason):
    prior = result.get('terminal_reasons', [result['terminal_reason']])
    prior = [item for item in prior if item != 'PLANNED_SCOPE_STOP']
    result['terminal_reasons'] = list(dict.fromkeys([*prior, reason]))
    result['terminal_reason'] = result['terminal_reasons'][0]


def _final_accounting(state, adapters):
    if 'output' not in state:
        return
    result = state['result']
    result['accounting_boundary'] = 'before terminal/outcome writes and reservation release'
    try:
        evidence = adapters['account_storage']()
        result['final_storage_before_receipts'] = evidence
        if not isinstance(evidence, dict) or evidence.get('status') != 'PASS':
            _additional_reason(result, 'STORAGE_INCOMPLETE')
        if state['window'].remaining_seconds() < 0:
            _additional_reason(result, 'DEADLINE_EXCEEDED')
    except Exception as error:
        result['final_accounting_error'] = f'{type(error).__name__}: {error}'
        _additional_reason(result, 'FINAL_ACCOUNTING_INCOMPLETE')


def _terminal_records(state):
    journal, result = state['journal'], state['result']
    if not journal.started:
        if state['ordinal'] == 3 and 'output' in state:
            try:
                result['preparation_binding'] = dict(
                    streams=verify_streams(state['config']),
                    config_sha256=state['approval']['config_sha256'], ordinal=state['ordinal'],
                    output=str(state['output']), parent_claim_sha256=journal.parent_sha256)
                _write_exclusive(state['output'] / 'preparation-refusal.json', deepcopy(result))
            except Exception as error:
                result['preparation_record_error'] = f'{type(error).__name__}: {error}'
                _additional_reason(result, 'PREPARATION_RETENTION_INCOMPLETE')
        return
    try:
        journal.terminal(result)
    except Exception as error:
        result['terminal_record_error'] = f'{type(error).__name__}: {error}'
        _additional_reason(result, 'TERMINAL_RETENTION_INCOMPLETE')
    try:
        _write_exclusive(state['output'] / 'outcome.json', result)
    except Exception as error:
        result['outcome_record_error'] = f'{type(error).__name__}: {error}'
        _additional_reason(result, 'OUTCOME_RETENTION_INCOMPLETE')


def _start_invocation(state):
    approval = state['approval']
    state['journal'].start(dict(state['window'].record(), ordinal=state['ordinal'],
        case_id=state['case_id'], parent_sha256=state['journal'].parent_sha256,
        output=str(state['output']), config_sha256=approval['config_sha256'],
        review_receipt_sha256=approval['review_receipt_sha256']))


def _run_prepared(state, adapters):
    if state['ordinal'] == 2:
        _start_invocation(state)
    _observe(state, adapters)
    _prepare_launch(state, adapters)
    if state['ordinal'] == 3:
        _start_invocation(state)
    _retain(state, adapters, _claim_and_launch(state, adapters))


def execute_pressure_resume(config, admission, *, replay_prefix, preflight,
                            phase2_recheck, launch, capture, clock=time,
                            reservation, account_storage, preclaim_recheck=None):
    """One call only; production driver must bind each required reviewed adapter."""
    try:
        ordinal, case_id = _scope(config)
        parent = config['lineage']['parent_claim']
        journal = PressureJournal(parent['path'], parent['sha256'], writer=_write_exclusive,
            ordinal=ordinal, predecessor=config['predecessor']['claim'] if ordinal == 3 else None)
    except BaseException as error:
        reservation.release(no_owned_processes=True)  # No native adapter was entered.
        if isinstance(error, (OSError, ValueError)):
            raise ValueError('existing or invalid ordinal lineage; no readmission') from error
        raise
    state = dict(config=config, approval=deepcopy(admission['approval']), journal=journal,
                 clock=clock, ordinal=ordinal, case_id=case_id, result=_empty(ordinal, case_id))
    adapters = dict(admission=admission, replay_prefix=replay_prefix, preflight=preflight,
                    phase2_recheck=phase2_recheck, launch=launch, capture=capture,
                    account_storage=account_storage, preclaim_recheck=preclaim_recheck)
    try:
        _verify(admission, state['approval'])
        reservation.evidence()
        output = owned_path(config['operational']['output_directory'])
        output.mkdir(exist_ok=False)
        state.update(output=output, window=InvocationClock(clock))
        _run_prepared(state, adapters)
    except Exception as error:
        state['result'].update(terminal_reason='POSTCLAIM_INCOMPLETE' if journal.consumed
            else 'PRECLAIM_REFUSAL', error=f'{type(error).__name__}: {error}')
    finally:
        state['result']['consumed_count'] = ordinal if journal.consumed else ordinal - 1
        state['result']['last_preflight_evidence'] = deepcopy(
            getattr(preflight, 'last_evidence', {}))
        _final_accounting(state, adapters)
        _release(state, reservation)
    result = state['result']
    _terminal_records(state)
    return result
