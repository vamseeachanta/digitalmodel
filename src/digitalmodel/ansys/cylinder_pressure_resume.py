"""Single ordinal-2 diagnostic capture; injected adapters are explicit trust boundaries."""
from copy import deepcopy
import os
from pathlib import Path
import time

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys.cylinder_benchmark import validate_deck
from digitalmodel.ansys.cylinder_canary import _manifest, _record_execution
from digitalmodel.ansys.cylinder_criteria import EXPECTED_KEYS
from digitalmodel.ansys.cylinder_pressure_journal import (
    InvocationClock, PressureJournal, owned_path, write_exclusive,
)
from digitalmodel.ansys.cylinder_pressure_resources import validate_observation_ages

CASE_ID = 'ocv-t60-p10-n4'
_write_exclusive = write_exclusive


def _empty():
    return dict(schema='cylinder-pressure-resume-1', case_id=CASE_ID,
                terminal_reason='PRECLAIM_REFUSAL', consumed_count=1,
                native_launch_count=0, launch_adapter_calls=0,
                launch_scope='this invocation only; null means unestablished',
                accepted_values={}, assessment_status='NOT_EVALUATED',
                assessment_reason='PRESSURE_PARSER_NOT_VALIDATED',
                campaign_status='INCOMPLETE', engineering_qualified=False,
                no_owned_processes_established=True, reservation_released=False)


def _scope(config):
    expected = dict(case_ids=[CASE_ID], ordinal=2, max_attempts=1,
                    capture_only=True, qualification='diagnostic_only')
    if canonical_bytes(config['scope']) != canonical_bytes(expected):
        raise ValueError('fixed ordinal-2 capture scope required')


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
    case = manifest['cases'][1]
    if case['case_id'] != CASE_ID:
        raise ValueError('ordinal-2 case differs')
    directory = output / CASE_ID
    directory.mkdir(exist_ok=False)
    raw = (bundle / case['deck']).read_bytes()
    validate_deck(CASE_ID, raw)
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


def _claim_and_launch(state, adapters):
    journal, window, result = state['journal'], state['window'], state['result']
    case, directory = _prepare_case(state['config'], state['approval'], state['output'])
    result['preclaim_storage'] = _storage(adapters)
    preflight = adapters['preflight']
    _ages(state, preflight, 20)
    if window.remaining_seconds() < 365:
        raise ValueError('less than 365 seconds before successor claim')
    claim_start = state['clock'].monotonic_ns()
    attempt = dict(ordinal=2, case_id=CASE_ID, state='attempt_consumed',
        parent_sha256=journal.parent_sha256, output=str(state['output']),
        input_sha256=digest_bytes((directory / Path(case['deck']).name).read_bytes()),
        config_sha256=state['approval']['config_sha256'],
        review_receipt_sha256=state['approval']['review_receipt_sha256'])
    journal.claim(attempt)
    result['consumed_count'] = 2
    _write_exclusive(state['output'] / 'attempt-2.json', attempt)
    adapters['phase2_recheck'](deepcopy(state['approval']))
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
    if remaining < 0:
        reasons.append('DEADLINE_EXCEEDED')
    result['terminal_reasons'] = reasons or ['PLANNED_SCOPE_STOP']
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


def execute_pressure_resume(config, admission, *, replay_prefix, preflight,
                            phase2_recheck, launch, capture, clock=time,
                            reservation, account_storage):
    """One call only; production driver must bind each required reviewed adapter."""
    try:
        _scope(config)
        parent = config['lineage']['parent_claim']
        journal = PressureJournal(parent['path'], parent['sha256'], writer=_write_exclusive)
    except BaseException as error:
        reservation.release(no_owned_processes=True)  # No native adapter was entered.
        if isinstance(error, (OSError, ValueError)):
            raise ValueError('existing or invalid ordinal lineage; no readmission') from error
        raise
    state = dict(config=config, approval=deepcopy(admission['approval']), journal=journal,
                 clock=clock, result=_empty())
    adapters = dict(admission=admission, replay_prefix=replay_prefix, preflight=preflight,
                    phase2_recheck=phase2_recheck, launch=launch, capture=capture,
                    account_storage=account_storage)
    try:
        _verify(admission, state['approval'])
        reservation.evidence()
        output = owned_path(config['operational']['output_directory'])
        output.mkdir(exist_ok=False)
        state.update(output=output, window=InvocationClock(clock))
        journal.start(dict(state['window'].record(), ordinal=2, case_id=CASE_ID,
            parent_sha256=parent['sha256'], output=str(output),
            config_sha256=state['approval']['config_sha256'],
            review_receipt_sha256=state['approval']['review_receipt_sha256']))
        _observe(state, adapters)
        execution = _claim_and_launch(state, adapters)
        _retain(state, adapters, execution)
    except Exception as error:
        state['result'].update(terminal_reason='POSTCLAIM_INCOMPLETE' if journal.consumed
            else 'PRECLAIM_REFUSAL', error=f'{type(error).__name__}: {error}')
    finally:
        state['result']['consumed_count'] = 2 if journal.consumed else 1
        _final_accounting(state, adapters)
        _release(state, reservation)
    result = state['result']
    if journal.started:
        journal.terminal(result)
        _write_exclusive(state['output'] / 'outcome.json', result)
    return result
