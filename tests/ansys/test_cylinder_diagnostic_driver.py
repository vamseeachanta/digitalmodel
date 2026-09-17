"""Synthetic operator orchestration: never acquire a real seat or launch ANSYS."""
import importlib.util
from pathlib import Path

import pytest
from digitalmodel.ansys import cylinder_canary
from digitalmodel.ansys import cylinder_diagnostic_preflight

from tests.ansys.test_cylinder_canary import protocol
from tests.ansys.test_cylinder_diagnostic_admission import admission_inputs
from tests.ansys.test_cylinder_diagnostic_integration import diagnostic_protocol


SCRIPT = Path(__file__).resolve().parents[2] / 'scripts/ansys/run_zero_control_diagnostic.py'
SPEC = importlib.util.spec_from_file_location('synthetic_diagnostic_driver', SCRIPT)
driver = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(driver)


class SyntheticReservation:
    def __init__(self):
        self.releases = []
        self.changed = False

    def evidence(self):
        if self.changed:
            raise ValueError('synthetic reservation changed')
        return {'pid': 123, 'device': 1, 'inode': 2, 'content_sha256': 'a' * 64,
                'path': 'synthetic-only.lock'}

    def release(self, *, no_owned_processes):
        self.releases.append(no_owned_processes)
        return no_owned_processes


@pytest.fixture
def operation(diagnostic_protocol, monkeypatch):
    bundle, output, approval, callbacks, calls, inputs = diagnostic_protocol
    for key, value in approval['launch_environment'].items():
        monkeypatch.setenv(key, value)
    admission = {key: callbacks[key] for key in ('verify_authority', 'adjudicate')}
    execution = {key: callbacks[key] for key in ('launch', 'extract', 'assess', 'verify_reference')}
    arguments = dict(bundle=bundle, output=output, approval=approval, admission=admission,
                     preflight=callbacks['preflight'], execution=execution,
                     reservation=SyntheticReservation())
    return arguments, calls


def test_settled_zero_releases_only_after_one_native_adapter_return(operation):
    args, calls = operation
    result = driver.execute_diagnostic(**args)
    assert calls == ['ocv-zero-t60-n16']
    assert result['outcome']['status'] == 'INCOMPLETE'
    assert result['reservation_released'] is True
    assert args['reservation'].releases == [True]


@pytest.mark.parametrize('damage', ['raises', 'unknown_remaining', 'retained_supervisor'])
def test_uncertain_launch_retains_reservation(operation, damage):
    args, calls = operation
    original = args['execution']['launch']

    def uncertain(*parameters):
        result = original(*parameters)
        if damage == 'raises':
            raise OSError('synthetic uncertainty after entering launch adapter')
        if damage == 'unknown_remaining':
            result['owned_processes_remaining'] = None
        else:
            result['retained_supervisor_token'] = 'synthetic-retained-job'
        return result

    args['execution']['launch'] = uncertain
    result = driver.execute_diagnostic(**args)
    assert len(calls) == 1
    assert result['reservation_released'] is False
    assert args['reservation'].releases == [False]


def test_environment_refusal_before_launch_releases_without_consumption(operation, monkeypatch):
    args, calls = operation
    monkeypatch.setenv('ANSYS261_PRODUCT', 'different')
    result = driver.execute_diagnostic(**args)
    assert calls == []
    assert result['outcome']['attempted'] == []
    assert args['reservation'].releases == [True]


def test_environment_changed_during_preflight_is_checked_again(operation, monkeypatch):
    args, calls = operation
    original = args['preflight']

    def changed(approval):
        evidence = original(approval)
        monkeypatch.setenv('ANS_CONSEC', 'different')
        return evidence

    args['preflight'] = changed
    result = driver.execute_diagnostic(**args)
    assert calls == []
    assert result['outcome']['status'] == 'INCOMPLETE'
    assert args['reservation'].releases == [True]


def test_lock_changed_during_preflight_prevents_launch(operation):
    args, calls = operation
    original = args['preflight']

    def changed(approval):
        evidence = original(approval)
        args['reservation'].changed = True
        return evidence

    args['preflight'] = changed
    result = driver.execute_diagnostic(**args)
    assert calls == []
    assert result['outcome']['status'] == 'INCOMPLETE'
    assert args['reservation'].releases == [True]


def test_environment_changed_after_claim_consumes_without_native(operation, monkeypatch):
    args, calls = operation
    original = cylinder_canary._claim

    def changed(*parameters):
        original(*parameters)
        monkeypatch.setenv('ANS_CONSEC', 'different')

    monkeypatch.setattr(cylinder_canary, '_claim', changed)
    result = driver.execute_diagnostic(**args)
    assert calls == []
    assert result['outcome']['attempted'] == ['ocv-zero-t60-n16']
    assert result['outcome']['status'] == 'INCOMPLETE'
    assert list(Path(args['approval']['ledger_directory']).glob('*.json'))
    assert args['reservation'].releases == [True]


def test_failed_preflight_observation_is_retained(operation):
    args, calls = operation

    def refused(approval):
        raise ValueError('synthetic licence refusal')

    refused.last_evidence = {'license_query': {'return_code': 1, 'stdout_base64': ''}}
    args['preflight'] = refused
    result = driver.execute_diagnostic(**args)
    assert calls == []
    assert result['preflight_observation'] == refused.last_evidence


def test_main_passes_pinned_operational_object_to_preflight(operation, monkeypatch):
    args, calls = operation
    operational = {'bundle': str(args['bundle']), 'executable': 'synthetic-not-executed',
                   'lock_path': 'synthetic-not-acquired', 'output_directory': str(args['output'])}
    seen = []
    monkeypatch.setattr(driver, '_bound_inputs', lambda ignored:
        ({'operational': operational}, args['approval'], args['admission']))
    monkeypatch.setattr(driver, 'make_execution_adapters', lambda *ignored: args['execution'])
    monkeypatch.setattr(driver, 'acquire_local_reservation', lambda ignored: args['reservation'])

    def factory(config, approval, reservation):
        seen.append(config)
        return args['preflight']

    monkeypatch.setattr(cylinder_diagnostic_preflight, 'ProductionPreflight', factory)
    assert driver.main(['config', 'receipt', 'stdout', 'bundle',
                        '--config-sha256', 'synthetic', '--review-sha256', 'synthetic']) == 0
    assert seen == [operational]
    assert calls == ['ocv-zero-t60-n16']


@pytest.mark.parametrize('damage', ['missing_record', 'wrong_reason', 'failed_zero', 'retained_seat'])
def test_cli_success_requires_retained_valid_zero_and_planned_stop(damage):
    result = {'outcome': {'status': 'INCOMPLETE', 'records': [{'case_id': 'ocv-zero-t60-n16'}],
                         'attempted': ['ocv-zero-t60-n16'],
                         'reason': 'diagnostic scope exhausted: durable claim consumed'},
              'reservation_released': True, 'no_owned_processes_established': True}
    assert driver.diagnostic_exit_code(result) == 0
    if damage == 'missing_record':
        result['outcome']['records'] = []
    elif damage == 'wrong_reason':
        result['outcome']['reason'] = 'missing evidence'
    elif damage == 'failed_zero':
        result['outcome']['status'] = 'FAIL'
    else:
        result['reservation_released'] = False
    assert driver.diagnostic_exit_code(result) != 0
