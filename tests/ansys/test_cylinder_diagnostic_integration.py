"""Synthetic admission/runner integration; no native, provider or engineering authority."""
import copy
import json

import pytest

from digitalmodel.ansys import cylinder_canary
from digitalmodel.ansys.cylinder_canary import run_canary
from tests.ansys.test_cylinder_canary import protocol, ORDER
from tests.ansys.test_cylinder_diagnostic_admission import (
    admission_inputs, claim_path, make, refresh_bundle, LAUNCH_ENVIRONMENT,
)


@pytest.fixture
def diagnostic_protocol(protocol, admission_inputs):
    bundle, output, original_approval, callbacks, calls = protocol
    inputs = admission_inputs
    fields = ('manifest_sha256', 'executable_sha256', 'execution_host', 'profile',
              'capture_allowance_bytes', 'reserve_bytes')
    binding = {key: copy.deepcopy(original_approval[key]) for key in fields}
    binding['runtime_profile'] = copy.deepcopy(inputs['config']['execution_binding']['runtime_profile'])
    binding['launch_environment'] = copy.deepcopy(LAUNCH_ENVIRONMENT)
    inputs['config']['execution_binding'] = binding
    refresh_bundle(inputs)
    approval = dict(binding, approval_id=inputs['config']['campaign_id'],
                    operator_id=inputs['config']['operator_id'],
                    checker_id=inputs['transport']['session_id'],
                    ledger_directory=inputs['config']['ledger_directory'],
                    config_sha256=inputs['expected_config_sha256'],
                    review_receipt_sha256=inputs['expected_review_sha256'])
    adapters = make(inputs)
    callbacks['verify_authority'] = adapters['verify_authority']
    callbacks['adjudicate'] = adapters['adjudicate']
    return bundle, output, approval, callbacks, calls, inputs


def test_valid_zero_then_durable_scope_stop_records_one_launch(diagnostic_protocol):
    bundle, output, approval, callbacks, calls, inputs = diagnostic_protocol
    callbacks['assess'] = lambda records: {'status': 'PASS', 'checks': ['synthetic-zero-pass']}
    outcome = run_canary(bundle, output, approval, **callbacks)
    assert calls == ORDER[:1]
    assert outcome['attempted'] == ORDER[:1]
    assert outcome['unattempted'] == ORDER[1:]
    assert outcome['status'] == 'INCOMPLETE'
    assert 'scope' in outcome['reason'].lower() or 'consum' in outcome['reason'].lower()
    assert outcome['records'] == [{'case_id': ORDER[0]}]
    assert outcome['checks'] == ['synthetic-zero-pass']
    assert 'adjudication' not in outcome
    assert claim_path(inputs).is_file()
    assert not (output / 'attempt-2.json').exists()
    assert json.loads((output / 'outcome.json').read_bytes()) == outcome
    with pytest.raises(ValueError):
        make(inputs)


def test_zero_failure_is_fail_not_planned_scope_incomplete(diagnostic_protocol):
    bundle, output, approval, callbacks, calls, inputs = diagnostic_protocol
    callbacks['assess'] = lambda records: {'status': 'FAIL', 'checks': ['synthetic-zero-fail']}
    outcome = run_canary(bundle, output, approval, **callbacks)
    assert outcome['status'] == 'FAIL'
    assert calls == ORDER[:1]
    assert outcome['attempted'] == ORDER[:1]
    assert outcome['unattempted'] == ORDER[1:]
    assert outcome['checks'] == ['synthetic-zero-fail']
    assert 'adjudication' not in outcome
    assert claim_path(inputs).exists()


def test_preflight_refusal_does_not_consume_claim_or_launch(diagnostic_protocol):
    bundle, output, approval, callbacks, calls, inputs = diagnostic_protocol
    original = callbacks['preflight']
    callbacks['preflight'] = lambda a: dict(original(a), exclusive_seat_owner='synthetic-other')
    outcome = run_canary(bundle, output, approval, **callbacks)
    assert outcome['status'] == 'INCOMPLETE'
    assert calls == []
    assert outcome['attempted'] == []
    assert outcome['unattempted'] == ORDER
    assert not claim_path(inputs).exists()
    assert not list(output.glob('attempt-*.json'))


def test_claim_survives_journal_exception_and_refuses_restart(diagnostic_protocol, monkeypatch):
    bundle, output, approval, callbacks, calls, inputs = diagnostic_protocol
    original_save = cylinder_canary._save
    def fail_attempt_journal(path, record):
        if path.name == 'attempt-1.json':
            raise OSError('synthetic journal failure after durable claim')
        return original_save(path, record)
    monkeypatch.setattr(cylinder_canary, '_save', fail_attempt_journal)
    outcome = run_canary(bundle, output, approval, **callbacks)
    assert outcome['status'] == 'INCOMPLETE'
    assert calls == []
    assert outcome['attempted'] == []
    assert outcome['unattempted'] == ORDER
    assert claim_path(inputs).is_file()
    assert not (output / 'attempt-1.json').exists()
    claim = json.loads(claim_path(inputs).read_bytes())
    assert claim['approval_id'] == approval['approval_id']
    # No recorded launch is not proof of reusable allowance after a durable claim.
    with pytest.raises(ValueError):
        make(inputs)
    assert json.loads(claim_path(inputs).read_bytes()) == claim


@pytest.mark.parametrize('field,value', [('manifest_sha256', 'f' * 64),
                                       ('approval_id', 'synthetic-new-campaign')])
def test_changed_approval_refuses_before_claim(diagnostic_protocol, field, value):
    bundle, output, approval, callbacks, calls, inputs = diagnostic_protocol
    changed = dict(approval, **{field: value})
    outcome = run_canary(bundle, output, changed, **callbacks)
    assert outcome['status'] == 'INCOMPLETE'
    assert calls == []
    assert outcome['unattempted'] == ORDER
    assert not claim_path(inputs).exists()


def test_nonnumerical_launch_failure_consumes_scope_without_readmission(diagnostic_protocol):
    bundle, output, approval, callbacks, calls, inputs = diagnostic_protocol
    def fail_launch(case, directory, timeout):
        calls.append(case['case_id'])
        raise OSError('synthetic launch transport failure; no native process')
    callbacks['launch'] = fail_launch
    outcome = run_canary(bundle, output, approval, **callbacks)
    assert outcome['status'] == 'INCOMPLETE'
    assert calls == ORDER[:1]
    assert claim_path(inputs).is_file()
    assert (output / 'attempt-1.json').is_file()
    assert not (output / 'attempt-2.json').exists()
    with pytest.raises(ValueError):
        make(inputs)
