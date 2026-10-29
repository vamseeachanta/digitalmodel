"""Synthetic failure transitions from issue 2128; no native execution.

Expected dispositions follow the issue's consumed-attempt/no-retry contract:
one synthetic launch consumes one attempt; a recording failure cannot remove
that attempt or imply completion. Counts are event counts, not engineering
constants. No numerical model or acceptance tolerance is introduced.
"""
import json
import os
from pathlib import Path

import pytest

from digitalmodel.ansys import cylinder_canary as canary
from digitalmodel.ansys import cylinder_pressure_resume as resume
from tests.ansys.test_cylinder_canary import ORDER, protocol as protocol
from tests.ansys.test_cylinder_diagnostic_driver import SyntheticReservation, driver
from tests.ansys.test_cylinder_intermediate_resume import intermediate as intermediate
from tests.ansys.test_cylinder_pressure_resume import harness as harness


@pytest.fixture(autouse=True)
def synthetic_junction_api(monkeypatch):
    """Provide the missing API only for synthetic non-Windows retention tests.

    These temporary trees cannot contain Windows junctions. Python 3.12+ and
    Windows keep the real implementation; symlink checks remain real everywhere.
    Hash-bound runtime sources and the existing platform gate stay unchanged.
    """
    if os.name != 'nt' and not hasattr(Path, 'is_junction'):
        monkeypatch.setattr(Path, 'is_junction', lambda self: False, raising=False)


@pytest.mark.parametrize('execution_failure', [False, True])
@pytest.mark.parametrize('write_failure', ['before', 'partial', 'readback', 'existing'])
def test_canary_outcome_failure_returns_consumed_disposition(
        protocol, monkeypatch, execution_failure, write_failure):
    """Hand trace: failure at first launch => one attempt, otherwise all four.

    Outcome writes occur after these events and therefore cannot change their
    count. Existing and partial evidence must remain byte-for-byte unchanged.
    """
    bundle, output, approval, callbacks, calls = protocol
    original_save = canary._save
    writes = []

    def save(path, value):
        if path.name != 'outcome.json':
            return original_save(path, value)
        writes.append(path)
        if write_failure in ('partial', 'existing'):
            path.write_bytes(b'retained incomplete evidence')
        elif write_failure == 'readback':
            original_save(path, value)
        if write_failure == 'existing':
            return original_save(path, value)
        raise OSError('synthetic outcome retention failure')

    if execution_failure:
        def launch(case, directory, timeout):
            calls.append(case['case_id'])
            raise RuntimeError('synthetic launch failure')
        callbacks['launch'] = launch
    monkeypatch.setattr(canary, '_save', save)

    result = canary.run_canary(bundle, output, approval, **callbacks)
    expected = ORDER[:1] if execution_failure else ORDER
    assert calls == expected == result['attempted']
    assert result['unattempted'] == ORDER[len(expected):]
    assert result['status'] == 'INCOMPLETE'
    assert 'outcome_record_error' in result
    assert len(writes) == 1
    if execution_failure:
        assert result['reason'] == 'synthetic launch failure'
    if write_failure in ('partial', 'existing'):
        assert (output / 'outcome.json').read_bytes() == b'retained incomplete evidence'
    elif write_failure == 'before':
        assert not (output / 'outcome.json').exists()
    else:
        assert json.loads((output / 'outcome.json').read_bytes())['attempted'] == expected

    claim = next((output.parent / 'authority-ledger').glob('*.json'))
    claim_bytes = claim.read_bytes()
    second = canary.run_canary(bundle, output.with_name('another-output'), approval, **callbacks)
    assert second['status'] == 'INCOMPLETE'
    assert calls == expected and claim.read_bytes() == claim_bytes


def test_diagnostic_exit_rejects_unretained_planned_stop():
    """The planned stop is usable only when its independent outcome is retained."""
    result = dict(outcome=dict(status='INCOMPLETE', attempted=ORDER[:1],
        records=[dict(case_id=ORDER[0])],
        reason='diagnostic scope exhausted: durable claim consumed'),
        reservation_released=True, no_owned_processes_established=True)
    assert driver.diagnostic_exit_code(result) == 0
    result['outcome']['outcome_record_error'] = 'OSError: synthetic retention failure'
    assert driver.diagnostic_exit_code(result) != 0


@pytest.mark.parametrize('settled', [False, True])
def test_operator_returns_independent_outcome_after_record_failure(
        protocol, monkeypatch, settled):
    """One launch is consumed; only a settled return releases the reservation.

    The second preflight will refuse further work. Failure to retain the first
    outcome must still reach the operator's independent return boundary.
    """
    bundle, output, approval, callbacks, calls = protocol
    original_save, original_launch = canary._save, callbacks['launch']
    reservation = SyntheticReservation()

    def save(path, value):
        if path.name == 'outcome.json':
            raise OSError('synthetic outcome retention failure')
        return original_save(path, value)

    def launch(*args):
        result = original_launch(*args)
        if not settled:
            raise RuntimeError('synthetic uncertain launch')
        return result

    def preflight(bound):
        if calls:
            raise ValueError('synthetic stop after first attempt')
        return callbacks['preflight'](bound)

    monkeypatch.setattr(canary, '_save', save)
    monkeypatch.setattr(driver, 'validate_environment', lambda *args: {})
    result = driver.execute_diagnostic(bundle=bundle, output=output, approval=approval,
        admission={key: callbacks[key] for key in ('verify_authority', 'adjudicate')},
        preflight=preflight, reservation=reservation,
        execution=dict(launch=launch, **{key: callbacks[key] for key in
                       ('extract', 'assess', 'verify_reference')}))
    assert result['outcome']['attempted'] == calls == ORDER[:1]
    assert result['outcome']['status'] == 'INCOMPLETE'
    assert 'outcome_record_error' in result['outcome']
    assert result['reservation_released'] is settled
    assert reservation.releases == [settled]
    assert driver.diagnostic_exit_code(result) != 0


@pytest.mark.parametrize('fixture_name', ['harness', 'intermediate'])
@pytest.mark.parametrize('failure', ['terminal', 'outcome', 'both'])
def test_pressure_record_failures_preserve_consumption(
        request, monkeypatch, fixture_name, failure):
    """Hand trace: one new claim and one launch, then two independent writes.

    Failure of either write cannot reverse the claim; failure of both still
    returns an incomplete result. A repeated invocation cannot launch again.
    """
    operation = request.getfixturevalue(fixture_name)
    original_write = resume._write_exclusive
    writes = []

    def write(path, value):
        role = ('terminal' if path.name.endswith('.terminal.json') else
                'outcome' if path.name == 'outcome.json' else None)
        if role:
            writes.append(role)
            if failure in (role, 'both'):
                raise OSError('synthetic ' + role + ' retention failure')
        return original_write(path, value)

    monkeypatch.setattr(resume, '_write_exclusive', write)
    result = operation.run()
    assert writes == ['terminal', 'outcome']
    assert result['consumed_count'] == operation.config['scope']['ordinal']
    assert len(operation.launches) == 1
    assert result['campaign_status'] == 'INCOMPLETE'
    assert result['reservation_released'] is True
    assert result['accepted_values'] == {} and result['engineering_qualified'] is False
    for role in ('terminal', 'outcome'):
        assert (role + '_record_error' in result) == (failure in (role, 'both'))
    if failure == 'terminal':
        output = Path(operation.config['operational']['output_directory'])
        assert json.loads((output / 'outcome.json').read_bytes()) == result
    claim = operation.record('')
    claim_bytes = claim.read_bytes()
    with pytest.raises(ValueError):
        operation.run()
    assert claim.read_bytes() == claim_bytes and len(operation.launches) == 1
