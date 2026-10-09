"""Real retained-file validation through driver callbacks; no native resources."""
from pathlib import Path

import pytest

from . import test_cylinder_intermediate_driver as driver_fixtures
from . import test_cylinder_preparation_history as history_fixtures

driver = driver_fixtures.driver
history = history_fixtures.history


def test_storage_includes_failed_preparation_and_driver_streams(driver, history):
    history['predecessor'] = {'original_root': 'coarse'}
    entry = history['preparation_history']['preparations'][0]
    result = driver.history_storage(history)
    assert result['prior_capture_roots'] == ('coarse', entry['root'])
    assert result['supplemental_files'] == tuple(x['path'] for x in entry['streams'])


def test_binding_rechecks_membership_after_authority(driver, history, monkeypatch):
    monkeypatch.setattr(driver, 'verify_streams', lambda config: {})
    check = driver.history_binding(history, lambda approval: True)
    assert check({}) is True
    root = history['preparation_history']['preparations'][0]['root']
    (Path(root)/'late-file').write_bytes(b'new')
    with pytest.raises(ValueError): check({})


def test_binding_cannot_be_changed_by_mutating_callers_config(driver, history, monkeypatch):
    monkeypatch.setattr(driver, 'verify_streams', lambda config: {})
    check = driver.history_binding(history, lambda approval: True)
    original = history['preparation_history']['preparations'][0]['files'][1]['path']
    history['preparation_history']['preparations'] = []
    Path(original).write_bytes(b'changed')
    with pytest.raises(ValueError): check({})


def test_authority_failure_precedes_history_reads(driver, history):
    check = driver.history_binding(history, lambda approval: False)
    with pytest.raises(ValueError, match='binding'): check({})


def test_binding_rechecks_actual_streams(driver, history, monkeypatch):
    calls = []
    monkeypatch.setattr(driver, 'verify_streams', lambda config: calls.append(config))
    check = driver.history_binding(history, lambda approval: True)
    assert check({}) is True
    assert len(calls) == 1
    def refuse(config):
        raise ValueError('stream changed')
    monkeypatch.setattr(driver, 'verify_streams', refuse)
    with pytest.raises(ValueError, match='stream changed'): check({})


def test_main_checks_streams_before_seat_acquisition(driver, history, monkeypatch):
    from digitalmodel.ansys import cylinder_operational_reservation as reservations
    calls = []
    monkeypatch.setattr(driver.sys, 'argv', [driver.__file__])
    monkeypatch.setattr(driver, '_arguments', lambda argv: object())
    monkeypatch.setattr(driver, 'bound_inputs', lambda *args: (history, {}))
    def refuse(config):
        calls.append('stream')
        raise ValueError('stream identity')
    monkeypatch.setattr(driver, 'verify_streams', refuse)
    monkeypatch.setattr(reservations, 'acquire_local_reservation',
                        lambda *args: calls.append('seat'))
    with pytest.raises(ValueError, match='stream identity'):
        driver.main([])
    assert calls == ['stream']
