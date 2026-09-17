"""Synthetic scheduling/retention boundaries; no native solver calls."""
from pathlib import Path
import pytest
from . import test_cylinder_intermediate_resume as fixtures
from digitalmodel.ansys import cylinder_pressure_resume as resume
from digitalmodel.ansys.analysis_records import parse_json

intermediate = fixtures.intermediate


def test_refusal_does_not_claim_unverified_stream_binding(intermediate, monkeypatch):
    intermediate.failure = 'preclaim'
    def refuse(config):
        raise ValueError('changed stream')
    monkeypatch.setattr(resume, 'verify_streams', refuse)
    result = intermediate.run()
    assert 'preparation_binding' not in result
    assert 'PREPARATION_RETENTION_INCOMPLETE' in result['terminal_reasons']
    assert not (Path(intermediate.config['operational']['output_directory']) /
                'preparation-refusal.json').exists()


@pytest.mark.parametrize('elapsed,passes', [(2_000_000_000, True),
    (2_000_000_001, False), (-1, False)])
def test_measured_probe_threshold_precedes_all_ledger_records(intermediate, elapsed, passes):
    intermediate.preclaim_delay = elapsed
    result = intermediate.run()
    assert bool(intermediate.launches) == passes, result.get('error')
    assert result['preclaim_probe']['elapsed_ns'] == elapsed
    if not passes:
        assert result['consumed_count'] == 2
        assert not any(intermediate.record(s).exists() for s in ('', '.invocation', '.terminal'))
        assert result['preclaim_probe']['status'] == 'REFUSED'


def test_probe_error_retains_release_and_fresh_output_can_be_prepared(intermediate):
    intermediate.failure = 'preclaim'
    first = intermediate.run()
    old_output = Path(intermediate.config['operational']['output_directory'])
    retained = parse_json((old_output/'preparation-refusal.json').read_bytes())
    assert retained == first and retained['reservation_released'] is True
    assert retained['preparation_binding'] == dict(
        config_sha256=intermediate.approval['config_sha256'], ordinal=3,
        output=str(old_output),
        streams={'stdout': 'synthetic-stdout', 'stderr': 'synthetic-stderr'},
        parent_claim_sha256=intermediate.config['lineage']['parent_claim']['sha256'])
    assert 'last_preflight_evidence' in retained and 'final_storage_before_receipts' in retained
    intermediate.failure = None
    new_output = str(old_output.with_name('fresh-reviewed-output'))
    intermediate.config['operational']['output_directory'] = new_output
    intermediate.approval['operational']['output_directory'] = new_output
    assert intermediate.run()['terminal_reason'] == 'PLANNED_SCOPE_STOP'
    assert parse_json((old_output/'preparation-refusal.json').read_bytes()) == retained


def test_probe_and_postclaim_evidence_are_separate(intermediate):
    intermediate.phase2 = lambda _: {'status': 'PASS', 'phase': 'postclaim'}
    result = intermediate.run()
    assert result['preclaim_probe']['checked_evidence'] == {'status': 'PASS', 'synthetic': True}
    assert result['phase2_evidence'] == {'status': 'PASS', 'phase': 'postclaim'}
    assert intermediate.events.index('preclaim_probe') < intermediate.events.index('launch')


def test_probe_storage_time_is_measured(intermediate):
    calls = []
    def storage():
        calls.append(1)
        if len(calls) == 2:
            intermediate.clock.advance(2_000_000_001)
        return {'status': 'PASS'}
    intermediate.storage = storage
    result = intermediate.run()
    assert result['consumed_count'] == 2 and not intermediate.record('.invocation').exists()
    assert not intermediate.launches


@pytest.mark.parametrize('field', ['ready', 'license'])
def test_probe_cannot_refresh_observation_authority(intermediate, field):
    original = intermediate.preclaim
    def altered(approval):
        result = original(approval)
        if field == 'ready':
            intermediate._ready_at = '1800000001'
        else:
            intermediate.last_evidence['license_observation']['observed_at'] = '1800000001'
        return result
    intermediate.preclaim = altered
    result = intermediate.run()
    assert result['consumed_count'] == 2 and not intermediate.launches


def test_observation_staleness_after_probe_refuses_without_invocation(intermediate):
    original = intermediate.before_launch
    def aged():
        evidence = original()
        intermediate.clock.advance(19_000_000_000)
        return evidence
    intermediate.before_launch = aged
    intermediate.preclaim_delay = 2_000_000_000
    result = intermediate.run()
    assert result['consumed_count'] == 2 and not intermediate.record('.invocation').exists()


def test_slow_claim_write_still_stops_before_native(intermediate, monkeypatch):
    original = resume._write_exclusive
    def slow(path, value):
        original(path, value)
        if path == intermediate.record(''):
            intermediate.clock.advance(5_000_000_001)
    monkeypatch.setattr(resume, '_write_exclusive', slow)
    result = intermediate.run()
    assert result['consumed_count'] == 3 and not intermediate.launches
    assert result['terminal_reason'] == 'POSTCLAIM_INCOMPLETE'
    with pytest.raises(ValueError): intermediate.run()


def test_preparation_receipt_failure_is_explicit(intermediate, monkeypatch):
    original = resume._write_exclusive
    def failed(path, value):
        if path.name == 'preparation-refusal.json': raise OSError('synthetic receipt failure')
        return original(path, value)
    monkeypatch.setattr(resume, '_write_exclusive', failed)
    intermediate.failure = 'preclaim'
    result = intermediate.run()
    assert 'PREPARATION_RETENTION_INCOMPLETE' in result['terminal_reasons']
    assert result['consumed_count'] == 2
