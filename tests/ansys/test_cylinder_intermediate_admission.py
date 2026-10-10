"""Synthetic third-attempt contracts; no licence or native operations."""
from copy import deepcopy
from pathlib import Path

import pytest

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys import cylinder_intermediate_lineage as history
from digitalmodel.ansys.cylinder_pressure_scope import pressure_step, INTERMEDIATE_SCOPE
from digitalmodel.ansys import cylinder_pressure_admission as admission
from digitalmodel.ansys import cylinder_diagnostic_preflight as preflight
from . import test_cylinder_diagnostic_preflight as preflight_fixtures
from . import test_cylinder_pressure_admission as admission_fixtures

prepared = preflight_fixtures.prepared
_set_cfd_binding = preflight_fixtures._set_cfd_binding

fixture = admission_fixtures.fixture
refresh = admission_fixtures.refresh
make = admission_fixtures.make


def write(path, value):
    raw = canonical_bytes(value); path.write_bytes(raw)
    return dict(path=str(path), sha256=digest_bytes(raw))


@pytest.fixture
def coarse(tmp_path, monkeypatch):
    root = tmp_path / 'coarse'; root.mkdir()
    case = root / 'ocv-t60-p10-n4'; case.mkdir()
    ledger = tmp_path / 'ledger'; ledger.mkdir()
    parent = write(ledger / ('a' * 64 + '.json'), {'synthetic': 'parent'})
    stem = 'a' * 64 + '.ordinal-2'
    common = dict(case_id='ocv-t60-p10-n4', ordinal=2, output=str(root),
                  parent_sha256=parent['sha256'])
    claim = write(ledger / (stem + '.json'), dict(common, state='attempt_consumed'))
    invocation = write(ledger / (stem + '.invocation.json'), common)
    execution = dict(return_code=0, timed_out=False, containment_verified=True,
        streams_finalized=True, owned_processes_remaining=0, settlement_required=False,
        evidence_complete=True, error=[], retained_supervisor_token=None)
    capture = dict(case_id='ocv-t60-p10-n4', retention_status='COMPLETE',
        independent_check_status='COMPLETE', execution_status='COMPLETE',
        engineering_qualified=False, accepted_values={}, capture_status='INCOMPLETE')
    outcome = dict(case_id='ocv-t60-p10-n4', consumed_count=2, native_launch_count=1,
        launch_adapter_calls=1, terminal_reason='PLANNED_SCOPE_STOP',
        reservation_released=True, no_owned_processes_established=True,
        engineering_qualified=False, capture=capture)
    pins = dict(invocation=invocation, claim=claim,
        terminal=write(ledger / (stem + '.terminal.json'), outcome),
        outcome=write(root / 'outcome.json', outcome),
        execution=write(case / 'execution.json', execution),
        capture=write(case / 'capture.json', capture))
    monkeypatch.setattr(history, 'FIXED', {k: v['sha256'] for k, v in pins.items()})
    descriptor = dict(schema='cylinder-coarse-predecessor-1', original_root=str(root),
        dataset_root=str(tmp_path), runtime_root=str(tmp_path),
        observation=dict(path=str(tmp_path / 'observation.json'), sha256=history.OBSERVATION_SHA256),
        **pins)
    config = dict(scope=deepcopy(INTERMEDIATE_SCOPE), predecessor=descriptor,
        lineage=dict(parent_claim=parent), ledger_directory=str(ledger))
    return config


def test_intermediate_scope_is_exact_and_typed():
    assert pressure_step({'scope': INTERMEDIATE_SCOPE}) == (3, 'ocv-t60-p10-n8')


@pytest.mark.parametrize('key,value', [('ordinal', 2), ('ordinal', True),
    ('case_ids', ['ocv-t60-p10-n16']), ('max_attempts', 2), ('capture_only', False)])
def test_wrong_scope_refuses(key, value):
    scope = dict(INTERMEDIATE_SCOPE, **{key: value})
    with pytest.raises(ValueError): pressure_step({'scope': scope})


def test_coarse_predecessor_readonly(coarse):
    root = Path(coarse['predecessor']['original_root'])
    before = {p: p.read_bytes() for p in root.rglob('*') if p.is_file()}
    history.validate_coarse_predecessor(coarse)
    assert {p: p.read_bytes() for p in root.rglob('*') if p.is_file()} == before


@pytest.mark.parametrize('role', ['invocation', 'claim', 'terminal', 'outcome', 'execution', 'capture'])
def test_altered_historical_record_refuses(coarse, role):
    Path(coarse['predecessor'][role]['path']).write_bytes(b'{}')
    with pytest.raises(ValueError): history.validate_coarse_predecessor(coarse)


def test_wrong_campaign_claim_path_refuses_even_identical_bytes(coarse, tmp_path):
    pin = coarse['predecessor']['claim']
    other = tmp_path / 'other.json'; other.write_bytes(Path(pin['path']).read_bytes())
    pin['path'] = str(other)
    with pytest.raises(ValueError): history.validate_coarse_predecessor(coarse)


@pytest.mark.parametrize('field,value', [('timed_out', True), ('owned_processes_remaining', 1),
    ('streams_finalized', False), ('settlement_required', True), ('return_code', True)])
def test_synthetic_reanchored_unsettled_execution_refuses(coarse, monkeypatch, field, value):
    from digitalmodel.ansys.analysis_records import parse_json
    pin = coarse['predecessor']['execution']; data = parse_json(Path(pin['path']).read_bytes())
    data[field] = value; replacement = write(Path(pin['path']), data)
    pin.update(replacement); monkeypatch.setitem(history.FIXED, 'execution', pin['sha256'])
    with pytest.raises(ValueError): history.validate_coarse_predecessor(coarse)


@pytest.mark.parametrize('binding', [None, {'schema': 'cfd-process-binding-1'}])
def test_intermediate_preflight_refuses_missing_version_two(prepared, binding):
    config, approval, reservation, _, _ = prepared
    approval['scope'] = deepcopy(INTERMEDIATE_SCOPE)
    _set_cfd_binding(config, binding)
    with pytest.raises(ValueError, match='pressure.*v2'):
        preflight.ProductionPreflight(config, approval, reservation)._bindings()


def test_new_schema_required_for_intermediate(fixture):
    fixture['config']['scope'] = deepcopy(INTERMEDIATE_SCOPE)
    refresh(fixture)
    with pytest.raises(ValueError, match='schema'):
        make(fixture)


def test_intermediate_factory_checks_history_after_review(fixture, monkeypatch):
    fixture['config'].update(schema='cylinder-pressure-admission-2',
        scope=deepcopy(INTERMEDIATE_SCOPE), predecessor={'synthetic': True})
    calls = []
    monkeypatch.setattr(admission, 'validate_coarse_predecessor', lambda c: calls.append(c))
    refresh(fixture); result = make(fixture)
    assert result['approval']['predecessor'] == {'synthetic': True} and len(calls) == 1
    fixture['findings'] = ['MAJOR: blocked']; refresh(fixture)
    with pytest.raises(ValueError): make(fixture)
    assert len(calls) == 1


@pytest.mark.parametrize('finding', ['The launch would be unsafe because containment is absent.', 'no major concerns'])
def test_unlabelled_findings_refuse(fixture, finding):
    fixture['findings'] = [finding]; refresh(fixture)
    with pytest.raises(ValueError, match='blocking|severity'): make(fixture)
