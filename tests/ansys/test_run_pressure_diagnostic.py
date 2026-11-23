"""Synthetic driver contracts; no native subprocess, licence query or claim."""
import importlib.util
import json
from pathlib import Path
from types import SimpleNamespace

import pytest

SCRIPT = Path(__file__).parents[2] / 'scripts/ansys/run_pressure_diagnostic.py'


@pytest.fixture
def driver():
    spec = importlib.util.spec_from_file_location('pressure_driver_contract', SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def pin(driver, path, value):
    path.write_text(json.dumps(value), encoding='utf-8')
    return {'path': str(path), 'sha256': driver.digest_bytes(path.read_bytes())}


def setup_replay(driver, tmp_path):
    evidence = pin(driver, tmp_path / 'evidence.json', {'synthetic': True})
    resolver = pin(driver, tmp_path / 'resolver.json', {'synthetic': evidence['path']})
    baseline = pin(driver, tmp_path / 'baseline.json', {'cases': []})
    config = {'lineage': {'baseline': {'path': baseline['path'], 'file_sha256': baseline['sha256']}},
              'replay': {'reference': {'id': 'synthetic', 'sha256': evidence['sha256'], 'required': True},
                         'resolver': resolver, 'review_sha256': 'b' * 64}}
    return config


def test_replay_calls_actual_derivation_with_pinned_dictionary(driver, tmp_path, monkeypatch):
    config = setup_replay(driver, tmp_path)
    seen = []
    monkeypatch.setattr(driver, 'derive_replay_case',
                        lambda *args, **kwargs: seen.append((args, kwargs)) or {'case_id': 'synthetic'})
    replay = driver.replay_callback(config)
    assert replay({'bound': True}) == {'case_id': 'synthetic'}
    args, kwargs = seen[0]
    assert args[0] == {'cases': []}
    assert isinstance(args[2]['synthetic'], Path)
    assert kwargs['review_sha256'] == 'b' * 64


@pytest.mark.parametrize('target', ['resolver', 'baseline'])
def test_replay_changed_pin_refuses_before_derivation(driver, tmp_path, monkeypatch, target):
    config = setup_replay(driver, tmp_path)
    path = config['replay']['resolver']['path'] if target == 'resolver' else config['lineage']['baseline']['path']
    Path(path).write_bytes(b'{}')
    monkeypatch.setattr(driver, 'derive_replay_case', lambda *a, **k: pytest.fail('derived changed input'))
    with pytest.raises(ValueError):
        driver.replay_callback(config)({})


def test_fast_checker_binds_sources_config_and_approval_without_git(driver, tmp_path):
    source = pin(driver, tmp_path / 'source.py', {'synthetic': True})
    config_pin = pin(driver, tmp_path / 'config.json', {'synthetic': True})
    approval = {'config_sha256': config_pin['sha256']}
    check = driver.fast_binding_check([source, config_pin], approval)
    assert check(dict(approval)) is True
    with pytest.raises(ValueError):
        check({'config_sha256': '0' * 64})
    Path(source['path']).write_bytes(b'changed')
    with pytest.raises(ValueError):
        check(approval)


def test_ledger_names_are_parent_linked_and_only_existing(driver, tmp_path):
    parent = tmp_path / 'original.claim.json'
    parent.write_bytes(b'parent')
    expected = parent.with_name(parent.stem + '.ordinal-2.invocation.json')
    expected.write_bytes(b'invocation')
    (tmp_path / 'unrelated.json').write_bytes(b'other')
    assert driver.ledger_paths(parent) == [expected]


def test_bound_config_mismatch_refuses_before_admission(driver, tmp_path, monkeypatch):
    config = tmp_path / 'config.json'
    config.write_bytes(b'{}')
    arguments = SimpleNamespace(config=str(config), config_sha256='0' * 64)
    monkeypatch.setattr(driver, 'make_pressure_admission', lambda *a, **k: pytest.fail('admission called'))
    with pytest.raises(ValueError):
        driver.bound_inputs(arguments, tmp_path)


def test_bound_inputs_calls_real_factory_interface_with_external_pins(driver, tmp_path, monkeypatch):
    record = pin(driver, tmp_path / 'config.json', {'synthetic': True})
    args = SimpleNamespace(config=record['path'], config_sha256=record['sha256'],
                           review_receipt='receipt', review_stdout='stdout', review_bundle='bundle',
                           review_sha256='a' * 64)
    seen = []
    monkeypatch.setattr(driver, 'make_pressure_admission', lambda *a, **k:
                        seen.append((a, k)) or {'approval': {'synthetic': True}})
    monkeypatch.setattr(driver, 'validate_environment', lambda *a: None)
    driver.bound_inputs(args, tmp_path)
    assert seen == [((record['path'], 'receipt', 'stdout', 'bundle'), {
        'expected_config_sha256': record['sha256'], 'expected_review_sha256': 'a' * 64,
        'source_root': tmp_path})]


def test_production_callbacks_bind_existing_implementations(driver, tmp_path, monkeypatch):
    from digitalmodel.ansys import cylinder_diagnostic_preflight as pf
    from digitalmodel.ansys import cylinder_pressure_resources as resources
    from digitalmodel.ansys import cylinder_pressure_capture as capture
    from digitalmodel.ansys import cylinder_runner as runner
    seen = []
    fake_preflight = object()
    monkeypatch.setattr(pf, 'ProductionPreflight', lambda *a: fake_preflight)
    monkeypatch.setattr(driver, '_pins', lambda *a: [])
    monkeypatch.setattr(driver, 'fast_binding_check', lambda *a: 'bound-check')
    monkeypatch.setattr(driver, 'validate_environment', lambda *a: None)
    monkeypatch.setattr(resources, 'production_phase2', lambda *a: seen.append(('phase2', a)))
    monkeypatch.setattr(runner, 'launch_case', lambda *a: seen.append(('launch', a)))
    monkeypatch.setattr(capture, 'capture_pressure', lambda *a, **k: seen.append(('capture', a, k)))
    config = {'operational': {'executable': 'bound-executable'}, 'synthetic': True,
              'scope': dict(case_ids=['ocv-t60-p10-n4'], ordinal=2, max_attempts=1,
                            capture_only=True, qualification='diagnostic_only')}
    admission = {'approval': {'runtime_profile': {'release': 'synthetic'}}}
    callbacks = driver.production_callbacks(config, admission, None, tmp_path, 'seat')
    callbacks['phase2_recheck'](admission['approval'])
    case = {'case_id': 'synthetic', 'deck': 'prepared/synthetic.inp'}
    callbacks['launch'](case, 'directory', 300)
    callbacks['capture']('case', 'directory', 'execution')
    assert seen == [('phase2', (fake_preflight, 'bound-check', admission['approval'])),
                    ('launch', ({**case, 'deck_basename': 'synthetic.inp'}, 'directory', 300,
                                'bound-executable')),
                    ('capture', ('case', 'directory', 'execution'),
                     {'runtime_profile': {'release': 'synthetic'}})]
    assert callbacks['preflight'] is fake_preflight
    assert callbacks['reservation'] == 'seat'
    assert callable(callbacks['clock'].time_ns)
    assert callable(callbacks['clock'].monotonic_ns)


@pytest.mark.parametrize('changed', [None, 'reason', 'released', 'settled', 'qualified'])
def test_exit_zero_only_for_planned_settled_unqualified_stop(driver, changed):
    result = {'terminal_reason': 'PLANNED_SCOPE_STOP', 'reservation_released': True,
              'no_owned_processes_established': True, 'engineering_qualified': False}
    if changed == 'reason':
        result['terminal_reason'] = 'PRECLAIM_REFUSAL'
    elif changed == 'released':
        result['reservation_released'] = False
    elif changed == 'settled':
        result['no_owned_processes_established'] = False
    elif changed == 'qualified':
        result['engineering_qualified'] = True
    assert driver.pressure_exit_code(result) == (0 if changed is None else 2)


def test_actual_preflight_preserves_private_ready_timestamp(monkeypatch):
    from digitalmodel.ansys import cylinder_diagnostic_preflight as pf
    instance = pf.ProductionPreflight({}, {}, None)
    instance._ready_at = '10'
    instance.last_evidence = {'capacity_observation': {'synthetic': True},
                              'process_snapshot': {'observed_at': '10'}}
    monkeypatch.setattr(pf, '_now', lambda: '11')
    monkeypatch.setattr(pf, 'validate_capacity', lambda *a, **k: {})
    for name in ('_bindings', '_reservation', '_licence'):
        monkeypatch.setattr(instance, name, lambda: None)
    instance.before_launch()
    assert instance._ready_at == '10'
    instance._ready_at = None
    with pytest.raises(ValueError):
        instance.before_launch()


def test_main_does_not_duplicate_resume_owned_early_release(driver, monkeypatch):
    from digitalmodel.ansys import cylinder_pressure_resume as resume
    from digitalmodel.ansys import cylinder_operational_reservation as seats
    releases = []
    seat = SimpleNamespace(release=lambda **kwargs: releases.append(kwargs))
    monkeypatch.setattr(driver.sys, 'argv', [str(SCRIPT)])
    monkeypatch.setattr(driver, '_arguments', lambda _: None)
    monkeypatch.setattr(driver, 'bound_inputs', lambda *a:
                        ({'operational': {'lock_path': 'synthetic'},
                          'scope': dict(case_ids=['ocv-t60-p10-n4'], ordinal=2, max_attempts=1,
                                        capture_only=True, qualification='diagnostic_only')}, {}))
    monkeypatch.setattr(driver, 'production_callbacks', lambda *a: {'reservation': seat})
    monkeypatch.setattr(seats, 'acquire_local_reservation', lambda _: seat)
    def early_refusal(*args, **kwargs):
        kwargs['reservation'].release(no_owned_processes=True)
        raise ValueError('synthetic journal refused before launch')
    monkeypatch.setattr(resume, 'execute_pressure_resume', early_refusal)
    with pytest.raises(ValueError):
        driver.main([])
    assert releases == [{'no_owned_processes': True}]
