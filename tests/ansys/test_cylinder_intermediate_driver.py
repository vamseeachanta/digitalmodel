"""Synthetic driver wiring for intermediate mesh (N8); no native execution."""
import importlib.util
from pathlib import Path
from types import SimpleNamespace
import sys
import pytest


@pytest.fixture
def driver(monkeypatch):
    path = Path(__file__).parents[2] / 'scripts/ansys/run_pressure_diagnostic.py'
    spec = importlib.util.spec_from_file_location('intermediate_driver', path)
    module = importlib.util.module_from_spec(spec); spec.loader.exec_module(module)
    return module


def test_both_ordinal_ledger_sets_only(driver, tmp_path):
    parent = tmp_path / ('a'*64+'.json'); parent.write_bytes(b'parent')
    expected = []
    for ordinal in (2, 3):
        for suffix in ('.invocation.json', '.json', '.terminal.json'):
            p = parent.with_name(parent.stem+f'.ordinal-{ordinal}'+suffix)
            p.write_bytes(b'synthetic'); expected.append(p)
    (tmp_path/'unrelated.json').write_bytes(b'not owned')
    assert driver.ledger_paths(parent, ordinal=3) == expected
    assert driver.ledger_paths(parent) == expected[:3]


@pytest.mark.parametrize('ordinal', [True, 1, 4, '3', 3.0])
def test_invalid_ordinal_refuses(driver, tmp_path, ordinal):
    p = tmp_path/'parent.json'; p.write_bytes(b'parent')
    with pytest.raises(ValueError): driver.ledger_paths(p, ordinal=ordinal)


def test_coarse_pins_join_fast_binding_and_detect_change(driver, tmp_path, monkeypatch):
    p = tmp_path/'coarse.json'; p.write_bytes(b'coarse')
    pin = dict(path=str(p), sha256=driver.digest_bytes(p.read_bytes()))
    config = dict(scope=dict(ordinal=3, case_ids=['ocv-t60-p10-n8'], max_attempts=1,
        capture_only=True, qualification='diagnostic_only'), source_files=[],
        replay={'resolver':pin}, lineage={'baseline':dict(path=str(p), file_sha256=pin['sha256'])},
        synthetic_coarse_pins=[pin])
    args = SimpleNamespace(config=str(p),config_sha256=pin['sha256'],review_receipt=str(p),
        review_sha256=pin['sha256'],review_stdout=str(p),review_bundle=str(p))
    monkeypatch.setattr(driver, '_resolver', lambda c: {})
    calls = []
    module = sys.modules['digitalmodel.ansys.cylinder_intermediate_lineage']
    monkeypatch.setattr(module, 'coarse_read_pins', lambda c: calls.append(c) or [pin])
    pins = driver._pins(config, args, tmp_path)
    assert calls == [config] and pin in pins
    check = driver.fast_binding_check(pins, {})
    p.write_bytes(b'changed')
    with pytest.raises(ValueError): check({})


def wiring(driver, tmp_path, monkeypatch, ordinal):
    from digitalmodel.ansys import cylinder_diagnostic_preflight as pf
    from digitalmodel.ansys import cylinder_pressure_capture as capture
    from digitalmodel.ansys import cylinder_pressure_resources as resources
    calls = []
    monkeypatch.setattr(driver, 'verify_streams', lambda config: calls.append(('streams', {})))
    monkeypatch.setattr(driver, 'validate_preparation_history', lambda *a, **k:
        calls.append(('history', k)) or dict(prior_capture_roots=['abandoned'],
            supplemental_files=['driver-stdout', 'driver-stderr'], retained_bytes=941247))
    monkeypatch.setattr(pf, 'ProductionPreflight', lambda *a: object())
    monkeypatch.setattr(driver, '_pins', lambda *a: [])
    monkeypatch.setattr(driver, 'fast_binding_check', lambda *a: object())
    monkeypatch.setattr(driver, 'ledger_paths', lambda *a, **k: calls.append(('ledger',k)) or [])
    monkeypatch.setattr(driver.shutil, 'disk_usage', lambda *a: SimpleNamespace(free=999))
    monkeypatch.setattr(capture, 'capture_pressure', lambda *a, **k: calls.append(('capture',k)))
    monkeypatch.setattr(resources, 'cumulative_storage', lambda *a, **k: calls.append(('storage',k)))
    config = dict(scope=dict(ordinal=ordinal,case_ids=[f'ocv-t60-p10-n{8 if ordinal==3 else 4}'],
        max_attempts=1,capture_only=True,qualification='diagnostic_only'),
        operational=dict(executable='synthetic',output_directory='out',lock_path='lock'),
        lineage=dict(parent_claim={'path':'parent'},original_root='zero'),
        predecessor={'original_root':'coarse'})
    callbacks = driver.production_callbacks(config, {'approval':{'runtime_profile':{}}},None,tmp_path,object())
    callbacks['capture']({'case_id':config['scope']['case_ids'][0]},'dir',{})
    callbacks['account_storage']()
    return calls


def test_intermediate_capture_and_prior_storage_wiring(driver, tmp_path, monkeypatch):
    calls = wiring(driver,tmp_path,monkeypatch,3)
    assert ('capture',{'runtime_profile':{},'capture_case_id':'ocv-t60-p10-n8'}) in calls
    assert ('storage',{'prior_capture_roots':('coarse', 'abandoned'),
                      'supplemental_files':('driver-stdout', 'driver-stderr')}) in calls
    assert ('history', {}) in calls
    assert ('history', {'require_unclaimed':False}) in calls
    assert ('ledger',{'ordinal':3}) in calls


def test_coarse_wiring_preserves_legacy_keyword_shape(driver, tmp_path, monkeypatch):
    calls = wiring(driver,tmp_path,monkeypatch,2)
    assert ('capture',{'runtime_profile':{}}) in calls
    assert ('storage',{}) in calls
