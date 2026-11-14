"""Fixture-driven scenario acceptance and independent rejection paths."""
import importlib.util
from pathlib import Path

import pytest


def _example():
    path = Path(__file__).resolve().parents[3] / 'examples/marine_ops/towing/towing_scenario.py'
    spec = importlib.util.spec_from_file_location('towing_scenario', path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_worked_fixture_passes_and_twenty_percent_downsize_fails():
    run = _example().run_scenario
    baseline = run()
    assert baseline['verdict'] == 'SCREEN_PASS'
    assert baseline['bollard_pull']['passed']
    assert all(check['passed'] for check in baseline['checks'].values())
    assert baseline['line_length_m'] == 600
    assert baseline['citations'][0]['code_id'] == 'towing-plan-criteria'
    assert baseline['towline_below_full_bollard_pull']
    assert baseline['warnings']
    assert 'assumed, based on previous experience' in baseline['mbl_basis']
    assert 'plan-criterion screening' in baseline['assessment']
    reduced = run(diameter_scale=0.8)
    assert reduced['verdict'] == 'SCREEN_FAIL'
    assert not reduced['checks']['towline']['passed']
    assert reduced['mbl_n'] == pytest.approx(baseline['mbl_n'] * 0.8**2)
    assert baseline['optimized']['feasible_count'] > 0


def test_fixture_storm_exceeds_available_pull():
    result = _example().run_scenario(environment='storm')
    assert result['verdict'] == 'SCREEN_FAIL'
    assert result['bollard_pull']['margin_n'] < 0
    assert result['optimized'] is None
