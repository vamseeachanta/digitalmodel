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
    assert baseline['verdict'] == 'PASS'
    assert baseline['bollard_pull']['passed']
    assert all(check['passed'] for check in baseline['checks'].values())
    assert baseline['line_length_m'] == 600
    assert baseline['citations'][0]['code_id'] == 'digitalmodel-towing-plan-487'
    reduced = run(diameter_scale=0.8)
    assert reduced['verdict'] == 'FAIL'
    assert not reduced['checks']['towline']['passed']
    assert reduced['mbl_n'] == pytest.approx(baseline['mbl_n'] * 0.8**2)
    assert baseline['optimized']['feasible_count'] > 0


def test_fixture_storm_exceeds_available_pull():
    result = _example().run_scenario(environment='storm')
    assert result['verdict'] == 'FAIL'
    assert result['bollard_pull']['margin_n'] < 0
    assert result['optimized'] is None


def test_screening_pass_does_not_qualify_line_against_tug_capacity():
    example = _example()
    result = example.run_scenario()
    assert result['assessment'] == 'plan-criterion screening'
    assert result['operational_acceptance'] == 'NOT_ESTABLISHED'
    comparison = result['equipment_compatibility']
    assert comparison['mbl_below_bollard_pull'] is True
    assert comparison['margin_n'] == pytest.approx(430000 - 80 * 1000 * 9.80665)
    assert 'synthetic' in example._load_fixture()['line']['mbl_source']
    for check in result['checks'].values():
        assert check['assessment'] == 'plan-criterion screening'
        assert all(citation['code_id'] != 'dnvgl-st-n001' for citation in check['citations'])
