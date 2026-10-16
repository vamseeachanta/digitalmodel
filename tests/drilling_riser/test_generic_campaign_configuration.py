"""Solver-free regressions for campaign configuration and report references."""
import ast
import json
import importlib.util
from pathlib import Path
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[2]
DRIVER = ROOT / 'scripts/orcaflex/run_generic_drilling_riser_campaign.py'
REPORT = ROOT / 'scripts/reporting/build_generic_riser_report.py'
CRITERIA = Path(__file__).parent / 'fixtures/generic_synthetic_criteria.json'


def load_module(path, name):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_configuration_failure_stops_campaign_once(tmp_path):
    driver = load_module(DRIVER, 'configuration_driver')
    from digitalmodel.drilling_riser.generic_campaign import DEST
    calls = []

    def unavailable(*args):
        calls.append(args)
        raise driver.ConfigurationError('Solver interface not importable')

    result = driver.run_campaign(ROOT / DEST, tmp_path,
                                 criteria_file=CRITERIA, case_runner=unavailable)
    assert result == driver.CONFIGURATION_EXIT
    assert result not in (0, 1, driver.LICENCE_EXIT)
    assert len(calls) == 1
    assert len(list(tmp_path.glob('*.failure.json'))) == 1
    record = json.loads((tmp_path / 'o0-c0.failure.json').read_text(encoding='utf-8'))
    assert record['status'] == 'configuration-failure'
    assert record['error_type'] == 'ConfigurationError'
    assert record['message'] == 'Solver interface not importable'


def test_case_fault_is_not_configuration_failure(tmp_path):
    driver = load_module(DRIVER, 'case_fault_driver')
    from digitalmodel.drilling_riser.generic_campaign import DEST
    calls = []

    def broken(*args):
        calls.append(args)
        raise RuntimeError('synthetic case fault')

    result = driver.run_campaign(ROOT / DEST, tmp_path,
                                 criteria_file=CRITERIA, case_runner=broken)
    assert result == 1
    assert len(calls) == 24
    records = list(tmp_path.glob('*.failure.json'))
    assert len(records) == 24
    assert all(json.loads(path.read_text(encoding='utf-8'))['status'] == 'incomplete'
               for path in records)


def test_missing_interface_raises_dedicated_error():
    driver = load_module(DRIVER, 'missing_interface_driver')
    import pytest
    import sys
    with patch.dict(sys.modules, {'OrcFxAPI': None}):
        with pytest.raises(driver.ConfigurationError, match='interface not importable'):
            driver.run_case(Path('unused'), Path('unused'))


def test_ring_reference_is_read_before_model_construction():
    tree = ast.parse(DRIVER.read_text(encoding='utf-8'))
    run_case = next(node for node in tree.body
                    if isinstance(node, ast.FunctionDef) and node.name == 'run_case')
    reference = [node.lineno for node in ast.walk(run_case)
                 if isinstance(node, ast.Call) and isinstance(node.func, ast.Name)
                 and node.func.id == 'ring_reference_z']
    construction = [node.lineno for node in ast.walk(run_case)
                    if isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)
                    and isinstance(node.func.value, ast.Name)
                    and node.func.value.id == 'OrcFxAPI' and node.func.attr == 'Model']
    assert len(reference) == len(construction) == 1
    assert reference[0] < construction[0]


def test_report_ring_reference_tracks_each_case():
    builder = load_module(REPORT, 'ring_reference_report')
    table = {'cases': [
        {'id': 'synthetic-a', 'ring_initial_position_m': {'value': [0, 0, 37]}},
        {'id': 'synthetic-b', 'ring_initial_position_m': {'value': [0, 0, 41]}},
    ]}
    description = builder.ring_reference_description(table)
    assert description == 'synthetic-a: 37 m; synthetic-b: 41 m'
    table['cases'][1]['ring_initial_position_m']['value'][2] = 37
    assert builder.ring_reference_description(table) == '37 m for every case'


def test_native_library_import_failure_is_configuration():
    import builtins
    import pytest
    driver = load_module(DRIVER, 'library_failure_driver')
    original_import = builtins.__import__

    def fail_interface(name, *args, **kwargs):
        if name == 'OrcFxAPI':
            raise OSError('synthetic native library failure')
        return original_import(name, *args, **kwargs)

    with patch.object(builtins, '__import__', side_effect=fail_interface):
        with pytest.raises(driver.ConfigurationError, match='OSError') as caught:
            driver.run_case(Path('unused'), Path('unused'))
    assert isinstance(caught.value.__cause__, OSError)
