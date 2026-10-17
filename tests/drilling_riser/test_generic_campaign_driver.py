# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Driver contract exercised with a synthetic case runner only."""
import ast
import os
from unittest.mock import Mock, patch
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
CRITERIA = Path(__file__).parent / 'fixtures/generic_synthetic_criteria.json'
DRIVER = ROOT / 'scripts/orcaflex/run_generic_drilling_riser_campaign.py'


def load_driver():
    spec = importlib.util.spec_from_file_location('generic_driver', DRIVER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class DriverTests(unittest.TestCase):
    def test_missing_criteria_refuses_before_runner(self):
        driver = load_driver()
        from digitalmodel.drilling_riser.generic_campaign import DEST
        with tempfile.TemporaryDirectory() as temporary:
            with self.assertRaisesRegex(ValueError, 'criteria file'):
                driver.run_campaign(ROOT / DEST, Path(temporary), case_runner=lambda *args: self.fail('runner called'))

    def test_source_digest_refuses_before_runner(self):
        driver = load_driver()
        from digitalmodel.drilling_riser.generic_campaign import DEST
        with tempfile.TemporaryDirectory() as temporary, patch.dict(os.environ, {'SOURCE_SHA256': 'stale'}):
            with self.assertRaisesRegex(ValueError, 'Source specification digest'):
                driver.run_campaign(ROOT / DEST, Path(temporary), criteria_file=CRITERIA,
                    case_runner=lambda *args: self.fail('runner called'))
            self.assertEqual(list(Path(temporary).iterdir()), [])

    def test_invalid_criteria_are_rejected(self):
        driver = load_driver()
        from digitalmodel.drilling_riser.generic_campaign import case_table
        criteria = case_table(ROOT)['criteria']
        criteria['mean_angle_deg']['value'] = -2
        with self.assertRaisesRegex(ValueError, 'finite and positive'):
            driver.criterion_rows({name: 1.0 for name in driver.METRICS}, criteria)

    def test_null_criteria_raise_value_error(self):
        driver = load_driver()
        from digitalmodel.drilling_riser.generic_campaign import case_table
        with self.assertRaisesRegex(ValueError, 'finite and positive'):
            driver.criterion_rows({}, case_table(ROOT)['criteria'])

    def test_missing_interface_is_configuration_failure(self):
        driver = load_driver()
        import sys
        with patch.dict(sys.modules, {'OrcFxAPI': None}):
            with self.assertRaisesRegex(RuntimeError, 'interface not importable'):
                driver.run_case(Path('unused'), Path('unused'))

    def test_missing_interface_stops_campaign_with_one_configuration_record(self):
        driver = load_driver()
        import sys
        from digitalmodel.drilling_riser.generic_campaign import DEST
        with tempfile.TemporaryDirectory() as temporary, patch.dict(sys.modules, {'OrcFxAPI': None}):
            output = Path(temporary)
            with patch.object(driver, 'run_case', wraps=driver.run_case) as runner:
                code = driver.run_campaign(ROOT / DEST, output, criteria_file=CRITERIA)
            self.assertEqual(code, 13)
            runner.assert_called_once()
            records = list(output.glob('*.failure.json'))
            self.assertEqual(len(records), 1)
            self.assertEqual(json.loads(records[0].read_text(encoding='utf-8')), {
                'case_id': 'o0-c0', 'status': 'configuration-failure',
                'error_type': 'ConfigurationError', 'message': 'Solver interface not importable (ModuleNotFoundError)'})
            self.assertFalse(list(output.glob('*.summary.json')))
            self.assertFalse((output / 'timing.json').exists())

    def test_ring_reference_errors_precede_native_model_creation(self):
        driver = load_driver()
        import sys
        for error in (FileNotFoundError('missing case table'), StopIteration('unmatched case')):
            api = Mock()
            with self.subTest(error=type(error).__name__), patch.dict(sys.modules, {'OrcFxAPI': api}):
                with patch.object(driver, 'ring_reference_z', side_effect=error):
                    with self.assertRaises(type(error)):
                        driver.run_case(Path('unused'), Path('unused'))
                api.Model.assert_not_called()

    def test_ring_reference_uses_case_table(self):
        driver = load_driver()
        import yaml
        with tempfile.TemporaryDirectory() as temporary:
            folder = Path(temporary)
            table = {'tension_basis': {'n_units': 4, 'n_fail': 1, 'fwt': 1.05},
                'cases': [{'id': 'synthetic',
                'ring_initial_position_m': {'value': [0, 0, 37]}}]}
            (folder / 'case-table.yml').write_text(yaml.safe_dump(table), encoding='utf-8')
            self.assertEqual(driver.ring_reference_z(folder / 'models/synthetic/master.yml'), 37)
            self.assertEqual(driver.case_tension_basis(folder / 'models/synthetic/master.yml'),
                             table['tension_basis'])

    def test_interface_import_is_local(self):
        tree = ast.parse(DRIVER.read_text(encoding='utf-8'))
        imports = []
        for node in tree.body:
            if isinstance(node, ast.FunctionDef):
                for child in ast.walk(node):
                    if isinstance(child, ast.Import) and any(a.name == 'OrcFxAPI' for a in child.names):
                        imports.append(node.name)
            else:
                self.assertFalse(any(isinstance(n, ast.Import) and any(a.name == 'OrcFxAPI' for a in n.names)
                                     for n in ast.walk(node)))
        self.assertEqual(imports, ['run_case'])

    def test_resume_licence_failure_and_timing(self):
        driver = load_driver()
        from digitalmodel.drilling_riser.generic_campaign import DEST
        campaign = ROOT / DEST
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary)
            calls = []
            def fake(model, simulation):
                calls.append(model)
                simulation.write_text('synthetic', encoding='utf-8')
                return {**{name: 1.0 for name in driver.METRICS},
                        'solver_version': 'synthetic', 'native_state': 'SimulationStopped',
                        'wall_seconds': 2.0}
            self.assertEqual(driver.run_campaign(campaign, output, criteria_file=CRITERIA, case_runner=fake), 0)
            self.assertEqual(len(calls), 24)
            self.assertEqual(driver.run_campaign(campaign, output, criteria_file=CRITERIA, case_runner=fake), 0)
            self.assertEqual(len(calls), 24)
            self.assertEqual(len(list(output.glob('*.sim'))), 4)
            self.assertEqual(json.loads((output / 'timing.json').read_text(encoding='utf-8'))['projected_seconds'], 48)
            altered = output / 'criteria.json'
            criteria = json.loads(CRITERIA.read_text(encoding='utf-8'))
            criteria['mean_angle_deg']['value'] /= 10
            altered.write_text(json.dumps(criteria), encoding='utf-8')
            with self.assertRaisesRegex(ValueError, 'Criteria changed'):
                driver.run_campaign(campaign, output, criteria_file=altered, case_runner=fake)
            self.assertEqual(len(calls), 24)
            retained = next(output.glob('*.sim'))
            retained.write_text('damaged synthetic artifact', encoding='utf-8')
            self.assertEqual(driver.run_campaign(campaign, output, criteria_file=CRITERIA, case_runner=fake), 0)
            self.assertEqual(len(calls), 25)
        with tempfile.TemporaryDirectory() as temporary:
            def unavailable(*args):
                raise driver.LicenceUnavailable('synthetic unavailable')
            self.assertEqual(driver.run_campaign(campaign, Path(temporary), criteria_file=CRITERIA, case_runner=unavailable), 12)
        with tempfile.TemporaryDirectory() as temporary:
            versions = []
            def mixed(model, simulation):
                result = fake(model, simulation)
                versions.append('next')
                result['solver_version'] = str(len(versions))
                return result
            self.assertEqual(driver.run_campaign(campaign, Path(temporary), criteria_file=CRITERIA, case_runner=mixed), 1)
            self.assertEqual(len(list(Path(temporary).glob('*.summary.json'))), 1)
            self.assertFalse((Path(temporary) / 'retention.json').exists())
        with tempfile.TemporaryDirectory() as temporary:
            def broken(*args):
                raise ValueError('synthetic invalid channel')
            self.assertEqual(driver.run_campaign(campaign, Path(temporary), criteria_file=CRITERIA, case_runner=broken), 1)
            failures = list(Path(temporary).glob('*.failure.json'))
            self.assertEqual(len(failures), 24)
            self.assertFalse(list(Path(temporary).glob('*.summary.json')))


def test_configuration_failure_stops_campaign_once(tmp_path):
    driver = load_driver()
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
    driver = load_driver()
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



def test_native_library_import_failure_is_configuration():
    import builtins
    import pytest
    driver = load_driver()
    original_import = builtins.__import__

    def fail_interface(name, *args, **kwargs):
        if name == 'OrcFxAPI':
            raise OSError('synthetic native library failure')
        return original_import(name, *args, **kwargs)

    with patch.object(builtins, '__import__', side_effect=fail_interface):
        with pytest.raises(driver.ConfigurationError, match='OSError') as caught:
            driver.run_case(Path('unused'), Path('unused'))
    assert isinstance(caught.value.__cause__, OSError)


class StagedModel:
    """Synthetic API state machine; changing current invalidates statics."""
    def __init__(self, speed=1.0, fail_stage=None, bad_state=False):
        from types import SimpleNamespace
        self.state = SimpleNamespace(name='Reset')
        self.environment = self
        self.speed = speed
        self.calls = []
        self.fail_stage = fail_stage
        self.bad_state = bad_state

    @property
    def RefCurrentSpeed(self):
        return self.speed

    @RefCurrentSpeed.setter
    def RefCurrentSpeed(self, value):
        self.calls.append(('current', value))
        self.speed = value
        self.state.name = 'Reset'

    def CalculateStatics(self):
        self.calls.append(('solve', self.speed))
        if self.speed == self.fail_stage:
            if not self.bad_state:
                raise RuntimeError('synthetic non-convergence')
            return
        self.state.name = 'InStaticState'

    def UseCalculatedPositions(self, enabled):
        assert self.state.name == 'InStaticState'
        assert enabled is True
        self.calls.append(('seed', self.speed))
        self.state.name = 'Reset'

    def RunSimulation(self):
        self.calls.append(('dynamics', self.speed))
        raise RuntimeError('synthetic dynamics reached')


def test_staged_statics_seeds_before_current_change():
    for speed in (0.5, 1.0, 1.5):
        model = StagedModel(speed=speed)
        load_driver().calculate_staged_statics(model)
        expected = [('current', 0.0), ('solve', 0.0)]
        previous = 0.0
        for fraction in (0.25, 0.5, 0.75, 1.0):
            current = fraction * speed
            expected.extend([('seed', previous), ('current', current), ('solve', current)])
            previous = current
        assert model.calls == expected
        assert model.state.name == 'InStaticState'
        assert model.RefCurrentSpeed == speed


def test_zero_current_solves_once():
    model = StagedModel(speed=0.0)
    load_driver().calculate_staged_statics(model)
    assert model.calls == [('solve', 0.0)]


def test_invalid_current_is_refused_before_statics():
    import math
    import pytest
    driver = load_driver()
    for speed in (-1.0, float('nan'), float('inf'), float('-inf')):
        model = StagedModel(speed=speed)
        with pytest.raises(ValueError, match='finite and nonnegative'):
            driver.calculate_staged_statics(model)
        assert model.calls == []
        assert model.state.name == 'Reset'
        assert (math.isnan(model.RefCurrentSpeed) if math.isnan(speed)
                else model.RefCurrentSpeed == speed)


def test_staged_failure_never_reaches_dynamics():
    import pytest
    driver = load_driver()
    for stage in (0.0, 0.25, 0.5, 0.75, 1.0):
        for bad_state in (False, True):
            model = StagedModel(fail_stage=stage, bad_state=bad_state)
            with pytest.raises((RuntimeError, ValueError)):
                driver.calculate_staged_statics(model)
                model.RunSimulation()
            assert not any(call[0] == 'dynamics' for call in model.calls)
            assert model.RefCurrentSpeed == 1.0
            solves = [call[1] for call in model.calls if call[0] == 'solve']
            assert solves == [value for value in (0.0, 0.25, 0.5, 0.75, 1.0) if value <= stage]
            if stage == 0.0:
                assert not any(call[0] == 'seed' for call in model.calls)


def test_successful_staging_precedes_dynamics():
    import pytest
    driver = load_driver()
    model = StagedModel()
    with pytest.raises(RuntimeError, match='synthetic dynamics reached'):
        driver.calculate_staged_statics(model)
        model.RunSimulation()
    assert model.calls[-3:] == [('current', 1.0), ('solve', 1.0), ('dynamics', 1.0)]
    tree = ast.parse(DRIVER.read_text(encoding='utf-8'))
    run = next(n for n in tree.body if isinstance(n, ast.FunctionDef) and n.name == 'run_case')
    calls = [n for n in ast.walk(run) if isinstance(n, ast.Call)]
    staging = next(n.lineno for n in calls if isinstance(n.func, ast.Name) and n.func.id == 'calculate_staged_statics')
    dynamics = next(n.lineno for n in calls if isinstance(n.func, ast.Attribute) and n.func.attr == 'RunSimulation')
    assert staging < dynamics


def test_failure_message_is_bounded(tmp_path):
    from digitalmodel.drilling_riser.generic_campaign import DEST
    driver = load_driver()
    message = 'synthetic non-convergence ' + 'x' * 600
    def broken(*args):
        raise RuntimeError(message)
    assert driver.run_campaign(ROOT / DEST, tmp_path, criteria_file=CRITERIA,
                               case_runner=broken) == 1
    records = list(tmp_path.glob('*.failure.json'))
    assert len(records) == 24
    for path in records:
        record = json.loads(path.read_text(encoding='utf-8'))
        assert record['message'] == message[:500]
        assert record['error_type'] == 'RuntimeError'



def test_ramp_matches_repository_precedent_without_importing_it():
    source = ROOT / 'src/digitalmodel/drilling_riser/campaign.py'
    tree = ast.parse(source.read_text(encoding='utf-8'))
    assignment = next(node for node in tree.body if isinstance(node, ast.Assign)
                      and any(isinstance(target, ast.Name) and target.id == 'CURRENT_RAMP'
                              for target in node.targets))
    assert load_driver().CURRENT_RAMP == ast.literal_eval(assignment.value)


def test_position_retention_failure_restores_current_and_stops():
    import sys
    import pytest
    from types import SimpleNamespace
    driver = load_driver()
    for stage in (0.0, 0.25, 0.5, 0.75):
        model = StagedModel()
        retain = model.UseCalculatedPositions

        def fail_retention(enabled):
            if model.speed == stage:
                raise RuntimeError('synthetic position retention failure')
            retain(enabled)

        model.UseCalculatedPositions = fail_retention
        with patch.dict(sys.modules, {'OrcFxAPI': SimpleNamespace(Model=lambda _: model)}):
            with patch.object(driver, 'ring_reference_z', return_value=37), \
                    patch.object(driver, 'case_tension_basis', return_value={}):
                with pytest.raises(RuntimeError, match='position retention failure'):
                    driver.run_case(Path('unused'), Path('unused'))
        assert model.RefCurrentSpeed == 1.0
        assert not any(call[0] == 'dynamics' for call in model.calls)
        assert [call[1] for call in model.calls if call[0] == 'solve'] == [
            value for value in (0.0, 0.25, 0.5, 0.75) if value <= stage]


def test_restore_failure_preserves_original_error_and_blocks_dynamics():
    import sys
    import pytest
    from types import SimpleNamespace

    class RestoreFailureModel(StagedModel):
        @StagedModel.RefCurrentSpeed.setter
        def RefCurrentSpeed(self, value):
            if value == 1.0:
                raise OSError('synthetic restoration failure')
            StagedModel.RefCurrentSpeed.fset(self, value)

    driver = load_driver()
    model = RestoreFailureModel(fail_stage=0.25)
    with patch.dict(sys.modules, {'OrcFxAPI': SimpleNamespace(Model=lambda _: model)}):
        with patch.object(driver, 'ring_reference_z', return_value=37), \
                patch.object(driver, 'case_tension_basis', return_value={}):
            with pytest.raises(OSError, match='restoration failure') as caught:
                driver.run_case(Path('unused'), Path('unused'))
    assert isinstance(caught.value.__context__, RuntimeError)
    assert str(caught.value.__context__) == 'synthetic non-convergence'
    assert not any(call[0] == 'dynamics' for call in model.calls)


if __name__ == '__main__':
    unittest.main()
