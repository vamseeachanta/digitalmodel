# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Driver contract exercised with a synthetic case runner only."""
import ast
import importlib.util
import json
import os
from pathlib import Path
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
DRIVER = ROOT / 'scripts/orcaflex/run_generic_drilling_riser_campaign.py'


def load_driver():
    spec = importlib.util.spec_from_file_location('generic_driver', DRIVER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class DriverTests(unittest.TestCase):
    def test_invalid_criteria_are_rejected(self):
        driver = load_driver()
        from digitalmodel.drilling_riser.generic_campaign import case_table
        criteria = case_table(ROOT)['criteria']
        criteria['mean_angle_deg']['value'] = -2
        with self.assertRaisesRegex(ValueError, 'finite and positive'):
            driver.criterion_rows({name: 1.0 for name in driver.METRICS}, criteria)

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
        with tempfile.TemporaryDirectory(dir=os.environ['TMPDIR']) as temporary:
            output = Path(temporary)
            calls = []
            def fake(model, simulation):
                calls.append(model)
                simulation.write_text('synthetic', encoding='utf-8')
                return {**{name: 1.0 for name in driver.METRICS},
                        'solver_version': 'synthetic', 'native_state': 'SimulationStopped',
                        'wall_seconds': 2.0}
            self.assertEqual(driver.run_campaign(campaign, output, case_runner=fake), 0)
            self.assertEqual(len(calls), 24)
            self.assertEqual(driver.run_campaign(campaign, output, case_runner=fake), 0)
            self.assertEqual(len(calls), 24)
            self.assertEqual(len(list(output.glob('*.sim'))), 4)
            self.assertEqual(json.loads((output / 'timing.json').read_text(encoding='utf-8'))['projected_seconds'], 48)
            retained = next(output.glob('*.sim'))
            retained.write_text('damaged synthetic artifact', encoding='utf-8')
            self.assertEqual(driver.run_campaign(campaign, output, case_runner=fake), 0)
            self.assertEqual(len(calls), 25)
        with tempfile.TemporaryDirectory(dir=os.environ['TMPDIR']) as temporary:
            def unavailable(*args):
                raise driver.LicenceUnavailable('synthetic unavailable')
            self.assertEqual(driver.run_campaign(campaign, Path(temporary), case_runner=unavailable), 12)
        with tempfile.TemporaryDirectory(dir=os.environ['TMPDIR']) as temporary:
            versions = []
            def mixed(model, simulation):
                result = fake(model, simulation)
                versions.append('next')
                result['solver_version'] = str(len(versions))
                return result
            self.assertEqual(driver.run_campaign(campaign, Path(temporary), case_runner=mixed), 1)
            self.assertEqual(len(list(Path(temporary).glob('*.summary.json'))), 1)
            self.assertFalse((Path(temporary) / 'retention.json').exists())
        with tempfile.TemporaryDirectory(dir=os.environ['TMPDIR']) as temporary:
            def broken(*args):
                raise ValueError('synthetic invalid channel')
            self.assertEqual(driver.run_campaign(campaign, Path(temporary), case_runner=broken), 1)
            failures = list(Path(temporary).glob('*.failure.json'))
            self.assertEqual(len(failures), 24)
            self.assertFalse(list(Path(temporary).glob('*.summary.json')))


if __name__ == '__main__':
    unittest.main()
