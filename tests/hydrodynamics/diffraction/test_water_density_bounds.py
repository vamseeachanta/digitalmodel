"""Standalone schema tests with no solver package initialization."""
import importlib.util
from pathlib import Path
import sys
import unittest


ROOT = Path(__file__).resolve().parents[3]


class WaterDensityBoundsTest(unittest.TestCase):
    def test_both_environment_schemas(self):
        for domain in ("diffraction", "passing_ship"):
            path = ROOT / f"src/digitalmodel/hydrodynamics/{domain}/input_schemas.py"
            spec = importlib.util.spec_from_file_location(f"density_{domain}", path)
            module = importlib.util.module_from_spec(spec)
            sys.modules[spec.name] = module
            spec.loader.exec_module(module)
            for value in (958.65, 1000, 1025, 1214):
                with self.subTest(domain=domain, accepted=value):
                    env = module.EnvironmentSpec(water_depth=100, water_density=value)
                    self.assertEqual(env.water_density, value)
            for value in (958.64, 1214.01, 1.025, float("nan"), float("inf")):
                with self.subTest(domain=domain, rejected=value):
                    with self.assertRaisesRegex(ValueError, "water_density"):
                        module.EnvironmentSpec(water_depth=100, water_density=value)


if __name__ == "__main__":
    unittest.main()
