"""Standalone numerical QA tests; no package initializers or solver imports.

Run directly with unittest to exercise the pure numerical module in isolation.
Synthetic values below are test cases, not qualified engineering inputs.
"""
import importlib.util
from pathlib import Path
import unittest


SOURCE = Path(__file__).resolve().parents[3] / "src/digitalmodel/hydrodynamics/diffraction/equilibrium_checks.py"


class EquilibriumChecksTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        spec = importlib.util.spec_from_file_location("equilibrium_checks", SOURCE)
        cls.qa = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(cls.qa)

    def test_horizontal_alignment_ignores_vertical_separation(self):
        self.assertEqual(self.qa.check_cog_cob([0, 0, 5], [0, 0, -5], 0), [])

    def test_horizontal_offset_fails(self):
        self.assertTrue(self.qa.check_cog_cob([1, 0, 0], [0, 0, 0], 0.5))

    def test_alignment_boundary(self):
        self.assertEqual(self.qa.check_cog_cob([3, 4, 0], [0, 0, 0], 5), [])

    def test_mass_balance_and_boundary(self):
        self.assertEqual(self.qa.check_mass_displacement(1000, 1, 1000, 0), [])
        self.assertEqual(self.qa.check_mass_displacement(1100, 1, 1000, 0.1), [])
        self.assertTrue(self.qa.check_mass_displacement(1101, 1, 1000, 0.1))

    def test_force_balance_componentwise(self):
        self.assertEqual(self.qa.check_force_balance([100, 0, 0], [101, 0, 0], 0.01, 0), [])
        self.assertTrue(self.qa.check_force_balance([100, 0, 0], [100, 1, 0], 0.01, 0))

    def test_force_sign_and_zero_reference(self):
        self.assertTrue(self.qa.check_force_balance([1, 0, 0], [-1, 0, 0], 0, 0))
        self.assertEqual(self.qa.check_force_balance([0, 0, 0], [0, 0, 0], 0, 0), [])

    def test_invalid_values_fail_closed(self):
        for value in (float("nan"), float("inf"), -1):
            with self.subTest(value=value), self.assertRaises(ValueError):
                self.qa.check_mass_displacement(value, 1, 1000, 0.01)
        with self.assertRaises(ValueError):
            self.qa.check_cog_cob([0, 0], [0, 0, 0], 0)
        with self.assertRaises(ValueError):
            self.qa.check_force_balance([], [], 0, 0)
        with self.assertRaises(ValueError):
            self.qa.check_cog_cob([0, 0, 0], [0, 0, 0], -1)


if __name__ == "__main__":
    unittest.main()
