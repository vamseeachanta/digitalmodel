"""Tests for Pint performance benchmark functions.

TDD: written before benchmark implementation to validate correctness
of hydrostatic pressure and wave loading (Morison equation) calculations.

Reference: vamseeachanta/workspace-hub#1486
"""

import numpy as np
import pint
import pytest

ureg = pint.UnitRegistry()
Q_ = ureg.Quantity

# Physical constants
RHO = 1025.0  # seawater density, kg/m^3
G = 9.80665  # gravitational acceleration, m/s^2

RHO_Q = Q_(RHO, "kg/m**3")
G_Q = Q_(G, "m/s**2")


# ── Hydrostatic pressure P = rho * g * h ──────────────────────────────────


class TestHydrostaticPressure:
    """Verify hydrostatic pressure calculations return correct physical results."""

    def test_known_value_100m(self):
        """P at 100 m depth: 1025 * 9.80665 * 100 = 1,005,181.625 Pa."""
        expected_pa = RHO * G * 100.0
        assert abs(expected_pa - 1_005_181.625) < 0.01

    def test_raw_numpy_hydrostatic(self):
        """Raw NumPy returns correct hydrostatic pressure array."""
        depths = np.array([0.0, 100.0, 3000.0])
        result = RHO * G * depths
        assert result[0] == 0.0
        assert abs(result[1] - 1_005_181.625) < 0.01
        assert abs(result[2] - 30_155_448.75) < 1.0

    def test_pint_quantity_hydrostatic(self):
        """Full Pint Quantity returns correct pressure in Pa."""
        depths = Q_(np.array([0.0, 100.0, 3000.0]), "m")
        result = RHO_Q * G_Q * depths
        result_pa = result.to("Pa").magnitude
        expected = RHO * G * np.array([0.0, 100.0, 3000.0])
        np.testing.assert_allclose(result_pa, expected, rtol=1e-10)

    def test_ureg_wraps_hydrostatic(self):
        """@ureg.wraps decorator returns correct pressure in Pa."""

        @ureg.wraps("Pa", ("kg/m**3", "m/s**2", "m"))
        def calc(rho, g, depth):
            return rho * g * depth

        depths = Q_(np.array([0.0, 100.0, 3000.0]), "m")
        result = calc(RHO_Q, G_Q, depths)
        expected = RHO * G * np.array([0.0, 100.0, 3000.0])
        np.testing.assert_allclose(result.magnitude, expected, rtol=1e-10)

    def test_all_three_methods_agree(self):
        """All three calculation methods produce identical results."""
        depths_np = np.linspace(0, 3000, 500)
        depths_q = Q_(depths_np, "m")

        @ureg.wraps("Pa", ("kg/m**3", "m/s**2", "m"))
        def calc_wraps(rho, g, depth):
            return rho * g * depth

        raw = RHO * G * depths_np
        pint_result = (RHO_Q * G_Q * depths_q).to("Pa").magnitude
        wraps_result = calc_wraps(RHO_Q, G_Q, depths_q).magnitude

        np.testing.assert_allclose(raw, pint_result, rtol=1e-10)
        np.testing.assert_allclose(raw, wraps_result, rtol=1e-10)


# ── Wave loading (Morison equation) F = 0.5 * Cd * rho * A * v^2 ─────────


class TestMorisonEquation:
    """Verify Morison drag force calculations return correct physical results."""

    # Typical values for a 1m-diameter cylinder
    CD = 1.0  # drag coefficient (dimensionless)
    DIAMETER = 1.0  # m
    AREA = DIAMETER * 1.0  # projected area per unit length, m^2/m

    CD_Q = Q_(CD, "dimensionless")
    AREA_Q = Q_(AREA, "m**2")

    def test_known_morison_force(self):
        """F at v=2 m/s: 0.5 * 1.0 * 1025 * 1.0 * 4.0 = 2050.0 N."""
        v = 2.0
        expected = 0.5 * self.CD * RHO * self.AREA * v**2
        assert abs(expected - 2050.0) < 0.01

    def test_raw_numpy_morison(self):
        """Raw NumPy Morison calculation over velocity array."""
        velocities = np.array([0.0, 1.0, 2.0, 5.0])
        result = 0.5 * self.CD * RHO * self.AREA * velocities**2
        assert result[0] == 0.0
        assert abs(result[1] - 512.5) < 0.01
        assert abs(result[2] - 2050.0) < 0.01
        assert abs(result[3] - 12812.5) < 0.01

    def test_pint_quantity_morison(self):
        """Full Pint Quantity Morison returns correct force in N."""
        velocities = Q_(np.array([0.0, 1.0, 2.0, 5.0]), "m/s")
        result = 0.5 * self.CD_Q * RHO_Q * self.AREA_Q * velocities**2
        result_n = result.to("N").magnitude
        expected = 0.5 * self.CD * RHO * self.AREA * np.array([0.0, 1.0, 2.0, 5.0]) ** 2
        np.testing.assert_allclose(result_n, expected, rtol=1e-10)

    def test_ureg_wraps_morison(self):
        """@ureg.wraps Morison returns correct force in N.

        Note: Cd is dimensionless — use None in @ureg.wraps signature
        and pass the raw float. Pint 0.25 has a bug with "dimensionless"
        in @ureg.wraps input specs.
        """

        @ureg.wraps("kg * m / s**2", (None, "kg/m**3", "m**2", "m/s"))
        def calc(cd, rho, area, v):
            return 0.5 * cd * rho * area * v**2

        velocities = Q_(np.array([0.0, 1.0, 2.0, 5.0]), "m/s")
        result = calc(self.CD, RHO_Q, self.AREA_Q, velocities)
        expected = 0.5 * self.CD * RHO * self.AREA * np.array([0.0, 1.0, 2.0, 5.0]) ** 2
        np.testing.assert_allclose(result.magnitude, expected, rtol=1e-10)

    def test_morison_force_is_positive(self):
        """Morison drag force should be non-negative for any velocity."""
        velocities = np.linspace(-5, 5, 100)
        result = 0.5 * self.CD * RHO * self.AREA * velocities**2
        assert np.all(result >= 0.0)

    def test_all_three_morison_agree(self):
        """All three Morison methods produce identical results."""
        v_np = np.linspace(0, 10, 200)
        v_q = Q_(v_np, "m/s")

        @ureg.wraps("kg * m / s**2", (None, "kg/m**3", "m**2", "m/s"))
        def calc_wraps(cd, rho, area, v):
            return 0.5 * cd * rho * area * v**2

        raw = 0.5 * self.CD * RHO * self.AREA * v_np**2
        pint_result = (0.5 * self.CD_Q * RHO_Q * self.AREA_Q * v_q**2).to("N").magnitude
        wraps_result = calc_wraps(self.CD, RHO_Q, self.AREA_Q, v_q).magnitude

        np.testing.assert_allclose(raw, pint_result, rtol=1e-10)
        np.testing.assert_allclose(raw, wraps_result, rtol=1e-10)
