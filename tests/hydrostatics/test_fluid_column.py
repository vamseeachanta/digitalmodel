"""Tests for fluid-column hydrostatics.

Oracle values are the computed cells of a generic "Fluid Column and
Pressure" design spreadsheet (seabed-pressure worksheet). All inputs
are generic round numbers — no project- or client-specific data.

Worksheet scenario (generic):
    density        = 18.0 ppg          -> 2156.875679 kg/m^3
    water depth    = 10000 ft          -> 3048 m
    seabed pressure= 20000 psi         -> 137894896.5 Pa
    surface press. = seabed - rho*g*h  -> 73402415.65 Pa = 10646.13956 psi
"""

import numpy as np
import pytest

from digitalmodel.hydrostatics.fluid_column import (
    ppg_to_kg_m3,
    ft_to_m,
    psi_to_pa,
    pa_to_psi,
    bar_to_psi,
    psi_to_bar,
    hydrostatic_pressure,
    seabed_pressure,
)


class TestUnitConversions:
    def test_ppg_to_kg_m3(self):
        assert ppg_to_kg_m3(18.0) == pytest.approx(2156.875679, rel=1e-6)

    def test_ft_to_m(self):
        assert ft_to_m(10000.0) == pytest.approx(3048.0, rel=1e-9)

    def test_psi_to_pa(self):
        assert psi_to_pa(20000.0) == pytest.approx(137894896.5, rel=1e-6)

    def test_pa_to_psi_roundtrip(self):
        assert pa_to_psi(137894896.5) == pytest.approx(20000.0, rel=1e-6)

    def test_surface_pressure_pa_to_psi(self):
        # 73402415.65 Pa -> 10646.13956 psi (worksheet)
        assert pa_to_psi(73402415.65) == pytest.approx(10646.13956, rel=1e-6)

    def test_bar_psi_roundtrip(self):
        # 1 bar = 1e5 Pa -> ~14.5038 psi (uses the sheet's psi<->Pa factor,
        # so it agrees with the literature 14.5037738 to ~1e-5, not exactly).
        assert bar_to_psi(1.0) == pytest.approx(14.5037738, rel=1e-5)
        # round-trip must be exact regardless of the constant chosen
        assert psi_to_bar(bar_to_psi(1.0)) == pytest.approx(1.0, rel=1e-12)


class TestHydrostaticPressure:
    def test_rho_g_h(self):
        rho = 2156.875679       # kg/m^3 (18 ppg)
        h = 3048.0              # m (10000 ft)
        # worksheet: seabed - surface = rho*g*h
        expected = 137894896.5 - 73402415.65
        assert hydrostatic_pressure(rho, h, gravity=9.81) == pytest.approx(
            expected, rel=1e-4
        )

    def test_vectorised(self):
        rho = 1025.0
        h = np.array([0.0, 100.0, 1000.0])
        got = hydrostatic_pressure(rho, h)
        expected = rho * 9.81 * h
        np.testing.assert_allclose(got, expected, rtol=1e-12)


class TestSeabedPressure:
    def test_matches_worksheet(self):
        # surface + rho*g*h -> seabed pressure (all in Pa)
        rho = ppg_to_kg_m3(18.0)
        h = ft_to_m(10000.0)
        surface_pa = 73402415.65
        got = seabed_pressure(rho, h, surface_pressure=surface_pa, gravity=9.81)
        assert got == pytest.approx(137894896.5, rel=1e-4)
        # and in psi -> 20000 psi
        assert pa_to_psi(got) == pytest.approx(20000.0, rel=1e-3)

    def test_zero_surface_is_gauge(self):
        rho, h = 1025.0, 50.0
        assert seabed_pressure(rho, h) == pytest.approx(
            hydrostatic_pressure(rho, h), rel=1e-12
        )
