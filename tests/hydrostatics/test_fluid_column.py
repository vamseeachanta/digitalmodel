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

from pathlib import Path

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


class TestPipelinePressureBasis:
    """Independent arithmetic from the synthetic inputs in the basis note.

    Source: docs/examples/pipeline-hydrostatic-pressure-basis.md, Synthetic
    basis. A unit-area column has mass 1000 * 75 = 75000 kg and weight
    75000 * 10 = 750000 N. Its head is 750000 Pa; adding the supplied
    120000 Pa absolute surface pressure gives 870000 Pa absolute.
    """

    def test_synthetic_gauge_and_absolute_pressure(self):
        # Inputs and exact expected values: synthetic basis cited above.
        density, gravity, height, surface_pa = 1000.0, 10.0, 75.0, 120000.0
        assert hydrostatic_pressure(density, height, gravity=gravity) == 750000.0
        assert seabed_pressure(
            density, height, surface_pressure=surface_pa, gravity=gravity
        ) == 870000.0

    @pytest.mark.parametrize(
        "datum_shift_m",
        [0.0, 250.0, -400.0],
        ids=["original-datum", "raised-datum", "lowered-datum"],
    )
    def test_height_datum_invariance(self, datum_shift_m):
        # Source: basis note, Datum convention; elevations increase upward.
        top_m = 20.0 + datum_shift_m
        bottom_m = -55.0 + datum_shift_m
        height_m = top_m - bottom_m
        assert height_m == 75.0
        assert hydrostatic_pressure(1000.0, height_m, gravity=10.0) == 750000.0
        assert seabed_pressure(
            1000.0, height_m, surface_pressure=120000.0, gravity=10.0
        ) == 870000.0

    def test_documented_example_executes(self):
        note = Path("docs/examples/pipeline-hydrostatic-pressure-basis.md")
        text = (Path(__file__).resolve().parents[2] / note).read_text(
            encoding="utf-8"
        )
        _, opening, remainder = text.partition("```python\n")
        assert opening, "The executable pressure-basis example is missing"
        snippet, closing, tail = remainder.partition("```")
        assert closing, "The example fence is not closed"
        assert "```python" not in tail, "Keep one executable basis example"
        namespace = {"__name__": "__main__"}
        exec(compile(snippet, note.as_posix(), "exec"), namespace)
        # Expected values come from the unit-area hand calculation above.
        assert namespace["height_m"] == 75.0
        assert namespace["gauge_pa"] == 750000.0
        assert namespace["absolute_pa"] == 870000.0
