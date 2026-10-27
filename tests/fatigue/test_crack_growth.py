"""Tests for crack growth module — Paris' law integration.

Material constants: BS 7910 mean values for steel in air.
  C = 1.65e-11  (m/cycle per (MPa*sqrt(m))^m)
  m = 3.0
"""

import math

import numpy as np
import pandas as pd
import pytest

from digitalmodel.fatigue.crack_growth import (
    crack_growth_curve_plot,
    inspection_interval,
    paris_law_life,
    stress_intensity_factor,
)

# ── BS 7910 mean steel constants ──────────────────────────────────────
C_STEEL = 1.65e-11  # m/cycle per (MPa√m)^3
M_STEEL = 3.0


# ── stress_intensity_factor ───────────────────────────────────────────

class TestStressIntensityFactor:
    def test_through_thickness_default(self):
        """DeltaK = 1.12 * stress * sqrt(pi * a) for default geometry."""
        stress = 100.0  # MPa
        a = 0.005  # 5 mm
        expected = 1.12 * stress * math.sqrt(math.pi * a)
        result = stress_intensity_factor(stress, a)
        assert result == pytest.approx(expected, rel=1e-10)

    def test_custom_Y_factor(self):
        """User-supplied Y overrides default 1.12."""
        stress, a, Y = 80.0, 0.01, 1.0
        expected = Y * stress * math.sqrt(math.pi * a)
        result = stress_intensity_factor(stress, a, Y=Y)
        assert result == pytest.approx(expected, rel=1e-10)

    def test_surface_geometry(self):
        """Surface geometry uses Y=1.12."""
        stress, a = 100.0, 0.005
        result = stress_intensity_factor(stress, a, geometry="surface")
        expected = 1.12 * stress * math.sqrt(math.pi * a)
        assert result == pytest.approx(expected, rel=1e-10)

    def test_embedded_geometry(self):
        """Embedded crack uses Y = 2/pi."""
        stress, a = 100.0, 0.005
        result = stress_intensity_factor(stress, a, geometry="embedded")
        expected = (2.0 / math.pi) * stress * math.sqrt(math.pi * a)
        assert result == pytest.approx(expected, rel=1e-10)

    def test_unknown_geometry_raises(self):
        with pytest.raises(ValueError, match="Unknown geometry"):
            stress_intensity_factor(100.0, 0.005, geometry="unknown")

    def test_array_input(self):
        """Accepts array of crack sizes."""
        stress = 100.0
        a_arr = np.array([0.001, 0.005, 0.01])
        result = stress_intensity_factor(stress, a_arr)
        assert isinstance(result, np.ndarray)
        assert len(result) == 3
        for i, a in enumerate(a_arr):
            expected = 1.12 * stress * math.sqrt(math.pi * a)
            assert result[i] == pytest.approx(expected, rel=1e-10)


# ── paris_law_life ────────────────────────────────────────────────────

class TestParisLawLife:
    def test_constant_delta_k_analytical(self):
        """With constant DeltaK, analytical solution:
        N = (a_crit - a_init) / (C * DeltaK^m)
        """
        delta_K = 20.0  # MPa*sqrt(m)
        a_init = 0.001
        a_crit = 0.010
        da_dN = C_STEEL * delta_K ** M_STEEL
        expected_N = (a_crit - a_init) / da_dN

        N, history = paris_law_life(C_STEEL, M_STEEL, delta_K, a_init, a_crit)
        # Numerical integration with 1000 steps should be very close
        assert N == pytest.approx(expected_N, rel=1e-3)

    def test_history_dataframe_columns(self):
        """History DataFrame has required columns."""
        N, history = paris_law_life(C_STEEL, M_STEEL, 15.0, 0.001, 0.005)
        assert isinstance(history, pd.DataFrame)
        assert set(history.columns) == {"crack_size", "cycles", "da_dN"}

    def test_history_monotonic(self):
        """Crack size and cycles are monotonically increasing."""
        N, history = paris_law_life(C_STEEL, M_STEEL, 20.0, 0.001, 0.01)
        assert np.all(np.diff(history["crack_size"]) >= 0)
        assert np.all(np.diff(history["cycles"]) >= 0)

    def test_callable_delta_k(self):
        """Accepts callable DeltaK(a) — stress-dependent integration."""
        stress = 100.0

        def dk_func(a):
            return stress_intensity_factor(stress, a)

        N, history = paris_law_life(C_STEEL, M_STEEL, dk_func, 0.001, 0.01)
        assert N > 0
        assert len(history) > 2

    def test_invalid_initial_ge_critical(self):
        with pytest.raises(ValueError, match="a_initial must be less"):
            paris_law_life(C_STEEL, M_STEEL, 10.0, 0.01, 0.01)

    def test_invalid_negative_initial(self):
        with pytest.raises(ValueError, match="a_initial must be positive"):
            paris_law_life(C_STEEL, M_STEEL, 10.0, -0.001, 0.01)

    def test_custom_da_step(self):
        """Custom da_step produces different number of integration steps."""
        _, h_default = paris_law_life(C_STEEL, M_STEEL, 20.0, 0.001, 0.01)
        _, h_coarse = paris_law_life(
            C_STEEL, M_STEEL, 20.0, 0.001, 0.01, da_step=0.003
        )
        assert len(h_coarse) < len(h_default)

    def test_realistic_steel_life_order_of_magnitude(self):
        """Sanity check: through-thickness crack in steel plate at 100 MPa
        should give life in millions of cycles range."""
        stress = 100.0  # MPa

        def dk(a):
            return stress_intensity_factor(stress, a)

        N, _ = paris_law_life(C_STEEL, M_STEEL, dk, 0.001, 0.025)
        # Expect order of 1e5 to 1e7 cycles for this scenario
        assert 1e4 < N < 1e8


# ── inspection_interval ──────────────────────────────────────────────

class TestInspectionInterval:
    def test_basic_result_keys(self):
        result = inspection_interval(
            C_STEEL, M_STEEL,
            stress_range=100.0,
            a_initial=0.001,
            a_critical=0.025,
            a_detectable=0.005,
        )
        assert "total_life" in result
        assert "detectable_life" in result
        assert "inspection_interval" in result
        assert "total_history" in result
        assert "detectable_history" in result

    def test_detectable_life_less_than_total(self):
        result = inspection_interval(
            C_STEEL, M_STEEL,
            stress_range=100.0,
            a_initial=0.001,
            a_critical=0.025,
            a_detectable=0.005,
        )
        assert result["detectable_life"] < result["total_life"]

    def test_safety_factor_applied(self):
        sf = 3.0
        result = inspection_interval(
            C_STEEL, M_STEEL,
            stress_range=100.0,
            a_initial=0.001,
            a_critical=0.025,
            a_detectable=0.005,
            safety_factor=sf,
        )
        assert result["inspection_interval"] == pytest.approx(
            result["detectable_life"] / sf, rel=1e-10
        )

    def test_invalid_detectable_less_than_initial(self):
        with pytest.raises(ValueError, match="a_detectable must be >= a_initial"):
            inspection_interval(
                C_STEEL, M_STEEL,
                stress_range=100.0,
                a_initial=0.005,
                a_critical=0.025,
                a_detectable=0.001,
            )

    def test_invalid_detectable_ge_critical(self):
        with pytest.raises(ValueError, match="a_detectable must be < a_critical"):
            inspection_interval(
                C_STEEL, M_STEEL,
                stress_range=100.0,
                a_initial=0.001,
                a_critical=0.025,
                a_detectable=0.025,
            )


# ── crack_growth_curve_plot ──────────────────────────────────────────

class TestCrackGrowthPlot:
    def test_returns_axes(self):
        """Plot function returns a matplotlib Axes object."""
        try:
            import matplotlib
            matplotlib.use("Agg")  # non-interactive backend for CI
        except ImportError:
            pytest.skip("matplotlib not available")

        _, history = paris_law_life(C_STEEL, M_STEEL, 20.0, 0.001, 0.01)
        ax = crack_growth_curve_plot(history)
        assert ax is not None
        assert hasattr(ax, "get_xlabel")


def test_legacy_coarse_start_converges_to_independent_integral():
    """Synthetic C=1e-11, m=2, K=100 sqrt(a): N=ln(5)/1e-7."""
    cycles, history = paris_law_life(
        1e-11, 2, lambda a: 100 * math.sqrt(a), 0.001, 0.005, da_step=0.004,
    )
    assert cycles == pytest.approx(math.log(5) / 1e-7, rel=1e-6)
    assert history.iloc[-1]["crack_size"] == pytest.approx(0.005)
    assert history.iloc[-1]["cycles"] == cycles


def test_legacy_default_life_matches_shared_growth_api():
    from digitalmodel.fatigue.threshold_growth import GrowthLaw, integrate_growth

    def dk(a):
        return 100 * math.sqrt(a)
    material = GrowthLaw(1e-11, 2, 0, 0, 1, "Synthetic arithmetic inputs")
    expected = integrate_growth(
        material, dk, initial_m=0.001, limit_m=0.005, steps=1000,
    )
    cycles, history = paris_law_life(1e-11, 2, dk, 0.001, 0.005)
    assert cycles == pytest.approx(expected.life_cycles, rel=1e-12)
    assert history["cycles"].tolist() == pytest.approx(
        [point.cycles for point in expected.history]
    )


@pytest.mark.parametrize("at_start", [False, True])
def test_legacy_arrest_returns_infinite_life_and_only_reached_points(at_start):
    def dk(a):
        return 0 if at_start or a >= 0.003 else 10
    cycles, history = paris_law_life(1e-11, 3, dk, 0.001, 0.005, da_step=0.001)
    assert math.isinf(cycles)
    assert list(history.columns) == ["crack_size", "cycles", "da_dN"]
    assert history["crack_size"].tolist() == pytest.approx(
        [0.001] if at_start else [0.001, 0.002]
    )
    assert history["cycles"].tolist() == pytest.approx(
        [0] if at_start else [0, 100_000]
    )


@pytest.mark.parametrize(
    "field, value",
    [(field, value) for field in ("C", "m", "a_initial", "a_critical", "da_step")
     for value in (True, np.bool_(False), "1", None, float("inf"))
     if not (field == "da_step" and value is None)],
)
def test_legacy_scalar_validation_even_for_arrest(field, value):
    inputs = dict(C=1e-11, m=3, delta_K=0, a_initial=0.001, a_critical=0.005, da_step=0.001)
    inputs[field] = value
    with pytest.raises(ValueError):
        paris_law_life(**inputs)


@pytest.mark.parametrize("field", ["C", "m", "da_step"])
@pytest.mark.parametrize("value", [0, -1])
def test_legacy_invalid_positive_parameters_rejected_before_zero_rate(field, value):
    inputs = dict(C=1e-11, m=3, delta_K=0, a_initial=0.001, a_critical=0.005, da_step=0.001)
    inputs[field] = value
    with pytest.raises(ValueError):
        paris_law_life(**inputs)


@pytest.mark.parametrize("value", [True, np.bool_(False), "0", None])
def test_legacy_invalid_scalar_delta_k_rejected(value):
    with pytest.raises(ValueError):
        paris_law_life(1e-11, 3, value, 0.001, 0.005)


@pytest.mark.parametrize("field", ["stress_range", "crack_size", "Y"])
@pytest.mark.parametrize("value", [True, np.bool_(False), "1", None, float("inf")])
def test_sif_scalar_validation(field, value):
    inputs = dict(stress_range=100, crack_size=0.005, Y=1.12)
    inputs[field] = value
    with pytest.raises(ValueError):
        stress_intensity_factor(**inputs)


@pytest.mark.parametrize("value", [[True, False], [0.001, float("nan")], ["0.001", "0.005"]])
def test_sif_invalid_array_elements_rejected(value):
    with pytest.raises(ValueError):
        stress_intensity_factor(100, value)


@pytest.mark.parametrize("field", ["a_initial", "a_critical", "a_detectable", "safety_factor"])
@pytest.mark.parametrize("value", [True, "1", None, float("nan")])
def test_legacy_inspection_scalar_validation(field, value):
    inputs = dict(C=1e-11, m=3, stress_range=100, a_initial=0.001,
                  a_critical=0.005, a_detectable=0.002, safety_factor=2)
    inputs[field] = value
    with pytest.raises(ValueError):
        inspection_interval(**inputs)


@pytest.mark.parametrize("factor", [0, -1])
def test_legacy_inspection_factor_must_be_positive(factor):
    with pytest.raises(ValueError):
        inspection_interval(1e-11, 3, 100, 0.001, 0.005, 0.002, safety_factor=factor)
