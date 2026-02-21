"""Tests for ForceTimeHistory and generate_time_history (WRK-131 Phase 2).

Tests cover:
- generate_time_history returns a ForceTimeHistory
- time array starts at 0 and is monotonically increasing
- stagger range covers expected values
- peak_sway > 0 for typical separation
- to_dataframe has correct columns
- to_csv writes a readable file
- Faster speed → shorter duration
- Closer separation → higher peak forces
- ValueError on invalid inputs
"""

import tempfile
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.passing_ship.calculator import (
    PassingShipCalculator,
)
from digitalmodel.hydrodynamics.passing_ship.configuration import (
    CalculationConfig,
    EnvironmentalConfig,
    VesselConfig,
)
from digitalmodel.hydrodynamics.passing_ship.force_time_history import (
    ForceTimeHistory,
    generate_time_history,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture(scope="module")
def moored_vessel():
    return VesselConfig(
        length=200.0,
        beam=32.0,
        draft=12.0,
        block_coefficient=0.82,
    )


@pytest.fixture(scope="module")
def passing_vessel():
    return VesselConfig(
        length=250.0,
        beam=42.0,
        draft=15.0,
        block_coefficient=0.78,
    )


@pytest.fixture(scope="module")
def calculator(moored_vessel, passing_vessel):
    env = EnvironmentalConfig(water_depth=None)  # infinite depth
    calc_cfg = CalculationConfig(cache_size=500)
    return PassingShipCalculator(
        moored_vessel=moored_vessel,
        passing_vessel=passing_vessel,
        environment=env,
        calculation_config=calc_cfg,
    )


@pytest.fixture(scope="module")
def standard_fth(calculator):
    """Standard time history: 2 m/s, 100 m separation, 5 s time step."""
    return generate_time_history(
        calculator,
        speed_ms=2.0,
        separation_m=100.0,
        dt=5.0,
    )


# ---------------------------------------------------------------------------
# Basic correctness
# ---------------------------------------------------------------------------


class TestGenerateTimeHistory:
    def test_returns_force_time_history(self, standard_fth):
        assert isinstance(standard_fth, ForceTimeHistory)

    def test_time_starts_at_zero(self, standard_fth):
        assert standard_fth.time[0] == pytest.approx(0.0)

    def test_time_is_monotonically_increasing(self, standard_fth):
        diffs = np.diff(standard_fth.time)
        assert np.all(diffs > 0)

    def test_stagger_is_monotonically_increasing(self, standard_fth):
        diffs = np.diff(standard_fth.stagger)
        assert np.all(diffs > 0)

    def test_stagger_starts_at_minus_two_loa(self, calculator, standard_fth):
        expected_start = -2.0 * calculator.moored_vessel.length
        assert standard_fth.stagger[0] == pytest.approx(expected_start, rel=1e-3)

    def test_stagger_ends_near_plus_two_loa(self, calculator, standard_fth):
        expected_end = 2.0 * calculator.moored_vessel.length
        # Last stagger value should be within one step of the end
        dt = 5.0
        speed = 2.0
        step = speed * dt
        assert standard_fth.stagger[-1] >= expected_end - step
        assert standard_fth.stagger[-1] < expected_end + step

    def test_arrays_have_equal_length(self, standard_fth):
        n = len(standard_fth.time)
        assert len(standard_fth.surge) == n
        assert len(standard_fth.sway) == n
        assert len(standard_fth.yaw) == n
        assert len(standard_fth.stagger) == n

    def test_speed_stored_correctly(self, standard_fth):
        assert standard_fth.speed == pytest.approx(2.0)

    def test_separation_stored_correctly(self, standard_fth):
        assert standard_fth.separation == pytest.approx(100.0)

    def test_at_least_one_time_step(self, standard_fth):
        assert len(standard_fth.time) >= 1


# ---------------------------------------------------------------------------
# Force magnitude properties
# ---------------------------------------------------------------------------


class TestForceMagnitudes:
    def test_sway_forces_are_finite(self, standard_fth):
        assert np.all(np.isfinite(standard_fth.sway))

    def test_surge_forces_are_finite(self, standard_fth):
        assert np.all(np.isfinite(standard_fth.surge))

    def test_yaw_moments_are_finite(self, standard_fth):
        assert np.all(np.isfinite(standard_fth.yaw))

    def test_peak_sway_is_positive(self, standard_fth):
        # Peak absolute sway should be nonzero for a non-trivial scenario
        assert standard_fth.peak_sway > 0.0

    def test_peak_surge_property(self, standard_fth):
        expected = float(np.max(np.abs(standard_fth.surge)))
        assert standard_fth.peak_surge == pytest.approx(expected)

    def test_peak_sway_property(self, standard_fth):
        expected = float(np.max(np.abs(standard_fth.sway)))
        assert standard_fth.peak_sway == pytest.approx(expected)

    def test_peak_yaw_property(self, standard_fth):
        expected = float(np.max(np.abs(standard_fth.yaw)))
        assert standard_fth.peak_yaw == pytest.approx(expected)


# ---------------------------------------------------------------------------
# Speed effect
# ---------------------------------------------------------------------------


class TestSpeedEffect:
    def test_faster_speed_shorter_duration(self, calculator):
        slow = generate_time_history(
            calculator,
            speed_ms=1.0,
            separation_m=100.0,
            dt=5.0,
        )
        fast = generate_time_history(
            calculator,
            speed_ms=4.0,
            separation_m=100.0,
            dt=5.0,
        )
        assert fast.time[-1] < slow.time[-1]

    def test_faster_speed_fewer_steps(self, calculator):
        slow = generate_time_history(
            calculator,
            speed_ms=1.0,
            separation_m=100.0,
            dt=5.0,
        )
        fast = generate_time_history(
            calculator,
            speed_ms=4.0,
            separation_m=100.0,
            dt=5.0,
        )
        assert len(fast.time) < len(slow.time)

    def test_higher_speed_higher_peak_forces(self, calculator):
        """Forces scale with U^2 per Wang formulation.

        Both histories sample the same stagger grid (step = 10 m), so the
        only difference is velocity.  Peak sway at U=3 m/s should be ~9x
        that at U=1 m/s (Wang: F ∝ U²; 3²/1² = 9).
        """
        # Use dt so step = 10 m for both grids to ensure stagger=0 is hit.
        slow = generate_time_history(
            calculator,
            speed_ms=1.0,
            separation_m=100.0,
            dt=10.0,         # step = 1.0 * 10.0 = 10 m
        )
        fast = generate_time_history(
            calculator,
            speed_ms=3.0,
            separation_m=100.0,
            dt=10.0 / 3.0,   # step = 3.0 * (10/3) = 10 m
        )
        # Forces scale with U^2: fast should be ~9x slow
        ratio = fast.peak_sway / slow.peak_sway
        assert ratio > 5.0, f"Expected U² scaling (~9x), got {ratio:.2f}x"


# ---------------------------------------------------------------------------
# Separation effect
# ---------------------------------------------------------------------------


class TestSeparationEffect:
    def test_closer_separation_higher_peak_sway(self, calculator):
        close = generate_time_history(
            calculator,
            speed_ms=2.0,
            separation_m=50.0,
            dt=5.0,
        )
        far = generate_time_history(
            calculator,
            speed_ms=2.0,
            separation_m=200.0,
            dt=5.0,
        )
        assert close.peak_sway > far.peak_sway

    def test_closer_separation_higher_peak_yaw(self, calculator):
        close = generate_time_history(
            calculator,
            speed_ms=2.0,
            separation_m=50.0,
            dt=5.0,
        )
        far = generate_time_history(
            calculator,
            speed_ms=2.0,
            separation_m=200.0,
            dt=5.0,
        )
        assert close.peak_yaw > far.peak_yaw


# ---------------------------------------------------------------------------
# Export: to_dataframe
# ---------------------------------------------------------------------------


class TestToDataframe:
    def test_to_dataframe_returns_dataframe(self, standard_fth):
        df = standard_fth.to_dataframe()
        import pandas as pd
        assert isinstance(df, pd.DataFrame)

    def test_to_dataframe_has_correct_columns(self, standard_fth):
        df = standard_fth.to_dataframe()
        expected = {"time", "surge", "sway", "yaw", "stagger"}
        assert expected == set(df.columns)

    def test_to_dataframe_row_count_matches_time_array(self, standard_fth):
        df = standard_fth.to_dataframe()
        assert len(df) == len(standard_fth.time)

    def test_to_dataframe_time_column_matches(self, standard_fth):
        df = standard_fth.to_dataframe()
        np.testing.assert_array_equal(df["time"].values, standard_fth.time)

    def test_to_dataframe_sway_column_matches(self, standard_fth):
        df = standard_fth.to_dataframe()
        np.testing.assert_array_equal(df["sway"].values, standard_fth.sway)


# ---------------------------------------------------------------------------
# Export: to_csv
# ---------------------------------------------------------------------------


class TestToCsv:
    def test_to_csv_writes_file(self, standard_fth):
        with tempfile.TemporaryDirectory() as tmp:
            csv_path = Path(tmp) / "forces.csv"
            standard_fth.to_csv(csv_path)
            assert csv_path.exists()

    def test_to_csv_returns_path(self, standard_fth):
        with tempfile.TemporaryDirectory() as tmp:
            csv_path = Path(tmp) / "forces.csv"
            result = standard_fth.to_csv(csv_path)
            assert isinstance(result, Path)
            assert result == csv_path

    def test_to_csv_readable_back_with_pandas(self, standard_fth):
        import pandas as pd

        with tempfile.TemporaryDirectory() as tmp:
            csv_path = Path(tmp) / "forces.csv"
            standard_fth.to_csv(csv_path)
            df = pd.read_csv(csv_path)
        expected_cols = {"time", "surge", "sway", "yaw", "stagger"}
        assert expected_cols == set(df.columns)

    def test_to_csv_data_matches_original(self, standard_fth):
        import pandas as pd

        with tempfile.TemporaryDirectory() as tmp:
            csv_path = Path(tmp) / "forces.csv"
            standard_fth.to_csv(csv_path)
            df = pd.read_csv(csv_path)
        np.testing.assert_allclose(
            df["time"].values,
            standard_fth.time,
            rtol=1e-6,
        )
        np.testing.assert_allclose(
            df["sway"].values,
            standard_fth.sway,
            rtol=1e-6,
        )


# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------


class TestInputValidation:
    def test_zero_speed_raises_value_error(self, calculator):
        with pytest.raises(ValueError, match="speed_ms"):
            generate_time_history(calculator, speed_ms=0.0, separation_m=100.0)

    def test_negative_speed_raises_value_error(self, calculator):
        with pytest.raises(ValueError, match="speed_ms"):
            generate_time_history(
                calculator, speed_ms=-1.0, separation_m=100.0
            )

    def test_zero_separation_raises_value_error(self, calculator):
        with pytest.raises(ValueError, match="separation_m"):
            generate_time_history(calculator, speed_ms=2.0, separation_m=0.0)

    def test_negative_separation_raises_value_error(self, calculator):
        with pytest.raises(ValueError, match="separation_m"):
            generate_time_history(
                calculator, speed_ms=2.0, separation_m=-50.0
            )

    def test_zero_dt_raises_value_error(self, calculator):
        with pytest.raises(ValueError, match="dt"):
            generate_time_history(
                calculator, speed_ms=2.0, separation_m=100.0, dt=0.0
            )


# ---------------------------------------------------------------------------
# Custom x-range parameters
# ---------------------------------------------------------------------------


class TestCustomRange:
    def test_custom_x_start_factor(self, calculator):
        fth = generate_time_history(
            calculator,
            speed_ms=2.0,
            separation_m=100.0,
            dt=5.0,
            x_start_factor=-1.0,
        )
        expected_start = -1.0 * calculator.moored_vessel.length
        assert fth.stagger[0] == pytest.approx(expected_start, rel=1e-3)

    def test_custom_x_end_factor(self, calculator):
        fth = generate_time_history(
            calculator,
            speed_ms=2.0,
            separation_m=100.0,
            dt=5.0,
            x_start_factor=-1.0,
            x_end_factor=1.0,
        )
        expected_end = 1.0 * calculator.moored_vessel.length
        step = 2.0 * 5.0
        assert fth.stagger[-1] >= expected_end - step
        assert fth.stagger[-1] < expected_end + step

    def test_smaller_range_fewer_steps(self, calculator):
        wide = generate_time_history(
            calculator,
            speed_ms=2.0,
            separation_m=100.0,
            dt=5.0,
            x_start_factor=-2.0,
            x_end_factor=2.0,
        )
        narrow = generate_time_history(
            calculator,
            speed_ms=2.0,
            separation_m=100.0,
            dt=5.0,
            x_start_factor=-1.0,
            x_end_factor=1.0,
        )
        assert len(narrow.time) < len(wide.time)
