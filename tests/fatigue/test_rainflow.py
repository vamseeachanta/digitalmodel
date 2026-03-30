"""Tests for rainflow cycle counting pipeline."""

import numpy as np
import pandas as pd
import pytest

from digitalmodel.fatigue.rainflow import (
    fatigue_life,
    rainflow_count,
    stress_histogram,
)


# ── Fixtures ──────────────────────────────────────────────────────────

@pytest.fixture
def simple_signal():
    """Signal with known turning points for deterministic cycle checking."""
    return np.array([0.0, 10.0, -5.0, 15.0, -10.0, 5.0, -15.0, 10.0, 0.0])


@pytest.fixture
def sinusoidal_signal():
    """5-period sine wave with amplitude 100 MPa."""
    t = np.linspace(0, 2 * np.pi * 5, 1000)
    return 100.0 * np.sin(t)


@pytest.fixture
def random_signal(seed=42):
    """Random broad-band stress signal."""
    rng = np.random.default_rng(seed)
    return rng.normal(loc=50.0, scale=30.0, size=2000)


# ── rainflow_count ────────────────────────────────────────────────────

class TestRainflowCount:
    def test_returns_correct_columns(self, simple_signal):
        result = rainflow_count(simple_signal)
        assert set(result.columns) == {"stress_range", "mean_stress", "cycles"}

    def test_cycles_are_one_or_half(self, simple_signal):
        result = rainflow_count(simple_signal)
        assert all(c in (0.5, 1.0) for c in result["cycles"])

    def test_stress_ranges_positive(self, simple_signal):
        result = rainflow_count(simple_signal)
        assert (result["stress_range"] > 0).all()

    def test_sinusoidal_cycle_count(self, sinusoidal_signal):
        """A 5-period sine should produce ~5 full-cycle equivalents."""
        result = rainflow_count(sinusoidal_signal)
        total_cycles = result["cycles"].sum()
        # 4 full cycles + residual half-cycles that add up to ~1 more
        assert 4.0 <= total_cycles <= 6.0

    def test_sinusoidal_stress_range(self, sinusoidal_signal):
        """Dominant stress range should be ~200 MPa (2 * amplitude)."""
        result = rainflow_count(sinusoidal_signal)
        # Full cycles should have range close to 200
        full = result[result["cycles"] == 1.0]
        if not full.empty:
            assert all(full["stress_range"] > 195.0)

    def test_rejects_nan(self):
        with pytest.raises(ValueError, match="NaN"):
            rainflow_count(np.array([0.0, np.nan, 1.0]))

    def test_constant_signal_returns_empty(self):
        result = rainflow_count(np.array([5.0, 5.0, 5.0, 5.0]))
        assert result.empty or result["stress_range"].sum() == 0

    def test_accepts_pandas_series(self):
        signal = pd.Series([0.0, 100.0, -50.0, 100.0, 0.0])
        result = rainflow_count(signal)
        assert isinstance(result, pd.DataFrame)
        assert len(result) > 0


# ── stress_histogram ──────────────────────────────────────────────────

class TestStressHistogram:
    def test_output_columns(self, simple_signal):
        cycles = rainflow_count(simple_signal)
        hist = stress_histogram(cycles)
        assert set(hist.columns) == {"stress_range", "cycles"}

    def test_total_cycles_preserved(self, sinusoidal_signal):
        """Binning must not lose or create cycles."""
        cycles = rainflow_count(sinusoidal_signal)
        total_raw = cycles["cycles"].sum()
        hist = stress_histogram(cycles, n_bins=10)
        total_binned = hist["cycles"].sum()
        assert abs(total_binned - total_raw) < 1e-9

    def test_custom_bin_edges(self, simple_signal):
        cycles = rainflow_count(simple_signal)
        edges = np.array([0.0, 10.0, 20.0, 30.0])
        hist = stress_histogram(cycles, bin_edges=edges)
        assert len(hist) <= 3  # at most 3 bins

    def test_empty_input(self):
        empty = pd.DataFrame(columns=["stress_range", "mean_stress", "cycles"])
        hist = stress_histogram(empty)
        assert hist.empty

    def test_no_empty_bins(self, simple_signal):
        cycles = rainflow_count(simple_signal)
        hist = stress_histogram(cycles, n_bins=50)
        assert (hist["cycles"] > 0).all()


# ── fatigue_life ──────────────────────────────────────────────────────

class TestFatigueLife:
    def test_returns_expected_keys(self, sinusoidal_signal):
        result = fatigue_life(sinusoidal_signal, "D")
        expected_keys = {
            "damage", "life_factor", "years_to_failure",
            "pass_fail", "histogram", "cycle_count",
        }
        assert set(result.keys()) == expected_keys

    def test_damage_positive(self, sinusoidal_signal):
        result = fatigue_life(sinusoidal_signal, "D")
        assert result["damage"] > 0

    def test_pass_fail_is_string(self, sinusoidal_signal):
        result = fatigue_life(sinusoidal_signal, "D")
        assert result["pass_fail"] in ("PASS", "FAIL")

    def test_histogram_compatible_with_miner(self, sinusoidal_signal):
        """Histogram output must have the columns miner_damage() expects."""
        result = fatigue_life(sinusoidal_signal, "D")
        hist = result["histogram"]
        assert "stress_range" in hist.columns
        assert "cycles" in hist.columns

    def test_different_environments(self, sinusoidal_signal):
        air = fatigue_life(sinusoidal_signal, "D", environment="air")
        corr = fatigue_life(sinusoidal_signal, "D", environment="free_corrosion")
        # Free corrosion should give more damage than air
        assert corr["damage"] > air["damage"]

    def test_random_signal_smoke(self, random_signal):
        """Random signal should run without error and produce positive damage."""
        result = fatigue_life(random_signal, "F")
        assert result["damage"] > 0
        assert isinstance(result["histogram"], pd.DataFrame)
        assert isinstance(result["cycle_count"], pd.DataFrame)

    def test_life_factor_consistency(self, sinusoidal_signal):
        result = fatigue_life(sinusoidal_signal, "D", target_years=25.0)
        if result["damage"] > 0:
            expected_ytf = 1.0 / result["damage"]
            assert abs(result["years_to_failure"] - expected_ytf) < 1e-6
            assert abs(result["life_factor"] - expected_ytf / 25.0) < 1e-6
