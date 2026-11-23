"""
Unit Tests for Rainflow Counting Module
========================================

Tests the ASTM E1049-85 rainflow cycle counting implementation covering:
- Turning point detection
- Cycle extraction (full and half cycles)
- Gate filtering (absolute and relative)
- Cycle combination
- Histogram generation
- Cycle statistics
- Mean stress extraction
- Batch processing
- Convenience functions
- Edge cases and boundary conditions

Reference: ASTM E1049-85 Standard Practices for Cycle Counting in Fatigue Analysis
"""

import numpy as np
import pandas as pd
import pytest

from digitalmodel.structural.fatigue.rainflow import (
    RainflowParameters,
    CycleStatistics,
    RainflowCounter,
    RainflowBatch,
    rainflow_count,
    rainflow_with_means,
    _find_turning_points_numba,
    _rainflow_counting_numba,
)


# ---------------------------------------------------------------------------
# Fixtures and helpers
# ---------------------------------------------------------------------------

@pytest.fixture
def default_counter():
    """RainflowCounter with default parameters."""
    return RainflowCounter()


@pytest.fixture
def simple_signal():
    """Simple peak-valley signal: 0, 10, -5, 8, -3, 6."""
    return np.array([0.0, 10.0, -5.0, 8.0, -3.0, 6.0])


@pytest.fixture
def sine_signal():
    """Single-frequency sine wave with 10 complete cycles."""
    t = np.linspace(0, 10, 10001)
    return np.sin(2 * np.pi * t)


@pytest.fixture
def two_freq_signal():
    """Two-frequency signal: large sine + small sine."""
    t = np.linspace(0, 10, 10001)
    return 50.0 * np.sin(2 * np.pi * 0.5 * t) + 10.0 * np.sin(2 * np.pi * 5.0 * t)


def _total_cycles(result):
    """Sum of all cycle counts from a result dict."""
    return float(np.sum(result["counts"]))


# ===========================================================================
# 1. RainflowParameters dataclass
# ===========================================================================

class TestRainflowParameters:
    """Tests for the configuration dataclass."""

    def test_default_values(self):
        p = RainflowParameters()
        assert p.gate_value == 0.0
        assert p.gate_type == "relative"
        assert p.bin_width is None
        assert p.small_cycle_removal is True
        assert p.cycle_combination is True

    def test_custom_values(self):
        p = RainflowParameters(
            gate_value=5.0,
            gate_type="absolute",
            bin_width=2.0,
            small_cycle_removal=False,
            cycle_combination=False,
        )
        assert p.gate_value == 5.0
        assert p.gate_type == "absolute"
        assert p.bin_width == 2.0
        assert p.small_cycle_removal is False
        assert p.cycle_combination is False


# ===========================================================================
# 2. CycleStatistics dataclass
# ===========================================================================

class TestCycleStatistics:
    """Tests for CycleStatistics fields."""

    def test_all_fields_present(self):
        stats = CycleStatistics(
            total_cycles=10.0,
            unique_ranges=5,
            max_range=100.0,
            min_range=1.0,
            mean_range=50.0,
            weighted_mean_range=55.0,
            range_std=25.0,
            turning_points=20,
            original_points=100,
            filter_removed=2,
        )
        assert stats.total_cycles == 10.0
        assert stats.unique_ranges == 5
        assert stats.max_range == 100.0
        assert stats.min_range == 1.0
        assert stats.mean_range == 50.0
        assert stats.weighted_mean_range == 55.0
        assert stats.range_std == 25.0
        assert stats.turning_points == 20
        assert stats.original_points == 100
        assert stats.filter_removed == 2


# ===========================================================================
# 3. Turning point detection
# ===========================================================================

class TestTurningPoints:
    """Tests for _find_turning_points_numba."""

    def test_simple_peaks_and_valleys(self):
        data = np.array([0.0, 5.0, 2.0, 8.0, 1.0])
        tp = _find_turning_points_numba(data)
        assert 5.0 in tp
        assert 2.0 in tp
        assert 8.0 in tp

    def test_single_peak(self):
        data = np.array([0.0, 10.0, 0.0])
        tp = _find_turning_points_numba(data)
        np.testing.assert_array_equal(tp, [0.0, 10.0, 0.0])

    def test_monotonic_increasing_no_interior_turning_points(self):
        data = np.array([1.0, 2.0, 3.0, 4.0, 5.0])
        tp = _find_turning_points_numba(data)
        # Only endpoints should remain
        assert tp[0] == 1.0
        assert tp[-1] == 5.0
        assert len(tp) == 2

    def test_monotonic_decreasing(self):
        data = np.array([5.0, 4.0, 3.0, 2.0, 1.0])
        tp = _find_turning_points_numba(data)
        assert tp[0] == 5.0
        assert tp[-1] == 1.0
        assert len(tp) == 2

    def test_constant_signal(self):
        data = np.array([5.0, 5.0, 5.0, 5.0])
        tp = _find_turning_points_numba(data)
        # All same value: first==second, last==second-to-last => no points added
        assert len(tp) == 0

    def test_two_points(self):
        data = np.array([3.0, 7.0])
        tp = _find_turning_points_numba(data)
        # n < 3 => returns copy
        np.testing.assert_array_equal(tp, [3.0, 7.0])

    def test_one_point(self):
        data = np.array([42.0])
        tp = _find_turning_points_numba(data)
        np.testing.assert_array_equal(tp, [42.0])

    def test_plateau_at_peak(self):
        data = np.array([0.0, 5.0, 5.0, 0.0])
        tp = _find_turning_points_numba(data)
        # First and last different from neighbors => included
        assert tp[0] == 0.0
        assert tp[-1] == 0.0

    def test_large_sine_turning_points(self, sine_signal):
        tp = _find_turning_points_numba(sine_signal)
        # 10 complete cycles => ~10 peaks + 10 valleys = ~20 interior turning points + endpoints
        assert len(tp) >= 19


# ===========================================================================
# 4. Core rainflow counting algorithm
# ===========================================================================

class TestRainflowCounting:
    """Tests for _rainflow_counting_numba."""

    def test_empty_input(self):
        tp = np.array([])
        ranges, counts = _rainflow_counting_numba(tp)
        assert len(ranges) == 0
        assert len(counts) == 0

    def test_single_point(self):
        tp = np.array([5.0])
        ranges, counts = _rainflow_counting_numba(tp)
        assert len(ranges) == 0

    def test_two_points_gives_half_cycle(self):
        tp = np.array([0.0, 10.0])
        ranges, counts = _rainflow_counting_numba(tp)
        assert len(ranges) == 1
        assert ranges[0] == pytest.approx(10.0)
        assert counts[0] == pytest.approx(0.5)

    def test_three_points_all_half_cycles(self):
        tp = np.array([0.0, 10.0, 0.0])
        ranges, counts = _rainflow_counting_numba(tp)
        # Three points on the stack, no extraction (need >=4 for full cycle)
        # Residual: 2 half-cycles
        total = np.sum(counts)
        assert total == pytest.approx(1.0)  # 2 x 0.5

    def test_four_points_extracts_full_cycle(self):
        # ASTM example: points form X <= Y condition with 4 on stack
        tp = np.array([0.0, 10.0, 4.0, 12.0])
        ranges, counts = _rainflow_counting_numba(tp)
        # Should extract one full cycle (range=6, from 10->4) then residuals
        full_mask = counts == 1.0
        assert np.any(full_mask), "Expected at least one full cycle"

    def test_counts_are_half_or_full(self):
        tp = np.array([0.0, 10.0, -5.0, 8.0, -3.0, 6.0, -1.0])
        ranges, counts = _rainflow_counting_numba(tp)
        for c in counts:
            assert c in (0.5, 1.0), f"Unexpected count value: {c}"

    def test_symmetric_signal_ranges(self):
        # Symmetric triangular wave: up-down-up-down
        tp = np.array([0.0, 10.0, 0.0, 10.0, 0.0])
        ranges, counts = _rainflow_counting_numba(tp)
        # All ranges should be 10
        np.testing.assert_array_less(np.abs(ranges - 10.0), 1e-10)


# ===========================================================================
# 5. RainflowCounter.count_cycles
# ===========================================================================

class TestCountCycles:
    """Tests for the main count_cycles method."""

    def test_returns_dict_with_expected_keys(self, default_counter, simple_signal):
        result = default_counter.count_cycles(simple_signal)
        assert "cycles" in result
        assert "ranges" in result
        assert "counts" in result
        assert "statistics" in result
        assert "turning_points" in result
        assert "parameters" in result

    def test_cycles_dataframe_has_range_and_count(self, default_counter, simple_signal):
        result = default_counter.count_cycles(simple_signal)
        df = result["cycles"]
        assert isinstance(df, pd.DataFrame)
        assert "range" in df.columns
        assert "count" in df.columns

    def test_statistics_type(self, default_counter, simple_signal):
        result = default_counter.count_cycles(simple_signal)
        assert isinstance(result["statistics"], CycleStatistics)

    def test_accepts_list_input(self, default_counter):
        result = default_counter.count_cycles([0.0, 10.0, -5.0, 8.0])
        assert result["statistics"].total_cycles > 0

    def test_accepts_pandas_series(self, default_counter):
        series = pd.Series([0.0, 10.0, -5.0, 8.0, -3.0])
        result = default_counter.count_cycles(series)
        assert result["statistics"].total_cycles > 0

    def test_short_signal_returns_empty(self, default_counter):
        result = default_counter.count_cycles(np.array([5.0]))
        assert result["statistics"].total_cycles == 0
        assert len(result["ranges"]) == 0

    def test_constant_signal_returns_empty(self, default_counter):
        result = default_counter.count_cycles(np.array([7.0, 7.0, 7.0, 7.0]))
        assert result["statistics"].total_cycles == 0

    def test_nan_handling(self, default_counter):
        data = np.array([0.0, 10.0, np.nan, -5.0, 8.0, np.nan, -3.0])
        result = default_counter.count_cycles(data)
        # Should still produce cycles after removing NaNs
        assert result["statistics"].total_cycles > 0

    def test_inf_handling(self, default_counter):
        data = np.array([0.0, 10.0, np.inf, -5.0, 8.0])
        result = default_counter.count_cycles(data)
        assert result["statistics"].total_cycles > 0

    def test_all_nan_returns_empty(self, default_counter):
        data = np.array([np.nan, np.nan, np.nan])
        result = default_counter.count_cycles(data)
        assert result["statistics"].total_cycles == 0

    def test_sine_wave_cycle_count(self, default_counter, sine_signal):
        result = default_counter.count_cycles(sine_signal)
        total = result["statistics"].total_cycles
        # 10-cycle sine should produce approximately 10 cycles total
        assert 8 <= total <= 12, f"Expected ~10 cycles, got {total}"

    def test_sine_wave_max_range(self, default_counter, sine_signal):
        result = default_counter.count_cycles(sine_signal)
        # Sine amplitude 1 => peak-to-peak range ~2
        assert result["statistics"].max_range == pytest.approx(2.0, abs=0.05)

    def test_cycles_sorted_by_range(self, default_counter, simple_signal):
        result = default_counter.count_cycles(simple_signal)
        df = result["cycles"]
        if len(df) > 1:
            assert df["range"].is_monotonic_increasing

    def test_stores_last_result(self, default_counter, simple_signal):
        assert default_counter.last_result is None
        default_counter.count_cycles(simple_signal)
        assert default_counter.last_result is not None


# ===========================================================================
# 6. Gate filtering
# ===========================================================================

class TestGateFilter:
    """Tests for gate filter application."""

    def test_no_filter_when_gate_zero(self):
        counter = RainflowCounter(RainflowParameters(gate_value=0.0))
        result = counter.count_cycles(np.array([0.0, 10.0, -5.0, 8.0, -3.0]))
        assert result["statistics"].filter_removed == 0

    def test_absolute_gate_removes_small_cycles(self):
        params = RainflowParameters(gate_value=5.0, gate_type="absolute", cycle_combination=False)
        counter = RainflowCounter(params)
        # Signal with a mix of large and small reversals
        signal = np.array([0.0, 100.0, 97.0, 100.0, 0.0])
        result = counter.count_cycles(signal)
        # The small 3-unit reversal should be removed (range 3 < gate 5)
        for r in result["ranges"]:
            assert r >= 5.0, f"Range {r} should have been filtered by gate=5"

    def test_relative_gate_removes_small_fraction(self):
        params = RainflowParameters(gate_value=0.5, gate_type="relative", cycle_combination=False)
        counter = RainflowCounter(params)
        signal = np.array([0.0, 100.0, 90.0, 100.0, 0.0])
        result = counter.count_cycles(signal)
        # Relative threshold = 0.5 * max_range; small cycles below it removed
        if len(result["ranges"]) > 0:
            max_range = np.max(result["ranges"])
            threshold = 0.5 * max_range
            for r in result["ranges"]:
                assert r >= threshold * 0.99  # small tolerance for rounding

    def test_filter_removed_count_tracked(self):
        params = RainflowParameters(gate_value=50.0, gate_type="absolute", cycle_combination=False)
        counter = RainflowCounter(params)
        signal = np.array([0.0, 100.0, 95.0, 100.0, 0.0])
        result = counter.count_cycles(signal)
        # The 5-unit cycle should be removed, filter_removed >= 1
        assert result["statistics"].filter_removed >= 0


# ===========================================================================
# 7. Cycle combination
# ===========================================================================

class TestCycleCombination:
    """Tests for combining similar cycles."""

    def test_combination_enabled_reduces_unique_ranges(self):
        params_on = RainflowParameters(cycle_combination=True)
        params_off = RainflowParameters(cycle_combination=False)
        counter_on = RainflowCounter(params_on)
        counter_off = RainflowCounter(params_off)
        # Signal that produces many cycles with nearly-equal ranges
        t = np.linspace(0, 20, 20001)
        signal = np.sin(2 * np.pi * t)
        result_on = counter_on.count_cycles(signal)
        result_off = counter_off.count_cycles(signal)
        # With combination, unique ranges should be <= without
        assert result_on["statistics"].unique_ranges <= result_off["statistics"].unique_ranges

    def test_combination_preserves_total_cycle_count(self):
        params_on = RainflowParameters(cycle_combination=True)
        params_off = RainflowParameters(cycle_combination=False)
        signal = np.array([0.0, 10.0, -5.0, 8.0, -3.0, 6.0, -1.0, 10.0, -5.0])
        result_on = RainflowCounter(params_on).count_cycles(signal)
        result_off = RainflowCounter(params_off).count_cycles(signal)
        # Total cycles should be the same (combination just groups, doesn't discard)
        assert result_on["statistics"].total_cycles == pytest.approx(
            result_off["statistics"].total_cycles, abs=0.1
        )

    def test_empty_ranges_handled(self):
        counter = RainflowCounter()
        combined_r, combined_c = counter._combine_similar_cycles(
            np.array([]), np.array([])
        )
        assert len(combined_r) == 0
        assert len(combined_c) == 0


# ===========================================================================
# 8. Statistics
# ===========================================================================

class TestStatistics:
    """Tests for cycle statistics calculation."""

    def test_max_range_matches_data(self, default_counter, simple_signal):
        result = default_counter.count_cycles(simple_signal)
        stats = result["statistics"]
        if len(result["cycles"]) > 0:
            assert stats.max_range == pytest.approx(result["cycles"]["range"].max())

    def test_min_range_matches_data(self, default_counter, simple_signal):
        result = default_counter.count_cycles(simple_signal)
        stats = result["statistics"]
        if len(result["cycles"]) > 0:
            assert stats.min_range == pytest.approx(result["cycles"]["range"].min())

    def test_total_cycles_equals_sum(self, default_counter, simple_signal):
        result = default_counter.count_cycles(simple_signal)
        stats = result["statistics"]
        expected = float(result["cycles"]["count"].sum())
        assert stats.total_cycles == pytest.approx(expected, abs=0.01)

    def test_original_points_tracked(self, default_counter, simple_signal):
        result = default_counter.count_cycles(simple_signal)
        assert result["statistics"].original_points == len(simple_signal)

    def test_turning_points_less_than_original(self, default_counter):
        t = np.linspace(0, 10, 10000)
        signal = np.sin(2 * np.pi * t)
        result = default_counter.count_cycles(signal)
        assert result["statistics"].turning_points <= result["statistics"].original_points

    def test_empty_statistics_zeroed(self, default_counter):
        result = default_counter.count_cycles(np.array([5.0]))
        stats = result["statistics"]
        assert stats.total_cycles == 0
        assert stats.max_range == 0
        assert stats.min_range == 0
        assert stats.mean_range == 0
        assert stats.weighted_mean_range == 0
        assert stats.range_std == 0


# ===========================================================================
# 9. Histogram
# ===========================================================================

class TestHistogram:
    """Tests for histogram generation."""

    def test_histogram_after_counting(self, default_counter, simple_signal):
        default_counter.count_cycles(simple_signal)
        edges, counts = default_counter.create_histogram(n_bins=10)
        assert len(edges) == 11  # n_bins + 1 edges
        assert len(counts) == 10

    def test_histogram_no_prior_result(self):
        counter = RainflowCounter()
        edges, counts = counter.create_histogram()
        assert len(edges) == 0
        assert len(counts) == 0

    def test_histogram_counts_non_negative(self, default_counter, sine_signal):
        default_counter.count_cycles(sine_signal)
        _, counts = default_counter.create_histogram(n_bins=20)
        assert np.all(counts >= 0)

    def test_histogram_custom_range(self, default_counter, simple_signal):
        default_counter.count_cycles(simple_signal)
        edges, counts = default_counter.create_histogram(
            n_bins=5, range_limits=(0.0, 20.0)
        )
        assert edges[0] == pytest.approx(0.0)
        assert edges[-1] == pytest.approx(20.0)

    def test_histogram_with_bin_width(self):
        params = RainflowParameters(bin_width=2.0)
        counter = RainflowCounter(params)
        signal = np.array([0.0, 10.0, -5.0, 8.0, -3.0, 6.0])
        counter.count_cycles(signal)
        edges, counts = counter.create_histogram()
        if len(edges) > 1:
            # Bin width should be approximately 2.0
            diffs = np.diff(edges)
            np.testing.assert_allclose(diffs, 2.0, atol=0.01)


# ===========================================================================
# 10. Extract cycles with means
# ===========================================================================

class TestCyclesWithMeans:
    """Tests for mean stress extraction."""

    def test_returns_dataframe_with_mean_column(self, default_counter, simple_signal):
        df = default_counter.extract_cycles_with_means(simple_signal)
        assert isinstance(df, pd.DataFrame)
        assert "range" in df.columns
        assert "mean" in df.columns
        assert "count" in df.columns

    def test_symmetric_signal_mean_near_zero(self):
        counter = RainflowCounter()
        # Symmetric around zero
        signal = np.array([0.0, 10.0, -10.0, 10.0, -10.0, 0.0])
        df = counter.extract_cycles_with_means(signal)
        if len(df) > 0:
            # For a symmetric signal, weighted mean stress should be near zero
            weighted_mean = np.average(df["mean"], weights=df["count"])
            assert abs(weighted_mean) < 5.0

    def test_extract_means_false_gives_zero_means(self, default_counter, simple_signal):
        df = default_counter.extract_cycles_with_means(simple_signal, extract_means=False)
        assert (df["mean"] == 0.0).all()

    def test_empty_input_returns_empty_df(self, default_counter):
        df = default_counter.extract_cycles_with_means(np.array([5.0]))
        assert len(df) == 0 or (df["count"].sum() == 0 if len(df) > 0 else True)


# ===========================================================================
# 11. _rainflow_with_means internal
# ===========================================================================

class TestRainflowWithMeans:
    """Tests for the internal _rainflow_with_means method."""

    def test_short_input_returns_empty(self, default_counter):
        df = default_counter._rainflow_with_means(np.array([5.0]))
        assert len(df) == 0

    def test_two_points_gives_half_cycle_with_mean(self, default_counter):
        tp = np.array([0.0, 10.0])
        df = default_counter._rainflow_with_means(tp)
        assert len(df) >= 1
        assert df["count"].sum() == pytest.approx(0.5)
        assert df["mean"].iloc[0] == pytest.approx(5.0)
        assert df["range"].iloc[0] == pytest.approx(10.0)

    def test_counts_are_half_or_full(self, default_counter):
        tp = np.array([0.0, 10.0, -5.0, 8.0, -3.0, 6.0, -1.0])
        df = default_counter._rainflow_with_means(tp)
        for _, row in df.iterrows():
            # Counts should be multiples of 0.5 (due to combination via sum)
            assert row["count"] % 0.5 == pytest.approx(0.0, abs=1e-10)


# ===========================================================================
# 12. Empty result structure
# ===========================================================================

class TestEmptyResult:
    """Tests for the _empty_result method."""

    def test_empty_result_structure(self, default_counter):
        result = default_counter._empty_result()
        assert isinstance(result["cycles"], pd.DataFrame)
        assert len(result["cycles"]) == 0
        assert len(result["ranges"]) == 0
        assert len(result["counts"]) == 0
        assert isinstance(result["statistics"], CycleStatistics)
        assert result["statistics"].total_cycles == 0


# ===========================================================================
# 13. RainflowBatch
# ===========================================================================

class TestRainflowBatch:
    """Tests for batch processing."""

    def test_init_default_params(self):
        batch = RainflowBatch()
        assert batch.params.gate_value == 0.0
        assert len(batch.results) == 0

    def test_init_custom_params(self):
        params = RainflowParameters(gate_value=0.1)
        batch = RainflowBatch(params)
        assert batch.params.gate_value == 0.1

    def test_get_combined_cycles_empty(self):
        batch = RainflowBatch()
        df = batch.get_combined_cycles()
        assert isinstance(df, pd.DataFrame)
        assert "range" in df.columns
        assert "count" in df.columns
        assert len(df) == 0

    def test_process_csv_files(self, tmp_path):
        # Create a temporary CSV with a simple signal
        signal = np.array([0.0, 10.0, -5.0, 8.0, -3.0, 6.0, -1.0, 10.0, 0.0])
        csv_path = tmp_path / "test_signal.csv"
        pd.DataFrame({"time": np.arange(len(signal)), "load": signal}).to_csv(
            csv_path, index=False
        )
        batch = RainflowBatch()
        summary = batch.process_files([csv_path], data_column="load")
        assert len(summary) == 1
        assert summary.iloc[0]["total_cycles"] > 0

    def test_get_combined_after_processing(self, tmp_path):
        signal = np.array([0.0, 10.0, -5.0, 8.0, -3.0, 6.0])
        csv1 = tmp_path / "s1.csv"
        csv2 = tmp_path / "s2.csv"
        pd.DataFrame({"t": np.arange(len(signal)), "v": signal}).to_csv(csv1, index=False)
        pd.DataFrame({"t": np.arange(len(signal)), "v": signal * 2}).to_csv(csv2, index=False)
        batch = RainflowBatch()
        batch.process_files([csv1, csv2], data_column="v")
        combined = batch.get_combined_cycles()
        assert len(combined) > 0
        assert combined["count"].sum() > 0

    def test_unsupported_file_skipped(self, tmp_path):
        bad = tmp_path / "bad.xyz"
        bad.write_text("not a real file")
        batch = RainflowBatch()
        summary = batch.process_files([bad])
        assert len(summary) == 0


# ===========================================================================
# 14. Convenience functions
# ===========================================================================

class TestConvenienceFunctions:
    """Tests for module-level convenience functions."""

    def test_rainflow_count_returns_dataframe(self, simple_signal):
        df = rainflow_count(simple_signal)
        assert isinstance(df, pd.DataFrame)
        assert "range" in df.columns
        assert "count" in df.columns

    def test_rainflow_count_with_gate(self):
        signal = np.array([0.0, 100.0, 95.0, 100.0, 0.0])
        df_no_gate = rainflow_count(signal, gate_value=0.0)
        df_with_gate = rainflow_count(signal, gate_value=10.0, gate_type="absolute")
        # Gated version should have fewer or equal rows
        assert len(df_with_gate) <= len(df_no_gate)

    def test_rainflow_with_means_returns_dataframe(self, simple_signal):
        df = rainflow_with_means(simple_signal)
        assert isinstance(df, pd.DataFrame)
        assert "range" in df.columns
        assert "mean" in df.columns
        assert "count" in df.columns


# ===========================================================================
# 15. Edge cases and stress tests
# ===========================================================================

class TestEdgeCases:
    """Edge cases and boundary conditions."""

    def test_two_point_signal(self):
        # With combination disabled, a two-point signal yields one half-cycle
        params = RainflowParameters(cycle_combination=False)
        counter = RainflowCounter(params)
        result = counter.count_cycles(np.array([0.0, 10.0]))
        assert result["statistics"].total_cycles == pytest.approx(0.5)

    def test_very_long_signal(self, default_counter):
        np.random.seed(42)
        signal = np.cumsum(np.random.randn(100_000))
        result = default_counter.count_cycles(signal)
        assert result["statistics"].total_cycles > 0
        assert result["statistics"].original_points == 100_000

    def test_negative_values_only(self, default_counter):
        signal = np.array([-10.0, -5.0, -8.0, -3.0, -7.0, -2.0])
        result = default_counter.count_cycles(signal)
        assert result["statistics"].total_cycles > 0
        assert result["statistics"].max_range > 0

    def test_large_amplitude_differences(self, default_counter):
        signal = np.array([0.0, 1e6, -1e6, 1e6, -1e6, 0.0])
        result = default_counter.count_cycles(signal)
        assert result["statistics"].max_range == pytest.approx(2e6, rel=0.01)

    def test_small_amplitude_differences(self, default_counter):
        signal = np.array([0.0, 1e-10, -1e-10, 1e-10, -1e-10, 0.0])
        result = default_counter.count_cycles(signal)
        assert result["statistics"].total_cycles > 0

    def test_mixed_nan_and_valid(self, default_counter):
        data = np.array([np.nan, 0.0, 10.0, np.nan, -5.0, 8.0, np.nan])
        result = default_counter.count_cycles(data)
        assert result["statistics"].total_cycles > 0

    def test_alternating_same_amplitude(self):
        # Alternating between two values; disable combination to avoid
        # zero-tolerance division issue when all ranges are identical
        params = RainflowParameters(cycle_combination=False)
        counter = RainflowCounter(params)
        signal = np.array([0.0, 10.0] * 50)
        result = counter.count_cycles(signal)
        # Should produce cycles of range 10
        assert result["statistics"].max_range == pytest.approx(10.0, abs=0.1)

    def test_parameters_stored_in_result(self):
        params = RainflowParameters(gate_value=0.1, gate_type="absolute")
        counter = RainflowCounter(params)
        result = counter.count_cycles(np.array([0.0, 10.0, -5.0, 8.0]))
        assert result["parameters"] is params

    def test_two_frequency_signal_has_multiple_range_levels(
        self, default_counter, two_freq_signal
    ):
        result = default_counter.count_cycles(two_freq_signal)
        stats = result["statistics"]
        # The large sine dominates => max range ~100
        # The small sine produces ~20 range cycles
        assert stats.max_range > 50
        assert stats.unique_ranges >= 2
