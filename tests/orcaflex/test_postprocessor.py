"""Tests for digitalmodel.orcaflex.postprocessor module."""

import math

import numpy as np
import pytest

from digitalmodel.orcaflex.postprocessor import (
    ExtremeValueResult,
    MultiSeedSummary,
    RangeGraphData,
    TimeSeriesStats,
    compute_time_series_stats,
    fit_gumbel,
    fit_weibull,
    generate_range_graph,
    process_multi_seed,
)


class TestTimeSeriesStats:
    """Tests for time series statistics computation."""

    def test_basic_statistics(self):
        """Should compute correct mean, std, min, max."""
        data = np.array([1.0, 2.0, 3.0, 4.0, 5.0])
        stats = compute_time_series_stats(data, name="test")
        assert stats.mean == pytest.approx(3.0)
        assert stats.min_val == pytest.approx(1.0)
        assert stats.max_val == pytest.approx(5.0)
        assert stats.n_samples == 5

    def test_rms_calculation(self):
        """RMS should be sqrt(mean(x^2))."""
        data = np.array([3.0, 4.0])  # RMS = sqrt((9+16)/2) = sqrt(12.5)
        stats = compute_time_series_stats(data)
        assert stats.rms == pytest.approx(math.sqrt(12.5), abs=1e-4)

    def test_percentiles(self):
        """Percentiles should be ordered correctly."""
        data = np.random.randn(1000)
        stats = compute_time_series_stats(data)
        assert stats.p01 < stats.p05 < stats.p50 < stats.p95 < stats.p99

    def test_empty_data(self):
        """Empty array should return zeroed stats."""
        stats = compute_time_series_stats(np.array([]))
        assert stats.n_samples == 0
        assert stats.mean == 0.0

    def test_significant_value(self):
        """Significant value should be mean of top 1/3 absolute values."""
        data = np.array([1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
        stats = compute_time_series_stats(data)
        # Top 1/3 of abs values: [5, 6] => mean = 5.5
        assert stats.significant == pytest.approx(5.5, abs=0.1)


class TestRangeGraph:
    """Tests for range graph generation."""

    def test_basic_range_graph(self):
        """Should compute stats at each arc length position."""
        arc = np.array([0.0, 50.0, 100.0])
        # 10 timesteps, 3 positions
        data = np.random.randn(10, 3) + np.array([100, 200, 300])
        rg = generate_range_graph(arc, data, "Tension (kN)")
        assert len(rg.arc_lengths) == 3
        assert len(rg.max_values) == 3
        assert rg.max_values[2] > rg.min_values[2]

    def test_max_of_max(self):
        """max_of_max should be overall maximum."""
        rg = RangeGraphData(max_values=[100, 200, 150])
        assert rg.max_of_max == 200

    def test_min_of_min(self):
        """min_of_min should be overall minimum."""
        rg = RangeGraphData(min_values=[-10, -20, -5])
        assert rg.min_of_min == -20


class TestExtremeValueAnalysis:
    """Tests for Gumbel and Weibull fitting."""

    def test_gumbel_fit(self):
        """Gumbel fit should produce valid parameters."""
        np.random.seed(42)
        from scipy.stats import gumbel_r
        data = gumbel_r.rvs(loc=100, scale=10, size=100)
        result = fit_gumbel(data)
        assert isinstance(result, ExtremeValueResult)
        assert result.distribution == "Gumbel"
        assert result.location > 0
        assert result.scale > 0
        assert len(result.return_values) > 0

    def test_gumbel_return_values_increase(self):
        """Longer return periods should give higher extreme values."""
        np.random.seed(42)
        data = np.random.gumbel(loc=50, scale=5, size=200)
        result = fit_gumbel(data, return_periods=[10, 100, 1000])
        vals = list(result.return_values.values())
        assert vals[0] < vals[1] < vals[2]

    def test_weibull_fit(self):
        """Weibull fit should produce valid parameters."""
        np.random.seed(42)
        from scipy.stats import weibull_min
        data = weibull_min.rvs(c=2.0, loc=0, scale=10, size=100)
        result = fit_weibull(data)
        assert result.distribution == "Weibull"
        assert result.shape > 0
        assert result.scale > 0

    def test_ks_test_good_fit(self):
        """Gumbel data fitted to Gumbel should have high p-value."""
        np.random.seed(42)
        data = np.random.gumbel(loc=100, scale=10, size=500)
        result = fit_gumbel(data)
        # p-value should be decent for a correct distribution
        assert result.ks_pvalue > 0.01


class TestMultiSeed:
    """Tests for multi-seed processing."""

    def test_basic_multi_seed(self):
        """Multi-seed processing should compute correct summary."""
        maxima = np.array([100.0, 105.0, 98.0, 102.0, 110.0])
        result = process_multi_seed(maxima)
        assert result.n_seeds == 5
        assert result.max_of_max == pytest.approx(110.0)
        assert result.mean_of_max == pytest.approx(103.0)

    def test_characteristic_above_mean(self):
        """Characteristic value should be above mean of maxima."""
        maxima = np.array([100, 120, 110, 105, 115, 108, 112])
        result = process_multi_seed(maxima)
        assert result.characteristic > result.mean_of_max

    def test_single_seed(self):
        """Single seed should work without error."""
        maxima = np.array([50.0])
        result = process_multi_seed(maxima)
        assert result.n_seeds == 1
        assert result.max_of_max == pytest.approx(50.0)

    def test_empty_seeds(self):
        """Empty array should return default values."""
        result = process_multi_seed(np.array([]))
        assert result.n_seeds == 0
