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
    encounter_probability,
    return_period_for_probability,
    fit_gumbel,
    fit_weibull,
    generate_range_graph,
    process_multi_seed,
)


class TestEncounterProbability:
    """Independent-year encounter probabilities and their inverse."""

    @pytest.mark.parametrize("period, life, expected, tolerance", [
        (100, 25, 0.222, 0.0005),
        (10000, 25, 0.0025, 0.00005),
        (1, 1, 1.0, 1e-15),
        (2, 2, 0.75, 1e-15),
        (100, 1, 0.01, 1e-15),
        (1e20, 25, 2.5e-19, 1e-30),
    ])
    def test_hand_values(self, period, life, expected, tolerance):
        assert encounter_probability(period, life) == pytest.approx(
            expected, abs=tolerance
        )

    @pytest.mark.parametrize("probability, life, expected", [
        (0.75, 2, 2.0),
        (0.01, 1, 100.0),
        (1.0, 1, 1.0),
        (1.0, 25, 1.0),
        (2.5e-19, 25, 1e20),
    ])
    def test_inverse_hand_values(self, probability, life, expected):
        assert return_period_for_probability(probability, life) == pytest.approx(
            expected, rel=1e-12
        )

    @pytest.mark.parametrize("life", [1e10, 1e100])
    def test_inverse_beyond_float_range(self, life):
        assert return_period_for_probability(1e-300, life) == math.inf

    @pytest.mark.parametrize("period, life", [(100, 25), (10000, 25), (2, 0.5)])
    def test_round_trip(self, period, life):
        probability = encounter_probability(period, life)
        assert return_period_for_probability(probability, life) == pytest.approx(period)

    @pytest.mark.parametrize("period", [0, -1, 0.5, math.inf, -math.inf, math.nan])
    def test_invalid_return_period(self, period):
        with pytest.raises(ValueError, match="return_period"):
            encounter_probability(period, 25)

    @pytest.mark.parametrize("life", [0, -1, math.inf, -math.inf, math.nan])
    def test_invalid_design_life(self, life):
        with pytest.raises(ValueError, match="design_life"):
            encounter_probability(100, life)
        with pytest.raises(ValueError, match="design_life"):
            return_period_for_probability(0.2, life)

    @pytest.mark.parametrize("probability", [0, -0.1, 1.1, math.inf, -math.inf, math.nan])
    def test_invalid_probability(self, probability):
        with pytest.raises(ValueError, match="probability"):
            return_period_for_probability(probability, 25)


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

    @pytest.mark.parametrize("location", [7.0, -7.0])
    def test_weibull_recovers_three_parameters(self, location):
        """Shifted synthetic maxima recover shape, location and scale."""
        shape, scale = 2.5, 3.0
        rng = np.random.default_rng(2026)
        data = location + scale * rng.weibull(shape, size=20000)

        result = fit_weibull(data, return_periods=[10, 100, 1000])

        assert result.location == pytest.approx(location, abs=0.08)
        assert result.shape == pytest.approx(shape, rel=0.05)
        assert result.scale == pytest.approx(scale, rel=0.03)
        for period in (10, 100, 1000):
            expected = location + scale * math.log(period) ** (1 / shape)
            assert result.return_values[str(period)] == pytest.approx(
                expected, abs=0.15
            )

    def test_weibull_rejects_degenerate_ten_maxima_fit(self):
        """A small sample with a boundary MLE must not yield return values."""
        data = 7 + 3 * np.random.default_rng(2).weibull(2.5, size=10)
        with pytest.raises(ValueError, match="degenerate Weibull fit"):
            fit_weibull(data)

    @pytest.mark.parametrize("offset", [0.0, 1000.0])
    def test_weibull_rejects_location_at_sample_minimum(self, monkeypatch, offset):
        """The location guard also catches boundary fits with shape >= 1."""
        data = offset + np.linspace(1.0, 4.0, 20)
        monkeypatch.setattr(
            "digitalmodel.orcaflex.postprocessor.sp_stats.weibull_min.fit",
            lambda maxima: (2.5, float(maxima.min()) - 1e-7, 3.0),
        )
        with pytest.raises(ValueError, match="degenerate Weibull fit"):
            fit_weibull(data)

    def test_weibull_rejects_subunit_shape_away_from_boundary(self, monkeypatch):
        monkeypatch.setattr(
            "digitalmodel.orcaflex.postprocessor.sp_stats.weibull_min.fit",
            lambda maxima: (0.8, 0.0, 3.0),
        )
        with pytest.raises(ValueError, match="degenerate Weibull fit"):
            fit_weibull(np.linspace(1.0, 4.0, 20))

    @pytest.mark.parametrize("data", [
        np.arange(9.0), np.ones(10), np.zeros((2, 10)),
        np.append(np.arange(9.0), np.nan),
        np.append(np.arange(9.0), np.inf),
    ])
    def test_weibull_requires_ten_finite_nonconstant_maxima(self, data):
        with pytest.raises(ValueError, match="at least 10 finite, nonconstant"):
            fit_weibull(data)


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
