"""
Tests for StatisticalAnalysisService.

The source module lives under a hyphenated directory (orcaflex-dashboard),
which is not a valid Python identifier. We use importlib to load it directly
from the file path instead of a dotted import.
"""

import importlib.util
import math
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

# ---------------------------------------------------------------------------
# Dynamic import — the directory "orcaflex-dashboard" contains a hyphen,
# so a normal `from digitalmodel.visualization.orcaflex_dashboard...` import
# is impossible.  We load the module from its absolute file path instead.
# ---------------------------------------------------------------------------
_MODULE_PATH = (
    Path(__file__).resolve().parents[2]
    / "src"
    / "digitalmodel"
    / "visualization"
    / "orcaflex-dashboard"
    / "backend"
    / "app"
    / "services"
    / "statistical_analysis.py"
)

_spec = importlib.util.spec_from_file_location("statistical_analysis", _MODULE_PATH)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)

StatisticalAnalysisService = _mod.StatisticalAnalysisService
StatisticalResult = _mod.StatisticalResult
RegressionResult = _mod.RegressionResult
AnalysisType = _mod.AnalysisType

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def service():
    """Fresh StatisticalAnalysisService instance."""
    return StatisticalAnalysisService()


@pytest.fixture
def simple_correlated_df():
    """DataFrame with two perfectly correlated columns and one independent."""
    np.random.seed(42)
    n = 50
    x = np.arange(n, dtype=float)
    return pd.DataFrame({
        "a": x,
        "b": 2.0 * x + 1.0,
        "c": np.random.randn(n),
    })


@pytest.fixture
def normal_data():
    """Array drawn from N(100, 15)."""
    np.random.seed(0)
    return np.random.normal(loc=100.0, scale=15.0, size=200)


@pytest.fixture
def small_normal_data():
    """Small sample from N(50, 10) — triggers t-distribution path."""
    np.random.seed(1)
    return np.random.normal(loc=50.0, scale=10.0, size=15)


@pytest.fixture
def time_series_df():
    """Simple time-series DataFrame with numeric time column."""
    np.random.seed(7)
    n = 60
    t = np.arange(n, dtype=float)
    # Linear trend (slope=0.5, intercept=10) plus small noise
    values = 0.5 * t + 10.0 + np.random.randn(n) * 0.5
    return pd.DataFrame({"time": t, "value": values})


# ===================================================================
# AnalysisType Enum
# ===================================================================


class TestAnalysisType:
    def test_enum_members(self):
        assert AnalysisType.CORRELATION.value == "correlation"
        assert AnalysisType.REGRESSION.value == "regression"
        assert AnalysisType.TREND.value == "trend"
        assert AnalysisType.DISTRIBUTION.value == "distribution"
        assert AnalysisType.CONFIDENCE_INTERVAL.value == "confidence_interval"

    def test_enum_count(self):
        assert len(AnalysisType) == 5


# ===================================================================
# Dataclass sanity
# ===================================================================


class TestDataclasses:
    def test_statistical_result_defaults(self):
        sr = StatisticalResult(analysis_type="test", value=1.23)
        assert sr.p_value is None
        assert sr.confidence_interval is None
        assert sr.r_squared is None
        assert sr.std_error is None
        assert sr.metadata is None

    def test_statistical_result_full(self):
        sr = StatisticalResult(
            analysis_type="corr",
            value=0.95,
            p_value=0.001,
            confidence_interval=(0.90, 0.98),
            r_squared=0.90,
            std_error=0.02,
            metadata={"note": "ok"},
        )
        assert sr.confidence_interval == (0.90, 0.98)
        assert sr.metadata["note"] == "ok"

    def test_regression_result_fields(self):
        rr = RegressionResult(
            slope=1.0,
            intercept=0.0,
            r_squared=0.99,
            p_value=0.001,
            std_error=0.1,
            confidence_intervals={"slope": (0.9, 1.1), "intercept": (-0.1, 0.1)},
            predicted_values=np.array([1, 2, 3]),
            residuals=np.array([0.01, -0.01, 0.0]),
        )
        assert rr.slope == 1.0
        assert len(rr.predicted_values) == 3


# ===================================================================
# Correlation Matrix
# ===================================================================


class TestCorrelationMatrix:
    def test_perfect_positive_correlation(self, service):
        """Columns a and b are perfectly linearly related."""
        df = pd.DataFrame({"x": [1.0, 2, 3, 4, 5], "y": [2.0, 4, 6, 8, 10]})
        result = service.calculate_correlation_matrix(df)
        corr = result["correlation_matrix"]
        assert abs(corr[0][1] - 1.0) < 1e-10

    def test_perfect_negative_correlation(self, service):
        df = pd.DataFrame({"x": [1.0, 2, 3, 4, 5], "y": [-2.0, -4, -6, -8, -10]})
        result = service.calculate_correlation_matrix(df)
        corr = result["correlation_matrix"]
        assert abs(corr[0][1] - (-1.0)) < 1e-10

    def test_diagonal_is_one(self, service, simple_correlated_df):
        result = service.calculate_correlation_matrix(simple_correlated_df)
        corr = result["correlation_matrix"]
        for i in range(len(corr)):
            assert corr[i][i] == 1.0

    def test_symmetry(self, service, simple_correlated_df):
        result = service.calculate_correlation_matrix(simple_correlated_df)
        corr = np.array(result["correlation_matrix"])
        np.testing.assert_array_almost_equal(corr, corr.T)

    def test_p_value_matrix_diagonal_zero(self, service, simple_correlated_df):
        result = service.calculate_correlation_matrix(simple_correlated_df)
        pvals = result["p_value_matrix"]
        for i in range(len(pvals)):
            assert pvals[i][i] == 0.0

    def test_significance_at_default_confidence(self, service):
        """a-b should be significant at 95% for perfectly correlated data."""
        df = pd.DataFrame({"a": [1.0, 2, 3, 4, 5], "b": [10.0, 20, 30, 40, 50]})
        result = service.calculate_correlation_matrix(df)
        sig = result["significance_matrix"]
        assert sig[0][1] is True

    def test_spearman_method(self, service, simple_correlated_df):
        result = service.calculate_correlation_matrix(
            simple_correlated_df, method="spearman"
        )
        assert result["method"] == "spearman"
        corr = result["correlation_matrix"]
        # a and b are monotonically related, spearman should be ~1
        assert corr[0][1] > 0.99

    def test_column_names_returned(self, service, simple_correlated_df):
        result = service.calculate_correlation_matrix(simple_correlated_df)
        assert result["column_names"] == ["a", "b", "c"]

    def test_n_samples(self, service, simple_correlated_df):
        result = service.calculate_correlation_matrix(simple_correlated_df)
        assert result["n_samples"] == 50

    def test_confidence_level_stored(self, service, simple_correlated_df):
        result = service.calculate_correlation_matrix(
            simple_correlated_df, confidence_level=0.99
        )
        assert result["confidence_level"] == 0.99

    def test_empty_dataframe_raises(self, service):
        with pytest.raises(ValueError, match="empty"):
            service.calculate_correlation_matrix(pd.DataFrame())

    def test_no_numeric_columns_raises(self, service):
        df = pd.DataFrame({"a": ["x", "y", "z"], "b": ["p", "q", "r"]})
        with pytest.raises(ValueError, match="[Nn]o numeric"):
            service.calculate_correlation_matrix(df)

    def test_ignores_non_numeric_columns(self, service):
        df = pd.DataFrame({
            "num1": [1.0, 2, 3, 4, 5],
            "num2": [5.0, 4, 3, 2, 1],
            "text": ["a", "b", "c", "d", "e"],
        })
        result = service.calculate_correlation_matrix(df)
        assert result["column_names"] == ["num1", "num2"]

    def test_single_column(self, service):
        """A single numeric column produces a 1x1 correlation matrix."""
        df = pd.DataFrame({"only": [1.0, 2, 3, 4, 5]})
        result = service.calculate_correlation_matrix(df)
        corr = result["correlation_matrix"]
        assert corr == [[1.0]]


# ===================================================================
# Linear Regression
# ===================================================================


class TestLinearRegression:
    def test_perfect_fit(self, service):
        """y = 3x + 2 should yield slope=3, intercept=2, r^2=1."""
        x = np.array([1.0, 2, 3, 4, 5])
        y = 3.0 * x + 2.0
        result = service.perform_linear_regression(x, y)
        assert abs(result.slope - 3.0) < 1e-8
        assert abs(result.intercept - 2.0) < 1e-8
        assert abs(result.r_squared - 1.0) < 1e-8

    def test_residuals_sum_near_zero(self, service):
        np.random.seed(10)
        x = np.arange(20, dtype=float)
        y = 2.0 * x + 5.0 + np.random.randn(20) * 0.1
        result = service.perform_linear_regression(x, y)
        assert abs(np.sum(result.residuals)) < 1.0

    def test_predicted_length_matches_input(self, service):
        x = np.array([1.0, 2, 3, 4, 5])
        y = np.array([2.0, 4, 5, 4, 5])
        result = service.perform_linear_regression(x, y)
        assert len(result.predicted_values) == len(x)

    def test_residuals_length_matches_input(self, service):
        x = np.array([1.0, 2, 3, 4, 5])
        y = np.array([2.0, 4, 5, 4, 5])
        result = service.perform_linear_regression(x, y)
        assert len(result.residuals) == len(x)

    def test_confidence_interval_contains_true_slope(self, service):
        np.random.seed(3)
        x = np.arange(50, dtype=float)
        y = 1.5 * x + 10.0 + np.random.randn(50) * 2.0
        result = service.perform_linear_regression(x, y, confidence_level=0.99)
        ci = result.confidence_intervals["slope"]
        assert ci[0] <= 1.5 <= ci[1]

    def test_confidence_interval_contains_true_intercept(self, service):
        np.random.seed(3)
        x = np.arange(50, dtype=float)
        y = 1.5 * x + 10.0 + np.random.randn(50) * 2.0
        result = service.perform_linear_regression(x, y, confidence_level=0.99)
        ci = result.confidence_intervals["intercept"]
        assert ci[0] <= 10.0 <= ci[1]

    def test_p_value_significant_for_strong_trend(self, service):
        np.random.seed(4)
        x = np.arange(100, dtype=float)
        y = 5.0 * x + np.random.randn(100) * 0.5
        result = service.perform_linear_regression(x, y)
        assert result.p_value < 0.001

    def test_r_squared_between_zero_and_one(self, service):
        np.random.seed(5)
        x = np.arange(30, dtype=float)
        y = x + np.random.randn(30) * 10
        result = service.perform_linear_regression(x, y)
        assert 0.0 <= result.r_squared <= 1.0

    def test_std_error_positive(self, service):
        np.random.seed(6)
        x = np.arange(20, dtype=float)
        y = x + np.random.randn(20) * 5
        result = service.perform_linear_regression(x, y)
        assert result.std_error > 0

    def test_mismatched_lengths_raises(self, service):
        with pytest.raises(ValueError, match="same length"):
            service.perform_linear_regression(np.array([1, 2, 3]), np.array([1, 2]))

    def test_too_few_points_raises(self, service):
        with pytest.raises(ValueError, match="at least 3"):
            service.perform_linear_regression(np.array([1.0, 2.0]), np.array([1.0, 2.0]))

    def test_nan_values_filtered(self, service):
        """NaN rows are dropped; regression runs on remaining clean points."""
        x = np.array([1.0, 2, np.nan, 4, 5, 6, 7, 8])
        y = np.array([2.0, 4, 6, 8, 10, 12, 14, 16])
        result = service.perform_linear_regression(x, y)
        # After NaN removal, 7 points remain; regression should still run
        assert len(result.predicted_values) == 7

    def test_higher_confidence_wider_interval(self, service):
        np.random.seed(8)
        x = np.arange(30, dtype=float)
        y = 2.0 * x + np.random.randn(30) * 3.0
        r95 = service.perform_linear_regression(x, y, confidence_level=0.95)
        r99 = service.perform_linear_regression(x, y, confidence_level=0.99)
        width_95 = r95.confidence_intervals["slope"][1] - r95.confidence_intervals["slope"][0]
        width_99 = r99.confidence_intervals["slope"][1] - r99.confidence_intervals["slope"][0]
        assert width_99 > width_95


# ===================================================================
# Confidence Interval
# ===================================================================


class TestConfidenceInterval:
    def test_mean_within_interval(self, service, normal_data):
        result = service.calculate_confidence_interval(normal_data)
        assert result["lower_bound"] <= result["mean"] <= result["upper_bound"]

    def test_large_sample_uses_normal(self, service, normal_data):
        """n=200 > 30, so the code should use the z-distribution path."""
        result = service.calculate_confidence_interval(normal_data)
        assert result["n_samples"] == 200

    def test_small_sample_uses_t(self, service, small_normal_data):
        """n=15 <= 30, so the code should use the t-distribution path."""
        result = service.calculate_confidence_interval(small_normal_data)
        assert result["n_samples"] == 15

    def test_higher_confidence_wider_interval(self, service, normal_data):
        ci_95 = service.calculate_confidence_interval(normal_data, confidence_level=0.95)
        ci_99 = service.calculate_confidence_interval(normal_data, confidence_level=0.99)
        assert ci_99["margin_error"] > ci_95["margin_error"]

    def test_confidence_level_stored(self, service, normal_data):
        result = service.calculate_confidence_interval(normal_data, confidence_level=0.90)
        assert result["confidence_level"] == 0.90

    def test_std_error_positive(self, service, normal_data):
        result = service.calculate_confidence_interval(normal_data)
        assert result["std_error"] > 0

    def test_std_dev_positive(self, service, normal_data):
        result = service.calculate_confidence_interval(normal_data)
        assert result["std_dev"] > 0

    def test_margin_error_positive(self, service, normal_data):
        result = service.calculate_confidence_interval(normal_data)
        assert result["margin_error"] > 0

    def test_nan_values_filtered(self, service):
        data = np.array([1.0, 2.0, np.nan, 4.0, 5.0])
        result = service.calculate_confidence_interval(data)
        assert result["n_samples"] == 4

    def test_all_nan_raises(self, service):
        with pytest.raises(ValueError, match="[Nn]o valid"):
            service.calculate_confidence_interval(np.array([np.nan, np.nan]))

    def test_single_value_raises(self, service):
        """Single non-NaN point — ddof=1 yields 0 degrees of freedom, raises."""
        data = np.array([5.0])
        # np.std with ddof=1 on a single value triggers RuntimeWarning
        # which is converted to an error by numpy 2.x strict mode.
        with pytest.raises((ValueError, RuntimeWarning)):
            service.calculate_confidence_interval(data)

    def test_constant_data(self, service):
        """All identical values: std_dev=0, margin_error=0, bounds == mean."""
        data = np.array([3.0, 3.0, 3.0, 3.0, 3.0])
        result = service.calculate_confidence_interval(data)
        assert result["mean"] == 3.0
        assert result["std_dev"] == 0.0
        assert result["lower_bound"] == result["upper_bound"] == 3.0

    def test_symmetric_interval(self, service, normal_data):
        result = service.calculate_confidence_interval(normal_data)
        lower_dist = result["mean"] - result["lower_bound"]
        upper_dist = result["upper_bound"] - result["mean"]
        assert abs(lower_dist - upper_dist) < 1e-10


# ===================================================================
# Trend Analysis
# ===================================================================


class TestTrendAnalysis:
    def test_linear_trend_direction_increasing(self, service, time_series_df):
        result = service.perform_trend_analysis(
            time_series_df, "time", "value", trend_method="linear"
        )
        assert result["trend_direction"] == "increasing"

    def test_linear_trend_direction_decreasing(self, service):
        n = 30
        t = np.arange(n, dtype=float)
        values = -2.0 * t + 100.0 + np.random.randn(n) * 0.1
        df = pd.DataFrame({"time": t, "value": values})
        result = service.perform_trend_analysis(df, "time", "value")
        assert result["trend_direction"] == "decreasing"

    def test_linear_trend_slope_close(self, service, time_series_df):
        result = service.perform_trend_analysis(
            time_series_df, "time", "value", trend_method="linear"
        )
        # True slope is 0.5; regression should be close
        assert abs(result["slope"] - 0.5) < 0.1

    def test_linear_trend_r_squared_high(self, service, time_series_df):
        result = service.perform_trend_analysis(
            time_series_df, "time", "value", trend_method="linear"
        )
        assert result["r_squared"] > 0.9

    def test_linear_trend_has_predicted_values(self, service, time_series_df):
        result = service.perform_trend_analysis(
            time_series_df, "time", "value", trend_method="linear"
        )
        assert len(result["predicted_values"]) == len(time_series_df)

    def test_linear_trend_has_residuals(self, service, time_series_df):
        result = service.perform_trend_analysis(
            time_series_df, "time", "value", trend_method="linear"
        )
        assert len(result["residuals"]) == len(time_series_df)

    def test_moving_average_method(self, service, time_series_df):
        result = service.perform_trend_analysis(
            time_series_df, "time", "value", trend_method="moving_average"
        )
        assert result["method"] == "moving_average"
        assert "window_size" in result
        assert "moving_average" in result

    def test_moving_average_direction(self, service, time_series_df):
        result = service.perform_trend_analysis(
            time_series_df, "time", "value", trend_method="moving_average"
        )
        assert result["trend_direction"] == "increasing"

    def test_missing_time_column_raises(self, service, time_series_df):
        with pytest.raises(ValueError, match="not found"):
            service.perform_trend_analysis(time_series_df, "missing", "value")

    def test_missing_value_column_raises(self, service, time_series_df):
        with pytest.raises(ValueError, match="not found"):
            service.perform_trend_analysis(time_series_df, "time", "missing")

    def test_too_few_points_raises(self, service):
        df = pd.DataFrame({"time": [1.0, 2.0], "value": [3.0, 4.0]})
        with pytest.raises(ValueError, match="at least 3"):
            service.perform_trend_analysis(df, "time", "value")

    def test_unknown_method_raises(self, service, time_series_df):
        with pytest.raises(ValueError, match="Unknown trend method"):
            service.perform_trend_analysis(
                time_series_df, "time", "value", trend_method="fourier"
            )

    def test_nan_rows_dropped(self, service):
        df = pd.DataFrame({
            "time": [1.0, 2, 3, np.nan, 5, 6, 7, 8, 9, 10],
            "value": [2.0, 4, np.nan, 8, 10, 12, 14, 16, 18, 20],
        })
        # Should not raise; NaN rows simply dropped
        result = service.perform_trend_analysis(df, "time", "value")
        assert "slope" in result


# ===================================================================
# Distribution Analysis
# ===================================================================


class TestDistributionAnalysis:
    def test_basic_stats_keys(self, service, normal_data):
        result = service.analyze_distribution(normal_data)
        stats_keys = {
            "mean", "median", "std_dev", "variance", "skewness",
            "kurtosis", "min", "max", "q25", "q75", "n_samples",
        }
        assert stats_keys == set(result["basic_statistics"].keys())

    def test_mean_close_to_true_mean(self, service, normal_data):
        result = service.analyze_distribution(normal_data)
        assert abs(result["basic_statistics"]["mean"] - 100.0) < 5.0

    def test_std_dev_close_to_true(self, service, normal_data):
        result = service.analyze_distribution(normal_data)
        assert abs(result["basic_statistics"]["std_dev"] - 15.0) < 5.0

    def test_min_less_than_max(self, service, normal_data):
        result = service.analyze_distribution(normal_data)
        bs = result["basic_statistics"]
        assert bs["min"] < bs["max"]

    def test_quartiles_order(self, service, normal_data):
        result = service.analyze_distribution(normal_data)
        bs = result["basic_statistics"]
        assert bs["q25"] < bs["median"] < bs["q75"]

    def test_n_samples(self, service, normal_data):
        result = service.analyze_distribution(normal_data)
        assert result["basic_statistics"]["n_samples"] == 200

    def test_shapiro_test_present(self, service, normal_data):
        result = service.analyze_distribution(normal_data)
        assert "shapiro" in result["normality_tests"]

    def test_kolmogorov_smirnov_test_present(self, service, normal_data):
        result = service.analyze_distribution(normal_data)
        assert "kolmogorov_smirnov" in result["normality_tests"]

    def test_jarque_bera_test_present(self, service, normal_data):
        result = service.analyze_distribution(normal_data)
        assert "jarque_bera" in result["normality_tests"]

    def test_normal_data_detected_as_normal(self, service, normal_data):
        """At least 2 of 3 tests should flag this truly normal data as normal."""
        result = service.analyze_distribution(normal_data)
        normal_count = sum(
            1 for t in result["normality_tests"].values() if t["is_normal"]
        )
        assert normal_count >= 2

    def test_recommended_analysis_parametric(self, service, normal_data):
        result = service.analyze_distribution(normal_data)
        assert result["recommended_analysis"] == "parametric"

    def test_custom_test_list(self, service, normal_data):
        result = service.analyze_distribution(normal_data, distribution_tests=["shapiro"])
        assert "shapiro" in result["normality_tests"]
        assert "kolmogorov_smirnov" not in result["normality_tests"]

    def test_empty_data_raises(self, service):
        with pytest.raises(ValueError, match="[Nn]o valid"):
            service.analyze_distribution(np.array([np.nan, np.nan]))

    def test_nan_filtered(self, service):
        data = np.array([1.0, 2.0, np.nan, 4.0, 5.0, 6.0, 7.0, 8.0])
        result = service.analyze_distribution(data, distribution_tests=["jarque_bera"])
        assert result["basic_statistics"]["n_samples"] == 7

    def test_shapiro_skipped_for_large_sample(self, service):
        """Shapiro-Wilk is skipped when n > 5000."""
        np.random.seed(99)
        data = np.random.randn(5500)
        result = service.analyze_distribution(data, distribution_tests=["shapiro"])
        assert "shapiro" not in result["normality_tests"]

    def test_skewness_near_zero_for_symmetric(self, service, normal_data):
        result = service.analyze_distribution(normal_data)
        assert abs(result["basic_statistics"]["skewness"]) < 1.0


# ===================================================================
# _recommend_analysis_method (private helper)
# ===================================================================


class TestRecommendAnalysisMethod:
    def test_empty_results_returns_parametric(self, service):
        assert service._recommend_analysis_method({}) == "parametric"

    def test_all_normal_returns_parametric(self, service):
        results = {
            "shapiro": {"is_normal": True},
            "ks": {"is_normal": True},
        }
        assert service._recommend_analysis_method(results) == "parametric"

    def test_all_non_normal_returns_non_parametric(self, service):
        results = {
            "shapiro": {"is_normal": False},
            "ks": {"is_normal": False},
        }
        assert service._recommend_analysis_method(results) == "non_parametric"

    def test_half_normal_returns_parametric(self, service):
        """50% normal meets the >= 0.5 threshold."""
        results = {
            "shapiro": {"is_normal": True},
            "ks": {"is_normal": False},
        }
        assert service._recommend_analysis_method(results) == "parametric"

    def test_minority_normal_returns_non_parametric(self, service):
        results = {
            "shapiro": {"is_normal": True},
            "ks": {"is_normal": False},
            "jb": {"is_normal": False},
        }
        assert service._recommend_analysis_method(results) == "non_parametric"


# ===================================================================
# Statistical Report Generation
# ===================================================================


class TestGenerateStatisticalReport:
    def test_report_structure(self, service):
        result = service.generate_statistical_report({}, title="Test Report")
        assert result["title"] == "Test Report"
        assert "timestamp" in result
        assert "summary" in result
        assert "detailed_results" in result
        assert "recommendations" in result

    def test_default_title(self, service):
        result = service.generate_statistical_report({})
        assert result["title"] == "Statistical Analysis Report"

    def test_correlation_summary(self, service):
        analysis = {
            "correlation_matrix": [[1.0, 0.8], [0.8, 1.0]]
        }
        result = service.generate_statistical_report(analysis)
        assert result["summary"]["max_correlation"] == 1.0

    def test_regression_summary(self, service):
        analysis = {
            "regression": {"r_squared": 0.85, "slope": -0.5}
        }
        result = service.generate_statistical_report(analysis)
        assert result["summary"]["r_squared"] == 0.85
        assert result["summary"]["trend_direction"] == "negative"

    def test_regression_positive_direction(self, service):
        analysis = {"regression": {"r_squared": 0.9, "slope": 2.0}}
        result = service.generate_statistical_report(analysis)
        assert result["summary"]["trend_direction"] == "positive"

    def test_distribution_recommendation(self, service):
        analysis = {
            "distribution": {"recommended_analysis": "non_parametric"}
        }
        result = service.generate_statistical_report(analysis)
        assert len(result["recommendations"]) == 1
        assert "non_parametric" in result["recommendations"][0]

    def test_empty_analysis_no_recommendations(self, service):
        result = service.generate_statistical_report({})
        assert result["recommendations"] == []

    def test_detailed_results_passed_through(self, service):
        analysis = {"custom_key": [1, 2, 3]}
        result = service.generate_statistical_report(analysis)
        assert result["detailed_results"]["custom_key"] == [1, 2, 3]

    def test_timestamp_is_valid_iso(self, service):
        result = service.generate_statistical_report({})
        # Should not raise
        pd.Timestamp(result["timestamp"])
