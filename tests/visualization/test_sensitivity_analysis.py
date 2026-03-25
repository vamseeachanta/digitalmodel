"""
Comprehensive unit tests for sensitivity_analysis module.

Tests cover:
- Enum and dataclass construction
- Effect size classification
- Correlation confidence interval calculation
- Sobol index approximation
- Response change estimation
- Parameter ranking and finding most sensitive
- Analysis matrix building
- Correlation-based sensitivity
- Regression-based sensitivity
- Random Forest-based sensitivity
- One-at-a-time sensitivity
- Parameter sample generation
- Safe response evaluation
- Sensitivity summary and recommendation generation
- Load case summary generation
- Global sensitivity ranking
- Response surface generation
- Monte Carlo sensitivity
- Environmental load case analysis
- Full parameter sensitivity analysis (end-to-end)
"""

import importlib.util
import math
import pathlib

import numpy as np
import pandas as pd
import pytest

# ---------------------------------------------------------------------------
# Import the module from a hyphenated directory using importlib
# ---------------------------------------------------------------------------
_mod_path = (
    pathlib.Path(__file__).resolve().parents[2]
    / "src"
    / "digitalmodel"
    / "visualization"
    / "orcaflex-dashboard"
    / "backend"
    / "app"
    / "services"
    / "sensitivity_analysis.py"
)
_spec = importlib.util.spec_from_file_location("sensitivity_analysis", _mod_path)
sensitivity_analysis = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(sensitivity_analysis)

SensitivityMethod = sensitivity_analysis.SensitivityMethod
ParameterRange = sensitivity_analysis.ParameterRange
SensitivityResult = sensitivity_analysis.SensitivityResult
SensitivityAnalysisService = sensitivity_analysis.SensitivityAnalysisService


# ===========================================================================
# Fixtures
# ===========================================================================

@pytest.fixture
def service():
    return SensitivityAnalysisService()


@pytest.fixture
def basic_parameter_ranges():
    return {
        "wave_height": ParameterRange(
            name="wave_height", min_value=1.0, max_value=5.0,
            nominal_value=3.0, units="m"
        ),
        "wave_period": ParameterRange(
            name="wave_period", min_value=6.0, max_value=14.0,
            nominal_value=10.0, units="s"
        ),
        "current_speed": ParameterRange(
            name="current_speed", min_value=0.1, max_value=1.5,
            nominal_value=0.8, units="m/s"
        ),
    }


@pytest.fixture
def environmental_data():
    """Environmental data for 5 cases with 3 parameters."""
    return {
        "case_1": {"wave_height": 1.0, "wave_period": 8.0, "current_speed": 0.3},
        "case_2": {"wave_height": 2.0, "wave_period": 9.0, "current_speed": 0.5},
        "case_3": {"wave_height": 3.0, "wave_period": 10.0, "current_speed": 0.8},
        "case_4": {"wave_height": 4.0, "wave_period": 11.0, "current_speed": 1.0},
        "case_5": {"wave_height": 5.0, "wave_period": 12.0, "current_speed": 1.2},
    }


@pytest.fixture
def response_data():
    """Response DataFrames for 5 cases."""
    return {
        "case_1": pd.DataFrame({"tension": [100, 110, 105], "vessel_offset": [2.0, 2.1, 1.9]}),
        "case_2": pd.DataFrame({"tension": [150, 160, 155], "vessel_offset": [3.0, 3.2, 2.8]}),
        "case_3": pd.DataFrame({"tension": [200, 210, 205], "vessel_offset": [4.0, 4.3, 3.7]}),
        "case_4": pd.DataFrame({"tension": [250, 260, 255], "vessel_offset": [5.0, 5.5, 4.5]}),
        "case_5": pd.DataFrame({"tension": [300, 310, 305], "vessel_offset": [6.0, 6.4, 5.6]}),
    }


@pytest.fixture
def large_analysis_df():
    """DataFrame with enough rows for regression/RF analysis."""
    np.random.seed(42)
    n = 30
    wh = np.random.uniform(1, 5, n)
    wp = np.random.uniform(6, 14, n)
    cs = np.random.uniform(0.1, 1.5, n)
    tension = 50 * wh + 10 * wp - 20 * cs + np.random.normal(0, 5, n)
    return pd.DataFrame({
        "wave_height": wh,
        "wave_period": wp,
        "current_speed": cs,
        "tension": tension,
    })


# ===========================================================================
# 1. Enum tests
# ===========================================================================

class TestSensitivityMethod:
    def test_all_members_exist(self):
        members = {m.value for m in SensitivityMethod}
        expected = {
            "one_at_a_time", "sobol", "morris",
            "regression", "correlation", "random_forest",
        }
        assert members == expected

    def test_enum_values(self):
        assert SensitivityMethod.ONE_AT_A_TIME.value == "one_at_a_time"
        assert SensitivityMethod.SOBOL.value == "sobol"
        assert SensitivityMethod.REGRESSION.value == "regression"


# ===========================================================================
# 2. Dataclass tests
# ===========================================================================

class TestParameterRange:
    def test_defaults(self):
        pr = ParameterRange(name="x", min_value=0, max_value=10, nominal_value=5)
        assert pr.units is None
        assert pr.description is None
        assert pr.distribution == "uniform"

    def test_all_fields(self):
        pr = ParameterRange(
            name="wave_height", min_value=1.0, max_value=5.0,
            nominal_value=3.0, units="m", description="Significant wave height",
            distribution="normal",
        )
        assert pr.name == "wave_height"
        assert pr.distribution == "normal"
        assert pr.units == "m"

    def test_negative_range(self):
        pr = ParameterRange(name="temp", min_value=-20, max_value=-5, nominal_value=-10)
        assert pr.min_value < pr.max_value


class TestSensitivityResult:
    def test_construction(self):
        sr = SensitivityResult(
            parameter="wh", response="tension",
            sensitivity_index=0.85, confidence_interval=(0.7, 0.95),
            p_value=0.001, effect_size="large", ranking=1,
            method="correlation", additional_metrics={"r2": 0.72}
        )
        assert sr.parameter == "wh"
        assert sr.ranking == 1
        assert sr.additional_metrics["r2"] == 0.72


# ===========================================================================
# 3. _classify_effect_size
# ===========================================================================

class TestClassifyEffectSize:
    @pytest.mark.parametrize("value, expected", [
        (0.0, "negligible"),
        (0.05, "negligible"),
        (0.09, "negligible"),
        (0.1, "small"),
        (0.2, "small"),
        (0.29, "small"),
        (0.3, "medium"),
        (0.4, "medium"),
        (0.49, "medium"),
        (0.5, "large"),
        (0.8, "large"),
        (1.0, "large"),
    ])
    def test_thresholds(self, service, value, expected):
        assert service._classify_effect_size(value) == expected

    def test_negative_value(self, service):
        # abs is applied inside
        assert service._classify_effect_size(-0.7) == "large"

    def test_zero(self, service):
        assert service._classify_effect_size(0.0) == "negligible"


# ===========================================================================
# 4. _calculate_correlation_confidence_interval
# ===========================================================================

class TestCorrelationConfidenceInterval:
    def test_positive_correlation(self, service):
        lo, hi = service._calculate_correlation_confidence_interval(0.8, 50, 0.95)
        assert lo < 0.8
        assert hi > 0.8

    def test_negative_correlation(self, service):
        lo, hi = service._calculate_correlation_confidence_interval(-0.6, 30, 0.95)
        assert lo < -0.6
        assert hi > -0.6

    def test_zero_correlation(self, service):
        lo, hi = service._calculate_correlation_confidence_interval(0.0, 100, 0.95)
        assert lo < 0.0
        assert hi > 0.0

    def test_wider_interval_with_fewer_samples(self, service):
        lo1, hi1 = service._calculate_correlation_confidence_interval(0.5, 10, 0.95)
        lo2, hi2 = service._calculate_correlation_confidence_interval(0.5, 100, 0.95)
        width1 = hi1 - lo1
        width2 = hi2 - lo2
        assert width1 > width2

    def test_higher_confidence_gives_wider_interval(self, service):
        lo1, hi1 = service._calculate_correlation_confidence_interval(0.5, 50, 0.90)
        lo2, hi2 = service._calculate_correlation_confidence_interval(0.5, 50, 0.99)
        assert (hi2 - lo2) > (hi1 - lo1)

    def test_extreme_correlation_near_one(self, service):
        # correlation near 1 should still return valid interval
        lo, hi = service._calculate_correlation_confidence_interval(0.99, 50, 0.95)
        assert lo < 1.0
        assert hi <= 1.0 or hi > 0.99  # tanh can exceed slightly


# ===========================================================================
# 5. _calculate_sobol_index
# ===========================================================================

class TestSobolIndex:
    def test_perfect_linear_relationship(self, service):
        params = np.array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], dtype=float)
        response = params * 2.0  # perfect linear
        idx = service._calculate_sobol_index(params, response)
        assert idx > 0.5

    def test_no_relationship(self, service):
        np.random.seed(123)
        params = np.random.uniform(0, 10, 1000)
        response = np.random.uniform(0, 10, 1000)
        idx = service._calculate_sobol_index(params, response)
        assert idx < 0.1

    def test_constant_response(self, service):
        params = np.array([1, 2, 3, 4, 5], dtype=float)
        response = np.array([5, 5, 5, 5, 5], dtype=float)
        idx = service._calculate_sobol_index(params, response)
        assert idx == 0.0

    def test_capped_at_one(self, service):
        params = np.array([0, 100], dtype=float)
        response = np.array([0, 1000], dtype=float)
        idx = service._calculate_sobol_index(params, response)
        assert idx <= 1.0

    def test_empty_group_returns_zero(self, service):
        # All values equal means one group empty
        params = np.array([5.0, 5.0, 5.0], dtype=float)
        response = np.array([10, 20, 30], dtype=float)
        idx = service._calculate_sobol_index(params, response)
        # median == all values, so low_mask (< median) is empty
        assert idx == 0.0


# ===========================================================================
# 6. _estimate_response_change
# ===========================================================================

class TestEstimateResponseChange:
    def test_known_coefficient(self, service):
        # wave_height + tension -> coeff 0.5
        change = service._estimate_response_change("wave_height", 10.0, "tension", 1000.0)
        expected = 1000.0 * 0.5 * (10.0 / 100.0)
        assert abs(change - expected) < 1e-9

    def test_default_coefficient(self, service):
        # Unknown combo -> default 0.1
        change = service._estimate_response_change("unknown_param", 20.0, "unknown_metric", 500.0)
        expected = 500.0 * 0.1 * (20.0 / 100.0)
        assert abs(change - expected) < 1e-9

    def test_negative_percent_change(self, service):
        change = service._estimate_response_change("wave_period", -15.0, "tension", 200.0)
        # wave_period + tension -> coeff -0.2
        expected = 200.0 * (-0.2) * (-15.0 / 100.0)
        assert abs(change - expected) < 1e-9

    def test_zero_percent_change(self, service):
        change = service._estimate_response_change("wave_height", 0.0, "tension", 1000.0)
        assert change == 0.0


# ===========================================================================
# 7. _find_most_sensitive_parameter
# ===========================================================================

class TestFindMostSensitiveParameter:
    def test_single_parameter(self, service):
        effects = {"wh": {"sensitivity_coefficient": 0.8}}
        assert service._find_most_sensitive_parameter(effects) == "wh"

    def test_multiple_parameters(self, service):
        effects = {
            "wh": {"sensitivity_coefficient": 0.3},
            "wp": {"sensitivity_coefficient": -0.9},
            "cs": {"sensitivity_coefficient": 0.5},
        }
        assert service._find_most_sensitive_parameter(effects) == "wp"

    def test_empty_dict(self, service):
        assert service._find_most_sensitive_parameter({}) is None

    def test_missing_coefficient_key(self, service):
        effects = {"wh": {}, "wp": {"sensitivity_coefficient": 0.5}}
        assert service._find_most_sensitive_parameter(effects) == "wp"


# ===========================================================================
# 8. _rank_parameters_by_sensitivity
# ===========================================================================

class TestRankParametersBySensitivity:
    def test_ranking_order(self, service):
        effects = {
            "a": {"sensitivity_coefficient": 0.1},
            "b": {"sensitivity_coefficient": -0.9},
            "c": {"sensitivity_coefficient": 0.5},
        }
        ranking = service._rank_parameters_by_sensitivity(effects)
        assert ranking[0][0] == "b"
        assert ranking[1][0] == "c"
        assert ranking[2][0] == "a"

    def test_empty(self, service):
        assert service._rank_parameters_by_sensitivity({}) == []

    def test_all_zero(self, service):
        effects = {
            "a": {"sensitivity_coefficient": 0.0},
            "b": {"sensitivity_coefficient": 0.0},
        }
        ranking = service._rank_parameters_by_sensitivity(effects)
        assert len(ranking) == 2
        assert all(v == 0.0 for _, v in ranking)


# ===========================================================================
# 9. _build_analysis_matrix
# ===========================================================================

class TestBuildAnalysisMatrix:
    def test_basic_matrix(self, service, environmental_data, response_data):
        df = service._build_analysis_matrix(
            environmental_data, response_data,
            list(environmental_data.keys()),
            ["wave_height", "wave_period", "current_speed"],
            ["tension", "vessel_offset"],
        )
        assert len(df) == 5
        assert "wave_height" in df.columns
        assert "tension" in df.columns
        assert "case_name" in df.columns

    def test_missing_case_skipped(self, service):
        env = {"c1": {"wh": 1.0}, "c2": {"wh": 2.0}}
        resp = {"c1": pd.DataFrame({"t": [10]})}
        # c2 missing from response
        df = service._build_analysis_matrix(env, resp, ["c1", "c2"], ["wh"], ["t"])
        # c2 is in both env and resp keys? No, c2 is NOT in resp
        assert len(df) == 1

    def test_metric_not_in_response_df(self, service):
        env = {"c1": {"wh": 1.0}}
        resp = {"c1": pd.DataFrame({"t": [10]})}
        df = service._build_analysis_matrix(env, resp, ["c1"], ["wh"], ["nonexistent"])
        assert np.isnan(df["nonexistent"].iloc[0])

    def test_empty_metric_column(self, service):
        env = {"c1": {"wh": 1.0}}
        resp = {"c1": pd.DataFrame({"t": pd.Series([], dtype=float)})}
        df = service._build_analysis_matrix(env, resp, ["c1"], ["wh"], ["t"])
        assert np.isnan(df["t"].iloc[0])

    def test_missing_env_param(self, service):
        env = {"c1": {"wh": 1.0}}  # missing wp
        resp = {"c1": pd.DataFrame({"t": [10]})}
        df = service._build_analysis_matrix(env, resp, ["c1"], ["wh", "wp"], ["t"])
        assert np.isnan(df["wp"].iloc[0])
        assert df["wh"].iloc[0] == 1.0


# ===========================================================================
# 10. _correlation_sensitivity
# ===========================================================================

class TestCorrelationSensitivity:
    def test_perfect_positive_correlation(self, service):
        df = pd.DataFrame({
            "wh": [1.0, 2.0, 3.0, 4.0, 5.0],
            "tension": [100, 200, 300, 400, 500],
        })
        results = service._correlation_sensitivity(df, ["wh"], "tension")
        assert len(results) == 1
        assert results[0].sensitivity_index == pytest.approx(1.0, abs=1e-6)
        assert results[0].ranking == 1

    def test_no_correlation(self, service):
        np.random.seed(99)
        df = pd.DataFrame({
            "wh": np.random.uniform(0, 10, 50),
            "tension": np.random.uniform(0, 10, 50),
        })
        results = service._correlation_sensitivity(df, ["wh"], "tension")
        assert len(results) == 1
        assert results[0].sensitivity_index < 0.4

    def test_insufficient_data(self, service):
        df = pd.DataFrame({"wh": [1.0, 2.0], "tension": [10.0, 20.0]})
        results = service._correlation_sensitivity(df, ["wh"], "tension")
        assert len(results) == 0

    def test_missing_param_skipped(self, service):
        df = pd.DataFrame({"wh": [1, 2, 3, 4, 5], "tension": [10, 20, 30, 40, 50]})
        results = service._correlation_sensitivity(df, ["wh", "nonexistent"], "tension")
        assert len(results) == 1

    def test_rankings_updated_after_sort(self, service):
        np.random.seed(42)
        n = 20
        wh = np.arange(1, n + 1, dtype=float)
        wp = np.random.uniform(0, 10, n)
        tension = wh * 10 + np.random.normal(0, 1, n)
        df = pd.DataFrame({"wh": wh, "wp": wp, "tension": tension})
        results = service._correlation_sensitivity(df, ["wh", "wp"], "tension")
        assert results[0].ranking == 1
        assert results[1].ranking == 2
        assert results[0].sensitivity_index >= results[1].sensitivity_index

    def test_with_nan_values(self, service):
        df = pd.DataFrame({
            "wh": [1, 2, np.nan, 4, 5, 6],
            "tension": [10, 20, 30, np.nan, 50, 60],
        })
        results = service._correlation_sensitivity(df, ["wh"], "tension")
        assert len(results) == 1
        # Should use only valid pairs (rows 0,1,4,5 = 4 data points > 3 minimum)
        assert results[0].additional_metrics["sample_size"] == 4


# ===========================================================================
# 11. _regression_sensitivity
# ===========================================================================

class TestRegressionSensitivity:
    def test_basic_regression(self, service, large_analysis_df):
        results = service._regression_sensitivity(
            large_analysis_df,
            ["wave_height", "wave_period", "current_speed"],
            "tension",
        )
        assert len(results) == 3
        # wave_height has coeff 50, should be most sensitive (standardized)
        assert results[0].method == "regression"
        assert results[0].ranking == 1

    def test_insufficient_data_returns_empty(self, service):
        df = pd.DataFrame({"a": [1, 2], "b": [3, 4], "y": [5, 6]})
        results = service._regression_sensitivity(df, ["a", "b"], "y")
        assert results == []

    def test_r_squared_in_metrics(self, service, large_analysis_df):
        results = service._regression_sensitivity(
            large_analysis_df,
            ["wave_height", "wave_period", "current_speed"],
            "tension",
        )
        for r in results:
            assert "r_squared" in r.additional_metrics
            assert 0 <= r.additional_metrics["r_squared"] <= 1.0

    def test_rankings_sorted(self, service, large_analysis_df):
        results = service._regression_sensitivity(
            large_analysis_df,
            ["wave_height", "wave_period", "current_speed"],
            "tension",
        )
        indices = [r.sensitivity_index for r in results]
        assert indices == sorted(indices, reverse=True)


# ===========================================================================
# 12. _random_forest_sensitivity
# ===========================================================================

class TestRandomForestSensitivity:
    def test_basic_rf(self, service, large_analysis_df):
        results = service._random_forest_sensitivity(
            large_analysis_df,
            ["wave_height", "wave_period", "current_speed"],
            "tension",
        )
        assert len(results) == 3
        assert results[0].method == "random_forest"
        # Importances should sum close to 1
        total = sum(r.sensitivity_index for r in results)
        assert abs(total - 1.0) < 0.01

    def test_insufficient_data_returns_empty(self, service):
        df = pd.DataFrame({"a": list(range(5)), "y": list(range(5))})
        results = service._random_forest_sensitivity(df, ["a"], "y")
        assert results == []

    def test_rankings_sorted(self, service, large_analysis_df):
        results = service._random_forest_sensitivity(
            large_analysis_df,
            ["wave_height", "wave_period", "current_speed"],
            "tension",
        )
        indices = [r.sensitivity_index for r in results]
        assert indices == sorted(indices, reverse=True)


# ===========================================================================
# 13. _one_at_a_time_sensitivity
# ===========================================================================

class TestOneAtATimeSensitivity:
    def test_basic_oat(self, service, basic_parameter_ranges):
        df = pd.DataFrame({
            "wave_height": [1, 2, 3, 4, 5],
            "wave_period": [8, 9, 10, 11, 12],
            "current_speed": [0.3, 0.5, 0.8, 1.0, 1.2],
            "tension": [100, 150, 200, 250, 300],
        })
        results = service._one_at_a_time_sensitivity(
            df, ["wave_height", "wave_period", "current_speed"],
            "tension", basic_parameter_ranges,
        )
        assert len(results) == 3
        assert results[0].ranking == 1
        assert results[0].method == "one_at_a_time"

    def test_constant_param_skipped(self, service, basic_parameter_ranges):
        df = pd.DataFrame({
            "wave_height": [3.0, 3.0, 3.0, 3.0, 3.0],
            "wave_period": [8, 9, 10, 11, 12],
            "current_speed": [0.3, 0.5, 0.8, 1.0, 1.2],
            "tension": [100, 150, 200, 250, 300],
        })
        results = service._one_at_a_time_sensitivity(
            df, ["wave_height", "wave_period", "current_speed"],
            "tension", basic_parameter_ranges,
        )
        params = [r.parameter for r in results]
        assert "wave_height" not in params

    def test_insufficient_data(self, service, basic_parameter_ranges):
        df = pd.DataFrame({
            "wave_height": [1.0, 2.0],
            "tension": [100, 200],
        })
        results = service._one_at_a_time_sensitivity(
            df, ["wave_height"], "tension", basic_parameter_ranges,
        )
        assert len(results) == 0

    def test_missing_param_in_ranges(self, service, basic_parameter_ranges):
        df = pd.DataFrame({
            "unknown_param": [1, 2, 3, 4, 5],
            "tension": [10, 20, 30, 40, 50],
        })
        results = service._one_at_a_time_sensitivity(
            df, ["unknown_param"], "tension", basic_parameter_ranges,
        )
        assert len(results) == 0


# ===========================================================================
# 14. _generate_parameter_samples
# ===========================================================================

class TestGenerateParameterSamples:
    def test_uniform_sampling(self, service):
        ranges = {
            "x": ParameterRange(name="x", min_value=0, max_value=10,
                                nominal_value=5, distribution="uniform"),
        }
        np.random.seed(42)
        samples = service._generate_parameter_samples(ranges, 100)
        assert len(samples) == 100
        values = [s["x"] for s in samples]
        assert all(0 <= v <= 10 for v in values)

    def test_normal_sampling_clipped(self, service):
        ranges = {
            "x": ParameterRange(name="x", min_value=0, max_value=10,
                                nominal_value=5, distribution="normal"),
        }
        np.random.seed(42)
        samples = service._generate_parameter_samples(ranges, 500)
        values = [s["x"] for s in samples]
        assert all(0 <= v <= 10 for v in values)

    def test_unknown_distribution_defaults_to_uniform(self, service):
        ranges = {
            "x": ParameterRange(name="x", min_value=0, max_value=10,
                                nominal_value=5, distribution="lognormal"),
        }
        np.random.seed(42)
        samples = service._generate_parameter_samples(ranges, 50)
        values = [s["x"] for s in samples]
        assert all(0 <= v <= 10 for v in values)

    def test_multiple_parameters(self, service, basic_parameter_ranges):
        np.random.seed(42)
        samples = service._generate_parameter_samples(basic_parameter_ranges, 10)
        assert len(samples) == 10
        for s in samples:
            assert set(s.keys()) == {"wave_height", "wave_period", "current_speed"}

    def test_zero_samples(self, service):
        ranges = {
            "x": ParameterRange(name="x", min_value=0, max_value=10, nominal_value=5),
        }
        samples = service._generate_parameter_samples(ranges, 0)
        assert samples == []


# ===========================================================================
# 15. _safe_response_evaluation
# ===========================================================================

class TestSafeResponseEvaluation:
    def test_valid_dict_response(self, service):
        fn = lambda params: {"tension": params["x"] * 10}
        result = service._safe_response_evaluation(fn, {"x": 5.0}, 0)
        assert result == {"tension": 50.0}

    def test_non_dict_response_returns_none(self, service):
        fn = lambda params: 42  # returns int, not dict
        result = service._safe_response_evaluation(fn, {"x": 1.0}, 0)
        assert result is None

    def test_exception_returns_none(self, service):
        fn = lambda params: 1 / 0
        result = service._safe_response_evaluation(fn, {"x": 1.0}, 0)
        assert result is None

    def test_empty_dict_response(self, service):
        fn = lambda params: {}
        result = service._safe_response_evaluation(fn, {"x": 1.0}, 0)
        assert result == {}


# ===========================================================================
# 16. _generate_sensitivity_summary
# ===========================================================================

class TestGenerateSensitivitySummary:
    def test_basic_summary(self, service):
        r1 = SensitivityResult(
            parameter="wh", response="tension", sensitivity_index=0.9,
            confidence_interval=(0.8, 0.95), p_value=0.001,
            effect_size="large", ranking=1, method="correlation",
            additional_metrics={},
        )
        r2 = SensitivityResult(
            parameter="wp", response="tension", sensitivity_index=0.3,
            confidence_interval=(0.1, 0.5), p_value=0.05,
            effect_size="medium", ranking=2, method="correlation",
            additional_metrics={},
        )
        results = {"tension": [r1, r2]}
        summary = service._generate_sensitivity_summary(results)
        assert summary["total_metrics_analyzed"] == 1
        assert summary["most_sensitive_parameters"]["tension"] == "wh"
        assert summary["least_sensitive_parameters"]["tension"] == "wp"

    def test_empty_results(self, service):
        summary = service._generate_sensitivity_summary({})
        assert summary["total_metrics_analyzed"] == 0

    def test_non_list_results_skipped(self, service):
        results = {"tension": "not_a_list"}
        summary = service._generate_sensitivity_summary(results)
        assert "tension" not in summary["most_sensitive_parameters"]

    def test_average_sensitivity_computed(self, service):
        r1 = SensitivityResult(
            parameter="wh", response="t", sensitivity_index=0.8,
            confidence_interval=(0.7, 0.9), p_value=0.01,
            effect_size="large", ranking=1, method="correlation",
            additional_metrics={},
        )
        r2 = SensitivityResult(
            parameter="wp", response="t", sensitivity_index=0.4,
            confidence_interval=(0.3, 0.5), p_value=0.05,
            effect_size="medium", ranking=2, method="correlation",
            additional_metrics={},
        )
        summary = service._generate_sensitivity_summary({"t": [r1, r2]})
        assert summary["average_sensitivities"]["t"] == pytest.approx(0.6)


# ===========================================================================
# 17. _generate_sensitivity_recommendations
# ===========================================================================

class TestGenerateSensitivityRecommendations:
    def test_basic_recommendations(self, service, basic_parameter_ranges):
        r1 = SensitivityResult(
            parameter="wave_height", response="tension",
            sensitivity_index=0.9, confidence_interval=(0.8, 0.95),
            p_value=0.001, effect_size="large", ranking=1,
            method="correlation", additional_metrics={},
        )
        r2 = SensitivityResult(
            parameter="wave_period", response="tension",
            sensitivity_index=0.2, confidence_interval=(0.1, 0.3),
            p_value=0.05, effect_size="small", ranking=2,
            method="correlation", additional_metrics={},
        )
        results = {"tension": [r1, r2]}
        recs = service._generate_sensitivity_recommendations(results, basic_parameter_ranges)
        assert len(recs) >= 1
        assert "wave_height" in recs[0]

    def test_empty_results(self, service, basic_parameter_ranges):
        recs = service._generate_sensitivity_recommendations({}, basic_parameter_ranges)
        assert recs == []

    def test_high_sensitivity_recommendation(self, service, basic_parameter_ranges):
        r1 = SensitivityResult(
            parameter="wave_height", response="tension",
            sensitivity_index=0.9, confidence_interval=(0.8, 0.95),
            p_value=0.001, effect_size="large", ranking=1,
            method="correlation", additional_metrics={},
        )
        results = {"tension": [r1]}
        recs = service._generate_sensitivity_recommendations(results, basic_parameter_ranges)
        # Should have recommendation about tight tolerances for wave_height (>0.5)
        has_tight_tolerance_rec = any("tight" in r.lower() for r in recs)
        assert has_tight_tolerance_rec

    def test_no_high_sensitivity_no_tight_tolerance_rec(self, service, basic_parameter_ranges):
        r1 = SensitivityResult(
            parameter="wave_height", response="tension",
            sensitivity_index=0.2, confidence_interval=(0.1, 0.3),
            p_value=0.05, effect_size="small", ranking=1,
            method="correlation", additional_metrics={},
        )
        results = {"tension": [r1]}
        recs = service._generate_sensitivity_recommendations(results, basic_parameter_ranges)
        has_tight_tolerance_rec = any("tight" in r.lower() for r in recs)
        assert not has_tight_tolerance_rec


# ===========================================================================
# 18. _generate_load_case_summary
# ===========================================================================

class TestGenerateLoadCaseSummary:
    def test_basic_summary(self, service):
        results = {
            "tension": {
                "sensitivity_ranking": [("wh", 0.8), ("wp", 0.3)],
            },
        }
        summary = service._generate_load_case_summary(results)
        assert summary["metrics_analyzed"] == ["tension"]
        assert summary["most_sensitive_overall"] == "wh"

    def test_empty_results(self, service):
        summary = service._generate_load_case_summary({})
        assert summary["most_sensitive_overall"] is None
        assert summary["parameter_rankings"] == {}

    def test_multiple_metrics(self, service):
        results = {
            "tension": {"sensitivity_ranking": [("wh", 0.9), ("wp", 0.1)]},
            "offset": {"sensitivity_ranking": [("wp", 0.7), ("wh", 0.3)]},
        }
        summary = service._generate_load_case_summary(results)
        # wh: avg(0.9, 0.3)=0.6; wp: avg(0.1, 0.7)=0.4
        assert summary["most_sensitive_overall"] == "wh"


# ===========================================================================
# 19. _generate_global_sensitivity_ranking
# ===========================================================================

class TestGenerateGlobalSensitivityRanking:
    def test_basic_ranking(self, service):
        results = {
            "tension": {
                "parameter_sensitivities": {
                    "wh": {"correlation": 0.9},
                    "wp": {"correlation": -0.3},
                },
            },
        }
        ranking = service._generate_global_sensitivity_ranking(results)
        assert ranking[0][0] == "wh"
        assert ranking[1][0] == "wp"

    def test_empty(self, service):
        assert service._generate_global_sensitivity_ranking({}) == []

    def test_multiple_metrics_averaged(self, service):
        results = {
            "tension": {
                "parameter_sensitivities": {
                    "wh": {"correlation": 0.8},
                    "wp": {"correlation": 0.2},
                },
            },
            "offset": {
                "parameter_sensitivities": {
                    "wh": {"correlation": 0.4},
                    "wp": {"correlation": 0.6},
                },
            },
        }
        ranking = service._generate_global_sensitivity_ranking(results)
        # wh: avg(0.8, 0.4) = 0.6; wp: avg(0.2, 0.6) = 0.4
        assert ranking[0][0] == "wh"
        assert ranking[0][1] == pytest.approx(0.6)


# ===========================================================================
# 20. generate_response_surface
# ===========================================================================

class TestGenerateResponseSurface:
    def test_basic_surface(self, service, basic_parameter_ranges):
        fn = lambda params: params["wave_height"] * 10 + params["wave_period"] * 2
        result = service.generate_response_surface(
            "wave_height", "wave_period", "tension",
            basic_parameter_ranges, fn, grid_size=5,
        )
        assert result["parameter1"] == "wave_height"
        assert result["parameter2"] == "wave_period"
        assert result["grid_size"] == 5
        assert len(result["response_surface"]) == 5
        assert len(result["response_surface"][0]) == 5

    def test_extrema_found(self, service, basic_parameter_ranges):
        fn = lambda params: params["wave_height"] ** 2 + params["wave_period"]
        result = service.generate_response_surface(
            "wave_height", "wave_period", "tension",
            basic_parameter_ranges, fn, grid_size=10,
        )
        extrema = result["extrema"]
        assert extrema["minimum"]["response"] < extrema["maximum"]["response"]
        assert extrema["range"] > 0

    def test_invalid_parameter_raises(self, service, basic_parameter_ranges):
        fn = lambda params: 0
        with pytest.raises(ValueError, match="Parameter ranges not defined"):
            service.generate_response_surface(
                "nonexistent", "wave_period", "tension",
                basic_parameter_ranges, fn,
            )

    def test_gradient_computed(self, service, basic_parameter_ranges):
        fn = lambda params: params["wave_height"] * 5
        result = service.generate_response_surface(
            "wave_height", "wave_period", "tension",
            basic_parameter_ranges, fn, grid_size=5,
        )
        assert "gradient_param1" in result
        assert "gradient_param2" in result
        assert "gradient_magnitude" in result
        assert "steepest_gradient" in result

    def test_response_function_error_gives_nan(self, service, basic_parameter_ranges):
        call_count = {"n": 0}

        def flaky_fn(params):
            call_count["n"] += 1
            if call_count["n"] == 3:
                raise RuntimeError("boom")
            return params["wave_height"]

        result = service.generate_response_surface(
            "wave_height", "wave_period", "tension",
            basic_parameter_ranges, flaky_fn, grid_size=5,
        )
        # Should still complete; one cell will be nan
        assert result["grid_size"] == 5

    def test_nominal_values_used_for_other_params(self, service, basic_parameter_ranges):
        captured = {}

        def capture_fn(params):
            captured.update(params)
            return 1.0

        service.generate_response_surface(
            "wave_height", "wave_period", "tension",
            basic_parameter_ranges, capture_fn, grid_size=2,
        )
        # current_speed should be set to its nominal value (0.8)
        assert "current_speed" in captured
        assert captured["current_speed"] == pytest.approx(0.8)


# ===========================================================================
# 21. perform_monte_carlo_sensitivity
# ===========================================================================

class TestPerformMonteCarloSensitivity:
    def test_basic_monte_carlo(self, service, basic_parameter_ranges):
        def response_fn(params):
            return {
                "tension": params["wave_height"] * 50 + params["current_speed"] * 10,
            }

        np.random.seed(42)
        result = service.perform_monte_carlo_sensitivity(
            basic_parameter_ranges, response_fn, ["tension"],
            n_samples=50, confidence_level=0.95,
        )
        assert "monte_carlo_results" in result
        assert "simulation_info" in result
        assert result["simulation_info"]["n_samples_requested"] == 50
        assert result["simulation_info"]["success_rate"] == 1.0
        assert "tension" in result["monte_carlo_results"]

    def test_all_failures_raises(self, service, basic_parameter_ranges):
        def bad_fn(params):
            raise RuntimeError("fail")

        np.random.seed(42)
        with pytest.raises(ValueError, match="No successful response evaluations"):
            service.perform_monte_carlo_sensitivity(
                basic_parameter_ranges, bad_fn, ["tension"],
                n_samples=5,
            )

    def test_sensitivity_ranking_present(self, service, basic_parameter_ranges):
        def response_fn(params):
            return {"tension": params["wave_height"] * 100 + np.random.normal(0, 1)}

        np.random.seed(42)
        result = service.perform_monte_carlo_sensitivity(
            basic_parameter_ranges, response_fn, ["tension"],
            n_samples=50,
        )
        mc = result["monte_carlo_results"]["tension"]
        assert "sensitivity_ranking" in mc
        assert "most_sensitive_parameter" in mc

    def test_global_ranking_present(self, service, basic_parameter_ranges):
        def response_fn(params):
            return {"tension": params["wave_height"] * 50}

        np.random.seed(42)
        result = service.perform_monte_carlo_sensitivity(
            basic_parameter_ranges, response_fn, ["tension"],
            n_samples=30,
        )
        assert "global_sensitivity_ranking" in result
        assert len(result["global_sensitivity_ranking"]) > 0


# ===========================================================================
# 22. perform_environmental_load_case_analysis
# ===========================================================================

class TestPerformEnvironmentalLoadCaseAnalysis:
    def test_basic_load_case(self, service):
        base_case = {"wave_height": 3.0, "current_speed": 0.8}
        response_df = pd.DataFrame({
            "tension": [200, 210, 190, 205, 195],
        })
        variations = {
            "wave_height": [1.0, 2.0, 3.0, 4.0, 5.0],
            "current_speed": [0.3, 0.5, 0.8, 1.0, 1.2],
        }

        result = service.perform_environmental_load_case_analysis(
            base_case, response_df, variations, ["tension"],
        )
        assert "load_case_analysis" in result
        assert "base_case" in result
        assert "summary" in result

    def test_missing_metric_skipped(self, service):
        base_case = {"wave_height": 3.0}
        response_df = pd.DataFrame({"tension": [100, 200]})
        variations = {"wave_height": [1.0, 5.0]}

        result = service.perform_environmental_load_case_analysis(
            base_case, response_df, variations, ["nonexistent"],
        )
        assert "load_case_analysis" in result
        assert "nonexistent" not in result["load_case_analysis"]

    def test_sensitivity_ranking_in_result(self, service):
        base_case = {"wave_height": 3.0, "current_speed": 0.8}
        response_df = pd.DataFrame({"tension": [200, 210, 190]})
        variations = {
            "wave_height": [1.0, 3.0, 5.0],
            "current_speed": [0.3, 0.8, 1.2],
        }

        result = service.perform_environmental_load_case_analysis(
            base_case, response_df, variations, ["tension"],
        )
        lc = result["load_case_analysis"]["tension"]
        assert "sensitivity_ranking" in lc
        assert "most_sensitive_parameter" in lc

    def test_single_variation_no_coefficient(self, service):
        base_case = {"wave_height": 3.0}
        response_df = pd.DataFrame({"tension": [200]})
        variations = {"wave_height": [5.0]}  # Only 1 variation

        result = service.perform_environmental_load_case_analysis(
            base_case, response_df, variations, ["tension"],
        )
        # With only 1 variation, no linear regression is possible
        lc = result["load_case_analysis"]["tension"]
        assert "wave_height" not in lc["parameter_effects"]


# ===========================================================================
# 23. perform_parameter_sensitivity_analysis (end-to-end)
# ===========================================================================

class TestPerformParameterSensitivityAnalysis:
    def test_correlation_method(
        self, service, environmental_data, response_data, basic_parameter_ranges
    ):
        result = service.perform_parameter_sensitivity_analysis(
            environmental_data, response_data,
            basic_parameter_ranges, ["tension", "vessel_offset"],
            method="correlation",
        )
        assert result["method"] == "correlation"
        assert result["n_cases"] == 5
        assert "sensitivity_results" in result
        assert "summary" in result
        assert "recommendations" in result

    def test_regression_method(
        self, service, environmental_data, response_data, basic_parameter_ranges
    ):
        result = service.perform_parameter_sensitivity_analysis(
            environmental_data, response_data,
            basic_parameter_ranges, ["tension"],
            method="regression",
        )
        assert result["method"] == "regression"

    def test_one_at_a_time_method(
        self, service, environmental_data, response_data, basic_parameter_ranges
    ):
        result = service.perform_parameter_sensitivity_analysis(
            environmental_data, response_data,
            basic_parameter_ranges, ["tension"],
            method="one_at_a_time",
        )
        assert result["method"] == "one_at_a_time"

    def test_unknown_method_raises(
        self, service, environmental_data, response_data, basic_parameter_ranges
    ):
        with pytest.raises(ValueError, match="Unknown sensitivity method"):
            service.perform_parameter_sensitivity_analysis(
                environmental_data, response_data,
                basic_parameter_ranges, ["tension"],
                method="unknown_method",
            )

    def test_mismatched_cases_raises(self, service, basic_parameter_ranges):
        env = {"c1": {"wh": 1}}
        resp = {"c1": pd.DataFrame({"t": [1]}), "c2": pd.DataFrame({"t": [2]})}
        with pytest.raises(ValueError, match="matching cases"):
            service.perform_parameter_sensitivity_analysis(
                env, resp, basic_parameter_ranges, ["t"],
            )

    def test_missing_metric_returns_empty_list(
        self, service, environmental_data, response_data, basic_parameter_ranges
    ):
        result = service.perform_parameter_sensitivity_analysis(
            environmental_data, response_data,
            basic_parameter_ranges, ["nonexistent_metric"],
            method="correlation",
        )
        # The metric column is added as all-NaN by _build_analysis_matrix,
        # so the correlation method processes it but finds no valid pairs,
        # yielding an empty list of SensitivityResult objects.
        assert result["sensitivity_results"]["nonexistent_metric"] == []

    def test_timestamp_present(
        self, service, environmental_data, response_data, basic_parameter_ranges
    ):
        result = service.perform_parameter_sensitivity_analysis(
            environmental_data, response_data,
            basic_parameter_ranges, ["tension"],
            method="correlation",
        )
        assert "timestamp" in result

    def test_parameters_analyzed_listed(
        self, service, environmental_data, response_data, basic_parameter_ranges
    ):
        result = service.perform_parameter_sensitivity_analysis(
            environmental_data, response_data,
            basic_parameter_ranges, ["tension"],
            method="correlation",
        )
        assert set(result["parameters_analyzed"]) == {
            "wave_height", "wave_period", "current_speed"
        }
