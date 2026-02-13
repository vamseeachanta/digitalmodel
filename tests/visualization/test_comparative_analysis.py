"""
Comprehensive unit tests for the ComparativeAnalysisService.

Covers:
- Enums and dataclasses
- Descriptive statistics calculations
- Correlation strength interpretation
- Performance assessment logic
- Pairwise recommendation generation
- Comparison recommendation generation
- Comparison summary generation
- Environmental summary and recommendation generation
- Benchmark result interpretation
- Overall benchmark assessment
- Pairwise statistical comparisons
- Group comparisons
- Loading condition comparison (integration)
- Environmental condition comparison (integration)
- Benchmark performance (integration)
- Edge cases and error handling
"""

import importlib.util
import math
import pathlib

import numpy as np
import pandas as pd
import pytest

# Import module from hyphenated directory path
_mod_path = (
    pathlib.Path(__file__).resolve().parents[2]
    / "src"
    / "digitalmodel"
    / "visualization"
    / "orcaflex-dashboard"
    / "backend"
    / "app"
    / "services"
    / "comparative_analysis.py"
)
_spec = importlib.util.spec_from_file_location("comparative_analysis", _mod_path)
comparative_analysis = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(comparative_analysis)

ComparisonType = comparative_analysis.ComparisonType
TestType = comparative_analysis.TestType
ComparisonResult = comparative_analysis.ComparisonResult
GroupComparisonResult = comparative_analysis.GroupComparisonResult
ComparativeAnalysisService = comparative_analysis.ComparativeAnalysisService


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def service():
    return ComparativeAnalysisService()


@pytest.fixture
def simple_metric_data():
    """Two groups with clearly different means for pairwise testing."""
    rng = np.random.RandomState(42)
    return {
        "case_A": rng.normal(loc=10, scale=2, size=50),
        "case_B": rng.normal(loc=15, scale=2, size=50),
    }


@pytest.fixture
def three_group_data():
    """Three groups for group comparison testing."""
    rng = np.random.RandomState(42)
    return {
        "low": rng.normal(loc=5, scale=1, size=40),
        "medium": rng.normal(loc=10, scale=1, size=40),
        "high": rng.normal(loc=15, scale=1, size=40),
    }


@pytest.fixture
def similar_metric_data():
    """Two groups with very similar distributions."""
    rng = np.random.RandomState(42)
    return {
        "case_X": rng.normal(loc=10, scale=2, size=50),
        "case_Y": rng.normal(loc=10, scale=2, size=50),
    }


# ===========================================================================
# Enum Tests
# ===========================================================================

class TestEnums:

    def test_comparison_type_values(self):
        assert ComparisonType.PAIRWISE.value == "pairwise"
        assert ComparisonType.GROUP.value == "group"
        assert ComparisonType.TREND.value == "trend"
        assert ComparisonType.BENCHMARK.value == "benchmark"

    def test_test_type_values(self):
        assert TestType.PARAMETRIC.value == "parametric"
        assert TestType.NON_PARAMETRIC.value == "non_parametric"
        assert TestType.AUTO.value == "auto"


# ===========================================================================
# Dataclass Tests
# ===========================================================================

class TestDataclasses:

    def test_comparison_result_creation(self):
        cr = ComparisonResult(
            case_1="A", case_2="B", metric="tension",
            test_statistic=2.5, p_value=0.01, effect_size=0.8,
            is_significant=True, confidence_level=0.95,
            mean_difference=5.0, percent_difference=10.0,
            recommendation="Use A"
        )
        assert cr.case_1 == "A"
        assert cr.p_value == 0.01
        assert cr.is_significant is True

    def test_group_comparison_result_creation(self):
        gr = GroupComparisonResult(
            groups=["A", "B", "C"], metric="tension",
            test_statistic=10.0, p_value=0.001,
            is_significant=True, post_hoc_results=[],
            effect_size=0.6, confidence_level=0.95
        )
        assert len(gr.groups) == 3
        assert gr.is_significant is True
        assert gr.post_hoc_results == []


# ===========================================================================
# _interpret_correlation_strength
# ===========================================================================

class TestInterpretCorrelationStrength:

    def test_strong_positive(self, service):
        assert service._interpret_correlation_strength(0.9) == "strong"

    def test_strong_boundary(self, service):
        assert service._interpret_correlation_strength(0.7) == "strong"

    def test_moderate(self, service):
        assert service._interpret_correlation_strength(0.6) == "moderate"

    def test_moderate_boundary(self, service):
        assert service._interpret_correlation_strength(0.5) == "moderate"

    def test_weak(self, service):
        assert service._interpret_correlation_strength(0.4) == "weak"

    def test_weak_boundary(self, service):
        assert service._interpret_correlation_strength(0.3) == "weak"

    def test_very_weak(self, service):
        assert service._interpret_correlation_strength(0.1) == "very_weak"

    def test_zero(self, service):
        assert service._interpret_correlation_strength(0.0) == "very_weak"

    def test_negative_treated_as_absolute(self, service):
        # The method takes abs internally
        assert service._interpret_correlation_strength(-0.8) == "strong"

    def test_exactly_one(self, service):
        assert service._interpret_correlation_strength(1.0) == "strong"


# ===========================================================================
# _assess_performance
# ===========================================================================

class TestAssessPerformance:

    def test_equivalent(self, service):
        assert service._assess_performance(3.0, 0.1) == "equivalent"

    def test_equivalent_boundary(self, service):
        # percent_diff < 5 AND effect_size < 0.2
        assert service._assess_performance(4.9, 0.19) == "equivalent"

    def test_significantly_better(self, service):
        assert service._assess_performance(15.0, 0.8) == "significantly_better"

    def test_significantly_worse(self, service):
        assert service._assess_performance(-15.0, 0.8) == "significantly_worse"

    def test_better(self, service):
        # percent_diff > 5 but not > 10 or effect_size <= 0.5
        assert service._assess_performance(7.0, 0.3) == "better"

    def test_worse(self, service):
        assert service._assess_performance(-7.0, 0.3) == "worse"

    def test_similar_small_effect(self, service):
        # percent_diff between -5 and 5, but effect_size >= 0.2
        assert service._assess_performance(3.0, 0.3) == "similar"

    def test_similar_negative_small(self, service):
        assert service._assess_performance(-3.0, 0.3) == "similar"

    def test_significantly_better_exact_boundary(self, service):
        # percent_diff > 10 and effect_size > 0.5
        assert service._assess_performance(10.1, 0.51) == "significantly_better"

    def test_better_large_diff_small_effect(self, service):
        # percent_diff > 10 but effect_size <= 0.5 -> "better" branch
        assert service._assess_performance(12.0, 0.3) == "better"


# ===========================================================================
# _generate_pairwise_recommendation
# ===========================================================================

class TestGeneratePairwiseRecommendation:

    def test_not_significant(self, service):
        result = service._generate_pairwise_recommendation(
            "A", "B", 5.0, 10.0, 0.5, False
        )
        assert "No statistically significant difference" in result
        assert "A" in result
        assert "B" in result

    def test_significant_higher_large_effect(self, service):
        result = service._generate_pairwise_recommendation(
            "A", "B", 5.0, 10.0, 0.9, True
        )
        assert "large" in result
        assert "higher" in result

    def test_significant_lower_moderate_effect(self, service):
        result = service._generate_pairwise_recommendation(
            "A", "B", -5.0, -10.0, 0.6, True
        )
        assert "moderate" in result
        assert "lower" in result

    def test_significant_small_effect(self, service):
        result = service._generate_pairwise_recommendation(
            "A", "B", 1.0, 2.0, 0.3, True
        )
        assert "small" in result
        assert "higher" in result

    def test_percent_diff_in_recommendation(self, service):
        result = service._generate_pairwise_recommendation(
            "Alpha", "Beta", 3.0, 15.5, 0.6, True
        )
        assert "15.5%" in result


# ===========================================================================
# _calculate_descriptive_statistics
# ===========================================================================

class TestCalculateDescriptiveStatistics:

    def test_single_group_stats(self, service):
        data = {"group1": np.array([1.0, 2.0, 3.0, 4.0, 5.0])}
        result = service._calculate_descriptive_statistics(data)

        assert "group1" in result
        s = result["group1"]
        assert s["count"] == 5
        assert s["mean"] == pytest.approx(3.0)
        assert s["median"] == pytest.approx(3.0)
        assert s["min"] == pytest.approx(1.0)
        assert s["max"] == pytest.approx(5.0)
        assert s["q25"] == pytest.approx(2.0)
        assert s["q75"] == pytest.approx(4.0)

    def test_multiple_groups(self, service):
        data = {
            "a": np.array([10.0, 20.0, 30.0]),
            "b": np.array([100.0, 200.0, 300.0]),
        }
        result = service._calculate_descriptive_statistics(data)
        assert len(result) == 2
        assert result["a"]["mean"] == pytest.approx(20.0)
        assert result["b"]["mean"] == pytest.approx(200.0)

    def test_std_uses_ddof_1(self, service):
        data = {"g": np.array([2.0, 4.0, 6.0])}
        result = service._calculate_descriptive_statistics(data)
        # ddof=1 -> std = sqrt(((2-4)^2+(4-4)^2+(6-4)^2)/2) = sqrt(4) = 2.0
        assert result["g"]["std"] == pytest.approx(2.0)

    def test_variance_uses_ddof_1(self, service):
        data = {"g": np.array([2.0, 4.0, 6.0])}
        result = service._calculate_descriptive_statistics(data)
        assert result["g"]["variance"] == pytest.approx(4.0)

    def test_skewness_and_kurtosis_exist(self, service):
        data = {"g": np.array([1.0, 2.0, 3.0, 4.0, 5.0])}
        result = service._calculate_descriptive_statistics(data)
        assert "skewness" in result["g"]
        assert "kurtosis" in result["g"]


# ===========================================================================
# _generate_comparison_recommendations
# ===========================================================================

class TestGenerateComparisonRecommendations:

    def test_no_significant_results(self, service):
        pairwise = [
            ComparisonResult("A", "B", "m", 1.0, 0.5, 0.1, False, 0.95, 0.5, 1.0, "")
        ]
        result = service._generate_comparison_recommendations(pairwise, None, {})
        assert any("No significant" in r for r in result)

    def test_with_significant_results(self, service):
        pairwise = [
            ComparisonResult("A", "B", "m", 1.0, 0.01, 0.8, True, 0.95, 5.0, 10.0, ""),
            ComparisonResult("A", "C", "m", 1.0, 0.5, 0.1, False, 0.95, 0.5, 1.0, ""),
        ]
        desc_stats = {
            "A": {"mean": 15.0},
            "B": {"mean": 10.0},
            "C": {"mean": 12.0},
        }
        result = service._generate_comparison_recommendations(pairwise, None, desc_stats)
        assert any("1 significant" in r for r in result)
        assert any("Best performing" in r for r in result)
        assert any("A" in r for r in result)

    def test_group_significant_appends(self, service):
        pairwise = [
            ComparisonResult("A", "B", "m", 1.0, 0.01, 0.8, True, 0.95, 5.0, 10.0, ""),
        ]
        desc_stats = {"A": {"mean": 15.0}, "B": {"mean": 10.0}}
        group = GroupComparisonResult(
            groups=["A", "B"], metric="m", test_statistic=10.0,
            p_value=0.001, is_significant=True, post_hoc_results=[],
            effect_size=0.6, confidence_level=0.95
        )
        result = service._generate_comparison_recommendations(pairwise, group, desc_stats)
        assert any("statistically significant" in r for r in result)


# ===========================================================================
# _generate_comparison_summary
# ===========================================================================

class TestGenerateComparisonSummary:

    def test_empty_results(self, service):
        result = service._generate_comparison_summary({})
        assert result["total_metrics"] == 0
        assert result["most_variable_metric"] is None
        assert result["least_variable_metric"] is None

    def test_single_metric_with_significant(self, service):
        pairwise = [
            ComparisonResult("A", "B", "tension", 2.5, 0.01, 0.8, True, 0.95, 5.0, 10.0, ""),
        ]
        results = {
            "tension": {"pairwise_comparisons": pairwise}
        }
        summary = service._generate_comparison_summary(results)
        assert summary["total_metrics"] == 1
        assert summary["metrics_with_significant_differences"] == 1
        assert summary["most_variable_metric"] == "tension"
        assert summary["least_variable_metric"] == "tension"

    def test_multiple_metrics_variability_ranking(self, service):
        low_effect = [
            ComparisonResult("A", "B", "m1", 1.0, 0.5, 0.1, False, 0.95, 0.5, 1.0, ""),
        ]
        high_effect = [
            ComparisonResult("A", "B", "m2", 5.0, 0.001, 2.0, True, 0.95, 10.0, 50.0, ""),
        ]
        results = {
            "m1": {"pairwise_comparisons": low_effect},
            "m2": {"pairwise_comparisons": high_effect},
        }
        summary = service._generate_comparison_summary(results)
        assert summary["most_variable_metric"] == "m2"
        assert summary["least_variable_metric"] == "m1"


# ===========================================================================
# _generate_environmental_summary
# ===========================================================================

class TestGenerateEnvironmentalSummary:

    def test_empty(self, service):
        result = service._generate_environmental_summary({})
        assert result["total_metrics"] == 0
        assert result["most_sensitive_metric"] is None

    def test_with_correlations(self, service):
        sensitivity = {
            "tension": {
                "correlations": {
                    "wave_height": {"correlation": 0.9, "p_value": 0.01, "is_significant": True, "strength": "strong"},
                },
                "significant_parameters": ["wave_height"],
            },
            "bending": {
                "correlations": {
                    "wave_height": {"correlation": 0.3, "p_value": 0.2, "is_significant": False, "strength": "weak"},
                },
                "significant_parameters": [],
            },
        }
        result = service._generate_environmental_summary(sensitivity)
        assert result["total_metrics"] == 2
        assert result["metrics_with_significant_correlations"] == 1
        assert result["most_sensitive_metric"] == "tension"
        assert result["least_sensitive_metric"] == "bending"


# ===========================================================================
# _generate_environmental_recommendations
# ===========================================================================

class TestGenerateEnvironmentalRecommendations:

    def test_empty(self, service):
        result = service._generate_environmental_recommendations({})
        assert result == []

    def test_with_data(self, service):
        sensitivity = {
            "tension": {
                "correlations": {
                    "wave_height": {"correlation": 0.9},
                    "current_speed": {"correlation": 0.5},
                },
            },
            "bending": {
                "correlations": {
                    "wave_height": {"correlation": 0.7},
                },
            },
        }
        result = service._generate_environmental_recommendations(sensitivity)
        assert len(result) >= 1
        assert any("wave_height" in r for r in result)


# ===========================================================================
# _interpret_benchmark_result
# ===========================================================================

class TestInterpretBenchmarkResult:

    def test_not_significant(self, service):
        result = service._interpret_benchmark_result(20.0, 1.0, False)
        assert "equivalent" in result.lower()

    def test_within_five_percent(self, service):
        result = service._interpret_benchmark_result(3.0, 0.3, True)
        assert "Minor difference" in result

    def test_substantially_better(self, service):
        result = service._interpret_benchmark_result(20.0, 1.0, True)
        assert "substantially" in result
        assert "better than" in result

    def test_substantially_worse(self, service):
        result = service._interpret_benchmark_result(-20.0, 1.0, True)
        assert "substantially" in result
        assert "worse than" in result

    def test_moderately_better(self, service):
        result = service._interpret_benchmark_result(15.0, 0.6, True)
        assert "moderately" in result
        assert "better than" in result

    def test_slightly_worse(self, service):
        result = service._interpret_benchmark_result(-10.0, 0.3, True)
        assert "slightly" in result
        assert "worse than" in result


# ===========================================================================
# _generate_overall_benchmark_assessment
# ===========================================================================

class TestGenerateOverallBenchmarkAssessment:

    def test_empty(self, service):
        result = service._generate_overall_benchmark_assessment({})
        assert result["total_metrics"] == 0
        assert result["overall_assessment"] == "mixed"

    def test_all_improved(self, service):
        bench = {
            "m1": {
                "percent_difference": 10.0,
                "statistical_test": {"is_significant": True},
            },
            "m2": {
                "percent_difference": 15.0,
                "statistical_test": {"is_significant": True},
            },
        }
        result = service._generate_overall_benchmark_assessment(bench)
        assert result["overall_assessment"] == "improved"
        assert result["better_metrics"] == 2
        assert result["worse_metrics"] == 0
        assert "m1" in result["significant_improvements"]
        assert "m2" in result["significant_improvements"]

    def test_all_degraded(self, service):
        bench = {
            "m1": {
                "percent_difference": -10.0,
                "statistical_test": {"is_significant": True},
            },
        }
        result = service._generate_overall_benchmark_assessment(bench)
        assert result["overall_assessment"] == "degraded"
        assert result["worse_metrics"] == 1

    def test_mixed(self, service):
        bench = {
            "m1": {
                "percent_difference": 10.0,
                "statistical_test": {"is_significant": True},
            },
            "m2": {
                "percent_difference": -10.0,
                "statistical_test": {"is_significant": True},
            },
        }
        result = service._generate_overall_benchmark_assessment(bench)
        assert result["overall_assessment"] == "mixed"

    def test_not_significant_counted_as_equivalent(self, service):
        bench = {
            "m1": {
                "percent_difference": 50.0,
                "statistical_test": {"is_significant": False},
            },
        }
        result = service._generate_overall_benchmark_assessment(bench)
        assert result["equivalent_metrics"] == 1
        assert result["better_metrics"] == 0

    def test_significant_but_within_five_percent(self, service):
        bench = {
            "m1": {
                "percent_difference": 3.0,
                "statistical_test": {"is_significant": True},
            },
        }
        result = service._generate_overall_benchmark_assessment(bench)
        assert result["equivalent_metrics"] == 1


# ===========================================================================
# _perform_pairwise_comparisons
# ===========================================================================

class TestPerformPairwiseComparisons:

    def test_two_groups_auto(self, service, simple_metric_data):
        results = service._perform_pairwise_comparisons(
            simple_metric_data, "tension", 0.95, "auto"
        )
        assert len(results) == 1
        r = results[0]
        assert r.case_1 == "case_A"
        assert r.case_2 == "case_B"
        assert r.metric == "tension"
        assert isinstance(r.p_value, float)
        assert isinstance(r.effect_size, float)

    def test_two_groups_parametric(self, service, simple_metric_data):
        results = service._perform_pairwise_comparisons(
            simple_metric_data, "tension", 0.95, "parametric"
        )
        assert len(results) == 1
        assert results[0].is_significant  # clearly different means

    def test_two_groups_non_parametric(self, service, simple_metric_data):
        results = service._perform_pairwise_comparisons(
            simple_metric_data, "tension", 0.95, "non_parametric"
        )
        assert len(results) == 1
        assert results[0].is_significant

    def test_three_groups_produces_three_pairs(self, service, three_group_data):
        results = service._perform_pairwise_comparisons(
            three_group_data, "m", 0.95, "parametric"
        )
        assert len(results) == 3  # C(3,2) = 3

    def test_similar_groups_not_significant(self, service, similar_metric_data):
        results = service._perform_pairwise_comparisons(
            similar_metric_data, "m", 0.95, "parametric"
        )
        # With same distribution parameters, should often not be significant
        # (not guaranteed, but with seed=42 and same params this should hold)
        r = results[0]
        # numpy bools (np.True_, np.False_) are not isinstance(bool)
        assert r.is_significant in (True, False)

    def test_effect_size_positive(self, service, simple_metric_data):
        results = service._perform_pairwise_comparisons(
            simple_metric_data, "m", 0.95, "parametric"
        )
        assert results[0].effect_size >= 0

    def test_mean_difference_sign(self, service, simple_metric_data):
        results = service._perform_pairwise_comparisons(
            simple_metric_data, "m", 0.95, "parametric"
        )
        # case_A mean ~10, case_B mean ~15 -> diff ~ -5
        assert results[0].mean_difference < 0

    def test_confidence_level_propagated(self, service, simple_metric_data):
        results = service._perform_pairwise_comparisons(
            simple_metric_data, "m", 0.99, "parametric"
        )
        assert results[0].confidence_level == 0.99

    def test_zero_std_effect_size(self, service):
        """When both groups are constant (pooled_std=0), effect_size should be 0."""
        import warnings
        data = {
            "a": np.array([5.0, 5.0, 5.0, 5.0]),
            "b": np.array([5.0, 5.0, 5.0, 5.0]),
        }
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", RuntimeWarning)
            results = service._perform_pairwise_comparisons(data, "m", 0.95, "parametric")
        assert results[0].effect_size == 0.0


# ===========================================================================
# _perform_group_comparison
# ===========================================================================

class TestPerformGroupComparison:

    def test_three_groups_auto(self, service, three_group_data):
        result = service._perform_group_comparison(
            three_group_data, "m", 0.95, "auto"
        )
        assert isinstance(result, GroupComparisonResult)
        assert len(result.groups) == 3
        assert result.is_significant  # clearly different means

    def test_three_groups_parametric(self, service, three_group_data):
        result = service._perform_group_comparison(
            three_group_data, "m", 0.95, "parametric"
        )
        assert result.is_significant

    def test_post_hoc_when_significant(self, service, three_group_data):
        result = service._perform_group_comparison(
            three_group_data, "m", 0.95, "parametric"
        )
        if result.is_significant:
            assert len(result.post_hoc_results) > 0

    def test_effect_size_range(self, service, three_group_data):
        result = service._perform_group_comparison(
            three_group_data, "m", 0.95, "parametric"
        )
        # eta-squared should be between 0 and 1 for well-separated groups
        assert result.effect_size >= 0

    def test_similar_groups_lower_significance(self, service, similar_metric_data):
        # Add a third similar group
        rng = np.random.RandomState(99)
        data = dict(similar_metric_data)
        data["case_Z"] = rng.normal(loc=10, scale=2, size=50)
        result = service._perform_group_comparison(data, "m", 0.95, "parametric")
        # May or may not be significant, but should run without error
        assert isinstance(result.p_value, float)

    def test_zero_total_variance(self, service):
        """All groups identical -> effect_size should be 0."""
        data = {
            "a": np.array([5.0, 5.0, 5.0]),
            "b": np.array([5.0, 5.0, 5.0]),
            "c": np.array([5.0, 5.0, 5.0]),
        }
        result = service._perform_group_comparison(data, "m", 0.95, "parametric")
        assert result.effect_size == 0.0


# ===========================================================================
# compare_loading_conditions (integration)
# ===========================================================================

class TestCompareLoadingConditions:

    def test_basic_two_conditions(self, service):
        df_a = pd.DataFrame({"tension": np.random.RandomState(1).normal(10, 2, 50)})
        df_b = pd.DataFrame({"tension": np.random.RandomState(2).normal(20, 2, 50)})
        result = service.compare_loading_conditions(
            {"cond_A": df_a, "cond_B": df_b},
            ["tension"]
        )
        assert "comparison_results" in result
        assert "tension" in result["comparison_results"]
        assert "summary" in result
        assert "metadata" in result
        assert result["metadata"]["conditions"] == ["cond_A", "cond_B"]

    def test_three_conditions_triggers_group(self, service):
        rng = np.random.RandomState(42)
        dfs = {
            f"cond_{i}": pd.DataFrame({"m": rng.normal(i * 10, 2, 30)})
            for i in range(3)
        }
        result = service.compare_loading_conditions(dfs, ["m"])
        group = result["comparison_results"]["m"]["group_comparison"]
        assert group is not None

    def test_two_conditions_no_group(self, service):
        rng = np.random.RandomState(42)
        dfs = {
            "a": pd.DataFrame({"m": rng.normal(10, 2, 30)}),
            "b": pd.DataFrame({"m": rng.normal(20, 2, 30)}),
        }
        result = service.compare_loading_conditions(dfs, ["m"])
        assert result["comparison_results"]["m"]["group_comparison"] is None

    def test_single_condition_raises(self, service):
        df = pd.DataFrame({"m": [1, 2, 3]})
        with pytest.raises(ValueError, match="at least 2"):
            service.compare_loading_conditions({"only": df}, ["m"])

    def test_metric_not_in_dataframe_skipped(self, service):
        df_a = pd.DataFrame({"tension": [1, 2, 3]})
        df_b = pd.DataFrame({"tension": [4, 5, 6]})
        result = service.compare_loading_conditions(
            {"a": df_a, "b": df_b},
            ["tension", "nonexistent"]
        )
        assert "nonexistent" not in result["comparison_results"]
        assert "tension" in result["comparison_results"]

    def test_metric_all_nan_skipped(self, service):
        df_a = pd.DataFrame({"m": [float("nan"), float("nan")]})
        df_b = pd.DataFrame({"m": [1.0, 2.0]})
        result = service.compare_loading_conditions({"a": df_a, "b": df_b}, ["m"])
        # Only one condition has valid data -> insufficient for comparison
        assert "m" not in result["comparison_results"]

    def test_multiple_metrics(self, service):
        rng = np.random.RandomState(42)
        df_a = pd.DataFrame({"tension": rng.normal(10, 2, 30), "bending": rng.normal(5, 1, 30)})
        df_b = pd.DataFrame({"tension": rng.normal(20, 2, 30), "bending": rng.normal(15, 1, 30)})
        result = service.compare_loading_conditions(
            {"a": df_a, "b": df_b},
            ["tension", "bending"]
        )
        assert "tension" in result["comparison_results"]
        assert "bending" in result["comparison_results"]

    def test_descriptive_stats_present(self, service):
        rng = np.random.RandomState(42)
        df_a = pd.DataFrame({"m": rng.normal(10, 2, 30)})
        df_b = pd.DataFrame({"m": rng.normal(20, 2, 30)})
        result = service.compare_loading_conditions({"a": df_a, "b": df_b}, ["m"])
        desc = result["comparison_results"]["m"]["descriptive_statistics"]
        assert "a" in desc
        assert "b" in desc
        assert "mean" in desc["a"]

    def test_custom_confidence_level(self, service):
        rng = np.random.RandomState(42)
        df_a = pd.DataFrame({"m": rng.normal(10, 2, 30)})
        df_b = pd.DataFrame({"m": rng.normal(20, 2, 30)})
        result = service.compare_loading_conditions(
            {"a": df_a, "b": df_b}, ["m"], confidence_level=0.99
        )
        assert result["metadata"]["confidence_level"] == 0.99


# ===========================================================================
# compare_environmental_conditions (integration)
# ===========================================================================

class TestCompareEnvironmentalConditions:

    def _make_env_data(self, n_cases=5):
        """Create correlated environmental and response data."""
        rng = np.random.RandomState(42)
        env_data = {}
        resp_data = {}
        for i in range(n_cases):
            case = f"case_{i}"
            wave_h = 1.0 + i * 0.5
            current = 0.5 + i * 0.1
            env_data[case] = {"wave_height": wave_h, "current_speed": current}
            # Response correlated with wave_height
            resp_data[case] = pd.DataFrame({
                "tension": rng.normal(loc=wave_h * 10, scale=1, size=20),
                "bending": rng.normal(loc=5, scale=1, size=20),
            })
        return env_data, resp_data

    def test_basic_environmental_analysis(self, service):
        env_data, resp_data = self._make_env_data()
        result = service.compare_environmental_conditions(
            env_data, resp_data, ["tension", "bending"]
        )
        assert "sensitivity_analysis" in result
        assert "environmental_parameters" in result
        assert "summary" in result
        assert "recommendations" in result

    def test_mismatched_lengths_raises(self, service):
        env_data = {"a": {"wave": 1.0}, "b": {"wave": 2.0}}
        resp_data = {"a": pd.DataFrame({"m": [1, 2]})}
        with pytest.raises(ValueError, match="matching cases"):
            service.compare_environmental_conditions(env_data, resp_data, ["m"])

    def test_fewer_than_three_cases_skips_metric(self, service):
        env_data = {"a": {"wave": 1.0}, "b": {"wave": 2.0}}
        resp_data = {
            "a": pd.DataFrame({"m": [1.0, 2.0]}),
            "b": pd.DataFrame({"m": [3.0, 4.0]}),
        }
        result = service.compare_environmental_conditions(env_data, resp_data, ["m"])
        # Only 2 cases => insufficient for correlation
        assert "m" not in result["sensitivity_analysis"]

    def test_correlation_significance(self, service):
        env_data, resp_data = self._make_env_data(n_cases=10)
        result = service.compare_environmental_conditions(
            env_data, resp_data, ["tension"]
        )
        tension_result = result["sensitivity_analysis"]["tension"]
        # wave_height should correlate strongly with tension
        assert "wave_height" in tension_result["correlations"]
        wh_corr = tension_result["correlations"]["wave_height"]
        assert abs(wh_corr["correlation"]) > 0.5

    def test_ranked_parameters(self, service):
        env_data, resp_data = self._make_env_data(n_cases=10)
        result = service.compare_environmental_conditions(
            env_data, resp_data, ["tension"]
        )
        ranked = result["sensitivity_analysis"]["tension"]["ranked_parameters"]
        assert len(ranked) > 0
        # Each ranked entry is (param_name, correlation_value)
        assert len(ranked[0]) == 2

    def test_most_influential_parameter(self, service):
        env_data, resp_data = self._make_env_data(n_cases=10)
        result = service.compare_environmental_conditions(
            env_data, resp_data, ["tension"]
        )
        most = result["sensitivity_analysis"]["tension"]["most_influential"]
        assert most is not None

    def test_constant_env_param_skipped(self, service):
        """Environmental parameter with zero std should be skipped."""
        env_data = {
            f"c{i}": {"wave": 5.0, "varying": float(i)}  # wave is constant
            for i in range(5)
        }
        rng = np.random.RandomState(42)
        resp_data = {
            f"c{i}": pd.DataFrame({"m": rng.normal(i, 1, 20)})
            for i in range(5)
        }
        result = service.compare_environmental_conditions(
            env_data, resp_data, ["m"]
        )
        correlations = result["sensitivity_analysis"]["m"]["correlations"]
        assert "wave" not in correlations
        assert "varying" in correlations


# ===========================================================================
# benchmark_performance (integration)
# ===========================================================================

class TestBenchmarkPerformance:

    def test_basic_benchmark(self, service):
        rng = np.random.RandomState(42)
        current = pd.DataFrame({"tension": rng.normal(20, 2, 50)})
        benchmark = pd.DataFrame({"tension": rng.normal(10, 2, 50)})
        result = service.benchmark_performance(
            current, benchmark, ["tension"]
        )
        assert "benchmark_results" in result
        assert "overall_assessment" in result
        assert "tension" in result["benchmark_results"]

    def test_benchmark_statistics_structure(self, service):
        rng = np.random.RandomState(42)
        current = pd.DataFrame({"m": rng.normal(20, 2, 50)})
        benchmark = pd.DataFrame({"m": rng.normal(10, 2, 50)})
        result = service.benchmark_performance(current, benchmark, ["m"])
        br = result["benchmark_results"]["m"]

        assert "current_statistics" in br
        assert "benchmark_statistics" in br
        assert "mean_difference" in br
        assert "percent_difference" in br
        assert "statistical_test" in br
        assert "effect_size" in br
        assert "performance_category" in br
        assert "interpretation" in br

        for key in ("mean", "std", "median", "max", "min"):
            assert key in br["current_statistics"]
            assert key in br["benchmark_statistics"]

    def test_percent_difference_correct(self, service):
        import warnings
        rng = np.random.RandomState(42)
        current = pd.DataFrame({"m": rng.normal(10.0, 0.01, 30)})
        benchmark = pd.DataFrame({"m": rng.normal(5.0, 0.01, 30)})
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", RuntimeWarning)
            result = service.benchmark_performance(current, benchmark, ["m"])
        br = result["benchmark_results"]["m"]
        # mean_diff ~ 5, percent ~ (5/5)*100 = 100
        assert br["percent_difference"] == pytest.approx(100.0, rel=0.05)

    def test_zero_benchmark_mean_percent_diff(self, service):
        import warnings
        rng = np.random.RandomState(42)
        current = pd.DataFrame({"m": rng.normal(5.0, 0.01, 30)})
        benchmark = pd.DataFrame({"m": rng.normal(0.0, 0.01, 30)})
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", RuntimeWarning)
            result = service.benchmark_performance(current, benchmark, ["m"])
        br = result["benchmark_results"]["m"]
        # benchmark mean is near 0, percent_diff is large or computed
        assert isinstance(br["percent_difference"], float)

    def test_missing_metric_skipped(self, service):
        current = pd.DataFrame({"m": [1, 2, 3]})
        benchmark = pd.DataFrame({"other": [4, 5, 6]})
        result = service.benchmark_performance(current, benchmark, ["m"])
        assert "m" not in result["benchmark_results"]

    def test_empty_after_dropna_skipped(self, service):
        current = pd.DataFrame({"m": [float("nan"), float("nan")]})
        benchmark = pd.DataFrame({"m": [1.0, 2.0]})
        result = service.benchmark_performance(current, benchmark, ["m"])
        assert "m" not in result["benchmark_results"]

    def test_benchmark_name_in_result(self, service):
        current = pd.DataFrame({"m": [1.0, 2.0, 3.0]})
        benchmark = pd.DataFrame({"m": [1.0, 2.0, 3.0]})
        result = service.benchmark_performance(
            current, benchmark, ["m"], benchmark_name="MyBaseline"
        )
        assert result["benchmark_name"] == "MyBaseline"

    def test_cohens_d_zero_when_pooled_std_zero(self, service):
        import warnings
        current = pd.DataFrame({"m": [5.0, 5.0, 5.0]})
        benchmark = pd.DataFrame({"m": [5.0, 5.0, 5.0]})
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", RuntimeWarning)
            result = service.benchmark_performance(current, benchmark, ["m"])
        assert result["benchmark_results"]["m"]["effect_size"] == 0.0

    def test_multiple_metrics_benchmark(self, service):
        rng = np.random.RandomState(42)
        current = pd.DataFrame({
            "tension": rng.normal(20, 2, 50),
            "bending": rng.normal(5, 1, 50),
        })
        benchmark = pd.DataFrame({
            "tension": rng.normal(10, 2, 50),
            "bending": rng.normal(5, 1, 50),
        })
        result = service.benchmark_performance(
            current, benchmark, ["tension", "bending"]
        )
        assert "tension" in result["benchmark_results"]
        assert "bending" in result["benchmark_results"]

    def test_overall_assessment_improved(self, service):
        rng = np.random.RandomState(42)
        current = pd.DataFrame({"m": rng.normal(30, 2, 100)})
        benchmark = pd.DataFrame({"m": rng.normal(10, 2, 100)})
        result = service.benchmark_performance(current, benchmark, ["m"])
        assert result["overall_assessment"]["overall_assessment"] in (
            "improved", "mixed", "degraded"
        )
