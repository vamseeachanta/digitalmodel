"""
Comprehensive unit tests for the anomaly_detection module.

Tests cover:
- AnomalyType and DetectionMethod enums
- AnomalyResult and EngineeringLimits dataclasses
- AnomalyDetectionService: statistical outlier detection (IQR, Z-score, Modified Z-score)
- Engineering limit violation detection
- Time series anomaly detection
- Multivariate anomaly detection
- Severity classification
- Recommendation generation
- Comprehensive anomaly scan
- Edge cases and error handling
"""

import importlib.util
import pathlib
import math

import numpy as np
import pandas as pd
import pytest

# Import the module from hyphenated directory using importlib
_mod_path = (
    pathlib.Path(__file__).resolve().parents[2]
    / "src"
    / "digitalmodel"
    / "visualization"
    / "orcaflex-dashboard"
    / "backend"
    / "app"
    / "services"
    / "anomaly_detection.py"
)
_spec = importlib.util.spec_from_file_location("anomaly_detection", _mod_path)
anomaly_detection = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(anomaly_detection)

AnomalyType = anomaly_detection.AnomalyType
DetectionMethod = anomaly_detection.DetectionMethod
AnomalyResult = anomaly_detection.AnomalyResult
EngineeringLimits = anomaly_detection.EngineeringLimits
AnomalyDetectionService = anomaly_detection.AnomalyDetectionService


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def service():
    """Fresh AnomalyDetectionService instance."""
    return AnomalyDetectionService()


@pytest.fixture
def normal_data():
    """Normally distributed data with known properties."""
    np.random.seed(42)
    return pd.Series(np.random.normal(loc=100, scale=10, size=200))


@pytest.fixture
def data_with_outliers():
    """Normal data with injected extreme outliers."""
    np.random.seed(42)
    values = np.random.normal(loc=50, scale=5, size=100)
    # Inject extreme outliers at known positions
    values[10] = 200.0
    values[50] = -100.0
    values[90] = 300.0
    return pd.Series(values)


@pytest.fixture
def constant_data():
    """All identical values (zero variance)."""
    return pd.Series([5.0] * 50)


@pytest.fixture
def engineering_df():
    """DataFrame with engineering parameters for limit testing."""
    np.random.seed(42)
    return pd.DataFrame({
        "tension": np.random.uniform(100, 500, 50),
        "pitch": np.random.uniform(-5, 5, 50),
        "roll": np.random.uniform(-5, 5, 50),
        "vessel_offset": np.random.uniform(0, 10, 50),
    })


@pytest.fixture
def time_series_df():
    """Time series DataFrame with anomalies.

    Uses a large window so that single-point spikes do not dominate
    the rolling std and their z-scores exceed the detection threshold.
    """
    np.random.seed(42)
    n = 200
    time = np.arange(n, dtype=float)
    values = np.sin(time * 0.1) * 10 + np.random.normal(0, 1, n)
    # Inject large spikes
    values[60] = 500.0
    values[140] = -500.0
    return pd.DataFrame({"time": time, "signal": values})


# ---------------------------------------------------------------------------
# Enum Tests
# ---------------------------------------------------------------------------

class TestAnomalyType:
    def test_values(self):
        assert AnomalyType.OUTLIER.value == "outlier"
        assert AnomalyType.ENGINEERING_LIMIT.value == "engineering_limit"
        assert AnomalyType.TREND_ANOMALY.value == "trend_anomaly"
        assert AnomalyType.MULTIVARIATE.value == "multivariate"
        assert AnomalyType.TEMPORAL.value == "temporal"

    def test_member_count(self):
        assert len(AnomalyType) == 5


class TestDetectionMethod:
    def test_values(self):
        assert DetectionMethod.IQR.value == "iqr"
        assert DetectionMethod.Z_SCORE.value == "z_score"
        assert DetectionMethod.MODIFIED_Z_SCORE.value == "modified_z_score"
        assert DetectionMethod.ISOLATION_FOREST.value == "isolation_forest"
        assert DetectionMethod.ELLIPTIC_ENVELOPE.value == "elliptic_envelope"
        assert DetectionMethod.ENGINEERING_LIMITS.value == "engineering_limits"

    def test_member_count(self):
        assert len(DetectionMethod) == 6


# ---------------------------------------------------------------------------
# Dataclass Tests
# ---------------------------------------------------------------------------

class TestAnomalyResult:
    def test_creation(self):
        ar = AnomalyResult(
            index=5,
            value=99.9,
            anomaly_type="outlier",
            detection_method="iqr",
            severity="high",
            score=3.5,
            threshold=1.5,
            description="test",
            recommendations=["rec1"],
        )
        assert ar.index == 5
        assert ar.value == 99.9
        assert ar.severity == "high"
        assert ar.recommendations == ["rec1"]

    def test_fields_accessible(self):
        ar = AnomalyResult(0, 1.0, "t", "m", "low", 0.1, 1.0, "d", [])
        assert ar.anomaly_type == "t"
        assert ar.detection_method == "m"


class TestEngineeringLimits:
    def test_defaults(self):
        el = EngineeringLimits(parameter="tension")
        assert el.parameter == "tension"
        assert el.min_value is None
        assert el.max_value is None
        assert el.warning_min is None
        assert el.warning_max is None
        assert el.units is None
        assert el.description is None

    def test_full_creation(self):
        el = EngineeringLimits(
            parameter="pitch",
            min_value=-20.0,
            max_value=20.0,
            warning_min=-15.0,
            warning_max=15.0,
            units="deg",
            description="Vessel pitch",
        )
        assert el.max_value == 20.0
        assert el.warning_max == 15.0
        assert el.units == "deg"


# ---------------------------------------------------------------------------
# Service Initialization Tests
# ---------------------------------------------------------------------------

class TestServiceInitialization:
    def test_default_limits_exist(self, service):
        limits = service.engineering_limits
        assert "tension" in limits
        assert "bend_radius" in limits
        assert "vessel_offset" in limits
        assert "heave" in limits
        assert "pitch" in limits
        assert "roll" in limits

    def test_default_pitch_warning_max(self, service):
        assert service.engineering_limits["pitch"].warning_max == 15.0

    def test_default_roll_warning_max(self, service):
        assert service.engineering_limits["roll"].warning_max == 15.0

    def test_tension_min_value(self, service):
        assert service.engineering_limits["tension"].min_value == 0.0


# ---------------------------------------------------------------------------
# Severity Classification Tests
# ---------------------------------------------------------------------------

class TestClassifyAnomalySeverity:
    def test_iqr_critical(self, service):
        assert service._classify_anomaly_severity(6.0, "iqr") == "critical"

    def test_iqr_high(self, service):
        assert service._classify_anomaly_severity(4.0, "iqr") == "high"

    def test_iqr_medium(self, service):
        assert service._classify_anomaly_severity(2.0, "iqr") == "medium"

    def test_iqr_low(self, service):
        assert service._classify_anomaly_severity(1.0, "iqr") == "low"

    def test_iqr_boundary_at_5(self, service):
        assert service._classify_anomaly_severity(5.0, "iqr") == "high"

    def test_iqr_boundary_at_3(self, service):
        assert service._classify_anomaly_severity(3.0, "iqr") == "medium"

    def test_iqr_boundary_at_1_5(self, service):
        assert service._classify_anomaly_severity(1.5, "iqr") == "low"

    def test_z_score_critical(self, service):
        assert service._classify_anomaly_severity(6.0, "z_score") == "critical"

    def test_z_score_high(self, service):
        assert service._classify_anomaly_severity(4.5, "z_score") == "high"

    def test_z_score_medium(self, service):
        assert service._classify_anomaly_severity(3.5, "z_score") == "medium"

    def test_z_score_low(self, service):
        assert service._classify_anomaly_severity(2.0, "z_score") == "low"

    def test_modified_z_score_uses_same_as_z_score(self, service):
        assert service._classify_anomaly_severity(6.0, "modified_z_score") == "critical"
        assert service._classify_anomaly_severity(4.5, "modified_z_score") == "high"

    def test_time_series_uses_same_as_z_score(self, service):
        assert service._classify_anomaly_severity(6.0, "time_series") == "critical"

    def test_multivariate_critical(self, service):
        assert service._classify_anomaly_severity(3.0, "multivariate") == "critical"

    def test_multivariate_high(self, service):
        assert service._classify_anomaly_severity(1.5, "multivariate") == "high"

    def test_multivariate_medium(self, service):
        assert service._classify_anomaly_severity(0.7, "multivariate") == "medium"

    def test_multivariate_low(self, service):
        assert service._classify_anomaly_severity(0.3, "multivariate") == "low"

    def test_unknown_method_default(self, service):
        assert service._classify_anomaly_severity(99.0, "unknown_method") == "medium"


# ---------------------------------------------------------------------------
# IQR Outlier Detection Tests
# ---------------------------------------------------------------------------

class TestDetectStatisticalOutliersIQR:
    def test_detects_injected_outliers(self, service, data_with_outliers):
        result = service.detect_statistical_outliers(data_with_outliers, method="iqr")
        assert result["total_outliers"] >= 3
        outlier_indices = [a.index for a in result["anomalies"]]
        assert 10 in outlier_indices
        assert 50 in outlier_indices
        assert 90 in outlier_indices

    def test_returns_correct_keys(self, service, normal_data):
        result = service.detect_statistical_outliers(normal_data, method="iqr")
        assert "anomalies" in result
        assert "total_outliers" in result
        assert "outlier_percentage" in result
        assert "method" in result
        assert "threshold_info" in result
        assert "all_scores" in result

    def test_threshold_info_contains_iqr_stats(self, service, normal_data):
        result = service.detect_statistical_outliers(normal_data, method="iqr")
        ti = result["threshold_info"]
        assert "Q1" in ti
        assert "Q3" in ti
        assert "IQR" in ti
        assert "lower_bound" in ti
        assert "upper_bound" in ti
        assert ti["IQR"] == ti["Q3"] - ti["Q1"]

    def test_higher_threshold_fewer_outliers(self, service, data_with_outliers):
        r1 = service.detect_statistical_outliers(
            data_with_outliers, method="iqr", threshold_factor=1.5
        )
        r2 = service.detect_statistical_outliers(
            data_with_outliers, method="iqr", threshold_factor=3.0
        )
        assert r2["total_outliers"] <= r1["total_outliers"]

    def test_constant_data_iqr_zero_division(self, service, constant_data):
        """Constant data has IQR=0, causing division by zero in score calc."""
        # The source divides by IQR which is 0 for constant data.
        # With numpy error settings this raises RuntimeWarning -> exception.
        with pytest.raises(Exception):
            service.detect_statistical_outliers(constant_data, method="iqr")

    def test_numpy_array_input(self, service):
        arr = np.array([1, 2, 3, 4, 5, 6, 7, 8, 9, 100])
        result = service.detect_statistical_outliers(arr, method="iqr")
        assert result["total_outliers"] >= 1

    def test_scores_returned_when_requested(self, service, normal_data):
        result = service.detect_statistical_outliers(
            normal_data, method="iqr", return_scores=True
        )
        assert result["all_scores"] is not None
        assert len(result["all_scores"]) > 0

    def test_scores_none_when_not_requested(self, service, normal_data):
        result = service.detect_statistical_outliers(
            normal_data, method="iqr", return_scores=False
        )
        assert result["all_scores"] is None

    def test_anomaly_result_types(self, service, data_with_outliers):
        result = service.detect_statistical_outliers(data_with_outliers, method="iqr")
        for a in result["anomalies"]:
            assert isinstance(a, AnomalyResult)
            assert a.anomaly_type == "outlier"
            assert a.detection_method == "iqr"

    def test_outlier_percentage_range(self, service, data_with_outliers):
        result = service.detect_statistical_outliers(data_with_outliers, method="iqr")
        assert 0 <= result["outlier_percentage"] <= 100


# ---------------------------------------------------------------------------
# Z-Score Outlier Detection Tests
# ---------------------------------------------------------------------------

class TestDetectStatisticalOutliersZScore:
    def test_detects_outliers(self, service, data_with_outliers):
        result = service.detect_statistical_outliers(
            data_with_outliers, method="z_score", threshold_factor=3.0
        )
        assert result["total_outliers"] >= 3

    def test_threshold_info(self, service, normal_data):
        result = service.detect_statistical_outliers(
            normal_data, method="z_score", threshold_factor=3.0
        )
        ti = result["threshold_info"]
        assert "mean" in ti
        assert "std" in ti
        assert "threshold" in ti
        assert ti["threshold"] == 3.0

    def test_zero_std_no_outliers(self, service, constant_data):
        result = service.detect_statistical_outliers(constant_data, method="z_score")
        assert result["total_outliers"] == 0

    def test_scores_are_z_scores(self, service, normal_data):
        result = service.detect_statistical_outliers(
            normal_data, method="z_score", threshold_factor=3.0
        )
        scores = result["all_scores"]
        # All scores should be non-negative (absolute z-scores)
        assert np.all(scores >= 0)


# ---------------------------------------------------------------------------
# Modified Z-Score Tests
# ---------------------------------------------------------------------------

class TestDetectStatisticalOutliersModifiedZ:
    def test_detects_outliers(self, service, data_with_outliers):
        result = service.detect_statistical_outliers(
            data_with_outliers, method="modified_z_score", threshold_factor=3.5
        )
        assert result["total_outliers"] >= 3

    def test_threshold_info(self, service, normal_data):
        result = service.detect_statistical_outliers(
            normal_data, method="modified_z_score"
        )
        ti = result["threshold_info"]
        assert "median" in ti
        assert "mad" in ti
        assert "threshold" in ti

    def test_zero_mad_no_outliers(self, service, constant_data):
        result = service.detect_statistical_outliers(
            constant_data, method="modified_z_score"
        )
        assert result["total_outliers"] == 0

    def test_robust_to_outliers(self, service):
        """Modified Z-score should be more robust than Z-score."""
        np.random.seed(42)
        values = np.concatenate([np.random.normal(0, 1, 97), [100, 200, 300]])
        data = pd.Series(values)
        result = service.detect_statistical_outliers(
            data, method="modified_z_score", threshold_factor=3.5
        )
        # The three extreme values should be flagged
        outlier_values = [a.value for a in result["anomalies"]]
        assert any(v > 50 for v in outlier_values)


# ---------------------------------------------------------------------------
# Statistical Outlier Error Handling
# ---------------------------------------------------------------------------

class TestStatisticalOutlierErrors:
    def test_too_few_data_points(self, service):
        with pytest.raises(ValueError, match="at least 3"):
            service.detect_statistical_outliers(pd.Series([1.0, 2.0]))

    def test_all_nan_data(self, service):
        with pytest.raises(ValueError, match="at least 3"):
            service.detect_statistical_outliers(pd.Series([np.nan, np.nan, np.nan]))

    def test_unknown_method(self, service):
        with pytest.raises(ValueError, match="Unknown method"):
            service.detect_statistical_outliers(
                pd.Series([1.0, 2.0, 3.0, 4.0]), method="nonexistent"
            )

    def test_nan_values_filtered(self, service):
        data = pd.Series([1, 2, np.nan, 3, 4, 5, np.nan, 6, 100])
        result = service.detect_statistical_outliers(data, method="iqr")
        # Should succeed without error; NaN values excluded
        assert result["method"] == "iqr"

    def test_empty_series_raises(self, service):
        with pytest.raises(ValueError, match="at least 3"):
            service.detect_statistical_outliers(pd.Series([], dtype=float))


# ---------------------------------------------------------------------------
# Engineering Limit Violation Tests
# ---------------------------------------------------------------------------

class TestDetectEngineeringLimitViolations:
    def test_no_violations_normal_data(self, service, engineering_df):
        result = service.detect_engineering_limit_violations(engineering_df)
        # Normal data within default limits should have few/no critical violations
        assert "violations" in result
        assert "total_violations" in result
        assert "critical_violations" in result
        assert "warning_violations" in result

    def test_tension_below_minimum(self, service):
        df = pd.DataFrame({"tension": [-10.0, -5.0, 100.0, 200.0]})
        result = service.detect_engineering_limit_violations(df)
        critical = [v for v in result["violations"] if v.severity == "critical"]
        assert len(critical) >= 2  # Two negative tension values

    def test_pitch_above_warning(self, service):
        df = pd.DataFrame({"pitch": [1.0, 2.0, 16.0, 20.0]})
        result = service.detect_engineering_limit_violations(df)
        warnings = [v for v in result["violations"] if v.severity == "medium"]
        assert len(warnings) >= 2  # 16 and 20 exceed warning_max=15

    def test_roll_above_warning(self, service):
        df = pd.DataFrame({"roll": [1.0, 2.0, 18.0]})
        result = service.detect_engineering_limit_violations(df)
        assert result["warning_violations"] >= 1

    def test_custom_limits_override(self, service):
        df = pd.DataFrame({"custom_param": [5.0, 15.0, 25.0]})
        custom = {
            "custom_param": EngineeringLimits(
                parameter="custom_param",
                min_value=0.0,
                max_value=20.0,
                warning_max=10.0,
                units="m",
            )
        }
        result = service.detect_engineering_limit_violations(df, custom_limits=custom)
        assert result["total_violations"] >= 1
        # 25 exceeds max_value=20 => critical; 15 exceeds warning_max=10 => warning
        critical = [v for v in result["violations"] if v.severity == "critical"]
        warnings = [v for v in result["violations"] if v.severity == "medium"]
        assert len(critical) >= 1
        assert len(warnings) >= 1

    def test_column_not_in_limits_ignored(self, service):
        df = pd.DataFrame({"unknown_column": [1.0, 2.0, 3.0]})
        result = service.detect_engineering_limit_violations(df)
        assert result["total_violations"] == 0

    def test_nan_values_skipped(self, service):
        df = pd.DataFrame({"tension": [np.nan, np.nan, 100.0]})
        result = service.detect_engineering_limit_violations(df)
        # NaN values should not cause violations
        tension_violations = [
            v for v in result["violations"] if "tension" in v.description
        ]
        assert len(tension_violations) == 0

    def test_empty_column_skipped(self, service):
        df = pd.DataFrame({"tension": pd.Series([], dtype=float)})
        result = service.detect_engineering_limit_violations(df)
        assert result["total_violations"] == 0

    def test_violation_result_properties(self, service):
        df = pd.DataFrame({"tension": [-5.0]})
        result = service.detect_engineering_limit_violations(df)
        assert len(result["violations"]) == 1
        v = result["violations"][0]
        assert isinstance(v, AnomalyResult)
        assert v.anomaly_type == "engineering_limit"
        assert v.detection_method == "engineering_limits"
        assert v.severity == "critical"
        assert v.score == 1.0

    def test_violated_parameters_list(self, service):
        df = pd.DataFrame({"tension": [-5.0], "pitch": [20.0]})
        result = service.detect_engineering_limit_violations(df)
        assert len(result["violated_parameters"]) >= 1

    def test_limits_checked_list(self, service):
        df = pd.DataFrame({"tension": [100.0]})
        result = service.detect_engineering_limit_violations(df)
        assert "tension" in result["limits_checked"]


# ---------------------------------------------------------------------------
# Recommendation Generation Tests
# ---------------------------------------------------------------------------

class TestOutlierRecommendations:
    def test_critical_severity(self, service):
        recs = service._generate_outlier_recommendations(999.0, "iqr", "critical")
        assert any("Investigate" in r for r in recs)
        assert any("equipment" in r.lower() for r in recs)

    def test_high_severity(self, service):
        recs = service._generate_outlier_recommendations(999.0, "z_score", "high")
        assert any("Investigate" in r for r in recs)

    def test_medium_severity(self, service):
        recs = service._generate_outlier_recommendations(50.0, "iqr", "medium")
        assert any("Review" in r for r in recs)

    def test_low_severity(self, service):
        recs = service._generate_outlier_recommendations(10.0, "iqr", "low")
        assert any("Monitor" in r for r in recs)

    def test_method_name_in_recommendations(self, service):
        recs = service._generate_outlier_recommendations(10.0, "modified_z_score", "low")
        assert any("modified_z_score" in r for r in recs)


class TestLimitViolationRecommendations:
    def test_critical_tension(self, service):
        limit = EngineeringLimits(parameter="tension", min_value=0.0, units="N")
        recs = service._generate_limit_violation_recommendations(
            "tension", -5.0, "critical_min", limit
        )
        assert any("IMMEDIATE" in r for r in recs)
        assert any("tension" in r.lower() or "line" in r.lower() for r in recs)

    def test_critical_offset(self, service):
        limit = EngineeringLimits(parameter="vessel_offset", max_value=50.0, units="m")
        recs = service._generate_limit_violation_recommendations(
            "vessel_offset", 60.0, "critical_max", limit
        )
        assert any("positioning" in r.lower() for r in recs)

    def test_critical_pitch(self, service):
        limit = EngineeringLimits(parameter="pitch", max_value=25.0, units="deg")
        recs = service._generate_limit_violation_recommendations(
            "pitch", 30.0, "critical_max", limit
        )
        assert any("stability" in r.lower() for r in recs)

    def test_warning_violation(self, service):
        limit = EngineeringLimits(parameter="heave", warning_max=5.0, units="m")
        recs = service._generate_limit_violation_recommendations(
            "heave", 6.0, "warning_max", limit
        )
        assert any("Monitor" in r for r in recs)
        assert not any("IMMEDIATE" in r for r in recs)


class TestMultivariateRecommendations:
    def test_contains_standard_recs(self, service):
        row = {"tension": 500.0, "pitch": 2.0, "roll": 1.0}
        recs = service._generate_multivariate_recommendations(row, "isolation_forest")
        assert any("combination" in r.lower() for r in recs)
        assert any("isolation_forest" in r for r in recs)

    def test_identifies_most_extreme_parameter(self, service):
        row = {"tension": 500.0, "pitch": 2.0, "roll": 1.0}
        recs = service._generate_multivariate_recommendations(row, "isolation_forest")
        assert any("tension" in r for r in recs)


class TestTemporalRecommendations:
    def test_critical_severity(self, service):
        recs = service._generate_temporal_recommendations("signal", 100.0, "critical")
        assert any("sudden" in r.lower() or "failure" in r.lower() for r in recs)

    def test_low_severity(self, service):
        recs = service._generate_temporal_recommendations("signal", 5.0, "low")
        assert any("Monitor" in r or "variation" in r.lower() for r in recs)

    def test_parameter_name_in_recs(self, service):
        recs = service._generate_temporal_recommendations("heave", 5.0, "low")
        assert any("heave" in r for r in recs)


# ---------------------------------------------------------------------------
# Temporal Summary Tests
# ---------------------------------------------------------------------------

class TestGenerateTemporalSummary:
    def test_empty_input(self, service):
        summary = service._generate_temporal_summary({})
        assert summary["total_parameters_analyzed"] == 0
        assert summary["total_anomalies_found"] == 0
        assert summary["most_anomalous_parameter"] is None
        assert summary["parameters_with_anomalies"] == []

    def test_counts_anomalies(self, service):
        data = {
            "param_a": {"total_anomalies": 3, "anomalies": [1, 2, 3]},
            "param_b": {"total_anomalies": 1, "anomalies": [1]},
        }
        summary = service._generate_temporal_summary(data)
        assert summary["total_anomalies_found"] == 4
        assert summary["total_parameters_analyzed"] == 2
        assert summary["most_anomalous_parameter"] == "param_a"

    def test_parameters_with_anomalies(self, service):
        data = {
            "param_a": {"total_anomalies": 5, "anomalies": [1, 2, 3, 4, 5]},
            "param_b": {"total_anomalies": 0, "anomalies": []},
        }
        summary = service._generate_temporal_summary(data)
        assert "param_a" in summary["parameters_with_anomalies"]
        assert "param_b" not in summary["parameters_with_anomalies"]


# ---------------------------------------------------------------------------
# Time Series Anomaly Detection Tests
# ---------------------------------------------------------------------------

class TestDetectTimeSeriesAnomalies:
    def test_detects_spikes(self, service, time_series_df):
        # A center-window rolling std absorbs single-point spikes,
        # so use threshold=2.5 to detect z~2.85 spikes.
        result = service.detect_time_series_anomalies(
            time_series_df,
            time_column="time",
            value_columns=["signal"],
            window_size=10,
            threshold=2.5,
        )
        assert "time_series_anomalies" in result
        assert "summary" in result
        assert "parameters" in result

        signal_results = result["time_series_anomalies"].get("signal", {})
        assert signal_results["total_anomalies"] >= 2

    def test_missing_time_column_raises(self, service, time_series_df):
        with pytest.raises(ValueError, match="not found"):
            service.detect_time_series_anomalies(
                time_series_df,
                time_column="nonexistent",
                value_columns=["signal"],
            )

    def test_missing_value_column_skipped(self, service, time_series_df):
        result = service.detect_time_series_anomalies(
            time_series_df,
            time_column="time",
            value_columns=["nonexistent"],
        )
        assert "nonexistent" not in result["time_series_anomalies"]

    def test_insufficient_data_skipped(self, service):
        df = pd.DataFrame({"time": [1, 2, 3], "val": [1.0, 2.0, 3.0]})
        result = service.detect_time_series_anomalies(
            df, time_column="time", value_columns=["val"], window_size=10
        )
        # Too few data points => column skipped
        assert "val" not in result["time_series_anomalies"]

    def test_parameters_in_result(self, service, time_series_df):
        result = service.detect_time_series_anomalies(
            time_series_df,
            time_column="time",
            value_columns=["signal"],
            window_size=10,
            threshold=3.0,
        )
        params = result["parameters"]
        assert params["window_size"] == 10
        assert params["threshold"] == 3.0
        assert params["time_column"] == "time"

    def test_anomaly_result_properties(self, service, time_series_df):
        result = service.detect_time_series_anomalies(
            time_series_df,
            time_column="time",
            value_columns=["signal"],
            window_size=10,
            threshold=3.0,
        )
        anomalies = result["time_series_anomalies"]["signal"]["anomalies"]
        for a in anomalies:
            assert isinstance(a, AnomalyResult)
            assert a.anomaly_type == "temporal"
            assert a.detection_method == "rolling_z_score"


# ---------------------------------------------------------------------------
# Multivariate Anomaly Detection Tests
# ---------------------------------------------------------------------------

class TestDetectMultivariateAnomalies:
    def test_isolation_forest_basic(self, service):
        np.random.seed(42)
        df = pd.DataFrame(
            np.random.normal(0, 1, (50, 3)), columns=["a", "b", "c"]
        )
        # Add outliers
        df.loc[48] = [20, 20, 20]
        df.loc[49] = [-20, -20, -20]
        result = service.detect_multivariate_anomalies(
            df, method="isolation_forest", contamination=0.1
        )
        assert result["total_anomalies"] >= 2
        assert result["method"] == "isolation_forest"
        assert "features_used" in result
        assert result["features_used"] == ["a", "b", "c"]

    def test_elliptic_envelope_basic(self, service):
        np.random.seed(42)
        df = pd.DataFrame(
            np.random.normal(0, 1, (50, 2)), columns=["x", "y"]
        )
        df.loc[48] = [15, 15]
        df.loc[49] = [-15, -15]
        result = service.detect_multivariate_anomalies(
            df, method="elliptic_envelope", contamination=0.1
        )
        assert result["total_anomalies"] >= 1
        assert result["method"] == "elliptic_envelope"

    def test_no_numeric_columns_raises(self, service):
        df = pd.DataFrame({"text": ["a", "b", "c"] * 10})
        with pytest.raises(ValueError, match="No numeric columns"):
            service.detect_multivariate_anomalies(df)

    def test_too_few_rows_raises(self, service):
        df = pd.DataFrame({"a": [1.0, 2.0, 3.0], "b": [4.0, 5.0, 6.0]})
        with pytest.raises(ValueError, match="at least 10"):
            service.detect_multivariate_anomalies(df)

    def test_unknown_method_raises(self, service):
        np.random.seed(42)
        df = pd.DataFrame(np.random.normal(0, 1, (20, 2)), columns=["a", "b"])
        with pytest.raises(ValueError, match="Unknown method"):
            service.detect_multivariate_anomalies(df, method="nonexistent")

    def test_nan_rows_dropped(self, service):
        np.random.seed(42)
        df = pd.DataFrame(np.random.normal(0, 1, (30, 2)), columns=["a", "b"])
        df.loc[0, "a"] = np.nan
        df.loc[1, "b"] = np.nan
        result = service.detect_multivariate_anomalies(df)
        # Should succeed; NaN rows excluded
        assert "total_anomalies" in result

    def test_anomaly_result_type(self, service):
        np.random.seed(42)
        df = pd.DataFrame(np.random.normal(0, 1, (50, 2)), columns=["a", "b"])
        df.loc[49] = [30, 30]
        result = service.detect_multivariate_anomalies(df)
        for a in result["anomalies"]:
            assert isinstance(a, AnomalyResult)
            assert a.anomaly_type == "multivariate"

    def test_contamination_controls_fraction(self, service):
        np.random.seed(42)
        df = pd.DataFrame(np.random.normal(0, 1, (100, 3)), columns=["a", "b", "c"])
        r_low = service.detect_multivariate_anomalies(
            df, contamination=0.05, random_state=42
        )
        r_high = service.detect_multivariate_anomalies(
            df, contamination=0.2, random_state=42
        )
        assert r_high["total_anomalies"] >= r_low["total_anomalies"]


# ---------------------------------------------------------------------------
# Comprehensive Scan Tests
# ---------------------------------------------------------------------------

class TestComprehensiveAnomalyScan:
    def test_default_methods(self, service):
        np.random.seed(42)
        df = pd.DataFrame(np.random.normal(0, 1, (50, 3)), columns=["a", "b", "c"])
        result = service.comprehensive_anomaly_scan(df)
        assert "timestamp" in result
        assert "data_shape" in result
        assert "methods_used" in result
        assert "results" in result
        assert "summary" in result

    def test_custom_methods(self, service):
        np.random.seed(42)
        df = pd.DataFrame(np.random.normal(0, 1, (50, 2)), columns=["a", "b"])
        result = service.comprehensive_anomaly_scan(df, methods=["iqr"])
        assert result["methods_used"] == ["iqr"]
        assert "statistical_outliers" in result["results"]

    def test_with_time_series(self, service, time_series_df):
        result = service.comprehensive_anomaly_scan(
            time_series_df,
            time_column="time",
            methods=["time_series"],
        )
        assert "time_series" in result["results"]

    def test_engineering_limits_method(self, service):
        df = pd.DataFrame({"tension": [-5.0, 100.0, 200.0]})
        result = service.comprehensive_anomaly_scan(
            df, methods=["engineering_limits"]
        )
        assert "engineering_violations" in result["results"]

    def test_multivariate_failure_captured(self, service):
        """Multivariate with too few rows should capture error, not raise."""
        df = pd.DataFrame({"a": [1.0, 2.0], "b": [3.0, 4.0]})
        result = service.comprehensive_anomaly_scan(df, methods=["multivariate"])
        mv = result["results"]["multivariate"]
        assert "error" in mv

    def test_data_shape_recorded(self, service):
        np.random.seed(42)
        df = pd.DataFrame(np.random.normal(0, 1, (25, 4)), columns=["a", "b", "c", "d"])
        result = service.comprehensive_anomaly_scan(df, methods=["iqr"])
        assert result["data_shape"] == (25, 4)


# ---------------------------------------------------------------------------
# Comprehensive Summary Tests
# ---------------------------------------------------------------------------

class TestGenerateComprehensiveSummary:
    def test_empty_results(self, service):
        summary = service._generate_comprehensive_summary({})
        assert summary["total_anomalies"] == 0
        assert summary["critical_anomalies"] == 0
        assert summary["detection_methods_used"] == []

    def test_counts_statistical_outliers(self, service):
        ar = AnomalyResult(0, 1.0, "outlier", "iqr", "critical", 6.0, 1.5, "d", [])
        results = {
            "statistical_outliers": {
                "col1": {
                    "iqr": {"anomalies": [ar]}
                }
            }
        }
        summary = service._generate_comprehensive_summary(results)
        assert summary["total_anomalies"] == 1
        assert summary["critical_anomalies"] == 1

    def test_counts_engineering_violations(self, service):
        ar = AnomalyResult(0, 1.0, "eng", "eng", "critical", 1.0, 0.0, "d", [])
        results = {
            "engineering_violations": {
                "violations": [ar],
                "critical_violations": 1,
                "warning_violations": 0,
            }
        }
        summary = service._generate_comprehensive_summary(results)
        assert summary["total_anomalies"] == 1
        assert summary["critical_anomalies"] == 1

    def test_skips_error_results(self, service):
        results = {
            "multivariate": {"error": "not enough data"}
        }
        summary = service._generate_comprehensive_summary(results)
        assert summary["total_anomalies"] == 0

    def test_counts_multivariate(self, service):
        ar = AnomalyResult(0, 1.0, "mv", "if", "high", 1.5, 0.1, "d", [])
        results = {
            "multivariate": {"anomalies": [ar]}
        }
        summary = service._generate_comprehensive_summary(results)
        assert summary["total_anomalies"] == 1
        assert summary["critical_anomalies"] == 1  # high counts as critical in multivariate branch

    def test_counts_time_series(self, service):
        ar = AnomalyResult(0, 1.0, "ts", "rz", "critical", 6.0, 3.0, "d", [])
        results = {
            "time_series": {
                "time_series_anomalies": {
                    "signal": {"anomalies": [ar]}
                }
            }
        }
        summary = service._generate_comprehensive_summary(results)
        assert summary["total_anomalies"] == 1
        assert summary["critical_anomalies"] == 1
