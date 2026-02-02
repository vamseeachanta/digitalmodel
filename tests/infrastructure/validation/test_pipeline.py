# ABOUTME: Comprehensive tests for validation pipeline
# Tests individual validators, pipeline composition, severity filtering, and HTML reports

import pytest
import numpy as np
import pandas as pd
from pathlib import Path
from hypothesis import given, strategies as st
from typing import List, Dict, Any
import hashlib
import time

from digitalmodel.infrastructure.validation.pipeline import (
    ValidationSeverity,
    ValidationResult,
    ValidationIssue,
    BaseValidator,
    RangeValidator,
    MatrixValidator,
    PhysicalPlausibilityValidator,
    UnitConsistencyValidator,
    PolarDataValidator,
    TimeSeriesValidator,
    ValidationPipeline,
    ValidationCache,
    generate_html_report,
)


class TestValidationSeverity:
    """Test ValidationSeverity enum"""

    def test_severity_levels_exist(self):
        """Test all severity levels are defined"""
        assert hasattr(ValidationSeverity, 'INFO')
        assert hasattr(ValidationSeverity, 'WARNING')
        assert hasattr(ValidationSeverity, 'ERROR')
        assert hasattr(ValidationSeverity, 'CRITICAL')

    def test_severity_ordering(self):
        """Test severity levels have correct priority ordering"""
        assert ValidationSeverity.INFO.value < ValidationSeverity.WARNING.value
        assert ValidationSeverity.WARNING.value < ValidationSeverity.ERROR.value
        assert ValidationSeverity.ERROR.value < ValidationSeverity.CRITICAL.value

    def test_severity_comparison(self):
        """Test severity levels can be compared"""
        assert ValidationSeverity.CRITICAL > ValidationSeverity.ERROR
        assert ValidationSeverity.WARNING < ValidationSeverity.ERROR
        assert ValidationSeverity.INFO <= ValidationSeverity.INFO


class TestValidationResult:
    """Test ValidationResult dataclass"""

    def test_validation_result_creation(self):
        """Test creating ValidationResult with all fields"""
        issues = [
            ValidationIssue(
                severity=ValidationSeverity.WARNING,
                message="Test warning",
                validator_name="TestValidator"
            )
        ]

        result = ValidationResult(
            passed=True,
            issues=issues,
            summary={"total_checks": 10, "passed": 9},
            validator_name="TestValidator",
            data_hash="abc123"
        )

        assert result.passed is True
        assert len(result.issues) == 1
        assert result.summary["total_checks"] == 10
        assert result.validator_name == "TestValidator"

    def test_validation_result_fail_on_critical(self):
        """Test ValidationResult marks as failed on CRITICAL issues"""
        issues = [
            ValidationIssue(
                severity=ValidationSeverity.CRITICAL,
                message="Critical error",
                validator_name="TestValidator"
            )
        ]

        result = ValidationResult(
            passed=False,
            issues=issues,
            summary={},
            validator_name="TestValidator"
        )

        assert result.passed is False
        assert any(i.severity == ValidationSeverity.CRITICAL for i in result.issues)

    def test_validation_result_to_dict(self):
        """Test ValidationResult can be serialized to dict"""
        result = ValidationResult(
            passed=True,
            issues=[],
            summary={"test": 123},
            validator_name="Test"
        )

        data = result.model_dump()
        assert isinstance(data, dict)
        assert data["passed"] is True
        assert data["validator_name"] == "Test"


class TestValidationIssue:
    """Test ValidationIssue dataclass"""

    def test_issue_creation_minimal(self):
        """Test creating issue with minimal fields"""
        issue = ValidationIssue(
            severity=ValidationSeverity.ERROR,
            message="Test error",
            validator_name="TestValidator"
        )

        assert issue.severity == ValidationSeverity.ERROR
        assert issue.message == "Test error"
        assert issue.validator_name == "TestValidator"

    def test_issue_creation_with_metadata(self):
        """Test creating issue with full metadata"""
        issue = ValidationIssue(
            severity=ValidationSeverity.WARNING,
            message="Value out of range",
            validator_name="RangeValidator",
            field_name="temperature",
            value=150.0,
            expected_range=(0.0, 100.0),
            location="row 42"
        )

        assert issue.field_name == "temperature"
        assert issue.value == 150.0
        assert issue.expected_range == (0.0, 100.0)


class TestBaseValidator:
    """Test BaseValidator abstract class"""

    def test_base_validator_cannot_instantiate(self):
        """Test BaseValidator is abstract"""
        with pytest.raises(TypeError):
            BaseValidator()

    def test_base_validator_subclass_must_implement_validate(self):
        """Test subclass must implement validate method"""
        class IncompleteValidator(BaseValidator):
            pass

        with pytest.raises(TypeError):
            IncompleteValidator()

    def test_base_validator_subclass_works(self):
        """Test proper subclass can be instantiated"""
        class CompleteValidator(BaseValidator):
            def validate(self, data: Any) -> ValidationResult:
                return ValidationResult(
                    passed=True,
                    issues=[],
                    summary={},
                    validator_name="CompleteValidator"
                )

        validator = CompleteValidator(cacheable=True)
        assert validator.cacheable is True
        result = validator.validate({"test": "data"})
        assert result.passed is True


class TestRangeValidator:
    """Test RangeValidator with property-based testing"""

    def test_range_validator_creation(self):
        """Test creating RangeValidator with min/max"""
        validator = RangeValidator(
            field_name="temperature",
            min_value=0.0,
            max_value=100.0
        )

        assert validator.field_name == "temperature"
        assert validator.min_value == 0.0
        assert validator.max_value == 100.0

    def test_range_validator_valid_value(self):
        """Test validation passes for value in range"""
        validator = RangeValidator("temp", min_value=0.0, max_value=100.0)
        data = {"temp": 50.0}

        result = validator.validate(data)
        assert result.passed is True
        assert len(result.issues) == 0

    def test_range_validator_value_below_min(self):
        """Test validation fails for value below minimum"""
        validator = RangeValidator("temp", min_value=0.0, max_value=100.0)
        data = {"temp": -10.0}

        result = validator.validate(data)
        assert result.passed is False
        assert len(result.issues) == 1
        assert result.issues[0].severity == ValidationSeverity.ERROR

    def test_range_validator_value_above_max(self):
        """Test validation fails for value above maximum"""
        validator = RangeValidator("temp", min_value=0.0, max_value=100.0)
        data = {"temp": 150.0}

        result = validator.validate(data)
        assert result.passed is False
        assert len(result.issues) == 1

    def test_range_validator_array_data(self):
        """Test validation with numpy array"""
        validator = RangeValidator("values", min_value=0.0, max_value=10.0)
        data = {"values": np.array([1.0, 5.0, 9.0])}

        result = validator.validate(data)
        assert result.passed is True

    def test_range_validator_array_with_outliers(self):
        """Test validation detects outliers in array"""
        validator = RangeValidator("values", min_value=0.0, max_value=10.0)
        data = {"values": np.array([1.0, 5.0, 15.0, -2.0])}

        result = validator.validate(data)
        assert result.passed is False
        assert len(result.issues) >= 2  # Two out-of-range values

    @given(st.floats(min_value=-1e6, max_value=1e6))
    def test_range_validator_property_symmetric(self, value: float):
        """Property test: symmetric ranges work correctly"""
        validator = RangeValidator("x", min_value=-abs(value), max_value=abs(value))

        # Value exactly at bounds should pass
        result = validator.validate({"x": abs(value)})
        assert result.passed is True

        result = validator.validate({"x": -abs(value)})
        assert result.passed is True

    @given(
        st.floats(min_value=0, max_value=100),
        st.floats(min_value=0, max_value=100)
    )
    def test_range_validator_property_bounds(self, min_val: float, max_val: float):
        """Property test: min must be less than or equal to max"""
        if min_val > max_val:
            # Test that passing min > max directly raises ValueError
            with pytest.raises(ValueError):
                RangeValidator("x", min_value=min_val, max_value=max_val)


class TestMatrixValidator:
    """Test MatrixValidator for matrix properties"""

    def test_matrix_validator_shape(self):
        """Test matrix shape validation"""
        validator = MatrixValidator(
            field_name="matrix",
            expected_shape=(3, 3)
        )

        data = {"matrix": np.eye(3)}
        result = validator.validate(data)
        assert result.passed is True

    def test_matrix_validator_wrong_shape(self):
        """Test matrix shape validation fails on wrong shape"""
        validator = MatrixValidator(
            field_name="matrix",
            expected_shape=(3, 3)
        )

        data = {"matrix": np.eye(4)}
        result = validator.validate(data)
        assert result.passed is False
        assert any("shape" in issue.message.lower() for issue in result.issues)

    def test_matrix_validator_symmetry(self):
        """Test symmetric matrix validation"""
        validator = MatrixValidator(
            field_name="matrix",
            check_symmetric=True
        )

        # Symmetric matrix
        symmetric = np.array([[1, 2, 3], [2, 4, 5], [3, 5, 6]])
        result = validator.validate({"matrix": symmetric})
        assert result.passed is True

    def test_matrix_validator_not_symmetric(self):
        """Test non-symmetric matrix fails symmetry check"""
        validator = MatrixValidator(
            field_name="matrix",
            check_symmetric=True
        )

        # Non-symmetric matrix
        non_symmetric = np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
        result = validator.validate({"matrix": non_symmetric})
        assert result.passed is False

    def test_matrix_validator_positive_definite(self):
        """Test positive definite matrix validation"""
        validator = MatrixValidator(
            field_name="matrix",
            check_positive_definite=True
        )

        # Positive definite matrix
        pos_def = np.array([[2, -1, 0], [-1, 2, -1], [0, -1, 2]])
        result = validator.validate({"matrix": pos_def})
        assert result.passed is True

    def test_matrix_validator_not_positive_definite(self):
        """Test non-positive definite matrix fails"""
        validator = MatrixValidator(
            field_name="matrix",
            check_positive_definite=True
        )

        # Not positive definite (has negative eigenvalue)
        not_pos_def = np.array([[1, 2], [2, 1]])
        result = validator.validate({"matrix": not_pos_def})
        assert result.passed is False

    def test_matrix_validator_6x6_stiffness_matrix(self):
        """Test 6x6 stiffness matrix (common in marine engineering)"""
        validator = MatrixValidator(
            field_name="stiffness",
            expected_shape=(6, 6),
            check_symmetric=True,
            check_positive_definite=True
        )

        # Create valid 6x6 stiffness matrix
        stiffness = np.diag([1e6, 1e6, 1e6, 1e8, 1e8, 1e8])
        result = validator.validate({"stiffness": stiffness})
        assert result.passed is True


class TestPhysicalPlausibilityValidator:
    """Test PhysicalPlausibilityValidator for engineering limits"""

    def test_physical_validator_force_valid(self):
        """Test valid force value"""
        validator = PhysicalPlausibilityValidator(
            field_name="tension",
            physical_type="force"
        )

        data = {"tension": 500000.0}  # 500 kN - reasonable
        result = validator.validate(data)
        assert result.passed is True

    def test_physical_validator_force_extreme(self):
        """Test extreme force value beyond absolute max"""
        validator = PhysicalPlausibilityValidator(
            field_name="tension",
            physical_type="force"
        )

        data = {"tension": 1e9}  # 1 GN - beyond absolute max (1e8)
        result = validator.validate(data)
        # Should be CRITICAL (beyond absolute limit)
        assert any(i.severity == ValidationSeverity.CRITICAL for i in result.issues)

    def test_physical_validator_force_impossible(self):
        """Test impossible force value fails"""
        validator = PhysicalPlausibilityValidator(
            field_name="tension",
            physical_type="force"
        )

        data = {"tension": 1e15}  # Physically impossible
        result = validator.validate(data)
        assert result.passed is False

    def test_physical_validator_moment(self):
        """Test moment validation"""
        validator = PhysicalPlausibilityValidator(
            field_name="bending_moment",
            physical_type="moment"
        )

        data = {"bending_moment": 1e6}  # 1 MN⋅m
        result = validator.validate(data)
        assert result.passed is True

    def test_physical_validator_displacement(self):
        """Test displacement validation"""
        validator = PhysicalPlausibilityValidator(
            field_name="heave",
            physical_type="displacement"
        )

        data = {"heave": 5.0}  # 5 meters
        result = validator.validate(data)
        assert result.passed is True

    def test_physical_validator_custom_limits(self):
        """Test validator with custom limits"""
        custom_limits = {
            "min": 0.0,
            "max": 1000.0,
            "typical_max": 500.0
        }

        validator = PhysicalPlausibilityValidator(
            field_name="custom",
            physical_type="force",
            custom_limits=custom_limits
        )

        data = {"custom": 750.0}  # Above typical but below max
        result = validator.validate(data)
        assert any(i.severity == ValidationSeverity.WARNING for i in result.issues)


class TestUnitConsistencyValidator:
    """Test UnitConsistencyValidator for SI unit checks"""

    def test_unit_validator_consistent_si(self):
        """Test consistent SI units pass validation"""
        validator = UnitConsistencyValidator()

        data = {
            "force": {"value": 1000.0, "unit": "N"},
            "length": {"value": 10.0, "unit": "m"},
            "stress": {"value": 100.0, "unit": "Pa"}  # N/m²
        }

        result = validator.validate(data)
        assert result.passed is True

    def test_unit_validator_mixed_units_warning(self):
        """Test mixed unit systems generate warning"""
        validator = UnitConsistencyValidator()

        data = {
            "force": {"value": 1000.0, "unit": "N"},
            "length": {"value": 10.0, "unit": "ft"},  # Imperial
        }

        result = validator.validate(data)
        assert any(i.severity == ValidationSeverity.WARNING for i in result.issues)

    def test_unit_validator_invalid_unit(self):
        """Test invalid unit fails validation"""
        validator = UnitConsistencyValidator()

        data = {
            "force": {"value": 1000.0, "unit": "invalid"}
        }

        result = validator.validate(data)
        assert result.passed is False

    def test_unit_validator_dimensionality_check(self):
        """Test dimensional consistency checking"""
        validator = UnitConsistencyValidator(check_dimensionality=True)

        data = {
            "stress": {"value": 1000.0, "unit": "Pa"},  # N/m²
            "pressure": {"value": 2000.0, "unit": "Pa"}  # Same dimension
        }

        result = validator.validate(data)
        assert result.passed is True


class TestPolarDataValidator:
    """Test PolarDataValidator for 360° coverage"""

    def test_polar_validator_complete_coverage(self):
        """Test validation passes for complete 360° coverage"""
        validator = PolarDataValidator(
            angle_field="heading",
            value_field="rao"
        )

        data = {
            "heading": np.arange(0, 360, 15),  # Every 15°
            "rao": np.random.rand(24)
        }

        result = validator.validate(data)
        assert result.passed is True

    def test_polar_validator_incomplete_coverage(self):
        """Test validation fails for incomplete coverage"""
        validator = PolarDataValidator(
            angle_field="heading",
            value_field="rao"
        )

        data = {
            "heading": np.arange(0, 180, 15),  # Only 0-180°
            "rao": np.random.rand(12)
        }

        result = validator.validate(data)
        assert result.passed is False
        assert any("coverage" in issue.message.lower() for issue in result.issues)

    def test_polar_validator_gap_detection(self):
        """Test detection of gaps in polar data"""
        validator = PolarDataValidator(
            angle_field="heading",
            value_field="rao",
            max_gap_degrees=30.0
        )

        # Data with large gap
        angles = np.concatenate([
            np.arange(0, 90, 15),
            np.arange(150, 360, 15)  # Gap from 90-150°
        ])

        data = {
            "heading": angles,
            "rao": np.random.rand(len(angles))
        }

        result = validator.validate(data)
        assert result.passed is False
        assert any("gap" in issue.message.lower() for issue in result.issues)

    def test_polar_validator_symmetry_check(self):
        """Test polar symmetry validation"""
        validator = PolarDataValidator(
            angle_field="heading",
            value_field="rao",
            check_symmetry=True,
            symmetry_tolerance=0.1
        )

        # Create symmetric data
        angles = np.arange(0, 360, 30)
        values = np.array([1.0, 0.9, 0.7, 0.5, 0.3, 0.3, 0.5, 0.7, 0.9, 1.0, 0.9, 0.7])

        data = {"heading": angles, "rao": values}
        result = validator.validate(data)
        assert result.passed is True


class TestTimeSeriesValidator:
    """Test TimeSeriesValidator for gaps and outliers"""

    def test_timeseries_validator_regular_data(self):
        """Test validation passes for regular time series"""
        validator = TimeSeriesValidator(
            time_field="time",
            value_field="displacement"
        )

        data = {
            "time": np.arange(0, 100, 0.1),
            "displacement": np.sin(np.arange(0, 100, 0.1))
        }

        result = validator.validate(data)
        assert result.passed is True

    def test_timeseries_validator_gap_detection(self):
        """Test detection of gaps in time series"""
        validator = TimeSeriesValidator(
            time_field="time",
            value_field="displacement",
            detect_gaps=True
        )

        # Time series with gap
        time = np.concatenate([
            np.arange(0, 50, 0.1),
            np.arange(60, 100, 0.1)  # Gap from 50-60
        ])

        data = {
            "time": time,
            "displacement": np.random.rand(len(time))
        }

        result = validator.validate(data)
        assert result.passed is False
        assert any("gap" in issue.message.lower() for issue in result.issues)

    def test_timeseries_validator_outlier_detection(self):
        """Test outlier detection in time series"""
        validator = TimeSeriesValidator(
            time_field="time",
            value_field="displacement",
            detect_outliers=True,
            outlier_std_threshold=3.0
        )

        # Data with outliers
        values = np.random.normal(0, 1, 1000)
        values[500] = 10.0  # Outlier

        data = {
            "time": np.arange(1000),
            "displacement": values
        }

        result = validator.validate(data)
        assert len(result.issues) > 0
        assert any("outlier" in issue.message.lower() for issue in result.issues)

    def test_timeseries_validator_monotonic_time(self):
        """Test validation of monotonic time"""
        validator = TimeSeriesValidator(
            time_field="time",
            value_field="displacement",
            check_monotonic=True
        )

        # Non-monotonic time
        data = {
            "time": np.array([0, 1, 2, 1.5, 3, 4]),  # 1.5 breaks monotonicity
            "displacement": np.random.rand(6)
        }

        result = validator.validate(data)
        assert result.passed is False
        assert any("monotonic" in issue.message.lower() for issue in result.issues)


class TestValidationCache:
    """Test ValidationCache for caching results"""

    def test_cache_creation(self):
        """Test creating validation cache"""
        cache = ValidationCache(ttl_seconds=3600)
        assert cache.ttl_seconds == 3600

    def test_cache_store_and_retrieve(self):
        """Test storing and retrieving from cache"""
        cache = ValidationCache(ttl_seconds=3600)

        result = ValidationResult(
            passed=True,
            issues=[],
            summary={},
            validator_name="Test"
        )

        cache_key = "test_key"
        cache.store(cache_key, result)

        cached = cache.retrieve(cache_key)
        assert cached is not None
        assert cached.passed is True

    def test_cache_expiration(self):
        """Test cache expiration after TTL"""
        cache = ValidationCache(ttl_seconds=1)  # 1 second TTL

        result = ValidationResult(
            passed=True,
            issues=[],
            summary={},
            validator_name="Test"
        )

        cache_key = "test_key"
        cache.store(cache_key, result)

        # Wait for expiration
        time.sleep(1.1)

        cached = cache.retrieve(cache_key)
        assert cached is None

    def test_cache_key_generation_from_data(self):
        """Test automatic cache key generation from data"""
        cache = ValidationCache()

        data1 = {"value": 123, "name": "test"}
        data2 = {"value": 123, "name": "test"}
        data3 = {"value": 456, "name": "test"}

        key1 = cache.generate_key(data1)
        key2 = cache.generate_key(data2)
        key3 = cache.generate_key(data3)

        assert key1 == key2  # Same data should have same key
        assert key1 != key3  # Different data should have different key


class TestValidationPipeline:
    """Test ValidationPipeline orchestration"""

    def test_pipeline_creation(self):
        """Test creating validation pipeline"""
        validators = [
            RangeValidator("temp", 0, 100),
            RangeValidator("pressure", 0, 1000)
        ]

        pipeline = ValidationPipeline(
            validators=validators,
            fail_fast=True,
            parallel=True
        )

        assert len(pipeline.validators) == 2
        assert pipeline.fail_fast is True
        assert pipeline.parallel is True

    def test_pipeline_execute_all_pass(self):
        """Test pipeline execution with all validators passing"""
        validators = [
            RangeValidator("temp", 0, 100),
            RangeValidator("pressure", 0, 1000)
        ]

        pipeline = ValidationPipeline(validators=validators)

        data = {"temp": 50, "pressure": 500}
        results = pipeline.execute(data)

        assert len(results) == 2
        assert all(r.passed for r in results)

    def test_pipeline_execute_with_failures(self):
        """Test pipeline execution with failures"""
        validators = [
            RangeValidator("temp", 0, 100),
            RangeValidator("pressure", 0, 1000)
        ]

        pipeline = ValidationPipeline(validators=validators)

        data = {"temp": 150, "pressure": 500}  # temp out of range
        results = pipeline.execute(data)

        assert len(results) == 2
        assert not results[0].passed  # temp validator failed
        assert results[1].passed      # pressure validator passed

    def test_pipeline_fail_fast_on_critical(self):
        """Test pipeline stops on CRITICAL error with fail_fast=True"""

        class CriticalValidator(BaseValidator):
            def validate(self, data):
                return ValidationResult(
                    passed=False,
                    issues=[ValidationIssue(
                        severity=ValidationSeverity.CRITICAL,
                        message="Critical error",
                        validator_name="CriticalValidator"
                    )],
                    summary={},
                    validator_name="CriticalValidator"
                )

        validators = [
            CriticalValidator(),
            RangeValidator("temp", 0, 100)
        ]

        pipeline = ValidationPipeline(validators=validators, fail_fast=True)

        data = {"temp": 50}
        results = pipeline.execute(data)

        # Should stop after first critical error
        assert len(results) == 1
        assert results[0].issues[0].severity == ValidationSeverity.CRITICAL

    def test_pipeline_parallel_execution(self):
        """Test pipeline executes validators in parallel"""

        class SlowValidator(BaseValidator):
            def __init__(self, name: str, delay: float):
                super().__init__()
                self.name = name
                self.delay = delay

            def validate(self, data):
                time.sleep(self.delay)
                return ValidationResult(
                    passed=True,
                    issues=[],
                    summary={},
                    validator_name=self.name
                )

        validators = [
            SlowValidator("v1", 0.1),
            SlowValidator("v2", 0.1),
            SlowValidator("v3", 0.1)
        ]

        # Parallel execution
        pipeline_parallel = ValidationPipeline(validators=validators, parallel=True)
        start = time.time()
        pipeline_parallel.execute({})
        parallel_time = time.time() - start

        # Sequential execution
        pipeline_sequential = ValidationPipeline(validators=validators, parallel=False)
        start = time.time()
        pipeline_sequential.execute({})
        sequential_time = time.time() - start

        # Parallel should be faster (with some tolerance)
        assert parallel_time < sequential_time * 0.8

    def test_pipeline_caching(self):
        """Test pipeline uses caching for repeated data"""

        call_count = {"count": 0}

        class CountingValidator(BaseValidator):
            def validate(self, data):
                call_count["count"] += 1
                return ValidationResult(
                    passed=True,
                    issues=[],
                    summary={},
                    validator_name="CountingValidator"
                )

        validator = CountingValidator(cacheable=True)
        pipeline = ValidationPipeline(validators=[validator])

        data = {"value": 123}

        # First execution
        pipeline.execute(data)
        assert call_count["count"] == 1

        # Second execution with same data
        pipeline.execute(data)
        assert call_count["count"] == 1  # Should use cache, not call again

        # Third execution with different data
        pipeline.execute({"value": 456})
        assert call_count["count"] == 2  # New data, should call validator

    def test_pipeline_severity_filtering(self):
        """Test pipeline can filter results by severity"""
        validators = [
            RangeValidator("temp", 0, 100),
            RangeValidator("pressure", 0, 1000)
        ]

        pipeline = ValidationPipeline(validators=validators)

        data = {"temp": 150, "pressure": 1500}  # Both out of range
        results = pipeline.execute(data)

        # Filter only ERROR severity
        errors = pipeline.filter_by_severity(results, ValidationSeverity.ERROR)
        assert all(
            any(i.severity == ValidationSeverity.ERROR for i in r.issues)
            for r in errors
        )

    def test_pipeline_aggregate_results(self):
        """Test pipeline can aggregate multiple results"""
        validators = [
            RangeValidator("temp", 0, 100),
            RangeValidator("pressure", 0, 1000)
        ]

        pipeline = ValidationPipeline(validators=validators)

        data = {"temp": 150, "pressure": 500}
        results = pipeline.execute(data)

        aggregated = pipeline.aggregate_results(results)

        assert "total_validators" in aggregated
        assert "passed_validators" in aggregated
        assert "total_issues" in aggregated
        assert aggregated["total_validators"] == 2
        assert aggregated["passed_validators"] == 1


class TestHTMLReportGeneration:
    """Test HTML validation report generation"""

    def test_generate_html_report_basic(self, tmp_path):
        """Test generating basic HTML report"""
        issues = [
            ValidationIssue(
                severity=ValidationSeverity.WARNING,
                message="Test warning",
                validator_name="TestValidator"
            )
        ]

        results = [
            ValidationResult(
                passed=True,
                issues=issues,
                summary={"total": 10, "passed": 9},
                validator_name="TestValidator"
            )
        ]

        output_file = tmp_path / "report.html"
        generate_html_report(results, output_file)

        assert output_file.exists()
        content = output_file.read_text()
        assert "Validation Report" in content
        assert "TestValidator" in content

    def test_html_report_contains_plotly(self, tmp_path):
        """Test HTML report contains interactive Plotly visualizations"""
        results = [
            ValidationResult(
                passed=False,
                issues=[
                    ValidationIssue(
                        severity=ValidationSeverity.ERROR,
                        message="Error 1",
                        validator_name="V1"
                    ),
                    ValidationIssue(
                        severity=ValidationSeverity.WARNING,
                        message="Warning 1",
                        validator_name="V1"
                    )
                ],
                summary={},
                validator_name="V1"
            )
        ]

        output_file = tmp_path / "report.html"
        generate_html_report(results, output_file)

        content = output_file.read_text()
        assert "plotly" in content.lower()
        assert "interactive" in content.lower() or "plot" in content.lower()

    def test_html_report_severity_distribution(self, tmp_path):
        """Test HTML report includes severity distribution chart"""
        results = [
            ValidationResult(
                passed=False,
                issues=[
                    ValidationIssue(
                        severity=ValidationSeverity.ERROR,
                        message="E1",
                        validator_name="V1"
                    ),
                    ValidationIssue(
                        severity=ValidationSeverity.WARNING,
                        message="W1",
                        validator_name="V1"
                    ),
                    ValidationIssue(
                        severity=ValidationSeverity.INFO,
                        message="I1",
                        validator_name="V1"
                    )
                ],
                summary={},
                validator_name="V1"
            )
        ]

        output_file = tmp_path / "report.html"
        generate_html_report(results, output_file)

        content = output_file.read_text()
        assert "severity" in content.lower()

    def test_html_report_export_json(self, tmp_path):
        """Test HTML report can export results to JSON"""
        results = [
            ValidationResult(
                passed=True,
                issues=[],
                summary={},
                validator_name="Test"
            )
        ]

        output_file = tmp_path / "report.html"
        json_file = tmp_path / "results.json"

        generate_html_report(results, output_file, export_json=json_file)

        assert json_file.exists()
        import json
        data = json.loads(json_file.read_text())
        assert isinstance(data, list)
        assert len(data) == 1


class TestIntegrationWithExistingValidators:
    """Test integration with existing validation modules"""

    def test_adapter_pattern_for_existing_validator(self):
        """Test adapter wraps existing validator"""

        # Simulate existing validator with different interface
        class LegacyValidator:
            def check_data(self, data):
                if data.get("value", 0) > 100:
                    return {"status": "fail", "errors": ["Value too high"]}
                return {"status": "pass", "errors": []}

        # Adapter to make it compatible
        class LegacyValidatorAdapter(BaseValidator):
            def __init__(self, legacy_validator):
                super().__init__()
                self.legacy = legacy_validator

            def validate(self, data):
                result = self.legacy.check_data(data)

                if result["status"] == "pass":
                    return ValidationResult(
                        passed=True,
                        issues=[],
                        summary={},
                        validator_name="LegacyValidator"
                    )
                else:
                    issues = [
                        ValidationIssue(
                            severity=ValidationSeverity.ERROR,
                            message=err,
                            validator_name="LegacyValidator"
                        )
                        for err in result["errors"]
                    ]
                    return ValidationResult(
                        passed=False,
                        issues=issues,
                        summary={},
                        validator_name="LegacyValidator"
                    )

        # Test adapter
        legacy = LegacyValidator()
        adapter = LegacyValidatorAdapter(legacy)

        result = adapter.validate({"value": 50})
        assert result.passed is True

        result = adapter.validate({"value": 150})
        assert result.passed is False


class TestValidationPipelineWithRealData:
    """Integration tests with real-world data"""

    def test_pipeline_with_marine_engineering_data(self):
        """Test pipeline with marine engineering dataset"""

        # Simulate mooring line data
        data = {
            "tension": 850000.0,  # 850 kN
            "angle": 45.0,        # degrees
            "displacement": 3.2,  # meters
            "stiffness_matrix": np.diag([1e6, 1e6, 1e6, 1e8, 1e8, 1e8])
        }

        validators = [
            PhysicalPlausibilityValidator("tension", "force"),
            RangeValidator("angle", min_value=0, max_value=90),
            RangeValidator("displacement", min_value=0, max_value=50),
            MatrixValidator(
                "stiffness_matrix",
                expected_shape=(6, 6),
                check_symmetric=True,
                check_positive_definite=True
            )
        ]

        pipeline = ValidationPipeline(validators=validators, parallel=True)
        results = pipeline.execute(data)

        # All should pass
        assert all(r.passed for r in results)

    def test_pipeline_with_rao_polar_data(self):
        """Test pipeline with RAO polar data"""

        # Simulate RAO data (Response Amplitude Operator)
        headings = np.arange(0, 360, 15)  # Every 15 degrees
        surge_rao = np.abs(np.sin(np.radians(headings))) + 0.5
        heave_rao = np.abs(np.cos(np.radians(headings))) + 0.3

        data = {
            "heading": headings,
            "surge_rao": surge_rao,
            "heave_rao": heave_rao
        }

        validators = [
            PolarDataValidator("heading", "surge_rao", max_gap_degrees=20),
            PolarDataValidator("heading", "heave_rao", max_gap_degrees=20),
            RangeValidator("surge_rao", min_value=0, max_value=5),
            RangeValidator("heave_rao", min_value=0, max_value=5)
        ]

        pipeline = ValidationPipeline(validators=validators)
        results = pipeline.execute(data)

        assert all(r.passed for r in results)
