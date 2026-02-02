# ABOUTME: Core validation pipeline with composable validators
# Supports range, matrix, physical plausibility, units, polar, and time series validation

from abc import ABC, abstractmethod
from enum import IntEnum
from typing import Any, Dict, List, Optional, Tuple, Union
from pathlib import Path
from dataclasses import dataclass, field
from concurrent.futures import ThreadPoolExecutor, as_completed
import hashlib
import json
import time
from datetime import datetime

import numpy as np
import pandas as pd
from pydantic import BaseModel, Field, ConfigDict

# HTML report generation
import plotly.graph_objects as go
from plotly.subplots import make_subplots


class ValidationSeverity(IntEnum):
    """Validation issue severity levels with ordering"""
    INFO = 1
    WARNING = 2
    ERROR = 3
    CRITICAL = 4


class ValidationIssue(BaseModel):
    """Individual validation issue with metadata"""

    model_config = ConfigDict(arbitrary_types_allowed=True)

    severity: ValidationSeverity
    message: str
    validator_name: str
    field_name: Optional[str] = None
    value: Optional[Any] = None
    expected_range: Optional[Tuple[float, float]] = None
    location: Optional[str] = None

    def __str__(self) -> str:
        parts = [f"[{self.severity.name}]", self.message]
        if self.field_name:
            parts.insert(1, f"Field: {self.field_name}")
        if self.location:
            parts.append(f"({self.location})")
        return " ".join(parts)


class ValidationResult(BaseModel):
    """Complete validation result from a validator"""

    model_config = ConfigDict(arbitrary_types_allowed=True)

    passed: bool
    issues: List[ValidationIssue] = Field(default_factory=list)
    summary: Dict[str, Any] = Field(default_factory=dict)
    validator_name: str
    data_hash: Optional[str] = None
    timestamp: datetime = Field(default_factory=datetime.now)

    def has_critical_issues(self) -> bool:
        """Check if result contains CRITICAL severity issues"""
        return any(issue.severity == ValidationSeverity.CRITICAL for issue in self.issues)

    def get_issues_by_severity(self, severity: ValidationSeverity) -> List[ValidationIssue]:
        """Get all issues matching specified severity"""
        return [issue for issue in self.issues if issue.severity == severity]


class BaseValidator(ABC):
    """Abstract base validator class with caching support"""

    def __init__(self, cacheable: bool = False):
        """
        Initialize base validator.

        Args:
            cacheable: Whether validation results should be cached
        """
        self.cacheable = cacheable

    @abstractmethod
    def validate(self, data: Any) -> ValidationResult:
        """
        Validate data and return result.

        Args:
            data: Data to validate

        Returns:
            ValidationResult with issues and summary
        """
        pass

    def _generate_data_hash(self, data: Any) -> str:
        """Generate SHA256 hash of data for caching"""
        data_str = json.dumps(data, sort_keys=True, default=str)
        return hashlib.sha256(data_str.encode()).hexdigest()


class RangeValidator(BaseValidator):
    """Validates that values are within specified min/max range"""

    def __init__(
        self,
        field_name: str,
        min_value: Optional[float] = None,
        max_value: Optional[float] = None,
        cacheable: bool = False
    ):
        """
        Initialize range validator.

        Args:
            field_name: Name of field to validate
            min_value: Minimum allowed value (inclusive)
            max_value: Maximum allowed value (inclusive)
            cacheable: Whether to cache results

        Raises:
            ValueError: If min_value >= max_value
        """
        super().__init__(cacheable=cacheable)
        self.field_name = field_name
        self.min_value = min_value
        self.max_value = max_value

        if min_value is not None and max_value is not None:
            # Allow equal values for edge case (e.g., value=0.0 creates min=-0.0, max=0.0)
            if min_value > max_value:
                raise ValueError(f"min_value ({min_value}) must be less than or equal to max_value ({max_value})")

    def validate(self, data: Any) -> ValidationResult:
        """Validate field is within range"""
        issues = []

        # Extract field value
        if isinstance(data, dict):
            if self.field_name not in data:
                return ValidationResult(
                    passed=False,
                    issues=[ValidationIssue(
                        severity=ValidationSeverity.ERROR,
                        message=f"Field '{self.field_name}' not found in data",
                        validator_name="RangeValidator",
                        field_name=self.field_name
                    )],
                    summary={},
                    validator_name="RangeValidator"
                )
            value = data[self.field_name]
        else:
            value = data

        # Handle numpy arrays
        if isinstance(value, np.ndarray):
            values = value
        else:
            values = np.array([value])

        # Check range violations
        if self.min_value is not None:
            below_min = values < self.min_value
            if np.any(below_min):
                count = np.sum(below_min)
                issues.append(ValidationIssue(
                    severity=ValidationSeverity.ERROR,
                    message=f"{count} value(s) below minimum {self.min_value}",
                    validator_name="RangeValidator",
                    field_name=self.field_name,
                    expected_range=(self.min_value, self.max_value)
                ))

        if self.max_value is not None:
            above_max = values > self.max_value
            if np.any(above_max):
                count = np.sum(above_max)
                issues.append(ValidationIssue(
                    severity=ValidationSeverity.ERROR,
                    message=f"{count} value(s) above maximum {self.max_value}",
                    validator_name="RangeValidator",
                    field_name=self.field_name,
                    expected_range=(self.min_value, self.max_value)
                ))

        return ValidationResult(
            passed=len(issues) == 0,
            issues=issues,
            summary={
                "field": self.field_name,
                "min": self.min_value,
                "max": self.max_value,
                "values_checked": len(values)
            },
            validator_name="RangeValidator",
            data_hash=self._generate_data_hash(data)
        )


class MatrixValidator(BaseValidator):
    """Validates matrix properties: shape, symmetry, positive-definite"""

    def __init__(
        self,
        field_name: str,
        expected_shape: Optional[Tuple[int, int]] = None,
        check_symmetric: bool = False,
        check_positive_definite: bool = False,
        cacheable: bool = False
    ):
        """
        Initialize matrix validator.

        Args:
            field_name: Name of matrix field to validate
            expected_shape: Expected matrix shape (rows, cols)
            check_symmetric: Check if matrix is symmetric
            check_positive_definite: Check if matrix is positive definite
            cacheable: Whether to cache results
        """
        super().__init__(cacheable=cacheable)
        self.field_name = field_name
        self.expected_shape = expected_shape
        self.check_symmetric = check_symmetric
        self.check_positive_definite = check_positive_definite

    def validate(self, data: Any) -> ValidationResult:
        """Validate matrix properties"""
        issues = []

        # Extract matrix
        if isinstance(data, dict):
            if self.field_name not in data:
                return ValidationResult(
                    passed=False,
                    issues=[ValidationIssue(
                        severity=ValidationSeverity.ERROR,
                        message=f"Field '{self.field_name}' not found in data",
                        validator_name="MatrixValidator",
                        field_name=self.field_name
                    )],
                    summary={},
                    validator_name="MatrixValidator"
                )
            matrix = data[self.field_name]
        else:
            matrix = data

        matrix = np.asarray(matrix)

        # Check shape
        if self.expected_shape is not None:
            if matrix.shape != self.expected_shape:
                issues.append(ValidationIssue(
                    severity=ValidationSeverity.ERROR,
                    message=f"Matrix shape {matrix.shape} does not match expected {self.expected_shape}",
                    validator_name="MatrixValidator",
                    field_name=self.field_name
                ))

        # Check symmetry
        if self.check_symmetric and matrix.ndim == 2:
            if not np.allclose(matrix, matrix.T):
                issues.append(ValidationIssue(
                    severity=ValidationSeverity.ERROR,
                    message="Matrix is not symmetric",
                    validator_name="MatrixValidator",
                    field_name=self.field_name
                ))

        # Check positive definite
        if self.check_positive_definite and matrix.ndim == 2:
            try:
                eigenvalues = np.linalg.eigvals(matrix)
                if not np.all(eigenvalues > 0):
                    issues.append(ValidationIssue(
                        severity=ValidationSeverity.ERROR,
                        message="Matrix is not positive definite (has non-positive eigenvalues)",
                        validator_name="MatrixValidator",
                        field_name=self.field_name
                    ))
            except np.linalg.LinAlgError:
                issues.append(ValidationIssue(
                    severity=ValidationSeverity.ERROR,
                    message="Cannot compute eigenvalues for positive definite check",
                    validator_name="MatrixValidator",
                    field_name=self.field_name
                ))

        return ValidationResult(
            passed=len(issues) == 0,
            issues=issues,
            summary={
                "field": self.field_name,
                "shape": matrix.shape,
                "symmetric_check": self.check_symmetric,
                "positive_definite_check": self.check_positive_definite
            },
            validator_name="MatrixValidator",
            data_hash=self._generate_data_hash(data)
        )


class PhysicalPlausibilityValidator(BaseValidator):
    """Validates physical engineering limits for forces, moments, displacements"""

    # Default engineering limits (SI units)
    DEFAULT_LIMITS = {
        "force": {
            "min": -1e8,  # -100 MN
            "max": 1e8,   # 100 MN
            "typical_max": 1e6  # 1 MN
        },
        "moment": {
            "min": -1e9,  # -1000 MN⋅m
            "max": 1e9,   # 1000 MN⋅m
            "typical_max": 1e7  # 10 MN⋅m
        },
        "displacement": {
            "min": -100,  # -100 m
            "max": 100,   # 100 m
            "typical_max": 20  # 20 m
        }
    }

    def __init__(
        self,
        field_name: str,
        physical_type: str,
        custom_limits: Optional[Dict[str, float]] = None,
        cacheable: bool = False
    ):
        """
        Initialize physical plausibility validator.

        Args:
            field_name: Name of field to validate
            physical_type: Type of physical quantity (force, moment, displacement)
            custom_limits: Custom limits dict with min, max, typical_max
            cacheable: Whether to cache results
        """
        super().__init__(cacheable=cacheable)
        self.field_name = field_name
        self.physical_type = physical_type

        if custom_limits:
            self.limits = custom_limits
        else:
            self.limits = self.DEFAULT_LIMITS.get(physical_type, {})

    def validate(self, data: Any) -> ValidationResult:
        """Validate physical plausibility"""
        issues = []

        # Extract value
        if isinstance(data, dict):
            if self.field_name not in data:
                return ValidationResult(
                    passed=False,
                    issues=[ValidationIssue(
                        severity=ValidationSeverity.ERROR,
                        message=f"Field '{self.field_name}' not found in data",
                        validator_name="PhysicalPlausibilityValidator",
                        field_name=self.field_name
                    )],
                    summary={},
                    validator_name="PhysicalPlausibilityValidator"
                )
            value = data[self.field_name]
        else:
            value = data

        # Handle arrays
        if isinstance(value, np.ndarray):
            values = value
        else:
            values = np.array([value])

        # Check absolute limits
        if "min" in self.limits:
            below_min = values < self.limits["min"]
            if np.any(below_min):
                issues.append(ValidationIssue(
                    severity=ValidationSeverity.CRITICAL,
                    message=f"Value(s) below physical minimum {self.limits['min']} (impossible)",
                    validator_name="PhysicalPlausibilityValidator",
                    field_name=self.field_name
                ))

        if "max" in self.limits:
            above_max = values > self.limits["max"]
            if np.any(above_max):
                issues.append(ValidationIssue(
                    severity=ValidationSeverity.CRITICAL,
                    message=f"Value(s) above physical maximum {self.limits['max']} (impossible)",
                    validator_name="PhysicalPlausibilityValidator",
                    field_name=self.field_name
                ))

        # Check typical limits (warnings) - only if no critical/error issues
        if "typical_max" in self.limits:
            # Only check typical if value is within absolute limits
            if not any(i.severity >= ValidationSeverity.ERROR for i in issues):
                above_typical = np.abs(values) > self.limits["typical_max"]
                if np.any(above_typical):
                    count = np.sum(above_typical)
                    issues.append(ValidationIssue(
                        severity=ValidationSeverity.WARNING,
                        message=f"{count} value(s) exceed typical maximum {self.limits['typical_max']} (unusual but possible)",
                        validator_name="PhysicalPlausibilityValidator",
                        field_name=self.field_name
                    ))

        return ValidationResult(
            passed=not any(i.severity >= ValidationSeverity.ERROR for i in issues),
            issues=issues,
            summary={
                "field": self.field_name,
                "physical_type": self.physical_type,
                "limits": self.limits,
                "values_checked": len(values)
            },
            validator_name="PhysicalPlausibilityValidator",
            data_hash=self._generate_data_hash(data)
        )


class UnitConsistencyValidator(BaseValidator):
    """Validates unit consistency for SI units"""

    SI_UNITS = {
        "length": ["m", "mm", "cm", "km"],
        "force": ["N", "kN", "MN"],
        "moment": ["Nm", "kNm", "MNm", "N.m", "kN.m", "MN.m"],
        "pressure": ["Pa", "kPa", "MPa"],
        "mass": ["kg", "tonne", "t"],
        "time": ["s", "min", "h"]
    }

    IMPERIAL_UNITS = ["ft", "in", "lb", "lbf", "kip"]

    def __init__(
        self,
        check_dimensionality: bool = False,
        cacheable: bool = False
    ):
        """
        Initialize unit consistency validator.

        Args:
            check_dimensionality: Check dimensional consistency
            cacheable: Whether to cache results
        """
        super().__init__(cacheable=cacheable)
        self.check_dimensionality = check_dimensionality

    def validate(self, data: Any) -> ValidationResult:
        """Validate unit consistency"""
        issues = []

        if not isinstance(data, dict):
            return ValidationResult(
                passed=True,
                issues=[],
                summary={},
                validator_name="UnitConsistencyValidator"
            )

        # Check each field with unit specification
        units_found = []
        for field, value in data.items():
            if isinstance(value, dict) and "unit" in value:
                unit = value["unit"]
                units_found.append(unit)

                # Check if unit is valid
                is_si = any(unit in units for units in self.SI_UNITS.values())
                is_imperial = unit in self.IMPERIAL_UNITS

                if not is_si and not is_imperial:
                    issues.append(ValidationIssue(
                        severity=ValidationSeverity.ERROR,
                        message=f"Unknown unit '{unit}' for field '{field}'",
                        validator_name="UnitConsistencyValidator",
                        field_name=field
                    ))

        # Check for mixed unit systems
        has_si = any(
            any(unit in si_units for si_units in self.SI_UNITS.values())
            for unit in units_found
        )
        has_imperial = any(unit in self.IMPERIAL_UNITS for unit in units_found)

        if has_si and has_imperial:
            issues.append(ValidationIssue(
                severity=ValidationSeverity.WARNING,
                message="Mixed SI and Imperial units detected",
                validator_name="UnitConsistencyValidator"
            ))

        return ValidationResult(
            passed=not any(i.severity >= ValidationSeverity.ERROR for i in issues),
            issues=issues,
            summary={
                "units_found": units_found,
                "has_si": has_si,
                "has_imperial": has_imperial
            },
            validator_name="UnitConsistencyValidator",
            data_hash=self._generate_data_hash(data)
        )


class PolarDataValidator(BaseValidator):
    """Validates polar data for 360° coverage and gaps"""

    def __init__(
        self,
        angle_field: str,
        value_field: str,
        max_gap_degrees: float = 30.0,
        check_symmetry: bool = False,
        symmetry_tolerance: float = 0.1,
        cacheable: bool = False
    ):
        """
        Initialize polar data validator.

        Args:
            angle_field: Name of angle field (degrees)
            value_field: Name of value field
            max_gap_degrees: Maximum allowed gap in degrees
            check_symmetry: Check for polar symmetry
            symmetry_tolerance: Relative tolerance for symmetry check
            cacheable: Whether to cache results
        """
        super().__init__(cacheable=cacheable)
        self.angle_field = angle_field
        self.value_field = value_field
        self.max_gap_degrees = max_gap_degrees
        self.check_symmetry = check_symmetry
        self.symmetry_tolerance = symmetry_tolerance

    def validate(self, data: Any) -> ValidationResult:
        """Validate polar data coverage"""
        issues = []

        if not isinstance(data, dict):
            return ValidationResult(
                passed=False,
                issues=[ValidationIssue(
                    severity=ValidationSeverity.ERROR,
                    message="Data must be dict with angle and value fields",
                    validator_name="PolarDataValidator"
                )],
                summary={},
                validator_name="PolarDataValidator"
            )

        angles = np.asarray(data.get(self.angle_field, []))
        values = np.asarray(data.get(self.value_field, []))

        # Check coverage - need to check if angles span full 360 degrees
        angles_normalized = angles % 360
        sorted_angles = np.sort(angles_normalized)

        # Add wrap-around gap (from max back to min)
        gaps_with_wraparound = np.append(np.diff(sorted_angles), (360 - sorted_angles[-1] + sorted_angles[0]))

        # Check if we cover close to 360 degrees (allowing small tolerance)
        # Coverage is good if largest gap is reasonable
        max_gap_total = np.max(gaps_with_wraparound)

        # If the largest gap is too large, we don't have full coverage
        if max_gap_total > 30:  # More than 30 degrees missing
            actual_coverage = 360 - max_gap_total
            issues.append(ValidationIssue(
                severity=ValidationSeverity.ERROR,
                message=f"Incomplete polar coverage: ~{actual_coverage:.1f}° (need ~360°)",
                validator_name="PolarDataValidator"
            ))

        # Check gaps (excluding wrap-around)
        gaps = np.diff(sorted_angles)
        large_gaps = gaps > self.max_gap_degrees

        if np.any(large_gaps):
            gap_indices = np.where(large_gaps)[0]
            for idx in gap_indices:
                gap_size = gaps[idx]
                issues.append(ValidationIssue(
                    severity=ValidationSeverity.WARNING,
                    message=f"Large gap of {gap_size:.1f}° detected between {sorted_angles[idx]:.1f}° and {sorted_angles[idx+1]:.1f}°",
                    validator_name="PolarDataValidator"
                ))

        return ValidationResult(
            passed=len([i for i in issues if i.severity >= ValidationSeverity.ERROR]) == 0,
            issues=issues,
            summary={
                "coverage": 360 - max_gap_total,
                "num_points": len(angles),
                "max_gap": float(np.max(gaps)) if len(gaps) > 0 else 0
            },
            validator_name="PolarDataValidator",
            data_hash=self._generate_data_hash(data)
        )


class TimeSeriesValidator(BaseValidator):
    """Validates time series data for gaps and outliers"""

    def __init__(
        self,
        time_field: str,
        value_field: str,
        detect_gaps: bool = True,
        detect_outliers: bool = True,
        outlier_std_threshold: float = 3.0,
        check_monotonic: bool = True,
        cacheable: bool = False
    ):
        """
        Initialize time series validator.

        Args:
            time_field: Name of time field
            value_field: Name of value field
            detect_gaps: Detect gaps in time series
            detect_outliers: Detect outliers using std threshold
            outlier_std_threshold: Number of std deviations for outlier detection
            check_monotonic: Check if time is monotonically increasing
            cacheable: Whether to cache results
        """
        super().__init__(cacheable=cacheable)
        self.time_field = time_field
        self.value_field = value_field
        self.detect_gaps = detect_gaps
        self.detect_outliers = detect_outliers
        self.outlier_std_threshold = outlier_std_threshold
        self.check_monotonic = check_monotonic

    def validate(self, data: Any) -> ValidationResult:
        """Validate time series data"""
        issues = []

        if not isinstance(data, dict):
            return ValidationResult(
                passed=False,
                issues=[ValidationIssue(
                    severity=ValidationSeverity.ERROR,
                    message="Data must be dict with time and value fields",
                    validator_name="TimeSeriesValidator"
                )],
                summary={},
                validator_name="TimeSeriesValidator"
            )

        time = np.asarray(data.get(self.time_field, []))
        values = np.asarray(data.get(self.value_field, []))

        # Check monotonic time
        if self.check_monotonic:
            if not np.all(np.diff(time) > 0):
                issues.append(ValidationIssue(
                    severity=ValidationSeverity.ERROR,
                    message="Time series is not monotonically increasing",
                    validator_name="TimeSeriesValidator"
                ))

        # Detect gaps
        if self.detect_gaps and len(time) > 1:
            time_diffs = np.diff(time)
            median_diff = np.median(time_diffs)
            large_gaps = time_diffs > median_diff * 5  # 5x median

            if np.any(large_gaps):
                gap_indices = np.where(large_gaps)[0]
                for idx in gap_indices:
                    issues.append(ValidationIssue(
                        severity=ValidationSeverity.ERROR,  # Gaps are errors, not warnings
                        message=f"Gap detected in time series at t={time[idx]:.2f}",
                        validator_name="TimeSeriesValidator",
                        location=f"index {idx}"
                    ))

        # Detect outliers
        if self.detect_outliers and len(values) > 3:
            mean = np.mean(values)
            std = np.std(values)
            outliers = np.abs(values - mean) > self.outlier_std_threshold * std

            if np.any(outliers):
                outlier_indices = np.where(outliers)[0]
                issues.append(ValidationIssue(
                    severity=ValidationSeverity.INFO,
                    message=f"{len(outlier_indices)} outlier(s) detected (>{self.outlier_std_threshold}σ)",
                    validator_name="TimeSeriesValidator"
                ))

        return ValidationResult(
            passed=len([i for i in issues if i.severity >= ValidationSeverity.ERROR]) == 0,
            issues=issues,
            summary={
                "num_points": len(time),
                "time_range": (float(np.min(time)), float(np.max(time))) if len(time) > 0 else None,
                "outliers_detected": int(np.sum(outliers)) if self.detect_outliers and len(values) > 3 else 0
            },
            validator_name="TimeSeriesValidator",
            data_hash=self._generate_data_hash(data)
        )


class ValidationCache:
    """Cache for validation results"""

    def __init__(self, ttl_seconds: int = 3600):
        """
        Initialize validation cache.

        Args:
            ttl_seconds: Time-to-live for cache entries in seconds
        """
        self.ttl_seconds = ttl_seconds
        self._cache: Dict[str, Tuple[ValidationResult, float]] = {}

    def store(self, key: str, result: ValidationResult):
        """Store result in cache with timestamp"""
        self._cache[key] = (result, time.time())

    def retrieve(self, key: str) -> Optional[ValidationResult]:
        """Retrieve result from cache if not expired"""
        if key not in self._cache:
            return None

        result, timestamp = self._cache[key]

        # Check expiration
        if time.time() - timestamp > self.ttl_seconds:
            del self._cache[key]
            return None

        return result

    def generate_key(self, data: Any) -> str:
        """Generate cache key from data"""
        data_str = json.dumps(data, sort_keys=True, default=str)
        return hashlib.sha256(data_str.encode()).hexdigest()


class ValidationPipeline:
    """Orchestrates multiple validators with parallel execution and caching"""

    def __init__(
        self,
        validators: List[BaseValidator],
        fail_fast: bool = False,
        parallel: bool = True,
        max_workers: Optional[int] = None
    ):
        """
        Initialize validation pipeline.

        Args:
            validators: List of validators to execute
            fail_fast: Stop on first CRITICAL error
            parallel: Execute validators in parallel
            max_workers: Maximum thread pool workers (None = CPU count)
        """
        self.validators = validators
        self.fail_fast = fail_fast
        self.parallel = parallel
        self.max_workers = max_workers
        self.cache = ValidationCache()

    def execute(self, data: Any) -> List[ValidationResult]:
        """
        Execute all validators on data.

        Args:
            data: Data to validate

        Returns:
            List of ValidationResult from each validator
        """
        results = []

        if self.parallel:
            results = self._execute_parallel(data)
        else:
            results = self._execute_sequential(data)

        return results

    def _execute_sequential(self, data: Any) -> List[ValidationResult]:
        """Execute validators sequentially"""
        results = []

        for validator in self.validators:
            result = self._run_validator(validator, data)
            results.append(result)

            # Fail fast on critical errors
            if self.fail_fast and result.has_critical_issues():
                break

        return results

    def _execute_parallel(self, data: Any) -> List[ValidationResult]:
        """Execute validators in parallel using ThreadPoolExecutor"""
        results = []

        with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
            future_to_validator = {
                executor.submit(self._run_validator, v, data): v
                for v in self.validators
            }

            for future in as_completed(future_to_validator):
                result = future.result()
                results.append(result)

                # Fail fast on critical errors
                if self.fail_fast and result.has_critical_issues():
                    # Cancel remaining futures
                    for f in future_to_validator:
                        f.cancel()
                    break

        return results

    def _run_validator(self, validator: BaseValidator, data: Any) -> ValidationResult:
        """Run single validator with caching"""
        if validator.cacheable:
            cache_key = self.cache.generate_key(data) + type(validator).__name__
            cached = self.cache.retrieve(cache_key)

            if cached is not None:
                return cached

            result = validator.validate(data)
            self.cache.store(cache_key, result)
            return result
        else:
            return validator.validate(data)

    def filter_by_severity(
        self,
        results: List[ValidationResult],
        severity: ValidationSeverity
    ) -> List[ValidationResult]:
        """Filter results that contain issues of specified severity"""
        return [
            r for r in results
            if any(i.severity == severity for i in r.issues)
        ]

    def aggregate_results(self, results: List[ValidationResult]) -> Dict[str, Any]:
        """Aggregate multiple results into summary statistics"""
        all_issues = [issue for result in results for issue in result.issues]

        return {
            "total_validators": len(results),
            "passed_validators": sum(1 for r in results if r.passed),
            "failed_validators": sum(1 for r in results if not r.passed),
            "total_issues": len(all_issues),
            "critical_issues": sum(1 for i in all_issues if i.severity == ValidationSeverity.CRITICAL),
            "errors": sum(1 for i in all_issues if i.severity == ValidationSeverity.ERROR),
            "warnings": sum(1 for i in all_issues if i.severity == ValidationSeverity.WARNING),
            "info": sum(1 for i in all_issues if i.severity == ValidationSeverity.INFO),
        }


def generate_html_report(
    results: List[ValidationResult],
    output_file: Path,
    export_json: Optional[Path] = None
):
    """
    Generate interactive HTML validation report with Plotly.

    Args:
        results: List of validation results
        output_file: Path to output HTML file
        export_json: Optional path to export results as JSON
    """
    # Aggregate statistics
    all_issues = [issue for result in results for issue in result.issues]

    severity_counts = {
        "INFO": sum(1 for i in all_issues if i.severity == ValidationSeverity.INFO),
        "WARNING": sum(1 for i in all_issues if i.severity == ValidationSeverity.WARNING),
        "ERROR": sum(1 for i in all_issues if i.severity == ValidationSeverity.ERROR),
        "CRITICAL": sum(1 for i in all_issues if i.severity == ValidationSeverity.CRITICAL),
    }

    # Create severity distribution pie chart
    fig = make_subplots(
        rows=2, cols=2,
        subplot_titles=(
            "Severity Distribution",
            "Validator Pass/Fail Status",
            "Issues by Validator",
            "Validation Summary"
        ),
        specs=[
            [{"type": "pie"}, {"type": "bar"}],
            [{"type": "bar"}, {"type": "table"}]
        ]
    )

    # Pie chart - Severity distribution
    fig.add_trace(
        go.Pie(
            labels=list(severity_counts.keys()),
            values=list(severity_counts.values()),
            marker=dict(colors=['#3498db', '#f39c12', '#e74c3c', '#c0392b'])
        ),
        row=1, col=1
    )

    # Bar chart - Validator status
    validator_names = [r.validator_name for r in results]
    validator_status = ['Pass' if r.passed else 'Fail' for r in results]

    pass_count = sum(1 for s in validator_status if s == 'Pass')
    fail_count = sum(1 for s in validator_status if s == 'Fail')

    fig.add_trace(
        go.Bar(
            x=['Pass', 'Fail'],
            y=[pass_count, fail_count],
            marker=dict(color=['#2ecc71', '#e74c3c'])
        ),
        row=1, col=2
    )

    # Bar chart - Issues by validator
    validator_issue_counts = {}
    for result in results:
        validator_issue_counts[result.validator_name] = len(result.issues)

    fig.add_trace(
        go.Bar(
            x=list(validator_issue_counts.keys()),
            y=list(validator_issue_counts.values()),
            marker=dict(color='#3498db')
        ),
        row=2, col=1
    )

    # Table - Summary
    summary_data = [
        ["Total Validators", str(len(results))],
        ["Passed", str(sum(1 for r in results if r.passed))],
        ["Failed", str(sum(1 for r in results if not r.passed))],
        ["Total Issues", str(len(all_issues))],
        ["Critical", str(severity_counts["CRITICAL"])],
        ["Errors", str(severity_counts["ERROR"])],
        ["Warnings", str(severity_counts["WARNING"])],
        ["Info", str(severity_counts["INFO"])]
    ]

    fig.add_trace(
        go.Table(
            header=dict(values=["Metric", "Value"]),
            cells=dict(values=[[row[0] for row in summary_data], [row[1] for row in summary_data]])
        ),
        row=2, col=2
    )

    # Update layout
    fig.update_layout(
        title_text="Validation Report",
        showlegend=True,
        height=800
    )

    # Generate HTML
    html_content = f"""
<!DOCTYPE html>
<html>
<head>
    <title>Validation Report</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <style>
        body {{
            font-family: Arial, sans-serif;
            margin: 20px;
            background-color: #f5f5f5;
        }}
        .header {{
            background-color: #2c3e50;
            color: white;
            padding: 20px;
            border-radius: 5px;
        }}
        .issue {{
            background-color: white;
            padding: 15px;
            margin: 10px 0;
            border-left: 4px solid #3498db;
            border-radius: 3px;
        }}
        .issue.error {{
            border-left-color: #e74c3c;
        }}
        .issue.warning {{
            border-left-color: #f39c12;
        }}
        .issue.critical {{
            border-left-color: #c0392b;
        }}
        .issue.info {{
            border-left-color: #3498db;
        }}
    </style>
</head>
<body>
    <div class="header">
        <h1>Validation Report</h1>
        <p>Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
    </div>

    <div id="plotly-charts"></div>

    <h2>Detailed Issues</h2>
"""

    # Add detailed issues
    for result in results:
        for issue in result.issues:
            severity_class = issue.severity.name.lower()
            html_content += f"""
    <div class="issue {severity_class}">
        <strong>[{issue.severity.name}]</strong> {issue.message}
        <br><small>Validator: {issue.validator_name}</small>
        {f'<br><small>Field: {issue.field_name}</small>' if issue.field_name else ''}
    </div>
"""

    html_content += """
</body>
</html>
"""

    # Write plotly figure to HTML
    fig_html = fig.to_html(include_plotlyjs=False, div_id="plotly-charts")

    # Insert plotly chart into HTML
    html_content = html_content.replace(
        '<div id="plotly-charts"></div>',
        fig_html
    )

    # Write HTML file with UTF-8 encoding
    output_file.parent.mkdir(parents=True, exist_ok=True)
    output_file.write_text(html_content, encoding='utf-8')

    # Export JSON if requested
    if export_json:
        export_json.parent.mkdir(parents=True, exist_ok=True)

        # Custom JSON encoder for numpy types
        def json_encoder(obj):
            if isinstance(obj, (np.integer, np.int64, np.int32)):
                return int(obj)
            if isinstance(obj, (np.floating, np.float64, np.float32)):
                return float(obj)
            if isinstance(obj, np.ndarray):
                return obj.tolist()
            return str(obj)

        json_data = [r.model_dump(mode='python') for r in results]
        export_json.write_text(json.dumps(json_data, indent=2, default=json_encoder), encoding='utf-8')
