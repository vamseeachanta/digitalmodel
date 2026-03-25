"""
Tests for DataValidator service.

The source module lives under a hyphenated directory (orcaflex-dashboard),
which is not a valid Python identifier. We use importlib to load it directly
from the file path instead of a dotted import.
"""

import importlib.util
import math
import os
import sys
import warnings
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
    / "data_validator.py"
)

_spec = importlib.util.spec_from_file_location("data_validator", _MODULE_PATH)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)

DataValidator = _mod.DataValidator
ValidationLevel = _mod.ValidationLevel
ValidationCategory = _mod.ValidationCategory
Severity = _mod.Severity
ValidationIssue = _mod.ValidationIssue
ValidationResult = _mod.ValidationResult
quick_validate = _mod.quick_validate


# ---------------------------------------------------------------------------
# Helpers — reusable DataFrame factories
# ---------------------------------------------------------------------------

def _clean_df(rows: int = 50) -> pd.DataFrame:
    """Return a small, clean DataFrame with realistic column names."""
    rng = np.random.default_rng(42)
    return pd.DataFrame({
        "time": np.linspace(0, 10, rows),
        "Fx_kN": rng.normal(0, 100, rows),
        "Fy_kN": rng.normal(0, 100, rows),
        "Fz_kN": rng.normal(0, 500, rows),
        "Mx_kNm": rng.normal(0, 50, rows),
        "My_kNm": rng.normal(0, 50, rows),
        "Mz_kNm": rng.normal(0, 50, rows),
        "x_m": rng.normal(0, 1, rows),
        "y_m": rng.normal(0, 1, rows),
        "z_m": rng.normal(0, 1, rows),
    })


def _empty_df() -> pd.DataFrame:
    """Return an empty DataFrame with a few columns."""
    return pd.DataFrame({"Fx_kN": pd.Series(dtype=float),
                          "Fy_kN": pd.Series(dtype=float)})


def _nan_df(rows: int = 30) -> pd.DataFrame:
    """Return a DataFrame where every value is NaN."""
    return pd.DataFrame({
        "Fx_kN": [np.nan] * rows,
        "Fy_kN": [np.nan] * rows,
        "Fz_kN": [np.nan] * rows,
    })


def _inf_df(rows: int = 30) -> pd.DataFrame:
    """Return a DataFrame with inf values in force columns."""
    rng = np.random.default_rng(7)
    data = rng.normal(0, 100, (rows, 3))
    data[0, 0] = np.inf
    data[1, 1] = -np.inf
    return pd.DataFrame(data, columns=["Fx_kN", "Fy_kN", "Fz_kN"])


def _exceeds_limits_df(rows: int = 20) -> pd.DataFrame:
    """Return a DataFrame with values that exceed engineering limits."""
    rng = np.random.default_rng(99)
    return pd.DataFrame({
        "Fx_kN": [2e8] * rows,          # > 1e8 force limit
        "Mx_kNm": [-2e9] * rows,        # < -1e9 moment limit
        "x_m": rng.normal(0, 1, rows),  # within limits
    })


# ===================================================================
# Section 1 — Enum smoke tests
# ===================================================================

class TestEnums:
    def test_validation_level_values(self):
        assert ValidationLevel.MINIMAL.value == "minimal"
        assert ValidationLevel.STANDARD.value == "standard"
        assert ValidationLevel.STRICT.value == "strict"

    def test_validation_category_values(self):
        assert len(ValidationCategory) == 6
        expected = {
            "data_integrity", "engineering_limits", "unit_consistency",
            "polar_data", "time_series", "component_consistency",
        }
        assert {c.value for c in ValidationCategory} == expected

    def test_severity_values(self):
        assert Severity.ERROR.value == "error"
        assert Severity.WARNING.value == "warning"
        assert Severity.INFO.value == "info"


# ===================================================================
# Section 2 — Dataclass creation & behaviour
# ===================================================================

class TestValidationIssue:
    def test_create_minimal_issue(self):
        issue = ValidationIssue(
            category=ValidationCategory.DATA_INTEGRITY,
            severity=Severity.ERROR,
            message="test message",
        )
        assert issue.column is None
        assert issue.value_range is None
        assert issue.affected_rows is None
        assert issue.metadata == {}

    def test_create_full_issue(self):
        issue = ValidationIssue(
            category=ValidationCategory.ENGINEERING_LIMITS,
            severity=Severity.WARNING,
            message="out of range",
            column="Fx_kN",
            value_range=(-1e9, 1e9),
            affected_rows=[0, 1, 2],
            metadata={"detail": "exceeded"},
        )
        assert issue.column == "Fx_kN"
        assert issue.value_range == (-1e9, 1e9)
        assert issue.affected_rows == [0, 1, 2]
        assert issue.metadata["detail"] == "exceeded"


class TestValidationResult:
    def _make_result(self, score: float, issues: list = None,
                     total_checks: int = 10, passed_checks: int = 8) -> ValidationResult:
        return ValidationResult(
            overall_score=score,
            category_scores={ValidationCategory.DATA_INTEGRITY: score},
            issues=issues or [],
            statistics={},
            validation_level=ValidationLevel.STANDARD,
            total_checks=total_checks,
            passed_checks=passed_checks,
        )

    def test_is_valid_high_score_no_errors(self):
        result = self._make_result(0.9)
        assert result.is_valid() is True

    def test_is_valid_low_score(self):
        result = self._make_result(0.5)
        assert result.is_valid() is False

    def test_is_valid_exact_threshold(self):
        result = self._make_result(0.7)
        assert result.is_valid() is True

    def test_is_valid_custom_threshold(self):
        result = self._make_result(0.85)
        assert result.is_valid(min_score=0.9) is False
        assert result.is_valid(min_score=0.8) is True

    def test_is_valid_high_score_but_has_error(self):
        error_issue = ValidationIssue(
            category=ValidationCategory.DATA_INTEGRITY,
            severity=Severity.ERROR,
            message="critical failure",
        )
        result = self._make_result(0.95, issues=[error_issue])
        assert result.is_valid() is False

    def test_is_valid_high_score_with_only_warnings(self):
        warning_issue = ValidationIssue(
            category=ValidationCategory.DATA_INTEGRITY,
            severity=Severity.WARNING,
            message="minor concern",
        )
        result = self._make_result(0.9, issues=[warning_issue])
        assert result.is_valid() is True


# ===================================================================
# Section 3 — DataValidator construction
# ===================================================================

class TestDataValidatorInit:
    def test_default_level(self):
        v = DataValidator()
        assert v.validation_level == ValidationLevel.STANDARD

    def test_custom_level(self):
        for lvl in ValidationLevel:
            v = DataValidator(lvl)
            assert v.validation_level == lvl

    def test_initial_stats(self):
        v = DataValidator()
        assert v.validation_stats["total_validations"] == 0
        assert v.validation_stats["total_issues_found"] == 0
        assert v.validation_stats["validation_times"] == []


# ===================================================================
# Section 4 — validate_analysis_data with clean data
# ===================================================================

class TestValidateCleanData:
    def test_clean_data_high_score(self):
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(_clean_df())
        assert result.overall_score > 0.6
        assert result.total_checks > 0
        assert result.passed_checks > 0

    def test_clean_data_has_no_errors(self):
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(_clean_df())
        errors = [i for i in result.issues if i.severity == Severity.ERROR]
        assert len(errors) == 0

    def test_clean_data_is_valid(self):
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(_clean_df())
        assert result.is_valid()

    def test_statistics_populated(self):
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(_clean_df())
        stats = result.statistics
        assert "data_shape" in stats
        assert "numeric_columns" in stats
        assert "missing_data_percentage" in stats
        assert stats["missing_data_percentage"] == 0.0


# ===================================================================
# Section 5 — validate_analysis_data with NaN-filled data
# ===================================================================

class TestValidateNaNData:
    def test_all_nan_produces_issues(self):
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(_nan_df())
        assert len(result.issues) > 0

    def test_all_nan_has_integrity_warnings(self):
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(_nan_df())
        integrity_issues = [
            i for i in result.issues
            if i.category == ValidationCategory.DATA_INTEGRITY
        ]
        assert len(integrity_issues) > 0

    def test_all_nan_lowers_score(self):
        v = DataValidator(ValidationLevel.STANDARD)
        clean_result = v.validate_analysis_data(_clean_df())
        nan_result = v.validate_analysis_data(_nan_df())
        assert nan_result.overall_score < clean_result.overall_score

    def test_missing_data_percentage_is_100(self):
        v = DataValidator(ValidationLevel.MINIMAL)
        result = v.validate_analysis_data(_nan_df())
        assert result.statistics["missing_data_percentage"] == 100.0


# ===================================================================
# Section 6 — validate_analysis_data with infinite values
# ===================================================================

class TestValidateInfData:
    def test_inf_produces_error_issues(self):
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(_inf_df())
        errors = [i for i in result.issues if i.severity == Severity.ERROR]
        assert len(errors) > 0

    def test_inf_detected_in_integrity_category(self):
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(_inf_df())
        inf_errors = [
            i for i in result.issues
            if i.category == ValidationCategory.DATA_INTEGRITY
            and "infinite" in i.message.lower()
        ]
        assert len(inf_errors) >= 1

    def test_inf_makes_result_invalid(self):
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(_inf_df())
        # Has ERROR-severity issues, so is_valid must be False
        assert result.is_valid() is False


# ===================================================================
# Section 7 — validate_analysis_data with empty DataFrame
# ===================================================================

class TestValidateEmptyData:
    """Empty DataFrame triggers a 0/0 division in the source code when computing
    missing_data_percentage.  We suppress the resulting numpy RuntimeWarning
    because it is a known source-code limitation, not a test problem."""

    @pytest.mark.filterwarnings("ignore::RuntimeWarning")
    def test_empty_df_produces_error(self):
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(_empty_df())
        errors = [i for i in result.issues if i.severity == Severity.ERROR]
        assert len(errors) >= 1

    @pytest.mark.filterwarnings("ignore::RuntimeWarning")
    def test_empty_df_error_message(self):
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(_empty_df())
        messages = [i.message for i in result.issues]
        assert any("empty" in m.lower() for m in messages)

    @pytest.mark.filterwarnings("ignore::RuntimeWarning")
    def test_empty_df_is_invalid(self):
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(_empty_df())
        assert result.is_valid() is False


# ===================================================================
# Section 8 — Validation level controls
# ===================================================================

class TestValidationLevels:
    def test_minimal_skips_engineering_limits(self):
        v = DataValidator(ValidationLevel.MINIMAL)
        result = v.validate_analysis_data(_clean_df())
        assert ValidationCategory.ENGINEERING_LIMITS not in result.category_scores

    def test_standard_includes_engineering_limits(self):
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(_clean_df())
        assert ValidationCategory.ENGINEERING_LIMITS in result.category_scores

    def test_strict_includes_engineering_limits(self):
        v = DataValidator(ValidationLevel.STRICT)
        result = v.validate_analysis_data(_clean_df())
        assert ValidationCategory.ENGINEERING_LIMITS in result.category_scores

    def test_minimal_skips_unit_consistency(self):
        v = DataValidator(ValidationLevel.MINIMAL)
        result = v.validate_analysis_data(_clean_df())
        assert ValidationCategory.UNIT_CONSISTENCY not in result.category_scores

    def test_standard_includes_unit_consistency(self):
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(_clean_df())
        assert ValidationCategory.UNIT_CONSISTENCY in result.category_scores

    def test_override_level_in_call(self):
        """The per-call override should take precedence over the constructor level."""
        v = DataValidator(ValidationLevel.MINIMAL)
        result = v.validate_analysis_data(
            _clean_df(), validation_level=ValidationLevel.STANDARD
        )
        assert result.validation_level == ValidationLevel.STANDARD
        assert ValidationCategory.ENGINEERING_LIMITS in result.category_scores

    def test_strict_more_checks_than_minimal(self):
        df = _clean_df()
        strict_result = DataValidator(ValidationLevel.STRICT).validate_analysis_data(df)
        minimal_result = DataValidator(ValidationLevel.MINIMAL).validate_analysis_data(df)
        assert strict_result.total_checks >= minimal_result.total_checks


# ===================================================================
# Section 9 — Engineering limits exceeded
# ===================================================================

class TestEngineeringLimitsExceeded:
    def test_exceeding_limits_produces_errors(self):
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(_exceeds_limits_df())
        eng_errors = [
            i for i in result.issues
            if i.category == ValidationCategory.ENGINEERING_LIMITS
            and i.severity == Severity.ERROR
        ]
        assert len(eng_errors) >= 1

    def test_strict_adds_typical_limit_warnings(self):
        """STRICT mode checks 'typical' limits in addition to absolute limits."""
        df = pd.DataFrame({
            "Fx_kN": [5e6] * 10,  # above typical_max (1e6) but below absolute max (1e8)
        })
        v = DataValidator(ValidationLevel.STRICT)
        result = v.validate_analysis_data(df)
        eng_warnings = [
            i for i in result.issues
            if i.category == ValidationCategory.ENGINEERING_LIMITS
            and i.severity == Severity.WARNING
        ]
        assert len(eng_warnings) >= 1


# ===================================================================
# Section 10 — quick_validate convenience function
# ===================================================================

class TestQuickValidate:
    def test_returns_validation_result(self):
        result = quick_validate(_clean_df())
        assert isinstance(result, ValidationResult)

    def test_default_level_is_standard(self):
        result = quick_validate(_clean_df())
        assert result.validation_level == ValidationLevel.STANDARD

    def test_custom_level(self):
        result = quick_validate(_clean_df(), validation_level=ValidationLevel.STRICT)
        assert result.validation_level == ValidationLevel.STRICT


# ===================================================================
# Section 11 — Validator stats accumulation
# ===================================================================

class TestValidatorStats:
    def test_stats_count_validations(self):
        v = DataValidator()
        v.validate_analysis_data(_clean_df())
        v.validate_analysis_data(_clean_df())
        stats = v.get_validator_stats()
        assert stats["total_validations"] == 2
        assert len(stats["validation_times"]) == 2

    @pytest.mark.filterwarnings("ignore::RuntimeWarning")
    def test_stats_tracks_issues(self):
        v = DataValidator()
        v.validate_analysis_data(_empty_df())
        stats = v.get_validator_stats()
        assert stats["total_issues_found"] >= 1

    def test_stats_has_average_time(self):
        v = DataValidator()
        v.validate_analysis_data(_clean_df())
        stats = v.get_validator_stats()
        assert "average_validation_time" in stats
        assert stats["average_validation_time"] >= 0


# ===================================================================
# Section 12 — Validation summary
# ===================================================================

class TestValidationSummary:
    def test_summary_contains_score(self):
        v = DataValidator()
        result = v.validate_analysis_data(_clean_df())
        summary = v.get_validation_summary(result)
        assert "Validation Score" in summary

    def test_summary_with_no_issues(self):
        v = DataValidator(ValidationLevel.MINIMAL)
        # Use a clean df that should pass minimal validation with no issues
        df = pd.DataFrame({"a": [1, 2, 3], "b": [4, 5, 6]})
        result = v.validate_analysis_data(df)
        if not result.issues:
            summary = v.get_validation_summary(result)
            assert "No issues found" in summary

    @pytest.mark.filterwarnings("ignore::RuntimeWarning")
    def test_summary_with_errors(self):
        v = DataValidator()
        result = v.validate_analysis_data(_empty_df())
        summary = v.get_validation_summary(result)
        assert "error" in summary.lower()


# ===================================================================
# Section 13 — Time series validation
# ===================================================================

class TestTimeSeriesValidation:
    def test_monotonic_time_passes(self):
        df = _clean_df()  # time column is linspace => monotonic
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(df)
        ts_issues = [
            i for i in result.issues
            if i.category == ValidationCategory.TIME_SERIES
        ]
        # Monotonic time should produce no time-series errors
        ts_errors = [i for i in ts_issues if i.severity == Severity.ERROR]
        assert len(ts_errors) == 0

    def test_non_monotonic_time_warns(self):
        df = pd.DataFrame({
            "time": [0, 2, 1, 3, 4],
            "Fx_kN": [1, 2, 3, 4, 5],
        })
        v = DataValidator(ValidationLevel.STANDARD)
        result = v.validate_analysis_data(df)
        ts_issues = [
            i for i in result.issues
            if i.category == ValidationCategory.TIME_SERIES
            and "monotonic" in i.message.lower()
        ]
        assert len(ts_issues) >= 1
