"""
ABOUTME: Comprehensive unit tests for Pydantic reporting models
ABOUTME: Tests ParameterSet, ValidationResult, AnalysisResult, ReportMetadata,
ABOUTME: StandardReport, and ParametricStudy with creation, methods, and serialization
"""

import json
import pytest
from datetime import datetime
from pathlib import Path

from digitalmodel.visualization.reporting.models import (
    AnalysisResult,
    ParameterSet,
    ParametricStudy,
    ReportMetadata,
    StandardReport,
    ValidationResult,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _make_metadata(**overrides) -> ReportMetadata:
    """Build a ReportMetadata with sensible defaults, allowing overrides."""
    defaults = {
        "module": "test_module",
        "analysis_type": "test_analysis",
        "execution_time_seconds": 1.0,
    }
    defaults.update(overrides)
    return ReportMetadata(**defaults)


def _make_report(**meta_overrides) -> StandardReport:
    """Build a minimal StandardReport."""
    return StandardReport(metadata=_make_metadata(**meta_overrides))


# ===========================================================================
# 1. ParameterSet
# ===========================================================================

class TestParameterSet:
    """Tests for ParameterSet model."""

    def test_parameter_set_creation_all_fields(self):
        """Create ParameterSet with all fields populated."""
        param = ParameterSet(
            name="water_depth",
            value=1500.0,
            unit="m",
            description="Water depth at site",
        )
        assert param.name == "water_depth"
        assert param.value == 1500.0
        assert param.unit == "m"
        assert param.description == "Water depth at site"

    def test_parameter_set_creation_required_only(self):
        """Create ParameterSet with only required fields; optional are None."""
        param = ParameterSet(name="count", value=7)
        assert param.name == "count"
        assert param.value == 7
        assert param.unit is None
        assert param.description is None

    def test_parameter_set_value_type_float(self):
        """Float values are preserved."""
        param = ParameterSet(name="pi", value=3.14159)
        assert isinstance(param.value, float)
        assert param.value == pytest.approx(3.14159)

    def test_parameter_set_value_type_int(self):
        """Integer values are preserved."""
        param = ParameterSet(name="n_iterations", value=100)
        assert isinstance(param.value, int)
        assert param.value == 100

    def test_parameter_set_value_type_str(self):
        """String values are preserved."""
        param = ParameterSet(name="material", value="S355")
        assert isinstance(param.value, str)
        assert param.value == "S355"

    def test_parameter_set_value_type_bool(self):
        """Boolean values are preserved."""
        param = ParameterSet(name="include_buoyancy", value=True)
        assert param.value is True

    def test_parameter_set_json_round_trip(self):
        """Serialize to JSON and back; values survive round-trip."""
        param = ParameterSet(
            name="safety_factor", value=1.5, unit="-", description="SF"
        )
        json_str = param.model_dump_json()
        restored = ParameterSet.model_validate_json(json_str)
        assert restored.name == param.name
        assert restored.value == param.value
        assert restored.unit == param.unit
        assert restored.description == param.description


# ===========================================================================
# 2. ValidationResult
# ===========================================================================

class TestValidationResult:
    """Tests for ValidationResult model."""

    def test_validation_result_creation(self):
        """Create ValidationResult with all fields."""
        vr = ValidationResult(
            check_name="stress_limit",
            passed=True,
            message="Within allowable",
            severity="info",
            details={"max": 280, "allowable": 300},
        )
        assert vr.check_name == "stress_limit"
        assert vr.passed is True
        assert vr.message == "Within allowable"
        assert vr.severity == "info"
        assert vr.details == {"max": 280, "allowable": 300}

    def test_validation_result_default_severity(self):
        """Default severity is 'info'."""
        vr = ValidationResult(
            check_name="check", passed=True, message="ok"
        )
        assert vr.severity == "info"

    @pytest.mark.parametrize("level", ["info", "warning", "error", "critical"])
    def test_validation_result_severity_levels(self, level):
        """All four severity literals are accepted."""
        vr = ValidationResult(
            check_name="check", passed=False, message="msg", severity=level
        )
        assert vr.severity == level

    def test_validation_result_invalid_severity_raises(self):
        """Invalid severity literal raises ValidationError."""
        with pytest.raises(Exception):
            ValidationResult(
                check_name="check",
                passed=False,
                message="msg",
                severity="fatal",
            )

    def test_validation_result_details_none_by_default(self):
        """Details default to None when not provided."""
        vr = ValidationResult(
            check_name="check", passed=True, message="ok"
        )
        assert vr.details is None

    def test_validation_result_details_nested_dict(self):
        """Details can contain nested structures."""
        nested = {"section": {"thickness": 12.5, "valid": True}}
        vr = ValidationResult(
            check_name="wall_check",
            passed=False,
            message="Too thin",
            severity="error",
            details=nested,
        )
        assert vr.details["section"]["thickness"] == 12.5


# ===========================================================================
# 3. AnalysisResult
# ===========================================================================

class TestAnalysisResult:
    """Tests for AnalysisResult model."""

    def test_analysis_result_creation_scalar(self):
        """Create AnalysisResult with scalar float value."""
        ar = AnalysisResult(
            metric_name="max_stress",
            value=250.5,
            unit="MPa",
            description="Peak von Mises",
            passed=True,
            threshold=300.0,
        )
        assert ar.metric_name == "max_stress"
        assert ar.value == 250.5
        assert ar.unit == "MPa"
        assert ar.passed is True
        assert ar.threshold == 300.0

    def test_analysis_result_value_list(self):
        """Value can be a list."""
        ar = AnalysisResult(
            metric_name="stress_profile", value=[100.0, 200.0, 250.0]
        )
        assert ar.value == [100.0, 200.0, 250.0]

    def test_analysis_result_value_dict(self):
        """Value can be a dict."""
        ar = AnalysisResult(
            metric_name="tensor",
            value={"xx": 100, "yy": 200, "xy": 50},
        )
        assert ar.value["xx"] == 100

    def test_analysis_result_optional_fields_default_none(self):
        """Optional fields default to None."""
        ar = AnalysisResult(metric_name="m", value=1)
        assert ar.unit is None
        assert ar.description is None
        assert ar.passed is None
        assert ar.threshold is None
        assert ar.validation is None

    def test_analysis_result_nested_validation(self):
        """AnalysisResult can embed a ValidationResult."""
        vr = ValidationResult(
            check_name="inner", passed=True, message="ok"
        )
        ar = AnalysisResult(
            metric_name="stress",
            value=200.0,
            validation=vr,
        )
        assert ar.validation is not None
        assert ar.validation.check_name == "inner"
        assert ar.validation.passed is True


# ===========================================================================
# 4. ReportMetadata
# ===========================================================================

class TestReportMetadata:
    """Tests for ReportMetadata model."""

    def test_report_metadata_creation(self):
        """Create with required fields; check defaults."""
        meta = ReportMetadata(
            module="fatigue",
            analysis_type="rainflow",
            execution_time_seconds=12.3,
        )
        assert meta.module == "fatigue"
        assert meta.analysis_type == "rainflow"
        assert meta.execution_time_seconds == 12.3

    def test_report_metadata_default_version(self):
        """Default version is '1.0.0'."""
        meta = _make_metadata()
        assert meta.version == "1.0.0"

    def test_report_metadata_default_generated_by(self):
        """Default generated_by is 'digitalmodel'."""
        meta = _make_metadata()
        assert meta.generated_by == "digitalmodel"

    def test_report_metadata_default_status(self):
        """Default status is 'success'."""
        meta = _make_metadata()
        assert meta.status == "success"

    def test_report_metadata_generated_at_is_datetime(self):
        """generated_at is populated with a datetime."""
        meta = _make_metadata()
        assert isinstance(meta.generated_at, datetime)

    def test_report_metadata_custom_status(self):
        """Status accepts 'warning' and 'error'."""
        for status in ("success", "warning", "error"):
            meta = _make_metadata(status=status)
            assert meta.status == status

    def test_report_metadata_invalid_status_raises(self):
        """Invalid status literal raises."""
        with pytest.raises(Exception):
            _make_metadata(status="unknown")


# ===========================================================================
# 5. StandardReport
# ===========================================================================

class TestStandardReport:
    """Tests for StandardReport model."""

    def test_standard_report_creation_minimal(self):
        """Minimal report has empty collections."""
        report = _make_report()
        assert len(report.parameters) == 0
        assert len(report.results) == 0
        assert len(report.validations) == 0
        assert len(report.plots) == 0
        assert len(report.attachments) == 0
        assert len(report.notes) == 0

    def test_standard_report_add_parameter(self):
        """add_parameter appends a ParameterSet."""
        report = _make_report()
        report.add_parameter("depth", 1200.0, unit="m", description="Water depth")
        assert len(report.parameters) == 1
        p = report.parameters[0]
        assert p.name == "depth"
        assert p.value == 1200.0
        assert p.unit == "m"
        assert p.description == "Water depth"

    def test_standard_report_add_result(self):
        """add_result appends an AnalysisResult."""
        report = _make_report()
        report.add_result(
            "max_stress", 250.0, unit="MPa", passed=True, threshold=300.0
        )
        assert len(report.results) == 1
        r = report.results[0]
        assert r.metric_name == "max_stress"
        assert r.passed is True

    def test_standard_report_add_validation(self):
        """add_validation appends a ValidationResult."""
        report = _make_report()
        report.add_validation(
            "check_a", True, "All good", severity="info",
            details={"margin": 0.15},
        )
        assert len(report.validations) == 1
        v = report.validations[0]
        assert v.check_name == "check_a"
        assert v.details == {"margin": 0.15}

    def test_standard_report_add_plot(self):
        """add_plot appends plot metadata dict."""
        report = _make_report()
        report.add_plot(
            "stress_plot.png", "scatter", description="Stress vs depth",
            dpi=150,
        )
        assert len(report.plots) == 1
        plot = report.plots[0]
        assert plot["filename"] == "stress_plot.png"
        assert plot["plot_type"] == "scatter"
        assert plot["description"] == "Stress vs depth"
        assert plot["dpi"] == 150

    def test_standard_report_get_summary_counts(self):
        """get_summary returns correct counts and pass/fail tallies."""
        report = _make_report(
            module="structural",
            analysis_type="stress",
            execution_time_seconds=5.5,
            status="warning",
        )
        report.add_parameter("a", 1)
        report.add_parameter("b", 2)
        report.add_parameter("c", 3)
        report.add_result("r1", 10.0)
        report.add_result("r2", 20.0)
        report.add_validation("v1", True, "pass")
        report.add_validation("v2", False, "fail")
        report.add_validation("v3", True, "pass")
        report.add_plot("p.png", "line")

        summary = report.get_summary()

        assert summary["module"] == "structural"
        assert summary["analysis_type"] == "stress"
        assert summary["status"] == "warning"
        assert summary["execution_time_seconds"] == 5.5
        assert summary["parameter_count"] == 3
        assert summary["result_count"] == 2
        assert summary["validation_count"] == 3
        assert summary["validations_passed"] == 2
        assert summary["validations_failed"] == 1
        assert summary["plot_count"] == 1

    def test_standard_report_to_dict(self):
        """to_dict returns a plain dict with expected keys."""
        report = _make_report()
        report.add_parameter("x", 42)
        d = report.to_dict()
        assert isinstance(d, dict)
        assert "metadata" in d
        assert "parameters" in d
        assert "results" in d
        assert "validations" in d
        assert "plots" in d
        assert "attachments" in d
        assert "notes" in d
        assert len(d["parameters"]) == 1

    def test_standard_report_to_json_parseable(self):
        """to_json produces valid, parseable JSON."""
        report = _make_report()
        report.add_parameter("sf", 1.5)
        report.add_result("stress", 200.0, unit="MPa")
        json_str = report.to_json()
        data = json.loads(json_str)
        assert data["metadata"]["module"] == "test_module"
        assert len(data["parameters"]) == 1
        assert len(data["results"]) == 1

    def test_standard_report_json_round_trip(self):
        """Serialize to JSON and reconstruct; data is equivalent."""
        report = _make_report(
            module="fatigue",
            analysis_type="sn_curve",
            execution_time_seconds=7.7,
        )
        report.add_parameter("material", "S355")
        report.add_result("damage", 0.45, unit="-", passed=True, threshold=1.0)
        report.add_validation("dmg_check", True, "OK")

        json_str = report.to_json()
        restored = StandardReport.model_validate_json(json_str)

        assert restored.metadata.module == "fatigue"
        assert len(restored.parameters) == 1
        assert restored.parameters[0].value == "S355"
        assert len(restored.results) == 1
        assert restored.results[0].value == 0.45
        assert len(restored.validations) == 1
        assert restored.validations[0].passed is True

    def test_standard_report_save_json(self, tmp_path):
        """save_json writes a valid JSON file to disk."""
        report = _make_report()
        report.add_parameter("a", 1)
        report.add_result("b", 2.0)

        filepath = tmp_path / "output" / "report.json"
        report.save_json(filepath)

        assert filepath.exists()
        with open(filepath) as f:
            data = json.load(f)
        assert data["metadata"]["module"] == "test_module"
        assert len(data["parameters"]) == 1
        assert len(data["results"]) == 1

    def test_standard_report_save_json_creates_parent_dirs(self, tmp_path):
        """save_json creates intermediate directories."""
        report = _make_report()
        deep_path = tmp_path / "a" / "b" / "c" / "report.json"
        report.save_json(deep_path)
        assert deep_path.exists()


# ===========================================================================
# 6. ParametricStudy
# ===========================================================================

class TestParametricStudy:
    """Tests for ParametricStudy model."""

    @staticmethod
    def _make_study_report(sf_value: float) -> StandardReport:
        """Helper: build a report with safety_factor param and two results."""
        report = _make_report(
            module="structural",
            analysis_type="parametric",
        )
        report.add_parameter("safety_factor", sf_value, unit="-")
        report.add_result("max_stress", 300.0 / sf_value, unit="MPa")
        report.add_result("displacement", 10.0 * sf_value, unit="mm")
        return report

    def test_parametric_study_creation(self):
        """Create ParametricStudy with required fields."""
        study = ParametricStudy(
            study_name="sf_sweep",
            parameter_name="safety_factor",
        )
        assert study.study_name == "sf_sweep"
        assert study.parameter_name == "safety_factor"
        assert study.description is None
        assert len(study.reports) == 0
        assert isinstance(study.created_at, datetime)

    def test_parametric_study_add_report(self):
        """add_report grows the reports list."""
        study = ParametricStudy(
            study_name="s", parameter_name="safety_factor"
        )
        study.add_report(self._make_study_report(1.2))
        study.add_report(self._make_study_report(1.5))
        assert len(study.reports) == 2

    def test_parametric_study_get_parameter_values(self):
        """get_parameter_values extracts the varied parameter from each report."""
        study = ParametricStudy(
            study_name="s", parameter_name="safety_factor"
        )
        for sf in [1.2, 1.5, 2.0]:
            study.add_report(self._make_study_report(sf))

        values = study.get_parameter_values()
        assert values == [1.2, 1.5, 2.0]

    def test_parametric_study_get_parameter_values_empty(self):
        """get_parameter_values returns empty list when no reports."""
        study = ParametricStudy(
            study_name="s", parameter_name="safety_factor"
        )
        assert study.get_parameter_values() == []

    def test_parametric_study_get_comparison_table_all_metrics(self):
        """get_comparison_table with no filter returns all metrics."""
        study = ParametricStudy(
            study_name="s", parameter_name="safety_factor"
        )
        study.add_report(self._make_study_report(1.5))
        study.add_report(self._make_study_report(2.0))

        table = study.get_comparison_table()

        assert "safety_factor" in table
        assert "max_stress" in table
        assert "displacement" in table
        assert table["safety_factor"] == [1.5, 2.0]
        assert len(table["max_stress"]) == 2
        assert len(table["displacement"]) == 2
        # Verify computed values
        assert table["max_stress"][0] == pytest.approx(200.0)
        assert table["max_stress"][1] == pytest.approx(150.0)

    def test_parametric_study_get_comparison_table_filtered(self):
        """get_comparison_table with metric_names filters columns."""
        study = ParametricStudy(
            study_name="s", parameter_name="safety_factor"
        )
        study.add_report(self._make_study_report(1.5))
        study.add_report(self._make_study_report(2.0))

        table = study.get_comparison_table(metric_names=["max_stress"])

        assert "max_stress" in table
        assert "displacement" not in table

    def test_parametric_study_get_comparison_table_missing_metric(self):
        """When a metric is absent from a report, None is placed in the table."""
        study = ParametricStudy(
            study_name="s", parameter_name="safety_factor"
        )
        # First report has both metrics
        study.add_report(self._make_study_report(1.5))
        # Second report has only one metric
        report2 = _make_report()
        report2.add_parameter("safety_factor", 2.0)
        report2.add_result("max_stress", 150.0)
        study.add_report(report2)

        table = study.get_comparison_table()
        # displacement present in first, missing in second
        assert table["displacement"][0] == pytest.approx(15.0)
        assert table["displacement"][1] is None

    def test_parametric_study_get_summary(self):
        """get_summary returns expected keys and values."""
        study = ParametricStudy(
            study_name="sf_sweep",
            description="Sweep from 1.2 to 2.0",
            parameter_name="safety_factor",
        )
        study.add_report(self._make_study_report(1.2))
        study.add_report(self._make_study_report(2.0))

        summary = study.get_summary()

        assert summary["study_name"] == "sf_sweep"
        assert summary["parameter_name"] == "safety_factor"
        assert summary["report_count"] == 2
        assert summary["parameter_values"] == [1.2, 2.0]
        assert "created_at" in summary

    def test_parametric_study_to_json_parseable(self):
        """to_json produces valid JSON."""
        study = ParametricStudy(
            study_name="s", parameter_name="safety_factor"
        )
        study.add_report(self._make_study_report(1.5))
        json_str = study.to_json()
        data = json.loads(json_str)
        assert data["study_name"] == "s"
        assert len(data["reports"]) == 1

    def test_parametric_study_save_json(self, tmp_path):
        """save_json writes valid JSON to disk."""
        study = ParametricStudy(
            study_name="sf_study",
            parameter_name="safety_factor",
        )
        study.add_report(self._make_study_report(1.5))

        filepath = tmp_path / "study.json"
        study.save_json(filepath)

        assert filepath.exists()
        with open(filepath) as f:
            data = json.load(f)
        assert data["study_name"] == "sf_study"
        assert len(data["reports"]) == 1

    def test_parametric_study_save_json_creates_parent_dirs(self, tmp_path):
        """save_json creates intermediate directories."""
        study = ParametricStudy(
            study_name="s", parameter_name="p"
        )
        deep_path = tmp_path / "x" / "y" / "study.json"
        study.save_json(deep_path)
        assert deep_path.exists()
