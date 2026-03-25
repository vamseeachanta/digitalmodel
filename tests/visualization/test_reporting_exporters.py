"""
ABOUTME: Comprehensive unit tests for reporting export functions
ABOUTME: Tests export_to_json, export_to_csv, export_to_html, export_all_formats,
ABOUTME: and export_parametric_study_html with file creation, content validation, and edge cases
"""

import csv
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
from digitalmodel.visualization.reporting.exporters import (
    export_to_json,
    export_to_csv,
    export_to_html,
    export_all_formats,
    export_parametric_study_html,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _make_metadata(**overrides) -> ReportMetadata:
    """Build a ReportMetadata with sensible defaults, allowing overrides."""
    defaults = {
        "module": "test_module",
        "analysis_type": "stress_analysis",
        "execution_time_seconds": 2.5,
        "status": "success",
    }
    defaults.update(overrides)
    return ReportMetadata(**defaults)


def _make_report(**meta_overrides) -> StandardReport:
    """Build a minimal StandardReport with only metadata."""
    return StandardReport(metadata=_make_metadata(**meta_overrides))


def _make_populated_report() -> StandardReport:
    """Build a fully populated StandardReport with parameters, results, and validations."""
    report = _make_report(
        module="structural_analysis",
        analysis_type="fatigue_check",
        execution_time_seconds=12.5,
        status="warning",
    )
    report.add_parameter("safety_factor", 1.5, unit="-", description="Global SF")
    report.add_parameter("water_depth", 1200.0, unit="m", description="Site depth")
    report.add_parameter("material", "S355", unit="-", description="Steel grade")

    report.add_result(
        "max_stress", 250.5, unit="MPa",
        description="Peak von Mises stress", passed=True, threshold=300.0,
    )
    report.add_result(
        "fatigue_damage", 0.85, unit="-",
        description="Cumulative damage ratio", passed=True, threshold=1.0,
    )
    report.add_result(
        "displacement", 45.3, unit="mm",
        description="Max lateral displacement", passed=False, threshold=40.0,
    )

    report.add_validation("stress_check", True, "Within allowable limits", severity="info")
    report.add_validation("fatigue_check", True, "Damage below 1.0", severity="info")
    report.add_validation(
        "displacement_check", False, "Exceeds 40mm limit", severity="error",
    )

    report.add_plot("stress_plot.png", "scatter", description="Stress distribution")
    return report


# ===========================================================================
# 1. export_to_json
# ===========================================================================

class TestExportToJson:
    """Tests for the export_to_json function."""

    def test_json_export_creates_file(self, tmp_path):
        """export_to_json creates a JSON file at the specified path."""
        report = _make_report()
        filepath = tmp_path / "report.json"
        result = export_to_json(report, filepath)

        assert filepath.exists()
        assert result == filepath

    def test_json_export_returns_path(self, tmp_path):
        """Return value is the Path to the written file."""
        report = _make_report()
        filepath = tmp_path / "output.json"
        result = export_to_json(report, filepath)

        assert isinstance(result, Path)
        assert result == filepath

    def test_json_export_content_is_valid_json(self, tmp_path):
        """Exported file contains valid, parseable JSON."""
        report = _make_populated_report()
        filepath = tmp_path / "report.json"
        export_to_json(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            data = json.load(f)

        assert isinstance(data, dict)
        assert "metadata" in data
        assert "parameters" in data
        assert "results" in data

    def test_json_export_metadata_matches(self, tmp_path):
        """JSON metadata fields match the original report."""
        report = _make_report(module="mooring", analysis_type="catenary")
        filepath = tmp_path / "report.json"
        export_to_json(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            data = json.load(f)

        assert data["metadata"]["module"] == "mooring"
        assert data["metadata"]["analysis_type"] == "catenary"

    def test_json_export_parameters_preserved(self, tmp_path):
        """Parameters are correctly serialized in the JSON output."""
        report = _make_populated_report()
        filepath = tmp_path / "report.json"
        export_to_json(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            data = json.load(f)

        params = data["parameters"]
        assert len(params) == 3
        names = [p["name"] for p in params]
        assert "safety_factor" in names
        assert "water_depth" in names
        assert "material" in names

    def test_json_export_creates_parent_directories(self, tmp_path):
        """export_to_json creates intermediate directories if needed."""
        report = _make_report()
        deep_path = tmp_path / "a" / "b" / "c" / "report.json"
        export_to_json(report, deep_path)

        assert deep_path.exists()

    def test_json_export_empty_report(self, tmp_path):
        """Minimal report with no parameters/results/validations exports cleanly."""
        report = _make_report()
        filepath = tmp_path / "empty.json"
        export_to_json(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            data = json.load(f)

        assert data["parameters"] == []
        assert data["results"] == []
        assert data["validations"] == []

    def test_json_export_overwrite_existing(self, tmp_path):
        """Exporting to an existing file overwrites it."""
        filepath = tmp_path / "report.json"

        # Write first report
        report1 = _make_report(module="first_module")
        export_to_json(report1, filepath)

        # Overwrite with second report
        report2 = _make_report(module="second_module")
        export_to_json(report2, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            data = json.load(f)

        assert data["metadata"]["module"] == "second_module"


# ===========================================================================
# 2. export_to_csv
# ===========================================================================

class TestExportToCsv:
    """Tests for the export_to_csv function."""

    def test_csv_export_creates_file(self, tmp_path):
        """export_to_csv creates a CSV file at the specified path."""
        report = _make_populated_report()
        filepath = tmp_path / "report.csv"
        result = export_to_csv(report, filepath)

        assert filepath.exists()
        assert result == filepath

    def test_csv_export_metadata_section(self, tmp_path):
        """CSV contains METADATA section with correct fields."""
        report = _make_report(module="fatigue", analysis_type="sn_curve")
        filepath = tmp_path / "report.csv"
        export_to_csv(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            reader = csv.reader(f)
            rows = list(reader)

        assert rows[0] == ["METADATA"]
        assert rows[1][0] == "Module"
        assert rows[1][1] == "fatigue"
        assert rows[2][0] == "Analysis Type"
        assert rows[2][1] == "sn_curve"

    def test_csv_export_parameters_section(self, tmp_path):
        """CSV includes PARAMETERS section with header and data rows."""
        report = _make_populated_report()
        filepath = tmp_path / "report.csv"
        export_to_csv(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "PARAMETERS" in content
        assert "safety_factor" in content
        assert "water_depth" in content
        assert "S355" in content

    def test_csv_export_results_section(self, tmp_path):
        """CSV includes RESULTS section with metric names and values."""
        report = _make_populated_report()
        filepath = tmp_path / "report.csv"
        export_to_csv(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "RESULTS" in content
        assert "max_stress" in content
        assert "fatigue_damage" in content

    def test_csv_export_validations_section(self, tmp_path):
        """CSV includes VALIDATIONS section with check results."""
        report = _make_populated_report()
        filepath = tmp_path / "report.csv"
        export_to_csv(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "VALIDATIONS" in content
        assert "stress_check" in content
        assert "displacement_check" in content

    def test_csv_export_exclude_parameters(self, tmp_path):
        """Setting include_parameters=False omits the parameters section."""
        report = _make_populated_report()
        filepath = tmp_path / "report.csv"
        export_to_csv(report, filepath, include_parameters=False)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "PARAMETERS" not in content
        # Results should still be present
        assert "RESULTS" in content

    def test_csv_export_exclude_results(self, tmp_path):
        """Setting include_results=False omits the results section."""
        report = _make_populated_report()
        filepath = tmp_path / "report.csv"
        export_to_csv(report, filepath, include_results=False)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "RESULTS" not in content
        assert "PARAMETERS" in content

    def test_csv_export_exclude_validations(self, tmp_path):
        """Setting include_validations=False omits the validations section."""
        report = _make_populated_report()
        filepath = tmp_path / "report.csv"
        export_to_csv(report, filepath, include_validations=False)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "VALIDATIONS" not in content
        assert "PARAMETERS" in content

    def test_csv_export_creates_parent_directories(self, tmp_path):
        """export_to_csv creates intermediate directories."""
        report = _make_report()
        deep_path = tmp_path / "x" / "y" / "report.csv"
        export_to_csv(report, deep_path)

        assert deep_path.exists()

    def test_csv_export_complex_value_serialized(self, tmp_path):
        """List and dict result values are JSON-serialized in CSV."""
        report = _make_report()
        report.add_result("profile", [10.0, 20.0, 30.0], unit="MPa")
        filepath = tmp_path / "report.csv"
        export_to_csv(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        # The list should appear as a JSON string in the CSV
        assert "[10.0, 20.0, 30.0]" in content

    def test_csv_export_empty_report_no_sections(self, tmp_path):
        """Minimal report produces CSV with only METADATA section."""
        report = _make_report()
        filepath = tmp_path / "empty.csv"
        export_to_csv(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "METADATA" in content
        assert "PARAMETERS" not in content
        assert "RESULTS" not in content
        assert "VALIDATIONS" not in content


# ===========================================================================
# 3. export_to_html
# ===========================================================================

class TestExportToHtml:
    """Tests for the export_to_html function."""

    def test_html_export_creates_file(self, tmp_path):
        """export_to_html creates an HTML file."""
        report = _make_populated_report()
        filepath = tmp_path / "report.html"
        result = export_to_html(report, filepath)

        assert filepath.exists()
        assert result == filepath

    def test_html_export_contains_doctype(self, tmp_path):
        """HTML output starts with DOCTYPE declaration."""
        report = _make_report()
        filepath = tmp_path / "report.html"
        export_to_html(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "<!DOCTYPE html>" in content

    def test_html_export_contains_title(self, tmp_path):
        """HTML title tag contains the expected report title."""
        report = _make_report(module="mooring", analysis_type="catenary")
        filepath = tmp_path / "report.html"
        export_to_html(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "<title>mooring - catenary</title>" in content

    def test_html_export_custom_title(self, tmp_path):
        """Custom title overrides the default module-analysis title."""
        report = _make_report()
        filepath = tmp_path / "report.html"
        export_to_html(report, filepath, title="My Custom Report")

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "<title>My Custom Report</title>" in content

    def test_html_export_contains_metadata(self, tmp_path):
        """HTML includes report metadata fields."""
        report = _make_report(module="structural", analysis_type="buckling")
        filepath = tmp_path / "report.html"
        export_to_html(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "structural" in content
        assert "buckling" in content

    def test_html_export_contains_parameters_table(self, tmp_path):
        """HTML includes a parameters table when parameters are present."""
        report = _make_populated_report()
        filepath = tmp_path / "report.html"
        export_to_html(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "Input Parameters" in content
        assert "safety_factor" in content
        assert "water_depth" in content

    def test_html_export_contains_results_table(self, tmp_path):
        """HTML includes an analysis results table."""
        report = _make_populated_report()
        filepath = tmp_path / "report.html"
        export_to_html(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "Analysis Results" in content
        assert "max_stress" in content
        assert "fatigue_damage" in content

    def test_html_export_pass_fail_indicators(self, tmp_path):
        """HTML shows PASS and FAIL indicators for results."""
        report = _make_populated_report()
        filepath = tmp_path / "report.html"
        export_to_html(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "PASS" in content
        assert "FAIL" in content

    def test_html_export_contains_validation_section(self, tmp_path):
        """HTML includes validation checks section."""
        report = _make_populated_report()
        filepath = tmp_path / "report.html"
        export_to_html(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "Validation Checks" in content
        assert "stress_check" in content
        assert "displacement_check" in content

    def test_html_export_contains_footer(self, tmp_path):
        """HTML includes footer with generation info."""
        report = _make_report()
        filepath = tmp_path / "report.html"
        export_to_html(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "digitalmodel" in content
        assert "reporting framework" in content

    def test_html_export_creates_parent_directories(self, tmp_path):
        """export_to_html creates intermediate directories."""
        report = _make_report()
        deep_path = tmp_path / "d" / "e" / "f" / "report.html"
        export_to_html(report, deep_path)

        assert deep_path.exists()

    def test_html_export_empty_report_no_parameter_section(self, tmp_path):
        """Minimal report produces HTML without parameter/result tables."""
        report = _make_report()
        filepath = tmp_path / "empty.html"
        export_to_html(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "<!DOCTYPE html>" in content
        # No parameters section when list is empty
        assert "Input Parameters" not in content
        # No results section when list is empty
        assert "Analysis Results" not in content

    def test_html_export_status_color_success(self, tmp_path):
        """Success status uses green color."""
        report = _make_report(status="success")
        filepath = tmp_path / "report.html"
        export_to_html(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "#28a745" in content

    def test_html_export_status_color_error(self, tmp_path):
        """Error status uses red color."""
        report = _make_report(status="error")
        filepath = tmp_path / "report.html"
        export_to_html(report, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "#dc3545" in content


# ===========================================================================
# 4. export_all_formats
# ===========================================================================

class TestExportAllFormats:
    """Tests for the export_all_formats batch export function."""

    def test_export_all_creates_three_files(self, tmp_path):
        """Default export creates HTML, JSON, and CSV files."""
        report = _make_populated_report()
        base = tmp_path / "analysis"
        result = export_all_formats(report, base)

        assert (tmp_path / "analysis.html").exists()
        assert (tmp_path / "analysis.json").exists()
        assert (tmp_path / "analysis.csv").exists()
        assert len(result) == 3

    def test_export_all_returns_path_dict(self, tmp_path):
        """Return value maps format names to Path objects."""
        report = _make_report()
        base = tmp_path / "report"
        result = export_all_formats(report, base)

        assert "html" in result
        assert "json" in result
        assert "csv" in result
        assert isinstance(result["html"], Path)
        assert result["html"].suffix == ".html"
        assert result["json"].suffix == ".json"
        assert result["csv"].suffix == ".csv"

    def test_export_all_selective_formats(self, tmp_path):
        """Specifying formats list limits which files are created."""
        report = _make_report()
        base = tmp_path / "report"
        result = export_all_formats(report, base, formats=["json", "csv"])

        assert "json" in result
        assert "csv" in result
        assert "html" not in result
        assert not (tmp_path / "report.html").exists()
        assert (tmp_path / "report.json").exists()
        assert (tmp_path / "report.csv").exists()

    def test_export_all_single_format(self, tmp_path):
        """Requesting a single format produces only that file."""
        report = _make_report()
        base = tmp_path / "report"
        result = export_all_formats(report, base, formats=["html"])

        assert len(result) == 1
        assert "html" in result
        assert (tmp_path / "report.html").exists()
        assert not (tmp_path / "report.json").exists()

    def test_export_all_creates_parent_directories(self, tmp_path):
        """Batch export creates intermediate directories."""
        report = _make_report()
        base = tmp_path / "deep" / "nested" / "report"
        result = export_all_formats(report, base)

        for path in result.values():
            assert path.exists()

    def test_export_all_json_content_valid(self, tmp_path):
        """JSON file from batch export contains valid report data."""
        report = _make_populated_report()
        base = tmp_path / "report"
        result = export_all_formats(report, base)

        with open(result["json"], "r", encoding="utf-8") as f:
            data = json.load(f)

        assert data["metadata"]["module"] == "structural_analysis"
        assert len(data["parameters"]) == 3


# ===========================================================================
# 5. export_parametric_study_html
# ===========================================================================

class TestExportParametricStudyHtml:
    """Tests for the export_parametric_study_html function."""

    @staticmethod
    def _make_study() -> ParametricStudy:
        """Build a parametric study with two reports."""
        study = ParametricStudy(
            study_name="safety_factor_sweep",
            description="Varying SF from 1.2 to 2.0",
            parameter_name="safety_factor",
        )
        for sf in [1.2, 1.5, 2.0]:
            report = _make_report(
                module="structural",
                analysis_type="parametric",
            )
            report.add_parameter("safety_factor", sf, unit="-")
            report.add_result("max_stress", 300.0 / sf, unit="MPa")
            report.add_result("displacement", 10.0 * sf, unit="mm")
            study.add_report(report)
        return study

    def test_parametric_html_creates_file(self, tmp_path):
        """export_parametric_study_html creates an HTML file."""
        study = self._make_study()
        filepath = tmp_path / "study.html"
        result = export_parametric_study_html(study, filepath)

        assert filepath.exists()
        assert result == filepath

    def test_parametric_html_contains_study_name(self, tmp_path):
        """HTML contains the study name in the title."""
        study = self._make_study()
        filepath = tmp_path / "study.html"
        export_parametric_study_html(study, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "safety_factor_sweep" in content

    def test_parametric_html_contains_comparison_table(self, tmp_path):
        """HTML contains a comparison table with parameter values and metrics."""
        study = self._make_study()
        filepath = tmp_path / "study.html"
        export_parametric_study_html(study, filepath)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "Comparison Table" in content
        assert "safety_factor" in content
        assert "max_stress" in content
        assert "displacement" in content

    def test_parametric_html_creates_parent_directories(self, tmp_path):
        """export_parametric_study_html creates intermediate directories."""
        study = self._make_study()
        deep_path = tmp_path / "p" / "q" / "study.html"
        export_parametric_study_html(study, deep_path)

        assert deep_path.exists()

    def test_parametric_html_filtered_metrics(self, tmp_path):
        """Providing comparison_metrics filters the table columns."""
        study = self._make_study()
        filepath = tmp_path / "study.html"
        export_parametric_study_html(
            study, filepath, comparison_metrics=["max_stress"],
        )

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        assert "max_stress" in content
        # displacement should not appear as a table header
        # (it may appear elsewhere, but check the table header row)
        assert "<th>displacement</th>" not in content
