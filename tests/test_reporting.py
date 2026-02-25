"""
ABOUTME: Tests for standardized reporting framework
ABOUTME: Validates report creation, parametric studies, and export functions
"""

import pytest
from pathlib import Path
import json
import csv
from datetime import datetime

from digitalmodel.reporting import (
    StandardReport,
    ParameterSet,
    AnalysisResult,
    ValidationResult,
    ReportMetadata,
    ParametricStudy,
    export_to_html,
    export_to_json,
    export_to_csv,
    export_all_formats,
)


class TestParameterSet:
    """Test ParameterSet model"""

    def test_create_parameter(self):
        """Test creating a parameter set"""
        param = ParameterSet(
            name="safety_factor",
            value=1.5,
            unit="dimensionless",
            description="Global safety factor"
        )

        assert param.name == "safety_factor"
        assert param.value == 1.5
        assert param.unit == "dimensionless"
        assert param.description == "Global safety factor"

    def test_parameter_without_optional_fields(self):
        """Test parameter with only required fields"""
        param = ParameterSet(name="test", value=42)

        assert param.name == "test"
        assert param.value == 42
        assert param.unit is None
        assert param.description is None

    def test_parameter_various_value_types(self):
        """Test parameter with different value types"""
        ParameterSet(name="float_val", value=3.14)
        ParameterSet(name="int_val", value=42)
        ParameterSet(name="str_val", value="test")
        ParameterSet(name="bool_val", value=True)


class TestAnalysisResult:
    """Test AnalysisResult model"""

    def test_create_result(self):
        """Test creating an analysis result"""
        result = AnalysisResult(
            metric_name="max_stress",
            value=250.5,
            unit="MPa",
            description="Maximum von Mises stress",
            passed=True,
            threshold=300.0
        )

        assert result.metric_name == "max_stress"
        assert result.value == 250.5
        assert result.unit == "MPa"
        assert result.passed is True
        assert result.threshold == 300.0

    def test_result_with_complex_value(self):
        """Test result with list or dict value"""
        # List value
        result_list = AnalysisResult(
            metric_name="stress_components",
            value=[100, 200, 300]
        )
        assert result_list.value == [100, 200, 300]

        # Dict value
        result_dict = AnalysisResult(
            metric_name="stress_tensor",
            value={"sigma_x": 100, "sigma_y": 200, "tau_xy": 50}
        )
        assert result_dict.value["sigma_x"] == 100


class TestValidationResult:
    """Test ValidationResult model"""

    def test_create_validation(self):
        """Test creating a validation result"""
        validation = ValidationResult(
            check_name="stress_check",
            passed=True,
            message="All stresses within limits",
            severity="info"
        )

        assert validation.check_name == "stress_check"
        assert validation.passed is True
        assert validation.message == "All stresses within limits"
        assert validation.severity == "info"

    def test_validation_severities(self):
        """Test different severity levels"""
        for severity in ["info", "warning", "error", "critical"]:
            validation = ValidationResult(
                check_name="test",
                passed=False,
                message="test",
                severity=severity
            )
            assert validation.severity == severity

    def test_validation_with_details(self):
        """Test validation with additional details"""
        validation = ValidationResult(
            check_name="geometry_check",
            passed=False,
            message="Invalid thickness",
            severity="error",
            details={"min_thickness": 10, "actual_thickness": 8}
        )

        assert validation.details["min_thickness"] == 10
        assert validation.details["actual_thickness"] == 8


class TestStandardReport:
    """Test StandardReport model"""

    def test_create_minimal_report(self):
        """Test creating report with minimal metadata"""
        metadata = ReportMetadata(
            module="test_module",
            analysis_type="test_analysis",
            execution_time_seconds=1.5
        )

        report = StandardReport(metadata=metadata)

        assert report.metadata.module == "test_module"
        assert report.metadata.analysis_type == "test_analysis"
        assert len(report.parameters) == 0
        assert len(report.results) == 0

    def test_create_complete_report(self):
        """Test creating report with all sections"""
        metadata = ReportMetadata(
            module="structural_analysis",
            analysis_type="stress_analysis",
            execution_time_seconds=45.2
        )

        report = StandardReport(
            metadata=metadata,
            parameters=[
                ParameterSet(name="safety_factor", value=1.5, unit="-"),
                ParameterSet(name="material", value="S355", unit="-")
            ],
            results=[
                AnalysisResult(
                    metric_name="max_stress",
                    value=250.5,
                    unit="MPa",
                    passed=True,
                    threshold=300.0
                )
            ],
            validations=[
                ValidationResult(
                    check_name="stress_check",
                    passed=True,
                    message="Stresses within limits"
                )
            ]
        )

        assert len(report.parameters) == 2
        assert len(report.results) == 1
        assert len(report.validations) == 1

    def test_add_parameter_method(self):
        """Test adding parameters using method"""
        metadata = ReportMetadata(
            module="test",
            analysis_type="test",
            execution_time_seconds=1.0
        )
        report = StandardReport(metadata=metadata)

        report.add_parameter("test_param", 42, unit="mm", description="Test parameter")

        assert len(report.parameters) == 1
        assert report.parameters[0].name == "test_param"
        assert report.parameters[0].value == 42

    def test_add_result_method(self):
        """Test adding results using method"""
        metadata = ReportMetadata(
            module="test",
            analysis_type="test",
            execution_time_seconds=1.0
        )
        report = StandardReport(metadata=metadata)

        report.add_result(
            metric_name="max_stress",
            value=250.5,
            unit="MPa",
            passed=True,
            threshold=300.0
        )

        assert len(report.results) == 1
        assert report.results[0].metric_name == "max_stress"
        assert report.results[0].passed is True

    def test_add_validation_method(self):
        """Test adding validations using method"""
        metadata = ReportMetadata(
            module="test",
            analysis_type="test",
            execution_time_seconds=1.0
        )
        report = StandardReport(metadata=metadata)

        report.add_validation(
            check_name="test_check",
            passed=True,
            message="Check passed",
            severity="info"
        )

        assert len(report.validations) == 1
        assert report.validations[0].check_name == "test_check"

    def test_get_summary(self):
        """Test report summary generation"""
        metadata = ReportMetadata(
            module="test",
            analysis_type="test",
            execution_time_seconds=5.5,
            status="success"
        )
        report = StandardReport(metadata=metadata)

        report.add_parameter("param1", 1.0)
        report.add_parameter("param2", 2.0)
        report.add_result("result1", 10.0)
        report.add_validation("check1", True, "Passed")
        report.add_validation("check2", False, "Failed")

        summary = report.get_summary()

        assert summary["module"] == "test"
        assert summary["parameter_count"] == 2
        assert summary["result_count"] == 1
        assert summary["validation_count"] == 2
        assert summary["validations_passed"] == 1
        assert summary["validations_failed"] == 1

    def test_to_dict(self):
        """Test converting report to dictionary"""
        metadata = ReportMetadata(
            module="test",
            analysis_type="test",
            execution_time_seconds=1.0
        )
        report = StandardReport(metadata=metadata)
        report.add_parameter("test", 42)

        data = report.to_dict()

        assert isinstance(data, dict)
        assert "metadata" in data
        assert "parameters" in data
        assert len(data["parameters"]) == 1

    def test_to_json(self):
        """Test converting report to JSON"""
        metadata = ReportMetadata(
            module="test",
            analysis_type="test",
            execution_time_seconds=1.0
        )
        report = StandardReport(metadata=metadata)
        report.add_parameter("test", 42)

        json_str = report.to_json()

        assert isinstance(json_str, str)
        data = json.loads(json_str)
        assert data["metadata"]["module"] == "test"


class TestParametricStudy:
    """Test ParametricStudy model"""

    def create_sample_report(self, safety_factor: float) -> StandardReport:
        """Helper to create sample report"""
        metadata = ReportMetadata(
            module="test",
            analysis_type="parametric_test",
            execution_time_seconds=1.0
        )

        report = StandardReport(metadata=metadata)
        report.add_parameter("safety_factor", safety_factor, unit="-")
        report.add_result("max_stress", 300.0 / safety_factor, unit="MPa")
        report.add_result("displacement", 10.0 * safety_factor, unit="mm")

        return report

    def test_create_parametric_study(self):
        """Test creating parametric study"""
        study = ParametricStudy(
            study_name="safety_factor_study",
            description="Varying safety factor",
            parameter_name="safety_factor"
        )

        assert study.study_name == "safety_factor_study"
        assert study.parameter_name == "safety_factor"
        assert len(study.reports) == 0

    def test_add_reports(self):
        """Test adding reports to study"""
        study = ParametricStudy(
            study_name="test_study",
            parameter_name="safety_factor"
        )

        study.add_report(self.create_sample_report(1.2))
        study.add_report(self.create_sample_report(1.5))
        study.add_report(self.create_sample_report(2.0))

        assert len(study.reports) == 3

    def test_get_parameter_values(self):
        """Test extracting parameter values"""
        study = ParametricStudy(
            study_name="test_study",
            parameter_name="safety_factor"
        )

        study.add_report(self.create_sample_report(1.2))
        study.add_report(self.create_sample_report(1.5))
        study.add_report(self.create_sample_report(2.0))

        values = study.get_parameter_values()

        assert values == [1.2, 1.5, 2.0]

    def test_get_comparison_table(self):
        """Test generating comparison table"""
        study = ParametricStudy(
            study_name="test_study",
            parameter_name="safety_factor"
        )

        study.add_report(self.create_sample_report(1.2))
        study.add_report(self.create_sample_report(1.5))
        study.add_report(self.create_sample_report(2.0))

        table = study.get_comparison_table()

        assert "safety_factor" in table
        assert "max_stress" in table
        assert "displacement" in table

        assert table["safety_factor"] == [1.2, 1.5, 2.0]
        assert len(table["max_stress"]) == 3
        assert len(table["displacement"]) == 3

    def test_get_comparison_table_specific_metrics(self):
        """Test comparison table with specific metrics"""
        study = ParametricStudy(
            study_name="test_study",
            parameter_name="safety_factor"
        )

        study.add_report(self.create_sample_report(1.5))
        study.add_report(self.create_sample_report(2.0))

        table = study.get_comparison_table(metric_names=["max_stress"])

        assert "safety_factor" in table
        assert "max_stress" in table
        assert "displacement" not in table

    def test_get_summary(self):
        """Test parametric study summary"""
        study = ParametricStudy(
            study_name="test_study",
            parameter_name="safety_factor"
        )

        study.add_report(self.create_sample_report(1.5))
        study.add_report(self.create_sample_report(2.0))

        summary = study.get_summary()

        assert summary["study_name"] == "test_study"
        assert summary["parameter_name"] == "safety_factor"
        assert summary["report_count"] == 2
        assert summary["parameter_values"] == [1.5, 2.0]


class TestExporters:
    """Test export functions"""

    def create_sample_report(self) -> StandardReport:
        """Helper to create sample report"""
        metadata = ReportMetadata(
            module="structural_analysis",
            analysis_type="stress_check",
            execution_time_seconds=10.5,
            status="success"
        )

        report = StandardReport(metadata=metadata)
        report.add_parameter("safety_factor", 1.5, unit="-", description="Safety factor")
        report.add_parameter("material", "S355", unit="-", description="Material grade")

        report.add_result("max_stress", 250.5, unit="MPa", description="Max stress", passed=True, threshold=300.0)
        report.add_result("displacement", 12.3, unit="mm", description="Max displacement")

        report.add_validation("stress_check", True, "Stresses within limits", "info")
        report.add_validation("geometry_check", True, "Geometry valid", "info")

        return report

    def test_export_to_json(self, tmp_path):
        """Test JSON export"""
        report = self.create_sample_report()
        output_file = tmp_path / "report.json"

        result_path = export_to_json(report, output_file)

        assert result_path.exists()
        assert result_path == output_file

        # Verify JSON content
        with open(result_path) as f:
            data = json.load(f)

        assert data["metadata"]["module"] == "structural_analysis"
        assert len(data["parameters"]) == 2
        assert len(data["results"]) == 2

    def test_export_to_csv(self, tmp_path):
        """Test CSV export"""
        report = self.create_sample_report()
        output_file = tmp_path / "report.csv"

        result_path = export_to_csv(report, output_file)

        assert result_path.exists()
        assert result_path == output_file

        # Verify CSV can be read
        with open(result_path, newline='', encoding='utf-8') as f:
            reader = csv.reader(f)
            rows = list(reader)

        assert len(rows) > 0
        assert "METADATA" in rows[0]

    def test_export_to_html(self, tmp_path):
        """Test HTML export"""
        report = self.create_sample_report()
        output_file = tmp_path / "report.html"

        result_path = export_to_html(report, output_file)

        assert result_path.exists()
        assert result_path == output_file

        # Verify HTML content
        with open(result_path, encoding='utf-8') as f:
            html_content = f.read()

        assert "<!DOCTYPE html>" in html_content
        assert "structural_analysis" in html_content
        assert "max_stress" in html_content

    def test_export_all_formats(self, tmp_path):
        """Test exporting to all formats at once"""
        report = self.create_sample_report()
        base_path = tmp_path / "report"

        files = export_all_formats(report, base_path)

        assert "html" in files
        assert "json" in files
        assert "csv" in files

        assert files["html"].exists()
        assert files["json"].exists()
        assert files["csv"].exists()

    def test_export_specific_formats(self, tmp_path):
        """Test exporting to specific formats"""
        report = self.create_sample_report()
        base_path = tmp_path / "report"

        files = export_all_formats(report, base_path, formats=["json", "csv"])

        assert "json" in files
        assert "csv" in files
        assert "html" not in files


class TestIntegrationScenarios:
    """Test real-world usage scenarios"""

    def test_complete_workflow(self, tmp_path):
        """Test complete workflow from creation to export"""
        # Create report
        metadata = ReportMetadata(
            module="fatigue_analysis",
            analysis_type="rainflow_counting",
            execution_time_seconds=25.7,
            status="success"
        )

        report = StandardReport(metadata=metadata)

        # Add parameters
        report.add_parameter("material_sn_curve", "DNV-RP-C203 Curve D", description="S-N curve")
        report.add_parameter("safety_factor", 1.0, unit="-")

        # Add results
        report.add_result("fatigue_damage", 0.75, unit="-", passed=True, threshold=1.0)
        report.add_result("cycles_counted", 15234, unit="cycles")

        # Add validations
        report.add_validation("damage_check", True, "Fatigue damage acceptable")

        # Export to all formats
        files = export_all_formats(report, tmp_path / "fatigue_report")

        # Verify all files created
        assert files["html"].exists()
        assert files["json"].exists()
        assert files["csv"].exists()

    def test_parametric_study_workflow(self, tmp_path):
        """Test parametric study workflow"""
        # Create parametric study
        study = ParametricStudy(
            study_name="Safety Factor Study",
            description="Varying safety factor from 1.2 to 2.0",
            parameter_name="safety_factor"
        )

        # Run multiple analyses
        for sf in [1.2, 1.5, 1.8, 2.0]:
            metadata = ReportMetadata(
                module="structural_analysis",
                analysis_type="stress_check",
                execution_time_seconds=5.0
            )

            report = StandardReport(metadata=metadata)
            report.add_parameter("safety_factor", sf, unit="-")
            report.add_result("max_stress", 300.0 / sf, unit="MPa")
            report.add_result("capacity_ratio", 1.0 / sf, unit="-")

            study.add_report(report)

        # Get comparison table
        table = study.get_comparison_table()

        assert len(table["safety_factor"]) == 4
        assert len(table["max_stress"]) == 4

        # Export study
        study.save_json(tmp_path / "parametric_study.json")
        assert (tmp_path / "parametric_study.json").exists()
