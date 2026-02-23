import pytest
from digitalmodel.solvers.orcaflex.reporting import generate_orcaflex_report, OrcaFlexAnalysisReport
from digitalmodel.solvers.orcaflex.reporting.models import (
    GeometryData, LineProfileData, KeyPointData, DesignCheckData, UtilizationData, EnvironmentData, LoadCaseData
)
from pathlib import Path

def test_html_injection_project_name(tmp_path):
    injection_str = "<script>alert(1)</script>"
    report_data = OrcaFlexAnalysisReport(
        project_name=injection_str,
        structure_id="TEST-001",
        structure_type="riser"
    )
    output_file = tmp_path / "injection_project.html"
    generate_orcaflex_report(report_data, output_path=output_file)
    with open(output_file, 'r', encoding='utf-8') as f:
        content = f.read()
        assert injection_str not in content
        assert "&lt;script&gt;alert(1)&lt;/script&gt;" in content

def test_html_injection_structure_id(tmp_path):
    injection_str = "<script>alert(2)</script>"
    report_data = OrcaFlexAnalysisReport(
        project_name="Test Project",
        structure_id=injection_str,
        structure_type="riser"
    )
    output_file = tmp_path / "injection_id.html"
    generate_orcaflex_report(report_data, output_path=output_file)
    with open(output_file, 'r', encoding='utf-8') as f:
        content = f.read()
        assert injection_str not in content
        assert "&lt;script&gt;alert(2)&lt;/script&gt;" in content

def test_html_injection_analyst(tmp_path):
    injection_str = "<script>alert(3)</script>"
    report_data = OrcaFlexAnalysisReport(
        project_name="Test Project",
        structure_id="TEST-001",
        structure_type="riser",
        analyst=injection_str
    )
    output_file = tmp_path / "injection_analyst.html"
    generate_orcaflex_report(report_data, output_path=output_file)
    with open(output_file, 'r', encoding='utf-8') as f:
        content = f.read()
        assert injection_str not in content
        assert "&lt;script&gt;alert(3)&lt;/script&gt;" in content

def test_html_injection_load_case_label(tmp_path):
    injection_str = "<script>alert(4)</script>"
    report_data = OrcaFlexAnalysisReport(
        project_name="Test Project",
        structure_id="TEST-001",
        structure_type="riser",
        loads=EnvironmentData(
            load_cases=[LoadCaseData(case_id=injection_str, hs_m=1.0, tp_s=10.0)]
        )
    )
    output_file = tmp_path / "injection_loadcase.html"
    generate_orcaflex_report(report_data, output_path=output_file)
    with open(output_file, 'r', encoding='utf-8') as f:
        content = f.read()
        assert injection_str not in content
        assert "&lt;script&gt;alert(4)&lt;/script&gt;" in content

def test_html_injection_check_name(tmp_path):
    injection_str = "<script>alert(5)</script>"
    report_data = OrcaFlexAnalysisReport(
        project_name="Test Project",
        structure_id="TEST-001",
        structure_type="riser",
        design_checks=DesignCheckData(
            code="API RP 2RD",
            checks=[UtilizationData(name=injection_str, uc=0.5)]
        )
    )
    output_file = tmp_path / "injection_check.html"
    generate_orcaflex_report(report_data, output_path=output_file)
    with open(output_file, 'r', encoding='utf-8') as f:
        content = f.read()
        assert injection_str not in content
        assert "&lt;script&gt;alert(5)&lt;/script&gt;" in content

def test_html_injection_recommendation(tmp_path):
    injection_str = "<script>alert(6)</script>"
    report_data = OrcaFlexAnalysisReport(
        project_name="Test Project",
        structure_id="TEST-001",
        structure_type="riser",
        recommendations=[injection_str]
    )
    output_file = tmp_path / "injection_rec.html"
    generate_orcaflex_report(report_data, output_path=output_file)
    with open(output_file, 'r', encoding='utf-8') as f:
        content = f.read()
        assert injection_str not in content
        assert "&lt;script&gt;alert(6)&lt;/script&gt;" in content
