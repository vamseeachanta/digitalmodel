import pytest
from pathlib import Path
from digitalmodel.solvers.orcaflex.reporting import generate_orcaflex_report, OrcaFlexAnalysisReport
from digitalmodel.solvers.orcaflex.reporting.models import StaticResultsData, GeometryData, LineProfileData, KeyPointData

def test_generate_mooring_report(tmp_path):
    report_data = OrcaFlexAnalysisReport(
        project_name="Mooring Project",
        structure_id="MOOR-001",
        structure_type="mooring",
        geometry=GeometryData(
            line_profile=LineProfileData(arc_length=[0, 1], x=[0, 0], y=[0, 0], z=[0, 0]),
            key_points=[KeyPointData(label="Fairlead", arc_length_m=0, x=0, y=0, z=0)]
        ),
        static_results=StaticResultsData(
            per_line_tensions={"Line 1": 2000.0, "Line 2": 2100.0, "Line 3": 1950.0}
        )
    )
    
    output_file = tmp_path / "mooring_report.html"
    result_path = generate_orcaflex_report(
        report_data,
        output_path=output_file
    )
    
    assert result_path.exists()
    with open(result_path, 'r', encoding='utf-8') as f:
        content = f.read()
        assert "Mooring Project" in content
        assert "geometry-spider" in content
        assert "static-per-line-tensions" in content
        assert "Line 1" in content
        assert "2000.0" in content
