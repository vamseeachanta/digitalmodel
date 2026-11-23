import pytest
from datetime import datetime
from digitalmodel.solvers.orcaflex.reporting.models.report import OrcaFlexAnalysisReport
from digitalmodel.solvers.orcaflex.reporting.models.geometry import GeometryData, LineProfileData, KeyPointData
from digitalmodel.solvers.orcaflex.reporting.models.materials import MaterialData, LineTypeData
from digitalmodel.solvers.orcaflex.reporting.models.design_checks import DesignCheckData, UtilizationData

# ENFORCEMENT RULE: Any PR adding a new str or list[str] field to a data model 
# MUST add a corresponding escaping test for that field in test_html_injection.py
# or expand the global XSS test in test_section_builders.py.

def test_orcaflex_analysis_report_minimal():
    report = OrcaFlexAnalysisReport(
        project_name="Test",
        structure_id="S-01",
        structure_type="riser"
    )
    assert report.project_name == "Test"
    assert report.structure_id == "S-01"
    assert report.structure_type == "riser"
    assert isinstance(report.date, datetime)
    assert report.geometry is None
    assert report.overall_pass is None

def test_geometry_data_validation():
    with pytest.raises(ValueError):
        # Missing required fields in LineProfileData
        GeometryData(line_profile=LineProfileData(arc_length=[0, 10]))
    
    profile = LineProfileData(arc_length=[0, 10], x=[0, 1], y=[0, 0], z=[0, -10])
    geo = GeometryData(line_profile=profile)
    assert geo.line_profile.arc_length == [0, 10]

def test_utilization_data_pass_fail_logic():
    # Implicit pass
    u1 = UtilizationData(name="Check 1", uc=0.8)
    assert u1.pass_fail is True
    assert u1.has_conflict is False
    
    # Implicit fail
    u2 = UtilizationData(name="Check 2", uc=1.1)
    assert u2.pass_fail is False
    assert u2.has_conflict is False
    
    # Explicit pass with UC > 1.0 (Conflict)
    u3 = UtilizationData(name="Check 3", uc=1.2, pass_fail=True)
    assert u3.pass_fail is True
    assert u3.has_conflict is True
    
    # Explicit fail with UC <= 1.0 (Conflict)
    u4 = UtilizationData(name="Check 4", uc=0.5, pass_fail=False)
    assert u4.pass_fail is False
    assert u4.has_conflict is True

def test_overall_pass_logic():
    report = OrcaFlexAnalysisReport(
        project_name="Test",
        structure_id="S-01",
        structure_type="riser",
        design_checks=DesignCheckData(
            code="API",
            checks=[
                UtilizationData(name="C1", uc=0.8),
                UtilizationData(name="C2", uc=0.9)
            ]
        )
    )
    assert report.overall_pass is True
    
    report.design_checks.checks.append(UtilizationData(name="C3", uc=1.1))
    assert report.overall_pass is False
    
    # Even if one passes and one fails, overall is fail
    report.design_checks.checks = [
        UtilizationData(name="C1", uc=0.8),
        UtilizationData(name="C3", uc=1.1)
    ]
    assert report.overall_pass is False

def test_keypoint_data():
    kp = KeyPointData(label="Test KP", arc_length_m=10.5)
    assert kp.label == "Test KP"
    assert kp.arc_length_m == 10.5
    assert kp.x is None

def test_linetype_data():
    lt = LineTypeData(
        name="6in Pipe",
        od=0.168,
        id=0.140,
        wt=0.014,
        density_kg_m3=7850
    )
    assert lt.name == "6in Pipe"
    assert lt.od == 0.168

def test_design_check_data_empty():
    dc = DesignCheckData(code="API", checks=[])
    assert dc.overall_pass is None
