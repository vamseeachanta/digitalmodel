import pytest
from digitalmodel.solvers.orcaflex.reporting.models.design_checks import UtilizationData, DesignCheckData
from digitalmodel.solvers.orcaflex.reporting.models.report import OrcaFlexAnalysisReport
from digitalmodel.solvers.orcaflex.reporting.section_builders.design_checks import _build_design_checks_html

def test_utilization_conflict_detection():
    # UC > 1.0 but pass_fail=True (Conflict)
    u = UtilizationData(name="Test Check", uc=1.1, pass_fail=True)
    assert u.pass_fail is True
    assert u.derived_pass_fail is False
    assert u.has_conflict is True
    
    # UC <= 1.0 and pass_fail=True (No Conflict)
    u2 = UtilizationData(name="Test Check 2", uc=0.9, pass_fail=True)
    assert u2.has_conflict is False
    
    # UC > 1.0 and pass_fail=None (Derived False, No Conflict)
    u3 = UtilizationData(name="Test Check 3", uc=1.1)
    assert u3.pass_fail is False
    assert u3.has_conflict is False

def test_design_check_html_conflict_badge():
    report = OrcaFlexAnalysisReport(
        project_name="Conflict Test",
        structure_id="C-001",
        structure_type="riser",
        design_checks=DesignCheckData(
            code="API",
            checks=[UtilizationData(name="Conflict Check", uc=1.2, pass_fail=True)]
        )
    )
    
    html_out = _build_design_checks_html(report)
    assert "⚠️ CONFLICT" in html_out
    assert "badge-pass" in html_out # pass_fail governs
