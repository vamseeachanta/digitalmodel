"""Tests for CP assessment report generation."""

import pytest

from digitalmodel.cathodic_protection.cp_reporting import (
    ComplianceCheck,
    ComplianceStatus,
    RecommendationPriority,
    compliance_check_potential,
    generate_assessment_report,
    remaining_life_summary,
)


def test_compliance_check_compliant():
    """Protected potential (-0.900 V) should pass criterion (-0.850 V)."""
    result = compliance_check_potential(
        measured_potential_V=-0.900,
        criterion_V=-0.850,
        reference_electrode="CSE",
    )
    assert result.status == ComplianceStatus.COMPLIANT
    assert result.measured_value == -0.900
    assert result.criterion_value == -0.850


def test_compliance_check_non_compliant():
    """Unprotected potential (-0.750 V) should fail criterion."""
    result = compliance_check_potential(
        measured_potential_V=-0.750,
        criterion_V=-0.850,
    )
    assert result.status == ComplianceStatus.NON_COMPLIANT


def test_compliance_check_marginal():
    """Potential near criterion should be classified as marginal."""
    result = compliance_check_potential(
        measured_potential_V=-0.840,
        criterion_V=-0.850,
        tolerance_V=0.020,
    )
    # -0.840 is between -0.850 and -0.830 (criterion + tolerance)
    assert result.status == ComplianceStatus.MARGINAL


def test_generate_report_compliant():
    """Generate report with all checks compliant."""
    checks = [
        compliance_check_potential(-0.920, criterion_V=-0.850),
        compliance_check_potential(-0.880, criterion_V=-0.850),
    ]
    report = generate_assessment_report(
        system_id="CP-001",
        asset_description="12-inch buried pipeline",
        compliance_checks=checks,
    )

    assert report.overall_status == ComplianceStatus.COMPLIANT
    assert report.system_id == "CP-001"
    assert len(report.recommendations) == 0
    assert "2/2" in report.summary_text


def test_generate_report_non_compliant():
    """Non-compliant check should generate high-priority recommendation."""
    checks = [
        compliance_check_potential(-0.920, criterion_V=-0.850),
        compliance_check_potential(-0.700, criterion_V=-0.850),
    ]
    report = generate_assessment_report(
        system_id="CP-002",
        asset_description="Offshore jacket",
        compliance_checks=checks,
    )

    assert report.overall_status == ComplianceStatus.NON_COMPLIANT
    assert len(report.recommendations) >= 1
    assert report.recommendations[0].priority == RecommendationPriority.HIGH


def test_generate_report_with_remaining_life():
    """Report should include remaining life warning when < 5 years."""
    life = remaining_life_summary(
        system_id="CP-003",
        design_life_years=25.0,
        elapsed_years=22.0,
        anode_remaining_life_years=2.5,
    )
    checks = [compliance_check_potential(-0.860, criterion_V=-0.850)]

    report = generate_assessment_report(
        system_id="CP-003",
        asset_description="Platform CP",
        compliance_checks=checks,
        remaining_life=life,
    )

    assert report.remaining_life is not None
    # Should have recommendation about low remaining life
    life_recs = [r for r in report.recommendations if "LIFE" in r.recommendation_id]
    assert len(life_recs) >= 1


def test_remaining_life_summary_anode_limiting():
    """Anode depletion limits life before design life expires."""
    result = remaining_life_summary(
        system_id="CP-004",
        design_life_years=25.0,
        elapsed_years=10.0,
        anode_remaining_life_years=8.0,
    )
    assert result.remaining_design_life_years == 15.0
    assert result.remaining_anode_life_years == 8.0
    assert result.limiting_factor == "anode_depletion"
    assert result.life_extension_feasible


def test_remaining_life_summary_design_limiting():
    """Anodes outlast design life."""
    result = remaining_life_summary(
        system_id="CP-005",
        design_life_years=25.0,
        elapsed_years=10.0,
        anode_remaining_life_years=20.0,
    )
    assert result.limiting_factor == "design_life"
