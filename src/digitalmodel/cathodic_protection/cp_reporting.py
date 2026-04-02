"""Auto-generate CP assessment reports.

Provides tools for generating cathodic protection assessment reports
including compliance checks against design standards, field survey
comparison, remaining life summary, and recommendation matrices.

References
----------
- NACE SP0169 (2013) — Reporting requirements for CP systems
- DNV-RP-B401 (2017) §12 — Documentation requirements
- API RP 1632 (1996) — Record keeping and reporting
"""

from __future__ import annotations

from datetime import datetime
from enum import Enum
from typing import Optional

from pydantic import BaseModel, Field


class ComplianceStatus(str, Enum):
    """Compliance status for a CP system check."""

    COMPLIANT = "compliant"
    NON_COMPLIANT = "non_compliant"
    MARGINAL = "marginal"
    NOT_ASSESSED = "not_assessed"


class RecommendationPriority(str, Enum):
    """Priority level for recommendations."""

    CRITICAL = "critical"
    HIGH = "high"
    MEDIUM = "medium"
    LOW = "low"
    INFORMATION = "information"


class ComplianceCheck(BaseModel):
    """Result of a single compliance check."""

    check_id: str = Field(..., description="Check identifier")
    description: str = Field(..., description="Check description")
    standard_reference: str = Field(..., description="Applicable standard clause")
    criterion_value: float = Field(..., description="Criterion value")
    measured_value: float = Field(..., description="Measured/calculated value")
    unit: str = Field(default="", description="Unit of measurement")
    status: ComplianceStatus = Field(..., description="Compliance status")
    notes: str = Field(default="", description="Additional notes")


class Recommendation(BaseModel):
    """A single recommendation from CP assessment."""

    recommendation_id: str = Field(..., description="Recommendation identifier")
    priority: RecommendationPriority = Field(..., description="Priority level")
    description: str = Field(..., description="Recommendation description")
    action_required: str = Field(..., description="Required action")
    estimated_cost_category: str = Field(
        default="medium",
        description="Cost category: low/medium/high/very_high",
    )
    timeframe: str = Field(
        default="12_months",
        description="Recommended completion timeframe",
    )


class RemainingLifeSummary(BaseModel):
    """Summary of CP system remaining life."""

    system_id: str = Field(..., description="CP system identifier")
    installation_date: Optional[str] = Field(None, description="Installation date")
    design_life_years: float = Field(..., description="Original design life [years]")
    elapsed_years: float = Field(..., description="Years since installation")
    remaining_anode_life_years: float = Field(
        ..., description="Estimated remaining anode life [years]"
    )
    remaining_design_life_years: float = Field(
        ..., description="Remaining design life [years]"
    )
    life_extension_feasible: bool = Field(
        ..., description="Whether life extension is feasible"
    )
    limiting_factor: str = Field(
        ..., description="Factor limiting remaining life"
    )


class CPAssessmentReport(BaseModel):
    """Complete CP assessment report."""

    report_title: str = Field(..., description="Report title")
    report_date: str = Field(..., description="Report generation date")
    system_id: str = Field(..., description="CP system identifier")
    asset_description: str = Field(..., description="Protected asset description")
    compliance_checks: list[ComplianceCheck] = Field(
        default_factory=list, description="Compliance check results"
    )
    remaining_life: Optional[RemainingLifeSummary] = Field(
        None, description="Remaining life summary"
    )
    recommendations: list[Recommendation] = Field(
        default_factory=list, description="Recommendations"
    )
    overall_status: ComplianceStatus = Field(
        ..., description="Overall compliance status"
    )
    summary_text: str = Field(default="", description="Executive summary text")


def compliance_check_potential(
    measured_potential_V: float,
    criterion_V: float = -0.850,
    reference_electrode: str = "CSE",
    tolerance_V: float = 0.020,
) -> ComplianceCheck:
    """Check measured protection potential against criterion.

    Parameters
    ----------
    measured_potential_V : float
        Measured structure-to-electrolyte potential [V].
    criterion_V : float
        Protection criterion potential [V] (default -0.850 V vs CSE).
    reference_electrode : str
        Reference electrode type for reporting.
    tolerance_V : float
        Tolerance band for marginal classification [V].

    Returns
    -------
    ComplianceCheck
        Compliance check result.
    """
    if measured_potential_V <= criterion_V:
        status = ComplianceStatus.COMPLIANT
    elif measured_potential_V <= criterion_V + tolerance_V:
        status = ComplianceStatus.MARGINAL
    else:
        status = ComplianceStatus.NON_COMPLIANT

    return ComplianceCheck(
        check_id="POT-001",
        description=f"Protection potential vs {reference_electrode}",
        standard_reference="NACE SP0169 §6.2.2.1",
        criterion_value=criterion_V,
        measured_value=measured_potential_V,
        unit=f"V vs {reference_electrode}",
        status=status,
        notes=f"Measured: {measured_potential_V:.3f} V, Criterion: {criterion_V:.3f} V",
    )


def generate_assessment_report(
    system_id: str,
    asset_description: str,
    compliance_checks: list[ComplianceCheck],
    remaining_life: Optional[RemainingLifeSummary] = None,
    additional_recommendations: Optional[list[Recommendation]] = None,
) -> CPAssessmentReport:
    """Generate a complete CP assessment report.

    Aggregates compliance checks, determines overall status, generates
    recommendations, and produces an executive summary.

    Parameters
    ----------
    system_id : str
        CP system identifier.
    asset_description : str
        Description of the protected asset.
    compliance_checks : list[ComplianceCheck]
        List of compliance check results.
    remaining_life : RemainingLifeSummary, optional
        Remaining life assessment.
    additional_recommendations : list[Recommendation], optional
        Additional manual recommendations to include.

    Returns
    -------
    CPAssessmentReport
        Complete assessment report.
    """
    # Determine overall status
    statuses = [c.status for c in compliance_checks]
    if ComplianceStatus.NON_COMPLIANT in statuses:
        overall = ComplianceStatus.NON_COMPLIANT
    elif ComplianceStatus.MARGINAL in statuses:
        overall = ComplianceStatus.MARGINAL
    elif all(s == ComplianceStatus.COMPLIANT for s in statuses):
        overall = ComplianceStatus.COMPLIANT
    else:
        overall = ComplianceStatus.NOT_ASSESSED

    # Auto-generate recommendations
    recommendations: list[Recommendation] = []

    for check in compliance_checks:
        if check.status == ComplianceStatus.NON_COMPLIANT:
            recommendations.append(
                Recommendation(
                    recommendation_id=f"REC-{check.check_id}",
                    priority=RecommendationPriority.HIGH,
                    description=f"Non-compliant: {check.description}",
                    action_required=(
                        f"Investigate and remediate {check.description}. "
                        f"Measured {check.measured_value:.3f} {check.unit} "
                        f"vs criterion {check.criterion_value:.3f} {check.unit}."
                    ),
                    estimated_cost_category="high",
                    timeframe="3_months",
                )
            )
        elif check.status == ComplianceStatus.MARGINAL:
            recommendations.append(
                Recommendation(
                    recommendation_id=f"REC-{check.check_id}",
                    priority=RecommendationPriority.MEDIUM,
                    description=f"Marginal: {check.description}",
                    action_required=(
                        f"Monitor closely and plan remediation for {check.description}."
                    ),
                    estimated_cost_category="medium",
                    timeframe="6_months",
                )
            )

    # Remaining life recommendations
    if remaining_life is not None:
        if remaining_life.remaining_anode_life_years < 5.0:
            recommendations.append(
                Recommendation(
                    recommendation_id="REC-LIFE-001",
                    priority=RecommendationPriority.HIGH,
                    description="Anode life approaching end",
                    action_required=(
                        f"Remaining anode life: {remaining_life.remaining_anode_life_years:.1f} years. "
                        f"Plan anode replacement or retrofit."
                    ),
                    estimated_cost_category="very_high",
                    timeframe="12_months",
                )
            )

    if additional_recommendations:
        recommendations.extend(additional_recommendations)

    # Generate summary
    n_compliant = sum(1 for s in statuses if s == ComplianceStatus.COMPLIANT)
    n_total = len(statuses)
    summary = (
        f"CP assessment for {asset_description} (System: {system_id}). "
        f"{n_compliant}/{n_total} checks compliant. "
        f"Overall status: {overall.value}. "
        f"{len(recommendations)} recommendation(s) issued."
    )

    if remaining_life is not None:
        summary += (
            f" Remaining anode life: {remaining_life.remaining_anode_life_years:.1f} years."
        )

    report_date = datetime.now().strftime("%Y-%m-%d")

    return CPAssessmentReport(
        report_title=f"CP Assessment Report — {system_id}",
        report_date=report_date,
        system_id=system_id,
        asset_description=asset_description,
        compliance_checks=compliance_checks,
        remaining_life=remaining_life,
        recommendations=recommendations,
        overall_status=overall,
        summary_text=summary,
    )


def remaining_life_summary(
    system_id: str,
    design_life_years: float,
    elapsed_years: float,
    anode_remaining_life_years: float,
    installation_date: Optional[str] = None,
) -> RemainingLifeSummary:
    """Generate remaining life summary for a CP system.

    Parameters
    ----------
    system_id : str
        CP system identifier.
    design_life_years : float
        Original design life [years].
    elapsed_years : float
        Years since installation.
    anode_remaining_life_years : float
        Estimated remaining anode life [years].
    installation_date : str, optional
        Installation date string.

    Returns
    -------
    RemainingLifeSummary
        Remaining life assessment summary.
    """
    remaining_design = max(0, design_life_years - elapsed_years)

    # Determine limiting factor
    if anode_remaining_life_years < remaining_design:
        limiting_factor = "anode_depletion"
        life_extension = True  # can retrofit anodes
    else:
        limiting_factor = "design_life"
        life_extension = anode_remaining_life_years > remaining_design * 1.2

    return RemainingLifeSummary(
        system_id=system_id,
        installation_date=installation_date,
        design_life_years=design_life_years,
        elapsed_years=elapsed_years,
        remaining_anode_life_years=anode_remaining_life_years,
        remaining_design_life_years=remaining_design,
        life_extension_feasible=life_extension,
        limiting_factor=limiting_factor,
    )
