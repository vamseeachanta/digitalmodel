"""
Adapter: maps ComprehensiveResults (mooring analysis) → OrcaFlexAnalysisReport.
"""
from typing import Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from digitalmodel.solvers.orcaflex.mooring_analysis.comprehensive_analysis.models import (
        ComprehensiveResults,
    )

from ..models.report import OrcaFlexAnalysisReport
from ..models.results import StaticResultsData
from ..models.design_checks import DesignCheckData, UtilizationData


def extract_mooring_report(
    comprehensive_results: 'ComprehensiveResults',
    project_name: str = "Mooring Analysis",
    structure_id: str = "MOORING-001",
    analyst: Optional[str] = None,
) -> OrcaFlexAnalysisReport:
    """Map a ``ComprehensiveResults`` object to an ``OrcaFlexAnalysisReport``.

    This adapter bridges the mooring analysis dataclasses to the generic
    HTML report model so existing renderers can produce a mooring report
    without any OrcFxAPI dependency.

    Args:
        comprehensive_results: Output of the comprehensive mooring analysis.
        project_name: Project label shown in the report header.
        structure_id: Structure identifier (e.g., "MOORING-001").
        analyst: Optional analyst name.

    Returns:
        OrcaFlexAnalysisReport ready to pass to ``generate_orcaflex_report()``.
    """
    static_results = _extract_static_results(comprehensive_results)
    design_checks = _extract_design_checks(comprehensive_results)
    summary_notes = _extract_summary_notes(comprehensive_results)

    return OrcaFlexAnalysisReport(
        project_name=project_name,
        structure_id=structure_id,
        structure_type="mooring",
        analyst=analyst,
        static_results=static_results,
        design_checks=design_checks,
        summary_notes=summary_notes,
        recommendations=_extract_recommendations(comprehensive_results),
    )


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _extract_static_results(cr: 'ComprehensiveResults') -> Optional[StaticResultsData]:
    """Build StaticResultsData from pretension data in individual_results."""
    if not cr.individual_results:
        return None

    per_line_tensions: dict = {}
    for run_id, ar in cr.individual_results.items():
        if ar.pretension_metrics is None:
            continue
        dist = ar.pretension_metrics.tension_distribution
        for line_name, tension in dist.items():
            per_line_tensions[f"{run_id}/{line_name}"] = tension

    if not per_line_tensions:
        return None

    return StaticResultsData(
        end_tensions_kn={},
        per_line_tensions=per_line_tensions,
    )


def _extract_design_checks(cr: 'ComprehensiveResults') -> Optional[DesignCheckData]:
    """Build DesignCheckData from fender utilization rates."""
    checks = []

    for run_id, ar in cr.individual_results.items():
        if ar.fender_metrics is None:
            continue
        for fender_id, utilization in ar.fender_metrics.utilization_rates.items():
            checks.append(
                UtilizationData(
                    name=f"{run_id} — {fender_id}",
                    uc=utilization / 100.0,  # convert % to ratio
                    load_case=run_id,
                )
            )

    if not checks:
        return None

    return DesignCheckData(code="Mooring analysis", checks=checks)


def _extract_summary_notes(cr: 'ComprehensiveResults') -> Optional[str]:
    """Build a plain-text summary from stiffness natural periods if present."""
    notes: list = []

    for run_id, ar in cr.individual_results.items():
        if ar.stiffness_metrics and ar.stiffness_metrics.natural_periods:
            period_strs = ", ".join(
                f"{dof}={val:.2f}s"
                for dof, val in ar.stiffness_metrics.natural_periods.items()
            )
            notes.append(f"{run_id}: natural periods [{period_strs}]")

    return "; ".join(notes) if notes else None


def _extract_recommendations(cr: 'ComprehensiveResults') -> list:
    """Collect recommendations from all sub-metrics."""
    recs: list = []

    if cr.overall_summary:
        recs.extend(cr.overall_summary.design_recommendations)

    for ar in cr.individual_results.values():
        for metrics_attr in ("pretension_metrics", "stiffness_metrics", "fender_metrics"):
            metrics = getattr(ar, metrics_attr, None)
            if metrics and hasattr(metrics, "recommendations"):
                recs.extend(metrics.recommendations)

    # Deduplicate while preserving order
    seen: set = set()
    unique: list = []
    for r in recs:
        if r not in seen:
            seen.add(r)
            unique.append(r)
    return unique
