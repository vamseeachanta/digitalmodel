"""
Fatigue Assessment Reporting
============================

Auto-generate fatigue assessment summary reports from a collection of
fatigue check results.  Outputs include:

- Markdown summary table (suitable for engineering reports)
- Dict/JSON structure (suitable for HTML dashboards)
- Cumulative damage bar-chart data
- Inspection interval recommendations

Designed to work with the outputs from :mod:`digitalmodel.fatigue.damage`
and :mod:`digitalmodel.fatigue.crack_growth`.

References
----------
- DNV-RP-C203 (2021), Section 5 — Fatigue assessment
- NORSOK N-004 (2013), Annex C — Fatigue analysis reporting requirements
"""

import html
from pathlib import Path
from typing import TYPE_CHECKING, Dict, List, Optional, Union

from pydantic import BaseModel, Field

if TYPE_CHECKING:  # avoid importing the reporting library at module load
    from digitalmodel.reporting import Provenance


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------


class FatigueCheckLocation(BaseModel):
    """Single fatigue assessment location.

    Attributes
    ----------
    location_id : str
        Unique identifier (e.g. "Frame 12, Brace 3").
    description : str
        Description of the location.
    sn_curve : str
        S-N curve used (e.g. "DNV D in air").
    cumulative_damage : float
        Miner's rule cumulative damage D.
    design_life_years : float
        Target design life (years).
    calculated_life_years : float
        Calculated fatigue life (years). ``inf`` if no damage.
    dff : float
        Design Fatigue Factor applied (default 1.0).
    utilisation : float
        D × DFF — must be ≤ 1.0 to pass.
    stress_range_max : float
        Maximum stress range in the histogram (MPa).
    pass_fail : str
        ``"PASS"`` or ``"FAIL"``.
    inspection_interval : Optional[float]
        Recommended inspection interval (years), if computed.
    notes : str
        Additional notes.
    """

    location_id: str
    description: str = ""
    sn_curve: str = ""
    cumulative_damage: float = 0.0
    design_life_years: float = 25.0
    calculated_life_years: float = float("inf")
    dff: float = 1.0
    utilisation: float = 0.0
    stress_range_max: float = 0.0
    pass_fail: str = "PASS"
    inspection_interval: Optional[float] = None
    notes: str = ""


class FatigueReport(BaseModel):
    """Complete fatigue assessment report.

    Attributes
    ----------
    project : str
    structure : str
    analyst : str
    date : str
    standard : str
    locations : list[FatigueCheckLocation]
    summary : dict
    """

    project: str = ""
    structure: str = ""
    analyst: str = ""
    date: str = ""
    standard: str = "DNV-RP-C203 (2021)"
    locations: List[FatigueCheckLocation] = Field(default_factory=list)
    summary: dict = Field(default_factory=dict)


# ---------------------------------------------------------------------------
# Report generation
# ---------------------------------------------------------------------------


def generate_report(
    locations: List[FatigueCheckLocation],
    project: str = "",
    structure: str = "",
    analyst: str = "",
    date: str = "",
    standard: str = "DNV-RP-C203 (2021)",
) -> FatigueReport:
    """Create a FatigueReport from a list of check locations.

    Parameters
    ----------
    locations : list[FatigueCheckLocation]
    project, structure, analyst, date, standard : str
        Header metadata.

    Returns
    -------
    FatigueReport
    """
    n_pass = sum(1 for loc in locations if loc.pass_fail == "PASS")
    n_fail = len(locations) - n_pass
    max_util = max((loc.utilisation for loc in locations), default=0.0)
    min_life = min(
        (loc.calculated_life_years for loc in locations),
        default=float("inf"),
    )

    summary = {
        "total_locations": len(locations),
        "pass_count": n_pass,
        "fail_count": n_fail,
        "max_utilisation": round(max_util, 4),
        "min_calculated_life_years": (
            round(min_life, 2) if min_life != float("inf") else "inf"
        ),
        "overall_status": "PASS" if n_fail == 0 else "FAIL",
    }

    return FatigueReport(
        project=project,
        structure=structure,
        analyst=analyst,
        date=date,
        standard=standard,
        locations=locations,
        summary=summary,
    )


def report_to_markdown(report: FatigueReport) -> str:
    """Render a FatigueReport as a Markdown string.

    Parameters
    ----------
    report : FatigueReport

    Returns
    -------
    str
        Markdown-formatted report.
    """
    lines = []
    lines.append("# Fatigue Assessment Report")
    lines.append("")
    if report.project:
        lines.append(f"**Project:** {report.project}  ")
    if report.structure:
        lines.append(f"**Structure:** {report.structure}  ")
    if report.analyst:
        lines.append(f"**Analyst:** {report.analyst}  ")
    if report.date:
        lines.append(f"**Date:** {report.date}  ")
    lines.append(f"**Standard:** {report.standard}  ")
    lines.append("")

    # Summary
    s = report.summary
    lines.append("## Summary")
    lines.append("")
    lines.append(f"- Total locations checked: **{s.get('total_locations', 0)}**")
    lines.append(f"- Passed: **{s.get('pass_count', 0)}**")
    lines.append(f"- Failed: **{s.get('fail_count', 0)}**")
    lines.append(f"- Maximum utilisation: **{s.get('max_utilisation', 0):.4f}**")
    lines.append(
        f"- Minimum calculated life: **{s.get('min_calculated_life_years', 'inf')} years**"
    )
    lines.append(f"- Overall status: **{s.get('overall_status', 'N/A')}**")
    lines.append("")

    # Detail table
    lines.append("## Location Details")
    lines.append("")
    lines.append(
        "| Location | S-N Curve | Damage D | DFF | Utilisation | "
        "Life (yr) | Status |"
    )
    lines.append(
        "|----------|-----------|----------|-----|-------------|"
        "-----------|--------|"
    )

    for loc in report.locations:
        life_str = (
            f"{loc.calculated_life_years:.1f}"
            if loc.calculated_life_years < 1e6
            else "∞"
        )
        lines.append(
            f"| {loc.location_id} | {loc.sn_curve} | "
            f"{loc.cumulative_damage:.4f} | {loc.dff:.1f} | "
            f"{loc.utilisation:.4f} | {life_str} | "
            f"**{loc.pass_fail}** |"
        )

    lines.append("")

    # Notes for failed locations
    failed = [loc for loc in report.locations if loc.pass_fail == "FAIL"]
    if failed:
        lines.append("## Failed Locations — Actions Required")
        lines.append("")
        for loc in failed:
            lines.append(f"- **{loc.location_id}**: Utilisation {loc.utilisation:.4f}")
            if loc.notes:
                lines.append(f"  - {loc.notes}")
            if loc.inspection_interval is not None:
                lines.append(
                    f"  - Recommended inspection interval: "
                    f"{loc.inspection_interval:.1f} years"
                )
        lines.append("")

    return "\n".join(lines)


def report_to_dict(report: FatigueReport) -> dict:
    """Convert report to a plain dict suitable for JSON serialisation.

    Parameters
    ----------
    report : FatigueReport

    Returns
    -------
    dict
    """
    return report.model_dump()


def damage_barchart_data(report: FatigueReport) -> Dict[str, list]:
    """Extract data for a cumulative damage bar chart.

    Parameters
    ----------
    report : FatigueReport

    Returns
    -------
    dict
        Keys: ``"labels"`` (list[str]), ``"damages"`` (list[float]),
        ``"utilisations"`` (list[float]), ``"colors"`` (list[str]).
    """
    labels = []
    damages = []
    utilisations = []
    colors = []

    for loc in report.locations:
        labels.append(loc.location_id)
        damages.append(loc.cumulative_damage)
        utilisations.append(loc.utilisation)
        colors.append("green" if loc.pass_fail == "PASS" else "red")

    return {
        "labels": labels,
        "damages": damages,
        "utilisations": utilisations,
        "colors": colors,
    }


def inspection_recommendations(
    locations: List[FatigueCheckLocation],
    base_interval_years: float = 5.0,
) -> List[Dict[str, Union[str, float]]]:
    """Generate inspection interval recommendations.

    Simple risk-based approach: high-utilisation locations get shorter
    inspection intervals.

    Parameters
    ----------
    locations : list[FatigueCheckLocation]
    base_interval_years : float
        Base inspection interval for utilisation ≈ 0.5 (years).

    Returns
    -------
    list[dict]
        Each dict: ``location_id``, ``utilisation``, ``interval_years``,
        ``priority``.
    """
    recs = []
    for loc in locations:
        u = max(loc.utilisation, 0.001)
        if u >= 1.0:
            interval = base_interval_years * 0.25
            priority = "CRITICAL"
        elif u >= 0.8:
            interval = base_interval_years * 0.5
            priority = "HIGH"
        elif u >= 0.5:
            interval = base_interval_years
            priority = "MEDIUM"
        else:
            interval = base_interval_years * 2.0
            priority = "LOW"

        recs.append(
            {
                "location_id": loc.location_id,
                "utilisation": round(u, 4),
                "interval_years": round(interval, 1),
                "priority": priority,
            }
        )

    return sorted(recs, key=lambda r: r["utilisation"], reverse=True)


# ---------------------------------------------------------------------------
# HTML report on the shared backbone (report-as-backbone, #1018/#1019/#1020)
# ---------------------------------------------------------------------------
#
# Tracer-bullet migration of a non-diffraction reporter onto the shared
# ``digitalmodel.reporting`` block library. Every section is a *view* over the
# ``FatigueReport`` single-source-of-truth (read here, never copied/pasted), and
# provenance is mandatory (a required argument; ``assemble_report`` also fails
# closed). The existing markdown/dict/chart helpers are unchanged.


def _sec_fatigue_summary(report: "FatigueReport", **_) -> str:
    s = report.summary
    meta_rows = "".join(
        f"<tr><td>{html.escape(k)}</td><td>{html.escape(str(v))}</td></tr>"
        for k, v in (
            ("Project", report.project),
            ("Structure", report.structure),
            ("Analyst", report.analyst),
            ("Date", report.date),
            ("Standard", report.standard),
        )
        if v
    )
    summary_rows = "".join(
        f"<tr><td>{html.escape(str(k))}</td><td>{html.escape(str(v))}</td></tr>"
        for k, v in s.items()
    )
    return (
        '<div class="section" id="summary"><h2>Summary</h2>'
        f"<table><tbody>{meta_rows}</tbody></table>"
        f"<table><tbody>{summary_rows}</tbody></table></div>"
    )


def _sec_fatigue_locations(report: "FatigueReport", **_) -> str:
    rows = []
    for loc in report.locations:
        life = (
            "inf"
            if loc.calculated_life_years == float("inf")
            else f"{loc.calculated_life_years:.1f}"
        )
        rows.append(
            "<tr>"
            f"<td>{html.escape(loc.location_id)}</td>"
            f"<td>{html.escape(loc.sn_curve)}</td>"
            f"<td>{html.escape(f'{loc.utilisation:.3f}')}</td>"
            f"<td>{html.escape(life)}</td>"
            f"<td>{html.escape(loc.pass_fail)}</td>"
            "</tr>"
        )
    body = "".join(rows) or '<tr><td colspan="5"><em>No locations.</em></td></tr>'
    return (
        '<div class="section" id="locations"><h2>Fatigue check locations</h2>'
        "<table><thead><tr><th>Location</th><th>S-N curve</th>"
        "<th>Utilisation</th><th>Calc. life (yr)</th><th>Result</th></tr></thead>"
        f"<tbody>{body}</tbody></table></div>"
    )


def _sec_fatigue_inspection(report: "FatigueReport", **_) -> str:
    recs = inspection_recommendations(report.locations)
    rows = "".join(
        "<tr>"
        f"<td>{html.escape(str(r['location_id']))}</td>"
        f"<td>{html.escape(str(r['utilisation']))}</td>"
        f"<td>{html.escape(str(r['interval_years']))}</td>"
        f"<td>{html.escape(str(r['priority']))}</td>"
        "</tr>"
        for r in recs
    )
    body = rows or '<tr><td colspan="4"><em>No locations.</em></td></tr>'
    return (
        '<div class="section" id="inspection"><h2>Inspection recommendations</h2>'
        "<table><thead><tr><th>Location</th><th>Utilisation</th>"
        "<th>Interval (yr)</th><th>Priority</th></tr></thead>"
        f"<tbody>{body}</tbody></table></div>"
    )


def report_to_html(
    report: "FatigueReport",
    *,
    provenance: "Provenance",
    output_path: Optional[Union[str, Path]] = None,
) -> Union[str, Path]:
    """Render a FatigueReport as a provenance-gated HTML report (#1020).

    Assembles from the shared ``digitalmodel.reporting`` block library; each
    section is a view over ``report`` (the SSOT). ``provenance`` is mandatory --
    declare where the fatigue results came from. Returns the HTML string, or
    writes to ``output_path`` and returns it.
    """
    from digitalmodel.reporting import ReportSection, SectionMode, assemble_report

    sections = [
        ReportSection("summary", "Summary", SectionMode.ALWAYS, _sec_fatigue_summary),
        ReportSection(
            "locations", "Locations", SectionMode.ALWAYS, _sec_fatigue_locations
        ),
        ReportSection(
            "inspection", "Inspection", SectionMode.ALWAYS, _sec_fatigue_inspection
        ),
    ]
    title = f"Fatigue assessment - {report.structure or report.project or 'report'}"
    return assemble_report(
        title,
        provenance=provenance,
        sections=sections,
        data=report,
        output_path=output_path,
    )
