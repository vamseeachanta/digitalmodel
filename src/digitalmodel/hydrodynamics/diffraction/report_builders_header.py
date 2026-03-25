#!/usr/bin/env python3
"""
Diffraction Report HTML Section Builders — Header & TOC & Executive Summary

ABOUTME: Header, table of contents, executive summary, and hull description
builders. Split from report_builders.py (WRK-593 God Object split).

Imports from report_data_models ONLY (no circular deps).
"""

from __future__ import annotations

from typing import Dict, List, Optional

import plotly.graph_objects as go
from plotly.subplots import make_subplots

from digitalmodel.hydrodynamics.diffraction.diffraction_units import (
    period_s_to_rad_per_s,
)
from digitalmodel.hydrodynamics.diffraction.report_data_models import (
    DOF_NAMES,
    LOAD_RAO_UNITS,
    DiffractionReportData,
    HydrostaticData,
    LoadRAOData,
    MeshQualityData,
    RollDampingData,
)


HULL_TYPE_NOTES: Dict[str, Dict[str, str]] = {
    "barge": {
        "stability": "Wide beam typically gives large GM_T. Resonance peaks at roll natural period are the primary concern, not stability.",
        "natural_periods": "Roll T_n typically 6-15s. Sharp heave resonance expected due to low damping and high waterplane area.",
        "coefficients": "Negligible surge-pitch and sway-roll coupling expected for symmetric box barge.",
        "roll_damping": "Radiation-only roll damping typically 0.5-2% critical. Viscous contributions essential for realistic response.",
        "excitation": "Broad frequency excitation expected. Large waterplane area drives strong heave excitation.",
    },
    "fpso": {
        "stability": "GM_T typically 2-6m. Turret mooring affects yaw response significantly.",
        "natural_periods": "Roll T_n 12-20s. Pitch T_n 8-12s. Surge-pitch coupling (A_15) significant for ship-like forms.",
        "coefficients": "Surge-pitch coupling significant. Check A_15/A_51 terms carefully.",
        "roll_damping": "Low radiation roll damping (1-5% critical). Viscous damping dominates — bilge keels critical for FPSO operations.",
        "excitation": "Yaw excitation at quartering seas important for mooring design.",
    },
    "tanker": {
        "stability": "Similar characteristics to FPSO. GM_T depends heavily on loading condition.",
        "natural_periods": "Roll T_n 12-20s. Surge-pitch coupling significant.",
        "coefficients": "Ship-like coupling terms expected (A_15, A_24).",
        "roll_damping": "Low radiation roll damping. Bilge keels essential.",
        "excitation": "Full 360-degree heading analysis recommended for spread mooring.",
    },
    "semi_pontoon": {
        "stability": "Column spacing drives GM_T. Typically adequate stability due to wide pontoon separation.",
        "natural_periods": "Heave T_n 18-25s (small Awp). Roll/pitch T_n 30-60s. Well above typical wave periods.",
        "coefficients": "Column interference patterns in added mass/damping. Higher radiation damping than monohulls.",
        "roll_damping": "Radiation damping typically 2-8% critical. Column geometry provides inherent damping.",
        "excitation": "Column interference creates complex excitation patterns in load RAOs.",
    },
    "spar": {
        "stability": "Deep draft provides large KB. GM_T typically small but sufficient.",
        "natural_periods": "Heave T_n 25-35s (very small Awp). Deep draft reduces short-period excitation.",
        "coefficients": "Minimal coupling for axisymmetric spar. VIM not captured by potential flow.",
        "roll_damping": "Damping depends on hull appendages. Strakes often used for VIM suppression.",
        "excitation": "Deep draft significantly reduces wave excitation forces at typical wave periods.",
    },
    "lngc": {
        "stability": "Similar to FPSO. Prismatic midship section. GM varies significantly with cargo loading.",
        "natural_periods": "Roll T_n typically 12-18s. Internal sloshing effects not captured by external diffraction.",
        "coefficients": "Ship-like coupling. Sloshing effects require separate analysis.",
        "roll_damping": "Roll damping critical for LNG cargo transfer operations. Bilge keels essential.",
        "excitation": "Side-by-side operations require multi-body analysis (not captured here).",
    },
    "cylinder": {
        "stability": "Analytical solutions available (McCamy-Fuchs). Useful for code validation.",
        "natural_periods": "Depends on draft-to-radius ratio. Analytical natural periods available for verification.",
        "coefficients": "Axisymmetric — no coupling between translational and rotational DOFs.",
        "roll_damping": "Radiation damping only. No bilge keel effects.",
        "excitation": "McCamy-Fuchs analytical solution available for comparison.",
    },
    "sphere": {
        "stability": "Analytical solutions available (Hulme). Useful for code validation.",
        "natural_periods": "Heave dominated. Analytical solution available.",
        "coefficients": "Perfectly symmetric — all cross-couplings should be zero.",
        "roll_damping": "Minimal roll damping for submerged sphere.",
        "excitation": "Hulme analytical solution available for comparison.",
    },
}


def _get_hull_type_note(hull_type: Optional[str], section: str) -> str:
    """Get hull-type-specific interpretation note for a section."""
    if not hull_type or hull_type not in HULL_TYPE_NOTES:
        return ""
    notes = HULL_TYPE_NOTES.get(hull_type, {})
    note_text = notes.get(section, "")
    if not note_text:
        return ""
    return (
        f'<div style="background:#eaf2f8;border-left:4px solid #3498db;'
        f'padding:0.6em 1em;margin-top:0.8em;border-radius:4px;font-size:0.85em;">'
        f'<strong>Hull Type Note ({hull_type}):</strong> {note_text}</div>'
    )


def _find_closest_idx(values: List[float], target: float) -> int:
    """Find index of closest value in a list."""
    return min(range(len(values)), key=lambda i: abs(values[i] - target))


def _build_header_html(data: DiffractionReportData) -> str:
    n_freq = len(data.frequencies_rad_s)
    n_head = len(data.headings_deg)
    headings_str = ", ".join(f"{h:.1f}" for h in data.headings_deg)
    solvers_str = ", ".join(data.solver_names) if data.solver_names else "N/A"

    title = data.report_title or f"Diffraction Analysis Report &mdash; {data.vessel_name}"
    subtitle_html = (
        f'<div class="subtitle">{data.report_subtitle}</div>'
        if data.report_subtitle else ""
    )

    nav_html = ""
    if data.benchmark_html_sections and "navigation" in data.benchmark_html_sections:
        nav_html = data.benchmark_html_sections["navigation"]

    return f"""\
<div class="report-header">
  <h1>{title}</h1>
  {subtitle_html}
  <div class="meta">
    Generated: {data.report_date} |
    Frequencies: {n_freq} |
    Headings: {n_head} ({headings_str}&deg;) |
    Solver(s): {solvers_str} |
    Source: {data.source_file or 'N/A'}
  </div>
  {nav_html}
</div>
"""


def _build_toc_html(data: DiffractionReportData) -> str:
    """Build table of contents with anchor links."""
    is_multi = len(data.solver_names) >= 2
    compact = data.mode == "compact"

    entries = [
        ("header", "Header & Identification", True),
        ("executive-summary", "Executive Summary", True),
        ("hull-mesh", "Hull Description & Mesh Quality", not compact),
        ("input-config", "Input Configuration", is_multi and not compact),
        ("hydrostatics", "Hydrostatic Properties & Stability", True),
        ("natural-periods", "Natural Period Estimates", not compact),
        ("hydro-coefficients", "Hydrodynamic Coefficients", not compact),
        ("load-raos", "Wave Excitation Forces (Load RAOs)", not compact),
        ("displacement-raos", "Displacement RAOs (Motion Response)", True),
        ("roll-damping", "Roll Damping Assessment", True),
        ("raw-data", "Raw Data & Overlay Plots", not compact),
        ("appendices", "Appendices", not compact),
    ]

    links = []
    for idx, (anchor, label, show) in enumerate(entries):
        if show:
            links.append(
                f'<li><a href="#{anchor}">{idx}. {label}</a></li>'
            )

    return f"""\
<div class="section" id="toc">
  <h2>Table of Contents</h2>
  <ul style="list-style:none;padding:0;column-count:2;font-size:0.9em;">
    {''.join(links)}
  </ul>
</div>
"""


def _build_executive_summary_html(data: DiffractionReportData) -> str:
    """Build executive summary with key findings and warnings."""
    rows = []

    # Mesh quality
    if data.mesh_quality:
        mq = data.mesh_quality
        mq_status = "PASS" if mq.area_ratio < 10 and mq.panel_count >= 100 else "WARN"
        mq_color = "#27ae60" if mq_status == "PASS" else "#f39c12"
        rows.append(
            f"<tr><td>Mesh Quality</td>"
            f"<td>{mq.panel_count} panels, ratio {mq.area_ratio:.1f}</td>"
            f"<td style='color:{mq_color};font-weight:bold'>{mq_status}</td></tr>"
        )

    # GM_T
    if data.gm_transverse is not None:
        gmt = data.gm_transverse
        if gmt <= 0:
            gm_status, gm_color = "UNSTABLE", "#e74c3c"
        elif gmt < 1.0:
            gm_status, gm_color = "WARNING", "#f39c12"
        else:
            gm_status, gm_color = "OK", "#27ae60"
        rows.append(
            f"<tr><td>GM_T (transverse)</td>"
            f"<td>{gmt:.3f} m</td>"
            f"<td style='color:{gm_color};font-weight:bold'>{gm_status}</td></tr>"
        )

    if data.gm_longitudinal is not None:
        rows.append(
            f"<tr><td>GM_L (longitudinal)</td>"
            f"<td>{data.gm_longitudinal:.3f} m</td>"
            f"<td>-</td></tr>"
        )

    # Natural roll period
    if data.natural_periods and data.natural_periods.get("roll"):
        tn_roll = data.natural_periods["roll"]
        rows.append(
            f"<tr><td>Natural Roll Period</td>"
            f"<td>{tn_roll:.2f} s</td>"
            f"<td>-</td></tr>"
        )

    # Roll damping at resonance
    if data.roll_damping and data.roll_damping.zeta_at_peak is not None:
        zeta = data.roll_damping.zeta_at_peak
        zeta_status = "LOW" if zeta < 2.0 else "OK"
        zeta_color = "#f39c12" if zeta < 2.0 else "#27ae60"
        rows.append(
            f"<tr><td>Roll Damping at Resonance</td>"
            f"<td>{zeta:.2f}% critical</td>"
            f"<td style='color:{zeta_color};font-weight:bold'>{zeta_status}</td></tr>"
        )

    status_table = ""
    if rows:
        status_table = (
            '<table style="max-width:700px;">'
            "<thead><tr><th>Item</th><th>Value</th><th>Status</th></tr></thead>"
            f"<tbody>{''.join(rows)}</tbody></table>"
        )

    # Peak response summary
    peak_table = ""
    if data.peak_responses:
        peak_rows = []
        for dof in DOF_NAMES:
            pr = data.peak_responses.get(dof, {})
            if pr:
                peak_rows.append(
                    f"<tr><td>{dof.capitalize()}</td>"
                    f"<td>{pr.get('amplitude', 0):.4g}</td>"
                    f"<td>{pr.get('period_s', 0):.2f}</td>"
                    f"<td>{pr.get('heading_deg', 0):.0f}</td>"
                    f"<td>{pr.get('unit', '-')}</td></tr>"
                )
        if peak_rows:
            peak_table = (
                '<h3 style="margin-top:1em;">Peak Response Summary</h3>'
                '<table style="max-width:700px;">'
                "<thead><tr><th>DOF</th><th>Peak Amplitude</th>"
                "<th>Period (s)</th><th>Heading (deg)</th><th>Unit</th></tr></thead>"
                f"<tbody>{''.join(peak_rows)}</tbody></table>"
            )

    # Warnings
    warnings_html = ""
    if data.executive_warnings:
        items = "".join(
            f'<li style="margin:0.3em 0;">{w}</li>'
            for w in data.executive_warnings
        )
        warnings_html = (
            '<div style="background:#fef9e7;border-left:4px solid #f39c12;'
            'padding:0.8em 1em;margin-top:1em;border-radius:4px;">'
            f'<strong>Warnings & Alerts</strong><ul style="margin:0.3em 0;">{items}</ul></div>'
        )

    benchmark_exec = ""
    if data.benchmark_html_sections and data.benchmark_html_sections.get(
        "benchmark_executive",
    ):
        benchmark_exec = data.benchmark_html_sections["benchmark_executive"]

    return f"""\
<div class="section" id="executive-summary">
  <h2>Executive Summary</h2>
  {status_table}
  {peak_table}
  {warnings_html}
  {benchmark_exec}
</div>
"""


def _build_hull_description_html(data: DiffractionReportData) -> str:
    """Build hull description section merging metadata + mesh quality."""
    parts = ['<div class="section" id="hull-mesh">', "<h2>Hull Description & Mesh Quality</h2>"]

    # Hull metadata
    parts.append("<h3>Hull Metadata</h3>")
    meta_rows = [f"<tr><td>Vessel Name</td><td>{data.vessel_name}</td></tr>"]
    if data.hull_type:
        parts.append(f"<p><strong>Hull Type:</strong> {data.hull_type}</p>")
    if data.solver_names:
        meta_rows.append(f"<tr><td>Solver(s)</td><td>{', '.join(data.solver_names)}</td></tr>")
    if data.source_file:
        meta_rows.append(f"<tr><td>Source File</td><td>{data.source_file}</td></tr>")
    parts.append(
        '<table style="max-width:600px;">'
        "<thead><tr><th>Property</th><th>Value</th></tr></thead>"
        f"<tbody>{''.join(meta_rows)}</tbody></table>"
    )

    # Mesh quality
    if data.mesh_quality:
        mq = data.mesh_quality
        count_status = "PASS" if mq.panel_count >= 100 else "WARNING"
        count_color = "#27ae60" if count_status == "PASS" else "#f39c12"

        vertex_row = ""
        if mq.vertex_count is not None:
            vertex_row = f"<tr><td>Vertex Count</td><td>{mq.vertex_count}</td><td>-</td></tr>"

        parts.append(f"""\
<h3 style="margin-top:1em;">Mesh Quality Assessment</h3>
<table style="max-width:600px;">
  <thead><tr><th>Property</th><th>Value</th><th>Status</th></tr></thead>
  <tbody>
    <tr><td>Panel Count</td><td>{mq.panel_count}</td>
        <td style="color:{count_color};font-weight:bold">{count_status}</td></tr>
    {vertex_row}
    <tr><td>Mean Panel Area</td><td>{mq.mean_area:.6f} m2</td><td>-</td></tr>
    <tr><td>Min Panel Area</td><td>{mq.min_area:.6f} m2</td><td>-</td></tr>
    <tr><td>Max Panel Area</td><td>{mq.max_area:.6f} m2</td><td>-</td></tr>
    <tr><td>Area Ratio (max/min)</td><td>{mq.area_ratio:.2f}</td><td>-</td></tr>
  </tbody>
</table>
<p style="font-size:0.85em;color:#666;margin-top:0.5em;">
  Mesh quality directly affects accuracy of added mass, damping, and wave excitation forces.
  The key metric is the <strong>adjacent panel ratio</strong> (size ratio between neighboring panels) --
  gradual size variation is acceptable even with a large global min/max ratio.
  Only abrupt transitions between adjacent panels degrade BEM solver accuracy.
  Advanced mesh quality metrics (Jacobian, aspect ratio, skewness, orthogonality)
  are available via the gmsh meshing tool.
</p>""")

    parts.append("</div>")
    return "\n".join(parts)
