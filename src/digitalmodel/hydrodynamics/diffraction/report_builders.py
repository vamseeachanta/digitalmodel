#!/usr/bin/env python3
"""
Diffraction Report HTML Section Builders

ABOUTME: All _build_*_html() functions for generating HTML report sections.
Extracted from report_generator.py as part of WRK-591 God Object split.

Imports from report_data_models ONLY (no circular deps with computations/extractors).
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
  The key metric is the <strong>adjacent panel ratio</strong> (size ratio between neighboring panels) —
  gradual size variation is acceptable even with a large global min/max ratio.
  Only abrupt transitions between adjacent panels degrade BEM solver accuracy.
  Advanced mesh quality metrics (Jacobian, aspect ratio, skewness, orthogonality)
  are available via the gmsh meshing tool.
</p>""")

    parts.append("</div>")
    return "\n".join(parts)


def _build_stability_html(data: DiffractionReportData) -> str:
    """Build stability assessment section with GM, BM, KB, radii of gyration."""
    if not data.hydrostatics:
        return ""

    hs = data.hydrostatics
    C = hs.restoring_matrix
    I = hs.inertia_matrix

    # --- Hydrostatic properties table ---
    cob = ", ".join(f"{v:.4f}" for v in hs.centre_of_buoyancy)
    com = ", ".join(f"{v:.4f}" for v in hs.centre_of_mass)
    cof = ", ".join(f"{v:.4f}" for v in hs.centre_of_floatation)

    props_html = f"""\
<h3>Hydrostatic Properties</h3>
<table style="max-width:600px;">
  <thead><tr><th>Property</th><th>Value</th></tr></thead>
  <tbody>
    <tr><td>Displaced Volume</td><td>{hs.volume:.4f} m3</td></tr>
    <tr><td>Mass</td><td>{hs.mass:.4f}</td></tr>
    <tr><td>Centre of Buoyancy (x, y, z)</td><td>{cob}</td></tr>
    <tr><td>Centre of Mass (x, y, z)</td><td>{com}</td></tr>
    <tr><td>Waterplane Area (Awp)</td><td>{hs.waterplane_area:.4f} m2</td></tr>
    <tr><td>Waterplane I_xx</td><td>{hs.Lxx:.4f}</td></tr>
    <tr><td>Waterplane I_yy</td><td>{hs.Lyy:.4f}</td></tr>
    <tr><td>Centre of Floatation (x, y)</td><td>{cof}</td></tr>
  </tbody>
</table>"""

    # --- Stability assessment ---
    stab_rows = []
    if data.gm_transverse is not None:
        gmt = data.gm_transverse
        if gmt <= 0:
            s, c = "UNSTABLE", "#e74c3c"
        elif gmt < 1.0:
            s, c = "WARNING", "#f39c12"
        else:
            s, c = "OK", "#27ae60"
        stab_rows.append(
            f"<tr><td>GM_T (transverse metacentric height)</td>"
            f"<td>{gmt:.4f} m</td>"
            f"<td style='color:{c};font-weight:bold'>{s}</td></tr>"
        )
    if data.gm_longitudinal is not None:
        stab_rows.append(
            f"<tr><td>GM_L (longitudinal metacentric height)</td>"
            f"<td>{data.gm_longitudinal:.4f} m</td><td>-</td></tr>"
        )
    if data.bm_transverse is not None:
        stab_rows.append(
            f"<tr><td>BM_T (transverse)</td>"
            f"<td>{data.bm_transverse:.4f} m</td><td>-</td></tr>"
        )
    if data.bm_longitudinal is not None:
        stab_rows.append(
            f"<tr><td>BM_L (longitudinal)</td>"
            f"<td>{data.bm_longitudinal:.4f} m</td><td>-</td></tr>"
        )
    if data.kb is not None:
        stab_rows.append(
            f"<tr><td>KB (vertical centre of buoyancy)</td>"
            f"<td>{data.kb:.4f} m</td><td>-</td></tr>"
        )

    # Cross-check: GM_T approx KB + BM_T - KG
    crosscheck_html = ""
    if (data.kb is not None and data.bm_transverse is not None
            and data.gm_transverse is not None and len(hs.centre_of_mass) >= 3):
        kg = hs.centre_of_mass[2]
        gm_check = data.kb + data.bm_transverse - kg
        diff = abs(gm_check - data.gm_transverse)
        crosscheck_html = (
            f'<p style="font-size:0.85em;color:#666;margin-top:0.5em;">'
            f'Cross-check: KB + BM_T - KG = {data.kb:.3f} + {data.bm_transverse:.3f} '
            f'- ({kg:.3f}) = {gm_check:.3f} m '
            f'(vs GM_T = {data.gm_transverse:.3f} m, diff = {diff:.4f} m)</p>'
        )

    stab_html = ""
    if stab_rows:
        stab_html = (
            '<h3 style="margin-top:1em;">Stability Assessment</h3>'
            '<table style="max-width:700px;">'
            "<thead><tr><th>Parameter</th><th>Value</th><th>Status</th></tr></thead>"
            f"<tbody>{''.join(stab_rows)}</tbody></table>"
            '<p style="font-size:0.85em;color:#666;">Reference: DNV-OS-C301 requires '
            'GM_T &gt; 1.0m for intact stability of mobile offshore units.</p>'
            f'{crosscheck_html}'
        )

    # --- Radii of gyration ---
    rog_html = ""
    if data.radii_of_gyration:
        rog = data.radii_of_gyration
        rog_rows = []
        for key, label in [("r_xx", "r_xx (roll)"), ("r_yy", "r_yy (pitch)"), ("r_zz", "r_zz (yaw)")]:
            if key in rog:
                rog_rows.append(f"<tr><td>{label}</td><td>{rog[key]:.4f} m</td></tr>")
        if rog_rows:
            rog_html = (
                '<h3 style="margin-top:1em;">Radii of Gyration</h3>'
                '<table style="max-width:400px;">'
                "<thead><tr><th>Parameter</th><th>Value</th></tr></thead>"
                f"<tbody>{''.join(rog_rows)}</tbody></table>"
            )

    # --- Restoring matrix ---
    dof_labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
    matrix_rows = ""
    for i in range(6):
        cells = "".join(
            f"<td class='{'highlight' if abs(C[i][j]) > 1e-6 else ''}'>"
            f"{C[i][j]:.4f}</td>"
            for j in range(6)
        )
        matrix_rows += f"<tr><th>{dof_labels[i]}</th>{cells}</tr>\n"
    matrix_header = "".join(f"<th>{d}</th>" for d in dof_labels)

    restoring_html = f"""\
<h3 style="margin-top:1em;">Restoring Matrix C (Hydrostatic Stiffness)</h3>
<table class="matrix-table">
  <thead><tr><th></th>{matrix_header}</tr></thead>
  <tbody>{matrix_rows}</tbody>
</table>
<p style="font-size:0.85em;color:#666;">
  Heave (C_33), Roll (C_44), and Pitch (C_55) have hydrostatic restoring.
  Surge, Sway, and Yaw are neutrally stable -- restoring comes from mooring (not modelled in diffraction).
</p>"""

    # --- Inertia matrix ---
    inertia_rows = ""
    for i in range(6):
        cells = "".join(
            f"<td class='{'highlight' if abs(I[i][j]) > 1e-6 else ''}'>"
            f"{I[i][j]:.4e}</td>"
            for j in range(6)
        )
        inertia_rows += f"<tr><th>{dof_labels[i]}</th>{cells}</tr>\n"

    inertia_html = f"""\
<h3 style="margin-top:1em;">Inertia Matrix</h3>
<table class="matrix-table">
  <thead><tr><th></th>{matrix_header}</tr></thead>
  <tbody>{inertia_rows}</tbody>
</table>"""

    # Hull type note
    hull_note = _get_hull_type_note(data.hull_type, "stability")

    return f"""\
<div class="section" id="hydrostatics">
  <h2>Hydrostatic Properties & Stability</h2>
  {props_html}
  {stab_html}
  {rog_html}
  {restoring_html}
  {inertia_html}
  {hull_note}
</div>
"""


def _build_natural_periods_html(data: DiffractionReportData) -> str:
    """Build natural period estimates section."""
    if not data.natural_periods:
        return ""

    np_data = data.natural_periods
    rows = []
    for dof in DOF_NAMES:
        tn = np_data.get(dof)
        if tn is not None:
            fn = 1.0 / tn if tn > 0 else 0
            wn = period_s_to_rad_per_s(tn) if tn > 0 else 0
            rows.append(
                f"<tr><td>{dof.capitalize()}</td>"
                f"<td>{tn:.3f}</td><td>{fn:.4f}</td><td>{wn:.4f}</td></tr>"
            )
        else:
            rows.append(
                f"<tr><td>{dof.capitalize()}</td>"
                "<td colspan='3' style='text-align:center;color:#999;'>"
                "Depends on mooring stiffness</td></tr>"
            )

    hull_note = _get_hull_type_note(data.hull_type, "natural_periods")

    return f"""\
<div class="section" id="natural-periods">
  <h2>Natural Period Estimates</h2>
  <table style="max-width:600px;">
    <thead><tr><th>DOF</th><th>T_n (s)</th><th>f_n (Hz)</th><th>omega_n (rad/s)</th></tr></thead>
    <tbody>{''.join(rows)}</tbody>
  </table>
  <p style="font-size:0.85em;color:#666;margin-top:0.5em;">
    T_n = 2pi * sqrt((M_ii + A_ii(omega_n)) / C_ii). Surge, Sway, and Yaw natural periods
    depend on mooring stiffness (not available in diffraction analysis).
    These natural periods should correspond to RAO peak locations. Discrepancy &gt; 5% warrants investigation.
  </p>
  {hull_note}
</div>
"""


def _build_added_mass_diagonal_html(data: DiffractionReportData) -> str:
    """Build added mass diagonal A_ii(omega) line plots."""
    if not data.added_mass_diagonal or not data.periods_s:
        return ""

    fig = make_subplots(
        rows=3, cols=2,
        subplot_titles=[f"A_{i+1}{i+1} ({dof.capitalize()})" for i, dof in enumerate(DOF_NAMES)],
        vertical_spacing=0.08,
        horizontal_spacing=0.1,
    )

    for i, dof in enumerate(DOF_NAMES):
        row = i // 2 + 1
        col = i % 2 + 1
        values = data.added_mass_diagonal.get(dof, [])
        if values:
            fig.add_trace(
                go.Scatter(
                    x=data.periods_s, y=values,
                    mode="lines", name=f"A_{i+1}{i+1}",
                    line=dict(color="#1f77b4", width=2),
                    showlegend=False,
                ),
                row=row, col=col,
            )
            # Mark natural period if available
            if data.natural_periods and data.natural_periods.get(dof):
                tn = data.natural_periods[dof]
                fig.add_vline(
                    x=tn, line_dash="dash", line_color="#e74c3c",
                    line_width=1, row=row, col=col,
                    annotation_text=f"T_n={tn:.1f}s",
                    annotation_font_size=9,
                )
        fig.update_xaxes(title_text="Period (s)", row=row, col=col)
        fig.update_yaxes(title_text="Added Mass", row=row, col=col)

    fig.update_layout(height=800, template="plotly_white",
                      margin=dict(l=60, r=40, t=60, b=40))

    plot_html = fig.to_html(full_html=False, include_plotlyjs=False, div_id="added-mass-diag")

    hull_note = _get_hull_type_note(data.hull_type, "coefficients")

    return f"""\
<div class="section">
  <h3>Added Mass A(omega) -- Diagonal Terms</h3>
  <p style="font-size:0.85em;color:#666;">
    Added mass represents entrained water inertia. Generally increases at low frequencies.
    Dashed red lines mark natural periods from Section 6.
  </p>
  <div class="plot-container">{plot_html}</div>
  {hull_note}
</div>
"""


def _build_damping_diagonal_html(data: DiffractionReportData) -> str:
    """Build radiation damping diagonal B_ii(omega) line plots."""
    if not data.damping_diagonal or not data.periods_s:
        return ""

    fig = make_subplots(
        rows=3, cols=2,
        subplot_titles=[f"B_{i+1}{i+1} ({dof.capitalize()})" for i, dof in enumerate(DOF_NAMES)],
        vertical_spacing=0.08,
        horizontal_spacing=0.1,
    )

    for i, dof in enumerate(DOF_NAMES):
        row = i // 2 + 1
        col = i % 2 + 1
        values = data.damping_diagonal.get(dof, [])
        if values:
            fig.add_trace(
                go.Scatter(
                    x=data.periods_s, y=values,
                    mode="lines", name=f"B_{i+1}{i+1}",
                    line=dict(color="#ff7f0e", width=2),
                    showlegend=False,
                ),
                row=row, col=col,
            )
            if data.natural_periods and data.natural_periods.get(dof):
                tn = data.natural_periods[dof]
                fig.add_vline(
                    x=tn, line_dash="dash", line_color="#e74c3c",
                    line_width=1, row=row, col=col,
                    annotation_text=f"T_n={tn:.1f}s",
                    annotation_font_size=9,
                )
        fig.update_xaxes(title_text="Period (s)", row=row, col=col)
        fig.update_yaxes(title_text="Damping", row=row, col=col)

    fig.update_layout(height=800, template="plotly_white",
                      margin=dict(l=60, r=40, t=60, b=40))

    plot_html = fig.to_html(full_html=False, include_plotlyjs=False, div_id="damping-diag")

    return f"""\
<div class="section">
  <h3>Radiation Damping B(omega) -- Diagonal Terms</h3>
  <p style="font-size:0.85em;color:#666;">
    Radiation damping represents energy lost to radiated waves. Peaks near natural frequency.
    Zero at omega=0 and omega->infinity.
  </p>
  <div class="plot-container">{plot_html}</div>
</div>
"""


def _build_coupling_assessment_html(data: DiffractionReportData) -> str:
    """Build off-diagonal coupling significance assessment."""
    if not data.coupling_significance:
        return ""

    coupling_meanings = {
        "A_15": "Surge-Pitch: ship-like forms, CoG offset",
        "A_51": "Pitch-Surge: ship-like forms, CoG offset",
        "A_24": "Sway-Roll: asymmetric or ship-like forms",
        "A_42": "Roll-Sway: asymmetric or ship-like forms",
        "A_26": "Sway-Yaw: beam-sea effect",
        "A_62": "Yaw-Sway: beam-sea effect",
    }

    rows = []
    for key, ratio in sorted(data.coupling_significance.items(), key=lambda x: -x[1]):
        meaning = coupling_meanings.get(key, "")
        color = "#e74c3c" if ratio > 0.2 else "#f39c12" if ratio > 0.1 else "#27ae60"
        rows.append(
            f"<tr><td>{key}</td><td style='color:{color};font-weight:bold'>"
            f"{ratio:.1%}</td><td>{meaning}</td></tr>"
        )

    return f"""\
<div class="section">
  <h3>Coupling Assessment</h3>
  <p style="font-size:0.85em;color:#666;">
    Off-diagonal coefficients with max |A_ij| / max(|A_ii|, |A_jj|) &gt; 5% at any frequency.
    Significant coupling affects motion predictions and should be considered in design.
  </p>
  <table style="max-width:700px;">
    <thead><tr><th>Coupling</th><th>Max Ratio</th><th>Physical Meaning</th></tr></thead>
    <tbody>{''.join(rows)}</tbody>
  </table>
</div>
"""


def _build_infinite_added_mass_html(
    inf_am: List[List[float]],
) -> str:
    """Build infinite frequency added mass matrix table."""
    dof_labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
    header = "".join(f"<th>{d}</th>" for d in dof_labels)
    rows = ""
    for i in range(6):
        cells = "".join(
            f"<td class='{'highlight' if abs(inf_am[i][j]) > 1e-6 else ''}'>"
            f"{inf_am[i][j]:.6f}</td>"
            for j in range(6)
        )
        rows += f"<tr><th>{dof_labels[i]}</th>{cells}</tr>\n"

    return f"""\
<div class="section">
  <h2>Infinite Frequency Added Mass</h2>
  <table class="matrix-table">
    <thead><tr><th></th>{header}</tr></thead>
    <tbody>{rows}</tbody>
  </table>
</div>
"""


def _build_load_raos_html(lr: LoadRAOData, include_plotlyjs: str) -> str:
    """Build Load RAOs section with per-DOF plots."""
    n_freq = len(lr.frequencies_rad_s)
    periods = lr.periods_s
    headings = lr.headings_deg

    # Create a 3x2 subplot grid for 6 DOFs
    fig = make_subplots(
        rows=3, cols=2,
        subplot_titles=[
            f"{dof.capitalize()} ({LOAD_RAO_UNITS[i]})"
            for i, dof in enumerate(DOF_NAMES)
        ],
        vertical_spacing=0.08,
        horizontal_spacing=0.08,
    )

    colors = [
        "#e74c3c", "#3498db", "#2ecc71", "#f39c12",
        "#9b59b6", "#1abc9c", "#e67e22", "#34495e",
    ]

    for dof_idx, dof in enumerate(DOF_NAMES):
        row = dof_idx // 2 + 1
        col = dof_idx % 2 + 1
        amp = lr.amplitude[dof]  # [nfreq x nheading]

        for h_idx, heading in enumerate(headings):
            values = [amp[f_idx][h_idx] for f_idx in range(n_freq)]
            # Skip if all near-zero
            if max(abs(v) for v in values) < 1e-10:
                continue
            fig.add_trace(
                go.Scatter(
                    x=periods,
                    y=values,
                    mode="lines",
                    name=f"{heading:.0f}°",
                    line=dict(color=colors[h_idx % len(colors)]),
                    legendgroup=f"h{h_idx}",
                    showlegend=(dof_idx == 0),
                ),
                row=row, col=col,
            )

    fig.update_layout(
        height=900,
        title_text=f"Load RAOs (Wave Excitation Forces) — {lr.method.capitalize()} Method",
        font=dict(size=11),
        margin=dict(l=60, r=140, t=80, b=50),
        legend=dict(
            orientation="v", x=1.02, y=1,
            font=dict(size=10),
            tracegroupgap=2,
        ),
    )
    for i in range(1, 7):
        row = (i - 1) // 2 + 1
        col = (i - 1) % 2 + 1
        fig.update_xaxes(title_text="Period (s)", row=row, col=col)
        fig.update_yaxes(title_text="Amplitude", row=row, col=col)

    plot_html = fig.to_html(
        full_html=False,
        include_plotlyjs=False,
        div_id="load-raos-plot",
    )

    return f"""\
<div class="section">
  <h2>Load RAOs (Wave Excitation Forces)</h2>
  <div class="plot-container">{plot_html}</div>
</div>
"""


def _build_roll_damping_html(rd: RollDampingData, include_plotlyjs: str) -> str:
    """Build roll critical damping section with annotated plot."""
    fig = go.Figure()

    # Plot roll damping as % of critical vs period
    fig.add_trace(go.Scatter(
        x=rd.periods_s,
        y=rd.roll_damping_percent_critical,
        mode="lines+markers",
        name="Roll Damping (% Critical)",
        line=dict(color="#e74c3c", width=2),
        marker=dict(size=4),
    ))

    # Annotate peak roll RAO period
    if rd.peak_roll_rao_period and rd.zeta_at_peak is not None:
        fig.add_vline(
            x=rd.peak_roll_rao_period,
            line_dash="dash",
            line_color="#2c3e50",
            line_width=2,
            annotation_text=(
                f"Peak Roll RAO: T={rd.peak_roll_rao_period:.2f}s"
                f"\nζ={rd.zeta_at_peak:.4f}%"
            ),
            annotation_position="top right",
            annotation_font_size=11,
            annotation_bgcolor="rgba(255,255,255,0.8)",
        )

    fig.update_layout(
        title="Roll Radiation Damping as % of Critical Damping",
        xaxis_title="Period (s)",
        yaxis_title="% Critical Damping",
        height=450,
        font=dict(size=12),
        margin=dict(l=60, r=60, t=60, b=50),
    )

    plot_html = fig.to_html(
        full_html=False,
        include_plotlyjs=False,
        div_id="roll-damping-plot",
    )

    # Key values table
    peak_row = ""
    if rd.peak_roll_rao_period:
        peak_idx = _find_closest_idx(rd.periods_s, rd.peak_roll_rao_period)
        peak_row = f"""\
      <tr class="highlight">
        <td>Peak Roll RAO Period (T_peak)</td>
        <td>{rd.peak_roll_rao_period:.4f} s</td>
      </tr>
      <tr class="highlight">
        <td>ζ at T_peak</td>
        <td>{rd.zeta_at_peak:.6f} %</td>
      </tr>
      <tr>
        <td>B_44 at T_peak</td>
        <td>{rd.B_44[peak_idx]:.6f}</td>
      </tr>
      <tr>
        <td>A_44 at T_peak</td>
        <td>{rd.A_44[peak_idx]:.6f}</td>
      </tr>"""

    return f"""\
<div class="section">
  <h2>Roll Critical Damping Analysis</h2>
  <p>Critical damping ratio: ζ(ω) = B₄₄(ω) / (2√((A₄₄(ω) + I₄₄) × C₄₄))</p>
  <div class="plot-container">{plot_html}</div>
  <table>
    <thead><tr><th>Parameter</th><th>Value</th></tr></thead>
    <tbody>
      <tr><td>C_44 (Hydrostatic Restoring)</td><td>{rd.C_44:.6f}</td></tr>
      <tr><td>I_44 (Roll Inertia)</td><td>{rd.I_44:.6f}</td></tr>
      {peak_row}
    </tbody>
  </table>
</div>
"""


def _build_phase_guide_html() -> str:
    """Build static phase interpretation guide."""
    return """\
<div class="section">
  <h3>Phase Interpretation Guide</h3>
  <ul style="font-size:0.85em;color:#555;">
    <li><strong>Phase = 0 deg</strong>: Response in-phase with wave (crest at origin = peak response)</li>
    <li><strong>Phase = -90 deg</strong>: Lagging (response peaks after wave crest passes)</li>
    <li><strong>Phase = +90 deg</strong>: Leading (response peaks before wave crest arrives)</li>
    <li><strong>~180 deg jump near resonance</strong>: Transition from quasi-static to resonant regime</li>
    <li><strong>Phase at near-zero amplitude</strong>: Physically meaningless -- ignore phase when amplitude is insignificant</li>
  </ul>
</div>
"""


def _build_appendices_html() -> str:
    """Build appendices with notation, theory, and references."""
    return """\
<div class="section" id="appendices">
  <h2>Appendices</h2>

  <h3>A. Notation & Conventions</h3>
  <table style="max-width:600px;font-size:0.85em;">
    <thead><tr><th>Symbol</th><th>Description</th></tr></thead>
    <tbody>
      <tr><td>DOF 1-6</td><td>Surge, Sway, Heave, Roll, Pitch, Yaw</td></tr>
      <tr><td>Coordinate System</td><td>x-forward, z-up, right-hand rule</td></tr>
      <tr><td>A_ii(omega)</td><td>Frequency-dependent added mass (diagonal)</td></tr>
      <tr><td>B_ii(omega)</td><td>Frequency-dependent radiation damping (diagonal)</td></tr>
      <tr><td>C_ii</td><td>Hydrostatic restoring stiffness (diagonal)</td></tr>
      <tr><td>GM_T</td><td>Transverse metacentric height (metres)</td></tr>
      <tr><td>T_n</td><td>Natural period (seconds)</td></tr>
      <tr><td>zeta</td><td>Critical damping ratio (%)</td></tr>
      <tr><td>RAO</td><td>Response Amplitude Operator</td></tr>
    </tbody>
  </table>

  <h3 style="margin-top:1em;">B. Key Formulas</h3>
  <ul style="font-size:0.85em;color:#555;">
    <li>T_n = 2pi * sqrt((M_ii + A_ii(omega_n)) / C_ii)</li>
    <li>zeta(omega) = B_ii(omega) / (2 * sqrt((M_ii + A_ii(omega)) * C_ii))</li>
    <li>GM_T = C_44 / (rho * g * V), where rho=1025 kg/m3, g=9.81 m/s2</li>
    <li>BM_T = I_xx / V; GM_T = KB + BM_T - KG (cross-check)</li>
  </ul>

  <h3 style="margin-top:1em;">C. References</h3>
  <ol style="font-size:0.85em;color:#555;">
    <li>Newman, J.N. (1977). <em>Marine Hydrodynamics</em>. MIT Press.</li>
    <li>Faltinsen, O.M. (1990). <em>Sea Loads on Ships and Offshore Structures</em>. Cambridge.</li>
    <li>Journee, J.M.J. & Massie, W.W. (2001). <em>Offshore Hydromechanics</em>. TU Delft.</li>
    <li>Lee, C.H. (1995). <em>WAMIT Theory Manual</em>. MIT.</li>
    <li>Chakrabarti, S.K. (2005). <em>Handbook of Offshore Engineering</em>. Elsevier.</li>
    <li>DNV-RP-C205 (2021). <em>Environmental Conditions and Environmental Loads</em>.</li>
    <li>DNV-OS-C301. <em>Stability and Watertight Integrity</em>.</li>
  </ol>
</div>
"""
