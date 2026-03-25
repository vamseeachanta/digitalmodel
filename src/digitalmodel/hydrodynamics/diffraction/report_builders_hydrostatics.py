#!/usr/bin/env python3
"""
Diffraction Report HTML Section Builders — Hydrostatics & Coefficients

ABOUTME: Stability, natural periods, added mass, damping, coupling, and
infinite added mass builders. Split from report_builders.py (WRK-593).

Imports from report_data_models and report_builders_header.
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
    DiffractionReportData,
)
from digitalmodel.hydrodynamics.diffraction.report_builders_header import (
    _get_hull_type_note,
)


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


# _build_infinite_added_mass_html moved to report_builders_responses
# Re-exported here for backward compat
from digitalmodel.hydrodynamics.diffraction.report_builders_responses import (  # noqa: F401
    _build_infinite_added_mass_html,
)
