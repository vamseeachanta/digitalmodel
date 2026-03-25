#!/usr/bin/env python3
"""
Diffraction Report HTML Section Builders — Load RAOs, Roll Damping & Appendices

ABOUTME: Load RAO plots, roll damping section, phase guide, and appendices
builders. Split from report_builders.py (WRK-593 God Object split).

Imports from report_data_models and report_builders_header.
"""

from __future__ import annotations

from typing import List

import plotly.graph_objects as go
from plotly.subplots import make_subplots

from digitalmodel.hydrodynamics.diffraction.report_data_models import (
    DOF_NAMES,
    LOAD_RAO_UNITS,
    LoadRAOData,
    RollDampingData,
)
from digitalmodel.hydrodynamics.diffraction.report_builders_header import (
    _find_closest_idx,
)


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
