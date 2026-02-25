"""Benchmark pairwise correlation, per-DOF report sections, and data tables.

ABOUTME: Module-level functions for pairwise solver correlation heatmaps,
per-DOF report sections with inline plots + commentary, hydrodynamic
coefficient matrices, and raw RAO data tables.
Extracted from BenchmarkPlotter as part of WRK-592 God Object split.

Imports from benchmark_helpers and benchmark_rao_plots. No circular deps.
"""
from __future__ import annotations

import html as html_mod
from pathlib import Path
from typing import Any, Dict, List, Optional

import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots

from digitalmodel.hydrodynamics.diffraction.benchmark_helpers import (
    DOF_ORDER,
    _AMPLITUDE_UNITS,
    _is_phase_at_negligible_amplitude,
    generate_dof_observations,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_rao_plots import (
    add_solver_traces,
    apply_layout,
    get_heading_indices,
    get_significant_heading_indices,
    get_x_values,
    get_solver_style,
    save_figure,
    x_axis_label,
)
from digitalmodel.hydrodynamics.diffraction.diffraction_units import (
    rad_per_s_to_period_s,
)
from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
    BenchmarkReport,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DOF,
    RAOComponent,
)


# ------------------------------------------------------------------
# Pairwise correlation heatmap
# ------------------------------------------------------------------

def plot_pairwise_correlation_heatmap(
    report: BenchmarkReport,
    output_dir: Path,
) -> Path:
    """Heatmap of mean pairwise magnitude correlation across all DOFs."""
    n = len(report.solver_names)
    matrix = np.ones((n, n), dtype=float)

    name_to_idx = {
        name: idx for idx, name in enumerate(report.solver_names)
    }

    for pair_key, pw_result in report.pairwise_results.items():
        rao_comps = pw_result.rao_comparisons
        corrs = [
            c.magnitude_stats.correlation for c in rao_comps.values()
        ]
        mean_corr = float(np.mean(corrs))
        i = name_to_idx[pw_result.solver_a]
        j = name_to_idx[pw_result.solver_b]
        matrix[i, j] = mean_corr
        matrix[j, i] = mean_corr

    fig = go.Figure(
        data=go.Heatmap(
            z=matrix,
            x=report.solver_names,
            y=report.solver_names,
            colorscale="RdYlGn",
            zmin=0.0,
            zmax=1.0,
            text=np.round(matrix, 3),
            texttemplate="%{text:.3f}",
        )
    )
    apply_layout(fig, "Pairwise Solver Correlation")
    fig.update_layout(height=500)
    return save_figure(fig, "benchmark_heatmap", output_dir)


# ------------------------------------------------------------------
# 6x6 coefficient matrix rendering
# ------------------------------------------------------------------

def render_6x6_matrix(
    corr_dict: dict,
    labels: List[str],
) -> str:
    """Render a 6x6 correlation matrix as an HTML table."""
    rows: List[str] = ['<table class="solver-table" '
                       'style="width:auto;max-width:600px;">']
    rows.append("<tr><th></th>")
    for label in labels:
        rows.append(f"<th>{label[:2]}</th>")
    rows.append("</tr>")

    for i, row_label in enumerate(labels, start=1):
        rows.append(f"<tr><td style='font-weight:600;text-align:left;'>"
                    f"{row_label}</td>")
        for j in range(1, 7):
            val = corr_dict.get((i, j), corr_dict.get(f"{i},{j}", None))
            if val is None:
                rows.append("<td>-</td>")
                continue
            if val >= 0.999:
                bg = "#d5f5e3"
            elif val >= 0.99:
                bg = "#fef9e7"
            else:
                bg = "#fadbd8"
            rows.append(
                f'<td style="background:{bg};text-align:center;">'
                f"{val:.6f}</td>"
            )
        rows.append("</tr>")
    rows.append("</table>")
    return "\n".join(rows)


def build_hydro_coefficients_html(
    report: BenchmarkReport,
) -> str:
    """Build 6x6 added-mass and damping correlation matrices."""
    if not report.pairwise_results:
        return ""

    dof_labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
    parts: List[str] = [
        "<h2>Hydrodynamic Coefficients</h2>"
        "<p>Pairwise correlation of frequency-dependent added-mass "
        "and radiation-damping matrices (6&times;6 DOF). Values "
        "near 1.000 indicate identical coefficients.</p>",
    ]

    for pair_key, pair_result in report.pairwise_results.items():
        parts.append(
            f'<h3 style="font-size:0.95em;color:#555;">'
            f"{html_mod.escape(pair_key)}</h3>"
        )
        for matrix_name, corr_dict in [
            ("Added Mass", pair_result.added_mass_correlations),
            ("Radiation Damping", pair_result.damping_correlations),
        ]:
            parts.append(
                f'<h4 style="margin:0.8em 0 0.3em;">{matrix_name} '
                f"Correlation</h4>"
            )
            parts.append(render_6x6_matrix(corr_dict, dof_labels))

    return "\n".join(parts)


def build_coupling_heatmap_html(
    output_dir: Path,
    am_corr: list[list[float]],
    damp_corr: list[list[float]],
    body_i_name: str,
    body_j_name: str,
) -> Path:
    """Render 6x6 correlation heatmaps for coupling matrices."""
    dof_labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]

    def _to_dict(matrix):
        d = {}
        for i in range(6):
            for j in range(6):
                d[(i + 1, j + 1)] = matrix[i][j]
        return d

    am_dict = _to_dict(am_corr)
    damp_dict = _to_dict(damp_corr)

    am_table = render_6x6_matrix(am_dict, dof_labels)
    damp_table = render_6x6_matrix(damp_dict, dof_labels)

    title = f"Coupling: {body_i_name} \u2194 {body_j_name}"
    safe_i = "".join(c for c in body_i_name if c.isalnum() or c in "_-")
    safe_j = "".join(c for c in body_j_name if c.isalnum() or c in "_-")
    filename = f"coupling_{safe_i}_{safe_j}".lower()

    page = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<title>{html_mod.escape(title)}</title>
<style>
  body {{ margin:20px; font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; }}
  h1 {{ font-size: 1.5em; margin-bottom: 0.5em; }}
  h2 {{ font-size: 1.2em; margin-top: 1.5em; color: #555; }}
  .container {{ max-width: 800px; margin: 0 auto; }}
  table {{ border-collapse: collapse; width: 100%; max-width: 600px; margin-bottom: 20px; }}
  th, td {{ border: 1px solid #ddd; padding: 8px; text-align: center; font-size: 0.9em; }}
  th {{ background-color: #f2f2f2; }}
  .solver-table td {{ padding: 6px; }}
</style>
</head>
<body>
<div class="container">
  <h1>{html_mod.escape(title)}</h1>
  <p>Correlation of frequency-dependent coupling coefficients between .owd and spec.yml results.
     Values near 1.000 indicate identical coefficients.</p>

  <h2>Added Mass Coupling</h2>
  {am_table}

  <h2>Radiation Damping Coupling</h2>
  {damp_table}
</div>
</body>
</html>"""

    path = output_dir / f"{filename}.html"
    path.write_text(page, encoding="utf-8")
    return path


# ------------------------------------------------------------------
# Raw RAO data tables
# ------------------------------------------------------------------

def build_raw_rao_data_html(
    solver_results: Dict[str, Any],
    solver_names: List[str],
    headings: Optional[List[float]] = None,
) -> str:
    """Build collapsible tables of raw RAO magnitude+phase per DOF."""
    parts: List[str] = [
        "<h2>Raw RAO Data</h2>"
        "<p>Full frequency-by-heading magnitude and phase values "
        "for each DOF. Expand each section to inspect.</p>",
    ]

    for dof in DOF_ORDER:
        dof_name = dof.name.lower()
        dof_cap = dof.name.capitalize()
        unit = _AMPLITUDE_UNITS[dof]

        first_comp: RAOComponent = getattr(
            solver_results[solver_names[0]].raos, dof_name,
        )
        h_indices = get_heading_indices(first_comp, headings)
        periods = first_comp.frequencies.periods

        parts.append(
            f"<details><summary><strong>{dof_cap}</strong> "
            f"({len(periods)} freq &times; {len(h_indices)} hdg)"
            f"</summary>"
        )

        tbl: List[str] = ['<div style="overflow-x:auto;">'
                          '<table class="solver-table">']
        tbl.append("<tr><th>T (s)</th>")
        for hi in h_indices:
            hdg_val = first_comp.headings.values[hi]
            for solver in solver_names:
                short = (
                    solver.split("(")[-1].rstrip(")")
                    if "(" in solver else solver
                )
                tbl.append(
                    f"<th>{hdg_val:.0f}&deg; {short}<br>"
                    f"Mag ({unit})</th>"
                    f"<th>{hdg_val:.0f}&deg; {short}<br>"
                    f"Phase (&deg;)</th>"
                )
        tbl.append("</tr>")

        n_freq = min(len(periods), 100)
        for fi in range(n_freq):
            tbl.append(f"<tr><td>{periods[fi]:.4f}</td>")
            for hi in h_indices:
                for solver in solver_names:
                    comp: RAOComponent = getattr(
                        solver_results[solver].raos, dof_name,
                    )
                    mag_val = float(comp.magnitude[fi, hi])
                    phase_val = float(comp.phase[fi, hi])
                    tbl.append(
                        f"<td>{mag_val:.6g}</td>"
                        f"<td>{phase_val:.1f}</td>"
                    )
            tbl.append("</tr>")

        if len(periods) > 100:
            tbl.append(
                f'<tr><td colspan="99" style="text-align:center;'
                f'font-style:italic;">... truncated at 100 of '
                f'{len(periods)} frequencies</td></tr>'
            )

        tbl.append("</table></div>")
        parts.append("\n".join(tbl))
        parts.append("</details>")

    return "\n".join(parts)


# ------------------------------------------------------------------
# Per-DOF report sections
# ------------------------------------------------------------------

def _compute_dof_amplitude_rows(
    dof: DOF,
    h_indices: List[int],
    solver_results: Dict[str, Any],
    solver_names: List[str],
) -> List[Dict[str, Any]]:
    """Compute amplitude summary rows for a DOF using given headings."""
    dof_name = dof.name.lower()
    rows: List[Dict[str, Any]] = []
    for solver in solver_names:
        comp: RAOComponent = getattr(
            solver_results[solver].raos, dof_name,
        )
        periods = comp.frequencies.periods
        for hi in h_indices:
            mag = comp.magnitude[:, hi]
            peak_idx = int(np.argmax(mag))
            rows.append({
                "heading": f"{comp.headings.values[hi]:.0f}",
                "solver": solver,
                "peak_amp": f"{mag[peak_idx]:.4g}",
                "peak_period": f"{periods[peak_idx]:.2f}",
                "long_period_amp": f"{mag[0]:.4g}",
            })
    return rows


def _compute_dof_phase_rows(
    dof: DOF,
    h_indices: List[int],
    solver_results: Dict[str, Any],
    solver_names: List[str],
) -> List[Dict[str, Any]]:
    """Compute phase summary rows for a DOF using given headings."""
    dof_name = dof.name.lower()
    rows: List[Dict[str, Any]] = []
    for solver in solver_names:
        comp: RAOComponent = getattr(
            solver_results[solver].raos, dof_name,
        )
        for hi in h_indices:
            mag = comp.magnitude[:, hi]
            phase = comp.phase[:, hi]
            peak_idx = int(np.argmax(mag))
            rows.append({
                "heading": f"{comp.headings.values[hi]:.0f}",
                "solver": solver,
                "phase_at_peak": f"{float(phase[peak_idx]):.1f}",
                "long_period_phase": f"{float(phase[0]):.1f}",
            })
    return rows


def _build_solver_column_table(
    rows: List[Dict[str, Any]],
    mode: str,
    solver_names: List[str],
) -> str:
    """Build a comparison table with solver names as columns."""
    if not rows:
        return "<p><em>No data</em></p>"

    by_heading: Dict[str, Dict[str, Dict[str, Any]]] = {}
    for r in rows:
        h = r["heading"]
        solver = r.get("solver", "")
        if h not in by_heading:
            by_heading[h] = {}
        by_heading[h][solver] = r

    parts: List[str] = ['<table class="solver-table">']

    if mode == "amplitude":
        parts.append("<tr><th rowspan='2'>Hdg</th>")
        for solver in solver_names:
            parts.append(
                f"<th colspan='3'>"
                f"{html_mod.escape(solver)}</th>"
            )
        parts.append("</tr><tr>")
        for _ in solver_names:
            parts.append("<th>Peak</th><th>T(s)</th><th>LP</th>")
        parts.append("</tr>")

        for h, solvers in by_heading.items():
            parts.append(f"<tr><td>{h}&deg;</td>")
            for solver in solver_names:
                r = solvers.get(solver, {})
                parts.append(
                    f"<td>{r.get('peak_amp', '-')}</td>"
                    f"<td>{r.get('peak_period', '-')}</td>"
                    f"<td>{r.get('long_period_amp', '-')}</td>"
                )
            parts.append("</tr>")
    else:
        parts.append("<tr><th rowspan='2'>Hdg</th>")
        for solver in solver_names:
            parts.append(
                f"<th colspan='2'>"
                f"{html_mod.escape(solver)}</th>"
            )
        parts.append("</tr><tr>")
        for _ in solver_names:
            parts.append("<th>@Peak</th><th>LP</th>")
        parts.append("</tr>")

        for h, solvers in by_heading.items():
            parts.append(f"<tr><td>{h}&deg;</td>")
            for solver in solver_names:
                r = solvers.get(solver, {})
                parts.append(
                    f"<td>{r.get('phase_at_peak', '-')}</td>"
                    f"<td>{r.get('long_period_phase', '-')}</td>"
                )
            parts.append("</tr>")

    parts.append("</table>")
    return "\n".join(parts)


def _add_phase_annotations(
    fig: go.Figure,
    dof: DOF,
    max_phase_diff: float,
    mag_at_max_pd: float,
    peak_mag: float,
    max_pd_freq: float,
    unit: str,
    x_axis: str,
    solver_results: Dict[str, Any],
    solver_names: List[str],
    phase_diff_heading_idx: int = 0,
    visible_heading_indices: Optional[List[int]] = None,
) -> None:
    """Add annotations to the phase subplot based on plotted data only."""
    if max_phase_diff < 20:
        return

    if visible_heading_indices is not None:
        if phase_diff_heading_idx not in visible_heading_indices:
            return

    if x_axis == "period" and max_pd_freq > 0:
        x_val = rad_per_s_to_period_s(max_pd_freq)
    else:
        x_val = max_pd_freq

    dof_name = dof.name.lower()
    first_comp: RAOComponent = getattr(
        solver_results[solver_names[0]].raos, dof_name,
    )
    freq_idx = int(np.argmin(
        np.abs(first_comp.frequencies.values - max_pd_freq),
    ))
    y_val = float(
        first_comp.phase[freq_idx, phase_diff_heading_idx],
    )
    x_label = f"{x_val:.1f}s" if x_axis == "period" else (
        f"{x_val:.2f} rad/s"
    )

    negligible = _is_phase_at_negligible_amplitude(
        mag_at_max_pd, peak_mag,
    )

    if negligible:
        text = (
            f"<b>\u0394\u03c6 = {max_phase_diff:.0f}\u00b0</b> "
            f"at ({x_label}, {y_val:.0f}\u00b0)<br>"
            f"Amplitude here: {mag_at_max_pd:.2e} {unit}<br>"
            f"<i>Ignore \u2014 magnitude is insignificant</i>"
        )
        bg_color = "rgba(241, 196, 15, 0.4)"
        border_color = "#f39c12"
    else:
        text = (
            f"<b>\u0394\u03c6 = {max_phase_diff:.0f}\u00b0</b> "
            f"at ({x_label}, {y_val:.0f}\u00b0)<br>"
            f"Amplitude: {mag_at_max_pd:.4g} {unit}<br>"
            f"<i>Review phase convention</i>"
        )
        bg_color = "rgba(231, 76, 60, 0.4)"
        border_color = "#e74c3c"

    fig.add_annotation(
        x=x_val,
        y=y_val,
        text=text,
        showarrow=True,
        arrowhead=2,
        arrowsize=1,
        arrowwidth=2,
        arrowcolor=border_color,
        ax=40,
        ay=-60,
        bgcolor=bg_color,
        bordercolor=border_color,
        borderwidth=1,
        borderpad=6,
        font=dict(size=11, color="#2c3e50"),
        align="left",
        row=2,
        col=1,
    )


def build_dof_report_sections(
    report: BenchmarkReport,
    solver_results: Dict[str, Any],
    solver_names: List[str],
    x_axis: str,
    heading_x_axis: bool,
    headings: Optional[List[float]] = None,
) -> str:
    """Build per-DOF two-column HTML sections (text left, plot right)."""
    pair_data = {}
    if report.pairwise_results:
        first_pair = next(iter(report.pairwise_results.values()))
        pair_data = first_pair.rao_comparisons

    parts: List[str] = []
    for dof in DOF_ORDER:
        dof_name = dof.name.lower()
        dof_upper = dof.name.upper()
        dof_cap = dof.name.capitalize()

        sig_indices = get_significant_heading_indices(
            dof, solver_results, solver_names, headings,
        )
        first_comp: RAOComponent = getattr(
            solver_results[solver_names[0]].raos, dof_name,
        )
        sig_heading_vals = {
            f"{first_comp.headings.values[hi]:.0f}"
            for hi in sig_indices
        }
        all_indices = get_heading_indices(first_comp, headings)
        skipped_headings = [
            f"{first_comp.headings.values[hi]:.0f}"
            for hi in all_indices if hi not in sig_indices
        ]

        cm = report.consensus_by_dof.get(dof_upper)
        consensus_level = cm.consensus_level if cm else "UNKNOWN"
        mean_corr = cm.mean_pairwise_correlation if cm else 0.0
        consensus_color = {
            "FULL": "#27ae60",
            "MAJORITY": "#f39c12",
            "NO_CONSENSUS": "#e74c3c",
        }.get(consensus_level, "#999")

        rao_comp = pair_data.get(dof_name)
        mag_corr = rao_comp.magnitude_stats.correlation if rao_comp else 0
        mag_rms = rao_comp.magnitude_stats.rms_error if rao_comp else 0
        phase_corr = rao_comp.phase_stats.correlation if rao_comp else 0
        max_mag_diff = rao_comp.max_magnitude_diff if rao_comp else 0
        max_phase_diff = rao_comp.max_phase_diff if rao_comp else 0
        mag_at_max_pd = (
            rao_comp.magnitude_at_max_phase_diff if rao_comp else 0
        )
        peak_mag = rao_comp.peak_magnitude if rao_comp else 0
        max_pd_freq = rao_comp.max_phase_diff_x if rao_comp else 0
        max_pd_hi = (
            rao_comp.max_phase_diff_heading_idx if rao_comp else 0
        )

        fig = make_subplots(
            rows=2, cols=1, shared_xaxes=True,
            subplot_titles=[
                f"Amplitude ({_AMPLITUDE_UNITS[dof]})",
                "Phase (deg)",
            ],
            vertical_spacing=0.15,
        )
        add_solver_traces(
            fig, dof, headings, 1, 1, "amplitude",
            show_legend=True, heading_indices=sig_indices,
            solver_results=solver_results,
            solver_names=solver_names,
            x_axis=x_axis,
            heading_x_axis=heading_x_axis,
        )
        add_solver_traces(
            fig, dof, headings, 2, 1, "phase",
            show_legend=False, heading_indices=sig_indices,
            solver_results=solver_results,
            solver_names=solver_names,
            x_axis=x_axis,
            heading_x_axis=heading_x_axis,
        )
        fig.update_xaxes(
            title_text=x_axis_label(x_axis, heading_x_axis), row=2, col=1,
        )
        fig.update_yaxes(
            title_text=_AMPLITUDE_UNITS[dof], row=1, col=1,
        )
        fig.update_yaxes(title_text="deg", row=2, col=1)

        _add_phase_annotations(
            fig, dof, max_phase_diff, mag_at_max_pd,
            peak_mag, max_pd_freq, _AMPLITUDE_UNITS[dof],
            x_axis=x_axis,
            solver_results=solver_results,
            solver_names=solver_names,
            phase_diff_heading_idx=max_pd_hi,
            visible_heading_indices=sig_indices,
        )

        fig.update_layout(
            template="plotly_white",
            legend=dict(
                orientation="v",
                yanchor="top",
                y=1.0,
                xanchor="left",
                x=1.02,
                font=dict(size=10),
                tracegroupgap=2,
            ),
            margin=dict(l=50, r=140, t=30, b=30),
            height=400,
        )
        plot_div = fig.to_html(
            full_html=False,
            include_plotlyjs=False,
            div_id=f"plot_{dof_name}",
        )

        amp_rows = _compute_dof_amplitude_rows(
            dof, sig_indices, solver_results, solver_names,
        )
        phase_rows = _compute_dof_phase_rows(
            dof, sig_indices, solver_results, solver_names,
        )
        amp_table = _build_solver_column_table(
            amp_rows, "amplitude", solver_names,
        )
        phase_table = _build_solver_column_table(
            phase_rows, "phase", solver_names,
        )

        skipped_note = ""
        if skipped_headings:
            skipped_note = (
                f'<p class="skipped-note">Headings with negligible '
                f'response omitted: {", ".join(skipped_headings)}&deg;</p>'
            )

        pd_heading_visible = max_pd_hi in sig_indices
        obs = generate_dof_observations(
            dof_cap, consensus_level, mag_corr, phase_corr,
            max_mag_diff, max_phase_diff, _AMPLITUDE_UNITS[dof],
            magnitude_at_max_phase_diff=mag_at_max_pd,
            peak_magnitude=peak_mag,
            phase_diff_at_visible_heading=pd_heading_visible,
        )

        parts.append(f"""
<div class="dof-section" id="dof-{dof_name}">
  <h3 class="dof-title">{dof_cap}</h3>
  <div class="dof-grid">
    <div class="dof-text">
      <div class="consensus-badge" style="background:{consensus_color};">
        {consensus_level}
      </div>
      <table class="stats-table">
        <tr><th>Metric</th><th>Value</th></tr>
        <tr><td>Magnitude correlation</td><td>{mag_corr:.4f}</td></tr>
        <tr><td>Phase correlation</td><td>{phase_corr:.4f}</td></tr>
        <tr><td>Magnitude RMS error</td><td>{mag_rms:.4f}</td></tr>
        <tr><td>Max amplitude diff</td>
            <td>{max_mag_diff:.4f} {_AMPLITUDE_UNITS[dof]}</td></tr>
        <tr><td>Max phase diff</td><td>{max_phase_diff:.1f} deg</td></tr>
      </table>
      <h4>Amplitude Comparison</h4>
      {amp_table}
      <h4>Phase Comparison</h4>
      {phase_table}
      <div class="observations">{obs}</div>
      {skipped_note}
    </div>
    <div class="dof-plot">{plot_div}</div>
  </div>
</div>""")

    return "\n".join(parts)
