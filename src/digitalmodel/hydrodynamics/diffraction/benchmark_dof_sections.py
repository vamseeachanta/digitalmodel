"""Benchmark per-DOF HTML report section builder.

ABOUTME: _add_phase_annotations and build_dof_report_sections.
Split from benchmark_correlation.py (WRK-593 God Object split).

Depends on: benchmark_helpers, benchmark_rao_plots (re-exports from helpers),
benchmark_dof_tables, diffraction_units, multi_solver_comparator, output_schemas.
"""
from __future__ import annotations

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
    x_axis_label,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_dof_tables import (
    _build_solver_column_table,
    _compute_dof_amplitude_rows,
    _compute_dof_phase_rows,
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
