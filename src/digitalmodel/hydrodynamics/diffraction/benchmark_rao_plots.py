"""Benchmark RAO overlay and difference plotting functions.

ABOUTME: Module-level functions for generating multi-solver RAO overlay plots
(amplitude, phase, combined, difference, per-DOF). Extracted from
BenchmarkPlotter as part of WRK-592 God Object split.

Imports from benchmark_helpers (leaf) only. No circular dependencies.
"""
from __future__ import annotations

import html as html_mod
from pathlib import Path
from typing import Any, Dict, List, Literal, Optional

import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots

from digitalmodel.hydrodynamics.diffraction.benchmark_helpers import (
    DOF_ORDER,
    _AMPLITUDE_UNITS,
    _SOLVER_STYLES,
)
from digitalmodel.hydrodynamics.diffraction.diffraction_units import (
    rad_per_s_to_period_s,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DOF,
    RAOComponent,
)


# ------------------------------------------------------------------
# Trace helpers
# ------------------------------------------------------------------

def get_solver_style(solver_idx: int) -> dict:
    """Return dash and colour style for a solver index."""
    return _SOLVER_STYLES.get(
        solver_idx % len(_SOLVER_STYLES),
        _SOLVER_STYLES[0],
    )


def get_x_values(
    component: RAOComponent,
    x_axis: str,
) -> np.ndarray:
    """Return period or frequency array based on *x_axis*."""
    if x_axis == "frequency":
        return component.frequencies.values
    return component.frequencies.periods


def x_axis_label(x_axis: str, heading_x_axis: bool) -> str:
    """Human-readable label for the x-axis."""
    if heading_x_axis:
        return "Heading (deg)"
    if x_axis == "frequency":
        return "Frequency (rad/s)"
    return "Period (s)"


def get_heading_indices(
    component: RAOComponent,
    headings: Optional[List[float]],
) -> List[int]:
    """Resolve heading filter to column indices.

    Falls back to all headings when *headings* is ``None`` or no
    matches are found within a 1-degree tolerance.
    """
    if headings is None:
        return list(range(component.headings.count))
    indices: List[int] = []
    for h in headings:
        diffs = np.abs(component.headings.values - h)
        idx = int(np.argmin(diffs))
        if diffs[idx] < 1.0:
            indices.append(idx)
    return indices if indices else list(range(component.headings.count))


def get_significant_heading_indices(
    dof: DOF,
    solver_results: Dict[str, Any],
    solver_names: List[str],
    headings: Optional[List[float]],
    threshold: float = 0.01,
) -> List[int]:
    """Return heading indices where the DOF response is significant.

    A heading is significant if *any* solver has a peak amplitude
    exceeding ``threshold`` times the overall peak for that DOF across
    all solvers and headings.
    """
    dof_name = dof.name.lower()
    first_comp: RAOComponent = getattr(
        solver_results[solver_names[0]].raos, dof_name,
    )
    candidate_indices = get_heading_indices(first_comp, headings)

    overall_peak = 0.0
    for solver in solver_names:
        comp: RAOComponent = getattr(
            solver_results[solver].raos, dof_name,
        )
        for hi in candidate_indices:
            peak = float(np.max(np.abs(comp.magnitude[:, hi])))
            if peak > overall_peak:
                overall_peak = peak

    if overall_peak < 1e-30:
        return candidate_indices

    cutoff = overall_peak * threshold
    significant: List[int] = []
    for hi in candidate_indices:
        for solver in solver_names:
            comp = getattr(
                solver_results[solver].raos, dof_name,
            )
            if float(np.max(np.abs(comp.magnitude[:, hi]))) > cutoff:
                significant.append(hi)
                break

    return significant if significant else candidate_indices


# ------------------------------------------------------------------
# Core trace-adding function
# ------------------------------------------------------------------

def add_solver_traces(
    fig: go.Figure,
    dof: DOF,
    headings: Optional[List[float]],
    row: int,
    col: int,
    value_type: str,
    show_legend: bool,
    solver_results: Dict[str, Any],
    solver_names: List[str],
    x_axis: str,
    heading_x_axis: bool,
    heading_indices: Optional[List[int]] = None,
) -> None:
    """Add traces for every solver and heading to the subplot."""
    dof_name = dof.name.lower()

    first_comp: RAOComponent = getattr(
        solver_results[solver_names[0]].raos, dof_name,
    )
    h_indices = (
        heading_indices
        if heading_indices is not None
        else get_heading_indices(first_comp, headings)
    )

    if heading_x_axis:
        for si, solver in enumerate(solver_names):
            style = get_solver_style(si)
            comp: RAOComponent = getattr(
                solver_results[solver].raos, dof_name,
            )
            x_vals = comp.headings.values
            nfreqs = len(comp.frequencies.values)
            for fi in range(nfreqs):
                period_s = comp.frequencies.periods[fi]
                trace_name = f"{solver} {period_s:.2f}s"
                y_vals = (
                    comp.magnitude[fi, :]
                    if value_type == "amplitude"
                    else comp.phase[fi, :]
                )
                fig.add_trace(
                    go.Scatter(
                        x=x_vals,
                        y=y_vals,
                        mode="lines+markers",
                        name=trace_name,
                        legendgroup=f"{solver}_{fi}",
                        showlegend=show_legend,
                        line=dict(
                            dash=style["dash"],
                            color=style["color_base"],
                        ),
                    ),
                    row=row,
                    col=col,
                )
    else:
        for hi in h_indices:
            heading_label = f"{first_comp.headings.values[hi]:.0f}"
            for si, solver in enumerate(solver_names):
                style = get_solver_style(si)
                comp = getattr(
                    solver_results[solver].raos, dof_name,
                )
                x_vals = get_x_values(comp, x_axis)
                y_vals = (
                    comp.magnitude[:, hi]
                    if value_type == "amplitude"
                    else comp.phase[:, hi]
                )
                trace_name = f"{solver} H{heading_label}"
                fig.add_trace(
                    go.Scatter(
                        x=x_vals,
                        y=y_vals,
                        mode="lines",
                        name=trace_name,
                        legendgroup=f"{solver}_{heading_label}",
                        showlegend=show_legend,
                        line=dict(
                            dash=style["dash"],
                            color=style["color_base"],
                        ),
                    ),
                    row=row,
                    col=col,
                )


def apply_layout(fig: go.Figure, title: str) -> None:
    """Apply common Plotly layout: white template, vertical legend."""
    fig.update_layout(
        title_text=title,
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
        margin=dict(l=60, r=160, t=80, b=40),
    )


# ------------------------------------------------------------------
# Summary computation helpers
# ------------------------------------------------------------------

def compute_amplitude_summary(
    solver_results: Dict[str, Any],
    solver_names: List[str],
    headings: Optional[List[float]],
) -> List[Dict[str, Any]]:
    """Per-DOF amplitude summary rows for the comparison table."""
    sections: List[Dict[str, Any]] = []
    for dof in DOF_ORDER:
        dof_name = dof.name.lower()
        rows: List[Dict[str, Any]] = []
        solver_peaks: Dict[str, Dict[float, float]] = {}
        for solver in solver_names:
            comp: RAOComponent = getattr(
                solver_results[solver].raos, dof_name,
            )
            h_indices = get_heading_indices(comp, headings)
            periods = comp.frequencies.periods
            solver_peaks[solver] = {}
            for hi in h_indices:
                mag = comp.magnitude[:, hi]
                peak_idx = int(np.argmax(mag))
                heading_val = float(comp.headings.values[hi])
                solver_peaks[solver][heading_val] = float(mag[peak_idx])
                rows.append({
                    "heading": f"{heading_val:.0f}",
                    "solver": solver,
                    "peak_amp": f"{mag[peak_idx]:.4g}",
                    "peak_period": f"{periods[peak_idx]:.2f}",
                    "long_period_amp": f"{mag[0]:.4g}",
                })
        ref_solver = solver_names[0]
        for row in rows:
            h_val = float(row["heading"])
            ref_peak = solver_peaks[ref_solver].get(h_val, 0.0)
            cur_peak = solver_peaks[row["solver"]].get(h_val, 0.0)
            if ref_peak > 1e-12:
                diff = abs(cur_peak - ref_peak) / ref_peak * 100
                row["diff_pct"] = f"{diff:.1f}"
            else:
                row["diff_pct"] = "-"
        sections.append({
            "dof": dof.name.capitalize(),
            "unit": _AMPLITUDE_UNITS[dof],
            "rows": rows,
        })
    return sections


def compute_phase_summary(
    solver_results: Dict[str, Any],
    solver_names: List[str],
    headings: Optional[List[float]],
) -> List[Dict[str, Any]]:
    """Per-DOF phase summary rows for the comparison table."""
    sections: List[Dict[str, Any]] = []
    for dof in DOF_ORDER:
        dof_name = dof.name.lower()
        rows: List[Dict[str, Any]] = []
        solver_phase_at_peak: Dict[str, Dict[float, float]] = {}
        for solver in solver_names:
            comp: RAOComponent = getattr(
                solver_results[solver].raos, dof_name,
            )
            h_indices = get_heading_indices(comp, headings)
            solver_phase_at_peak[solver] = {}
            for hi in h_indices:
                mag = comp.magnitude[:, hi]
                phase = comp.phase[:, hi]
                peak_idx = int(np.argmax(mag))
                heading_val = float(comp.headings.values[hi])
                phase_peak = float(phase[peak_idx])
                solver_phase_at_peak[solver][heading_val] = phase_peak
                rows.append({
                    "heading": f"{heading_val:.0f}",
                    "solver": solver,
                    "phase_at_peak": f"{phase_peak:.1f}",
                    "long_period_phase": f"{float(phase[0]):.1f}",
                })
        ref_solver = solver_names[0]
        for row in rows:
            h_val = float(row["heading"])
            ref_ph = solver_phase_at_peak[ref_solver].get(h_val, 0.0)
            cur_ph = solver_phase_at_peak[row["solver"]].get(h_val, 0.0)
            row["phase_diff"] = f"{abs(cur_ph - ref_ph):.1f}"
        sections.append({
            "dof": dof.name.capitalize(),
            "rows": rows,
        })
    return sections


# ------------------------------------------------------------------
# HTML rendering with side-by-side table
# ------------------------------------------------------------------

def build_summary_table(
    sections: List[Dict[str, Any]],
    mode: Literal["Amplitude", "Phase"],
) -> str:
    """Render summary *sections* as an HTML string of tables."""
    parts: List[str] = []
    for sec in sections:
        dof_label = html_mod.escape(sec["dof"])
        unit = html_mod.escape(sec.get("unit", ""))
        heading_text = (
            f"{dof_label} ({unit})" if unit else dof_label
        )
        parts.append(f"<h3>{heading_text}</h3>")
        parts.append("<table>")
        if mode == "Amplitude":
            parts.append(
                "<tr><th>Hdg</th><th>Solver</th>"
                "<th>Peak&nbsp;Amp</th><th>Peak&nbsp;T(s)</th>"
                "<th>LP&nbsp;Amp</th><th>Diff(%)</th></tr>"
            )
            for r in sec["rows"]:
                parts.append(
                    f"<tr><td>{r['heading']}</td>"
                    f"<td>{html_mod.escape(r['solver'])}</td>"
                    f"<td>{r['peak_amp']}</td>"
                    f"<td>{r['peak_period']}</td>"
                    f"<td>{r['long_period_amp']}</td>"
                    f"<td>{r['diff_pct']}</td></tr>"
                )
        else:
            parts.append(
                "<tr><th>Hdg</th><th>Solver</th>"
                "<th>Phase@Peak</th><th>LP&nbsp;Phase</th>"
                "<th>Diff(deg)</th></tr>"
            )
            for r in sec["rows"]:
                parts.append(
                    f"<tr><td>{r['heading']}</td>"
                    f"<td>{html_mod.escape(r['solver'])}</td>"
                    f"<td>{r['phase_at_peak']}</td>"
                    f"<td>{r['long_period_phase']}</td>"
                    f"<td>{r['phase_diff']}</td></tr>"
                )
        parts.append("</table>")
    return "\n".join(parts)


def render_html_with_table(
    fig: go.Figure,
    summary: List[Dict[str, Any]],
    filename: str,
    mode: Literal["Amplitude", "Phase"],
    output_dir: Path,
) -> Path:
    """Write an HTML page with Plotly plot (left) and table (right)."""
    plot_html = fig.to_html(
        full_html=False, include_plotlyjs="cdn",
    )
    table_html = build_summary_table(summary, mode)
    title = html_mod.escape(
        fig.layout.title.text if fig.layout.title.text else filename,
    )
    page = f"""\
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<title>{title}</title>
<style>
  body {{ margin:0; font-family: Arial, Helvetica, sans-serif; }}
  .grid {{ display:grid; grid-template-columns:70% 30%; height:100vh; }}
  .plot {{ overflow:auto; }}
  .table-panel {{
    overflow:auto; padding:8px; background:#fafafa;
    border-left:1px solid #ddd; font-size:12px;
  }}
  .table-panel h3 {{ margin:12px 0 4px; font-size:13px; }}
  .table-panel table {{
    border-collapse:collapse; width:100%; margin-bottom:8px;
  }}
  .table-panel th, .table-panel td {{
    border:1px solid #ccc; padding:3px 5px; text-align:right;
  }}
  .table-panel th {{ background:#e8e8e8; font-weight:600; }}
  .table-panel td:first-child, .table-panel td:nth-child(2) {{
    text-align:left;
  }}
</style>
</head>
<body>
<div class="grid">
  <div class="plot">{plot_html}</div>
  <div class="table-panel">{table_html}</div>
</div>
</body>
</html>"""
    path = output_dir / f"{filename}.html"
    path.write_text(page, encoding="utf-8")
    return path


def save_figure(fig: go.Figure, filename: str, output_dir: Path) -> Path:
    """Write figure to HTML with CDN Plotly.js and return the path."""
    path = output_dir / f"{filename}.html"
    fig.write_html(str(path), include_plotlyjs="cdn")
    return path


# ------------------------------------------------------------------
# Public plot functions
# ------------------------------------------------------------------

def plot_amplitude_overlay(
    solver_results: Dict[str, Any],
    solver_names: List[str],
    output_dir: Path,
    x_axis: str,
    heading_x_axis: bool,
    headings: Optional[List[float]] = None,
) -> Path:
    """6-row subplot of RAO amplitude with one trace per solver/heading."""
    subplot_titles = [
        f"{d.name.capitalize()} ({_AMPLITUDE_UNITS[d]})"
        for d in DOF_ORDER
    ]
    fig = make_subplots(
        rows=6, cols=1, shared_xaxes=True,
        subplot_titles=subplot_titles, vertical_spacing=0.03,
    )
    for row, dof in enumerate(DOF_ORDER, start=1):
        add_solver_traces(
            fig, dof, headings, row, 1, "amplitude",
            show_legend=(row == 1),
            solver_results=solver_results,
            solver_names=solver_names,
            x_axis=x_axis,
            heading_x_axis=heading_x_axis,
        )
    fig.update_xaxes(
        title_text=x_axis_label(x_axis, heading_x_axis), row=6, col=1,
    )
    apply_layout(fig, "Benchmark RAO Amplitude Overlay")
    fig.update_layout(height=1200)
    summary = compute_amplitude_summary(solver_results, solver_names, headings)
    return render_html_with_table(
        fig, summary, "benchmark_amplitude", "Amplitude", output_dir,
    )


def plot_phase_overlay(
    solver_results: Dict[str, Any],
    solver_names: List[str],
    output_dir: Path,
    x_axis: str,
    heading_x_axis: bool,
    headings: Optional[List[float]] = None,
) -> Path:
    """6-row subplot of RAO phase with one trace per solver/heading."""
    subplot_titles = [
        f"{d.name.capitalize()} Phase (deg)" for d in DOF_ORDER
    ]
    fig = make_subplots(
        rows=6, cols=1, shared_xaxes=True,
        subplot_titles=subplot_titles, vertical_spacing=0.03,
    )
    for row, dof in enumerate(DOF_ORDER, start=1):
        add_solver_traces(
            fig, dof, headings, row, 1, "phase",
            show_legend=(row == 1),
            solver_results=solver_results,
            solver_names=solver_names,
            x_axis=x_axis,
            heading_x_axis=heading_x_axis,
        )
    fig.update_xaxes(
        title_text=x_axis_label(x_axis, heading_x_axis), row=6, col=1,
    )
    apply_layout(fig, "Benchmark RAO Phase Overlay")
    fig.update_layout(height=1200)
    summary = compute_phase_summary(solver_results, solver_names, headings)
    return render_html_with_table(
        fig, summary, "benchmark_phase", "Phase", output_dir,
    )


def plot_combined_overlay(
    solver_results: Dict[str, Any],
    solver_names: List[str],
    output_dir: Path,
    x_axis: str,
    heading_x_axis: bool,
    headings: Optional[List[float]] = None,
) -> Path:
    """6 rows x 2 cols: amplitude left, phase right."""
    subplot_titles: List[str] = []
    for dof in DOF_ORDER:
        subplot_titles.append(f"{dof.name.capitalize()} Amplitude")
        subplot_titles.append(f"{dof.name.capitalize()} Phase")

    fig = make_subplots(
        rows=6, cols=2, shared_xaxes=True,
        subplot_titles=subplot_titles,
        vertical_spacing=0.03, horizontal_spacing=0.06,
    )
    for row, dof in enumerate(DOF_ORDER, start=1):
        add_solver_traces(
            fig, dof, headings, row, 1, "amplitude",
            show_legend=(row == 1),
            solver_results=solver_results,
            solver_names=solver_names,
            x_axis=x_axis,
            heading_x_axis=heading_x_axis,
        )
        add_solver_traces(
            fig, dof, headings, row, 2, "phase",
            show_legend=False,
            solver_results=solver_results,
            solver_names=solver_names,
            x_axis=x_axis,
            heading_x_axis=heading_x_axis,
        )
    label = x_axis_label(x_axis, heading_x_axis)
    fig.update_xaxes(title_text=label, row=6, col=1)
    fig.update_xaxes(title_text=label, row=6, col=2)
    apply_layout(fig, "Benchmark RAO Combined (Amplitude | Phase)")
    fig.update_layout(height=1400)
    return save_figure(fig, "benchmark_combined", output_dir)


def plot_difference(
    reference_solver: str,
    solver_results: Dict[str, Any],
    solver_names: List[str],
    output_dir: Path,
    x_axis: str,
    heading_x_axis: bool,
    headings: Optional[List[float]] = None,
) -> Path:
    """Plot magnitude difference relative to a reference solver."""
    if reference_solver not in solver_results:
        raise ValueError(
            f"Reference solver '{reference_solver}' not found. "
            f"Available: {solver_names}"
        )

    other_solvers = [s for s in solver_names if s != reference_solver]
    subplot_titles = [
        f"{d.name.capitalize()} Diff ({_AMPLITUDE_UNITS[d]})"
        for d in DOF_ORDER
    ]
    fig = make_subplots(
        rows=6, cols=1, shared_xaxes=True,
        subplot_titles=subplot_titles, vertical_spacing=0.03,
    )

    ref_results = solver_results[reference_solver]

    for row, dof in enumerate(DOF_ORDER, start=1):
        dof_name = dof.name.lower()
        ref_comp: RAOComponent = getattr(ref_results.raos, dof_name)
        h_indices = get_heading_indices(ref_comp, headings)
        x_vals = get_x_values(ref_comp, x_axis)

        for si, solver in enumerate(other_solvers):
            style = get_solver_style(si)
            comp: RAOComponent = getattr(
                solver_results[solver].raos, dof_name,
            )
            for hi in h_indices:
                heading_label = f"{ref_comp.headings.values[hi]:.0f}"
                diff = comp.magnitude[:, hi] - ref_comp.magnitude[:, hi]
                trace_name = f"{solver} H{heading_label}"
                fig.add_trace(
                    go.Scatter(
                        x=x_vals,
                        y=diff,
                        mode="lines",
                        name=trace_name,
                        legendgroup=solver,
                        showlegend=(row == 1),
                        line=dict(
                            dash=style["dash"],
                            color=style["color_base"],
                        ),
                    ),
                    row=row,
                    col=1,
                )

    fig.update_xaxes(
        title_text=x_axis_label(x_axis, heading_x_axis), row=6, col=1,
    )
    apply_layout(
        fig, f"Benchmark Difference (ref: {reference_solver})",
    )
    fig.update_layout(height=1200)
    return save_figure(fig, "benchmark_difference", output_dir)


def plot_per_dof(
    solver_results: Dict[str, Any],
    solver_names: List[str],
    output_dir: Path,
    x_axis: str,
    heading_x_axis: bool,
    headings: Optional[List[float]] = None,
) -> Dict[str, Path]:
    """Generate individual per-DOF plots (amplitude + phase, 2 rows)."""
    paths: Dict[str, Path] = {}
    for dof in DOF_ORDER:
        dof_name = dof.name.lower()
        fig = make_subplots(
            rows=2, cols=1, shared_xaxes=True,
            subplot_titles=[
                f"Amplitude ({_AMPLITUDE_UNITS[dof]})",
                "Phase (deg)",
            ],
            vertical_spacing=0.12,
        )
        add_solver_traces(
            fig, dof, headings, 1, 1, "amplitude", show_legend=True,
            solver_results=solver_results,
            solver_names=solver_names,
            x_axis=x_axis,
            heading_x_axis=heading_x_axis,
        )
        add_solver_traces(
            fig, dof, headings, 2, 1, "phase", show_legend=False,
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
        fig.update_layout(
            title_text=f"{dof.name.capitalize()} RAO",
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
            margin=dict(l=50, r=140, t=50, b=30),
            height=400,
        )
        path = save_figure(fig, f"benchmark_{dof_name}", output_dir)
        paths[dof_name] = path
    return paths
