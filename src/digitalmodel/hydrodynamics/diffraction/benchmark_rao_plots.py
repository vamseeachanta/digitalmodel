"""Benchmark RAO overlay and difference plotting functions.

ABOUTME: 5 public plot functions (amplitude overlay, phase overlay, combined,
difference, per-DOF). Helpers/summary extracted to benchmark_rao_helpers and
benchmark_rao_summary as part of WRK-593 God Object split.

Re-exports all helper/summary names for backward compatibility — callers
that import from benchmark_rao_plots continue to work unchanged.
"""
from __future__ import annotations

from pathlib import Path
from typing import Any, Dict, List, Optional

import plotly.graph_objects as go
from plotly.subplots import make_subplots

# --- backward-compat re-exports from extracted sub-modules ---
from digitalmodel.hydrodynamics.diffraction.benchmark_rao_helpers import (  # noqa: F401
    add_solver_traces,
    apply_layout,
    get_heading_indices,
    get_significant_heading_indices,
    get_solver_style,
    get_x_values,
    save_figure,
    x_axis_label,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_rao_summary import (  # noqa: F401
    build_summary_table,
    compute_amplitude_summary,
    compute_phase_summary,
    render_html_with_table,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_helpers import (
    DOF_ORDER,
    _AMPLITUDE_UNITS,
)


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
    from digitalmodel.hydrodynamics.diffraction.output_schemas import RAOComponent
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
