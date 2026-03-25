"""Benchmark RAO trace helper functions.

ABOUTME: Low-level helpers for building Plotly traces in RAO comparison plots.
Split from benchmark_rao_plots.py (WRK-593 God Object split).

Leaf module — imports from benchmark_helpers and output_schemas only.
No circular dependencies.
"""
from __future__ import annotations

from pathlib import Path
from typing import Any, Dict, List, Optional

import numpy as np
import plotly.graph_objects as go

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


def save_figure(fig: go.Figure, filename: str, output_dir: Path) -> Path:
    """Write figure to HTML with CDN Plotly.js and return the path."""
    path = output_dir / f"{filename}.html"
    fig.write_html(str(path), include_plotlyjs="cdn")
    return path
