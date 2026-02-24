"""Interactive Plotly HTML plotter for RAO results (WRK-030).

Generates 6-row subplot figures for RAO amplitude, phase, and combined views
across all six degrees of freedom with per-heading traces.
"""
from __future__ import annotations

from pathlib import Path
from typing import Literal

import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
    DOF,
    RAOComponent,
)

DOF_ORDER = [DOF.SURGE, DOF.SWAY, DOF.HEAVE, DOF.ROLL, DOF.PITCH, DOF.YAW]

_TRANSLATION_DOFS = {DOF.SURGE, DOF.SWAY, DOF.HEAVE}

_AMPLITUDE_UNITS = {
    DOF.SURGE: "m/m",
    DOF.SWAY: "m/m",
    DOF.HEAVE: "m/m",
    DOF.ROLL: "deg/m",
    DOF.PITCH: "deg/m",
    DOF.YAW: "deg/m",
}

# Qualitative colour palette (up to 24 headings)
_COLORS = [
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
    "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
    "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5",
    "#393b79", "#637939", "#8c6d31", "#843c39",
]


class RAOPlotter:
    """Generate interactive Plotly HTML plots from DiffractionResults."""

    def __init__(
        self,
        results: DiffractionResults,
        output_dir: Path,
        x_axis: Literal["period", "frequency"] = "period",
    ) -> None:
        self._results = results
        self._output_dir = Path(output_dir)
        self._output_dir.mkdir(parents=True, exist_ok=True)
        self._x_axis = x_axis

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def plot_all(self) -> list[Path]:
        return [
            self.plot_amplitude(),
            self.plot_phase(),
            self.plot_combined(),
        ]

    def plot_amplitude(self, headings: list[float] | None = None) -> Path:
        fig = self._create_amplitude_figure(headings)
        return self._save_figure(fig, "rao_amplitude")

    def plot_phase(self, headings: list[float] | None = None) -> Path:
        fig = self._create_phase_figure(headings)
        return self._save_figure(fig, "rao_phase")

    def plot_combined(self, headings: list[float] | None = None) -> Path:
        fig = self._create_combined_figure(headings)
        return self._save_figure(fig, "rao_combined")

    def plot_single_dof(
        self, dof: DOF, headings: list[float] | None = None
    ) -> Path:
        comp = self._results.raos.get_component(dof)
        h_indices = self._get_heading_indices(comp, headings)
        colors = self._get_heading_colors(len(h_indices))

        fig = make_subplots(
            rows=2, cols=1,
            shared_xaxes=True,
            subplot_titles=(
                f"{dof.name.capitalize()} Amplitude",
                f"{dof.name.capitalize()} Phase",
            ),
            vertical_spacing=0.10,
        )

        x_vals = self._get_x_values(comp)
        for ci, hi in enumerate(h_indices):
            heading_label = f"{comp.headings.values[hi]:.0f}°"
            fig.add_trace(
                go.Scatter(
                    x=x_vals,
                    y=comp.magnitude[:, hi],
                    mode="lines",
                    name=heading_label,
                    line=dict(color=colors[ci]),
                    legendgroup=heading_label,
                ),
                row=1, col=1,
            )
            fig.add_trace(
                go.Scatter(
                    x=x_vals,
                    y=comp.phase[:, hi],
                    mode="lines",
                    name=heading_label,
                    line=dict(color=colors[ci]),
                    legendgroup=heading_label,
                    showlegend=False,
                ),
                row=2, col=1,
            )

        x_label = self._x_axis_label()
        fig.update_xaxes(title_text=x_label, row=2, col=1)
        fig.update_yaxes(title_text=_AMPLITUDE_UNITS[dof], row=1, col=1)
        fig.update_yaxes(title_text="Phase (deg)", row=2, col=1)
        self._apply_layout(fig, f"RAO – {dof.name.capitalize()}")
        return self._save_figure(fig, f"rao_{dof.name.lower()}")

    # ------------------------------------------------------------------
    # Figure builders
    # ------------------------------------------------------------------

    def _create_amplitude_figure(self, headings: list[float] | None) -> go.Figure:
        subplot_titles = [f"{d.name.capitalize()} ({_AMPLITUDE_UNITS[d]})" for d in DOF_ORDER]
        fig = make_subplots(
            rows=6, cols=1,
            shared_xaxes=True,
            subplot_titles=subplot_titles,
            vertical_spacing=0.03,
        )
        for row, dof in enumerate(DOF_ORDER, start=1):
            comp = self._results.raos.get_component(dof)
            h_indices = self._get_heading_indices(comp, headings)
            self._add_dof_traces(
                fig, comp, h_indices, row, 1, "amplitude", show_legend=(row == 1),
            )
        fig.update_xaxes(title_text=self._x_axis_label(), row=6, col=1)
        self._apply_layout(fig, "RAO Amplitude")
        fig.update_layout(height=1200)
        return fig

    def _create_phase_figure(self, headings: list[float] | None) -> go.Figure:
        subplot_titles = [f"{d.name.capitalize()} Phase (deg)" for d in DOF_ORDER]
        fig = make_subplots(
            rows=6, cols=1,
            shared_xaxes=True,
            subplot_titles=subplot_titles,
            vertical_spacing=0.03,
        )
        for row, dof in enumerate(DOF_ORDER, start=1):
            comp = self._results.raos.get_component(dof)
            h_indices = self._get_heading_indices(comp, headings)
            self._add_dof_traces(
                fig, comp, h_indices, row, 1, "phase", show_legend=(row == 1),
            )
        fig.update_xaxes(title_text=self._x_axis_label(), row=6, col=1)
        self._apply_layout(fig, "RAO Phase")
        fig.update_layout(height=1200)
        return fig

    def _create_combined_figure(self, headings: list[float] | None) -> go.Figure:
        subplot_titles: list[str] = []
        for dof in DOF_ORDER:
            subplot_titles.append(f"{dof.name.capitalize()} Amplitude")
            subplot_titles.append(f"{dof.name.capitalize()} Phase")

        fig = make_subplots(
            rows=6, cols=2,
            shared_xaxes=True,
            subplot_titles=subplot_titles,
            vertical_spacing=0.03,
            horizontal_spacing=0.06,
        )
        for row, dof in enumerate(DOF_ORDER, start=1):
            comp = self._results.raos.get_component(dof)
            h_indices = self._get_heading_indices(comp, headings)
            self._add_dof_traces(
                fig, comp, h_indices, row, 1, "amplitude", show_legend=(row == 1),
            )
            self._add_dof_traces(
                fig, comp, h_indices, row, 2, "phase", show_legend=False,
            )
        fig.update_xaxes(title_text=self._x_axis_label(), row=6, col=1)
        fig.update_xaxes(title_text=self._x_axis_label(), row=6, col=2)
        self._apply_layout(fig, "RAO Combined (Amplitude | Phase)")
        fig.update_layout(height=1400)
        return fig

    # ------------------------------------------------------------------
    # Trace helpers
    # ------------------------------------------------------------------

    def _add_dof_traces(
        self,
        fig: go.Figure,
        component: RAOComponent,
        h_indices: list[int],
        row: int,
        col: int,
        value_type: str,
        show_legend: bool,
    ) -> None:
        colors = self._get_heading_colors(len(h_indices))
        x_vals = self._get_x_values(component)
        for ci, hi in enumerate(h_indices):
            heading_label = f"{component.headings.values[hi]:.0f}°"
            y_vals = (
                component.magnitude[:, hi]
                if value_type == "amplitude"
                else component.phase[:, hi]
            )
            fig.add_trace(
                go.Scatter(
                    x=x_vals,
                    y=y_vals,
                    mode="lines",
                    name=heading_label,
                    legendgroup=heading_label,
                    showlegend=show_legend,
                    line=dict(color=colors[ci]),
                ),
                row=row, col=col,
            )

    # ------------------------------------------------------------------
    # Utilities
    # ------------------------------------------------------------------

    def _get_x_values(self, component: RAOComponent) -> np.ndarray:
        if self._x_axis == "frequency":
            return component.frequencies.values
        return component.frequencies.periods

    def _x_axis_label(self) -> str:
        if self._x_axis == "frequency":
            return "Frequency (rad/s)"
        return "Period (s)"

    def _get_heading_indices(
        self, component: RAOComponent, headings: list[float] | None
    ) -> list[int]:
        if headings is None:
            return list(range(component.headings.count))
        indices: list[int] = []
        for h in headings:
            diffs = np.abs(component.headings.values - h)
            idx = int(np.argmin(diffs))
            if diffs[idx] < 1.0:  # within 1 degree tolerance
                indices.append(idx)
        return indices if indices else list(range(component.headings.count))

    @staticmethod
    def _get_heading_colors(n: int) -> list[str]:
        return [_COLORS[i % len(_COLORS)] for i in range(n)]

    @staticmethod
    def _apply_layout(fig: go.Figure, title: str) -> None:
        fig.update_layout(
            title_text=title,
            template="plotly_white",
            legend=dict(
                orientation="h",
                yanchor="bottom",
                y=1.01,
                xanchor="center",
                x=0.5,
            ),
            margin=dict(l=60, r=30, t=80, b=40),
        )

    def _save_figure(self, fig: go.Figure, filename: str) -> Path:
        path = self._output_dir / f"{filename}.html"
        fig.write_html(str(path), include_plotlyjs="cdn")
        return path
