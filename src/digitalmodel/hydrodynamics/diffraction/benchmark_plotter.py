"""Multi-solver overlay RAO plotter for benchmark comparison (WRK-032).

Generates interactive Plotly HTML plots that overlay RAO results from
multiple diffraction solvers (AQWA, OrcaWave, BEMRosetta, etc.) on the
same axes, enabling visual comparison of amplitude, phase, differences,
and pairwise correlations.
"""
from __future__ import annotations

from pathlib import Path
from typing import Dict, List, Literal, Optional

import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots

from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
    BenchmarkReport,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
    DOF,
    RAOComponent,
)

DOF_ORDER = [DOF.SURGE, DOF.SWAY, DOF.HEAVE, DOF.ROLL, DOF.PITCH, DOF.YAW]

_AMPLITUDE_UNITS = {
    DOF.SURGE: "m/m",
    DOF.SWAY: "m/m",
    DOF.HEAVE: "m/m",
    DOF.ROLL: "deg/m",
    DOF.PITCH: "deg/m",
    DOF.YAW: "deg/m",
}

_SOLVER_STYLES = {
    0: {"dash": "solid", "color_base": "#1f77b4"},
    1: {"dash": "dash", "color_base": "#ff7f0e"},
    2: {"dash": "dot", "color_base": "#2ca02c"},
    3: {"dash": "dashdot", "color_base": "#d62728"},
}


class BenchmarkPlotter:
    """Generate interactive Plotly HTML overlay plots from multiple solvers.

    Args:
        solver_results: Mapping of solver name to DiffractionResults.
        output_dir: Directory for saving HTML files (created if absent).
        x_axis: Domain for the horizontal axis, ``"period"`` or
            ``"frequency"``.
    """

    def __init__(
        self,
        solver_results: Dict[str, DiffractionResults],
        output_dir: Path,
        x_axis: Literal["period", "frequency"] = "period",
    ) -> None:
        self._solver_results = solver_results
        self._solver_names = sorted(solver_results.keys())
        self._output_dir = Path(output_dir)
        self._output_dir.mkdir(parents=True, exist_ok=True)
        self.x_axis = x_axis

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def plot_amplitude_overlay(
        self,
        headings: Optional[List[float]] = None,
    ) -> Path:
        """6-row subplot of RAO amplitude with one trace per solver/heading.

        Returns:
            Path to the written HTML file.
        """
        subplot_titles = [
            f"{d.name.capitalize()} ({_AMPLITUDE_UNITS[d]})"
            for d in DOF_ORDER
        ]
        fig = make_subplots(
            rows=6,
            cols=1,
            shared_xaxes=True,
            subplot_titles=subplot_titles,
            vertical_spacing=0.03,
        )
        for row, dof in enumerate(DOF_ORDER, start=1):
            self._add_solver_traces(
                fig, dof, headings, row, 1, "amplitude", show_legend=(row == 1)
            )
        fig.update_xaxes(title_text=self._x_axis_label(), row=6, col=1)
        self._apply_layout(fig, "Benchmark RAO Amplitude Overlay")
        fig.update_layout(height=1200)
        return self._save_figure(fig, "benchmark_amplitude")

    def plot_phase_overlay(
        self,
        headings: Optional[List[float]] = None,
    ) -> Path:
        """6-row subplot of RAO phase with one trace per solver/heading.

        Returns:
            Path to the written HTML file.
        """
        subplot_titles = [
            f"{d.name.capitalize()} Phase (deg)" for d in DOF_ORDER
        ]
        fig = make_subplots(
            rows=6,
            cols=1,
            shared_xaxes=True,
            subplot_titles=subplot_titles,
            vertical_spacing=0.03,
        )
        for row, dof in enumerate(DOF_ORDER, start=1):
            self._add_solver_traces(
                fig, dof, headings, row, 1, "phase", show_legend=(row == 1)
            )
        fig.update_xaxes(title_text=self._x_axis_label(), row=6, col=1)
        self._apply_layout(fig, "Benchmark RAO Phase Overlay")
        fig.update_layout(height=1200)
        return self._save_figure(fig, "benchmark_phase")

    def plot_combined_overlay(
        self,
        headings: Optional[List[float]] = None,
    ) -> Path:
        """6 rows x 2 cols: amplitude left, phase right.

        Returns:
            Path to the written HTML file.
        """
        subplot_titles: List[str] = []
        for dof in DOF_ORDER:
            subplot_titles.append(f"{dof.name.capitalize()} Amplitude")
            subplot_titles.append(f"{dof.name.capitalize()} Phase")

        fig = make_subplots(
            rows=6,
            cols=2,
            shared_xaxes=True,
            subplot_titles=subplot_titles,
            vertical_spacing=0.03,
            horizontal_spacing=0.06,
        )
        for row, dof in enumerate(DOF_ORDER, start=1):
            self._add_solver_traces(
                fig, dof, headings, row, 1, "amplitude", show_legend=(row == 1)
            )
            self._add_solver_traces(
                fig, dof, headings, row, 2, "phase", show_legend=False
            )
        fig.update_xaxes(title_text=self._x_axis_label(), row=6, col=1)
        fig.update_xaxes(title_text=self._x_axis_label(), row=6, col=2)
        self._apply_layout(fig, "Benchmark RAO Combined (Amplitude | Phase)")
        fig.update_layout(height=1400)
        return self._save_figure(fig, "benchmark_combined")

    def plot_difference(
        self,
        reference_solver: str,
        headings: Optional[List[float]] = None,
    ) -> Path:
        """Plot magnitude difference relative to a reference solver.

        Args:
            reference_solver: Name of the solver used as the baseline.
            headings: Optional heading filter.

        Raises:
            ValueError: If *reference_solver* is not among the stored results.

        Returns:
            Path to the written HTML file.
        """
        if reference_solver not in self._solver_results:
            raise ValueError(
                f"Reference solver '{reference_solver}' not found. "
                f"Available: {self._solver_names}"
            )

        other_solvers = [
            s for s in self._solver_names if s != reference_solver
        ]

        subplot_titles = [
            f"{d.name.capitalize()} Diff ({_AMPLITUDE_UNITS[d]})"
            for d in DOF_ORDER
        ]
        fig = make_subplots(
            rows=6,
            cols=1,
            shared_xaxes=True,
            subplot_titles=subplot_titles,
            vertical_spacing=0.03,
        )

        ref_results = self._solver_results[reference_solver]

        for row, dof in enumerate(DOF_ORDER, start=1):
            dof_name = dof.name.lower()
            ref_comp: RAOComponent = getattr(ref_results.raos, dof_name)
            h_indices = self._get_heading_indices(ref_comp, headings)
            x_vals = self._get_x_values(ref_comp)

            for si, solver in enumerate(other_solvers):
                style = self._get_solver_style(si)
                comp: RAOComponent = getattr(
                    self._solver_results[solver].raos, dof_name
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

        fig.update_xaxes(title_text=self._x_axis_label(), row=6, col=1)
        self._apply_layout(
            fig,
            f"Benchmark Difference (ref: {reference_solver})",
        )
        fig.update_layout(height=1200)
        return self._save_figure(fig, "benchmark_difference")

    def plot_pairwise_correlation_heatmap(
        self,
        report: BenchmarkReport,
    ) -> Path:
        """Heatmap of mean pairwise magnitude correlation across all DOFs.

        Args:
            report: A ``BenchmarkReport`` produced by
                :class:`MultiSolverComparator`.

        Returns:
            Path to the written HTML file.
        """
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
        self._apply_layout(fig, "Pairwise Solver Correlation")
        fig.update_layout(height=500)
        return self._save_figure(fig, "benchmark_heatmap")

    def plot_all(
        self,
        headings: Optional[List[float]] = None,
    ) -> List[Path]:
        """Generate amplitude, phase, and combined overlay plots.

        Returns:
            List of at least three :class:`~pathlib.Path` objects.
        """
        return [
            self.plot_amplitude_overlay(headings=headings),
            self.plot_phase_overlay(headings=headings),
            self.plot_combined_overlay(headings=headings),
        ]

    # ------------------------------------------------------------------
    # Trace helpers
    # ------------------------------------------------------------------

    def _add_solver_traces(
        self,
        fig: go.Figure,
        dof: DOF,
        headings: Optional[List[float]],
        row: int,
        col: int,
        value_type: str,
        show_legend: bool,
    ) -> None:
        """Add traces for every solver and heading to the subplot."""
        dof_name = dof.name.lower()

        for si, solver in enumerate(self._solver_names):
            style = self._get_solver_style(si)
            comp: RAOComponent = getattr(
                self._solver_results[solver].raos, dof_name
            )
            h_indices = self._get_heading_indices(comp, headings)
            x_vals = self._get_x_values(comp)

            for hi in h_indices:
                heading_label = f"{comp.headings.values[hi]:.0f}"
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

    # ------------------------------------------------------------------
    # Internal utilities
    # ------------------------------------------------------------------

    def _get_x_values(self, component: RAOComponent) -> np.ndarray:
        """Return period or frequency array based on ``x_axis``."""
        if self.x_axis == "frequency":
            return component.frequencies.values
        return component.frequencies.periods

    def _x_axis_label(self) -> str:
        """Human-readable label for the x-axis."""
        if self.x_axis == "frequency":
            return "Frequency (rad/s)"
        return "Period (s)"

    def _get_heading_indices(
        self,
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

    @staticmethod
    def _get_solver_style(solver_idx: int) -> dict:
        """Return dash and colour style for a solver index."""
        return _SOLVER_STYLES.get(
            solver_idx % len(_SOLVER_STYLES),
            _SOLVER_STYLES[0],
        )

    @staticmethod
    def _apply_layout(fig: go.Figure, title: str) -> None:
        """Apply common Plotly layout: white template, horizontal legend."""
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
        """Write figure to HTML with CDN Plotly.js and return the path."""
        path = self._output_dir / f"{filename}.html"
        fig.write_html(str(path), include_plotlyjs="cdn")
        return path
