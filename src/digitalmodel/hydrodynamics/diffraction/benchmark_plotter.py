"""Multi-solver overlay RAO plotter for benchmark comparison (WRK-032).

Generates interactive Plotly HTML plots that overlay RAO results from
multiple diffraction solvers (AQWA, OrcaWave, BEMRosetta, etc.) on the
same axes, enabling visual comparison of amplitude, phase, differences,
and pairwise correlations.
"""
from __future__ import annotations

import html as html_mod
from pathlib import Path
from typing import Any, Dict, List, Literal, Optional

import numpy as np
import plotly.graph_objects as go
from loguru import logger
from plotly.subplots import make_subplots

from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import MeshPipeline
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
        solver_metadata: Optional[Dict[str, Dict[str, Any]]] = None,
    ) -> None:
        self._solver_results = solver_results
        self._solver_names = sorted(solver_results.keys())
        self._output_dir = Path(output_dir)
        self._output_dir.mkdir(parents=True, exist_ok=True)
        self.x_axis = x_axis
        self._solver_metadata = solver_metadata or {}

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
        summary = self._compute_amplitude_summary(headings)
        return self._render_html_with_table(
            fig, summary, "benchmark_amplitude", "Amplitude",
        )

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
        summary = self._compute_phase_summary(headings)
        return self._render_html_with_table(
            fig, summary, "benchmark_phase", "Phase",
        )

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
        heading_indices: Optional[List[int]] = None,
    ) -> None:
        """Add traces for every solver and heading to the subplot.

        Traces are added heading-first so the legend groups solvers
        together for each heading (e.g. AQWA H0, OrcaWave H0, AQWA H45,
        OrcaWave H45, ...), making it easy to toggle a heading pair.

        Args:
            heading_indices: If provided, use these indices directly
                instead of computing from *headings*. This allows
                pre-filtered significant headings to be passed in.
        """
        dof_name = dof.name.lower()

        # Resolve heading indices from the first solver
        first_comp: RAOComponent = getattr(
            self._solver_results[self._solver_names[0]].raos, dof_name,
        )
        h_indices = (
            heading_indices
            if heading_indices is not None
            else self._get_heading_indices(first_comp, headings)
        )

        # Iterate headings first, then solvers — legend groups by heading
        for hi in h_indices:
            heading_label = f"{first_comp.headings.values[hi]:.0f}"
            for si, solver in enumerate(self._solver_names):
                style = self._get_solver_style(si)
                comp: RAOComponent = getattr(
                    self._solver_results[solver].raos, dof_name,
                )
                x_vals = self._get_x_values(comp)
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

    def _get_significant_heading_indices(
        self,
        dof: DOF,
        headings: Optional[List[float]],
        threshold: float = 0.01,
    ) -> List[int]:
        """Return heading indices where the DOF response is significant.

        A heading is significant if *any* solver has a peak amplitude
        exceeding ``threshold`` times the overall peak for that DOF across
        all solvers and headings. This filters out e.g. surge at 90 deg
        where the theoretical response is zero.

        Args:
            dof: The degree of freedom.
            headings: Optional heading filter (applied first).
            threshold: Fraction of overall peak below which a heading is
                considered insignificant (default 1%).

        Returns:
            List of heading indices with meaningful response.
        """
        dof_name = dof.name.lower()
        # Start with the standard heading filter
        first_comp: RAOComponent = getattr(
            self._solver_results[self._solver_names[0]].raos, dof_name,
        )
        candidate_indices = self._get_heading_indices(first_comp, headings)

        # Find overall peak amplitude across all solvers and headings
        overall_peak = 0.0
        for solver in self._solver_names:
            comp: RAOComponent = getattr(
                self._solver_results[solver].raos, dof_name,
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
            for solver in self._solver_names:
                comp = getattr(
                    self._solver_results[solver].raos, dof_name,
                )
                if float(np.max(np.abs(comp.magnitude[:, hi]))) > cutoff:
                    significant.append(hi)
                    break

        return significant if significant else candidate_indices

    @staticmethod
    def _get_solver_style(solver_idx: int) -> dict:
        """Return dash and colour style for a solver index."""
        return _SOLVER_STYLES.get(
            solver_idx % len(_SOLVER_STYLES),
            _SOLVER_STYLES[0],
        )

    @staticmethod
    def _apply_layout(fig: go.Figure, title: str) -> None:
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

    def _compute_amplitude_summary(
        self,
        headings: Optional[List[float]],
    ) -> List[Dict[str, Any]]:
        """Per-DOF amplitude summary rows for the comparison table.

        Returns a list of dicts, one per DOF, each containing a ``dof``
        label and ``rows`` — a list of per-solver/heading row dicts with
        keys: heading, solver, peak_amp, peak_period, long_period_amp,
        diff_pct.
        """
        sections: List[Dict[str, Any]] = []
        for dof in DOF_ORDER:
            dof_name = dof.name.lower()
            rows: List[Dict[str, Any]] = []
            # Collect peak info per solver so we can compute diff%
            solver_peaks: Dict[str, Dict[float, float]] = {}
            for solver in self._solver_names:
                comp: RAOComponent = getattr(
                    self._solver_results[solver].raos, dof_name,
                )
                h_indices = self._get_heading_indices(comp, headings)
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
            # Compute diff% relative to first solver
            ref_solver = self._solver_names[0]
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

    def _compute_phase_summary(
        self,
        headings: Optional[List[float]],
    ) -> List[Dict[str, Any]]:
        """Per-DOF phase summary rows for the comparison table.

        Returns a list of dicts similar to amplitude summary, with keys:
        heading, solver, phase_at_peak, long_period_phase, phase_diff.
        """
        sections: List[Dict[str, Any]] = []
        for dof in DOF_ORDER:
            dof_name = dof.name.lower()
            rows: List[Dict[str, Any]] = []
            solver_phase_at_peak: Dict[str, Dict[float, float]] = {}
            for solver in self._solver_names:
                comp: RAOComponent = getattr(
                    self._solver_results[solver].raos, dof_name,
                )
                h_indices = self._get_heading_indices(comp, headings)
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
            ref_solver = self._solver_names[0]
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

    def _render_html_with_table(
        self,
        fig: go.Figure,
        summary: List[Dict[str, Any]],
        filename: str,
        mode: Literal["Amplitude", "Phase"],
    ) -> Path:
        """Write an HTML page with Plotly plot (left) and table (right)."""
        plot_html = fig.to_html(
            full_html=False, include_plotlyjs="cdn",
        )
        table_html = self._build_summary_table(summary, mode)
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
        path = self._output_dir / f"{filename}.html"
        path.write_text(page, encoding="utf-8")
        return path

    @staticmethod
    def _build_summary_table(
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

    def build_input_comparison_html(self) -> str:
        """Render an HTML table comparing solver input parameters.

        Builds a structured comparison covering geometry, mass properties,
        environment, mesh, analysis settings, and phase/unit conventions.
        Values come from ``DiffractionResults`` fields and the optional
        ``solver_metadata`` dict passed at construction time.

        Returns:
            HTML string suitable for embedding in the benchmark report.
        """
        # Ordered parameter definitions: (display_label, source)
        # source is either a callable(dr, meta) -> str, or a meta key string
        _PARAM_ROWS: List[tuple] = [
            # -- Geometry --
            ("_section", "Geometry"),
            ("Length (m)", "length"),
            ("Beam (m)", "beam"),
            ("Draft (m)", "draft"),
            ("Body dimensions (L x B x T)", "body_dimensions"),
            # -- Mass Properties --
            ("_section", "Mass Properties"),
            ("Mass", "mass"),
            ("Centre of gravity (m)", "centre_of_gravity"),
            ("Radii of gyration (m)", "radii_of_gyration"),
            # -- Environment --
            ("_section", "Environment"),
            ("Water depth (m)", "_water_depth"),
            ("Water density (kg/m3)", "water_density"),
            ("Gravity (m/s2)", "gravity"),
            # -- Mesh --
            ("_section", "Mesh"),
            ("Mesh file", "mesh_file"),
            ("Mesh format", "mesh_format"),
            ("Panel count", "panel_count"),
            ("Symmetry", "mesh_symmetry"),
            # -- Damping --
            ("_section", "Damping"),
            ("Radiation damping", "radiation_damping"),
            ("Viscous damping", "viscous_damping"),
            ("Damping lid", "damping_lid"),
            # -- Analysis Settings --
            ("_section", "Analysis Settings"),
            ("Frequency range (rad/s)", "_freq_range"),
            ("Heading range (deg)", "_head_range"),
            ("Calculation method", "calculation_method"),
            ("Remove irregular freq.", "remove_irregular_frequencies"),
            ("QTF calculation", "qtf_calculation"),
            ("Precision", "precision"),
            # -- Conventions --
            ("_section", "Conventions"),
            ("Phase convention (raw)", "_raw_phase"),
            ("Phase convention (normalized)", "_norm_phase"),
            ("Unit system", "_unit_system"),
        ]

        # --- Build per-solver values for each parameter ---
        merged: Dict[str, Dict[str, str]] = {}
        section_order: List[str] = []

        for label, source in _PARAM_ROWS:
            if label == "_section":
                section_order.append(f"__section__{source}")
                continue

            values_found = False
            if label not in merged:
                merged[label] = {"parameter": label}

            for solver in self._solver_names:
                dr = self._solver_results[solver]
                meta = self._solver_metadata.get(solver, {})
                freq = dr.raos.surge.frequencies
                head = dr.raos.surge.headings

                val: Optional[str] = None

                # Built-in fields derived from DiffractionResults
                if source == "_water_depth":
                    val = f"{dr.water_depth:.1f}"
                elif source == "_freq_range":
                    val = (f"{freq.min_freq:.3f} – {freq.max_freq:.3f}"
                           f" ({freq.count})")
                elif source == "_head_range":
                    val = (f"{head.min_heading:.0f} – {head.max_heading:.0f}"
                           f" ({head.count})")
                elif source == "_raw_phase":
                    val = meta.get(
                        "raw_phase_convention",
                        "ISO 6954 (lead)" if dr.analysis_tool == "AQWA"
                        else "Orcina (lag)",
                    )
                elif source == "_norm_phase":
                    val = dr.phase_convention
                elif source == "_unit_system":
                    val = dr.unit_system
                elif source in meta:
                    val = str(meta[source])

                if val is not None:
                    merged[label][solver] = val
                    values_found = True

            # Track parameter in section order if any solver has data
            if values_found:
                section_order.append(label)

        # --- Render HTML ---
        parts: List[str] = [
            "<h2>Input Comparison</h2>",
            '<table class="input-table" style="max-width:800px;">',
            "<thead><tr><th>Parameter</th>",
        ]
        for solver in self._solver_names:
            parts.append(f"<th>{html_mod.escape(solver)}</th>")
        parts.append("</tr></thead><tbody>")

        n_cols = 1 + len(self._solver_names)
        for entry in section_order:
            if entry.startswith("__section__"):
                sec_name = entry.replace("__section__", "")
                parts.append(
                    f"<tr class='section-row'>"
                    f"<td colspan='{n_cols}'>"
                    f"{html_mod.escape(sec_name)}</td></tr>"
                )
                continue

            param_data = merged.get(entry)
            if not param_data:
                continue

            parts.append("<tr>")
            parts.append(
                f"<td class='param-label'>"
                f"{html_mod.escape(param_data['parameter'])}</td>"
            )
            for solver in self._solver_names:
                val = param_data.get(solver, "-")
                parts.append(f"<td>{html_mod.escape(val)}</td>")
            parts.append("</tr>")

        parts.append("</tbody></table>")
        return "\n".join(parts)

    def build_input_files_html(self) -> str:
        """Render scrollable input file previews for each solver.

        Looks for ``input_file`` key in solver_metadata. For each file
        found, renders a scrollable code block with the file contents
        and a button to open it in a new browser window.

        Returns:
            HTML string with file viewer sections, or empty string if
            no solver has ``input_file`` metadata.
        """
        max_lines = 2000
        file_entries: List[tuple] = []  # (solver, path_str, content)

        for solver in self._solver_names:
            meta = self._solver_metadata.get(solver, {})
            input_file = meta.get("input_file")
            if not input_file:
                continue

            file_path = Path(input_file)
            if not file_path.exists():
                logger.warning(
                    f"Input file for solver '{solver}' not found: "
                    f"{input_file}"
                )
                continue

            # Read file content with encoding fallback
            content: Optional[str] = None
            for encoding in ("utf-8", "latin-1"):
                try:
                    content = file_path.read_text(encoding=encoding)
                    break
                except (UnicodeDecodeError, OSError):
                    continue

            if content is None:
                logger.warning(
                    f"Could not read input file for solver '{solver}': "
                    f"{input_file}"
                )
                continue

            # Limit to max_lines
            lines = content.splitlines()
            truncated = len(lines) > max_lines
            if truncated:
                lines = lines[:max_lines]
            content = "\n".join(lines)

            file_entries.append(
                (solver, str(file_path), content, truncated, len(lines))
            )

        if not file_entries:
            return ""

        parts: List[str] = ["<h2>Input Files</h2>"]

        for idx, (solver, path_str, content, truncated, n_lines) in enumerate(
            file_entries
        ):
            safe_solver = html_mod.escape(solver)
            safe_path = html_mod.escape(path_str)
            safe_content = html_mod.escape(content)
            textarea_id = f"file_content_{idx}"

            # Build line-numbered content
            line_spans: List[str] = []
            for line in content.splitlines():
                line_spans.append(
                    f'<span class="line">{html_mod.escape(line)}</span>'
                )
            numbered_content = "\n".join(line_spans)

            truncation_note = ""
            if truncated:
                truncation_note = (
                    f'<div style="padding:0.4em 1em;background:#fef9e7;'
                    f'border:1px solid #ddd;border-top:none;font-size:0.8em;'
                    f'color:#888;font-style:italic;">'
                    f"Showing first {n_lines} lines (file truncated)"
                    f"</div>"
                )

            parts.append(f"""\
<div class="file-viewer">
  <div class="file-viewer-header">
    <div>
      <span class="solver-label">{safe_solver}</span>
      <span class="file-path">{safe_path}</span>
    </div>
    <button onclick="openFileWindow_{idx}()">Open in New Window</button>
  </div>
  <div class="file-content">
    <pre>{numbered_content}</pre>
  </div>
  {truncation_note}
  <textarea id="{textarea_id}" style="display:none;">{safe_content}</textarea>
  <script>
    function openFileWindow_{idx}() {{
      var ta = document.getElementById('{textarea_id}');
      var w = window.open('', '_blank');
      w.document.write(
        '<html><head><title>{safe_solver} - {safe_path}</title>' +
        '<style>body{{font-family:"SF Mono","Cascadia Code","Consolas",' +
        'monospace;white-space:pre;margin:1em;font-size:13px;' +
        'line-height:1.5;tab-size:4;}}</style></head><body>' +
        ta.value.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;') +
        '</body></html>'
      );
      w.document.close();
    }}
  </script>
</div>""")

        return "\n".join(parts)

    # ------------------------------------------------------------------
    # Mesh schematic
    # ------------------------------------------------------------------

    def build_mesh_schematic_html(self) -> str:
        """Build 3D mesh schematic section for the benchmark report.

        Loads mesh files from solver_metadata["mesh_path"] and creates
        an interactive Plotly 3D visualization showing the panel mesh
        with a waterline plane.

        Returns:
            HTML string with the mesh schematic section, or empty string
            if no mesh paths are available.
        """
        # Collect mesh paths from solver metadata
        mesh_entries: List[tuple] = []  # (solver_name, mesh_path_str)
        for solver in self._solver_names:
            meta = self._solver_metadata.get(solver, {})
            mesh_path = meta.get("mesh_path")
            if mesh_path:
                mesh_entries.append((solver, str(mesh_path)))

        if not mesh_entries:
            return ""

        # Load meshes
        pipeline = MeshPipeline()
        loaded: List[tuple] = []  # (solver_name, PanelMesh)
        for solver, mesh_path_str in mesh_entries:
            try:
                mesh = pipeline.load(Path(mesh_path_str))
                loaded.append((solver, mesh))
            except Exception as exc:
                logger.warning(
                    f"Could not load mesh for solver '{solver}' "
                    f"from '{mesh_path_str}': {exc}"
                )

        if not loaded:
            return ""

        # Build Plotly figure
        fig = go.Figure()

        for idx, (solver, mesh) in enumerate(loaded):
            style = self._get_solver_style(idx)
            color = style["color_base"]
            verts = mesh.vertices
            panels = mesh.panels

            # --- Surface: split quads into two triangles ---
            i_list: List[int] = []
            j_list: List[int] = []
            k_list: List[int] = []
            for panel in panels:
                valid = [v for v in panel if v >= 0]
                if len(valid) >= 3:
                    i_list.append(valid[0])
                    j_list.append(valid[1])
                    k_list.append(valid[2])
                if len(valid) == 4:
                    i_list.append(valid[0])
                    j_list.append(valid[2])
                    k_list.append(valid[3])

            fig.add_trace(
                go.Mesh3d(
                    x=verts[:, 0],
                    y=verts[:, 1],
                    z=verts[:, 2],
                    i=i_list,
                    j=j_list,
                    k=k_list,
                    opacity=0.6,
                    color=color,
                    name=f"{solver} ({mesh.n_panels} panels)",
                    showlegend=True,
                )
            )

            # --- Wireframe edges ---
            edge_x: List[Optional[float]] = []
            edge_y: List[Optional[float]] = []
            edge_z: List[Optional[float]] = []
            for panel in panels:
                valid = [v for v in panel if v >= 0]
                if len(valid) < 3:
                    continue
                for vi in valid:
                    edge_x.append(float(verts[vi, 0]))
                    edge_y.append(float(verts[vi, 1]))
                    edge_z.append(float(verts[vi, 2]))
                # Close the polygon
                edge_x.append(float(verts[valid[0], 0]))
                edge_y.append(float(verts[valid[0], 1]))
                edge_z.append(float(verts[valid[0], 2]))
                # None separator
                edge_x.append(None)
                edge_y.append(None)
                edge_z.append(None)

            fig.add_trace(
                go.Scatter3d(
                    x=edge_x,
                    y=edge_y,
                    z=edge_z,
                    mode="lines",
                    line=dict(color=color, width=1),
                    name=f"{solver} edges",
                    showlegend=False,
                    hoverinfo="skip",
                )
            )

        # --- Waterline plane at z=0 ---
        # Compute bounding box across all loaded meshes
        all_verts = np.vstack([m.vertices for _, m in loaded])
        x_min, y_min, _ = np.min(all_verts, axis=0)
        x_max, y_max, _ = np.max(all_verts, axis=0)
        # Extend waterline plane slightly beyond mesh bounds
        pad_x = (x_max - x_min) * 0.15
        pad_y = (y_max - y_min) * 0.15
        wx = [x_min - pad_x, x_max + pad_x, x_max + pad_x, x_min - pad_x]
        wy = [y_min - pad_y, y_min - pad_y, y_max + pad_y, y_max + pad_y]
        wz = [0.0, 0.0, 0.0, 0.0]
        fig.add_trace(
            go.Mesh3d(
                x=wx,
                y=wy,
                z=wz,
                i=[0, 0],
                j=[1, 2],
                k=[2, 3],
                opacity=0.15,
                color="#3498db",
                name="Waterline (z=0)",
                showlegend=True,
                hoverinfo="skip",
            )
        )

        fig.update_layout(
            title_text="Panel Mesh Geometry",
            template="plotly_white",
            scene=dict(
                aspectmode="data",
                xaxis_title="X (m)",
                yaxis_title="Y (m)",
                zaxis_title="Z (m)",
            ),
            margin=dict(l=0, r=0, t=40, b=0),
            height=600,
        )

        plot_div = fig.to_html(
            full_html=False,
            include_plotlyjs=False,
            div_id="mesh_schematic",
        )

        # --- Summary table ---
        table_rows = ""
        for solver, mesh in loaded:
            table_rows += (
                f"<tr>"
                f"<td>{html_mod.escape(solver)}</td>"
                f"<td>{mesh.n_panels}</td>"
                f"<td>{mesh.n_vertices}</td>"
                f"<td>{mesh.total_area:.2f}</td>"
                f"</tr>\n"
            )

        summary_table = (
            "<table style='max-width:500px;margin-top:0.8em;'>"
            "<tr><th>Solver</th><th>Panels</th>"
            "<th>Vertices</th><th>Total Area (m&sup2;)</th></tr>"
            f"{table_rows}"
            "</table>"
        )

        return (
            f"<h2>Panel Mesh Geometry</h2>\n"
            f"{plot_div}\n"
            f"{summary_table}"
        )

    # ------------------------------------------------------------------
    # Per-DOF plots with individual legends
    # ------------------------------------------------------------------

    def plot_per_dof(
        self,
        headings: Optional[List[float]] = None,
    ) -> Dict[str, Path]:
        """Generate individual per-DOF plots (amplitude + phase, 2 rows).

        Each DOF gets its own HTML file with an independent legend.

        Returns:
            Mapping of DOF name to the saved HTML path.
        """
        paths: Dict[str, Path] = {}
        for dof in DOF_ORDER:
            dof_name = dof.name.lower()
            fig = make_subplots(
                rows=2,
                cols=1,
                shared_xaxes=True,
                subplot_titles=[
                    f"Amplitude ({_AMPLITUDE_UNITS[dof]})",
                    "Phase (deg)",
                ],
                vertical_spacing=0.12,
            )
            # All traces show legend (individual per plot)
            self._add_solver_traces(
                fig, dof, headings, 1, 1, "amplitude", show_legend=True,
            )
            self._add_solver_traces(
                fig, dof, headings, 2, 1, "phase", show_legend=False,
            )
            fig.update_xaxes(title_text=self._x_axis_label(), row=2, col=1)
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
            path = self._save_figure(fig, f"benchmark_{dof_name}")
            paths[dof_name] = path
        return paths

    def build_dof_report_sections(
        self,
        report: BenchmarkReport,
        headings: Optional[List[float]] = None,
    ) -> str:
        """Build per-DOF two-column HTML sections (text left, plot right).

        Each DOF section contains:
        - Left column: conclusions, statistics table, and observations
        - Right column: inline Plotly plot (amplitude + phase)

        Args:
            report: BenchmarkReport with consensus and pairwise data.
            headings: Optional heading filter.

        Returns:
            HTML string with all 6 DOF sections.
        """
        # Get pairwise comparison data (first pair for 2-solver case)
        pair_data = {}
        if report.pairwise_results:
            first_pair = next(iter(report.pairwise_results.values()))
            pair_data = first_pair.rao_comparisons

        parts: List[str] = []
        for dof in DOF_ORDER:
            dof_name = dof.name.lower()
            dof_upper = dof.name.upper()
            dof_cap = dof.name.capitalize()

            # --- Determine significant headings for this DOF ---
            sig_indices = self._get_significant_heading_indices(
                dof, headings,
            )
            # Resolve heading values for table filtering
            first_comp: RAOComponent = getattr(
                self._solver_results[self._solver_names[0]].raos, dof_name,
            )
            sig_heading_vals = {
                f"{first_comp.headings.values[hi]:.0f}"
                for hi in sig_indices
            }
            all_indices = self._get_heading_indices(first_comp, headings)
            skipped_headings = [
                f"{first_comp.headings.values[hi]:.0f}"
                for hi in all_indices if hi not in sig_indices
            ]

            # --- Build per-DOF plot (significant headings only) ---
            fig = make_subplots(
                rows=2,
                cols=1,
                shared_xaxes=True,
                subplot_titles=[
                    f"Amplitude ({_AMPLITUDE_UNITS[dof]})",
                    "Phase (deg)",
                ],
                vertical_spacing=0.15,
            )
            self._add_solver_traces(
                fig, dof, headings, 1, 1, "amplitude",
                show_legend=True, heading_indices=sig_indices,
            )
            self._add_solver_traces(
                fig, dof, headings, 2, 1, "phase",
                show_legend=False, heading_indices=sig_indices,
            )
            fig.update_xaxes(title_text=self._x_axis_label(), row=2, col=1)
            fig.update_yaxes(
                title_text=_AMPLITUDE_UNITS[dof], row=1, col=1,
            )
            fig.update_yaxes(title_text="deg", row=2, col=1)
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

            # --- Consensus info ---
            cm = report.consensus_by_dof.get(dof_upper)
            consensus_level = cm.consensus_level if cm else "UNKNOWN"
            mean_corr = cm.mean_pairwise_correlation if cm else 0.0
            consensus_color = {
                "FULL": "#27ae60",
                "MAJORITY": "#f39c12",
                "NO_CONSENSUS": "#e74c3c",
            }.get(consensus_level, "#999")

            # --- Pairwise RAO stats ---
            rao_comp = pair_data.get(dof_name)
            mag_corr = rao_comp.magnitude_stats.correlation if rao_comp else 0
            mag_rms = rao_comp.magnitude_stats.rms_error if rao_comp else 0
            phase_corr = rao_comp.phase_stats.correlation if rao_comp else 0
            max_mag_diff = rao_comp.max_magnitude_diff if rao_comp else 0
            max_phase_diff = rao_comp.max_phase_diff if rao_comp else 0

            # --- Amplitude/phase tables (significant headings only) ---
            amp_rows = self._compute_dof_amplitude_rows(
                dof, sig_indices,
            )
            phase_rows = self._compute_dof_phase_rows(
                dof, sig_indices,
            )
            amp_table = self._build_solver_column_table(
                amp_rows, "amplitude",
            )
            phase_table = self._build_solver_column_table(
                phase_rows, "phase",
            )

            # Skipped headings note
            skipped_note = ""
            if skipped_headings:
                skipped_note = (
                    f'<p class="skipped-note">Headings with negligible '
                    f'response omitted: {", ".join(skipped_headings)}&deg;</p>'
                )

            # --- Observation text ---
            obs = self._generate_dof_observations(
                dof_cap, consensus_level, mag_corr, phase_corr,
                max_mag_diff, max_phase_diff, _AMPLITUDE_UNITS[dof],
            )

            # --- Assemble DOF section ---
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

    def _build_solver_column_table(
        self,
        rows: List[Dict[str, Any]],
        mode: str,
    ) -> str:
        """Build a comparison table with solver names as columns.

        Instead of rows per solver, groups by heading with one column per
        solver for side-by-side reading.
        """
        if not rows:
            return "<p><em>No data</em></p>"

        # Group rows by heading
        by_heading: Dict[str, Dict[str, Dict[str, Any]]] = {}
        for r in rows:
            h = r["heading"]
            solver = r.get("solver", "")
            if h not in by_heading:
                by_heading[h] = {}
            by_heading[h][solver] = r

        parts: List[str] = ['<table class="solver-table">']

        if mode == "amplitude":
            # Header row with solver sub-columns
            parts.append("<tr><th rowspan='2'>Hdg</th>")
            for solver in self._solver_names:
                parts.append(
                    f"<th colspan='3'>"
                    f"{html_mod.escape(solver)}</th>"
                )
            parts.append("</tr><tr>")
            for _ in self._solver_names:
                parts.append(
                    "<th>Peak</th><th>T(s)</th><th>LP</th>"
                )
            parts.append("</tr>")

            for h, solvers in by_heading.items():
                parts.append(f"<tr><td>{h}&deg;</td>")
                for solver in self._solver_names:
                    r = solvers.get(solver, {})
                    parts.append(
                        f"<td>{r.get('peak_amp', '-')}</td>"
                        f"<td>{r.get('peak_period', '-')}</td>"
                        f"<td>{r.get('long_period_amp', '-')}</td>"
                    )
                parts.append("</tr>")
        else:
            parts.append("<tr><th rowspan='2'>Hdg</th>")
            for solver in self._solver_names:
                parts.append(
                    f"<th colspan='2'>"
                    f"{html_mod.escape(solver)}</th>"
                )
            parts.append("</tr><tr>")
            for _ in self._solver_names:
                parts.append("<th>@Peak</th><th>LP</th>")
            parts.append("</tr>")

            for h, solvers in by_heading.items():
                parts.append(f"<tr><td>{h}&deg;</td>")
                for solver in self._solver_names:
                    r = solvers.get(solver, {})
                    parts.append(
                        f"<td>{r.get('phase_at_peak', '-')}</td>"
                        f"<td>{r.get('long_period_phase', '-')}</td>"
                    )
                parts.append("</tr>")

        parts.append("</table>")
        return "\n".join(parts)

    def _compute_dof_amplitude_rows(
        self,
        dof: DOF,
        h_indices: List[int],
    ) -> List[Dict[str, Any]]:
        """Compute amplitude summary rows for a DOF using given headings."""
        dof_name = dof.name.lower()
        rows: List[Dict[str, Any]] = []
        for solver in self._solver_names:
            comp: RAOComponent = getattr(
                self._solver_results[solver].raos, dof_name,
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
        self,
        dof: DOF,
        h_indices: List[int],
    ) -> List[Dict[str, Any]]:
        """Compute phase summary rows for a DOF using given headings."""
        dof_name = dof.name.lower()
        rows: List[Dict[str, Any]] = []
        for solver in self._solver_names:
            comp: RAOComponent = getattr(
                self._solver_results[solver].raos, dof_name,
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

    @staticmethod
    def _generate_dof_observations(
        dof_name: str,
        consensus: str,
        mag_corr: float,
        phase_corr: float,
        max_mag_diff: float,
        max_phase_diff: float,
        unit: str,
    ) -> str:
        """Generate human-readable observation text for a DOF."""
        lines: List[str] = []

        if consensus == "FULL":
            lines.append(
                f"<p>Solvers show <strong>full agreement</strong> on "
                f"{dof_name} response.</p>"
            )
        elif consensus == "MAJORITY":
            lines.append(
                f"<p>Solvers show <strong>majority agreement</strong> on "
                f"{dof_name}; minor outlier detected.</p>"
            )
        else:
            lines.append(
                f"<p>Solvers show <strong>no consensus</strong> on "
                f"{dof_name} response — review recommended.</p>"
            )

        if mag_corr > 0.999:
            lines.append(
                "<p>Amplitude curves are virtually identical "
                f"(r={mag_corr:.4f}).</p>"
            )
        elif mag_corr > 0.99:
            lines.append(
                f"<p>Amplitude agreement is excellent (r={mag_corr:.4f}), "
                f"with max diff of {max_mag_diff:.4g} {unit}.</p>"
            )
        elif mag_corr > 0.95:
            lines.append(
                f"<p>Amplitude correlation is good (r={mag_corr:.4f}) "
                f"but max diff reaches {max_mag_diff:.4g} {unit}.</p>"
            )
        else:
            lines.append(
                f"<p>Amplitude correlation is moderate (r={mag_corr:.4f}); "
                f"max diff of {max_mag_diff:.4g} {unit} warrants "
                "investigation.</p>"
            )

        if max_phase_diff > 90:
            lines.append(
                f"<p>Phase difference reaches {max_phase_diff:.1f}&deg; "
                "— check phase convention or resonance behavior.</p>"
            )
        elif max_phase_diff > 20:
            lines.append(
                f"<p>Phase difference up to {max_phase_diff:.1f}&deg; "
                "near resonance — typical for sharp peaks.</p>"
            )
        else:
            lines.append(
                f"<p>Phase agreement within {max_phase_diff:.1f}&deg;.</p>"
            )

        return "\n".join(lines)

    def _save_figure(self, fig: go.Figure, filename: str) -> Path:
        """Write figure to HTML with CDN Plotly.js and return the path."""
        path = self._output_dir / f"{filename}.html"
        fig.write_html(str(path), include_plotlyjs="cdn")
        return path
