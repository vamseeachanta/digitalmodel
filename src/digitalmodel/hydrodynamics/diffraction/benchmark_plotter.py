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

        Extracts metadata from ``DiffractionResults`` fields (frequency
        range, heading range, phase convention, unit system) and any
        additional solver_metadata passed at construction time.

        Returns:
            HTML string suitable for embedding in the benchmark report.
        """
        rows: List[Dict[str, str]] = []

        # --- Build per-solver column data ---
        for solver in self._solver_names:
            dr = self._solver_results[solver]
            meta = self._solver_metadata.get(solver, {})

            freq = dr.raos.surge.frequencies
            head = dr.raos.surge.headings

            rows.append({
                "parameter": "Solver",
                solver: dr.analysis_tool,
            })
            rows.append({
                "parameter": "Water depth (m)",
                solver: f"{dr.water_depth:.1f}",
            })
            rows.append({
                "parameter": "Frequency range (rad/s)",
                solver: f"{freq.min_freq:.3f} – {freq.max_freq:.3f} ({freq.count})",
            })
            rows.append({
                "parameter": "Heading range (deg)",
                solver: f"{head.min_heading:.0f} – {head.max_heading:.0f} ({head.count})",
            })
            rows.append({
                "parameter": "Phase convention (raw)",
                solver: meta.get(
                    "raw_phase_convention",
                    "ISO 6954 (lead)" if dr.analysis_tool == "AQWA"
                    else "Orcina (lag)",
                ),
            })
            rows.append({
                "parameter": "Phase convention (normalized)",
                solver: dr.phase_convention,
            })
            rows.append({
                "parameter": "Unit system",
                solver: dr.unit_system,
            })
            # Optional metadata fields
            for key in ("panel_count", "calculation_method", "mesh_file",
                        "body_dimensions"):
                if key in meta:
                    label = key.replace("_", " ").title()
                    rows.append({
                        "parameter": label,
                        solver: str(meta[key]),
                    })

        # --- Merge rows by parameter ---
        merged: Dict[str, Dict[str, str]] = {}
        for row in rows:
            param = row["parameter"]
            if param not in merged:
                merged[param] = {"parameter": param}
            for k, v in row.items():
                if k != "parameter":
                    merged[param][k] = v

        # --- Render HTML table ---
        parts: List[str] = [
            "<h2>Input Comparison</h2>",
            '<table style="border-collapse:collapse;margin:1em 0;">',
            "<tr><th style='border:1px solid #ccc;padding:0.5em 1em;"
            "background:#f0f0f0;text-align:left;'>Parameter</th>",
        ]
        for solver in self._solver_names:
            parts.append(
                f"<th style='border:1px solid #ccc;padding:0.5em 1em;"
                f"background:#f0f0f0;text-align:left;'>"
                f"{html_mod.escape(solver)}</th>"
            )
        parts.append("</tr>")

        for param_data in merged.values():
            parts.append("<tr>")
            parts.append(
                f"<td style='border:1px solid #ccc;padding:0.4em 0.8em;"
                f"font-weight:600;'>"
                f"{html_mod.escape(param_data['parameter'])}</td>"
            )
            for solver in self._solver_names:
                val = param_data.get(solver, "-")
                parts.append(
                    f"<td style='border:1px solid #ccc;padding:0.4em 0.8em;'>"
                    f"{html_mod.escape(val)}</td>"
                )
            parts.append("</tr>")

        parts.append("</table>")
        return "\n".join(parts)

    def _save_figure(self, fig: go.Figure, filename: str) -> Path:
        """Write figure to HTML with CDN Plotly.js and return the path."""
        path = self._output_dir / f"{filename}.html"
        fig.write_html(str(path), include_plotlyjs="cdn")
        return path
