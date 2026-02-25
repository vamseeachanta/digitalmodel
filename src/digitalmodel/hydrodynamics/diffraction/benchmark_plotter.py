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
from digitalmodel.hydrodynamics.diffraction.diffraction_units import rad_per_s_to_period_s
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

# Threshold: amplitude below 5% of peak is considered negligible for
# phase interpretation. Phase is physically meaningless when the signal
# is near zero (e.g. yaw on a symmetric body, or off-axis DOFs).
_NEGLIGIBLE_AMPLITUDE_RATIO = 0.05


def _is_phase_at_negligible_amplitude(
    mag_at_phase_diff: float,
    peak_mag: float,
) -> bool:
    """Return True if the amplitude where max phase diff occurs is negligible.

    Phase values are undefined / meaningless when the underlying signal
    amplitude is near zero. This helper lets the commentary and plot
    annotations communicate this clearly.
    """
    if peak_mag <= 0:
        return True  # entirely zero signal — phase is meaningless
    return mag_at_phase_diff / peak_mag < _NEGLIGIBLE_AMPLITUDE_RATIO


def _parse_fdf_panels(fdf_path: Path) -> list[list[list[float]]]:
    """Parse a WAMIT .fdf free-surface panel file.

    Each data row contains 8 values encoding 4 panel corner coordinates in the
    horizontal plane (z=0): x1,x2,x3,x4,y1,y2,y3,y4 (all x-coords then all
    y-coords, for the XZ-symmetry quadrant y>=0).

    Returns a list of panels; each panel is a list of 4 [x, y, 0.0] vertices.
    Returns an empty list on any read failure.
    """
    try:
        panels: list[list[list[float]]] = []
        with fdf_path.open(encoding="utf-8", errors="replace") as fh:
            lines = fh.readlines()
        # First 4 lines are header (title, RINNER, NPF/NTCL, NAL params)
        for raw in lines[4:]:
            raw = raw.strip()
            if not raw:
                continue
            try:
                vals = [float(v) for v in raw.split()]
            except ValueError:
                continue
            if len(vals) != 8:
                continue
            # x1,x2,x3,x4 then y1,y2,y3,y4
            xs = vals[:4]
            ys = vals[4:]
            panels.append([[xs[i], ys[i], 0.0] for i in range(4)])
        return panels
    except Exception:
        return []


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
        # Auto-detect axis orientation: when headings outnumber frequencies,
        # plot RAO vs heading with period as legend (prevents empty plots when
        # nfreqs == 1 but nheadings is large, e.g. OrcaWave validation 2.8).
        _first = self._solver_names[0]
        _first_comp = self._solver_results[_first].raos.get_component(DOF_ORDER[0])
        self._heading_x_axis: bool = _first_comp.headings.count > len(
            _first_comp.frequencies.values
        )

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

        if self._heading_x_axis:
            # Heading on x-axis; one trace per solver×period.
            # mode=lines+markers so even a single-period case is visible.
            for si, solver in enumerate(self._solver_names):
                style = self._get_solver_style(si)
                comp: RAOComponent = getattr(
                    self._solver_results[solver].raos, dof_name,
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
            # Iterate headings first, then solvers — legend groups by heading
            for hi in h_indices:
                heading_label = f"{first_comp.headings.values[hi]:.0f}"
                for si, solver in enumerate(self._solver_names):
                    style = self._get_solver_style(si)
                    comp = getattr(
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
        if self._heading_x_axis:
            return "Heading (deg)"
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
            # -- OrcaWave Solver Settings --
            ("_section", "OrcaWave Solver Settings"),
            ("Units system", "ow_units_system"),
            ("Solve type", "ow_solve_type"),
            ("Load RAO method", "ow_load_rao_method"),
            ("Linear solver", "ow_linear_solver"),
            ("Divide non-planar panels", "ow_divide_nonplanar"),
            ("Length tolerance", "ow_length_tolerance"),
            ("Waves referred to by", "ow_waves_referred_to_by"),
            # -- OrcaWave Body Details --
            ("_section", "OrcaWave Body Details"),
            ("Body count", "ow_body_count"),
            ("Body name", "ow_body_name"),
            ("Inertia specification", "ow_inertia_spec"),
            ("Interior surface panels", "ow_interior_panels"),
            ("Connection parent", "ow_connection_parent"),
            ("Fixed DOFs", "ow_fixed_dofs"),
            ("Roll damping target", "ow_roll_damping_target"),
            ("External damping", "ow_external_damping"),
            ("External stiffness", "ow_external_stiffness"),
            # -- OrcaWave Advanced --
            ("_section", "OrcaWave Advanced"),
            ("Damping lid", "ow_damping_lid"),
            ("Damping lid mesh", "ow_damping_lid_mesh"),
            ("Damping factor (epsilon)", "ow_damping_epsilon"),
            ("QTF calc method", "ow_qtf_method"),
            ("QTF crossing angles", "ow_qtf_crossing_angles"),
            ("QTF frequency types", "ow_qtf_freq_types"),
            ("Include mean drift QTFs", "ow_qtf_mean_drift"),
            ("Free surface zone type", "ow_fsz_type"),
            ("Free surface zone mesh", "ow_fsz_mesh"),
            ("Free surface inner radius", "ow_fsz_inner_radius"),
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

        # Pre-scan: identify sections where ALL rows are "-" (skip them)
        sections_with_data: set[str] = set()
        current_section = ""
        for entry in section_order:
            if entry.startswith("__section__"):
                current_section = entry
                continue
            param_data = merged.get(entry)
            if param_data:
                has_real_value = any(
                    param_data.get(s, "-") != "-"
                    for s in self._solver_names
                )
                if has_real_value:
                    sections_with_data.add(current_section)

        for entry in section_order:
            if entry.startswith("__section__"):
                # Skip section header if all its rows are "-"
                if entry not in sections_with_data:
                    continue
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

            # Skip rows where all solvers show "-"
            all_dash = all(
                param_data.get(s, "-") == "-"
                for s in self._solver_names
            )
            if all_dash:
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

    def build_semantic_equivalence_html(self) -> str:
        """Render semantic equivalence comparison between solver inputs.

        Reads ``_semantic_equivalence`` from solver_metadata (set by the
        validation script) and renders a summary table plus diff details.

        Returns:
            HTML string with the semantic equivalence section, or empty
            string if no semantic data is available.
        """
        # Look for semantic data in any solver's metadata
        sem_data: Optional[Dict[str, Any]] = None
        for solver in self._solver_names:
            meta = self._solver_metadata.get(solver, {})
            if "_semantic_equivalence" in meta:
                sem_data = meta["_semantic_equivalence"]
                break

        if sem_data is None:
            return ""

        match_count = sem_data.get("match_count", 0)
        cosmetic_count = sem_data.get("cosmetic_count", 0)
        convention_count = sem_data.get("convention_count", 0)
        sig_count = sem_data.get("significant_count", 0)
        total = match_count + cosmetic_count + convention_count + sig_count
        diffs = sem_data.get("diffs", [])

        # Verdict badge
        if sig_count == 0:
            badge_color = "#27ae60"
            badge_text = "EQUIVALENT"
        elif sig_count <= 5:
            badge_color = "#f39c12"
            badge_text = f"{sig_count} SIGNIFICANT DIFF(S)"
        else:
            badge_color = "#e74c3c"
            badge_text = f"{sig_count} SIGNIFICANT DIFF(S)"

        parts: List[str] = [
            "<h3>Semantic Equivalence</h3>",
            "<p>Key-by-key comparison of the OrcaWave SaveData() YAML "
            "exports from both solver paths. Keys are classified as:</p>"
            "<ul style='font-size:13px;margin:0.3em 0 0.8em;'>"
            "<li><strong>Cosmetic</strong> — GUI display, naming, "
            "output flags, dormant settings (no solver effect)</li>"
            "<li><strong>Convention</strong> — equivalent data in "
            "different representation (e.g. Hz vs rad/s)</li>"
            "<li><strong>Significant</strong> — real solver parameter "
            "difference that may affect results</li></ul>",
            "<table class='stats-table' style='max-width:500px;"
            "margin-bottom:1em;'>",
            "<tr><th>Metric</th><th>Count</th></tr>",
            f"<tr><td>Matching keys</td>"
            f"<td style='color:#27ae60;font-weight:bold'>"
            f"{match_count}</td></tr>",
            f"<tr><td>Cosmetic differences</td>"
            f"<td style='color:#6b7280'>{cosmetic_count}</td></tr>",
            f"<tr><td>Convention differences</td>"
            f"<td style='color:#2563eb'>{convention_count}</td></tr>",
            f"<tr><td>Significant differences</td>"
            f"<td style='color:{'#e74c3c' if sig_count > 0 else '#27ae60'}"
            f";font-weight:bold'>{sig_count}</td></tr>",
            f"<tr><td>Total keys compared</td><td>{total}</td></tr>",
            "</table>",
            f"<div style='display:inline-block;padding:4px 16px;"
            f"border-radius:4px;color:white;font-weight:bold;"
            f"background:{badge_color};margin-bottom:1em;'>"
            f"{badge_text}</div>",
        ]

        # Diff details table
        if diffs:
            sig_diffs = [d for d in diffs if d["level"] == "significant"]
            conv_diffs = [d for d in diffs if d["level"] == "convention"]
            cos_diffs = [d for d in diffs if d["level"] == "cosmetic"]

            # Comments explaining why each key is classified this way
            _KEY_COMMENTS: Dict[str, str] = {
                # Significant
                "DivideNonPlanarPanels": (
                    "Splits non-planar panels into triangles; "
                    "no effect on planar meshes but matters "
                    "for curved geometry"
                ),
                # Convention
                "WavesReferredToBy": (
                    "Same frequencies, different unit label"
                ),
                "PeriodOrFrequency": (
                    "Same frequency grid expressed as "
                    "rad/s vs period (s)"
                ),
                # Cosmetic — QTF dormant
                "PreferredQuadraticLoadCalculationMethod": (
                    "QTF setting; dormant when QTF disabled"
                ),
                "QTFMinCrossingAngle": (
                    "QTF setting; dormant when QTF disabled"
                ),
                "QTFMaxCrossingAngle": (
                    "QTF setting; dormant when QTF disabled"
                ),
                "QuadraticLoadPressureIntegration": (
                    "QTF setting; dormant when QTF disabled"
                ),
                "QTFCalculationMethod": (
                    "QTF setting; dormant when QTF disabled"
                ),
                "QTFFrequencyTypes": (
                    "QTF setting; dormant when QTF disabled"
                ),
                "IncludeMeanDriftFullQTFs": (
                    "QTF setting; dormant when QTF disabled"
                ),
                # Cosmetic — output flags
                "OutputPanelPressures": (
                    "Output flag; no effect on RAO results"
                ),
                "OutputPanelVelocities": (
                    "Output flag; no effect on RAO results"
                ),
                # Cosmetic — field points
                "FieldPointX, FieldPointY, FieldPointZ": (
                    "Pressure monitoring points; "
                    "not used in RAO calculation"
                ),
                # Cosmetic — GUI pens
                "FreeSurfaceMeshPen": "GUI display color only",
                "InteriorSurfacePanelsPen": "GUI display color only",
                "BodyMeshPen": "GUI display color only",
                "WaterlinePen": "GUI display color only",
                "DampingLidMeshPen": "GUI display color only",
                # Cosmetic — naming / identity
                "BodyName": "Label only; no solver effect",
                "BodyMeshFileName": (
                    "Different filename, same mesh geometry"
                ),
                "BodyOrcaFlexImportLength": (
                    "OrcaFlex GUI import hint; "
                    "not used by OrcaWave solver"
                ),
                "BodyOrcaFlexImportSymmetry": (
                    "OrcaFlex import setting; "
                    "not a solver parameter"
                ),
                "DampingLidMeshFileName": (
                    "Different filename; same lid geometry"
                ),
                # Cosmetic — OrcaWave internal defaults
                "ComputationStrategy": (
                    "OrcaWave internal default; "
                    "not configurable via spec"
                ),
                "EnableMultibodyConstraints": (
                    "OrcaWave internal default; "
                    "not configurable via spec"
                ),
                "BodyOriginType": (
                    "OrcaWave internal default; "
                    "not configurable via spec"
                ),
                "BodyVolumeWarningLevel": (
                    "OrcaWave internal default; "
                    "not configurable via spec"
                ),
            }

            # Default comments by level
            _LEVEL_DEFAULTS: Dict[str, str] = {
                "significant": "Solver parameter difference",
                "convention": "Same physics, different representation",
                "cosmetic": "No solver effect",
            }

            def _comment_for(d: Dict[str, Any]) -> str:
                # Strip Bodies[N]. prefix for lookup
                key = d["key"]
                bare = key.split(".")[-1] if "." in key else key
                return _KEY_COMMENTS.get(
                    bare,
                    _LEVEL_DEFAULTS.get(d["level"], ""),
                )

            def _render_diff_table(
                items: List[Dict[str, Any]], title: str,
                collapsed: bool = False,
            ) -> None:
                if not items:
                    return
                header = (
                    "<table style='max-width:1000px;font-size:12px;"
                    "margin-top:0.5em;'>"
                    "<tr><th style='text-align:left'>Key</th>"
                    "<th style='text-align:left'>OrcaWave (.owd)</th>"
                    "<th style='text-align:left'>OrcaWave (spec.yml)</th>"
                    "<th style='text-align:left'>Comment</th>"
                    "</tr>"
                )
                rows = ""
                for d in items:
                    comment = html_mod.escape(_comment_for(d))
                    rows += (
                        f"<tr>"
                        f"<td><code>{html_mod.escape(d['key'])}</code></td>"
                        f"<td>{html_mod.escape(d['owd'])}</td>"
                        f"<td>{html_mod.escape(d['spec'])}</td>"
                        f"<td style='color:#777;font-style:italic;'>"
                        f"{comment}</td>"
                        f"</tr>"
                    )
                if collapsed:
                    parts.append(
                        f"<details style='margin-top:0.5em;font-size:12px;'>"
                        f"<summary>{title} ({len(items)})</summary>"
                        f"{header}{rows}</table></details>"
                    )
                else:
                    parts.append(f"<h4>{title}</h4>{header}{rows}</table>")

            _render_diff_table(sig_diffs, "Significant Differences")
            _render_diff_table(
                conv_diffs, "Convention Differences", collapsed=True,
            )
            _render_diff_table(
                cos_diffs, "Cosmetic Differences", collapsed=True,
            )

        return "\n".join(parts)

    # Descriptive labels for solver input files
    _FILE_DESCRIPTIONS: Dict[str, str] = {
        "OrcaWave (.owd)": (
            "OrcaWave input configuration exported from the original .owd "
            "project via SaveData(). This is the ground truth — the exact "
            "parameters used by the manually-configured OrcaWave project."
        ),
        "OrcaWave (spec.yml)": (
            "OrcaWave input configuration exported from the spec.yml "
            "pipeline via SaveData(). This is the auto-generated project — "
            "built by OrcaWaveRunner from the declarative spec."
        ),
        "AQWA": (
            "AQWA solver input listing (.LIS) showing the full solver "
            "configuration including element data, boundary conditions, "
            "and analysis parameters."
        ),
    }

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

        # Build semantic equivalence section first
        semantic_html = self.build_semantic_equivalence_html()

        parts: List[str] = ["<h2>Input Files</h2>"]
        if semantic_html:
            parts.append(semantic_html)

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

            # Descriptive subsection header
            description = self._FILE_DESCRIPTIONS.get(solver, "")
            desc_html = ""
            if description:
                desc_html = (
                    f'<p style="margin:0.3em 0 0.5em;font-size:12px;'
                    f'color:#64748b;max-width:700px;">'
                    f'{html_mod.escape(description)}</p>'
                )

            parts.append(f"""\
<h3 style="margin-top:1.5em;margin-bottom:0.2em;">{safe_solver}</h3>
{desc_html}
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

        Primary: uses OrcFxAPI panelGeometry if available in solver_metadata
        (symmetry-expanded, eliminates GDF path resolution issues).
        Fallback: loads mesh files from solver_metadata["mesh_path"].

        Returns:
            HTML string with the mesh schematic section, or empty string
            if no mesh source is available.
        """
        # --- Primary: OrcFxAPI panelGeometry (symmetry-expanded) ---
        for solver in self._solver_names:
            meta = self._solver_metadata.get(solver, {})
            panel_geometry_data = meta.get("panel_geometry")
            if panel_geometry_data:
                n_panels = len(panel_geometry_data)
                has_vertices = bool(
                    panel_geometry_data and "vertices" in panel_geometry_data[0]
                )
                fdf_path = meta.get("fdf_path")
                if has_vertices:
                    mesh_html = self._build_panel_mesh3d_html(
                        panel_geometry_data,
                        fdf_path=fdf_path,
                        title=f"Panel Mesh & Free Surface Zone ({solver})",
                    )
                    fs_note = (
                        " + free-surface zone from FDF"
                        if fdf_path else ""
                    )
                    return (
                        f"<h2>Panel Mesh Geometry</h2>\n"
                        f'<p style="color:#555;font-size:0.9em">Source: OrcFxAPI panelGeometry '
                        f'({n_panels} panels, symmetry-expanded{fs_note}). '
                        f"Use the view buttons or drag to rotate.</p>\n"
                        f"{mesh_html}"
                    )
                # fallback: scatter plot of centroids when vertices not available
                scatter_html = self._build_panel_scatter_html(
                    panel_geometry_data,
                    title=f"Panel Geometry ({solver})",
                )
                return (
                    f"<h2>Panel Mesh Geometry</h2>\n"
                    f'<p style="color:#555;font-size:0.9em">Source: OrcFxAPI panelGeometry '
                    f'({n_panels} panels, symmetry-expanded)</p>\n'
                    f"{scatter_html}"
                )

        # --- Fallback: GDF file loading ---
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

    # ------------------------------------------------------------------
    # Hydrodynamic coefficients section
    # ------------------------------------------------------------------

    def build_hydro_coefficients_html(
        self,
        report: BenchmarkReport,
    ) -> str:
        """Build 6x6 added-mass and damping correlation matrices.

        Renders the pairwise added_mass_correlations and
        damping_correlations as colour-coded 6x6 tables so the
        reviewer can see at a glance which DOF couplings agree.
        """
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
                parts.append(self._render_6x6_matrix(
                    corr_dict, dof_labels,
                ))

        return "\n".join(parts)

    @staticmethod
    def build_coupling_heatmap_html(
        output_dir: Path,
        am_corr: list[list[float]],
        damp_corr: list[list[float]],
        body_i_name: str,
        body_j_name: str,
    ) -> Path:
        """Render 6x6 correlation heatmaps for coupling matrices (AM & Damp)."""
        import html as html_mod

        dof_labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]

        # Convert list of lists to dict for _render_6x6_matrix
        def _to_dict(matrix):
            d = {}
            for i in range(6):
                for j in range(6):
                    d[(i + 1, j + 1)] = matrix[i][j]
            return d

        am_dict = _to_dict(am_corr)
        damp_dict = _to_dict(damp_corr)

        am_table = BenchmarkPlotter._render_6x6_matrix(am_dict, dof_labels)
        damp_table = BenchmarkPlotter._render_6x6_matrix(damp_dict, dof_labels)

        title = f"Coupling: {body_i_name} \u2194 {body_j_name}"
        # Sanitize filename
        safe_i = "".join(c for c in body_i_name if c.isalnum() or c in "_-")
        safe_j = "".join(c for c in body_j_name if c.isalnum() or c in "_-")
        filename = f"coupling_{safe_i}_{safe_j}".lower()

        html = f"""<!DOCTYPE html>
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
        path.write_text(html, encoding="utf-8")
        return path

    @staticmethod
    def _render_6x6_matrix(
        corr_dict: dict,
        labels: List[str],
    ) -> str:
        """Render a 6x6 correlation matrix as an HTML table."""
        rows: List[str] = ['<table class="solver-table" '
                           'style="width:auto;max-width:600px;">']
        # Header
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
                # Colour: green for >0.999, yellow for >0.99, red below
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

    @staticmethod
    def _build_panel_scatter_html(
        panel_geometry_data: list,
        title: str = "Panel Geometry",
        height: int = 400,
    ) -> str:
        """Build interactive 3D scatter of panel centroids from panelGeometry data.

        Groups panels by objectName (body name), uses one colour per body.
        Marker size scales with sqrt(area). Adds a semi-transparent waterplane at z=0.

        Args:
            panel_geometry_data: List of dicts with 'area', 'centroid', 'objectName'
            title: Plot title
            height: Figure height in pixels

        Returns:
            HTML div string (Plotly figure, no full_html wrapper)
        """
        import numpy as np
        import plotly.graph_objects as go

        # Group panels by body name
        bodies_data: dict = {}
        for p in panel_geometry_data:
            name = p.get("objectName", "Body")
            if name not in bodies_data:
                bodies_data[name] = {"x": [], "y": [], "z": [], "area": []}
            c = p["centroid"]
            bodies_data[name]["x"].append(c[0])
            bodies_data[name]["y"].append(c[1])
            bodies_data[name]["z"].append(c[2])
            bodies_data[name]["area"].append(p["area"])

        colours = ["#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"]
        traces: List[Any] = []
        for i, (name, pts) in enumerate(bodies_data.items()):
            sizes = [max(4.0, 12.0 * (a ** 0.5)) for a in pts["area"]]
            traces.append(go.Scatter3d(
                x=pts["x"], y=pts["y"], z=pts["z"],
                mode="markers",
                marker=dict(size=sizes, color=colours[i % len(colours)], opacity=0.7),
                name=name,
                text=[f"Area: {a:.3f} m\u00b2" for a in pts["area"]],
                hovertemplate="%{text}<extra>%{fullData.name}</extra>",
            ))

        # Waterplane reference at z=0
        all_x = [v for b in bodies_data.values() for v in b["x"]]
        all_y = [v for b in bodies_data.values() for v in b["y"]]
        if all_x and all_y:
            xr = [min(all_x) * 1.1, max(all_x) * 1.1]
            yr = [min(all_y) * 1.1, max(all_y) * 1.1]
            traces.append(go.Surface(
                x=[[xr[0], xr[1]], [xr[0], xr[1]]],
                y=[[yr[0], yr[0]], [yr[1], yr[1]]],
                z=[[0.0, 0.0], [0.0, 0.0]],
                showscale=False, opacity=0.15,
                colorscale=[[0, "#4fc3f7"], [1, "#4fc3f7"]],
                name="Waterplane", showlegend=False,
            ))

        fig = go.Figure(data=traces)
        fig.update_layout(
            title=title, height=height,
            scene=dict(
                xaxis_title="X (m)", yaxis_title="Y (m)", zaxis_title="Z (m)",
                aspectmode="data",
            ),
            margin=dict(l=0, r=0, t=40, b=0),
        )
        return fig.to_html(full_html=False, include_plotlyjs="cdn")

    @staticmethod
    def _build_panel_mesh3d_html(
        panel_geometry_data: list,
        fdf_path: Optional[str] = None,
        title: str = "Panel Mesh & Free Surface Zone",
        height: int = 560,
    ) -> str:
        """Build interactive Mesh3d figure from panelGeometry vertices.

        Renders body panels as a semi-transparent coral surface with wireframe
        edges.  If fdf_path is supplied, also renders the free-surface zone
        panels as a cyan overlay.  Three camera-preset buttons are provided:
        Perspective (default), Plan (top-down), and Elevation (side view).

        Args:
            panel_geometry_data: List of dicts with 'vertices' key (4×3 float).
            fdf_path: Optional path to WAMIT .fdf free-surface panel file.
            title: Figure title.
            height: Figure height in pixels.

        Returns:
            HTML div string (no full_html wrapper, assumes Plotly already loaded).
        """
        # ---- body mesh --------------------------------------------------
        vx: List[float] = []
        vy: List[float] = []
        vz: List[float] = []
        tri_i: List[int] = []
        tri_j: List[int] = []
        tri_k: List[int] = []
        ex: List[Optional[float]] = []
        ey: List[Optional[float]] = []
        ez: List[Optional[float]] = []

        for panel in panel_geometry_data:
            verts = panel.get("vertices")
            if not verts or len(verts) < 3:
                continue
            n = len(vx)
            for v in verts:
                vx.append(float(v[0]))
                vy.append(float(v[1]))
                vz.append(float(v[2]))
            tri_i.append(n); tri_j.append(n + 1); tri_k.append(n + 2)
            if len(verts) == 4:
                tri_i.append(n); tri_j.append(n + 2); tri_k.append(n + 3)
            # wireframe — close the polygon
            for v in verts:
                ex.append(float(v[0]))
                ey.append(float(v[1]))
                ez.append(float(v[2]))
            ex.append(float(verts[0][0]))
            ey.append(float(verts[0][1]))
            ez.append(float(verts[0][2]))
            ex.append(None); ey.append(None); ez.append(None)

        fig = go.Figure()
        if vx:
            fig.add_trace(go.Mesh3d(
                x=vx, y=vy, z=vz,
                i=tri_i, j=tri_j, k=tri_k,
                color="#e05a5a",
                opacity=0.45,
                name="Body panels",
                showlegend=True,
                hoverinfo="skip",
            ))
            fig.add_trace(go.Scatter3d(
                x=ex, y=ey, z=ez,
                mode="lines",
                line=dict(color="#c0392b", width=1),
                name="Body edges",
                showlegend=False,
                hoverinfo="skip",
            ))

        # ---- free-surface zone -----------------------------------------
        if fdf_path:
            fdf_panels = _parse_fdf_panels(Path(fdf_path))
            if fdf_panels:
                fvx: List[float] = []
                fvy: List[float] = []
                fvz: List[float] = []
                fti: List[int] = []
                ftj: List[int] = []
                ftk: List[int] = []
                fex: List[Optional[float]] = []
                fey: List[Optional[float]] = []
                fez: List[Optional[float]] = []
                for pverts in fdf_panels:
                    n = len(fvx)
                    for v in pverts:
                        fvx.append(float(v[0]))
                        fvy.append(float(v[1]))
                        fvz.append(float(v[2]))
                    fti.append(n); ftj.append(n + 1); ftk.append(n + 2)
                    if len(pverts) == 4:
                        fti.append(n); ftj.append(n + 2); ftk.append(n + 3)
                    for v in pverts:
                        fex.append(float(v[0]))
                        fey.append(float(v[1]))
                        fez.append(float(v[2]))
                    fex.append(float(pverts[0][0]))
                    fey.append(float(pverts[0][1]))
                    fez.append(float(pverts[0][2]))
                    fex.append(None); fey.append(None); fez.append(None)
                # also mirror y >= 0 panels to y <= 0 for XZ symmetry
                n0 = len(fvx)
                mirror_vx = list(fvx)
                mirror_vy = [-y for y in fvy]
                mirror_vz = list(fvz)
                mirror_i = [i + n0 for i in fti]
                mirror_j = [j + n0 for j in ftj]
                mirror_k = [k + n0 for k in ftk]
                fvx += mirror_vx
                fvy += mirror_vy
                fvz += mirror_vz
                fti += mirror_i
                ftj += mirror_j
                ftk += mirror_k
                fig.add_trace(go.Mesh3d(
                    x=fvx, y=fvy, z=fvz,
                    i=fti, j=ftj, k=ftk,
                    color="#00bcd4",
                    opacity=0.25,
                    name="Free-surface zone",
                    showlegend=True,
                    hoverinfo="skip",
                ))
                # mirror wire edges too
                mirror_ex = list(fex)
                mirror_ey = [(-y if y is not None else None) for y in fey]
                mirror_ez = list(fez)
                fex += mirror_ex
                fey += mirror_ey
                fez += mirror_ez
                fig.add_trace(go.Scatter3d(
                    x=fex, y=fey, z=fez,
                    mode="lines",
                    line=dict(color="#0097a7", width=0.8),
                    name="FS edges",
                    showlegend=False,
                    hoverinfo="skip",
                ))

        # ---- camera presets -------------------------------------------
        cam_persp = dict(
            eye=dict(x=1.6, y=1.2, z=0.9),
            up=dict(x=0, y=0, z=1),
        )
        cam_plan = dict(
            eye=dict(x=0, y=0, z=3.0),
            up=dict(x=0, y=1, z=0),
        )
        cam_elev = dict(
            eye=dict(x=3.0, y=0, z=0),
            up=dict(x=0, y=0, z=1),
        )
        fig.update_layout(
            title=dict(text=title, font=dict(size=13)),
            height=height,
            scene=dict(
                aspectmode="data",
                xaxis_title="X (m)",
                yaxis_title="Y (m)",
                zaxis_title="Z (m)",
            ),
            margin=dict(l=0, r=0, t=55, b=0),
            legend=dict(x=0.01, y=0.99, bgcolor="rgba(255,255,255,0.7)"),
            updatemenus=[dict(
                type="buttons",
                direction="right",
                x=0.0, y=1.10,
                xanchor="left",
                buttons=[
                    dict(label="Perspective",
                         method="relayout",
                         args=[{"scene.camera": cam_persp}]),
                    dict(label="Plan (top)",
                         method="relayout",
                         args=[{"scene.camera": cam_plan}]),
                    dict(label="Elevation (side)",
                         method="relayout",
                         args=[{"scene.camera": cam_elev}]),
                ],
            )],
        )
        fig.update_layout(scene_camera=cam_persp)
        return fig.to_html(full_html=False, include_plotlyjs=False)

    # ------------------------------------------------------------------
    # Raw RAO data tables (collapsible)
    # ------------------------------------------------------------------

    def build_raw_rao_data_html(
        self,
        headings: Optional[List[float]] = None,
    ) -> str:
        """Build collapsible tables of raw RAO magnitude+phase per DOF.

        Provides full transparency: every frequency point, every heading,
        every solver value side-by-side. Wrapped in <details> so it
        doesn't overwhelm the report when collapsed.
        """
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
                self._solver_results[self._solver_names[0]].raos, dof_name,
            )
            h_indices = self._get_heading_indices(first_comp, headings)
            periods = first_comp.frequencies.periods

            parts.append(
                f"<details><summary><strong>{dof_cap}</strong> "
                f"({len(periods)} freq &times; {len(h_indices)} hdg)"
                f"</summary>"
            )

            # Build table header
            tbl: List[str] = ['<div style="overflow-x:auto;">'
                              '<table class="solver-table">']
            tbl.append("<tr><th>T (s)</th>")
            for hi in h_indices:
                hdg_val = first_comp.headings.values[hi]
                for solver in self._solver_names:
                    short = solver.split("(")[-1].rstrip(")") if "(" in solver else solver
                    tbl.append(
                        f"<th>{hdg_val:.0f}&deg; {short}<br>"
                        f"Mag ({unit})</th>"
                        f"<th>{hdg_val:.0f}&deg; {short}<br>"
                        f"Phase (&deg;)</th>"
                    )
            tbl.append("</tr>")

            # Data rows (limit to first 100 freq to avoid huge HTML)
            n_freq = min(len(periods), 100)
            for fi in range(n_freq):
                tbl.append(f"<tr><td>{periods[fi]:.4f}</td>")
                for hi in h_indices:
                    for solver in self._solver_names:
                        comp: RAOComponent = getattr(
                            self._solver_results[solver].raos, dof_name,
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
            mag_at_max_pd = (
                rao_comp.magnitude_at_max_phase_diff if rao_comp else 0
            )
            peak_mag = rao_comp.peak_magnitude if rao_comp else 0
            max_pd_freq = rao_comp.max_phase_diff_x if rao_comp else 0
            max_pd_hi = (
                rao_comp.max_phase_diff_heading_idx if rao_comp else 0
            )

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

            # --- Add intelligent annotations to the phase subplot ---
            # Only annotate if the max phase diff heading is actually
            # displayed in the plot. Annotations about invisible data
            # are misleading and reduce report credibility.
            self._add_phase_annotations(
                fig, dof, max_phase_diff, mag_at_max_pd,
                peak_mag, max_pd_freq, _AMPLITUDE_UNITS[dof],
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
            pd_heading_visible = max_pd_hi in sig_indices
            obs = self._generate_dof_observations(
                dof_cap, consensus_level, mag_corr, phase_corr,
                max_mag_diff, max_phase_diff, _AMPLITUDE_UNITS[dof],
                magnitude_at_max_phase_diff=mag_at_max_pd,
                peak_magnitude=peak_mag,
                phase_diff_at_visible_heading=pd_heading_visible,
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

    def _add_phase_annotations(
        self,
        fig: go.Figure,
        dof: DOF,
        max_phase_diff: float,
        mag_at_max_pd: float,
        peak_mag: float,
        max_pd_freq: float,
        unit: str,
        phase_diff_heading_idx: int = 0,
        visible_heading_indices: Optional[List[int]] = None,
    ) -> None:
        """Add annotations to the phase subplot based on plotted data only.

        Annotations must correspond to data visible in the plot.
        If the max phase diff occurs at a heading that was filtered out
        (negligible response at that heading), no annotation is shown —
        it would reference invisible data and mislead the reader.
        """
        if max_phase_diff < 20:
            return  # small phase diff — no annotation needed

        # Gate: only annotate data the reader can actually see.
        if visible_heading_indices is not None:
            if phase_diff_heading_idx not in visible_heading_indices:
                return  # diff is at a hidden heading — skip

        # Compute the (x, y) coordinates on the phase plot
        if self.x_axis == "period" and max_pd_freq > 0:
            x_val = rad_per_s_to_period_s(max_pd_freq)
        else:
            x_val = max_pd_freq

        dof_name = dof.name.lower()
        first_comp: RAOComponent = getattr(
            self._solver_results[self._solver_names[0]].raos, dof_name,
        )
        freq_idx = int(np.argmin(
            np.abs(first_comp.frequencies.values - max_pd_freq),
        ))
        y_val = float(
            first_comp.phase[freq_idx, phase_diff_heading_idx],
        )
        x_label = f"{x_val:.1f}s" if self.x_axis == "period" else (
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

    @staticmethod
    def _generate_dof_observations(
        dof_name: str,
        consensus: str,
        mag_corr: float,
        phase_corr: float,
        max_mag_diff: float,
        max_phase_diff: float,
        unit: str,
        magnitude_at_max_phase_diff: float = 0.0,
        peak_magnitude: float = 0.0,
        phase_diff_at_visible_heading: bool = True,
    ) -> str:
        """Generate human-readable observation text for a DOF.

        Commentary must only describe data visible in the plot.
        When the max phase diff occurs at a hidden heading (filtered
        due to negligible response), the text acknowledges this rather
        than alarming the reader about invisible discrepancies.
        """
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

        # Phase commentary — only describe what the reader can see.
        if not phase_diff_at_visible_heading and max_phase_diff > 20:
            # Diff is at a heading filtered from the plot (negligible
            # response at that heading). Don't alarm the reader about
            # data they can't see — note it factually instead.
            lines.append(
                f"<p>Max phase difference of {max_phase_diff:.1f}&deg; "
                "occurs at a heading omitted from the plot (negligible "
                "response). Displayed headings show good phase "
                "agreement.</p>"
            )
        elif max_phase_diff > 90:
            phase_at_negligible = _is_phase_at_negligible_amplitude(
                magnitude_at_max_phase_diff, peak_magnitude,
            )
            if phase_at_negligible:
                lines.append(
                    f"<p>Phase difference of {max_phase_diff:.1f}&deg; "
                    "occurs where amplitude is insignificant "
                    f"({magnitude_at_max_phase_diff:.2e} {unit}). "
                    "Phase angle is physically undefined at near-zero "
                    "magnitude and <strong>can be ignored</strong>.</p>"
                )
            else:
                lines.append(
                    f"<p>Phase difference reaches {max_phase_diff:.1f}&deg; "
                    f"at significant amplitude "
                    f"({magnitude_at_max_phase_diff:.4g} {unit}) "
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
