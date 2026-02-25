"""Multi-solver overlay RAO plotter for benchmark comparison (WRK-032).

ABOUTME: Thin orchestrator class that delegates to four focused modules:
  - benchmark_helpers.py   (constants, pure utilities)
  - benchmark_rao_plots.py (RAO overlay/difference/per-DOF plotting)
  - benchmark_correlation.py (pairwise heatmap, DOF report sections,
                              hydro coefficients, raw data tables)
  - benchmark_input_reports.py (input comparison, files, mesh schematic)

Refactored from a 2700-line God Object as part of WRK-592.
"""
from __future__ import annotations

from pathlib import Path
from typing import Any, Dict, List, Literal, Optional

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
    DOF,
)

# Re-export module-level constants for any external callers
from digitalmodel.hydrodynamics.diffraction.benchmark_helpers import (  # noqa: F401
    DOF_ORDER,
    _AMPLITUDE_UNITS,
    _SOLVER_STYLES,
    _NEGLIGIBLE_AMPLITUDE_RATIO,
    _is_phase_at_negligible_amplitude,
    _parse_fdf_panels,
    _FILE_DESCRIPTIONS,
    generate_dof_observations,
)

from digitalmodel.hydrodynamics.diffraction.benchmark_rao_plots import (
    add_solver_traces as _add_solver_traces,
    apply_layout as _apply_layout,
    build_summary_table as _build_summary_table,
    compute_amplitude_summary as _compute_amplitude_summary,
    compute_phase_summary as _compute_phase_summary,
    get_heading_indices as _get_heading_indices,
    get_significant_heading_indices as _get_significant_heading_indices,
    get_solver_style as _get_solver_style,
    get_x_values as _get_x_values,
    plot_amplitude_overlay as _plot_amplitude_overlay,
    plot_combined_overlay as _plot_combined_overlay,
    plot_difference as _plot_difference,
    plot_per_dof as _plot_per_dof,
    plot_phase_overlay as _plot_phase_overlay,
    render_html_with_table as _render_html_with_table,
    save_figure as _save_figure,
    x_axis_label as _x_axis_label,
)

from digitalmodel.hydrodynamics.diffraction.benchmark_correlation import (
    build_coupling_heatmap_html as _build_coupling_heatmap_html,
    build_dof_report_sections as _build_dof_report_sections,
    build_hydro_coefficients_html as _build_hydro_coefficients_html,
    build_raw_rao_data_html as _build_raw_rao_data_html,
    plot_pairwise_correlation_heatmap as _plot_pairwise_correlation_heatmap,
    render_6x6_matrix as _render_6x6_matrix,
)

from digitalmodel.hydrodynamics.diffraction.benchmark_input_reports import (
    build_input_comparison_html as _build_input_comparison_html,
    build_input_files_html as _build_input_files_html,
    build_mesh_schematic_html as _build_mesh_schematic_html,
)

# Lazy import only when needed
# from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
#     BenchmarkReport,
# )


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
        _first = self._solver_names[0]
        _first_comp = self._solver_results[_first].raos.get_component(
            DOF_ORDER[0],
        )
        self._heading_x_axis: bool = _first_comp.headings.count > len(
            _first_comp.frequencies.values
        )

    # ------------------------------------------------------------------
    # Public API — RAO overlay plots
    # ------------------------------------------------------------------

    def plot_amplitude_overlay(
        self,
        headings: Optional[List[float]] = None,
    ) -> Path:
        """6-row subplot of RAO amplitude with one trace per solver/heading."""
        return _plot_amplitude_overlay(
            self._solver_results, self._solver_names,
            self._output_dir, self.x_axis, self._heading_x_axis,
            headings=headings,
        )

    def plot_phase_overlay(
        self,
        headings: Optional[List[float]] = None,
    ) -> Path:
        """6-row subplot of RAO phase with one trace per solver/heading."""
        return _plot_phase_overlay(
            self._solver_results, self._solver_names,
            self._output_dir, self.x_axis, self._heading_x_axis,
            headings=headings,
        )

    def plot_combined_overlay(
        self,
        headings: Optional[List[float]] = None,
    ) -> Path:
        """6 rows x 2 cols: amplitude left, phase right."""
        return _plot_combined_overlay(
            self._solver_results, self._solver_names,
            self._output_dir, self.x_axis, self._heading_x_axis,
            headings=headings,
        )

    def plot_difference(
        self,
        reference_solver: str,
        headings: Optional[List[float]] = None,
    ) -> Path:
        """Plot magnitude difference relative to a reference solver."""
        return _plot_difference(
            reference_solver,
            self._solver_results, self._solver_names,
            self._output_dir, self.x_axis, self._heading_x_axis,
            headings=headings,
        )

    def plot_all(
        self,
        headings: Optional[List[float]] = None,
    ) -> List[Path]:
        """Generate amplitude, phase, and combined overlay plots."""
        return [
            self.plot_amplitude_overlay(headings=headings),
            self.plot_phase_overlay(headings=headings),
            self.plot_combined_overlay(headings=headings),
        ]

    def plot_per_dof(
        self,
        headings: Optional[List[float]] = None,
    ) -> Dict[str, Path]:
        """Generate individual per-DOF plots (amplitude + phase, 2 rows)."""
        return _plot_per_dof(
            self._solver_results, self._solver_names,
            self._output_dir, self.x_axis, self._heading_x_axis,
            headings=headings,
        )

    # ------------------------------------------------------------------
    # Public API — correlation & report sections
    # ------------------------------------------------------------------

    def plot_pairwise_correlation_heatmap(self, report) -> Path:
        """Heatmap of mean pairwise magnitude correlation across all DOFs."""
        return _plot_pairwise_correlation_heatmap(
            report, self._output_dir,
        )

    def build_hydro_coefficients_html(self, report) -> str:
        """Build 6x6 added-mass and damping correlation matrices."""
        return _build_hydro_coefficients_html(report)

    @staticmethod
    def build_coupling_heatmap_html(
        output_dir: Path,
        am_corr: list[list[float]],
        damp_corr: list[list[float]],
        body_i_name: str,
        body_j_name: str,
    ) -> Path:
        """Render 6x6 correlation heatmaps for coupling matrices."""
        return _build_coupling_heatmap_html(
            output_dir, am_corr, damp_corr, body_i_name, body_j_name,
        )

    def build_raw_rao_data_html(
        self,
        headings: Optional[List[float]] = None,
    ) -> str:
        """Build collapsible tables of raw RAO data per DOF."""
        return _build_raw_rao_data_html(
            self._solver_results, self._solver_names, headings,
        )

    def build_dof_report_sections(
        self,
        report,
        headings: Optional[List[float]] = None,
    ) -> str:
        """Build per-DOF two-column HTML sections (text left, plot right)."""
        return _build_dof_report_sections(
            report,
            self._solver_results, self._solver_names,
            self.x_axis, self._heading_x_axis,
            headings=headings,
        )

    # ------------------------------------------------------------------
    # Public API — input comparison & mesh schematic
    # ------------------------------------------------------------------

    def build_input_comparison_html(self) -> str:
        """Render an HTML table comparing solver input parameters."""
        return _build_input_comparison_html(
            self._solver_results, self._solver_names,
            self._solver_metadata,
        )

    def build_input_files_html(self) -> str:
        """Render scrollable input file previews for each solver."""
        return _build_input_files_html(
            self._solver_names, self._solver_metadata,
        )

    def build_mesh_schematic_html(self) -> str:
        """Build 3D mesh schematic section for the benchmark report."""
        return _build_mesh_schematic_html(
            self._solver_names, self._solver_metadata,
        )

    def build_semantic_equivalence_html(self) -> str:
        """Render semantic equivalence comparison between solver inputs."""
        from digitalmodel.hydrodynamics.diffraction.benchmark_input_reports import (
            build_semantic_equivalence_html as _build_sem,
        )
        return _build_sem(
            self._solver_names, self._solver_metadata,
        )

    # ------------------------------------------------------------------
    # Internal helpers (kept as thin wrappers for backward compat)
    # ------------------------------------------------------------------

    def _add_solver_traces(self, fig, dof, headings, row, col,
                           value_type, show_legend,
                           heading_indices=None):
        """Delegate to benchmark_rao_plots.add_solver_traces."""
        _add_solver_traces(
            fig, dof, headings, row, col, value_type, show_legend,
            solver_results=self._solver_results,
            solver_names=self._solver_names,
            x_axis=self.x_axis,
            heading_x_axis=self._heading_x_axis,
            heading_indices=heading_indices,
        )

    def _get_x_values(self, component):
        return _get_x_values(component, self.x_axis)

    def _x_axis_label(self):
        return _x_axis_label(self.x_axis, self._heading_x_axis)

    def _get_heading_indices(self, component, headings):
        return _get_heading_indices(component, headings)

    def _get_significant_heading_indices(self, dof, headings,
                                         threshold=0.01):
        return _get_significant_heading_indices(
            dof, self._solver_results, self._solver_names,
            headings, threshold,
        )

    @staticmethod
    def _get_solver_style(solver_idx):
        return _get_solver_style(solver_idx)

    @staticmethod
    def _apply_layout(fig, title):
        _apply_layout(fig, title)

    def _compute_amplitude_summary(self, headings):
        return _compute_amplitude_summary(
            self._solver_results, self._solver_names, headings,
        )

    def _compute_phase_summary(self, headings):
        return _compute_phase_summary(
            self._solver_results, self._solver_names, headings,
        )

    def _render_html_with_table(self, fig, summary, filename, mode):
        return _render_html_with_table(
            fig, summary, filename, mode, self._output_dir,
        )

    @staticmethod
    def _build_summary_table(sections, mode):
        return _build_summary_table(sections, mode)

    @staticmethod
    def _render_6x6_matrix(corr_dict, labels):
        return _render_6x6_matrix(corr_dict, labels)

    @staticmethod
    def _build_panel_scatter_html(panel_geometry_data, title="Panel Geometry",
                                   height=400):
        from digitalmodel.hydrodynamics.diffraction.benchmark_input_reports import (
            _build_panel_scatter_html as _scatter,
        )
        return _scatter(panel_geometry_data, title, height)

    @staticmethod
    def _build_panel_mesh3d_html(panel_geometry_data, fdf_path=None,
                                  title="Panel Mesh & Free Surface Zone",
                                  height=560):
        from digitalmodel.hydrodynamics.diffraction.benchmark_input_reports import (
            _build_panel_mesh3d_html as _mesh3d,
        )
        return _mesh3d(panel_geometry_data, fdf_path, title, height)

    @staticmethod
    def _generate_dof_observations(dof_name, consensus, mag_corr,
                                    phase_corr, max_mag_diff,
                                    max_phase_diff, unit,
                                    magnitude_at_max_phase_diff=0.0,
                                    peak_magnitude=0.0,
                                    phase_diff_at_visible_heading=True):
        return generate_dof_observations(
            dof_name, consensus, mag_corr, phase_corr,
            max_mag_diff, max_phase_diff, unit,
            magnitude_at_max_phase_diff=magnitude_at_max_phase_diff,
            peak_magnitude=peak_magnitude,
            phase_diff_at_visible_heading=phase_diff_at_visible_heading,
        )

    def _save_figure(self, fig, filename):
        return _save_figure(fig, filename, self._output_dir)
