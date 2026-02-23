#!/usr/bin/env python3
"""
Benchmark Runner - End-to-end orchestration for N-way solver comparison.

ABOUTME: Ties together MultiSolverComparator and BenchmarkPlotter to produce
a complete benchmark report with JSON export, HTML summary, and plots.

Provides:
- BenchmarkConfig: Pydantic configuration model
- BenchmarkRunResult: Dataclass holding orchestration outputs
- BenchmarkRunner: Main orchestrator class
- run_benchmark: Convenience function for quick comparisons
- benchmark_solvers_cmd: Click CLI entry point

Version: 1.0.0
Status: Benchmark orchestration
"""

from __future__ import annotations

import json
import math
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional

import click
import numpy as np
import yaml
from pydantic import BaseModel

from digitalmodel.hydrodynamics.diffraction.benchmark_plotter import (
    BenchmarkPlotter,
)
from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
    BenchmarkReport,
    MultiSolverComparator,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
)


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------


class SolverType(str, Enum):
    """Supported diffraction solver types."""

    AQWA = "aqwa"
    ORCAWAVE = "orcawave"
    BEMROSETTA = "bemrosetta"


# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------


class BenchmarkConfig(BaseModel):
    """Pydantic configuration for a benchmark run."""

    spec_path: Path = Path(".")
    solvers: list[SolverType] = [
        SolverType.AQWA,
        SolverType.ORCAWAVE,
        SolverType.BEMROSETTA,
    ]
    output_dir: Path = Path("benchmark_output")
    dry_run: bool = False
    tolerance: float = 0.05
    x_axis: str = "period"
    headings: Optional[list[float]] = None
    timeout_seconds: int = 7200
    reference_solver: Optional[str] = None

    # UI / Reporting overrides
    report_title: Optional[str] = None
    report_subtitle: Optional[str] = None
    navigation_html: Optional[str] = None


# ---------------------------------------------------------------------------
# Run result
# ---------------------------------------------------------------------------


@dataclass
class BenchmarkRunResult:
    """Container for all outputs produced by a benchmark run."""

    config: Optional[BenchmarkConfig] = None
    solver_results: Dict[str, DiffractionResults] = field(
        default_factory=dict,
    )
    report: Optional[BenchmarkReport] = None
    plot_paths: list[Path] = field(default_factory=list)
    report_json_path: Optional[Path] = None
    hydro_data_yaml_path: Optional[Path] = None
    report_html_path: Optional[Path] = None
    error_message: Optional[str] = None
    success: bool = False


# ---------------------------------------------------------------------------
# Runner
# ---------------------------------------------------------------------------


class BenchmarkRunner:
    """Orchestrates comparison, plotting, and report generation.

    Args:
        config: BenchmarkConfig with run parameters.
    """

    def __init__(self, config: BenchmarkConfig) -> None:
        self.config = config

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def run_from_results(
        self,
        solver_results: Dict[str, DiffractionResults],
        solver_metadata: Optional[Dict[str, Dict]] = None,
    ) -> BenchmarkRunResult:
        """Run benchmark from pre-computed solver results.

        Args:
            solver_results: Mapping of solver name to DiffractionResults.
            solver_metadata: Optional per-solver metadata dicts for the
                input comparison table (geometry, mass, mesh info, etc.).

        Returns:
            BenchmarkRunResult with all outputs populated.
        """
        result = BenchmarkRunResult(
            config=self.config,
            solver_results=solver_results,
        )

        try:
            output_dir = Path(self.config.output_dir)
            output_dir.mkdir(parents=True, exist_ok=True)

            # 1. Compare solvers
            report = self._compare(solver_results)
            result.report = report

            # 2. Generate plots (skip in dry_run mode)
            if not self.config.dry_run:
                plot_paths = self._generate_plots(solver_results, report)
                result.plot_paths = plot_paths

            # 3. Generate JSON report file
            result.report_json_path = self._generate_json_report(
                report, solver_metadata=solver_metadata,
            )

            # 3b. Generate hydro data YAML
            result.hydro_data_yaml_path = self._generate_hydro_data_yaml(
                solver_results, report,
            )

            # 4. Generate HTML report file
            result.report_html_path = self._generate_html_report(
                report,
                result.plot_paths,
                solver_results=solver_results,
                solver_metadata=solver_metadata,
            )

            result.success = True

        except Exception as exc:
            result.error_message = str(exc)
            result.success = False

        return result

    # ------------------------------------------------------------------
    # Internal: comparison
    # ------------------------------------------------------------------

    def _compare(
        self,
        solver_results: Dict[str, DiffractionResults],
    ) -> BenchmarkReport:
        """Run multi-solver comparison and return a BenchmarkReport."""
        comparator = MultiSolverComparator(
            solver_results,
            tolerance=self.config.tolerance,
        )
        return comparator.generate_report()

    # ------------------------------------------------------------------
    # Internal: plotting
    # ------------------------------------------------------------------

    def _generate_plots(
        self,
        solver_results: Dict[str, DiffractionResults],
        report: BenchmarkReport,
    ) -> list[Path]:
        """Generate all benchmark plots and return their paths."""
        plotter = BenchmarkPlotter(
            solver_results,
            output_dir=self.config.output_dir,
            x_axis=self.config.x_axis,
        )

        paths = plotter.plot_all(headings=self.config.headings)

        # Pairwise correlation heatmap
        heatmap_path = plotter.plot_pairwise_correlation_heatmap(report)
        paths.append(heatmap_path)

        # Difference plot when a reference solver is specified
        if self.config.reference_solver:
            diff_path = plotter.plot_difference(
                reference_solver=self.config.reference_solver,
                headings=self.config.headings,
            )
            paths.append(diff_path)

        return paths

    # ------------------------------------------------------------------
    # Internal: JSON report
    # ------------------------------------------------------------------

    def _generate_json_report(
        self,
        report: BenchmarkReport,
        solver_metadata: Optional[Dict[str, Dict]] = None,
    ) -> Path:
        """Serialize BenchmarkReport to a JSON file."""
        output_dir = Path(self.config.output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        json_path = output_dir / "benchmark_report.json"

        data = self._report_to_dict(report, solver_metadata=solver_metadata)

        with open(json_path, "w", encoding="utf-8") as fh:
            json.dump(data, fh, indent=2, default=_json_default)

        return json_path

    def _generate_hydro_data_yaml(
        self,
        solver_results: Dict[str, DiffractionResults],
        report: BenchmarkReport,
    ) -> Path:
        """Serialize raw hydrodynamic data to a human/agent-readable YAML.

        Exports per-solver RAO arrays (magnitude + phase), added mass and
        radiation damping 6x6 matrices at every frequency, plus the
        comparison correlation metrics.  This is the canonical structured
        data file for each benchmark case.
        """
        output_dir = Path(self.config.output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        yaml_path = output_dir / "hydro_data.yml"

        dof_names = ["surge", "sway", "heave", "roll", "pitch", "yaw"]

        # -- helpers ---------------------------------------------------------
        def _safe_float(v: float) -> object:
            """Convert numpy/inf to YAML-safe value."""
            if isinstance(v, (np.floating, np.integer)):
                v = float(v)
            if math.isinf(v):
                return -1.0  # convention: -1 = infinite
            if math.isnan(v):
                return None
            return round(v, 8)

        def _round_list(arr: np.ndarray, decimals: int = 6) -> list:
            """Convert ndarray to rounded nested list."""
            if arr.ndim == 1:
                return [round(float(x), decimals) for x in arr]
            return [_round_list(row, decimals) for row in arr]

        # -- build grid section ----------------------------------------------
        first_result = next(iter(solver_results.values()))
        freq_data = first_result.raos.surge.frequencies
        head_data = first_result.raos.surge.headings

        grid: Dict[str, Any] = {
            "frequencies_rad_s": _round_list(freq_data.values, 6),
            "periods_s": _round_list(freq_data.periods, 6),
            "headings_deg": _round_list(head_data.values, 2),
        }

        # -- build per-solver section ----------------------------------------
        solvers_data: Dict[str, Any] = {}
        for solver_name, dr in solver_results.items():
            raos_dict: Dict[str, Any] = {}
            for dof_name in dof_names:
                comp = getattr(dr.raos, dof_name)
                raos_dict[dof_name] = {
                    "unit": comp.unit,
                    "magnitude": _round_list(comp.magnitude, 6),
                    "phase_deg": _round_list(comp.phase, 4),
                }

            # Added mass: list of 6x6 matrices indexed by frequency
            am_matrices = []
            for hm in dr.added_mass.matrices:
                am_matrices.append(_round_list(hm.matrix, 6))

            # Damping: same structure
            damp_matrices = []
            for hm in dr.damping.matrices:
                damp_matrices.append(_round_list(hm.matrix, 6))

            solvers_data[solver_name] = {
                "vessel_name": dr.vessel_name,
                "analysis_tool": dr.analysis_tool,
                "water_depth": _safe_float(dr.water_depth),
                "phase_convention": dr.phase_convention,
                "unit_system": dr.unit_system,
                "raos": raos_dict,
                "added_mass": am_matrices,
                "damping": damp_matrices,
            }

        # -- build comparison section ----------------------------------------
        comparison: Dict[str, Any] = {
            "overall_consensus": report.overall_consensus,
        }

        for pair_key, pr in report.pairwise_results.items():
            rao_corrs: Dict[str, Any] = {}
            for dof_name, comp in pr.rao_comparisons.items():
                rao_corrs[dof_name] = {
                    "magnitude_r": round(
                        float(comp.magnitude_stats.correlation), 8,
                    ),
                    "phase_r": round(
                        float(comp.phase_stats.correlation), 8,
                    ),
                    "magnitude_rms": round(
                        float(comp.magnitude_stats.rms_error), 8,
                    ),
                    "max_magnitude_diff": round(
                        float(comp.max_magnitude_diff), 8,
                    ),
                    "max_phase_diff_deg": round(
                        float(comp.max_phase_diff), 4,
                    ),
                }

            am_corrs = {
                f"{k[0]},{k[1]}": round(float(v), 8)
                for k, v in pr.added_mass_correlations.items()
            }
            damp_corrs = {
                f"{k[0]},{k[1]}": round(float(v), 8)
                for k, v in pr.damping_correlations.items()
            }

            comparison[pair_key] = {
                "overall_agreement": pr.overall_agreement,
                "rao_correlations": rao_corrs,
                "added_mass_correlations": am_corrs,
                "damping_correlations": damp_corrs,
            }

        consensus_by_dof: Dict[str, Any] = {}
        for dof_key, cm in report.consensus_by_dof.items():
            consensus_by_dof[dof_key] = {
                "level": cm.consensus_level,
                "mean_correlation": round(
                    float(cm.mean_pairwise_correlation), 8,
                ),
            }
        comparison["consensus_by_dof"] = consensus_by_dof

        # -- assemble top-level document -------------------------------------
        doc: Dict[str, Any] = {
            "metadata": {
                "vessel_name": report.vessel_name,
                "generated": datetime.now().strftime("%Y-%m-%dT%H:%M:%S"),
                "solver_names": report.solver_names,
            },
            "grid": grid,
            "solvers": solvers_data,
            "comparison": comparison,
        }

        with open(yaml_path, "w", encoding="utf-8") as fh:
            yaml.dump(
                doc,
                fh,
                default_flow_style=False,
                sort_keys=False,
                allow_unicode=True,
                width=120,
            )

        return yaml_path

    @staticmethod
    def _report_to_dict(
        self,
        report: BenchmarkReport,
        solver_metadata: Optional[Dict[str, Dict]] = None,
    ) -> Dict[str, Any]:
        """Convert a BenchmarkReport dataclass to a JSON-safe dict."""
        pairwise_data: Dict[str, Any] = {}
        for pair_key, pr in report.pairwise_results.items():
            rao_dict: Dict[str, Any] = {}
            for dof_name, comp in pr.rao_comparisons.items():
                rao_dict[dof_name] = {
                    "magnitude_correlation": float(
                        comp.magnitude_stats.correlation,
                    ),
                    "magnitude_rms_error": float(
                        comp.magnitude_stats.rms_error,
                    ),
                    "phase_correlation": float(
                        comp.phase_stats.correlation,
                    ),
                    "max_magnitude_diff": float(comp.max_magnitude_diff),
                    "max_phase_diff": float(comp.max_phase_diff),
                }

            am_corrs = {
                f"{k[0]},{k[1]}": float(v)
                for k, v in pr.added_mass_correlations.items()
            }
            damp_corrs = {
                f"{k[0]},{k[1]}": float(v)
                for k, v in pr.damping_correlations.items()
            }

            hc_dict = None
            if pr.hydrostatic_comparison:
                hc = pr.hydrostatic_comparison
                hc_dict = {
                    "displacement_volume_diff": float(hc.displacement_volume_diff),
                    "mass_diff": float(hc.mass_diff),
                    "cog_diff": [float(v) for v in hc.cog_diff],
                    "cob_diff": [float(v) for v in hc.cob_diff],
                    "waterplane_area_diff": float(hc.waterplane_area_diff),
                    "stiffness_matrix_correlation": float(
                        hc.stiffness_matrix_correlation,
                    ),
                }

            pairwise_data[pair_key] = {
                "solver_a": pr.solver_a,
                "solver_b": pr.solver_b,
                "overall_agreement": pr.overall_agreement,
                "rao_comparisons": rao_dict,
                "added_mass_correlations": am_corrs,
                "damping_correlations": damp_corrs,
                "hydrostatic_comparison": hc_dict,
            }

        consensus_data: Dict[str, Any] = {}
        for dof_key, cm in report.consensus_by_dof.items():
            consensus_data[dof_key] = {
                "consensus_level": cm.consensus_level,
                "mean_pairwise_correlation": float(
                    cm.mean_pairwise_correlation,
                ),
                "outlier_solver": cm.outlier_solver,
                "agreement_pairs": [
                    list(pair) for pair in cm.agreement_pairs
                ],
            }

        data = {
            "vessel_name": report.vessel_name,
            "solver_names": report.solver_names,
            "comparison_date": report.comparison_date,
            "overall_consensus": report.overall_consensus,
            "pairwise_results": pairwise_data,
            "consensus_by_dof": consensus_data,
            "notes": report.notes,
        }

        if solver_metadata:
            # Extract semantic equivalence if present in any of the metadata dicts
            for meta in solver_metadata.values():
                if "_semantic_equivalence" in meta:
                    data["semantic_equivalence"] = meta[
                        "_semantic_equivalence"
                    ]
                    break

        return data

    # ------------------------------------------------------------------
    # Internal: HTML report
    # ------------------------------------------------------------------

    def _generate_html_report(
        self,
        report: BenchmarkReport,
        plot_paths: list[Path],
        solver_results: Optional[Dict[str, DiffractionResults]] = None,
        solver_metadata: Optional[Dict[str, Dict]] = None,
    ) -> Path:
        """Generate a single-page HTML benchmark report.

        Delegates to ``generate_diffraction_report`` from
        ``report_generator``, interleaving multi-solver benchmark sections
        at their natural positions in the physics causal chain.
        """
        from digitalmodel.hydrodynamics.diffraction.report_generator import (
            build_report_data_from_solver_results,
            generate_diffraction_report,
        )

        output_dir = Path(self.config.output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        html_path = output_dir / "benchmark_report.html"

        # --- 1. Build DiffractionReportData ---------------------------------
        # Look for an .owr path in solver_metadata
        owr_path: Optional[Path] = None
        if solver_metadata:
            for meta in solver_metadata.values():
                for key in ("results_file", "owr_path"):
                    candidate = meta.get(key)
                    if candidate and str(candidate).endswith(".owr"):
                        owr_path = Path(candidate)
                        break
                if owr_path:
                    break

        if solver_results:
            report_data = build_report_data_from_solver_results(
                solver_results,
                owr_path=owr_path,
                vessel_name=report.vessel_name,
            )
        else:
            from digitalmodel.hydrodynamics.diffraction.report_generator import (
                DiffractionReportData,
            )
            report_data = DiffractionReportData(
                vessel_name=report.vessel_name,
                solver_names=report.solver_names,
            )

        # --- 1b. Apply config overrides -------------------------------------
        if self.config.report_title:
            report_data.report_title = self.config.report_title
        if self.config.report_subtitle:
            report_data.report_subtitle = self.config.report_subtitle
        if self.config.navigation_html:
            if report_data.benchmark_html_sections is None:
                report_data.benchmark_html_sections = {}
            report_data.benchmark_html_sections["navigation"] = self.config.navigation_html

        # --- 2. Build benchmark sections via BenchmarkPlotter ---------------
        input_comparison_html = ""
        input_files_html = ""
        mesh_schematic_html = ""
        hydro_coefficients_html = ""
        dof_sections_html = ""
        raw_rao_data_html = ""
        if solver_results:
            plotter = BenchmarkPlotter(
                solver_results,
                output_dir=output_dir,
                x_axis=self.config.x_axis,
                solver_metadata=solver_metadata,
            )
            input_comparison_html = plotter.build_input_comparison_html()
            input_files_html = plotter.build_input_files_html()
            mesh_schematic_html = plotter.build_mesh_schematic_html()
            hydro_coefficients_html = plotter.build_hydro_coefficients_html(
                report,
            )
            dof_sections_html = plotter.build_dof_report_sections(
                report,
                headings=self.config.headings,
            )
            raw_rao_data_html = plotter.build_raw_rao_data_html(
                headings=self.config.headings,
            )

        # --- 3. Build consensus summary table -------------------------------
        consensus_rows = ""
        for dof_key, cm in report.consensus_by_dof.items():
            color = {
                "FULL": "#27ae60",
                "MAJORITY": "#f39c12",
                "NO_CONSENSUS": "#e74c3c",
            }.get(cm.consensus_level, "#999")
            consensus_rows += (
                f"<tr>"
                f"<td><a href='#dof-{dof_key.lower()}'>{dof_key}</a></td>"
                f"<td style='color:{color};font-weight:600;'>"
                f"{cm.consensus_level}</td>"
                f"<td>{cm.mean_pairwise_correlation:.4f}</td>"
                f"<td>{cm.outlier_solver or '-'}</td>"
                f"</tr>\n"
            )
        consensus_html = (
            '<h2>Consensus Summary</h2>'
            '<table style="width:100%;max-width:700px;">'
            "<tr><th>DOF</th><th>Consensus</th>"
            "<th>Mean Correlation</th><th>Outlier</th></tr>"
            f"{consensus_rows}</table>"
        )

        # --- 4. Build overlay plot links ------------------------------------
        plot_links = ""
        for p in plot_paths:
            try:
                rel = p.relative_to(output_dir)
            except ValueError:
                rel = p
            plot_links += f'<li><a href="{rel}">{p.name}</a></li>\n'
        overlay_html = ""
        if plot_links:
            overlay_html = (
                '<h2>Full Overlay Plots</h2>'
                f'<ul class="plot-links">{plot_links}</ul>'
            )

        # --- 5. Build executive summary metrics --------------------------------
        sem_data = None
        if solver_metadata:
            for meta in solver_metadata.values():
                if "_semantic_equivalence" in meta:
                    sem_data = meta["_semantic_equivalence"]
                    break
        benchmark_executive = self._build_benchmark_executive_html(
            report, semantic=sem_data,
        )

        # --- 6. Pack into report_data.benchmark_html_sections ---------------
        # Preserve any keys set earlier (e.g. "navigation" from config)
        existing = report_data.benchmark_html_sections or {}
        report_data.benchmark_html_sections = {
            **existing,
            "benchmark_executive": benchmark_executive,
            "input_comparison": input_comparison_html,
            "input_files": input_files_html,
            "mesh_schematic": mesh_schematic_html,
            "consensus_summary": consensus_html,
            "hydro_coefficients": hydro_coefficients_html,
            "dof_sections": (
                f'<h2>Per-DOF Analysis</h2>{dof_sections_html}'
            ),
            "raw_rao_data": raw_rao_data_html,
            "overlay_plots": overlay_html,
        }

        # --- 7. Set metadata from BenchmarkReport ---------------------------
        report_data.solver_names = report.solver_names
        report_data.notes = list(report.notes)

        # --- 8. Generate via unified template --------------------------------
        generate_diffraction_report(report_data, html_path)
        return html_path


    # ------------------------------------------------------------------
    # Internal: benchmark executive summary HTML
    # ------------------------------------------------------------------

    @staticmethod
    def _build_benchmark_executive_html(
        report: BenchmarkReport,
        semantic: Optional[Dict[str, Any]] = None,
    ) -> str:
        """Build HTML for the executive summary benchmark metrics.

        Returns a compact overview with verdict badge, per-DOF RAO table,
        hydrodynamic coefficient summary, and semantic equivalence line.
        """
        if not report.pairwise_results:
            return ""

        # Use first pairwise result (typical: 2 solvers → 1 pair)
        pw_key = next(iter(report.pairwise_results))
        pw = report.pairwise_results[pw_key]

        # --- 1. Verdict banner -------------------------------------------
        agreement = pw.overall_agreement
        badge_colors = {
            "EXCELLENT": ("#27ae60", "#eafaf1"),
            "GOOD": ("#2980b9", "#ebf5fb"),
            "FAIR": ("#f39c12", "#fef9e7"),
            "POOR": ("#e74c3c", "#fdedec"),
        }
        fg, bg = badge_colors.get(agreement, ("#999", "#f5f5f5"))
        solver_label = f"{pw.solver_a} vs {pw.solver_b}"

        verdict_html = (
            f'<div style="background:{bg};border-left:4px solid {fg};'
            f'padding:0.8em 1em;margin:0.8em 0;border-radius:4px;">'
            f'<span style="font-size:1.3em;font-weight:700;color:{fg};">'
            f"{agreement}</span>"
            f'<span style="margin-left:1em;color:#555;">{solver_label}</span>'
            f"</div>"
        )

        # --- 2. RAO comparison table (6 DOFs) ----------------------------
        dof_names = ["surge", "sway", "heave", "roll", "pitch", "yaw"]

        def _color(val: float) -> str:
            if val >= 0.999:
                return "#27ae60"
            if val >= 0.99:
                return "#f39c12"
            return "#e74c3c"

        rao_rows = []
        for dof in dof_names:
            comp = pw.rao_comparisons.get(dof)
            if not comp:
                continue
            mag_r = comp.magnitude_stats.correlation
            phase_r = comp.phase_stats.correlation
            max_mag_diff = comp.max_magnitude_diff
            max_phase_diff = comp.max_phase_diff
            near_zero = max_mag_diff < 1e-6

            signal_label = (
                '<span style="color:#999;">Near-zero</span>'
                if near_zero
                else "Active"
            )
            phase_style = (
                f"color:#999;"
                if near_zero
                else f"color:{_color(phase_r)};font-weight:600;"
            )
            phase_val = f"{phase_r:.4f}" if not near_zero else "-"

            rao_rows.append(
                f"<tr>"
                f"<td>{dof.capitalize()}</td>"
                f"<td style='color:{_color(mag_r)};font-weight:600;'>"
                f"{mag_r:.4f}</td>"
                f"<td style='{phase_style}'>{phase_val}</td>"
                f"<td>{max_mag_diff:.4g}</td>"
                f"<td>{max_phase_diff:.2f}&deg;</td>"
                f"<td>{signal_label}</td>"
                f"</tr>"
            )

        rao_table = (
            '<h3 style="margin-top:1em;">RAO Comparison</h3>'
            '<table style="width:100%;max-width:800px;">'
            "<thead><tr>"
            "<th>DOF</th><th>Mag r</th><th>Phase r</th>"
            "<th>Max |diff|</th><th>Max Phase Diff</th><th>Signal</th>"
            "</tr></thead>"
            f"<tbody>{''.join(rao_rows)}</tbody></table>"
        )

        # --- 3. Hydrodynamic coefficient summary -------------------------
        def _matrix_summary(
            corrs: Dict,
        ) -> tuple:
            """Return (min_diag, min_offdiag, min_overall) from 6x6 corrs."""
            diag_vals = []
            offdiag_vals = []
            for (i, j), v in corrs.items():
                if not np.isfinite(v):
                    continue
                if i == j:
                    diag_vals.append(v)
                else:
                    offdiag_vals.append(v)
            min_diag = min(diag_vals) if diag_vals else float("nan")
            min_offdiag = (
                min(offdiag_vals) if offdiag_vals else float("nan")
            )
            all_vals = diag_vals + offdiag_vals
            min_all = min(all_vals) if all_vals else float("nan")
            return min_diag, min_offdiag, min_all

        hydro_rows = []
        for label, corrs in [
            ("Added Mass", pw.added_mass_correlations),
            ("Radiation Damping", pw.damping_correlations),
        ]:
            if not corrs:
                continue
            min_d, min_od, min_a = _matrix_summary(corrs)

            def _fmt(v: float) -> str:
                if math.isnan(v):
                    return "-"
                c = _color(v)
                return (
                    f"<span style='color:{c};font-weight:600;'>"
                    f"{v:.4f}</span>"
                )

            hydro_rows.append(
                f"<tr><td>{label}</td>"
                f"<td>{_fmt(min_d)}</td>"
                f"<td>{_fmt(min_od)}</td>"
                f"<td>{_fmt(min_a)}</td></tr>"
            )

        hydro_html = ""
        if hydro_rows:
            hydro_html = (
                '<h3 style="margin-top:1em;">'
                "Hydrodynamic Coefficients</h3>"
                '<table style="width:100%;max-width:700px;">'
                "<thead><tr>"
                "<th>Matrix</th><th>Min Diagonal r</th>"
                "<th>Min Off-Diag r</th><th>Min Overall r</th>"
                "</tr></thead>"
                f"<tbody>{''.join(hydro_rows)}</tbody></table>"
                '<p style="font-size:0.85em;color:#777;">'
                'See <a href="#hydro-coefficients">'
                "full 6&times;6 heatmaps</a> below.</p>"
            )

        # --- 4. Semantic equivalence summary --------------------------------
        semantic_html = ""
        if semantic:
            sig = semantic.get("significant_count", 0)
            cos = semantic.get("cosmetic_count", 0)
            conv = semantic.get("convention_count", 0)
            mat = semantic.get("match_count", 0)
            total = mat + cos + conv + sig

            if sig == 0:
                badge = (
                    '<span style="color:#27ae60;font-weight:600;">'
                    "0 significant</span>"
                )
            else:
                badge = (
                    f'<span style="color:#e74c3c;font-weight:600;">'
                    f"{sig} significant</span>"
                )

            # Build diff detail lists by level
            all_diffs = semantic.get("diffs", [])
            sig_diffs = [
                d for d in all_diffs if d["level"] == "significant"
            ]
            cos_diffs = [
                d for d in all_diffs if d["level"] == "cosmetic"
            ]
            conv_diffs = [
                d for d in all_diffs if d["level"] == "convention"
            ]

            # Significant diffs — shown prominently
            diff_detail = ""
            if sig_diffs:
                items = "".join(
                    f"<li><code>{d['key']}</code>: "
                    f"{d['owd']} vs {d['spec']}</li>"
                    for d in sig_diffs
                )
                diff_detail = (
                    f'<ul style="margin:0.3em 0 0 1em;'
                    f'font-size:0.9em;">{items}</ul>'
                )

            # Footnote for cosmetic + convention diffs
            footnote = ""
            footnote_items = []
            for d in cos_diffs:
                footnote_items.append(
                    f"<code>{d['key']}</code>: "
                    f"{d['owd']} vs {d['spec']}"
                )
            for d in conv_diffs:
                footnote_items.append(
                    f"<code>{d['key']}</code>: "
                    f"{d['owd']} vs {d['spec']}"
                )
            if footnote_items:
                fn_list = "".join(
                    f"<li>{item}</li>" for item in footnote_items
                )
                footnote = (
                    '<details style="margin-top:0.5em;'
                    'font-size:0.85em;color:#777;">'
                    f"<summary>{cos} cosmetic + {conv} convention "
                    f"diffs (no solver effect)</summary>"
                    f'<ul style="margin:0.3em 0 0 1em;">'
                    f"{fn_list}</ul>"
                    "<p style='margin:0.3em 0 0 0.5em;"
                    "font-style:italic;'>"
                    "Cosmetic: GUI pens, naming, output flags. "
                    "Convention: same physics, different "
                    "representation (e.g. freq vs period). "
                    "QTF-related settings are dormant when "
                    "QTF is disabled.</p>"
                    "</details>"
                )

            semantic_html = (
                '<h3 style="margin-top:1em;">'
                "Semantic Equivalence</h3>"
                f'<p style="margin:0.3em 0;">'
                f"{mat}/{total} match, {badge}, "
                f"{conv} convention, {cos} cosmetic"
                f"</p>{diff_detail}{footnote}"
            )

        return f"{verdict_html}{rao_table}{hydro_html}{semantic_html}"


# ---------------------------------------------------------------------------
# JSON serialization helper
# ---------------------------------------------------------------------------


def _json_default(obj: object) -> object:
    """Fallback serializer for numpy types."""
    if isinstance(obj, np.integer):
        return int(obj)
    if isinstance(obj, np.floating):
        return float(obj)
    if isinstance(obj, np.ndarray):
        return obj.tolist()
    raise TypeError(f"Object of type {type(obj)} is not serializable")


# ---------------------------------------------------------------------------
# Convenience function
# ---------------------------------------------------------------------------


def run_benchmark(
    solver_results: Dict[str, DiffractionResults],
    output_dir: Path = Path("benchmark_output"),
    tolerance: float = 0.05,
) -> BenchmarkRunResult:
    """Convenience wrapper: configure and run a benchmark in one call.

    Args:
        solver_results: Mapping of solver name to DiffractionResults.
        output_dir: Directory for output artifacts.
        tolerance: Relative tolerance for agreement assessment.

    Returns:
        BenchmarkRunResult with all outputs populated.
    """
    config = BenchmarkConfig(output_dir=output_dir, tolerance=tolerance)
    runner = BenchmarkRunner(config)
    return runner.run_from_results(solver_results)


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


@click.command("benchmark-solvers")
@click.argument("spec_path", type=click.Path(exists=True))
@click.option(
    "--solvers",
    "-s",
    multiple=True,
    default=["aqwa", "orcawave", "bemrosetta"],
)
@click.option(
    "--output",
    "-o",
    type=click.Path(),
    default="benchmark_output",
)
@click.option("--dry-run", is_flag=True)
@click.option("--tolerance", "-t", type=float, default=0.05)
@click.option(
    "--x-axis",
    type=click.Choice(["period", "frequency"]),
    default="period",
)
@click.option(
    "--headings",
    type=str,
    default=None,
    help="Comma-separated heading angles",
)
@click.option(
    "--reference",
    type=str,
    default=None,
    help="Reference solver for difference plots",
)
def benchmark_solvers_cmd(
    spec_path: str,
    solvers: tuple[str, ...],
    output: str,
    dry_run: bool,
    tolerance: float,
    x_axis: str,
    headings: Optional[str],
    reference: Optional[str],
) -> None:
    """Run N-way solver benchmark comparison."""
    heading_list: Optional[List[float]] = None
    if headings:
        heading_list = [float(h.strip()) for h in headings.split(",")]

    config = BenchmarkConfig(
        spec_path=Path(spec_path),
        solvers=[SolverType(s) for s in solvers],
        output_dir=Path(output),
        dry_run=dry_run,
        tolerance=tolerance,
        x_axis=x_axis,
        headings=heading_list,
        reference_solver=reference,
    )
    runner = BenchmarkRunner(config)

    click.echo(
        f"Benchmark configuration: {len(config.solvers)} solvers",
    )
    click.echo(f"Output directory: {config.output_dir}")

    if dry_run:
        click.echo(
            click.style(
                "[DRY RUN] No solvers will be executed.",
                fg="yellow",
            ),
        )

    # Actual solver execution is future work; for now report setup
    click.echo(
        click.style("[OK] Benchmark setup complete.", fg="green"),
    )
