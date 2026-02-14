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
            result.report_json_path = self._generate_json_report(report)

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

    def _generate_json_report(self, report: BenchmarkReport) -> Path:
        """Serialize BenchmarkReport to a JSON file."""
        output_dir = Path(self.config.output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        json_path = output_dir / "benchmark_report.json"

        data = self._report_to_dict(report)

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
    def _report_to_dict(report: BenchmarkReport) -> Dict[str, Any]:
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

            pairwise_data[pair_key] = {
                "solver_a": pr.solver_a,
                "solver_b": pr.solver_b,
                "overall_agreement": pr.overall_agreement,
                "rao_comparisons": rao_dict,
                "added_mass_correlations": am_corrs,
                "damping_correlations": damp_corrs,
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

        return {
            "vessel_name": report.vessel_name,
            "solver_names": report.solver_names,
            "comparison_date": report.comparison_date,
            "overall_consensus": report.overall_consensus,
            "pairwise_results": pairwise_data,
            "consensus_by_dof": consensus_data,
            "notes": report.notes,
        }

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

        Layout flows top-to-bottom in sections:
        1. Header (vessel, date, overall consensus)
        2. Input comparison table (solver names as column headers)
        3. Consensus summary table
        4. Per-DOF sections (text/conclusions left, plot right)
        5. Additional plot links and notes
        """
        output_dir = Path(self.config.output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        html_path = output_dir / "benchmark_report.html"

        now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        # Consensus summary table
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

        # Additional plot links
        plot_links = ""
        for p in plot_paths:
            try:
                rel = p.relative_to(output_dir)
            except ValueError:
                rel = p
            plot_links += (
                f'<li><a href="{rel}">{p.name}</a></li>\n'
            )

        notes_items = "\n".join(
            f"<li>{note}</li>" for note in report.notes
        )

        # Build input comparison, input files, mesh schematic, per-DOF
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

        html = f"""\
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Benchmark Report - {report.vessel_name}</title>
<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
<style>
  * {{ box-sizing: border-box; }}
  body {{
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI',
                 Roboto, Arial, sans-serif;
    margin: 0; padding: 0; color: #333; background: #f8f9fa;
    font-size: 14px; line-height: 1.5;
  }}
  .container {{ max-width: 1400px; margin: 0 auto; padding: 1.5em 2em; }}

  /* Header */
  .report-header {{
    background: #2c3e50; color: #fff; padding: 1.2em 2em;
    margin-bottom: 1.5em; border-radius: 6px;
  }}
  .report-header h1 {{ margin: 0 0 0.3em; font-size: 1.6em; }}
  .report-header .meta {{ font-size: 0.9em; opacity: 0.85; }}
  .report-header .consensus-overall {{
    display: inline-block; padding: 4px 12px; border-radius: 4px;
    font-weight: 700; margin-left: 1em; font-size: 0.85em;
  }}

  /* Section cards */
  .section {{ background: #fff; border-radius: 6px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.08);
    margin-bottom: 1.5em; padding: 1.2em 1.5em;
  }}
  .section h2 {{ margin: 0 0 0.8em; font-size: 1.2em; color: #2c3e50;
    border-bottom: 2px solid #3498db; padding-bottom: 0.3em; }}

  /* Tables */
  table {{ border-collapse: collapse; margin: 0.5em 0; font-size: 0.85em;
    width: 100%; }}
  th, td {{ border: 1px solid #ddd; padding: 0.45em 0.7em;
    text-align: left; }}
  th {{ background: #34495e; color: #fff; font-weight: 600;
    font-size: 0.85em; text-transform: uppercase; letter-spacing: 0.3px; }}
  tbody tr:nth-child(even) {{ background: #f8f9fa; }}
  tbody tr:nth-child(odd) {{ background: #fff; }}
  tbody tr:hover {{ background: #ebf5fb; }}
  td {{ vertical-align: top; }}

  /* Input comparison table */
  .input-table .param-label {{ font-weight: 600; }}
  .section-row td {{
    background: #2c3e50 !important; color: #fff;
    font-weight: 700; font-size: 0.8em; text-transform: uppercase;
    letter-spacing: 0.5px; padding: 0.5em 0.7em;
  }}

  /* Input file viewer */
  .file-viewer {{ margin-bottom: 1.5em; }}
  .file-viewer-header {{
    display: flex; justify-content: space-between; align-items: center;
    background: #34495e; color: #fff; padding: 0.5em 1em;
    border-radius: 4px 4px 0 0; font-size: 0.85em;
  }}
  .file-viewer-header .file-path {{
    font-family: 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
    font-size: 0.9em; word-break: break-all;
  }}
  .file-viewer-header .solver-label {{
    font-weight: 700; margin-right: 0.8em; white-space: nowrap;
  }}
  .file-viewer-header button {{
    background: #3498db; color: #fff; border: none; padding: 4px 12px;
    border-radius: 3px; cursor: pointer; font-size: 0.85em;
    white-space: nowrap;
  }}
  .file-viewer-header button:hover {{ background: #2980b9; }}
  .file-content {{
    max-height: 400px; overflow-y: auto; overflow-x: auto;
    border: 1px solid #ddd; border-top: none;
    border-radius: 0 0 4px 4px; background: #fafafa; margin: 0;
  }}
  .file-content pre {{
    margin: 0; padding: 0.8em 1em; font-size: 12px; line-height: 1.5;
    font-family: 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
    counter-reset: line;
  }}
  .file-content pre .line {{
    display: block;
  }}
  .file-content pre .line::before {{
    counter-increment: line;
    content: counter(line);
    display: inline-block; width: 3.5em; text-align: right;
    margin-right: 1em; color: #999; font-size: 0.85em;
    border-right: 1px solid #ddd; padding-right: 0.5em;
    -webkit-user-select: none; user-select: none;
  }}

  /* DOF sections: 2-column grid */
  .dof-section {{ border-top: 2px solid #ecf0f1; padding-top: 1em;
    margin-top: 1em; }}
  .dof-section:first-child {{ border-top: none; margin-top: 0; }}
  .dof-title {{ font-size: 1.1em; color: #2c3e50; margin: 0 0 0.6em; }}
  .dof-grid {{
    display: grid; grid-template-columns: 45% 55%;
    gap: 1em; align-items: start;
  }}
  .dof-text {{ font-size: 0.85em; }}
  .dof-plot {{ min-height: 340px; }}

  /* Consensus badges */
  .consensus-badge {{
    display: inline-block; padding: 3px 10px; border-radius: 3px;
    color: #fff; font-size: 0.8em; font-weight: 700;
    margin-bottom: 0.5em;
  }}

  /* Numeric font */
  .mono {{ font-family: 'SF Mono', 'Cascadia Code', 'Consolas',
    'Fira Code', monospace; }}

  /* Stats table */
  .stats-table {{ width: 100%; margin-bottom: 0.6em; }}
  .stats-table td:last-child {{ text-align: right;
    font-family: 'SF Mono', 'Cascadia Code', 'Consolas', monospace; }}

  /* Solver comparison tables */
  .solver-table {{ width: 100%; margin-bottom: 0.5em; }}
  .solver-table th {{ font-size: 0.75em; text-align: center;
    padding: 0.3em 0.4em; }}
  .solver-table td {{ text-align: right;
    font-family: 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
    font-size: 0.8em; padding: 0.25em 0.4em; }}
  .solver-table td:first-child {{ text-align: left; font-family: inherit;
    font-weight: 600; }}

  .dof-text h4 {{ margin: 0.7em 0 0.3em; font-size: 0.9em;
    color: #2c3e50; border-bottom: 1px solid #ddd;
    padding-bottom: 0.15em; }}
  .observations p {{ margin: 0.3em 0; line-height: 1.4; }}
  .skipped-note {{
    font-size: 0.8em; color: #888; font-style: italic;
    margin-top: 0.5em; padding: 0.3em 0.5em;
    background: #fef9e7; border-left: 3px solid #f0c674;
    border-radius: 2px;
  }}

  /* Links section */
  .plot-links {{ column-count: 2; font-size: 0.85em; }}
  .plot-links li {{ margin-bottom: 0.3em; }}

  /* Responsive */
  @media (max-width: 900px) {{
    .dof-grid {{ grid-template-columns: 1fr; }}
  }}
</style>
</head>
<body>
<div class="container">

  <!-- Section 1: Header -->
  <div class="report-header">
    <h1>Benchmark Report</h1>
    <div class="meta">
      <strong>Vessel:</strong> {report.vessel_name} &nbsp;|&nbsp;
      <strong>Date:</strong> {now} &nbsp;|&nbsp;
      <strong>Solvers:</strong> {', '.join(report.solver_names)}
      <span class="consensus-overall"
            style="background:{
                '#27ae60' if report.overall_consensus == 'FULL'
                else '#f39c12' if report.overall_consensus == 'SPLIT'
                else '#e74c3c'
            };">
        {report.overall_consensus}
      </span>
    </div>
    <div class="meta" style="margin-top:0.4em;">
      <strong>Data:</strong>
      <a href="hydro_data.yml" style="color:#85c1e9;">hydro_data.yml</a> &nbsp;|&nbsp;
      <a href="benchmark_report.json" style="color:#85c1e9;">benchmark_report.json</a>
    </div>
  </div>

  <!-- Section 2: Input Comparison -->
  <div class="section">
    {input_comparison_html}
  </div>

  <!-- Section 2.1: Input Files -->
  {f'<div class="section">{input_files_html}</div>' if input_files_html else ''}

  <!-- Section 2.5: Mesh Schematic -->
  {f'<div class="section">{mesh_schematic_html}</div>' if mesh_schematic_html else ''}

  <!-- Section 3: Consensus Summary -->
  <div class="section">
    <h2>Consensus Summary</h2>
    <table style="width:100%;max-width:700px;">
      <tr>
        <th>DOF</th><th>Consensus</th>
        <th>Mean Correlation</th><th>Outlier</th>
      </tr>
      {consensus_rows}
    </table>
  </div>

  <!-- Section 3.5: Hydrodynamic Coefficients -->
  {f'<div class="section">{hydro_coefficients_html}</div>' if hydro_coefficients_html else ''}

  <!-- Section 4: Per-DOF Analysis -->
  <div class="section">
    <h2>Per-DOF Analysis</h2>
    {dof_sections_html}
  </div>

  <!-- Section 4.5: Raw RAO Data -->
  {f'<div class="section">{raw_rao_data_html}</div>' if raw_rao_data_html else ''}

  <!-- Section 5: Additional Plots -->
  <div class="section">
    <h2>Full Overlay Plots</h2>
    <ul class="plot-links">
      {plot_links}
    </ul>
  </div>

  <!-- Section 6: Notes -->
  <div class="section">
    <h2>Notes</h2>
    <ul>{notes_items}</ul>
  </div>

</div>
</body>
</html>"""

        with open(html_path, "w", encoding="utf-8") as fh:
            fh.write(html)

        return html_path


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
