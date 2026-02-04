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
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional

import click
import numpy as np
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
    ) -> BenchmarkRunResult:
        """Run benchmark from pre-computed solver results.

        Args:
            solver_results: Mapping of solver name to DiffractionResults.

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

            # 4. Generate HTML report file
            result.report_html_path = self._generate_html_report(
                report,
                result.plot_paths,
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
    ) -> Path:
        """Generate a simple HTML summary report."""
        output_dir = Path(self.config.output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        html_path = output_dir / "benchmark_report.html"

        now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        solver_rows = "\n".join(
            f"        <tr><td>{name}</td></tr>"
            for name in report.solver_names
        )

        plot_links = ""
        for p in plot_paths:
            try:
                rel = p.relative_to(output_dir)
            except ValueError:
                rel = p
            plot_links += (
                f'        <li><a href="{rel}">{p.name}</a></li>\n'
            )

        consensus_rows = ""
        for dof_key, cm in report.consensus_by_dof.items():
            consensus_rows += (
                f"        <tr>"
                f"<td>{dof_key}</td>"
                f"<td>{cm.consensus_level}</td>"
                f"<td>{cm.mean_pairwise_correlation:.4f}</td>"
                f"<td>{cm.outlier_solver or '-'}</td>"
                f"</tr>\n"
            )

        notes_items = "\n".join(
            f"        <li>{note}</li>" for note in report.notes
        )

        html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <title>Benchmark Report - {report.vessel_name}</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 2em; }}
        table {{ border-collapse: collapse; margin: 1em 0; }}
        th, td {{ border: 1px solid #ccc; padding: 0.5em 1em; text-align: left; }}
        th {{ background: #f0f0f0; }}
        h1, h2 {{ color: #333; }}
    </style>
</head>
<body>
    <h1>Benchmark Report</h1>
    <p><strong>Vessel:</strong> {report.vessel_name}</p>
    <p><strong>Date:</strong> {now}</p>
    <p><strong>Overall consensus:</strong> {report.overall_consensus}</p>

    <h2>Solvers</h2>
    <table>
        <tr><th>Solver</th></tr>
{solver_rows}
    </table>

    <h2>Consensus by DOF</h2>
    <table>
        <tr>
            <th>DOF</th>
            <th>Consensus</th>
            <th>Mean Correlation</th>
            <th>Outlier</th>
        </tr>
{consensus_rows}
    </table>

    <h2>Plots</h2>
    <ul>
{plot_links}
    </ul>

    <h2>Notes</h2>
    <ul>
{notes_items}
    </ul>
</body>
</html>
"""

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
