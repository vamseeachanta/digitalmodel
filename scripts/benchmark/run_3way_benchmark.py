#!/usr/bin/env python3
"""
3-Way Solver Benchmark Runner

Orchestrates diffraction analysis across AQWA, OrcaWave, and BEMRosetta,
then runs comparison using the benchmark framework.

Usage:
    uv run python scripts/benchmark/run_3way_benchmark.py specs/modules/benchmark/unit_box_spec.yml
    uv run python scripts/benchmark/run_3way_benchmark.py spec.yml --solvers orcawave,aqwa
    uv run python scripts/benchmark/run_3way_benchmark.py spec.yml --dry-run

WRK-099: Run 3-way benchmark on Unit Box hull
"""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
import time
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional

import yaml

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from digitalmodel.hydrodynamics.diffraction.benchmark_runner import (
    BenchmarkConfig,
    BenchmarkRunner,
    BenchmarkRunResult,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import DiffractionResults


# ---------------------------------------------------------------------------
# Solver Configuration
# ---------------------------------------------------------------------------

SOLVER_PATHS = {
    "orcawave": None,  # Uses OrcFxAPI Python binding
    "aqwa": Path("C:/Program Files/ANSYS Inc/v252/aqwa/bin/winx64/Aqwa.exe"),
    "bemrosetta": Path("D:/software/BEMRosetta/BEMRosetta_cl.exe"),
}


@dataclass
class SolverRunResult:
    """Result of running a single solver."""

    solver_name: str
    status: str  # "completed", "failed", "skipped", "dry_run"
    duration_seconds: float = 0.0
    output_dir: Optional[Path] = None
    results: Optional[DiffractionResults] = None
    error_message: Optional[str] = None
    log_file: Optional[Path] = None


@dataclass
class BenchmarkOrchestrationResult:
    """Result of the full benchmark orchestration."""

    spec_path: Path
    solvers_requested: List[str]
    solver_results: Dict[str, SolverRunResult] = field(default_factory=dict)
    benchmark_result: Optional[BenchmarkRunResult] = None
    total_duration_seconds: float = 0.0
    success: bool = False
    error_message: Optional[str] = None


# ---------------------------------------------------------------------------
# Solver Runners
# ---------------------------------------------------------------------------


def run_orcawave(
    spec_path: Path,
    output_dir: Path,
    dry_run: bool = False,
) -> SolverRunResult:
    """Run OrcaWave diffraction analysis."""
    print(f"\n{'='*60}")
    print("Running OrcaWave...")
    print(f"{'='*60}")

    start_time = time.time()
    result = SolverRunResult(solver_name="OrcaWave", status="pending")

    try:
        from digitalmodel.hydrodynamics.diffraction.input_schemas import (
            DiffractionSpec,
        )
        from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
            OrcaWaveRunner,
            RunConfig,
            RunStatus,
        )

        # Load spec
        print(f"  Loading spec from: {spec_path}")
        spec = DiffractionSpec.from_yaml(spec_path)
        print(f"  Spec loaded successfully")

        # Configure runner
        orcawave_output = output_dir / "orcawave"
        config = RunConfig(
            output_dir=orcawave_output,
            dry_run=dry_run,
            timeout_seconds=3600,
        )

        # Run (pass spec_path so mesh files can be resolved and copied)
        print(f"  Creating OrcaWave runner...")
        runner = OrcaWaveRunner(config)
        print(f"  Running OrcaWave analysis...")
        run_result = runner.run(spec, spec_path=spec_path)

        result.output_dir = orcawave_output
        result.log_file = orcawave_output / "orcawave.log"

        # Debug: print actual status received
        print(f"  OrcaWave run_result.status = {run_result.status} (type: {type(run_result.status).__name__})")

        if dry_run:
            result.status = "dry_run"
            print("  [DRY RUN] OrcaWave files generated, solver not executed")
        elif run_result.status == RunStatus.COMPLETED:
            result.status = "completed"
            # Load results if available
            results_file = orcawave_output / "results" / "diffraction_results.json"
            if results_file.exists():
                result.results = _load_results(results_file, "OrcaWave")
            print(f"  [OK] OrcaWave completed")
        elif run_result.status == RunStatus.DRY_RUN:
            # OrcaWave runner fell back to dry-run (executable not found)
            result.status = "dry_run"
            if run_result.error_message:
                print(f"  [DRY RUN] {run_result.error_message}")
            else:
                print(f"  [DRY RUN] OrcaWave executable not found, files generated only")
            # Check mesh file status
            if run_result.mesh_files:
                print(f"  Mesh files copied: {[f.name for f in run_result.mesh_files]}")
            else:
                print(f"  WARNING: No mesh files were copied")
        else:
            result.status = "failed"
            # Build a meaningful error message
            error_parts = []
            if run_result.error_message:
                error_parts.append(run_result.error_message)
            if run_result.stderr:
                error_parts.append(f"stderr: {run_result.stderr[:200]}")
            if run_result.return_code is not None and run_result.return_code != 0:
                error_parts.append(f"exit code: {run_result.return_code}")
            if not error_parts:
                error_parts.append(f"Unknown error (status: {run_result.status.value})")
            result.error_message = "; ".join(error_parts)
            print(f"  [FAILED] {result.error_message}")

    except ImportError as e:
        result.status = "skipped"
        result.error_message = f"OrcFxAPI not available: {e}"
        print(f"  [SKIPPED] {result.error_message}")
    except Exception as e:
        import traceback
        result.status = "failed"
        result.error_message = f"{type(e).__name__}: {e}"
        print(f"  [FAILED] {result.error_message}")
        print(f"  Traceback:\n{traceback.format_exc()}")

    result.duration_seconds = time.time() - start_time
    print(f"  Duration: {result.duration_seconds:.1f}s")
    return result


def run_aqwa(
    spec_path: Path,
    output_dir: Path,
    dry_run: bool = False,
) -> SolverRunResult:
    """Run AQWA diffraction analysis."""
    print(f"\n{'='*60}")
    print("Running AQWA...")
    print(f"{'='*60}")

    start_time = time.time()
    result = SolverRunResult(solver_name="AQWA", status="pending")

    try:
        from digitalmodel.hydrodynamics.diffraction.aqwa_runner import (
            AQWARunner,
            AQWARunConfig,
        )
        from digitalmodel.hydrodynamics.diffraction.input_schemas import (
            DiffractionSpec,
        )

        # Load spec
        spec = DiffractionSpec.from_yaml(spec_path)

        # Configure runner
        aqwa_output = output_dir / "aqwa"
        config = AQWARunConfig(
            executable_path=SOLVER_PATHS["aqwa"],
            output_dir=aqwa_output,
            dry_run=dry_run,
            timeout_seconds=3600,
        )

        # Run (pass spec_path so mesh files can be resolved and copied)
        runner = AQWARunner(config)
        run_result = runner.run(spec, spec_path=spec_path)

        result.output_dir = aqwa_output
        result.log_file = aqwa_output / "aqwa.log"

        if dry_run:
            result.status = "dry_run"
            print("  [DRY RUN] AQWA files generated, solver not executed")
        elif run_result.status.value == "completed":
            result.status = "completed"
            # Load results if available
            results_file = aqwa_output / "results" / "diffraction_results.json"
            if results_file.exists():
                result.results = _load_results(results_file, "AQWA")
            print(f"  [OK] AQWA completed")
        else:
            result.status = "failed"
            result.error_message = run_result.error_message
            print(f"  [FAILED] {run_result.error_message}")

    except Exception as e:
        result.status = "failed"
        result.error_message = str(e)
        print(f"  [FAILED] {e}")

    result.duration_seconds = time.time() - start_time
    print(f"  Duration: {result.duration_seconds:.1f}s")
    return result


def run_bemrosetta(
    spec_path: Path,
    output_dir: Path,
    dry_run: bool = False,
) -> SolverRunResult:
    """Run BEMRosetta (NEMOH backend) diffraction analysis."""
    print(f"\n{'='*60}")
    print("Running BEMRosetta...")
    print(f"{'='*60}")

    start_time = time.time()
    result = SolverRunResult(solver_name="BEMRosetta", status="pending")

    bemrosetta_exe = SOLVER_PATHS["bemrosetta"]
    if not bemrosetta_exe or not bemrosetta_exe.exists():
        result.status = "skipped"
        result.error_message = "BEMRosetta executable not found"
        print(f"  [SKIPPED] {result.error_message}")
        return result

    try:
        # Load spec
        with open(spec_path) as f:
            spec = yaml.safe_load(f)

        # Create output directory
        bemrosetta_output = output_dir / "bemrosetta"
        bemrosetta_output.mkdir(parents=True, exist_ok=True)
        result.output_dir = bemrosetta_output

        # Get mesh file path (resolve relative to spec)
        mesh_rel = spec["vessel"]["geometry"]["mesh_file"]
        mesh_path = (spec_path.parent / mesh_rel).resolve()

        if not mesh_path.exists():
            raise FileNotFoundError(f"Mesh file not found: {mesh_path}")

        # Build BEMRosetta command using -mesh mode to convert GDF to NEMOH
        # Use absolute path for output file
        nemoh_mesh_out = (bemrosetta_output / "mesh_nemoh.dat").resolve()
        cg = spec["vessel"]["inertia"]["centre_of_gravity"]
        cmd = [
            str(bemrosetta_exe),
            "-mesh",
            "-i", str(mesh_path),
            "-cg", str(cg[0]), str(cg[1]), str(cg[2]),
            "-c", str(nemoh_mesh_out),
        ]

        result.log_file = bemrosetta_output / "bemrosetta.log"

        if dry_run:
            result.status = "dry_run"
            print(f"  [DRY RUN] Would execute: {' '.join(cmd[:6])}...")
            with open(result.log_file, "w") as f:
                f.write(f"DRY RUN - Command: {' '.join(cmd)}\n")
        else:
            print(f"  Executing BEMRosetta...")
            with open(result.log_file, "w") as log_f:
                proc = subprocess.run(
                    cmd,
                    capture_output=True,
                    text=True,
                    timeout=3600,
                    cwd=str(bemrosetta_output),
                )
                log_f.write(f"Command: {' '.join(cmd)}\n\n")
                log_f.write(f"STDOUT:\n{proc.stdout}\n\n")
                log_f.write(f"STDERR:\n{proc.stderr}\n\n")
                log_f.write(f"Return code: {proc.returncode}\n")

            if proc.returncode == 0:
                result.status = "completed"
                print(f"  [OK] BEMRosetta case generated")
                # Note: Actual NEMOH execution would be a separate step
            else:
                result.status = "failed"
                result.error_message = proc.stderr[:500] if proc.stderr else "Unknown error"
                print(f"  [FAILED] Return code {proc.returncode}")

    except subprocess.TimeoutExpired:
        result.status = "failed"
        result.error_message = "Timeout after 3600s"
        print(f"  [FAILED] {result.error_message}")
    except Exception as e:
        result.status = "failed"
        result.error_message = str(e)
        print(f"  [FAILED] {e}")

    result.duration_seconds = time.time() - start_time
    print(f"  Duration: {result.duration_seconds:.1f}s")
    return result


def _load_results(results_file: Path, solver_name: str) -> Optional[DiffractionResults]:
    """Load DiffractionResults from JSON file."""
    try:
        with open(results_file) as f:
            data = json.load(f)
        # TODO: Implement proper deserialization
        return None
    except Exception:
        return None


# ---------------------------------------------------------------------------
# Orchestrator
# ---------------------------------------------------------------------------


def run_benchmark(
    spec_path: Path,
    output_dir: Path,
    solvers: List[str],
    dry_run: bool = False,
) -> BenchmarkOrchestrationResult:
    """Run full 3-way benchmark orchestration."""
    print(f"\n{'#'*60}")
    print("# 3-Way Solver Benchmark")
    print(f"# Spec: {spec_path}")
    print(f"# Solvers: {', '.join(solvers)}")
    print(f"# Output: {output_dir}")
    print(f"# Dry Run: {dry_run}")
    print(f"{'#'*60}")

    start_time = time.time()
    result = BenchmarkOrchestrationResult(
        spec_path=spec_path,
        solvers_requested=solvers,
    )

    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)

    # Run each solver
    solver_runners = {
        "orcawave": run_orcawave,
        "aqwa": run_aqwa,
        "bemrosetta": run_bemrosetta,
    }

    for solver in solvers:
        solver_lower = solver.lower()
        if solver_lower in solver_runners:
            solver_result = solver_runners[solver_lower](
                spec_path=spec_path,
                output_dir=output_dir,
                dry_run=dry_run,
            )
            result.solver_results[solver_result.solver_name] = solver_result
        else:
            print(f"\n[WARNING] Unknown solver: {solver}")

    # Collect successful results for comparison
    successful_results: Dict[str, DiffractionResults] = {}
    for name, sr in result.solver_results.items():
        if sr.status == "completed" and sr.results is not None:
            successful_results[name] = sr.results

    # Run benchmark comparison if we have 2+ results
    if len(successful_results) >= 2:
        print(f"\n{'='*60}")
        print("Running Benchmark Comparison...")
        print(f"{'='*60}")

        try:
            benchmark_output = output_dir / "benchmark_comparison"
            config = BenchmarkConfig(
                output_dir=benchmark_output,
                tolerance=0.05,
                x_axis="period",
            )
            runner = BenchmarkRunner(config)
            result.benchmark_result = runner.run_from_results(successful_results)

            if result.benchmark_result.success:
                print(f"  [OK] Benchmark comparison complete")
                print(f"  Consensus: {result.benchmark_result.report.overall_consensus}")
                print(f"  Report: {result.benchmark_result.report_html_path}")
            else:
                print(f"  [FAILED] {result.benchmark_result.error_message}")

        except Exception as e:
            print(f"  [FAILED] Benchmark comparison error: {e}")

    elif dry_run:
        print(f"\n[DRY RUN] Skipping benchmark comparison (no actual results)")
    else:
        print(f"\n[WARNING] Need 2+ successful solver runs for comparison")
        print(f"  Completed: {[n for n, r in result.solver_results.items() if r.status == 'completed']}")

    # Summary
    result.total_duration_seconds = time.time() - start_time
    result.success = any(
        r.status in ("completed", "dry_run")
        for r in result.solver_results.values()
    )

    print(f"\n{'#'*60}")
    print("# Summary")
    print(f"{'#'*60}")
    for name, sr in result.solver_results.items():
        status_icon = {
            "completed": "[OK]",
            "dry_run": "[DRY]",
            "failed": "[FAIL]",
            "skipped": "[SKIP]",
        }.get(sr.status, "[?]")
        print(f"  {status_icon} {name}: {sr.status} ({sr.duration_seconds:.1f}s)")

    print(f"\n  Total duration: {result.total_duration_seconds:.1f}s")
    print(f"  Output directory: {output_dir}")

    # Write summary JSON
    summary_file = output_dir / "benchmark_summary.json"
    _write_summary(result, summary_file)
    print(f"  Summary: {summary_file}")

    return result


def _write_summary(result: BenchmarkOrchestrationResult, output_file: Path) -> None:
    """Write orchestration summary to JSON."""
    summary = {
        "spec_path": str(result.spec_path),
        "timestamp": datetime.now().isoformat(),
        "solvers_requested": result.solvers_requested,
        "total_duration_seconds": result.total_duration_seconds,
        "success": result.success,
        "solver_results": {
            name: {
                "status": sr.status,
                "duration_seconds": sr.duration_seconds,
                "output_dir": str(sr.output_dir) if sr.output_dir else None,
                "error_message": sr.error_message,
            }
            for name, sr in result.solver_results.items()
        },
    }

    if result.benchmark_result and result.benchmark_result.report:
        summary["benchmark"] = {
            "overall_consensus": result.benchmark_result.report.overall_consensus,
            "report_json": str(result.benchmark_result.report_json_path),
            "report_html": str(result.benchmark_result.report_html_path),
        }

    with open(output_file, "w") as f:
        json.dump(summary, f, indent=2)


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def main():
    parser = argparse.ArgumentParser(
        description="Run 3-way solver benchmark (AQWA, OrcaWave, BEMRosetta)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Run all three solvers on Unit Box
  uv run python scripts/benchmark/run_3way_benchmark.py specs/modules/benchmark/unit_box_spec.yml

  # Run only OrcaWave and AQWA
  uv run python scripts/benchmark/run_3way_benchmark.py spec.yml --solvers orcawave,aqwa

  # Dry run (generate files, skip solver execution)
  uv run python scripts/benchmark/run_3way_benchmark.py spec.yml --dry-run

  # Custom output directory
  uv run python scripts/benchmark/run_3way_benchmark.py spec.yml -o results/my_benchmark
        """,
    )

    parser.add_argument(
        "spec_path",
        type=Path,
        help="Path to DiffractionSpec YAML file",
    )
    parser.add_argument(
        "--solvers", "-s",
        type=str,
        default="orcawave,aqwa,bemrosetta",
        help="Comma-separated list of solvers (default: orcawave,aqwa,bemrosetta)",
    )
    parser.add_argument(
        "--output", "-o",
        type=Path,
        default=None,
        help="Output directory (default: benchmark_output/<spec_name>_<timestamp>)",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Generate input files but don't execute solvers",
    )

    args = parser.parse_args()

    # Validate spec path
    if not args.spec_path.exists():
        print(f"Error: Spec file not found: {args.spec_path}")
        sys.exit(1)

    # Parse solvers
    solvers = [s.strip() for s in args.solvers.split(",")]

    # Determine output directory
    if args.output:
        output_dir = args.output
    else:
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        spec_name = args.spec_path.stem
        output_dir = Path("benchmark_output") / f"{spec_name}_{timestamp}"

    # Run benchmark
    result = run_benchmark(
        spec_path=args.spec_path,
        output_dir=output_dir,
        solvers=solvers,
        dry_run=args.dry_run,
    )

    # Exit code
    sys.exit(0 if result.success else 1)


if __name__ == "__main__":
    main()
