#!/usr/bin/env python3
"""
WAMIT Validation Benchmark Runner

Orchestrates OrcaWave vs WAMIT validation benchmarks across the 12 Orcina
validation cases (2.1-2.9, 3.1-3.3). Loads case metadata from
validation_config.yaml, runs OrcaWave via the existing runner infrastructure,
loads WAMIT reference data where available, and produces HTML benchmark reports.

Usage:
    uv run python scripts/benchmark/run_validation_benchmark.py --list
    uv run python scripts/benchmark/run_validation_benchmark.py --case 2.7
    uv run python scripts/benchmark/run_validation_benchmark.py --phase 1
    uv run python scripts/benchmark/run_validation_benchmark.py --all --dry-run
    uv run python scripts/benchmark/run_validation_benchmark.py --case 2.7 --output-dir results/val

WRK-134: WAMIT validation benchmark system
"""

from __future__ import annotations

import argparse
import json
import sys
import time
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional

import yaml

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------


@dataclass
class CaseConfig:
    """Metadata for a single validation case from validation_config.yaml."""

    case_id: str
    name: str
    geometry: object  # str or list[str]
    panels: object  # int or list[int]
    physics: str
    hull_library_id: Optional[str]
    phase: int
    status: str
    wamit_version: str
    water_depth: object  # numeric or "infinite"
    notes: str


@dataclass
class CaseRunResult:
    """Result of running a single validation case."""

    case_id: str
    name: str
    status: str  # "completed", "failed", "skipped", "dry_run", "no_spec"
    duration_seconds: float = 0.0
    report_path: Optional[Path] = None
    error_message: Optional[str] = None
    orcawave_status: Optional[str] = None
    wamit_status: Optional[str] = None


# ---------------------------------------------------------------------------
# Config loader
# ---------------------------------------------------------------------------

# Default path to the validation config relative to project root
_PROJECT_ROOT = Path(__file__).parent.parent.parent
_DEFAULT_CONFIG_PATH = (
    _PROJECT_ROOT
    / "docs"
    / "modules"
    / "orcawave"
    / "L00_validation_wamit"
    / "validation_config.yaml"
)


def load_validation_config(
    config_path: Optional[Path] = None,
) -> Dict[str, CaseConfig]:
    """Load validation_config.yaml and return a dict of CaseConfig by case ID.

    Args:
        config_path: Path to validation_config.yaml.
            Defaults to the standard location.

    Returns:
        Ordered dict mapping case ID strings to CaseConfig objects.

    Raises:
        FileNotFoundError: If config file does not exist.
        ValueError: If required fields are missing.
    """
    path = config_path or _DEFAULT_CONFIG_PATH
    if not path.exists():
        raise FileNotFoundError(f"Validation config not found: {path}")

    with open(path, "r", encoding="utf-8") as f:
        raw = yaml.safe_load(f)

    cases_raw = raw.get("cases", {})
    if not cases_raw:
        raise ValueError("No cases defined in validation config")

    cases: Dict[str, CaseConfig] = {}
    for case_id, meta in cases_raw.items():
        cases[str(case_id)] = CaseConfig(
            case_id=str(case_id),
            name=meta.get("name", f"Case {case_id}"),
            geometry=meta.get("geometry", "unknown"),
            panels=meta.get("panels", 0),
            physics=meta.get("physics", "unknown"),
            hull_library_id=meta.get("hull_library_id"),
            phase=meta.get("phase", 0),
            status=meta.get("status", "pending"),
            wamit_version=meta.get("wamit_version", "unknown"),
            water_depth=meta.get("water_depth", "infinite"),
            notes=meta.get("notes", ""),
        )

    return cases


def _resolve_case_dir(base_dir: Path, case_id: str) -> Path:
    """Resolve the filesystem path for a validation case directory."""
    return base_dir / case_id


# ---------------------------------------------------------------------------
# OrcaWave runner (reuses pattern from run_3way_benchmark.py)
# ---------------------------------------------------------------------------


def _run_orcawave_for_case(
    spec_path: Path,
    output_dir: Path,
    dry_run: bool = False,
) -> Dict:
    """Run OrcaWave on a single spec.yml and return a result dict.

    Returns:
        Dict with keys: status, results, duration_seconds, error_message.
        status is one of: "completed", "dry_run", "failed", "skipped".
        results is a DiffractionResults or None.
    """
    import numpy as np

    result = {
        "status": "pending",
        "results": None,
        "duration_seconds": 0.0,
        "error_message": None,
    }
    start = time.time()

    try:
        from digitalmodel.hydrodynamics.diffraction.input_schemas import (
            DiffractionSpec,
        )
        from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
            OrcaWaveRunner,
            RunConfig,
            RunStatus,
        )

        spec = DiffractionSpec.from_yaml(spec_path)
        print(f"    Spec loaded: {spec_path.name}")

        orcawave_output = output_dir / "orcawave"
        config = RunConfig(
            output_dir=orcawave_output,
            dry_run=dry_run,
            timeout_seconds=3600,
        )

        runner = OrcaWaveRunner(config)
        run_result = runner.run(spec, spec_path=spec_path)

        if dry_run:
            result["status"] = "dry_run"
            print("    [DRY RUN] OrcaWave files generated, solver not executed")
        elif run_result.status == RunStatus.COMPLETED:
            result["status"] = "completed"
            # Extract results from .owr file
            owr_files = sorted(orcawave_output.glob("*.owr"))
            if owr_files:
                print(f"    Extracting results from: {owr_files[0].name}")
                vessel_name = spec.get_bodies()[0].vessel.name
                water_depth = (
                    spec.environment.water_depth if spec.environment else 100.0
                )
                # Import the extraction helper from run_3way_benchmark
                from scripts.benchmark.run_3way_benchmark import (
                    _extract_from_owr,
                )

                result["results"] = _extract_from_owr(
                    owr_files[0], vessel_name, water_depth
                )
                if result["results"]:
                    n_freq = result["results"].raos.surge.frequencies.count
                    n_head = result["results"].raos.surge.headings.count
                    print(
                        f"    [OK] Extracted {n_freq} frequencies x "
                        f"{n_head} headings"
                    )
                else:
                    print("    [WARNING] Could not extract results from .owr")
            else:
                print(
                    f"    [WARNING] No .owr file found in {orcawave_output}"
                )
            print("    [OK] OrcaWave completed")
        elif run_result.status == RunStatus.DRY_RUN:
            result["status"] = "dry_run"
            msg = run_result.error_message or "Executable not found"
            print(f"    [DRY RUN] {msg}")
        else:
            result["status"] = "failed"
            parts = []
            if run_result.error_message:
                parts.append(run_result.error_message)
            if run_result.stderr:
                parts.append(f"stderr: {run_result.stderr[:200]}")
            if run_result.return_code is not None and run_result.return_code != 0:
                parts.append(f"exit code: {run_result.return_code}")
            if not parts:
                parts.append(
                    f"Unknown error (status: {run_result.status.value})"
                )
            result["error_message"] = "; ".join(parts)
            print(f"    [FAILED] {result['error_message']}")

    except ImportError as e:
        result["status"] = "skipped"
        result["error_message"] = f"OrcFxAPI not available: {e}"
        print(f"    [SKIPPED] {result['error_message']}")
    except Exception as e:
        import traceback

        result["status"] = "failed"
        result["error_message"] = f"{type(e).__name__}: {e}"
        print(f"    [FAILED] {result['error_message']}")
        print(f"    Traceback:\n{traceback.format_exc()}")

    result["duration_seconds"] = time.time() - start
    return result


# ---------------------------------------------------------------------------
# WAMIT reference loader
# ---------------------------------------------------------------------------


def _load_wamit_reference(case_dir: Path) -> Optional[object]:
    """Attempt to load WAMIT reference data from a case directory.

    Looks for reference_data.yaml in the case directory and loads it
    via WamitReferenceLoader.from_yaml().

    Returns:
        DiffractionResults or None if no reference data found.
    """
    ref_path = case_dir / "reference_data.yaml"
    if not ref_path.exists():
        return None

    try:
        from digitalmodel.hydrodynamics.diffraction.wamit_reference_loader import (
            WamitReferenceLoader,
        )

        results = WamitReferenceLoader.from_yaml(ref_path)
        print(f"    [OK] WAMIT reference loaded from {ref_path.name}")
        return results
    except Exception as e:
        print(f"    [WARNING] Failed to load WAMIT reference: {e}")
        return None


# ---------------------------------------------------------------------------
# Benchmark comparison
# ---------------------------------------------------------------------------


def _run_benchmark_comparison(
    orcawave_results,
    wamit_results,
    output_dir: Path,
    case_config: CaseConfig,
    spec: Optional[dict] = None,
) -> Optional[Path]:
    """Run benchmark comparison between OrcaWave and WAMIT results.

    Args:
        orcawave_results: DiffractionResults from OrcaWave.
        wamit_results: DiffractionResults from WAMIT reference.
        output_dir: Directory for benchmark output artifacts.
        case_config: Case metadata for the report.
        spec: Optional loaded spec dict for solver metadata.

    Returns:
        Path to the HTML report, or None on failure.
    """
    try:
        from digitalmodel.hydrodynamics.diffraction.benchmark_runner import (
            BenchmarkConfig,
            BenchmarkRunner,
        )

        solver_results = {
            "OrcaWave": orcawave_results,
            "WAMIT": wamit_results,
        }

        config = BenchmarkConfig(
            output_dir=output_dir,
            tolerance=0.05,
            x_axis="period",
            reference_solver="WAMIT",
        )

        # Build solver metadata if spec is available
        solver_metadata = None
        if spec is not None:
            try:
                from scripts.benchmark.solver_metadata import (
                    build_solver_metadata,
                )

                solver_metadata = build_solver_metadata(
                    spec,
                    wamit_version=case_config.wamit_version,
                )
            except Exception as e:
                print(f"    [WARNING] Could not build solver metadata: {e}")

        runner = BenchmarkRunner(config)
        benchmark_result = runner.run_from_results(
            solver_results, solver_metadata=solver_metadata
        )

        if benchmark_result.success:
            print(
                f"    [OK] Benchmark: "
                f"{benchmark_result.report.overall_consensus}"
            )
            return benchmark_result.report_html_path
        else:
            print(
                f"    [WARNING] Benchmark failed: "
                f"{benchmark_result.error_message}"
            )
            return None

    except Exception as e:
        print(f"    [WARNING] Benchmark comparison error: {e}")
        return None


# ---------------------------------------------------------------------------
# Single case runner
# ---------------------------------------------------------------------------


def run_single_case(
    case_config: CaseConfig,
    base_dir: Path,
    output_base: Path,
    dry_run: bool = False,
) -> CaseRunResult:
    """Run a single validation case end-to-end.

    Steps:
        1. Look for spec.yml in case directory.
        2. Run OrcaWave via the existing runner infrastructure.
        3. Attempt to load WAMIT reference data.
        4. If both results available, run benchmark comparison.
        5. Return CaseRunResult with status and paths.

    Args:
        case_config: Metadata for this case.
        base_dir: Root directory containing case subdirectories.
        output_base: Base output directory for benchmark artifacts.
        dry_run: If True, generate files without running the solver.

    Returns:
        CaseRunResult with status and optional report path.
    """
    case_id = case_config.case_id
    case_dir = _resolve_case_dir(base_dir, case_id)
    output_dir = output_base / case_id

    print(f"\n{'='*60}")
    print(f"Case {case_id}: {case_config.name}")
    print(f"  Phase: {case_config.phase} | Status: {case_config.status}")
    print(f"  Directory: {case_dir}")
    print(f"{'='*60}")

    start_time = time.time()
    result = CaseRunResult(
        case_id=case_id,
        name=case_config.name,
        status="pending",
    )

    # Step 1: Check for spec.yml
    spec_path = case_dir / "spec.yml"
    if not spec_path.exists():
        result.status = "no_spec"
        result.error_message = f"No spec.yml found in {case_dir}"
        print(f"  [SKIP] {result.error_message}")
        result.duration_seconds = time.time() - start_time
        return result

    # Load the raw spec for metadata
    spec_dict = None
    try:
        with open(spec_path, "r", encoding="utf-8") as f:
            spec_dict = yaml.safe_load(f)
    except Exception as e:
        print(f"  [WARNING] Could not parse spec.yml as dict: {e}")

    # Step 2: Run OrcaWave
    print(f"\n  --- OrcaWave ---")
    output_dir.mkdir(parents=True, exist_ok=True)
    ow_result = _run_orcawave_for_case(
        spec_path=spec_path,
        output_dir=output_dir,
        dry_run=dry_run,
    )
    result.orcawave_status = ow_result["status"]
    print(f"    Duration: {ow_result['duration_seconds']:.1f}s")

    # Step 3: Load WAMIT reference data
    print(f"\n  --- WAMIT Reference ---")
    wamit_results = _load_wamit_reference(case_dir)
    result.wamit_status = "loaded" if wamit_results else "not_found"
    if wamit_results is None:
        print(f"    [INFO] No reference_data.yaml in {case_dir}")

    # Step 4: Run benchmark comparison if both available
    ow_results = ow_result["results"]
    if ow_results is not None and wamit_results is not None:
        print(f"\n  --- Benchmark Comparison ---")
        benchmark_output = output_dir / "benchmark_comparison"
        report_path = _run_benchmark_comparison(
            orcawave_results=ow_results,
            wamit_results=wamit_results,
            output_dir=benchmark_output,
            case_config=case_config,
            spec=spec_dict,
        )
        result.report_path = report_path
        result.status = "completed" if report_path else "partial"
    elif ow_result["status"] == "dry_run":
        result.status = "dry_run"
    elif ow_result["status"] in ("completed", "dry_run"):
        # OrcaWave ran but no WAMIT reference to compare against
        result.status = "partial"
        result.error_message = "OrcaWave completed but no WAMIT reference data"
        print(f"  [INFO] {result.error_message}")
    else:
        result.status = ow_result["status"]
        result.error_message = ow_result["error_message"]

    result.duration_seconds = time.time() - start_time
    return result


# ---------------------------------------------------------------------------
# Listing
# ---------------------------------------------------------------------------


def list_cases(cases: Dict[str, CaseConfig], base_dir: Path) -> None:
    """Print a summary table of all validation cases."""
    print(f"\nWAMIT Validation Cases ({len(cases)} total)")
    print(f"Base directory: {base_dir}\n")

    # Header
    header = (
        f"{'Case':<6}| {'Name':<35}| {'Phase':<6}| {'Status':<8}| "
        f"{'Panels':<12}| {'WAMIT':<8}| {'spec.yml':<9}| {'ref_data':<9}"
    )
    separator = "-" * len(header)
    print(header)
    print(separator)

    for case_id, cfg in cases.items():
        case_dir = _resolve_case_dir(base_dir, case_id)
        has_spec = "yes" if (case_dir / "spec.yml").exists() else "no"
        has_ref = (
            "yes"
            if (case_dir / "reference_data.yaml").exists()
            else "no"
        )
        panels_str = str(cfg.panels)
        if len(panels_str) > 10:
            panels_str = panels_str[:10] + ".."

        print(
            f"{case_id:<6}| {cfg.name:<35}| {cfg.phase:<6}| "
            f"{cfg.status:<8}| {panels_str:<12}| {cfg.wamit_version:<8}| "
            f"{has_spec:<9}| {has_ref:<9}"
        )

    print(separator)

    # Summary counts
    by_phase = {}
    for cfg in cases.values():
        by_phase.setdefault(cfg.phase, []).append(cfg)

    print(f"\nBy phase:")
    for phase in sorted(by_phase):
        count = len(by_phase[phase])
        ready = sum(1 for c in by_phase[phase] if c.status == "ready")
        print(f"  Phase {phase}: {count} cases ({ready} ready)")


# ---------------------------------------------------------------------------
# Summary table
# ---------------------------------------------------------------------------


def print_summary(results: List[CaseRunResult]) -> None:
    """Print a summary table of all case run results."""
    print(f"\n{'#'*60}")
    print("# Validation Benchmark Summary")
    print(f"{'#'*60}\n")

    header = (
        f"{'Case':<6}| {'Name':<28}| {'Status':<11}| "
        f"{'Duration':<10}| {'Report'}"
    )
    separator = "-" * 100
    print(header)
    print(separator)

    for r in results:
        duration_str = f"{r.duration_seconds:.1f}s"
        report_str = str(r.report_path) if r.report_path else "-"

        # Truncate name for alignment
        name = r.name
        if len(name) > 26:
            name = name[:25] + ".."

        print(
            f"{r.case_id:<6}| {name:<28}| {r.status:<11}| "
            f"{duration_str:<10}| {report_str}"
        )

    print(separator)

    # Totals
    total_duration = sum(r.duration_seconds for r in results)
    completed = sum(1 for r in results if r.status == "completed")
    failed = sum(1 for r in results if r.status == "failed")
    skipped = sum(
        1 for r in results if r.status in ("skipped", "no_spec")
    )
    dry_runs = sum(1 for r in results if r.status == "dry_run")
    partial = sum(1 for r in results if r.status == "partial")

    print(f"\nTotal: {len(results)} cases in {total_duration:.1f}s")
    print(
        f"  Completed: {completed} | Partial: {partial} | "
        f"Dry run: {dry_runs} | Skipped: {skipped} | Failed: {failed}"
    )


# ---------------------------------------------------------------------------
# Write summary JSON
# ---------------------------------------------------------------------------


def _write_summary_json(
    results: List[CaseRunResult],
    output_dir: Path,
) -> Path:
    """Write a machine-readable summary of all case results to JSON."""
    summary = {
        "timestamp": datetime.now().isoformat(),
        "total_cases": len(results),
        "completed": sum(1 for r in results if r.status == "completed"),
        "failed": sum(1 for r in results if r.status == "failed"),
        "cases": {
            r.case_id: {
                "name": r.name,
                "status": r.status,
                "duration_seconds": r.duration_seconds,
                "report_path": str(r.report_path) if r.report_path else None,
                "error_message": r.error_message,
                "orcawave_status": r.orcawave_status,
                "wamit_status": r.wamit_status,
            }
            for r in results
        },
    }

    output_dir.mkdir(parents=True, exist_ok=True)
    json_path = output_dir / "validation_summary.json"
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(summary, f, indent=2)

    print(f"\nSummary JSON: {json_path}")
    return json_path


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def main() -> None:
    parser = argparse.ArgumentParser(
        description=(
            "Run WAMIT validation benchmarks for OrcaWave "
            "(Orcina cases 2.1-2.9, 3.1-3.3)"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # List all cases with status
  uv run python scripts/benchmark/run_validation_benchmark.py --list

  # Run a single case
  uv run python scripts/benchmark/run_validation_benchmark.py --case 2.7

  # Run all Phase 1 cases
  uv run python scripts/benchmark/run_validation_benchmark.py --phase 1

  # Run all cases (dry run)
  uv run python scripts/benchmark/run_validation_benchmark.py --all --dry-run

  # Custom output directory
  uv run python scripts/benchmark/run_validation_benchmark.py --case 2.7 --output-dir results/val
        """,
    )

    # Mutually exclusive run modes
    mode_group = parser.add_mutually_exclusive_group(required=True)
    mode_group.add_argument(
        "--case",
        type=str,
        default=None,
        help="Run a single case by ID (e.g. 2.7)",
    )
    mode_group.add_argument(
        "--phase",
        type=int,
        default=None,
        help="Run all cases in a phase (1, 2, or 3)",
    )
    mode_group.add_argument(
        "--all",
        action="store_true",
        default=False,
        help="Run all cases",
    )
    mode_group.add_argument(
        "--list",
        action="store_true",
        default=False,
        help="List all cases with status (no execution)",
    )

    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Generate input files but do not execute the solver",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("benchmark_output") / "validation",
        help=(
            "Output directory for benchmark artifacts "
            "(default: benchmark_output/validation)"
        ),
    )
    parser.add_argument(
        "--config",
        type=Path,
        default=None,
        help="Path to validation_config.yaml (default: auto-detect)",
    )

    args = parser.parse_args()

    # Load configuration
    try:
        cases = load_validation_config(args.config)
    except (FileNotFoundError, ValueError) as e:
        print(f"Error loading config: {e}")
        sys.exit(1)

    # Determine base directory for case data
    config_path = args.config or _DEFAULT_CONFIG_PATH
    base_dir = config_path.parent

    # Handle --list mode
    if args.list:
        list_cases(cases, base_dir)
        sys.exit(0)

    # Determine which cases to run
    cases_to_run: List[CaseConfig] = []

    if args.case is not None:
        case_id = args.case
        if case_id not in cases:
            print(f"Error: Unknown case '{case_id}'")
            print(f"Available cases: {', '.join(sorted(cases.keys()))}")
            sys.exit(1)
        cases_to_run.append(cases[case_id])

    elif args.phase is not None:
        phase = args.phase
        cases_to_run = [c for c in cases.values() if c.phase == phase]
        if not cases_to_run:
            print(f"Error: No cases found for phase {phase}")
            sys.exit(1)
        print(f"Running {len(cases_to_run)} cases for Phase {phase}")

    elif args.all:
        cases_to_run = list(cases.values())
        print(f"Running all {len(cases_to_run)} cases")

    # Print banner
    print(f"\n{'#'*60}")
    print("# WAMIT Validation Benchmark Runner")
    print(f"# Cases: {len(cases_to_run)}")
    print(f"# Output: {args.output_dir}")
    print(f"# Dry Run: {args.dry_run}")
    print(f"{'#'*60}")

    # Run each case
    total_start = time.time()
    results: List[CaseRunResult] = []

    for case_config in cases_to_run:
        case_result = run_single_case(
            case_config=case_config,
            base_dir=base_dir,
            output_base=args.output_dir,
            dry_run=args.dry_run,
        )
        results.append(case_result)

        # Print per-case status line
        status_icon = {
            "completed": "[OK]",
            "partial": "[PARTIAL]",
            "dry_run": "[DRY]",
            "failed": "[FAIL]",
            "skipped": "[SKIP]",
            "no_spec": "[NO SPEC]",
        }.get(case_result.status, "[?]")
        print(
            f"\n  {status_icon} Case {case_result.case_id}: "
            f"{case_result.status} ({case_result.duration_seconds:.1f}s)"
        )

    total_duration = time.time() - total_start

    # Print summary
    print_summary(results)
    print(f"\nTotal wall time: {total_duration:.1f}s")

    # Write summary JSON
    _write_summary_json(results, args.output_dir)

    # Exit code: 0 if at least one case completed or dry-ran successfully
    has_success = any(
        r.status in ("completed", "dry_run", "partial")
        for r in results
    )
    sys.exit(0 if has_success else 1)


if __name__ == "__main__":
    main()
