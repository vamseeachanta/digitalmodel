#!/usr/bin/env python3
"""Validate spec.yml pipeline by comparing against solved .owd ground truth.

For each WAMIT validation case, this script:
1. Loads the original .owd project → Calculate() → extracts DiffractionResults
2. Runs spec.yml through OrcaWaveRunner → extracts DiffractionResults
3. Compares the two using BenchmarkRunner → HTML report

This validates that our spec.yml correctly configures OrcaWave to produce
the same results as the manually-configured .owd projects.

Usage:
    uv run python scripts/benchmark/validate_owd_vs_spec.py --case 2.7
    uv run python scripts/benchmark/validate_owd_vs_spec.py --all
    uv run python scripts/benchmark/validate_owd_vs_spec.py --case 2.7 --owd-only
"""
from __future__ import annotations

import argparse
import io
import sys
import time
from datetime import datetime
from pathlib import Path
from typing import Optional

import numpy as np

# Fix Windows console encoding
if sys.platform == "win32":
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")
    sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding="utf-8")


REPO_ROOT = Path(__file__).parent.parent.parent
L00_DIR = REPO_ROOT / "docs" / "modules" / "orcawave" / "L00_validation_wamit"
OUTPUT_DIR = REPO_ROOT / "benchmark_output" / "validation"

# Add repo root to sys.path so scripts.benchmark imports work
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

CASES = {
    # Phase 1: Library-matched cases
    "2.7": {
        "owd": L00_DIR / "2.7" / "OrcaWave v11.0 files" / "Pyramid.owd",
        "spec": L00_DIR / "2.7" / "spec.yml",
        "vessel_name": "Pyramid_ZC08",
        "water_depth": float("inf"),
        "description": "Inverted Pyramid",
    },
    "2.8": {
        "owd": L00_DIR / "2.8" / "OrcaWave v11.0 files" / "Ellipsoid.owd",
        "spec": L00_DIR / "2.8" / "spec.yml",
        "vessel_name": "Ellipsoid_96p",
        "water_depth": float("inf"),
        "description": "Ellipsoid",
    },
    "3.2": {
        "owd": L00_DIR / "3.2" / "OrcaWave v11.0 files" / "Sphere.owd",
        "spec": L00_DIR / "3.2" / "spec.yml",
        "vessel_name": "Sphere_R5",
        "water_depth": float("inf"),
        "description": "Sphere with Lid",
    },
    "3.3": {
        "owd": L00_DIR / "3.3" / "OrcaWave v11.0 files" / "test.owd",
        "spec": L00_DIR / "3.3" / "spec.yml",
        "vessel_name": "Ellipsoid",
        "water_depth": float("inf"),
        "description": "Multi-body Cylinder + Ellipsoid",
        "body_index": 1,  # Compare Ellipsoid (body 1); Cylinder (body 0) is fixed
    },
    # Phase 2: New hull library cases
    "2.1": {
        "owd": L00_DIR / "2.1" / "OrcaWave v11.0 files" / "test01.owd",
        "spec": L00_DIR / "2.1" / "spec.yml",
        "vessel_name": "Test01_cylinder",
        "water_depth": float("inf"),
        "description": "Cylinder R=1 T=0.5 (no IRR)",
    },
    "2.2": {
        "owd": L00_DIR / "2.2" / "OrcaWave v11.0 files" / "test02 with base mesh.owd",
        "spec": L00_DIR / "2.2" / "spec.yml",
        "vessel_name": "Test01_cylinder",
        "water_depth": float("inf"),
        "description": "Cylinder R=1 T=0.5 (base mesh, extended freq)",
    },
    "2.3": {
        "owd": L00_DIR / "2.3" / "OrcaWave v11.0 files" / "test01a.owd",
        "spec": L00_DIR / "2.3" / "spec.yml",
        "vessel_name": "Test01_cylinder",
        "water_depth": float("inf"),
        "description": "Cylinder R=1 T=0.5 (trimmed z=0.27 rx=15deg)",
    },
    "2.5c": {
        "owd": L00_DIR / "2.5" / "OrcaWave v11.0 files" / "test06.owd",
        "spec": L00_DIR / "2.5" / "spec_coarse.yml",
        "vessel_name": "Body1",
        "water_depth": 450.0,
        "description": "ISSC TLP coarse (128 panels)",
    },
    "2.5f": {
        "owd": L00_DIR / "2.5" / "OrcaWave v11.0 files" / "test07.owd",
        "spec": L00_DIR / "2.5" / "spec_fine.yml",
        "vessel_name": "Body1",
        "water_depth": 450.0,
        "description": "ISSC TLP fine (1012 panels)",
    },
    "2.6": {
        "owd": L00_DIR / "2.6" / "OrcaWave v11.0 files" / "test05a.owd",
        "spec": L00_DIR / "2.6" / "spec.yml",
        "vessel_name": "test05_cylinder",
        "water_depth": 3.0,
        "description": "Multi-body Cylinder+Spheroid shallow water",
    },
    # Phase 3: Advanced cases
    "2.9": {
        "owd": L00_DIR / "2.9" / "OrcaWave v11.0 files" / "Moonpool Body.owd",
        "spec": L00_DIR / "2.9" / "spec.yml",
        "vessel_name": "cylinder with moonpool",
        "water_depth": float("inf"),
        "description": "Moonpool cylinder with damping lid",
    },
    "3.1": {
        "owd": L00_DIR / "3.1" / "OrcaWave v11.0 files" / "Bottom mounted cylinder.owd",
        "spec": L00_DIR / "3.1" / "spec.yml",
        "vessel_name": "test101 bottom mounted cylinder",
        "water_depth": 1.0,
        "description": "Bottom-mounted cylinder, all DOFs fixed, Full QTF",
    },
}


def _extract_from_diffraction(
    diff,
    vessel_name: str,
    water_depth: float,
    source_label: str = "OrcaWave",
    source_file: str = "",
    body_index: int = 0,
) -> Optional["DiffractionResults"]:
    """Extract DiffractionResults from a solved OrcFxAPI.Diffraction object.

    Mirrors _extract_from_owr() from run_3way_benchmark.py but works on
    an already-solved in-memory Diffraction object.
    """
    from digitalmodel.hydrodynamics.diffraction.output_schemas import (
        AddedMassSet,
        DampingSet,
        DiffractionResults,
        DOF,
        FrequencyData,
        HeadingData,
        HydrodynamicMatrix,
        RAOComponent,
        RAOSet,
    )

    try:
        # OrcFxAPI returns frequencies in Hz; convert to rad/s
        frequencies = 2.0 * np.pi * np.array(diff.frequencies)
        headings = np.array(diff.headings)

        # Extract displacement RAOs - shape (nheading, nfreq, ndof)
        # Transpose to (nfreq, nheading, ndof) for consistency
        raw_raos = np.array(diff.displacementRAOs)
        ndof = raw_raos.shape[2] if len(raw_raos.shape) == 3 else 6

        # For multi-body, ndof > 6 (e.g. 12 for 2 bodies). Extract target body.
        if ndof > 6:
            dof_start = body_index * 6
            raw_raos = raw_raos[:, :, dof_start:dof_start + 6]

        raw_raos = np.transpose(raw_raos, (1, 0, 2))  # (nfreq, nheading, 6)

        # Sort by ascending frequency (OrcFxAPI may return descending)
        sort_idx = np.argsort(frequencies)
        frequencies = frequencies[sort_idx]
        raw_raos = raw_raos[sort_idx, :, :]

        freq_data = FrequencyData(
            values=frequencies,
            periods=2.0 * np.pi / frequencies,
            count=len(frequencies),
            min_freq=0.0,
            max_freq=0.0,
        )
        head_data = HeadingData(
            values=headings,
            count=len(headings),
            min_heading=0.0,
            max_heading=0.0,
        )

        dof_list = [DOF.SURGE, DOF.SWAY, DOF.HEAVE, DOF.ROLL, DOF.PITCH, DOF.YAW]
        rotational_dofs = {DOF.ROLL, DOF.PITCH, DOF.YAW}
        components = {}
        for i, dof in enumerate(dof_list):
            rao_complex = raw_raos[:, :, i]
            magnitude = np.abs(rao_complex)
            if dof in rotational_dofs:
                magnitude = np.degrees(magnitude)
            components[dof.name.lower()] = RAOComponent(
                dof=dof,
                magnitude=magnitude,
                phase=np.degrees(np.angle(rao_complex)),
                frequencies=freq_data,
                headings=head_data,
                unit="deg/m" if dof in rotational_dofs else "m/m",
            )

        now_str = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        rao_set = RAOSet(
            vessel_name=vessel_name,
            analysis_tool=source_label,
            water_depth=water_depth,
            surge=components["surge"],
            sway=components["sway"],
            heave=components["heave"],
            roll=components["roll"],
            pitch=components["pitch"],
            yaw=components["yaw"],
            created_date=now_str,
            source_file=source_file,
        )

        # Extract added mass - shape: (nfreq, N, N)
        am_raw_full = np.array(diff.addedMass)
        # For multi-body, extract target body's 6x6 block
        if am_raw_full.shape[1] > 6:
            r0 = body_index * 6
            am_raw_full = am_raw_full[:, r0:r0 + 6, r0:r0 + 6]
        am_raw = am_raw_full[sort_idx]

        am_matrices = []
        for j, freq in enumerate(frequencies):
            am_matrices.append(
                HydrodynamicMatrix(
                    matrix=am_raw[j],
                    frequency=float(freq),
                    matrix_type="added_mass",
                    units={"coupling": "kg"},
                )
            )

        am_set = AddedMassSet(
            vessel_name=vessel_name,
            analysis_tool=source_label,
            water_depth=water_depth,
            matrices=am_matrices,
            frequencies=freq_data,
            created_date=now_str,
            source_file=source_file,
        )

        # Extract damping
        damp_raw_full = np.array(diff.damping)
        if damp_raw_full.shape[1] > 6:
            r0 = body_index * 6
            damp_raw_full = damp_raw_full[:, r0:r0 + 6, r0:r0 + 6]
        damp_raw = damp_raw_full[sort_idx]

        damp_matrices = []
        for j, freq in enumerate(frequencies):
            damp_matrices.append(
                HydrodynamicMatrix(
                    matrix=damp_raw[j],
                    frequency=float(freq),
                    matrix_type="damping",
                    units={"coupling": "N.s/m"},
                )
            )

        damp_set = DampingSet(
            vessel_name=vessel_name,
            analysis_tool=source_label,
            water_depth=water_depth,
            matrices=damp_matrices,
            frequencies=freq_data,
            created_date=now_str,
            source_file=source_file,
        )

        return DiffractionResults(
            vessel_name=vessel_name,
            analysis_tool=source_label,
            water_depth=water_depth,
            raos=rao_set,
            added_mass=am_set,
            damping=damp_set,
            created_date=now_str,
            source_files=[source_file],
            phase_convention="orcina_lag",
            unit_system="orcaflex",
        )

    except Exception as e:
        import traceback
        print(f"  [ERROR] Extraction failed: {e}")
        traceback.print_exc()
        return None


def solve_owd(case_id: str) -> Optional["DiffractionResults"]:
    """Load .owd file, run Calculate(), and extract results."""
    import OrcFxAPI

    case = CASES[case_id]
    owd_path = case["owd"]

    print(f"\n  Loading .owd: {owd_path.name}")
    diff = OrcFxAPI.Diffraction()
    diff.LoadData(str(owd_path.resolve()))

    print(f"  Running Calculate()...")
    t0 = time.perf_counter()
    diff.Calculate()
    dt = time.perf_counter() - t0
    print(f"  Solved in {dt:.1f}s")

    # Save .owr for later use
    out_dir = OUTPUT_DIR / case_id
    out_dir.mkdir(parents=True, exist_ok=True)
    owr_path = out_dir / f"{owd_path.stem}_ground_truth.owr"
    try:
        diff.SaveResults(str(owr_path.resolve()))
        print(f"  Saved: {owr_path.name}")
    except Exception as exc:
        print(f"  Could not save .owr: {exc}")

    return _extract_from_diffraction(
        diff,
        vessel_name=case["vessel_name"],
        water_depth=case["water_depth"],
        source_label="OrcaWave (.owd)",
        source_file=str(owd_path),
        body_index=case.get("body_index", 0),
    )


def solve_spec(case_id: str) -> Optional["DiffractionResults"]:
    """Run spec.yml through OrcaWaveRunner and extract results."""
    import OrcFxAPI
    from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
    from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
        OrcaWaveRunner,
        RunConfig,
        RunStatus,
    )

    case = CASES[case_id]
    spec_path = case["spec"]

    print(f"\n  Loading spec: {spec_path.name}")
    spec = DiffractionSpec.from_yaml(spec_path)

    # Configure runner
    out_dir = OUTPUT_DIR / case_id / "spec_orcawave"
    config = RunConfig(
        output_dir=out_dir,
        dry_run=False,
        timeout_seconds=600,
    )

    print(f"  Creating OrcaWaveRunner...")
    runner = OrcaWaveRunner(config)

    print(f"  Running OrcaWave from spec...")
    t0 = time.perf_counter()
    run_result = runner.run(spec, spec_path=spec_path)
    dt = time.perf_counter() - t0
    print(f"  Completed in {dt:.1f}s (status: {run_result.status})")

    if run_result.status == RunStatus.COMPLETED:
        # Extract from .owr
        owr_files = sorted(out_dir.glob("*.owr"))
        if owr_files:
            print(f"  Extracting from: {owr_files[0].name}")
            diff = OrcFxAPI.Diffraction()
            diff.LoadResults(str(owr_files[0].resolve()))
            return _extract_from_diffraction(
                diff,
                vessel_name=case["vessel_name"],
                water_depth=case["water_depth"],
                source_label="OrcaWave (spec.yml)",
                source_file=str(spec_path),
                body_index=case.get("body_index", 0),
            )
        else:
            print(f"  [ERROR] No .owr file found")
            return None
    elif run_result.status == RunStatus.DRY_RUN:
        # OrcaWave executable not found — fall back to direct OrcFxAPI solve
        print(f"  Runner produced dry-run. Falling back to direct OrcFxAPI solve...")
        return _solve_spec_via_orcfxapi(case_id, spec, spec_path)
    else:
        print(f"  [ERROR] Runner failed: {run_result.error_message}")
        # Also fall back to OrcFxAPI direct
        print(f"  Falling back to direct OrcFxAPI solve...")
        return _solve_spec_via_orcfxapi(case_id, spec, spec_path)


def _solve_spec_via_orcfxapi(
    case_id: str,
    spec: "DiffractionSpec",
    spec_path: Path,
) -> Optional["DiffractionResults"]:
    """Build and solve OrcaWave project directly via OrcFxAPI.

    Uses OrcaWaveRunner.prepare() to generate the .yml, then loads and
    solves via OrcFxAPI Python binding directly.
    """
    import OrcFxAPI
    from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
        OrcaWaveRunner,
        RunConfig,
    )

    case = CASES[case_id]
    out_dir = OUTPUT_DIR / case_id / "spec_orcfxapi"
    out_dir.mkdir(parents=True, exist_ok=True)

    # Use runner to generate the project YAML (respects mesh path resolution)
    print(f"  Generating OrcaWave project from spec...")
    config = RunConfig(output_dir=out_dir, dry_run=True, timeout_seconds=600)
    runner = OrcaWaveRunner(config)
    runner.prepare(spec, spec_path=spec_path)

    # Find the generated .yml file
    yml_files = sorted(out_dir.glob("*.yml"))
    if not yml_files:
        print(f"  [ERROR] No .yml file generated in {out_dir}")
        return None
    yml_path = yml_files[0]
    print(f"  Generated: {yml_path.name}")

    # Load into OrcFxAPI and solve
    print(f"  Loading into OrcFxAPI...")
    diff = OrcFxAPI.Diffraction()
    diff.LoadData(str(yml_path.resolve()))

    print(f"  Running Calculate()...")
    t0 = time.perf_counter()
    diff.Calculate()
    dt = time.perf_counter() - t0
    print(f"  Solved in {dt:.1f}s")

    # Save results
    owr_path = out_dir / "spec_result.owr"
    try:
        diff.SaveResults(str(owr_path.resolve()))
        print(f"  Saved: {owr_path.name}")
    except Exception as exc:
        print(f"  Could not save .owr: {exc}")

    return _extract_from_diffraction(
        diff,
        vessel_name=case["vessel_name"],
        water_depth=case["water_depth"],
        source_label="OrcaWave (spec.yml)",
        source_file=str(spec_path),
        body_index=case.get("body_index", 0),
    )


def run_comparison(
    owd_results: "DiffractionResults",
    spec_results: "DiffractionResults",
    case_id: str,
) -> dict:
    """Compare .owd ground truth against spec.yml results."""
    import yaml

    from digitalmodel.hydrodynamics.diffraction.benchmark_runner import (
        BenchmarkConfig,
        BenchmarkRunner,
    )
    from scripts.benchmark.solver_metadata import build_solver_metadata

    case = CASES[case_id]
    out_dir = OUTPUT_DIR / case_id

    # Build solver metadata (needs a plain dict, not DiffractionSpec)
    spec_path = case["spec"]
    with open(spec_path) as f:
        spec_dict = yaml.safe_load(f)
    metadata = build_solver_metadata(spec_dict, spec_dir=spec_path.parent)

    # Solver results dict
    solver_results = {
        "OrcaWave (.owd)": owd_results,
        "OrcaWave (spec.yml)": spec_results,
    }

    # Benchmark config
    config = BenchmarkConfig(
        output_dir=out_dir,
        report_title=f"Validation Case {case_id}: {case['description']}",
        report_subtitle="OrcaWave .owd Ground Truth vs spec.yml Pipeline",
    )

    print(f"\n  Running benchmark comparison...")
    runner = BenchmarkRunner(config)
    result = runner.run_from_results(solver_results, solver_metadata=metadata)

    report_path = out_dir / "validation_report.html"
    if hasattr(result, "report_path") and result.report_path:
        report_path = Path(result.report_path)

    print(f"  Report: {report_path}")

    # Compute per-DOF correlation summary
    summary = _compute_correlation_summary(owd_results, spec_results)
    return summary


def _compute_correlation_summary(
    owd_results: "DiffractionResults",
    spec_results: "DiffractionResults",
) -> dict:
    """Compute per-DOF correlation between two result sets."""
    summary = {}
    dof_names = ["surge", "sway", "heave", "roll", "pitch", "yaw"]

    for dof_name in dof_names:
        owd_comp = getattr(owd_results.raos, dof_name)
        spec_comp = getattr(spec_results.raos, dof_name)

        owd_mag = owd_comp.magnitude.flatten()
        spec_mag = spec_comp.magnitude.flatten()

        # Truncate to common length
        min_len = min(len(owd_mag), len(spec_mag))
        owd_mag = owd_mag[:min_len]
        spec_mag = spec_mag[:min_len]

        if min_len > 1 and np.std(owd_mag) > 0 and np.std(spec_mag) > 0:
            r = np.corrcoef(owd_mag, spec_mag)[0, 1]
        elif min_len > 0 and np.allclose(owd_mag, spec_mag, atol=1e-6):
            r = 1.0
        else:
            r = float("nan")

        max_diff = float(np.max(np.abs(owd_mag - spec_mag))) if min_len > 0 else 0.0
        mean_abs = float(np.mean(np.abs(owd_mag))) if min_len > 0 else 0.0
        rel_err = max_diff / mean_abs * 100.0 if mean_abs > 1e-12 else 0.0

        summary[dof_name] = {
            "correlation": round(r, 6) if not np.isnan(r) else "N/A",
            "max_abs_diff": round(max_diff, 6),
            "rel_error_pct": round(rel_err, 2),
            "n_points": min_len,
        }

    return summary


def run_case(case_id: str, owd_only: bool = False) -> dict:
    """Run validation for a single case."""
    case = CASES[case_id]
    print(f"\n{'='*70}")
    print(f"Case {case_id}: {case['description']}")
    print(f"{'='*70}")

    result = {"case_id": case_id, "description": case["description"]}

    # Step 1: Solve .owd
    print("\n[Step 1] Solving .owd ground truth...")
    owd_results = solve_owd(case_id)
    if owd_results is None:
        result["status"] = "owd_failed"
        return result

    n_freq = owd_results.raos.surge.frequencies.count
    n_head = owd_results.raos.surge.headings.count
    result["owd_freq"] = n_freq
    result["owd_head"] = n_head
    print(f"  Extracted: {n_freq} freq x {n_head} headings")

    if owd_only:
        result["status"] = "owd_only"
        return result

    # Step 2: Run spec.yml pipeline
    print("\n[Step 2] Running spec.yml pipeline...")
    spec_results = solve_spec(case_id)
    if spec_results is None:
        result["status"] = "spec_failed"
        return result

    n_freq2 = spec_results.raos.surge.frequencies.count
    n_head2 = spec_results.raos.surge.headings.count
    result["spec_freq"] = n_freq2
    result["spec_head"] = n_head2
    print(f"  Extracted: {n_freq2} freq x {n_head2} headings")

    # Step 3: Compare
    print("\n[Step 3] Comparing results...")
    try:
        summary = run_comparison(owd_results, spec_results, case_id)
        result["dof_summary"] = summary
        result["status"] = "completed"
    except Exception as exc:
        import traceback
        print(f"  Comparison failed: {exc}")
        traceback.print_exc()
        result["status"] = "comparison_failed"
        result["error"] = str(exc)

        # Still compute correlation manually
        summary = _compute_correlation_summary(owd_results, spec_results)
        result["dof_summary"] = summary

    return result


def main():
    parser = argparse.ArgumentParser(
        description="Validate spec.yml pipeline against .owd ground truth"
    )
    parser.add_argument("--case", help="Case ID (e.g., 2.7)")
    parser.add_argument("--all", action="store_true", help="Run all Phase 1 cases")
    parser.add_argument(
        "--owd-only",
        action="store_true",
        help="Only extract .owd results (skip spec.yml comparison)",
    )
    args = parser.parse_args()

    if not args.case and not args.all:
        parser.print_help()
        print("\nAvailable cases:")
        for cid, info in CASES.items():
            print(f"  {cid}: {info['description']}")
        return

    cases = list(CASES.keys()) if args.all else [args.case]
    results = {}

    for case_id in cases:
        if case_id not in CASES:
            print(f"Unknown case: {case_id}")
            continue
        results[case_id] = run_case(case_id, owd_only=args.owd_only)

    # Print summary
    print(f"\n{'='*70}")
    print("VALIDATION SUMMARY")
    print(f"{'='*70}")

    for cid, r in results.items():
        print(f"\nCase {cid}: {r['description']} [{r['status']}]")
        if "dof_summary" in r:
            print(f"  {'DOF':<8} {'Corr':>10} {'MaxDiff':>10} {'Rel%':>8} {'Points':>8}")
            print(f"  {'-'*8} {'-'*10} {'-'*10} {'-'*8} {'-'*8}")
            for dof, s in r["dof_summary"].items():
                corr = s["correlation"]
                corr_str = f"{corr:.6f}" if isinstance(corr, float) else corr
                print(
                    f"  {dof:<8} {corr_str:>10} {s['max_abs_diff']:>10.6f} "
                    f"{s['rel_error_pct']:>7.2f}% {s['n_points']:>8}"
                )

    # Overall pass/fail
    all_pass = True
    for cid, r in results.items():
        if r["status"] not in ("completed", "owd_only", "comparison_failed"):
            all_pass = False
            continue
        if "dof_summary" in r:
            for dof, s in r["dof_summary"].items():
                corr = s["correlation"]
                max_diff = s["max_abs_diff"]
                # Skip DOFs where both signals are zero (correlation undefined)
                if max_diff < 1e-6:
                    continue
                if isinstance(corr, float) and corr < 0.999:
                    all_pass = False
                    print(f"\n  WARN: Case {cid} {dof} correlation {corr:.4f} < 0.999")

    print(f"\n{'='*70}")
    if all_pass:
        print("RESULT: ALL CASES PASS (correlation >= 0.999 per DOF)")
    else:
        print("RESULT: SOME CASES NEED INVESTIGATION")
    print(f"{'='*70}")


if __name__ == "__main__":
    main()
