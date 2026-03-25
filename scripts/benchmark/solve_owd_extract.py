#!/usr/bin/env python3
"""Solve .owd files and extract OrcaWave results + inspect for WAMIT data.

Phase 1 data extraction: runs OrcaWave Calculate() on pre-configured .owd
projects, then inspects all available results to determine what data types
(RAOs, mean drift, QTFs, added mass, damping) are accessible.

Usage:
    uv run python scripts/benchmark/solve_owd_extract.py --case 2.7
    uv run python scripts/benchmark/solve_owd_extract.py --all
    uv run python scripts/benchmark/solve_owd_extract.py --case 2.7 --dry-run
"""
from __future__ import annotations

import argparse
import io
import json
import sys
import time
from pathlib import Path
from typing import Any

# Fix Windows console encoding
if sys.platform == "win32":
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")
    sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding="utf-8")


REPO_ROOT = Path(__file__).parent.parent.parent
L00_DIR = REPO_ROOT / "docs" / "modules" / "orcawave" / "L00_validation_wamit"
OUTPUT_DIR = REPO_ROOT / "benchmark_output" / "validation"

# Phase 1 cases with their .owd file paths
PHASE1_CASES = {
    "2.7": {
        "owd": L00_DIR / "2.7" / "OrcaWave v11.0 files" / "Pyramid.owd",
        "description": "Inverted Pyramid - CS and PI Mean Drift Loads",
    },
    "2.8": {
        "owd": L00_DIR / "2.8" / "OrcaWave v11.0 files" / "Ellipsoid.owd",
        "description": "Ellipsoid - CS Mean Drift Load Convergence",
    },
    "3.2": {
        "owd": L00_DIR / "3.2" / "OrcaWave v11.0 files" / "Sphere.owd",
        "description": "Sphere with Lid - Difference-Frequency QTFs",
    },
    "3.3": {
        "owd": L00_DIR / "3.3" / "OrcaWave v11.0 files" / "test.owd",
        "description": "Multi-body Cylinder + Ellipsoid - Full QTFs",
    },
}

# Result attributes to probe on a solved Diffraction object
HYDRO_ATTRS = [
    "addedMass",
    "addedMassInfFreq",
    "damping",
    "displacementRAOs",
    "velocityRAOs",
    "accelerationRAOs",
    "excitationForceRAOs",
    "haskindForceRAOs",
    "meanDriftLoads",
    "meanDriftLoadsPressureIntegration",
    "meanDriftLoadsControlSurface",
    "qtfs",
    "qtfDifferenceFrequency",
    "qtfSumFrequency",
    "quadraticTransferFunctions",
    "driftForces",
    "waveExciting",
    "waveExcitation",
    "responseAmplitudeOperators",
    "motionRAOs",
    "forceRAOs",
    "hydrostaticStiffness",
    "bodyMass",
    "bodyInertia",
    "wetSurfaceArea",
    "displacedVolume",
    "centreOfBuoyancy",
    "waterplaneArea",
    "waterplaneCentroid",
]


def _safe_get(obj: Any, attr: str) -> tuple[bool, Any]:
    """Safely get an attribute, returning (success, value_or_error)."""
    try:
        val = getattr(obj, attr)
        if callable(val):
            val = val()
        return True, val
    except Exception as exc:
        return False, str(exc)


def _describe_value(val: Any) -> str:
    """Describe a value's type/shape for reporting."""
    if hasattr(val, "shape"):
        return f"ndarray shape={val.shape}"
    if isinstance(val, (list, tuple)):
        n = len(val)
        if n > 0 and isinstance(val[0], (list, tuple)):
            return f"list[list] shape=({n}, {len(val[0])})"
        if n > 0 and hasattr(val[0], "__len__"):
            return f"list[array] len={n}, inner_len={len(val[0])}"
        return f"list len={n}"
    if isinstance(val, (int, float)):
        return f"{type(val).__name__} = {val}"
    if isinstance(val, str):
        return f"str = {val!r}"
    return type(val).__name__


def solve_and_extract(case_id: str, dry_run: bool = False) -> dict:
    """Solve an .owd file and extract all available results."""
    import OrcFxAPI

    case = PHASE1_CASES[case_id]
    owd_path = case["owd"]
    result = {
        "case_id": case_id,
        "description": case["description"],
        "owd_path": str(owd_path),
        "owd_exists": owd_path.exists(),
    }

    if not owd_path.exists():
        result["error"] = f"OWD file not found: {owd_path}"
        return result

    if dry_run:
        result["dry_run"] = True
        result["status"] = "would_solve"
        return result

    # Load the project
    print(f"\n{'='*70}")
    print(f"Case {case_id}: {case['description']}")
    print(f"{'='*70}")
    print(f"Loading: {owd_path.name}")

    try:
        diff = OrcFxAPI.Diffraction()
        diff.LoadData(str(owd_path.resolve()))
        result["load_success"] = True
    except Exception as exc:
        result["load_success"] = False
        result["load_error"] = str(exc)
        return result

    # Check pre-solve state
    ok, state = _safe_get(diff, "state")
    result["pre_solve_state"] = state if ok else f"<error: {state}>"
    print(f"Pre-solve state: {result['pre_solve_state']}")

    # Inspect pre-solve: number of bodies, frequencies, headings
    for attr in ["bodyCount", "frequencyCount", "headingCount"]:
        ok, val = _safe_get(diff, attr)
        if ok:
            result[f"pre_{attr}"] = val
            print(f"  {attr}: {val}")

    # Run Calculate
    print(f"Running OrcaWave Calculate()...")
    t0 = time.perf_counter()
    try:
        diff.Calculate()
        solve_time = time.perf_counter() - t0
        result["calculate_success"] = True
        result["solve_time_s"] = round(solve_time, 2)
        print(f"  Solved in {solve_time:.1f}s")
    except Exception as exc:
        solve_time = time.perf_counter() - t0
        result["calculate_success"] = False
        result["calculate_error"] = str(exc)
        result["solve_time_s"] = round(solve_time, 2)
        print(f"  Calculate FAILED after {solve_time:.1f}s: {exc}")
        return result

    # Check post-solve state
    ok, state = _safe_get(diff, "state")
    result["post_solve_state"] = state if ok else f"<error: {state}>"
    print(f"Post-solve state: {result['post_solve_state']}")

    # Probe all hydrodynamic attributes
    print(f"\nProbing {len(HYDRO_ATTRS)} hydrodynamic attributes:")
    available = {}
    unavailable = {}

    for attr in HYDRO_ATTRS:
        ok, val = _safe_get(diff, attr)
        if ok:
            desc = _describe_value(val)
            available[attr] = desc
            print(f"  [OK] {attr}: {desc}")
        else:
            unavailable[attr] = val
            # Only print first few to avoid noise
            if len(unavailable) <= 5:
                short_err = val[:80] if len(val) > 80 else val
                print(f"  [--] {attr}: {short_err}")

    if len(unavailable) > 5:
        print(f"  ... and {len(unavailable) - 5} more unavailable attributes")

    result["available_attrs"] = available
    result["unavailable_count"] = len(unavailable)
    result["unavailable_attrs"] = unavailable

    # Deep probe: try to extract actual numeric data from available attrs
    print(f"\nExtracting numeric data from {len(available)} available attributes:")
    extracted_data = {}

    for attr in available:
        ok, val = _safe_get(diff, attr)
        if not ok:
            continue

        try:
            if hasattr(val, "tolist"):
                # numpy array
                data = val.tolist()
                extracted_data[attr] = {
                    "type": "ndarray",
                    "shape": list(val.shape),
                    "sample": data[:3] if len(data) > 3 else data,
                }
                print(f"  {attr}: ndarray {val.shape}")
            elif isinstance(val, (list, tuple)) and len(val) > 0:
                if isinstance(val[0], (int, float)):
                    extracted_data[attr] = {
                        "type": "list_numeric",
                        "length": len(val),
                        "sample": list(val[:5]),
                    }
                    print(f"  {attr}: list[{len(val)}] = {val[:3]}...")
                elif isinstance(val[0], (list, tuple)):
                    extracted_data[attr] = {
                        "type": "list_of_lists",
                        "outer": len(val),
                        "inner": len(val[0]) if val else 0,
                    }
                    print(f"  {attr}: list[{len(val)}][{len(val[0])}]")
                else:
                    extracted_data[attr] = {
                        "type": f"list_of_{type(val[0]).__name__}",
                        "length": len(val),
                    }
                    print(f"  {attr}: list[{type(val[0]).__name__}] len={len(val)}")
            elif isinstance(val, (int, float)):
                extracted_data[attr] = {"type": "scalar", "value": val}
                print(f"  {attr}: {val}")
            else:
                extracted_data[attr] = {"type": type(val).__name__}
                print(f"  {attr}: {type(val).__name__}")
        except Exception as exc:
            print(f"  {attr}: extraction error: {exc}")

    result["extracted_data"] = extracted_data

    # Try to discover ALL non-dunder attributes on the solved object
    all_attrs = sorted(
        a for a in dir(diff) if not a.startswith("_")
    )
    # Filter to those not in our HYDRO_ATTRS list (unknown/new)
    unknown_attrs = [a for a in all_attrs if a not in HYDRO_ATTRS]
    result["total_attrs"] = len(all_attrs)
    result["unknown_attrs_count"] = len(unknown_attrs)

    # Probe unknown attrs for data
    interesting_unknown = {}
    for attr in unknown_attrs:
        ok, val = _safe_get(diff, attr)
        if ok and not callable(val):
            desc = _describe_value(val)
            if any(kw in attr.lower() for kw in [
                "drift", "qtf", "mean", "transfer", "second", "wave",
                "force", "moment", "comparison", "reference", "wamit",
            ]):
                interesting_unknown[attr] = desc

    if interesting_unknown:
        print(f"\nInteresting unknown attributes ({len(interesting_unknown)}):")
        for attr, desc in sorted(interesting_unknown.items()):
            print(f"  {attr}: {desc}")
    result["interesting_unknown"] = interesting_unknown

    # Save solved .owd for later extraction
    solved_dir = OUTPUT_DIR / case_id
    solved_dir.mkdir(parents=True, exist_ok=True)
    solved_path = solved_dir / f"{owd_path.stem}_solved.owd"
    try:
        diff.SaveData(str(solved_path.resolve()))
        result["saved_solved"] = str(solved_path)
        print(f"\nSaved solved project: {solved_path}")
    except Exception as exc:
        result["save_error"] = str(exc)
        print(f"\nFailed to save solved project: {exc}")

    return result


def main():
    parser = argparse.ArgumentParser(
        description="Solve .owd files and extract OrcaWave results"
    )
    parser.add_argument("--case", help="Case ID (e.g., 2.7)")
    parser.add_argument("--all", action="store_true", help="Run all Phase 1 cases")
    parser.add_argument(
        "--dry-run", action="store_true", help="Check files without solving"
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=OUTPUT_DIR,
        help="Output directory for results",
    )
    args = parser.parse_args()

    if not args.case and not args.all:
        parser.print_help()
        print("\nAvailable cases:")
        for cid, info in PHASE1_CASES.items():
            exists = "EXISTS" if info["owd"].exists() else "MISSING"
            print(f"  {cid}: {info['description']} [{exists}]")
        return

    cases = list(PHASE1_CASES.keys()) if args.all else [args.case]
    results = {}

    for case_id in cases:
        if case_id not in PHASE1_CASES:
            print(f"Unknown case: {case_id}")
            continue
        result = solve_and_extract(case_id, dry_run=args.dry_run)
        results[case_id] = result

    # Save results JSON
    args.output_dir.mkdir(parents=True, exist_ok=True)
    out_path = args.output_dir / "owd_solve_results.json"
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(results, f, indent=2, default=str)
    print(f"\nResults saved: {out_path}")

    # Summary
    print(f"\n{'='*70}")
    print("SUMMARY")
    print(f"{'='*70}")
    print(f"{'Case':<8} {'Status':<12} {'Time':<8} {'Avail Attrs':<12} {'Notes'}")
    print(f"{'-'*8} {'-'*12} {'-'*8} {'-'*12} {'-'*30}")
    for cid, r in results.items():
        if r.get("dry_run"):
            status = "DRY_RUN"
            time_s = "-"
            n_attrs = "-"
            notes = r.get("description", "")
        elif r.get("calculate_success"):
            status = "SOLVED"
            time_s = f"{r.get('solve_time_s', 0):.1f}s"
            n_attrs = str(len(r.get("available_attrs", {})))
            notes = ", ".join(list(r.get("available_attrs", {}).keys())[:3])
        elif r.get("calculate_success") is False:
            status = "FAILED"
            time_s = f"{r.get('solve_time_s', 0):.1f}s"
            n_attrs = "0"
            notes = r.get("calculate_error", "")[:40]
        else:
            status = "ERROR"
            time_s = "-"
            n_attrs = "0"
            notes = r.get("error", r.get("load_error", ""))[:40]
        print(f"{cid:<8} {status:<12} {time_s:<8} {n_attrs:<12} {notes}")


if __name__ == "__main__":
    main()
