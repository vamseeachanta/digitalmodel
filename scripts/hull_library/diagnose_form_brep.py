"""Bounded synthetic BRep experiment; no client inputs or physical validation.

Run with --output DIRECTORY. Each assessment runs in a hidden, time-bounded
child process. JSON records use null for unavailable numbers and preserve native
statuses. This diagnostic does not certify the candidate as a qualified repair.
"""

import argparse
import json
import os
import subprocess
import sys
import time
import traceback
from datetime import UTC, datetime
from pathlib import Path

import numpy as np

from digitalmodel.hydrodynamics.hull_library import (
    MonohullFormParameters,
    generate_profile,
)
from digitalmodel.hydrodynamics.hull_library.curvature_screen import screen_step
from digitalmodel.hydrodynamics.hull_library.hull_surface_brep import profile_to_step


def synthetic_profile(case):
    extra = (
        {}
        if case == "rounded"
        else {"cb": 0.72, "transom_fraction": 0.9, "lcb_fraction": -0.1}
    )
    return generate_profile(
        MonohullFormParameters(length_bp=100, beam=20, draft=8, depth=12, **extra)
    )


def native_acceptance(signature, metadata):
    """Necessary native gates only; passing is not full geometry qualification."""
    errors = []
    metrics = metadata.get("metric_validity", {})
    for name in ("developability_deviation", "curvature_classes", "surface_area"):
        if metrics.get(name, {}).get("status") != "valid":
            errors.append(f"{name}: status is not valid")
    fractions = [metadata.get("curvature_valid_area_fraction")]
    fractions += [
        metrics.get(name, {}).get("valid_area_fraction")
        for name in ("developability_deviation", "curvature_classes")
    ]
    if any(v is None or not np.isfinite(v) or v < 1 - 1e-6 for v in fractions):
        errors.append("missing or incomplete valid-area coverage")
    values = signature.as_vector()
    if not np.isfinite(values).all():
        errors.append("nonfinite signature")
    if (
        abs(
            sum(
                getattr(signature, f"a_{key}")
                for key in ("flat", "single", "elliptic", "saddle")
            )
            - 1
        )
        > 1e-6
    ):
        errors.append("class fractions do not sum to one")
    return errors


def clean_json(value):
    if isinstance(value, dict):
        return {key: clean_json(item) for key, item in value.items()}
    if isinstance(value, (list, tuple, np.ndarray)):
        return [clean_json(item) for item in value]
    if isinstance(value, (float, np.floating)):
        return float(value) if np.isfinite(value) else None
    if isinstance(value, np.integer):
        return int(value)
    return value


def assess_case(args):
    profile = synthetic_profile(args.case)
    path = args.output.with_suffix(".step")
    profile_to_step(
        profile,
        path,
        n_x=args.nx,
        n_z=args.nz,
        sampling=args.sampling,
        mirror=not args.side_only,
        bottom=not args.side_only,
    )
    result = screen_step(path, lref=profile.length_bp)
    metadata = result.provenance
    keys = (
        "metric_validity",
        "curvature_valid_area_fraction",
        "quadrature",
        "regularity",
        "brep_regularity",
        "surface_area",
        "hullprod_version",
    )
    return {
        "case": args.case,
        "sampling": args.sampling,
        "grid": [args.nx, args.nz],
        "side_only": args.side_only,
        "comparator_class": "archived-run",
        "signature": result.signature.model_dump(),
        "native": {key: metadata[key] for key in keys if key in metadata},
        "native_gate_failures": native_acceptance(result.signature, metadata),
        "qualified_repair": False,
        "qualification_note": "Native gates are necessary, not sufficient.",
        "native_assessment_completed": True,
    }


def worker(args):
    try:
        record = assess_case(args)
    except (ValueError, RuntimeError, OSError) as error:
        # Exception text can include private paths; keep type only in public JSON.
        record = {
            "case": args.case,
            "sampling": args.sampling,
            "grid": [args.nx, args.nz],
            "side_only": args.side_only,
            "error_type": type(error).__name__,
            "qualified_repair": False,
        }
        traceback.print_exc()
    record["timestamp"] = datetime.now(UTC).isoformat()
    args.output.write_text(
        json.dumps(clean_json(record), indent=2, allow_nan=False), encoding="utf-8"
    )
    return int("error_type" in record)


def run_child(args, case, nx, nz, sampling, side_only):
    name = f"{case}-{sampling}-{nx}x{nz}" + ("-side" if side_only else "")
    output = args.output / f"{name}.json"
    command = [
        sys.executable,
        str(Path(__file__).resolve()),
        "--worker",
        "--case",
        case,
        "--nx",
        str(nx),
        "--nz",
        str(nz),
        "--sampling",
        sampling,
        "--output",
        str(output),
    ]
    if side_only:
        command.append("--side-only")
    flags = subprocess.CREATE_NO_WINDOW if os.name == "nt" else 0
    with output.with_suffix(".log").open("w", encoding="utf-8") as log:
        try:
            run = subprocess.run(
                command,
                stdout=log,
                stderr=log,
                timeout=120,
                creationflags=flags,
                check=False,
            )
            return {"record": output.name, "exit_code": run.returncode}
        except subprocess.TimeoutExpired:
            return {"record": output.name, "timeout_seconds": 120}


def batch(args):
    args.output.mkdir(parents=True, exist_ok=False)
    start = time.monotonic()
    ledger = {
        "prior_native_calls": 2 if args.prior_complete_red else 0,
        "prior_calls": (
            "two default complete RED tests" if args.prior_complete_red else "none"
        ),
        "assessment_limit": 20,
        "calls": [],
    }
    jobs = [(case, 41, 21, "uniform_z", True) for case in ("rounded", "transom")]
    if not args.prior_complete_red:
        jobs += [(case, 41, 21, "uniform_z", False) for case in ("rounded", "transom")]
    jobs += [
        (case, nx, nz, "section_arclength", False)
        for case in ("rounded", "transom")
        for nx, nz in ((41, 21), (81, 41), (161, 81))
    ]
    for job in jobs:
        if time.monotonic() - start > 45 * 60 - 120:
            ledger["stopped"] = "batch time limit"
            break
        ledger["calls"].append(run_child(args, *job))
        (args.output / "ledger.json").write_text(
            json.dumps(ledger, indent=2), encoding="utf-8"
        )
        print(ledger["calls"][-1], flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--worker", action="store_true")
    parser.add_argument("--case", choices=("rounded", "transom"), default="rounded")
    parser.add_argument(
        "--sampling", choices=("uniform_z", "section_arclength"), default="uniform_z"
    )
    parser.add_argument("--nx", type=int, default=41)
    parser.add_argument("--nz", type=int, default=21)
    parser.add_argument("--side-only", action="store_true")
    parser.add_argument(
        "--prior-complete-red",
        action="store_true",
        help="Account for two complete default RED assessments already run.",
    )
    args = parser.parse_args()
    return worker(args) if args.worker else batch(args)


if __name__ == "__main__":
    raise SystemExit(main())
