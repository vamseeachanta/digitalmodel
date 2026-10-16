# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Reproduce homogeneous static lines; do not infer environmental response."""
from __future__ import annotations

import argparse
import csv
import hashlib
import itertools
import json
import math
import sys
from datetime import datetime
from pathlib import Path

from digitalmodel.marine_ops.marine_analysis.catenary.simplified import (
    SimplifiedCatenarySolver,
)

RECORD = Path(__file__).resolve().parent
ROOT = RECORD.parents[3]


def digest(path: Path) -> str:
    """Hash file bytes without path-dependent metadata."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def write_json(path: Path, value: dict) -> None:
    """Write deterministic JSON, rejecting non-finite results."""
    path.write_text(json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + "\n",
                    encoding="utf-8")


def solve_static(force_n: float, weight_n_m: float, depth_m: float) -> dict:
    """Return the native line geometry and recomputed fairlead resultant."""
    if not all(math.isfinite(x) for x in (force_n, weight_n_m, depth_m)):
        raise ValueError("Static inputs must be finite")
    native = SimplifiedCatenarySolver().solve_from_force(
        force=force_n, weight_per_length=weight_n_m, vertical_distance=depth_m,
    )
    return {
        "suspended_length_m": native.arc_length,
        "suspended_horizontal_span_m": native.horizontal_distance,
        "horizontal_tension_n": native.horizontal_tension,
        "vertical_tension_n": native.weight_suspended,
        "fairlead_tension_n": math.hypot(native.horizontal_tension, native.weight_suspended),
    }


def validate_evidence(criteria_path: Path, receipt: dict) -> None:
    """Require the frozen criteria bytes and their pre-run timestamp."""
    if receipt["criteria_sha256"] != digest(criteria_path):
        raise ValueError("Criteria digest mismatch")
    frozen = datetime.fromisoformat(receipt["criteria_recorded_at_utc"])
    started = datetime.fromisoformat(receipt["first_run_started_at_utc"])
    if frozen.tzinfo is None or started.tzinfo is None or frozen >= started:
        raise ValueError("Criteria must be recorded before the first run")


def verify_sources(basis: dict, root: Path = ROOT) -> None:
    """Fail closed when a source or computation module differs from the pin."""
    for relative, expected in basis["source_files"].items():
        path = Path(relative)
        if path.is_absolute() or ".." in path.parts:
            raise ValueError("Source paths must remain repository relative")
        if digest(root / path) != expected:
            raise ValueError(f"Source digest mismatch: {relative}")


def line_solution(depth: float, weight: float, mbl_n: float, force_ratio: float,
                  basis: dict, criteria: dict) -> dict:
    """Qualify one static line against mathematical closure and length criteria."""
    target = force_ratio * weight * depth
    result = solve_static(target, weight, depth)
    total_length = basis["assumptions"]["length_to_depth_ratio"] * depth
    grounded = total_length - result["suspended_length_m"]
    error = abs(result["fairlead_tension_n"] - target)
    checks = {
        "positive_horizontal_tension": result["horizontal_tension_n"] > 0,
        "suspended_length_within_total": grounded >= 0,
        "resultant_matches_prescribed_force": error <= criteria["resultant_tolerance_n"],
    }
    if not all(checks.values()):
        raise ValueError(f"Static geometry criterion failed at depth {depth}")
    return {
        "id": f"depth-{depth}", "water_depth_m": depth,
        "input": {"target_force_n": target, "weight_n_m": weight, "total_length_m": total_length},
        "native": result, "grounded_length_m": grounded,
        "anchor_radius_m": result["suspended_horizontal_span_m"] + grounded,
        "empirical_mbl_n": mbl_n,
        "provided_static_mbl_to_tension": mbl_n / result["fairlead_tension_n"],
        "required_standard_factor": None, "standards_disposition": "not established",
        "resultant_error_n": error, "checks": checks,
    }


def compute_record(basis: dict, criteria: dict) -> dict:
    """Run five independent lines; retain all matrix labels without qualification."""
    verify_sources(basis)
    loaded = Path(sys.modules[SimplifiedCatenarySolver.__module__].__file__).resolve()
    expected = (ROOT / basis["solver_module"]).resolve()
    if loaded != expected or digest(loaded) != basis["source_files"][basis["solver_module"]]:
        raise ValueError("Imported module digest mismatch")
    with (ROOT / basis["wire_table"]).open(encoding="utf-8", newline="") as stream:
        rows = list(csv.DictReader(stream))
    row = next(r for r in rows if float(r["diameter"]) == basis["assumptions"]["wire_diameter_mm"])
    weight = float(row["weight_water"]) * basis["assumptions"]["gravity_m_s2"]
    mbl_n = float(row["mbl"]) * basis["assumptions"]["n_per_kn"]
    reference = basis["assumptions"]["force_reference"]
    force_ratio = reference["force_n"] / (reference["weight_n_m"] * reference["depth_m"])
    solutions = [line_solution(depth, weight, mbl_n, force_ratio, basis, criteria)
                 for depth in basis["matrix"]["water_depths_m"]]
    matrix = []
    for platform, concept, depth, state in itertools.product(
        basis["matrix"]["platforms"], basis["matrix"]["concepts"],
        basis["matrix"]["water_depths_m"], basis["matrix"]["sea_states"],
    ):
        supported = concept in basis["static_concepts"]
        matrix.append({
            "platform": platform, "concept": concept, "water_depth_m": depth, "sea_state": state,
            "static_solution": f"depth-{depth}" if supported else None,
            "status": "static line only; environmental response not established" if supported else "not reproduced",
        })
    static_entries = sum(row["static_solution"] is not None for row in matrix)
    return {
        "revision": basis["revision"], "criteria_revision": criteria["revision"],
        "module_commit": basis["module_commit"],
        "component": {"construction": row["construction"], "diameter_mm": float(row["diameter"]),
                      "source_id": basis["component_source_id"], "readiness": "computed example; not certified"},
        "counts": {"matrix_entries": len(matrix), "unique_static_solves": len(solutions),
                   "entries_with_static_line": static_entries,
                   "entries_not_reproduced": len(matrix) - static_entries,
                   "qualified_environmental_cases": 0},
        "solutions": solutions, "matrix": matrix, "not_reproduced": basis["not_reproduced"],
    }


def main() -> None:
    """Write deterministic native results to an explicit destination."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--receipt", type=Path, required=True)
    args = parser.parse_args()
    criteria = RECORD / "criteria.json"
    validate_evidence(criteria, json.loads(args.receipt.read_text(encoding="utf-8")))
    result = compute_record(json.loads((RECORD / "basis.json").read_text(encoding="utf-8")),
                            json.loads(criteria.read_text(encoding="utf-8")))
    args.output_dir.mkdir(parents=True, exist_ok=True)
    write_json(args.output_dir / "results.json", result)
    print(f"results.json sha256={digest(args.output_dir / 'results.json')}")


if __name__ == "__main__":
    main()
