# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Prepare generic S-lay inputs and independent screens without solver access."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
import re
import time
from typing import Any

import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.post_validator import PostGenerationValidator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


def catenary(weight_kn_m: float, depth_m: float, tension_kn: float) -> dict[str, float]:
    """Calculate an inextensible, weight-only catenary with horizontal touchdown.

    Args:
        weight_kn_m: Positive submerged weight per unit length.
        depth_m: Height of the upper catenary endpoint above touchdown.
        tension_kn: Total tension at the upper endpoint.

    Returns:
        Independent force, geometry and touchdown curvature quantities.

    Raises:
        ValueError: Inputs are nonfinite or cannot sustain the hanging weight.
    """
    if not all(math.isfinite(v) and v > 0 for v in (weight_kn_m, depth_m, tension_kn)):
        raise ValueError("Finite positive catenary inputs are required")
    horizontal = tension_kn - weight_kn_m * depth_m
    if horizontal <= 0:
        raise ValueError("Top tension must exceed weight times endpoint height")
    a = horizontal / weight_kn_m
    length = math.sqrt(depth_m**2 + 2 * a * depth_m)
    return {"horizontal_tension_kn": horizontal, "suspended_length_m": length,
            "span_m": a * math.acosh(1 + depth_m / a),
            "top_tension_kn": math.hypot(horizontal, weight_kn_m * length),
            "touchdown_curvature_per_m": 1 / a}


def submerged_weight(basis: dict[str, Any]) -> float:
    """Calculate empty coated-pipe submerged weight in kN/m by annular areas."""
    pipe = basis["pipeline"]
    od = pipe["dimensions"]["outer_diameter"]
    wall = pipe["dimensions"]["wall_thickness"]
    mass = basis["steel_density_te_m3"] * math.pi / 4 * (od**2 - (od - 2 * wall)**2)
    coatings = [pipe["coatings"]["corrosion"], *pipe["coatings"]["weight"]]
    for coating in coatings:
        outer = od + 2 * coating["thickness"]
        mass += coating["density"] * math.pi / 4 * (outer**2 - od**2)
        od = outer
    buoyancy = basis["water_density_te_m3"] * math.pi / 4 * od**2
    return float((mass - buoyancy) * basis["gravity_m_s2"])


def case_spec(basis: dict[str, Any], case: dict[str, Any]) -> ProjectInputSpec:
    """Build a neutral scenario from declared inputs, retaining schema defaults."""
    equipment = basis["equipment"]
    data = {"metadata": {"name": case["id"], "description": "Generic still-water S-lay preparation",
                         "structure": "pipeline", "operation": "installation/s-lay"},
            "environment": {"water": {"depth": case["depth_m"], "density": basis["water_density_te_m3"]},
                            "seabed": basis["seabed"]},
            "pipeline": basis["pipeline"], "equipment": equipment}
    data = json.loads(json.dumps(data))
    data["equipment"]["tensioner"]["tension_value"] = case["top_tension_kn"]
    return ProjectInputSpec.model_validate(data)


def prepare_case(basis: dict[str, Any], case: dict[str, Any], output: Path) -> dict[str, Any]:
    """Generate one model, reject validator errors and bind every output by digest."""
    started = time.perf_counter()
    folder = output / "models" / case["id"]
    if folder.exists():
        raise ValueError("Use a new output directory; existing models are preserved")
    spec = case_spec(basis, case)
    ModularModelGenerator.from_spec(spec).generate(folder)
    master = folder / "master.yml"
    master.write_text(master.read_text(encoding="utf-8").rstrip() + "\n", encoding="utf-8")
    findings = PostGenerationValidator().validate_directory(folder / "includes")
    if any(f.level == "error" for f in findings):
        raise ValueError("Post-generation validation failed")
    files = {p.relative_to(output).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
             for p in sorted(folder.rglob("*.yml"))}
    weight = submerged_weight(basis)
    screen = catenary(weight, case["depth_m"], case["top_tension_kn"])
    screen["overbend_strain"] = (basis["pipeline"]["dimensions"]["outer_diameter"] /
                                  (2 * basis["equipment"]["stinger"]["radius"]))
    screen["submerged_weight_kn_m"] = weight
    return {"id": case["id"], "files": files, "validator_errors": 0,
            "validator_warnings": [f.category for f in findings], "screen": screen,
            "preparation_wall_seconds": time.perf_counter() - started,
            "native_wall_seconds": None, "native_status": "not_run"}


def prepare(campaign: Path, output: Path) -> dict[str, Any]:
    """Prepare all declared cases; record preparation timing separately from solves."""
    source = campaign / "case-table.yml"
    table = yaml.safe_load(source.read_text(encoding="utf-8"))
    identities = [case["id"] for case in table["cases"]]
    if (not identities or len(set(identities)) != len(identities) or
            any(not re.fullmatch(r"[a-z0-9]+(?:-[a-z0-9]+)*", name) for name in identities)):
        raise ValueError("Invalid or duplicate case identity")
    for case in table["cases"]:
        case_spec(table["basis"], case)
        catenary(submerged_weight(table["basis"]), case["depth_m"], case["top_tension_kn"])
    output.mkdir(parents=True, exist_ok=True)
    records = [prepare_case(table["basis"], case, output) for case in table["cases"]]
    result = {"schema": "slay-preparation-1", "criteria_revision": table["criteria_revision"],
              "case_table_sha256": hashlib.sha256(source.read_bytes()).hexdigest(),
              "native_attempted": 0, "native_completed": 0, "qualified_results": 0,
              "cases": records}
    (output / "preparation.json").write_text(json.dumps(result, indent=2, allow_nan=False) + "\n",
                                            encoding="utf-8")
    return result


def main() -> int:
    """Require an explicit mode; native execution is outside this preparation driver."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--campaign", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--dry-run", action="store_true")
    mode.add_argument("--launch", action="store_true")
    args = parser.parse_args()
    if args.launch:
        parser.error("A qualified execution adapter is required in a later solver job")
    prepare(args.campaign, args.output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
