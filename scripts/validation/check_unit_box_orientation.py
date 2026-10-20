# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exact fixture QA and native hydrostatic screening; no solver imports.

Geometry contract: examples/hydrodynamics/diffraction/unit_box_rao/unit_box.gdf.
Axis-volume identity: src/digitalmodel/hydrodynamics/diffraction/mesh_orientation.py,
module docstring. This checker is deliberately limited to the five-panel unit box.
It does not replace that module's general-purpose topology checks.
"""

import argparse
from collections.abc import Sequence
from fractions import Fraction
import hashlib
import json
import math
from pathlib import Path
from typing import Any


def cross(a: Sequence[Fraction], b: Sequence[Fraction]) -> tuple[Fraction, Fraction, Fraction]:
    """Right-hand vector area, as in mesh_orientation.panel_vector_areas."""
    return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0])


def check_mesh(path: str | Path) -> dict[str, Any]:
    """Reject malformed, crossed, duplicate, missing or inward box faces."""
    # Stable across LF/CRLF checkouts; all other input bytes remain significant.
    raw = Path(path).read_bytes().replace(b"\r\n", b"\n")
    lines = raw.decode("utf-8").splitlines()
    # Header layout, unit scale, no symmetry and panel count: canonical GDF.
    if len(lines) != 24 or int(lines[3].split()[0]) != 5:
        raise ValueError("expected five quadrilateral faces")
    scale, gravity = map(Fraction, lines[1].split()[:2])
    if scale != 1 or gravity <= 0 or lines[2].split()[:2] != ["0", "0"]:
        raise ValueError("expected full unit-scale GDF with positive gravity")
    coords = [tuple(map(Fraction, line.split())) for line in lines[4:]]
    # Exact bounds and interior point from canonical GDF (metres).
    bounds = ((0, 1), (0, 1), (-1, 0))
    interior = (Fraction(1, 2), Fraction(1, 2), Fraction(-1, 2))
    if any(len(v) != 3 or any(v[k] not in bounds[k] for k in range(3))
           for v in coords):
        raise ValueError("coordinates do not describe the canonical unit box")
    faces, axis_volumes = set(), [Fraction(0)] * 3
    for start in range(0, len(coords), 4):
        quad = coords[start:start + 4]
        planes = [(k, quad[0][k]) for k in range(3)
                  if all(v[k] == quad[0][k] for v in quad)]
        if len(set(quad)) != 4 or len(planes) != 1:
            raise ValueError("faces must be rectangular and nondegenerate")
        face = planes[0]
        if face in faces or face == (2, 0):
            raise ValueError("duplicate faces or free-surface lid")
        faces.add(face)
        # Polygon vector area = half the sum of edge cross products;
        # equivalent to the triangle integration in mesh_orientation.py.
        edges = [cross(quad[i], quad[(i + 1) % 4]) for i in range(4)]
        area = [sum((e[k] for e in edges), Fraction(0)) / 2 for k in range(3)]
        center = [sum((v[k] for v in quad), Fraction(0)) / 4 for k in range(3)]
        if sum(area[k] * (center[k] - interior[k]) for k in range(3)) <= 0:
            raise ValueError("panel is not outward or its vertex order crosses")
        for k in range(3):
            axis_volumes[k] += center[k] * area[k]
    if axis_volumes != [1, 1, 1]:
        raise ValueError("axis volumes do not match unit-box geometry")
    return {"orientation": "PASS", "mesh_sha256": hashlib.sha256(raw).hexdigest(),
            "axis_volumes_m3": [float(v) for v in axis_volumes],
            "volume_m3": 1.0, "waterplane_area_m2": 1.0, "cob_z_m": -0.5,
            "native_hydrostatics": "NOT_EVALUATED"}


def compare_native(mesh: dict[str, Any], observed: dict[str, Any]) -> dict[str, Any]:
    """Screen manually extracted SI data, without claiming native provenance.

    The digest binds the mesh only. A manager must verify the native result,
    extraction and run settings before accepting the comparison.
    """
    if observed.get("mesh_sha256") != mesh["mesh_sha256"]:
        raise ValueError("mesh digest mismatch")
    keys = ("density_kg_m3", "gravity_m_s2", "k33_n_per_m", "volume_m3",
            "waterplane_area_m2", "cob_z_m")
    for key in keys:
        value = observed.get(key)
        if isinstance(value, bool) or not isinstance(value, (int, float)):
            raise ValueError(f"{key} must be a finite number")
        if not math.isfinite(value):
            raise ValueError(f"{key} must be a finite number")
    if observed["density_kg_m3"] <= 0 or observed["gravity_m_s2"] <= 0:
        raise ValueError("density and gravity must be positive")
    # Fixed environment: examples/hydrodynamics/diffraction/unit_box_rao/spec.yml.
    # Observations must not redefine the analytical comparison reference.
    density, gravity = 1025, 9.80665
    settings_match = (observed["density_kg_m3"] == density
                      and observed["gravity_m_s2"] == gravity)
    expected = {key: mesh[key] for key in ("volume_m3", "waterplane_area_m2", "cob_z_m")}
    expected["k33_n_per_m"] = density * gravity * mesh["waterplane_area_m2"]
    if not math.isfinite(expected["k33_n_per_m"]):
        raise ValueError("stiffness reference overflows")
    residuals = {key: abs(observed[key] - ref) / abs(ref)
                 for key, ref in expected.items()}
    # Repository AQG-style hydrostatics tier, not an independently verified
    # standard: cross_solver_acceptance.py DEFAULT_CRITERIA[HYDROSTATICS].
    limit = 0.005
    return {"status": "PASS_CONDITIONAL" if settings_match
            and all(v <= limit for v in residuals.values())
            else "FAIL", "relative_limit": limit, "relative_errors": residuals,
            "settings_match_spec": settings_match,
            "expected": expected, "native_provenance": "NOT_EVALUATED",
            "convergence": "NOT_EVALUATED"}


def main(argv: list[str] | None = None) -> int:
    """Print the offline screening record and return a failing exit status on rejection."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mesh", type=Path)
    parser.add_argument("--observations", type=Path,
                        help="manually extracted native hydrostatics in SI JSON")
    args = parser.parse_args(argv)
    try:
        result = check_mesh(args.mesh)
        if args.observations:
            observed = json.loads(args.observations.read_text(encoding="utf-8"))
            if not isinstance(observed, dict):
                raise ValueError("observations must be a JSON object")
            result["native_hydrostatics"] = compare_native(result, observed)
        print(json.dumps(result, sort_keys=True, allow_nan=False))
        return int(isinstance(result["native_hydrostatics"], dict)
                   and result["native_hydrostatics"]["status"] == "FAIL")
    except (ValueError, OSError, IndexError, ZeroDivisionError):
        # No machine paths or raw input text in the public-facing failure record.
        print(json.dumps({"orientation": "NOT_EVALUATED", "status": "FAIL",
                          "reason": "invalid mesh or observation input"}))
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
