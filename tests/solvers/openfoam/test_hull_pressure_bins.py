from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.hull_face_resolution import (
    _read_boundary,
    _read_faces,
    _read_points,
)
from digitalmodel.solvers.openfoam.hull_pressure_bins import (
    _patch_field_scalar,
    bin_pressure_force,
    patch_faces_geometry,
)

HEADER = "FoamFile {{ version 2.0; format ascii; class {cls}; object {obj}; }}\n"


def _write_list(path: Path, cls: str, obj: str, values):
    path.write_text(
        HEADER.format(cls=cls, obj=obj)
        + f"{len(values)}\n(\n" + "\n".join(values) + "\n)\n"
    )


def write_case(root: Path) -> Path:
    poly = root / "constant" / "polyMesh"
    poly.mkdir(parents=True)
    # Three yz quads (Sf points +x), centred at x=1, 5, 9, plus another patch.
    points, faces = [], []
    for x in (1.0, 5.0, 9.0, 20.0):
        base = len(points)
        points.extend([f"({x} 0 0)", f"({x} 1 0)", f"({x} 1 1)", f"({x} 0 1)"])
        faces.append(f"4({base} {base+1} {base+2} {base+3})")
    _write_list(poly / "points", "vectorField", "points", points)
    _write_list(poly / "faces", "faceList", "faces", faces)
    _write_list(poly / "owner", "labelList", "owner", ["0", "0", "0", "0"])
    _write_list(poly / "neighbour", "labelList", "neighbour", [])
    (poly / "boundary").write_text(
        HEADER.format(cls="polyBoundaryMesh", obj="boundary")
        + "2\n(\nhull\n{ type wall; nFaces 3; startFace 0; }\n"
          "other\n{ type patch; nFaces 1; startFace 3; }\n)\n"
    )
    field = root / "100" / "p"
    field.parent.mkdir()
    field.write_text(
        HEADER.format(cls="volScalarField", obj="p")
        + "boundaryField\n{\nhull\n{\n type calculated;\n value nonuniform List<scalar> 3\n(\n10\n20\n30\n);\n}\n"
          "other { type calculated; value uniform 999; }\n}\n"
    )
    return root


def _loop_geometry(poly: Path, patch: str):
    n, start = _read_boundary(poly / "boundary")[patch]
    faces = _read_faces(poly / "faces", start, n)
    pts = _read_points(poly / "points", {v for face in faces for v in face})
    sf, cf = [], []
    for face in faces:
        p = np.asarray([pts[v] for v in face])
        c0 = p.mean(axis=0)
        area_vector = np.zeros(3)
        weighted_centre = np.zeros(3)
        area = 0.0
        for i, a in enumerate(p):
            b = p[(i + 1) % len(p)]
            tri = 0.5 * np.cross(a - c0, b - c0)
            weight = np.linalg.norm(tri)
            area_vector += tri
            weighted_centre += weight * (a + b + c0) / 3.0
            area += weight
        sf.append(area_vector)
        cf.append(weighted_centre / area if area else c0)
    return np.asarray(sf), np.asarray(cf)


def test_pressure_force_bins_and_vectorised_geometry(tmp_path):
    case = write_case(tmp_path)
    poly = case / "constant" / "polyMesh"
    sf, cf = patch_faces_geometry(poly, "hull")
    expected_sf, expected_cf = _loop_geometry(poly, "hull")
    np.testing.assert_allclose(sf, expected_sf, atol=1e-12, rtol=0)
    np.testing.assert_allclose(cf, expected_cf, atol=1e-12, rtol=0)

    result = bin_pressure_force(case, "hull", ["100"], 3, [1, 0, 0], lo=0, hi=10)
    expected_face_force = np.array([10.0, 20.0, 30.0]) * expected_sf[:, 0]
    np.testing.assert_allclose(result["times"]["100"]["per_bin"], expected_face_force)
    assert result["times"]["100"]["total_along_direction"] == pytest.approx(
        expected_face_force.sum()
    )
    assert sum(result["times"]["100"]["per_bin"]) == pytest.approx(
        result["times"]["100"]["total_along_direction"]
    )


def test_patch_field_scalar_uniform_value_is_bounded_to_patch(tmp_path):
    field = tmp_path / "p"
    field.write_text(
        HEADER.format(cls="volScalarField", obj="p")
        + "boundaryField { hull { type calculated; value uniform 12.5; } "
          "other { value nonuniform List<scalar> 2 ( 90 91 ); } }\n"
    )
    np.testing.assert_array_equal(_patch_field_scalar(field, "hull"), [12.5])
