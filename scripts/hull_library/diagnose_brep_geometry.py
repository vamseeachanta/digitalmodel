"""Sampled counterexamples for generated BRep geometry, never an acceptance proof."""

import argparse
import json
from pathlib import Path

import numpy as np
from scipy.interpolate import PchipInterpolator

from digitalmodel.hydrodynamics.hull_library.hull_surface_brep import (
    bspline_face_from_grid,
)
from digitalmodel.hydrodynamics.hull_library.hull_surface_brep_sampling import (
    section_arclength_grid,
    shared_bottom_face,
)
from scripts.hull_library.diagnose_form_brep import synthetic_profile


def _derivatives(surface, u, v):
    from OCP.gp import gp_Pnt, gp_Vec

    point = gp_Pnt()
    vectors = [gp_Vec() for _ in range(5)]
    surface.D2(float(u), float(v), point, *vectors)
    a, b, aa, bb, ab = [np.asarray(vector.Coord()) for vector in vectors]
    cross = np.cross(a, b)
    jacobian = np.linalg.norm(cross)
    normal = cross / jacobian if jacobian > 0 else np.full(3, np.nan)
    gaussian = ((normal @ aa) * (normal @ bb) - (normal @ ab) ** 2) / jacobian**2
    return point.Coord(), a[0] * b[2] - b[0] * a[2], jacobian, gaussian


def sample_surface(face, count=161):
    from OCP.BRepAdaptor import BRepAdaptor_Surface
    from OCP.BRepTools import BRepTools

    surface = BRepAdaptor_Surface(face, True)
    u0, u1, v0, v1 = BRepTools.UVBounds_s(face)
    uv = [
        (u, v) for u in np.linspace(u0, u1, count) for v in np.linspace(v0, v1, count)
    ]
    rows = [_derivatives(surface, u, v) for u, v in uv]
    positions = np.asarray([row[0] for row in rows])
    projection, jacobian, gaussian = np.asarray([row[1:] for row in rows]).T
    worst = int(np.argmin(projection))
    low = int(np.argmin(positions[:, 2]))
    interior = projection.reshape(count, count)[1:-1, 1:-1]
    return {
        "evidence_class": "sampled counterexamples; not full acceptance",
        "comparator_class": "source-reference geometry",
        "uv_grid": [count, count],
        "bounds_min_m": positions.min(axis=0).tolist(),
        "bounds_max_m": positions.max(axis=0).tolist(),
        "lowest_z_uv": list(uv[low]),
        "lowest_z_point_m": positions[low].tolist(),
        "projection_jacobian_min_m2": float(projection.min()),
        "projection_jacobian_min_uv": list(uv[worst]),
        "projection_jacobian_negative_count": int((projection < 0).sum()),
        "interior_projection_jacobian_negative_count": int((interior < 0).sum()),
        "surface_jacobian_min_m2": float(jacobian.min()),
        "surface_jacobian_max_m2": float(jacobian.max()),
        "max_abs_gaussian_curvature_m2_inverse": float(np.max(abs(gaussian))),
    }


def synthetic_reference(profile, x, z):
    """Independent PCHIP evaluation for these dense synthetic profiles only."""
    stations = sorted(profile.stations, key=lambda station: station.x_position)
    values = []
    for station in stations:
        offsets = np.asarray(sorted(station.waterline_offsets))
        if len(offsets) < 3:
            raise ValueError("diagnostic reference requires at least three offsets")
        t = np.asarray(z) + profile.draft
        interp = PchipInterpolator(offsets[:, 0], offsets[:, 1], extrapolate=False)
        values.append(
            np.where(
                t < offsets[0, 0],
                offsets[0, 1],
                np.where(t > offsets[-1, 0], offsets[-1, 1], interp(t)),
            )
        )
    sx = np.array([station.x_position for station in stations])
    interp = PchipInterpolator(sx, np.asarray(values), axis=0, extrapolate=False)
    k = np.clip(np.searchsorted(sx, x, side="right") - 1, 0, len(sx) - 2)
    dx, columns = x - sx[k], np.arange(len(x))
    a, b, c, d = [coeff[k, columns] for coeff in interp.c]
    return np.maximum(((a * dx + b) * dx + c) * dx + d, 0)


def reference_distances(face, profile):
    from OCP.BRep import BRep_Tool
    from OCP.BRepTools import BRepTools
    from OCP.GeomAPI import GeomAPI_ProjectPointOnSurf
    from OCP.gp import gp_Pnt

    x, z = np.meshgrid(
        (np.arange(31) + 0.5) * 100 / 31,
        -8 + (np.arange(17) + 0.5) * 8 / 17,
        indexing="ij",
    )
    x, z = x.ravel(), z.ravel()
    y = synthetic_reference(profile, x, z)
    surface, bounds = BRep_Tool.Surface_s(face), BRepTools.UVBounds_s(face)
    distances, points, failed = [], [], 0
    for point in zip(x, y, z):
        projection = GeomAPI_ProjectPointOnSurf(gp_Pnt(*point), surface, *bounds)
        if projection.NbPoints():
            distances.append(projection.LowerDistance())
            points.append(point)
        else:
            failed += 1
    worst = int(np.argmax(distances)) if distances else None
    return {
        "reference_grid": [31, 17],
        "projection_failed_count": failed,
        "reference_distance_max_m": distances[worst] if worst is not None else None,
        "reference_distance_worst_point_m": (
            points[worst] if worst is not None else None
        ),
    }


def keel_counterexample(face, draft):
    """Sample the actual fitted boundary, independently of pole-bound proofs."""
    from OCP.BRepAdaptor import BRepAdaptor_Surface
    from OCP.BRepTools import BRepTools

    u0, u1, v0, _ = BRepTools.UVBounds_s(face)
    parameters = np.linspace(u0, u1, 2001)
    surface = BRepAdaptor_Surface(face, True)
    points = np.array([surface.Value(float(u), v0).Coord() for u in parameters])
    deviations = abs(points[:, 2] + draft)
    worst = int(np.argmax(deviations))
    return {
        "sample_count": 2001,
        "max_z_deviation_m": float(deviations[worst]),
        "worst_u": float(parameters[worst]),
        "worst_point_m": points[worst].tolist(),
    }


def diagnose(case, nx, nz):
    profile = synthetic_profile(case)
    face = bspline_face_from_grid(section_arclength_grid(profile, nx, nz))
    result = {"case": case, "fit_grid": [nx, nz], **sample_surface(face)}
    result.update(reference_distances(face, profile))
    result["keel_counterexample"] = keel_counterexample(face, profile.draft)
    try:
        shared_bottom_face(face, profile.draft)
        result["bottom_boundary"] = "constructed"
    except ValueError as error:
        result["bottom_boundary"] = str(error)  # our fixed, path-free validation text
    result["qualified_repair"] = False
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    records = [
        diagnose(case, nx, nz)
        for case in ("rounded", "transom")
        for nx, nz in ((41, 21), (81, 41), (161, 81))
    ]
    args.output.write_text(
        json.dumps(records, indent=2, allow_nan=False), encoding="utf-8"
    )


if __name__ == "__main__":
    main()
