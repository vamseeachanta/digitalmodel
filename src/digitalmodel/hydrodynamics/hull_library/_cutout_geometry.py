"""Local polygon triangulation and clearance checks for flat-keel cutouts."""

from __future__ import annotations

import math
from typing import Iterator, TypeAlias

import numpy as np
from numpy.typing import NDArray
from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
from shapely.geometry import LineString, MultiPoint, Point, Polygon
from shapely.ops import triangulate

Points: TypeAlias = NDArray[np.float64]
Faces: TypeAlias = list[tuple[Points, int]]
MAX_TRIANGLE_ASPECT = 50.0


def triangle_aspect(points: Points) -> float:
    """Longest edge divided by its altitude (dimensionless)."""
    lengths = np.linalg.norm(points - np.roll(points, 1, axis=0), axis=1)
    twice_area = np.linalg.norm(np.cross(points[1] - points[0], points[2] - points[0]))
    return float(max(lengths) ** 2 / twice_area) if twice_area else math.inf


def _boundary_segments(polygon: Polygon) -> Iterator[tuple[Points, Points]]:
    for ring in [polygon.exterior, *polygon.interiors]:
        points = np.asarray(ring.coords)[:-1]
        yield from zip(points, np.roll(points, -1, axis=0))


def _verify_boundary(triangles: list[Points], polygon: Polygon) -> bool:
    """Require each prescribed boundary segment as an unpaired triangle edge."""
    edges: dict[tuple, int] = {}
    for points in triangles:
        for a, b in zip(points, np.roll(points, -1, axis=0)):
            key = tuple(sorted((tuple(a), tuple(b))))
            edges[key] = edges.get(key, 0) + 1
    return all(
        edges.get(tuple(sorted((tuple(a), tuple(b))))) == 1
        for a, b in _boundary_segments(polygon)
    )


def _split_hole_constraints(
    polygon: Polygon, triangles: list[Points]
) -> tuple[Polygon, list[Points]]:
    """Recover encroached hole segments without inserting source-edge vertices."""
    edges = {
        tuple(sorted((tuple(a), tuple(b))))
        for p in triangles
        for a, b in zip(p, np.roll(p, -1, axis=0))
    }
    rings, additions = [], []
    for ring in polygon.interiors:
        points = np.asarray(ring.coords)[:-1]
        refined = []
        for a, b in zip(points, np.roll(points, -1, axis=0)):
            refined.append(a)
            if tuple(sorted((tuple(a), tuple(b)))) not in edges:
                midpoint = (a + b) / 2
                refined.append(midpoint)
                additions.append(midpoint)
        rings.append(refined)
    return Polygon(polygon.exterior, rings), additions


def _refine_sites(
    sites: Points, fixed: set[tuple], polygon: Polygon, bad: list[Points]
) -> Points:
    # Boundary sites are immutable. Coarsen short interior edges instead of
    # repeatedly subdividing them into a band of needles near long edges.
    remove: set[tuple] = set()
    additions: list[Points] = []
    worst = max(bad, key=lambda p: triangle_aspect(np.column_stack((p, np.zeros(3)))))
    for p in [worst]:
        j = int(np.argmin(np.linalg.norm(p - np.roll(p, 1, axis=0), axis=1)))
        a, b = p[j - 1], p[j]
        internal = [v for v in (a, b) if tuple(v) not in fixed]
        remove.update(map(tuple, internal))
        additions.append((a + b) / 2 if len(internal) == 2 else p.mean(axis=0))
    additions = [p for p in additions if polygon.contains(Point(p))]
    retained = [p for p in sites if tuple(p) not in remove]
    refined = (
        np.unique(np.vstack((retained, additions)), axis=0)
        if additions
        else np.asarray(retained)
    )
    return refined


def triangulate_remnant(polygon: Polygon, z: float) -> list[Points]:
    """Delaunay with interior sites; accept only recovered boundaries and quality.

    Source boundary edges remain immutable. Encroached hole edges may gain
    midpoint nodes without changing area. Difficult constraints fail closed.
    """
    sites = np.unique(
        np.vstack(
            [np.asarray(r.coords)[:-1] for r in [polygon.exterior, *polygon.interiors]]
        ),
        axis=0,
    )
    fixed = set(map(tuple, sites))
    for _ in range(512):
        all_geometries = triangulate(MultiPoint(sites))
        all_triangles = [np.asarray(t.exterior.coords)[:3] for t in all_geometries]
        recovered, extra = _split_hole_constraints(polygon, all_triangles)
        if extra:
            polygon = recovered
            fixed.update(map(tuple, extra))
            sites = np.unique(np.vstack((sites, extra)), axis=0)
            if len(sites) > 10000:
                break
            continue
        geometries = [
            t for t in all_geometries if t.difference(polygon).area <= t.area * 1e-12
        ]
        triangles = [np.asarray(t.exterior.coords)[:3] for t in geometries]
        if not math.isclose(
            sum(t.area for t in geometries), polygon.area, rel_tol=1e-9
        ):
            raise ValueError("cutout triangulation cannot recover constrained area")
        if not _verify_boundary(triangles, polygon):
            raise ValueError("cutout triangulation cannot recover constrained boundary")
        bad = [
            p
            for p in triangles
            if triangle_aspect(np.column_stack((p, np.zeros(3)))) > MAX_TRIANGLE_ASPECT
        ]
        if not bad:
            # Shapely Delaunay triangles are CCW; keel normals shall point down.
            return [np.column_stack((p[::-1], np.full(3, z))) for p in triangles]
        refined = _refine_sites(sites, fixed, polygon, bad)
        if np.array_equal(refined, sites) or len(refined) > 10000:
            break
        sites = refined
    raise ValueError(
        "cutout remnant cannot meet max triangle aspect ratio 50; "
        "increase clearance_tolerance or change footprint/mesh"
    )


def validate_clearance(
    bottom: list[Points], footprint: Points, relative_tolerance: float
) -> None:
    """Reject positive corner-to-edge gaps relative to the local shortest edge.

    Exact coincidence is supported. Ordinary transverse crossings away from
    source/footprint corners are supported. Near corners and parallel edges are
    rejected rather than snapped, keeping reported footprint area unchanged.
    """
    boundary = Polygon(footprint).boundary
    for points in bottom:
        xy = points[:, :2]
        edges = list(zip(xy, np.roll(xy, -1, axis=0)))
        scale = min(np.linalg.norm(b - a) for a, b in edges)
        tolerance = relative_tolerance * scale
        if Polygon(xy).distance(Polygon(footprint)) > tolerance:
            continue
        distances = [Point(p).distance(boundary) for p in xy]
        distances.extend(
            Point(p).distance(LineString([a, b])) for a, b in edges for p in footprint
        )
        if any(1e-9 < d < tolerance for d in distances):
            raise ValueError(
                "footprint is near a mesh line or previous cut; "
                f"clearance_tolerance={relative_tolerance:g} "
                f"requires corner/edge clearance {tolerance:g} m"
            )


def conform_local(faces: Faces, candidates: list[Points]) -> Faces:
    """Insert only actual cut-boundary nodes on source bottom edges."""
    if not len(candidates):
        return faces
    nodes = np.unique(np.asarray(candidates), axis=0)
    lower, upper = nodes.min(axis=0), nodes.max(axis=0)
    output = []
    for points, index in faces:
        if np.any(points.max(axis=0) < lower - 1e-9) or np.any(
            points.min(axis=0) > upper + 1e-9
        ):
            output.append((points, index))
            continue
        # All cut candidates lie at the keel. Do not inspect remote surfaces.
        if not np.all(np.abs(points[:, 2] - nodes[0, 2]) < 1e-9):
            output.append((points, index))
            continue
        boundary = []
        for a, b in zip(points, np.roll(points, -1, axis=0)):
            direction = b - a
            squared = np.dot(direction, direction)
            t = (nodes - a) @ direction / squared
            separation = np.linalg.norm(nodes - a - t[:, None] * direction, axis=1)
            selected = (t > 1e-9) & (t < 1 - 1e-9) & (separation < 1e-9)
            boundary.append(a)
            boundary.extend(nodes[selected][np.argsort(t[selected])])
        if len(boundary) == len(points):
            output.append((points, index))
        else:
            polygon = Polygon(np.asarray(boundary)[:, :2])
            output.extend(
                (p, index) for p in triangulate_remnant(polygon, points[0, 2])
            )
    return output


def mesh_quality(mesh: PanelMesh) -> dict[str, float | int]:
    """Actual output metrics, including retained source panels."""
    triangles = [
        mesh.vertices[list(dict.fromkeys(p))] for p in mesh.panels if len(set(p)) == 3
    ]
    areas = mesh.panel_areas
    assert areas is not None
    return {
        "min_panel_area": float(min(areas)),
        "max_triangle_aspect_ratio": max(map(triangle_aspect, triangles), default=0.0),
        "triangle_count": len(triangles),
        "quad_count": mesh.n_panels - len(triangles),
    }
