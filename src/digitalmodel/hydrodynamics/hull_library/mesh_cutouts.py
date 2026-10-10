"""Mesh-level through-hull cuts on a flat keel; coordinates and dimensions in metres.

Bottom fragments use triangles padded by a repeated fourth index. Moonpool walls
are genuine quads. The wetted mesh stays open at the waterline: no lid is applied.
"""

from __future__ import annotations

from copy import deepcopy
from dataclasses import dataclass
import math
from typing import Any, TypeAlias

import numpy as np
from numpy.typing import NDArray
from shapely.geometry import LineString, Polygon
from shapely.ops import unary_union
from shapely.geometry.base import BaseGeometry

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh

_TOL = 1e-9
Points: TypeAlias = NDArray[np.float64]
Faces: TypeAlias = list[tuple[Points, bool]]


@dataclass(frozen=True)
class MoonpoolFootprint:
    """Convex footprint; use rectangle() or circle() to construct validated cuts."""

    shape: str
    center: tuple[float, float]
    length: float = 0.0
    width: float = 0.0
    radius: float = 0.0
    segments: int = 64

    def __post_init__(self) -> None:
        if len(self.center) != 2 or not np.isfinite(self.center).all():
            raise ValueError("center must contain two finite coordinates")
        if self.shape not in ("rectangle", "circle"):
            raise ValueError("shape must be rectangle or circle")
        dimensions = (
            (self.length, self.width) if self.shape == "rectangle" else (self.radius,)
        )
        if any(not math.isfinite(d) or d <= 0 for d in dimensions):
            raise ValueError("footprint dimensions must be finite and positive")
        if not isinstance(self.segments, int) or self.segments < 64:
            raise ValueError("circle segments must be an integer >= 64")

    @classmethod
    def rectangle(
        cls, *, center: tuple[float, float], length: float, width: float
    ) -> MoonpoolFootprint:
        """Rectangle aligned with the mesh x/y axes."""
        return cls("rectangle", center, length=length, width=width)

    @classmethod
    def circle(
        cls, *, center: tuple[float, float], radius: float, segments: int = 64
    ) -> MoonpoolFootprint:
        """Inscribed polygon, with <= 0.161% area error at 64 segments."""
        return cls("circle", center, radius=radius, segments=segments)

    def polygon(self) -> Points:
        """Counterclockwise footprint vertices in the x/y plane."""
        if self.shape == "rectangle":
            points = np.array([[-1, -1], [1, -1], [1, 1], [-1, 1]])
            points = points * [self.length / 2, self.width / 2]
        else:
            angles = np.arange(self.segments) * 2 * math.pi / self.segments
            points = self.radius * np.column_stack((np.cos(angles), np.sin(angles)))
        return points + self.center


@dataclass
class MeshCutoutResult:
    """New mesh and report; metadata carries panel-aligned moonpool_wall flags."""

    mesh: PanelMesh
    report: dict[str, Any]


def _clip(points: Points, a: Points, b: Points, inside: bool) -> Points:
    """Clip an ordered convex polygon against one vertical half-plane."""
    if not len(points):
        return points
    direction = b - a
    distances = direction[0] * (points[:, 1] - a[1]) - direction[1] * (
        points[:, 0] - a[0]
    )
    if not inside:
        distances = -distances
    output = []
    for i, point in enumerate(points):
        previous, d0 = points[i - 1], distances[i - 1]
        d1 = distances[i]
        if (d0 >= 0) != (d1 >= 0):
            output.append(previous + (point - previous) * d0 / (d0 - d1))
        if d1 >= 0:
            output.append(point)
    clipped = np.round(np.asarray(output).reshape(-1, 3), 9)
    if len(clipped):
        clipped = clipped[
            np.linalg.norm(clipped - np.roll(clipped, 1, axis=0), axis=1) > _TOL
        ]
    return clipped


def _area(points: Points) -> float:
    if len(points) < 3:
        return 0.0
    return float(
        abs(
            np.sum(
                points[:, 0] * np.roll(points[:, 1], -1)
                - points[:, 1] * np.roll(points[:, 0], -1)
            )
        )
        / 2
    )


def _subtract(points: Points, footprint: Points) -> list[Points]:
    """Disjoint convex remnants, retaining the source polygon's winding."""
    remnants = []
    for a, b in zip(footprint, np.roll(footprint, -1, axis=0)):
        outside = _clip(points, a, b, False)
        if _area(outside) > _TOL:
            remnants.append(outside)
        points = _clip(points, a, b, True)
        if _area(points) <= _TOL:
            break
    return remnants


def _source_faces(mesh: PanelMesh) -> Faces:
    """Expand a y-symmetric half-hull without altering caller-owned arrays."""
    if mesh.symmetry_plane not in (None, "y"):
        raise ValueError("only full meshes or y-symmetric half-hulls are supported")
    if not np.isfinite(mesh.vertices).all():
        raise ValueError("mesh vertices must be finite")
    flags = mesh.metadata.get("moonpool_wall", [False] * mesh.n_panels)
    if len(flags) != mesh.n_panels:
        raise ValueError("moonpool_wall flags must align with panels")
    faces = []
    for panel, flag in zip(mesh.panels, flags):
        indices = list(dict.fromkeys(int(i) for i in panel if i >= 0))
        if len(indices) < 3 or max(indices) >= mesh.n_vertices:
            raise ValueError("invalid panel indices")
        points = mesh.vertices[indices].copy()
        faces.append((points, bool(flag)))
        if mesh.symmetry_plane == "y" and not np.allclose(points[:, 1], 0):
            mirrored = points[::-1].copy()
            mirrored[:, 1] *= -1
            faces.append((mirrored, bool(flag)))
    return faces


def _validate_shaft(
    faces: Faces, footprint: Points, keel: float, waterline: float
) -> None:
    polygon = Polygon(footprint).buffer(-10 * _TOL)
    for points, _ in faces:
        if np.all(np.abs(points[:, 2] - keel) < _TOL):
            bottom = Polygon(points[:, :2])
            if not bottom.is_valid or bottom.convex_hull.area - bottom.area > _TOL:
                raise ValueError("flat bottom panels must be valid convex polygons")
            signed: float = float(
                np.sum(
                    points[:, 0] * np.roll(points[:, 1], -1)
                    - points[:, 1] * np.roll(points[:, 0], -1)
                )
            )
            if signed >= 0:
                raise ValueError("flat bottom winding must point downward")
            continue
        if points[:, 2].min() >= waterline - _TOL:
            if Polygon(points[:, :2]).intersection(polygon).area > _TOL:
                raise ValueError("cutout input must be an uncapped wetted mesh")
            continue
        projection: BaseGeometry = Polygon(points[:, :2])
        if projection.area <= _TOL:
            projection = LineString(points[:, :2])
        if projection.intersects(polygon):
            raise ValueError("a non-bottom surface obstructs the vertical shaft")


def _bottom_cut(faces: Faces, footprint: Points, keel: float) -> Faces:
    bottom = [p for p, _ in faces if np.all(np.abs(p[:, 2] - keel) < _TOL)]
    region = unary_union([Polygon(p[:, :2]) for p in bottom])
    polygon = Polygon(footprint)
    if not region.covers(polygon) or region.boundary.distance(polygon) <= _TOL:
        raise ValueError("footprint must lie strictly inside the flat bottom")
    output: Faces = []
    for points, flag in faces:
        if (
            np.all(np.abs(points[:, 2] - keel) < _TOL)
            and Polygon(points[:, :2]).intersection(polygon).area > _TOL
        ):
            output.extend((p, flag) for p in _subtract(points, footprint))
        else:
            output.append((points, flag))
    return output


def _split_walls(faces: Faces) -> Faces:
    """Propagate horizontal seam subdivisions through every existing quad layer."""
    candidates = np.unique(np.round(np.vstack([p[:, :2] for p, _ in faces]), 9), axis=0)
    output: Faces = []
    for points, flag in faces:
        if not flag:
            output.append((points, flag))
            continue
        if len(points) != 4:
            raise ValueError("existing moonpool walls must be quads")
        direction = points[1, :2] - points[0, :2]
        squared = float(np.dot(direction, direction))
        t = (candidates - points[0, :2]) @ direction / squared
        distance = np.linalg.norm(
            candidates - points[0, :2] - t[:, None] * direction, axis=1
        )
        selected = (t > _TOL) & (t < 1 - _TOL) & (distance < 2 * _TOL)
        splits = np.unique(np.concatenate(([0.0], t[selected], [1.0])))
        for t0, t1 in zip(splits, splits[1:]):
            bottom = points[1] - points[0]
            top = points[2] - points[3]
            quad = np.array(
                [
                    points[0] + t0 * bottom,
                    points[0] + t1 * bottom,
                    points[3] + t1 * top,
                    points[3] + t0 * top,
                ]
            )
            output.append((np.round(quad, 9), True))
    return output


def _conform(faces: Faces) -> Faces:
    """Insert all collinear edge vertices before triangulation (no hanging nodes)."""
    candidates = np.unique(np.round(np.vstack([p for p, _ in faces]), 9), axis=0)
    output: Faces = []
    for points, flag in faces:
        points = np.round(points, 9)
        boundary = []
        for a, b in zip(points, np.roll(points, -1, axis=0)):
            direction = b - a
            squared = np.dot(direction, direction)
            if squared < _TOL**2:
                continue
            t = (candidates - a) @ direction / squared
            separation = np.linalg.norm(candidates - a - t[:, None] * direction, axis=1)
            selected = (t > _TOL) & (t < 1 - _TOL) & (separation < 2 * _TOL)
            boundary.append(a)
            boundary.extend(candidates[selected][np.argsort(t[selected])])
        boundary_points = np.asarray(boundary)
        if len(boundary_points) == len(points) and len(points) <= 4:
            output.append((points, flag))
        else:
            center = np.mean(boundary_points, axis=0)
            for a, b in zip(boundary_points, np.roll(boundary_points, -1, axis=0)):
                output.append((np.array([center, a, b]), flag))
    return output


def _assemble(faces: Faces, mesh: PanelMesh) -> PanelMesh:
    vertices: list[Points] = []
    panels: list[list[int]] = []
    flags: list[bool] = []
    lookup: dict[tuple, int] = {}
    for points, flag in faces:
        panel = []
        for point in points:
            key = tuple(np.round(point, 9))
            if key not in lookup:
                lookup[key] = len(vertices)
                vertices.append(point)
            panel.append(lookup[key])
        if len(panel) == 3:
            panel.append(panel[-1])
        panels.append(panel)
        flags.append(flag)
    metadata = deepcopy(mesh.metadata)
    metadata.pop("winding", None)  # Source orientation statistics are now stale.
    metadata["moonpool_wall"] = flags
    return PanelMesh(
        vertices=np.array(vertices),
        panels=np.array(panels, dtype=np.int32),
        name=mesh.name,
        format_origin=mesh.format_origin,
        reference_point=list(mesh.reference_point),
        metadata=metadata,
    )


def _wall_faces(
    mesh: PanelMesh, footprint: Points, keel: float, waterline: float, layers: int
) -> Faces:
    """Use unpaired keel edges so wall/bottom endpoints are exactly shared."""
    edges: dict[tuple[int, int], list[tuple[int, int]]] = {}
    for panel in mesh.panels:
        indices = list(dict.fromkeys(int(i) for i in panel))
        for a, b in zip(indices, indices[1:] + indices[:1]):
            edges.setdefault((min(a, b), max(a, b)), []).append((a, b))
    walls = []
    boundary = Polygon(footprint).boundary
    levels = np.linspace(keel, waterline, layers + 1)
    for occurrences in edges.values():
        if len(occurrences) != 1:
            continue
        a, b = mesh.vertices[list(occurrences[0])]
        if not np.all(np.abs(np.array([a[2], b[2]]) - keel) < _TOL):
            continue
        if boundary.distance(LineString([a[:2], b[:2]])) > 10 * _TOL:
            continue
        for z0, z1 in zip(levels, levels[1:]):
            points = np.array([b, a, a, b])
            points[:, 2] = [z0, z0, z1, z1]
            walls.append((points, True))
    return walls


def cut_moonpool(
    mesh: PanelMesh,
    footprint: MoonpoolFootprint,
    *,
    waterline: float = 0.0,
    wall_layers: int = 1,
) -> MeshCutoutResult:
    """Cut a strictly interior footprint through a flat keel, returning mesh/report.

    Only the lowest flat bottom is cut. Unsupported sloping or stepped cuts are
    rejected. y-symmetric input is expanded to a full mesh. Existing waterline
    boundaries remain open; caller geometry is never mutated. Multiple disjoint
    cuts are supported, with cumulative report entries and panel flags.
    """
    if not isinstance(wall_layers, int) or wall_layers < 1:
        raise ValueError("wall_layers must be a positive integer")
    faces = _source_faces(mesh)
    keel = min(p[:, 2].min() for p, _ in faces)
    if not math.isfinite(waterline) or waterline <= keel:
        raise ValueError("waterline must be finite and above keel")
    if mesh.vertices[:, 2].max() > waterline + _TOL:
        raise ValueError("waterline is below the existing wetted surface")
    points = footprint.polygon()
    _validate_shaft(faces, points, keel, waterline)
    faces = _conform(_split_walls(_bottom_cut(faces, points, keel)))
    bottom_mesh = _assemble(faces, mesh)
    walls = _wall_faces(bottom_mesh, points, keel, waterline, wall_layers)
    result = _assemble(faces + walls, mesh)
    entry = {
        "shape": footprint.shape,
        "center": list(footprint.center),
        "length": footprint.length,
        "width": footprint.width,
        "radius": footprint.radius,
        "segments": footprint.segments,
        "keel": float(keel),
        "waterline": waterline,
        "footprint_area": float(_area(points)),
        "wall_layers": wall_layers,
    }
    result.metadata.setdefault("cutouts", []).append(entry)
    return MeshCutoutResult(
        result,
        {"cutouts": deepcopy(result.metadata["cutouts"]), "n_panels": result.n_panels},
    )
