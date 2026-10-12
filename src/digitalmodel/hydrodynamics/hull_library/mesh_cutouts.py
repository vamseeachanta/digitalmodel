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
from shapely.geometry import LineString, Polygon, Point
from shapely.ops import unary_union
from shapely import get_parts
from shapely.geometry.base import BaseGeometry

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
from ._cutout_geometry import (
    conform_local,
    mesh_quality,
    triangulate_remnant,
    validate_clearance,
)

_TOL = 1e-9
Points: TypeAlias = NDArray[np.float64]
Faces: TypeAlias = list[tuple[Points, int]]


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
        if not isinstance(self.segments, int) or self.segments < 8:
            raise ValueError("circle segments must be an integer >= 8")

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
        """Equal-area regular polygon; segments >= 8, radius is nominal."""
        return cls("circle", center, radius=radius, segments=segments)

    def polygon(self) -> Points:
        """Counterclockwise footprint vertices in the x/y plane."""
        if self.shape == "rectangle":
            points = np.array([[-1, -1], [1, -1], [1, 1], [-1, 1]])
            points = points * [self.length / 2, self.width / 2]
        else:
            angles = np.arange(self.segments) * 2 * math.pi / self.segments
            scale = math.sqrt(
                2 * math.pi / (self.segments * math.sin(2 * math.pi / self.segments))
            )
            points = (
                self.radius * scale * np.column_stack((np.cos(angles), np.sin(angles)))
            )
        return points + self.center


@dataclass
class MeshCutoutResult:
    """New mesh and report; metadata carries panel-aligned moonpool_wall flags."""

    mesh: PanelMesh
    report: dict[str, Any]


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


def _source_faces(mesh: PanelMesh) -> Faces:
    """Expand a y-symmetric half-hull without altering caller-owned arrays."""
    if mesh.symmetry_plane not in (None, "y"):
        raise ValueError("only full meshes or y-symmetric half-hulls are supported")
    if not np.isfinite(mesh.vertices).all():
        raise ValueError("mesh vertices must be finite")
    flags = mesh.metadata.get("moonpool_wall", [False] * mesh.n_panels)
    if len(flags) != mesh.n_panels:
        raise ValueError("moonpool_wall flags must align with panels")
    if any(flags) and "moonpool_index" not in mesh.metadata:
        raise ValueError("source moonpool walls require panel-aligned moonpool_index")
    indices_by_panel = mesh.metadata.get("moonpool_index", [-1] * mesh.n_panels)
    if len(indices_by_panel) != mesh.n_panels:
        raise ValueError("moonpool_index must align with panels")
    if any(
        bool(flag) != (int(index) >= 0) for flag, index in zip(flags, indices_by_panel)
    ):
        raise ValueError("moonpool_index must agree with moonpool_wall flags")
    faces = []
    for panel, flag, index in zip(mesh.panels, flags, indices_by_panel):
        indices = list(dict.fromkeys(int(i) for i in panel if i >= 0))
        if len(indices) < 3 or max(indices) >= mesh.n_vertices:
            raise ValueError("invalid panel indices")
        points = mesh.vertices[indices].copy()
        faces.append((points, int(index) if flag else -1))
        if mesh.symmetry_plane == "y" and not np.allclose(points[:, 1], 0):
            mirrored = points[::-1].copy()
            mirrored[:, 1] *= -1
            faces.append((mirrored, int(index) if flag else -1))
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


def _bottom_cut(
    faces: Faces, footprint: Points, keel: float, clearance: float
) -> Faces:
    bottom = [p for p, _ in faces if np.all(np.abs(p[:, 2] - keel) < _TOL)]
    region = unary_union([Polygon(p[:, :2]) for p in bottom])
    polygon = Polygon(footprint)
    if not region.covers(polygon) or region.boundary.distance(polygon) <= _TOL:
        raise ValueError("footprint must lie strictly inside the flat bottom")
    validate_clearance(bottom, footprint, clearance)
    touched = [p for p in bottom if Polygon(p[:, :2]).intersection(polygon).area > _TOL]
    selected = unary_union([Polygon(p[:, :2]) for p in touched])
    if not touched or not math.isclose(
        selected.intersection(polygon).area, polygon.area, rel_tol=1e-9
    ):
        raise ValueError("footprint is below the supported clipping resolution")
    patch = selected.difference(polygon)
    pieces = [p for p in get_parts(patch) if isinstance(p, Polygon)]
    output = [
        (p, flag)
        for p, flag in faces
        if not (
            np.all(np.abs(p[:, 2] - keel) < _TOL)
            and Polygon(p[:, :2]).intersection(polygon).area > _TOL
        )
    ]
    candidates = []
    for piece in pieces:
        if piece.area == 0:
            continue
        for ring in [piece.exterior, *piece.interiors]:
            for xy in np.asarray(ring.coords)[:-1]:
                if polygon.boundary.distance(Point(xy)) < _TOL:
                    candidates.append(np.array([*xy, keel]))
        output.extend((p, -1) for p in triangulate_remnant(piece, keel))
    return conform_local(output, candidates)


def _assemble(faces: Faces, mesh: PanelMesh) -> PanelMesh:
    vertices: list[Points] = []
    panels: list[list[int]] = []
    flags: list[bool] = []
    cutout_indices: list[int] = []
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
        flags.append(flag >= 0)
        cutout_indices.append(flag)
    metadata = deepcopy(mesh.metadata)
    metadata.pop("winding", None)  # Source orientation statistics are now stale.
    metadata["moonpool_wall"] = flags
    metadata["moonpool_index"] = cutout_indices
    return PanelMesh(
        vertices=np.array(vertices),
        panels=np.array(panels, dtype=np.int32),
        name=mesh.name,
        format_origin=mesh.format_origin,
        reference_point=list(mesh.reference_point),
        metadata=metadata,
    )


def _wall_faces(
    mesh: PanelMesh,
    footprint: Points,
    keel: float,
    waterline: float,
    layers: int,
    index: int,
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
        if any(boundary.distance(Point(p[:2])) > 10 * _TOL for p in (a, b)):
            continue
        for z0, z1 in zip(levels, levels[1:]):
            points = np.array([b, a, a, b])
            points[:, 2] = [z0, z0, z1, z1]
            walls.append((points, index))
    return walls


def _report_entry(
    footprint: MoonpoolFootprint,
    points: Points,
    keel: float,
    waterline: float,
    wall_layers: int,
    clearance_tolerance: float,
) -> dict[str, Any]:
    return {
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
        "clearance_tolerance": clearance_tolerance,
        "nominal_area": (
            math.pi * footprint.radius**2
            if footprint.shape == "circle"
            else footprint.length * footprint.width
        ),
        "draft": float(waterline - keel),
        "removed_volume": float(_area(points) * (waterline - keel)),
    }


def cut_moonpool(
    mesh: PanelMesh,
    footprint: MoonpoolFootprint,
    *,
    waterline: float = 0.0,
    wall_layers: int = 1,
    clearance_tolerance: float = 1e-4,
) -> MeshCutoutResult:
    """Cut a strictly interior footprint through a flat keel, returning mesh/report.

    Only the lowest flat bottom is cut. Unsupported sloping or stepped cuts are
    rejected. y-symmetric input is expanded to a full mesh. Existing waterline
    boundaries remain open; caller geometry is never mutated. Multiple disjoint
    cuts are supported, with cumulative report entries and panel flags.

    clearance_tolerance is a positive dimensionless fraction of each nearby
    bottom panel's shortest edge (default 1e-4). Positive corner-to-edge gaps
    below this clearance are rejected, including gaps to previous cuts. Exact
    alignment and ordinary transverse crossings are supported. New bottom
    triangles have longest-edge/altitude aspect ratio <= 50 or are rejected.
    """
    if not isinstance(wall_layers, int) or wall_layers < 1:
        raise ValueError("wall_layers must be a positive integer")
    if not math.isfinite(clearance_tolerance) or clearance_tolerance <= 0:
        raise ValueError("clearance_tolerance must be finite and positive")
    faces = _source_faces(mesh)
    keel = min(p[:, 2].min() for p, _ in faces)
    if not math.isfinite(waterline) or waterline <= keel:
        raise ValueError("waterline must be finite and above keel")
    if mesh.vertices[:, 2].max() > waterline + _TOL:
        raise ValueError("waterline is below the existing wetted surface")
    points = footprint.polygon()
    _validate_shaft(faces, points, keel, waterline)
    faces = _bottom_cut(faces, points, keel, clearance_tolerance)
    bottom_mesh = _assemble(faces, mesh)
    index = len(mesh.metadata.get("cutouts", []))
    walls = _wall_faces(bottom_mesh, points, keel, waterline, wall_layers, index)
    result = _assemble(faces + walls, mesh)
    entry = _report_entry(
        footprint, points, keel, waterline, wall_layers, clearance_tolerance
    )
    result.metadata.setdefault("cutouts", []).append(entry)
    return MeshCutoutResult(
        result,
        {
            "cutouts": deepcopy(result.metadata["cutouts"]),
            "n_panels": result.n_panels,
            "mesh_quality": mesh_quality(result),
        },
    )
