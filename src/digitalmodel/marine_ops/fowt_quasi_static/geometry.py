# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""External submerged boundary of circular columns and rectangular pontoons.

The primitive union removes internal faces. Bottom-tier and upper-tier surfaces
share the same polygonal column boundaries; no coincident wetted faces remain.
Dimensions and elevations are supplied by the attributed public-reference basis.
"""
from __future__ import annotations

import math
from typing import TYPE_CHECKING

import numpy as np
from scipy.integrate import quad
from shapely import affinity, constrained_delaunay_triangles, segmentize
from shapely.geometry import MultiPoint, Point, Polygon, box
from shapely.ops import triangulate, unary_union

if TYPE_CHECKING:
    import capytaine as cpt


def _dimensions(basis: dict) -> tuple[float, ...]:
    geometry = basis["geometry"]
    return tuple(geometry[key]["value"] for key in (
        "central_column_diameter", "outer_column_diameter", "outer_column_radius",
        "pontoon_width", "pontoon_height", "draft"))


def _sections(basis: dict, segments: int) -> tuple:
    import capytaine as cpt
    central, outer, radius, width, _, draft = _dimensions(basis)
    centers = [(0.0, 0.0)] + [
        (radius * math.cos(math.radians(a)), radius * math.sin(math.radians(a)))
        for a in basis["geometry"]["column_azimuths"]["value"]]
    columns = []
    for (x, y), diameter in zip(centers, [central, outer, outer, outer]):
        primitive = cpt.mesh_vertical_cylinder(
            radius=diameter / 2, length=draft, center=(x, y, -draft / 2),
            resolution=(1, segments, 1))
        vertices = primitive.vertices[np.isclose(primitive.vertices[:, 2], 0), :2]
        angles = np.arctan2(vertices[:, 1] - y, vertices[:, 0] - x)
        boundary = vertices[np.argsort(angles)]
        columns.append(Polygon(boundary).convex_hull)
    arms = [affinity.rotate(box(0, -width / 2, radius, width / 2), angle,
                            origin=(0, 0))
            for angle in basis["geometry"]["column_azimuths"]["value"]]
    return unary_union(columns + arms), unary_union(columns)


def _horizontal_faces(polygon: Polygon, z: float, step: float, up: bool) -> list:
    polygon = segmentize(polygon, max_segment_length=step)
    xmin, ymin, xmax, ymax = polygon.bounds
    grid = [Point(x, y) for x in np.arange(xmin, xmax + step, step)
            for y in np.arange(ymin, ymax + step, step)
            if polygon.contains(Point(x, y))]
    points = unary_union([polygon.boundary, MultiPoint(grid)])
    faces = []
    for triangle in triangulate(points):
        clipped = triangle.intersection(polygon)
        for part in constrained_delaunay_triangles(clipped).geoms:
            if part.area > 1e-10:
                xy = np.array(part.exterior.coords[:3])
                if np.cross(xy[1] - xy[0], xy[2] - xy[0]) < 0:
                    xy = xy[::-1]
                if not up:
                    xy = xy[::-1]
                xyz = np.column_stack([xy, np.full(3, z)])
                faces.append(np.vstack([xyz, xyz[-1]]))
    return faces


def _vertical_faces(polygon: Polygon, bottom: float, top: float, step: float) -> list:
    # Shapely exterior may be clockwise; enforce counterclockwise for outward normals.
    from shapely.geometry.polygon import orient
    boundary = np.asarray(orient(polygon, sign=1).exterior.coords)
    heights = np.linspace(bottom, top, math.ceil((top - bottom) / step) + 1)
    faces = []
    for start, end in zip(boundary[:-1], boundary[1:]):
        division = np.linspace(start, end, math.ceil(np.linalg.norm(end - start) / step) + 1)
        for left, right in zip(division[:-1], division[1:]):
            for low, high in zip(heights[:-1], heights[1:]):
                faces.append([[*left, low], [*right, low], [*right, high], [*left, high]])
    return faces


def hull_mesh(basis: dict, step: float) -> cpt.Mesh:
    """Generate an external surface from Capytaine column primitive boundaries."""
    import capytaine as cpt
    if step <= 0:
        raise ValueError("Panel size must be positive")
    *_, height, draft = _dimensions(basis)
    # At least 64 facets limits circular cross-section area error to 0.2%.
    segments = max(64, 4 * math.ceil(math.pi * 12.5 / step / 4))
    lower, upper = _sections(basis, segments)
    top = -draft + height
    faces = _horizontal_faces(lower, -draft, step, False)
    faces += _vertical_faces(lower, -draft, top, step)
    shelf = lower.difference(upper)
    shelves = list(shelf.geoms) if hasattr(shelf, "geoms") else [shelf]
    for polygon in shelves:
        faces += _horizontal_faces(polygon, top, step, True)
    for polygon in upper.geoms:
        faces += _vertical_faces(polygon, top, 0, step)
    flat = np.asarray(faces).reshape(-1, 3).round(10)
    vertices, index = np.unique(flat, axis=0, return_inverse=True)
    mesh = cpt.Mesh(vertices=vertices, faces=index.reshape(-1, 4), name="reference_hull")
    half = mesh.clipped(origin=(0, 0, 0), normal=(0, -1, 0))
    return cpt.ReflectionSymmetricMesh(half=half, plane="xOz", name="reference_hull")


def analytical_volume(basis: dict) -> float:
    """Integrate exact circle/rectangle union cross sections analytically in y.

    Each x slice is the union of exact circular chords and polygonal intervals.
    Adaptive quadrature integrates the chord lengths in x. Independent of mesh.
    """
    central, outer, radius, width, height, draft = _dimensions(basis)
    angles = np.radians(basis["geometry"]["column_azimuths"]["value"])
    circles = [(0.0, 0.0, central / 2)] + [
        (radius * np.cos(a), radius * np.sin(a), outer / 2) for a in angles]
    rectangles = [affinity.rotate(box(0, -width / 2, radius, width / 2),
                                 float(np.degrees(a)), origin=(0, 0)) for a in angles]
    limits = sorted(set([v for x, _, r in circles for v in (x-r, x+r)]
                        + [float(x) for p in rectangles for x, _ in p.exterior.coords]))

    def chord(x: float) -> float:
        intervals = []
        for cx, cy, r in circles:
            if abs(x-cx) < r:
                half = math.sqrt(r*r-(x-cx)**2)
                intervals.append((cy-half, cy+half))
        for polygon in rectangles:
            points = np.asarray(polygon.exterior.coords)
            ys = [a[1]+(x-a[0])*(b[1]-a[1])/(b[0]-a[0])
                  for a, b in zip(points[:-1], points[1:])
                  if min(a[0], b[0]) <= x < max(a[0], b[0])]
            if len(ys) == 2:
                intervals.append((min(ys), max(ys)))
        intervals.sort()
        length = 0.0
        end = -math.inf
        for lo, hi in intervals:
            length += max(0, hi-max(lo, end))
            end = max(end, hi)
        return length

    union_area: float = sum(quad(chord, lo, hi, epsabs=1e-5, limit=200)[0]
                     for lo, hi in zip(limits[:-1], limits[1:]))
    column_area = math.pi * ((central/2)**2 + 3*(outer/2)**2)
    return height*union_area + (draft-height)*column_area
