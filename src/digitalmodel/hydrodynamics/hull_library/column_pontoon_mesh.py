"""Convex union clipping and conforming quadrilateral welding."""
from typing import Any, cast
import numpy as np
from numpy.typing import NDArray
from scipy.spatial import cKDTree
from .column_pontoon_patches import refine
from .column_pontoon_quality import optimize_quad_centres
from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh

_EPS = 1e-8
Array = NDArray[np.float64]
Solid = dict[str, Any]

def _clip(poly: Array, plane: Array, inside: bool) -> Array | None:
    values = poly @ plane[:3] + plane[3]
    values[np.abs(values) < _EPS] = 0
    result = []
    for a, b, da, db in zip(poly, np.roll(poly, -1, axis=0), values, np.roll(values, -1)):
        if (da <= 0 if inside else da >= 0):
            result.append(a)
        if da*db < 0:
            result.append(a + da/(da-db)*(b-a))
    if len(result) < 3:
        return None
    poly = np.array(result)
    poly = poly[np.linalg.norm(poly - np.roll(poly, 1, axis=0), axis=1) > _EPS]
    return poly if len(poly) >= 3 else None


def _subtract(poly: Array, cutter: Solid, source_index: int, cutter_index: int) -> list[Array]:
    low, high = cutter["bounds"]
    if np.any(poly.max(0) < low - _EPS) or np.any(poly.min(0) > high + _EPS):
        return [poly]
    distances = poly @ cutter["planes"][:, :3].T + cutter["planes"][:, 3]
    if np.any(np.min(distances, axis=0) > _EPS):
        return [poly]
    normal = np.sum(np.cross(poly, np.roll(poly, -1, axis=0)), axis=0)
    coplanar = np.max(np.abs(distances), axis=0) < _EPS
    if source_index < cutter_index and np.any(coplanar & (cutter["planes"][:, :3] @ normal > 0)):
        return [poly]  # Keep same-facing overlap once, from the earlier primitive.
    outside, remaining = [], poly
    # Cut the tightest planes first to avoid fragmenting the far outside region.
    for plane in cutter["planes"][np.argsort(np.min(distances, axis=0))[::-1]]:
        values = remaining @ plane[:3] + plane[3]
        if np.max(values) <= _EPS:
            continue
        fragment = _clip(remaining, plane, False)
        if fragment is not None:
            outside.append(fragment)
        clipped = _clip(remaining, plane, True)
        if clipped is None:
            break
        remaining = clipped
    return outside


def _union_faces(solids: list[Solid]) -> list[Array]:
    output = []
    for i, solid in enumerate(solids):
        for face in solid["faces"]:
            fragments = [face]
            for j, cutter in enumerate(solids):
                if i != j:
                    fragments = [piece for fragment in fragments
                                 for piece in _subtract(fragment, cutter, i, j)]
            output.extend(fragments)
    return output


def _snap_faces(faces: list[Array]) -> tuple[list[Array], Array]:
    original = np.vstack(faces)
    _, first = np.unique(np.round(original, 8), axis=0, return_index=True)
    points = original[first]
    tree = cKDTree(points)
    parents = np.arange(len(points))
    def root(i: int) -> int:
        while parents[i] != i:
            parents[i] = parents[parents[i]]
            i = parents[i]
        return i
    for a, b in tree.query_pairs(10*_EPS):
        parents[root(b)] = root(a)
    mapped = points[[root(i) for i in range(len(points))]]
    snapped = [mapped[tree.query(face)[1]] for face in faces]
    points = np.unique(mapped, axis=0)
    return cast(list[Array], snapped), cast(Array, points)


def _ordinary_quads(patches: list[Array], changed: set[int]) -> set[int]:
    ordinary = set()
    for group, ring in enumerate(patches):
        if len(ring) != 4 or group in changed:
            continue
        edges = np.roll(ring, -1, axis=0)-ring
        normal = np.sum(np.cross(ring, np.roll(ring, -1, axis=0)), axis=0)
        turns = np.cross(edges, np.roll(edges, -1, axis=0)) @ normal
        if np.all(turns > _EPS*np.linalg.norm(normal)):
            ordinary.add(group)
    return ordinary


def _quad_mesh(faces: list[Array], lid: bool) -> PanelMesh:
    snapped, points = _snap_faces(faces)
    tree = cKDTree(points)
    vertices: list[Array] = []
    panels: list[list[int]] = []
    index: dict[tuple, int] = {}
    def vertex(point: Array) -> int:
        key = tuple(np.round(point, 8))
        if key not in index:
            index[key] = len(vertices)
            vertices.append(point)
        return index[key]
    patches = []
    for face in snapped:
        face = face[np.linalg.norm(face-np.roll(face, 1, axis=0), axis=1) > _EPS]
        if len(face) < 3:
            continue
        if not lid and np.max(np.abs(face[:, 2])) < _EPS:
            continue
        boundary = []
        for a, b in zip(face, np.roll(face, -1, axis=0)):
            edge = b-a
            length2 = edge @ edge
            candidates = points[tree.query_ball_point((a+b)/2, np.sqrt(length2)/2+_EPS)]
            t = (candidates-a) @ edge / length2
            on = (np.linalg.norm(candidates-a-t[:, None]*edge, axis=1) < 3*_EPS) & (t >= -_EPS) & (t < 1-_EPS)
            boundary.extend(candidates[on][np.argsort(t[on])])
        patches.append(np.array(boundary))
    triangles, groups, changed = refine(patches)
    ordinary = _ordinary_quads(patches, changed)
    for group, ring in enumerate(patches):
        if group in ordinary:
            centre = ring.mean(0)
            mids = (ring + np.roll(ring, -1, axis=0))/2
            for i, a in enumerate(ring):
                panels.append([vertex(v) for v in (a, mids[i], centre, mids[i-1])])
    centres, _ = optimize_quad_centres(triangles)
    for tri, group, centre in zip(triangles, groups, centres):
        if group in ordinary:
            continue
        mids = (tri + np.roll(tri, -1, axis=0))/2
        for i, a in enumerate(tri):
            panels.append([vertex(v) for v in (a, mids[i], centre, mids[i-1])])
    return PanelMesh(np.array(vertices), np.array(panels, dtype=np.int32), name="column_pontoon")
