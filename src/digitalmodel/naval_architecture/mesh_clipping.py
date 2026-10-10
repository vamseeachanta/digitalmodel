"""Mesh clipping for the mesh hydrostatics adapter."""
from __future__ import annotations

import numpy as np
import math
from typing import Optional
from dataclasses import dataclass
from .mesh_geometry import (MeshContractError, _read_only, _face_area_vectors,
    _SNAP_REL, _CLOSURE_VOLUME_REL, volume_divergence, volume_tetra)
from .mesh_validation import TriMesh


def _clip_keep_below(
    vertices: np.ndarray,
    faces: np.ndarray,
    artificial: np.ndarray,
    normal: np.ndarray,
    offset: float,
    eps: float,
):
    """Keep the part of each face with normal.p - offset <= 0.

    Vertices within ``eps`` of the plane are snapped: their distance is set to zero and the
    vertex is projected onto the plane. Faces entirely on the plane are dropped.
    Returns (vertices, faces, artificial, boundary_edges, boundary_artificial) where the
    boundary edges are the directed edges of the kept surface with no reverse partner.
    """
    d = vertices @ normal - offset
    snap = (np.abs(d) <= eps) & (d != 0.0)
    verts = np.array(vertices, dtype=float, copy=True)
    if np.any(snap):
        verts[snap] -= d[snap, None] * normal[None, :]
    d = np.where(np.abs(d) <= eps, 0.0, d)
    df = d[faces]
    on_plane = np.all(df == 0.0, axis=1)
    keep_all = np.all(df <= 0.0, axis=1) & ~on_plane
    drop = np.all(df >= 0.0, axis=1)
    mixed = ~keep_all & ~drop

    new_pts: list = []
    cache: dict = {}
    n0 = verts.shape[0]
    extra_faces = []
    extra_art = []
    for fi in np.nonzero(mixed)[0]:
        tri = faces[fi]
        poly = []
        for k in range(3):
            a, b = int(tri[k]), int(tri[(k + 1) % 3])
            da, db = d[a], d[b]
            if da <= 0.0:
                poly.append(a)
            if (da < 0.0 < db) or (db < 0.0 < da):
                key = (a, b) if a < b else (b, a)
                idx = cache.get(key)
                if idx is None:
                    i, j = key
                    t = d[i] / (d[i] - d[j])
                    p = verts[i] + t * (verts[j] - verts[i])
                    p = p - (p @ normal - offset) * normal  # exactly on the plane
                    new_pts.append(p)
                    idx = n0 + len(new_pts) - 1
                    cache[key] = idx
                poly.append(idx)
        for m in range(1, len(poly) - 1):
            t3 = (poly[0], poly[m], poly[m + 1])
            if len(set(t3)) == 3:
                extra_faces.append(t3)
                extra_art.append(bool(artificial[fi]))
    out_faces = [faces[keep_all]]
    out_art = [artificial[keep_all]]
    if extra_faces:
        out_faces.append(np.asarray(extra_faces, dtype=np.int64))
        out_art.append(np.asarray(extra_art, dtype=bool))
    if new_pts:
        verts = np.vstack([verts, np.asarray(new_pts)])
    kept = np.vstack(out_faces)
    art = np.concatenate(out_art)

    if kept.shape[0] == 0:
        return verts, kept, art, np.zeros((0, 2), np.int64), np.zeros(0, bool)
    directed = kept[:, [0, 1, 1, 2, 2, 0]].reshape(-1, 2)
    n = np.int64(verts.shape[0])
    keys = directed[:, 0] * n + directed[:, 1]
    rev = directed[:, 1] * n + directed[:, 0]
    is_boundary = ~np.isin(rev, keys)
    return verts, kept, art, directed[is_boundary], np.repeat(art, 3)[is_boundary]



def _cap_area_vector(vertices: np.ndarray, boundary: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    """Signed fan cap over boundary edges (a->b) with triangles (c, b, a). Returns (centre, area vector)."""
    c = vertices[np.unique(boundary)].mean(axis=0)
    a = vertices[boundary[:, 0]] - c
    b = vertices[boundary[:, 1]] - c
    return c, 0.5 * np.cross(b, a).sum(axis=0)



@dataclass(frozen=True, slots=True)
class ClippedHull:
    """Closed submerged body in the canonical hull frame: physical faces plus artificial cap."""

    vertices: np.ndarray
    faces: np.ndarray
    artificial: np.ndarray
    waterline_points: np.ndarray
    plane_point: np.ndarray
    plane_normal: np.ndarray
    waterplane_x_axis: np.ndarray

    def __post_init__(self) -> None:
        for name in ("vertices", "faces", "artificial", "waterline_points",
                     "plane_point", "plane_normal", "waterplane_x_axis"):
            object.__setattr__(self, name, _read_only(getattr(self, name)))

    def __getattribute__(self, name):
        value = object.__getattribute__(self, name)
        return value.view() if isinstance(value, np.ndarray) else value

    def __reduce__(self):
        return type(self), tuple(getattr(self, name) for name in self.__dataclass_fields__)

    def face_areas(self) -> np.ndarray:
        return _read_only(np.linalg.norm(_face_area_vectors(self.vertices, self.faces), axis=1))



def _condition_geometry(mesh: TriMesh, draft, trim, x_midship, l_pp):
    lo, hi = mesh.bounds
    draft = float(draft)
    trim = float(trim)
    if not (math.isfinite(draft) and math.isfinite(trim)):
        raise MeshContractError("draft and trim must be finite")
    if draft <= 0.0:
        raise MeshContractError(
            f"dry hull: mean draft {draft!r} m is at or below the keel (baseline z = 0)"
        )
    xm_declared = x_midship is not None
    xm = float(x_midship) if xm_declared else 0.5 * float(lo[0] + hi[0])
    lpp_declared = l_pp is not None
    lpp = float(l_pp) if lpp_declared else float(hi[0] - lo[0])
    if not (math.isfinite(xm) and math.isfinite(lpp)) or lpp <= 0:
        raise MeshContractError("x_midship must be finite and l_pp positive")
    theta = math.atan2(trim, lpp)
    normal = np.array([-math.sin(theta), 0.0, math.cos(theta)])
    ex = np.array([math.cos(theta), 0.0, math.sin(theta)])
    point = np.array([xm, 0.0, draft])
    return draft, trim, xm, xm_declared, lpp, lpp_declared, normal, ex, point



def _closure_check(verts: np.ndarray, faces: np.ndarray) -> None:
    n = np.int64(verts.shape[0])
    directed = faces[:, [0, 1, 1, 2, 2, 0]].reshape(-1, 2)
    keys = np.sort(directed[:, 0] * n + directed[:, 1])
    rev = np.sort(directed[:, 1] * n + directed[:, 0])
    if not np.array_equal(keys, rev):
        raise MeshContractError("post-clip check failed: the capped submerged body is not closed")
    v1 = volume_divergence(verts, faces)
    v2 = volume_tetra(verts, faces)
    if not abs(v1 - v2) <= _CLOSURE_VOLUME_REL * max(abs(v1), abs(v2)):
        raise MeshContractError(
            f"post-clip check failed: volume integrals disagree ({v1!r} vs {v2!r})"
        )



def clip_at_waterline(
    mesh: TriMesh,
    draft: float,
    trim: float = 0.0,
    *,
    x_midship: Optional[float] = None,
    l_pp: Optional[float] = None,
) -> ClippedHull:
    """Cut the mesh at the (trimmed) waterline and cap it with artificial faces.

    The waterplane passes through (x_midship, 0, draft) and rises forward by
    trim / l_pp (trim = T_fwd - T_aft). Refuses a dry or fully submerged hull, and a cut
    that fails the post-clip closure / two-way volume check.
    """
    _, _, _, _, _, _, normal, ex, point = _condition_geometry(mesh, draft, trim, x_midship, l_pp)
    eps = _SNAP_REL * mesh.scale_diag
    verts, faces, art, boundary, _ = _clip_keep_below(
        mesh.vertices, mesh.faces, np.zeros(mesh.faces.shape[0], bool), normal,
        float(normal @ point), eps,
    )
    if faces.shape[0] == 0:
        raise MeshContractError("dry hull: no part of the mesh lies below the waterline (keel)")
    if boundary.shape[0] == 0:
        raise MeshContractError("hull fully submerged: the waterline does not cut the mesh")
    c, _ = _cap_area_vector(verts, boundary)
    ci = verts.shape[0]
    verts = np.vstack([verts, c])
    cap = np.column_stack([np.full(boundary.shape[0], ci), boundary[:, 1], boundary[:, 0]])
    faces = np.vstack([faces, cap])
    art = np.concatenate([art, np.ones(cap.shape[0], bool)])
    _closure_check(verts, faces)
    wl = verts[np.unique(boundary)]
    return ClippedHull(verts, faces, art, wl, point, normal, ex)


ClippedHull.__module__ = "digitalmodel.naval_architecture.mesh_hydrostatics"
