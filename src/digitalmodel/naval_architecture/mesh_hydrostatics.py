# ABOUTME: Triangle-mesh hull adapter: geometry contract checks, waterline clip with trim, hydrostatics
# ABOUTME: Refuses (never repairs) bad meshes; outputs tagged computed/declared with an input hash (#2239 W0)
"""
Mesh hydrostatics adapter (issue #2239, work package W0).

Geometry contract (violations are refused with :class:`MeshContractError`, never repaired):

* Canonical frame: right-handed, x positive forward, y positive to port, z positive up.
  The caller declares how the mesh's own axes map onto it (``axes``), e.g.
  ``("forward", "port", "up")`` or ``("aft", "starboard", "up")``; a left-handed or
  non-permutation declaration is refused.
* Units are declared, ``"m"`` or ``"mm"``; an undeclared mesh is refused. All outputs are SI.
  Condition inputs (draft, trim, stations) are always metres in the canonical frame.
* The draft datum is the baseline (keel) at canonical z = 0: the lowest vertex must lie on it.
* Topology and orientation policy:

  - every edge is used by exactly two faces (watertight, no non-manifold edges);
  - each directed edge is used once (consistent orientation);
  - every vertex has a single fan of faces around it (no non-manifold vertices, e.g. two
    shells touching at one shared vertex);
  - the mesh may have several connected components (e.g. twin hulls), but **every component
    must enclose positive signed volume** (outward normals; inward shells such as cavities
    or reversed bodies are refused) and **component axis-aligned bounding boxes must be
    pairwise disjoint** (overlap > 1e-9 of the mesh diagonal is refused; this also refuses
    nested shells). Triangle-triangle intersection *within* a component is not tested.
  - no degenerate (zero-area / repeated-index) or non-finite faces.

  A mesh reversed as a whole is refused unless ``flip_normals=True`` is passed explicitly.
* The validated vertex and face arrays are private read-only copies; the digest is computed
  from the validated canonical geometry.
* Trim is T_fwd - T_aft (negative by the stern) over ``l_pp`` and is applied about the midship
  waterline point (x_midship, T_mean), so the mean draft is preserved. ``x_midship`` must lie
  within the waterline extent.
* The waterline cut is capped; **cap faces are tagged artificial**: they count toward volume
  and centroid integrals, never toward wetted area, and never toward any geometric extent
  (L_wl, B_wl, half-breadths come from physical faces only). The cap is a signed fan, exact
  for integrals over concave or multi-loop waterplanes. Signed distances within 1e-12 of the
  mesh diagonal are snapped and the vertex is projected onto the cutting plane; after the
  cut the body is checked closed and its two independent volume integrals must agree to
  1e-9 relative.
* LCB is positive forward of midship, measured along the waterplane from the midship
  waterline point and reported as a percentage of L_wl.
* Sections are transverse planes normal to hull-frame x. A station may lie anywhere in the
  closed hull x-range **including its end points**: the section is taken as the boundary of
  the part of the body on the side of the station that contains midship, so at an end
  station it is the (physical) end face itself, e.g. an immersed transom.
  A_BT and A_T are submerged section areas at caller-declared bulb and transom stations;
  without a declared station they are not computed and carry the reason.
* The half-breadth grid y(x_i, z_j) (max |y| of the physical submerged section, z from
  baseline; symmetric hull assumed) is given only on caller-declared stations and waterlines.
* Denominators are checked: V, L_wl and B_wl at or below 1e-12 diag^3 / 1e-8 diag are refused
  as degenerate; a dry midship section (A_M ~ 0, e.g. between tandem bodies) makes C_M and
  C_P ``not_computed`` with the reason.

Two independent volume integrations are exposed for verification: :func:`volume_divergence`
(divergence theorem with F = (0, 0, z)) and :func:`volume_tetra` (signed tetrahedra to an
origin). Analytic fixtures (box, Wigley, Wigley wetted-area quadrature) live in
:mod:`digitalmodel.naval_architecture.hull_fixtures` and are re-exported here.
"""

from __future__ import annotations

import hashlib
import json
import math
from collections.abc import Mapping
from dataclasses import dataclass, field
from typing import Iterator, Optional, Sequence

import numpy as np

from digitalmodel.naval_architecture.hull_fixtures import (  # noqa: F401  (re-exported API)
    box_mesh,
    wigley_half_breadth,
    wigley_mesh,
    wigley_wetted_area_reference,
)

SCHEMA_VERSION = "mesh_hydrostatics/2"

UNIT_SCALE = {"m": 1.0, "mm": 1e-3}

_AXIS_UNIT = {
    "forward": (1.0, 0.0, 0.0),
    "aft": (-1.0, 0.0, 0.0),
    "port": (0.0, 1.0, 0.0),
    "starboard": (0.0, -1.0, 0.0),
    "up": (0.0, 0.0, 1.0),
    "down": (0.0, 0.0, -1.0),
}

CANONICAL_AXES = ("forward", "port", "up")

# Relative tolerances (scaled by the mesh bounding-box diagonal).
_DEGENERATE_AREA_REL = 1e-12
_SNAP_REL = 1e-12
_BASELINE_REL = 1e-9
_OVERLAP_REL = 1e-9
_LENGTH_DEGENERATE_REL = 1e-8
_VOLUME_DEGENERATE_REL = 1e-12
_CLOSURE_VOLUME_REL = 1e-9


class MeshContractError(ValueError):
    """The mesh or loading condition violates the geometry contract (refused, not repaired)."""


# ---------------------------------------------------------------- mesh container


def _axes_matrix(axes: Optional[Sequence[str]]) -> np.ndarray:
    if axes is None:
        raise MeshContractError(
            "axes must be declared, e.g. ('forward', 'port', 'up'); undeclared axes are refused"
        )
    axes = tuple(axes)
    if len(axes) != 3 or any(a not in _AXIS_UNIT for a in axes):
        raise MeshContractError(f"axes must be three of {sorted(_AXIS_UNIT)}, got {axes!r}")
    r = np.column_stack([_AXIS_UNIT[a] for a in axes])
    if not np.allclose(np.abs(r).sum(axis=0), 1.0) or not np.allclose(np.abs(r).sum(axis=1), 1.0):
        raise MeshContractError(f"axes {axes!r} do not form a permutation of the canonical axes")
    if np.linalg.det(r) < 0:
        raise MeshContractError(
            f"axes {axes!r} declare a left-handed frame; the contract requires right-handed axes"
        )
    return r


def _face_area_vectors(vertices: np.ndarray, faces: np.ndarray) -> np.ndarray:
    a = vertices[faces[:, 0]]
    return 0.5 * np.cross(vertices[faces[:, 1]] - a, vertices[faces[:, 2]] - a)


def _read_only(a: np.ndarray) -> np.ndarray:
    a = np.array(a, copy=True)
    a.flags.writeable = False
    return a


class TriMesh:
    """Closed, oriented triangle mesh in the canonical SI frame after contract checks.

    ``vertices`` (N, 3) and ``faces`` (M, 3) are given in the caller's declared ``units`` and
    ``axes``; ``self.vertices`` / ``self.faces`` are read-only copies in metres in the
    canonical frame.
    """

    def __init__(
        self,
        vertices,
        faces,
        *,
        units: Optional[str],
        axes: Optional[Sequence[str]],
        flip_normals: bool = False,
    ) -> None:
        if units not in UNIT_SCALE:
            raise MeshContractError(
                f"units must be declared as one of {sorted(UNIT_SCALE)}, got {units!r}"
            )
        r = _axes_matrix(axes)
        v = np.array(vertices, dtype=float, copy=True)
        f = np.array(faces, copy=True)
        if v.ndim != 2 or v.shape[1] != 3 or v.shape[0] < 4:
            raise MeshContractError(f"vertices must be an (N>=4, 3) array, got shape {v.shape}")
        if f.ndim != 2 or f.shape[1] != 3 or f.shape[0] < 4:
            raise MeshContractError(f"faces must be an (M>=4, 3) array, got shape {f.shape}")
        if not np.issubdtype(f.dtype, np.integer):
            raise MeshContractError("faces must be integer vertex indices")
        f = f.astype(np.int64)
        if f.min() < 0 or f.max() >= v.shape[0]:
            raise MeshContractError("face vertex index out of range")
        if not np.all(np.isfinite(v)):
            raise MeshContractError("mesh has non-finite vertex coordinates")

        canon = (v @ r.T) * UNIT_SCALE[units]
        used = np.unique(f)
        extent = canon[used].max(axis=0) - canon[used].min(axis=0)
        diag = float(np.linalg.norm(extent))
        if diag <= 0:
            raise MeshContractError("mesh has zero extent")

        repeated = (f[:, 0] == f[:, 1]) | (f[:, 1] == f[:, 2]) | (f[:, 2] == f[:, 0])
        areas = np.linalg.norm(_face_area_vectors(canon, f), axis=1)
        degenerate = repeated | (areas <= _DEGENERATE_AREA_REL * diag**2)
        if np.any(degenerate):
            raise MeshContractError(
                f"mesh has {int(degenerate.sum())} degenerate (zero-area or repeated-index) faces, "
                f"first at index {int(np.nonzero(degenerate)[0][0])}"
            )

        if flip_normals:
            f = f[:, ::-1].copy()
        _check_topology(canon, f, diag)

        zmin = float(canon[used, 2].min())
        if abs(zmin) > _BASELINE_REL * diag:
            raise MeshContractError(
                f"baseline (lowest vertex) must be at canonical z = 0 (draft datum); found z = {zmin!r} m"
            )

        digest = hashlib.sha256()
        digest.update(np.ascontiguousarray(canon).tobytes())
        digest.update(np.ascontiguousarray(f).tobytes())
        digest.update(json.dumps([units, list(axes), bool(flip_normals)]).encode())
        self._vertices = _read_only(canon)
        self._faces = _read_only(f)
        self.source_digest = digest.hexdigest()
        self.units = units
        self.axes = tuple(axes)
        self.scale_diag = diag

    @property
    def vertices(self) -> np.ndarray:
        return self._vertices

    @property
    def faces(self) -> np.ndarray:
        return self._faces

    @property
    def bounds(self) -> tuple[np.ndarray, np.ndarray]:
        used = self._vertices[np.unique(self._faces)]
        return used.min(axis=0), used.max(axis=0)


def _check_edges(faces: np.ndarray) -> None:
    directed = faces[:, [0, 1, 1, 2, 2, 0]].reshape(-1, 2)
    undirected = np.sort(directed, axis=1)
    _, counts = np.unique(undirected, axis=0, return_counts=True)
    if np.any(counts > 2):
        raise MeshContractError(
            f"mesh is non-manifold: {int((counts > 2).sum())} edges are shared by more than two faces"
        )
    if np.any(counts == 1):
        raise MeshContractError(
            f"mesh is open (not watertight): {int((counts == 1).sum())} boundary edges"
        )
    uniq_directed = np.unique(directed, axis=0)
    if uniq_directed.shape[0] != directed.shape[0]:
        raise MeshContractError(
            "mesh has inconsistent face orientation: a directed edge is used by two faces"
        )


def _check_topology(canon: np.ndarray, faces: np.ndarray, diag: float) -> None:
    """Edge, vertex-fan, per-component orientation and component-overlap checks."""
    from scipy.sparse import coo_matrix
    from scipy.sparse.csgraph import connected_components

    _check_edges(faces)
    m = faces.shape[0]
    n = np.int64(canon.shape[0])
    directed = faces[:, [0, 1, 1, 2, 2, 0]].reshape(-1, 2)
    key = directed[:, 0] * n + directed[:, 1]
    rkey = directed[:, 1] * n + directed[:, 0]
    order = np.argsort(key)
    twin = order[np.searchsorted(key[order], rkey)]  # every reverse exists exactly once here

    e = np.arange(3 * m)
    face_e, k_e = e // 3, e % 3
    face_t, k_t = twin // 3, twin % 3
    # edge (a -> b) of face f and its twin (b -> a) of face g are neighbours in the fan of a
    # and in the fan of b: link the corresponding corners (corner id = 3 * face + slot).
    rows = np.concatenate([3 * face_e + k_e, 3 * face_e + (k_e + 1) % 3])
    cols = np.concatenate([3 * face_t + (k_t + 1) % 3, 3 * face_t + k_t])
    graph = coo_matrix((np.ones(rows.size), (rows, cols)), shape=(3 * m, 3 * m))
    _, labels = connected_components(graph, directed=False)
    pairs = np.unique(np.column_stack([faces.ravel(), labels]), axis=0)
    fans = np.bincount(pairs[:, 0])
    if np.any(fans > 1):
        bad = np.nonzero(fans > 1)[0]
        raise MeshContractError(
            f"mesh has {bad.size} non-manifold vertex/vertices (several face fans meet at one "
            f"vertex), first at index {int(bad[0])}"
        )

    fgraph = coo_matrix((np.ones(3 * m), (face_e, face_t)), shape=(m, m))
    ncomp, flabels = connected_components(fgraph, directed=False)
    av = _face_area_vectors(canon, faces)
    zc = canon[faces][:, :, 2].mean(axis=1)
    vols = np.bincount(flabels, weights=av[:, 2] * zc, minlength=ncomp)
    if np.any(vols <= 0):
        raise MeshContractError(
            f"{int((vols <= 0).sum())} of {ncomp} mesh component(s) have inward normals "
            "(signed volume <= 0: reversed body or internal cavity); pass flip_normals=True only "
            "if the whole mesh is reversed"
        )
    if ncomp > 1:
        lo = np.full((ncomp, 3), np.inf)
        hi = np.full((ncomp, 3), -np.inf)
        pts = canon[faces]  # (m, 3, 3)
        np.minimum.at(lo, flabels, pts.min(axis=1))
        np.maximum.at(hi, flabels, pts.max(axis=1))
        tol = _OVERLAP_REL * diag
        for i in range(ncomp):
            ov = np.minimum(hi[i], hi[i + 1:]) - np.maximum(lo[i], lo[i + 1:])
            clash = np.all(ov > tol, axis=1)
            if np.any(clash):
                j = i + 1 + int(np.nonzero(clash)[0][0])
                raise MeshContractError(
                    f"mesh components {i} and {j} have overlapping bounding boxes (overlapping or "
                    "nested shells are refused)"
                )


# ---------------------------------------------------------------- integrations


def volume_divergence(vertices: np.ndarray, faces: np.ndarray) -> float:
    """Enclosed volume by the divergence theorem with F = (0, 0, z): V = sum A_z * z_centroid."""
    vertices = np.asarray(vertices, float)
    faces = np.asarray(faces)
    av = _face_area_vectors(vertices, faces)
    zc = vertices[faces][:, :, 2].mean(axis=1)
    return float((av[:, 2] * zc).sum())


def _tetra_moments(vertices: np.ndarray, faces: np.ndarray, origin: Optional[np.ndarray]):
    vertices = np.asarray(vertices, float)
    faces = np.asarray(faces)
    if origin is None:
        origin = vertices[np.unique(faces)].mean(axis=0)
    o = np.asarray(origin, float)
    a = vertices[faces[:, 0]] - o
    b = vertices[faces[:, 1]] - o
    c = vertices[faces[:, 2]] - o
    six_v = np.einsum("ij,ij->i", a, np.cross(b, c))
    vol = six_v.sum() / 6.0
    centroid = o + (six_v[:, None] * (a + b + c)).sum(axis=0) / (4.0 * six_v.sum())
    return float(vol), centroid


def volume_tetra(vertices: np.ndarray, faces: np.ndarray, origin: Optional[np.ndarray] = None) -> float:
    """Enclosed volume as a sum of signed tetrahedra (origin, v0, v1, v2)."""
    return _tetra_moments(vertices, faces, origin)[0]


# ---------------------------------------------------------------- clipping


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


@dataclass
class ClippedHull:
    """Closed submerged body in the canonical hull frame: physical faces plus artificial cap."""

    vertices: np.ndarray
    faces: np.ndarray
    artificial: np.ndarray
    waterline_points: np.ndarray
    plane_point: np.ndarray
    plane_normal: np.ndarray
    waterplane_x_axis: np.ndarray

    def face_areas(self) -> np.ndarray:
        return np.linalg.norm(_face_area_vectors(self.vertices, self.faces), axis=1)


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


def _section(clipped: ClippedHull, x_station: float, x_keep_side: float, eps: float):
    """Submerged transverse section at hull-frame x.

    The body is clipped to the side of the station that contains ``x_keep_side`` (midship),
    so that at an end station the section is the end face. Only faces whose x-range spans the
    station are clipped. Returns (area, physical boundary segments (K, 2, 3)); the area uses
    the signed fan over all boundary edges (exact integral), the segments exclude edges of
    artificial (cap) faces.
    """
    s = float(x_station)
    if x_keep_side >= s:
        normal, offset = np.array([-1.0, 0.0, 0.0]), -s  # keep x >= s
    else:
        normal, offset = np.array([1.0, 0.0, 0.0]), s  # keep x <= s
    fx = clipped.vertices[clipped.faces][:, :, 0]
    cand = (fx.min(axis=1) <= s + eps) & (fx.max(axis=1) >= s - eps)
    if not np.any(cand):
        return 0.0, np.zeros((0, 2, 3))
    verts, _, _, boundary, b_art = _clip_keep_below(
        clipped.vertices, clipped.faces[cand], clipped.artificial[cand], normal, offset, eps
    )
    if boundary.shape[0]:
        on = (np.abs(verts[boundary[:, 0], 0] - s) <= eps) & (np.abs(verts[boundary[:, 1], 0] - s) <= eps)
        boundary, b_art = boundary[on], b_art[on]
    if boundary.shape[0] == 0:
        return 0.0, np.zeros((0, 2, 3))
    _, av = _cap_area_vector(verts, boundary)
    area = float(av @ normal)
    phys = boundary[~b_art]
    segs = np.stack([verts[phys[:, 0]], verts[phys[:, 1]]], axis=1)
    return area, segs


def _half_breadth(segs: np.ndarray, z: float, tol: float) -> float:
    best = 0.0
    for (pa, pb) in segs:
        za, zb = pa[2], pb[2]
        if min(za, zb) - tol > z or max(za, zb) + tol < z:
            continue
        if abs(zb - za) <= tol:
            ys = (abs(pa[1]), abs(pb[1]))
        else:
            t = (z - za) / (zb - za)
            ys = (abs(pa[1] + t * (pb[1] - pa[1])),)
        best = max(best, *ys)
    return float(best)


# ---------------------------------------------------------------- output schema


@dataclass(frozen=True)
class Quantity:
    value: object
    units: str
    provenance: str  # "computed" | "declared" | "not_computed"
    input_hash: str
    reason: Optional[str] = None

    def to_dict(self) -> dict:
        v = self.value
        if isinstance(v, np.ndarray):
            v = v.tolist()
        return {"value": v, "units": self.units, "provenance": self.provenance,
                "input_hash": self.input_hash, "reason": self.reason}


@dataclass(frozen=True)
class HydrostaticsResult(Mapping):
    quantities: dict
    input_hash: str
    conventions: dict = field(default_factory=dict)

    def __getitem__(self, key: str) -> Quantity:
        return self.quantities[key]

    def __iter__(self) -> Iterator[str]:
        return iter(self.quantities)

    def __len__(self) -> int:
        return len(self.quantities)

    def to_dict(self) -> dict:
        return {
            "schema": SCHEMA_VERSION,
            "input_hash": self.input_hash,
            "conventions": dict(self.conventions),
            "quantities": {k: q.to_dict() for k, q in self.quantities.items()},
        }


_CONVENTIONS = {
    "frame": "right-handed; x forward, y port, z up; baseline z = 0",
    "trim": "T_fwd - T_aft over l_pp, negative by the stern, about the midship waterline point",
    "LCB": "percent of L_wl, positive forward of midship, along the waterplane",
    "S": "physical (non-cap) submerged faces only",
    "extents": "L_wl, B_wl and half-breadths from physical faces only",
    "sections": "transverse planes normal to hull-frame x; end stations give the end face",
    "half_breadth": "max |y| of the physical submerged section at (x_i, height z_j above baseline)",
}


def _check_station(name: str, x: float, lo: np.ndarray, hi: np.ndarray, tol: float) -> float:
    x = float(x)
    if not math.isfinite(x) or not (lo[0] - tol <= x <= hi[0] + tol):
        raise MeshContractError(
            f"{name} station x = {x!r} m lies outside the hull [{lo[0]!r}, {hi[0]!r}]"
        )
    return min(max(x, float(lo[0])), float(hi[0]))


def compute_hydrostatics(
    mesh: TriMesh,
    draft: float,
    trim: float = 0.0,
    *,
    x_midship: Optional[float] = None,
    l_pp: Optional[float] = None,
    bulb_station: Optional[float] = None,
    transom_station: Optional[float] = None,
    grid_stations: Optional[Sequence[float]] = None,
    grid_waterlines: Optional[Sequence[float]] = None,
) -> HydrostaticsResult:
    """Hydrostatics of the mesh at mean draft ``draft`` (m) and trim (m) in the canonical frame.

    ``x_midship`` defaults to the mid-point of the mesh x-extent and ``l_pp`` to that extent
    (both then tagged ``computed``). Stations are hull-frame x in metres, end points included.
    """
    draft, trim, xm, xm_decl, lpp, lpp_decl, normal, ex, point = _condition_geometry(
        mesh, draft, trim, x_midship, l_pp
    )
    lo, hi = mesh.bounds
    diag = mesh.scale_diag
    eps = _SNAP_REL * diag
    tol = 1e-9 * diag
    bulb = None if bulb_station is None else _check_station("bulb", bulb_station, lo, hi, eps)
    transom = None if transom_station is None else _check_station("transom", transom_station, lo, hi, eps)
    grid_decl = grid_stations is not None and grid_waterlines is not None
    if grid_decl:
        gx = np.asarray(grid_stations, dtype=float).ravel()
        gz = np.asarray(grid_waterlines, dtype=float).ravel()
        if gx.size == 0 or gz.size == 0 or not (np.all(np.isfinite(gx)) and np.all(np.isfinite(gz))):
            raise MeshContractError("grid stations and waterlines must be finite, non-empty")
        gx = np.array([_check_station("half-breadth grid", x, lo, hi, eps) for x in gx])
        if np.any(gz < 0):
            raise MeshContractError("grid waterlines are heights above baseline and must be >= 0")

    payload = json.dumps(
        {
            "mesh": mesh.source_digest, "draft": repr(draft), "trim": repr(trim),
            "x_midship": None if x_midship is None else repr(float(x_midship)),
            "l_pp": None if l_pp is None else repr(float(l_pp)),
            "bulb": None if bulb is None else repr(bulb),
            "transom": None if transom is None else repr(transom),
            "grid_x": None if not grid_decl else [repr(float(v)) for v in gx],
            "grid_z": None if not grid_decl else [repr(float(v)) for v in gz],
        },
        sort_keys=True,
    )
    h = hashlib.sha256(payload.encode()).hexdigest()

    clipped = clip_at_waterline(mesh, draft, trim, x_midship=x_midship, l_pp=l_pp)
    wl = clipped.waterline_points
    if not (wl[:, 0].min() - eps <= xm <= wl[:, 0].max() + eps):
        raise MeshContractError(
            f"x_midship = {xm!r} m lies outside the waterline extent "
            f"[{wl[:, 0].min()!r}, {wl[:, 0].max()!r}] m"
        )

    vol, centroid = _tetra_moments(clipped.vertices, clipped.faces, None)
    if vol <= _VOLUME_DEGENERATE_REL * diag**3:
        raise MeshContractError(f"degenerate submerged volume V = {vol!r} m^3")
    areas = clipped.face_areas()
    s_wet = float(areas[~clipped.artificial].sum())
    cap_av = _face_area_vectors(clipped.vertices, clipped.faces[clipped.artificial]).sum(axis=0)
    a_wp = float(cap_av @ normal)

    xw = (wl - point) @ ex
    l_wl = float(xw.max() - xw.min())
    b_wl = float(wl[:, 1].max() - wl[:, 1].min())
    if l_wl <= _LENGTH_DEGENERATE_REL * diag or b_wl <= _LENGTH_DEGENERATE_REL * diag:
        raise MeshContractError(
            f"degenerate waterplane: L_wl = {l_wl!r} m, B_wl = {b_wl!r} m"
        )
    lcb_m = float((centroid - point) @ ex)

    a_m, _ = _section(clipped, xm, xm, eps)
    c_b = vol / (l_wl * b_wl * draft)
    c_wp = a_wp / (l_wl * b_wl)

    def comp(v, u):
        return Quantity(float(v), u, "computed", h)

    def decl(v, u):
        return Quantity(float(v), u, "declared", h)

    q: dict = {
        "draft_mean": decl(draft, "m"),
        "trim": decl(trim, "m"),
        "draft_fwd": decl(draft + 0.5 * trim, "m"),
        "draft_aft": decl(draft - 0.5 * trim, "m"),
        "x_midship": (decl if xm_decl else comp)(xm, "m"),
        "l_pp": (decl if lpp_decl else comp)(lpp, "m"),
        "V": comp(vol, "m^3"),
        "S": comp(s_wet, "m^2"),
        "L_wl": comp(l_wl, "m"),
        "B_wl": comp(b_wl, "m"),
        "A_WP": comp(a_wp, "m^2"),
        "A_M": comp(a_m, "m^2"),
        "C_B": comp(c_b, "-"),
        "C_WP": comp(c_wp, "-"),
        "LCB": comp(100.0 * lcb_m / l_wl, "% L_wl"),
    }
    if a_m <= _DEGENERATE_AREA_REL * diag**2:
        reason = (f"midship section at x = {xm!r} m is dry (A_M = {a_m!r} m^2); "
                  "C_M and C_P are undefined")
        q["A_M"] = comp(0.0, "m^2")
        q["C_M"] = Quantity(None, "-", "not_computed", h, reason)
        q["C_P"] = Quantity(None, "-", "not_computed", h, reason)
    else:
        q["C_M"] = comp(a_m / (b_wl * draft), "-")
        q["C_P"] = comp(vol / (a_m * l_wl), "-")
    for key, station in (("A_BT", bulb), ("A_T", transom)):
        if station is None:
            q[key] = Quantity(None, "m^2", "not_computed", h,
                              f"no {('bulb' if key == 'A_BT' else 'transom')} station declared; "
                              "the adapter does not infer feature stations")
        else:
            q[key] = comp(_section(clipped, station, xm, eps)[0], "m^2")
    if grid_decl:
        grid = np.zeros((gx.size, gz.size))
        for i, x in enumerate(gx):
            _, segs = _section(clipped, x, xm, eps)
            for j, z in enumerate(gz):
                grid[i, j] = _half_breadth(segs, float(z), tol)
        q["half_breadth"] = Quantity(grid.tolist(), "m", "computed", h,
                                     f"stations x={gx.tolist()} m; heights z={gz.tolist()} m")
    else:
        q["half_breadth"] = Quantity(None, "m", "not_computed", h,
                                     "no grid stations/waterlines declared")
    return HydrostaticsResult(q, h, dict(_CONVENTIONS))
