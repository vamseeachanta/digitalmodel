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
* The mesh is watertight and manifold (every edge used by exactly two faces), consistently
  oriented (each directed edge used once), outward-facing (signed volume > 0; a reversed mesh
  is refused unless ``flip_normals=True`` is passed explicitly) and has no degenerate or
  non-finite faces.
* Trim is T_fwd - T_aft (negative by the stern) over ``l_pp`` and is applied about the midship
  waterline point (x_midship, T_mean), so the mean draft is preserved.
* The waterline cut is capped; **cap faces are tagged artificial**: they count toward volume
  and centroid, never toward wetted area.
* LCB is positive forward of midship, measured along the waterplane from the midship
  waterline point and reported as a percentage of L_wl.
* A_BT and A_T are submerged transverse section areas at caller-declared bulb and transom
  stations; without a declared station they are not computed and carry the reason.
* The half-breadth grid y(x_i, z_j) (max |y| of the submerged body, z from baseline) is given
  only on caller-declared stations and waterlines.

Two independent volume integrations are exposed for verification: :func:`volume_divergence`
(divergence theorem with F = (0, 0, z)) and :func:`volume_tetra` (signed tetrahedra to an
origin). :func:`wigley_mesh` generates independent tessellations of the analytic Wigley hull
and :func:`wigley_wetted_area_reference` integrates its exact surface by adaptive quadrature.
"""

from __future__ import annotations

import hashlib
import json
import math
from collections.abc import Mapping
from dataclasses import dataclass, field
from typing import Iterator, Optional, Sequence

import numpy as np

SCHEMA_VERSION = "mesh_hydrostatics/1"

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
        raise MeshContractError(
            f"axes must be three of {sorted(_AXIS_UNIT)}, got {axes!r}"
        )
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


class TriMesh:
    """Closed, oriented triangle mesh in the canonical SI frame after contract checks.

    ``vertices`` (N, 3) and ``faces`` (M, 3) are given in the caller's declared ``units`` and
    ``axes``; ``self.vertices`` holds them converted to metres in the canonical frame.
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
        v = np.asarray(vertices, dtype=float)
        f = np.asarray(faces)
        if v.ndim != 2 or v.shape[1] != 3 or v.shape[0] < 4:
            raise MeshContractError(f"vertices must be an (N>=4, 3) array, got shape {v.shape}")
        if f.ndim != 2 or f.shape[1] != 3 or f.shape[0] < 4:
            raise MeshContractError(f"faces must be an (M>=4, 3) array, got shape {f.shape}")
        if not np.issubdtype(f.dtype, np.integer):
            raise MeshContractError("faces must be integer vertex indices")
        f = f.astype(np.int64)
        if f.min() < 0 or f.max() >= v.shape[0]:
            raise MeshContractError("face vertex index out of range")

        digest = hashlib.sha256()
        digest.update(np.ascontiguousarray(v).tobytes())
        digest.update(np.ascontiguousarray(f).tobytes())
        digest.update(json.dumps([units, list(axes), bool(flip_normals)]).encode())
        self.source_digest = digest.hexdigest()

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
        _check_edges(f)

        vol = volume_divergence(canon, f)
        if vol <= 0:
            raise MeshContractError(
                "mesh normals point inward (signed volume <= 0); pass flip_normals=True to flip "
                "explicitly"
            )
        zmin = float(canon[used, 2].min())
        if abs(zmin) > _BASELINE_REL * diag:
            raise MeshContractError(
                f"baseline (lowest vertex) must be at canonical z = 0 (draft datum); found z = {zmin!r} m"
            )

        self.vertices = canon
        self.faces = f
        self.units = units
        self.axes = tuple(axes)
        self.scale_diag = diag

    @property
    def bounds(self) -> tuple[np.ndarray, np.ndarray]:
        used = self.vertices[np.unique(self.faces)]
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


# ---------------------------------------------------------------- integrations


def volume_divergence(vertices: np.ndarray, faces: np.ndarray) -> float:
    """Enclosed volume by the divergence theorem with F = (0, 0, z): V = sum A_z * z_centroid."""
    av = _face_area_vectors(np.asarray(vertices, float), np.asarray(faces))
    zc = np.asarray(vertices, float)[np.asarray(faces)][:, :, 2].mean(axis=1)
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

    Returns (vertices, faces, artificial, boundary_edges) where boundary_edges are the
    directed edges of the kept surface with no reverse partner (they lie on the plane).
    Faces entirely on the plane are dropped (the cap replaces them).
    """
    d = vertices @ normal - offset
    d = np.where(np.abs(d) <= eps, 0.0, d)
    df = d[faces]
    on_plane = np.all(df == 0.0, axis=1)
    keep_all = np.all(df <= 0.0, axis=1) & ~on_plane
    drop = np.all(df >= 0.0, axis=1)
    mixed = ~keep_all & ~drop

    new_pts: list = []
    cache: dict = {}
    n0 = vertices.shape[0]
    out_faces = [faces[keep_all]]
    out_art = [artificial[keep_all]]
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
                    new_pts.append(vertices[i] + t * (vertices[j] - vertices[i]))
                    idx = n0 + len(new_pts) - 1
                    cache[key] = idx
                poly.append(idx)
        for m in range(1, len(poly) - 1):
            t3 = (poly[0], poly[m], poly[m + 1])
            if len(set(t3)) == 3:
                extra_faces.append(t3)
                extra_art.append(bool(artificial[fi]))
    if extra_faces:
        out_faces.append(np.asarray(extra_faces, dtype=np.int64))
        out_art.append(np.asarray(extra_art, dtype=bool))
    verts = np.vstack([vertices, np.asarray(new_pts).reshape(-1, 3)]) if new_pts else vertices.copy()
    kept = np.vstack(out_faces) if out_faces else np.zeros((0, 3), np.int64)
    art = np.concatenate(out_art) if out_art else np.zeros(0, bool)

    if kept.shape[0] == 0:
        return verts, kept, art, np.zeros((0, 2), np.int64)
    directed = kept[:, [0, 1, 1, 2, 2, 0]].reshape(-1, 2)
    n = np.int64(verts.shape[0])
    keys = directed[:, 0] * n + directed[:, 1]
    rev = directed[:, 1] * n + directed[:, 0]
    boundary = directed[~np.isin(rev, keys)]
    return verts, kept, art, boundary


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
    trim / l_pp (trim = T_fwd - T_aft). Refuses a dry or fully submerged hull.
    """
    _, _, _, _, _, _, normal, ex, point = _condition_geometry(mesh, draft, trim, x_midship, l_pp)
    eps = _SNAP_REL * mesh.scale_diag
    verts, faces, art, boundary = _clip_keep_below(
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
    wl = verts[np.unique(boundary)]
    return ClippedHull(verts, faces, art, wl, point, normal, ex)


def _section(clipped: ClippedHull, x_station: float, eps: float):
    """Submerged transverse section at hull-frame x: (area, boundary segments (K, 2, 3))."""
    verts, _, _, boundary = _clip_keep_below(
        clipped.vertices, clipped.faces, clipped.artificial,
        np.array([1.0, 0.0, 0.0]), float(x_station), eps,
    )
    if boundary.shape[0] == 0:
        return 0.0, np.zeros((0, 2, 3))
    _, av = _cap_area_vector(verts, boundary)
    segs = np.stack([verts[boundary[:, 0]], verts[boundary[:, 1]]], axis=1)
    return float(av[0]), segs


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
    "sections": "transverse planes normal to hull-frame x",
    "half_breadth": "max |y| of the submerged body at (station x_i, height z_j above baseline)",
}


def _check_station(name: str, x: float, lo: np.ndarray, hi: np.ndarray) -> float:
    x = float(x)
    if not math.isfinite(x) or not (lo[0] < x < hi[0]):
        raise MeshContractError(
            f"{name} station x = {x!r} m lies outside the hull ({lo[0]!r}, {hi[0]!r})"
        )
    return x


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
    (both then tagged ``computed``). Stations are hull-frame x in metres.
    """
    draft, trim, xm, xm_decl, lpp, lpp_decl, normal, ex, point = _condition_geometry(
        mesh, draft, trim, x_midship, l_pp
    )
    lo, hi = mesh.bounds
    bulb = None if bulb_station is None else _check_station("bulb", bulb_station, lo, hi)
    transom = None if transom_station is None else _check_station("transom", transom_station, lo, hi)
    grid_decl = grid_stations is not None and grid_waterlines is not None
    if grid_decl:
        gx = np.asarray(grid_stations, dtype=float).ravel()
        gz = np.asarray(grid_waterlines, dtype=float).ravel()
        if gx.size == 0 or gz.size == 0 or not (np.all(np.isfinite(gx)) and np.all(np.isfinite(gz))):
            raise MeshContractError("grid stations and waterlines must be finite, non-empty")
        for x in gx:
            _check_station("half-breadth grid", x, lo, hi)
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
    eps = _SNAP_REL * mesh.scale_diag
    tol = 1e-9 * mesh.scale_diag

    vol, centroid = _tetra_moments(clipped.vertices, clipped.faces, None)
    areas = clipped.face_areas()
    s_wet = float(areas[~clipped.artificial].sum())
    cap_av = _face_area_vectors(clipped.vertices, clipped.faces[clipped.artificial]).sum(axis=0)
    a_wp = float(cap_av @ normal)

    wl = clipped.waterline_points
    xw = (wl - point) @ ex
    l_wl = float(xw.max() - xw.min())
    b_wl = float(wl[:, 1].max() - wl[:, 1].min())
    lcb_m = float((centroid - point) @ ex)

    a_m, _ = _section(clipped, xm, eps)
    c_b = vol / (l_wl * b_wl * draft)
    c_m = a_m / (b_wl * draft)
    c_p = vol / (a_m * l_wl)
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
        "C_P": comp(c_p, "-"),
        "C_M": comp(c_m, "-"),
        "C_WP": comp(c_wp, "-"),
        "LCB": comp(100.0 * lcb_m / l_wl, "% L_wl"),
    }
    for key, station in (("A_BT", bulb), ("A_T", transom)):
        if station is None:
            q[key] = Quantity(None, "m^2", "not_computed", h,
                              f"no {('bulb' if key == 'A_BT' else 'transom')} station declared; "
                              "the adapter does not infer feature stations")
        else:
            q[key] = comp(_section(clipped, station, eps)[0], "m^2")
    if grid_decl:
        grid = np.zeros((gx.size, gz.size))
        for i, x in enumerate(gx):
            _, segs = _section(clipped, x, eps)
            for j, z in enumerate(gz):
                grid[i, j] = _half_breadth(segs, float(z), tol)
        q["half_breadth"] = Quantity(grid.tolist(), "m", "computed", h,
                                     f"stations x={gx.tolist()} m; heights z={gz.tolist()} m")
    else:
        q["half_breadth"] = Quantity(None, "m", "not_computed", h,
                                     "no grid stations/waterlines declared")
    return HydrostaticsResult(q, h, dict(_CONVENTIONS))


# ---------------------------------------------------------------- analytic geometry


def box_mesh(length: float, beam: float, depth: float) -> tuple[np.ndarray, np.ndarray]:
    """Closed outward box: x in [-L/2, L/2], y in [-B/2, B/2], z in [0, D] (12 faces).

    Vertex index = ix + 2 iy + 4 iz.
    """
    xs = (-length / 2, length / 2)
    ys = (-beam / 2, beam / 2)
    zs = (0.0, depth)
    v = np.array([[xs[i & 1], ys[(i >> 1) & 1], zs[(i >> 2) & 1]] for i in range(8)], float)
    f = np.array(
        [
            [0, 2, 3], [0, 3, 1],  # bottom
            [4, 5, 7], [4, 7, 6],  # deck
            [0, 1, 5], [0, 5, 4],  # starboard side (y = -B/2)
            [2, 6, 7], [2, 7, 3],  # port side (y = +B/2)
            [0, 4, 6], [0, 6, 2],  # aft end
            [1, 3, 7], [1, 7, 5],  # forward end
        ],
        dtype=np.int64,
    )
    return v, f


def wigley_half_breadth(x, z_baseline, length: float, beam: float, draft: float):
    """Wigley y = (B/2)(1-(2x/L)^2)(1-(z/T)^2), z from the waterline (= z_baseline - T)."""
    xi = 2.0 * np.asarray(x, float) / length
    zeta = (np.asarray(z_baseline, float) - draft) / draft
    return 0.5 * beam * (1.0 - xi**2) * (1.0 - zeta**2)


def wigley_mesh(
    length: float,
    beam: float,
    draft: float,
    *,
    depth: Optional[float] = None,
    nx: int = 40,
    nz: int = 12,
    n_topside: int = 2,
    spacing: str = "uniform",
    diagonal: str = "ac",
) -> tuple[np.ndarray, np.ndarray]:
    """Closed Wigley hull (metres, canonical axes, baseline z = 0, midship at x = 0).

    Below the waterline the surface is the analytic Wigley form sampled on an (nx x nz)
    parametric grid (``spacing`` "uniform" or "cosine"); above it, vertical topsides with the
    waterline half-breadth rise to ``depth`` (default 1.5 T) and a flat deck closes the body.
    ``diagonal`` ("ac", "bd", "alternate") selects the quad split. Different grids give
    independent tessellations, not subdivisions of one mesh.
    """
    depth = 1.5 * draft if depth is None else float(depth)
    if depth <= draft or nx < 2 or nz < 1 or n_topside < 1:
        raise ValueError("wigley_mesh needs depth > draft, nx >= 2, nz >= 1, n_topside >= 1")
    if spacing == "uniform":
        u = np.linspace(-1.0, 1.0, nx + 1)
        v = np.linspace(0.0, 1.0, nz + 1)
    elif spacing == "cosine":
        u = -np.cos(np.pi * np.arange(nx + 1) / nx)
        v = 0.5 * (1.0 - np.cos(np.pi * np.arange(nz + 1) / nz))
        u[0], u[-1], v[0], v[-1] = -1.0, 1.0, 0.0, 1.0
    else:
        raise ValueError(f"unknown spacing {spacing!r}")
    if diagonal not in ("ac", "bd", "alternate"):
        raise ValueError(f"unknown diagonal {diagonal!r}")
    x = u * length / 2.0
    z_rows = np.concatenate([v * draft, draft + (depth - draft) * np.arange(1, n_topside + 1) / n_topside])
    nr = z_rows.size

    def half_breadth(i: int, j: int) -> float:
        if j <= nz:
            return float(wigley_half_breadth(x[i], z_rows[j], length, beam, draft))
        return float(0.5 * beam * (1.0 - u[i] ** 2))

    def on_centre(i: int, j: int) -> bool:
        return j == 0 or i == 0 or i == nx

    index: dict = {}
    pts: list = []

    def vid(side: str, i: int, j: int) -> int:
        key = ("c", i, j) if on_centre(i, j) else (side, i, j)
        if key not in index:
            y = 0.0 if key[0] == "c" else half_breadth(i, j) * (1.0 if side == "p" else -1.0)
            index[key] = len(pts)
            pts.append((x[i], y, z_rows[j]))
        return index[key]

    tris: list = []

    def centre_only(t) -> bool:
        return all(on_centre(i, j) for (i, j) in t)

    for j in range(nr - 1):
        for i in range(nx):
            a, b, c, d = (i, j), (i + 1, j), (i + 1, j + 1), (i, j + 1)
            split_ac = [(a, d, c), (a, c, b)]
            split_bd = [(a, d, b), (b, d, c)]
            prefer_ac = diagonal == "ac" or (diagonal == "alternate" and (i + j) % 2 == 0)
            first, second = (split_ac, split_bd) if prefer_ac else (split_bd, split_ac)
            chosen = second if any(centre_only(t) for t in first) else first
            for t in chosen:
                port = tuple(vid("p", *p) for p in t)
                stbd = tuple(vid("s", *p) for p in t)[::-1]
                for tri in (port, stbd):
                    if len(set(tri)) == 3:
                        tris.append(tri)
    jt = nr - 1
    for i in range(nx):
        p0, p1 = vid("p", i, jt), vid("p", i + 1, jt)
        s0, s1 = vid("s", i, jt), vid("s", i + 1, jt)
        for tri in ((p0, s1, p1), (p0, s0, s1)):
            if len(set(tri)) == 3:
                tris.append(tri)
    verts = np.asarray(pts, float)
    faces = np.asarray(tris, np.int64)
    if volume_divergence(verts, faces) < 0:  # generator-internal orientation guard
        faces = faces[:, ::-1].copy()
    return verts, faces


def wigley_wetted_area_reference(length: float, beam: float, draft: float, *, epsrel: float = 1e-12) -> float:
    """Wetted area of the analytic Wigley hull at its design draft by adaptive quadrature.

    S = 2 * integral over x in [-L/2, L/2], z in [0, T] of sqrt(1 + y_x^2 + y_z^2).
    """
    from scipy import integrate

    def integrand(z, x):
        xi = 2.0 * x / length
        zeta = (z - draft) / draft
        y_x = 0.5 * beam * (-4.0 * xi / length) * (1.0 - zeta**2)
        y_z = 0.5 * beam * (1.0 - xi**2) * (-2.0 * zeta / draft)
        return math.sqrt(1.0 + y_x * y_x + y_z * y_z)

    half, _ = integrate.dblquad(integrand, 0.0, length / 2.0, 0.0, draft, epsabs=0.0, epsrel=epsrel)
    return 4.0 * half
