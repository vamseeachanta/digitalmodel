# ABOUTME: Analytic hull geometry for verification: closed box and Wigley triangle meshes
# ABOUTME: plus the Wigley wetted-area reference by adaptive quadrature (#2239 W0 fixtures)
"""
Analytic hull fixtures used to verify :mod:`digitalmodel.naval_architecture.mesh_hydrostatics`.

All geometry is analytic (no vessel data). Meshes are returned in metres in the canonical
frame of the adapter: x forward, y port, z up, baseline z = 0, midship at x = 0.
Factories return independent mutable arrays for geometry authoring; no geometry or
provenance is retained. TriMesh snapshots them on construction.
Re-exported from ``mesh_hydrostatics`` so the public API is unchanged.
"""

from __future__ import annotations

import math
from typing import Optional

import numpy as np


def _signed_volume(vertices: np.ndarray, faces: np.ndarray) -> float:
    a = vertices[faces[:, 0]]
    av = 0.5 * np.cross(vertices[faces[:, 1]] - a, vertices[faces[:, 2]] - a)
    return float((av[:, 2] * vertices[faces][:, :, 2].mean(axis=1)).sum())


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
    if _signed_volume(verts, faces) < 0:  # generator-internal orientation guard
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
