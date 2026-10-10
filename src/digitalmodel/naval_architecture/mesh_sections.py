"""Mesh sections for the mesh hydrostatics adapter."""
from __future__ import annotations

import numpy as np
from .mesh_clipping import ClippedHull, _clip_keep_below, _cap_area_vector


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

