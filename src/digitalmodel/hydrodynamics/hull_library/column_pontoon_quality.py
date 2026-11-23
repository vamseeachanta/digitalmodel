"""Convex triangle-to-quad centres chosen by the resulting edge ratios."""
from itertools import permutations
from typing import cast

import numpy as np
from numpy.typing import NDArray

Array = NDArray[np.float64]


def quad_ratios(triangles: list[Array] | Array, centres: Array) -> Array:
    """Maximum edge ratio across each triangle's three midpoint quads."""
    triangles = np.asarray(triangles, dtype=float)
    centres = np.asarray(centres, dtype=float)
    if not len(triangles):
        return np.empty(0)
    midpoints = (triangles+np.roll(triangles, -1, axis=1))/2
    repeated = np.broadcast_to(centres[:, None, :], triangles.shape)
    quads = np.stack((triangles, midpoints, repeated,
                      np.roll(midpoints, 1, axis=1)), axis=2)
    lengths = np.linalg.norm(quads-np.roll(quads, -1, axis=2), axis=3)
    minimum = lengths.min(axis=2)
    return cast(Array, np.divide(lengths.max(axis=2), minimum,
                     out=np.full_like(minimum, np.inf), where=minimum > 0).max(axis=1))


def optimize_quad_centres(triangles: list[Array] | Array) -> tuple[Array, Array]:
    """Return convex centres and actual worst quad ratios, without moving edges.

    Strict barycentric weights below 0.5 keep every midpoint quad convex.
    Flat triangles that cannot meet a quality bound will remain candidates
    for conforming edge subdivision rather than receive folded quads.
    """
    triangles = np.asarray(triangles, dtype=float)
    if not len(triangles):
        return np.empty((0, 3)), np.empty(0)
    if triangles.ndim != 3 or triangles.shape[1:] != (3, 3):
        raise ValueError("triangle coordinates require shape (n, 3, 3)")
    if not np.isfinite(triangles).all():
        raise ValueError("triangle coordinates must be finite")
    centres = triangles.mean(axis=1)
    quality = quad_ratios(triangles, centres)
    candidates = sorted(set(permutations((.49, .255, .255))) |
                        set(permutations((.49, .49, .02))) |
                        set(permutations((.45, .45, .1))) |
                        set(permutations((.25, .375, .375))))
    opposite = np.linalg.norm(np.roll(triangles, -1, axis=1)
                              - np.roll(triangles, -2, axis=1), axis=2)
    total = opposite.sum(axis=1)
    weights = np.divide(opposite, total[:, None], out=np.zeros_like(opposite),
                        where=total[:, None] > 0)
    dynamic = np.sum(triangles*weights[..., None], axis=1)
    ratio = quad_ratios(triangles, dynamic)
    use = (weights.max(axis=1) < .5) & (weights.min(axis=1) > 0) & (ratio < quality)
    centres[use], quality[use] = dynamic[use], ratio[use]
    for weights in candidates:
        candidate = np.einsum('nij,i->nj', triangles, weights)
        ratio = quad_ratios(triangles, candidate)
        use = ratio < quality
        centres[use], quality[use] = candidate[use], ratio[use]
    return centres, quality
