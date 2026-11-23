"""Quality-improving diagonal flips confined to coplanar triangle pairs."""
from collections import defaultdict
from typing import cast

import numpy as np
from numpy.typing import NDArray

Array = NDArray[np.float64]


def _edge(a: Array, b: Array) -> tuple:
    return tuple(sorted((tuple(np.round(a, 8)), tuple(np.round(b, 8)))))


def _score(triangle: Array) -> float:
    """Largest corner cosine; lower means a larger minimum triangle angle."""
    edges = np.roll(triangle, -1, axis=0)-triangle
    lengths = np.linalg.norm(edges, axis=1)
    if lengths.min() < 1e-12:
        return 1.
    cosines = -np.sum(edges*np.roll(edges, 1, axis=0), axis=1)/(lengths*np.roll(lengths, 1))
    return float(cosines.max())


def _candidate(first: Array, second: Array, edge: tuple) -> list[Array] | None:
    """Return positively oriented alternate triangles for a convex plane pair."""
    a, b = [next(v for v in first if tuple(np.round(v, 8)) == key) for key in edge]
    c = next(v for v in first if tuple(np.round(v, 8)) not in edge)
    d = next(v for v in second if tuple(np.round(v, 8)) not in edge)
    n1 = np.cross(first[1]-first[0], first[2]-first[0])
    n2 = np.cross(second[1]-second[0], second[2]-second[0])
    if min(np.linalg.norm(n1), np.linalg.norm(n2)) < 1e-12:
        return None
    n1, n2 = n1/np.linalg.norm(n1), n2/np.linalg.norm(n2)
    if np.linalg.norm(n1-n2) > 1e-6 or abs((d-first[0]) @ n1) > 1e-7:
        return None
    proposed = [np.array([c, d, a]), np.array([d, c, b])]
    areas = [np.cross(t[1]-t[0], t[2]-t[0]) @ n1 for t in proposed]
    if areas[0] < 0 and areas[1] < 0:
        proposed = [t[::-1] for t in proposed]
        areas = [-v for v in areas]
    if min(areas) <= 1e-12:
        return None
    old_score = max(_score(first), _score(second))
    if max(_score(t) for t in proposed) >= old_score-1e-10:
        return None
    return proposed


def flip_planar(triangles: list[Array], groups: list[int], *, max_passes: int = 40) -> tuple[list[Array], list[int], set[int]]:
    """Flip unconstrained interior diagonals; retain coordinates and group IDs."""
    triangles = list(triangles)
    changed: set[int] = set()
    for _ in range(max_passes):
        edges = defaultdict(list)
        for i, triangle in enumerate(triangles):
            for a, b in zip(triangle, np.roll(triangle, -1, axis=0)):
                edges[_edge(a, b)].append(i)
        scores = triangle_scores(triangles)
        reserved, flipped = set(), False
        for edge, owners in edges.items():
            if len(owners) != 2 or any(i in reserved for i in owners):
                continue
            i, j = owners
            if max(scores[i], scores[j]) < 0.99:
                continue
            proposed = _candidate(triangles[i], triangles[j], edge)
            if proposed is None:
                continue
            shared = set(map(tuple, np.round(proposed[0], 8))) & set(map(tuple, np.round(proposed[1], 8)))
            if tuple(sorted(shared)) in edges:
                continue
            triangles[i], triangles[j] = proposed
            scores[i], scores[j] = [_score(t) for t in proposed]
            changed.update((groups[i], groups[j]))
            reserved.update(owners)
            flipped = True
        if not flipped:
            break
    return triangles, list(groups), changed


def triangle_scores(triangles: list[Array]) -> Array:
    """Largest corner cosine detects flat triangles even with equal edge lengths."""
    coordinates = np.asarray(triangles)
    vectors = np.roll(coordinates, -1, axis=1)-coordinates
    lengths = np.linalg.norm(vectors, axis=2)
    denominator = np.maximum(lengths*np.roll(lengths, 1, axis=1), 1e-30)
    return cast(Array, (-np.sum(vectors*np.roll(vectors, 1, axis=1), axis=2)/denominator).max(axis=1))
