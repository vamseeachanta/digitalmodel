"""Conforming local triangulation/refinement for clipped convex patches."""
import numpy as np
from numpy.typing import NDArray

from .column_pontoon_flip import flip_planar
from .column_pontoon_quality import optimize_quad_centres

Array = NDArray[np.float64]


def triangulate(ring: Array) -> list[Array]:
    """Remove convex ears while retaining seam vertices on straight edges."""
    ids = list(range(len(ring)))
    normal = np.sum(np.cross(ring, np.roll(ring, -1, axis=0)), axis=0)
    result = []
    while len(ids) > 3:
        choices = []
        for j in range(len(ids)):
            tri = ring[[ids[j-1], ids[j], ids[(j+1) % len(ids)]]]
            area = np.dot(np.cross(tri[1]-tri[0], tri[2]-tri[0]), normal)
            remaining = ring[ids[:j] + ids[j+1:]]
            rem_area = np.dot(np.sum(np.cross(remaining, np.roll(remaining, -1, axis=0)), axis=0), normal)
            if area > np.linalg.norm(normal)*1e-7 and rem_area > np.linalg.norm(normal)*1e-7:
                lengths = np.linalg.norm(tri-np.roll(tri, -1, axis=0), axis=1)
                choices.append((lengths.min()/lengths.max(), j, tri))
        if not choices:
            break
        _, j, tri = max(choices, key=lambda item: item[0])
        result.append(tri)
        ids.pop(j)
    if len(ids) == 3:
        result.append(ring[ids])
    return result


def refine(patches: list[Array], limit: float = 20) -> tuple[list[Array], list[int], set[int]]:
    """Bisect long triangle edges consistently across all incident patches."""
    triangles = []
    groups = []
    for group, ring in enumerate(patches):
        for tri in triangulate(ring):
            triangles.append(tri)
            groups.append(group)
    changed = set()
    for _ in range(18):
        cuts = set()
        _, ratios = optimize_quad_centres(triangles)
        for tri, ratio in zip(triangles, ratios):
            lengths = np.linalg.norm(tri-np.roll(tri, -1, axis=0), axis=1)
            if ratio > limit:
                i = int(np.argmax(lengths))
                cuts.add(edge_key(tri[i], tri[(i+1)%3]))
        if not cuts:
            break
        output, owners = [], []
        for tri, group in zip(triangles, groups):
            marks = [edge_key(tri[i], tri[(i+1)%3]) in cuts for i in range(3)]
            pieces = split_triangle(tri, marks)
            if any(marks):
                changed.add(group)
            output.extend(pieces)
            owners.extend([group]*len(pieces))
        triangles, groups, flipped = flip_planar(output, owners, max_passes=4)
        changed.update(flipped)
    return triangles, groups, changed


def edge_key(a: Array, b: Array) -> tuple:
    return tuple(sorted((tuple(np.round(a, 8)), tuple(np.round(b, 8)))))


def split_triangle(tri: Array, marks: list[bool]) -> list[Array]:
    """Conforming one/two/three-edge bisection without introducing a centre."""
    count = sum(marks)
    if not count:
        return [tri]
    if count == 3:
        a, b, c = tri
        ab, bc, ca = (a+b)/2, (b+c)/2, (c+a)/2
        return [np.array(t) for t in [(a, ab, ca), (ab, b, bc), (ca, bc, c), (ab, bc, ca)]]
    i = marks.index(True) if count == 1 else marks.index(False)
    a, b, c = np.roll(tri, -i, axis=0)
    if count == 1:
        m = (a+b)/2
        return [np.array((a, m, c)), np.array((m, b, c))]
    bc, ca = (b+c)/2, (c+a)/2
    first = [np.array(t) for t in [(a, b, bc), (a, bc, ca), (ca, bc, c)]]
    second = [np.array(t) for t in [(a, b, ca), (b, bc, ca), (ca, bc, c)]]
    def quality(pieces: list[Array]) -> float:
        return float(min(np.linalg.norm(t-np.roll(t, -1, axis=0), axis=1).min() /
                   np.linalg.norm(t-np.roll(t, -1, axis=0), axis=1).max() for t in pieces))
    return max((first, second), key=quality)
