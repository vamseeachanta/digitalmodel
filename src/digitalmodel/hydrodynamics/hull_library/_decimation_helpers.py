"""
ABOUTME: Internal helpers for QEM mesh decimation (decimation.py).

Contains pure geometric operations:
- Quad-to-triangle conversion
- Plane quadric computation
- Vertex quadric accumulation
- Boundary vertex detection
- Optimal contraction vertex computation
- Degenerate panel filtering
- Triangle-to-quad merging

These functions are not part of the public API. Import from decimation.py.
"""

from __future__ import annotations

import numpy as np
from numpy.typing import NDArray


def quads_to_tris(
    panels: NDArray[np.int32],
) -> NDArray[np.int32]:
    """Split quad panels into triangles.

    Each quad [v0, v1, v2, v3] becomes two triangles:
    [v0, v1, v2] and [v0, v2, v3].

    Args:
        panels: Array of shape (n_panels, 4) or (n_panels, 3).

    Returns:
        Triangle array of shape (n_tris, 3).
    """
    if panels.shape[1] == 3:
        return panels.copy()
    tris = []
    for p in panels:
        tris.append([p[0], p[1], p[2]])
        tris.append([p[0], p[2], p[3]])
    return np.array(tris, dtype=np.int32)


def compute_plane_quadric(
    v0: NDArray[np.float64],
    v1: NDArray[np.float64],
    v2: NDArray[np.float64],
) -> NDArray[np.float64]:
    """Compute the 4x4 fundamental quadric for a triangle plane.

    The quadric K_p = p * p^T where p = [a, b, c, d] is the plane
    equation ax + by + cz + d = 0.

    Args:
        v0, v1, v2: Triangle vertex coordinates.

    Returns:
        4x4 symmetric quadric matrix Q.
    """
    normal = np.cross(v1 - v0, v2 - v0)
    norm = np.linalg.norm(normal)
    if norm < 1e-14:
        return np.zeros((4, 4), dtype=np.float64)
    normal /= norm
    a, b, c = normal
    d = -float(np.dot(normal, v0))
    p = np.array([a, b, c, d], dtype=np.float64)
    return np.outer(p, p)


def compute_vertex_quadrics(
    vertices: NDArray[np.float64],
    tris: NDArray[np.int32],
) -> list[NDArray[np.float64]]:
    """Compute the summed quadric for each vertex from incident triangles.

    Args:
        vertices: Vertex coordinates (n_verts, 3).
        tris: Triangle indices (n_tris, 3).

    Returns:
        List of n_verts 4x4 quadric matrices.
    """
    n_verts = len(vertices)
    quadrics: list[NDArray[np.float64]] = [
        np.zeros((4, 4), dtype=np.float64) for _ in range(n_verts)
    ]
    for tri in tris:
        v0, v1, v2 = vertices[tri[0]], vertices[tri[1]], vertices[tri[2]]
        q = compute_plane_quadric(v0, v1, v2)
        quadrics[tri[0]] += q
        quadrics[tri[1]] += q
        quadrics[tri[2]] += q
    return quadrics


def find_boundary_vertices(
    n_verts: int,
    tris: NDArray[np.int32],
) -> set[int]:
    """Identify boundary vertices (on edges with only one adjacent triangle).

    Args:
        n_verts: Number of vertices.
        tris: Triangle indices (n_tris, 3).

    Returns:
        Set of boundary vertex indices.
    """
    edge_count: dict[tuple[int, int], int] = {}
    for tri in tris:
        for k in range(3):
            e = (int(tri[k]), int(tri[(k + 1) % 3]))
            key = (min(e), max(e))
            edge_count[key] = edge_count.get(key, 0) + 1
    boundary_verts: set[int] = set()
    for (va, vb), count in edge_count.items():
        if count == 1:
            boundary_verts.add(va)
            boundary_verts.add(vb)
    return boundary_verts


def optimal_contraction_vertex(
    q_sum: NDArray[np.float64],
    v1: NDArray[np.float64],
    v2: NDArray[np.float64],
) -> tuple[NDArray[np.float64], float]:
    """Find optimal contraction target and error cost for edge (v1, v2).

    Attempts to solve Q_bar * v = [0, 0, 0, 1]^T. Falls back to midpoint
    if the matrix is singular.

    Args:
        q_sum: Summed 4x4 quadric for the contracted pair.
        v1: First vertex coordinates.
        v2: Second vertex coordinates.

    Returns:
        Tuple of (optimal_position, error_cost).
    """
    A = q_sum.copy()
    A[3, :] = [0.0, 0.0, 0.0, 1.0]
    b = np.array([0.0, 0.0, 0.0, 1.0])
    try:
        if abs(np.linalg.det(A)) > 1e-10:
            v_opt_4 = np.linalg.solve(A, b)
            v_opt = v_opt_4[:3]
        else:
            raise np.linalg.LinAlgError("Singular")
    except np.linalg.LinAlgError:
        midpoint = 0.5 * (v1 + v2)
        candidates = [v1, v2, midpoint]
        costs = []
        for cand in candidates:
            v4 = np.append(cand, 1.0)
            costs.append(float(v4 @ q_sum @ v4))
        v_opt = candidates[int(np.argmin(costs))]

    v4 = np.append(v_opt, 1.0)
    cost = float(v4 @ q_sum @ v4)
    return v_opt, cost


def filter_degenerate_panels(
    panels: NDArray[np.int32],
    vertices: NDArray[np.float64],
    area_tol: float = 1e-12,
) -> tuple[NDArray[np.int32], NDArray[np.float64]]:
    """Remove panels whose first triangle has zero or near-zero area.

    Args:
        panels: Panel index array (n_panels, width).
        vertices: Vertex coordinate array (n_verts, 3).
        area_tol: Minimum acceptable triangle area.

    Returns:
        Tuple of (filtered_panels, vertices_unchanged).
    """
    keep = []
    for panel in panels:
        v0 = vertices[panel[0]]
        v1 = vertices[panel[1]]
        v2 = vertices[panel[2]]
        area = np.linalg.norm(np.cross(v1 - v0, v2 - v0))
        if area > area_tol:
            keep.append(panel.tolist())
    if len(keep) == 0:
        return np.empty((0, panels.shape[1]), dtype=np.int32), vertices
    return np.array(keep, dtype=np.int32), vertices


def merge_tris_to_quads(
    tris: NDArray[np.int32],
    n_verts: int,
) -> NDArray[np.int32]:
    """Greedily merge adjacent triangle pairs into quads.

    Two triangles sharing an edge [v0, v1] are merged into a quad
    [v_a, v0, v_b, v1]. Unmerged triangles become [v0, v1, v2, v2].

    Args:
        tris: Triangle index array (n_tris, 3).
        n_verts: Number of vertices (unused, kept for signature).

    Returns:
        Panel array with shape (n_panels, 4).
    """
    used = np.zeros(len(tris), dtype=bool)
    edge_to_tris: dict[tuple[int, int], list[int]] = {}
    for ti, tri in enumerate(tris):
        for k in range(3):
            e = (int(tri[k]), int(tri[(k + 1) % 3]))
            key = (min(e), max(e))
            if key not in edge_to_tris:
                edge_to_tris[key] = []
            edge_to_tris[key].append(ti)

    panels: list[list[int]] = []
    for ti, tri in enumerate(tris):
        if used[ti]:
            continue
        merged = False
        for k in range(3):
            e = (int(tri[k]), int(tri[(k + 1) % 3]))
            key = (min(e), max(e))
            candidates = [t for t in edge_to_tris.get(key, []) if t != ti]
            for tj in candidates:
                if used[tj]:
                    continue
                other = tris[tj]
                shared = {e[0], e[1]}
                opp_i = [v for v in tri if v not in shared]
                opp_j = [v for v in other if v not in shared]
                if len(opp_i) == 1 and len(opp_j) == 1:
                    panels.append([opp_i[0], e[0], opp_j[0], e[1]])
                    used[ti] = True
                    used[tj] = True
                    merged = True
                    break
            if merged:
                break
        if not merged:
            panels.append([int(tri[0]), int(tri[1]), int(tri[2]), int(tri[2])])

    if len(panels) == 0:
        return np.empty((0, 4), dtype=np.int32)
    return np.array(panels, dtype=np.int32)
