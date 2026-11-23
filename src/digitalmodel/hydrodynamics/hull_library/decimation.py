"""
ABOUTME: Quadric Error Metrics (QEM) mesh decimation -- pure NumPy.

Implements the Garland-Heckbert (1997) QEM algorithm for mesh simplification:
1. Compute a 4x4 quadric error matrix Q for each vertex from incident panels.
2. For each edge, compute the optimal contraction position and error cost.
3. Use a min-heap to iteratively collapse the lowest-cost edge.
4. After each collapse, update affected quadrics and edge costs.
5. Stop when panel count reaches the target.

Internal helpers: _decimation_helpers.py
Public API: decimate_mesh()

References:
    Garland, M. and Heckbert, P.S. (1997) "Surface Simplification Using
    Quadric Error Metrics". SIGGRAPH '97.

Dependencies:
    numpy (required), heapq (stdlib)
"""

from __future__ import annotations

import heapq
from typing import Optional

import numpy as np
from numpy.typing import NDArray

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    PanelMesh,
)
from digitalmodel.hydrodynamics.hull_library._decimation_helpers import (
    compute_vertex_quadrics,
    filter_degenerate_panels,
    find_boundary_vertices,
    merge_tris_to_quads,
    optimal_contraction_vertex,
    quads_to_tris,
)

# Penalty cost assigned to boundary edges so they are collapsed last.
_BOUNDARY_COST_PENALTY = 1.0e8


def decimate_mesh(
    mesh: PanelMesh,
    target_panels: int,
    preserve_boundary: bool = True,
) -> PanelMesh:
    """Decimate a panel mesh to approximately target_panels panels.

    Uses the Garland-Heckbert Quadric Error Metrics (QEM) algorithm:
    1. Convert quads to triangles internally.
    2. Compute per-vertex quadric error matrices.
    3. Build a min-heap of edge contraction candidates sorted by error cost.
    4. Iteratively collapse the cheapest edge; update adjacency and quadrics.
    5. Merge triangle pairs back to quads where possible.
    6. Stop when panel count reaches target_panels.

    Args:
        mesh: Input PanelMesh to decimate. May be quad or triangle mesh.
        target_panels: Desired number of panels in the output mesh.
        preserve_boundary: When True, assign high cost to boundary edges
            so they are collapsed last (recommended for open meshes).

    Returns:
        A new PanelMesh with reduced panel count. Metadata (name,
        format_origin, symmetry_plane) is copied from the input.

    Notes:
        - If target_panels >= mesh.n_panels, returns the input mesh unchanged.
        - Output panel count may be slightly above target when topology
          constraints prevent further reduction.
        - No external dependencies beyond numpy.
    """
    if target_panels >= mesh.n_panels:
        return mesh

    is_quad = mesh.panels.shape[1] == 4
    tris = quads_to_tris(mesh.panels)
    target_tris = target_panels * 2 if is_quad else target_panels

    vertices = mesh.vertices.copy()
    n_verts = len(vertices)

    quadrics = compute_vertex_quadrics(vertices, tris)

    boundary_verts: set[int] = set()
    if preserve_boundary:
        boundary_verts = find_boundary_vertices(n_verts, tris)

    live_tris: list[Optional[tuple[int, int, int]]] = [
        (int(t[0]), int(t[1]), int(t[2])) for t in tris
    ]
    n_live = len(live_tris)

    vertex_to_tris: list[set[int]] = [set() for _ in range(n_verts)]
    for ti, tri in enumerate(live_tris):
        if tri is not None:
            vertex_to_tris[tri[0]].add(ti)
            vertex_to_tris[tri[1]].add(ti)
            vertex_to_tris[tri[2]].add(ti)

    parent = list(range(n_verts))

    def find(v: int) -> int:
        """Path-compressed union-find root."""
        while parent[v] != v:
            parent[v] = parent[parent[v]]
            v = parent[v]
        return v

    edge_version: list[int] = [0] * n_verts
    heap: list[tuple] = []

    def push_edge(va: int, vb: int) -> None:
        """Compute edge cost and push onto heap."""
        q_sum = quadrics[va] + quadrics[vb]
        v_opt, cost = optimal_contraction_vertex(q_sum, vertices[va], vertices[vb])
        if preserve_boundary and (va in boundary_verts or vb in boundary_verts):
            cost += _BOUNDARY_COST_PENALTY
        heapq.heappush(
            heap,
            (cost, edge_version[va], edge_version[vb], va, vb,
             v_opt[0], v_opt[1], v_opt[2]),
        )

    seen_edges: set[tuple[int, int]] = set()
    for tri in live_tris:
        if tri is None:
            continue
        for k in range(3):
            va, vb = find(tri[k]), find(tri[(k + 1) % 3])
            key = (min(va, vb), max(va, vb))
            if key not in seen_edges:
                seen_edges.add(key)
                push_edge(va, vb)

    while n_live > target_tris and heap:
        cost, ver_a, ver_b, va, vb, ox, oy, oz = heapq.heappop(heap)

        va_c, vb_c = find(va), find(vb)

        if edge_version[va_c] != ver_a or edge_version[vb_c] != ver_b:
            continue
        if va_c == vb_c:
            continue

        shared_tris = vertex_to_tris[va_c] & vertex_to_tris[vb_c]
        if len(shared_tris) == 0:
            continue

        # Link condition: prevent non-manifold geometry
        neighbours_a = {
            find(v)
            for ti in vertex_to_tris[va_c]
            if live_tris[ti] is not None
            for v in live_tris[ti]
            if find(v) != va_c
        }
        neighbours_b = {
            find(v)
            for ti in vertex_to_tris[vb_c]
            if live_tris[ti] is not None
            for v in live_tris[ti]
            if find(v) != vb_c
        }
        shared_in_edge_tris = {
            find(v)
            for ti in shared_tris
            if live_tris[ti] is not None
            for v in live_tris[ti]
            if find(v) != va_c and find(v) != vb_c
        }
        if (neighbours_a & neighbours_b) - shared_in_edge_tris:
            continue

        v_opt = np.array([ox, oy, oz], dtype=np.float64)
        vertices[va_c] = v_opt
        parent[vb_c] = va_c
        edge_version[va_c] += 1
        edge_version[vb_c] += 1
        quadrics[va_c] = quadrics[va_c] + quadrics[vb_c]

        if vb_c in boundary_verts:
            boundary_verts.discard(vb_c)
            boundary_verts.add(va_c)

        for ti in shared_tris:
            live_tris[ti] = None
            n_live -= 1

        for ti in vertex_to_tris[vb_c]:
            if live_tris[ti] is None:
                continue
            old_tri = live_tris[ti]
            new_tri = tuple(va_c if find(v) == va_c else find(v) for v in old_tri)
            if len(set(new_tri)) < 3:
                live_tris[ti] = None
                n_live -= 1
                for v in old_tri:
                    vertex_to_tris[find(v)].discard(ti)
                continue
            live_tris[ti] = new_tri  # type: ignore[assignment]
            for v in new_tri:
                vertex_to_tris[v].add(ti)
            vertex_to_tris[va_c].add(ti)

        vertex_to_tris[va_c] = vertex_to_tris[va_c] | vertex_to_tris[vb_c]
        vertex_to_tris[va_c] -= {
            ti for ti in vertex_to_tris[va_c] if live_tris[ti] is None
        }

        new_neighbours: set[tuple[int, int]] = set()
        for ti in vertex_to_tris[va_c]:
            if live_tris[ti] is None:
                continue
            for v in live_tris[ti]:
                vc = find(v)
                if vc != va_c:
                    new_neighbours.add((min(va_c, vc), max(va_c, vc)))
        for key in new_neighbours:
            push_edge(key[0], key[1])

    final_tris = [t for t in live_tris if t is not None]

    if len(final_tris) == 0:
        return _empty_stub(mesh)

    tri_arr = np.vectorize(find)(np.array(final_tris, dtype=np.int32))
    used_verts = np.unique(tri_arr)
    remap = np.full(n_verts, -1, dtype=np.int32)
    remap[used_verts] = np.arange(len(used_verts), dtype=np.int32)
    new_vertices = vertices[used_verts]
    tri_arr = remap[tri_arr]

    out_panels = merge_tris_to_quads(tri_arr, len(new_vertices)) if is_quad else tri_arr
    out_panels, new_vertices = filter_degenerate_panels(out_panels, new_vertices)

    if len(out_panels) == 0:
        return _empty_stub(mesh)

    return PanelMesh(
        vertices=new_vertices.astype(np.float64),
        panels=out_panels.astype(np.int32),
        name=mesh.name,
        format_origin=mesh.format_origin,
        symmetry_plane=mesh.symmetry_plane,
    )


def _empty_stub(mesh: PanelMesh) -> PanelMesh:
    """Return a minimal valid mesh when decimation over-collapses."""
    return PanelMesh(
        vertices=np.zeros((3, 3), dtype=np.float64),
        panels=np.array([[0, 1, 2, 2]], dtype=np.int32),
        name=mesh.name,
        format_origin=mesh.format_origin,
        symmetry_plane=mesh.symmetry_plane,
    )


__all__ = [
    "decimate_mesh",
]
