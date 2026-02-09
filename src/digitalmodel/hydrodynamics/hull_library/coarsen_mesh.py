"""
ABOUTME: Mesh coarsening via vertex clustering with optional feature-preserving
subdivision. Extracted from mesh_generator.py for modularity.

Algorithm overview:
1. Divide the bounding box into a 3D grid of clusters.
2. Assign each vertex to a grid cell.
3. For each cell, compute a representative vertex (centroid of contained
   vertices).
4. Remap each panel's vertex indices to the cell-centroid indices.
5. Remove degenerate panels (collapsed edges or duplicate vertices).
6. Rebuild a PanelMesh from the coarsened geometry.

When preserve_features is True, cells in high-curvature regions are subdivided
so that geometric detail is retained even at lower panel counts.
"""

from __future__ import annotations

import numpy as np
from numpy.typing import NDArray

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    PanelMesh,
)


def coarsen_mesh(
    mesh: PanelMesh,
    target_panels: int,
    preserve_features: bool = True,
) -> PanelMesh:
    """Reduce panel count of an existing mesh via vertex clustering.

    When preserve_features is True, uses panel normal variance to identify
    flat regions (low variance = safe to merge) vs curved regions
    (high variance = preserve detail).

    Algorithm:
        1. Divide the bounding box into a 3D grid of clusters.
        2. Assign each vertex to a grid cell.
        3. For each cell, compute a representative vertex (centroid of
           contained vertices).
        4. Remap each panel's vertex indices to the cell-centroid indices.
        5. Remove degenerate panels (collapsed edges or duplicate vertices).
        6. Rebuild a PanelMesh from the coarsened geometry.

    Args:
        mesh: Input PanelMesh to coarsen.
        target_panels: Desired number of panels in the output mesh.
        preserve_features: When True, subdivides cells in high-curvature
            regions to retain geometric detail.

    Returns:
        A new PanelMesh with reduced panel count.
    """
    if target_panels >= mesh.n_panels:
        return mesh

    vertices = mesh.vertices
    panels = mesh.panels
    normals = mesh.normals

    bb_min, bb_max = mesh.bounding_box
    bb_size = bb_max - bb_min

    # Compute grid resolution from target panel count, adjusted for
    # aspect ratio of the bounding box.
    reduction_ratio = mesh.n_panels / max(target_panels, 1)
    # Start with a cubic cell size estimate, then refine
    bb_volume = float(np.prod(bb_size + 1e-12))
    cell_size_base = (bb_volume / max(target_panels, 1)) ** (1.0 / 3.0)

    # Per-axis cell counts (at least 2 per axis for meaningful results)
    n_cells = np.maximum(
        np.ceil(bb_size / max(cell_size_base, 1e-12)).astype(int), 2
    )

    # If preserve_features, compute per-panel normal variance to identify
    # high-curvature regions and use finer cells there.
    cell_subdivision = None
    if preserve_features and normals is not None:
        cell_subdivision = _compute_feature_subdivision(
            vertices, panels, normals, bb_min, bb_size, n_cells
        )

    # Assign each vertex to a grid cell
    # Normalized position in [0, 1) per axis
    norm_pos = (vertices - bb_min) / (bb_size + 1e-12)
    cell_indices = np.floor(norm_pos * n_cells).astype(int)
    # Clamp to valid range
    cell_indices = np.clip(cell_indices, 0, n_cells - 1)

    if cell_subdivision is not None:
        # For vertices in high-curvature cells, use finer sub-indexing
        # by multiplying cell index by subdivision factor
        sub_factors = np.ones(len(vertices), dtype=int)
        for vi in range(len(vertices)):
            ci = tuple(cell_indices[vi])
            if ci in cell_subdivision:
                sub_factors[vi] = cell_subdivision[ci]

        # Encode subdivided cell as (cx*sf, cy*sf, cz*sf) + sub-offset
        sub_pos = norm_pos * n_cells - cell_indices  # fractional within cell
        sub_indices = np.floor(sub_pos * sub_factors[:, None]).astype(int)
        sub_indices = np.clip(sub_indices, 0, sub_factors[:, None] - 1)

        # Combine: use a composite key (cx, cy, cz, sx, sy, sz, sf)
        # Encode as single integer for fast hashing
        # Max sub_factor is small (2 or 3), so encoding is safe
        max_sf = max(int(np.max(sub_factors)), 1)
        max_n = max(int(np.max(n_cells)), 1)
        stride_z = 1
        stride_y = (max_n * max_sf + 1)
        stride_x = stride_y * (max_n * max_sf + 1)
        vertex_cell_keys = (
            (cell_indices[:, 0] * max_sf + sub_indices[:, 0]) * stride_x
            + (cell_indices[:, 1] * max_sf + sub_indices[:, 1]) * stride_y
            + (cell_indices[:, 2] * max_sf + sub_indices[:, 2]) * stride_z
        )
    else:
        stride_z = 1
        stride_y = int(n_cells[2]) + 1
        stride_x = stride_y * (int(n_cells[1]) + 1)
        vertex_cell_keys = (
            cell_indices[:, 0] * stride_x
            + cell_indices[:, 1] * stride_y
            + cell_indices[:, 2] * stride_z
        )

    # Map each unique cell key to a new vertex index (centroid)
    unique_keys, inverse_map = np.unique(vertex_cell_keys, return_inverse=True)
    n_new_verts = len(unique_keys)
    new_vertices = np.zeros((n_new_verts, 3), dtype=np.float64)
    counts = np.zeros(n_new_verts, dtype=np.float64)
    for vi in range(len(vertices)):
        new_idx = inverse_map[vi]
        new_vertices[new_idx] += vertices[vi]
        counts[new_idx] += 1.0
    new_vertices /= counts[:, None]

    # Remap panels
    new_panels_list = []
    for pi in range(len(panels)):
        panel = panels[pi]
        mapped = [int(inverse_map[v]) for v in panel]

        # Skip degenerate: all four vertices collapsed to same cell
        if mapped[0] == mapped[1] == mapped[2] == mapped[3]:
            continue

        # Skip degenerate: any adjacent pair of vertices is the same
        # (creates zero-length edge)
        has_collapsed_edge = False
        for ei in range(4):
            if mapped[ei] == mapped[(ei + 1) % 4]:
                has_collapsed_edge = True
                break
        if has_collapsed_edge:
            continue

        # Skip degenerate: only 2 unique vertices
        if len(set(mapped)) < 3:
            continue

        new_panels_list.append(mapped)

    if len(new_panels_list) == 0:
        # Edge case: everything collapsed. Return a minimal mesh.
        new_panels_arr = np.empty((0, 4), dtype=np.int32)
    else:
        new_panels_arr = np.array(new_panels_list, dtype=np.int32)

    # Deduplicate panels (same set of vertex indices)
    if len(new_panels_arr) > 0:
        sorted_per_panel = np.sort(new_panels_arr, axis=1)
        _, unique_panel_idx = np.unique(
            sorted_per_panel, axis=0, return_index=True
        )
        new_panels_arr = new_panels_arr[np.sort(unique_panel_idx)]

    # Remove unreferenced vertices
    if len(new_panels_arr) > 0:
        used_verts = np.unique(new_panels_arr)
        vert_remap = np.full(n_new_verts, -1, dtype=np.int32)
        vert_remap[used_verts] = np.arange(len(used_verts), dtype=np.int32)
        new_vertices = new_vertices[used_verts]
        new_panels_arr = vert_remap[new_panels_arr]

    return PanelMesh(
        vertices=new_vertices.astype(np.float64),
        panels=new_panels_arr.astype(np.int32),
        name=mesh.name,
        format_origin=mesh.format_origin,
        symmetry_plane=mesh.symmetry_plane,
    )


def _compute_feature_subdivision(
    vertices: NDArray[np.float64],
    panels: NDArray[np.int32],
    normals: NDArray[np.float64],
    bb_min: NDArray[np.float64],
    bb_size: NDArray[np.float64],
    n_cells: NDArray[np.int64],
) -> dict[tuple[int, int, int], int]:
    """Compute per-cell subdivision factors based on normal variance.

    Cells with high normal variance (curved regions) get a subdivision
    factor > 1, preserving more detail during coarsening.

    Returns:
        Dict mapping cell index (cx, cy, cz) to subdivision factor (1, 2, or 3).
    """
    # Assign each panel center to a cell
    centers = np.mean(
        vertices[panels], axis=1
    )  # shape (n_panels, 3)
    norm_pos = (centers - bb_min) / (bb_size + 1e-12)
    cell_idx = np.floor(norm_pos * n_cells).astype(int)
    cell_idx = np.clip(cell_idx, 0, n_cells - 1)

    # Group normals by cell and compute variance
    cell_normal_groups: dict[tuple[int, int, int], list[int]] = {}
    for pi in range(len(panels)):
        key = (int(cell_idx[pi, 0]), int(cell_idx[pi, 1]), int(cell_idx[pi, 2]))
        if key not in cell_normal_groups:
            cell_normal_groups[key] = []
        cell_normal_groups[key].append(pi)

    subdivision: dict[tuple[int, int, int], int] = {}
    for key, panel_indices in cell_normal_groups.items():
        if len(panel_indices) < 2:
            continue
        cell_normals = normals[panel_indices]
        # Variance of normal components as a proxy for curvature
        variance = float(np.sum(np.var(cell_normals, axis=0)))
        if variance > 0.1:
            subdivision[key] = 3
        elif variance > 0.01:
            subdivision[key] = 2

    return subdivision


__all__ = [
    "coarsen_mesh",
]
