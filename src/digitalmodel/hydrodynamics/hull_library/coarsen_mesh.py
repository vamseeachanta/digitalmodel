"""
ABOUTME: Unified mesh coarsening dispatcher with QEM and vertex-clustering
backends. Backward-compatible drop-in for the original coarsen_mesh().

Strategy selection:
    "auto"               -- QEM for <=50K panels, vertex_clustering otherwise
    "qem"                -- Garland-Heckbert quadric error metrics (decimation.py)
    "vertex_clustering"  -- Original cell-centroid algorithm (this file)
    "vtk"                -- PyVista/VTK backend (decimation_vtk.py)
    "gmsh"               -- GMSH surface remeshing (decimation_gmsh.py)

Algorithm (vertex clustering):
1. Divide the bounding box into a 3D grid of clusters.
2. Assign each vertex to a grid cell.
3. For each cell, compute a representative vertex (centroid).
4. Remap each panel's vertex indices to the cell-centroid indices.
5. Remove degenerate panels (collapsed edges or duplicate vertices).
6. Rebuild a PanelMesh from the coarsened geometry.

When preserve_features is True, cells in high-curvature regions are
subdivided so that geometric detail is retained even at lower panel counts.
"""

from __future__ import annotations

import time
from dataclasses import dataclass
from typing import Optional

import numpy as np
from numpy.typing import NDArray

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    MeshQualityReport,
    PanelMesh,
)

# Threshold above which vertex clustering is preferred over QEM in "auto" mode
_QEM_PANEL_LIMIT = 50_000


@dataclass
class DecimationResult:
    """Metadata container for a decimation operation.

    Attributes:
        mesh: The decimated output PanelMesh.
        method_used: Name of the backend used (e.g., "qem", "vertex_clustering").
        input_panels: Panel count of the original mesh.
        output_panels: Panel count of the decimated mesh.
        quality_before: MeshQualityReport captured before decimation.
        quality_after: MeshQualityReport captured after decimation.
        elapsed_seconds: Wall-clock time for the decimation operation.
    """

    mesh: PanelMesh
    method_used: str
    input_panels: int
    output_panels: int
    quality_before: MeshQualityReport
    quality_after: MeshQualityReport
    elapsed_seconds: float


def _stub_quality_report(mesh: PanelMesh) -> MeshQualityReport:
    """Produce a lightweight MeshQualityReport without the full validator.

    Only populates the fields that can be derived cheaply from PanelMesh.

    Args:
        mesh: Input PanelMesh.

    Returns:
        Partially-filled MeshQualityReport.
    """
    report = MeshQualityReport()
    report.n_panels = mesh.n_panels
    report.n_vertices = mesh.n_vertices
    if mesh.panel_areas is not None and mesh.n_panels > 0:
        report.total_area = float(np.sum(mesh.panel_areas))
        report.min_panel_area = float(np.min(mesh.panel_areas))
        report.max_panel_area = float(np.max(mesh.panel_areas))
        report.mean_panel_area = float(np.mean(mesh.panel_areas))
    return report


def coarsen_mesh(
    mesh: PanelMesh,
    target_panels: int,
    preserve_features: bool = True,
    method: str = "auto",
    max_aspect_ratio: Optional[float] = None,
) -> PanelMesh:
    """Reduce panel count of an existing mesh.

    Dispatches to the selected backend algorithm. The existing two- and
    three-argument call signatures are preserved for backward compatibility.

    Args:
        mesh: Input PanelMesh to coarsen.
        target_panels: Desired number of panels in the output mesh.
        preserve_features: When True (default), enables feature preservation
            in the vertex clustering backend, and enables boundary edge
            preservation in the QEM backend.
        method: Backend to use. One of:
            - "auto": QEM if mesh has <=50K panels, vertex_clustering otherwise.
            - "qem": Garland-Heckbert quadric error metrics decimation.
            - "vertex_clustering": Original cell-centroid coarsening algorithm.
            - "vtk": PyVista/VTK (requires pyvista; raises ImportError if absent).
            - "gmsh": GMSH surface remeshing (requires gmsh; raises ImportError
              if absent).
        max_aspect_ratio: If set, the decimation stops early if the maximum
            panel aspect ratio exceeds this threshold. Currently accepted as a
            parameter for API compatibility; quality-gated stopping will be
            added in a future phase.

    Returns:
        A new PanelMesh with reduced panel count.

    Raises:
        ValueError: If method is not one of the supported values.
        ImportError: If method='vtk' or method='gmsh' and the required library
            is not installed.
    """
    if target_panels >= mesh.n_panels:
        return mesh

    _VALID_METHODS = {"auto", "qem", "vertex_clustering", "vtk", "gmsh"}
    if method not in _VALID_METHODS:
        raise ValueError(
            f"Unknown decimation method: {method!r}. "
            f"Valid options: {sorted(_VALID_METHODS)}"
        )

    if method == "auto":
        method = "qem" if mesh.n_panels <= _QEM_PANEL_LIMIT else "vertex_clustering"

    if method == "qem":
        from digitalmodel.hydrodynamics.hull_library.decimation import (
            decimate_mesh,
        )
        return decimate_mesh(
            mesh,
            target_panels=target_panels,
            preserve_boundary=preserve_features,
        )

    if method == "vertex_clustering":
        return _vertex_clustering_coarsen(
            mesh,
            target_panels=target_panels,
            preserve_features=preserve_features,
        )

    if method == "vtk":
        from digitalmodel.hydrodynamics.hull_library.decimation_vtk import (
            VTK_AVAILABLE,
            decimate_mesh_vtk,
        )
        if not VTK_AVAILABLE:
            raise ImportError(
                "PyVista/VTK is required for method='vtk'. "
                "Install with: pip install pyvista"
            )
        target_reduction = 1.0 - (target_panels / max(mesh.n_panels, 1))
        return decimate_mesh_vtk(mesh, target_reduction=target_reduction)

    # method == "gmsh"
    from digitalmodel.hydrodynamics.hull_library.decimation_gmsh import (
        GMSH_AVAILABLE,
        remesh_coarsen,
    )
    if not GMSH_AVAILABLE:
        raise ImportError(
            "GMSH is required for method='gmsh'. "
            "Install with: pip install gmsh"
        )
    # Estimate element size from target panel count and bounding box
    bb_min, bb_max = mesh.bounding_box
    bb_area = float(np.prod(bb_max - bb_min + 1e-12) ** (2.0 / 3.0))
    target_element_size = float(np.sqrt(bb_area / max(target_panels, 1)))
    return remesh_coarsen(mesh, target_element_size=target_element_size)


# ---------------------------------------------------------------------------
# Vertex clustering implementation (renamed from original coarsen_mesh)
# ---------------------------------------------------------------------------


def _vertex_clustering_coarsen(
    mesh: PanelMesh,
    target_panels: int,
    preserve_features: bool = True,
) -> PanelMesh:
    """Reduce panel count via vertex clustering (original algorithm).

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
    norm_pos = (vertices - bb_min) / (bb_size + 1e-12)
    cell_indices = np.floor(norm_pos * n_cells).astype(int)
    cell_indices = np.clip(cell_indices, 0, n_cells - 1)

    if cell_subdivision is not None:
        sub_factors = np.ones(len(vertices), dtype=int)
        for vi in range(len(vertices)):
            ci = tuple(cell_indices[vi])
            if ci in cell_subdivision:
                sub_factors[vi] = cell_subdivision[ci]

        sub_pos = norm_pos * n_cells - cell_indices
        sub_indices = np.floor(sub_pos * sub_factors[:, None]).astype(int)
        sub_indices = np.clip(sub_indices, 0, sub_factors[:, None] - 1)

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

        if mapped[0] == mapped[1] == mapped[2] == mapped[3]:
            continue

        has_collapsed_edge = False
        for ei in range(4):
            if mapped[ei] == mapped[(ei + 1) % 4]:
                has_collapsed_edge = True
                break
        if has_collapsed_edge:
            continue

        if len(set(mapped)) < 3:
            continue

        new_panels_list.append(mapped)

    if len(new_panels_list) == 0:
        new_panels_arr = np.empty((0, 4), dtype=np.int32)
    else:
        new_panels_arr = np.array(new_panels_list, dtype=np.int32)

    if len(new_panels_arr) > 0:
        sorted_per_panel = np.sort(new_panels_arr, axis=1)
        _, unique_panel_idx = np.unique(
            sorted_per_panel, axis=0, return_index=True
        )
        new_panels_arr = new_panels_arr[np.sort(unique_panel_idx)]

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
    centers = np.mean(vertices[panels], axis=1)
    norm_pos = (centers - bb_min) / (bb_size + 1e-12)
    cell_idx = np.floor(norm_pos * n_cells).astype(int)
    cell_idx = np.clip(cell_idx, 0, n_cells - 1)

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
        variance = float(np.sum(np.var(cell_normals, axis=0)))
        if variance > 0.1:
            subdivision[key] = 3
        elif variance > 0.01:
            subdivision[key] = 2

    return subdivision


__all__ = [
    "DecimationResult",
    "coarsen_mesh",
]
