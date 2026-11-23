"""
ABOUTME: Optional VTK/PyVista mesh decimation backend.

Wraps pyvista.PolyData.decimate() and vtkDecimatePro for mesh simplification.
Gated behind a VTK_AVAILABLE flag -- gracefully degrades when PyVista is not
installed.

Usage (only when PyVista is available):
    from digitalmodel.hydrodynamics.hull_library.decimation_vtk import (
        decimate_mesh_vtk,
    )
    result = decimate_mesh_vtk(mesh, target_reduction=0.5)

Dependencies:
    pyvista (optional): Install with `pip install pyvista`
    numpy (required)
"""

from __future__ import annotations

import numpy as np
from numpy.typing import NDArray

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    PanelMesh,
)

try:
    import pyvista as pv
    VTK_AVAILABLE = True
except ImportError:
    VTK_AVAILABLE = False


def panel_mesh_to_pyvista(mesh: PanelMesh) -> "pv.PolyData":
    """Convert a PanelMesh to a pyvista.PolyData surface mesh.

    Quad panels are passed directly as VTK cells. Triangle panels are also
    supported.

    Args:
        mesh: Source PanelMesh (quad or triangle).

    Returns:
        pyvista.PolyData with the same geometry.

    Raises:
        ImportError: If PyVista/VTK is not installed.
    """
    if not VTK_AVAILABLE:
        raise ImportError(
            "PyVista/VTK is required for this backend. "
            "Install with: pip install pyvista"
        )

    n_verts_per_face = mesh.panels.shape[1]
    # pyvista cell format: [n_verts, v0, v1, ..., vN, n_verts, ...]
    cells = []
    for panel in mesh.panels:
        cells.append(n_verts_per_face)
        cells.extend(panel.tolist())

    faces = np.array(cells, dtype=np.int32)
    return pv.PolyData(mesh.vertices, faces)


def pyvista_to_panel_mesh(
    polydata: "pv.PolyData",
    name: str,
) -> PanelMesh:
    """Convert a pyvista.PolyData back to a PanelMesh.

    Args:
        polydata: Source pyvista.PolyData object.
        name: Name for the output PanelMesh.

    Returns:
        PanelMesh equivalent of the PolyData.

    Raises:
        ImportError: If PyVista/VTK is not installed.
    """
    if not VTK_AVAILABLE:
        raise ImportError(
            "PyVista/VTK is required for this backend. "
            "Install with: pip install pyvista"
        )

    vertices = np.array(polydata.points, dtype=np.float64)
    faces = polydata.faces  # flat array [n0, v0, v1, ..., n1, v3, ...]

    panels: list[list[int]] = []
    i = 0
    while i < len(faces):
        n = int(faces[i])
        i += 1
        panel = [int(faces[i + k]) for k in range(n)]
        # Pad triangles to 4 vertices by repeating last vertex
        if n == 3:
            panel.append(panel[-1])
        panels.append(panel[:4])
        i += n

    if len(panels) == 0:
        panels_arr = np.empty((0, 4), dtype=np.int32)
    else:
        panels_arr = np.array(panels, dtype=np.int32)

    return PanelMesh(
        vertices=vertices,
        panels=panels_arr,
        name=name,
        format_origin=MeshFormat.UNKNOWN,
    )


def decimate_mesh_vtk(
    mesh: PanelMesh,
    target_reduction: float,
    method: str = "quadric_clustering",
) -> PanelMesh:
    """Decimate a mesh using PyVista/VTK.

    Args:
        mesh: Input PanelMesh to decimate.
        target_reduction: Fraction of panels to remove (0.0–1.0). A value
            of 0.5 reduces the mesh to approximately 50% of its original
            panel count.
        method: Decimation method. One of:
            - "quadric_clustering": pyvista decimate() (default).
            - "pro_decimate": vtkDecimatePro for faster but lower-quality
              decimation.

    Returns:
        Decimated PanelMesh.

    Raises:
        ImportError: If PyVista/VTK is not installed.
        ValueError: If method is not one of the supported methods.
    """
    if not VTK_AVAILABLE:
        raise ImportError(
            "PyVista/VTK is required for the 'vtk' decimation backend. "
            "Install with: pip install pyvista"
        )
    if method not in ("quadric_clustering", "pro_decimate"):
        raise ValueError(
            f"Unsupported VTK method: {method!r}. "
            "Choose 'quadric_clustering' or 'pro_decimate'."
        )

    polydata = panel_mesh_to_pyvista(mesh)
    # PyVista works with triangulated meshes for decimation
    polydata = polydata.triangulate()

    if method == "quadric_clustering":
        decimated = polydata.decimate(target_reduction)
    else:
        decimated = polydata.decimate_pro(target_reduction)

    result = pyvista_to_panel_mesh(decimated, mesh.name)
    result.format_origin = mesh.format_origin
    result.symmetry_plane = mesh.symmetry_plane
    return result


__all__ = [
    "VTK_AVAILABLE",
    "panel_mesh_to_pyvista",
    "pyvista_to_panel_mesh",
    "decimate_mesh_vtk",
]
