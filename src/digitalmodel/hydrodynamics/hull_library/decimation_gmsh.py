"""
ABOUTME: Optional GMSH surface remeshing decimation backend.

Exports the input PanelMesh to a temporary STL file, remeshes it with GMSH
at the specified target element size, and imports the result back as a
PanelMesh. The element size controls output mesh density.

Gated behind a GMSH_AVAILABLE flag -- gracefully degrades when GMSH is not
installed.

Usage (only when GMSH is available):
    from digitalmodel.hydrodynamics.hull_library.decimation_gmsh import (
        remesh_coarsen,
    )
    result = remesh_coarsen(mesh, target_element_size=0.5)

Dependencies:
    gmsh (optional): Install with `pip install gmsh`
    numpy (required)
"""

from __future__ import annotations

import struct
import tempfile
from pathlib import Path

import numpy as np
from numpy.typing import NDArray

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    PanelMesh,
)

try:
    import gmsh
    GMSH_AVAILABLE = True
except ImportError:
    GMSH_AVAILABLE = False


# ---------------------------------------------------------------------------
# STL I/O helpers (no external dependency)
# ---------------------------------------------------------------------------


def _write_binary_stl(
    path: Path,
    vertices: NDArray[np.float64],
    tris: NDArray[np.int32],
) -> None:
    """Write triangle mesh as a binary STL file.

    Args:
        path: Output file path.
        vertices: Vertex coordinates (n_verts, 3).
        tris: Triangle indices (n_tris, 3).
    """
    with open(path, "wb") as f:
        f.write(b"\x00" * 80)  # header
        f.write(struct.pack("<I", len(tris)))
        for tri in tris:
            v0 = vertices[tri[0]]
            v1 = vertices[tri[1]]
            v2 = vertices[tri[2]]
            normal = np.cross(v1 - v0, v2 - v0)
            n_len = np.linalg.norm(normal)
            if n_len > 1e-14:
                normal /= n_len
            f.write(struct.pack("<fff", *normal))
            f.write(struct.pack("<fff", *v0))
            f.write(struct.pack("<fff", *v1))
            f.write(struct.pack("<fff", *v2))
            f.write(struct.pack("<H", 0))  # attribute byte count


def _read_binary_stl(path: Path) -> PanelMesh:
    """Read a binary STL file into a PanelMesh (triangles).

    Args:
        path: STL file path.

    Returns:
        PanelMesh with triangle panels.
    """
    with open(path, "rb") as f:
        f.read(80)  # skip header
        n_tris = struct.unpack("<I", f.read(4))[0]
        vertices_list: list[list[float]] = []
        panels_list: list[list[int]] = []
        for _ in range(n_tris):
            f.read(12)  # skip normal
            tri_verts = []
            for _ in range(3):
                xyz = struct.unpack("<fff", f.read(12))
                vertices_list.append(list(xyz))
                tri_verts.append(len(vertices_list) - 1)
            panels_list.append(tri_verts)
            f.read(2)  # skip attribute byte count

    vertices = np.array(vertices_list, dtype=np.float64)
    panels = np.array(panels_list, dtype=np.int32)
    # Deduplicate vertices with tolerance
    if len(vertices) > 0:
        vertices, panels = _deduplicate_vertices(vertices, panels)
    return PanelMesh(
        vertices=vertices,
        panels=panels,
        name="gmsh_output",
        format_origin=MeshFormat.STL,
    )


def _deduplicate_vertices(
    vertices: NDArray[np.float64],
    panels: NDArray[np.int32],
    tol: float = 1e-8,
) -> tuple[NDArray[np.float64], NDArray[np.int32]]:
    """Merge vertices within tolerance distance of each other.

    Args:
        vertices: Vertex array (n_verts, 3).
        panels: Panel index array (n_panels, n_verts_per_panel).
        tol: Merge tolerance distance.

    Returns:
        Tuple of (deduplicated_vertices, remapped_panels).
    """
    # Round to tolerance grid and find unique
    scale = 1.0 / tol
    keys = np.round(vertices * scale).astype(np.int64)
    _, inverse, unique_idx = np.unique(
        keys, axis=0, return_inverse=True, return_index=True
    )
    new_vertices = vertices[unique_idx]
    new_panels = inverse[panels]
    return new_vertices, new_panels.astype(np.int32)


# ---------------------------------------------------------------------------
# Quads to triangles helper
# ---------------------------------------------------------------------------


def _quads_to_tris(panels: NDArray[np.int32]) -> NDArray[np.int32]:
    """Split quad panels into pairs of triangles."""
    if panels.shape[1] == 3:
        return panels
    tris = []
    for p in panels:
        tris.append([p[0], p[1], p[2]])
        tris.append([p[0], p[2], p[3]])
    return np.array(tris, dtype=np.int32) if tris else np.empty((0, 3), dtype=np.int32)


# ---------------------------------------------------------------------------
# Main GMSH remeshing function
# ---------------------------------------------------------------------------


def remesh_coarsen(
    mesh: PanelMesh,
    target_element_size: float,
) -> PanelMesh:
    """Remesh a surface mesh using GMSH at a coarser element size.

    The input mesh is exported to a temporary STL, loaded into GMSH,
    and regenerated with a larger characteristic element length (lc).
    The result is imported back as a PanelMesh with triangle panels.

    Args:
        mesh: Input PanelMesh to coarsen.
        target_element_size: Target characteristic element length for
            GMSH remeshing. Larger values produce coarser meshes. The
            appropriate value depends on the geometry scale.

    Returns:
        A coarsened PanelMesh with triangle panels.

    Raises:
        ImportError: If GMSH is not installed.
        RuntimeError: If GMSH meshing fails.
    """
    if not GMSH_AVAILABLE:
        raise ImportError(
            "GMSH is required for the 'gmsh' decimation backend. "
            "Install with: pip install gmsh"
        )

    # Convert quads to triangles for STL export
    tris = _quads_to_tris(mesh.panels)

    with tempfile.TemporaryDirectory() as tmpdir:
        stl_in = Path(tmpdir) / "input.stl"
        stl_out = Path(tmpdir) / "output.stl"

        _write_binary_stl(stl_in, mesh.vertices, tris)

        gmsh.initialize()
        try:
            gmsh.option.setNumber("General.Terminal", 0)
            gmsh.option.setNumber("Mesh.CharacteristicLengthMin", target_element_size)
            gmsh.option.setNumber("Mesh.CharacteristicLengthMax", target_element_size)
            gmsh.option.setNumber("Mesh.Algorithm", 6)  # Frontal-Delaunay

            gmsh.model.add("coarsen")
            gmsh.merge(str(stl_in))
            gmsh.model.mesh.classifySurfaces(
                angle=0.5, boundary=True, forReparametrization=False, curveAngle=180
            )
            gmsh.model.mesh.createGeometry()
            gmsh.model.mesh.generate(2)

            gmsh.write(str(stl_out))
        except Exception as exc:
            raise RuntimeError(f"GMSH remeshing failed: {exc}") from exc
        finally:
            gmsh.finalize()

        result = _read_binary_stl(stl_out)

    result.name = mesh.name
    result.format_origin = mesh.format_origin
    result.symmetry_plane = mesh.symmetry_plane
    return result


__all__ = [
    "GMSH_AVAILABLE",
    "remesh_coarsen",
]
