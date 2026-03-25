"""Tests for optional VTK/PyVista decimation backend -- Phase 2 of WRK-101.

All tests are skipped when PyVista/VTK is not installed.
"""

from __future__ import annotations

import math

import numpy as np
import pytest

try:
    import pyvista  # noqa: F401
    VTK_AVAILABLE = True
except ImportError:
    VTK_AVAILABLE = False

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    PanelMesh,
)


def make_sphere_mesh(n_lat: int = 15, n_lon: int = 15) -> PanelMesh:
    """Create a UV-sphere mesh."""
    lats = np.linspace(math.pi, 0.0, n_lat + 1)
    lons = np.linspace(0.0, 2 * math.pi, n_lon + 1)
    vertices = []
    for lat in lats:
        for lon in lons:
            vertices.append([
                math.sin(lat) * math.cos(lon),
                math.sin(lat) * math.sin(lon),
                math.cos(lat),
            ])
    stride = n_lon + 1
    panels = []
    for i in range(n_lat):
        for j in range(n_lon):
            panels.append([
                i * stride + j,
                i * stride + (j + 1),
                (i + 1) * stride + (j + 1),
                (i + 1) * stride + j,
            ])
    return PanelMesh(
        vertices=np.array(vertices, dtype=np.float64),
        panels=np.array(panels, dtype=np.int32),
        name="vtk_test_sphere",
        format_origin=MeshFormat.GDF,
    )


@pytest.mark.skipif(not VTK_AVAILABLE, reason="PyVista/VTK not installed")
class TestDecimationVTK:
    """VTK backend decimation tests (only run when PyVista is available)."""

    def test_decimate_mesh_vtk_import(self):
        """decimate_mesh_vtk is importable when VTK is available."""
        from digitalmodel.hydrodynamics.hull_library.decimation_vtk import (  # noqa: F401
            decimate_mesh_vtk,
        )

    def test_panel_mesh_to_pyvista_conversion(self):
        """panel_mesh_to_pyvista returns a pyvista.PolyData."""
        from digitalmodel.hydrodynamics.hull_library.decimation_vtk import (
            panel_mesh_to_pyvista,
        )

        mesh = make_sphere_mesh()
        polydata = panel_mesh_to_pyvista(mesh)
        assert polydata.n_points == mesh.n_vertices

    def test_pyvista_to_panel_mesh_roundtrip(self):
        """pyvista_to_panel_mesh converts back to a PanelMesh."""
        from digitalmodel.hydrodynamics.hull_library.decimation_vtk import (
            panel_mesh_to_pyvista,
            pyvista_to_panel_mesh,
        )

        mesh = make_sphere_mesh()
        polydata = panel_mesh_to_pyvista(mesh)
        result = pyvista_to_panel_mesh(polydata, "roundtrip")
        assert isinstance(result, PanelMesh)
        assert result.n_panels > 0

    def test_decimate_mesh_vtk_reduces_panels(self):
        """decimate_mesh_vtk reduces panel count by target_reduction."""
        from digitalmodel.hydrodynamics.hull_library.decimation_vtk import (
            decimate_mesh_vtk,
        )

        mesh = make_sphere_mesh(n_lat=20, n_lon=20)
        result = decimate_mesh_vtk(mesh, target_reduction=0.5)
        assert isinstance(result, PanelMesh)
        assert result.n_panels < mesh.n_panels

    def test_decimate_mesh_vtk_valid_panels(self):
        """VTK-decimated mesh has valid panel indices."""
        from digitalmodel.hydrodynamics.hull_library.decimation_vtk import (
            decimate_mesh_vtk,
        )

        mesh = make_sphere_mesh(n_lat=15, n_lon=15)
        result = decimate_mesh_vtk(mesh, target_reduction=0.4)
        assert result.panels.min() >= 0
        assert result.panels.max() < result.n_vertices
