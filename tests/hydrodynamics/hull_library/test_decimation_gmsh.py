"""Tests for optional GMSH remeshing decimation backend -- Phase 3 of WRK-101.

All tests are skipped when GMSH is not installed.
"""

from __future__ import annotations

import math

import numpy as np
import pytest

try:
    import gmsh  # noqa: F401
    GMSH_AVAILABLE = True
except ImportError:
    GMSH_AVAILABLE = False

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    PanelMesh,
)


def make_sphere_mesh(n_lat: int = 10, n_lon: int = 10) -> PanelMesh:
    """Create a UV-sphere mesh for GMSH backend tests."""
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
        name="gmsh_test_sphere",
        format_origin=MeshFormat.GDF,
    )


@pytest.mark.skipif(not GMSH_AVAILABLE, reason="GMSH not installed")
class TestDecimationGMSH:
    """GMSH backend remeshing tests (only run when GMSH is available)."""

    def test_remesh_coarsen_import(self):
        """remesh_coarsen is importable when GMSH is available."""
        from digitalmodel.hydrodynamics.hull_library.decimation_gmsh import (  # noqa: F401
            remesh_coarsen,
        )

    def test_remesh_coarsen_returns_panel_mesh(self):
        """remesh_coarsen returns a PanelMesh."""
        from digitalmodel.hydrodynamics.hull_library.decimation_gmsh import (
            remesh_coarsen,
        )

        mesh = make_sphere_mesh()
        # Use a large element size to get fewer panels
        result = remesh_coarsen(mesh, target_element_size=0.5)
        assert isinstance(result, PanelMesh)
        assert result.n_panels > 0

    def test_remesh_coarsen_valid_panels(self):
        """GMSH-remeshed mesh has valid panel indices."""
        from digitalmodel.hydrodynamics.hull_library.decimation_gmsh import (
            remesh_coarsen,
        )

        mesh = make_sphere_mesh()
        result = remesh_coarsen(mesh, target_element_size=0.4)
        assert result.panels.min() >= 0
        assert result.panels.max() < result.n_vertices
