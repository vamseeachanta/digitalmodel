"""Integration tests for meshio multi-format mesh I/O library.

Issue: workspace-hub#1449
"""

import numpy as np
import pytest

import meshio


@pytest.fixture
def tmp_mesh_dir(tmp_path):
    """Provide a temporary directory for mesh file I/O."""
    return tmp_path


@pytest.fixture
def triangle_mesh():
    """Create a simple triangular mesh (two triangles forming a square)."""
    points = np.array([
        [0.0, 0.0, 0.0],
        [1.0, 0.0, 0.0],
        [1.0, 1.0, 0.0],
        [0.0, 1.0, 0.0],
    ])
    cells = [meshio.CellBlock("triangle", np.array([[0, 1, 2], [0, 2, 3]]))]
    return meshio.Mesh(points=points, cells=cells)


class TestMeshioImport:
    """Verify meshio is importable and has expected attributes."""

    def test_import_succeeds(self):
        assert hasattr(meshio, "__version__")
        assert hasattr(meshio, "read")
        assert hasattr(meshio, "Mesh")

    def test_version_is_5x(self):
        major = int(meshio.__version__.split(".")[0])
        assert major >= 5, f"Expected meshio 5.x+, got {meshio.__version__}"


class TestVTKRoundTrip:
    """Write a mesh to VTK, read it back, verify fidelity."""

    def test_write_and_read_vtk(self, triangle_mesh, tmp_mesh_dir):
        vtk_path = tmp_mesh_dir / "test.vtk"
        meshio.write(vtk_path, triangle_mesh)

        assert vtk_path.exists()
        assert vtk_path.stat().st_size > 0

        mesh_back = meshio.read(vtk_path)
        assert mesh_back.points.shape == triangle_mesh.points.shape
        assert len(mesh_back.cells) == len(triangle_mesh.cells)

    def test_vertex_count_matches(self, triangle_mesh, tmp_mesh_dir):
        vtk_path = tmp_mesh_dir / "verts.vtk"
        meshio.write(vtk_path, triangle_mesh)
        mesh_back = meshio.read(vtk_path)
        assert mesh_back.points.shape[0] == 4

    def test_cell_count_matches(self, triangle_mesh, tmp_mesh_dir):
        vtk_path = tmp_mesh_dir / "cells.vtk"
        meshio.write(vtk_path, triangle_mesh)
        mesh_back = meshio.read(vtk_path)
        total_cells = sum(len(cb.data) for cb in mesh_back.cells)
        assert total_cells == 2


class TestSTLConversion:
    """Test format conversion via STL and verify geometry preservation."""

    def test_write_and_read_stl(self, triangle_mesh, tmp_mesh_dir):
        stl_path = tmp_mesh_dir / "test.stl"
        meshio.write(stl_path, triangle_mesh)

        assert stl_path.exists()
        mesh_back = meshio.read(stl_path)

        # STL duplicates vertices per triangle, so check cell count instead
        total_cells = sum(len(cb.data) for cb in mesh_back.cells)
        assert total_cells == 2

    def test_stl_geometry_bounds_preserved(self, triangle_mesh, tmp_mesh_dir):
        stl_path = tmp_mesh_dir / "bounds.stl"
        meshio.write(stl_path, triangle_mesh)
        mesh_back = meshio.read(stl_path)

        # Bounding box should match original
        np.testing.assert_allclose(mesh_back.points.min(axis=0), [0.0, 0.0, 0.0], atol=1e-10)
        np.testing.assert_allclose(mesh_back.points.max(axis=0), [1.0, 1.0, 0.0], atol=1e-10)


class TestErrorHandling:
    """Verify meshio raises appropriate errors."""

    def test_read_nonexistent_file_raises(self, tmp_mesh_dir):
        fake_path = tmp_mesh_dir / "nonexistent.vtk"
        with pytest.raises((FileNotFoundError, meshio.ReadError, Exception)):
            meshio.read(fake_path)
