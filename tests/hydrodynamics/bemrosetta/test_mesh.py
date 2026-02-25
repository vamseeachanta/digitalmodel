"""Tests for BEMRosetta mesh handlers.

Tests cover:
- BaseMeshHandler: validation, quality metrics
- GDFHandler: WAMIT GDF format read/write
- DATHandler: AQWA/NEMOH DAT format read/write
- STLHandler: ASCII and binary STL format read/write
- convert_mesh: format conversion utility
"""

import struct
import tempfile
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.bemrosetta.models import PanelMesh, MeshQualityReport
from digitalmodel.hydrodynamics.bemrosetta.core.exceptions import MeshError


class TestBaseMeshHandler:
    """Tests for BaseMeshHandler base class."""

    @pytest.fixture
    def handler(self):
        """Create a BaseMeshHandler instance for testing."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh import BaseMeshHandler

        # Create concrete subclass for testing abstract base
        class TestHandler(BaseMeshHandler):
            @property
            def format_name(self) -> str:
                return "TEST"

            @property
            def supported_extensions(self) -> list[str]:
                return [".test"]

            def read(self, file_path: Path) -> PanelMesh:
                raise NotImplementedError

            def write(self, mesh: PanelMesh, file_path: Path) -> Path:
                raise NotImplementedError

        return TestHandler()

    @pytest.fixture
    def simple_quad_mesh(self):
        """Create a simple quad mesh for testing."""
        vertices = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [1.0, 1.0, 0.0],
            [0.0, 1.0, 0.0],
        ], dtype=np.float64)
        panels = np.array([[0, 1, 2, 3]], dtype=np.int32)
        return PanelMesh(vertices=vertices, panels=panels)

    @pytest.fixture
    def cube_mesh(self):
        """Create a cube mesh (5 panels, open top) for testing."""
        vertices = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [1.0, 1.0, 0.0],
            [0.0, 1.0, 0.0],
            [0.0, 0.0, -1.0],
            [1.0, 0.0, -1.0],
            [1.0, 1.0, -1.0],
            [0.0, 1.0, -1.0],
        ], dtype=np.float64)
        panels = np.array([
            [4, 5, 6, 7],  # bottom
            [0, 1, 5, 4],  # front
            [2, 3, 7, 6],  # back
            [0, 4, 7, 3],  # left
            [1, 2, 6, 5],  # right
        ], dtype=np.int32)
        return PanelMesh(vertices=vertices, panels=panels)

    @pytest.fixture
    def mesh_with_degenerate_panel(self):
        """Create mesh with a degenerate (zero area) panel."""
        vertices = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [1.0, 1.0, 0.0],
            [0.0, 1.0, 0.0],
            [0.5, 0.5, 0.0],  # Point on the plane (degenerate panel)
        ], dtype=np.float64)
        panels = np.array([
            [0, 1, 2, 3],  # Normal panel
            [4, 4, 4, 4],  # Degenerate panel (all same vertex)
        ], dtype=np.int32)
        return PanelMesh(vertices=vertices, panels=panels)

    @pytest.fixture
    def mesh_with_duplicates(self):
        """Create mesh with duplicate vertices."""
        vertices = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [1.0, 1.0, 0.0],
            [0.0, 1.0, 0.0],
            [0.0, 0.0, 0.0],  # Duplicate of vertex 0
            [1.0, 0.0, 0.0],  # Duplicate of vertex 1
        ], dtype=np.float64)
        panels = np.array([[0, 1, 2, 3]], dtype=np.int32)
        return PanelMesh(vertices=vertices, panels=panels)

    @pytest.fixture
    def mesh_with_high_aspect_ratio(self):
        """Create mesh with high aspect ratio panel."""
        vertices = np.array([
            [0.0, 0.0, 0.0],
            [100.0, 0.0, 0.0],  # Very long edge
            [100.0, 1.0, 0.0],  # Short edge
            [0.0, 1.0, 0.0],
        ], dtype=np.float64)
        panels = np.array([[0, 1, 2, 3]], dtype=np.int32)
        return PanelMesh(vertices=vertices, panels=panels)

    def test_validate_mesh_returns_quality_report(self, handler, simple_quad_mesh):
        """Test that validate_mesh returns a MeshQualityReport."""
        report = handler.validate_mesh(simple_quad_mesh)
        assert isinstance(report, MeshQualityReport)

    def test_validate_mesh_counts_panels_and_vertices(self, handler, cube_mesh):
        """Test that validate_mesh correctly counts panels and vertices."""
        report = handler.validate_mesh(cube_mesh)
        assert report.n_panels == 5
        assert report.n_vertices == 8

    def test_validate_mesh_calculates_areas(self, handler, simple_quad_mesh):
        """Test that validate_mesh calculates panel areas."""
        report = handler.validate_mesh(simple_quad_mesh)
        assert report.total_area == pytest.approx(1.0, rel=1e-6)
        assert report.min_panel_area == pytest.approx(1.0, rel=1e-6)
        assert report.max_panel_area == pytest.approx(1.0, rel=1e-6)
        assert report.mean_panel_area == pytest.approx(1.0, rel=1e-6)

    def test_validate_mesh_detects_degenerate_panels(self, handler, mesh_with_degenerate_panel):
        """Test that validate_mesh detects degenerate panels."""
        report = handler.validate_mesh(mesh_with_degenerate_panel)
        assert report.n_degenerate_panels > 0
        assert any("degenerate" in w.lower() for w in report.warnings)

    def test_validate_mesh_detects_duplicate_vertices(self, handler, mesh_with_duplicates):
        """Test that validate_mesh detects duplicate vertices."""
        report = handler.validate_mesh(mesh_with_duplicates)
        assert report.n_duplicate_vertices > 0
        assert any("duplicate" in w.lower() for w in report.warnings)

    def test_validate_mesh_calculates_aspect_ratio(self, handler, mesh_with_high_aspect_ratio):
        """Test that validate_mesh calculates max aspect ratio."""
        report = handler.validate_mesh(mesh_with_high_aspect_ratio)
        assert report.aspect_ratio_max > 10
        assert any("aspect ratio" in w.lower() for w in report.warnings)

    def test_validate_mesh_checks_normal_consistency(self, handler, simple_quad_mesh):
        """Test that validate_mesh checks normal consistency."""
        report = handler.validate_mesh(simple_quad_mesh)
        assert report.has_consistent_normals is True

    def test_validate_mesh_calculates_quality_score(self, handler, simple_quad_mesh):
        """Test that validate_mesh calculates quality score."""
        report = handler.validate_mesh(simple_quad_mesh)
        assert 0 <= report.quality_score <= 100
        # Good mesh should have high score
        assert report.quality_score > 80

    def test_validate_mesh_quality_score_penalizes_issues(
        self, handler, mesh_with_degenerate_panel
    ):
        """Test that quality score is penalized for mesh issues."""
        report = handler.validate_mesh(mesh_with_degenerate_panel)
        assert report.quality_score < 100


class TestGDFHandler:
    """Tests for WAMIT GDF format handler."""

    @pytest.fixture
    def handler(self):
        """Create a GDFHandler instance."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh import GDFHandler
        return GDFHandler()

    @pytest.fixture
    def simple_gdf_content(self):
        """Create simple GDF file content."""
        return """# Simple GDF file
1.0  9.81
0  0
1
0.0  0.0  0.0
1.0  0.0  0.0
1.0  1.0  0.0
0.0  1.0  0.0
"""

    @pytest.fixture
    def symmetric_gdf_content(self):
        """Create GDF file with symmetry."""
        return """# Symmetric GDF file
1.0  9.81
1  0
1
0.0  0.0  0.0
1.0  0.0  0.0
1.0  1.0  0.0
0.0  1.0  0.0
"""

    def test_gdf_handler_format_name(self, handler):
        """Test GDF handler format name."""
        assert handler.format_name == "GDF"

    def test_gdf_handler_supported_extensions(self, handler):
        """Test GDF handler supported extensions."""
        assert ".gdf" in handler.supported_extensions
        assert ".GDF" in handler.supported_extensions

    def test_read_gdf_file(self, handler, simple_gdf_content, tmp_path):
        """Test reading a GDF file."""
        gdf_file = tmp_path / "test.gdf"
        gdf_file.write_text(simple_gdf_content)

        mesh = handler.read(gdf_file)

        assert isinstance(mesh, PanelMesh)
        assert mesh.n_vertices == 4
        assert mesh.n_panels == 1

    def test_read_gdf_symmetric(self, handler, symmetric_gdf_content, tmp_path):
        """Test reading symmetric GDF file."""
        gdf_file = tmp_path / "symmetric.gdf"
        gdf_file.write_text(symmetric_gdf_content)

        mesh = handler.read(gdf_file)

        assert mesh.symmetry_plane is not None

    def test_read_gdf_file_not_found(self, handler, tmp_path):
        """Test MeshError raised for missing file."""
        missing_file = tmp_path / "missing.gdf"

        with pytest.raises(MeshError):
            handler.read(missing_file)

    def test_write_gdf_file(self, handler, tmp_path):
        """Test writing a GDF file."""
        vertices = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [1.0, 1.0, 0.0],
            [0.0, 1.0, 0.0],
        ], dtype=np.float64)
        panels = np.array([[0, 1, 2, 3]], dtype=np.int32)
        mesh = PanelMesh(vertices=vertices, panels=panels)

        output_file = tmp_path / "output.gdf"
        result = handler.write(mesh, output_file)

        assert result.exists()
        content = result.read_text()
        assert "GDF" in content or "gdf" in content.lower()

    def test_gdf_roundtrip(self, handler, tmp_path):
        """Test GDF write then read preserves mesh."""
        vertices = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [1.0, 1.0, 0.0],
            [0.0, 1.0, 0.0],
        ], dtype=np.float64)
        panels = np.array([[0, 1, 2, 3]], dtype=np.int32)
        original = PanelMesh(vertices=vertices, panels=panels)

        gdf_file = tmp_path / "roundtrip.gdf"
        handler.write(original, gdf_file)
        loaded = handler.read(gdf_file)

        assert loaded.n_panels == original.n_panels
        assert loaded.n_vertices == original.n_vertices
        np.testing.assert_array_almost_equal(
            np.sort(loaded.vertices, axis=0),
            np.sort(original.vertices, axis=0),
            decimal=5
        )


class TestDATHandler:
    """Tests for AQWA/NEMOH DAT format handler."""

    @pytest.fixture
    def handler(self):
        """Create a DATHandler instance."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh import DATHandler
        return DATHandler()

    @pytest.fixture
    def simple_dat_content(self):
        """Create simple DAT file content (NEMOH format)."""
        return """    4     1
    0.000000     0.000000     0.000000
    1.000000     0.000000     0.000000
    1.000000     1.000000     0.000000
    0.000000     1.000000     0.000000
    1    2    3    4
    0    0    0    0
"""

    def test_dat_handler_format_name(self, handler):
        """Test DAT handler format name."""
        assert handler.format_name == "DAT"

    def test_dat_handler_supported_extensions(self, handler):
        """Test DAT handler supported extensions."""
        assert ".dat" in handler.supported_extensions
        assert ".DAT" in handler.supported_extensions

    def test_read_dat_file(self, handler, simple_dat_content, tmp_path):
        """Test reading a DAT file."""
        dat_file = tmp_path / "test.dat"
        dat_file.write_text(simple_dat_content)

        mesh = handler.read(dat_file)

        assert isinstance(mesh, PanelMesh)
        assert mesh.n_vertices == 4
        assert mesh.n_panels == 1

    def test_read_dat_file_not_found(self, handler, tmp_path):
        """Test MeshError raised for missing file."""
        missing_file = tmp_path / "missing.dat"

        with pytest.raises(MeshError):
            handler.read(missing_file)

    def test_write_dat_file(self, handler, tmp_path):
        """Test writing a DAT file."""
        vertices = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [1.0, 1.0, 0.0],
            [0.0, 1.0, 0.0],
        ], dtype=np.float64)
        panels = np.array([[0, 1, 2, 3]], dtype=np.int32)
        mesh = PanelMesh(vertices=vertices, panels=panels)

        output_file = tmp_path / "output.dat"
        result = handler.write(mesh, output_file)

        assert result.exists()

    def test_dat_roundtrip(self, handler, tmp_path):
        """Test DAT write then read preserves mesh."""
        vertices = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [1.0, 1.0, 0.0],
            [0.0, 1.0, 0.0],
        ], dtype=np.float64)
        panels = np.array([[0, 1, 2, 3]], dtype=np.int32)
        original = PanelMesh(vertices=vertices, panels=panels)

        dat_file = tmp_path / "roundtrip.dat"
        handler.write(original, dat_file)
        loaded = handler.read(dat_file)

        assert loaded.n_panels == original.n_panels
        assert loaded.n_vertices == original.n_vertices


class TestSTLHandler:
    """Tests for STL format handler."""

    @pytest.fixture
    def handler(self):
        """Create a STLHandler instance."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh import STLHandler
        return STLHandler()

    @pytest.fixture
    def ascii_stl_content(self):
        """Create ASCII STL file content."""
        return """solid mesh
  facet normal 0.0e+00 0.0e+00 1.0e+00
    outer loop
      vertex 0.0e+00 0.0e+00 0.0e+00
      vertex 1.0e+00 0.0e+00 0.0e+00
      vertex 0.5e+00 1.0e+00 0.0e+00
    endloop
  endfacet
endsolid mesh
"""

    @pytest.fixture
    def binary_stl_file(self, tmp_path):
        """Create a binary STL file."""
        stl_file = tmp_path / "binary.stl"

        with open(stl_file, 'wb') as f:
            # Header (80 bytes) - NOT starting with 'solid'
            f.write(b'Binary STL test file'.ljust(80, b'\0'))

            # Number of triangles
            f.write(struct.pack('I', 1))

            # Triangle: normal + 3 vertices + attribute
            normal = (0.0, 0.0, 1.0)
            v1 = (0.0, 0.0, 0.0)
            v2 = (1.0, 0.0, 0.0)
            v3 = (0.5, 1.0, 0.0)

            f.write(struct.pack('fff', *normal))
            f.write(struct.pack('fff', *v1))
            f.write(struct.pack('fff', *v2))
            f.write(struct.pack('fff', *v3))
            f.write(struct.pack('H', 0))  # Attribute byte count

        return stl_file

    def test_stl_handler_format_name(self, handler):
        """Test STL handler format name."""
        assert handler.format_name == "STL"

    def test_stl_handler_supported_extensions(self, handler):
        """Test STL handler supported extensions."""
        assert ".stl" in handler.supported_extensions
        assert ".STL" in handler.supported_extensions

    def test_read_ascii_stl(self, handler, ascii_stl_content, tmp_path):
        """Test reading ASCII STL file."""
        stl_file = tmp_path / "ascii.stl"
        stl_file.write_text(ascii_stl_content)

        mesh = handler.read(stl_file)

        assert isinstance(mesh, PanelMesh)
        assert mesh.n_vertices == 3
        assert mesh.n_panels == 1

    def test_read_binary_stl(self, handler, binary_stl_file):
        """Test reading binary STL file."""
        mesh = handler.read(binary_stl_file)

        assert isinstance(mesh, PanelMesh)
        assert mesh.n_vertices == 3
        assert mesh.n_panels == 1

    def test_stl_triangles_padded_to_quads(self, handler, ascii_stl_content, tmp_path):
        """Test that STL triangles are padded to quad format with -1."""
        stl_file = tmp_path / "triangle.stl"
        stl_file.write_text(ascii_stl_content)

        mesh = handler.read(stl_file)

        # Panels should be shape (n, 4) with -1 for 4th vertex
        assert mesh.panels.shape[1] == 4
        assert mesh.panels[0, 3] == -1

    def test_read_stl_file_not_found(self, handler, tmp_path):
        """Test MeshError raised for missing file."""
        missing_file = tmp_path / "missing.stl"

        with pytest.raises(MeshError):
            handler.read(missing_file)

    def test_write_ascii_stl(self, handler, tmp_path):
        """Test writing ASCII STL file."""
        vertices = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [0.5, 1.0, 0.0],
        ], dtype=np.float64)
        panels = np.array([[0, 1, 2, -1]], dtype=np.int32)
        mesh = PanelMesh(vertices=vertices, panels=panels)

        output_file = tmp_path / "output.stl"
        result = handler.write(mesh, output_file, binary=False)

        assert result.exists()
        content = result.read_text()
        assert "solid" in content
        assert "facet" in content
        assert "vertex" in content

    def test_write_binary_stl(self, handler, tmp_path):
        """Test writing binary STL file."""
        vertices = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [0.5, 1.0, 0.0],
        ], dtype=np.float64)
        panels = np.array([[0, 1, 2, -1]], dtype=np.int32)
        mesh = PanelMesh(vertices=vertices, panels=panels)

        output_file = tmp_path / "output_binary.stl"
        result = handler.write(mesh, output_file, binary=True)

        assert result.exists()
        # Binary STL should have 84 byte header + triangle data
        file_size = result.stat().st_size
        assert file_size >= 84  # Header (80) + triangle count (4)

    def test_stl_roundtrip_ascii(self, handler, tmp_path):
        """Test ASCII STL write then read preserves mesh."""
        vertices = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [0.5, 1.0, 0.0],
        ], dtype=np.float64)
        panels = np.array([[0, 1, 2, -1]], dtype=np.int32)
        original = PanelMesh(vertices=vertices, panels=panels)

        stl_file = tmp_path / "roundtrip.stl"
        handler.write(original, stl_file, binary=False)
        loaded = handler.read(stl_file)

        assert loaded.n_panels == original.n_panels
        assert loaded.n_vertices == original.n_vertices

    def test_stl_roundtrip_binary(self, handler, tmp_path):
        """Test binary STL write then read preserves mesh."""
        vertices = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [0.5, 1.0, 0.0],
        ], dtype=np.float64)
        panels = np.array([[0, 1, 2, -1]], dtype=np.int32)
        original = PanelMesh(vertices=vertices, panels=panels)

        stl_file = tmp_path / "roundtrip_binary.stl"
        handler.write(original, stl_file, binary=True)
        loaded = handler.read(stl_file)

        assert loaded.n_panels == original.n_panels
        assert loaded.n_vertices == original.n_vertices

    def test_write_quad_mesh_as_stl_splits_to_triangles(self, handler, tmp_path):
        """Test that quad panels are split into triangles for STL."""
        vertices = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [1.0, 1.0, 0.0],
            [0.0, 1.0, 0.0],
        ], dtype=np.float64)
        panels = np.array([[0, 1, 2, 3]], dtype=np.int32)
        mesh = PanelMesh(vertices=vertices, panels=panels)

        stl_file = tmp_path / "quad_to_tri.stl"
        handler.write(mesh, stl_file, binary=False)

        content = stl_file.read_text()
        # Should have 2 facets (1 quad = 2 triangles)
        # Count "facet normal" to avoid counting "endfacet"
        assert content.count("facet normal") == 2


class TestConvertMesh:
    """Tests for mesh format conversion utility."""

    @pytest.fixture
    def sample_mesh(self):
        """Create sample mesh for conversion tests."""
        vertices = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [1.0, 1.0, 0.0],
            [0.0, 1.0, 0.0],
        ], dtype=np.float64)
        panels = np.array([[0, 1, 2, 3]], dtype=np.int32)
        return PanelMesh(vertices=vertices, panels=panels)

    @pytest.fixture
    def gdf_file(self, sample_mesh, tmp_path):
        """Create a GDF file for testing."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh import GDFHandler
        handler = GDFHandler()
        gdf_path = tmp_path / "input.gdf"
        handler.write(sample_mesh, gdf_path)
        return gdf_path

    def test_convert_gdf_to_stl(self, gdf_file, tmp_path):
        """Test converting GDF to STL."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh import convert_mesh

        output_file = tmp_path / "output.stl"
        result = convert_mesh(gdf_file, output_file)

        assert result.exists()
        assert result.suffix == ".stl"

    def test_convert_gdf_to_dat(self, gdf_file, tmp_path):
        """Test converting GDF to DAT."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh import convert_mesh

        output_file = tmp_path / "output.dat"
        result = convert_mesh(gdf_file, output_file)

        assert result.exists()
        assert result.suffix == ".dat"

    def test_convert_stl_to_gdf(self, sample_mesh, tmp_path):
        """Test converting STL to GDF."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh import STLHandler, convert_mesh

        # Create STL file first
        stl_handler = STLHandler()
        stl_path = tmp_path / "input.stl"
        stl_handler.write(sample_mesh, stl_path, binary=False)

        output_file = tmp_path / "output.gdf"
        result = convert_mesh(stl_path, output_file)

        assert result.exists()
        assert result.suffix == ".gdf"

    def test_convert_explicit_formats(self, gdf_file, tmp_path):
        """Test conversion with explicit format specification."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh import convert_mesh

        output_file = tmp_path / "output.mesh"  # Non-standard extension
        result = convert_mesh(
            gdf_file, output_file,
            input_format="gdf",
            output_format="stl"
        )

        assert result.exists()

    def test_convert_unsupported_format_raises(self, gdf_file, tmp_path):
        """Test that unsupported formats raise MeshError."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh import convert_mesh

        output_file = tmp_path / "output.xyz"

        with pytest.raises(MeshError):
            convert_mesh(gdf_file, output_file, output_format="xyz")

    def test_convert_missing_input_raises(self, tmp_path):
        """Test that missing input file raises MeshError."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh import convert_mesh

        missing_file = tmp_path / "missing.gdf"
        output_file = tmp_path / "output.stl"

        with pytest.raises(MeshError):
            convert_mesh(missing_file, output_file)
