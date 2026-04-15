"""
ABOUTME: Tests for CAD file import functionality
Tests CADImporter class with various CAD formats.
Designed to pass whether or not Blender is installed on the host machine.
"""

import tempfile
from pathlib import Path
from unittest.mock import patch

import pytest

from digitalmodel.blender_automation.converters.cad_importer import CADImporter

from .conftest import mock_blender_run, requires_blender


class TestCADImporter:
    """Test suite for CADImporter class (mocked Blender)."""

    @pytest.fixture
    def importer(self):
        """Create CADImporter with mocked Blender subprocess."""
        with patch("subprocess.run", side_effect=mock_blender_run):
            return CADImporter()

    @pytest.fixture
    def temp_dir(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir)

    def test_supported_formats(self, importer):
        """Test that supported formats are defined."""
        assert "stl" in importer.SUPPORTED_FORMATS
        assert "obj" in importer.SUPPORTED_FORMATS
        assert "fbx" in importer.SUPPORTED_FORMATS
        assert "step" in importer.SUPPORTED_FORMATS

    def test_import_obj_file(self, importer, temp_dir):
        """Test importing OBJ file."""
        obj_file = temp_dir / "test.obj"
        obj_file.write_text("v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n")
        output_blend = temp_dir / "output.blend"

        with patch("subprocess.run", side_effect=mock_blender_run):
            result = importer.import_file(obj_file, output_blend)
        assert result["success"] is True

    def test_import_stl_file(self, importer, temp_dir):
        """Test importing STL file."""
        stl_file = temp_dir / "test.stl"
        stl_file.write_text(
            "solid test\n"
            "  facet normal 0 0 1\n"
            "    outer loop\n"
            "      vertex 0 0 0\n"
            "      vertex 1 0 0\n"
            "      vertex 0 1 0\n"
            "    endloop\n"
            "  endfacet\n"
            "endsolid test\n"
        )
        output_blend = temp_dir / "output.blend"

        with patch("subprocess.run", side_effect=mock_blender_run):
            result = importer.import_file(stl_file, output_blend)
        assert "success" in result

    def test_unsupported_format_raises_error(self, importer, temp_dir):
        """Test that unsupported format raises ValueError."""
        bad_file = temp_dir / "test.unknown"
        bad_file.write_text("dummy")

        with pytest.raises(ValueError, match="Unsupported format"):
            importer.import_file(bad_file)

    def test_nonexistent_file_raises_error(self, importer, temp_dir):
        """Test that nonexistent file raises FileNotFoundError."""
        missing_file = temp_dir / "missing.obj"

        with pytest.raises(FileNotFoundError):
            importer.import_file(missing_file)

    def test_batch_import(self, importer, temp_dir):
        """Test batch importing multiple files."""
        files = []
        for i in range(3):
            obj_file = temp_dir / f"test_{i}.obj"
            obj_file.write_text(
                f"v {i} 0 0\nv {i+1} 0 0\nv {i} 1 0\nf 1 2 3\n"
            )
            files.append(obj_file)

        output_blend = temp_dir / "batch.blend"
        with patch("subprocess.run", side_effect=mock_blender_run):
            result = importer.batch_import(files, output_blend, merge=True)

        assert result["total"] == 3
        assert result["successful"] >= 1

    def test_freecad_availability_check(self, importer):
        """Test FreeCAD availability detection."""
        assert hasattr(importer, "freecad_available")
        assert isinstance(importer.freecad_available, bool)

    def test_step_import_without_freecad(self, importer, temp_dir):
        """Test STEP import behavior when FreeCAD not available."""
        if importer.freecad_available:
            pytest.skip("FreeCAD is available, cannot test without it")

        step_file = temp_dir / "test.step"
        step_file.write_text("dummy STEP content")

        with pytest.raises(RuntimeError, match="requires FreeCAD"):
            importer.import_file(step_file)

    def test_import_with_scale(self, importer, temp_dir):
        """Test importing with scale parameter."""
        obj_file = temp_dir / "test.obj"
        obj_file.write_text("v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n")
        output_blend = temp_dir / "output.blend"

        with patch("subprocess.run", side_effect=mock_blender_run):
            result = importer.import_file(obj_file, output_blend, scale=2.0)
        assert result["success"] is True

    def test_import_with_cleanup(self, importer, temp_dir):
        """Test importing with mesh cleanup."""
        obj_file = temp_dir / "test.obj"
        obj_file.write_text("v 0 0 0\nv 1 0 0\nv 0 1 0\nv 0 0 0\nf 1 2 3\n")
        output_blend = temp_dir / "output.blend"

        with patch("subprocess.run", side_effect=mock_blender_run):
            result = importer.import_file(obj_file, output_blend, cleanup=True)
        assert result["success"] is True
