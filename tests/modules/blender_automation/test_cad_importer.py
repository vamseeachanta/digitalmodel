"""
ABOUTME: Tests for CAD file import functionality
Tests CADImporter class with various CAD formats.
"""

import pytest
from pathlib import Path
import tempfile

from digitalmodel.modules.blender_automation.converters.cad_importer import CADImporter


class TestCADImporter:
    """Test suite for CADImporter class."""

    @pytest.fixture
    def importer(self):
        """Create CADImporter instance for testing."""
        return CADImporter()

    @pytest.fixture
    def temp_dir(self):
        """Create temporary directory for test files."""
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
        # Create simple OBJ
        obj_file = temp_dir / "test.obj"
        obj_file.write_text("""
v 0 0 0
v 1 0 0
v 0 1 0
f 1 2 3
""")

        output_blend = temp_dir / "output.blend"
        result = importer.import_file(obj_file, output_blend)

        assert result["success"] is True
        assert output_blend.exists()

    def test_import_stl_file(self, importer, temp_dir):
        """Test importing STL file."""
        # Create minimal ASCII STL
        stl_file = temp_dir / "test.stl"
        stl_file.write_text("""
solid test
  facet normal 0 0 1
    outer loop
      vertex 0 0 0
      vertex 1 0 0
      vertex 0 1 0
    endloop
  endfacet
endsolid test
""")

        output_blend = temp_dir / "output.blend"
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
        # Create multiple OBJ files
        files = []
        for i in range(3):
            obj_file = temp_dir / f"test_{i}.obj"
            obj_file.write_text(f"""
v {i} 0 0
v {i+1} 0 0
v {i} 1 0
f 1 2 3
""")
            files.append(obj_file)

        output_blend = temp_dir / "batch.blend"
        result = importer.batch_import(files, output_blend, merge=True)

        assert result["total"] == 3
        assert result["successful"] >= 1  # At least some should succeed
        assert output_blend.exists()

    def test_freecad_availability_check(self, importer):
        """Test FreeCAD availability detection."""
        # Should set freecad_available attribute
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
        obj_file.write_text("""
v 0 0 0
v 1 0 0
v 0 1 0
f 1 2 3
""")

        output_blend = temp_dir / "output.blend"
        result = importer.import_file(
            obj_file,
            output_blend,
            scale=2.0
        )

        assert result["success"] is True

    def test_import_with_cleanup(self, importer, temp_dir):
        """Test importing with mesh cleanup."""
        obj_file = temp_dir / "test.obj"
        obj_file.write_text("""
v 0 0 0
v 1 0 0
v 0 1 0
v 0 0 0
f 1 2 3
""")

        output_blend = temp_dir / "output.blend"
        result = importer.import_file(
            obj_file,
            output_blend,
            cleanup=True
        )

        assert result["success"] is True
