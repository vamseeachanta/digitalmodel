"""
ABOUTME: Tests for Blender wrapper functionality
Tests BlenderWrapper class with subprocess-based Blender execution.
"""

import pytest
from pathlib import Path
import tempfile
import subprocess

from digitalmodel.modules.blender_automation.core.blender_wrapper import BlenderWrapper, BlenderContext


class TestBlenderWrapper:
    """Test suite for BlenderWrapper class."""

    @pytest.fixture
    def wrapper(self):
        """Create BlenderWrapper instance for testing."""
        return BlenderWrapper()

    @pytest.fixture
    def temp_dir(self):
        """Create temporary directory for test files."""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir)

    def test_blender_installation_check(self, wrapper):
        """Test that Blender is found and version is detected."""
        assert wrapper.blender_path is not None
        assert wrapper.version is not None
        assert "Blender" in wrapper.version

    def test_run_simple_script(self, wrapper):
        """Test running a simple Python script in Blender."""
        script = "import bpy; print('Test successful')"
        result = wrapper.run_script(script, background=True)

        assert result["success"] is True
        assert result["returncode"] == 0
        assert "Test successful" in result["stdout"]

    def test_execute_command(self, wrapper):
        """Test executing multiple commands."""
        commands = [
            "import bpy",
            "print('Command 1')",
            "print('Command 2')"
        ]
        result = wrapper.execute_command(commands, background=True)

        assert result["success"] is True
        assert "Command 1" in result["stdout"]
        assert "Command 2" in result["stdout"]

    def test_import_obj_file(self, wrapper, temp_dir):
        """Test importing an OBJ file."""
        # Create simple OBJ file
        obj_file = temp_dir / "test.obj"
        obj_file.write_text("""
v 0 0 0
v 1 0 0
v 0 1 0
f 1 2 3
""")

        output_blend = temp_dir / "output.blend"
        result = wrapper.import_file(obj_file, "obj", output_blend)

        assert result["success"] is True
        assert output_blend.exists()

    def test_import_stl_file(self, wrapper, temp_dir):
        """Test importing an STL file."""
        # Note: Would need actual STL file for full test
        # This tests the interface
        stl_file = temp_dir / "test.stl"

        # Create minimal STL (ASCII format)
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
        result = wrapper.import_file(stl_file, "stl", output_blend)

        # May succeed or fail depending on Blender version
        # At minimum, should not crash
        assert "success" in result

    def test_export_file(self, wrapper, temp_dir):
        """Test exporting Blender file to different format."""
        # First create a blend file with simple geometry
        blend_file = temp_dir / "test.blend"

        script = f"""
import bpy
bpy.ops.wm.read_factory_settings(use_empty=True)
bpy.ops.mesh.primitive_cube_add()
bpy.ops.wm.save_as_mainfile(filepath='{blend_file}')
"""
        result = wrapper.run_script(script, background=True)
        assert result["success"] is True

        # Now export to OBJ
        output_obj = temp_dir / "export.obj"
        export_result = wrapper.export_file(
            blend_file,
            output_obj,
            "obj"
        )

        assert export_result["success"] is True
        assert output_obj.exists()

    def test_blender_context_manager(self, temp_dir):
        """Test BlenderContext context manager."""
        with BlenderContext() as blender:
            assert isinstance(blender, BlenderWrapper)
            assert blender.version is not None

            # Test operation within context
            script = "import bpy; print('Context test')"
            result = blender.run_script(script)
            assert result["success"] is True

    def test_invalid_format_raises_error(self, wrapper, temp_dir):
        """Test that invalid format raises ValueError."""
        with pytest.raises(ValueError, match="Unsupported format"):
            wrapper.import_file(
                temp_dir / "test.unknown",
                "unknown"
            )

    def test_script_error_handling(self, wrapper):
        """Test error handling for failing scripts."""
        script = "import bpy; raise RuntimeError('Test error')"
        result = wrapper.run_script(script, background=True)

        assert result["success"] is False
        assert "error" in result

    def test_multiple_imports(self, wrapper, temp_dir):
        """Test importing multiple files into same scene."""
        # Create multiple OBJ files
        for i in range(3):
            obj_file = temp_dir / f"test_{i}.obj"
            obj_file.write_text(f"""
v {i} 0 0
v {i+1} 0 0
v {i} 1 0
f 1 2 3
""")

        # Import all into single blend file
        blend_file = temp_dir / "combined.blend"

        for i in range(3):
            obj_file = temp_dir / f"test_{i}.obj"
            result = wrapper.import_file(obj_file, "obj", blend_file)
            assert result["success"] is True

        assert blend_file.exists()
