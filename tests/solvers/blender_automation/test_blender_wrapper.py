"""
ABOUTME: Tests for Blender wrapper functionality
Tests BlenderWrapper class with subprocess-based Blender execution.
Designed to pass whether or not Blender is installed on the host machine.
"""

import subprocess
import tempfile
from pathlib import Path
from unittest.mock import patch

import pytest

from digitalmodel.blender_automation.core.blender_wrapper import (
    BlenderWrapper,
    BlenderContext,
)

from .conftest import mock_blender_run, mock_blender_run_error, requires_blender


# ---------------------------------------------------------------------------
# Test: Availability / construction
# ---------------------------------------------------------------------------

class TestBlenderAvailability:
    """Tests that BlenderWrapper can be constructed regardless of Blender presence."""

    def test_wrapper_constructs_without_blender(self):
        """BlenderWrapper() must not raise even when Blender is absent."""
        with patch("shutil.which", return_value=None):
            with patch("subprocess.run", side_effect=FileNotFoundError("blender")):
                wrapper = BlenderWrapper()
                assert wrapper.is_available is False
                assert wrapper.version is None

    def test_wrapper_detects_blender_when_present(self):
        """BlenderWrapper detects Blender and reports version when it is installed."""
        with patch("subprocess.run", side_effect=mock_blender_run):
            wrapper = BlenderWrapper()
            assert wrapper.is_available is True
            assert "Blender" in wrapper.version

    def test_wrapper_with_explicit_path(self):
        """BlenderWrapper accepts an explicit blender_path and validates it."""
        with patch("subprocess.run", side_effect=mock_blender_run):
            wrapper = BlenderWrapper(blender_path="/usr/bin/blender")
            assert wrapper.is_available is True
            assert wrapper.blender_path == "/usr/bin/blender"

    def test_wrapper_with_bad_explicit_path(self):
        """BlenderWrapper with a bad explicit path is constructed but unavailable."""
        with patch("subprocess.run", side_effect=FileNotFoundError("nope")):
            wrapper = BlenderWrapper(blender_path="/no/such/blender")
            assert wrapper.is_available is False

    def test_context_manager_without_blender(self):
        """BlenderContext() yields a wrapper even when Blender is missing."""
        with patch("subprocess.run", side_effect=FileNotFoundError("blender")):
            with patch("shutil.which", return_value=None):
                with BlenderContext() as blender:
                    assert isinstance(blender, BlenderWrapper)
                    assert blender.is_available is False


# ---------------------------------------------------------------------------
# Test: Core operations (mocked Blender)
# ---------------------------------------------------------------------------

class TestBlenderWrapperOperations:
    """Tests that exercise wrapper operations via mocked subprocess."""

    @pytest.fixture
    def wrapper(self):
        with patch("subprocess.run", side_effect=mock_blender_run):
            return BlenderWrapper()

    @pytest.fixture
    def temp_dir(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir)

    def test_run_simple_script(self, wrapper):
        """Running a simple script returns success with stdout."""
        with patch("subprocess.run", side_effect=mock_blender_run):
            result = wrapper.run_script(
                "import bpy; print('Test successful')", background=True
            )
            assert result["success"] is True
            assert result["returncode"] == 0

    def test_execute_command(self, wrapper):
        """Execute multiple commands joined as a script."""
        commands = ["import bpy", "print('Command 1')", "print('Command 2')"]
        with patch("subprocess.run", side_effect=mock_blender_run):
            result = wrapper.execute_command(commands, background=True)
            assert result["success"] is True

    def test_script_error_handling(self, wrapper):
        """Failing scripts return success=False with error details."""
        with patch("subprocess.run", side_effect=mock_blender_run_error):
            result = wrapper.run_script(
                "import bpy; raise RuntimeError('Test error')", background=True
            )
            assert result["success"] is False
            assert "error" in result

    def test_invalid_format_raises_error(self, wrapper, temp_dir):
        """Invalid import format raises ValueError."""
        with pytest.raises(ValueError, match="Unsupported format"):
            wrapper.import_file(temp_dir / "test.unknown", "unknown")

    def test_import_obj_file(self, wrapper, temp_dir):
        """OBJ import generates the correct Blender script and reports success."""
        obj_file = temp_dir / "test.obj"
        obj_file.write_text("v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n")
        output_blend = temp_dir / "output.blend"

        with patch("subprocess.run", side_effect=mock_blender_run):
            result = wrapper.import_file(obj_file, "obj", output_blend)
            assert result["success"] is True

    def test_import_stl_file(self, wrapper, temp_dir):
        """STL import generates the correct Blender script."""
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
            result = wrapper.import_file(stl_file, "stl", output_blend)
            assert "success" in result

    def test_export_file(self, wrapper, temp_dir):
        """Export generates the correct Blender script and reports success."""
        blend_file = temp_dir / "test.blend"
        blend_file.write_bytes(b"")  # placeholder
        output_obj = temp_dir / "export.obj"

        with patch("subprocess.run", side_effect=mock_blender_run):
            result = wrapper.export_file(blend_file, output_obj, "obj")
            assert result["success"] is True

    def test_multiple_imports(self, wrapper, temp_dir):
        """Multiple sequential imports all succeed."""
        for i in range(3):
            obj_file = temp_dir / f"test_{i}.obj"
            obj_file.write_text(
                f"v {i} 0 0\nv {i+1} 0 0\nv {i} 1 0\nf 1 2 3\n"
            )

        blend_file = temp_dir / "combined.blend"
        with patch("subprocess.run", side_effect=mock_blender_run):
            for i in range(3):
                obj_file = temp_dir / f"test_{i}.obj"
                result = wrapper.import_file(obj_file, "obj", blend_file)
                assert result["success"] is True

    def test_run_script_on_unavailable_wrapper(self):
        """Running a script on an unavailable wrapper returns an error result."""
        with patch("subprocess.run", side_effect=FileNotFoundError("blender")):
            with patch("shutil.which", return_value=None):
                wrapper = BlenderWrapper()
        result = wrapper.run_script("print('hi')", background=True)
        assert result["success"] is False
        assert "not available" in result.get("error", "").lower()


# ---------------------------------------------------------------------------
# Test: Context manager
# ---------------------------------------------------------------------------

class TestBlenderContext:
    """Tests for the BlenderContext context manager."""

    def test_context_yields_wrapper(self):
        """BlenderContext yields a BlenderWrapper instance."""
        with patch("subprocess.run", side_effect=mock_blender_run):
            with BlenderContext() as blender:
                assert isinstance(blender, BlenderWrapper)
                assert blender.is_available is True

    def test_context_with_blender_path(self):
        """BlenderContext accepts an explicit blender_path."""
        with patch("subprocess.run", side_effect=mock_blender_run):
            with BlenderContext(blender_path="/usr/bin/blender") as blender:
                assert blender.blender_path == "/usr/bin/blender"


# ---------------------------------------------------------------------------
# Test: Real Blender (skip if not installed)
# ---------------------------------------------------------------------------

@requires_blender
class TestBlenderReal:
    """Integration tests that require a real Blender installation."""

    @pytest.fixture
    def wrapper(self):
        return BlenderWrapper()

    def test_real_version_detection(self, wrapper):
        assert wrapper.is_available is True
        assert "Blender" in wrapper.version

    def test_real_simple_script(self, wrapper):
        result = wrapper.run_script(
            "import bpy; print('Real test')", background=True
        )
        assert result["success"] is True
        assert "Real test" in result["stdout"]
