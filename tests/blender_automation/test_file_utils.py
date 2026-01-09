"""
ABOUTME: Tests for file utility functions
Tests file discovery, validation, and system integration utilities.
"""

import pytest
from pathlib import Path
import tempfile

from src.blender_automation.utils.file_utils import (
    find_blender_executable,
    verify_blender_installation,
    find_blend_files,
    find_cad_files,
    get_file_info,
    validate_file_format,
    create_output_path
)


class TestFileUtils:
    """Test suite for file utility functions."""

    @pytest.fixture
    def temp_dir(self):
        """Create temporary directory for test files."""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir)

    def test_find_blender_executable(self):
        """Test finding Blender executable."""
        blender_path = find_blender_executable()

        # Should return either a path or None
        assert blender_path is None or isinstance(blender_path, str)

        if blender_path:
            assert "blender" in blender_path.lower()

    def test_verify_blender_installation(self):
        """Test Blender installation verification."""
        result = verify_blender_installation()

        assert isinstance(result, dict)
        assert "installed" in result
        assert "path" in result
        assert "version" in result

        if result["installed"]:
            assert result["path"] is not None
            assert result["version"] is not None
            assert "Blender" in result["version"]

    def test_find_blend_files(self, temp_dir):
        """Test finding .blend files."""
        # Create test blend files
        (temp_dir / "scene1.blend").write_text("dummy")
        (temp_dir / "scene2.blend").write_text("dummy")

        subdir = temp_dir / "subdir"
        subdir.mkdir()
        (subdir / "scene3.blend").write_text("dummy")

        # Non-recursive
        files = find_blend_files(temp_dir, recursive=False)
        assert len(files) == 2

        # Recursive
        files = find_blend_files(temp_dir, recursive=True)
        assert len(files) == 3

    def test_find_cad_files_default_formats(self, temp_dir):
        """Test finding CAD files with default formats."""
        # Create various CAD files
        (temp_dir / "model.obj").write_text("dummy")
        (temp_dir / "part.stl").write_text("dummy")
        (temp_dir / "assembly.step").write_text("dummy")
        (temp_dir / "drawing.fbx").write_text("dummy")
        (temp_dir / "other.txt").write_text("dummy")  # Should be ignored

        files = find_cad_files(temp_dir, recursive=False)

        assert len(files) == 4
        assert any("obj" in str(f) for f in files)
        assert any("stl" in str(f) for f in files)
        assert any("step" in str(f) for f in files)
        assert any("fbx" in str(f) for f in files)
        assert not any("txt" in str(f) for f in files)

    def test_find_cad_files_specific_formats(self, temp_dir):
        """Test finding CAD files with specific formats."""
        (temp_dir / "model.obj").write_text("dummy")
        (temp_dir / "part.stl").write_text("dummy")
        (temp_dir / "assembly.step").write_text("dummy")

        files = find_cad_files(temp_dir, formats=["obj", "stl"], recursive=False)

        assert len(files) == 2
        assert not any("step" in str(f) for f in files)

    def test_find_files_nonexistent_directory(self):
        """Test that nonexistent directory raises error."""
        with pytest.raises(FileNotFoundError):
            find_blend_files(Path("/nonexistent/directory"))

    def test_get_file_info(self, temp_dir):
        """Test getting file information."""
        test_file = temp_dir / "test.obj"
        test_content = "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3"
        test_file.write_text(test_content)

        info = get_file_info(test_file)

        assert info["name"] == "test.obj"
        assert info["stem"] == "test"
        assert info["suffix"] == ".obj"
        assert info["size_bytes"] == len(test_content.encode())
        assert info["size_mb"] > 0
        assert "modified" in info
        assert "absolute_path" in info

    def test_get_file_info_nonexistent(self, temp_dir):
        """Test getting info for nonexistent file raises error."""
        with pytest.raises(FileNotFoundError):
            get_file_info(temp_dir / "missing.obj")

    def test_validate_file_format(self, temp_dir):
        """Test file format validation."""
        obj_file = temp_dir / "test.obj"
        obj_file.write_text("dummy")

        assert validate_file_format(obj_file, "obj") is True
        assert validate_file_format(obj_file, ".obj") is True  # With dot
        assert validate_file_format(obj_file, "OBJ") is True  # Case insensitive
        assert validate_file_format(obj_file, "stl") is False

    def test_create_output_path_flat(self, temp_dir):
        """Test creating output path without structure preservation."""
        input_file = Path("models/parts/widget.obj")
        output_dir = temp_dir / "output"

        output_path = create_output_path(
            input_file,
            output_dir,
            ".stl",
            preserve_structure=False
        )

        assert output_path.parent == output_dir
        assert output_path.name == "widget.stl"

    def test_create_output_path_preserve_structure(self, temp_dir):
        """Test creating output path with structure preservation."""
        input_file = Path("models/parts/widget.obj")
        output_dir = temp_dir / "output"

        output_path = create_output_path(
            input_file,
            output_dir,
            ".stl",
            preserve_structure=True
        )

        assert "models" in str(output_path)
        assert "parts" in str(output_path)
        assert output_path.name == "widget.stl"

    def test_create_output_path_without_dot(self, temp_dir):
        """Test creating output path with suffix without dot."""
        input_file = Path("test.obj")
        output_dir = temp_dir / "output"

        output_path = create_output_path(
            input_file,
            output_dir,
            "stl",  # No dot
            preserve_structure=False
        )

        assert output_path.suffix == ".stl"

    def test_find_cad_files_recursive(self, temp_dir):
        """Test recursive CAD file search."""
        # Create nested structure
        subdir1 = temp_dir / "level1"
        subdir2 = subdir1 / "level2"
        subdir2.mkdir(parents=True)

        (temp_dir / "root.obj").write_text("dummy")
        (subdir1 / "mid.stl").write_text("dummy")
        (subdir2 / "deep.fbx").write_text("dummy")

        files = find_cad_files(temp_dir, recursive=True)
        assert len(files) == 3

        files_flat = find_cad_files(temp_dir, recursive=False)
        assert len(files_flat) == 1
