"""
ABOUTME: Tests for batch processing functionality
Tests BatchProcessor class for parallel and sequential operations.
"""

import pytest
from pathlib import Path
import tempfile
import time

from src.blender_automation.utils.batch_processor import BatchProcessor


class TestBatchProcessor:
    """Test suite for BatchProcessor class."""

    @pytest.fixture
    def processor(self):
        """Create BatchProcessor instance for testing."""
        return BatchProcessor(max_workers=2)

    @pytest.fixture
    def temp_dir(self):
        """Create temporary directory for test files."""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir)

    @pytest.fixture
    def test_obj_files(self, temp_dir):
        """Create multiple test OBJ files."""
        files = []
        for i in range(5):
            obj_file = temp_dir / f"test_{i}.obj"
            obj_file.write_text(f"""
v {i} 0 0
v {i+1} 0 0
v {i} 1 0
f 1 2 3
""")
            files.append(obj_file)
        return files

    def test_process_files_sequential(self, processor, test_obj_files):
        """Test sequential file processing."""
        def simple_operation(file_path):
            return {"processed": str(file_path)}

        result = processor.process_files(
            test_obj_files,
            simple_operation,
            parallel=False
        )

        assert result["total_files"] == 5
        assert result["successful"] == 5
        assert result["failed"] == 0
        assert len(result["results"]) == 5

    def test_process_files_parallel(self, processor, test_obj_files):
        """Test parallel file processing."""
        def simple_operation(file_path):
            time.sleep(0.1)  # Simulate work
            return {"processed": str(file_path)}

        result = processor.process_files(
            test_obj_files,
            simple_operation,
            parallel=True
        )

        assert result["total_files"] == 5
        assert result["successful"] == 5
        assert result["failed"] == 0
        assert result["duration_seconds"] < 1.0  # Should be faster than sequential

    def test_process_files_with_error(self, processor, test_obj_files):
        """Test error handling in batch processing."""
        def failing_operation(file_path):
            if "test_2" in str(file_path):
                raise RuntimeError("Intentional error")
            return {"processed": str(file_path)}

        result = processor.process_files(
            test_obj_files,
            failing_operation,
            parallel=False,
            continue_on_error=True
        )

        assert result["total_files"] == 5
        assert result["successful"] == 4
        assert result["failed"] == 1

    def test_convert_directory(self, processor, temp_dir):
        """Test directory conversion functionality."""
        # Create input directory with OBJ files
        input_dir = temp_dir / "input"
        input_dir.mkdir()

        for i in range(3):
            obj_file = input_dir / f"model_{i}.obj"
            obj_file.write_text(f"""
v {i} 0 0
v {i+1} 0 0
v {i} 1 0
f 1 2 3
""")

        output_dir = temp_dir / "output"

        result = processor.convert_directory(
            input_dir,
            output_dir,
            "obj",
            "fbx",
            parallel=False
        )

        assert result["total_files"] == 3
        # Success depends on Blender, but should not crash

    def test_convert_directory_recursive(self, processor, temp_dir):
        """Test recursive directory conversion."""
        # Create nested directory structure
        input_dir = temp_dir / "input"
        subdir = input_dir / "subdir"
        subdir.mkdir(parents=True)

        # Files in root
        (input_dir / "root.obj").write_text("v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3")

        # Files in subdirectory
        (subdir / "nested.obj").write_text("v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3")

        output_dir = temp_dir / "output"

        result = processor.convert_directory(
            input_dir,
            output_dir,
            "obj",
            "stl",
            parallel=False,
            recursive=True
        )

        assert result["total_files"] == 2

    def test_batch_with_max_workers(self, temp_dir):
        """Test batch processor with different max_workers."""
        processor_small = BatchProcessor(max_workers=1)
        processor_large = BatchProcessor(max_workers=4)

        assert processor_small.max_workers == 1
        assert processor_large.max_workers == 4

    def test_apply_operation_to_directory(self, processor, temp_dir):
        """Test applying custom operation to directory."""
        # Create blend files (actually just test files)
        blend_files = []
        for i in range(3):
            blend_file = temp_dir / f"scene_{i}.blend"
            blend_file.write_text(f"dummy blend content {i}")
            blend_files.append(blend_file)

        operation_script = """
import bpy
print("Custom operation executed")
"""

        result = processor.apply_operation_to_directory(
            blend_files,
            operation_script,
            save_files=True,
            parallel=False
        )

        # Should process all files
        assert result["total_files"] == 3

    def test_continue_on_error_false(self, processor, test_obj_files):
        """Test that processing stops on error when continue_on_error=False."""
        call_count = [0]

        def failing_operation(file_path):
            call_count[0] += 1
            if call_count[0] == 2:
                raise RuntimeError("Stop processing")
            return {"processed": str(file_path)}

        result = processor.process_files(
            test_obj_files,
            failing_operation,
            parallel=False,
            continue_on_error=False
        )

        # Should stop after error
        assert result["failed"] > 0
        assert call_count[0] == 2  # Only processed 2 files

    def test_empty_file_list(self, processor):
        """Test processing empty file list."""
        def dummy_operation(file_path):
            return {"processed": str(file_path)}

        result = processor.process_files(
            [],
            dummy_operation,
            parallel=False
        )

        assert result["total_files"] == 0
        assert result["successful"] == 0
        assert result["failed"] == 0
