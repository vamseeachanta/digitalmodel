"""
ABOUTME: Tests for rollback manager implementation.
Validates checkpoint creation, restoration, and retention policies.
"""

import pytest
from pathlib import Path
import tempfile
import shutil
import json
from datetime import datetime

from digitalmodel.modules.automation.rollback_manager import (
    RollbackManager,
    Checkpoint,
)


@pytest.fixture
def temp_storage():
    """Create temporary storage directory."""
    temp_dir = Path(tempfile.mkdtemp())
    yield temp_dir
    # Windows-safe cleanup: retry with delay to allow file handles to close
    import time
    max_retries = 3
    for i in range(max_retries):
        try:
            shutil.rmtree(temp_dir)
            break
        except PermissionError:
            if i < max_retries - 1:
                time.sleep(0.5)
            else:
                # Last resort: ignore errors on Windows
                shutil.rmtree(temp_dir, ignore_errors=True)


@pytest.fixture
def temp_files(temp_storage):
    """Create temporary test files."""
    files_dir = temp_storage / "test_files"
    files_dir.mkdir()

    # Create test files
    file1 = files_dir / "test1.txt"
    file1.write_text("Content of file 1")

    file2 = files_dir / "test2.txt"
    file2.write_text("Content of file 2")

    file3 = files_dir / "test3.txt"
    file3.write_text("Content of file 3")

    return [file1, file2, file3]


@pytest.fixture
def rollback_manager(temp_storage):
    """Create rollback manager instance."""
    return RollbackManager(
        workflow_id="test-workflow",
        storage_path=temp_storage / "checkpoints",
        checkpoint_retention=3,
        git_enabled=False  # Disable git for tests
    )


class TestCheckpointCreation:
    """Test checkpoint creation."""

    def test_create_checkpoint_with_files(self, rollback_manager, temp_files):
        """Test creating checkpoint with specific files."""
        checkpoint_id = rollback_manager.create_checkpoint(files=temp_files)

        assert checkpoint_id.startswith("checkpoint_")
        assert len(rollback_manager.checkpoints) == 1

        checkpoint = rollback_manager.checkpoints[0]
        assert checkpoint.workflow_id == "test-workflow"
        assert len(checkpoint.files) == 3

    def test_checkpoint_stores_file_content(self, rollback_manager, temp_files):
        """Test checkpoint stores actual file content."""
        checkpoint_id = rollback_manager.create_checkpoint(files=temp_files)

        # Verify stored files exist
        checkpoint_path = rollback_manager._get_checkpoint_path(checkpoint_id)
        assert checkpoint_path.exists()

        # Check each file
        for file_path in temp_files:
            stored_file = checkpoint_path / file_path.name
            assert stored_file.exists()
            assert stored_file.read_text() == file_path.read_text()

    def test_checkpoint_with_metadata(self, rollback_manager, temp_files):
        """Test checkpoint stores metadata."""
        metadata = {
            "agent": "test-agent",
            "operation": "test-operation"
        }

        checkpoint_id = rollback_manager.create_checkpoint(
            files=temp_files,
            metadata=metadata
        )

        checkpoint = rollback_manager.get_checkpoint(checkpoint_id)
        assert checkpoint.metadata == metadata

    def test_checkpoint_file_hashes(self, rollback_manager, temp_files):
        """Test checkpoint generates file hashes."""
        checkpoint_id = rollback_manager.create_checkpoint(files=temp_files)

        checkpoint = rollback_manager.get_checkpoint(checkpoint_id)

        # Each file should have a hash
        assert len(checkpoint.files) == 3
        for file_path, file_hash in checkpoint.files.items():
            assert len(file_hash) == 64  # SHA256 hash length


class TestCheckpointRetrieval:
    """Test checkpoint retrieval."""

    def test_get_checkpoint_by_id(self, rollback_manager, temp_files):
        """Test retrieving checkpoint by ID."""
        checkpoint_id = rollback_manager.create_checkpoint(files=temp_files)

        checkpoint = rollback_manager.get_checkpoint(checkpoint_id)

        assert checkpoint is not None
        assert checkpoint.id == checkpoint_id

    def test_get_nonexistent_checkpoint(self, rollback_manager):
        """Test retrieving non-existent checkpoint returns None."""
        checkpoint = rollback_manager.get_checkpoint("nonexistent")

        assert checkpoint is None

    def test_get_latest_checkpoint(self, rollback_manager, temp_files):
        """Test retrieving latest checkpoint."""
        # Create multiple checkpoints
        checkpoint_id1 = rollback_manager.create_checkpoint(files=[temp_files[0]])
        checkpoint_id2 = rollback_manager.create_checkpoint(files=[temp_files[1]])
        checkpoint_id3 = rollback_manager.create_checkpoint(files=[temp_files[2]])

        latest = rollback_manager.get_latest_checkpoint()

        assert latest is not None
        assert latest.id == checkpoint_id3

    def test_list_checkpoints(self, rollback_manager, temp_files):
        """Test listing all checkpoints."""
        # Create checkpoints
        rollback_manager.create_checkpoint(files=[temp_files[0]])
        rollback_manager.create_checkpoint(files=[temp_files[1]])

        checkpoints = rollback_manager.list_checkpoints()

        assert len(checkpoints) == 2
        assert all("id" in cp for cp in checkpoints)
        assert all("timestamp" in cp for cp in checkpoints)
        assert all("file_count" in cp for cp in checkpoints)


class TestCheckpointRollback:
    """Test checkpoint rollback."""

    def test_rollback_to_checkpoint(self, rollback_manager, temp_files):
        """Test rolling back to specific checkpoint."""
        # Create checkpoint
        checkpoint_id = rollback_manager.create_checkpoint(files=temp_files)

        # Modify files
        for file_path in temp_files:
            file_path.write_text("Modified content")

        # Rollback
        success = rollback_manager.rollback_to(checkpoint_id)

        assert success

        # Verify files restored
        assert temp_files[0].read_text() == "Content of file 1"
        assert temp_files[1].read_text() == "Content of file 2"
        assert temp_files[2].read_text() == "Content of file 3"

    def test_rollback_to_latest(self, rollback_manager, temp_files):
        """Test rolling back to latest checkpoint."""
        # Create checkpoint
        rollback_manager.create_checkpoint(files=temp_files)

        # Modify files
        temp_files[0].write_text("Modified")

        # Rollback to latest
        success = rollback_manager.rollback_to_latest()

        assert success
        assert temp_files[0].read_text() == "Content of file 1"

    def test_rollback_to_nonexistent_checkpoint(self, rollback_manager):
        """Test rollback to non-existent checkpoint fails."""
        success = rollback_manager.rollback_to("nonexistent")

        assert not success

    def test_rollback_when_no_checkpoints(self, rollback_manager):
        """Test rollback when no checkpoints available."""
        success = rollback_manager.rollback_to_latest()

        assert not success


class TestRetentionPolicy:
    """Test checkpoint retention policy."""

    def test_retention_policy_enforced(self, rollback_manager, temp_files):
        """Test old checkpoints are removed."""
        # Create more checkpoints than retention limit
        for i in range(5):
            rollback_manager.create_checkpoint(files=[temp_files[0]])

        # Should only keep last 3
        assert len(rollback_manager.checkpoints) == 3

    def test_oldest_checkpoints_removed(self, rollback_manager, temp_files):
        """Test oldest checkpoints are removed first."""
        # Create checkpoints with identifiable metadata
        checkpoint_ids = []
        for i in range(5):
            checkpoint_id = rollback_manager.create_checkpoint(
                files=[temp_files[0]],
                metadata={"index": i}
            )
            checkpoint_ids.append(checkpoint_id)

        # Should keep last 3
        remaining = rollback_manager.list_checkpoints()
        remaining_indices = [cp["metadata"]["index"] for cp in remaining]

        assert remaining_indices == [4, 3, 2]

    def test_checkpoint_files_cleaned_up(self, rollback_manager, temp_files):
        """Test checkpoint files are removed from storage."""
        import time

        # Create more than retention limit
        checkpoint_ids = []
        for i in range(5):
            checkpoint_id = rollback_manager.create_checkpoint(files=[temp_files[0]])
            checkpoint_ids.append(checkpoint_id)
            # Small delay to ensure unique timestamps
            time.sleep(0.01)

        # Verify we have unique checkpoint IDs
        assert len(set(checkpoint_ids)) == 5, "Checkpoint IDs should be unique"

        # First 2 checkpoint directories should be removed
        for checkpoint_id in checkpoint_ids[:2]:
            checkpoint_path = rollback_manager._get_checkpoint_path(checkpoint_id)
            assert not checkpoint_path.exists()

        # Last 3 should exist
        for checkpoint_id in checkpoint_ids[2:]:
            checkpoint_path = rollback_manager._get_checkpoint_path(checkpoint_id)
            assert checkpoint_path.exists()


class TestPersistence:
    """Test checkpoint persistence."""

    def test_checkpoints_persisted_to_file(self, rollback_manager, temp_files):
        """Test checkpoints are saved to file."""
        rollback_manager.create_checkpoint(files=temp_files)

        checkpoints_file = rollback_manager._get_checkpoints_file()
        assert checkpoints_file.exists()

    def test_checkpoints_loaded_on_init(self, temp_storage, temp_files):
        """Test checkpoints are loaded when creating new manager."""
        # Create first manager and checkpoint
        manager1 = RollbackManager(
            workflow_id="test-workflow",
            storage_path=temp_storage / "checkpoints",
            git_enabled=False
        )

        checkpoint_id = manager1.create_checkpoint(files=temp_files)

        # Create second manager with same workflow
        manager2 = RollbackManager(
            workflow_id="test-workflow",
            storage_path=temp_storage / "checkpoints",
            git_enabled=False
        )

        # Should have loaded checkpoint
        assert len(manager2.checkpoints) == 1
        assert manager2.checkpoints[0].id == checkpoint_id

    def test_checkpoint_index_format(self, rollback_manager, temp_files):
        """Test checkpoint index file format."""
        rollback_manager.create_checkpoint(files=temp_files)

        checkpoints_file = rollback_manager._get_checkpoints_file()

        with open(checkpoints_file, 'r') as f:
            data = json.load(f)

        assert "workflow_id" in data
        assert "checkpoints" in data
        assert data["workflow_id"] == "test-workflow"
        assert len(data["checkpoints"]) == 1


class TestCleanup:
    """Test cleanup operations."""

    def test_cleanup_removes_all_checkpoints(self, rollback_manager, temp_files):
        """Test cleanup removes all checkpoints."""
        # Create checkpoints
        rollback_manager.create_checkpoint(files=temp_files)
        rollback_manager.create_checkpoint(files=temp_files)

        # Cleanup
        rollback_manager.cleanup()

        assert len(rollback_manager.checkpoints) == 0

        checkpoints_file = rollback_manager._get_checkpoints_file()
        assert not checkpoints_file.exists()

    def test_cleanup_removes_checkpoint_files(self, rollback_manager, temp_files):
        """Test cleanup removes checkpoint storage directories."""
        checkpoint_ids = []
        for _ in range(2):
            checkpoint_id = rollback_manager.create_checkpoint(files=temp_files)
            checkpoint_ids.append(checkpoint_id)

        # Cleanup
        rollback_manager.cleanup()

        # Verify directories removed
        for checkpoint_id in checkpoint_ids:
            checkpoint_path = rollback_manager._get_checkpoint_path(checkpoint_id)
            assert not checkpoint_path.exists()


class TestCheckpointDataclass:
    """Test Checkpoint dataclass."""

    def test_checkpoint_to_dict(self):
        """Test converting checkpoint to dictionary."""
        checkpoint = Checkpoint(
            id="test-checkpoint",
            timestamp=123456.789,
            workflow_id="test-workflow",
            git_commit="abc123",
            files={"file1.txt": "hash1"},
            metadata={"key": "value"}
        )

        data = checkpoint.to_dict()

        assert data["id"] == "test-checkpoint"
        assert data["workflow_id"] == "test-workflow"
        assert data["git_commit"] == "abc123"

    def test_checkpoint_from_dict(self):
        """Test creating checkpoint from dictionary."""
        data = {
            "id": "test-checkpoint",
            "timestamp": 123456.789,
            "workflow_id": "test-workflow",
            "git_commit": "abc123",
            "files": {"file1.txt": "hash1"},
            "metadata": {"key": "value"}
        }

        checkpoint = Checkpoint.from_dict(data)

        assert checkpoint.id == "test-checkpoint"
        assert checkpoint.workflow_id == "test-workflow"
        assert checkpoint.git_commit == "abc123"
