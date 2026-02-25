"""
ABOUTME: Rollback manager for checkpoint creation and restoration.
Manages file state snapshots and git-based recovery mechanisms.
"""

import json
import shutil
import subprocess
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, asdict
import hashlib
import logging


@dataclass
class Checkpoint:
    """Represents a state checkpoint."""
    id: str
    timestamp: float
    workflow_id: str
    git_commit: Optional[str]
    files: Dict[str, str]  # filepath -> content hash
    metadata: Dict[str, Any]

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return asdict(self)

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'Checkpoint':
        """Create from dictionary."""
        return cls(**data)


class RollbackManager:
    """
    Manages checkpoints and rollback operations.

    Features:
    - Automatic checkpoint creation before agent execution
    - File content snapshots
    - Git commit tracking
    - Automatic rollback on failures
    - Checkpoint retention policy
    """

    def __init__(
        self,
        workflow_id: str,
        storage_path: Optional[Path] = None,
        checkpoint_retention: int = 5,
        git_enabled: bool = True
    ):
        """
        Initialize rollback manager.

        Args:
            workflow_id: Unique workflow identifier
            storage_path: Path for checkpoint storage
            checkpoint_retention: Number of checkpoints to retain
            git_enabled: Enable git integration
        """
        self.workflow_id = workflow_id
        self.storage_path = storage_path or Path(".claude-flow/checkpoints")
        self.checkpoint_retention = checkpoint_retention
        self.git_enabled = git_enabled

        # Create storage directory
        self.workflow_path = self.storage_path / workflow_id
        self.workflow_path.mkdir(parents=True, exist_ok=True)

        # Setup logging
        self.logger = self._setup_logging()

        # Load existing checkpoints
        self.checkpoints: List[Checkpoint] = []
        self._load_checkpoints()

    def _setup_logging(self) -> logging.Logger:
        """Setup logging for rollback manager."""
        logger = logging.getLogger(f"rollback.{self.workflow_id}")
        logger.setLevel(logging.INFO)

        # File handler
        log_file = self.storage_path / "rollback.log"
        fh = logging.FileHandler(log_file)
        fh.setLevel(logging.INFO)

        # Format
        formatter = logging.Formatter(
            "[%(asctime)s] [%(name)s] %(message)s",
            datefmt="%Y-%m-%d %H:%M:%S"
        )
        fh.setFormatter(formatter)

        logger.addHandler(fh)
        return logger

    def _get_checkpoints_file(self) -> Path:
        """Get path to checkpoints index file."""
        return self.workflow_path / "checkpoints.json"

    def _load_checkpoints(self) -> None:
        """Load existing checkpoints from storage."""
        checkpoints_file = self._get_checkpoints_file()
        if not checkpoints_file.exists():
            return

        try:
            with open(checkpoints_file, 'r') as f:
                data = json.load(f)

            self.checkpoints = [
                Checkpoint.from_dict(cp) for cp in data.get("checkpoints", [])
            ]

            self.logger.info(f"Loaded {len(self.checkpoints)} checkpoints")

        except Exception as e:
            self.logger.error(f"Failed to load checkpoints: {e}")

    def _save_checkpoints(self) -> None:
        """Save checkpoints index to storage."""
        checkpoints_file = self._get_checkpoints_file()

        data = {
            "workflow_id": self.workflow_id,
            "checkpoints": [cp.to_dict() for cp in self.checkpoints]
        }

        try:
            with open(checkpoints_file, 'w') as f:
                json.dump(data, f, indent=2)
        except Exception as e:
            self.logger.error(f"Failed to save checkpoints: {e}")

    def _get_git_commit(self) -> Optional[str]:
        """
        Get current git commit hash.

        Returns:
            Commit hash or None if not in git repo
        """
        if not self.git_enabled:
            return None

        try:
            result = subprocess.run(
                ["git", "rev-parse", "HEAD"],
                capture_output=True,
                text=True,
                check=True
            )
            return result.stdout.strip()
        except (subprocess.CalledProcessError, FileNotFoundError):
            return None

    def _hash_file_content(self, content: str) -> str:
        """
        Generate hash of file content.

        Args:
            content: File content

        Returns:
            SHA256 hash
        """
        return hashlib.sha256(content.encode()).hexdigest()

    def _get_checkpoint_path(self, checkpoint_id: str) -> Path:
        """Get storage path for checkpoint files."""
        return self.workflow_path / checkpoint_id

    def create_checkpoint(
        self,
        files: Optional[List[Path]] = None,
        metadata: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        Create a new checkpoint.

        Args:
            files: List of files to checkpoint (None = track git changes)
            metadata: Additional metadata to store

        Returns:
            Checkpoint ID
        """
        timestamp = datetime.now().timestamp()
        # Use microseconds to ensure unique IDs even for rapid checkpoint creation
        checkpoint_id = f"checkpoint_{int(timestamp * 1000000)}"

        # Get git commit
        git_commit = self._get_git_commit()

        # Determine files to checkpoint
        if files is None:
            files = self._get_modified_files()

        # Create checkpoint directory
        checkpoint_path = self._get_checkpoint_path(checkpoint_id)
        checkpoint_path.mkdir(parents=True, exist_ok=True)

        # Store file contents and hashes
        file_hashes: Dict[str, str] = {}

        for file_path in files:
            if not file_path.exists():
                continue

            try:
                # Read content
                content = file_path.read_text(encoding='utf-8')
                content_hash = self._hash_file_content(content)

                # Store content
                stored_file = checkpoint_path / file_path.name
                stored_file.write_text(content, encoding='utf-8')

                # Record hash
                file_hashes[str(file_path)] = content_hash

            except Exception as e:
                self.logger.warning(f"Failed to checkpoint {file_path}: {e}")

        # Create checkpoint object
        checkpoint = Checkpoint(
            id=checkpoint_id,
            timestamp=timestamp,
            workflow_id=self.workflow_id,
            git_commit=git_commit,
            files=file_hashes,
            metadata=metadata or {}
        )

        # Add to list
        self.checkpoints.append(checkpoint)

        # Apply retention policy
        self._apply_retention_policy()

        # Save index
        self._save_checkpoints()

        self.logger.info(
            f"Created checkpoint {checkpoint_id} with {len(file_hashes)} files"
        )

        return checkpoint_id

    def _get_modified_files(self) -> List[Path]:
        """
        Get list of modified files from git.

        Returns:
            List of modified file paths
        """
        if not self.git_enabled:
            return []

        try:
            # Get modified and staged files
            result = subprocess.run(
                ["git", "diff", "--name-only", "HEAD"],
                capture_output=True,
                text=True,
                check=True
            )

            files = [
                Path(line.strip())
                for line in result.stdout.split('\n')
                if line.strip()
            ]

            return files

        except (subprocess.CalledProcessError, FileNotFoundError):
            return []

    def _apply_retention_policy(self) -> None:
        """Remove old checkpoints beyond retention limit."""
        if len(self.checkpoints) <= self.checkpoint_retention:
            return

        # Sort by timestamp
        self.checkpoints.sort(key=lambda cp: cp.timestamp)

        # Remove oldest
        to_remove = self.checkpoints[:-self.checkpoint_retention]

        for checkpoint in to_remove:
            checkpoint_path = self._get_checkpoint_path(checkpoint.id)
            if checkpoint_path.exists():
                shutil.rmtree(checkpoint_path)
            self.logger.info(f"Removed old checkpoint {checkpoint.id}")

        # Keep only recent checkpoints
        self.checkpoints = self.checkpoints[-self.checkpoint_retention:]

    def get_checkpoint(self, checkpoint_id: str) -> Optional[Checkpoint]:
        """
        Get checkpoint by ID.

        Args:
            checkpoint_id: Checkpoint identifier

        Returns:
            Checkpoint object or None
        """
        for checkpoint in self.checkpoints:
            if checkpoint.id == checkpoint_id:
                return checkpoint
        return None

    def get_latest_checkpoint(self) -> Optional[Checkpoint]:
        """
        Get most recent checkpoint.

        Returns:
            Latest checkpoint or None
        """
        if not self.checkpoints:
            return None

        return max(self.checkpoints, key=lambda cp: cp.timestamp)

    def rollback_to(self, checkpoint_id: str) -> bool:
        """
        Rollback to a specific checkpoint.

        Args:
            checkpoint_id: Checkpoint to restore

        Returns:
            True if rollback successful
        """
        checkpoint = self.get_checkpoint(checkpoint_id)
        if not checkpoint:
            self.logger.error(f"Checkpoint {checkpoint_id} not found")
            return False

        checkpoint_path = self._get_checkpoint_path(checkpoint_id)
        if not checkpoint_path.exists():
            self.logger.error(f"Checkpoint files not found: {checkpoint_path}")
            return False

        try:
            # Restore files
            restored_count = 0

            for file_path_str in checkpoint.files.keys():
                file_path = Path(file_path_str)
                stored_file = checkpoint_path / file_path.name

                if not stored_file.exists():
                    self.logger.warning(f"Stored file not found: {stored_file}")
                    continue

                # Restore content
                content = stored_file.read_text(encoding='utf-8')
                file_path.parent.mkdir(parents=True, exist_ok=True)
                file_path.write_text(content, encoding='utf-8')

                restored_count += 1

            self.logger.info(
                f"Rollback to {checkpoint_id} completed: {restored_count} files restored"
            )

            return True

        except Exception as e:
            self.logger.error(f"Rollback failed: {e}")
            return False

    def rollback_to_latest(self) -> bool:
        """
        Rollback to most recent checkpoint.

        Returns:
            True if rollback successful
        """
        checkpoint = self.get_latest_checkpoint()
        if not checkpoint:
            self.logger.error("No checkpoints available")
            return False

        return self.rollback_to(checkpoint.id)

    def list_checkpoints(self) -> List[Dict[str, Any]]:
        """
        List all checkpoints.

        Returns:
            List of checkpoint summaries
        """
        return [
            {
                "id": cp.id,
                "timestamp": datetime.fromtimestamp(cp.timestamp).isoformat(),
                "git_commit": cp.git_commit,
                "file_count": len(cp.files),
                "metadata": cp.metadata
            }
            for cp in sorted(self.checkpoints, key=lambda x: x.timestamp, reverse=True)
        ]

    def cleanup(self) -> None:
        """Remove all checkpoints for this workflow."""
        for checkpoint in self.checkpoints:
            checkpoint_path = self._get_checkpoint_path(checkpoint.id)
            if checkpoint_path.exists():
                shutil.rmtree(checkpoint_path)

        self.checkpoints.clear()

        checkpoints_file = self._get_checkpoints_file()
        if checkpoints_file.exists():
            checkpoints_file.unlink()

        self.logger.info("Cleaned up all checkpoints")
