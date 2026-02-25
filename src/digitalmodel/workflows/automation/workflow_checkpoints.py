# ABOUTME: Workflow checkpoint management system for capturing and restoring workflow state
# Provides automatic checkpointing before SPARC phases with git, memory, and file snapshots

import asyncio
import gzip
import json
import os
import shutil
import subprocess
from dataclasses import asdict, dataclass
from datetime import datetime, timedelta
from pathlib import Path
from typing import Any, Dict, List, Optional, Set

import yaml
from loguru import logger


@dataclass
class GitState:
    """Git repository state snapshot."""

    commit_hash: str
    branch: str
    author: str
    message: str
    dirty: bool
    staged_files: List[str]
    modified_files: List[str]
    untracked_files: List[str]


@dataclass
class AgentState:
    """Agent metadata and status."""

    agent_id: str
    agent_type: str
    status: str
    task_id: Optional[str]
    memory_keys: List[str]
    timestamp: str


@dataclass
class CheckpointMetadata:
    """Checkpoint metadata and configuration."""

    checkpoint_id: str
    workflow_id: str
    phase: str
    description: str
    timestamp: str
    tags: List[str]
    git_state: GitState
    agent_states: List[AgentState]
    memory_keys: List[str]
    file_count: int
    total_size_bytes: int
    compressed: bool


class WorkflowCheckpointManager:
    """
    Manages workflow checkpoints with full state capture and restoration.

    Features:
    - Full state capture (git + memory + files + agent metadata)
    - Automatic checkpointing before SPARC phases
    - Incremental snapshots (only changed files)
    - Compression for large snapshots
    - Safe restoration with current state backup
    - Automatic cleanup based on policies
    """

    def __init__(
        self,
        repo_root: Optional[Path] = None,
        checkpoint_dir: Optional[Path] = None,
        config_path: Optional[Path] = None,
    ):
        """
        Initialize checkpoint manager.

        Args:
            repo_root: Repository root directory (default: current git root)
            checkpoint_dir: Checkpoint storage directory (default: .claude-flow/checkpoints)
            config_path: Path to checkpoint policies config (default: config/checkpoint-policies.yaml)
        """
        self.repo_root = repo_root or self._find_git_root()
        self.checkpoint_dir = checkpoint_dir or (
            self.repo_root / ".claude-flow" / "checkpoints"
        )
        self.config_path = config_path or (
            self.repo_root / "config" / "checkpoint-policies.yaml"
        )

        # Ensure checkpoint directory exists
        self.checkpoint_dir.mkdir(parents=True, exist_ok=True)

        # Load configuration
        self.config = self._load_config()

        # Track last checkpoint for incremental snapshots
        self._last_checkpoint_files: Set[str] = set()

    def _find_git_root(self) -> Path:
        """Find git repository root."""
        try:
            result = subprocess.run(
                ["git", "rev-parse", "--show-toplevel"],
                capture_output=True,
                text=True,
                check=True,
            )
            return Path(result.stdout.strip())
        except subprocess.CalledProcessError as e:
            logger.error(f"Not in a git repository: {e}")
            # Fallback to current directory
            return Path.cwd()

    def _load_config(self) -> Dict[str, Any]:
        """Load checkpoint policies configuration."""
        if not self.config_path.exists():
            logger.warning(f"Config not found at {self.config_path}, using defaults")
            return self._default_config()

        try:
            with open(self.config_path, "r") as f:
                return yaml.safe_load(f)
        except Exception as e:
            logger.error(f"Failed to load config: {e}, using defaults")
            return self._default_config()

    def _default_config(self) -> Dict[str, Any]:
        """Default checkpoint policies."""
        return {
            "retention": {
                "time_based_days": 7,
                "count_based": 10,
                "keep_tagged_forever": True,
            },
            "compression": {
                "enabled": True,
                "threshold_mb": 1,
            },
            "incremental": {
                "enabled": True,
            },
            "automatic_cleanup": {
                "enabled": True,
                "on_session_end": True,
            },
        }

    def _get_git_state(self) -> GitState:
        """Capture current git state."""
        try:
            # Get commit info
            commit_hash = subprocess.run(
                ["git", "rev-parse", "HEAD"],
                capture_output=True,
                text=True,
                check=True,
                cwd=self.repo_root,
            ).stdout.strip()

            branch = subprocess.run(
                ["git", "rev-parse", "--abbrev-ref", "HEAD"],
                capture_output=True,
                text=True,
                check=True,
                cwd=self.repo_root,
            ).stdout.strip()

            author = subprocess.run(
                ["git", "log", "-1", "--format=%an"],
                capture_output=True,
                text=True,
                check=True,
                cwd=self.repo_root,
            ).stdout.strip()

            message = subprocess.run(
                ["git", "log", "-1", "--format=%s"],
                capture_output=True,
                text=True,
                check=True,
                cwd=self.repo_root,
            ).stdout.strip()

            # Get file states
            status = subprocess.run(
                ["git", "status", "--porcelain"],
                capture_output=True,
                text=True,
                check=True,
                cwd=self.repo_root,
            ).stdout

            staged_files = []
            modified_files = []
            untracked_files = []

            for line in status.split("\n"):
                if not line:
                    continue
                status_code = line[:2]
                filename = line[3:]

                if status_code[0] in ["A", "M", "D", "R", "C"]:
                    staged_files.append(filename)
                if status_code[1] in ["M", "D"]:
                    modified_files.append(filename)
                if status_code == "??":
                    untracked_files.append(filename)

            dirty = bool(staged_files or modified_files or untracked_files)

            return GitState(
                commit_hash=commit_hash,
                branch=branch,
                author=author,
                message=message,
                dirty=dirty,
                staged_files=staged_files,
                modified_files=modified_files,
                untracked_files=untracked_files,
            )
        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to get git state: {e}")
            raise

    def _get_agent_states(self) -> List[AgentState]:
        """
        Capture agent states from claude-flow memory.

        Returns list of agent states by querying MCP memory.
        """
        agent_states = []

        # Try to read from claude-flow memory
        memory_dir = self.repo_root / ".claude-flow" / "memory"
        if memory_dir.exists():
            for agent_file in memory_dir.glob("agent_*.json"):
                try:
                    with open(agent_file, "r") as f:
                        agent_data = json.load(f)
                        agent_states.append(
                            AgentState(
                                agent_id=agent_data.get("agent_id", "unknown"),
                                agent_type=agent_data.get("agent_type", "unknown"),
                                status=agent_data.get("status", "unknown"),
                                task_id=agent_data.get("task_id"),
                                memory_keys=agent_data.get("memory_keys", []),
                                timestamp=agent_data.get("timestamp", ""),
                            )
                        )
                except Exception as e:
                    logger.warning(f"Failed to read agent state from {agent_file}: {e}")

        return agent_states

    def _get_memory_state(self) -> Dict[str, Any]:
        """
        Capture MCP memory state.

        Returns dictionary of all memory keys and values.
        """
        memory_state = {}

        memory_dir = self.repo_root / ".claude-flow" / "memory"
        if memory_dir.exists():
            for memory_file in memory_dir.glob("*.json"):
                try:
                    with open(memory_file, "r") as f:
                        key = memory_file.stem
                        memory_state[key] = json.load(f)
                except Exception as e:
                    logger.warning(f"Failed to read memory from {memory_file}: {e}")

        return memory_state

    def _snapshot_files(
        self,
        checkpoint_path: Path,
        incremental: bool = True,
    ) -> tuple[int, int]:
        """
        Create file snapshot with optional compression.

        Args:
            checkpoint_path: Checkpoint directory path
            incremental: Only snapshot changed files

        Returns:
            Tuple of (file_count, total_size_bytes)
        """
        snapshot_dir = checkpoint_path / "files"
        snapshot_dir.mkdir(parents=True, exist_ok=True)

        file_count = 0
        total_size = 0
        current_files = set()

        # Get tracked and modified files
        tracked_files = subprocess.run(
            ["git", "ls-files"],
            capture_output=True,
            text=True,
            check=True,
            cwd=self.repo_root,
        ).stdout.strip().split("\n")

        modified_files = subprocess.run(
            ["git", "ls-files", "-m"],
            capture_output=True,
            text=True,
            check=True,
            cwd=self.repo_root,
        ).stdout.strip().split("\n")

        # Combine tracked and modified files
        files_to_snapshot = set(tracked_files + modified_files)
        files_to_snapshot.discard("")  # Remove empty strings

        # Filter for incremental
        if incremental and self._last_checkpoint_files:
            files_to_snapshot = files_to_snapshot - self._last_checkpoint_files
            logger.info(f"Incremental snapshot: {len(files_to_snapshot)} changed files")

        compression_threshold = self.config["compression"]["threshold_mb"] * 1024 * 1024

        for filepath in files_to_snapshot:
            src = self.repo_root / filepath
            if not src.exists():
                continue

            current_files.add(filepath)
            dst = snapshot_dir / filepath
            dst.parent.mkdir(parents=True, exist_ok=True)

            file_size = src.stat().st_size
            total_size += file_size

            # Compress if over threshold
            if (
                self.config["compression"]["enabled"]
                and file_size > compression_threshold
            ):
                with open(src, "rb") as f_in:
                    with gzip.open(f"{dst}.gz", "wb") as f_out:
                        shutil.copyfileobj(f_in, f_out)
            else:
                shutil.copy2(src, dst)

            file_count += 1

        # Update last checkpoint files for next incremental
        self._last_checkpoint_files = current_files

        return file_count, total_size

    async def create_checkpoint(
        self,
        workflow_id: str,
        phase: str,
        description: str = "",
        tags: Optional[List[str]] = None,
    ) -> str:
        """
        Create a new workflow checkpoint.

        Args:
            workflow_id: Unique workflow identifier
            phase: SPARC phase (spec, pseudocode, architecture, refinement, completion)
            description: Human-readable checkpoint description
            tags: Optional tags for organizing checkpoints

        Returns:
            Checkpoint ID
        """
        # Check if checkpoints are enabled
        if os.getenv("CLAUDE_FLOW_CHECKPOINTS_ENABLED", "true").lower() == "false":
            logger.info("Checkpoints disabled via environment variable")
            return ""

        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        checkpoint_id = f"{workflow_id}_{phase}_{timestamp}"

        logger.info(f"Creating checkpoint: {checkpoint_id}")

        # Create checkpoint directory
        checkpoint_path = self.checkpoint_dir / workflow_id / timestamp
        checkpoint_path.mkdir(parents=True, exist_ok=True)

        # Capture state
        git_state = self._get_git_state()
        agent_states = self._get_agent_states()
        memory_state = self._get_memory_state()

        # Snapshot files
        file_count, total_size = self._snapshot_files(
            checkpoint_path,
            incremental=self.config["incremental"]["enabled"],
        )

        # Save memory state
        memory_file = checkpoint_path / "memory.json"
        with open(memory_file, "w") as f:
            json.dump(memory_state, f, indent=2)

        # Create metadata
        metadata = CheckpointMetadata(
            checkpoint_id=checkpoint_id,
            workflow_id=workflow_id,
            phase=phase,
            description=description,
            timestamp=timestamp,
            tags=tags or [],
            git_state=git_state,
            agent_states=agent_states,
            memory_keys=list(memory_state.keys()),
            file_count=file_count,
            total_size_bytes=total_size,
            compressed=self.config["compression"]["enabled"],
        )

        # Save metadata
        metadata_file = checkpoint_path / "metadata.json"
        with open(metadata_file, "w") as f:
            json.dump(
                {
                    **asdict(metadata),
                    "git_state": asdict(git_state),
                    "agent_states": [asdict(a) for a in agent_states],
                },
                f,
                indent=2,
            )

        logger.info(
            f"Checkpoint created: {checkpoint_id} "
            f"({file_count} files, {total_size / 1024 / 1024:.2f} MB)"
        )

        return checkpoint_id

    async def restore_checkpoint(
        self,
        checkpoint_id: str,
        force: bool = False,
        create_backup: bool = True,
    ) -> bool:
        """
        Restore a workflow checkpoint.

        Args:
            checkpoint_id: Checkpoint ID to restore
            force: Skip confirmation (requires --force flag)
            create_backup: Create checkpoint of current state before restoring

        Returns:
            True if restoration successful
        """
        logger.info(f"Restoring checkpoint: {checkpoint_id}")

        # Find checkpoint
        checkpoint_path = None
        for workflow_dir in self.checkpoint_dir.iterdir():
            if not workflow_dir.is_dir():
                continue
            for timestamp_dir in workflow_dir.iterdir():
                if not timestamp_dir.is_dir():
                    continue
                metadata_file = timestamp_dir / "metadata.json"
                if metadata_file.exists():
                    with open(metadata_file, "r") as f:
                        metadata = json.load(f)
                        if metadata.get("checkpoint_id") == checkpoint_id:
                            checkpoint_path = timestamp_dir
                            break
            if checkpoint_path:
                break

        if not checkpoint_path:
            logger.error(f"Checkpoint not found: {checkpoint_id}")
            return False

        # Create backup of current state if requested
        if create_backup:
            logger.info("Creating backup of current state before restoration")
            backup_id = await self.create_checkpoint(
                workflow_id=f"backup_{checkpoint_id.split('_')[0]}",
                phase="pre_restore_backup",
                description=f"Automatic backup before restoring {checkpoint_id}",
                tags=["auto_backup"],
            )
            logger.info(f"Current state backed up to: {backup_id}")

        # Load metadata
        with open(checkpoint_path / "metadata.json", "r") as f:
            metadata = json.load(f)

        # Restore files
        snapshot_dir = checkpoint_path / "files"
        if snapshot_dir.exists():
            for src_file in snapshot_dir.rglob("*"):
                if src_file.is_file():
                    rel_path = src_file.relative_to(snapshot_dir)

                    # Handle compressed files
                    if src_file.suffix == ".gz":
                        rel_path = Path(str(rel_path)[:-3])  # Remove .gz
                        dst = self.repo_root / rel_path
                        dst.parent.mkdir(parents=True, exist_ok=True)
                        with gzip.open(src_file, "rb") as f_in:
                            with open(dst, "wb") as f_out:
                                shutil.copyfileobj(f_in, f_out)
                    else:
                        dst = self.repo_root / rel_path
                        dst.parent.mkdir(parents=True, exist_ok=True)
                        shutil.copy2(src_file, dst)

        # Restore memory state
        memory_file = checkpoint_path / "memory.json"
        if memory_file.exists():
            with open(memory_file, "r") as f:
                memory_state = json.load(f)

            memory_dir = self.repo_root / ".claude-flow" / "memory"
            memory_dir.mkdir(parents=True, exist_ok=True)

            for key, value in memory_state.items():
                with open(memory_dir / f"{key}.json", "w") as f:
                    json.dump(value, f, indent=2)

        logger.info(f"Successfully restored checkpoint: {checkpoint_id}")
        logger.warning(
            "Git state was captured but not automatically restored. "
            f"You may want to checkout: {metadata['git_state']['commit_hash']}"
        )

        return True

    def list_checkpoints(
        self,
        workflow_id: Optional[str] = None,
        phase: Optional[str] = None,
        tags: Optional[List[str]] = None,
        date_from: Optional[datetime] = None,
        date_to: Optional[datetime] = None,
    ) -> List[CheckpointMetadata]:
        """
        List available checkpoints with optional filtering.

        Args:
            workflow_id: Filter by workflow ID
            phase: Filter by SPARC phase
            tags: Filter by tags (any match)
            date_from: Filter by start date
            date_to: Filter by end date

        Returns:
            List of checkpoint metadata
        """
        checkpoints = []

        for workflow_dir in self.checkpoint_dir.iterdir():
            if not workflow_dir.is_dir():
                continue

            # Filter by workflow_id
            if workflow_id and workflow_dir.name != workflow_id:
                continue

            for timestamp_dir in workflow_dir.iterdir():
                if not timestamp_dir.is_dir():
                    continue

                metadata_file = timestamp_dir / "metadata.json"
                if not metadata_file.exists():
                    continue

                try:
                    with open(metadata_file, "r") as f:
                        data = json.load(f)

                    # Reconstruct metadata
                    checkpoint_time = datetime.strptime(
                        data["timestamp"], "%Y%m%d_%H%M%S"
                    )

                    # Filter by date
                    if date_from and checkpoint_time < date_from:
                        continue
                    if date_to and checkpoint_time > date_to:
                        continue

                    # Filter by phase
                    if phase and data.get("phase") != phase:
                        continue

                    # Filter by tags
                    if tags:
                        checkpoint_tags = set(data.get("tags", []))
                        if not checkpoint_tags.intersection(tags):
                            continue

                    # Create metadata object
                    git_state = GitState(**data["git_state"])
                    agent_states = [AgentState(**a) for a in data.get("agent_states", [])]

                    metadata = CheckpointMetadata(
                        checkpoint_id=data["checkpoint_id"],
                        workflow_id=data["workflow_id"],
                        phase=data["phase"],
                        description=data.get("description", ""),
                        timestamp=data["timestamp"],
                        tags=data.get("tags", []),
                        git_state=git_state,
                        agent_states=agent_states,
                        memory_keys=data.get("memory_keys", []),
                        file_count=data.get("file_count", 0),
                        total_size_bytes=data.get("total_size_bytes", 0),
                        compressed=data.get("compressed", False),
                    )

                    checkpoints.append(metadata)

                except Exception as e:
                    logger.warning(f"Failed to load checkpoint metadata from {metadata_file}: {e}")

        # Sort by timestamp (newest first)
        checkpoints.sort(key=lambda c: c.timestamp, reverse=True)

        return checkpoints

    async def cleanup_checkpoints(self) -> Dict[str, int]:
        """
        Clean up old checkpoints based on retention policies.

        Returns:
            Dictionary with cleanup statistics
        """
        logger.info("Starting checkpoint cleanup")

        stats = {
            "total_checkpoints": 0,
            "deleted_checkpoints": 0,
            "kept_by_tag": 0,
            "kept_by_time": 0,
            "kept_by_count": 0,
        }

        retention_days = self.config["retention"]["time_based_days"]
        max_count = self.config["retention"]["count_based"]
        keep_tagged = self.config["retention"]["keep_tagged_forever"]

        cutoff_date = datetime.now() - timedelta(days=retention_days)

        # Group checkpoints by workflow
        workflow_checkpoints: Dict[str, List[tuple[Path, CheckpointMetadata]]] = {}

        for workflow_dir in self.checkpoint_dir.iterdir():
            if not workflow_dir.is_dir():
                continue

            workflow_id = workflow_dir.name
            workflow_checkpoints[workflow_id] = []

            for timestamp_dir in workflow_dir.iterdir():
                if not timestamp_dir.is_dir():
                    continue

                metadata_file = timestamp_dir / "metadata.json"
                if not metadata_file.exists():
                    continue

                try:
                    with open(metadata_file, "r") as f:
                        data = json.load(f)

                    stats["total_checkpoints"] += 1

                    # Parse metadata
                    checkpoint_time = datetime.strptime(
                        data["timestamp"], "%Y%m%d_%H%M%S"
                    )
                    has_tags = bool(data.get("tags"))

                    # Keep if tagged
                    if keep_tagged and has_tags:
                        stats["kept_by_tag"] += 1
                        continue

                    # Keep if within time retention
                    if checkpoint_time >= cutoff_date:
                        stats["kept_by_time"] += 1
                        workflow_checkpoints[workflow_id].append(
                            (timestamp_dir, checkpoint_time)
                        )
                        continue

                    # Otherwise, mark for potential deletion
                    workflow_checkpoints[workflow_id].append(
                        (timestamp_dir, checkpoint_time)
                    )

                except Exception as e:
                    logger.warning(f"Failed to process {metadata_file}: {e}")

        # Apply count-based retention per workflow
        for workflow_id, checkpoints in workflow_checkpoints.items():
            # Sort by time (newest first)
            checkpoints.sort(key=lambda x: x[1], reverse=True)

            # Keep only max_count most recent
            for i, (checkpoint_path, _) in enumerate(checkpoints):
                if i < max_count:
                    stats["kept_by_count"] += 1
                else:
                    # Delete checkpoint
                    try:
                        shutil.rmtree(checkpoint_path)
                        stats["deleted_checkpoints"] += 1
                        logger.info(f"Deleted checkpoint: {checkpoint_path.name}")
                    except Exception as e:
                        logger.error(f"Failed to delete {checkpoint_path}: {e}")

        logger.info(
            f"Cleanup complete: {stats['deleted_checkpoints']} deleted, "
            f"{stats['kept_by_tag'] + stats['kept_by_time'] + stats['kept_by_count']} kept"
        )

        return stats

    def get_checkpoint_info(self, checkpoint_id: str) -> Optional[CheckpointMetadata]:
        """
        Get detailed information about a specific checkpoint.

        Args:
            checkpoint_id: Checkpoint ID

        Returns:
            CheckpointMetadata or None if not found
        """
        checkpoints = self.list_checkpoints()
        for checkpoint in checkpoints:
            if checkpoint.checkpoint_id == checkpoint_id:
                return checkpoint
        return None
