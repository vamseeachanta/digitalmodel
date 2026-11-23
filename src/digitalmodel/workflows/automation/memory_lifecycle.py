"""
ABOUTME: Memory lifecycle management system with TTL-based retention, garbage collection,
and CRDT conflict resolution for claude-flow memory coordination.

This module provides automatic memory cleanup, compression, and conflict resolution
for file-based memory storage used in agent coordination.
"""

import json
import gzip
import sqlite3
import time
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple, Union
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
import yaml
import logging

logger = logging.getLogger(__name__)


class ConflictStrategy(Enum):
    """CRDT conflict resolution strategies."""
    LAST_WRITE_WINS = "lww"
    MULTI_VALUE = "multi_value"
    COUNTER = "counter"


class CleanupStrategy(Enum):
    """Memory cleanup strategies."""
    LAZY = "lazy"  # On-access cleanup
    SCHEDULED = "scheduled"  # Periodic cleanup
    MANUAL = "manual"  # Explicit trigger only


@dataclass
class RetentionPolicy:
    """Retention policy configuration for memory namespace."""

    namespace: str
    ttl_seconds: int
    cleanup_strategy: CleanupStrategy
    conflict_strategy: ConflictStrategy
    compression_enabled: bool = True
    compression_threshold_bytes: int = 1024  # 1KB

    def is_expired(self, timestamp: float) -> bool:
        """Check if timestamp is expired based on TTL."""
        if self.ttl_seconds == 0:  # Permanent
            return False
        age_seconds = time.time() - timestamp
        return age_seconds > self.ttl_seconds

    def matches_namespace(self, key: str) -> bool:
        """Check if key matches this policy's namespace pattern."""
        # Support wildcards like "coordination/*"
        pattern = self.namespace.rstrip("*")
        return key.startswith(pattern)


@dataclass
class MemoryEntry:
    """Memory entry with metadata."""

    key: str
    value: Any
    timestamp: float
    namespace: str
    compressed: bool = False
    version: int = 1
    conflicted: bool = False
    conflict_values: List[Tuple[Any, float]] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        """Serialize to dictionary."""
        return {
            "key": self.key,
            "value": self.value,
            "timestamp": self.timestamp,
            "namespace": self.namespace,
            "compressed": self.compressed,
            "version": self.version,
            "conflicted": self.conflicted,
            "conflict_values": self.conflict_values
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "MemoryEntry":
        """Deserialize from dictionary."""
        return cls(
            key=data["key"],
            value=data["value"],
            timestamp=data["timestamp"],
            namespace=data["namespace"],
            compressed=data.get("compressed", False),
            version=data.get("version", 1),
            conflicted=data.get("conflicted", False),
            conflict_values=data.get("conflict_values", [])
        )


class CRDTResolver:
    """CRDT-based conflict resolution for distributed memory."""

    def resolve(
        self,
        strategy: ConflictStrategy,
        current: Optional[MemoryEntry],
        incoming: MemoryEntry
    ) -> MemoryEntry:
        """
        Resolve conflict between current and incoming entries.

        Args:
            strategy: Conflict resolution strategy
            current: Current memory entry (may be None)
            incoming: Incoming memory entry

        Returns:
            Resolved memory entry
        """
        if current is None:
            return incoming

        if strategy == ConflictStrategy.LAST_WRITE_WINS:
            return self._resolve_lww(current, incoming)
        elif strategy == ConflictStrategy.MULTI_VALUE:
            return self._resolve_multi_value(current, incoming)
        elif strategy == ConflictStrategy.COUNTER:
            return self._resolve_counter(current, incoming)
        else:
            raise ValueError(f"Unknown conflict strategy: {strategy}")

    def _resolve_lww(self, current: MemoryEntry, incoming: MemoryEntry) -> MemoryEntry:
        """Last-Write-Wins: Keep entry with latest timestamp."""
        if incoming.timestamp > current.timestamp:
            incoming.version = current.version + 1
            return incoming
        return current

    def _resolve_multi_value(self, current: MemoryEntry, incoming: MemoryEntry) -> MemoryEntry:
        """Multi-Value: Keep all conflicting values with timestamps."""
        if incoming.timestamp > current.timestamp:
            # Newer value becomes primary, old value stored as conflict
            incoming.conflicted = True
            incoming.conflict_values = [(current.value, current.timestamp)]
            if current.conflict_values:
                incoming.conflict_values.extend(current.conflict_values)
            incoming.version = current.version + 1
            return incoming
        else:
            # Keep current, add incoming as conflict
            current.conflicted = True
            if not current.conflict_values:
                current.conflict_values = []
            current.conflict_values.append((incoming.value, incoming.timestamp))
            return current

    def _resolve_counter(self, current: MemoryEntry, incoming: MemoryEntry) -> MemoryEntry:
        """Counter: Merge by addition (for counter CRDTs)."""
        try:
            current_val = int(current.value)
            incoming_val = int(incoming.value)
            merged_val = current_val + incoming_val

            current.value = merged_val
            current.timestamp = max(current.timestamp, incoming.timestamp)
            current.version = current.version + 1
            return current
        except (ValueError, TypeError):
            logger.warning(f"Counter strategy failed for non-numeric values, falling back to LWW")
            return self._resolve_lww(current, incoming)


class MemoryCompressor:
    """Memory value compression for optimization."""

    def compress(self, value: Any, threshold_bytes: int = 1024) -> Tuple[Any, bool]:
        """
        Compress value if it exceeds threshold.

        Args:
            value: Value to potentially compress
            threshold_bytes: Size threshold for compression

        Returns:
            Tuple of (compressed_value, was_compressed)
        """
        # Serialize to JSON first
        json_str = json.dumps(value, separators=(',', ':'))
        size_bytes = len(json_str.encode('utf-8'))

        if size_bytes < threshold_bytes:
            return value, False

        # Compress with gzip
        compressed = gzip.compress(json_str.encode('utf-8'))

        # Only keep compression if it actually reduces size
        if len(compressed) < size_bytes:
            # Store as base64 for JSON compatibility
            import base64
            return base64.b64encode(compressed).decode('ascii'), True

        return value, False

    def decompress(self, value: Any, compressed: bool) -> Any:
        """
        Decompress value if it was compressed.

        Args:
            value: Potentially compressed value
            compressed: Whether value is compressed

        Returns:
            Decompressed value
        """
        if not compressed:
            return value

        try:
            import base64
            compressed_bytes = base64.b64decode(value.encode('ascii'))
            json_str = gzip.decompress(compressed_bytes).decode('utf-8')
            return json.loads(json_str)
        except Exception as e:
            logger.error(f"Decompression failed: {e}")
            return value


class MemoryGarbageCollector:
    """Garbage collection for expired memory entries."""

    def __init__(self, storage_path: Path, db_path: Path):
        """
        Initialize garbage collector.

        Args:
            storage_path: Path to memory storage directory
            db_path: Path to SQLite metadata database
        """
        self.storage_path = storage_path
        self.db_path = db_path
        self._ensure_db()

    def _ensure_db(self) -> None:
        """Ensure SQLite database and schema exist."""
        self.db_path.parent.mkdir(parents=True, exist_ok=True)

        conn = sqlite3.connect(str(self.db_path))
        cursor = conn.cursor()

        cursor.execute("""
            CREATE TABLE IF NOT EXISTS memory_metadata (
                key TEXT PRIMARY KEY,
                namespace TEXT NOT NULL,
                timestamp REAL NOT NULL,
                version INTEGER DEFAULT 1,
                compressed INTEGER DEFAULT 0,
                file_path TEXT NOT NULL,
                created_at REAL DEFAULT (strftime('%s', 'now')),
                accessed_at REAL DEFAULT (strftime('%s', 'now'))
            )
        """)

        cursor.execute("""
            CREATE INDEX IF NOT EXISTS idx_namespace
            ON memory_metadata(namespace)
        """)

        cursor.execute("""
            CREATE INDEX IF NOT EXISTS idx_timestamp
            ON memory_metadata(timestamp)
        """)

        conn.commit()
        conn.close()

    def track_entry(self, entry: MemoryEntry, file_path: Path) -> None:
        """Track memory entry in metadata database."""
        conn = sqlite3.connect(str(self.db_path))
        cursor = conn.cursor()

        cursor.execute("""
            INSERT OR REPLACE INTO memory_metadata
            (key, namespace, timestamp, version, compressed, file_path, accessed_at)
            VALUES (?, ?, ?, ?, ?, ?, ?)
        """, (
            entry.key,
            entry.namespace,
            entry.timestamp,
            entry.version,
            1 if entry.compressed else 0,
            str(file_path),
            time.time()
        ))

        conn.commit()
        conn.close()

    def update_access_time(self, key: str) -> None:
        """Update last access time for lazy cleanup."""
        conn = sqlite3.connect(str(self.db_path))
        cursor = conn.cursor()

        cursor.execute("""
            UPDATE memory_metadata
            SET accessed_at = ?
            WHERE key = ?
        """, (time.time(), key))

        conn.commit()
        conn.close()

    def collect_expired(self, policies: List[RetentionPolicy]) -> int:
        """
        Collect and delete expired entries based on policies.

        Args:
            policies: List of retention policies

        Returns:
            Number of entries deleted
        """
        conn = sqlite3.connect(str(self.db_path))
        cursor = conn.cursor()

        deleted_count = 0

        for policy in policies:
            if policy.ttl_seconds == 0:  # Permanent
                continue

            # Find expired entries for this namespace
            namespace_pattern = policy.namespace.rstrip("*") + "%"
            cutoff_time = time.time() - policy.ttl_seconds

            cursor.execute("""
                SELECT key, file_path
                FROM memory_metadata
                WHERE namespace LIKE ? AND timestamp < ?
            """, (namespace_pattern, cutoff_time))

            expired_entries = cursor.fetchall()

            for key, file_path in expired_entries:
                try:
                    # Delete file
                    file_path_obj = Path(file_path)
                    if file_path_obj.exists():
                        file_path_obj.unlink()

                    # Delete metadata
                    cursor.execute("DELETE FROM memory_metadata WHERE key = ?", (key,))
                    deleted_count += 1

                    logger.debug(f"Deleted expired entry: {key}")
                except Exception as e:
                    logger.error(f"Failed to delete entry {key}: {e}")

        conn.commit()
        conn.close()

        return deleted_count

    def collect_all(self) -> int:
        """Delete all entries (for testing/reset)."""
        conn = sqlite3.connect(str(self.db_path))
        cursor = conn.cursor()

        cursor.execute("SELECT key, file_path FROM memory_metadata")
        all_entries = cursor.fetchall()

        deleted_count = 0
        for key, file_path in all_entries:
            try:
                Path(file_path).unlink(missing_ok=True)
                cursor.execute("DELETE FROM memory_metadata WHERE key = ?", (key,))
                deleted_count += 1
            except Exception as e:
                logger.error(f"Failed to delete entry {key}: {e}")

        conn.commit()
        conn.close()

        return deleted_count

    def get_stats(self) -> Dict[str, Any]:
        """Get memory statistics."""
        conn = sqlite3.connect(str(self.db_path))
        cursor = conn.cursor()

        cursor.execute("SELECT namespace, COUNT(*) FROM memory_metadata GROUP BY namespace")
        namespace_counts = dict(cursor.fetchall())

        cursor.execute("SELECT COUNT(*) FROM memory_metadata")
        total_count = cursor.fetchone()[0]

        cursor.execute("SELECT SUM(compressed) FROM memory_metadata")
        compressed_count = cursor.fetchone()[0] or 0

        conn.close()

        return {
            "total_entries": total_count,
            "compressed_entries": compressed_count,
            "namespace_counts": namespace_counts
        }


class MemoryLifecycleManager:
    """
    Central manager for memory lifecycle operations.

    Handles retention policies, garbage collection, compression,
    and CRDT conflict resolution for distributed agent memory.
    """

    def __init__(
        self,
        memory_dir: Optional[Path] = None,
        policy_file: Optional[Path] = None
    ):
        """
        Initialize memory lifecycle manager.

        Args:
            memory_dir: Memory storage directory (default: .claude-flow/memory/)
            policy_file: Retention policies YAML file
        """
        # Default paths
        if memory_dir is None:
            memory_dir = Path(".claude-flow/memory")
        if policy_file is None:
            policy_file = Path(".claude/memory-retention-policies.yaml")

        self.memory_dir = memory_dir
        self.policy_file = policy_file
        self.db_path = memory_dir.parent / "memory.db"

        # Ensure directories exist
        self.memory_dir.mkdir(parents=True, exist_ok=True)

        # Initialize components
        self.compressor = MemoryCompressor()
        self.resolver = CRDTResolver()
        self.gc = MemoryGarbageCollector(self.memory_dir, self.db_path)

        # Load policies
        self.policies = self._load_policies()

        logger.info(f"MemoryLifecycleManager initialized with {len(self.policies)} policies")

    def _load_policies(self) -> List[RetentionPolicy]:
        """Load retention policies from YAML file."""
        if not self.policy_file.exists():
            logger.warning(f"Policy file not found: {self.policy_file}, using defaults")
            return self._default_policies()

        try:
            with open(self.policy_file, 'r') as f:
                config = yaml.safe_load(f)

            policies = []
            for ns_config in config.get("namespaces", []):
                policy = RetentionPolicy(
                    namespace=ns_config["namespace"],
                    ttl_seconds=ns_config["ttl_seconds"],
                    cleanup_strategy=CleanupStrategy(ns_config.get("cleanup_strategy", "scheduled")),
                    conflict_strategy=ConflictStrategy(ns_config.get("conflict_strategy", "lww")),
                    compression_enabled=ns_config.get("compression_enabled", True),
                    compression_threshold_bytes=ns_config.get("compression_threshold_bytes", 1024)
                )
                policies.append(policy)

            return policies
        except Exception as e:
            logger.error(f"Failed to load policies: {e}, using defaults")
            return self._default_policies()

    def _default_policies(self) -> List[RetentionPolicy]:
        """Create default retention policies."""
        return [
            RetentionPolicy(
                namespace="coordination/*",
                ttl_seconds=3600,  # 1 hour
                cleanup_strategy=CleanupStrategy.LAZY,
                conflict_strategy=ConflictStrategy.LAST_WRITE_WINS
            ),
            RetentionPolicy(
                namespace="session/*",
                ttl_seconds=3600,  # 1 hour
                cleanup_strategy=CleanupStrategy.LAZY,
                conflict_strategy=ConflictStrategy.LAST_WRITE_WINS
            ),
            RetentionPolicy(
                namespace="swarm/shared/*",
                ttl_seconds=86400,  # 24 hours
                cleanup_strategy=CleanupStrategy.SCHEDULED,
                conflict_strategy=ConflictStrategy.MULTI_VALUE
            ),
            RetentionPolicy(
                namespace="swarm/agent/state/*",
                ttl_seconds=604800,  # 7 days
                cleanup_strategy=CleanupStrategy.SCHEDULED,
                conflict_strategy=ConflictStrategy.LAST_WRITE_WINS
            ),
            RetentionPolicy(
                namespace="knowledge/*",
                ttl_seconds=0,  # Permanent
                cleanup_strategy=CleanupStrategy.MANUAL,
                conflict_strategy=ConflictStrategy.MULTI_VALUE
            )
        ]

    def _get_policy(self, key: str) -> Optional[RetentionPolicy]:
        """Get retention policy for a memory key."""
        for policy in self.policies:
            if policy.matches_namespace(key):
                return policy
        return None

    def _get_namespace(self, key: str) -> str:
        """Extract namespace from memory key."""
        parts = key.split("/")
        if len(parts) >= 2:
            return f"{parts[0]}/{parts[1]}"
        return parts[0] if parts else "default"

    def _get_file_path(self, key: str) -> Path:
        """Get file path for memory key."""
        # Use key as filename with .json extension
        safe_key = key.replace("/", "_").replace(":", "_")
        return self.memory_dir / f"{safe_key}.json"

    def store(
        self,
        key: str,
        value: Any,
        namespace: Optional[str] = None,
        timestamp: Optional[float] = None
    ) -> bool:
        """
        Store value in memory with lifecycle management.

        Args:
            key: Memory key
            value: Value to store
            namespace: Optional explicit namespace (auto-detected if None)
            timestamp: Optional explicit timestamp (current time if None)

        Returns:
            True if stored successfully
        """
        if namespace is None:
            namespace = self._get_namespace(key)

        if timestamp is None:
            timestamp = time.time()

        # Get policy
        policy = self._get_policy(key)
        if policy is None:
            logger.warning(f"No policy found for key: {key}, using defaults")
            policy = self.policies[0]  # Use first policy as default

        # Create entry
        entry = MemoryEntry(
            key=key,
            value=value,
            timestamp=timestamp,
            namespace=namespace
        )

        # Compress if needed
        if policy.compression_enabled:
            compressed_value, was_compressed = self.compressor.compress(
                value,
                policy.compression_threshold_bytes
            )
            if was_compressed:
                entry.value = compressed_value
                entry.compressed = True

        # Check for conflicts
        file_path = self._get_file_path(key)
        if file_path.exists():
            try:
                with open(file_path, 'r') as f:
                    current_data = json.load(f)
                current_entry = MemoryEntry.from_dict(current_data)

                # Resolve conflict
                entry = self.resolver.resolve(
                    policy.conflict_strategy,
                    current_entry,
                    entry
                )
            except Exception as e:
                logger.error(f"Failed to resolve conflict for {key}: {e}")

        # Store to file
        try:
            with open(file_path, 'w') as f:
                json.dump(entry.to_dict(), f, indent=2)

            # Track in metadata
            self.gc.track_entry(entry, file_path)

            logger.debug(f"Stored entry: {key} (compressed: {entry.compressed})")
            return True
        except Exception as e:
            logger.error(f"Failed to store entry {key}: {e}")
            return False

    def retrieve(self, key: str) -> Optional[Any]:
        """
        Retrieve value from memory.

        Args:
            key: Memory key

        Returns:
            Retrieved value or None if not found/expired
        """
        file_path = self._get_file_path(key)

        if not file_path.exists():
            return None

        try:
            with open(file_path, 'r') as f:
                data = json.load(f)

            entry = MemoryEntry.from_dict(data)

            # Check if expired (lazy cleanup)
            policy = self._get_policy(key)
            if policy and policy.cleanup_strategy == CleanupStrategy.LAZY:
                if policy.is_expired(entry.timestamp):
                    logger.debug(f"Entry expired on access: {key}")
                    self.delete(key)
                    return None

            # Update access time
            self.gc.update_access_time(key)

            # Decompress if needed
            value = self.compressor.decompress(entry.value, entry.compressed)

            return value
        except Exception as e:
            logger.error(f"Failed to retrieve entry {key}: {e}")
            return None

    def delete(self, key: str) -> bool:
        """
        Delete entry from memory.

        Args:
            key: Memory key

        Returns:
            True if deleted successfully
        """
        file_path = self._get_file_path(key)

        try:
            if file_path.exists():
                file_path.unlink()

            # Delete from metadata
            conn = sqlite3.connect(str(self.db_path))
            cursor = conn.cursor()
            cursor.execute("DELETE FROM memory_metadata WHERE key = ?", (key,))
            conn.commit()
            conn.close()

            logger.debug(f"Deleted entry: {key}")
            return True
        except Exception as e:
            logger.error(f"Failed to delete entry {key}: {e}")
            return False

    def cleanup(self, force: bool = False) -> Dict[str, Any]:
        """
        Run garbage collection.

        Args:
            force: Force cleanup regardless of strategy

        Returns:
            Cleanup statistics
        """
        logger.info("Starting memory cleanup")
        start_time = time.time()

        # Get stats before
        stats_before = self.gc.get_stats()

        # Collect expired entries
        deleted_count = self.gc.collect_expired(self.policies)

        # Get stats after
        stats_after = self.gc.get_stats()

        elapsed_time = time.time() - start_time

        stats = {
            "deleted_count": deleted_count,
            "entries_before": stats_before["total_entries"],
            "entries_after": stats_after["total_entries"],
            "elapsed_seconds": elapsed_time,
            "timestamp": time.time()
        }

        logger.info(f"Cleanup completed: deleted {deleted_count} entries in {elapsed_time:.2f}s")
        return stats

    def get_stats(self) -> Dict[str, Any]:
        """Get comprehensive memory statistics."""
        gc_stats = self.gc.get_stats()

        return {
            **gc_stats,
            "policies_count": len(self.policies),
            "storage_path": str(self.memory_dir),
            "db_path": str(self.db_path)
        }

    def reset(self) -> bool:
        """Reset all memory (for testing)."""
        logger.warning("Resetting all memory")
        deleted_count = self.gc.collect_all()
        logger.info(f"Reset complete: deleted {deleted_count} entries")
        return True
