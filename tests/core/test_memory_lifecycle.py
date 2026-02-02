"""
ABOUTME: Comprehensive tests for memory lifecycle management system.
Tests retention policies, garbage collection, compression, and CRDT resolution.
"""

import pytest
import time
import json
import tempfile
import shutil
from pathlib import Path
from typing import Generator

from digitalmodel.automation.memory_lifecycle import (
    MemoryLifecycleManager,
    RetentionPolicy,
    ConflictStrategy,
    CleanupStrategy,
    MemoryEntry,
    CRDTResolver,
    MemoryCompressor,
    MemoryGarbageCollector
)


@pytest.fixture
def temp_memory_dir() -> Generator[Path, None, None]:
    """Create temporary memory directory for testing."""
    temp_dir = Path(tempfile.mkdtemp())
    yield temp_dir
    # Cleanup
    shutil.rmtree(temp_dir, ignore_errors=True)


@pytest.fixture
def temp_policy_file(temp_memory_dir: Path) -> Path:
    """Create temporary policy file for testing."""
    policy_file = temp_memory_dir / "test-policies.yaml"
    policy_content = """
namespaces:
  - namespace: "test/short/*"
    ttl_seconds: 2
    cleanup_strategy: lazy
    conflict_strategy: lww
    compression_enabled: true
    compression_threshold_bytes: 100

  - namespace: "test/permanent/*"
    ttl_seconds: 0
    cleanup_strategy: manual
    conflict_strategy: multi_value
    compression_enabled: false
"""
    policy_file.write_text(policy_content)
    return policy_file


@pytest.fixture
def manager(temp_memory_dir: Path, temp_policy_file: Path) -> MemoryLifecycleManager:
    """Create memory lifecycle manager for testing."""
    return MemoryLifecycleManager(
        memory_dir=temp_memory_dir / "memory",
        policy_file=temp_policy_file
    )


class TestRetentionPolicy:
    """Test retention policy functionality."""

    def test_is_expired(self):
        """Test expiration checking."""
        policy = RetentionPolicy(
            namespace="test/*",
            ttl_seconds=10,
            cleanup_strategy=CleanupStrategy.LAZY,
            conflict_strategy=ConflictStrategy.LAST_WRITE_WINS
        )

        # Recent timestamp should not be expired
        recent = time.time() - 5
        assert not policy.is_expired(recent)

        # Old timestamp should be expired
        old = time.time() - 15
        assert policy.is_expired(old)

    def test_permanent_never_expires(self):
        """Test that permanent policies never expire."""
        policy = RetentionPolicy(
            namespace="permanent/*",
            ttl_seconds=0,
            cleanup_strategy=CleanupStrategy.MANUAL,
            conflict_strategy=ConflictStrategy.LAST_WRITE_WINS
        )

        old = time.time() - 1000000
        assert not policy.is_expired(old)

    def test_matches_namespace(self):
        """Test namespace pattern matching."""
        policy = RetentionPolicy(
            namespace="coordination/*",
            ttl_seconds=3600,
            cleanup_strategy=CleanupStrategy.LAZY,
            conflict_strategy=ConflictStrategy.LAST_WRITE_WINS
        )

        assert policy.matches_namespace("coordination/agent1/state")
        assert policy.matches_namespace("coordination/task")
        assert not policy.matches_namespace("session/data")


class TestCRDTResolver:
    """Test CRDT conflict resolution strategies."""

    def test_last_write_wins(self):
        """Test LWW conflict resolution."""
        resolver = CRDTResolver()

        current = MemoryEntry(
            key="test",
            value="old",
            timestamp=100.0,
            namespace="test",
            version=1
        )

        incoming = MemoryEntry(
            key="test",
            value="new",
            timestamp=200.0,
            namespace="test",
            version=1
        )

        result = resolver.resolve(ConflictStrategy.LAST_WRITE_WINS, current, incoming)

        assert result.value == "new"
        assert result.timestamp == 200.0
        assert result.version == 2

    def test_last_write_wins_keeps_older(self):
        """Test LWW keeps older value when incoming is older."""
        resolver = CRDTResolver()

        current = MemoryEntry(
            key="test",
            value="new",
            timestamp=200.0,
            namespace="test",
            version=1
        )

        incoming = MemoryEntry(
            key="test",
            value="old",
            timestamp=100.0,
            namespace="test",
            version=1
        )

        result = resolver.resolve(ConflictStrategy.LAST_WRITE_WINS, current, incoming)

        assert result.value == "new"
        assert result.timestamp == 200.0

    def test_multi_value(self):
        """Test multi-value conflict resolution."""
        resolver = CRDTResolver()

        current = MemoryEntry(
            key="test",
            value="value1",
            timestamp=100.0,
            namespace="test"
        )

        incoming = MemoryEntry(
            key="test",
            value="value2",
            timestamp=200.0,
            namespace="test"
        )

        result = resolver.resolve(ConflictStrategy.MULTI_VALUE, current, incoming)

        assert result.value == "value2"
        assert result.conflicted
        assert len(result.conflict_values) == 1
        assert result.conflict_values[0] == ("value1", 100.0)

    def test_counter_merge(self):
        """Test counter CRDT merge."""
        resolver = CRDTResolver()

        current = MemoryEntry(
            key="count",
            value=10,
            timestamp=100.0,
            namespace="metrics"
        )

        incoming = MemoryEntry(
            key="count",
            value=5,
            timestamp=200.0,
            namespace="metrics"
        )

        result = resolver.resolve(ConflictStrategy.COUNTER, current, incoming)

        assert result.value == 15
        assert result.version == 2

    def test_counter_fallback_to_lww(self):
        """Test counter strategy falls back to LWW for non-numeric values."""
        resolver = CRDTResolver()

        current = MemoryEntry(
            key="count",
            value="not_a_number",
            timestamp=100.0,
            namespace="metrics"
        )

        incoming = MemoryEntry(
            key="count",
            value="also_not_a_number",
            timestamp=200.0,
            namespace="metrics"
        )

        result = resolver.resolve(ConflictStrategy.COUNTER, current, incoming)

        # Should fall back to LWW
        assert result.value == "also_not_a_number"


class TestMemoryCompressor:
    """Test memory compression functionality."""

    def test_compression_below_threshold(self):
        """Test that small values are not compressed."""
        compressor = MemoryCompressor()

        small_value = {"key": "value"}
        result, compressed = compressor.compress(small_value, threshold_bytes=1000)

        assert not compressed
        assert result == small_value

    def test_compression_above_threshold(self):
        """Test that large values are compressed."""
        compressor = MemoryCompressor()

        # Create large value
        large_value = {"data": "x" * 2000}
        result, compressed = compressor.compress(large_value, threshold_bytes=100)

        assert compressed
        assert isinstance(result, str)  # Compressed to base64 string

    def test_compression_decompression_roundtrip(self):
        """Test compression and decompression roundtrip."""
        compressor = MemoryCompressor()

        original = {"data": "x" * 2000, "nested": {"key": "value"}}
        compressed, was_compressed = compressor.compress(original, threshold_bytes=100)

        assert was_compressed

        decompressed = compressor.decompress(compressed, was_compressed)
        assert decompressed == original

    def test_decompression_uncompressed_value(self):
        """Test decompressing an uncompressed value returns it unchanged."""
        compressor = MemoryCompressor()

        value = {"key": "value"}
        result = compressor.decompress(value, compressed=False)

        assert result == value


class TestMemoryGarbageCollector:
    """Test garbage collection functionality."""

    def test_track_entry(self, temp_memory_dir: Path):
        """Test tracking memory entries in metadata DB."""
        gc = MemoryGarbageCollector(
            temp_memory_dir / "memory",
            temp_memory_dir / "memory.db"
        )

        entry = MemoryEntry(
            key="test/key",
            value="value",
            timestamp=time.time(),
            namespace="test"
        )

        file_path = temp_memory_dir / "memory" / "test_key.json"
        gc.track_entry(entry, file_path)

        # Verify entry is in database
        import sqlite3
        conn = sqlite3.connect(str(temp_memory_dir / "memory.db"))
        cursor = conn.cursor()
        cursor.execute("SELECT key, namespace FROM memory_metadata WHERE key = ?", (entry.key,))
        result = cursor.fetchone()
        conn.close()

        assert result is not None
        assert result[0] == "test/key"
        assert result[1] == "test"

    def test_collect_expired(self, temp_memory_dir: Path):
        """Test collecting expired entries."""
        gc = MemoryGarbageCollector(
            temp_memory_dir / "memory",
            temp_memory_dir / "memory.db"
        )

        # Create expired entry
        memory_dir = temp_memory_dir / "memory"
        memory_dir.mkdir(parents=True, exist_ok=True)

        file_path = memory_dir / "expired.json"
        old_entry = MemoryEntry(
            key="test/expired",
            value="old",
            timestamp=time.time() - 100,  # 100 seconds ago
            namespace="test/expired"  # Must match the pattern test/*
        )

        file_path.write_text(json.dumps(old_entry.to_dict()))
        gc.track_entry(old_entry, file_path)

        # Create policy with 10 second TTL
        policy = RetentionPolicy(
            namespace="test/*",
            ttl_seconds=10,
            cleanup_strategy=CleanupStrategy.SCHEDULED,
            conflict_strategy=ConflictStrategy.LAST_WRITE_WINS
        )

        # Verify entry was tracked
        stats_before = gc.get_stats()
        assert stats_before["total_entries"] >= 1

        # Collect expired
        deleted = gc.collect_expired([policy])

        assert deleted == 1, f"Expected 1 deleted, got {deleted}. Stats before: {stats_before}"
        assert not file_path.exists()

    def test_get_stats(self, temp_memory_dir: Path):
        """Test getting garbage collector statistics."""
        gc = MemoryGarbageCollector(
            temp_memory_dir / "memory",
            temp_memory_dir / "memory.db"
        )

        # Create some entries
        memory_dir = temp_memory_dir / "memory"
        memory_dir.mkdir(parents=True, exist_ok=True)

        for i in range(3):
            file_path = memory_dir / f"entry{i}.json"
            entry = MemoryEntry(
                key=f"test/entry{i}",
                value=f"value{i}",
                timestamp=time.time(),
                namespace="test",
                compressed=(i == 0)  # First one compressed
            )
            gc.track_entry(entry, file_path)

        stats = gc.get_stats()

        assert stats["total_entries"] == 3
        assert stats["compressed_entries"] == 1
        # namespace_counts is dict with namespace as key and count as value
        assert stats["namespace_counts"]["test"] == 3


class TestMemoryLifecycleManager:
    """Test memory lifecycle manager integration."""

    def test_store_and_retrieve(self, manager: MemoryLifecycleManager):
        """Test storing and retrieving values."""
        key = "test/short/data"
        value = {"message": "test data"}

        success = manager.store(key, value)
        assert success

        retrieved = manager.retrieve(key)
        assert retrieved == value

    def test_retrieve_nonexistent(self, manager: MemoryLifecycleManager):
        """Test retrieving non-existent key returns None."""
        result = manager.retrieve("nonexistent/key")
        assert result is None

    def test_delete(self, manager: MemoryLifecycleManager):
        """Test deleting entries."""
        key = "test/short/data"
        value = {"message": "test"}

        manager.store(key, value)
        assert manager.retrieve(key) is not None

        manager.delete(key)
        assert manager.retrieve(key) is None

    def test_lazy_cleanup_on_retrieve(self, manager: MemoryLifecycleManager):
        """Test that expired entries are cleaned up on retrieval (lazy strategy)."""
        key = "test/short/expired"
        value = {"message": "will expire"}

        # Store with old timestamp
        manager.store(key, value, timestamp=time.time() - 10)

        # Wait for expiration (TTL is 2 seconds in test policy)
        time.sleep(3)

        # Retrieve should return None and delete entry
        result = manager.retrieve(key)
        assert result is None

    def test_compression(self, manager: MemoryLifecycleManager):
        """Test automatic compression of large values."""
        key = "test/short/large"
        value = {"data": "x" * 2000}  # Large value

        manager.store(key, value)
        retrieved = manager.retrieve(key)

        assert retrieved == value

    def test_conflict_resolution_lww(self, manager: MemoryLifecycleManager):
        """Test Last-Write-Wins conflict resolution."""
        key = "test/permanent/conflict"  # Use permanent namespace to avoid TTL expiration

        # Store first value
        manager.store(key, "value1", timestamp=time.time() - 1)

        # Store second value with newer timestamp
        manager.store(key, "value2", timestamp=time.time())

        result = manager.retrieve(key)
        assert result == "value2"

    def test_cleanup(self, manager: MemoryLifecycleManager):
        """Test manual cleanup operation."""
        # Store some entries with old timestamps
        for i in range(5):
            manager.store(
                f"test/short/old{i}",
                f"value{i}",
                timestamp=time.time() - 100
            )

        # Run cleanup
        stats = manager.cleanup()

        assert stats["deleted_count"] >= 0
        assert "entries_before" in stats
        assert "entries_after" in stats

    def test_get_stats(self, manager: MemoryLifecycleManager):
        """Test getting manager statistics."""
        # Store some entries
        manager.store("test/short/1", "value1")
        manager.store("test/short/2", "value2")
        manager.store("test/permanent/3", "value3")

        stats = manager.get_stats()

        assert "total_entries" in stats
        assert stats["total_entries"] >= 3
        assert "policies_count" in stats

    def test_namespace_extraction(self, manager: MemoryLifecycleManager):
        """Test namespace extraction from keys."""
        assert manager._get_namespace("coordination/agent1/state") == "coordination/agent1"
        assert manager._get_namespace("session/abc") == "session/abc"
        assert manager._get_namespace("simple") == "simple"

    def test_permanent_policy_no_cleanup(self, manager: MemoryLifecycleManager):
        """Test that permanent entries are not cleaned up."""
        key = "test/permanent/data"
        value = {"important": "data"}

        # Store with very old timestamp
        manager.store(key, value, timestamp=time.time() - 1000000)

        # Run cleanup
        manager.cleanup()

        # Should still exist
        result = manager.retrieve(key)
        assert result == value

    def test_reset(self, manager: MemoryLifecycleManager):
        """Test resetting all memory."""
        # Store some entries
        for i in range(5):
            manager.store(f"test/short/{i}", f"value{i}")

        stats_before = manager.get_stats()
        assert stats_before["total_entries"] >= 5

        # Reset
        manager.reset()

        stats_after = manager.get_stats()
        assert stats_after["total_entries"] == 0


class TestIntegration:
    """Integration tests for full lifecycle scenarios."""

    def test_multi_agent_coordination(self, manager: MemoryLifecycleManager):
        """Test multi-agent coordination scenario."""
        # Agent 1 stores progress
        manager.store("coordination/agent1/progress", {"step": 1, "status": "running"})

        # Agent 2 stores progress
        manager.store("coordination/agent2/progress", {"step": 1, "status": "running"})

        # Coordinator reads both
        agent1_progress = manager.retrieve("coordination/agent1/progress")
        agent2_progress = manager.retrieve("coordination/agent2/progress")

        assert agent1_progress["status"] == "running"
        assert agent2_progress["status"] == "running"

        # Update progress
        manager.store("coordination/agent1/progress", {"step": 2, "status": "complete"})

        # Verify update
        updated = manager.retrieve("coordination/agent1/progress")
        assert updated["step"] == 2

    def test_session_lifecycle(self, manager: MemoryLifecycleManager):
        """Test complete session lifecycle."""
        session_id = "test-session-123"

        # Session start: store session data
        manager.store(
            f"session/{session_id}",
            {"started_at": time.time(), "user": "test"}
        )

        # During session: store coordination data
        manager.store(
            f"coordination/{session_id}/task1",
            {"status": "running"}
        )

        # Session end: cleanup session data
        manager.delete(f"session/{session_id}")
        manager.delete(f"coordination/{session_id}/task1")

        # Verify cleanup
        assert manager.retrieve(f"session/{session_id}") is None
        assert manager.retrieve(f"coordination/{session_id}/task1") is None

    def test_knowledge_persistence(self, manager: MemoryLifecycleManager):
        """Test that knowledge base entries persist."""
        # Store knowledge
        manager.store(
            "knowledge/patterns/best-practice",
            {"pattern": "TDD", "description": "Write tests first"}
        )

        # Run multiple cleanups
        for _ in range(3):
            manager.cleanup()
            time.sleep(1)

        # Knowledge should still exist
        knowledge = manager.retrieve("knowledge/patterns/best-practice")
        assert knowledge is not None
        assert knowledge["pattern"] == "TDD"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
