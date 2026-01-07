#!/usr/bin/env python3
"""
ABOUTME: Example script demonstrating memory lifecycle management system usage.
Shows basic operations, cleanup, and integration patterns.

Usage:
  python scripts/example_memory_lifecycle.py
"""

import sys
import time
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from digitalmodel.modules.automation.memory_lifecycle import MemoryLifecycleManager
import logging

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


def example_basic_operations():
    """Example: Basic store, retrieve, delete operations."""
    print("\n" + "="*60)
    print("EXAMPLE 1: Basic Operations")
    print("="*60)

    manager = MemoryLifecycleManager()

    # Store some values
    print("\n1. Storing values...")
    manager.store("coordination/agent1/progress", {"step": 1, "status": "running"})
    manager.store("coordination/agent2/progress", {"step": 2, "status": "running"})
    manager.store("session/test-session", {"user": "developer", "started_at": time.time()})

    # Retrieve values
    print("\n2. Retrieving values...")
    agent1 = manager.retrieve("coordination/agent1/progress")
    print(f"Agent 1 progress: {agent1}")

    agent2 = manager.retrieve("coordination/agent2/progress")
    print(f"Agent 2 progress: {agent2}")

    # Get statistics
    print("\n3. Statistics...")
    stats = manager.get_stats()
    print(f"Total entries: {stats['total_entries']}")
    print(f"Compressed entries: {stats['compressed_entries']}")
    print(f"Namespace counts: {stats['namespace_counts']}")

    # Delete specific entry
    print("\n4. Deleting entry...")
    manager.delete("coordination/agent1/progress")
    print("Deleted coordination/agent1/progress")

    # Verify deletion
    result = manager.retrieve("coordination/agent1/progress")
    print(f"Retrieve after delete: {result}")


def example_compression():
    """Example: Automatic compression for large values."""
    print("\n" + "="*60)
    print("EXAMPLE 2: Compression")
    print("="*60)

    manager = MemoryLifecycleManager()

    # Store small value (won't be compressed)
    print("\n1. Storing small value...")
    small_data = {"message": "Small data"}
    manager.store("test/small", small_data)

    # Store large value (will be compressed)
    print("\n2. Storing large value...")
    large_data = {
        "logs": ["Entry " + "x" * 100 for i in range(100)],
        "metadata": {"description": "Large log data"}
    }
    manager.store("test/large", large_data)

    # Retrieve both (compression is transparent)
    print("\n3. Retrieving values...")
    small = manager.retrieve("test/small")
    print(f"Small data retrieved: {small == small_data}")

    large = manager.retrieve("test/large")
    print(f"Large data retrieved: {large == large_data}")

    # Check statistics
    stats = manager.get_stats()
    print(f"\n4. Compression stats...")
    print(f"Total entries: {stats['total_entries']}")
    print(f"Compressed entries: {stats['compressed_entries']}")


def example_ttl_and_cleanup():
    """Example: TTL-based expiration and cleanup."""
    print("\n" + "="*60)
    print("EXAMPLE 3: TTL and Cleanup")
    print("="*60)

    manager = MemoryLifecycleManager()

    # Store with old timestamp (will expire)
    print("\n1. Storing old entry...")
    manager.store(
        "coordination/old-task",
        {"status": "completed"},
        timestamp=time.time() - 7200  # 2 hours ago
    )

    # Store recent entry (won't expire)
    print("\n2. Storing recent entry...")
    manager.store(
        "coordination/recent-task",
        {"status": "running"},
        timestamp=time.time()
    )

    print("\n3. Before cleanup...")
    stats_before = manager.get_stats()
    print(f"Entries: {stats_before['total_entries']}")

    # Run cleanup
    print("\n4. Running cleanup...")
    cleanup_stats = manager.cleanup()
    print(f"Deleted: {cleanup_stats['deleted_count']} entries")
    print(f"Entries before: {cleanup_stats['entries_before']}")
    print(f"Entries after: {cleanup_stats['entries_after']}")

    # Verify old entry deleted, recent entry still exists
    print("\n5. Verifying cleanup...")
    old = manager.retrieve("coordination/old-task")
    recent = manager.retrieve("coordination/recent-task")
    print(f"Old entry exists: {old is not None}")
    print(f"Recent entry exists: {recent is not None}")


def example_conflict_resolution():
    """Example: CRDT conflict resolution."""
    print("\n" + "="*60)
    print("EXAMPLE 4: Conflict Resolution")
    print("="*60)

    manager = MemoryLifecycleManager()

    # Last-Write-Wins example
    print("\n1. Last-Write-Wins (LWW)...")
    manager.store("agent/1/progress", "step1", timestamp=100.0)
    manager.store("agent/1/progress", "step2", timestamp=200.0)  # Newer wins
    result = manager.retrieve("agent/1/progress")
    print(f"Result: {result} (expected: step2)")

    # Counter example
    print("\n2. Counter CRDT...")
    manager.store("metrics/tasks/count", 5, timestamp=100.0)
    manager.store("metrics/tasks/count", 3, timestamp=200.0)  # Adds to existing
    result = manager.retrieve("metrics/tasks/count")
    print(f"Result: {result} (expected: 8)")


def example_multi_agent_coordination():
    """Example: Multi-agent coordination workflow."""
    print("\n" + "="*60)
    print("EXAMPLE 5: Multi-Agent Coordination")
    print("="*60)

    manager = MemoryLifecycleManager()

    # Coordinator assigns tasks
    print("\n1. Coordinator assigns tasks...")
    manager.store("swarm/shared/tasks", {
        "task1": {"agent": "agent1", "status": "assigned"},
        "task2": {"agent": "agent2", "status": "assigned"}
    })

    # Agents report progress
    print("\n2. Agents report progress...")
    manager.store("coordination/agent1/status", {
        "task": "task1",
        "progress": 50,
        "status": "running"
    })

    manager.store("coordination/agent2/status", {
        "task": "task2",
        "progress": 75,
        "status": "running"
    })

    # Coordinator monitors
    print("\n3. Coordinator monitors progress...")
    agent1_status = manager.retrieve("coordination/agent1/status")
    agent2_status = manager.retrieve("coordination/agent2/status")

    print(f"Agent 1: Task {agent1_status['task']} - {agent1_status['progress']}%")
    print(f"Agent 2: Task {agent2_status['task']} - {agent2_status['progress']}%")

    # Agent completes task
    print("\n4. Agent completes task...")
    manager.store("coordination/agent1/status", {
        "task": "task1",
        "progress": 100,
        "status": "completed"
    })

    # Coordinator cleanup
    print("\n5. Coordinator cleanup...")
    manager.delete("coordination/agent1/status")
    print("Cleaned up completed task coordination data")


def example_knowledge_persistence():
    """Example: Permanent knowledge base storage."""
    print("\n" + "="*60)
    print("EXAMPLE 6: Knowledge Persistence")
    print("="*60)

    manager = MemoryLifecycleManager()

    # Store knowledge (permanent namespace)
    print("\n1. Storing knowledge...")
    manager.store("knowledge/patterns/rest-api", {
        "pattern": "RESTful API Design",
        "principles": [
            "Stateless",
            "Cacheable",
            "Uniform Interface",
            "Layered System"
        ],
        "example": {
            "endpoint": "/api/v1/resources",
            "methods": ["GET", "POST", "PUT", "DELETE"]
        }
    })

    # Store more knowledge
    manager.store("knowledge/best-practices/testing", {
        "practice": "Test-Driven Development",
        "steps": [
            "Write failing test",
            "Write minimal code to pass",
            "Refactor"
        ]
    })

    # Run cleanup (knowledge should persist)
    print("\n2. Running cleanup...")
    cleanup_stats = manager.cleanup()
    print(f"Deleted: {cleanup_stats['deleted_count']} entries")

    # Verify knowledge still exists
    print("\n3. Verifying knowledge persistence...")
    api_pattern = manager.retrieve("knowledge/patterns/rest-api")
    tdd_practice = manager.retrieve("knowledge/best-practices/testing")

    print(f"API pattern exists: {api_pattern is not None}")
    print(f"TDD practice exists: {tdd_practice is not None}")

    if api_pattern:
        print(f"API pattern: {api_pattern['pattern']}")
        print(f"Principles: {len(api_pattern['principles'])}")


def main():
    """Run all examples."""
    print("\n" + "="*60)
    print("MEMORY LIFECYCLE MANAGEMENT - EXAMPLES")
    print("="*60)

    try:
        example_basic_operations()
        example_compression()
        example_ttl_and_cleanup()
        example_conflict_resolution()
        example_multi_agent_coordination()
        example_knowledge_persistence()

        print("\n" + "="*60)
        print("ALL EXAMPLES COMPLETED SUCCESSFULLY")
        print("="*60)

    except Exception as e:
        logger.error(f"Example failed: {e}", exc_info=True)
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
