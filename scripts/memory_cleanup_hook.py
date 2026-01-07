#!/usr/bin/env python3
"""
ABOUTME: Claude-flow hook integration script for automatic memory cleanup.
Triggers memory lifecycle cleanup at appropriate hook points.

Usage:
  python scripts/memory_cleanup_hook.py --hook session-end
  python scripts/memory_cleanup_hook.py --hook memory-cleanup --force
"""

import sys
import json
import argparse
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


def hook_session_end(manager: MemoryLifecycleManager, args: argparse.Namespace) -> dict:
    """
    Handle session-end hook: cleanup session-specific memory.

    Args:
        manager: Memory lifecycle manager
        args: Command-line arguments

    Returns:
        Hook result statistics
    """
    logger.info("Session-end hook triggered")

    # Get session ID if provided
    session_id = getattr(args, 'session_id', None)

    if session_id:
        logger.info(f"Cleaning up session: {session_id}")
        # Delete specific session keys
        session_key = f"session/{session_id}"
        manager.delete(session_key)

        # Delete coordination data for this session
        coord_key = f"coordination/{session_id}"
        manager.delete(coord_key)

    # Run general cleanup for session namespace
    stats = manager.cleanup()

    return {
        "hook": "session-end",
        "session_id": session_id,
        "cleanup_stats": stats
    }


def hook_memory_cleanup(manager: MemoryLifecycleManager, args: argparse.Namespace) -> dict:
    """
    Handle memory-cleanup hook: periodic maintenance and garbage collection.

    Args:
        manager: Memory lifecycle manager
        args: Command-line arguments

    Returns:
        Hook result statistics
    """
    logger.info("Memory-cleanup hook triggered")

    force = getattr(args, 'force', False)
    stats = manager.cleanup(force=force)

    # Get overall memory stats
    memory_stats = manager.get_stats()

    return {
        "hook": "memory-cleanup",
        "force": force,
        "cleanup_stats": stats,
        "memory_stats": memory_stats
    }


def hook_pre_task(manager: MemoryLifecycleManager, args: argparse.Namespace) -> dict:
    """
    Handle pre-task hook: cleanup stale coordination data before starting task.

    Args:
        manager: Memory lifecycle manager
        args: Command-line arguments

    Returns:
        Hook result statistics
    """
    logger.info("Pre-task hook triggered")

    # Light cleanup of coordination namespace only
    stats_before = manager.get_stats()

    # Force cleanup of coordination namespace (short TTL)
    stats = manager.cleanup(force=False)

    return {
        "hook": "pre-task",
        "cleanup_stats": stats,
        "entries_before": stats_before["total_entries"]
    }


def main():
    """Main entry point for memory cleanup hook."""
    parser = argparse.ArgumentParser(
        description="Claude-flow memory lifecycle cleanup hook"
    )
    parser.add_argument(
        '--hook',
        type=str,
        required=True,
        choices=['session-end', 'memory-cleanup', 'pre-task'],
        help='Hook type to execute'
    )
    parser.add_argument(
        '--session-id',
        type=str,
        help='Session ID for session-end hook'
    )
    parser.add_argument(
        '--force',
        action='store_true',
        help='Force cleanup regardless of strategy'
    )
    parser.add_argument(
        '--memory-dir',
        type=Path,
        help='Memory storage directory (default: .claude-flow/memory/)'
    )
    parser.add_argument(
        '--policy-file',
        type=Path,
        help='Retention policies file (default: .claude/memory-retention-policies.yaml)'
    )
    parser.add_argument(
        '--output',
        type=str,
        choices=['json', 'text'],
        default='json',
        help='Output format'
    )

    args = parser.parse_args()

    # Initialize manager
    manager_kwargs = {}
    if args.memory_dir:
        manager_kwargs['memory_dir'] = args.memory_dir
    if args.policy_file:
        manager_kwargs['policy_file'] = args.policy_file

    manager = MemoryLifecycleManager(**manager_kwargs)

    # Execute hook
    hook_handlers = {
        'session-end': hook_session_end,
        'memory-cleanup': hook_memory_cleanup,
        'pre-task': hook_pre_task
    }

    handler = hook_handlers[args.hook]
    result = handler(manager, args)

    # Output result
    if args.output == 'json':
        print(json.dumps(result, indent=2))
    else:
        print(f"\n=== Hook: {args.hook} ===")
        if 'cleanup_stats' in result:
            stats = result['cleanup_stats']
            print(f"Deleted: {stats['deleted_count']} entries")
            print(f"Entries before: {stats['entries_before']}")
            print(f"Entries after: {stats['entries_after']}")
            print(f"Elapsed: {stats['elapsed_seconds']:.2f}s")
        if 'memory_stats' in result:
            mem_stats = result['memory_stats']
            print(f"\nTotal entries: {mem_stats['total_entries']}")
            print(f"Compressed entries: {mem_stats['compressed_entries']}")
            print(f"Namespaces: {len(mem_stats['namespace_counts'])}")

    return 0


if __name__ == '__main__':
    sys.exit(main())
