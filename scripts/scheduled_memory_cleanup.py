#!/usr/bin/env python3
"""
ABOUTME: Scheduled memory cleanup daemon for daily garbage collection.
Runs as a background process to perform periodic memory maintenance.

Usage:
  python scripts/scheduled_memory_cleanup.py --daemon
  python scripts/scheduled_memory_cleanup.py --once
"""

import sys
import time
import json
import argparse
import schedule
from pathlib import Path
from datetime import datetime
import logging

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from digitalmodel.automation.memory_lifecycle import MemoryLifecycleManager

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class MemoryCleanupScheduler:
    """Scheduled memory cleanup daemon."""

    def __init__(
        self,
        manager: MemoryLifecycleManager,
        interval_hours: int = 24,
        time_of_day: str = "02:00"
    ):
        """
        Initialize cleanup scheduler.

        Args:
            manager: Memory lifecycle manager
            interval_hours: Cleanup interval in hours
            time_of_day: Time of day to run (HH:MM format)
        """
        self.manager = manager
        self.interval_hours = interval_hours
        self.time_of_day = time_of_day
        self.last_run = None

    def run_cleanup(self) -> dict:
        """Execute memory cleanup."""
        logger.info("Starting scheduled memory cleanup")
        start_time = time.time()

        try:
            stats = self.manager.cleanup(force=False)
            memory_stats = self.manager.get_stats()

            result = {
                "timestamp": datetime.now().isoformat(),
                "success": True,
                "cleanup_stats": stats,
                "memory_stats": memory_stats,
                "elapsed_seconds": time.time() - start_time
            }

            self.last_run = time.time()

            logger.info(
                f"Cleanup completed: deleted {stats['deleted_count']} entries, "
                f"{memory_stats['total_entries']} entries remaining"
            )

            return result
        except Exception as e:
            logger.error(f"Cleanup failed: {e}")
            return {
                "timestamp": datetime.now().isoformat(),
                "success": False,
                "error": str(e),
                "elapsed_seconds": time.time() - start_time
            }

    def schedule_daily(self):
        """Schedule daily cleanup at specified time."""
        logger.info(f"Scheduling daily cleanup at {self.time_of_day}")
        schedule.every().day.at(self.time_of_day).do(self.run_cleanup)

    def schedule_interval(self):
        """Schedule cleanup at specified interval."""
        logger.info(f"Scheduling cleanup every {self.interval_hours} hours")
        schedule.every(self.interval_hours).hours.do(self.run_cleanup)

    def run_daemon(self):
        """Run as daemon process."""
        logger.info("Starting memory cleanup daemon")

        # Schedule based on configuration
        if self.time_of_day:
            self.schedule_daily()
        else:
            self.schedule_interval()

        # Run immediately on startup
        self.run_cleanup()

        # Main loop
        try:
            while True:
                schedule.run_pending()
                time.sleep(60)  # Check every minute
        except KeyboardInterrupt:
            logger.info("Daemon stopped by user")


def main():
    """Main entry point for scheduled cleanup."""
    parser = argparse.ArgumentParser(
        description="Scheduled memory lifecycle cleanup daemon"
    )
    parser.add_argument(
        '--daemon',
        action='store_true',
        help='Run as daemon (continuous background process)'
    )
    parser.add_argument(
        '--once',
        action='store_true',
        help='Run cleanup once and exit'
    )
    parser.add_argument(
        '--interval-hours',
        type=int,
        default=24,
        help='Cleanup interval in hours (default: 24)'
    )
    parser.add_argument(
        '--time-of-day',
        type=str,
        default="02:00",
        help='Time of day to run (HH:MM format, default: 02:00)'
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

    args = parser.parse_args()

    # Initialize manager
    manager_kwargs = {}
    if args.memory_dir:
        manager_kwargs['memory_dir'] = args.memory_dir
    if args.policy_file:
        manager_kwargs['policy_file'] = args.policy_file

    manager = MemoryLifecycleManager(**manager_kwargs)

    # Create scheduler
    scheduler = MemoryCleanupScheduler(
        manager,
        interval_hours=args.interval_hours,
        time_of_day=args.time_of_day
    )

    # Run mode
    if args.once:
        # Run once and exit
        result = scheduler.run_cleanup()
        print(json.dumps(result, indent=2))
        return 0 if result['success'] else 1
    elif args.daemon:
        # Run as daemon
        scheduler.run_daemon()
        return 0
    else:
        parser.print_help()
        return 1


if __name__ == '__main__':
    sys.exit(main())
