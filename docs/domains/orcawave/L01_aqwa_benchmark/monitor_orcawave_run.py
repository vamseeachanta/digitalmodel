#!/usr/bin/env python3
"""
ABOUTME: Monitor OrcaWave API execution progress
"""

import time
from pathlib import Path
from datetime import datetime, timedelta
from loguru import logger

def monitor_progress():
    """Monitor OrcaWave execution"""

    benchmark_dir = Path(__file__).parent
    output_file = benchmark_dir / "orcawave_001_ship_raos_rev2_matched.owr"

    logger.info("="*80)
    logger.info("MONITORING ORCAWAVE EXECUTION")
    logger.info("="*80)

    start_time = datetime(2026, 1, 5, 23, 24, 49)  # From log
    logger.info(f"\n⏱️  Started at: {start_time.strftime('%H:%M:%S')}")

    while True:
        now = datetime.now()
        elapsed = (now - start_time).total_seconds() / 60  # minutes

        logger.info(f"\n📊 Status Update - {now.strftime('%H:%M:%S')}")
        logger.info(f"  Elapsed time: {elapsed:.1f} minutes")

        # Check if output file exists
        if output_file.exists():
            file_size = output_file.stat().st_size / (1024 * 1024)  # MB
            logger.success(f"  ✅ Output file exists: {file_size:.2f} MB")
            logger.success(f"  🎉 CALCULATION COMPLETE!")
            logger.info(f"  Total time: {elapsed:.1f} minutes")
            break
        else:
            logger.info(f"  ⏳ Still calculating... (no output file yet)")

            # Estimate progress
            if elapsed < 30:
                progress = (elapsed / 30) * 100
                logger.info(f"  Progress estimate: ~{progress:.0f}% (assuming 30 min)")
            elif elapsed < 60:
                progress = (elapsed / 60) * 100
                logger.info(f"  Progress estimate: ~{progress:.0f}% (assuming 60 min)")
            else:
                logger.warning(f"  ⚠️  Taking longer than expected (>{elapsed:.0f} min)")

        # Wait before next check
        logger.info("  Next check in 2 minutes...")
        time.sleep(120)  # Check every 2 minutes

def main():
    try:
        monitor_progress()
    except KeyboardInterrupt:
        logger.warning("\n⚠️  Monitoring stopped by user")
        logger.info("The OrcaWave calculation continues running in the background")
        return 0

    return 0

if __name__ == "__main__":
    import sys
    sys.exit(main())
