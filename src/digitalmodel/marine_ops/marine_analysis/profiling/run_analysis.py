"""
Complete Performance Analysis Workflow

Runs the complete performance analysis workflow:
1. Profile all modules
2. Run benchmark tests
3. Generate charts
4. Create optimization report

Usage:
    python scripts/run_performance_analysis.py
    python scripts/run_performance_analysis.py --quick  # Skip benchmarks
"""

import subprocess
import sys
import json
from pathlib import Path
from datetime import datetime
import argparse


def run_command(cmd: list, description: str):
    """Run a command and print status."""
    print(f"\n{'='*60}")
    print(f"{description}")
    print(f"{'='*60}")
    print(f"Command: {' '.join(cmd)}")
    print()

    result = subprocess.run(cmd, capture_output=True, text=True)

    if result.returncode == 0:
        print(result.stdout)
        print(f"✓ {description} completed successfully")
        return True
    else:
        print(f"✗ {description} failed")
        print(result.stderr)
        return False


def main():
    """Main workflow."""
    parser = argparse.ArgumentParser(description="Run complete performance analysis")
    parser.add_argument('--quick', action='store_true',
                       help='Skip pytest benchmarks (faster)')
    parser.add_argument('--output', default='outputs/profiling',
                       help='Output directory')

    args = parser.parse_args()

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_dir = Path(args.output)
    output_dir.mkdir(parents=True, exist_ok=True)

    print("\n" + "="*60)
    print("MARINE ENGINEERING PERFORMANCE ANALYSIS WORKFLOW")
    print("="*60)
    print(f"Timestamp: {timestamp}")
    print(f"Output Directory: {output_dir}")
    print()

    # Step 1: Run profiling script
    print("\n### STEP 1: Running Performance Profiler ###")
    success1 = run_command(
        [sys.executable, 'scripts/profile_marine_modules.py',
         '--module', 'all', '--output', str(output_dir)],
        "Performance Profiling"
    )

    if not success1:
        print("\n✗ Profiling failed. Stopping workflow.")
        return 1

    # Step 2: Run pytest benchmarks (optional)
    if not args.quick:
        print("\n### STEP 2: Running Pytest Benchmarks ###")
        success2 = run_command(
            [sys.executable, '-m', 'pytest',
             'tests/marine_engineering/test_performance.py',
             '--benchmark-only',
             '--benchmark-json', str(output_dir / 'metrics' / f'benchmark_{timestamp}.json')],
            "Pytest Benchmarks"
        )

        if not success2:
            print("\n⚠ Benchmarks failed, but continuing...")
    else:
        print("\n### STEP 2: Skipping Pytest Benchmarks (--quick mode) ###")

    # Step 3: Find latest metrics file
    metrics_dir = output_dir / 'metrics'
    metrics_files = sorted(metrics_dir.glob('performance_metrics_*.json'))

    if not metrics_files:
        print("\n✗ No metrics files found. Cannot generate charts.")
        return 1

    latest_metrics = metrics_files[-1]
    print(f"\n### STEP 3: Using Metrics File: {latest_metrics} ###")

    # Step 4: Generate charts
    print("\n### STEP 4: Generating Performance Charts ###")
    charts_output = f'docs/charts/phase3/performance/{timestamp}'
    success4 = run_command(
        [sys.executable, 'scripts/generate_performance_charts.py',
         '--input', str(latest_metrics),
         '--output', charts_output],
        "Chart Generation"
    )

    if not success4:
        print("\n⚠ Chart generation failed, but continuing...")

    # Step 5: Generate optimization report
    print("\n### STEP 5: Generating Optimization Report ###")
    success5 = run_command(
        [sys.executable, 'scripts/generate_optimization_report.py',
         '--input', str(latest_metrics),
         '--output', f'docs/performance_optimization_report_{timestamp}.md'],
        "Optimization Report"
    )

    # Summary
    print("\n" + "="*60)
    print("WORKFLOW COMPLETE")
    print("="*60)
    print(f"\nOutputs:")
    print(f"  - Profiling data: {output_dir}")
    print(f"  - Metrics: {latest_metrics}")
    print(f"  - Charts: {charts_output}")
    print(f"  - Report: docs/performance_optimization_report_{timestamp}.md")
    print("\n" + "="*60)

    return 0


if __name__ == '__main__':
    sys.exit(main())
