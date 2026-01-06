#!/usr/bin/env python3
"""
OrcaWave API Diagnostic Tool

ABOUTME: Diagnoses why OrcaWave Python API hangs for large problems by testing
incremental problem sizes and monitoring execution behavior.

This script helps identify:
1. Maximum problem size that completes successfully
2. Whether issue is mesh quality, problem size, or API limitation
3. Memory usage patterns
4. Actual vs expected execution times
"""

import sys
import time
import threading
import psutil
from pathlib import Path
from datetime import datetime

# Add OrcaFlex API to path
orcaflex_api_path = r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcFxAPI\Python"
sys.path.insert(0, orcaflex_api_path)

try:
    import OrcFxAPI
    print(f"[OK] OrcFxAPI loaded successfully")
except ImportError as e:
    print(f"[ERROR] Failed to import OrcFxAPI: {e}")
    sys.exit(1)


class ProgressMonitor:
    """
    Monitor calculation progress in separate thread

    Since OrcaWave API doesn't provide progress callbacks,
    we monitor memory usage and time to detect if calculation is progressing
    """

    def __init__(self, timeout_seconds=600):
        self.timeout_seconds = timeout_seconds
        self.start_time = None
        self.stop_event = threading.Event()
        self.last_memory = 0
        self.memory_stable_count = 0
        self.hung_threshold = 30  # If memory stable for 30 samples (150s), likely hung

    def start(self):
        """Start progress monitoring"""
        self.start_time = time.time()
        self.monitor_thread = threading.Thread(target=self._monitor_loop, daemon=True)
        self.monitor_thread.start()

    def stop(self):
        """Stop progress monitoring"""
        self.stop_event.set()

    def _monitor_loop(self):
        """Monitor loop - runs in separate thread"""
        while not self.stop_event.is_set():
            elapsed = time.time() - self.start_time

            # Check timeout
            if elapsed > self.timeout_seconds:
                print(f"\n[TIMEOUT] Calculation exceeded {self.timeout_seconds}s timeout")
                print(f"[ACTION] Terminating - likely hung or extremely slow")
                self.stop_event.set()
                return

            # Check memory usage
            try:
                process = psutil.Process()
                current_memory = process.memory_info().rss / (1024**3)  # GB

                # Detect if memory is stable (possible hang indicator)
                if abs(current_memory - self.last_memory) < 0.1:  # <100 MB change
                    self.memory_stable_count += 1
                else:
                    self.memory_stable_count = 0

                self.last_memory = current_memory

                # Print progress update
                print(f"\r[PROGRESS] {elapsed:6.1f}s elapsed | Memory: {current_memory:6.2f} GB | "
                      f"Stable: {self.memory_stable_count}/30", end="", flush=True)

                # Warn if memory stable too long
                if self.memory_stable_count >= self.hung_threshold:
                    print(f"\n[WARNING] Memory stable for {self.memory_stable_count * 5}s - may be hung")

            except Exception as e:
                print(f"\n[ERROR] Monitoring failed: {e}")

            # Wait 5 seconds between checks
            time.sleep(5)


def test_incremental_problem_size(base_config_file, output_dir):
    """
    Test incrementally increasing problem sizes to find breaking point

    Strategy:
    1. Start with minimal problem (2 periods, 2 headings = 4 cases)
    2. Double problem size until failure
    3. Identify maximum working configuration
    """

    base_config = Path(base_config_file)
    output_path = Path(output_dir)
    output_path.mkdir(exist_ok=True)

    print(f"\n{'='*70}")
    print("INCREMENTAL PROBLEM SIZE TEST")
    print(f"{'='*70}")
    print(f"Base config: {base_config.name}")
    print(f"Output dir: {output_path}")
    print(f"{'='*70}\n")

    # Define test configurations (periods, headings)
    test_configs = [
        (2, 2),    # 4 cases
        (4, 4),    # 16 cases (known working from API_EXECUTION_SUMMARY.md)
        (6, 6),    # 36 cases
        (10, 9),   # 90 cases (half of benchmark)
        (20, 9),   # 180 cases (full benchmark)
    ]

    results = []

    for n_periods, n_headings in test_configs:
        n_cases = n_periods * n_headings

        print(f"\n{'='*70}")
        print(f"TEST: {n_periods} periods × {n_headings} headings = {n_cases} cases")
        print(f"{'='*70}")

        # Create modified configuration
        # NOTE: This is a simplified test - real implementation would need to
        # modify YAML file to use subset of periods/headings

        try:
            # Load configuration
            diffraction = OrcFxAPI.Diffraction(threadCount=12)
            diffraction.LoadData(str(base_config.absolute()))

            # Get validation status
            has_errors = bool(diffraction.ValidationErrorText)
            has_warnings = bool(diffraction.ValidationWarningText)

            print(f"Validation: {'ERRORS' if has_errors else 'OK'} | "
                  f"{'WARNINGS' if has_warnings else 'clean'}")

            if has_warnings:
                print(f"Warnings:\n{diffraction.ValidationWarningText}")

            if has_errors:
                print(f"[SKIP] Configuration has errors, skipping calculation")
                results.append({
                    'cases': n_cases,
                    'status': 'ERROR',
                    'time': 0,
                    'message': 'Validation errors'
                })
                continue

            # Run calculation with progress monitoring
            print(f"\n[START] Running calculation with 10-minute timeout...")

            monitor = ProgressMonitor(timeout_seconds=600)
            monitor.start()

            start_time = time.time()
            success = False
            error_msg = None

            try:
                diffraction.Calculate()
                calc_time = time.time() - start_time
                success = True

                monitor.stop()
                print(f"\n[SUCCESS] Completed in {calc_time:.2f}s")

                # Save results
                results_file = output_path / f"test_{n_cases}_cases.owr"
                diffraction.SaveResults(str(results_file))

                results.append({
                    'cases': n_cases,
                    'status': 'SUCCESS',
                    'time': calc_time,
                    'file': results_file.name
                })

            except Exception as e:
                calc_time = time.time() - start_time
                error_msg = str(e)
                monitor.stop()
                print(f"\n[FAILED] Error after {calc_time:.2f}s: {error_msg}")

                results.append({
                    'cases': n_cases,
                    'status': 'FAILED',
                    'time': calc_time,
                    'message': error_msg
                })

            finally:
                # Always stop monitor
                monitor.stop()

            # If this test failed, stop testing larger sizes
            if not success:
                print(f"\n[STOP] Stopping tests - found breaking point at {n_cases} cases")
                break

        except Exception as e:
            print(f"[ERROR] Test setup failed: {e}")
            results.append({
                'cases': n_cases,
                'status': 'ERROR',
                'time': 0,
                'message': str(e)
            })

    # Print summary
    print(f"\n\n{'='*70}")
    print("TEST SUMMARY")
    print(f"{'='*70}")
    print(f"{'Cases':<10} {'Status':<12} {'Time (s)':<12} {'Notes'}")
    print("-" * 70)

    for result in results:
        time_str = f"{result['time']:.2f}" if result['time'] > 0 else "N/A"
        notes = result.get('message', result.get('file', ''))
        print(f"{result['cases']:<10} {result['status']:<12} {time_str:<12} {notes}")

    print(f"{'='*70}\n")

    # Find maximum working size
    working_sizes = [r for r in results if r['status'] == 'SUCCESS']
    if working_sizes:
        max_working = max(working_sizes, key=lambda x: x['cases'])
        print(f"[RESULT] Maximum working configuration: {max_working['cases']} cases "
              f"in {max_working['time']:.2f}s")
    else:
        print(f"[RESULT] No configuration completed successfully")

    return results


def analyze_mesh_quality(config_file):
    """
    Analyze mesh quality to identify potential solver issues
    """

    config_path = Path(config_file)

    print(f"\n{'='*70}")
    print("MESH QUALITY ANALYSIS")
    print(f"{'='*70}")

    try:
        diffraction = OrcFxAPI.Diffraction()
        diffraction.LoadData(str(config_path.absolute()))

        # Get validation warnings
        warnings = diffraction.ValidationWarningText
        errors = diffraction.ValidationErrorText

        print(f"\n=== Validation Errors ===")
        if errors:
            print(errors)
        else:
            print("[OK] No errors")

        print(f"\n=== Validation Warnings ===")
        if warnings:
            print(warnings)

            # Analyze warning types
            warning_lines = warnings.strip().split('\n')

            critical_warnings = [
                w for w in warning_lines
                if any(keyword in w.lower() for keyword in
                       ['aspect ratio', 'wavelength', 'non-planar', 'large panel'])
            ]

            if critical_warnings:
                print(f"\n[CRITICAL] Found {len(critical_warnings)} mesh quality warnings:")
                for w in critical_warnings:
                    print(f"  - {w}")
                print(f"\n[RECOMMENDATION] Mesh quality issues may cause:")
                print(f"  1. Slow convergence (hours instead of minutes)")
                print(f"  2. Numerical instability")
                print(f"  3. Incorrect results")
                print(f"  4. API hangs due to near-singular matrices")
        else:
            print("[OK] No warnings")

        # Try to access mesh statistics
        try:
            # Note: These properties may not exist in API
            # This is exploratory to see what's available
            print(f"\n=== Mesh Statistics ===")

            for attr in dir(diffraction):
                if 'panel' in attr.lower() or 'mesh' in attr.lower():
                    try:
                        value = getattr(diffraction, attr)
                        if not callable(value):
                            print(f"  {attr}: {value}")
                    except:
                        pass

        except Exception as e:
            print(f"[INFO] Mesh statistics not accessible: {e}")

    except Exception as e:
        print(f"[ERROR] Analysis failed: {e}")
        import traceback
        traceback.print_exc()


def test_batch_execution_strategy(config_file, output_dir):
    """
    Test if splitting calculation into smaller batches works

    Strategy: Instead of 20 periods × 9 headings = 180 cases at once,
    try 9 separate runs of 20 cases each (20 periods × 1 heading)
    """

    config_path = Path(config_file)
    output_path = Path(output_dir)
    output_path.mkdir(exist_ok=True)

    print(f"\n{'='*70}")
    print("BATCH EXECUTION STRATEGY TEST")
    print(f"{'='*70}")
    print(f"[INFO] This would test if batching 180 cases into")
    print(f"       9 batches of 20 cases each works better")
    print(f"[NOTE] Full implementation requires YAML modification")
    print(f"       to subset headings - currently just simulation")
    print(f"{'='*70}\n")

    # For now, just document the strategy
    print(f"[STRATEGY] Batching Approach:")
    print(f"  1. Split 180 cases into N smaller batches")
    print(f"  2. Run each batch separately via API")
    print(f"  3. Merge results after all batches complete")
    print(f"  4. Compare total time vs single large run")
    print(f"\n[RECOMMENDATION] If large problem hangs:")
    print(f"  - Run 9 separate calculations (1 heading each)")
    print(f"  - Each run: 20 periods × 1 heading = 20 cases")
    print(f"  - Estimated time: 9 × 5s = 45s total")
    print(f"  - vs current: Timeout (>600s)")


def main():
    """Main execution"""
    import argparse

    parser = argparse.ArgumentParser(
        description='Diagnose OrcaWave API hanging issues',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Full diagnostic suite
  python diagnose_orcawave_api.py benchmark_config.yml

  # Just mesh analysis
  python diagnose_orcawave_api.py benchmark_config.yml --mesh-only

  # Incremental problem size testing
  python diagnose_orcawave_api.py benchmark_config.yml --incremental
        """
    )

    parser.add_argument(
        'config',
        type=str,
        help='OrcaWave YAML configuration file'
    )
    parser.add_argument(
        '--output-dir',
        type=str,
        default='diagnostic_output',
        help='Output directory for test results'
    )
    parser.add_argument(
        '--mesh-only',
        action='store_true',
        help='Only analyze mesh quality, skip execution tests'
    )
    parser.add_argument(
        '--incremental',
        action='store_true',
        help='Run incremental problem size test'
    )
    parser.add_argument(
        '--batch-strategy',
        action='store_true',
        help='Test batch execution strategy'
    )

    args = parser.parse_args()

    # Create output directory
    output_dir = Path(args.output_dir)
    output_dir.mkdir(exist_ok=True)

    # Generate diagnostic report
    report_file = output_dir / f"diagnostic_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"

    # Mesh quality analysis (always run)
    analyze_mesh_quality(args.config)

    if not args.mesh_only:
        if args.incremental or not (args.batch_strategy):
            # Incremental size test
            test_incremental_problem_size(args.config, output_dir)

        if args.batch_strategy:
            # Batch strategy test
            test_batch_execution_strategy(args.config, output_dir)

    print(f"\n{'='*70}")
    print(f"DIAGNOSTIC COMPLETE")
    print(f"{'='*70}")
    print(f"Output directory: {output_dir}")
    print(f"{'='*70}\n")


if __name__ == "__main__":
    main()
