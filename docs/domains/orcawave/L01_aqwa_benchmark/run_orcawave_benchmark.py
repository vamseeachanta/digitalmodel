#!/usr/bin/env python3
"""
Run OrcaWave Benchmark Analysis with Parallel Validation

ABOUTME: Executes OrcaWave diffraction analysis for AQWA benchmark comparison with configurable threading.

Usage:
    python run_orcawave_benchmark.py --threads 4
    python run_orcawave_benchmark.py --dry-run  # Validation only
    python run_orcawave_benchmark.py --help
"""

import sys
import os
import yaml
import subprocess
import argparse
import time
from pathlib import Path
from datetime import datetime
from concurrent.futures import ThreadPoolExecutor, as_completed

# OrcaWave executable paths (auto-detect)
ORCAWAVE_PATHS = [
    r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcaWave64.exe",
    r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcaWave.exe",
    r"C:\Program Files\Orcina\OrcaFlex\11.6\OrcaWave64.exe",
    r"C:\Program Files\Orcina\OrcaFlex\11.6\OrcaWave.exe",
    r"C:\Program Files\Orcina\OrcaWave\OrcaWave.exe",
    r"C:\Program Files (x86)\Orcina\OrcaWave\OrcaWave.exe",
    r"D:\OrcaWave\OrcaWave.exe",
]


def find_orcawave_exe():
    """Find OrcaWave executable"""
    for path in ORCAWAVE_PATHS:
        if Path(path).exists():
            print(f"[OK] Found OrcaWave: {path}")
            return path

    print("[ERROR] OrcaWave executable not found!")
    print("Searched paths:")
    for path in ORCAWAVE_PATHS:
        print(f"  - {path}")
    return None


def validate_config(config_file):
    """Validate OrcaWave configuration"""
    print(f"\n[1/4] Validating configuration: {config_file.name}")

    if not config_file.exists():
        print(f"[ERROR] Config file not found: {config_file}")
        return False

    # Load YAML (handle multiple documents with --- separator)
    try:
        with open(config_file, 'r') as f:
            # Load all documents and use only dictionary documents
            docs = list(yaml.safe_load_all(f))
            # Filter for dictionary documents only (skip None, strings, etc.)
            dict_docs = [doc for doc in docs if isinstance(doc, dict)]

            if len(dict_docs) == 0:
                raise ValueError("No valid YAML dictionary found in file")
            elif len(dict_docs) == 1:
                config = dict_docs[0]
            else:
                # Merge multiple dictionary documents
                config = {}
                for doc in dict_docs:
                    config.update(doc)
        print(f"[OK] YAML parsed successfully")
    except Exception as e:
        print(f"[ERROR] Failed to parse YAML: {e}")
        return False

    # Check required fields
    required = ['UnitsSystem', 'WaterDepth', 'PeriodOrFrequency', 'WaveHeading', 'Bodies']
    missing = [field for field in required if field not in config]

    if missing:
        print(f"[ERROR] Missing required fields: {missing}")
        return False

    # Check mesh files
    print("\nChecking mesh files...")
    for i, body in enumerate(config.get('Bodies', [])):
        if 'BodyMeshFileName' in body:
            mesh_file = config_file.parent / body['BodyMeshFileName']
            if mesh_file.exists():
                size_mb = mesh_file.stat().st_size / (1024 * 1024)
                print(f"  [OK] Body {i}: {body['BodyMeshFileName']} ({size_mb:.2f} MB)")
            else:
                print(f"  [ERROR] Body {i}: Mesh file not found: {body['BodyMeshFileName']}")
                return False

    # Display analysis setup
    print(f"\nAnalysis Setup:")
    print(f"  - Water depth: {config.get('WaterDepth')} m")
    print(f"  - Periods: {len(config.get('PeriodOrFrequency', []))} values")
    print(f"  - Headings: {len(config.get('WaveHeading', []))} values")
    print(f"  - Bodies: {len(config.get('Bodies', []))}")

    # Estimate computation time
    n_periods = len(config.get('PeriodOrFrequency', []))
    n_headings = len(config.get('WaveHeading', []))
    estimated_min = (n_periods * n_headings * 0.3) / 60  # Rough estimate
    print(f"  - Estimated time: {estimated_min:.1f} minutes")

    return True


def run_parallel_validation(config_file, n_threads=4):
    """Run validation tasks in parallel"""
    print(f"\n[2/4] Running parallel validation ({n_threads} threads)...")

    validation_tasks = {
        'config': lambda: validate_config(config_file),
        'mesh_check': lambda: check_mesh_quality(config_file),
        'memory_estimate': lambda: estimate_resources(config_file),
        'output_setup': lambda: setup_output_directories(config_file),
    }

    results = {}
    with ThreadPoolExecutor(max_workers=n_threads) as executor:
        future_to_task = {
            executor.submit(task_func): task_name
            for task_name, task_func in validation_tasks.items()
        }

        for future in as_completed(future_to_task):
            task_name = future_to_task[future]
            try:
                result = future.result(timeout=30)
                results[task_name] = result
                status = "[OK]" if result else "[FAIL]"
                print(f"  {status} {task_name}")
            except Exception as e:
                results[task_name] = False
                print(f"  [ERROR] {task_name}: {e}")

    return all(results.values())


def check_mesh_quality(config_file):
    """Check mesh file quality"""
    # Simple check - just verify files exist and are reasonable size
    with open(config_file, 'r') as f:
        # Handle multi-document YAML
        docs = list(yaml.safe_load_all(f))
        dict_docs = [doc for doc in docs if isinstance(doc, dict)]
        if len(dict_docs) == 1:
            config = dict_docs[0]
        else:
            config = {}
            for doc in dict_docs:
                config.update(doc)

    for body in config.get('Bodies', []):
        if 'BodyMeshFileName' in body:
            mesh_file = config_file.parent / body['BodyMeshFileName']
            if not mesh_file.exists():
                return False
            # Check size (should be > 1KB, < 1GB)
            size = mesh_file.stat().st_size
            if size < 1024 or size > 1e9:
                return False

    return True


def estimate_resources(config_file):
    """Estimate memory and time requirements"""
    with open(config_file, 'r') as f:
        # Handle multi-document YAML
        docs = list(yaml.safe_load_all(f))
        dict_docs = [doc for doc in docs if isinstance(doc, dict)]
        if len(dict_docs) == 1:
            config = dict_docs[0]
        else:
            config = {}
            for doc in dict_docs:
                config.update(doc)

    n_periods = len(config.get('PeriodOrFrequency', []))
    n_headings = len(config.get('WaveHeading', []))

    # Rough estimates
    memory_mb = 500 + (n_periods * 50) + (n_headings * 30)
    time_min = (n_periods * n_headings * 0.3) / 60

    return True  # Always pass, just informational


def setup_output_directories(config_file):
    """Create output directories"""
    output_dir = config_file.parent / "orcawave_output"
    output_dir.mkdir(exist_ok=True)

    log_dir = config_file.parent / "logs"
    log_dir.mkdir(exist_ok=True)

    return True


def execute_orcawave(config_file, orcawave_exe, dry_run=False):
    """Execute OrcaWave analysis"""
    print(f"\n[3/4] {'DRY RUN - Skipping' if dry_run else 'Executing'} OrcaWave analysis...")

    if dry_run:
        print("  [OK] Dry run mode - validation complete")
        return True

    if not orcawave_exe:
        print("  [ERROR] OrcaWave executable not found - cannot execute")
        return False

    # Create batch file
    batch_file = create_batch_file(config_file, orcawave_exe)
    print(f"  Created batch file: {batch_file.name}")

    # Execute
    print(f"  Running OrcaWave...")
    start_time = time.time()

    try:
        result = subprocess.run(
            [str(batch_file)],
            capture_output=True,
            text=True,
            timeout=3600,  # 1 hour timeout
            cwd=str(config_file.parent)
        )

        elapsed = time.time() - start_time

        # Save log
        log_file = config_file.parent / "logs" / f"orcawave_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"
        with open(log_file, 'w') as f:
            f.write(f"OrcaWave Execution Log\n")
            f.write(f"Started: {datetime.now().isoformat()}\n")
            f.write(f"Config: {config_file}\n")
            f.write(f"Duration: {elapsed:.2f} seconds\n\n")
            f.write("=== STDOUT ===\n")
            f.write(result.stdout or "No output\n")
            f.write("\n=== STDERR ===\n")
            f.write(result.stderr or "No errors\n")

        if result.returncode == 0:
            print(f"  [SUCCESS] OrcaWave completed in {elapsed:.2f} seconds")
            print(f"  Log saved: {log_file}")
            return True
        else:
            print(f"  [ERROR] OrcaWave failed with return code: {result.returncode}")
            print(f"  Check log: {log_file}")
            return False

    except subprocess.TimeoutExpired:
        print("  [ERROR] Analysis timed out (exceeded 1 hour)")
        return False
    except Exception as e:
        print(f"  [ERROR] Execution error: {e}")
        return False


def create_batch_file(config_file, orcawave_exe):
    """Create Windows batch file for OrcaWave execution"""
    batch_content = f"""@echo off
echo ========================================
echo OrcaWave Benchmark Analysis
echo ========================================
echo.
echo Configuration: {config_file}
echo Started: %date% %time%
echo.

"{orcawave_exe}" "{config_file}"

echo.
echo Finished: %date% %time%
echo ========================================
"""

    batch_file = config_file.parent / f"run_orcawave_{datetime.now().strftime('%Y%m%d_%H%M%S')}.bat"
    with open(batch_file, 'w') as f:
        f.write(batch_content)

    return batch_file


def run_comparison(config_file):
    """Run post-analysis comparison"""
    print(f"\n[4/4] Running comparison with AQWA results...")

    # Check if comparison script exists
    comparison_script = config_file.parent / "run_comparison_peaks.py"
    if not comparison_script.exists():
        print("  [WARNING] Comparison script not found")
        return False

    # Run comparison
    try:
        result = subprocess.run(
            [sys.executable, str(comparison_script)],
            capture_output=True,
            text=True,
            timeout=60,
            cwd=str(config_file.parent)
        )

        if result.returncode == 0:
            print("  [SUCCESS] Comparison complete")
            print("  Check: comparison_results/peak_comparison_*.html")
            return True
        else:
            print(f"  [ERROR] Comparison failed")
            return False

    except Exception as e:
        print(f"  [ERROR] Comparison error: {e}")
        return False


def main():
    """Main execution"""
    parser = argparse.ArgumentParser(
        description='Run OrcaWave benchmark analysis with parallel processing',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python run_orcawave_benchmark.py --threads 4
  python run_orcawave_benchmark.py --dry-run
  python run_orcawave_benchmark.py --config custom_config.yml
        """
    )

    parser.add_argument(
        '--config',
        type=str,
        default='orcawave_001_ship_raos_rev2.yml',
        help='OrcaWave configuration file (default: orcawave_001_ship_raos_rev2.yml)'
    )
    parser.add_argument(
        '--threads',
        type=int,
        default=4,
        help='Number of parallel validation threads (default: 4)'
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Validate only without executing OrcaWave'
    )
    parser.add_argument(
        '--skip-comparison',
        action='store_true',
        help='Skip post-analysis comparison with AQWA'
    )

    args = parser.parse_args()

    # Setup
    print("="*60)
    print("OrcaWave Benchmark Analysis")
    print("="*60)
    print(f"Configuration: {args.config}")
    print(f"Threads: {args.threads}")
    print(f"Mode: {'Dry Run' if args.dry_run else 'Full Execution'}")
    print("="*60)

    # Find paths
    config_file = Path(args.config)
    if not config_file.is_absolute():
        config_file = Path(__file__).parent / config_file

    orcawave_exe = find_orcawave_exe()

    # Validate
    if not run_parallel_validation(config_file, args.threads):
        print("\n[ERROR] Validation failed! Fix issues before proceeding.")
        return 1

    # Execute
    if not execute_orcawave(config_file, orcawave_exe, args.dry_run):
        print("\n[ERROR] Execution failed!")
        return 1

    # Compare
    if not args.skip_comparison and not args.dry_run:
        run_comparison(config_file)

    # Summary
    print("\n" + "="*60)
    print("[SUCCESS] COMPLETE")
    print("="*60)

    if not args.dry_run:
        print("\nNext Steps:")
        print("1. Check OrcaWave output files")
        print("2. Review comparison report: comparison_results/peak_comparison_*.html")
        print("3. Verify 5% tolerance on peak RAO values")
    else:
        print("\nDry run complete - ready for full execution")
        print(f"Run: python {Path(__file__).name}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
