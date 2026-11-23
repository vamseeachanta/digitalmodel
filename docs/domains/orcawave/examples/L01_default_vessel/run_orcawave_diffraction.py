#!/usr/bin/env python3
"""
Run OrcaWave Diffraction Analysis via Python API

ABOUTME: Uses OrcFxAPI.Diffraction class to run OrcaWave analysis programmatically,
bypassing GUI requirement for batch/automated execution.

Based on Orcina documentation:
https://www.orcina.com/webhelp/OrcFxAPI/
"""

import sys
import time
from pathlib import Path

# Add OrcaFlex API to path
orcaflex_api_path = r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcFxAPI\Python"
sys.path.insert(0, orcaflex_api_path)

try:
    import OrcFxAPI
    print(f"[OK] OrcFxAPI loaded successfully")
except ImportError as e:
    print(f"[ERROR] Failed to import OrcFxAPI: {e}")
    print(f"[ERROR] Expected path: {orcaflex_api_path}")
    sys.exit(1)


def run_orcawave_diffraction(config_file: str, thread_count: int = 12):
    """
    Run OrcaWave diffraction analysis via API

    Args:
        config_file: Path to OrcaWave YAML configuration file
        thread_count: Number of processing threads (default: 12 for memory safety)

    Returns:
        bool: True if successful, False otherwise
    """
    config_path = Path(config_file)

    if not config_path.exists():
        print(f"[ERROR] Config file not found: {config_path}")
        return False

    # Display header
    print(f"\n{'='*60}")
    print("ORCAWAVE DIFFRACTION ANALYSIS (Python API)")
    print(f"{'='*60}")
    print(f"Config: {config_path.name}")
    print(f"Path: {config_path.absolute()}")
    print(f"Threads: {thread_count}")
    print(f"{'='*60}\n")

    try:
        # Create Diffraction object
        print("[1/6] Creating OrcaWave Diffraction object...")
        diffraction = OrcFxAPI.Diffraction(threadCount=thread_count)
        print(f"[OK] Diffraction object created")
        print(f"     Thread count: {diffraction.threadCount}")
        print(f"     State: {diffraction.state}")

        # Load configuration file
        print(f"\n[2/6] Loading configuration: {config_path.name}")
        start_load = time.time()
        diffraction.LoadData(str(config_path.absolute()))
        load_time = time.time() - start_load
        print(f"[OK] Configuration loaded in {load_time:.2f} seconds")
        print(f"     Latest file: {diffraction.latestFileName}")
        print(f"     Model type: {diffraction.type}")

        # Display validation information
        print(f"\n[3/6] Validating configuration...")
        validation_errors = diffraction.ValidationErrorText
        validation_warnings = diffraction.ValidationWarningText

        if validation_errors:
            print(f"[ERROR] Validation errors found:")
            print(validation_errors)
            return False
        else:
            print(f"[OK] No validation errors")

        if validation_warnings:
            print(f"[WARNING] Validation warnings:")
            print(validation_warnings)
        else:
            print(f"[OK] No validation warnings")

        # Perform calculation
        print(f"\n[4/6] Running diffraction calculation...")
        print(f"     State before: {diffraction.state}")

        start_calc = time.time()
        diffraction.Calculate()
        calc_time = time.time() - start_calc

        print(f"[SUCCESS] Calculation completed in {calc_time:.2f} seconds")
        print(f"     State after: {diffraction.state}")

        # Save results
        results_file = config_path.parent / f"{config_path.stem}.owr"
        print(f"\n[5/6] Saving results to: {results_file.name}")
        diffraction.SaveResults(str(results_file))

        if results_file.exists():
            size_mb = results_file.stat().st_size / (1024 * 1024)
            print(f"[OK] Results saved: {results_file.name} ({size_mb:.2f} MB)")
        else:
            print(f"[ERROR] Results file not created")
            return False

        # Save data file (for verification)
        data_file = config_path.parent / f"{config_path.stem}_data.dat"
        print(f"\n[6/6] Saving data file: {data_file.name}")
        diffraction.SaveData(str(data_file))

        if data_file.exists():
            size_mb = data_file.stat().st_size / (1024 * 1024)
            print(f"[OK] Data saved: {data_file.name} ({size_mb:.2f} MB)")

        # Extract and display summary results
        print(f"\n{'='*60}")
        print("CALCULATION SUMMARY")
        print(f"{'='*60}")

        try:
            import numpy as np

            # Get frequencies/periods
            frequencies = diffraction.frequencies
            periods = diffraction.periods
            headings = diffraction.headings

            print(f"Frequencies: {len(frequencies)} values")
            if len(frequencies) > 0:
                print(f"  Range: {frequencies.min():.4f} - {frequencies.max():.4f} rad/s")

            print(f"Periods: {len(periods)} values")
            if len(periods) > 0:
                print(f"  Range: {periods.min():.2f} - {periods.max():.2f} s")

            print(f"Headings: {len(headings)} values")
            if len(headings) > 0:
                print(f"  Range: {headings.min():.1f}° - {headings.max():.1f}°")

            # Get RAO results
            load_raos = diffraction.loadRAOsHaskind
            if load_raos.size > 0:
                print(f"\nLoad RAOs (Haskind): {load_raos.shape}")
                print(f"  Max magnitude: {np.abs(load_raos).max():.4f}")

            # Get added mass and damping
            added_mass = diffraction.addedMass
            damping = diffraction.damping

            if added_mass.size > 0:
                print(f"\nAdded Mass: {added_mass.shape}")

            if damping.size > 0:
                print(f"Damping: {damping.shape}")

        except Exception as e:
            print(f"[WARNING] Could not extract summary results: {e}")

        print(f"{'='*60}\n")

        # Summary
        total_time = load_time + calc_time
        results_size_mb = results_file.stat().st_size / (1024 * 1024) if results_file.exists() else 0
        data_size_mb = data_file.stat().st_size / (1024 * 1024) if data_file.exists() else 0

        print(f"[SUCCESS] Analysis complete!")
        print(f"  Total time: {total_time:.2f} seconds")
        print(f"  Load time: {load_time:.2f} seconds")
        print(f"  Calc time: {calc_time:.2f} seconds")
        print(f"\nOutput files:")
        print(f"  - {results_file.name} ({results_size_mb:.2f} MB)")
        print(f"  - {data_file.name} ({data_size_mb:.2f} MB)")

        return True

    except Exception as e:
        print(f"\n[ERROR] Execution failed: {e}")
        import traceback
        traceback.print_exc()
        return False


def main():
    """Main execution"""
    import argparse

    parser = argparse.ArgumentParser(
        description='Run OrcaWave diffraction analysis via Python API',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python run_orcawave_diffraction.py L01_license_test.yml
  python run_orcawave_diffraction.py L01_license_test.yml --threads 8
        """
    )

    parser.add_argument(
        'config',
        type=str,
        help='OrcaWave YAML configuration file'
    )
    parser.add_argument(
        '--threads',
        type=int,
        default=12,
        help='Number of processing threads (default: 12 for memory safety)'
    )

    args = parser.parse_args()

    success = run_orcawave_diffraction(args.config, args.threads)

    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
