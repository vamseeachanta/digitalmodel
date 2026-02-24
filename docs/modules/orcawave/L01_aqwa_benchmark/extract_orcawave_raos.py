#!/usr/bin/env python3
"""
Extract RAO Data from OrcaWave .owr File

ABOUTME: Reads OrcaWave results file (.owr) using OrcFxAPI and extracts RAO data
for comparison with AQWA results.
"""

import sys
from pathlib import Path
import numpy as np

# Add OrcaFlex API to path
orcaflex_api_path = r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcFxAPI\Python"
sys.path.insert(0, orcaflex_api_path)

try:
    import OrcFxAPI
    print(f"[OK] OrcFxAPI loaded successfully")
except ImportError as e:
    print(f"[ERROR] Failed to import OrcFxAPI: {e}")
    sys.exit(1)


def extract_orcawave_raos(owr_file: str):
    """
    Extract RAO data from OrcaWave .owr file

    Args:
        owr_file: Path to OrcaWave .owr results file

    Returns:
        dict: RAO data with frequencies, headings, and RAO arrays
    """
    owr_path = Path(owr_file)

    if not owr_path.exists():
        print(f"[ERROR] File not found: {owr_path}")
        return None

    print(f"\n{'='*60}")
    print("ORCAWAVE RAO EXTRACTION")
    print(f"{'='*60}")
    print(f"File: {owr_path.name}")
    print(f"Size: {owr_path.stat().st_size / (1024*1024):.2f} MB")
    print(f"{'='*60}\n")

    try:
        # Load OrcaWave results
        print("[1/4] Loading OrcaWave results file...")
        diffraction = OrcFxAPI.Diffraction()
        diffraction.LoadResults(str(owr_path.absolute()))
        print(f"[OK] Results loaded")
        print(f"     State: {diffraction.state}")

        # Extract frequencies and periods
        print("\n[2/4] Extracting frequency data...")
        frequencies = diffraction.frequencies
        periods = diffraction.periods
        print(f"[OK] Extracted {len(frequencies)} frequencies")
        print(f"     Range: {frequencies.min():.4f} - {frequencies.max():.4f} rad/s")
        print(f"     Periods: {periods.min():.2f} - {periods.max():.2f} s")

        # Extract headings
        print("\n[3/4] Extracting heading data...")
        headings = diffraction.headings
        print(f"[OK] Extracted {len(headings)} headings")
        print(f"     Range: {headings.min():.1f}° - {headings.max():.1f}°")

        # Extract RAO data
        print("\n[4/4] Extracting RAO data...")
        load_raos = diffraction.loadRAOsHaskind
        print(f"[OK] Extracted load RAOs")
        print(f"     Shape: {load_raos.shape}")
        print(f"     Format: (n_frequencies, n_headings, n_dofs)")

        # Calculate magnitudes for each DOF
        print(f"\n{'='*60}")
        print("RAO MAGNITUDE STATISTICS")
        print(f"{'='*60}")

        dof_names = ['SURGE', 'SWAY', 'HEAVE', 'ROLL', 'PITCH', 'YAW']

        for i, dof_name in enumerate(dof_names):
            # Extract complex RAOs for this DOF
            rao_complex = load_raos[:, :, i]
            # Calculate magnitude
            rao_mag = np.abs(rao_complex)

            print(f"\n{dof_name}:")
            print(f"  Max magnitude: {rao_mag.max():.6f}")
            print(f"  Min magnitude: {rao_mag.min():.6f}")
            print(f"  Mean magnitude: {rao_mag.mean():.6f}")
            print(f"  Peak location: freq_idx={np.unravel_index(rao_mag.argmax(), rao_mag.shape)[0]}, "
                  f"heading_idx={np.unravel_index(rao_mag.argmax(), rao_mag.shape)[1]}")

        # Extract added mass and damping
        print(f"\n{'='*60}")
        print("ADDITIONAL DATA")
        print(f"{'='*60}")

        added_mass = diffraction.addedMass
        damping = diffraction.damping

        print(f"Added Mass shape: {added_mass.shape}")
        print(f"Damping shape: {damping.shape}")

        # Return data structure
        results = {
            'frequencies': frequencies,
            'periods': periods,
            'headings': headings,
            'load_raos': load_raos,
            'added_mass': added_mass,
            'damping': damping,
            'n_frequencies': len(frequencies),
            'n_headings': len(headings),
            'n_dofs': load_raos.shape[2]
        }

        print(f"\n{'='*60}")
        print("[SUCCESS] RAO extraction complete!")
        print(f"{'='*60}\n")

        return results

    except Exception as e:
        print(f"\n[ERROR] Extraction failed: {e}")
        import traceback
        traceback.print_exc()
        return None


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description='Extract RAO data from OrcaWave .owr file')
    parser.add_argument('owr_file', type=str, help='OrcaWave .owr results file')

    args = parser.parse_args()

    results = extract_orcawave_raos(args.owr_file)

    if results:
        print(f"Extracted data from: {args.owr_file}")
        print(f"  - {results['n_frequencies']} frequencies")
        print(f"  - {results['n_headings']} headings")
        print(f"  - {results['n_dofs']} DOFs")
    else:
        sys.exit(1)
