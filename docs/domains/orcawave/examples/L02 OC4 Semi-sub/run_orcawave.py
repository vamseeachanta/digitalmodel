#!/usr/bin/env python3
"""
Run OrcaWave diffraction analysis for L02 OC4 Semi-sub

ABOUTME: Single-body OC4 semi-submersible with Morison drag linearisation.
Uses the Iterative AGS linear solver and a JONSWAP wave spectrum for
linearising drag on 19 Morison elements across 4 element types.

Example features:
- Morison drag linearisation (JONSWAP Hs=7m, Tz=8s, 200 components)
- Iterative AGS linear solver (max 30 iterations, tol=1e-6)
- 9 wave headings (0 to 180 deg), 32 periods (3.75 to 30 s)
- xz-plane mesh symmetry, interior surface panels (triangulation method)
- Produces: L02 OC4 Semi-sub.owr

Run with: uv run python run_orcawave.py
"""
import sys
import time
from pathlib import Path

# Add OrcaFlex API to path
orcaflex_api_path = r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcFxAPI\Python"
sys.path.insert(0, orcaflex_api_path)

try:
    import OrcFxAPI
    print("[OK] OrcFxAPI loaded successfully")
except ImportError as e:
    print(f"[ERROR] Failed to import OrcFxAPI: {e}")
    sys.exit(1)


def main():
    example_dir = Path(__file__).parent
    yml_file = example_dir / "L02 OC4 Semi-sub.yml"

    if not yml_file.exists():
        print(f"[ERROR] Input file not found: {yml_file}")
        sys.exit(1)

    print(f"Running OrcaWave diffraction: {yml_file.name}")
    print("  Solver: Iterative AGS | Drag linearisation: Yes | Bodies: 1")

    diff = OrcFxAPI.Diffraction(threadCount=12)
    diff.LoadData(str(yml_file))

    if diff.ValidationErrorText:
        print(f"[ERROR] Validation failed:\n{diff.ValidationErrorText}")
        sys.exit(1)
    if diff.ValidationWarningText:
        print(f"[WARNING] Validation warnings:\n{diff.ValidationWarningText}")

    print("[...] Calculating (drag linearisation iterations included)...")
    start = time.time()
    diff.Calculate()
    elapsed = time.time() - start
    print(f"[OK] Completed in {elapsed:.1f}s")

    owr_file = example_dir / "L02 OC4 Semi-sub.owr"
    diff.SaveResults(str(owr_file))
    size_mb = owr_file.stat().st_size / (1024 ** 2)
    print(f"[OK] Saved: {owr_file.name} ({size_mb:.2f} MB)")


if __name__ == "__main__":
    main()
