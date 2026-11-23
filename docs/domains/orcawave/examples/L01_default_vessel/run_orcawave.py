#!/usr/bin/env python3
"""
Run OrcaWave diffraction analysis for L01 Default vessel

ABOUTME: Default vessel (ship hull) with both potential and source formulations.
Includes a control surface for irregular frequency removal.

Example features:
- Potential and source formulations
- 9 wave headings (0 to 360 deg), 24 periods (4 to 22 s)
- xz-plane mesh symmetry
- Control surface panels for irregular frequency removal
- Produces: L01 Default vessel.owr

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
    yml_file = example_dir / "L01 Default vessel.yml"

    if not yml_file.exists():
        print(f"[ERROR] Input file not found: {yml_file}")
        sys.exit(1)

    print(f"Running OrcaWave diffraction: {yml_file.name}")
    print("  Solver: Potential+source | Control surface: Yes | Bodies: 1")

    diff = OrcFxAPI.Diffraction(threadCount=12)
    diff.LoadData(str(yml_file))

    if diff.ValidationErrorText:
        print(f"[ERROR] Validation failed:\n{diff.ValidationErrorText}")
        sys.exit(1)
    if diff.ValidationWarningText:
        print(f"[WARNING] Validation warnings:\n{diff.ValidationWarningText}")

    print("[...] Calculating...")
    start = time.time()
    diff.Calculate()
    elapsed = time.time() - start
    print(f"[OK] Completed in {elapsed:.1f}s")

    owr_file = example_dir / "L01 Default vessel.owr"
    diff.SaveResults(str(owr_file))
    size_mb = owr_file.stat().st_size / (1024 ** 2)
    print(f"[OK] Saved: {owr_file.name} ({size_mb:.2f} MB)")


if __name__ == "__main__":
    main()
