#!/usr/bin/env python3
"""
Run OrcaWave diffraction analysis for L03 Semi-sub multibody analysis

ABOUTME: Multi-body OC4-style semi-submersible split into 4 separate bodies
(centre column + 3 offset columns), demonstrating multi-body hydrodynamic
interaction with Morison drag linearisation across 16 wave headings.

Example features:
- 4 bodies: Centre column (free), 3 offset columns (connected to centre)
- Morison drag linearisation (JONSWAP Hs=7m, Tz=8s, 200 components)
- 16 wave headings (0 to 337.5 deg, full 360), 32 periods (3.75 to 30 s)
- Control surface mesh files (L03 *CS.gdf) for quadratic load calculation
- Direct LU linear solver
- Multi-body coupled added mass/damping matrices (12x12 per body pair)
- Produces: L03 Semi-sub multibody analysis.owr

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
    yml_file = example_dir / "L03 Semi-sub multibody analysis.yml"

    if not yml_file.exists():
        print(f"[ERROR] Input file not found: {yml_file}")
        sys.exit(1)

    print(f"Running OrcaWave diffraction: {yml_file.name}")
    print("  Solver: Direct LU | Drag linearisation: Yes | Bodies: 4 | Headings: 16")

    diff = OrcFxAPI.Diffraction(threadCount=12)
    diff.LoadData(str(yml_file))

    if diff.ValidationErrorText:
        print(f"[ERROR] Validation failed:\n{diff.ValidationErrorText}")
        sys.exit(1)
    if diff.ValidationWarningText:
        print(f"[WARNING] Validation warnings:\n{diff.ValidationWarningText}")

    print("[...] Calculating (4-body coupled problem, drag linearisation included)...")
    start = time.time()
    diff.Calculate()
    elapsed = time.time() - start
    print(f"[OK] Completed in {elapsed:.1f}s")

    owr_file = example_dir / "L03 Semi-sub multibody analysis.owr"
    diff.SaveResults(str(owr_file))
    size_mb = owr_file.stat().st_size / (1024 ** 2)
    print(f"[OK] Saved: {owr_file.name} ({size_mb:.2f} MB)")


if __name__ == "__main__":
    main()
