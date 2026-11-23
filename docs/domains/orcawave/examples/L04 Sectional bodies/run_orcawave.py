#!/usr/bin/env python3
"""
Run OrcaWave diffraction analysis for L04 Sectional bodies

ABOUTME: TLP-style structure with 7 sectional bodies (keystone hub + 3 pontoons
+ 3 columns) using potential formulation only (no Morison drag). Demonstrates
sectional hydrostatic stiffness and hierarchical body connectivity.

Example features:
- 7 bodies: Keystone (free), 3 pontoons + 3 columns (connected to keystone/pontoons)
- Potential formulation only (no drag linearisation, no wave spectrum)
- Sectional hydrostatic stiffness method (BodyHydrostaticStiffnessMethod: Sectional)
- 16 wave headings (0 to 337.5 deg, full 360), 30 periods (4 to 600 s)
- Non-planar panels divided (DivideNonPlanarPanels: Yes)
- OutputIntermediateResults enabled
- No Morison elements
- Produces: L04 Sectional bodies.owr

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

# Keys added in OrcaWave versions newer than 11.6 — strip before loading
_UNSUPPORTED_KEYS = {"PanelAngleWarningLevel"}


def _load_data_compat(diff, yml_file):
    """Load YAML stripping keys unsupported in OrcFxAPI 11.6.
    Temp file is written in same directory so relative .gdf paths stay valid.
    """
    p = Path(yml_file)
    text = p.read_text(encoding="utf-8")
    filtered = "\n".join(
        line for line in text.splitlines()
        if not any(line.strip().startswith(k) for k in _UNSUPPORTED_KEYS)
    )
    tmp_path = p.parent / "_tmp_compat.yml"
    try:
        tmp_path.write_text(filtered, encoding="utf-8")
        diff.LoadData(str(tmp_path))
    finally:
        tmp_path.unlink(missing_ok=True)


def main():
    example_dir = Path(__file__).parent
    yml_file = example_dir / "L04 Sectional bodies.yml"

    if not yml_file.exists():
        print(f"[ERROR] Input file not found: {yml_file}")
        sys.exit(1)

    print(f"Running OrcaWave diffraction: {yml_file.name}")
    print("  Solver: Direct LU | Drag linearisation: No | Bodies: 7 | Headings: 16")

    diff = OrcFxAPI.Diffraction(threadCount=12)
    _load_data_compat(diff, yml_file)

    if diff.ValidationErrorText:
        print(f"[ERROR] Validation failed:\n{diff.ValidationErrorText}")
        sys.exit(1)
    if diff.ValidationWarningText:
        print(f"[WARNING] Validation warnings:\n{diff.ValidationWarningText}")

    print("[...] Calculating (7-body sectional problem, potential formulation only)...")
    start = time.time()
    diff.Calculate()
    elapsed = time.time() - start
    print(f"[OK] Completed in {elapsed:.1f}s")

    owr_file = example_dir / "L04 Sectional bodies.owr"
    diff.SaveResults(str(owr_file))
    size_mb = owr_file.stat().st_size / (1024 ** 2)
    print(f"[OK] Saved: {owr_file.name} ({size_mb:.2f} MB)")


if __name__ == "__main__":
    main()
