#!/usr/bin/env python3
"""
Run OrcaWave diffraction analysis for L05 Panel pressures

ABOUTME: Same TLP-style geometry as L04 (7 sectional bodies) but with
OutputPanelPressures enabled, demonstrating per-panel hydrodynamic pressure
output for structural load assessment.

Example features:
- 7 bodies: Keystone (free), 3 pontoons + 3 columns (connected to keystone/pontoons)
- Potential formulation only (no drag linearisation, no wave spectrum)
- OutputPanelPressures: Yes (key difference from L04)
- Sectional hydrostatic stiffness method
- 16 wave headings (0 to 337.5 deg, full 360), 30 periods (4 to 600 s)
- OutputIntermediateResults enabled
- Panel pressure output significantly increases .owr file size
- Produces: L05 Panel pressures.owr

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
    yml_file = example_dir / "L05 Panel pressures.yml"

    if not yml_file.exists():
        print(f"[ERROR] Input file not found: {yml_file}")
        sys.exit(1)

    print(f"Running OrcaWave diffraction: {yml_file.name}")
    print("  Solver: Direct LU | Panel pressures: Yes | Bodies: 7 | Headings: 16")
    print("  [NOTE] Panel pressure output increases .owr file size substantially")

    diff = OrcFxAPI.Diffraction(threadCount=12)
    _load_data_compat(diff, yml_file)

    if diff.ValidationErrorText:
        print(f"[ERROR] Validation failed:\n{diff.ValidationErrorText}")
        sys.exit(1)
    if diff.ValidationWarningText:
        print(f"[WARNING] Validation warnings:\n{diff.ValidationWarningText}")

    print("[...] Calculating (7-body sectional problem with panel pressures)...")
    start = time.time()
    diff.Calculate()
    elapsed = time.time() - start
    print(f"[OK] Completed in {elapsed:.1f}s")

    owr_file = example_dir / "L05 Panel pressures.owr"
    diff.SaveResults(str(owr_file))
    size_mb = owr_file.stat().st_size / (1024 ** 2)
    print(f"[OK] Saved: {owr_file.name} ({size_mb:.2f} MB)")


if __name__ == "__main__":
    main()
