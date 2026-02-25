#!/usr/bin/env python3
"""
Run OrcaWave full QTF diffraction analysis for L06

ABOUTME: Two-stage QTF restart workflow for an ISSC TLP (1.25m panels). Both
YAML files use RestartingFrom: to extend existing .owd files rather than
solving from scratch. Parent .owd files must exist before running.

Stage A: RestartingFrom L06 Parent model for full QTF experiments.owd
         Sum-frequency QTF, auto-generated free-surface panelled zone.
Stage B: RestartingFrom L06 First order convergence study.owd
         Sum-frequency QTF + mean drift, direct method, user-defined FS mesh.

Prerequisites: both parent .owd files must exist (run in OrcaWave GUI first).

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


def run_stage(example_dir: Path, yml_name: str, owr_name: str, label: str) -> bool:
    yml_file = example_dir / yml_name
    owr_file = example_dir / owr_name

    if not yml_file.exists():
        print(f"[ERROR] Input file not found: {yml_file}")
        return False

    print(f"\n[{label}] Loading: {yml_name}")
    diff = OrcFxAPI.Diffraction(threadCount=12)
    diff.LoadData(str(yml_file))

    if diff.ValidationErrorText:
        print(f"[ERROR] Validation failed:\n{diff.ValidationErrorText}")
        return False
    if diff.ValidationWarningText:
        print(f"[WARNING] Validation warnings:\n{diff.ValidationWarningText}")

    print(f"[{label}] Calculating full QTF (restart from .owd)...")
    start = time.time()
    diff.Calculate()
    elapsed = time.time() - start
    print(f"[{label}] Completed in {elapsed:.1f}s")

    diff.SaveResults(str(owr_file))
    size_mb = owr_file.stat().st_size / (1024 ** 2)
    print(f"[{label}] Saved: {owr_file.name} ({size_mb:.2f} MB)")
    return True


def main():
    example_dir = Path(__file__).parent

    # Check prerequisites
    prerequisites = [
        "L06 Parent model for full QTF experiments.owd",
        "L06 First order convergence study.owd",
    ]
    missing = [p for p in prerequisites if not (example_dir / p).exists()]
    if missing:
        print("[ERROR] Required parent .owd files are missing:")
        for m in missing:
            print(f"  - {m}")
        print("Run the parent models in OrcaWave GUI before executing this script.")
        sys.exit(1)

    print("Running OrcaWave full QTF restart workflow (2 stages)")

    ok_a = run_stage(
        example_dir,
        yml_name="L06 Potential loads (first experimental model).yml",
        owr_name="L06 Potential loads (first experimental model).owr",
        label="Stage A",
    )
    if not ok_a:
        print("[ABORT] Stage A failed — skipping Stage B")
        sys.exit(1)

    ok_b = run_stage(
        example_dir,
        yml_name="L06 Time varying quadratic loads.yml",
        owr_name="L06 Time varying quadratic loads.owr",
        label="Stage B",
    )

    if ok_a and ok_b:
        print("\n[OK] Both stages completed successfully")
    else:
        print("\n[PARTIAL] One or more stages failed")
        sys.exit(1)


if __name__ == "__main__":
    main()
