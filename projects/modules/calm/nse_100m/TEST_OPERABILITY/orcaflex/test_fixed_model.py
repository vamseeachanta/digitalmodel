#!/usr/bin/env python3
"""
ABOUTME: Test fixed OrcaFlex model structure
ABOUTME: Tests that model loads and runs successfully
"""

import sys
from pathlib import Path

try:
    import OrcFxAPI
    print(f"OrcaFlex DLL version: {OrcFxAPI.DLLVersion()}")
    ORCAFLEX_AVAILABLE = True
except ImportError:
    print("ERROR: OrcaFlex not available")
    sys.exit(1)

def test_model(model_file: Path):
    """Test loading and running a single model."""
    print(f"\n{'='*80}")
    print(f"TESTING: {model_file.name}")
    print(f"{'='*80}")

    try:
        import os
        import time

        # Change to model directory (critical for relative paths)
        original_dir = os.getcwd()
        os.chdir(model_file.parent)
        print(f"Working directory: {os.getcwd()}")

        # Load model
        print(f"\n[1/3] Loading model...")
        start = time.time()
        model = OrcFxAPI.Model(model_file.name)
        load_time = time.time() - start
        print(f"  SUCCESS: Model loaded ({load_time:.2f}s)")

        # Count objects
        vessels = len([obj for obj in model.objects if obj.type == OrcFxAPI.otVessel])
        lines = len([obj for obj in model.objects if obj.type == OrcFxAPI.otLine])
        print(f"  Model has {vessels} vessels, {lines} lines")

        # Run statics
        print(f"\n[2/3] Running statics...")
        start = time.time()
        model.CalculateStatics()
        statics_time = time.time() - start
        print(f"  SUCCESS: Statics completed ({statics_time:.2f}s)")

        # Run dynamics (short duration for test)
        print(f"\n[3/3] Running dynamics...")
        start = time.time()
        model.RunSimulation()
        dynamics_time = time.time() - start
        print(f"  SUCCESS: Dynamics completed ({dynamics_time:.2f}s)")

        # Save test result
        output_file = model_file.parent.parent / "results" / "test" / f"{model_file.stem}.sim"
        output_file.parent.mkdir(parents=True, exist_ok=True)
        model.SaveSimulation(str(output_file))
        print(f"\n  Results saved: {output_file.name}")

        # Restore directory
        os.chdir(original_dir)

        print(f"\n{'='*80}")
        print("TEST PASSED")
        print(f"{'='*80}")
        print(f"Total time: {load_time + statics_time + dynamics_time:.2f}s")
        print(f"  - Load: {load_time:.2f}s")
        print(f"  - Statics: {statics_time:.2f}s")
        print(f"  - Dynamics: {dynamics_time:.2f}s")

        return True

    except Exception as e:
        os.chdir(original_dir)
        print(f"\n{'='*80}")
        print("TEST FAILED")
        print(f"{'='*80}")
        print(f"Error: {e}")
        return False

def main():
    print("="*80)
    print("TESTING FIXED ORCAFLEX MODEL STRUCTURE")
    print("="*80)

    if not ORCAFLEX_AVAILABLE:
        return 1

    # Test the 000deg 1yr simple model
    project_dir = Path(__file__).parent
    test_model_file = project_dir / "analysis_models" / "NSE_CALM_001_000deg_1yr_simple.yml"

    if not test_model_file.exists():
        print(f"\nERROR: Test model not found: {test_model_file}")
        return 1

    print(f"\nTest model: {test_model_file.name}")
    print(f"Location: {test_model_file.parent}")

    success = test_model(test_model_file)

    if success:
        print("\nThe fixed structure works!")
        print("\nYou can now run:")
        print("  - Single models in OrcaFlex GUI")
        print("  - Batch processing: python run_1year_operability.py")
        return 0
    else:
        print("\nModel failed to load/run")
        print("Check the error message above for details")
        return 1

if __name__ == "__main__":
    sys.exit(main())
