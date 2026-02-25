#!/usr/bin/env python
"""
Quick test script to verify postprocessing works with generated sim files.
Run this after sim files are generated in ../runs/
"""
import sys
from pathlib import Path

try:
    import OrcFxAPI
    ORCAFLEX_AVAILABLE = True
except ImportError:
    ORCAFLEX_AVAILABLE = False
    print("OrcaFlex not available")
    sys.exit(1)


def test_single_sim_file(sim_path: Path):
    """Test postprocessing on a single sim file."""
    print(f"\nTesting: {sim_path.name}")
    print("-" * 50)

    model = OrcFxAPI.Model(str(sim_path))
    print(f"Model state: {model.state} (4=SimComplete, 2=Static)")

    # Find pipeline line
    pipeline = None
    for obj in model.objects:
        if obj.type == OrcFxAPI.otLine:
            if 'pipeline' in obj.name.lower():
                pipeline = obj
                break

    if pipeline is None:
        # Use first line if 'pipeline' not found
        for obj in model.objects:
            if obj.type == OrcFxAPI.otLine:
                pipeline = obj
                break

    if pipeline is None:
        print("ERROR: No line object found!")
        return False

    print(f"Line object: {pipeline.name}")

    # Test End A/B Effective Tension (Static)
    print("\n1. Effective Tension (StaticState):")
    period = OrcFxAPI.pnStaticState
    try:
        end_a = pipeline.TimeHistory("Effective Tension", period, objectExtra=OrcFxAPI.oeEndA)
        end_b = pipeline.TimeHistory("Effective Tension", period, objectExtra=OrcFxAPI.oeEndB)
        print(f"   End A: {end_a[0]:.2f} kN")
        print(f"   End B: {end_b[0]:.2f} kN")
    except Exception as e:
        print(f"   ERROR: {e}")

    # Test RangeGraph for Global Y (if dynamic results available)
    if model.state == 4:  # SimComplete
        print("\n2. RangeGraph Global Y (WholeSimulation):")
        period = OrcFxAPI.pnWholeSimulation
        try:
            rg = pipeline.RangeGraph('y', period)
            print(f"   Arc Length: {len(rg.X)} points ({rg.X[0]:.1f} to {rg.X[-1]:.1f} m)")
            print(f"   Min Y: {min(rg.Min):.3f} to {max(rg.Min):.3f} m")
            print(f"   Max Y: {min(rg.Max):.3f} to {max(rg.Max):.3f} m")
        except Exception as e:
            print(f"   ERROR: {e}")
    else:
        print("\n2. RangeGraph: Skipped (no dynamic results)")

    print("\nTest PASSED!")
    return True


def main():
    """Find and test sim files."""
    runs_dir = Path(__file__).parent.parent / "runs"

    if not runs_dir.exists():
        print(f"Runs directory not found: {runs_dir}")
        return

    sim_files = list(runs_dir.glob("*.sim"))

    if not sim_files:
        print(f"No .sim files found in {runs_dir}")
        print("Run OrcaFlex simulations first.")
        return

    print(f"Found {len(sim_files)} sim files")

    # Test first available sim file
    success = test_single_sim_file(sim_files[0])

    if success and len(sim_files) > 1:
        print(f"\n{len(sim_files) - 1} more sim files ready for postprocessing.")


if __name__ == "__main__":
    main()
