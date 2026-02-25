#!/usr/bin/env python3
"""
Test script to verify generated OrcaFlex files load successfully
"""

import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

try:
    import OrcFxAPI
    print(f"OrcFxAPI DLL version: {OrcFxAPI.DLLVersion()}")
except ImportError:
    print("ERROR: OrcFxAPI not available")
    print("Cannot test OrcaFlex file loading without OrcFxAPI installed")
    sys.exit(1)

def test_load_file(file_path):
    """Test loading a single OrcaFlex file"""
    print(f"\n{'='*60}")
    print(f"Testing: {file_path.name}")
    print(f"{'='*60}")

    try:
        model = OrcFxAPI.Model()
        model.LoadData(str(file_path))
        print(f"[OK] SUCCESS: File loaded without errors")

        # Print basic info
        print(f"\nModel Info:")
        print(f"  - General.StageDuration: {model.general.StageDuration}")

        # Count objects
        vessels = [obj for obj in model.objects if obj.type == OrcFxAPI.otVessel]
        lines = [obj for obj in model.objects if obj.type == OrcFxAPI.otLine]
        buoys = [obj for obj in model.objects if obj.type == OrcFxAPI.ot6DBuoy]

        print(f"  - Vessels: {len(vessels)}")
        print(f"  - Lines: {len(lines)}")
        print(f"  - 6D Buoys: {len(buoys)}")

        if vessels:
            print(f"  - Vessel names: {[v.name for v in vessels]}")
        if lines:
            print(f"  - Line names: {[l.name for l in lines]}")
        if buoys:
            print(f"  - Buoy names: {[b.name for b in buoys]}")

        return True

    except Exception as e:
        print(f"[FAIL] FAILED: {str(e)}")
        print(f"\nError details:")
        print(f"  Type: {type(e).__name__}")
        print(f"  Message: {str(e)}")
        return False

def main():
    """Test loading generated files"""

    # Test base files
    base_output_dir = Path(__file__).parent.parent / 'tests' / 'output' / 'test_cli_base'
    master_file = base_output_dir / 'test_calm_buoy_base.yml'

    if not master_file.exists():
        print(f"ERROR: Test files not found at {base_output_dir}")
        print("Run the generator first:")
        print("  python scripts/test_orcaflex_agent_cli.py --verbose generate base-files ...")
        return False

    print(f"\n{'='*60}")
    print(f"OrcaFlex File Loading Test")
    print(f"{'='*60}")

    # Test master base file
    success = test_load_file(master_file)

    # Test individual files
    individual_files = [
        base_output_dir / '01_general.yml',
        base_output_dir / '02_var_data.yml',
        base_output_dir / '04_vessel_crowley650_atb.yml',
        base_output_dir / '05_lines.yml',
        base_output_dir / '06_buoys.yml',
    ]

    print(f"\n{'='*60}")
    print(f"Testing Individual Files")
    print(f"{'='*60}")

    for file_path in individual_files:
        if file_path.exists():
            test_load_file(file_path)

    # Test environmental files
    env_output_dir = Path(__file__).parent.parent / 'tests' / 'output' / 'test_cli_env'
    env_file = env_output_dir / 'env_10yr_000deg.yml'

    if env_file.exists():
        print(f"\n{'='*60}")
        print(f"Testing Environmental Files")
        print(f"{'='*60}")
        test_load_file(env_file)

    print(f"\n{'='*60}")
    print(f"Test Complete")
    print(f"{'='*60}")

    return success

if __name__ == '__main__':
    success = main()
    sys.exit(0 if success else 1)
