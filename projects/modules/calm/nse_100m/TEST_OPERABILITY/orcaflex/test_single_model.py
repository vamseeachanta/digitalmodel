#!/usr/bin/env python3
"""
ABOUTME: Test loading a single CALM buoy model to troubleshoot path issues
ABOUTME: Creates a flattened model with all includes in one directory
"""

import sys
from pathlib import Path
import shutil

try:
    import OrcFxAPI
    print(f"OrcaFlex DLL version: {OrcFxAPI.DLLVersion()}")
    ORCAFLEX_AVAILABLE = True
except ImportError:
    print("WARNING: OrcaFlex not available")
    ORCAFLEX_AVAILABLE = False
    sys.exit(1)
except Exception as e:
    print(f"OrcaFlex loaded but version check failed: {e}")
    ORCAFLEX_AVAILABLE = True

def create_flattened_model():
    """
    Create a single-directory version of the model with all includes.
    This matches the working pattern from tests/modules/orcaflex/analysis/moorings/pretension
    """
    project_dir = Path(__file__).parent
    base_dir = project_dir / "base_files"
    test_dir = project_dir / "test_flat_model"
    test_dir.mkdir(exist_ok=True)

    print(f"\nCreating flattened model in: {test_dir}")

    # Copy all base files to test directory
    print("\nCopying base files...")
    for base_file in base_dir.glob("*.yml"):
        dest = test_dir / base_file.name
        shutil.copy2(base_file, dest)
        print(f"  Copied: {base_file.name}")

    # Copy environment files for 000deg 1yr
    print("\nCopying environment files...")
    env_files = [
        "waves_000deg_1yr.yml",
        "current_000deg_1yr.yml",
        "wind_000deg_1yr.yml"
    ]
    for env_file in env_files:
        src = base_dir / "env" / env_file
        dest = test_dir / env_file
        if src.exists():
            shutil.copy2(src, dest)
            print(f"  Copied: {env_file}")

    # Create main model file with flat includes (no ../ paths)
    main_content = f"""%YAML 1.1
# Type: Model
# Program: OrcaFlex
---
General:
  - includefile: _01a_units_analysis.yml
  - includefile: _01b_statics.yml
  - includefile: _01c_dynamics.yml
  - includefile: _01d_stages.yml
  - includefile: _01e_view.yml

VariableData:
  - includefile: _02_variable_data.yml

Environment:
  - includefile: _03a_sea_density.yml
  - includefile: _03b_seabed.yml
  - includefile: waves_000deg_1yr.yml
  - includefile: current_000deg_1yr.yml
  - includefile: wind_000deg_1yr.yml

VesselTypes:
  - includefile: _04_vessel_types.yml

LineTypes:
  - includefile: _05_line_types.yml

Vessels:
  - includefile: _06_vessels_buoys.yml

Lines:
  - includefile: _07_lines.yml

Groups:
  - includefile: _08_groups.yml
"""

    main_model = test_dir / "calm_buoy_000deg_1yr_flat.yml"
    main_model.write_text(main_content)
    print(f"\nCreated main model: {main_model.name}")

    return main_model

def test_load_model(model_path: Path):
    """Test loading the model with OrcaFlex."""
    print(f"\n{'='*80}")
    print("TESTING MODEL LOAD")
    print(f"{'='*80}")
    print(f"Model: {model_path}")
    print(f"Working directory: {Path.cwd()}")

    try:
        # Change to model directory (this is key for relative paths)
        import os
        original_dir = os.getcwd()
        os.chdir(model_path.parent)
        print(f"Changed to: {os.getcwd()}")

        print("\nLoading model...")
        model = OrcFxAPI.Model(model_path.name)
        print("SUCCESS: Model loaded!")

        print("\nRunning statics...")
        model.CalculateStatics()
        print("SUCCESS: Statics completed!")

        print("\nModel details:")
        print(f"  Vessels: {len([obj for obj in model.objects if obj.type == OrcFxAPI.otVessel])}")
        print(f"  Lines: {len([obj for obj in model.objects if obj.type == OrcFxAPI.otLine])}")

        # Change back
        os.chdir(original_dir)

        return True

    except Exception as e:
        print(f"\nERROR: {e}")
        import os
        os.chdir(original_dir)
        return False

def main():
    print("="*80)
    print("CALM BUOY MODEL - SINGLE MODEL TEST")
    print("="*80)

    if not ORCAFLEX_AVAILABLE:
        return 1

    # Create flattened model
    model_path = create_flattened_model()

    # Test loading
    success = test_load_model(model_path)

    if success:
        print(f"\n{'='*80}")
        print("TEST PASSED")
        print(f"{'='*80}")
        print("\nThe flattened model structure works!")
        print("Next steps:")
        print("  1. Use this pattern for batch processing")
        print("  2. Copy all base_files + env files to single directory")
        print("  3. Update main models to use flat includes")
        return 0
    else:
        print(f"\n{'='*80}")
        print("TEST FAILED")
        print(f"{'='*80}")
        return 1

if __name__ == "__main__":
    sys.exit(main())
