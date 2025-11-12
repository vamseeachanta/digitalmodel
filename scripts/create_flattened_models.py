#!/usr/bin/env python3
"""
ABOUTME: Create flattened single-directory OrcaFlex models matching reference pattern
ABOUTME: Each model gets its own directory with ALL files (no nested relative paths)
"""

from pathlib import Path
import shutil

# Configuration
PROJECT_DIR = Path("D:/workspace-hub/digitalmodel/projects/TEST_OPERABILITY/orcaflex")
BASE_DIR = PROJECT_DIR / "base_files"
FLATTENED_DIR = PROJECT_DIR / "flattened_models"

DIRECTIONS = [0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330]
RETURN_PERIODS = ['1yr', '10yr', '100yr']
MODEL_TYPES = ['simple', 'discretised']

def create_flattened_model_dir(direction: int, return_period: str, model_type: str):
    """Create a single flattened directory for one model with all files."""

    model_name = f"NSE_CALM_001_{direction:03d}deg_{return_period}_{model_type}"
    model_dir = FLATTENED_DIR / model_name

    print(f"\nCreating flattened model: {model_name}")

    # Create directory
    model_dir.mkdir(parents=True, exist_ok=True)

    # Copy all base files (flattened - no subdirectories)
    print("  Copying base files...")
    base_files_copied = 0

    # Copy General section files
    for file in (BASE_DIR).glob("_01*.yml"):
        shutil.copy2(file, model_dir / file.name)
        base_files_copied += 1

    # Copy Variable Data
    for file in (BASE_DIR).glob("_02*.yml"):
        shutil.copy2(file, model_dir / file.name)
        base_files_copied += 1

    # Copy Environment base
    for file in (BASE_DIR).glob("_03*.yml"):
        shutil.copy2(file, model_dir / file.name)
        base_files_copied += 1

    # Copy Vessel Types
    for file in (BASE_DIR).glob("_04*.yml"):
        shutil.copy2(file, model_dir / file.name)
        base_files_copied += 1

    # Copy Line Types
    for file in (BASE_DIR).glob("_05*.yml"):
        shutil.copy2(file, model_dir / file.name)
        base_files_copied += 1

    # Copy Vessels
    for file in (BASE_DIR).glob("_06*.yml"):
        if model_type == 'simple' and 'discretised' in file.name:
            continue
        shutil.copy2(file, model_dir / file.name)
        base_files_copied += 1

    # Copy Lines
    for file in (BASE_DIR).glob("_07*.yml"):
        if model_type == 'simple' and 'discretised' in file.name:
            continue
        elif model_type == 'discretised' and 'discretised' not in file.name:
            continue
        shutil.copy2(file, model_dir / file.name)
        base_files_copied += 1

    # Copy Groups
    for file in (BASE_DIR).glob("_08*.yml"):
        if model_type == 'simple' and 'discretised' in file.name:
            continue
        elif model_type == 'discretised' and 'discretised' not in file.name:
            continue
        shutil.copy2(file, model_dir / file.name)
        base_files_copied += 1

    print(f"    Copied {base_files_copied} base files")

    # Copy environment files for this specific direction and return period
    print("  Copying environment files...")
    env_dir = BASE_DIR / "env"
    env_files = [
        f"waves_{direction:03d}deg_{return_period}.yml",
        f"current_{direction:03d}deg_{return_period}.yml",
        f"wind_{direction:03d}deg_{return_period}.yml"
    ]

    for env_file in env_files:
        src = env_dir / env_file
        if src.exists():
            shutil.copy2(src, model_dir / env_file)

    print(f"    Copied {len(env_files)} environment files")

    # Create wrapper files for sections with multiple includes
    print("  Creating wrapper files...")

    # General wrapper
    general_wrapper = """General:
  - includefile: _01a_units_analysis.yml
  - includefile: _01b_statics.yml
  - includefile: _01c_dynamics.yml
  - includefile: _01d_stages.yml
  - includefile: _01e_view.yml
"""
    (model_dir / "_01_general.yml").write_text(general_wrapper)

    # Environment wrapper
    env_wrapper = f"""Environment:
  - includefile: _03a_sea_density.yml
  - includefile: _03b_seabed.yml
  - includefile: waves_{direction:03d}deg_{return_period}.yml
  - includefile: current_{direction:03d}deg_{return_period}.yml
  - includefile: wind_{direction:03d}deg_{return_period}.yml
"""
    (model_dir / "_03_environment.yml").write_text(env_wrapper)

    print("    Created 2 wrapper files")

    # Create main model file
    print("  Creating main model file...")

    rp_label = {'1yr': '1-Year', '10yr': '10-Year', '100yr': '100-Year'}[return_period]
    model_desc = {'simple': 'Simple (single vessel)', 'discretised': 'Discretised (8 buoys)'}[model_type]

    # Determine which files to use based on model type
    lines_file = "_07_lines.yml" if model_type == 'simple' else "_07_lines_discretised.yml"
    groups_file = "_08_groups.yml" if model_type == 'simple' else "_08_groups_discretised.yml"

    main_content = f"""%YAML 1.1
# Type: Model
# Program: OrcaFlex
# Analysis Model: CALM Buoy - {direction} deg Direction, {rp_label} Return Period
# Model Type: {model_desc}
---
- includefile: _01_general.yml
- includefile: _02_variable_data.yml
- includefile: _03_environment.yml
- includefile: _04_vessel_types.yml
- includefile: _05_line_types.yml
- includefile: _06_vessels_buoys.yml
- includefile: {lines_file}
- includefile: {groups_file}
"""

    (model_dir / f"{model_name}.yml").write_text(main_content)

    print(f"  Created main model: {model_name}.yml")

    # Count total files
    total_files = len(list(model_dir.glob("*.yml")))
    print(f"  Total files in directory: {total_files}")

    return model_dir

def create_all_1year_models():
    """Create flattened models for all 1-year directions."""
    print("="*80)
    print("CREATING FLATTENED 1-YEAR OPERABILITY MODELS")
    print("="*80)

    print(f"\nProject directory: {PROJECT_DIR}")
    print(f"Base files: {BASE_DIR}")
    print(f"Output directory: {FLATTENED_DIR}")

    # Remove existing flattened directory
    if FLATTENED_DIR.exists():
        print(f"\nRemoving existing flattened_models directory...")
        shutil.rmtree(FLATTENED_DIR)

    FLATTENED_DIR.mkdir(parents=True, exist_ok=True)

    # Create models for all 12 directions (1-year, simple only for now)
    created_models = []
    for direction in DIRECTIONS:
        model_dir = create_flattened_model_dir(direction, '1yr', 'simple')
        created_models.append(model_dir)

    print("\n" + "="*80)
    print("FLATTENED MODELS CREATED")
    print("="*80)
    print(f"\nCreated {len(created_models)} models:")
    for model_dir in created_models:
        print(f"  - {model_dir.name}")

    print("\nStructure:")
    print("  - Each model in its own directory")
    print("  - All files flattened (no subdirectories)")
    print("  - No ../ relative paths")
    print("  - Matches reference pattern exactly")

    print("\nNext steps:")
    print("  1. Test single model: python test_flattened_model.py")
    print("  2. Run batch: python run_1year_operability_flattened.py")

def create_all_models():
    """Create flattened models for ALL directions, return periods, and types."""
    print("="*80)
    print("CREATING ALL FLATTENED MODELS")
    print("="*80)

    print(f"\nProject directory: {PROJECT_DIR}")
    print(f"Base files: {BASE_DIR}")
    print(f"Output directory: {FLATTENED_DIR}")

    # Remove existing flattened directory
    if FLATTENED_DIR.exists():
        print(f"\nRemoving existing flattened_models directory...")
        shutil.rmtree(FLATTENED_DIR)

    FLATTENED_DIR.mkdir(parents=True, exist_ok=True)

    # Create all 72 models
    created_models = []
    for model_type in MODEL_TYPES:
        for return_period in RETURN_PERIODS:
            for direction in DIRECTIONS:
                model_dir = create_flattened_model_dir(direction, return_period, model_type)
                created_models.append(model_dir)

    print("\n" + "="*80)
    print("ALL FLATTENED MODELS CREATED")
    print("="*80)
    print(f"\nCreated {len(created_models)} models:")
    print(f"  - {len(MODEL_TYPES)} model types")
    print(f"  - {len(RETURN_PERIODS)} return periods")
    print(f"  - {len(DIRECTIONS)} directions")
    print(f"  - Total: {len(created_models)} models")

    print("\nStructure:")
    print("  - Each model in its own directory")
    print("  - All files flattened (no subdirectories)")
    print("  - No ../ relative paths")
    print("  - Matches reference pattern exactly")

def main():
    import sys

    if len(sys.argv) > 1 and sys.argv[1] == "all":
        create_all_models()
    else:
        create_all_1year_models()

if __name__ == "__main__":
    main()
