#!/usr/bin/env python3
"""
ABOUTME: Fix OrcaFlex file structure to match working reference pattern
ABOUTME: Creates section wrappers and regenerates analysis models
"""

from pathlib import Path

# Configuration
PROJECT_DIR = Path("D:/workspace-hub/digitalmodel/projects/TEST_OPERABILITY/orcaflex")
BASE_DIR = PROJECT_DIR / "base_files"
SECTIONS_DIR = BASE_DIR / "sections"
ENV_SECTIONS_DIR = SECTIONS_DIR / "env"
ANALYSIS_DIR = PROJECT_DIR / "analysis_models"

DIRECTIONS = [0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330]
RETURN_PERIODS = ['1yr', '10yr', '100yr']
MODEL_TYPES = ['simple', 'discretised']

def create_env_section_wrappers():
    """Create Environment section wrappers for each direction and return period."""
    print("Creating environment section wrappers...")
    ENV_SECTIONS_DIR.mkdir(parents=True, exist_ok=True)

    count = 0
    for direction in DIRECTIONS:
        for rp in RETURN_PERIODS:
            content = f"""Environment:
  - includefile: ../../env/waves_{direction:03d}deg_{rp}.yml
  - includefile: ../../env/current_{direction:03d}deg_{rp}.yml
  - includefile: ../../env/wind_{direction:03d}deg_{rp}.yml
"""
            filename = ENV_SECTIONS_DIR / f"_env_{direction:03d}deg_{rp}.yml"
            filename.write_text(content)
            count += 1

    print(f"  Created {count} environment section wrappers")

def create_fixed_analysis_model(direction: int, return_period: str, model_type: str):
    """Create fixed analysis model following reference pattern."""

    rp_label = {'1yr': '1-Year', '10yr': '10-Year', '100yr': '100-Year'}[return_period]
    model_desc = {'simple': 'Simple (single vessel)', 'discretised': 'Discretised (8 buoys)'}[model_type]

    # Use discretised-specific files if needed
    lines_section = '_07_lines.yml' if model_type == 'simple' else '_07_lines_discretised.yml'
    groups_section = '_08_groups.yml' if model_type == 'simple' else '_08_groups_discretised.yml'

    # Create main model file matching reference structure
    content = f"""%YAML 1.1
# Type: Model
# Program: OrcaFlex
# Analysis Model: CALM Buoy - {direction} deg Direction, {rp_label} Return Period
# Model Type: {model_desc}
---
- includefile: ../base_files/sections/_01_general.yml
- includefile: ../base_files/sections/_02_variable_data.yml
- includefile: ../base_files/sections/_03_environment.yml
- includefile: ../base_files/sections/env/_env_{direction:03d}deg_{return_period}.yml
- includefile: ../base_files/sections/_04_vessel_types.yml
- includefile: ../base_files/sections/_05_line_types.yml
- includefile: ../base_files/sections/_06_vessels.yml
"""

    # Add discretised-specific sections
    if model_type == 'discretised':
        content += f"""- includefile: ../base_files/_06_buoys_discretised.yml
"""

    content += f"""- includefile: ../base_files/sections/{lines_section}
- includefile: ../base_files/sections/{groups_section}
"""

    filename = ANALYSIS_DIR / f"NSE_CALM_001_{direction:03d}deg_{return_period}_{model_type}.yml"
    filename.write_text(content)

def regenerate_analysis_models():
    """Regenerate all 72 analysis models with fixed structure."""
    print("\nRegenerating analysis models...")

    count = 0
    for model_type in MODEL_TYPES:
        for return_period in RETURN_PERIODS:
            for direction in DIRECTIONS:
                create_fixed_analysis_model(direction, return_period, model_type)
                count += 1

    print(f"  Regenerated {count} analysis models")

def fix_sections_for_discretised():
    """Create discretised-specific section wrappers."""
    print("\nCreating discretised-specific sections...")

    # Lines section for discretised
    content = f"""Lines:
  - includefile: ../_07_lines_discretised.yml
"""
    (SECTIONS_DIR / "_07_lines_discretised.yml").write_text(content)

    # Groups section for discretised
    content = f"""Groups:
  - includefile: ../_08_groups_discretised.yml
"""
    (SECTIONS_DIR / "_08_groups_discretised.yml").write_text(content)

    # 6DBuoys section for discretised
    content = f"""6DBuoys:
  - includefile: ../_06_buoys_discretised.yml
"""
    (SECTIONS_DIR / "_06_buoys_discretised.yml").write_text(content)

    print("  Created 3 discretised-specific sections")

def main():
    print("="*80)
    print("FIXING ORCAFLEX FILE STRUCTURE")
    print("="*80)
    print(f"\nProject directory: {PROJECT_DIR}")
    print(f"Base files: {BASE_DIR}")
    print(f"Sections: {SECTIONS_DIR}")
    print(f"Analysis models: {ANALYSIS_DIR}")

    # Create environment section wrappers
    create_env_section_wrappers()

    # Create discretised-specific sections
    fix_sections_for_discretised()

    # Regenerate all analysis models
    regenerate_analysis_models()

    print("\n" + "="*80)
    print("STRUCTURE FIX COMPLETE")
    print("="*80)
    print(f"\nCreated:")
    print(f"  - {len(DIRECTIONS) * len(RETURN_PERIODS)} environment section wrappers")
    print(f"  - 3 discretised-specific sections")
    print(f"  - {len(DIRECTIONS) * len(RETURN_PERIODS) * len(MODEL_TYPES)} fixed analysis models")

    print(f"\nStructure now matches reference pattern:")
    print(f"  tests/modules/orcaflex/analysis/moorings/pretension/source")

    print(f"\nNext steps:")
    print(f"  1. Test single model: python test_single_model_fixed.py")
    print(f"  2. If successful, run batch: python run_1year_operability.py")

if __name__ == "__main__":
    main()
