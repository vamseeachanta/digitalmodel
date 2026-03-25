#!/usr/bin/env python3
"""
ABOUTME: Generate analysis model files for all directions and return periods
ABOUTME: Creates complete OrcaFlex model files with base_files and env references
"""

from pathlib import Path

# Configuration
ANALYSIS_DIR = Path("D:/workspace-hub/digitalmodel/projects/TEST_OPERABILITY/orcaflex/analysis_models")
BASE_FILES_REL = "../base_files"  # Relative path from analysis_models to base_files
DIRECTIONS = [0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330]
RETURN_PERIODS = ['1yr', '10yr', '100yr']

# Model types
MODEL_TYPES = {
    'simple': 'Simple (single vessel)',
    'discretised': 'Discretised (8 buoys)'
}

def create_analysis_file(direction: int, return_period: str, model_type: str = 'simple'):
    """Create analysis model file for specific direction and return period."""

    rp_label = {'1yr': '1-Year', '10yr': '10-Year', '100yr': '100-Year'}[return_period]
    model_desc = MODEL_TYPES[model_type]

    # Determine which groups file to use
    groups_file = '_08_groups.yml' if model_type == 'simple' else '_08_groups_discretised.yml'
    lines_file = '_07_lines.yml' if model_type == 'simple' else '_07_lines_discretised.yml'

    # Build content based on model type
    if model_type == 'simple':
        content = f"""%YAML 1.1
# Type: Model
# Program: OrcaFlex
# Analysis Model: CALM Buoy - {direction}° Direction, {rp_label} Return Period
# Model Type: {model_desc}
# Description: Complete analysis model with base_files and environment references
---
General:
  - includefile: {BASE_FILES_REL}/_01a_units_analysis.yml
  - includefile: {BASE_FILES_REL}/_01b_statics.yml
  - includefile: {BASE_FILES_REL}/_01c_dynamics.yml
  - includefile: {BASE_FILES_REL}/_01d_stages.yml
  - includefile: {BASE_FILES_REL}/_01e_view.yml

VariableData:
  - includefile: {BASE_FILES_REL}/_02_variable_data.yml

Environment:
  - includefile: {BASE_FILES_REL}/_03a_sea_density.yml
  - includefile: {BASE_FILES_REL}/_03b_seabed.yml
  - includefile: {BASE_FILES_REL}/env/waves_{direction:03d}deg_{return_period}.yml
  - includefile: {BASE_FILES_REL}/env/current_{direction:03d}deg_{return_period}.yml
  - includefile: {BASE_FILES_REL}/env/wind_{direction:03d}deg_{return_period}.yml

VesselTypes:
  - includefile: {BASE_FILES_REL}/_04_vessel_types.yml

LineTypes:
  - includefile: {BASE_FILES_REL}/_05_line_types.yml

Vessels:
  - includefile: {BASE_FILES_REL}/_06_vessels_buoys.yml

Lines:
  - includefile: {BASE_FILES_REL}/{lines_file}

Groups:
  - includefile: {BASE_FILES_REL}/{groups_file}
"""
    else:  # discretised
        content = f"""%YAML 1.1
# Type: Model
# Program: OrcaFlex
# Analysis Model: CALM Buoy - {direction}° Direction, {rp_label} Return Period
# Model Type: {model_desc}
# Description: Complete analysis model with base_files and environment references
---
General:
  - includefile: {BASE_FILES_REL}/_01a_units_analysis.yml
  - includefile: {BASE_FILES_REL}/_01b_statics.yml
  - includefile: {BASE_FILES_REL}/_01c_dynamics.yml
  - includefile: {BASE_FILES_REL}/_01d_stages.yml
  - includefile: {BASE_FILES_REL}/_01e_view.yml

VariableData:
  - includefile: {BASE_FILES_REL}/_02_variable_data.yml

Environment:
  - includefile: {BASE_FILES_REL}/_03a_sea_density.yml
  - includefile: {BASE_FILES_REL}/_03b_seabed.yml
  - includefile: {BASE_FILES_REL}/env/waves_{direction:03d}deg_{return_period}.yml
  - includefile: {BASE_FILES_REL}/env/current_{direction:03d}deg_{return_period}.yml
  - includefile: {BASE_FILES_REL}/env/wind_{direction:03d}deg_{return_period}.yml

VesselTypes:
  - includefile: {BASE_FILES_REL}/_04_vessel_types.yml

LineTypes:
  - includefile: {BASE_FILES_REL}/_05_line_types.yml

Vessels:
  - includefile: {BASE_FILES_REL}/_06_vessels_buoys.yml

6DBuoys:
  - includefile: {BASE_FILES_REL}/_06_buoys_discretised.yml

Lines:
  - includefile: {BASE_FILES_REL}/{lines_file}

Groups:
  - includefile: {BASE_FILES_REL}/{groups_file}
"""

    # Flat file naming: NSE_CALM_001_000deg_1yr_simple.yml
    filename = ANALYSIS_DIR / f"NSE_CALM_001_{direction:03d}deg_{return_period}_{model_type}.yml"
    with open(filename, 'w') as f:
        f.write(content)
    print(f"Created: {filename.name}")

def main():
    """Generate all analysis model files."""
    print("=" * 80)
    print("GENERATING ANALYSIS MODEL FILES")
    print("=" * 80)
    print(f"\nOutput directory: {ANALYSIS_DIR}")
    print(f"Directions: {len(DIRECTIONS)} (0° to 330° in 30° steps)")
    print(f"Return Periods: {len(RETURN_PERIODS)} (1yr, 10yr, 100yr)")
    print(f"Model Types: {len(MODEL_TYPES)} (simple, discretised)")

    # Create output directory
    ANALYSIS_DIR.mkdir(parents=True, exist_ok=True)

    total_files = 0

    for model_type in MODEL_TYPES.keys():
        print(f"\n{'=' * 80}")
        print(f"Generating {MODEL_TYPES[model_type]} Models")
        print(f"{'=' * 80}")

        for return_period in RETURN_PERIODS:
            rp_label = {'1yr': '1-Year', '10yr': '10-Year', '100yr': '100-Year'}[return_period]
            print(f"\n{rp_label} Return Period:")

            for direction in DIRECTIONS:
                create_analysis_file(direction, return_period, model_type)
                total_files += 1

    print("\n" + "=" * 80)
    print("GENERATION COMPLETE")
    print("=" * 80)
    print(f"\nTotal files created: {total_files}")
    print(f"  - {len(DIRECTIONS) * len(RETURN_PERIODS)} files per model type")
    print(f"  - {len(DIRECTIONS)} directions × {len(RETURN_PERIODS)} return periods × {len(MODEL_TYPES)} model types")
    print(f"\nBreakdown:")
    print(f"  - {len(DIRECTIONS) * len(RETURN_PERIODS)} Simple model files")
    print(f"  - {len(DIRECTIONS) * len(RETURN_PERIODS)} Discretised model files")
    print(f"\nFile naming convention:")
    print(f"  NSE_CALM_001_<direction>deg_<return_period>_<model_type>.yml")
    print(f"\nExamples:")
    print(f"  - NSE_CALM_001_000deg_1yr_simple.yml")
    print(f"  - NSE_CALM_001_090deg_10yr_discretised.yml")
    print(f"  - NSE_CALM_001_180deg_100yr_simple.yml")
    print(f"\nEach file references:")
    print(f"  - Structural base_files (units, vessel types, line types, etc.)")
    print(f"  - Direction and return-period specific env files")

if __name__ == "__main__":
    main()
