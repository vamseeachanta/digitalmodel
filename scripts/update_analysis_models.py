"""
Update OrcaFlex Analysis Model Files

This script updates all analysis model files to use the new simplified format:
- BaseFile: points to calm_buoy_simple_base.yml or calm_buoy_discretised_base.yml
- includefile: points to environment file (_env_XXXdeg_YYyr.yml)

Pattern: NSE_CALM_001_{direction}deg_{return_period}yr_{model_type}.yml
"""

import os
import re
from pathlib import Path

# Base directory
analysis_models_dir = Path(__file__).parent.parent / "projects" / "TEST_OPERABILITY" / "orcaflex" / "analysis_models"

# Pattern to extract direction, return period, and model type from filename
filename_pattern = re.compile(r'NSE_CALM_001_(\d+)deg_(\d+)yr_(simple|discretised)\.yml')

def generate_new_content(direction: str, return_period: str, model_type: str) -> str:
    """Generate new file content based on parameters"""

    # Determine base file
    if model_type == "simple":
        base_file = "../base_files/calm_buoy_simple_base.yml"
    else:
        base_file = "../base_files/calm_buoy_discretised_base.yml"

    # Environment file
    env_file = f"../base_files/env/_env_{direction}deg_{return_period}yr.yml"

    # Generate content
    content = f"""BaseFile: {base_file}
includefile: {env_file}
"""

    return content

def update_analysis_models():
    """Update all analysis model files"""

    if not analysis_models_dir.exists():
        print(f"ERROR: Directory not found: {analysis_models_dir}")
        return

    # Get all YAML files
    yaml_files = list(analysis_models_dir.glob("NSE_CALM_001_*.yml"))

    print(f"Found {len(yaml_files)} analysis model files to update")
    print("=" * 70)

    updated_count = 0
    skipped_count = 0
    error_count = 0

    for yaml_file in sorted(yaml_files):
        filename = yaml_file.name

        # Parse filename
        match = filename_pattern.match(filename)
        if not match:
            print(f"SKIP: {filename} - doesn't match pattern")
            skipped_count += 1
            continue

        direction, return_period, model_type = match.groups()

        # Generate new content
        new_content = generate_new_content(direction, return_period, model_type)

        # Check if file already has correct format
        try:
            with open(yaml_file, 'r') as f:
                current_content = f.read()

            if current_content.strip() == new_content.strip():
                print(f"OK  : {filename} - already correct")
                continue

            # Write new content
            with open(yaml_file, 'w') as f:
                f.write(new_content)

            print(f"UPDATE: {filename}")
            print(f"        Base: calm_buoy_{model_type}_base.yml")
            print(f"        Env:  _env_{direction}deg_{return_period}yr.yml")
            updated_count += 1

        except Exception as e:
            print(f"ERROR: {filename} - {e}")
            error_count += 1

    print("=" * 70)
    print(f"Summary:")
    print(f"  Updated: {updated_count}")
    print(f"  Skipped: {skipped_count}")
    print(f"  Errors:  {error_count}")
    print(f"  Total:   {len(yaml_files)}")

if __name__ == "__main__":
    update_analysis_models()
