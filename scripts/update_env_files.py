"""
Update OrcaFlex Environment Files

This script updates all environment files to use the correct relative paths.
The _env_*.yml files reference waves, current, and wind files in the same directory.

Pattern: _env_{direction}deg_{return_period}yr.yml
References: waves_{direction}deg_{return_period}yr.yml
            current_{direction}deg_{return_period}yr.yml
            wind_{direction}deg_{return_period}yr.yml
"""

import os
import re
from pathlib import Path

# Base directory
env_dir = Path(__file__).parent.parent / "projects" / "TEST_OPERABILITY" / "orcaflex" / "base_files" / "env"

# Pattern to extract direction and return period from filename
filename_pattern = re.compile(r'_env_(\d+)deg_(\d+)yr\.yml')

def generate_new_content(direction: str, return_period: str) -> str:
    """Generate new file content based on parameters"""

    # Use the same path format as the manually fixed file
    # Path is relative from where the model is loaded (analysis_models directory)
    waves_file = f"../base_files/env/waves_{direction}deg_{return_period}yr.yml"
    current_file = f"../base_files/env/current_{direction}deg_{return_period}yr.yml"
    wind_file = f"../base_files/env/wind_{direction}deg_{return_period}yr.yml"

    # Generate content
    content = f"""Environment:
  - includefile: {waves_file}
  - includefile: {current_file}
  - includefile: {wind_file}
"""

    return content

def update_env_files():
    """Update all environment files"""

    if not env_dir.exists():
        print(f"ERROR: Directory not found: {env_dir}")
        return

    # Get all _env_*.yml files
    env_files = list(env_dir.glob("_env_*.yml"))

    print(f"Found {len(env_files)} environment files to update")
    print("=" * 70)

    updated_count = 0
    skipped_count = 0
    error_count = 0

    for env_file in sorted(env_files):
        filename = env_file.name

        # Parse filename
        match = filename_pattern.match(filename)
        if not match:
            print(f"SKIP: {filename} - doesn't match pattern")
            skipped_count += 1
            continue

        direction, return_period = match.groups()

        # Generate new content
        new_content = generate_new_content(direction, return_period)

        # Check if file already has correct format
        try:
            with open(env_file, 'r') as f:
                current_content = f.read()

            if current_content.strip() == new_content.strip():
                print(f"OK  : {filename} - already correct")
                continue

            # Write new content
            with open(env_file, 'w') as f:
                f.write(new_content)

            print(f"UPDATE: {filename}")
            print(f"        References: waves_{direction}deg_{return_period}yr.yml")
            print(f"                    current_{direction}deg_{return_period}yr.yml")
            print(f"                    wind_{direction}deg_{return_period}yr.yml")
            updated_count += 1

        except Exception as e:
            print(f"ERROR: {filename} - {e}")
            error_count += 1

    print("=" * 70)
    print(f"Summary:")
    print(f"  Updated: {updated_count}")
    print(f"  Skipped: {skipped_count}")
    print(f"  Errors:  {error_count}")
    print(f"  Total:   {len(env_files)}")

if __name__ == "__main__":
    update_env_files()
