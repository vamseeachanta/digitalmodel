#!/usr/bin/env python3
"""
ABOUTME: Fix indentation in base files after adding section headers
ABOUTME: Indents all content under section headers by 2 spaces
"""

from pathlib import Path

# Base files directory
BASE_DIR = Path("D:/workspace-hub/digitalmodel/projects/TEST_OPERABILITY/orcaflex/base_files")

# Files to fix
FILES_TO_FIX = [
    "_02_variable_data.yml",
    "_04_vessel_types.yml",
    "_05_line_types.yml",
    "_06_vessels_buoys.yml",
    "_06_buoys_discretised.yml",
    "_07_lines.yml",
    "_07_lines_discretised.yml",
    "_08_groups.yml",
    "_08_groups_discretised.yml",
]

def fix_indentation(file_path: Path):
    """Fix indentation in file - indent all lines except first by 2 spaces."""

    if not file_path.exists():
        print(f"  SKIP: {file_path.name} (not found)")
        return False

    lines = file_path.read_text().split('\n')

    if not lines:
        return False

    # First line should be section header (e.g., "VariableData:")
    # All other lines should be indented by 2 spaces
    fixed_lines = [lines[0]]  # Keep first line as-is

    for line in lines[1:]:
        if line.strip():  # Non-empty line
            # Add 2 spaces indent if not already indented properly
            if not line.startswith('  '):
                line = '  ' + line
        fixed_lines.append(line)

    new_content = '\n'.join(fixed_lines)
    file_path.write_text(new_content)
    print(f"  FIXED: {file_path.name}")
    return True

def main():
    print("="*80)
    print("FIXING INDENTATION IN BASE FILES")
    print("="*80)
    print(f"\nBase directory: {BASE_DIR}")

    fixed_count = 0

    for filename in FILES_TO_FIX:
        file_path = BASE_DIR / filename
        if fix_indentation(file_path):
            fixed_count += 1

    print(f"\n{'='*80}")
    print(f"COMPLETE: Fixed {fixed_count} files")
    print(f"{'='*80}")

    print("\nNext steps:")
    print("  1. Regenerate flattened models: python scripts/create_flattened_models.py")
    print("  2. Test: python test_flattened_model.py")

if __name__ == "__main__":
    main()
