#!/usr/bin/env python3
"""
ABOUTME: Fix YAML array structure in base files
ABOUTME: Ensures proper indentation: section (0), dash (2), properties (4)
"""

from pathlib import Path
import re

BASE_DIR = Path("D:/workspace-hub/digitalmodel/projects/TEST_OPERABILITY/orcaflex/base_files")

FILES_TO_FIX = [
    "_02_variable_data.yml",
    "_04_vessel_types.yml",
    "_05_line_types.yml",
    "_06_vessels_buoys.yml",
    "_07_lines.yml",
    "_08_groups.yml",
]

def fix_yaml_structure(file_path: Path, section_header: str):
    """Fix YAML structure with proper array indentation."""

    if not file_path.exists():
        print(f"  SKIP: {file_path.name} (not found)")
        return False

    content = file_path.read_text()
    lines = content.split('\n')

    # Remove existing section header if present
    if lines and lines[0].strip().endswith(':'):
        lines = lines[1:]

    # Process lines to fix indentation
    fixed_lines = [section_header]  # Add section header

    in_array = False
    for line in lines:
        stripped = line.lstrip()

        if not stripped:  # Empty line
            fixed_lines.append(line)
            continue

        # Check if this is an array item
        if stripped.startswith('- '):
            # Array marker should be indented 2 spaces under section
            fixed_lines.append('  ' + stripped)
            in_array = True
        elif in_array and stripped and not stripped.startswith('#'):
            # Array item properties should be indented 4 spaces under section (2 beyond dash)
            # Remove all leading whitespace and add correct indent
            fixed_lines.append('    ' + stripped)
        elif stripped.startswith('#'):
            # Comments - maintain relative indent of 4 spaces
            fixed_lines.append('    ' + stripped)
        else:
            # Other content
            fixed_lines.append('  ' + stripped)

    new_content = '\n'.join(fixed_lines)
    file_path.write_text(new_content)
    print(f"  FIXED: {file_path.name}")
    return True

def main():
    print("="*80)
    print("FIXING YAML ARRAY STRUCTURE IN BASE FILES")
    print("="*80)
    print("\nTarget indentation:")
    print("  Section header: 0 spaces")
    print("  Array marker (-): 2 spaces")
    print("  Array properties: 4 spaces")
    print()

    section_headers = {
        "_02_variable_data.yml": "VariableData:",
        "_04_vessel_types.yml": "VesselTypes:",
        "_05_line_types.yml": "LineTypes:",
        "_06_vessels_buoys.yml": "Vessels:",
        "_07_lines.yml": "Lines:",
        "_08_groups.yml": "Groups:",
    }

    fixed_count = 0
    for filename in FILES_TO_FIX:
        header = section_headers.get(filename, "Unknown:")
        if fix_yaml_structure(BASE_DIR / filename, header):
            fixed_count += 1

    print(f"\n{'='*80}")
    print(f"COMPLETE: Fixed {fixed_count} files")
    print(f"{'='*80}")

    print("\nNext steps:")
    print("  1. Regenerate: python scripts/create_flattened_models.py")
    print("  2. Test: python test_flattened_model.py")

if __name__ == "__main__":
    main()
