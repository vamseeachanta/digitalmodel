#!/usr/bin/env python3
"""
ABOUTME: Add section headers to base files to match OrcaFlex reference pattern
ABOUTME: Prepends appropriate section header to each base file
"""

from pathlib import Path

# Base files directory
BASE_DIR = Path("D:/workspace-hub/digitalmodel/projects/TEST_OPERABILITY/orcaflex/base_files")

# Section headers to add to each file
SECTION_HEADERS = {
    "_02_variable_data.yml": "VariableData:",
    "_04_vessel_types.yml": "VesselTypes:",
    "_05_line_types.yml": "LineTypes:",
    "_06_vessels_buoys.yml": "Vessels:",
    "_06_buoys_discretised.yml": "Buoys:",
    "_07_lines.yml": "Lines:",
    "_07_lines_discretised.yml": "Lines:",
    "_08_groups.yml": "Groups:",
    "_08_groups_discretised.yml": "Groups:",
}

def add_section_header(file_path: Path, section_header: str):
    """Add section header to a file if it doesn't already have one."""

    if not file_path.exists():
        print(f"  SKIP: {file_path.name} (file not found)")
        return False

    content = file_path.read_text()

    # Check if file already has the section header
    if content.strip().startswith(section_header):
        print(f"  SKIP: {file_path.name} (already has header)")
        return False

    # Add section header
    new_content = f"{section_header}\n{content}"

    file_path.write_text(new_content)
    print(f"  ADDED: {file_path.name} <- {section_header}")
    return True

def main():
    print("="*80)
    print("ADDING SECTION HEADERS TO BASE FILES")
    print("="*80)
    print(f"\nBase directory: {BASE_DIR}")

    modified_count = 0

    for filename, header in SECTION_HEADERS.items():
        file_path = BASE_DIR / filename
        if add_section_header(file_path, header):
            modified_count += 1

    print(f"\n{'='*80}")
    print(f"COMPLETE: Modified {modified_count} files")
    print(f"{'='*80}")

    print("\nNext steps:")
    print("  1. Regenerate flattened models: python scripts/create_flattened_models.py")
    print("  2. Update main model to use root-level includefiles")
    print("  3. Test: python test_flattened_model.py")

if __name__ == "__main__":
    main()
