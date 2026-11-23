#!/usr/bin/env python3
"""
ABOUTME: Add section headers to base files with proper indentation
ABOUTME: Prepends section header and indents ALL original content by 2 spaces
"""

from pathlib import Path

BASE_DIR = Path("D:/workspace-hub/digitalmodel/projects/TEST_OPERABILITY/orcaflex/base_files")

HEADERS = {
    "_02_variable_data.yml": "VariableData:",
    "_04_vessel_types.yml": "VesselTypes:",
    "_05_line_types.yml": "LineTypes:",
    "_06_vessels_buoys.yml": "Vessels:",
    "_07_lines.yml": "Lines:",
    "_08_groups.yml": "Groups:",
}

def add_header_with_indent(file_path: Path, header: str):
    """Add section header and indent all content."""
    if not file_path.exists():
        return False

    content = file_path.read_text()

    # Indent every line by 2 spaces
    indented_lines = []
    for line in content.split('\n'):
        indented_lines.append('  ' + line if line else line)

    new_content = header + '\n' + '\n'.join(indented_lines)
    file_path.write_text(new_content)
    print(f"  FIXED: {file_path.name}")
    return True

def main():
    print("="*80)
    print("ADDING SECTION HEADERS WITH PROPER INDENTATION")
    print("="*80)

    for filename, header in HEADERS.items():
        add_header_with_indent(BASE_DIR / filename, header)

    print("\n" + "="*80)
    print("COMPLETE")
    print("="*80)

if __name__ == "__main__":
    main()
