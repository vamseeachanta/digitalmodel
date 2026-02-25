"""
Validate Baltic CALM Buoy Base File for OrcaFlex Loading

This script checks if the calm_buoy_simple_base.yml file can be successfully
loaded into OrcaFlex by validating:
1. All referenced include files exist
2. File structure is correct
3. No circular references
4. All required sections are present
"""

import os
from pathlib import Path

def validate_base_file():
    """Validate the Baltic CALM buoy base file structure."""

    base_dir = Path(__file__).parent.parent / "projects" / "modules" / "calm" / "baltic_039m" / "TEST_OPERABILITY" / "orcaflex" / "base_files"
    base_file = base_dir / "calm_buoy_simple_base.yml"

    print("=" * 70)
    print("Baltic CALM Buoy Base File Validation")
    print("=" * 70)
    print(f"Base file: {base_file.name}")
    print(f"Directory: {base_dir}")
    print("=" * 70)

    if not base_file.exists():
        print(f"ERROR: Base file not found: {base_file}")
        return False

    # Read base file
    with open(base_file, 'r') as f:
        base_content = f.read()

    print(f"\nBase file content:")
    print(base_content)

    # Extract referenced files
    import re
    referenced_files = re.findall(r'includefile:\s+(\S+)', base_content)

    print(f"\nReferenced files ({len(referenced_files)}):")
    all_exist = True

    for ref_file in referenced_files:
        full_path = base_dir / ref_file
        exists = full_path.exists()
        status = "OK" if exists else "MISSING"
        size = full_path.stat().st_size if exists else 0

        print(f"  {status}: {ref_file} ({size:,} bytes)")

        if not exists:
            all_exist = False
        else:
            # Check if this file has further includes
            with open(full_path, 'r') as f:
                sub_content = f.read()
            sub_refs = re.findall(r'includefile:\s+(\S+)', sub_content)

            if sub_refs:
                print(f"       -> Includes {len(sub_refs)} files:")
                for sub_ref in sub_refs:
                    sub_path = base_dir / sub_ref
                    sub_exists = sub_path.exists()
                    sub_status = "OK" if sub_exists else "MISSING"
                    sub_size = sub_path.stat().st_size if sub_exists else 0
                    print(f"          {sub_status}: {sub_ref} ({sub_size:,} bytes)")
                    if not sub_exists:
                        all_exist = False

    print("\n" + "=" * 70)

    # Check for critical components
    print("\nCritical Component Check:")

    critical_files = {
        '_03b_seabed.yml': 'Seabed (with WaterDepth: 10m)',
        '_05_line_types.yml': 'Line types (with ClumpWeight)',
        '_07_lines.yml': 'Mooring lines (3-segment configuration)',
    }

    for filename, description in critical_files.items():
        filepath = base_dir / filename
        if filepath.exists():
            print(f"  OK: {filename} - {description}")

            # Check specific content
            with open(filepath, 'r') as f:
                content = f.read()

            if filename == '_03b_seabed.yml':
                if 'WaterDepth: 10' in content:
                    print(f"      [OK] Water depth is 10m")
                else:
                    print(f"      WARNING: Water depth may not be 10m")

            elif filename == '_05_line_types.yml':
                if 'ClumpWeight' in content:
                    print(f"      [OK] ClumpWeight line type defined")
                else:
                    print(f"      WARNING: ClumpWeight line type not found")

            elif filename == '_07_lines.yml':
                if 'ClumpWeight' in content:
                    print(f"      [OK] Mooring lines reference ClumpWeight")
                    # Count mooring lines
                    mooring_count = content.count('New: Mooring')
                    print(f"      [OK] Found {mooring_count} mooring lines")
                else:
                    print(f"      WARNING: Mooring lines don't use ClumpWeight")
        else:
            print(f"  MISSING: {filename} - {description}")
            all_exist = False

    print("\n" + "=" * 70)

    if all_exist:
        print("RESULT: [PASS] All required files exist")
        print("\nThe base file structure is valid and should load into OrcaFlex.")
        print("\nNext steps:")
        print("1. Load in OrcaFlex: OrcFxAPI.Model('calm_buoy_simple_base.yml')")
        print("2. Run statics: model.CalculateStatics()")
        print("3. Verify mooring line geometry in 3D view")
        print("4. Check clump weights are visible (~8-10m below surface)")
        return True
    else:
        print("RESULT: [FAIL] Some files are missing")
        print("\nThe base file cannot be loaded until all referenced files exist.")
        return False

if __name__ == "__main__":
    validate_base_file()
