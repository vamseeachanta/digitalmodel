#!/usr/bin/env python3
"""
Test AQWA .LIS Parser with Real Data

Quick test to verify the AQWA parser works with real .LIS files.
"""

from pathlib import Path
from digitalmodel.diffraction.aqwa_lis_parser import parse_aqwa_lis_file
from digitalmodel.diffraction import AQWAConverter, validate_results, OrcaFlexExporter

def test_aqwa_parser():
    """Test AQWA parser with real .LIS file"""

    # Use the real .LIS file we found
    lis_file = Path("docs/modules/aqwa/examples/03_dat/001_ship_raos/001_SHIP_RAOS.LIS")

    if not lis_file.exists():
        print(f"[ERROR] .LIS file not found: {lis_file}")
        return False

    print("=" * 80)
    print("Testing AQWA .LIS Parser")
    print("=" * 80)
    print(f"File: {lis_file}")
    print()

    try:
        # Test 1: Parse .LIS file directly
        print("[1/4] Parsing .LIS file...")
        parsed_data = parse_aqwa_lis_file(lis_file)

        print(f"[OK] Parsed successfully")
        print(f"  - Frequencies: {len(parsed_data['frequencies'])}")
        print(f"  - Headings: {len(parsed_data['headings'])}")
        print(f"  - Added mass matrices: {len(parsed_data['added_mass'])}")
        print(f"  - Damping matrices: {len(parsed_data['damping'])}")
        print(f"  - RAO data points: {len(parsed_data['raos'])}")
        print()

        # Test 2: Use AQWAConverter
        print("[2/4] Testing AQWAConverter...")
        analysis_folder = lis_file.parent
        vessel_name = "SHIP_RAOS"  # Generic name

        converter = AQWAConverter(analysis_folder, vessel_name)
        results = converter.convert_to_unified_schema(water_depth=100.0)  # Arbitrary depth

        print(f"[OK] Conversion successful")
        print(f"  - Vessel: {results.vessel_name}")
        print(f"  - Tool: {results.analysis_tool}")
        print(f"  - Water depth: {results.water_depth} m")
        print()

        # Test 3: Validate results
        print("[3/4] Validating results...")
        validation = validate_results(results)

        print(f"[OK] Validation status: {validation['overall_status']}")
        if 'issues_found' in validation:
            print(f"  - {validation['issues_found']} issues found")
        print()

        # Test 4: Quick export test (just vessel type YAML)
        print("[4/4] Testing export...")
        output_dir = Path("test_outputs/aqwa_parser_test")
        output_dir.mkdir(parents=True, exist_ok=True)

        exporter = OrcaFlexExporter(results, output_dir)
        vessel_file = exporter.export_vessel_type()

        print(f"[OK] Exported vessel type: {vessel_file.name}")
        print()

        print("=" * 80)
        print("[SUCCESS] All tests passed!")
        print("=" * 80)

        return True

    except Exception as e:
        print(f"\n[ERROR] Test failed: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


if __name__ == "__main__":
    success = test_aqwa_parser()
    exit(0 if success else 1)
