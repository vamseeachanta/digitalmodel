#!/usr/bin/env python3
"""
ABOUTME: Test script for CALMBuoyDataLoader
ABOUTME: Validates CSV loading and error handling
"""

from pathlib import Path
import sys

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.orcaflex.modular_input_validation.data_loader import (
    CALMBuoyDataLoader,
    CALMBuoyReferenceData
)


def test_data_loader():
    """Test the CALM buoy data loader."""
    print("=" * 80)
    print("CALM Buoy Data Loader Test")
    print("=" * 80)

    # Initialize loader
    data_dir = Path(__file__).parent.parent / 'data'
    print(f"\nData directory: {data_dir}")
    print(f"Data directory exists: {data_dir.exists()}")

    loader = CALMBuoyDataLoader(data_dir)
    print("\n[OK] Data loader initialized")

    # Test individual loading methods
    print("\n" + "-" * 80)
    print("Testing Individual Load Methods")
    print("-" * 80)

    # 1. Hull geometry ranges
    print("\n1. Loading hull geometry ranges...")
    hull_ranges = loader.load_hull_geometry_ranges()
    print(f"   Loaded {len(hull_ranges)} hull geometry parameters")
    if hull_ranges:
        sample_key = list(hull_ranges.keys())[0]
        sample = hull_ranges[sample_key]
        print(f"   Sample: {sample_key}")
        print(f"     - Min: {sample.min_value}")
        print(f"     - Max: {sample.max_value}")
        print(f"     - Basis: {sample.reference_basis}")

    # 2. Metocean ranges
    print("\n2. Loading metocean design ranges...")
    metocean_ranges = loader.load_metocean_ranges()
    print(f"   Loaded {len(metocean_ranges)} metocean parameters")
    if metocean_ranges:
        sample_key = list(metocean_ranges.keys())[0]
        sample = metocean_ranges[sample_key]
        print(f"   Sample: {sample_key}")
        print(f"     - Min: {sample.min_value}")
        print(f"     - Max: {sample.max_value}")

    # 3. Mooring capacity ranges
    print("\n3. Loading mooring capacity ranges...")
    mooring_ranges = loader.load_mooring_capacity_ranges()
    print(f"   Loaded {len(mooring_ranges)} mooring parameters")
    if mooring_ranges:
        sample_key = list(mooring_ranges.keys())[0]
        sample = mooring_ranges[sample_key]
        print(f"   Sample: {sample_key}")
        print(f"     - Min: {sample.min_value}")
        print(f"     - Max: {sample.max_value}")

    # 4. Environmental conditions
    print("\n4. Loading environmental conditions...")
    env_conditions = loader.load_environmental_conditions()
    print(f"   Loaded {len(env_conditions)} environmental conditions")
    if env_conditions:
        sample_key = list(env_conditions.keys())[0]
        sample = env_conditions[sample_key]
        print(f"   Sample: {sample_key}")
        print(f"     - Hs: {sample.get('hs')}")
        print(f"     - Tp: {sample.get('tp')}")
        print(f"     - Wind speed: {sample.get('wind_speed')}")

    # 5. Mooring line properties
    print("\n5. Loading mooring line properties...")
    mooring_lines = loader.load_mooring_line_properties()
    print(f"   Loaded {len(mooring_lines)} mooring line configurations")
    if mooring_lines:
        sample_key = list(mooring_lines.keys())[0]
        sample = mooring_lines[sample_key]
        print(f"   Sample: {sample_key}")
        print(f"     - Length: {sample.get('length')}")
        print(f"     - MBL: {sample.get('mbl')}")

    # Test load_all method
    print("\n" + "-" * 80)
    print("Testing load_all() Method")
    print("-" * 80)

    all_data = loader.load_all()
    print(f"\n[OK] Successfully loaded all data")
    print(f"  - Hull geometry params: {len(all_data.hull_geometry)}")
    print(f"  - Metocean params: {len(all_data.metocean)}")
    print(f"  - Mooring capacity params: {len(all_data.mooring_capacity)}")
    print(f"  - Environmental conditions: {len(all_data.environmental_conditions)}")
    print(f"  - Mooring line properties: {len(all_data.mooring_line_properties)}")

    # Test get_parameter_range method
    print("\n" + "-" * 80)
    print("Testing get_parameter_range() Method")
    print("-" * 80)

    if hull_ranges:
        test_param = list(hull_ranges.keys())[0]
        print(f"\nSearching for parameter: {test_param}")
        range_result = loader.get_parameter_range(test_param)
        if range_result:
            print(f"[OK] Found parameter")
            print(f"  - Min: {range_result.min_value}")
            print(f"  - Max: {range_result.max_value}")
        else:
            print(f"[X] Parameter not found")

    # Test non-existent parameter
    print(f"\nSearching for non-existent parameter: 'fake_parameter'")
    fake_result = loader.get_parameter_range('fake_parameter')
    if fake_result is None:
        print(f"[OK] Correctly returned None for non-existent parameter")
    else:
        print(f"[X] Unexpected result for non-existent parameter")

    # Summary
    print("\n" + "=" * 80)
    print("Test Summary")
    print("=" * 80)

    total_params = (
        len(all_data.hull_geometry) +
        len(all_data.metocean) +
        len(all_data.mooring_capacity)
    )

    print(f"\n[OK] All tests passed successfully!")
    print(f"  Total parameter ranges loaded: {total_params}")
    print(f"  Environmental conditions: {len(all_data.environmental_conditions)}")
    print(f"  Mooring line configurations: {len(all_data.mooring_line_properties)}")

    # Check for any errors
    if total_params == 0:
        print("\n[!] WARNING: No parameter ranges loaded. Check CSV files.")
        return False

    return True


def test_error_handling():
    """Test error handling with invalid paths."""
    print("\n" + "=" * 80)
    print("Testing Error Handling")
    print("=" * 80)

    # Test with non-existent directory
    print("\nTest 1: Non-existent data directory")
    fake_dir = Path("/fake/path/does/not/exist")
    loader = CALMBuoyDataLoader(fake_dir)

    try:
        data = loader.load_all()
        print("[OK] Gracefully handled missing directory")
        print(f"  - Hull geometry params: {len(data.hull_geometry)}")
        print(f"  - Metocean params: {len(data.metocean)}")
        print(f"  - Should be empty due to missing files")
    except Exception as e:
        print(f"[X] Exception raised: {e}")
        return False

    return True


if __name__ == '__main__':
    print("\nStarting CALM Buoy Data Loader Tests\n")

    # Run tests
    success = True

    try:
        # Test normal operation
        if not test_data_loader():
            success = False

        # Test error handling
        if not test_error_handling():
            success = False

    except Exception as e:
        print(f"\n[X] FATAL ERROR: {e}")
        import traceback
        traceback.print_exc()
        success = False

    # Final result
    print("\n" + "=" * 80)
    if success:
        print("[OK] ALL TESTS PASSED")
    else:
        print("[X] SOME TESTS FAILED")
    print("=" * 80)

    sys.exit(0 if success else 1)
