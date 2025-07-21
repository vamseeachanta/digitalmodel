"""Test the new fixed-width AQWA parser."""

from src.digitalmodel.modules.marine_analysis.aqwa_reader import AQWAReader
import os
import numpy as np

def test_fixed_width_parser():
    """Test the fixed-width parser."""
    
    # Test file path
    aqwa_file = os.path.join(os.path.dirname(__file__), "tests/modules/rao_analysis/NO_DAMP_FST1_L015.LIS")
    
    # Create reader
    reader = AQWAReader()
    
    try:
        # Parse the file
        result = reader.parse_lis_file(aqwa_file)
        
        print("Parse successful!")
        print(f"Number of frequencies: {len(result['frequencies'])}")
        print(f"Number of headings: {len(result['headings'])}")
        
        print("\nAll headings found:")
        print(result['headings'])
        
        # Check if missing headings are present
        missing_headings = [-135, -90, -45]
        found_missing = []
        for h in missing_headings:
            if h in result['headings']:
                found_missing.append(h)
        
        if len(found_missing) == len(missing_headings):
            print(f"\nSUCCESS: All previously missing headings {missing_headings} are now found!")
        else:
            print(f"\nISSUE: Still missing some headings.")
            print(f"  Expected: {missing_headings}")
            print(f"  Found: {found_missing}")
            print(f"  Not found: {[h for h in missing_headings if h not in found_missing]}")
            
        # Show sample data for verification
        if len(result['frequencies']) > 0 and len(result['headings']) > 0:
            print("\nSample data verification:")
            freq_idx = 0
            freq = result['frequencies'][freq_idx]
            print(f"\nFrequency: {freq} rad/s")
            
            # Show data for all headings
            for heading in result['headings']:
                heading_idx = result['headings'].tolist().index(heading)
                print(f"\n  Heading {heading}°:")
                for dof in ['surge', 'sway', 'heave']:
                    amp = result['raos'][dof]['amplitude'][freq_idx, heading_idx]
                    phase = result['raos'][dof]['phase'][freq_idx, heading_idx]
                    print(f"    {dof}: amplitude={amp:.4f}, phase={phase:.2f}°")
                
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    test_fixed_width_parser()