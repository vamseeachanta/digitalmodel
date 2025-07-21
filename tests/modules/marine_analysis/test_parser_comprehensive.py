"""Comprehensive test of the AQWA parser."""

from src.digitalmodel.modules.marine_analysis.aqwa_reader import AQWAReader
import os

def test_comprehensive():
    """Test the parser comprehensively."""
    
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
        
        print("\nFrequencies:", result['frequencies'][:10], "..." if len(result['frequencies']) > 10 else "")
        print("\nHeadings:", result['headings'])
        
        # Check specific frequency
        freq_idx = 0  # First frequency
        print(f"\nFor frequency {result['frequencies'][freq_idx]}:")
        
        # Check all DOFs
        for dof in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']:
            amp = result['raos'][dof]['amplitude'][freq_idx, :]
            phase = result['raos'][dof]['phase'][freq_idx, :]
            print(f"  {dof}: {len(amp)} heading values")
            
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    test_comprehensive()