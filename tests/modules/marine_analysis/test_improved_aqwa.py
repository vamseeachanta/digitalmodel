"""Test improved AQWA parser."""

from src.digitalmodel.modules.marine_analysis.aqwa_reader_v2 import AQWAReaderV2

def test_improved():
    """Test the improved AQWA parser."""
    
    parser = AQWAReaderV2()
    
    try:
        result = parser.parse_lis_file("tests/modules/rao_analysis/NO_DAMP_FST1_L015.LIS")
        
        print(f"[OK] AQWA parsing successful!")
        print(f"  - Frequencies: {len(result['frequencies'])} points")
        print(f"  - Headings: {len(result['headings'])} directions")
        print(f"  - Frequency range: {result['frequencies'].min():.3f} to {result['frequencies'].max():.3f} rad/s")
        print(f"  - Heading range: {result['headings'].min():.1f} to {result['headings'].max():.1f} degrees")
        
        # Check data structure
        for dof in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']:
            amp_shape = result['raos'][dof]['amplitude'].shape
            print(f"  - {dof}: {amp_shape}")
        
        # Sample data
        print(f"\nSample surge amplitude at first frequency:")
        surge_data = result['raos']['surge']['amplitude'][0, :]
        for i, heading in enumerate(result['headings'][:5]):  # First 5 headings
            print(f"  {heading:6.1f}°: {surge_data[i]:.6f}")
            
        return True
        
    except Exception as e:
        print(f"[ERROR] AQWA parsing failed: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    test_improved()