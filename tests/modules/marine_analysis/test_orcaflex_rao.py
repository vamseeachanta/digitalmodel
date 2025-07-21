"""Test OrcaFlex parser."""

from src.digitalmodel.modules.marine_analysis.orcaflex_reader import OrcaFlexReader

def test_orcaflex():
    """Test the OrcaFlex parser."""
    
    parser = OrcaFlexReader()
    
    try:
        result = parser.parse_yml_file("tests/modules/rao_analysis/SS_Off_0_8P.yml")
        
        print(f"[OK] OrcaFlex parsing successful!")
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
        print(f"[ERROR] OrcaFlex parsing failed: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    test_orcaflex()