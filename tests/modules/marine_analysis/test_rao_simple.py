"""Simple test of RAO functionality."""

import numpy as np
from src.digitalmodel.modules.marine_analysis import RAODataProcessor, RAOData

def test_basic_functionality():
    """Test basic RAO data processor functionality."""
    print("Testing basic RAO functionality...")
    
    # Create sample RAO data
    frequencies = np.linspace(0.1, 2.0, 10)
    headings = np.arange(0, 360, 30)
    
    rao_data = RAOData(
        frequencies=frequencies,
        headings=headings,
        raos={
            dof: {
                'amplitude': np.random.rand(len(frequencies), len(headings)),
                'phase': np.random.uniform(-180, 180, (len(frequencies), len(headings)))
            }
            for dof in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        },
        source_file="test_file.dat"
    )
    
    print(f"[OK] Created RAO data with {len(frequencies)} frequencies and {len(headings)} headings")
    
    # Create processor
    processor = RAODataProcessor()
    
    # Test DataFrame conversion
    df_dict = processor.get_rao_dataframes(rao_data)
    
    print(f"[OK] Created DataFrames:")
    print(f"  - Amplitude shape: {df_dict['amplitude'].shape}")
    print(f"  - Phase shape: {df_dict['phase'].shape}")
    print(f"  - Verification info present: {'verification_info' in df_dict}")
    
    # Test verification report
    report = processor.create_verification_report(df_dict)
    print(f"[OK] Created verification report")
    
    # Test spot checks
    spot_checks = processor.generate_spot_check_values(df_dict['amplitude'], n_samples=5)
    print(f"[OK] Generated {len(spot_checks)} spot checks")
    
    # Test phase continuity check
    phase_issues = processor.check_phase_continuity(df_dict['phase'])
    print(f"[OK] Phase continuity check found {len(phase_issues)} issues")
    
    print("\n[OK] All basic functionality tests passed!")


if __name__ == "__main__":
    test_basic_functionality()