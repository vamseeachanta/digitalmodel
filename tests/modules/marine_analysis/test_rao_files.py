"""Test RAO functionality with real data files."""

import numpy as np
from pathlib import Path
from src.digitalmodel.modules.marine_analysis import RAODataProcessor

def test_orcaflex_file():
    """Test OrcaFlex YAML file import."""
    print("Testing OrcaFlex YAML file import...")
    
    processor = RAODataProcessor()
    orcaflex_file = "tests/modules/rao_analysis/SS_Off_0_8P.yml"
    
    if not Path(orcaflex_file).exists():
        print(f"[SKIP] File not found: {orcaflex_file}")
        return
    
    try:
        # Import RAO data
        rao_data = processor.import_orcaflex_yml_file(orcaflex_file)
        print(f"[OK] Successfully imported OrcaFlex file")
        print(f"  - Vessel: {rao_data.vessel_name}")
        print(f"  - Frequencies: {len(rao_data.frequencies)} points")
        print(f"  - Headings: {len(rao_data.headings)} directions")
        
        # Convert to DataFrames
        df_dict = processor.get_rao_dataframes(rao_data)
        print(f"[OK] Converted to DataFrames: {df_dict['amplitude'].shape}")
        
        return True
        
    except Exception as e:
        print(f"[ERROR] Failed to import OrcaFlex file: {e}")
        import traceback
        traceback.print_exc()
        return False


def test_aqwa_file():
    """Test AQWA .lis file import."""
    print("\nTesting AQWA .lis file import...")
    
    processor = RAODataProcessor()
    aqwa_file = "tests/modules/rao_analysis/NO_DAMP_FST1_L015.LIS"
    
    if not Path(aqwa_file).exists():
        print(f"[SKIP] File not found: {aqwa_file}")
        return
    
    try:
        # Import RAO data
        rao_data = processor.import_aqwa_lis_file(aqwa_file)
        print(f"[OK] Successfully imported AQWA file")
        print(f"  - Frequencies: {len(rao_data.frequencies)} points")
        print(f"  - Headings: {len(rao_data.headings)} directions")
        print(f"  - Frequency range: {rao_data.frequencies.min():.3f} to {rao_data.frequencies.max():.3f} rad/s")
        
        # Convert to DataFrames
        df_dict = processor.get_rao_dataframes(rao_data)
        print(f"[OK] Converted to DataFrames: {df_dict['amplitude'].shape}")
        
        # Show sample data
        amp_df = df_dict['amplitude']
        print(f"[OK] Sample amplitude data (surge @ 0deg):")
        surge_0deg = amp_df.loc[:, ('surge', '0.0deg')].head()
        for freq, val in surge_0deg.items():
            print(f"  {freq:.3f} rad/s: {val:.4f}")
        
        return True
        
    except Exception as e:
        print(f"[ERROR] Failed to import AQWA file: {e}")
        import traceback
        traceback.print_exc()
        return False


def main():
    """Run file tests."""
    print("RAO Data File Import Tests")
    print("=" * 40)
    
    # Test files
    orcaflex_success = test_orcaflex_file()
    aqwa_success = test_aqwa_file()
    
    print("\n" + "=" * 40)
    print("Test Results:")
    print(f"  OrcaFlex import: {'PASS' if orcaflex_success else 'FAIL'}")
    print(f"  AQWA import: {'PASS' if aqwa_success else 'FAIL'}")


if __name__ == "__main__":
    main()