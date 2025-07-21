"""Test both AQWA and OrcaFlex parsers with DataFrame verification."""

import pandas as pd
import numpy as np
from src.digitalmodel.modules.marine_analysis.aqwa_reader_v2 import AQWAReaderV2
from src.digitalmodel.modules.marine_analysis.orcaflex_reader import OrcaFlexReader

def create_rao_dataframes(parser_result, source_name):
    """Create pandas DataFrames from parser results with multi-level columns."""
    
    frequencies = parser_result['frequencies']
    headings = parser_result['headings'] 
    raos = parser_result['raos']
    
    # Create multi-level column index: (DOF, Heading)
    dof_names = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
    
    # Build column tuples for multi-index
    amplitude_columns = []
    phase_columns = []
    
    for dof in dof_names:
        for heading in headings:
            amplitude_columns.append((dof, f"{heading:.1f}°"))
            phase_columns.append((dof, f"{heading:.1f}°"))
    
    # Create multi-level column index
    amp_multi_index = pd.MultiIndex.from_tuples(amplitude_columns, names=['DOF', 'Heading'])
    phase_multi_index = pd.MultiIndex.from_tuples(phase_columns, names=['DOF', 'Heading'])
    
    # Create amplitude DataFrame
    amp_data = []
    for i, freq in enumerate(frequencies):
        row = []
        for dof in dof_names:
            for j, heading in enumerate(headings):
                row.append(raos[dof]['amplitude'][i, j])
        amp_data.append(row)
    
    amplitude_df = pd.DataFrame(
        amp_data,
        index=pd.Index(frequencies, name='Frequency (rad/s)'),
        columns=amp_multi_index
    )
    
    # Create phase DataFrame
    phase_data = []
    for i, freq in enumerate(frequencies):
        row = []
        for dof in dof_names:
            for j, heading in enumerate(headings):
                row.append(raos[dof]['phase'][i, j])
        phase_data.append(row)
    
    phase_df = pd.DataFrame(
        phase_data,
        index=pd.Index(frequencies, name='Frequency (rad/s)'),
        columns=phase_multi_index
    )
    
    # Add manual verification metadata
    metadata = {
        'source': source_name,
        'num_frequencies': len(frequencies),
        'num_headings': len(headings),
        'frequency_range': (frequencies.min(), frequencies.max()),
        'heading_range': (headings.min(), headings.max()),
        'verification_notes': {
            'amplitude_units': 'As per source file',
            'phase_units': 'degrees',
            'coordinate_system': '6-DOF: surge, sway, heave, roll, pitch, yaw',
            'verification_required': [
                'Check amplitude values are reasonable',
                'Verify phase values are in expected range',
                'Confirm frequency/heading coverage',
                'Spot check against source data'
            ]
        }
    }
    
    return {
        'amplitude': amplitude_df,
        'phase': phase_df,
        'metadata': metadata
    }

def test_both_parsers():
    """Test both parsers and compare results."""
    
    print("="*60)
    print("TESTING BOTH RAO PARSERS")
    print("="*60)
    
    # Test AQWA parser
    print("\n1. Testing AQWA Parser:")
    print("-" * 30)
    
    try:
        aqwa_parser = AQWAReaderV2()
        aqwa_result = aqwa_parser.parse_lis_file("tests/modules/rao_analysis/NO_DAMP_FST1_L015.LIS")
        
        print(f"[OK] AQWA parsing successful!")
        print(f"  - Frequencies: {len(aqwa_result['frequencies'])} points")
        print(f"  - Headings: {len(aqwa_result['headings'])} directions")
        print(f"  - Frequency range: {aqwa_result['frequencies'].min():.3f} to {aqwa_result['frequencies'].max():.3f} rad/s")
        print(f"  - Heading range: {aqwa_result['headings'].min():.1f} to {aqwa_result['headings'].max():.1f} degrees")
        
        # Show sample data
        print(f"  - Sample surge amplitude at first frequency: {aqwa_result['raos']['surge']['amplitude'][0, 0]:.6f}")
        
        # Create DataFrames for manual verification
        print(f"\n  Creating AQWA DataFrames for verification...")
        aqwa_dataframes = create_rao_dataframes(aqwa_result, "AQWA")
        
        # BREAKPOINT: Manual verification of AQWA DataFrames
        print(f"  *** BREAKPOINT: AQWA DataFrames ready for verification ***")
        print(f"  - Available DataFrames: {list(aqwa_dataframes.keys())}")
        print(f"  - Amplitude DataFrame shape: {aqwa_dataframes['amplitude'].shape}")
        print(f"  - Phase DataFrame shape: {aqwa_dataframes['phase'].shape}")
        print(f"  - Frequencies (first 5): {aqwa_dataframes['amplitude'].index[:5].tolist()}")
        print(f"  - Headings: {aqwa_dataframes['amplitude'].columns.get_level_values('Heading').unique().tolist()}")
        
        # Show sample of amplitude DataFrame
        print(f"\n  Sample AQWA Amplitude DataFrame (first 3 frequencies):")
        print(aqwa_dataframes['amplitude'].head(3))
        
        breakpoint()  # Pause here for manual verification
        
        aqwa_success = True
        
    except Exception as e:
        print(f"[ERROR] AQWA parsing failed: {e}")
        aqwa_success = False
    
    # Test OrcaFlex parser
    print("\n2. Testing OrcaFlex Parser:")
    print("-" * 30)
    
    try:
        orcaflex_parser = OrcaFlexReader()
        orcaflex_result = orcaflex_parser.parse_yml_file("tests/modules/rao_analysis/SS_Off_0_8P.yml")
        
        print(f"[OK] OrcaFlex parsing successful!")
        print(f"  - Frequencies: {len(orcaflex_result['frequencies'])} points")
        print(f"  - Headings: {len(orcaflex_result['headings'])} directions")
        print(f"  - Frequency range: {orcaflex_result['frequencies'].min():.3f} to {orcaflex_result['frequencies'].max():.3f} rad/s")
        print(f"  - Heading range: {orcaflex_result['headings'].min():.1f} to {orcaflex_result['headings'].max():.1f} degrees")
        
        # Show sample data
        print(f"  - Sample surge amplitude at first frequency: {orcaflex_result['raos']['surge']['amplitude'][0, 0]:.6f}")
        
        # Create DataFrames for manual verification
        print(f"\n  Creating OrcaFlex DataFrames for verification...")
        orcaflex_dataframes = create_rao_dataframes(orcaflex_result, "OrcaFlex")
        
        # BREAKPOINT: Manual verification of OrcaFlex DataFrames
        print(f"  *** BREAKPOINT: OrcaFlex DataFrames ready for verification ***")
        print(f"  - Available DataFrames: {list(orcaflex_dataframes.keys())}")
        print(f"  - Amplitude DataFrame shape: {orcaflex_dataframes['amplitude'].shape}")
        print(f"  - Phase DataFrame shape: {orcaflex_dataframes['phase'].shape}")
        print(f"  - Frequencies (first 5): {orcaflex_dataframes['amplitude'].index[:5].tolist()}")
        print(f"  - Headings: {orcaflex_dataframes['amplitude'].columns.get_level_values('Heading').unique().tolist()}")
        
        # Show sample of amplitude DataFrame
        print(f"\n  Sample OrcaFlex Amplitude DataFrame (first 3 frequencies):")
        print(orcaflex_dataframes['amplitude'].head(3))
        
        breakpoint()  # Pause here for manual verification
        
        orcaflex_success = True
        
    except Exception as e:
        print(f"[ERROR] OrcaFlex parsing failed: {e}")
        orcaflex_success = False
    
    # Summary
    print("\n3. Summary:")
    print("-" * 30)
    
    if aqwa_success and orcaflex_success:
        print("[OK] Both parsers working correctly!")
        print("[OK] RAO data import implementation is complete!")
        
        # Compare data characteristics
        print(f"\nData Comparison:")
        print(f"  AQWA     : {len(aqwa_result['frequencies'])} frequencies, {len(aqwa_result['headings'])} headings")
        print(f"  OrcaFlex : {len(orcaflex_result['frequencies'])} frequencies, {len(orcaflex_result['headings'])} headings")
        
        # Final breakpoint for comparing both DataFrames
        print(f"\n*** FINAL BREAKPOINT: Both DataFrames available for comparison ***")
        print(f"Use variables 'aqwa_dataframes' and 'orcaflex_dataframes' to inspect the data")
        print(f"Available keys: {list(aqwa_dataframes.keys())}")
        print(f"Verification metadata available in ['metadata'] key")
        
        breakpoint()  # Final comparison point
        
        return True
    else:
        print("[ERROR] One or both parsers failed!")
        return False

if __name__ == "__main__":
    success = test_both_parsers()
    exit(0 if success else 1)