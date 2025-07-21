"""Example usage of the RAO data processing module.

This script demonstrates how to:
1. Import RAO data from AQWA and OrcaFlex files
2. Convert to pandas DataFrames
3. Perform manual verification
4. Interpolate RAO data
"""

import numpy as np
import pandas as pd
from pathlib import Path
import yaml

from .rao_processor import RAODataProcessor


def example_aqwa_import():
    """Example: Import RAO data from ANSYS AQWA .lis file."""
    print("=" * 60)
    print("Example 1: ANSYS AQWA RAO Import")
    print("=" * 60)
    
    # Initialize processor
    processor = RAODataProcessor()
    
    # Import AQWA file
    aqwa_file = "tests/modules/rao_analysis/NO_DAMP_FST1_L015.LIS"
    
    try:
        # Import RAO data
        rao_data = processor.import_aqwa_lis_file(aqwa_file)
        print(f"✓ Successfully imported AQWA file: {aqwa_file}")
        print(f"  - Frequencies: {len(rao_data.frequencies)} points")
        print(f"  - Headings: {len(rao_data.headings)} directions")
        print(f"  - Frequency range: {rao_data.frequencies.min():.3f} to {rao_data.frequencies.max():.3f} rad/s")
        
        # Convert to DataFrames
        df_dict = processor.get_rao_dataframes(rao_data)
        amp_df = df_dict['amplitude']
        phase_df = df_dict['phase']
        
        print(f"\n✓ Converted to DataFrames:")
        print(f"  - Amplitude shape: {amp_df.shape}")
        print(f"  - Phase shape: {phase_df.shape}")
        print(f"  - DOFs: {amp_df.columns.get_level_values('DOF').unique().tolist()}")
        
        # Show sample data
        print("\nSample amplitude data (first 5 frequencies, surge @ 0°):")
        print(amp_df[('surge', '0.0deg')].head())
        
        return rao_data, df_dict
        
    except Exception as e:
        print(f"✗ Error importing AQWA file: {e}")
        return None, None


def example_orcaflex_import():
    """Example: Import RAO data from OrcaFlex YAML file."""
    print("\n" + "=" * 60)
    print("Example 2: OrcaFlex RAO Import")
    print("=" * 60)
    
    # Initialize processor with configuration
    config = {
        'import_settings': {
            'orcaflex': {
                'yaml_structure': {
                    'vessel_types_key': 'VesselTypes',
                    'draughts_key': 'Draughts',
                    'displacement_raos_key': 'DisplacementRAOs'
                }
            }
        }
    }
    processor = RAODataProcessor(config)
    
    # Import OrcaFlex file
    orcaflex_file = "tests/modules/rao_analysis/SS_Off_0_8P.yml"
    
    try:
        # Import RAO data
        rao_data = processor.import_orcaflex_yml_file(orcaflex_file)
        print(f"✓ Successfully imported OrcaFlex file: {orcaflex_file}")
        print(f"  - Vessel: {rao_data.vessel_name}")
        print(f"  - Frequencies: {len(rao_data.frequencies)} points")
        print(f"  - Headings: {len(rao_data.headings)} directions")
        
        # Convert to DataFrames
        df_dict = processor.get_rao_dataframes(rao_data)
        
        return rao_data, df_dict
        
    except Exception as e:
        print(f"✗ Error importing OrcaFlex file: {e}")
        return None, None


def example_manual_verification(processor, df_dict):
    """Example: Perform manual verification of RAO data."""
    print("\n" + "=" * 60)
    print("Example 3: Manual Verification")
    print("=" * 60)
    
    if df_dict is None:
        print("✗ No data available for verification")
        return
    
    # Create verification report
    report = processor.create_verification_report(df_dict)
    
    print("Verification Report:")
    print(f"  - Amplitude check required: {report.amplitude_check_required}")
    print(f"  - Phase continuity check required: {report.phase_continuity_check_required}")
    print(f"  - Suspicious amplitudes found: {len(report.suspicious_amplitudes)}")
    print(f"  - Phase discontinuities found: {len(report.phase_discontinuities)}")
    
    # Generate spot checks
    spot_checks = processor.generate_spot_check_values(df_dict['amplitude'], n_samples=5)
    
    print("\nSpot Check Samples:")
    for i, check in enumerate(spot_checks, 1):
        print(f"  {i}. {check['location_in_source']}: {check['value']:.4f}")
    
    # Check phase continuity
    phase_issues = processor.check_phase_continuity(df_dict['phase'])
    if phase_issues:
        print(f"\n⚠ Found {len(phase_issues)} phase discontinuities")
        for issue in phase_issues[:3]:  # Show first 3
            print(f"  - {issue['dof']} @ {issue['heading']}: "
                  f"Jump of {issue['phase_jump']:.1f}° at freq index {issue['frequency_index']}")


def example_interpolation(processor, rao_data):
    """Example: Interpolate RAO data to new grid."""
    print("\n" + "=" * 60)
    print("Example 4: RAO Interpolation")
    print("=" * 60)
    
    if rao_data is None:
        print("✗ No data available for interpolation")
        return
    
    # Define target grid
    target_frequencies = np.linspace(0.1, 2.0, 40)  # 40 points
    target_headings = np.arange(0, 360, 10)  # Every 10 degrees
    
    print(f"Original grid: {len(rao_data.frequencies)} × {len(rao_data.headings)}")
    print(f"Target grid: {len(target_frequencies)} × {len(target_headings)}")
    
    # Interpolate
    interp_data = processor.interpolate_rao_data(
        rao_data, target_frequencies, target_headings
    )
    
    print(f"\n✓ Interpolation complete:")
    print(f"  - New shape: {len(interp_data.frequencies)} × {len(interp_data.headings)}")
    print(f"  - Interpolation method: {interp_data.metadata['interpolation']['method']}")
    
    # Convert interpolated data to DataFrame
    interp_df = processor.get_rao_dataframes(interp_data)
    print(f"  - DataFrame shape: {interp_df['amplitude'].shape}")
    
    return interp_data


def main():
    """Run all examples."""
    print("RAO Data Processing Examples")
    print("=" * 60)
    
    # Create processor
    processor = RAODataProcessor()
    
    # Example 1: AQWA import
    aqwa_data, aqwa_df = example_aqwa_import()
    
    # Example 2: OrcaFlex import
    orcaflex_data, orcaflex_df = example_orcaflex_import()
    
    # Example 3: Manual verification (using AQWA data)
    if aqwa_df:
        example_manual_verification(processor, aqwa_df)
    
    # Example 4: Interpolation (using AQWA data)
    if aqwa_data:
        interp_data = example_interpolation(processor, aqwa_data)
    
    print("\n" + "=" * 60)
    print("Examples complete!")
    
    # Save sample output
    if aqwa_df:
        print("\nSaving sample output to 'rao_amplitude_sample.csv'...")
        aqwa_df['amplitude'].head(10).to_csv('rao_amplitude_sample.csv')
        print("✓ Sample data saved")


if __name__ == "__main__":
    main()