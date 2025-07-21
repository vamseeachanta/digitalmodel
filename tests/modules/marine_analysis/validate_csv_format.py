#!/usr/bin/env python3
"""Validation script to create sample CSV files matching step1 and step2 formats."""

import sys
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from src.digitalmodel.modules.marine_analysis.aqwa_enhanced_parser import AQWAEnhancedParser


def create_sample_csv_formats():
    """Create sample CSV files matching the expected step1 and step2 formats."""
    
    # Sample data matching the provided step files
    sample_data = {
        'frequencies': [0.157],
        'headings': [-180.0, -135.0, -90.0, -45.0, 0.0, 45.0, 90.0, 135.0, 180.0],
        'raos': {
            'surge': {
                'amplitude': [[1.0099, 0.7173, 0.0006, 0.7173, 1.01, 0.7174, 0.0006, 0.7173, 1.0099]],
                'phase': [[-88.22, -88.71, -0.25, 88.73, 88.22, 88.71, 1.13, -88.73, -88.22]]
            },
            'sway': {
                'amplitude': [[0, 0.7069, 1.004, 0.7068, 0, 0.7069, 1.004, 0.7068, 0]],
                'phase': [[-33.08, -88.97, -89.99, -91.01, 34.39, 88.97, 89.99, 91.01, -32.43]]
            },
            'heave': {
                'amplitude': [[0.9912, 0.9957, 1.0001, 0.9955, 0.9911, 0.9957, 1.0001, 0.9955, 0.9912]],
                'phase': [[-0.03, -0.03, -0.01, 0.01, 0.03, 0.02, 0.01, -0.01, -0.03]]
            },
            'roll': {
                'amplitude': [[0.0014, 0.2032, 0.2789, 0.2041, 0.0014, 0.2032, 0.2789, 0.2041, 0.0014]],
                'phase': [[47.25, -73.56, -89.79, -105.8, -47.25, 73.56, 89.79, 105.79, 47.25]]
            },
            'pitch': {
                'amplitude': [[0.1455, 0.1032, 0.0028, 0.1032, 0.1455, 0.1032, 0.0028, 0.1032, 0.1455]],
                'phase': [[92.26, 92.38, -179.62, -92.38, -92.26, -92.38, 179.64, 92.38, 92.26]]
            },
            'yaw': {
                'amplitude': [[0.0011, 0.1195, 0.1484, 0.121, 0.0011, 0.1196, 0.1484, 0.121, 0.0011]],
                'phase': [[-88.75, 120.73, 90.01, 59.67, 88.76, -120.73, -90.02, -59.67, -88.75]]
            }
        }
    }
    
    # Convert to numpy arrays (simplified for this demo)
    import numpy as np
    sample_data['frequencies'] = np.array(sample_data['frequencies'])
    sample_data['headings'] = np.array(sample_data['headings'])
    
    for dof in sample_data['raos']:
        sample_data['raos'][dof]['amplitude'] = np.array(sample_data['raos'][dof]['amplitude'])
        sample_data['raos'][dof]['phase'] = np.array(sample_data['raos'][dof]['phase'])
    
    # Create enhanced parser
    enhanced_parser = AQWAEnhancedParser()
    
    # Generate step1 format
    step1_csv = enhanced_parser.export_to_csv(sample_data, output_format='step1')
    
    # Generate step2 format
    step2_csv = enhanced_parser.export_to_csv(sample_data, output_format='step2')
    
    # Write to files
    output_dir = Path("tests/modules/marine_analysis")
    
    step1_output = output_dir / "generated_step1_format.csv"
    step2_output = output_dir / "generated_step2_format.csv"
    
    with open(step1_output, 'w') as f:
        f.write(step1_csv)
    
    with open(step2_output, 'w') as f:
        f.write(step2_csv)
    
    print("Sample CSV formats created:")
    print(f"  Step1 (abbreviated): {step1_output}")
    print(f"  Step2 (full): {step2_output}")
    
    # Display the formats
    print("\n" + "=" * 80)
    print("STEP1 FORMAT (ABBREVIATED - MATCHES AQWA NATIVE OUTPUT)")
    print("=" * 80)
    print(step1_csv)
    
    print("\n" + "=" * 80)
    print("STEP2 FORMAT (FULL - ALL ROWS HAVE PERIOD/FREQUENCY)")
    print("=" * 80)
    print(step2_csv)
    
    # Compare with expected files if they exist
    expected_step1 = output_dir / "NO_DAMP_FST1_L015_frequency1_block_step1.csv"
    expected_step2 = output_dir / "NO_DAMP_FST1_L015_frequency1_block_step2.csv"
    
    if expected_step1.exists() and expected_step2.exists():
        print("\n" + "=" * 80)
        print("COMPARISON WITH PROVIDED STEP FILES")
        print("=" * 80)
        
        with open(expected_step1, 'r') as f:
            expected_step1_content = f.read().strip()
        with open(expected_step2, 'r') as f:
            expected_step2_content = f.read().strip()
        
        print("\nProvided Step1 file:")
        print(expected_step1_content)
        
        print("\nProvided Step2 file:")
        print(expected_step2_content)
        
        # Analyze differences
        step1_lines = step1_csv.strip().split('\n')[1:]  # Skip header
        expected_step1_lines = expected_step1_content.split('\n')[1:]  # Skip header
        
        print(f"\nGenerated step1 lines: {len(step1_lines)}")
        print(f"Expected step1 lines: {len(expected_step1_lines)}")
        
        print("\nStep1 Format Validation:")
        print("- Both formats should have the same pattern:")
        print("  * First line: Period, Frequency, Direction, Data...")
        print("  * Subsequent lines: ,, Direction, Data...")
        
        # Show pattern matching
        for i, line in enumerate(expected_step1_lines[:3]):
            parts = line.split(',')
            period = parts[0] if parts[0] else "EMPTY"
            freq = parts[1] if parts[1] else "EMPTY"
            direction = parts[2]
            print(f"  Expected line {i+1}: Period={period}, Freq={freq}, Dir={direction}")


if __name__ == "__main__":
    create_sample_csv_formats()