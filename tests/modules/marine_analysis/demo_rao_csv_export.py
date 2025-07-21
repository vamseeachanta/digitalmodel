#!/usr/bin/env python3
"""Demonstration script for RAO CSV export in step1/step2 formats."""

import sys
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from src.digitalmodel.modules.marine_analysis.aqwa_reader import AQWAReader
from src.digitalmodel.modules.marine_analysis.aqwa_enhanced_parser import AQWAEnhancedParser


def demo_rao_csv_export():
    """Demonstrate RAO CSV export functionality."""
    # File paths
    test_data_dir = Path("tests/modules/rao_analysis")
    aqwa_file = test_data_dir / "NO_DAMP_FST1_L015.LIS"
    step1_expected = test_data_dir / "NO_DAMP_FST1_L015_frequency1_block_step1.csv"
    step2_expected = test_data_dir / "NO_DAMP_FST1_L015_frequency1_block_step2.csv"
    
    if not aqwa_file.exists():
        print(f"AQWA file not found: {aqwa_file}")
        return
    
    # Initialize parsers
    aqwa_reader = AQWAReader()
    enhanced_parser = AQWAEnhancedParser()
    
    try:
        print("=" * 60)
        print("RAO DATA IMPORT AND PROCESSING DEMONSTRATION")
        print("=" * 60)
        
        # Parse AQWA file
        print(f"\n1. Parsing AQWA file: {aqwa_file.name}")
        rao_data = aqwa_reader.parse_lis_file(str(aqwa_file))
        
        print(f"   - Frequencies: {len(rao_data['frequencies'])}")
        print(f"   - Headings: {len(rao_data['headings'])}")
        print(f"   - DOF: {list(rao_data['raos'].keys())}")
        
        # Show frequency range
        freq_min = min(rao_data['frequencies'])
        freq_max = max(rao_data['frequencies'])
        print(f"   - Frequency range: {freq_min:.3f} to {freq_max:.3f} rad/s")
        
        # Show heading range
        head_min = min(rao_data['headings'])
        head_max = max(rao_data['headings'])
        print(f"   - Heading range: {head_min:.0f} to {head_max:.0f} degrees")
        
        # Export to Step1 format (abbreviated)
        print(f"\n2. Exporting to Step1 format (abbreviated - empty period/freq)")
        step1_csv = enhanced_parser.export_to_csv(rao_data, output_format='step1')
        step1_lines = step1_csv.strip().split('\n')
        
        print(f"   - Total lines: {len(step1_lines)} (including header)")
        print("   - Sample lines:")
        for i, line in enumerate(step1_lines[:5]):  # Show first 5 lines
            if i == 0:
                print(f"     Header: {line}")
            else:
                parts = line.split(',')
                period_info = f"Period: {parts[0] if parts[0] else 'EMPTY'}"
                freq_info = f"Freq: {parts[1] if parts[1] else 'EMPTY'}"
                direction_info = f"Dir: {parts[2]}"
                print(f"     Line {i}: {period_info}, {freq_info}, {direction_info}")
        
        # Export to Step2 format (full)
        print(f"\n3. Exporting to Step2 format (full - all period/freq)")
        step2_csv = enhanced_parser.export_to_csv(rao_data, output_format='step2')
        step2_lines = step2_csv.strip().split('\n')
        
        print(f"   - Total lines: {len(step2_lines)} (including header)")
        print("   - Sample lines:")
        for i, line in enumerate(step2_lines[:5]):  # Show first 5 lines
            if i == 0:
                print(f"     Header: {line}")
            else:
                parts = line.split(',')
                period_info = f"Period: {parts[0]}"
                freq_info = f"Freq: {parts[1]}"
                direction_info = f"Dir: {parts[2]}"
                print(f"     Line {i}: {period_info}, {freq_info}, {direction_info}")
        
        # Compare with expected files if they exist
        if step1_expected.exists() and step2_expected.exists():
            print(f"\n4. Comparing with expected step files")
            
            with open(step1_expected, 'r') as f:
                expected_step1 = f.read().strip()
            with open(step2_expected, 'r') as f:
                expected_step2 = f.read().strip()
            
            expected_step1_lines = expected_step1.split('\n')
            expected_step2_lines = expected_step2.split('\n')
            
            print(f"   - Expected Step1 lines: {len(expected_step1_lines)}")
            print(f"   - Expected Step2 lines: {len(expected_step2_lines)}")
            
            # Analyze step1 format
            print("   - Step1 format analysis:")
            step1_data_lines = expected_step1_lines[1:]  # Skip header
            full_lines = sum(1 for line in step1_data_lines if line.split(',')[0] and line.split(',')[1])
            empty_lines = len(step1_data_lines) - full_lines
            print(f"     Full lines (with period/freq): {full_lines}")
            print(f"     Abbreviated lines (empty period/freq): {empty_lines}")
            
            # Analyze step2 format
            print("   - Step2 format analysis:")
            step2_data_lines = expected_step2_lines[1:]  # Skip header
            full_lines_step2 = sum(1 for line in step2_data_lines if line.split(',')[0] and line.split(',')[1])
            print(f"     All lines have period/freq: {full_lines_step2 == len(step2_data_lines)}")
            
        print(f"\n5. Format Comparison Summary")
        print("   - Step1 (Abbreviated):")
        print("     * First line of each frequency block has period/frequency")
        print("     * Subsequent lines have empty period/frequency fields")
        print("     * Matches AQWA's native output format")
        print("     * More compact representation")
        
        print("   - Step2 (Full):")
        print("     * Every line has complete period/frequency information")
        print("     * Easier for data analysis and processing")
        print("     * Better for CSV imports into other tools")
        print("     * Self-contained row information")
        
        print(f"\n6. Implementation Features")
        print("   - Handles AQWA fixed-width format parsing")
        print("   - Supports data interpretation (fills missing period/freq)")
        print("   - Exports 6-DOF RAO data (surge, sway, heave, roll, pitch, yaw)")
        print("   - Includes both amplitude and phase data")
        print("   - Configurable output format (step1/step2)")
        
        print("\n" + "=" * 60)
        print("DEMONSTRATION COMPLETED SUCCESSFULLY")
        print("=" * 60)
        
    except Exception as e:
        print(f"\nError during demonstration: {str(e)}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    demo_rao_csv_export()