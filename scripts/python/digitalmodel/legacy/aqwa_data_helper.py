"""
Interactive AQWA .LIS Data Helper
==================================

This script helps debug and fix AQWA RAO data extraction issues.

Usage:
------
1. Run this script: python scripts/aqwa_data_helper.py
2. Paste a sample block from your AQWA .LIS file
3. Or provide path to .LIS file
4. Script will show the parsing and identify issues

You can also create a CSV with the expected structure:
- period, frequency, direction, X, Y, Z, RX, RY, RZ
- One row per period/heading combination
"""

import sys
from pathlib import Path
import re
import pandas as pd

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))


def parse_aqwa_block_interactive(text_block: str):
    """Parse a sample AQWA text block and show results."""
    print("\n" + "="*80)
    print("AQWA BLOCK PARSER - INTERACTIVE MODE")
    print("="*80)

    lines = text_block.strip().split('\n')
    print(f"\n📄 Input: {len(lines)} lines")
    print("\n" + "-"*80)
    print("SAMPLE INPUT:")
    print("-"*80)
    for i, line in enumerate(lines[:20], 1):
        print(f"{i:3d}: {line}")
    if len(lines) > 20:
        print(f"... ({len(lines) - 20} more lines)")

    # Try to identify structure
    print("\n" + "-"*80)
    print("STRUCTURE ANALYSIS:")
    print("-"*80)

    # Look for header
    header_pattern = re.compile(r'PERIOD\s+FREQ', re.IGNORECASE)
    data_pattern = re.compile(r'^\s*[\d.]+\s+[\d.]+\s+[\d.]+')
    direction_pattern = re.compile(r'DIRECTION\s+([\d.]+)', re.IGNORECASE)

    current_heading = None
    data_rows = []

    for i, line in enumerate(lines):
        # Check for header
        if header_pattern.search(line):
            print(f"✓ Found header at line {i+1}")
            continue

        # Check for direction
        dir_match = direction_pattern.search(line)
        if dir_match:
            current_heading = float(dir_match.group(1))
            print(f"✓ Found heading: {current_heading}° at line {i+1}")
            continue

        # Check for data row
        if data_pattern.match(line):
            parts = line.split()
            if len(parts) >= 3:
                period = float(parts[0])
                freq = float(parts[1])

                # Check if direction is on this line or inherited
                if len(parts) >= 9:  # Has direction on same line
                    direction = float(parts[2])
                    data_start = 3
                else:  # Direction inherited from block header
                    direction = current_heading
                    data_start = 2

                data_row = {
                    'line': i+1,
                    'period': period,
                    'freq': freq,
                    'direction': direction,
                    'values': parts[data_start:]
                }
                data_rows.append(data_row)
                print(f"  ↳ Data row {len(data_rows)}: T={period:.2f}s, Dir={direction}°, Values={len(parts[data_start:])}")

    print(f"\n📊 Extracted {len(data_rows)} data rows")

    if data_rows:
        # Show summary
        df = pd.DataFrame([
            {
                'Period': r['period'],
                'Frequency': r['freq'],
                'Direction': r['direction'],
                'Values': len(r['values'])
            }
            for r in data_rows
        ])

        print("\n" + "-"*80)
        print("EXTRACTED DATA SUMMARY:")
        print("-"*80)
        print(df.to_string(index=False))

        # Check for missing directions
        missing_dir = df['Direction'].isna().sum()
        if missing_dir > 0:
            print(f"\n⚠️  WARNING: {missing_dir} rows missing direction!")

        # Save to CSV for review
        output_path = Path('scripts/aqwa_parsed_output.csv')
        df.to_csv(output_path, index=False)
        print(f"\n💾 Saved to: {output_path}")

    return data_rows


def create_sample_csv_template():
    """Create a template CSV for user to fill in."""
    template = pd.DataFrame({
        'period': [10.0, 9.0, 8.0, 10.0, 9.0, 8.0],
        'frequency': [0.628, 0.698, 0.785, 0.628, 0.698, 0.785],
        'direction': [0.0, 0.0, 0.0, 30.0, 30.0, 30.0],
        'X': [1.23, 1.45, 1.67, 1.34, 1.56, 1.78],
        'Y': [0.12, 0.14, 0.16, 0.45, 0.52, 0.59],
        'Z': [0.98, 1.02, 1.08, 0.89, 0.95, 1.01],
        'RX': [0.05, 0.06, 0.07, 0.08, 0.09, 0.10],
        'RY': [0.15, 0.17, 0.19, 0.16, 0.18, 0.20],
        'RZ': [0.02, 0.03, 0.04, 0.05, 0.06, 0.07]
    })

    output_path = Path('scripts/aqwa_template.csv')
    template.to_csv(output_path, index=False)
    print(f"\n✓ Created template: {output_path}")
    print("\nTemplate format:")
    print(template.head())
    print("\n📝 Fill in this CSV with your actual AQWA data and save it.")

    return output_path


def main():
    """Interactive main function."""
    print("\n" + "="*80)
    print("AQWA .LIS DATA EXTRACTION HELPER")
    print("="*80)
    print("\nOptions:")
    print("  1. Paste sample AQWA text block")
    print("  2. Provide path to .LIS file")
    print("  3. Create CSV template for manual data entry")
    print("  4. Load existing CSV for validation")

    choice = input("\nSelect option (1-4): ").strip()

    if choice == '1':
        print("\nPaste your AQWA text block (end with empty line):")
        lines = []
        while True:
            line = input()
            if not line:
                break
            lines.append(line)

        if lines:
            parse_aqwa_block_interactive('\n'.join(lines))
        else:
            print("No input provided.")

    elif choice == '2':
        file_path = input("\nEnter path to .LIS file: ").strip()
        if Path(file_path).exists():
            with open(file_path, 'r') as f:
                content = f.read()

            # Find RAO section
            rao_start = content.find('R.A.O.S-VARIATION')
            if rao_start >= 0:
                # Get ~100 lines from RAO section
                section = '\n'.join(content[rao_start:].split('\n')[:100])
                parse_aqwa_block_interactive(section)
            else:
                print("Could not find R.A.O.S section in file")
        else:
            print(f"File not found: {file_path}")

    elif choice == '3':
        create_sample_csv_template()

    elif choice == '4':
        csv_path = input("\nEnter path to CSV file: ").strip()
        if Path(csv_path).exists():
            df = pd.read_csv(csv_path)
            print("\n✓ Loaded CSV:")
            print(df.head(10))
            print(f"\nTotal rows: {len(df)}")
            print(f"Unique directions: {df['direction'].nunique()}")
            print(f"Unique periods: {df['period'].nunique()}")

            # Validate
            required_cols = ['period', 'frequency', 'direction', 'X', 'Y', 'Z', 'RX', 'RY', 'RZ']
            missing = [c for c in required_cols if c not in df.columns]
            if missing:
                print(f"\n⚠️  Missing columns: {missing}")
            else:
                print("\n✓ All required columns present")
        else:
            print(f"File not found: {csv_path}")

    else:
        print("Invalid option")


if __name__ == '__main__':
    main()
