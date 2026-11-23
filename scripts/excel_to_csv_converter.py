#!/usr/bin/env python3
"""
Convert Excel files to CSV format with appropriate naming.
Handles multi-sheet Excel files by creating separate CSVs per sheet.
"""

import pandas as pd
from pathlib import Path
import re
import sys

def clean_filename(name: str) -> str:
    """Clean filename: lowercase, replace spaces with underscores, remove special chars."""
    name = name.lower()
    name = re.sub(r'[^\w\s-]', '', name)
    name = re.sub(r'[-\s]+', '_', name)
    return name.strip('_')

def convert_excel_to_csv(excel_path: Path, output_dir: Path = None):
    """
    Convert Excel file to CSV. Creates separate CSV for each sheet.

    Args:
        excel_path: Path to Excel file
        output_dir: Directory for CSV output (default: same as Excel file)
    """
    if output_dir is None:
        output_dir = excel_path.parent

    output_dir.mkdir(parents=True, exist_ok=True)

    # Read all sheets
    try:
        excel_file = pd.ExcelFile(excel_path, engine='openpyxl' if excel_path.suffix == '.xlsx' else None)
    except Exception as e:
        print(f"❌ Error reading {excel_path.name}: {e}")
        return

    base_name = clean_filename(excel_path.stem)
    sheet_names = excel_file.sheet_names

    print(f"\n📄 Processing: {excel_path.name}")
    print(f"   Sheets: {len(sheet_names)}")

    converted_files = []

    for sheet_name in sheet_names:
        try:
            df = pd.read_excel(excel_file, sheet_name=sheet_name)

            # Skip empty sheets
            if df.empty:
                print(f"   ⚠️  Skipping empty sheet: {sheet_name}")
                continue

            # Clean sheet name
            clean_sheet = clean_filename(sheet_name)

            # Create CSV filename
            if len(sheet_names) == 1:
                # Single sheet - use base name only
                csv_name = f"{base_name}.csv"
            else:
                # Multiple sheets - append sheet name
                csv_name = f"{base_name}_{clean_sheet}.csv"

            csv_path = output_dir / csv_name

            # Save to CSV
            df.to_csv(csv_path, index=False, encoding='utf-8')

            converted_files.append(csv_path)
            print(f"   ✅ {sheet_name} → {csv_name} ({len(df)} rows, {len(df.columns)} cols)")

        except Exception as e:
            print(f"   ❌ Error converting sheet '{sheet_name}': {e}")

    return converted_files

def main():
    """Main conversion process."""
    # Find all Excel files in data/modules
    data_dir = Path("/mnt/github/workspace-hub/digitalmodel/data/modules")
    excel_files = []

    for ext in ['*.xlsx', '*.xls', '*.xlsm']:
        excel_files.extend(data_dir.rglob(ext))

    if not excel_files:
        print("No Excel files found.")
        return

    print(f"Found {len(excel_files)} Excel files")
    print("=" * 80)

    total_converted = 0

    for excel_path in sorted(excel_files):
        csv_files = convert_excel_to_csv(excel_path)
        if csv_files:
            total_converted += len(csv_files)

    print("=" * 80)
    print(f"\n✅ Conversion complete: {total_converted} CSV files created")

if __name__ == "__main__":
    main()
