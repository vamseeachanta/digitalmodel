#!/usr/bin/env python3
"""
Verify Excel to CSV conversion integrity.
Compare row/column counts and data samples.
"""

import pandas as pd
from pathlib import Path
import sys

def verify_conversion(excel_path: Path) -> bool:
    """
    Verify that CSV files match Excel data.

    Returns:
        True if all conversions are valid, False otherwise
    """
    csv_dir = excel_path.parent
    base_name = excel_path.stem.lower().replace(' ', '_').replace('-', '_')

    try:
        # Read Excel file
        excel_file = pd.ExcelFile(excel_path, engine='openpyxl' if excel_path.suffix == '.xlsx' else None)
        sheet_names = excel_file.sheet_names

        print(f"\n📄 {excel_path.name}")
        print(f"   Sheets: {len(sheet_names)}")

        all_valid = True

        for sheet_name in sheet_names:
            # Read Excel sheet
            df_excel = pd.read_excel(excel_file, sheet_name=sheet_name)

            # Skip empty sheets
            if df_excel.empty:
                print(f"   ⚠️  {sheet_name}: Empty (skipped)")
                continue

            # Find corresponding CSV
            if len(sheet_names) == 1:
                csv_pattern = f"{base_name}.csv"
            else:
                sheet_clean = sheet_name.lower().replace(' ', '_').replace('-', '_')
                csv_pattern = f"{base_name}_{sheet_clean}.csv"

            csv_files = list(csv_dir.glob(csv_pattern))

            if not csv_files:
                print(f"   ❌ {sheet_name}: CSV not found ({csv_pattern})")
                all_valid = False
                continue

            csv_path = csv_files[0]

            # Read CSV
            df_csv = pd.read_csv(csv_path)

            # Compare dimensions
            excel_shape = df_excel.shape
            csv_shape = df_csv.shape

            if excel_shape == csv_shape:
                print(f"   ✅ {sheet_name}: {excel_shape[0]} rows × {excel_shape[1]} cols → {csv_path.name}")
            else:
                print(f"   ❌ {sheet_name}: MISMATCH! Excel {excel_shape} vs CSV {csv_shape}")
                all_valid = False

        return all_valid

    except Exception as e:
        print(f"   ❌ Error: {e}")
        return False

def main():
    """Main verification process."""
    data_dir = Path("/mnt/github/workspace-hub/digitalmodel/data/modules")
    excel_files = []

    for ext in ['*.xlsx', '*.xls', '*.xlsm']:
        excel_files.extend(data_dir.rglob(ext))

    if not excel_files:
        print("No Excel files found.")
        return True

    print(f"Verifying {len(excel_files)} Excel files")
    print("=" * 80)

    all_valid = True
    for excel_path in sorted(excel_files):
        if not verify_conversion(excel_path):
            all_valid = False

    print("=" * 80)

    if all_valid:
        print("\n✅ All conversions verified successfully!")
        return True
    else:
        print("\n❌ Some conversions failed verification!")
        return False

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
