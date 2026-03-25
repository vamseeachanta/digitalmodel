"""
Extract mooring component databases from Excel to CSV format.

This script extracts component property data from the marine analysis Excel workbook:
- Mooring Chain Data sheet → chain_properties.csv
- Mooring Wire Data (2) sheet → wire_properties.csv
- Mooring Line Data sheet → line_properties.csv

Uses pandas with openpyxl to extract calculated formula values.

Usage:
    python scripts/extract_mooring_components.py

Output:
    data/marine_engineering/mooring_components/*.csv
"""

import pandas as pd
from pathlib import Path


def extract_chain_database(excel_path: str, output_csv: str) -> int:
    """
    Extract chain database from Excel "Mooring Chain Data" sheet.

    Data starts at row 24 (header at row 22, units at row 23).

    Parameters
    ----------
    excel_path : str
        Path to Excel workbook
    output_csv : str
        Output CSV file path

    Returns
    -------
    count : int
        Number of components extracted
    """
    print(f"\n[*] Extracting Chain Database...")

    # Read from row 22 (0-indexed = 21) which is the header
    # Skip row 23 which has units
    df = pd.read_excel(
        excel_path,
        sheet_name='Mooring Chain Data',
        header=21,  # Row 22 in Excel (0-indexed)
        engine='openpyxl'
    )

    # Drop the units row (first row after header)
    df = df.iloc[1:].reset_index(drop=True)

    # Remove completely empty rows
    df = df.dropna(how='all')

    # Rename columns to standardized names
    column_mapping = {
        'Chain Brand': 'manufacturer',
        'Chain Type': 'link_type',
        'Diameter': 'diameter_mm',
        'Weight per Unit Length': 'weight_air_kg_m',
        'Minimum Breaking Strength': 'mbl_n',
        'Equivalent Cross-Sectional Area': 'area_m2',
        "Young's Modulus": 'youngs_modulus_pa',
        'E*A Value': 'ea_n'
    }

    df = df.rename(columns=column_mapping)

    # Select only the columns we need
    columns_to_keep = [col for col in column_mapping.values() if col in df.columns]
    df = df[columns_to_keep]

    # Remove rows where key fields are missing
    df = df.dropna(subset=['diameter_mm', 'mbl_n'])

    # Convert MBL from N to kN
    df['mbl_kn'] = df['mbl_n'] / 1000

    # Convert EA from N to kN
    df['ea_kn'] = df['ea_n'] / 1000

    # Save to CSV
    df.to_csv(output_csv, index=False)

    print(f"[OK] Exported {len(df)} chain components to {output_csv}")
    if len(df) > 0:
        print(f"   Manufacturers: {df['manufacturer'].unique() if 'manufacturer' in df.columns else 'N/A'}")
        print(f"   Diameter range: {df['diameter_mm'].min():.0f}-{df['diameter_mm'].max():.0f} mm")
        print(f"   MBL range: {df['mbl_kn'].min():.1f}-{df['mbl_kn'].max():.1f} kN")

    return len(df)


def extract_wire_database(excel_path: str, output_csv: str) -> int:
    """
    Extract wire rope database from Excel "Mooring Wire Data (2)" sheet.

    Parameters
    ----------
    excel_path : str
        Path to Excel workbook
    output_csv : str
        Output CSV file path

    Returns
    -------
    count : int
        Number of components extracted
    """
    print(f"\n[*] Extracting Wire Rope Database...")

    try:
        # Try to read the sheet - need to find the header row
        df = pd.read_excel(
            excel_path,
            sheet_name='Mooring Wire Data (2)',
            header=None,  # Read without header first
            engine='openpyxl'
        )

        # Find header row by looking for common column names
        header_row = None
        for idx in range(min(30, len(df))):
            row = df.iloc[idx]
            row_str = ' '.join([str(x).lower() for x in row if pd.notna(x)])
            if 'diameter' in row_str or 'breaking' in row_str or 'weight' in row_str:
                header_row = idx
                break

        if header_row is None:
            print(f"[WARN] Could not find header row in wire data sheet")
            return 0

        # Re-read with correct header
        df = pd.read_excel(
            excel_path,
            sheet_name='Mooring Wire Data (2)',
            header=header_row,
            engine='openpyxl'
        )

        # Skip units row if present
        if header_row + 1 < len(df):
            first_row = df.iloc[0]
            if any('mm' in str(x) or 'kg' in str(x) or 'kn' in str(x).lower() for x in first_row if pd.notna(x)):
                df = df.iloc[1:].reset_index(drop=True)

        # Remove empty rows
        df = df.dropna(how='all')
        df = df.dropna(subset=df.columns[0:3], how='all')  # At least one of first 3 columns must have data

        # Save to CSV with all columns
        df.to_csv(output_csv, index=False)

        print(f"[OK] Exported {len(df)} wire rope components to {output_csv}")
        if len(df) > 0:
            print(f"   Columns: {len(df.columns)}")

        return len(df)

    except Exception as e:
        print(f"[ERROR] Failed to extract wire data: {e}")
        return 0


def extract_line_database(excel_path: str, output_csv: str) -> int:
    """
    Extract synthetic line database from Excel "Mooring Line Data" sheet.

    Parameters
    ----------
    excel_path : str
        Path to Excel workbook
    output_csv : str
        Output CSV file path

    Returns
    -------
    count : int
        Number of components extracted
    """
    print(f"\n[*] Extracting Synthetic Line Database...")

    try:
        # Try to read the sheet - need to find the header row
        df = pd.read_excel(
            excel_path,
            sheet_name='Mooring Line Data',
            header=None,
            engine='openpyxl'
        )

        # Find header row
        header_row = None
        for idx in range(min(30, len(df))):
            row = df.iloc[idx]
            row_str = ' '.join([str(x).lower() for x in row if pd.notna(x)])
            if 'diameter' in row_str or 'material' in row_str or 'breaking' in row_str:
                header_row = idx
                break

        if header_row is None:
            print(f"[WARN] Could not find header row in line data sheet")
            return 0

        # Re-read with correct header
        df = pd.read_excel(
            excel_path,
            sheet_name='Mooring Line Data',
            header=header_row,
            engine='openpyxl'
        )

        # Skip units row if present
        if header_row + 1 < len(df):
            first_row = df.iloc[0]
            if any('mm' in str(x) or 'kg' in str(x) or 'kn' in str(x).lower() for x in first_row if pd.notna(x)):
                df = df.iloc[1:].reset_index(drop=True)

        # Remove empty rows
        df = df.dropna(how='all')
        df = df.dropna(subset=df.columns[0:3], how='all')

        # Save to CSV
        df.to_csv(output_csv, index=False)

        print(f"[OK] Exported {len(df)} synthetic line components to {output_csv}")
        if len(df) > 0:
            print(f"   Columns: {len(df.columns)}")

        return len(df)

    except Exception as e:
        print(f"[ERROR] Failed to extract line data: {e}")
        return 0


def main():
    """Extract all mooring component databases from Excel."""

    print("=" * 70)
    print("Mooring Component Database Extraction")
    print("=" * 70)

    # Paths
    excel_path = "D:/workspace-hub/_temp/marine_analysis_data.xlsm"
    output_dir = Path("data/marine_engineering/mooring_components")

    # Ensure output directory exists
    output_dir.mkdir(parents=True, exist_ok=True)

    # Check Excel file exists
    if not Path(excel_path).exists():
        print(f"[ERROR] Excel file not found at {excel_path}")
        return

    print(f"\nSource: {excel_path}")
    print(f"Output: {output_dir}")

    # Extract databases
    total_components = 0

    chain_count = extract_chain_database(
        excel_path,
        output_dir / "chain_properties.csv"
    )
    total_components += chain_count

    wire_count = extract_wire_database(
        excel_path,
        output_dir / "wire_properties.csv"
    )
    total_components += wire_count

    line_count = extract_line_database(
        excel_path,
        output_dir / "line_properties.csv"
    )
    total_components += line_count

    # Summary
    print("\n" + "=" * 70)
    print("Extraction Summary")
    print("=" * 70)
    print(f"Chain components:     {chain_count:>4}")
    print(f"Wire rope components: {wire_count:>4}")
    print(f"Synthetic lines:      {line_count:>4}")
    print(f"{'-' * 30}")
    print(f"Total components:     {total_components:>4}")
    print("=" * 70)

    # Verify files created
    print("\nCreated files:")
    for csv_file in output_dir.glob("*.csv"):
        size_kb = csv_file.stat().st_size / 1024
        print(f"   {csv_file.name:30s} ({size_kb:>6.1f} KB)")

    print("\n[OK] Extraction complete!")


if __name__ == "__main__":
    main()
