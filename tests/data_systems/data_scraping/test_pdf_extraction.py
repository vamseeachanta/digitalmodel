#!/usr/bin/env python3
"""Test PDF extraction from fender catalogs."""

import sys
from pathlib import Path

# Add parent to path
sys.path.insert(0, str(Path(__file__).parent))

from scrapers.pdf_extractor import PDFExtractor


def test_single_pdf():
    """Test extraction from a single PDF."""
    print("="*60)
    print("TESTING PDF EXTRACTION - SINGLE FILE")
    print("="*60)

    # Test with floating fenders size table (small, likely has table)
    pdf_path = Path("data/equipment/raw/fenders/floatingfenders_sizetable_50_1200x2000.pdf")

    if not pdf_path.exists():
        print(f"✗ PDF not found: {pdf_path}")
        return

    print(f"\nPDF: {pdf_path.name}")
    print(f"Size: {pdf_path.stat().st_size / 1024:.1f} KB")

    extractor = PDFExtractor(output_dir=Path("data/equipment/processed"))

    # Analyze structure first
    print("\n--- PDF Structure Analysis ---")
    analysis = extractor.analyze_pdf_structure(pdf_path)

    print(f"Pages: {analysis['pages']}")
    print(f"Tables found: {analysis['tables_found']}")
    print(f"Has headers: {analysis['has_headers']}")

    if analysis['tables_per_page']:
        print("\nTables per page:")
        for page_num, table_count in analysis['tables_per_page']:
            print(f"  Page {page_num}: {table_count} tables")

    if analysis.get('sample_content'):
        print("\nSample content (first 5 rows):")
        for i, row in enumerate(analysis['sample_content'][:5], 1):
            print(f"  Row {i}: {row[:3]}...")  # First 3 columns

    # Extract tables
    print("\n--- Extracting Tables ---")
    tables = extractor.extract_tables_from_pdf(pdf_path)

    if tables:
        print(f"\n✓ Extracted {len(tables)} tables")

        for i, df in enumerate(tables, 1):
            print(f"\nTable {i}:")
            print(f"  Rows: {len(df)}")
            print(f"  Columns: {list(df.columns)}")
            print(f"\n  First 3 rows:")
            print(df.head(3))

        # Process as fender specifications
        print("\n--- Processing Fender Specifications ---")
        df_specs = extractor.extract_from_catalog(pdf_path, equipment_type="fender")

        if df_specs is not None:
            print(f"\n✓ Processed {len(df_specs)} fender specifications")
            print(f"  Columns: {list(df_specs.columns)}")
            print(f"\n  Sample data:")
            print(df_specs.head())

            # Save
            output_file = Path("data/equipment/processed/test_single_pdf.csv")
            df_specs.to_csv(output_file, index=False)
            print(f"\n✓ Saved to: {output_file}")

    else:
        print("\n✗ No tables found in PDF")


def test_design_manual():
    """Test extraction from fender design manual."""
    print("\n" + "="*60)
    print("TESTING PDF EXTRACTION - DESIGN MANUAL")
    print("="*60)

    pdf_path = Path("data/equipment/raw/fenders/Fender Application Design Manual.pdf")

    if not pdf_path.exists():
        print(f"✗ PDF not found: {pdf_path}")
        return

    print(f"\nPDF: {pdf_path.name}")
    print(f"Size: {pdf_path.stat().st_size / (1024*1024):.1f} MB")

    extractor = PDFExtractor()

    # Analyze structure
    print("\n--- Analyzing PDF Structure ---")
    analysis = extractor.analyze_pdf_structure(pdf_path)

    print(f"Pages: {analysis['pages']}")
    print(f"Tables found: {analysis['tables_found']}")

    if analysis['tables_per_page']:
        print(f"\nTables distribution: {len(analysis['tables_per_page'])} pages with tables")
        print(f"  Pages: {[p for p, _ in analysis['tables_per_page'][:5]]}...")

    # Extract spec tables only
    print("\n--- Extracting Specification Tables ---")
    spec_tables = extractor.find_specification_tables(pdf_path)

    if spec_tables:
        print(f"\n✓ Found {len(spec_tables)} specification tables")

        for i, df in enumerate(spec_tables[:3], 1):  # Show first 3
            print(f"\nSpec Table {i}:")
            print(f"  {len(df)} rows × {len(df.columns)} columns")
            print(f"  Columns: {list(df.columns)[:5]}")

    else:
        print("\n✗ No specification tables found")


def test_multiple_pdfs():
    """Test extraction from multiple fender PDFs."""
    print("\n" + "="*60)
    print("TESTING PDF EXTRACTION - MULTIPLE FILES")
    print("="*60)

    pdf_dir = Path("data/equipment/raw/fenders")

    extractor = PDFExtractor(output_dir=Path("data/equipment/processed"))

    # Process first 5 PDFs as test
    pdf_files = list(pdf_dir.glob("*.pdf"))[:5]

    print(f"\nTesting with first 5 PDFs:")
    for pdf_file in pdf_files:
        print(f"  - {pdf_file.name}")

    print("\n--- Processing PDFs ---")

    all_data = []
    for pdf_file in pdf_files:
        print(f"\nProcessing: {pdf_file.name}")
        df = extractor.extract_from_catalog(pdf_file, equipment_type="fender")

        if df is not None and not df.empty:
            print(f"  ✓ Extracted {len(df)} rows")
            all_data.append(df)
        else:
            print(f"  ✗ No data extracted")

    if all_data:
        import pandas as pd
        combined_df = pd.concat(all_data, ignore_index=True)

        print(f"\n--- Combined Results ---")
        print(f"Total rows: {len(combined_df)}")
        print(f"Total columns: {len(combined_df.columns)}")
        print(f"Columns: {list(combined_df.columns)}")

        # Check for key fender specification columns
        spec_cols = ['product_name', 'diameter_m', 'length_m', 'energy_absorption_kj']
        found_cols = [col for col in spec_cols if col in combined_df.columns]

        if found_cols:
            print(f"\n✓ Found specification columns: {found_cols}")
            print(f"\nSample specifications:")
            print(combined_df[found_cols].head())

        # Save
        output_file = Path("data/equipment/processed/test_multiple_pdfs.csv")
        combined_df.to_csv(output_file, index=False)
        print(f"\n✓ Saved to: {output_file}")

    else:
        print("\n✗ No data extracted from any PDFs")


if __name__ == "__main__":
    try:
        # Test single PDF
        test_single_pdf()

        # Test design manual
        test_design_manual()

        # Test multiple PDFs
        test_multiple_pdfs()

        print("\n" + "="*60)
        print("PDF EXTRACTION TESTS COMPLETE")
        print("="*60)

    except KeyboardInterrupt:
        print("\n\nTest interrupted by user")
    except Exception as e:
        print(f"\n✗ Test failed with error: {e}")
        import traceback
        traceback.print_exc()
