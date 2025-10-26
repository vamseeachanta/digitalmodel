#!/usr/bin/env python3
"""
Process all fender PDFs and generate fenders_2025.csv.

This script processes all PDF files in data/equipment/raw/fenders/
and generates a comprehensive fenders dataset.
"""

import sys
from pathlib import Path
import pandas as pd
from datetime import datetime

# Add parent to path
sys.path.insert(0, str(Path(__file__).parent))

from scrapers.pdf_extractor import PDFExtractor
from validators.data_validator import DataValidator


def main():
    """Process all fender PDFs and generate final dataset."""

    print("=" * 70)
    print("PROCESSING ALL FENDER PDFs - Phase 2.3")
    print("=" * 70)
    print(f"Start Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

    # Paths
    pdf_dir = Path("data/equipment/raw/fenders")
    output_dir = Path("data/equipment/processed")
    output_dir.mkdir(parents=True, exist_ok=True)

    # Count PDFs
    pdf_files = list(pdf_dir.glob("*.pdf"))
    print(f"\nFound {len(pdf_files)} PDF files in {pdf_dir}")

    # Initialize extractor
    extractor = PDFExtractor(output_dir=output_dir)

    # Process all PDFs
    print(f"\n{'='*70}")
    print("EXTRACTING DATA FROM PDFs")
    print(f"{'='*70}\n")

    all_data = []
    success_count = 0
    failure_count = 0

    for i, pdf_file in enumerate(pdf_files, 1):
        print(f"\n[{i}/{len(pdf_files)}] Processing: {pdf_file.name}")
        print("-" * 70)

        try:
            df = extractor.extract_from_catalog(pdf_file, equipment_type="fender")

            if df is not None and not df.empty:
                print(f"✓ Success: Extracted {len(df)} rows with {len(df.columns)} columns")
                all_data.append(df)
                success_count += 1
            else:
                print(f"✗ Failed: No data extracted")
                failure_count += 1

        except Exception as e:
            print(f"✗ Error: {str(e)}")
            failure_count += 1
            continue

    print(f"\n{'='*70}")
    print("EXTRACTION SUMMARY")
    print(f"{'='*70}")
    print(f"Total PDFs: {len(pdf_files)}")
    print(f"Successful: {success_count} ({success_count/len(pdf_files)*100:.1f}%)")
    print(f"Failed: {failure_count} ({failure_count/len(pdf_files)*100:.1f}%)")

    if not all_data:
        print("\n✗ No data extracted from any PDFs - exiting")
        return

    # Combine all data
    print(f"\n{'='*70}")
    print("COMBINING DATA")
    print(f"{'='*70}\n")

    combined_df = pd.concat(all_data, ignore_index=True, sort=False)

    print(f"Combined DataFrame:")
    print(f"  Total Rows: {len(combined_df)}")
    print(f"  Total Columns: {len(combined_df.columns)}")
    print(f"  Unique Sources: {combined_df['catalog_source'].nunique()}")

    # Show column distribution
    print(f"\nColumn Names ({len(combined_df.columns)} total):")
    for col in sorted(combined_df.columns)[:20]:
        non_null = combined_df[col].notna().sum()
        pct = non_null / len(combined_df) * 100
        print(f"  {col:30s}: {non_null:5d} ({pct:5.1f}% filled)")

    if len(combined_df.columns) > 20:
        print(f"  ... and {len(combined_df.columns) - 20} more columns")

    # Validate data quality
    print(f"\n{'='*70}")
    print("DATA QUALITY VALIDATION")
    print(f"{'='*70}\n")

    validator = DataValidator()
    results = validator.validate_dataframe(combined_df)

    print(f"Quality Score: {results.get('quality_score', 0):.1f}/100")
    print(f"Total Rows: {results.get('total_rows', 0)}")

    if 'missing_percentage' in results:
        completeness = (1 - results['missing_percentage']/100) * 100
        print(f"Data Completeness: {completeness:.1f}%")

    if results.get('issues'):
        print(f"\nTop Issues:")
        for issue in results['issues'][:10]:
            print(f"  - {issue}")

    # Save final dataset
    print(f"\n{'='*70}")
    print("SAVING FINAL DATASET")
    print(f"{'='*70}\n")

    # Save to processed directory
    date_str = datetime.now().strftime('%Y%m%d')
    output_file = output_dir / f"fenders_extracted_{date_str}.csv"
    combined_df.to_csv(output_file, index=False)
    print(f"✓ Saved full dataset: {output_file}")
    print(f"  Size: {output_file.stat().st_size / 1024:.1f} KB")

    # Save as fenders_2025.csv (final product)
    final_output = Path("data/equipment/fenders_2025.csv")
    final_output.parent.mkdir(parents=True, exist_ok=True)
    combined_df.to_csv(final_output, index=False)
    print(f"✓ Saved final product: {final_output}")
    print(f"  Size: {final_output.stat().st_size / 1024:.1f} KB")

    # Generate summary report
    summary_file = output_dir / f"extraction_summary_{date_str}.txt"
    with open(summary_file, 'w') as f:
        f.write("="*70 + "\n")
        f.write("FENDER PDF EXTRACTION SUMMARY\n")
        f.write("="*70 + "\n\n")
        f.write(f"Date: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write(f"Total PDFs Processed: {len(pdf_files)}\n")
        f.write(f"Successful Extractions: {success_count} ({success_count/len(pdf_files)*100:.1f}%)\n")
        f.write(f"Failed Extractions: {failure_count} ({failure_count/len(pdf_files)*100:.1f}%)\n\n")
        f.write(f"Final Dataset:\n")
        f.write(f"  Rows: {len(combined_df)}\n")
        f.write(f"  Columns: {len(combined_df.columns)}\n")
        f.write(f"  Quality Score: {results.get('quality_score', 0):.1f}/100\n")
        f.write(f"  Output File: {final_output}\n\n")
        f.write(f"Unique Sources:\n")
        for source in sorted(combined_df['catalog_source'].unique()):
            count = (combined_df['catalog_source'] == source).sum()
            f.write(f"  {source}: {count} rows\n")

    print(f"✓ Saved summary report: {summary_file}")

    print(f"\n{'='*70}")
    print("PROCESSING COMPLETE")
    print(f"{'='*70}")
    print(f"End Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"\nFinal dataset ready: {final_output}")
    print(f"Total rows: {len(combined_df)}")
    print(f"Quality score: {results.get('quality_score', 0):.1f}/100")


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\n\n✗ Processing interrupted by user")
        sys.exit(1)
    except Exception as e:
        print(f"\n\n✗ Processing failed with error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
