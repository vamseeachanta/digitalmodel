"""
ABOUTME: Example script demonstrating Excel to Parquet migration.
Shows common use cases and migration patterns.
"""

from pathlib import Path

import pandas as pd

from digitalmodel.data.migration import (
    ExcelToParquetConverter,
    convert_excel_to_parquet,
    load_data,
    rollback_migration,
    scan_excel_files,
    validate_data_integrity,
)


def example_1_scan_files():
    """Example 1: Scan directory for Excel files."""
    print("=" * 60)
    print("Example 1: Scan for Excel Files")
    print("=" * 60)

    data_dir = Path("data")

    if data_dir.exists():
        excel_files = scan_excel_files(data_dir)
        print(f"\nFound {len(excel_files)} Excel file(s):")
        for f in excel_files[:5]:  # Show first 5
            size_mb = f.stat().st_size / (1024 ** 2)
            print(f"  • {f.name} ({size_mb:.2f} MB)")
        if len(excel_files) > 5:
            print(f"  ... and {len(excel_files) - 5} more")
    else:
        print(f"\n[WARNING]  Directory {data_dir} not found")

    print()


def example_2_convert_single_file():
    """Example 2: Convert single Excel file."""
    print("=" * 60)
    print("Example 2: Convert Single Excel File")
    print("=" * 60)

    # Create sample Excel file for demonstration
    sample_dir = Path("data/examples")
    sample_dir.mkdir(parents=True, exist_ok=True)

    sample_file = sample_dir / "sample_data.xlsx"

    if not sample_file.exists():
        # Create sample data with multiple sheets
        df1 = pd.DataFrame({
            "product_id": [101, 102, 103, 104, 105],
            "product_name": ["Widget A", "Widget B", "Gadget C", "Doohickey D", "Thingamajig E"],
            "price": [9.99, 19.99, 29.99, 39.99, 49.99],
            "stock": [100, 50, 75, 30, 60],
        })

        df2 = pd.DataFrame({
            "order_id": [1001, 1002, 1003, 1004, 1005],
            "product_id": [101, 102, 101, 103, 104],
            "quantity": [2, 1, 5, 3, 1],
            "total": [19.98, 19.99, 49.95, 89.97, 39.99],
        })

        with pd.ExcelWriter(sample_file, engine="openpyxl") as writer:
            df1.to_excel(writer, sheet_name="products", index=False)
            df2.to_excel(writer, sheet_name="orders", index=False)

        print(f"\n[OK] Created sample file: {sample_file}")

    # Convert to Parquet
    converter = ExcelToParquetConverter(compression="snappy")
    output_dir = sample_dir / "parquet"

    parquet_files = converter.convert_file(sample_file, output_dir)

    print(f"\n[OK] Converted to {len(parquet_files)} Parquet file(s):")
    for pf in parquet_files:
        size_kb = pf.stat().st_size / 1024
        print(f"  • {pf.name} ({size_kb:.2f} KB)")

    print()


def example_3_validate_integrity():
    """Example 3: Validate data integrity."""
    print("=" * 60)
    print("Example 3: Validate Data Integrity")
    print("=" * 60)

    sample_file = Path("data/examples/sample_data.xlsx")
    parquet_file = Path("data/examples/parquet/sample_data_products.parquet")

    if sample_file.exists() and parquet_file.exists():
        validation = validate_data_integrity(
            excel_file=sample_file,
            parquet_file=parquet_file,
            sheet_name="products"
        )

        print(f"\n[OK] Validation Results:")
        print(f"  • Row count match: {validation['row_count_match']}")
        print(f"  • Excel rows: {validation['excel_rows']}")
        print(f"  • Parquet rows: {validation['parquet_rows']}")
        print(f"  • Columns match: {validation['columns_match']}")
        print(f"  • Success: {validation['success']}")

        if validation['numeric_stats']:
            print(f"\n  Numeric Statistics:")
            for col, stats in validation['numeric_stats'].items():
                print(f"    {col}:")
                print(f"      Excel:   min={stats['excel_min']:.2f}, max={stats['excel_max']:.2f}")
                print(f"      Parquet: min={stats['parquet_min']:.2f}, max={stats['parquet_max']:.2f}")
    else:
        print("\n[WARNING]  Sample files not found. Run example 2 first.")

    print()


def example_4_batch_conversion():
    """Example 4: Batch convert all Excel files."""
    print("=" * 60)
    print("Example 4: Batch Convert All Excel Files")
    print("=" * 60)

    data_dir = Path("data/examples")

    if data_dir.exists():
        print(f"\nConverting all Excel files in {data_dir}...")

        report = convert_excel_to_parquet(
            source_dir=data_dir,
            output_dir=data_dir / "parquet",
            verify=True,
            update_catalog=True,
        )

        print("\n" + str(report))

        if report.failed_conversions > 0:
            print(f"\n[WARNING]  Failed conversions:")
            for error in report.errors:
                print(f"  • {error['file']}: {error['error']}")
    else:
        print(f"\n[WARNING]  Directory {data_dir} not found")

    print()


def example_5_load_data():
    """Example 5: Load data with Parquet-first fallback."""
    print("=" * 60)
    print("Example 5: Load Data with Fallback")
    print("=" * 60)

    search_dirs = [
        Path("data/examples/parquet"),
        Path("data/examples"),
    ]

    try:
        # Load Parquet file (preferred)
        df = load_data(
            name="sample_data_products",
            search_dirs=search_dirs,
            prefer_format="parquet"
        )

        print(f"\n[OK] Loaded data successfully:")
        print(f"  • Shape: {df.shape}")
        print(f"  • Columns: {list(df.columns)}")
        print(f"\nFirst 3 rows:")
        print(df.head(3).to_string(index=False))

    except FileNotFoundError as e:
        print(f"\n[X] Error: {e}")

    print()


def example_6_rollback():
    """Example 6: Rollback migration."""
    print("=" * 60)
    print("Example 6: Rollback Migration")
    print("=" * 60)

    manifest_file = Path("data/examples/parquet/migration_manifest.json")

    if manifest_file.exists():
        print(f"\nRollback would delete Parquet files listed in:")
        print(f"  {manifest_file}")
        print("\nOriginal Excel files would be preserved.")

        # Uncomment to actually perform rollback:
        # rollback_migration(manifest_file)
        # print("\n[OK] Rollback complete")

        print("\n(Rollback skipped in example)")
    else:
        print(f"\n[WARNING]  Manifest file not found: {manifest_file}")

    print()


def main():
    """Run all examples."""
    print("\n")
    print("+" + "=" * 58 + "+")
    print("|" + " " * 10 + "Excel to Parquet Migration Examples" + " " * 12 + "|")
    print("+" + "=" * 58 + "+")
    print()

    examples = [
        example_1_scan_files,
        example_2_convert_single_file,
        example_3_validate_integrity,
        example_4_batch_conversion,
        example_5_load_data,
        example_6_rollback,
    ]

    for i, example in enumerate(examples, 1):
        try:
            example()
        except Exception as e:
            print(f"\n[X] Example {i} failed: {e}\n")

    print("=" * 60)
    print("All examples completed!")
    print("=" * 60)
    print()


if __name__ == "__main__":
    main()
