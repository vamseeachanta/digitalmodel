# Excel to Parquet Migration Guide

## Overview

The migration module provides utilities to convert Excel files (.xlsx, .xls) to Parquet format with data integrity validation and rollback capabilities.

## Features

- **Batch Conversion**: Convert all Excel files in a directory recursively
- **Multi-Sheet Support**: Creates one Parquet file per Excel sheet
- **Data Integrity Validation**: Statistical validation (row count, column names, min/max/mean)
- **Snappy Compression**: Efficient Parquet compression
- **Rollback Support**: Undo migration using manifest file
- **Catalog Integration**: Auto-update catalog.yml with Parquet entries
- **Unified Data Loader**: Load data with Parquet-first, Excel fallback

## Installation

The migration utilities are included in the `digitalmodel.data` module. Required dependencies:

```bash
pip install pandas pyarrow openpyxl pyyaml
```

## Quick Start

### CLI Usage

#### Convert Excel Files

```bash
# Convert all Excel files in data/ directory
python -m digitalmodel.data.migration convert --source data/ --verify

# Specify output directory
python -m digitalmodel.data.migration convert --source data/ --output data/parquet/

# Update catalog.yml automatically
python -m digitalmodel.data.migration convert --source data/ --update-catalog
```

#### Scan for Excel Files

```bash
# Find all Excel files
python -m digitalmodel.data.migration scan --source data/
```

#### Rollback Migration

```bash
# Undo migration using manifest
python -m digitalmodel.data.migration rollback --manifest data/parquet/migration_manifest.json
```

### Python API Usage

#### Basic Conversion

```python
from digitalmodel.data.migration import convert_excel_to_parquet

# Convert all Excel files in directory
report = convert_excel_to_parquet(
    source_dir="data/",
    output_dir="data/parquet/",
    verify=True,  # Validate data integrity
    update_catalog=True  # Update catalog.yml
)

print(report)
# Migration Report
# ================
# Total Files: 15
# Successful: 15
# Failed: 0
# Excel Size: 45.23 MB
# Parquet Size: 12.34 MB
# Compression Ratio: 3.67x
```

#### Convert Single File

```python
from digitalmodel.data.migration import ExcelToParquetConverter

converter = ExcelToParquetConverter(compression="snappy")
parquet_files = converter.convert_file(
    excel_file="data/raw/sales_data.xlsx",
    output_dir="data/parquet/"
)

# For multi-sheet file, creates:
# - sales_data_Sheet1.parquet
# - sales_data_Sheet2.parquet
```

#### Data Integrity Validation

```python
from digitalmodel.data.migration import validate_data_integrity

validation = validate_data_integrity(
    excel_file="data/raw/sales_data.xlsx",
    parquet_file="data/parquet/sales_data_Sheet1.parquet",
    sheet_name="Sheet1"
)

if validation["success"]:
    print("✓ Data integrity verified")
    print(f"Rows: {validation['excel_rows']} == {validation['parquet_rows']}")
    print(f"Columns match: {validation['columns_match']}")
    print(f"Numeric stats: {validation['numeric_stats']}")
```

#### Load Data with Fallback

```python
from digitalmodel.data import load_data

# Try Parquet first, fallback to Excel
df = load_data(
    name="sales_data_Sheet1",
    search_dirs=["data/parquet", "data/raw"],
    prefer_format="parquet"
)

# Prefer Excel
df = load_data(
    name="sales_data",
    search_dirs=["data/raw"],
    prefer_format="excel"
)
```

#### Rollback Migration

```python
from digitalmodel.data.migration import rollback_migration

# Undo migration, delete Parquet files, preserve Excel
rollback_migration(manifest_path="data/parquet/migration_manifest.json")
```

## Migration Strategy

### Multi-Sheet Excel Files

For Excel files with multiple sheets, the converter creates one Parquet file per sheet:

**Input:** `financial_data.xlsx` with sheets: `revenue`, `expenses`, `profit`

**Output:**
- `financial_data_revenue.parquet`
- `financial_data_expenses.parquet`
- `financial_data_profit.parquet`

### Data Integrity Validation

The `--verify` flag performs statistical validation:

1. **Row Count**: Ensures same number of rows
2. **Column Names**: Verifies all columns present
3. **Numeric Statistics**: Validates min/max/mean for numeric columns

Example validation result:

```json
{
  "row_count_match": true,
  "excel_rows": 10000,
  "parquet_rows": 10000,
  "columns_match": true,
  "excel_columns": ["id", "name", "value", "date"],
  "parquet_columns": ["id", "name", "value", "date"],
  "numeric_stats": {
    "value": {
      "excel_min": 10.5,
      "excel_max": 9950.2,
      "excel_mean": 5023.45,
      "parquet_min": 10.5,
      "parquet_max": 9950.2,
      "parquet_mean": 5023.45,
      "match": true
    }
  },
  "success": true
}
```

## Migration Manifest

Each migration generates a manifest file (`migration_manifest.json`) containing:

- Migration ID and timestamp
- Source directory
- List of converted files with validation results
- Errors encountered

Example manifest:

```json
{
  "migration_id": "migration_20260107_123045",
  "timestamp": "2026-01-07T12:30:45.123456",
  "source_directory": "D:/workspace-hub/digitalmodel/data",
  "conversions": [
    {
      "excel_file": "data/raw/sales_data.xlsx",
      "parquet_files": [
        "data/parquet/sales_data_Sheet1.parquet",
        "data/parquet/sales_data_Sheet2.parquet"
      ],
      "validation": {
        "Sheet1": {"success": true, "row_count_match": true},
        "Sheet2": {"success": true, "row_count_match": true}
      },
      "timestamp": "2026-01-07T12:30:47.456789"
    }
  ],
  "errors": []
}
```

## Catalog Integration

When `--update-catalog` is used, the migration updates `data/catalog.yml`:

```yaml
datasets:
  sales_data_Sheet1:
    path: parquet/sales_data_Sheet1.parquet
    format: parquet
    source_excel: sales_data.xlsx
    migrated_at: '2026-01-07T12:30:47.456789'

  sales_data_Sheet2:
    path: parquet/sales_data_Sheet2.parquet
    format: parquet
    source_excel: sales_data.xlsx
    migrated_at: '2026-01-07T12:30:47.456789'
```

## Error Handling

The migration continues even if some files fail:

```python
report = convert_excel_to_parquet(source_dir="data/", verify=True)

if report.failed_conversions > 0:
    print(f"⚠️ {report.failed_conversions} files failed:")
    for error in report.errors:
        print(f"  • {error['file']}: {error['error']}")
```

Failed files are recorded in the manifest but don't stop the overall migration.

## Performance Benefits

Typical compression ratios:

- **Small files (<1MB)**: 2-3x compression
- **Medium files (1-10MB)**: 3-5x compression
- **Large files (>10MB)**: 5-10x compression

Parquet also provides:
- Columnar storage for faster analytics
- Schema preservation
- Better compatibility with big data tools (Spark, Dask, Arrow)

## Best Practices

1. **Test First**: Run migration on a copy of your data directory
2. **Verify Data**: Always use `--verify` flag for critical data
3. **Keep Manifests**: Save migration manifests for audit trail
4. **Rollback Ready**: Keep Excel files until Parquet migration is validated
5. **Update Code**: Transition code to use `load_data()` for automatic fallback

## Troubleshooting

### "File is locked" errors
- Close Excel files before migration
- Run migration outside business hours

### Validation failures
- Check for formulas in Excel (not preserved in Parquet)
- Verify date formats are compatible
- Review numeric precision requirements

### Out of memory
- Process large files individually
- Increase Python memory limit
- Use chunked processing for very large files

## Advanced Usage

### Custom Validation

```python
from digitalmodel.data.migration import validate_data_integrity
import pandas as pd

def custom_validate(excel_file, parquet_file):
    """Add custom validation logic."""
    validation = validate_data_integrity(excel_file, parquet_file)

    # Add custom checks
    df_excel = pd.read_excel(excel_file, sheet_name=0)
    df_parquet = pd.read_parquet(parquet_file)

    # Check specific business rules
    validation["custom_checks"] = {
        "no_negatives": (df_parquet >= 0).all().all(),
        "no_duplicates": not df_parquet.duplicated().any()
    }

    return validation
```

### Batch Processing with Progress

```python
from pathlib import Path
from tqdm import tqdm
from digitalmodel.data.migration import ExcelToParquetConverter

converter = ExcelToParquetConverter()
excel_files = list(Path("data").rglob("*.xlsx"))

for excel_file in tqdm(excel_files, desc="Converting"):
    try:
        converter.convert_file(excel_file, output_dir="data/parquet")
    except Exception as e:
        print(f"Failed: {excel_file.name}: {e}")
```

## API Reference

See module docstrings for detailed API documentation:

```python
from digitalmodel.data import migration
help(migration)
```

## Support

For issues or questions:
1. Check this guide
2. Review test suite in `tests/data/test_migration.py`
3. Open GitHub issue with migration manifest and error details
