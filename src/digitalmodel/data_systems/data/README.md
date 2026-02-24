# Data Migration Utilities

Excel to Parquet migration system with data integrity validation and rollback support.

## Quick Start

### CLI

```bash
# Convert all Excel files
python -m digitalmodel.data_systems.data convert --source data/ --verify

# Scan for Excel files
python -m digitalmodel.data_systems.data scan --source data/

# Rollback migration
python -m digitalmodel.data_systems.data rollback --manifest migration_manifest.json
```

### Python API

```python
from digitalmodel.data_systems.data import convert_excel_to_parquet, load_data

# Batch convert
report = convert_excel_to_parquet("data/", verify=True)

# Load with fallback
df = load_data("sales_data", prefer_format="parquet")
```

## Features

- ✅ Batch Excel to Parquet conversion
- ✅ Multi-sheet support (one Parquet per sheet)
- ✅ Data integrity validation (row count, columns, statistics)
- ✅ Snappy compression (3-5x reduction)
- ✅ Migration manifest for audit/rollback
- ✅ Catalog.yml integration
- ✅ Parquet-first loader with Excel fallback
- ✅ Comprehensive error handling

## Documentation

- **User Guide**: `docs/modules/data/MIGRATION_GUIDE.md`
- **Implementation Summary**: `docs/modules/data/MIGRATION_SUMMARY.md`
- **Examples**: `scripts/example_migration.py`

## Tests

```bash
pytest tests/data/test_migration.py -v
# 30 tests, 91.02% coverage
```

## Requirements

- pandas>=2.0.0
- pyarrow>=14.0.0
- openpyxl>=3.1.0
- pyyaml>=6.0.0
- click>=8.0.0
