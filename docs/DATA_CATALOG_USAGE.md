# Data Catalog System - Usage Guide

## Overview

The Data Catalog system provides a unified API for discovering, managing, and loading datasets across the digitalmodel project. It includes:

- **Auto-discovery** of data files
- **Metadata tracking** (version, hash, source, tags)
- **Schema validation** using JSON Schema
- **Versioning support** with automatic hash-based bumping
- **Search and filter** capabilities
- **Unified loading API** for multiple formats (Excel, CSV, Parquet, YAML, JSON)

## Quick Start

### 1. Create Master Catalog

```bash
# Run the catalog creation script
uv run python scripts/create_catalog.py
```

This discovers all data files in `/data` and creates `/data/catalog.yml`.

### 2. Load Data Using Catalog

```python
from digitalmodel.data import DataCatalog

# Load catalog
catalog = DataCatalog.load_catalog("data/catalog.yml")

# Load dataset by name (returns pandas DataFrame or dict)
data = catalog.load("shackles_g2100")

# Load with schema validation
data = catalog.load("shackles_g2100", validate=True)
```

### 3. Search Catalog

```python
# Search by format
excel_files = catalog.search(format="excel")

# Search by tags
rigging_data = catalog.search(tags=["rigging"])

# Search by description
subsea = catalog.search(description="subsea")

# Combined search
crosby_shackles = catalog.search(
    format="excel",
    tags=["shackles", "subsea"]
)
```

## Catalog Structure

### Master Catalog (`data/catalog.yml`)

```yaml
catalog_version: "1.0.0"
datasets:
  shackles_g2100:
    name: shackles_g2100
    file: data/crosby/204_subsea_shackles_g2100.xlsx
    format: excel
    schema: schemas/shackles.schema.json
    version: "2024.1"
    description: "Crosby G2100 subsea shackles"
    source: "Crosby Catalog"
    tags: ["rigging", "shackles", "subsea"]
    units: "metric"
    engineering_standard: "API 2C"
    hash: "abc123..."
    created_at: "2026-01-07T00:00:00"
    last_modified: "2026-01-07T00:00:00"
    schema_version: "1.0.0"
```

### Module Catalogs

Per-module catalogs can be created in subdirectories:

```
data/
├── catalog.yml              # Master catalog
├── crosby/
│   ├── catalog.yml          # Module catalog
│   └── *.xlsx
└── hooks/
    ├── catalog.yml
    └── *.xlsx
```

Module catalogs are automatically merged into the master catalog.

## JSON Schemas

### Creating a Schema

Create schemas in `data/schemas/`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "Shackles Data Schema",
  "type": "object",
  "properties": {
    "Size": {
      "type": "string",
      "description": "Shackle size designation"
    },
    "WLL": {
      "type": "number",
      "description": "Working Load Limit in tons",
      "minimum": 0
    }
  },
  "required": ["Size", "WLL"],
  "additionalProperties": true
}
```

### Using Schemas

```python
# Load catalog with schema directory
catalog = DataCatalog.load_catalog(
    "data/catalog.yml",
    schema_dir="data/schemas"
)

# Load and validate
data = catalog.load("shackles_g2100", validate=True)
```

Validation is **permissive** by default (warns on unknown fields, doesn't fail).

## Programmatic API

### Creating Entries

```python
from digitalmodel.data import CatalogEntry, DataCatalog

# Create entry manually
entry = CatalogEntry(
    name="my_dataset",
    file="data/my_data.xlsx",
    format="excel",
    version="1.0.0",
    description="My custom dataset",
    tags=["custom", "marine"],
    source="Internal",
)

# Create catalog and add entry
catalog = DataCatalog("data/catalog.yml")
catalog.add_entry(entry)
catalog.save()
```

### Discovery

```python
from digitalmodel.data import CatalogDiscovery
from pathlib import Path

# Discover files
discovery = CatalogDiscovery(Path("data"))
files = discovery.discover()

# Create entries from discovered files
for file in files:
    entry = discovery.create_entry(file)
    catalog.add_entry(entry)
```

### Versioning

```python
# Check if file has changed and auto-bump version
new_version = catalog.check_version("my_dataset", auto_bump=True)

# Manual hash update
entry = catalog.get_entry("my_dataset")
entry.update_hash(base_path=Path("data"))
```

## Supported Formats

| Format   | Extension         | Loader                |
|----------|-------------------|-----------------------|
| Excel    | .xlsx, .xls       | `pd.read_excel()`     |
| CSV      | .csv              | `pd.read_csv()`       |
| Parquet  | .parquet          | `pd.read_parquet()`   |
| YAML     | .yml, .yaml       | `yaml.safe_load()`    |
| JSON     | .json             | `json.load()`         |

## Metadata Fields

### Required Fields

- `name`: Dataset identifier
- `file`: Relative path from base_path
- `format`: File format (excel, csv, parquet, yaml, json)
- `version`: Version string (semantic or date-based)
- `description`: Human-readable description

### Optional Fields

- `schema`: Path to JSON schema for validation
- `source`: Data source (e.g., "Crosby Catalog", "DNV GL")
- `tags`: List of tags for categorization
- `units`: Unit system (metric, imperial)
- `engineering_standard`: Applicable standard (API 2C, DNV-RP-C203, etc.)
- `columns`: List of column names

### Auto-Generated Fields

- `hash`: SHA256 file hash
- `created_at`: Creation timestamp (ISO format)
- `last_modified`: Last modification timestamp
- `schema_version`: Catalog schema version

## Advanced Usage

### Module Catalog Merging

```python
from digitalmodel.data import create_master_catalog

# Create master with module catalog merging
catalog = create_master_catalog(
    data_dir=Path("data"),
    output_file=Path("data/catalog.yml"),
    discover=True  # Auto-discover files
)
```

### Custom Base Path

```python
# Use custom base path for relative file resolution
catalog = DataCatalog(
    catalog_file=Path("config/my_catalog.yml"),
    base_path=Path("/project/root")
)
```

### Batch Operations

```python
# Load multiple datasets
datasets = {}
for name in ["shackles_g2100", "masterlinks_a342", "hooks_s319"]:
    datasets[name] = catalog.load(name)

# Search and load
excel_data = {}
for entry in catalog.search(format="excel"):
    excel_data[entry.name] = catalog.load(entry.name)
```

## Integration with Existing Code

### Example: Replace Direct Excel Loading

**Before:**
```python
import pandas as pd

# Direct loading
shackles = pd.read_excel("data/crosby/204_subsea_shackles_g2100.xlsx")
```

**After:**
```python
from digitalmodel.data import DataCatalog

# Load via catalog
catalog = DataCatalog.load_catalog("data/catalog.yml")
shackles = catalog.load("shackles_g2100")
```

### Benefits

1. **Centralized metadata**: All dataset info in one place
2. **Versioning**: Track file changes automatically
3. **Validation**: Ensure data quality with schemas
4. **Discovery**: Find datasets by tags, format, description
5. **Portability**: Relative paths work across environments

## Testing

Run catalog tests:

```bash
uv run python -m pytest tests/data/test_catalog.py -v
```

All 31 tests should pass:
- CatalogEntry creation and serialization
- Schema validation (strict and permissive modes)
- File discovery and exclusion patterns
- Catalog CRUD operations
- Search and filter
- Version bumping
- Catalog merging
- Data loading with validation

## Best Practices

1. **Use schemas** for important datasets
2. **Add tags** for easy discovery
3. **Update catalog** when adding new data files
4. **Commit catalog.yml** to version control
5. **Use relative paths** for portability
6. **Enable validation** in production
7. **Document standards** in engineering_standard field
8. **Keep descriptions meaningful**

## Troubleshooting

### File Not Found

```python
# Check base_path is correct
catalog = DataCatalog.load_catalog(
    "data/catalog.yml",
    base_path=Path("/correct/project/root")
)
```

### Schema Validation Errors

```python
# Use permissive mode (default)
validator = SchemaValidator(schema_dir, strict=False)

# Or strict mode
validator = SchemaValidator(schema_dir, strict=True)
```

### Version Not Bumping

```python
# Ensure base_path is provided
entry.update_hash(base_path=catalog.base_path)
```

## Future Enhancements

Planned features:
- CLI for catalog management
- Database backend option
- Data quality metrics
- Automated schema inference
- Change history/changelog
- Integration with data lineage tracking
