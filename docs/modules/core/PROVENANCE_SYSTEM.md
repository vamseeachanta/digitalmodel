# Data Provenance Tracking System

## Overview

The Data Provenance Tracking System provides comprehensive lineage tracking for data processing workflows in the digitalmodel project. It tracks data sources, transformations, integrity (via SHA256 hashing), and supports complex provenance graphs for multi-source data operations.

## Features

✅ **Data Lineage Tracking** - Track data from source to output
✅ **SHA256 Integrity Hashing** - Verify data integrity with cryptographic hashing
✅ **Transformation History** - Record all data transformations with parameters
✅ **Multi-Source Support** - Handle provenance graphs for merged datasets
✅ **Export/Import** - JSON and YAML serialization
✅ **Decorator Integration** - `@track_provenance` for automatic tracking
✅ **Query Capabilities** - Find sources and outputs in lineage graph
✅ **Chunked Hashing** - Efficient handling of large files (1MB chunks)

## Quick Start

### Basic Usage

```python
from digitalmodel.core.provenance import DataProvenance, compute_hash
from pathlib import Path

# Create provenance record
prov = DataProvenance(
    source="data/input.csv",
    source_version="1.0",
    source_hash=compute_hash("data/input.csv"),
    metadata={"project": "mooring_analysis"}
)

# Track transformation
prov.add_transformation(
    function_name="normalize_data",
    parameters={"method": "minmax"},
    output_metadata={"shape": (100, 5), "dtype": "float64"}
)

# Save provenance alongside output file
prov.save_alongside("data/output.csv")
# Creates: data/output.csv.provenance.json
```

### Using the Decorator

```python
from digitalmodel.core.provenance import ProvenanceTracker, track_provenance
import pandas as pd

tracker = ProvenanceTracker()

@track_provenance(tracker, source="input.csv", output_key="processed_data")
def process_data(filepath):
    df = pd.read_csv(filepath)
    return df * 2

result = process_data("input.csv")

# Provenance automatically tracked in tracker
tracker.save("provenance_state.json")
```

## Core Components

### 1. DataProvenance

Main provenance record class.

**Fields:**
- `source`: Source file path or database identifier
- `source_version`: Version of source data
- `source_hash`: SHA256 hash of source data
- `created_at`: Timestamp when provenance was created
- `transformations`: List of TransformationRecord objects
- `metadata`: Custom metadata dictionary
- `parent_provenances`: Parent provenance records (for graphs)

**Methods:**
```python
# Add transformation
prov.add_transformation(function_name, parameters, output_metadata)

# Export to JSON/YAML
prov.export_json("output.json", detailed=True)
prov.export_yaml("output.yaml")

# Import from JSON/YAML
prov = DataProvenance.from_json("input.json")
prov = DataProvenance.from_yaml("input.yaml")

# Save alongside data file
prov.save_alongside("data/results.csv")
# Creates: data/results.csv.provenance.json

# Merge multiple provenance records
merged = DataProvenance.merge([prov1, prov2], operation="join")

# Get graph depth
depth = prov.get_depth()  # Max depth: 10 levels

# Export graph for visualization
graph_dict = prov.to_graph_dict()
```

### 2. TransformationRecord

Records a single data transformation.

**Fields:**
- `function_name`: Name of transformation function
- `parameters`: Dictionary of function parameters
- `output_metadata`: Metadata about transformation output
- `timestamp`: When transformation was applied

**Methods:**
```python
record = TransformationRecord(
    function_name="filter_outliers",
    parameters={"threshold": 3.0},
    output_metadata={"removed": 5}
)

record_dict = record.to_dict()
record = TransformationRecord.from_dict(record_dict)
```

### 3. ProvenanceTracker

Manages multiple provenance records with querying capabilities.

```python
tracker = ProvenanceTracker()

# Add records
tracker.add_record("output.csv", provenance_object)

# Retrieve record
prov = tracker.get_record("output.csv")

# Find all sources for an output
sources = tracker.find_sources("output.csv")

# Find all outputs from a source
outputs = tracker.find_outputs("input.csv")

# Save/load tracker state
tracker.save("tracker.json")
tracker.load("tracker.json")
```

### 4. Hash Computation

Compute SHA256 hashes for various data types.

**Supported Types:**
- Files (chunked reading for large files)
- pandas DataFrames
- numpy arrays
- Dictionaries

```python
from digitalmodel.core.provenance import compute_hash

# File
hash1 = compute_hash("data/large_file.csv")

# DataFrame
hash2 = compute_hash(df)

# Numpy array
hash3 = compute_hash(arr)

# Dictionary
hash4 = compute_hash({"key": "value"})
```

### 5. Query Functions

Helper functions for querying provenance relationships.

```python
from digitalmodel.core.provenance import find_sources_for, find_outputs_from

# Find all sources for an output
sources = find_sources_for("output.csv", tracker)

# Find all outputs derived from a source
outputs = find_outputs_from("input.csv", tracker)
```

## Integration Examples

### OrcaFlex Workflow

```python
tracker = ProvenanceTracker()

@track_provenance(tracker, source="model.yml", output_key="simulation")
def run_simulation(model_config):
    # Run OrcaFlex simulation
    results = orcaflex.run(model_config)
    return results

@track_provenance(tracker, source="simulation", output_key="fatigue")
def analyze_fatigue(results):
    # Perform fatigue analysis
    damage = calculate_fatigue_damage(results)
    return damage

# Execute workflow
sim_results = run_simulation(config)
fatigue_damage = analyze_fatigue(sim_results)

# Query lineage
sources = tracker.find_sources("fatigue")
# Returns: ["model.yml"]

# Save complete provenance
tracker.save("workflow_provenance.json")
```

### Multi-Source Data Merge

```python
# Load two datasets
df1 = pd.read_csv("vessel_data.csv")
df2 = pd.read_csv("environmental_data.csv")

# Create provenance for each
prov1 = DataProvenance(
    source="vessel_data.csv",
    source_version="1.0",
    source_hash=compute_hash("vessel_data.csv")
)

prov2 = DataProvenance(
    source="environmental_data.csv",
    source_version="1.0",
    source_hash=compute_hash("environmental_data.csv")
)

# Merge datasets
df_merged = pd.merge(df1, df2, on="timestamp")

# Create merged provenance
merged_prov = DataProvenance.merge(
    [prov1, prov2],
    operation="temporal_join",
    metadata={"join_key": "timestamp"}
)

merged_prov.add_transformation(
    function_name="merge",
    parameters={"on": "timestamp", "how": "inner"},
    output_metadata={"shape": df_merged.shape}
)

# Save merged provenance
merged_prov.save_alongside("merged_data.csv")
```

### YAML/Excel Integration

```python
from digitalmodel.core.provenance import DataProvenance, compute_hash
import yaml

# Load YAML configuration
with open("config.yml") as f:
    config = yaml.safe_load(f)

# Create provenance
prov = DataProvenance(
    source="config.yml",
    source_version="1.0",
    source_hash=compute_hash(config),
    metadata={"config_type": "analysis_parameters"}
)

# Track processing
prov.add_transformation(
    function_name="apply_config",
    parameters=config,
    output_metadata={"status": "success"}
)

# Export provenance
prov.export_yaml("config.provenance.yml")
```

## Provenance Graph Visualization

Provenance graphs can be exported for visualization using D3.js or other graph libraries.

```python
# Create multi-level provenance graph
prov1 = DataProvenance(source="input1.csv", source_version="1.0", source_hash="abc")
prov2 = DataProvenance(source="input2.csv", source_version="1.0", source_hash="def")
merged = DataProvenance.merge([prov1, prov2], operation="concat")

# Export graph structure
graph = merged.to_graph_dict()

# graph = {
#     "nodes": [
#         {"id": 0, "source": "merged(concat)", "hash": "abc12345", "transformations": 0},
#         {"id": 1, "source": "input1.csv", "hash": "abc", "transformations": 0},
#         {"id": 2, "source": "input2.csv", "hash": "def", "transformations": 0}
#     ],
#     "edges": [
#         {"from": 1, "to": 0},
#         {"from": 2, "to": 0}
#     ]
# }

# Use with D3.js for visualization (see HTML_REPORTING_STANDARDS.md)
```

## Export Formats

### JSON Summary (Minimal)

```json
{
  "source": "input.csv",
  "source_version": "1.0",
  "source_hash": "abc123...",
  "transformations": [
    {
      "function_name": "normalize",
      "parameters": {"method": "minmax"},
      "output_metadata": {"shape": [100, 5]},
      "timestamp": "2024-01-15T10:30:00"
    }
  ]
}
```

### JSON Detailed

```json
{
  "source": "input.csv",
  "source_version": "1.0",
  "source_hash": "abc123...",
  "created_at": "2024-01-15T10:00:00",
  "metadata": {
    "project": "mooring_analysis",
    "analyst": "John Doe"
  },
  "transformations": [...],
  "parent_provenances": [...]
}
```

### YAML

```yaml
source: input.csv
source_version: '1.0'
source_hash: abc123...
created_at: '2024-01-15T10:00:00'
metadata:
  project: mooring_analysis
  analyst: John Doe
transformations:
  - function_name: normalize
    parameters:
      method: minmax
    output_metadata:
      shape: [100, 5]
    timestamp: '2024-01-15T10:30:00'
```

## Best Practices

### 1. Always Track Source Hashes

```python
# ✅ GOOD
prov = DataProvenance(
    source="data.csv",
    source_version="1.0",
    source_hash=compute_hash("data.csv")  # Verify integrity
)

# ❌ BAD
prov = DataProvenance(
    source="data.csv",
    source_version="1.0",
    source_hash="unknown"  # No integrity verification
)
```

### 2. Use Descriptive Metadata

```python
# ✅ GOOD
prov = DataProvenance(
    source="vessel_raos.csv",
    source_version="2.1",
    source_hash=hash_value,
    metadata={
        "project": "P001_FPSO_Analysis",
        "analyst": "Jane Smith",
        "purpose": "Frequency domain analysis",
        "quality_check": "passed"
    }
)
```

### 3. Track All Transformation Parameters

```python
# ✅ GOOD
prov.add_transformation(
    function_name="filter_outliers",
    parameters={
        "method": "IQR",
        "threshold": 1.5,
        "columns": ["tension", "angle"]
    },
    output_metadata={
        "rows_removed": 12,
        "output_shape": (988, 3)
    }
)
```

### 4. Use Decorators for Automated Tracking

```python
# ✅ GOOD - Automatic tracking
@track_provenance(tracker, source="input.csv")
def process_pipeline(filepath):
    df = pd.read_csv(filepath)
    df = clean_data(df)
    df = normalize(df)
    return df

# ❌ BAD - Manual tracking (error-prone)
def process_pipeline(filepath):
    df = pd.read_csv(filepath)
    prov.add_transformation(...)  # Easy to forget
    df = clean_data(df)
    # Forgot to track!
    df = normalize(df)
    return df
```

### 5. Save Provenance Alongside Data

```python
# ✅ GOOD - Co-located provenance
df.to_csv("results.csv")
prov.save_alongside("results.csv")
# Creates: results.csv.provenance.json

# ❌ BAD - Separated provenance
df.to_csv("results.csv")
prov.export_json("provenance/2024/january/prov_001.json")
# Hard to find related provenance
```

## Testing

Run the comprehensive test suite:

```bash
cd /d/workspace-hub/digitalmodel
uv run python -m pytest tests/core/test_provenance.py -v
```

Test coverage includes:
- ✅ Provenance creation and metadata
- ✅ Transformation tracking
- ✅ Hash computation (files, DataFrames, arrays, dicts)
- ✅ Large file chunked hashing
- ✅ JSON/YAML export and import
- ✅ Provenance tracker operations
- ✅ Decorator-based tracking
- ✅ Multi-source provenance graphs
- ✅ Provenance querying
- ✅ Integration with data loaders

## Performance Considerations

### Large File Hashing

Large files are hashed in 1MB chunks to avoid memory issues:

```python
# Efficient for files > 100MB
hash_value = compute_hash("large_simulation_results.dat")
# Uses chunked reading internally
```

### Graph Depth Limits

Provenance graphs are limited to 10 levels to prevent performance degradation:

```python
prov = create_deep_chain(15)  # 15 transformation levels
depth = prov.get_depth()  # Returns 10 (capped)
```

### Tracker State Management

For workflows with many outputs, periodically save tracker state:

```python
tracker = ProvenanceTracker()

for i, file in enumerate(large_file_list):
    process_file(file)

    # Save every 100 files
    if i % 100 == 0:
        tracker.save(f"checkpoint_{i}.json")
```

## Integration with digitalmodel Modules

### OrcaFlex Module

```python
from digitalmodel.orcaflex_modeling import run_simulation
from digitalmodel.core.provenance import ProvenanceTracker, track_provenance

tracker = ProvenanceTracker()

@track_provenance(tracker, source="model.yml", output_key="sim_results")
def run_orcaflex_analysis(model_path):
    return run_simulation(model_path)
```

### Fatigue Analysis Module

```python
from digitalmodel.fatigue_analysis import calculate_damage
from digitalmodel.core.provenance import track_provenance

@track_provenance(tracker, source="stress_history.csv", output_key="fatigue_damage")
def perform_fatigue_calculation(stress_file):
    return calculate_damage(stress_file)
```

### Metocean Data Module

```python
from digitalmodel.data_procurement.metocean import fetch_era5_data
from digitalmodel.core.provenance import DataProvenance

data = fetch_era5_data(location, date_range)

prov = DataProvenance(
    source="ERA5_API",
    source_version="v1",
    source_hash=compute_hash(data),
    metadata={
        "location": location,
        "date_range": date_range,
        "variables": ["wave_height", "wave_period"]
    }
)
```

## Future Enhancements

Planned features for future releases:

- 🔄 **Automatic Git Integration** - Link provenance to git commits
- 📊 **D3.js Graph Visualization** - Interactive HTML provenance graphs
- 🔍 **Advanced Querying** - SQL-like queries on provenance data
- 🌐 **Distributed Provenance** - Track provenance across cloud storage
- 🔐 **Digital Signatures** - Cryptographic signing of provenance records
- 📈 **Provenance Analytics** - Statistics on transformation patterns

## Support

For issues or questions:
- Create an issue in the repository
- See `examples/provenance_integration_examples.py` for more examples
- Review test suite: `tests/core/test_provenance.py`

## References

- W3C PROV Model: https://www.w3.org/TR/prov-overview/
- SHA256 Hashing: https://en.wikipedia.org/wiki/SHA-2
- Data Lineage Best Practices: https://docs.aws.amazon.com/prescriptive-guidance/latest/data-lineage/welcome.html
