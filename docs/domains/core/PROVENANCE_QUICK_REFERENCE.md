# Data Provenance System - Quick Reference

## Installation

No installation needed - part of `digitalmodel.core` module.

```python
from digitalmodel.core.provenance import (
    DataProvenance,
    ProvenanceTracker,
    track_provenance,
    compute_hash,
    find_sources_for,
    find_outputs_from,
)
```

## Common Use Cases

### 1. Track Single File Transformation

```python
from digitalmodel.core.provenance import DataProvenance, compute_hash

prov = DataProvenance(
    source="input.csv",
    source_version="1.0",
    source_hash=compute_hash("input.csv")
)

prov.add_transformation(
    function_name="clean_data",
    parameters={"remove_nulls": True},
    output_metadata={"rows": 1000}
)

prov.save_alongside("output.csv")
# Creates: output.csv.provenance.json
```

### 2. Automatic Tracking with Decorator

```python
from digitalmodel.core.provenance import ProvenanceTracker, track_provenance

tracker = ProvenanceTracker()

@track_provenance(tracker, source="data.csv", output_key="processed")
def process(filepath):
    df = pd.read_csv(filepath)
    return df * 2

result = process("data.csv")
tracker.save("provenance.json")
```

### 3. Multi-Source Data Merge

```python
prov1 = DataProvenance(source="file1.csv", source_version="1.0", source_hash=hash1)
prov2 = DataProvenance(source="file2.csv", source_version="1.0", source_hash=hash2)

merged = DataProvenance.merge([prov1, prov2], operation="join")
merged.add_transformation("merge", {"on": "id"}, {"rows": 500})
```

### 4. Query Provenance Lineage

```python
tracker = ProvenanceTracker()
# ... add records ...

# Find all sources for an output
sources = tracker.find_sources("final_output.csv")

# Find all outputs from a source
outputs = tracker.find_outputs("raw_data.csv")
```

### 5. Export/Import

```python
# Export
prov.export_json("prov.json", detailed=True)
prov.export_yaml("prov.yaml")

# Import
prov = DataProvenance.from_json("prov.json")
prov = DataProvenance.from_yaml("prov.yaml")
```

## Hash Computation

```python
from digitalmodel.core.provenance import compute_hash

# File
hash1 = compute_hash("large_file.dat")  # Uses 1MB chunks

# DataFrame
hash2 = compute_hash(df)

# Numpy array
hash3 = compute_hash(arr)

# Dictionary
hash4 = compute_hash({"key": "value"})
```

## API Reference

### DataProvenance

| Method | Description |
|--------|-------------|
| `add_transformation(name, params, metadata)` | Track a transformation |
| `export_json(path, detailed=True)` | Export to JSON |
| `export_yaml(path)` | Export to YAML |
| `from_json(path)` | Import from JSON |
| `from_yaml(path)` | Import from YAML |
| `save_alongside(data_path)` | Save as `data_path.provenance.json` |
| `merge(provenances, operation)` | Merge multiple provenance records |
| `get_depth()` | Get graph depth (max 10) |
| `to_graph_dict()` | Export graph for visualization |

### ProvenanceTracker

| Method | Description |
|--------|-------------|
| `add_record(key, provenance)` | Add provenance record |
| `get_record(key)` | Retrieve provenance |
| `find_sources(output_key)` | Find all sources for output |
| `find_outputs(source_key)` | Find all outputs from source |
| `save(path)` | Save tracker state to JSON |
| `load(path)` | Load tracker state from JSON |

### Functions

| Function | Description |
|----------|-------------|
| `compute_hash(data)` | SHA256 hash for file/DataFrame/array/dict |
| `track_provenance(tracker, source, output_key)` | Decorator for auto-tracking |
| `find_sources_for(output, tracker)` | Query sources for output |
| `find_outputs_from(source, tracker)` | Query outputs from source |

## Field Reference

### DataProvenance Fields

```python
DataProvenance(
    source: str,              # Source file/database
    source_version: str,      # Version identifier
    source_hash: str,         # SHA256 hash
    created_at: datetime,     # Auto-set
    transformations: List,    # Transformation records
    metadata: Dict,           # Custom metadata
    parent_provenances: List  # For graphs
)
```

### TransformationRecord Fields

```python
TransformationRecord(
    function_name: str,       # Function that transformed data
    parameters: Dict,         # Function parameters
    output_metadata: Dict,    # Output characteristics
    timestamp: datetime       # Auto-set
)
```

## File Naming Convention

Provenance files are saved alongside data files:

```
data/
  input.csv
  input.csv.provenance.json          # Provenance for input.csv
  processed.csv
  processed.csv.provenance.json      # Provenance for processed.csv
  results.xlsx
  results.xlsx.provenance.json       # Provenance for results.xlsx
```

## Best Practices Checklist

- [ ] Always compute source hash for integrity verification
- [ ] Use descriptive metadata (project, analyst, purpose)
- [ ] Track all transformation parameters
- [ ] Save provenance alongside data files
- [ ] Use decorators for automated tracking
- [ ] Query provenance before data reuse
- [ ] Export to version control-friendly formats (JSON/YAML)
- [ ] Document custom transformation functions

## Examples Location

See full examples:
- `examples/provenance_integration_examples.py`
- `tests/core/test_provenance.py`
- `docs/core/PROVENANCE_SYSTEM.md`

## Testing

```bash
# Run all provenance tests
uv run python -m pytest tests/core/test_provenance.py -v

# Run integration examples
uv run python examples/provenance_integration_examples.py
```

## Performance Tips

1. **Large Files**: Use `compute_hash()` - automatically uses 1MB chunks
2. **Many Outputs**: Save tracker state periodically (`tracker.save()`)
3. **Deep Graphs**: Limited to 10 levels automatically
4. **Memory**: Use `detailed=False` for summary exports

## Troubleshooting

**Problem**: Hash mismatch between runs
**Solution**: Ensure identical data format (DataFrame column order matters)

**Problem**: Provenance file not found
**Solution**: Use `save_alongside()` to ensure co-location

**Problem**: Deep graph performance
**Solution**: Depth automatically capped at 10 levels

**Problem**: Large provenance files
**Solution**: Use `detailed=False` for exports

## Integration Patterns

### Pattern 1: Single-File Processing

```python
prov = DataProvenance(source=file, source_version="1.0", source_hash=compute_hash(file))
prov.add_transformation(...)
prov.save_alongside(output_file)
```

### Pattern 2: Pipeline with Tracker

```python
tracker = ProvenanceTracker()

@track_provenance(tracker, source="input.csv")
def step1(data): ...

@track_provenance(tracker, source="step1_output")
def step2(data): ...

tracker.save("pipeline_provenance.json")
```

### Pattern 3: Multi-Source Merge

```python
prov_list = [create_provenance(f) for f in input_files]
merged = DataProvenance.merge(prov_list, operation="concat")
merged.save_alongside(merged_file)
```

## Quick Commands

```python
# Create
prov = DataProvenance(source, version, hash)

# Track
prov.add_transformation(name, params, metadata)

# Save
prov.save_alongside("output.csv")

# Query
tracker.find_sources("output.csv")
tracker.find_outputs("input.csv")

# Export
prov.export_json("prov.json")
prov.export_yaml("prov.yaml")
```
