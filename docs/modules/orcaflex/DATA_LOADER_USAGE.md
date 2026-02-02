# CALM Buoy Data Loader - Usage Guide

## Quick Start

```python
from pathlib import Path
from digitalmodel.orcaflex.modular_input_validation.data_loader import CALMBuoyDataLoader

# Initialize loader
data_dir = Path('/path/to/digitalmodel/data')
loader = CALMBuoyDataLoader(data_dir)

# Load all reference data
data = loader.load_all()

# Access specific data
print(f"Hull parameters: {len(data.hull_geometry)}")
print(f"Metocean parameters: {len(data.metocean)}")
print(f"Mooring parameters: {len(data.mooring_capacity)}")
```

## Loading Individual Data Sources

### Hull Geometry Ranges

```python
hull_ranges = loader.load_hull_geometry_ranges()

for param, range_obj in hull_ranges.items():
    print(f"{param}:")
    print(f"  Min: {range_obj.min_value}")
    print(f"  Max: {range_obj.max_value}")
    print(f"  Basis: {range_obj.reference_basis}")
```

**Example Output:**
```
skirt_diameter:
  Min: 8.0
  Max: 16.0
  Basis: OCIMF Offshore CALM Guidance
```

### Metocean Design Ranges

```python
metocean_ranges = loader.load_metocean_ranges()

# Parameters are named: {condition}_{parameter_type}
# Example: operational_sea_state_hs
for param, range_obj in metocean_ranges.items():
    print(f"{param}: {range_obj.min_value} - {range_obj.max_value}")
```

**Example Output:**
```
operational_sea_state_hs: 1.5 - 3.5
operational_sea_state_tp: 7.0 - 11.0
operational_sea_state_surface_current: 0.3 - 0.8
operational_sea_state_wind_speed: 8.0 - 15.0
```

### Mooring Capacity Ranges

```python
mooring_ranges = loader.load_mooring_capacity_ranges()

# Parameters are named: {component}_{capacity|safety_factor}
# Example: top_chain_studless_R4_capacity
for param, range_obj in mooring_ranges.items():
    print(f"{param}: {range_obj.min_value} - {range_obj.max_value}")
```

**Example Output:**
```
top_chain_studless_R4_capacity: 6500.0 - 8200.0
top_chain_studless_R4_safety_factor: 2.5 - 3.5
```

### Environmental Conditions

```python
env_conditions = loader.load_environmental_conditions()

for sea_state_id, params in env_conditions.items():
    print(f"{sea_state_id}:")
    print(f"  Hs: {params['hs']} m")
    print(f"  Tp: {params['tp']} s")
    print(f"  Wind: {params['wind_speed']} m/s")
    print(f"  Current: {params['surface_current_speed']} m/s")
```

**Example Output:**
```
operational_case_ocimf:
  Hs: 3.0 m
  Tp: 9.0 s
  Wind: 12.0 m/s
  Current: 0.6 m/s
```

### Mooring Line Properties

```python
mooring_lines = loader.load_mooring_line_properties()

for line_id, props in mooring_lines.items():
    print(f"{line_id}:")
    print(f"  Length: {props['length']} m")
    print(f"  MBL: {props['mbl']} kN")
    print(f"  Diameter: {props['diameter']} m")
```

**Example Output:**
```
L1_OCIMF_top_chain:
  Length: 320.0 m
  MBL: 8000.0 kN
  Diameter: 0.084 m
```

## Parameter Lookup

```python
# Search across all data sources
range_obj = loader.get_parameter_range('skirt_diameter')

if range_obj:
    print(f"Found: {range_obj.min_value} - {range_obj.max_value}")
    print(f"Basis: {range_obj.reference_basis}")
else:
    print("Parameter not found")
```

## Using load_all()

```python
# Load everything at once
data = loader.load_all()

# Access via attributes
data.hull_geometry         # Dict[str, ParameterRange]
data.metocean             # Dict[str, ParameterRange]
data.mooring_capacity     # Dict[str, ParameterRange]
data.environmental_conditions  # Dict[str, Dict]
data.mooring_line_properties  # Dict[str, Dict]
```

## Validation Example

```python
from digitalmodel.orcaflex.modular_input_validation.data_loader import CALMBuoyDataLoader

# Load reference data
loader = CALMBuoyDataLoader(Path('data'))
data = loader.load_all()

# Validate user input
user_skirt_diameter = 12.5  # meters

skirt_range = data.hull_geometry.get('skirt_diameter')
if skirt_range:
    is_valid = skirt_range.min_value <= user_skirt_diameter <= skirt_range.max_value

    if is_valid:
        print(f"✓ Skirt diameter {user_skirt_diameter}m is within valid range")
    else:
        print(f"✗ Skirt diameter {user_skirt_diameter}m is outside range "
              f"[{skirt_range.min_value}, {skirt_range.max_value}]")
        print(f"  Reference: {skirt_range.reference_basis}")
```

## Error Handling

The loader handles errors gracefully:

```python
from pathlib import Path

# Non-existent directory
loader = CALMBuoyDataLoader(Path('/fake/path'))
data = loader.load_all()
# No exception raised, returns empty dictionaries

# Missing CSV file
hull_ranges = loader.load_hull_geometry_ranges()
# Returns empty dict, logs warning

# Malformed CSV
# Skips invalid rows, logs warning, continues processing
```

## Performance Considerations

### Caching

The loader uses `@lru_cache` to cache results:

```python
# First call: reads CSV file
hull_ranges_1 = loader.load_hull_geometry_ranges()

# Second call: returns cached result (faster)
hull_ranges_2 = loader.load_hull_geometry_ranges()

# Same object returned
assert hull_ranges_1 is hull_ranges_2
```

### Memory Usage

Each cached method stores up to 10 different parameter combinations:

```python
@lru_cache(maxsize=10)
def load_hull_geometry_ranges(self) -> Dict[str, ParameterRange]:
    ...
```

For typical usage, this is more than sufficient.

## Data Structures

### ParameterRange

```python
@dataclass
class ParameterRange:
    parameter: str              # Parameter name
    min_value: Optional[float]  # Minimum value (None if unbounded)
    max_value: Optional[float]  # Maximum value (None if unbounded)
    reference_basis: str        # Citation/standard reference
    notes: str                  # Additional context
```

### CALMBuoyReferenceData

```python
@dataclass
class CALMBuoyReferenceData:
    hull_geometry: Dict[str, ParameterRange]
    metocean: Dict[str, ParameterRange]
    mooring_capacity: Dict[str, ParameterRange]
    environmental_conditions: Dict[str, Dict]
    mooring_line_properties: Dict[str, Dict]
```

## CSV File Locations

The loader expects CSV files at these locations relative to `data_dir`:

**Generic Range Data:**
- `raw/calm_buoy/generic_range/hull_geometry_ranges.csv`
- `raw/calm_buoy/generic_range/metocean_design_ranges.csv`
- `raw/calm_buoy/generic_range/mooring_capacity_ranges.csv`

**Project-Specific Data:**
- `results/calm_buoy/project_specific/environmental_conditions.csv`
- `results/calm_buoy/project_specific/mooring_line_properties.csv`
- `results/calm_buoy/project_specific/offloading_configuration.csv`
- `results/calm_buoy/project_specific/project_metadata.csv`

## Best Practices

1. **Initialize once:** Create the loader once and reuse it
2. **Use load_all() for complete datasets:** More efficient than individual calls
3. **Check for None:** Always validate that parameters exist before using
4. **Handle missing files gracefully:** The loader returns empty dicts, check lengths
5. **Read reference_basis:** Important for understanding parameter context

## Common Patterns

### Pattern 1: Validate Multiple Parameters

```python
loader = CALMBuoyDataLoader(data_dir)
data = loader.load_all()

parameters_to_check = {
    'skirt_diameter': 12.5,
    'hull_draft': 10.0,
    'freeboard': 5.0
}

for param_name, value in parameters_to_check.items():
    range_obj = data.hull_geometry.get(param_name)
    if range_obj:
        is_valid = range_obj.min_value <= value <= range_obj.max_value
        status = "✓" if is_valid else "✗"
        print(f"{status} {param_name}: {value}")
```

### Pattern 2: Generate Validation Report

```python
loader = CALMBuoyDataLoader(data_dir)
data = loader.load_all()

print("Available Parameters:")
print("\nHull Geometry:")
for param, range_obj in data.hull_geometry.items():
    print(f"  {param}: [{range_obj.min_value}, {range_obj.max_value}]")

print("\nMetocean Conditions:")
for param, range_obj in data.metocean.items():
    print(f"  {param}: [{range_obj.min_value}, {range_obj.max_value}]")
```

### Pattern 3: Extract Reference Citations

```python
loader = CALMBuoyDataLoader(data_dir)
data = loader.load_all()

references = set()
for range_obj in data.hull_geometry.values():
    if range_obj.reference_basis:
        references.add(range_obj.reference_basis)

print("Reference Standards Used:")
for ref in sorted(references):
    print(f"  - {ref}")
```

## Testing

Use the provided test script:

```bash
cd D:/workspace-hub/digitalmodel
python scripts/test_calm_data_loader.py
```

## Troubleshooting

**Problem:** No data loaded (empty dictionaries)
- **Solution:** Check that `data_dir` path is correct
- **Solution:** Verify CSV files exist at expected locations

**Problem:** Partial data loaded
- **Solution:** Check console for warning messages
- **Solution:** Verify CSV file structure matches expected format

**Problem:** ValueError on numeric conversion
- **Solution:** Check CSV for non-numeric values in min/max columns
- **Solution:** Review warning messages for specific rows

## See Also

- Full verification report: `docs/modules/orcaflex/DATA_LOADER_VERIFICATION.md`
- Test summary: `scripts/CALM_DATA_LOADER_TEST_SUMMARY.md`
- Test script: `scripts/test_calm_data_loader.py`
- Source code: `src/digitalmodel/modules/orcaflex/modular_input_validation/data_loader.py`
