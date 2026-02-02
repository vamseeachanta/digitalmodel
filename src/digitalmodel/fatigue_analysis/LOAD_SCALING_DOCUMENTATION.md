# Load Scaling Module - Complete Documentation

## Overview

The Load Scaling Module is the first stage in the fatigue analysis pipeline. It takes reference seastate time series data (standardized at wind speed = 10 m/s and wave height Hs = 0.5 m) and scales them to match target fatigue conditions using validated scaling formulas.

## Mathematical Foundation

### Scaling Formulas

#### Wind Load Scaling
The wind load scaling follows a quadratic relationship with wind speed:

```
F_wind_scaled = F_wind_ref × (V_target / V_ref)²

Where:
- F_wind_scaled = Scaled wind load
- F_wind_ref = Reference wind load at 10 m/s
- V_target = Target wind speed (m/s)
- V_ref = Reference wind speed (10 m/s)
```

**Physical Basis**: Wind force is proportional to dynamic pressure, which varies with velocity squared (F ∝ ½ρV²)

#### Wave Load Scaling
The wave load scaling follows a linear relationship with significant wave height:

```
F_wave_scaled = F_wave_ref × (Hs_target / Hs_ref)

Where:
- F_wave_scaled = Scaled wave load
- F_wave_ref = Reference wave load at Hs = 0.5 m
- Hs_target = Target significant wave height (m)
- Hs_ref = Reference wave height (0.5 m)
```

**Physical Basis**: Wave forces are approximately proportional to wave height for small amplitude waves

#### Combined Loading
The total effective tension is obtained by superposition:

```
F_total = F_wind_scaled + F_wave_scaled
```

## Input File Specifications

### 1. Reference Seastates Definition (`reference_seastates.csv`)

Defines the available reference conditions for scaling.

| Column | Type | Description | Example |
|--------|------|-------------|---------|
| reference_id | string | Unique identifier | WD01, W01 |
| type | string | Either "wind" or "wave" | wind |
| wind_speed | float | Wind speed in m/s | 10.0 |
| wind_direction | float | Wind direction in degrees | 0 |
| hs | float | Significant wave height in m | 0.5 |
| tp | float | Peak period in seconds | 2.7 |
| wave_direction | float | Wave direction in degrees | 45 |

### 2. Fatigue Conditions (`fatigue_conditions.csv`)

Specifies the target conditions to scale to.

| Column | Type | Description | Example |
|--------|------|-------------|---------|
| condition_id | int | Unique condition identifier | 1 |
| wind_speed | float | Target wind speed in m/s | 15.0 |
| wind_direction | float | Wind direction in degrees | 180 |
| hs | float | Target wave height in m | 0.75 |
| tp | float | Peak period in seconds | 3.5 |
| wave_direction | float | Wave direction in degrees | 135 |
| occurrence_pct | float | Annual occurrence percentage | 12.5 |

### 3. Time Series Data

Reference time series files containing load measurements.

**Format**: CSV with columns:
- `Time` or `time`: Time in seconds
- `Effective Tension at Vessel End` or similar: Load in kN

**Naming Convention**: `{config}_mwl_{env_type}_Strut{#}.csv`
- Example: `fsts_l015_mwl_wind01_Strut1.csv`

## Output Files

### 1. Scaling Factors (`scaling_factors.csv`)

Documents all calculated scaling factors for traceability.

| Column | Description |
|--------|-------------|
| condition_id | Fatigue condition identifier |
| wind_speed | Target wind speed |
| hs | Target wave height |
| wind_scale_factor | Calculated wind scaling factor |
| wave_scale_factor | Calculated wave scaling factor |
| wind_formula | Formula used for wind scaling |
| wave_formula | Formula used for wave scaling |
| occurrence_pct | Annual occurrence percentage |

### 2. Combined Load Time Series

**Location**: `output/{config}/FC{###}_Strut{#}_combined.csv`

Contains the scaled and combined effective tension time series.

| Column | Description |
|--------|-------------|
| time | Time in seconds |
| effective_tension | Combined tension in kN |

### 3. Processing Summary (`processing_summary.csv`)

Summary statistics for all processed conditions.

| Column | Description |
|--------|-------------|
| condition_id | Fatigue condition ID |
| strut | Strut number |
| wind_scale | Wind scaling factor applied |
| wave_scale | Wave scaling factor applied |
| max_tension | Maximum combined tension |
| mean_tension | Mean combined tension |
| std_tension | Standard deviation |

## Usage Examples

### 1. Basic Usage

```python
from digitalmodel.fatigue_analysis.load_scaling import LoadScaler

# Initialize the scaler
scaler = LoadScaler(
    reference_seastates_path="input/reference_seastates.csv",
    fatigue_conditions_path="input/fatigue_conditions.csv",
    output_dir="output/scaled_loads"
)

# Process all conditions
summary = scaler.process_all_conditions()
```

### 2. Command Line Interface

```bash
# Basic execution
python cli_load_scaling.py

# Specify input files
python cli_load_scaling.py \
    --reference-seastates ref_seastates.csv \
    --fatigue-conditions fatigue_conds.csv \
    --output-dir output/

# Process specific configuration
python cli_load_scaling.py --config fsts_l015

# Batch process with parallel execution
python cli_load_scaling.py --batch --parallel 4

# Test mode with validation
python cli_load_scaling.py --test --validate --verbose
```

### 3. Running the Example

```bash
# Run the complete example with sample data
python examples/load_scaling_runner.py

# Run validation tests
python test_load_scaling.py
```

## Validation and Quality Checks

### 1. Scaling Factor Validation

The module performs automatic validation of scaling factors:

- **Range Checks**: Ensures factors are within reasonable bounds
- **Sum Validation**: Verifies occurrence percentages sum to 100%
- **Formula Verification**: Documents exact formulas used

### 2. Time Series Validation

- **Length Matching**: Ensures wind and wave time series have compatible lengths
- **Value Range**: Checks for unrealistic values (negative loads, etc.)
- **Statistics**: Computes and reports key statistics for verification

### 3. Test Suite

Run the comprehensive test suite to validate calculations:

```bash
python test_load_scaling.py
```

Tests include:
- Wind scaling formula (quadratic)
- Wave scaling formula (linear)
- Load combination (superposition)
- Edge cases (zero values, extreme values)
- File generation and format

## Configuration Options

### Processing Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| base_wind_speed | 10.0 m/s | Reference wind speed |
| base_hs | 0.5 m | Reference wave height |
| time_step | 0.1 s | Time series sampling interval |
| duration | 200 s | Time series duration |

### CLI Options

| Option | Description |
|--------|-------------|
| `--validate` | Validate inputs before processing |
| `--verbose` | Enable detailed logging |
| `--dry-run` | Show what would be done without executing |
| `--no-save` | Process without saving individual files |
| `--parallel N` | Use N workers for parallel processing |

## Performance Considerations

### Memory Usage

- Each time series: ~16 KB (2000 points × 8 bytes)
- Per condition-strut: ~100 KB including intermediate data
- Total for 81 conditions × 8 struts: ~65 MB

### Processing Time

- Single condition-strut: ~0.1 seconds
- Full configuration (81 × 8): ~1 minute (sequential)
- With parallel processing (4 workers): ~15 seconds

### Optimization Tips

1. **Use parallel processing** for multiple configurations
2. **Pre-calculate scaling factors** (done automatically)
3. **Batch process** similar conditions together
4. **Use binary format** (NPZ) for large datasets

## Troubleshooting

### Common Issues

1. **Missing Reference Data**
   - Error: "Reference file not found"
   - Solution: Ensure all reference seastates have corresponding time series files

2. **Occurrence Sum ≠ 100%**
   - Warning: "Occurrence sum: 95.5% (should be 100%)"
   - Solution: Adjust occurrence percentages to sum to 100%

3. **Memory Issues with Large Datasets**
   - Error: "MemoryError"
   - Solution: Process in batches or reduce time series length

### Debug Mode

Enable verbose logging for detailed troubleshooting:

```bash
python cli_load_scaling.py --verbose --validate
```

This will show:
- Detailed processing steps
- Intermediate calculations
- File I/O operations
- Timing information

## Integration with Pipeline

The load scaling module outputs are designed to feed directly into the next stage (Rainflow Counting):

```python
# Output from Load Scaling
scaled_loads_dir = "output/scaled_loads/"

# Input to Rainflow Counting
from digitalmodel.fatigue_analysis.rainflow_counting import RainflowCounter

counter = RainflowCounter()
counter.process_batch(scaled_loads_dir, pattern="*.npz")
```

## References

1. **Wind Load Scaling**: Based on dynamic pressure relationship (Bernoulli's equation)
2. **Wave Load Scaling**: Linear wave theory for small amplitude waves
3. **Superposition Principle**: Linear elastic response assumption
4. **ASTM Standards**: Reference for fatigue analysis procedures

## Version History

- v1.0.0: Initial implementation with basic scaling
- v1.1.0: Added parallel processing support
- v1.2.0: Enhanced validation and error handling
- v1.3.0: Added CLI interface and batch processing

---

*Last Updated: 2024*
*Module Version: 1.3.0*