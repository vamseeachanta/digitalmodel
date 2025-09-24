# Master Input Configuration Guide
## Fatigue Analysis Pipeline

This guide explains the master input configuration system that consolidates all input parameters and file references for the fatigue analysis pipeline.

## Configuration Files

### 1. Sample Configuration
**File**: `input/master_input_config_sample.yml`
- Pre-configured for verification testing
- Contains all sample data references
- Ready to use with provided test data

### 2. Production Template
**File**: `input/master_input_config_production.yml`
- Template for actual project analysis
- Requires updating with project-specific values
- Includes comprehensive documentation

## Configuration Structure

### Main Sections

```yaml
default:           # Main configuration section
  analysis_name    # Project identification
  parameters       # Analysis parameters
  reference_files  # Reference condition inputs
  seastate_files   # Sea state condition inputs
  fatigue_conditions  # Metocean data
  scaling_factors  # Wind/wave scaling
  tension_to_stress  # Stress conversion
  sn_curve        # Fatigue curve parameters
  rainflow        # Counting settings
  output          # Output configuration
  validation      # Quality thresholds
  processing      # Execution options

metadata:         # Document metadata
  created_date
  version
  notes
```

## Key Configuration Items

### 1. Analysis Parameters
```yaml
parameters:
  sample_duration_seconds: 100     # Length of time series
  time_step_seconds: 0.1           # Sample rate
  sample_points: 1000              # Total data points
  seconds_per_year: 31557600       # Annual scaling
  design_life_years: 20            # Target life
```

### 2. Fatigue Conditions
```yaml
fatigue_conditions:
  data:
    FC001:
      wind_speed_mps: 5      # Wind speed (m/s)
      hs_m: 0.15            # Significant wave height (m)
      tp_s: 2.0             # Peak period (s)
      occurrence_pct: 20    # Annual occurrence (%)
```

**Critical**: Occurrence percentages MUST sum to 100%

### 3. Input File References

#### Reference Files
```yaml
reference_files:
  base_directory: "input/reference"
  files:
    - "fsts_surge_Strut1.csv"
    - "fsts_sway_Strut1.csv"
    # ... all 6 DOF
  column_name: "Effective Tension (kN)"
```

#### Sea State Files
```yaml
seastate_files:
  base_directory: "input/seastate"
  files:
    FC001: "fsts_FC001_SS_Strut1.csv"
    FC002: "fsts_FC002_SS_Strut1.csv"
    # ... all fatigue conditions
```

### 4. Scaling Factors
```yaml
scaling_factors:
  wind:
    reference_speed_mps: 10
    exponent: 2              # (V/10)^2
  wave:
    reference_hs_m: 0.5
    exponent: 1              # Hs/0.5
```

### 5. Tension-to-Stress Conversion
```yaml
tension_to_stress:
  file: "input/tension_range_to_stress_range_function.csv"
  conversion_table:
    - tension_kN: 0
      stress_MPa: 0.0
    - tension_kN: 500
      stress_MPa: 125.0
    # ... complete table
  conversion_factor: 0.25   # MPa/kN (includes SCF)
  includes_scf: true
```

### 6. S-N Curve Parameters
```yaml
sn_curve:
  curve_type: "DNV-C"
  parameters:
    a: 6.0e11              # Fatigue strength coefficient
    m: 3.0                 # Fatigue strength exponent
  stress_units: "MPa"
```

## Loading the Configuration

### Python Usage
```python
from load_master_config import FatigueConfigLoader

# Load configuration
loader = FatigueConfigLoader("input/master_input_config_sample.yml")
config = loader.load_config()

# Validate
is_valid = loader.validate_config()

# Access parameters
design_life = loader.get_parameter('default.parameters.design_life_years')
sn_a = loader.get_parameter('default.sn_curve.parameters.a')
```

### Validation Checks

The configuration loader performs automatic validation:

1. **Completeness**: All required sections present
2. **Consistency**: Parameters are self-consistent
3. **Occurrence Sum**: Fatigue conditions sum to 100%
4. **File Existence**: Input files exist (for sample)
5. **Parameter Ranges**: Values are physically reasonable

## Pipeline Integration

The master configuration drives all 12 steps:

| Step | Configuration Items Used |
|------|-------------------------|
| 1-5 | `reference_files`, `seastate_files` |
| 6 | `scaling_factors`, `fatigue_conditions` |
| 7 | `rainflow`, `parameters` (normalization) |
| 8 | `tension_to_stress` |
| 9 | `sn_curve` |
| 10 | Damage calculation (uses Step 7-9 results) |
| 11 | Aggregation (no additional config) |
| 12 | `validation.min_fatigue_life_years` |

## Production Setup Checklist

When preparing for production analysis:

- [ ] Copy `master_input_config_production.yml` template
- [ ] Update project identification fields
- [ ] Set correct file paths for all inputs
- [ ] Verify fatigue conditions from metocean study
- [ ] Confirm occurrence percentages sum to 100%
- [ ] Update tension-to-stress calibration
- [ ] Select appropriate S-N curve
- [ ] Set design life requirement
- [ ] Configure output directories
- [ ] Run validation: `python load_master_config.py`

## Common Issues

### 1. Occurrence Sum
**Problem**: Total occurrence ≠ 100%
**Solution**: Adjust occurrence values, typically due to rounding

### 2. File Not Found
**Problem**: Input files not at specified paths
**Solution**: Update `base_directory` or file names

### 3. Inconsistent Parameters
**Problem**: `sample_points ≠ duration / time_step`
**Solution**: Ensure consistency in timing parameters

## Benefits of Master Configuration

1. **Single Source of Truth**: All parameters in one place
2. **Version Control**: Track configuration changes
3. **Reproducibility**: Exact analysis can be repeated
4. **Validation**: Automatic checking before analysis
5. **Documentation**: Self-documenting parameters
6. **Portability**: Easy to share complete setup

## Next Steps

1. Review and validate sample configuration
2. Run test analysis with sample data
3. Create production configuration for your project
4. Validate production configuration
5. Execute full fatigue analysis pipeline