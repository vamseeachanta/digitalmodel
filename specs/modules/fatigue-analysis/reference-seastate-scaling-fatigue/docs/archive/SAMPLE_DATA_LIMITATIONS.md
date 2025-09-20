# Sample Data Limitations

## Overview
The sample data provided in this directory is for **demonstration and testing purposes only** and does not contain the complete dataset required for full production fatigue analysis.

## Available Sample Data

### Current Sample Files
The `sample_timetraces/` directory contains only 6 example files:
- `W01_S1.csv` - Wave case 01, Strut 1
- `W01_S2.csv` - Wave case 01, Strut 2
- `W02_S1.csv` - Wave case 02, Strut 1
- `W02_S2.csv` - Wave case 02, Strut 2
- `WD01_S1.csv` - Wind case 01, Strut 1
- `WD01_S2.csv` - Wind case 01, Strut 2

### Configuration Coverage
Sample data represents only partial coverage of Configuration 1 (FSTs Light):
- **Configuration 1 (FSTs L015)**: Partial sample data only
- **Configuration 2 (FSTs L095)**: No sample data
- **Configuration 3 (FSTs L015 + LNGC Full)**: No sample data
- **Configuration 4 (FSTs L095 + LNGC Light)**: No sample data

## Full Dataset Requirements

### Complete Analysis Needs
For production fatigue analysis, the following data is required:

#### Per Configuration:
- 34 reference seastates (18 wave + 16 wind)
- 8 struts per seastate
- 272 time trace files per configuration
- Total: 1,088 time trace files for all 4 configurations

#### File Naming Pattern:
```
{config_prefix}_{seastate_id}_{strut_id}.csv

Examples:
- fsts_l015_W01_S1.csv
- fsts_l095_W18_S8.csv
- fsts_l015_lngc_full_WD16_S4.csv
- fsts_l095_lngc_light_W10_S6.csv
```

## Testing with Sample Data

### What Can Be Tested:
1. **Code Structure**: Verify the fatigue analysis pipeline works
2. **Scaling Calculations**: Test scaling factor application
3. **File I/O**: Validate metadata and time trace loading
4. **Output Generation**: Check output file structure creation
5. **Error Handling**: Test missing file scenarios

### What Cannot Be Tested:
1. **Full Configuration Coverage**: Only partial Config 1 available
2. **Complete Fatigue Damage**: Missing most reference seastates
3. **Cross-Configuration Comparison**: Need all 4 configs
4. **Production Performance**: Limited data size
5. **Statistical Validation**: Insufficient data points

## Working with Limited Data

### Development Approach:
```python
# Check available configurations before processing
available_configs = []
for config_name, config_id in configurations.items():
    # Check if any files exist for this configuration
    pattern = f"{config_id}_*.csv"
    if glob.glob(os.path.join(timetraces_dir, pattern)):
        available_configs.append(config_name)
        print(f"✓ Found data for: {config_name}")
    else:
        print(f"✗ No data for: {config_name}")

# Process only available configurations
if not available_configs:
    print("WARNING: No configuration data found. Using synthetic data for testing.")
    # Generate synthetic data for testing
else:
    print(f"Processing {len(available_configs)} available configurations...")
```

### Synthetic Data Generation:
For testing when full data is unavailable:
```python
def generate_synthetic_trace(config_id, seastate_type, duration=10800, sample_rate=0.1):
    """Generate synthetic time trace for testing"""
    time_points = int(duration / sample_rate)
    time = np.arange(time_points) * sample_rate
    
    if seastate_type == 'wind':
        # Wind: steady with turbulence
        mean_load = 150 if 'l095' in config_id else 100
        trace = np.random.normal(mean_load, 20, time_points)
        trace += 30 * np.sin(2 * np.pi * 0.01 * time)
    else:  # wave
        # Wave: periodic with noise
        mean_load = 120 if 'l095' in config_id else 80
        trace = mean_load + 50 * np.sin(2 * np.pi * 0.125 * time)
        trace += np.random.normal(0, 5, time_points)
    
    return time, trace
```

## Data Acquisition Path

### For Complete Analysis:
1. **FST Configurations (1 & 2)**:
   - Source: OrcaFlex simulations fat001-fat068
   - Location: `D:\1522\ctr7\orcaflex\rev_a08\07c_fatigue\`
   - Status: Available in production environment

2. **LNGC Configurations (3 & 4)**:
   - Source: New OrcaFlex simulations required
   - Setup: FST + LNGC moored configuration
   - Status: To be generated

### Validation Data:
- Manual calculations for selected conditions
- Previous fatigue analysis results
- Design basis documentation

## Recommendations

1. **For Development**: Use sample data with synthetic generation for missing files
2. **For Testing**: Create unit tests that don't depend on full dataset
3. **For Validation**: Process available FST configurations first
4. **For Production**: Ensure all 1,088 time traces are available before final analysis

## Contact for Full Data
For access to complete time trace datasets:
- Engineering team for FST configurations
- Simulation team for LNGC configurations
- Project manager for data transfer approvals