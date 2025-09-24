# Load Scaling Program Documentation

## Overview

The Load Scaling Program implements a comprehensive methodology for fatigue analysis using reference seastate scaling. It processes reference tension time series data and applies scaling factors based on environmental conditions to generate scaled load cases for fatigue assessment.

## Program Architecture

### Core Components

1. **LoadScalingProcessor** (`load_scaling.py`)
   - Main processing engine
   - Handles data loading, scaling calculations, and output generation
   - Implements reference selection logic
   - Manages scaling factor application

2. **CLI Interface** (`cli_load_scaling.py`)
   - Command-line interface for user interaction
   - Configuration validation
   - Template generation
   - Progress reporting

3. **Configuration System**
   - YAML-based configuration
   - Comprehensive parameter control
   - Validation and error handling

## Input Requirements

### 1. Configuration File (YAML)
Located at: `input/load_scaling_config.yml`

Key sections:
- **input_data**: Paths to reference and fatigue seastate files
- **load_scaling**: Scaling methodology parameters
- **output**: Output file specifications
- **execution**: Processing options

### 2. Reference Seastate Metadata
File: `input/reference_seastate_definitions_sample.csv`

Contains:
- Load case definitions
- Environmental conditions (wind/wave)
- Vessel configurations
- Reference identifiers

### 3. Fatigue Seastates
File: `input/fatigue_seastates_sample.csv`

Contains:
- Wind speed and direction
- Wave height (Hs) and period (Tp)
- Wave direction
- Occurrence percentages

### 4. Reference Time Series Data
Folder: `reference_data/`

Contains:
- Effective tension time series for each reference condition
- Named according to convention: `{config}_mwl_{env_ref}_Strut{n}.csv`

## Processing Methodology

### Step 1: Reference Selection
For each fatigue seastate:
- **Wind Reference**: Selected by closest direction match
- **Wave Reference**: Selected by direction and period (Tp)

### Step 2: Scaling Factor Calculation
- **Wind Scaling**: `(V_fatigue / V_reference)²`
- **Wave Scaling**: `Hs_fatigue / Hs_reference`

### Step 3: Load Combination
- **Formula**: `Effective_tension = scaled_wind_tension + scaled_wave_tension`
- Linear superposition of scaled components

### Step 4: Output Generation
- Scaled tension time series for each combination
- Summary reports with applied scaling factors

## Output Structure

### File Naming Convention
```
{config}_FC{###}_Strut{#}_scaled_tension.csv
```

### Output Files Include:
1. **Scaled Tension Time Series**
   - Time vector
   - Scaled tension values
   - Applied scaling factors

2. **Summary Report**
   - All scaling factors applied
   - Reference selections
   - Match quality indicators

## Usage Examples

### Basic Execution
```bash
python run_load_scaling.py
```

### Using CLI with Custom Configuration
```bash
python cli_load_scaling.py path/to/config.yml
```

### Validation Only
```bash
python cli_load_scaling.py config.yml --validate-only
```

### Generate Template Configuration
```bash
python cli_load_scaling.py --generate-template > new_config.yml
```

### Process Specific Configurations
```bash
python cli_load_scaling.py config.yml --configs fsts_l015 fsts_l095 --struts 1 2 3 4
```

## Configuration Parameters

### Key Scaling Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| Wind Exponent | 2.0 | Power for wind scaling |
| Wave Exponent | 1.0 | Power for wave scaling |
| Max Wind Factor | 100.0 | Maximum allowed wind scaling |
| Min Wind Factor | 0.01 | Minimum allowed wind scaling |
| Max Wave Factor | 10.0 | Maximum allowed wave scaling |
| Min Wave Factor | 0.1 | Minimum allowed wave scaling |

### Vessel Configurations

- `fsts_l015`: Light load (15%)
- `fsts_l095`: Full load (95%)
- `fsts_l015_125km3_l100_pb`: Light with port ballast
- `fsts_l095_125km3_l000_pb`: Full with no ballast

## Error Handling

The program includes comprehensive error handling:
- File existence validation
- Data format verification
- Scaling limit enforcement
- Occurrence sum validation
- Missing data handling

## Performance Considerations

- **Caching**: Reference data cached in memory
- **Parallel Processing**: Configurable worker threads
- **Batch Processing**: Efficient handling of multiple cases
- **Progress Reporting**: Real-time status updates

## Validation Features

1. **Pre-processing Validation**
   - File existence checks
   - Column name verification
   - Data range validation
   - Occurrence sum verification (should equal 100%)

2. **Post-processing Validation**
   - Output completeness check
   - Scaling factor range verification
   - Fatigue life threshold checks

## Troubleshooting

### Common Issues

1. **Missing Input Files**
   - Verify all paths in configuration
   - Check file naming conventions

2. **Column Name Mismatches**
   - Review column mappings in configuration
   - Check CSV headers match expected names

3. **Scaling Factor Limits**
   - Adjust validation limits if needed
   - Review reference selection logic

### Debug Mode

Enable verbose logging:
```bash
python cli_load_scaling.py config.yml --verbose --log-file debug.log
```

## Extension Points

The program is designed for extensibility:

1. **Custom Scaling Methods**: Override `calculate_scaling_factors()`
2. **Alternative Selection Logic**: Modify `select_reference_conditions()`
3. **Additional Output Formats**: Extend `save_outputs()`
4. **Custom Validation Rules**: Add to `validate_inputs()`

## Dependencies

- Python 3.8+
- pandas
- numpy
- pyyaml
- pathlib
- logging

## License and Support

This program is part of the Digital Model fatigue analysis suite.
For support, consult the project documentation or development team.