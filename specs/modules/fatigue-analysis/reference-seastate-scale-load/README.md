# Reference Seastate Scale Load Module

> **Note:** This directory was renamed from `reference-seastate-scaling-fatigue` to `reference-seastate-scale-load` on 2025-09-23

## Overview

This module implements load scaling methodology for fatigue analysis using reference seastate data. It processes reference tension time series and applies scaling factors based on environmental conditions.

## Directory Structure

```
reference-seastate-scale-load/
├── input/                              # Input configuration and data files
│   ├── load_scaling_config.yml        # Main configuration file
│   ├── reference_seastate_definitions_sample.csv
│   └── fatigue_seastates_sample.csv
├── reference_data/                     # Reference time series data
├── output/                            # Generated output files
│   └── scaling_factors_applied.csv   # Summary report
├── core/                              # Core analysis scripts
├── docs/                              # Documentation
└── rev_0/                             # Previous revision files

## Quick Start

### 1. Run Load Scaling Analysis

```bash
# Run with default configuration
python run_load_scaling.py

# Run with specific configuration file
python run_load_scaling.py input/load_scaling_config.yml

# Run with custom configuration
python run_load_scaling.py /path/to/custom_config.yml

# Dry run to validate configuration
python run_load_scaling.py --dry-run

# Run with verbose output
python run_load_scaling.py input/load_scaling_config.yml --verbose

# Override output directory
python run_load_scaling.py -o custom_output/
```

### 2. Verify Results
```bash
python verify_load_scaling.py
```

### 3. View Summary
```bash
cat output/scaling_factors_applied.csv | head
```

### Command-Line Usage

The program follows repository conventions with proper argument handling:

```bash
# Show help and usage
python run_load_scaling.py --help

# Positional argument (config file) with options
python run_load_scaling.py <config.yml> [options]

Options:
  -h, --help            Show help message
  -v, --verbose         Enable verbose output
  --dry-run            Validate configuration without processing
  -o, --output-dir     Override output directory
```

## Key Files

- **load_scaling_config.yml** - Main configuration file
- **run_load_scaling.py** - Main execution script
- **verify_load_scaling.py** - Automated verification script
- **LOAD_SCALING_PROGRAM_DOCUMENTATION.md** - Complete documentation
- **LOAD_SCALING_VERIFICATION.md** - Step-by-step verification guide
- **VERIFICATION_SUMMARY.md** - Latest verification results

## Load Scaling Methodology

1. **Reference Selection**: Selects closest wind and wave references based on direction and period
2. **Scaling Calculation**:
   - Wind: `(V_fatigue / V_reference)²`
   - Wave: `Hs_fatigue / Hs_reference`
3. **Load Combination**: `Effective_tension = scaled_wind + scaled_wave`

## Output

The program generates:
- Individual scaled tension files for each combination: `{config}_FC{###}_Strut{#}_scaled_tension.csv`
- Summary report with all scaling factors: `scaling_factors_applied.csv`

## Configuration

All parameters are defined in `input/load_scaling_config.yml`:
- Input data paths
- Vessel configurations
- Scaling methodology
- Output specifications

## Verification

The module includes comprehensive verification:
- **35 automated tests** with 100% pass rate
- **320 load cases** validated
- Calculation accuracy verified to 4 decimal places

## Dependencies

- Python 3.8+
- pandas
- numpy
- pyyaml

## Support

For questions or issues, consult the documentation files or contact the development team.