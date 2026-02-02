# Load Scaling Program - Usage Guide

## Overview
The load scaling program now follows repository best practices with proper command-line argument handling.

## Basic Usage

### Standard Execution
```bash
# Navigate to the program directory
cd specs/modules/fatigue-analysis/reference-seastate-scale-load

# Run with explicit configuration file (RECOMMENDED)
python run_load_scaling.py input/load_scaling_config.yml

# Run with default configuration
python run_load_scaling.py
```

## Command-Line Arguments

### Positional Arguments
- `config_file` - Path to YAML configuration file (default: `input/load_scaling_config.yml`)

### Optional Arguments
- `-h, --help` - Show help message and exit
- `-v, --verbose` - Enable verbose output for debugging
- `--dry-run` - Validate configuration without processing data
- `-o, --output-dir DIR` - Override output directory from configuration

## Usage Examples

### 1. Show Help
```bash
python run_load_scaling.py --help
```

### 2. Run with Default Configuration
```bash
python run_load_scaling.py
```

### 3. Run with Specific Configuration
```bash
python run_load_scaling.py input/load_scaling_config.yml
```

### 4. Run with Custom Configuration File
```bash
python run_load_scaling.py /path/to/custom_config.yml
```

### 5. Validate Configuration (Dry Run)
```bash
python run_load_scaling.py --dry-run
# or with specific config
python run_load_scaling.py input/load_scaling_config.yml --dry-run
```

### 6. Verbose Output for Debugging
```bash
python run_load_scaling.py input/load_scaling_config.yml --verbose
```

### 7. Override Output Directory
```bash
python run_load_scaling.py input/load_scaling_config.yml -o custom_output/
# or
python run_load_scaling.py --output-dir results/2025-09-23/
```

### 8. Combine Multiple Options
```bash
python run_load_scaling.py custom_config.yml --verbose -o test_output/
```

## Alternative Execution Methods

### As Python Module
```bash
# From repository root
python -m digitalmodel.fatigue_analysis.load_scaling specs/modules/fatigue-analysis/reference-seastate-scale-load/input/load_scaling_config.yml
```

### Direct Module Execution
```bash
# From src directory
cd src
python digitalmodel/modules/fatigue_analysis/load_scaling.py ../specs/modules/fatigue-analysis/reference-seastate-scale-load/input/load_scaling_config.yml
```

## Configuration File Format

The configuration file must be a valid YAML file with the following structure:

```yaml
input_data:
  reference_seastate:
    metadata_file: "path/to/reference_metadata.csv"
    data_folder: "path/to/reference_data"
  fatigue_seastates:
    metadata_file: "path/to/fatigue_seastates.csv"
  vessel_configurations:
    configs: [...]
    struts: [1, 2, 3, 4, 5, 6, 7, 8]

load_scaling:
  scaling_parameters:
    wind: ...
    wave: ...

output:
  base_folder: "output"
  file_naming:
    pattern: "{config}_FC{fc_number:03d}_Strut{strut_number}_{type}.csv"
```

## Output Structure

The program generates:
- **Individual files**: One CSV file per configuration/condition/strut combination
- **Summary report**: `scaling_factors_applied.csv` with all scaling factors

### Output Directory Contents
```
output/
├── fsts_l015_FC001_Strut1_scaled_tension.csv
├── fsts_l015_FC001_Strut2_scaled_tension.csv
├── ...
└── scaling_factors_applied.csv
```

## Verification

After running the analysis, verify results:

```bash
# Run verification script
python verify_load_scaling.py

# Check output files
ls output/*.csv | wc -l  # Should show 321 files

# View summary
head output/scaling_factors_applied.csv
```

## Error Handling

### Missing Configuration File
```bash
$ python run_load_scaling.py missing_config.yml
Error: Configuration file not found: missing_config.yml

Available configuration files:
  - input/load_scaling_config.yml
```

### Invalid YAML
The program will report YAML syntax errors with line numbers.

### Missing Input Files
Use `--dry-run` to validate all input files exist before processing:
```bash
python run_load_scaling.py --dry-run
```

## Best Practices

1. **Always specify the configuration file explicitly** for clarity and reproducibility
2. **Use --dry-run first** to validate configuration before long processing runs
3. **Use --verbose** when debugging issues
4. **Override output directory** for different test runs to avoid overwriting results
5. **Keep configuration files in version control** for reproducibility

## Repository Compliance

This implementation follows repository standards:
- ✅ Configuration file as command-line argument
- ✅ Standard argparse implementation
- ✅ Help documentation via --help
- ✅ Optional arguments for common overrides
- ✅ Proper error messages and exit codes
- ✅ Follows Python module conventions

## Support

For issues or questions:
1. Check the help: `python run_load_scaling.py --help`
2. Review the configuration file structure
3. Run with `--verbose` for detailed logging
4. Use `--dry-run` to validate setup