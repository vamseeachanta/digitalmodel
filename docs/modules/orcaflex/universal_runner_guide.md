# OrcaFlex Universal Runner Module Guide

## Overview
The `digitalmodel.modules.orcaflex.universal` module provides a command-line interface for batch processing OrcaFlex model files (.yml, .dat, .sim) to generate simulation files.

## Installation
Ensure digitalmodel is installed with OrcaFlex support:
```bash
pip install -e /path/to/digitalmodel
```

## Basic Usage

### Running OrcaFlex Model Files to Generate .sim Files

The module can process OrcaFlex YAML model files and generate corresponding .sim files:

```bash
python -m digitalmodel.modules.orcaflex.universal pattern="*.yml" input_directory="." output_directory="."
```

**Note:** If no pattern is specified, the module defaults to `"*.yml"` (all YAML files), NOT all files:
```bash
# This will process all .yml files (default pattern)
python -m digitalmodel.modules.orcaflex.universal input_directory="." output_directory="."
```

### Common Usage Patterns

#### 1. Process specific model files by pattern
```bash
# Process all yml files matching a pattern
python -m digitalmodel.modules.orcaflex.universal pattern="fsts*180km3*pb*.yml" input_directory="." output_directory="."

# Process all yml files in current directory
python -m digitalmodel.modules.orcaflex.universal pattern="*.yml" input_directory="." output_directory="."
```

#### 2. Process with different file types
```bash
# Process .dat files
python -m digitalmodel.modules.orcaflex.universal pattern="*.dat" input_directory="." output_directory="."

# Process existing .sim files
python -m digitalmodel.modules.orcaflex.universal pattern="*.sim" input_directory="." output_directory="."
```

#### 3. Mock mode (no OrcaFlex license required)
```bash
python -m digitalmodel.modules.orcaflex.universal --mock pattern="*.yml" input_directory="." output_directory="."
```

#### 4. Verbose output for debugging
```bash
python -m digitalmodel.modules.orcaflex.universal --verbose pattern="*.yml" input_directory="." output_directory="."
```

## Command-Line Options

```
Options:
  -h, --help       Show help message and exit
  --all            Process all matching files
  --test           Test mode (first 3 files)
  --mock           Mock mode (no license needed)
  --verbose, -v    Verbose output
  --report REPORT  Save JSON report to file

Keyword Arguments:
  pattern="*.yml"              # File pattern to match (glob or regex)
  input_directory="./models"   # Input directory containing models
  output_directory="./sim"     # Output directory for .sim files
  recursive=true               # Search subdirectories recursively
  parallel=true                # Enable parallel processing
  max_workers=20              # Number of concurrent workers
  skip_validation=true        # Skip model validation
  force_process=true          # Force processing even if output exists
```

## Examples

### Example 1: Process FSTS Models
```bash
cd /path/to/orcaflex/models
python -m digitalmodel.modules.orcaflex.universal pattern="fsts*.yml" input_directory="." output_directory="."
```

### Example 2: Process with Parallel Workers
```bash
# Default: 30 workers
python -m digitalmodel.modules.orcaflex.universal pattern="*.yml" parallel=true

# Increase to 40 workers
python -m digitalmodel.modules.orcaflex.universal pattern="*.yml" max_workers=40 parallel=true

# Increase to 60 workers for large batches
python -m digitalmodel.modules.orcaflex.universal pattern="*.yml" max_workers=60
```

### Example 3: Recursive Directory Search
```bash
python -m digitalmodel.modules.orcaflex.universal pattern="*.yml" recursive=true input_directory="/path/to/models"
```

### Example 4: Generate Report
```bash
python -m digitalmodel.modules.orcaflex.universal pattern="*.yml" --report processing_report.json
```

## Supported Model Types

The module supports OrcaFlex model files with the following characteristics:

### YAML Files (.yml, .yaml)
Files must contain at least one of these OrcaFlex-specific keys:
- `General`
- `Environment`
- `BaseFile`
- `UnitsSystem`
- `Vessels`
- `Lines`
- `Winches`
- `includefile` (for files using include directives)

### DAT Files (.dat)
Binary OrcaFlex data files are automatically recognized.

### SIM Files (.sim)
OrcaFlex simulation files containing both model and results.

## Processing Workflow

1. **Discovery**: Finds all files matching the specified pattern
2. **Validation**: Checks if files are valid OrcaFlex models (can be skipped with `skip_validation=true`)
3. **Processing**: Loads each model, runs static analysis, and saves as .sim
4. **Reporting**: Generates summary of successful and failed processes

## Output

The module generates:
- `.sim` files in the specified output directory
- Console output showing progress and results
- Optional JSON report with detailed processing information

## Troubleshooting

### Models Not Found
If models are not being discovered:
- Check the file pattern syntax
- Verify files contain OrcaFlex-specific content
- Use `--verbose` flag for detailed debugging
- Try `skip_validation=true` to bypass validation

### Encoding Errors
Some systems may show encoding warnings in the output. These can typically be ignored if .sim files are generated successfully.

### License Issues
- Ensure OrcaFlex is installed and licensed
- Use `--mock` mode for testing without a license
- Check available licenses with OrcaFlex License Manager

## Performance Tips

1. **Parallel Processing**: Use `max_workers` parameter to control parallelism
2. **Batch Size**: Process files in batches to manage memory usage
3. **Output Directory**: Use local drives for better I/O performance
4. **File Patterns**: Be specific with patterns to avoid processing unnecessary files

## Integration with CI/CD

The module can be integrated into automated workflows:

```bash
# In a batch script or CI pipeline
python -m digitalmodel.modules.orcaflex.universal \
    pattern="release_*.yml" \
    input_directory="./models" \
    output_directory="./simulations" \
    --report "build_${BUILD_NUMBER}.json"
```

## Notes

- The module automatically handles OrcaFlex license acquisition and release
- Progress is shown in real-time during batch processing
- Failed models are reported but don't stop the batch process
- The module is thread-safe for parallel processing