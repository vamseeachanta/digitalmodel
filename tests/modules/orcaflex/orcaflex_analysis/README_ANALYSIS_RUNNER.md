# OrcaFlex Analysis Runner Documentation

## Overview

This directory contains standardized YAML configuration files for running OrcaFlex analyses on `.sim`, `.yml`, and `.dat` files. All configurations follow the mandatory architecture defined in `.agent-os/standards/input-file-architecture.md`.

## Configuration Files

### 1. `run_orcaflex_analysis.yml`
**Purpose**: Comprehensive configuration with all available options  
**Use Case**: Production runs with full analysis and post-processing  
**Features**:
- Complete pre-processing validation
- Static and dynamic analysis options
- Extensive post-processing (visualization, statistics, fatigue)
- Batch processing capabilities
- Error handling and retry logic

### 2. `run_orcaflex_simple.yml`
**Purpose**: Simplified configuration for quick tests  
**Use Case**: Development testing and debugging  
**Features**:
- Minimal configuration
- Quick static analysis
- Basic summary statistics
- Fast execution

### 3. `run_orcaflex_batch.yml`
**Purpose**: Batch processing of multiple files  
**Use Case**: Processing entire directories of OrcaFlex files  
**Features**:
- Pattern-based file selection
- Sequential or parallel processing
- Results aggregation
- Performance monitoring
- Continuation on errors

## Usage

### Python Interface

```python
from digitalmodel.engine import engine

# Run analysis using configuration
engine.process_file("run_orcaflex_simple.yml")
```

### Command Line

```bash
# Simple analysis
python -m digitalmodel.engine run_orcaflex_simple.yml

# Comprehensive analysis
python -m digitalmodel.engine run_orcaflex_analysis.yml

# Batch processing
python -m digitalmodel.engine run_orcaflex_batch.yml
```

### Test Script

```bash
# Run the test script to validate configurations
python test_run_analysis.py
```

## File Selection Options

### Option 1: Specific Files
```yaml
input_files:
  yml:
    - orcaflex_test1.yml
  sim:
    - orcaflex_test1.sim
  dat:
    - orcaflex_test2.dat
```

### Option 2: Pattern-Based
```yaml
filename:
  extension: [yml, sim, dat]
  pattern: "orcaflex_test*"
  filters:
    contains: [test]
    not_contains: [backup, temp]
```

## Analysis Types

### Static Analysis
- Convergence tolerance: 1e-6
- Maximum iterations: 200
- Use calculated positions from previous runs
- Automatic line length calculation

### Dynamic Analysis
- Configurable simulation duration
- Time step control
- Ramp time settings
- Implicit integration options
- Restart file generation

## Post-Processing Options

### Available Outputs
1. **Summary Statistics**
   - Minimum, Maximum, Mean, Standard Deviation
   - Percentiles (5th, 95th)
   - Variable-specific summaries

2. **Visualizations**
   - Time series plots
   - Range graphs along line length
   - 3D model views
   - Histograms

3. **Data Extraction**
   - Time series at specific points
   - Range data along lines
   - Response statistics
   - Fatigue analysis results

## Configuration Structure

All files follow the mandatory structure:

```yaml
meta:
  library: digitalmodel
  basename: orcaflex_analysis
  version: "1.0.0"
  description: "Purpose of configuration"

default:
  log_level: INFO
  units:
    # Unit definitions
    
orcaflex:
  analysis:
    # Analysis settings
  postprocess:
    # Post-processing settings
    
file_management:
  input_directory: ./
  output_directory: ./results
  # File selection
```

## Input File Formats

### YAML Files (`.yml`)
- Human-readable configuration format
- Converted to DAT format before analysis
- Supports all OrcaFlex model features

### Simulation Files (`.sim`)
- Binary OrcaFlex simulation files
- Contains complete model and results
- Can continue from previous analyses

### Data Files (`.dat`)
- OrcaFlex native text format
- Direct model specification
- Version-compatible with OrcaFlex installation

## Output Structure

```
results/
├── analysis_run_[timestamp]/
│   ├── simulations/       # .sim output files
│   ├── data/             # .dat output files
│   ├── summaries/        # CSV/Excel summaries
│   ├── plots/            # Visualization outputs
│   └── logs/             # Analysis logs
```

## Best Practices

1. **Start Simple**: Use `run_orcaflex_simple.yml` for initial tests
2. **Validate First**: Check configurations with `test_run_analysis.py`
3. **Incremental Complexity**: Add features gradually
4. **Monitor Resources**: Use batch mode for large sets
5. **Version Control**: Track configuration changes

## Troubleshooting

### Common Issues

1. **File Not Found**
   - Check `input_directory` path
   - Verify file extensions match configuration
   - Check filter settings

2. **License Errors**
   - Ensure OrcaFlex license is available
   - Check `OrcFxAPI` installation
   - Verify Python environment

3. **Memory Issues**
   - Reduce `simulation_duration` for tests
   - Enable `cleanup_after_each_run` in batch mode
   - Process files sequentially instead of parallel

4. **Convergence Problems**
   - Increase `max_iterations`
   - Adjust `damping_factor`
   - Check model initial conditions

## Examples

### Example 1: Run Static Analysis Only
```yaml
orcaflex:
  analysis:
    static: True
    dynamic: False
```

### Example 2: Extract Specific Results
```yaml
results_extraction:
  time_series:
    objects:
      - name: Line1
        variables: [effective_tension]
    arc_length_positions: [0, 50, 100]
```

### Example 3: Batch Process with Filters
```yaml
filename:
  extension: [sim]
  pattern: "mooring_*"
  filters:
    not_contains: [old, backup]
```

## Requirements

- Python 3.8+
- digitalmodel package
- OrcFxAPI (for .sim and .dat files)
- Valid OrcaFlex license
- PyYAML for configuration parsing

## Support

For issues or questions:
1. Check this documentation
2. Review test script output
3. Validate against `.agent-os/standards/input-file-architecture.md`
4. Check OrcaFlex documentation

---
*Generated: 2025-08-15*  
*Standard: `.agent-os/standards/input-file-architecture.md`*