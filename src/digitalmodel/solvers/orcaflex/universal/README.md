# Universal OrcaFlex Simulation Runner

A comprehensive, library-based OrcaFlex simulation runner that can be executed from any directory on any computer with flexible keyword arguments for pattern matching, batch processing, and directory management.

## 🚀 Quick Start

### Installation

The Universal OrcaFlex Runner is included with the digitalmodel package. No additional installation required.

### Basic Usage - Module Interface

```bash
# Process OrcaFlex .yml files to generate .sim files
python -m digitalmodel.solvers.orcaflex.universal pattern="*.yml" input_directory="." output_directory="."

# If no pattern is specified, defaults to "*.yml" (all YAML files)
python -m digitalmodel.solvers.orcaflex.universal input_directory="." output_directory="."

# Specific example - FSTS 180km3 port/bow models
python -m digitalmodel.solvers.orcaflex.universal pattern="fsts*180km3*pb*.yml" input_directory="." output_directory="."

# Process .dat files instead of .yml
python -m digitalmodel.solvers.orcaflex.universal pattern="*.dat" input_directory="." output_directory="."

# Increase parallel workers to 40 (default is 30)
python -m digitalmodel.solvers.orcaflex.universal pattern="*.yml" max_workers=40

# Mock mode (no license required)
python -m digitalmodel.solvers.orcaflex.universal --mock pattern="*.yml"

# Dynamic analysis only (runs full time-domain simulation)
python -m digitalmodel.solvers.orcaflex.universal --dynamic pattern="*.yml" simulation_time=200

# Both static and dynamic analysis
python -m digitalmodel.solvers.orcaflex.universal --both pattern="*.yml" simulation_time=150

# Dynamic analysis with custom simulation time
python -m digitalmodel.solvers.orcaflex.universal analysis_type="dynamic" simulation_time=300.0 pattern="*.yml"

# Note: Dynamic analysis runs the full OrcaFlex simulation using model.RunSimulation()
# This includes all stages defined in the model (build-up, simulation, etc.)
```

### What This Does
The command will:
1. Find all .yml files matching your pattern
2. Load each OrcaFlex model file
3. Run analysis (static, dynamic, or both)
4. Save results as .sim files

### Example Files Processed
- `fsts_l015_hwl_180km3_l100_pb_vessel_statics_6dof.yml` → `.sim` (43MB)
- `fsts_l015_lwl_180km3_l100_pb_vessel_statics_6dof.yml` → `.sim` (43MB)
- `fsts_l095_hwl_180km3_l000_pb_vessel_statics_6dof.yml` → `.sim` (44MB)

## 📋 Features

- **Universal Access**: Run from any directory without path dependencies
- **Flexible Processing**: Single file, batch, pattern-based, and directory-based processing
- **Analysis Types**: Static analysis, dynamic simulation, or both
- **Live Status Reporting**: Real-time progress in terminal/bash window title
- **Parallel Processing**: Adaptive worker scaling based on system resources
- **Mock Mode**: Test without OrcaFlex license
- **Cross-Platform**: Windows, Linux, and macOS support

## 🎯 Usage Examples

### Command Line Interface

#### Process All Models
```bash
/orcaflex-universal --all --output ./sim
```

#### Pattern Matching
```bash
/orcaflex-universal --pattern "fsts_*.yml" --input-dir ./models --output-dir ./sims
```

#### Specific Files
```bash
/orcaflex-universal --models model1.yml model2.dat model3.yml
```

#### Recursive Search with Exclusions
```bash
/orcaflex-universal --recursive --exclude "*backup*" --exclude "*test*" --all
```

#### Test Mode (First 3 Files)
```bash
/orcaflex-universal --mock --test
```

### Python Library API

```python
from digitalmodel.solvers.orcaflex.universal import UniversalOrcaFlexRunner

# Initialize runner
runner = UniversalOrcaFlexRunner(
    mock_mode=False,
    max_workers=20,
    verbose=True
)

# Run with various options
results = runner.run(
    pattern="fsts_*.yml",
    input_directory="./models",
    output_directory="./sim",
    recursive=True,
    parallel=True,
    exclude_patterns=["*backup*", "*test*"]
)

# Check results
print(f"Total: {results.total}")
print(f"Successful: {results.successful}")
print(f"Failed: {results.failed}")
print(f"Success rate: {results.success_rate:.1f}%")
```

### Configuration File

Create a `batch_config.yml`:

```yaml
processing:
  pattern: 'fsts_*.yml'
  input_directory: './models'
  output_directory: './sim'
  recursive: true
  parallel: true
  max_workers: 20
  exclude_patterns:
    - '*backup*'
    - '*test*'
    - '*includefile*'

options:
  mock_mode: false
  verbose: true
  save_report: true
  report_path: './simulation_report.json'
```

Then run:
```bash
/orcaflex-universal --config batch_config.yml
```

## 📊 Live Status Reporting

The runner provides real-time status updates in your terminal:

### Terminal Title
```
OrcaFlex: [45/100] 45% | ✓40 ✗5 | 2.3/s | current_model.yml
```

### Progress Bar
```
[████████████████████░░░░░░░░░░░░░░░░░░░] 45.0% | OK:40 FAIL:5 | ETA: 23s
```

### Summary Report
```
================================================================================
ORCAFLEX PROCESSING SUMMARY
================================================================================
Total Models Processed: 100
Successful: 95 ✓
Failed: 5 ✗
Success Rate: 95.0%
Total Time: 2m 15s
Average Time per Model: 1.35s
Processing Rate: 0.74 models/second

Simulation Files Created: 95
  - model1.sim
  - model2.sim
  - model3.sim
  ... and 92 more

Failed Models:
  ✗ bad_model.yml: License error: No available tokens...
  ✗ corrupt.dat: File format error...
================================================================================
```

## 🔧 Advanced Options

### Command Line Options

| Option | Short | Description |
|--------|-------|-------------|
| `--pattern` | `-p` | File pattern to match (e.g., "*.yml", "fsts_*.dat") |
| `--input-dir` | `-i` | Input directory containing models |
| `--output-dir` | `-o` | Output directory for .sim files |
| `--models` | `-m` | Specific model files (can be used multiple times) |
| `--config` | `-c` | Configuration file (YAML) |
| `--recursive` | `-r` | Search directories recursively |
| `--parallel` | | Enable parallel processing (default) |
| `--sequential` | | Disable parallel processing |
| `--workers` | `-w` | Maximum parallel workers (default: 30) |
| `--mock` | | Run in mock mode (no license) |
| `--exclude` | `-e` | Patterns to exclude (can be used multiple times) |
| `--verbose` | `-v` | Enable verbose output |
| `--report` | | Save JSON report to file |
| `--all` | | Process all matching files |
| `--test` | | Test mode (first 3 files only) |

### Python API Keywords

```python
results = runner.run(
    # File discovery
    pattern="*.yml",              # File pattern to match
    input_directory="./models",   # Source directory
    output_directory="./sims",    # Output directory
    models=["file1.yml"],         # Specific files
    recursive=True,               # Recursive search
    exclude_patterns=["*test*"],  # Exclusion patterns
    
    # Processing options
    parallel=True,                # Enable parallel processing
    max_workers=20,               # Concurrent workers
    chunk_size=10,                # Batch chunk size
    
    # Configuration
    config_file="config.yml",     # Configuration file
    mock_mode=False,              # Mock mode
    verbose=True,                 # Verbose logging
    
    # Reporting
    status_reporter=reporter,     # Status reporter instance
)
```

## 🧪 Testing

### Run Tests
```bash
# Run all tests
python tests/modules/orcaflex/universal/test_universal_runner.py

# Run with real .dat files
python tests/modules/orcaflex/universal/test_real_orcaflex.py
```

### Mock Mode Testing
```python
# Test without OrcaFlex license
runner = UniversalOrcaFlexRunner(mock_mode=True)
results = runner.run(pattern="*.yml", input_directory="./test_models")
```

## 📁 File Support

The runner supports the following OrcaFlex file types:
- `.yml` / `.yaml` - YAML model files (including those with `includefile` directives)
- `.dat` - OrcaFlex data files  
- `.sim` - Simulation files (for validation)

### Support for Include Files
The module recognizes OrcaFlex YAML files that use `includefile` directives. Files with the following structure are fully supported:

```yaml
BaseFile: ../base_model.yml
includefile: includefile_general_settings.yml
includefile: includefile_vessel_statics.yml
```

The validation checks for these OrcaFlex-specific keys:
- `General`, `Environment`, `BaseFile`
- `UnitsSystem`, `Vessels`, `Lines`, `Winches`
- `includefile` (for modular model files)

## 🔍 Pattern Matching

### Glob Patterns
- `*.yml` - All .yml files
- `fsts_*.dat` - Files starting with "fsts_"
- `**/model*.yml` - Recursive search for model files

### Regex Patterns
- `^fsts_.*\.yml$` - Regex for FSTS models
- `model_\d{3}\.dat` - Models with 3-digit numbers

### Exclusion Patterns
Default exclusions:
- `*includefile*`
- `*backup*`
- `*_old*`
- `*_temp*`
- `*.tmp`
- `*.bak`

## 🚨 Error Handling

The runner provides comprehensive error handling:

- **License Errors**: Automatic retry with backoff
- **File Errors**: Skip and continue with next file
- **System Errors**: Graceful shutdown with status preservation
- **Memory Issues**: Automatic worker reduction

## 📈 Performance

### Resource Optimization
- Automatic worker scaling based on:
  - CPU count (2x CPUs max)
  - Available memory (2GB per worker)
  - Current system load
  - License availability

### Benchmarks
- 100 models: ~2-3 minutes
- 1000 models: ~20-30 minutes
- Processing rate: 0.5-2 models/second (depending on complexity)

## 🌍 Environment Variables

```bash
# Set repository root
export DIGITALMODEL_ROOT=/path/to/digitalmodel

# Override default workers
export ORCAFLEX_MAX_WORKERS=20

# Set license timeout
export ORCAFLEX_LICENSE_TIMEOUT=300
```

## 📝 Output Files

### Simulation Files
- Location: Specified output directory
- Format: `{model_name}.sim`
- Contains: Complete simulation state after static analysis

### Report Files
JSON report includes:
- Execution summary
- Performance metrics
- Success/failure details
- File lists
- Error messages

## 🔗 Integration

### With CI/CD
```yaml
# GitHub Actions example
- name: Run OrcaFlex Simulations
  run: |
    /orcaflex-universal --pattern "*.yml" --all --report results.json
```

### With Python Scripts
```python
import subprocess
result = subprocess.run([
    "python", "-m", "digitalmodel.solvers.orcaflex.universal_cli",
    "--pattern", "*.yml",
    "--all"
])
```

## ⚠️ Troubleshooting

### Common Issues

**No models found**
- Check pattern syntax
- Verify input directory exists
- Check exclusion patterns

**License errors**
- Use `--mock` for testing
- Check license server connection
- Reduce worker count

**Memory issues**
- Reduce `--workers` count
- Process in smaller batches
- Use `--sequential` mode

## 📚 Related Documentation

- [OrcaFlex Module Documentation](../README.md)
- [Batch Processing Guide](../batch_processing/README.md)
- [Universal Sim Runner Specification](../../../../specs/modules/orcaflex/universal-sim-runner/spec.md)

## 📄 License

Part of the digitalmodel project. See main repository for license details.

## 🤝 Contributing

Contributions welcome! Please follow the digitalmodel contribution guidelines.

## 📞 Support

For issues or questions:
- Create an issue in the digitalmodel repository
- Contact the engineering team
- Check the troubleshooting guide above