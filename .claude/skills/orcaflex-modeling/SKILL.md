---
name: orcaflex-modeling
description: Setup, configure, and run OrcaFlex hydrodynamic simulations using the universal runner. Use for marine/offshore analysis including static analysis, dynamic simulations, mooring analysis, and batch processing of OrcaFlex models.
---

# OrcaFlex Modeling Skill

Create, configure, and execute OrcaFlex hydrodynamic simulations for offshore engineering analysis.

## When to Use

- Setting up OrcaFlex analysis projects with standardized folder structure
- Running static or dynamic hydrodynamic simulations
- Batch processing multiple OrcaFlex models (.yml, .dat files)
- Configuring mooring analysis or iterative simulations
- Using the universal runner for pattern-based batch execution
- Preprocessing vessel data and checking model configurations

## Prerequisites

- OrcaFlex license (checked via `OrcFxAPI`)
- Python environment with `digitalmodel` package installed
- YAML configuration files for analysis parameters

## Standardized Folder Structure

All OrcaFlex analyses MUST use this directory structure:

```
<analysis_directory>/
├── .dat/                    # OrcaFlex data files
│   ├── original/           # Original unmodified files
│   ├── modified/           # Modified during iteration
│   └── archive/            # Timestamped archives
├── .sim/                    # OrcaFlex simulation files
├── configs/                 # Configuration files
│   └── analysis_config.yml
├── results/                 # Analysis outputs
│   ├── summary/
│   ├── time_series/
│   └── reports/
├── logs/                    # Execution logs
└── scripts/                 # Helper scripts
```

## Configuration Pattern

### Basic Analysis Configuration

```yaml
# configs/analysis_config.yml

basename: project_name

default:
  analysis:
    file_type: yml
    file_path_pattern: null  # Optional glob pattern

orcaflex:
  # Preprocessing options
  preprocess:
    VesselType_Loading:
      flag: false
    check_yml_file:
      flag: false

  # Analysis options
  analysis:
    static: false           # Run static analysis only
    simulation: true        # Run full dynamic simulation
    iterate:
      flag: false           # Iterative analysis
    mooring:
      flag: false           # Mooring-specific analysis

  # Output options
  postprocess:
    summary:
      flag: true
```

### Universal Runner Configuration

```yaml
# For batch processing with universal runner

default:
  analysis:
    file_type: yml
    file_path_pattern: "*.yml"  # Process all YAML files

orcaflex:
  universal_runner:
    flag: true
    input_directory: "."
    output_directory: "results/"
    parallel: true
    max_workers: 4
    mock_mode: false         # Dry run without OrcaFlex
```

## Universal Runner Usage

### Command Line Interface

```bash
# Process all YAML files in current directory
python -m digitalmodel.modules.orcaflex.universal pattern="*.yml"

# Specify input/output directories
python -m digitalmodel.modules.orcaflex.universal \
    pattern="mooring_*.yml" \
    input_directory="configs/" \
    output_directory="results/"

# Mock mode (dry run without OrcaFlex license)
python -m digitalmodel.modules.orcaflex.universal \
    pattern="*.yml" \
    mock_mode=true

# Parallel processing with limited workers
python -m digitalmodel.modules.orcaflex.universal \
    pattern="*.yml" \
    parallel=true \
    max_workers=8
```

### Python API

```python
from digitalmodel.modules.orcaflex.universal import UniversalOrcaFlexRunner

# Initialize runner
runner = UniversalOrcaFlexRunner(
    input_directory="configs/",
    output_directory="results/",
    mock_mode=False
)

# Run single model
result = runner.run_single("model.yml")

# Run batch with pattern
results = runner.run_batch(
    pattern="*.yml",
    parallel=True,
    max_workers=4
)

# Check results
for file_name, status in results.items():
    print(f"{file_name}: {status['success']}")
```

## Model Runner for .sim Generation

### Batch Processing

```python
from digitalmodel.modules.orcaflex.run_to_sim import OrcaFlexModelRunner

# Initialize runner
runner = OrcaFlexModelRunner()

# Run single model → .sim file
runner.run_single_model(
    model_file="model.yml",
    output_dir="results/.sim/"
)

# Batch processing with parallel execution
model_files = ["model1.yml", "model2.yml", "model3.yml"]
runner.run_batch(
    model_list=model_files,
    output_dir="results/.sim/",
    max_workers=30
)
```

## Analysis Types

### Static Analysis

Quick structural equilibrium check without time-domain simulation.

```yaml
orcaflex:
  analysis:
    static: true
    simulation: false
```

### Dynamic Simulation

Full time-domain hydrodynamic simulation.

```yaml
orcaflex:
  analysis:
    static: false
    simulation: true
```

### Iterative Analysis

Run multiple iterations with parameter variations.

```yaml
orcaflex:
  analysis:
    iterate:
      flag: true
      parameters:
        - name: wave_height
          values: [2.0, 4.0, 6.0, 8.0]
        - name: wave_period
          values: [8, 10, 12]
```

### Mooring Analysis

Specialized mooring system analysis.

```yaml
orcaflex:
  analysis:
    mooring:
      flag: true
      line_types: ["chain", "wire", "polyester"]
      pretension_range: [100, 200, 300]  # kN
```

## Preprocessing Options

### Vessel Loading

Load vessel data from external sources.

```yaml
orcaflex:
  preprocess:
    VesselType_Loading:
      flag: true
      source: "vessel_database.csv"
      vessel_name: "FPSO_Alpha"
```

### YAML Validation

Check model configuration before running.

```yaml
orcaflex:
  preprocess:
    check_yml_file:
      flag: true
      strict: true
      report_path: "logs/validation_report.txt"
```

## Utilities

### License Check

```python
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities

utils = OrcaflexUtilities()
if utils.is_orcaflex_available():
    print("OrcaFlex license available")
else:
    print("Running in mock mode")
```

### Model Loading

```python
# Load simulation with metadata
model, metadata = utils.get_model_and_metadata("simulation.sim")
print(f"Simulation period: {metadata['period']} seconds")
print(f"Time step: {metadata['timestep']} seconds")
```

### Wave Calculations

```python
# Calculate theoretical maximum wave height
Hmax = utils.get_theoretical_hmax({
    "Hs": 6.0,          # Significant wave height (m)
    "Tp": 12.0,         # Peak period (s)
    "duration": 3600,   # Storm duration (s)
    "gamma": 3.3        # JONSWAP peakedness
})
print(f"Theoretical Hmax: {Hmax:.2f} m")

# Calculate associated period
Tassociated = utils.get_tassociated(Hmax, Tp=12.0)
print(f"Associated period: {Tassociated:.2f} s")
```

## Error Handling

### Common Issues

1. **License Not Available**
   ```python
   # Use mock mode for development/testing
   runner = UniversalOrcaFlexRunner(mock_mode=True)
   ```

2. **Model Convergence Failure**
   - Check initial conditions
   - Reduce time step
   - Verify boundary conditions

3. **Memory Issues with Large Batches**
   - Limit `max_workers` parameter
   - Process in smaller batches
   - Use sequential processing for very large models

## Best Practices

### Project Setup

1. Always use the standardized folder structure
2. Keep original .dat files in `.dat/original/`
3. Use descriptive YAML configuration names
4. Version control configuration files, not .sim files

### Batch Processing

1. Start with mock mode to validate configuration
2. Use pattern matching for related analyses
3. Monitor logs for failures
4. Archive results with timestamps

### Performance

1. Use parallel processing for independent runs
2. Limit workers based on available licenses
3. Run compute-intensive jobs overnight
4. Clean up intermediate files after processing

## Related Skills

- [orcaflex-post-processing](../orcaflex-post-processing/SKILL.md) - Post-process simulation results
- [mooring-design](../mooring-design/SKILL.md) - Mooring system design
- [structural-analysis](../structural-analysis/SKILL.md) - Structural verification

## References

- OrcaFlex Documentation: [Orcina OrcaFlex](https://www.orcina.com/orcaflex/)
- OrcFxAPI Python Guide
- Universal Runner: `src/digitalmodel/modules/orcaflex/universal/README.md`
