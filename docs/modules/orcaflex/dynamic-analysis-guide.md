# OrcaFlex Dynamic Analysis Guide

## Overview

The Universal OrcaFlex Runner now supports full dynamic time-domain analysis in addition to static analysis. This guide covers how to use dynamic analysis capabilities for complex offshore engineering simulations.

## Analysis Types

The runner supports three analysis modes:

### 1. Static Analysis (Default)
```bash
# Runs only static equilibrium calculation
python -m digitalmodel.orcaflex.universal pattern="*.yml"
```
- Finds static equilibrium position
- Fast execution (typically 1-5 seconds per model)
- Produces smaller .sim files (3-5 MB)

### 2. Dynamic Analysis
```bash
# Runs full time-domain simulation
python -m digitalmodel.orcaflex.universal --dynamic pattern="*.yml" simulation_time=200
```
- Performs complete time-history simulation
- Includes all stages (build-up, simulation)
- Produces larger .sim files (50-70 MB)
- Execution time varies (30-60 seconds typical)

### 3. Both Static and Dynamic
```bash
# Runs static first, then dynamic
python -m digitalmodel.orcaflex.universal --both pattern="*.yml" simulation_time=150
```
- Performs static analysis first
- Then runs dynamic simulation from static position
- Most comprehensive analysis option

## Command Line Usage

### Basic Dynamic Analysis
```bash
# Run dynamic analysis on all .yml files
python -m digitalmodel.orcaflex.universal --dynamic pattern="*.yml"

# Specify custom simulation duration (seconds)
python -m digitalmodel.orcaflex.universal --dynamic simulation_time=300 pattern="*.yml"

# Process specific models with dynamic analysis
python -m digitalmodel.orcaflex.universal --dynamic models="model1.yml,model2.yml"
```

### Advanced Examples
```bash
# FSTS mooring analysis with 200-second simulation
python -m digitalmodel.orcaflex.universal \
    --dynamic \
    pattern="fsts*pb*.yml" \
    simulation_time=200 \
    input_directory="D:\1522\ctr7\orcaflex\rev_a08\base_files\fsts_lngc_pretension" \
    output_directory=".\dynamic_results"

# Batch processing with both analyses
python -m digitalmodel.orcaflex.universal \
    --both \
    pattern="fsts_l015_*_vessel_statics_6dof.yml" \
    simulation_time=150 \
    max_workers=10
```

## Python API Usage

### Basic Dynamic Analysis
```python
from digitalmodel.orcaflex.universal import UniversalOrcaFlexRunner

# Initialize runner for dynamic analysis
runner = UniversalOrcaFlexRunner(
    analysis_type='dynamic',
    verbose=True
)

# Run dynamic simulation
results = runner.run(
    pattern="*.yml",
    simulation_time=200.0,  # 200 seconds
    input_directory="./models",
    output_directory="./dynamic_sims"
)

print(f"Processed {results.total} models")
print(f"Success rate: {results.success_rate:.1f}%")
```

### Custom Configuration
```python
# Run both static and dynamic with custom settings
runner = UniversalOrcaFlexRunner(
    analysis_type='both',
    max_workers=15
)

results = runner.run(
    pattern="fsts*.yml",
    simulation_time=300.0,
    parallel=True,
    exclude_patterns=["*test*", "*backup*"]
)
```

### Override Analysis Type Per Run
```python
runner = UniversalOrcaFlexRunner()  # Defaults to static

# Run static analysis
static_results = runner.run(
    pattern="*.yml",
    analysis_type='static'
)

# Same runner, now dynamic
dynamic_results = runner.run(
    pattern="*.yml",
    analysis_type='dynamic',
    simulation_time=250.0
)
```

## Configuration File Support

Create a `dynamic_config.yml`:
```yaml
processing:
  pattern: 'fsts*.yml'
  input_directory: './models'
  output_directory: './dynamic_sims'
  analysis_type: 'dynamic'
  simulation_time: 200.0
  parallel: true
  max_workers: 20

options:
  verbose: true
  mock_mode: false
```

Run with:
```bash
python -m digitalmodel.orcaflex.universal --config dynamic_config.yml
```

## Performance Considerations

### Execution Times
| Analysis Type | Typical Time | File Size |
|--------------|--------------|-----------|
| Static Only | 1-5 seconds | 3-5 MB |
| Dynamic Only | 30-60 seconds | 50-70 MB |
| Both | 35-70 seconds | 55-75 MB |

### Resource Usage
- **Dynamic analysis** is CPU-intensive
- Recommended: Limit parallel workers for dynamic (10-15 max)
- Memory usage: ~500MB-1GB per dynamic simulation
- Disk I/O: Significant for large .sim files

### Optimization Tips
1. **Use parallel processing wisely**
   ```bash
   # Good for static
   --static max_workers=30
   
   # Better for dynamic
   --dynamic max_workers=10
   ```

2. **Batch by analysis type**
   ```bash
   # First run all static
   python -m digitalmodel.orcaflex.universal --static pattern="*.yml"
   
   # Then run dynamic on critical models
   python -m digitalmodel.orcaflex.universal --dynamic pattern="critical*.yml"
   ```

3. **Monitor system resources**
   - CPU usage will be high during dynamic analysis
   - Ensure adequate disk space for .sim files
   - Close other OrcaFlex instances to free licenses

## OrcaFlex API Details

The dynamic analysis implementation uses the official OrcaFlex Python API:

```python
import OrcFxAPI

# Load model
model = OrcFxAPI.Model('model.yml')

# Configure simulation duration
general = model['General']
general.StageDuration[0] = 200.0  # 200 seconds

# Run analyses
model.CalculateStatics()  # Static equilibrium
model.RunSimulation()     # Dynamic time-domain

# Save results
model.SaveSimulation('output.sim')
```

## Troubleshooting

### Common Issues

**1. Dynamic analysis takes too long**
- Reduce simulation_time
- Decrease max_workers
- Check model complexity

**2. Large .sim files**
- Expected for dynamic analysis
- Ensure adequate disk space
- Consider selective saving in OrcaFlex model

**3. License errors during parallel dynamic**
- Reduce max_workers
- Run sequentially with --sequential flag
- Check license server status

**4. Memory errors**
- Reduce parallel workers
- Process in smaller batches
- Close other applications

### Verification

Check if dynamic analysis ran correctly:
```python
# Verify .sim file size (should be >40MB for dynamic)
import os
sim_size = os.path.getsize('model.sim') / (1024*1024)
print(f"Sim file size: {sim_size:.1f} MB")

# Dynamic typically produces 50-70MB files
# Static typically produces 3-5MB files
```

## Best Practices

1. **Start with static analysis** to verify model stability
2. **Use appropriate simulation times** based on physics:
   - Wave periods: 100-200 seconds minimum
   - Mooring dynamics: 200-500 seconds
   - Long-term drift: 1000+ seconds

3. **Monitor convergence** in OrcaFlex post-processor
4. **Validate results** against known cases
5. **Document analysis parameters** for reproducibility

## Examples with Real FSTS Models

### Process LWL and HWL Conditions
```bash
# Low water line conditions with dynamic analysis
python -m digitalmodel.orcaflex.universal \
    --dynamic \
    pattern="*lwl*.yml" \
    simulation_time=200 \
    input_directory="D:\1522\ctr7\orcaflex\rev_a08\base_files\fsts_lngc_pretension"

# High water line conditions
python -m digitalmodel.orcaflex.universal \
    --dynamic \
    pattern="*hwl*.yml" \
    simulation_time=200 \
    input_directory="D:\1522\ctr7\orcaflex\rev_a08\base_files\fsts_lngc_pretension"
```

### Batch Processing Different Distances
```bash
# Process all 125km3 models
python -m digitalmodel.orcaflex.universal \
    --both \
    pattern="*125km3*.yml" \
    simulation_time=150

# Process all 180km3 models  
python -m digitalmodel.orcaflex.universal \
    --both \
    pattern="*180km3*.yml" \
    simulation_time=150
```

## Integration with Existing Workflows

The dynamic analysis capability integrates seamlessly with existing batch processing:

```python
# Your existing batch configuration
from digitalmodel.orcaflex.mooring_tension_iteration import batch_runner

# Simply add analysis_type and simulation_time
config = {
    'models': ['model1.yml', 'model2.yml'],
    'analysis_type': 'dynamic',
    'simulation_time': 200.0,
    # ... other config
}

batch_runner.run(config)
```

## Next Steps

- Review generated .sim files in OrcaFlex post-processor
- Extract time-series data for further analysis
- Compare static vs dynamic results
- Optimize simulation parameters for your specific use case

## References

- [OrcaFlex Python API Documentation](https://www.orcina.com/webhelp/OrcFxAPI/Content/html/Pythoninterface,Runningmodels.htm)
- [Universal Runner Main Documentation](../universal/README.md)
- [Batch Processing Guide](../mooring_tension_iteration/batch_processing/README.md)