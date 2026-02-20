# Dynamic Analysis Feature - Release Notes

## Version: Universal OrcaFlex Runner v2.0
**Release Date**: August 2025

## Summary

The Universal OrcaFlex Runner now supports full dynamic time-domain analysis capabilities, enabling comprehensive offshore engineering simulations including wave dynamics, vessel motions, and mooring line responses.

## Key Features Added

### 1. Analysis Type Selection
- **Static Analysis** (default): Equilibrium calculation only
- **Dynamic Analysis**: Full time-domain simulation  
- **Both**: Sequential static then dynamic analysis

### 2. Command Line Interface
```bash
# New flags
--static    # Run static analysis only
--dynamic   # Run dynamic simulation
--both      # Run both analyses

# With simulation time
python -m digitalmodel.orcaflex.universal --dynamic simulation_time=200
```

### 3. Simulation Time Control
- Configurable simulation duration
- Default: 100 seconds
- Applies to Build-up stage (Stage 0)

## Technical Implementation

### Core Changes

1. **universal_runner.py**
   - Added `analysis_type` parameter to `__init__` and `run` methods
   - Implemented proper OrcaFlex API usage:
     ```python
     model = OrcFxAPI.Model(str(model_file))
     general = model['General']
     general.StageDuration[0] = simulation_time
     model.RunSimulation()
     ```

2. **__main__.py**
   - Added command line flags: `--static`, `--dynamic`, `--both`
   - Enhanced argument parsing for analysis type

3. **batch_processor.py**
   - Updated to pass analysis parameters through processing pipeline
   - Fixed Unicode encoding for Windows compatibility

## Performance Metrics

### Tested with Real FSTS Models

| Model Type | Analysis | Time | File Size |
|------------|----------|------|-----------|
| fsts_l015_hwl_125km3 | Static | 2.8s | 4.7 MB |
| fsts_l015_hwl_125km3 | Dynamic | 43.3s | 66.2 MB |
| fsts_l015_hwl_125km3 | Both | 42.0s | 58.9 MB |
| fsts_l015_lwl_125km3 | Dynamic | 35.2s | 55.4 MB |

### Resource Usage
- CPU: High utilization during dynamic analysis
- Memory: 500MB-1GB per simulation
- Disk I/O: Significant for .sim file writing

## Compatibility

### System Requirements
- Windows 10/11 with OrcaFlex installed
- Python 3.9+
- OrcaFlex API (OrcFxAPI module)
- 8GB+ RAM recommended for parallel processing

### File Support
- ✅ .yml files with includefiles
- ✅ .dat files
- ✅ Complex FSTS models
- ✅ Models with vessel statics configurations

## Migration Guide

### For Existing Users

No breaking changes. Existing code continues to work with static analysis by default:

```python
# Old code - still works (uses static)
runner = UniversalOrcaFlexRunner()
results = runner.run(pattern="*.yml")

# New code - explicit dynamic
runner = UniversalOrcaFlexRunner(analysis_type='dynamic')
results = runner.run(pattern="*.yml", simulation_time=200)
```

### For Batch Processing

Update your batch configurations:
```yaml
# Add to existing config
analysis_type: 'dynamic'
simulation_time: 200.0
```

## Known Limitations

1. **Parallel Processing**: 
   - Display encoding issues in parallel mode (processing still works)
   - Recommended max 10-15 workers for dynamic analysis

2. **Simulation Parameters**:
   - Only Stage 0 duration is configurable
   - Other stages use model defaults

3. **File Sizes**:
   - Dynamic .sim files are 10-20x larger than static
   - Ensure adequate disk space

## Testing

### Test Coverage
- ✅ Static analysis with real models
- ✅ Dynamic analysis with real models  
- ✅ Both analysis modes
- ✅ Sequential processing
- ✅ Parallel processing
- ✅ Mock mode for CI/CD

### Verified Models
- FSTS LNGC pretension models
- 125km3 and 180km3 configurations
- HWL and LWL conditions
- 6DOF vessel statics

## Future Enhancements

### Planned for v2.1
- [ ] Stage-specific duration control
- [ ] Results extraction automation
- [ ] Progress callbacks for long simulations
- [ ] Memory optimization for large batches

### Under Consideration
- [ ] Distributed processing across machines
- [ ] Cloud execution support
- [ ] Real-time visualization
- [ ] Automatic convergence checking

## Support

### Documentation
- [Dynamic Analysis Guide](docs/domains/orcaflex/dynamic-analysis-guide.md)
- [Universal Runner README](src/digitalmodel/modules/orcaflex/universal/README.md)
- [API Reference](https://www.orcina.com/webhelp/OrcFxAPI/)

### Getting Help
- GitHub Issues: Report bugs or request features
- Engineering Team: Contact for production support
- OrcaFlex Support: For API-specific questions

## Acknowledgments

This feature was developed based on user requirements for comprehensive hydrodynamic analysis of FSTS mooring systems. Special thanks to the offshore engineering team for testing and validation.

## Upgrade Instructions

```bash
# Update repository
git pull origin master

# Install dependencies (if needed)
uv pip install -r requirements.txt

# Verify installation
python -m digitalmodel.orcaflex.universal --help
```

## Quick Start

```bash
# Run your first dynamic analysis
python -m digitalmodel.orcaflex.universal \
    --dynamic \
    pattern="*.yml" \
    simulation_time=200 \
    input_directory="./models" \
    output_directory="./dynamic_results"
```

---

**Note**: This release represents a significant enhancement to the Universal OrcaFlex Runner, enabling full time-domain analysis capabilities essential for offshore engineering applications.