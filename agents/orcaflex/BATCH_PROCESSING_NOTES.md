# OrcaFlex Batch Processing - Implementation Notes

## Critical Learnings from Testing (2025-08-17)

### OrcaFlex API Integration

#### License Requirements
- **OrcaFlex DLL Version Tested**: 11.5e
- **API Import**: `import OrcFxAPI` 
- **Version Check**: Use `OrcFxAPI.DLLVersion()` not `Version()`
- **License Mode**: Required for actual .sim file generation

#### File Format Compatibility

##### Successful Formats
- **.dat files**: Native OrcaFlex binary format - **100% success rate**
- **.yml files**: YAML format - requires specific structure (see below)

##### YAML Structure Requirements
```yaml
%YAML 1.1
# Type: Model
# Program: OrcaFlex 11.5e
---
General:
  UnitsSystem: SI
  StageDuration:
    - 8      # Must be positive (not 0)
    - 16     # Stage durations in seconds
  ImplicitConstantTimeStep: 0.1
  
Environment:
  WaterDepth: 1500.0
  Density: 1.025        # NOT WaterDensity
  SeabedModel: Elastic  # NOT Flat (options: Elastic, Nonlinear soil model)
  SeabedNormalStiffness: 100
```

#### Common YAML Errors to Avoid
1. ❌ `WaterDensity` → ✅ `Density`
2. ❌ `SeabedModel: Flat` → ✅ `SeabedModel: Elastic`
3. ❌ `StageDuration: [0, 10]` → ✅ `StageDuration: [8, 16]` (must be positive)
4. ❌ `LineTypeOuterDiameter` → Context not recognized in certain versions

### Batch Processing Implementation

**Location**: `src/modules/orcaflex/batch_processing/` (NOT under mooring_tension_iteration)

#### File Type Detection
- Batch runner includes `FileTypeDetector` for automatic classification
- Validates model files before processing
- Categories: OrcaFlex models, includefiles, target tensions, batch configs

#### Directory Structure
```
base_directory/
├── model_files/         # .yml, .dat files
├── includefiles/        # Line length modifications
├── target_tensions/     # CSV files with target values
└── output/
    ├── sim/            # Generated .sim files
    ├── csv/            # Results CSV files
    └── batch_report_*.txt
```

#### Batch Configuration Template
```yaml
batch_info:
  name: 'Batch Run Name'
  base_directory: './path/to/models'
  output_directory: './output'
  
simulation_settings:
  analysis_type: 'statics'
  calculate_statics: true
  save_simulation_file: true
  continue_on_error: true
  parallel_processing: false  # Set true for concurrent processing
  
output_settings:
  save_simulation: true
  sim_output_folder: 'sim'
  csv_output_folder: 'csv'
  
models:
  - model_file: 'model1.yml'
    includefile: 'lengths.yml'  # Optional
    target_tensions: 'tensions.csv'  # Optional
    
mooring_parameters:
  section_to_modify: 2  # Length[2] for mooring adjustments
  tension_tolerance: 0.01
  lines_to_check: ['Line01', 'Line02']
```

### .sim File Generation Verification

#### File Size Patterns (OrcaFlex 11.5e)
- **Typical .sim file size**: 3.4-3.9 MB for moderate complexity models
- **Consistency**: Same model generates identical file sizes
- **Version differences**: ~6-7% size variation between OrcaFlex versions

#### Verified File Sizes from Testing
```
Original (May 2025):
- orcaflex_test1.sim: 3,901,138 bytes (3.72 MB)
- orcaflex_test2.sim: 3,901,132 bytes (3.72 MB)

Recent (Aug 2025):
- All files: 3,641,326 bytes (3.47 MB) - EXACT match
- Indicates consistent generation
```

### Mock Mode vs License Mode

#### Mock Mode (mock_mode=True)
- No actual OrcaFlex API calls
- Useful for testing workflow logic
- Does NOT create actual .sim files
- Returns success with simulated results

#### License Mode (mock_mode=False)
- Requires OrcaFlex license
- Creates actual .sim files
- Runs real static/dynamic analysis
- File sizes match expected patterns

### Error Handling Patterns

#### Common Errors and Solutions
1. **File Format Issues**
   - Error: "Variable 'X' not recognised"
   - Solution: Check OrcaFlex documentation for correct variable names

2. **Stage Duration**
   - Error: "Must be positive"
   - Solution: Ensure all stage durations > 0

3. **Missing Dependencies**
   - Error: "OrcFxAPI not available"
   - Solution: Verify OrcaFlex installation and Python path

4. **File Lock Issues**
   - Error: "Process cannot access file"
   - Solution: Close log files before cleanup

### Performance Metrics

#### Processing Times (from tests)
- Model loading: ~0.3-0.6 seconds
- Static analysis: ~0.9-1.2 seconds
- .sim file save: ~0.1-0.2 seconds
- Total per model: ~1.4-2.0 seconds

#### Batch Processing Results
- Success rate with .dat files: ~100%
- Success rate with .yml files: ~50% (format sensitive)
- Parallel processing: Up to 5x speedup for large batches

### Testing Commands

#### Basic Test Run
```python
from modules.orcaflex.mooring_tension_iteration.batch_processing.orcaflex_batch_runner import OrcaFlexBatchRunner

# With license
runner = OrcaFlexBatchRunner('batch_config.yml', mock_mode=False)
summary = runner.run_batch()

# Verify .sim files
sim_files = list(runner.sim_dir.glob("*.sim"))
for sim in sim_files:
    print(f"{sim.name}: {sim.stat().st_size / (1024*1024):.2f} MB")
```

#### UV Environment Setup
```bash
# Check UV
uv --version  # Should show 0.8.0 or higher

# Run tests
python -m pytest tests/modules/orcaflex/mooring_tension_iteration/batch_processing/test_batch_runner.py -v
```

### Key Success Factors

1. **Use .dat format** when possible for reliability
2. **Validate YAML structure** before batch processing
3. **Check file sizes** to verify successful generation
4. **Monitor log files** for detailed error messages
5. **Test with mock_mode first** then switch to license mode
6. **Keep stage durations positive** 
7. **Use exact OrcaFlex variable names** from documentation

### Future Improvements

1. Add YAML validator for OrcaFlex format
2. Implement automatic retry for transient failures
3. Add file size validation thresholds
4. Create standard test suite with known-good models
5. Add progress bar for large batch runs
6. Implement distributed processing for cluster environments

---
*Last Updated: 2025-08-17*
*Verified with OrcaFlex 11.5e*