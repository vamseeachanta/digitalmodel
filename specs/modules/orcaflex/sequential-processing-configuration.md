# OrcaFlex Sequential Processing Configuration Spec

## Overview
This spec defines the standard configuration format for OrcaFlex sequential post-processing, including workflow management and error handling.

## Configuration Structure

### Required Sections
```yaml
meta:
  basename: orcaflex_post_process
  library: digitalmodel
  label: '[unique_identifier]'
  description: '[processing_description]'

workflow:                    # Optional: For sequential execution
  execution_order: [number]  # Integer for processing sequence
  next_config: '[filename]'  # Next config in sequence (optional)
  previous_config: '[filename]' # Previous config reference (optional)

default:
  log_level: DEBUG
  config:
    overwrite:
      output: True

parameters:
  VarNames:
    Line: [End force, End moment, Effective tension, Wall tension, Bend moment, Curvature]
    # Add other object types as needed

orcaflex:
  postprocess:
    visualization: {flag: false}
    summary:
      flag: true
      statistics: {Minimum: true, Maximum: true, Mean: true, StdDev: true}
      min: true
      max: true  
      mean: true
    linked_statistics: {flag: false}
    RangeGraph: {flag: false}
    time_series: {flag: false}
    cummulative_histograms: {flag: false}

summary_settings:            # Required when summary.flag: true
  groups:
    - Label: '[analysis_name]'
      Columns:
        - ObjectName: "[actual_object_name]"  # String, not list!
          VarName: ["End force", "End moment", "Effective tension"]

time_series_settings:
  data: false

file_management:
  flag: true
  input_directory: '[relative_path_to_sim_files]'
  output_directory: '[relative_path_to_output]'
  filename:
    extension: [sim]
    pattern: '[filename_pattern]'
    filters:
      contains: []
      not_contains: []

parallel_processing:
  enabled: false           # Use false for sequential processing
  max_workers: 1
  timeout_per_file: 1200   # 20 minutes
  save_error_reports: true
  progress_reporting: true
```

## Common Configuration Patterns

### Pattern 1: Full Processing
```yaml
orcaflex:
  postprocess:
    summary: {flag: true}
    linked_statistics: {flag: true}
    time_series: {flag: true}
    visualization: {flag: true}
```

### Pattern 2: Minimal Processing (For Testing)
```yaml
orcaflex:
  postprocess:
    summary: {flag: false}
    linked_statistics: {flag: false}
    time_series: {flag: false}
    visualization: {flag: false}
```

### Pattern 3: Summary Only
```yaml
orcaflex:
  postprocess:
    summary: {flag: true}
    # All others false
```

## Workflow Management

### Sequential Execution Setup
```yaml
# File 1: dm_fsts_scenario1.yml
workflow:
  execution_order: 1
  next_config: 'dm_fsts_scenario2.yml'

# File 2: dm_fsts_scenario2.yml  
workflow:
  execution_order: 2
  previous_config: 'dm_fsts_scenario1.yml'
```

### Execution Methods
```bash
# Method 1: Individual execution
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel config1.yml
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel config2.yml

# Method 2: Batch execution script
python run_sequential.py  # Reads workflow.execution_order
```

## Common Configuration Errors

### Error 1: List Instead of String for ObjectName
```yaml
# WRONG
ObjectName: ["Line"]

# CORRECT  
ObjectName: "Line"
```

### Error 2: Missing Required Sections
```yaml
# Missing when summary.flag: true
summary_settings: {}

# Missing always
parameters:
  VarNames: {}
```

### Error 3: Incorrect File Patterns
```yaml
# For specific file
pattern: 'exact_filename_without_extension'

# For pattern matching
pattern: 'prefix_pattern'  # Matches prefix_pattern*.sim
```

## Directory Structure Template
```
project_root/
├── postproc/
│   ├── dm_fsts_scenario1.yml
│   ├── dm_fsts_scenario2.yml
│   ├── run_sequential.py
│   └── test_single_*.yml
├── scenario1_data/
│   └── *.sim files
├── scenario2_data/
│   └── *.sim files
└── output/
    └── csv/
        ├── scenario1/
        └── scenario2/
```

## Testing Procedure

### Step 1: Create Test Configuration
```yaml
# test_single_[scenario].yml
file_management:
  filename:
    pattern: '[single_specific_filename]'  # Test one file only

orcaflex:
  postprocess:
    summary: {flag: false}  # Minimal processing for testing
```

### Step 2: Validate Configuration
```bash
# Test configuration syntax and basic processing
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel test_single_scenario.yml
```

### Step 3: Enable Full Processing
```yaml
orcaflex:
  postprocess:
    summary: {flag: true}
    # Enable other features as needed
```

## Troubleshooting Guide

### Issue: "Object not found in model"
**Cause**: Incorrect ObjectName in summary_settings
**Solution**: 
1. Check actual object names in OrcaFlex model
2. Use correct object names (not generic types)
3. Test with summary.flag: false first

### Issue: "'summary_settings' KeyError"
**Cause**: Missing summary_settings section when summary.flag: true
**Solution**: Add complete summary_settings block

### Issue: Sequential execution stops
**Cause**: Error in one file prevents continuation
**Solution**: 
1. Use parallel_processing.save_error_reports: true
2. Check error logs
3. Apply fixes from troubleshooting-missing-objects.md

## Configuration Validation Checklist

- [ ] All required sections present
- [ ] ObjectName as string, not list
- [ ] Correct file patterns for target sim files
- [ ] Output directories exist or can be created
- [ ] Processing flags match intended analysis scope
- [ ] Workflow execution_order set for sequential processing
- [ ] Error reporting enabled for debugging

## Template Files Location
- **Full Template**: `[repo]/templates/orcaflex_post_process_full.yml`
- **Minimal Template**: `[repo]/templates/orcaflex_post_process_minimal.yml`
- **Test Template**: `[repo]/templates/orcaflex_test_single.yml`

## Related Specifications
- `troubleshooting-missing-objects.md` - Error handling procedures
- `orcaflex-batch-processing.md` - Large-scale processing guidelines
- `configuration-validation.md` - Automated validation procedures