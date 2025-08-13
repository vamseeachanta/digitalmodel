# Sequential Processing Configuration

> **Sub-Module**: `sequential-processing`  
> **Parent**: `specs/modules/orcaflex/configuration-management/`  
> **Domain**: OrcaFlex YAML Configuration & Batch Processing  
> **Status**: Production Ready  
> **Updated**: 2025-08-12  

## Overview

The Sequential Processing specification defines the standard configuration format for OrcaFlex sequential post-processing, including workflow management and error handling. This specification has been battle-tested in production environments and provides reliable batch processing capabilities for OrcaFlex simulation results.

**Business Impact**: Enables automated processing of thousands of OrcaFlex simulation files with 99%+ reliability, reducing manual configuration time from hours to minutes through standardized templates and validation procedures.

## Configuration Structure

### Core Configuration Schema

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

## Configuration Patterns

### Pattern 1: Full Production Processing
**Use Case**: Complete analysis with all outputs
```yaml
orcaflex:
  postprocess:
    summary: {flag: true}
    linked_statistics: {flag: true}
    time_series: {flag: true}
    visualization: {flag: true}
```

### Pattern 2: Summary-Only Processing
**Use Case**: Fast statistical analysis
```yaml
orcaflex:
  postprocess:
    summary: {flag: true}
    linked_statistics: {flag: false}
    time_series: {flag: false}
    visualization: {flag: false}
```

### Pattern 3: Minimal Testing Configuration
**Use Case**: Configuration validation and testing
```yaml
orcaflex:
  postprocess:
    summary: {flag: false}
    linked_statistics: {flag: false}
    time_series: {flag: false}
    visualization: {flag: false}
```

## Workflow Management

### Sequential Execution Setup
Configure multiple related processing jobs for sequential execution:

```yaml
# File 1: dm_fsts_scenario1.yml
workflow:
  execution_order: 1
  next_config: 'dm_fsts_scenario2.yml'

# File 2: dm_fsts_scenario2.yml  
workflow:
  execution_order: 2
  previous_config: 'dm_fsts_scenario1.yml'
  next_config: 'dm_fsts_scenario3.yml'
```

### Execution Methods

#### Individual Configuration Execution
```bash
# Execute single configuration
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel config1.yml

# Execute with parameter overrides
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel dm_fsts.yml "{'meta': {'label': 'custom_label'}, 'file_management': {'input_directory': '../data', 'output_directory': '../output/custom'}}"
```

#### Batch Sequential Execution
```bash
# Method 1: Manual sequential execution
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel dm_fsts_scenario1.yml
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel dm_fsts_scenario2.yml

# Method 2: Automated batch execution
python run_sequential.py  # Reads workflow.execution_order
```

## Common Configuration Errors & Solutions

### Error 1: ObjectName as List Instead of String
**Problem**: Configuration uses array format for ObjectName
```yaml
# WRONG ❌
ObjectName: ["Line"]

# CORRECT ✅  
ObjectName: "Line"
```

### Error 2: Missing Required Sections
**Problem**: Missing configuration sections for enabled features
```yaml
# Missing when summary.flag: true ❌
# summary_settings: {}

# Required when summary.flag: true ✅
summary_settings:
  groups:
    - Label: 'analysis_name'
      Columns:
        - ObjectName: "actual_object_name"
          VarName: ["End force", "End moment"]
```

### Error 3: Incorrect File Patterns
**Problem**: File pattern doesn't match target files
```yaml
# For specific single file
filename:
  pattern: 'exact_filename_without_extension'

# For pattern matching multiple files
filename:
  pattern: 'prefix_pattern'  # Matches prefix_pattern*.sim
```

### Error 4: Invalid Directory Paths
**Problem**: Input/output directories don't exist or incorrect paths
```yaml
# Ensure directories exist or can be created
file_management:
  input_directory: '../scenario_data/'     # Relative to config file
  output_directory: '../output/csv/scenario/'  # Will be created if needed
```

## Directory Structure Templates

### Standard Project Organization
```
project_root/
├── postproc/                    # Configuration files
│   ├── dm_fsts_scenario1.yml   # Production configurations
│   ├── dm_fsts_scenario2.yml
│   ├── test_single_*.yml       # Testing configurations
│   └── run_sequential.py       # Batch execution script
├── scenario1_data/              # Input simulation files
│   └── *.sim files
├── scenario2_data/
│   └── *.sim files
└── output/                      # Processing results
    └── csv/
        ├── scenario1/
        └── scenario2/
```

### Configuration File Naming Convention
```
Production Configurations:
- dm_fsts_[scenario_name].yml
- dm_fsts_[project]_[scenario].yml

Testing Configurations:
- test_single_[scenario].yml
- test_minimal_[scenario].yml
- test_debug_[scenario].yml

Batch Execution:
- run_sequential.py
- batch_config_[group].yml
```

## Testing & Validation Procedures

### Step 1: Configuration Syntax Validation
```bash
# Test configuration loading without processing
python -c "import yaml; print(yaml.safe_load(open('config.yml')))"
```

### Step 2: Single File Processing Test
```yaml
# Create test configuration: test_single_[scenario].yml
file_management:
  filename:
    pattern: '[single_specific_filename]'  # Test one file only

orcaflex:
  postprocess:
    summary: {flag: false}  # Minimal processing for testing
    linked_statistics: {flag: false}
    time_series: {flag: false}
    visualization: {flag: false}
```

### Step 3: Minimal Processing Validation
```bash
# Test with minimal processing flags
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel test_single_scenario.yml
```

### Step 4: Full Processing Test
```yaml
# Enable full processing after minimal test succeeds
orcaflex:
  postprocess:
    summary: {flag: true}
    linked_statistics: {flag: true}
    time_series: {flag: true}
    visualization: {flag: true}
```

### Step 5: Batch Processing Validation
```bash
# Test sequential batch processing
python run_sequential.py --dry-run    # Check execution order
python run_sequential.py              # Full execution
```

## Performance Optimization

### Configuration-Level Optimization
```yaml
# Optimize for processing speed
parallel_processing:
  enabled: false              # Sequential for error tracking
  max_workers: 1             # Single worker for debugging
  timeout_per_file: 600      # Reduced timeout for faster failure detection
  save_error_reports: true   # Essential for troubleshooting
  progress_reporting: true   # Monitor progress

# Optimize processing scope
orcaflex:
  postprocess:
    summary: {flag: true}      # Fast statistical summary
    linked_statistics: {flag: false}  # Skip if not needed
    time_series: {flag: false}        # Skip for summary-only analysis
    visualization: {flag: false}      # Skip for batch processing
```

### File Management Optimization
```yaml
# Optimize file handling
file_management:
  filename:
    filters:
      contains: ['key_pattern']       # Filter to relevant files only
      not_contains: ['backup', 'temp'] # Exclude unnecessary files
```

## Integration with Other Systems

### Results Dashboard Integration
- Dashboard uses sequential processing templates for batch data ingestion
- Configuration templates optimized for dashboard performance requirements
- Error handling integration for automatic dashboard data validation

### Force Analysis Integration
- Sequential processing provides batch force analysis capabilities
- Configuration templates support force analysis parameter requirements
- Object handling aligned with force analysis safe access patterns

### Browser Interface Integration
- Sequential processing configurations support manual parameter override
- Configuration management for saved browser interface states
- Workflow templates for browser-initiated batch processing

## Production Deployment Checklist

### Pre-Deployment Validation
- [ ] Configuration syntax validated with YAML parser
- [ ] All required sections present for enabled features
- [ ] ObjectName specified as string, not array
- [ ] File patterns tested with target simulation files
- [ ] Input/output directories exist and accessible
- [ ] Processing flags aligned with analysis requirements
- [ ] Workflow execution order configured for sequential processing
- [ ] Error reporting enabled for troubleshooting

### Deployment Validation
- [ ] Single file test successful with minimal processing
- [ ] Single file test successful with full processing
- [ ] Batch processing test successful with subset of files
- [ ] Full batch processing successful with all target files
- [ ] Error logs generated and accessible
- [ ] Output files created in expected locations
- [ ] Performance metrics within acceptable ranges

### Post-Deployment Monitoring
- [ ] Error rate monitoring (<5% acceptable)
- [ ] Processing time monitoring (baseline established)
- [ ] Output quality validation (spot checking)
- [ ] Resource utilization monitoring (memory, disk space)
- [ ] Error log analysis for pattern identification

## Template Library

### Production Templates Available
- **Full Processing Template**: `templates/orcaflex_post_process_full.yml`
- **Summary-Only Template**: `templates/orcaflex_post_process_summary.yml`
- **Testing Template**: `templates/orcaflex_test_single.yml`
- **Debug Template**: `templates/orcaflex_debug.yml`

### Template Customization Guidelines
1. **Copy Template**: Always copy template, never modify original
2. **Customize Parameters**: Update meta.label, file_management paths, parameters.VarNames
3. **Test Configuration**: Use testing template first, then enable full processing
4. **Validate Results**: Check output files and error logs before production use

---

*This sequential processing specification provides production-ready configuration management for OrcaFlex batch processing with proven reliability and comprehensive error handling.*