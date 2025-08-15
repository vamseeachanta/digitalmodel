# AQWA Test Configuration Standardization Guide

## Overview
This document describes the standardization applied to all AQWA test configuration files to ensure consistency, maintainability, and alignment with the project's configuration architecture.

## Standard Configuration Structure

### Meta Section (Required)
All AQWA configuration files must start with a meta section following this format:

```yaml
meta:
  library: digitalmodel
  basename: aqwa
  version: "1.0.0"
  description: "AQWA [specific type] configuration"
```

This replaces the previous direct `basename: aqwa` approach and aligns with the OrcaFlex module's configuration pattern.

### Software Configuration
```yaml
software:
  version: []
  ANSYSInstallDir: C:\Program Files\ANSYS Inc\v222  # Standardized version
```

### Type Configuration
```yaml
type:
  preprocess: false  # Boolean values in lowercase
  analysis: false
  results: false
```

### File Management Configuration
```yaml
file_management:
  flag: true
  update_unfinished:
    flag: false
  files:
    files_in_current_directory:
      flag: false
      auto_read: true
      directory: null  # Use lowercase 'null'
      file_extensions: []
      filename_pattern: null
    output_directory: null
```

### Default Configuration
```yaml
default:
  log_level: DEBUG
  config:
    overwrite:
      output: true
    cfg_sensitivities: false
```

## Standardization Rules Applied

### 1. Meta Section
- **Old Format**: `basename: aqwa` at root level
- **New Format**: Structured meta section with library, basename, version, and description
- **Rationale**: Provides better metadata management and aligns with project standards

### 2. Boolean Values
- **Old Format**: Mixed case (`True`, `False`)
- **New Format**: Lowercase only (`true`, `false`)
- **Rationale**: YAML 1.2 specification recommends lowercase booleans

### 3. Null Values
- **Old Format**: Uppercase `NULL`
- **New Format**: Lowercase `null`
- **Rationale**: YAML standard uses lowercase null

### 4. ANSYS Installation Directory
- **Old Format**: Mixed versions (v181, v182, v222) with comments
- **New Format**: Standardized to `C:\Program Files\ANSYS Inc\v222`
- **Rationale**: Use latest version consistently across all tests

### 5. Comments
- Removed redundant commented-out ANSYS command examples
- Kept only essential documentation comments

## Files Updated

All 18 AQWA test configuration files have been updated:

1. `aqwa_anl_raos.yml` - RAO analysis
2. `aqwa_damping_analysis.yml` - Damping analysis
3. `aqwa_lis_mooring_forces.yml` - Mooring forces from LIS
4. `aqwa_lis_damp.yml` - LIS damping
5. `aqwa_mes_files.yml` - MES file processing
6. `aqwa_no_mes_files.yml` - Configuration without MES files
7. `aqwa_pre_process_deck.yml` - Deck preprocessing
8. `aqwa_pre_process_deck_10.yml` - Deck preprocessing variant 10
9. `aqwa_pre_process_deck_11.yml` - Deck preprocessing variant 11
10. `aqwa_pre_process_deck_15.yml` - Deck preprocessing variant 15
11. `aqwa_lis_rao_at_node.yml` - RAO at specific node
12. `aqwa_lis_rao_acc_at_CoG.yml` - RAO acceleration at center of gravity
13. `aqwa_anl_ef_run.yml` - ANL external force run
14. `aqwareader_raos.yml` - AQWA reader for RAOs
15. `aqwareader_raos_ad.yml` - AQWA reader with additional damping
16. `aqwareader_timeresponse.yml` - Time response reading
17. `aqwareader_moorings.yml` - Mooring data reading
18. `base_aqwa_config.yml` - Base template for new configurations

## Validation

All updated files have been validated for:
- ✅ Valid YAML syntax
- ✅ Proper indentation
- ✅ Compatibility with the digitalmodel engine (supports both old and new basename formats)
- ✅ Consistent formatting across all files

## Migration Notes

The digitalmodel engine (`src/digitalmodel/engine.py`) supports both formats:
- Direct `basename` at root (legacy)
- `meta.basename` structure (new standard)

This ensures backward compatibility while moving toward the standardized format.

## Benefits

1. **Consistency**: All AQWA test files follow the same structure
2. **Maintainability**: Easier to update and manage configurations
3. **Clarity**: Clear metadata about each configuration's purpose
4. **Alignment**: Matches the configuration pattern used by other modules (e.g., OrcaFlex)
5. **Version Control**: Version tracking in configuration files
6. **Documentation**: Self-documenting through descriptions

## Future Considerations

- Consider creating a schema validation tool to enforce these standards
- Automate configuration migration for new test files
- Extend standardization to other module configurations