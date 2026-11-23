# Input File Architecture Standards

## Overview

This document defines the **MANDATORY** architecture and standards for input configuration files across all DigitalModel modules. All modules MUST follow this standardized YAML-based configuration pattern to ensure consistency, maintainability, and interoperability.

## Core Architecture Pattern

### 1. Standard Configuration Structure

Every module input file MUST follow this hierarchical structure:

```yaml
# MANDATORY: Metadata section
meta:
  library: digitalmodel                    # REQUIRED: Always 'digitalmodel'
  basename: <module_name>                  # REQUIRED: Module identifier
  version: <semantic_version>              # OPTIONAL: Config version (e.g., "1.0.0")
  description: <brief_description>         # OPTIONAL: Purpose of this configuration

# MANDATORY: Default settings section
default:
  log_level: INFO                          # REQUIRED: Logging level
  config:
    overwrite:
      output: True                          # REQUIRED: Output overwrite policy
  units:                                   # REQUIRED: Unit system definitions
    <parameter>: <unit>

# MODULE-SPECIFIC: Primary module configuration
<module_name>:
  preprocess:                              # OPTIONAL: Pre-processing settings
    <preprocessing_options>
  analysis:                                 # REQUIRED: Analysis configuration
    <analysis_options>
  postprocess:                             # OPTIONAL: Post-processing settings
    <postprocessing_options>

# MANDATORY: File management section
file_management:
  flag: True                                # REQUIRED: Enable/disable file management
  input_directory: <path>                  # REQUIRED: Input files location
  output_directory: <path>                 # REQUIRED: Output files location
  filename:                                 # OPTIONAL: File filtering
    extension: [yml, sim, dat]
    pattern: <naming_pattern>
    filters:
      contains: []
      not_contains: []
```

### 2. Metadata Section (MANDATORY)

The `meta` section provides essential information about the configuration:

```yaml
meta:
  library: digitalmodel                    # Fixed value for all configs
  basename: orcaflex                       # Module being configured
  version: "2.1.0"                        # Configuration schema version
  description: "Mooring tension analysis"  # Human-readable description
  author: "Engineering Team"              # Optional: Config author
  date: "2025-08-15"                      # Optional: Creation/update date
```

### 3. Default Settings (MANDATORY)

Global settings that apply across the entire analysis:

```yaml
default:
  log_level: INFO                          # DEBUG, INFO, WARNING, ERROR, CRITICAL
  config:
    overwrite:
      output: True                          # Allow overwriting existing outputs
      input: False                          # Protect input files from modification
  units:
    # Define ALL units used in the configuration
    length: m                               # meters, ft, inch
    force: kN                              # kN, N, lbf
    pressure: psi                          # psi, Pa, bar
    temperature: degC                      # degC, degF, K
    angle: deg                             # deg, rad
  constants:
    g: 9.81                                # Gravitational acceleration
    rho_water: 1025                        # Water density (kg/m³)
```

### 4. Module-Specific Configuration

Each module defines its analysis pipeline:

```yaml
<module_name>:
  # Pre-processing stage (OPTIONAL)
  preprocess:
    validation:
      flag: True                            # Enable input validation
      strict_mode: False                    # Strict vs permissive validation
    data_preparation:
      flag: True
      interpolation: linear                # linear, cubic, nearest
    
  # Analysis stage (REQUIRED)
  analysis:
    type: static                           # static, dynamic, quasi-static
    method: finite_element                 # FEM, analytical, hybrid
    parameters:
      convergence_tolerance: 1e-6
      max_iterations: 100
      time_step: 0.1                      # For dynamic analysis
    
  # Post-processing stage (OPTIONAL)
  postprocess:
    reports:
      flag: True
      format: [excel, pdf]                 # Output formats
    visualization:
      flag: True
      plots: [time_series, contour]
    statistics:
      flag: True
      metrics: [min, max, mean, std]
```

### 5. File Management (MANDATORY)

Standardized file handling configuration:

```yaml
file_management:
  flag: True                                # Master enable/disable
  
  # Input configuration
  input_directory: ./input                 # Can be absolute or relative
  input_files:
    yml:                                   # Group by file type
      - base_model.yml
      - load_cases.yml
    csv:
      - vessel_data.csv
      - environmental.csv
  
  # Output configuration  
  output_directory: ./results
  output_prefix: analysis_                 # Prefix for generated files
  output_timestamp: True                   # Add timestamp to outputs
  
  # File filtering (OPTIONAL)
  filename:
    extension: [yml, sim, dat]             # File extensions to process
    pattern: "mooring_*"                   # Glob pattern for file names
    filters:
      contains: [final, approved]          # Must contain these strings
      not_contains: [temp, backup, test]   # Must not contain these
```

## Advanced Configuration Patterns

### 1. Multi-Stage Analysis Configuration

For complex analyses with multiple stages:

```yaml
analysis_pipeline:
  stages:
    - name: initialization
      type: static
      config:
        method: linear
        tolerance: 1e-3
    
    - name: main_analysis
      type: dynamic
      depends_on: initialization           # Stage dependency
      config:
        method: time_domain
        duration: 3600                     # seconds
        time_step: 0.1
    
    - name: fatigue_assessment
      type: postprocess
      depends_on: main_analysis
      config:
        method: rainflow
        bins: 20
```

### 2. Parametric Study Configuration

For running multiple variations:

```yaml
parametric_study:
  enabled: True
  parameters:
    wave_height:
      values: [2.0, 4.0, 6.0, 8.0]        # meters
      unit: m
    wave_period:
      values: [8, 10, 12, 14]             # seconds
      unit: s
  combinations: full                       # full, specified, random
  output_organization: by_parameter        # by_parameter, by_run
```

### 3. External Software Integration

Configuration for external tool integration:

```yaml
external_tools:
  orcaflex:
    enabled: True
    version: "11.4"
    license_server: "localhost:5053"
    working_directory: ./orcaflex_work
    timeout: 3600                          # seconds
    retry_attempts: 3
    
  ansys:
    enabled: False
    executable_path: "C:/Program Files/ANSYS Inc/v232/ansys/bin"
    batch_mode: True
```

### 4. Validation Rules

Define custom validation rules:

```yaml
validation:
  rules:
    - parameter: wave_height
      min: 0.1
      max: 15.0
      unit: m
      
    - parameter: water_depth
      min: 10.0
      max: 3000.0
      unit: m
      
    - parameter: vessel_displacement
      min: 1000
      max: 500000
      unit: tonnes
  
  cross_validation:
    - rule: "wave_height < 0.6 * water_depth"
    - rule: "wave_period > 3.0 and wave_period < 25.0"
```

## Configuration Inheritance

### Base Configuration Pattern

Support for configuration inheritance to reduce duplication:

```yaml
# base_config.yml
inherit_from: null                         # No parent
meta:
  library: digitalmodel
  basename: base

default:
  log_level: INFO
  units:
    length: m
    force: kN
```

```yaml
# specific_config.yml
inherit_from: base_config.yml              # Inherit from base
meta:
  basename: specific_analysis

# Override specific values
default:
  log_level: DEBUG                         # Override parent value

# Add new sections
analysis:
  type: dynamic
```

## Best Practices

### 1. Naming Conventions

- Use **lowercase** with underscores for keys: `wave_height`, not `WaveHeight`
- Use **descriptive names**: `max_iterations` instead of `max_iter`
- Group related parameters under sections

### 2. Documentation

Always include inline comments for complex parameters:

```yaml
analysis:
  convergence:
    tolerance: 1e-6  # Relative tolerance for force balance
    max_iterations: 100  # Prevent infinite loops
    damping_factor: 0.8  # 0.5-1.0, lower = more stable but slower
```

### 3. Defaults

Provide sensible defaults where possible:

```yaml
visualization:
  enabled: True
  dpi: 150  # Default: suitable for screen viewing
  format: png  # Default: widely compatible
  colormap: viridis  # Default: colorblind-friendly
```

### 4. Validation

Always validate configurations before use:

```python
def validate_config(config: Dict[str, Any]) -> None:
    """Validate configuration against schema."""
    # Check required sections
    required_sections = ['meta', 'default', 'file_management']
    for section in required_sections:
        if section not in config:
            raise ConfigurationError(f"Missing required section: {section}")
    
    # Validate meta section
    if config['meta'].get('library') != 'digitalmodel':
        raise ConfigurationError("Invalid library in meta section")
    
    # Validate units
    required_units = ['length', 'force']
    for unit in required_units:
        if unit not in config['default'].get('units', {}):
            raise ConfigurationError(f"Missing required unit: {unit}")
```

## Module-Specific Extensions

Each module can extend the base structure with domain-specific sections:

### OrcaFlex Module Example

```yaml
orcaflex_analysis:
  mooring_tension_iteration:
    method: scipy                          # scipy, newton_raphson
    convergence:
      tolerance: 1.0                      # percentage
      max_iterations: 10
    target_tensions:
      Line1: 1500.0                       # kN
      Line2: 1500.0                       # kN
```

### AQWA Module Example

```yaml
aqwa_analysis:
  hydrodynamics:
    frequency_range:
      min: 0.1                            # rad/s
      max: 2.0                            # rad/s
      steps: 50
    wave_directions: [0, 45, 90, 135, 180]  # degrees
```

## Validation Schema

JSON Schema for validation (store in `config_schemas.py`):

```python
CONFIG_SCHEMA = {
    "type": "object",
    "required": ["meta", "default", "file_management"],
    "properties": {
        "meta": {
            "type": "object",
            "required": ["library", "basename"],
            "properties": {
                "library": {"const": "digitalmodel"},
                "basename": {"type": "string"},
                "version": {"type": "string", "pattern": "^\\d+\\.\\d+\\.\\d+$"}
            }
        },
        "default": {
            "type": "object",
            "required": ["log_level", "config", "units"],
            "properties": {
                "log_level": {"enum": ["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"]},
                "config": {"type": "object"},
                "units": {"type": "object"}
            }
        },
        "file_management": {
            "type": "object",
            "required": ["flag", "input_directory", "output_directory"],
            "properties": {
                "flag": {"type": "boolean"},
                "input_directory": {"type": "string"},
                "output_directory": {"type": "string"}
            }
        }
    }
}
```

## Migration Guide

For existing configurations not following this standard:

1. **Add meta section** with library and basename
2. **Consolidate defaults** into default section
3. **Standardize file management** section
4. **Update key naming** to lowercase_with_underscores
5. **Add units section** with all unit definitions
6. **Validate** using the schema

## Enforcement

- **CI/CD Integration**: Validate all YAML configs in tests
- **Pre-commit Hooks**: Check configuration structure
- **Documentation**: Generate docs from config schemas
- **Tooling**: Provide config validation utilities

---

**IMPORTANT**: This architecture is MANDATORY for all new modules and should be adopted by existing modules during updates. Non-compliant configurations will be rejected during code review.