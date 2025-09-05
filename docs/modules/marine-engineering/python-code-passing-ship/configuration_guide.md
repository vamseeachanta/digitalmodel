# Configuration Guide

## Overview

The Passing Ship Forces module uses YAML configuration files to define vessel parameters, environmental conditions, and calculation settings. This guide provides comprehensive documentation for all configuration options.

## Configuration Structure

The configuration file consists of three main sections:

```yaml
vessel:       # Vessel parameters
  moored:     # Moored vessel specifications
  passing:    # Passing vessel specifications

environment:  # Environmental conditions
  water_depth: 
  water_density:
  separation_distance:

calculation:  # Calculation settings
  stagger_range:
  stagger_points:
  # ... additional options
```

## Vessel Configuration

### Moored Vessel Parameters

The moored vessel is the stationary vessel experiencing forces from the passing ship.

```yaml
vessel:
  moored:
    length: 180.0           # Length overall [m] (required)
    beam: 32.0             # Maximum beam [m] (required)
    draft: 12.0            # Draft at calculation condition [m] (required)
    displacement: 45000.0   # Displacement [tonnes] (optional)
    block_coefficient: 0.85 # Block coefficient [-] (default: 0.85)
    
    # Optional geometric refinements
    waterline_length: 175.0 # Length at waterline [m] (default: 0.97*length)
    wetted_beam: 31.5      # Beam at waterline [m] (default: 0.98*beam)
    
    # Cross-sectional area parameters (optional)
    midship_area_coefficient: 0.98  # Cm [-] (default: 0.98)
    prismatic_coefficient: 0.65     # Cp [-] (default: calculated)
```

### Passing Vessel Parameters

The passing vessel is the moving vessel generating the disturbance field.

```yaml
vessel:
  passing:
    length: 200.0          # Length overall [m] (required)
    beam: 35.0            # Maximum beam [m] (required)
    draft: 14.0           # Draft [m] (required)
    velocity: 5.0         # Passing speed [m/s] (required)
    displacement: 55000.0  # Displacement [tonnes] (optional)
    block_coefficient: 0.82 # Block coefficient [-] (default: 0.85)
    
    # Velocity components (optional, for oblique passing)
    velocity_x: 5.0       # Longitudinal velocity [m/s]
    velocity_y: 0.5       # Lateral velocity [m/s]
    
    # Optional parameters
    heading: 0.0          # Heading relative to moored vessel [deg]
```

### Vessel Type Templates

Pre-defined templates for common vessel types:

#### Tanker Configuration

```yaml
vessel:
  moored:
    length: 333.0
    beam: 60.0
    draft: 20.8
    displacement: 300000.0
    block_coefficient: 0.85
  
  passing:
    length: 280.0
    beam: 48.0
    draft: 16.5
    velocity: 3.0
    displacement: 160000.0
    block_coefficient: 0.82
```

#### Container Ship Configuration

```yaml
vessel:
  moored:
    length: 350.0
    beam: 45.6
    draft: 14.5
    displacement: 140000.0
    block_coefficient: 0.65
  
  passing:
    length: 300.0
    beam: 40.0
    draft: 13.0
    velocity: 8.0
    displacement: 100000.0
    block_coefficient: 0.68
```

#### Offshore Vessel Configuration

```yaml
vessel:
  moored:
    length: 180.0
    beam: 32.0
    draft: 8.5
    displacement: 35000.0
    block_coefficient: 0.75
  
  passing:
    length: 95.0
    beam: 22.0
    draft: 7.0
    velocity: 6.0
    displacement: 8000.0
    block_coefficient: 0.70
```

## Environmental Configuration

### Water and Depth Parameters

```yaml
environment:
  # Water depth - can be numeric or 'infinite'
  water_depth: 50.0        # [m] or 'infinite' (required)
  
  # Water properties
  water_density: 1025.0    # [kg/m³] (default: 1025.0)
  kinematic_viscosity: 1.19e-6  # [m²/s] (default: 1.19e-6)
  
  # Separation distance
  separation_distance: 50.0 # Lateral separation between vessel centerlines [m]
  
  # Current (optional)
  current:
    velocity: 0.5          # Current speed [m/s]
    direction: 90.0        # Current direction [deg]
    profile: 'uniform'     # 'uniform' or 'logarithmic'
```

### Depth Ratio Considerations

```yaml
environment:
  # Shallow water effects become significant when h/T < 3
  water_depth: 25.0        # Shallow water (h/T ≈ 2)
  # water_depth: 100.0    # Intermediate (h/T ≈ 8)
  # water_depth: 'infinite' # Deep water (h/T > 20)
```

## Calculation Configuration

### Basic Calculation Settings

```yaml
calculation:
  # Stagger distance range
  stagger_range: [-400.0, 400.0]  # [m] (default: [-2*L, 2*L])
  stagger_points: 81              # Number of points (default: 81)
  
  # Depth corrections
  use_finite_depth_correction: true  # Apply shallow water effects
  harmonic_terms: 10                 # Number of harmonic terms (default: 10)
  
  # Numerical integration
  integration_tolerance: 1e-6        # Quadrature tolerance (default: 1e-6)
  max_integration_points: 1000       # Maximum quadrature points
  
  # Performance settings
  use_cache: true                   # Enable result caching
  parallel_cores: 4                 # Number of CPU cores for parallel processing
```

### Advanced Calculation Options

```yaml
calculation:
  # Formulation selection
  formulation: 'wang'              # 'wang', 'yeung', or 'tuck'
  
  # Coordinate system
  coordinate_system: 'ship-fixed'   # 'ship-fixed' or 'earth-fixed'
  origin: 'midship'                # 'midship', 'bow', or 'stern'
  
  # Sectional area curves
  area_curve_method: 'parabolic'   # 'parabolic', 'lewis', or 'custom'
  
  # Custom area curves (if method='custom')
  custom_area_curve:
    stations: [0, 0.1, 0.2, ..., 1.0]  # Normalized positions
    areas: [0, 15.2, 28.5, ..., 0]     # Areas [m²]
  
  # Force components to calculate
  calculate_forces:
    surge: true
    sway: true
    heave: false   # Not implemented
    roll: false    # Not implemented
    pitch: false   # Not implemented
    yaw: true
```

### Parametric Study Configuration

```yaml
calculation:
  # Parametric study settings
  parametric_study:
    enabled: true
    variable: 'separation_distance'  # Parameter to vary
    values: [30, 40, 50, 60, 70, 80, 90, 100]  # Values to test
    
    # Alternative: range specification
    # range:
    #   start: 30
    #   stop: 100
    #   step: 10
    
    # Multiple parameters (2D study)
    variables:
      - name: 'separation_distance'
        values: [40, 50, 60]
      - name: 'water_depth'
        values: [25, 50, 'infinite']
```

### Optimization Settings

```yaml
calculation:
  # Performance optimization
  optimization:
    # Adaptive quadrature settings
    adaptive_quadrature: true
    initial_segments: 10
    max_subdivisions: 50
    
    # Caching strategy
    cache_strategy: 'aggressive'  # 'aggressive', 'moderate', 'minimal'
    cache_size_mb: 100           # Maximum cache size
    
    # Vectorization
    use_vectorization: true       # Use NumPy vectorization
    chunk_size: 1000             # Vector operation chunk size
```

## Output Configuration

### Output Formats and Options

```yaml
output:
  # Output directory
  directory: './results/'          # Base output directory
  
  # File formats
  formats:
    - json                        # JSON with metadata
    - csv                         # CSV tabular data
    - mat                         # MATLAB format
    - hdf5                        # HDF5 for large datasets
  
  # File naming
  filename_prefix: 'psf'          # Prefix for output files
  timestamp: true                 # Add timestamp to filenames
  
  # Data content
  include_metadata: true          # Include configuration in output
  include_derivatives: false      # Include force derivatives
  decimal_places: 6              # Precision for numeric output
```

### Visualization Configuration

```yaml
visualization:
  # Plot generation
  generate_plots: true
  plot_format: 'png'              # 'png', 'pdf', 'svg'
  dpi: 300                        # Resolution for raster formats
  
  # Plot types
  plots:
    - force_distribution         # Forces vs stagger distance
    - peak_values               # Maximum force values
    - parametric_sensitivity    # Parametric study results
    - force_coefficients        # Non-dimensional coefficients
  
  # Plot styling
  style:
    theme: 'seaborn'            # Matplotlib style
    figure_size: [12, 8]        # Figure size [width, height] inches
    line_width: 2               # Line width in points
    grid: true                  # Show grid
    legend_location: 'best'     # Legend position
  
  # Interactive features
  interactive: false            # Enable interactive plots
  save_html: false             # Save as interactive HTML (requires plotly)
```

## Complete Configuration Examples

### Example 1: Basic Analysis

```yaml
# basic_analysis.yaml
vessel:
  moored:
    length: 180.0
    beam: 32.0
    draft: 12.0
  
  passing:
    length: 200.0
    beam: 35.0
    draft: 14.0
    velocity: 5.0

environment:
  water_depth: 50.0
  separation_distance: 50.0

calculation:
  stagger_range: [-400, 400]
  stagger_points: 81
```

### Example 2: Shallow Water with Parametric Study

```yaml
# shallow_water_study.yaml
vessel:
  moored:
    length: 333.0
    beam: 60.0
    draft: 20.8
    displacement: 300000.0
    block_coefficient: 0.85
  
  passing:
    length: 280.0
    beam: 48.0
    draft: 16.5
    velocity: 3.0
    displacement: 160000.0

environment:
  water_depth: 30.0  # h/T = 1.44 (very shallow)
  water_density: 1025.0
  separation_distance: 60.0

calculation:
  stagger_range: [-600, 600]
  stagger_points: 121
  use_finite_depth_correction: true
  harmonic_terms: 20  # More terms for shallow water
  
  parametric_study:
    enabled: true
    variable: 'water_depth'
    values: [25, 30, 40, 50, 'infinite']

output:
  directory: './shallow_water_results/'
  formats: ['json', 'csv']

visualization:
  generate_plots: true
  plots:
    - force_distribution
    - parametric_sensitivity
```

### Example 3: High-Performance Batch Processing

```yaml
# batch_processing.yaml
vessel:
  moored:
    length: 180.0
    beam: 32.0
    draft: 12.0
  
  passing:
    length: ${PASSING_LENGTH}  # Variable substitution
    beam: ${PASSING_BEAM}
    draft: ${PASSING_DRAFT}
    velocity: ${PASSING_VELOCITY}

environment:
  water_depth: ${WATER_DEPTH}
  separation_distance: ${SEPARATION}

calculation:
  stagger_range: [-300, 300]
  stagger_points: 61
  
  # Performance optimization for batch
  optimization:
    adaptive_quadrature: true
    cache_strategy: 'aggressive'
    use_vectorization: true
  
  # Parallel processing
  parallel_cores: 8
  use_cache: true

output:
  directory: './batch_results/${RUN_ID}/'
  formats: ['json']
  decimal_places: 4  # Reduce precision for smaller files
```

## Environment Variables and Expressions

### Variable Substitution

The configuration supports environment variable substitution:

```yaml
vessel:
  moored:
    length: ${VESSEL_LENGTH}      # From environment
    beam: ${VESSEL_BEAM:-32.0}     # With default value
    draft: ${VESSEL_DRAFT}
```

### Mathematical Expressions

Simple expressions are evaluated:

```yaml
vessel:
  moored:
    length: 180.0
    beam: 32.0
    waterline_length: "0.97 * 180.0"  # Evaluated to 174.6
    
calculation:
  stagger_range: ["-2.5 * 180", "2.5 * 180"]  # [-450, 450]
```

### Configuration Inheritance

Configurations can inherit from templates:

```yaml
# Inherit from template
extends: 'templates/tanker.yaml'

# Override specific values
vessel:
  passing:
    velocity: 6.0  # Override template velocity

environment:
  separation_distance: 40.0  # Override template separation
```

## Validation Rules

### Required Fields

The following fields are mandatory:
- `vessel.moored.length`
- `vessel.moored.beam`
- `vessel.moored.draft`
- `vessel.passing.length`
- `vessel.passing.beam`
- `vessel.passing.draft`
- `vessel.passing.velocity`
- `environment.water_depth`
- `environment.separation_distance`

### Value Constraints

```yaml
# Dimension constraints
vessel:
  moored:
    length: 180.0      # Must be > 0
    beam: 32.0         # Must be > 0 and < length
    draft: 12.0        # Must be > 0 and < beam/2
    block_coefficient: 0.85  # Must be 0 < Cb < 1

# Physical constraints
environment:
  water_depth: 50.0    # Must be > max(draft) or 'infinite'
  water_density: 1025.0  # Typical: 1000-1030 kg/m³
  separation_distance: 50.0  # Must be > (B1 + B2)/2

# Numerical constraints
calculation:
  stagger_points: 81   # Must be >= 2
  harmonic_terms: 10   # Must be 1-50
  integration_tolerance: 1e-6  # Must be > 0 and < 0.01
```

### Warning Conditions

The configuration parser issues warnings for:
- Shallow water (h/T < 3) without finite depth correction
- Very close passing (separation < 1.5 * (B1 + B2))
- Excessive harmonic terms (> 30) impacting performance
- Cache disabled for batch processing

## Configuration Best Practices

### 1. Start Simple

Begin with minimal configuration and add complexity as needed:

```yaml
# Start with this
vessel:
  moored:
    length: 180.0
    beam: 32.0
    draft: 12.0
  passing:
    length: 200.0
    beam: 35.0
    draft: 14.0
    velocity: 5.0
environment:
  water_depth: 50.0
  separation_distance: 50.0
calculation:
  stagger_range: [-400, 400]
  stagger_points: 81
```

### 2. Use Templates

Create and reuse templates for common vessel types:

```yaml
# templates/vlcc.yaml
vessel:
  moored:
    length: 333.0
    beam: 60.0
    draft: 20.8
    displacement: 300000.0
    block_coefficient: 0.85
```

### 3. Document Units

Always include units in comments:

```yaml
vessel:
  moored:
    length: 180.0  # [m] Length overall
    beam: 32.0     # [m] Maximum beam
    draft: 12.0    # [m] Draft at calculation condition
```

### 4. Version Control

Include version and metadata:

```yaml
# Metadata (ignored by parser but useful for tracking)
_metadata:
  version: '1.0.0'
  created: '2025-01-05'
  author: 'Marine Engineering Team'
  description: 'Standard tanker passing scenario'
```

### 5. Validate Early

Use the configuration validator before running calculations:

```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship import validate_config

# Validate configuration
is_valid, errors = validate_config('config.yaml')
if not is_valid:
    for error in errors:
        print(f"Error: {error}")
```

## Loading Configuration in Code

```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship import load_config

# Load from file
config = load_config('config.yaml')

# Load with variable substitution
import os
os.environ['VESSEL_LENGTH'] = '200.0'
config = load_config('config_with_vars.yaml')

# Load and merge multiple configs
base_config = load_config('base.yaml')
override_config = load_config('override.yaml')
config = merge_configs(base_config, override_config)

# Programmatic configuration
config = {
    'vessel': {
        'moored': {'length': 180, 'beam': 32, 'draft': 12},
        'passing': {'length': 200, 'beam': 35, 'draft': 14, 'velocity': 5}
    },
    'environment': {
        'water_depth': 50,
        'separation_distance': 50
    }
}
```