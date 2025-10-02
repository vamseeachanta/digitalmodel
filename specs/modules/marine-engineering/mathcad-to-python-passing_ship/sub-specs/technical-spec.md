# Technical Specification

This is the technical specification for the spec detailed in @specs/modules/ship_design/passing-ship-forces/spec.md

> Created: 2025-01-01
> Version: 1.0.0

## Technical Requirements

### Core Calculation Requirements
- Implement Wang's formulation for ship-to-ship interaction forces
- Support both infinite and finite water depth calculations
- Calculate surge force, sway force, and yaw moment components
- Handle sectional area curves for arbitrary hull forms
- Implement numerical integration with configurable precision
- Support both SI and Imperial unit systems with automatic conversion

### Mathematical Formulations
- **Sectional Area Functions**: 
  $$S_1(x_1) = \left(1 - \frac{4x_1^2}{L^2}\right) \cdot A_1$$
  $$S_2(x_2) = \left(1 - \frac{4x_2^2}{L_2^2}\right) \cdot A_2$$

- **Force Integrals**:
  $$F_{surge} = \frac{\rho U^2}{2\pi} \int_{-L/2}^{L/2} dS_1(x_1) \cdot F(x_1, \xi, \eta) dx_1$$
  $$F_{sway} = \frac{\rho U^2 \eta}{\pi} \int_{-L/2}^{L/2} dS_1(x_1) \cdot G(x_1, \xi, \eta) dx_1$$
  $$M_{yaw} = \frac{\rho U^2 \eta}{\pi} \int_{-L/2}^{L/2} [dS_1(x_1) \cdot x_1 + S_1(x_1)] \cdot G(x_1, \xi, \eta) dx_1$$

### Performance Requirements
- Calculation completion within 100ms for single scenario
- Support batch processing of 1000+ scenarios
- Memory efficiency for large parametric studies
- Parallel processing capability using ProcessPoolExecutor
- Caching of intermediate results for repeated calculations

### UI/UX Specifications
- CLI interface with standard parameter naming (--input-directory, --output-directory)
- YAML configuration with validation and auto-completion support
- Interactive matplotlib plots with zoom, pan, and export capabilities
- Progress bars for batch calculations
- Clear error messages with suggested fixes

## Approach Options

### Option A: Direct NumPy Implementation
- **Pros**: 
  - Maximum performance with vectorized operations
  - Minimal dependencies
  - Full control over numerical methods
- **Cons**: 
  - More complex code for integration routines
  - Manual unit conversion handling

### Option B: SciPy Integration-Based (Selected)
- **Pros**: 
  - Robust numerical integration with adaptive quadrature
  - Built-in optimization for parametric studies
  - Proven stability for complex integrands
  - Better handling of singularities
- **Cons**: 
  - Additional dependency on SciPy
  - Slightly higher memory footprint

**Rationale:** SciPy's quad and dblquad functions provide superior numerical stability for the complex nested integrals in Wang's formulation, particularly when dealing with finite water depth corrections requiring summation over multiple harmonic terms.

## Configuration Schema

```yaml
# passing_ship_forces_config.yml
vessel:
  moored:
    length_bp: 950.0  # ft or m
    midship_area: 3192.0  # ft² or m²
    name: "Moored Vessel"
  
  passing:
    length_bp: 475.0  # Can use expressions like "0.5 * moored.length"
    midship_area: 6413.0
    velocity: 11.2  # ft/s or m/s
    name: "Passing Ship"

environment:
  water_density: 1.9905  # slug/ft³ or kg/m³
  water_depth: 95.0  # ft or m (or "0.1 * moored.length")
  
geometry:
  separation_distance: 190.0  # centerline to centerline
  stagger_distance: 0.0  # negative when passing ship behind

units:
  system: "imperial"  # or "SI"
  
calculation:
  integration_points: 1000
  harmonic_terms: 10  # for finite depth
  cache_results: true
  
output:
  format: ["json", "csv", "plots"]
  directory: "./results"
  prefix: "wang_forces"
```

## Module Structure

```
src/digitalmodel/modules/ship_design/passing_ship/
├── __init__.py
├── calculator.py         # Core calculation engine
├── configuration.py      # YAML parsing and validation
├── formulations.py       # Wang's mathematical formulations
├── integration.py        # Numerical integration routines
├── visualization.py      # Plotting and charts
├── units.py             # Unit conversion utilities
├── cli.py               # Command-line interface
└── templates/
    ├── basic_config.yml
    ├── tanker_config.yml
    └── offshore_config.yml
```

## External Dependencies

- **NumPy (>=1.24.0)** - Array operations and mathematical functions
  - **Justification:** Core numerical computing, already in project
  
- **SciPy (>=1.10.0)** - Numerical integration and optimization
  - **Justification:** Required for robust adaptive quadrature integration
  
- **Matplotlib (>=3.7.0)** - Visualization and plotting
  - **Justification:** Industry standard for engineering plots, already in project
  
- **PyYAML (>=6.0)** - Configuration file parsing
  - **Justification:** Consistent with project's YAML-driven approach
  
- **Pydantic (>=2.0)** - Configuration validation
  - **Justification:** Type safety and validation for complex configurations

## Integration Points

### With Existing Modules
- Extends `src/digitalmodel/modules/ship_design/` structure
- Inherits from `BaseCalculator` class if available
- Uses common configuration patterns from `digitalmodel.common`
- Integrates with existing parallel processing infrastructure

### Future Extensions
- OrcaFlex model generation for dynamic mooring analysis
- AQWA integration for validation studies
- Real-time monitoring interface
- Machine learning predictions for rapid assessment