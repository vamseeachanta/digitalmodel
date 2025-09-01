# Prompt Documentation

> Spec: Passing Ship Forces Calculation Module
> Created: 2025-01-01
> Purpose: Document the complete prompt history and provide reusable context

## Original User Request

```
/create-spec convert a mathcad pdf calculation into python code to integrate into the repository. The code should contain:
- input file for calculation configuration
- calculations in appropriate module
- charts etc.
PDF calculation: docs/modules/ship_design/passing_ship/Calculation of forces and moments from Wang.pdf
```

## Context Analysis

The user requested conversion of a MathCAD calculation document that implements Wang's methodology for calculating hydrodynamic forces on a moored vessel caused by a passing ship. The calculation determines:

1. **Surge Force** - Longitudinal force along vessel axis
2. **Sway Force** - Lateral force perpendicular to vessel
3. **Yaw Moment** - Turning moment about vertical axis

The MathCAD sheet includes:
- Vessel parameter inputs (length, cross-sectional area)
- Environmental conditions (water density, depth)
- Operational parameters (passing velocity, separation, stagger)
- Mathematical formulations for infinite and finite water depth
- Visualization plots showing force distributions

## Key Technical Requirements Identified

### From MathCAD Analysis:
- **Input Parameters:**
  - Moored vessel: Length (LBP), midship area
  - Passing vessel: Length, midship area, velocity
  - Geometry: Separation distance, stagger distance
  - Environment: Water density, water depth

- **Core Calculations:**
  - Sectional area curves: $S(x) = (1 - 4x^2/L^2) \cdot A$
  - Nested integrals for F and G kernel functions
  - Force formulations with $\rho U^2$ scaling
  - Finite depth corrections using harmonic summation

- **Expected Outputs:**
  - Forces in lbf (or N for SI)
  - Moments in ft-lbf (or N-m for SI)
  - Parametric plots vs. stagger distance

### From Repository Context:
- Integrate with existing `ship_design` module structure
- Follow YAML configuration pattern
- Use established parallel processing infrastructure
- Maintain compatibility with OrcaFlex/AQWA modules
- Apply repository coding standards and patterns

## Design Decisions Made

1. **Module Placement**: Extend existing `ship_design` module rather than create new top-level module
2. **Calculation Approach**: Use SciPy for robust numerical integration (adaptive quadrature)
3. **Configuration**: YAML-driven with Pydantic validation following repository patterns
4. **Performance**: Leverage ProcessPoolExecutor for batch calculations
5. **Testing**: Real calculations only (no mocks) per repository policy
6. **Visualization**: Interactive matplotlib plots with standard export formats

## Reusable Prompt for Future Enhancements

```
I need to enhance the passing ship forces calculation module located at 
src/digitalmodel/modules/ship_design/passing_ship/. 

Current capabilities:
- Wang's formulation for ship-to-ship interaction forces
- YAML configuration with vessel parameters
- Batch processing with parallel execution
- Matplotlib visualizations

The module implements the calculations from Wang's paper as documented in 
docs/modules/ship_design/passing_ship/Calculation of forces and moments from Wang.pdf

Please [describe enhancement needed], ensuring:
1. Maintain compatibility with existing YAML configuration structure
2. Follow established patterns in ship_design module
3. Use real calculations (no mocks) per repository policy
4. Include comprehensive tests validated against MathCAD reference
5. Update CLI interface with standard parameter naming
```

## Implementation Notes

### Critical Accuracy Requirements:
- Match MathCAD reference values within 0.1% tolerance
- Handle singularities at zero separation gracefully
- Ensure finite depth corrections converge properly

### Integration Considerations:
- Can extend to generate OrcaFlex constraint forces
- May integrate with mooring analysis module
- Potential for real-time monitoring dashboard

### Performance Targets:
- Single calculation: <100ms
- Batch of 1000: <30s with parallelization
- Memory usage: <50MB per calculation

## Agent Delegation Strategy

For implementation, utilize:
1. **Ship Design Agent** (primary) - Domain expertise and standards
2. **Testing Agent** - Parallel test execution
3. **Documentation Agent** - API documentation generation
4. **Visualization Agent** - Chart and plot generation

Each agent should be aware of the mathematical formulations and accuracy requirements documented here.