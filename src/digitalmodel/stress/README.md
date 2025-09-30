# Stress Analysis Module

This module provides comprehensive stress analysis capabilities for engineering applications, migrated and modernized from legacy VMStressCalculations code.

## Overview

The stress analysis module consists of three main components:

1. **Von Mises Stress Analysis** (`vm_stress.py`) - For pipe stress analysis under combined loading
2. **Stress-Strain Analysis** (`stress_strain.py`) - For material modeling and curve analysis
3. **Nonlinear Stress Analysis** (`nonlinear.py`) - For plasticity and nonlinear material behavior

## Features

### Von Mises Stress Analysis
- Pipe stress calculations under pressure, tension, and bending
- Combined loading analysis
- Tension-moment interaction envelopes
- Safety factor calculations
- Compatible with legacy VMStressCalculations code

### Stress-Strain Analysis
- Multiple material models: Linear elastic, Ramberg-Osgood, Bilinear
- Stress-strain curve generation and fitting
- Engineering properties calculation
- Model comparison capabilities
- Compatible with legacy stress-strain curve calculations

### Nonlinear Analysis
- Von Mises and Tresca yield criteria
- Isotropic and kinematic hardening models
- Incremental plasticity solutions
- Return mapping algorithms
- Plastic work calculations

## Installation

The module is part of the digitalmodel package. Optional dependencies:
- `scipy` - For advanced curve fitting and optimization (recommended)
- `matplotlib` - For plotting examples
- `numpy` - Required for numerical calculations

## Quick Start

### Von Mises Stress Analysis

```python
from digitalmodel.stress import PipeStressAnalyzer, PipeGeometry, MaterialProperties, LoadingCondition

# Define pipe geometry
geometry = PipeGeometry(
    outer_diameter=0.24765,  # m
    wall_thickness=0.034925  # m
)

# Define material properties
material = MaterialProperties(
    yield_strength=5.52e8,   # Pa
    ultimate_strength=6.20e8, # Pa
    elastic_modulus=2.1e11,  # Pa
    poisson_ratio=0.3
)

# Create analyzer
analyzer = PipeStressAnalyzer(geometry, material)

# Define loading
loading = LoadingCondition(
    internal_pressure=10e6,  # Pa
    axial_force=1e6,         # N
    bending_moment=1e5       # N⋅m
)

# Analyze combined stress
results = analyzer.calculate_combined_stress(loading)
print(f"Von Mises stress: {results['von_mises']/1e6:.1f} MPa")
print(f"Safety factor: {results['safety_factor']:.2f}")
```

### Stress-Strain Analysis

```python
from digitalmodel.stress import StressStrainAnalyzer, MaterialModel
import numpy as np

# Create analyzer
analyzer = StressStrainAnalyzer()

# Generate Ramberg-Osgood curve (legacy parameters)
strain_range = np.linspace(0, 0.1, 100)
curve = analyzer.generate_curve(
    MaterialModel.RAMBERG_OSGOOD,
    strain_range,
    elastic_modulus=2.12e8,  # Pa
    yield_strength=8.27e5,   # Pa
    k=0.002,
    n=18.85
)

# Calculate engineering properties
properties = analyzer.calculate_engineering_properties(curve)
print(f"Elastic modulus: {properties['elastic_modulus']/1e9:.1f} GPa")
```

### Nonlinear Analysis

```python
from digitalmodel.stress import NonlinearStressAnalyzer, VonMisesYield, LinearHardening
import numpy as np

# Material properties
elastic_modulus = 200e9  # Pa
yield_stress = 250e6     # Pa
hardening_modulus = 2e9  # Pa

# Create yield criterion and hardening model
yield_criterion = VonMisesYield(yield_stress)
hardening_model = LinearHardening(hardening_modulus)

# Create analyzer
analyzer = NonlinearStressAnalyzer(
    elastic_modulus, 0.3, yield_criterion, hardening_model
)

# Apply strain beyond yield
applied_strain = 0.005  # 0.5% strain
total_strain = np.array([applied_strain, 0, 0, 0, 0, 0])

# Solve nonlinear problem
solution = analyzer.solve_incremental_plasticity(total_strain, 10)
print(f"Final stress: {solution.final_stress[0]/1e6:.1f} MPa")
print(f"Plastic strain: {solution.plastic_strain_history[-1][0]*100:.3f}%")
```

## Examples

Complete examples are available in the `examples/stress/` directory:

- `vm_stress_example.py` - Von Mises stress analysis examples
- `stress_strain_example.py` - Stress-strain curve analysis
- `nonlinear_example.py` - Nonlinear plasticity analysis

Run examples:
```bash
python examples/stress/vm_stress_example.py
python examples/stress/stress_strain_example.py
python examples/stress/nonlinear_example.py
```

## API Reference

### Classes

#### PipeStressAnalyzer
Main class for pipe stress analysis under combined loading.

**Methods:**
- `calculate_pressure_stresses(internal_pressure, external_pressure=0)` - Pressure-induced stresses
- `calculate_combined_stress(loading)` - Combined loading analysis
- `calculate_maximum_tension(bending_moment)` - Maximum allowable tension
- `generate_interaction_envelope(moment_range)` - Tension-moment envelope

#### StressStrainAnalyzer
Main class for stress-strain curve analysis and material modeling.

**Methods:**
- `generate_curve(model, strain_range, **parameters)` - Generate stress-strain curve
- `fit_ramberg_osgood(experimental_data)` - Fit Ramberg-Osgood parameters
- `calculate_engineering_properties(curve)` - Calculate material properties
- `compare_models(experimental_data)` - Compare different models

#### NonlinearStressAnalyzer
Main class for nonlinear stress analysis with plasticity.

**Methods:**
- `solve_incremental_plasticity(total_strain, num_increments)` - Solve nonlinear problem
- `calculate_plastic_work(solution)` - Calculate plastic work
- `predict_failure(solution, failure_strain)` - Predict failure

### Material Models

- `MaterialModel.LINEAR_ELASTIC` - Linear elastic behavior
- `MaterialModel.RAMBERG_OSGOOD` - Ramberg-Osgood model
- `MaterialModel.BILINEAR` - Bilinear elastic-plastic model

### Yield Criteria

- `VonMisesYield` - Von Mises yield criterion
- `TrescaYield` - Tresca yield criterion

### Hardening Models

- `LinearHardening` - Linear hardening
- `IsotropicHardening` - Isotropic hardening with power law

## Legacy Code Compatibility

This module is designed to be compatible with legacy VMStressCalculations code:

- Same geometric calculations and formulations
- Compatible parameter sets and units
- Reproduces legacy stress-strain curve calculations
- Maintains engineering accuracy and standards

### Legacy Parameter Mapping

| Legacy Parameter | New Module | Description |
|------------------|------------|-------------|
| `pipeNominalOD_m` | `PipeGeometry.outer_diameter` | Outer diameter |
| `pipeNominalWT_m` | `PipeGeometry.wall_thickness` | Wall thickness |
| `pipeYieldStrength` | `MaterialProperties.yield_strength` | Yield strength |
| `AllowableStressFac` | `PipeStressAnalyzer.allowable_stress_factor` | Allowable stress factor |

## Testing

Run tests using pytest:
```bash
pytest tests/stress/
```

Test files:
- `test_vm_stress.py` - Von Mises stress analysis tests
- `test_stress_strain.py` - Stress-strain analysis tests
- `test_nonlinear.py` - Nonlinear analysis tests

## Engineering Standards

The module follows established engineering standards:

- ASME B31 for pipe stress analysis
- ASTM standards for material properties
- Classical plasticity theory for nonlinear analysis
- Von Mises and Tresca yield criteria
- Return mapping algorithms for plasticity

## Performance Notes

- All calculations use vectorized NumPy operations for efficiency
- Optional SciPy dependency for advanced optimization and fitting
- Incremental solution methods for nonlinear problems
- Robust convergence algorithms with error handling

## Limitations

- Assumes small strain theory for most calculations
- Linear elastic material behavior outside yield (except for specialized models)
- Isothermal conditions (temperature effects not included in stress calculations)
- Plane stress/strain assumptions where applicable

## Contributing

When contributing to this module:

1. Maintain compatibility with legacy calculations
2. Follow engineering accuracy standards
3. Include comprehensive tests for new features
4. Document all mathematical formulations
5. Provide examples for new functionality

## References

1. ASME Boiler and Pressure Vessel Code, Section VIII
2. Timoshenko, S.P. and Goodier, J.N., "Theory of Elasticity"
3. Hill, R., "The Mathematical Theory of Plasticity"
4. Ramberg, W. and Osgood, W.R., "Description of Stress-Strain Curves by Three Parameters"