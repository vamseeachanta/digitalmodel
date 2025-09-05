# Documentation Corrections Needed

Based on verification testing, the following corrections are needed in the documentation:

## 1. API Usage Correction

### Current (Incorrect) Example in README.md:
```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship import PassingShipCalculator

# Initialize calculator with configuration
calculator = PassingShipCalculator('config.yaml')
```

### Correct Usage:
```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship import (
    PassingShipCalculator, VesselConfig, EnvironmentalConfig, CalculationConfig
)

# Create vessel configurations
moored_vessel = VesselConfig(
    length=180.0,
    beam=32.0,
    draft=12.0,
    block_coefficient=0.85,  # Required parameter
    displacement=45000.0
)

passing_vessel = VesselConfig(
    length=200.0,
    beam=35.0,
    draft=14.0,
    block_coefficient=0.85,  # Required parameter
    velocity=5.0
)

environment = EnvironmentalConfig(
    water_depth=50.0,
    water_density=1025.0,
    separation_distance=50.0
)

# Initialize calculator with configuration objects
calculator = PassingShipCalculator(
    moored_vessel=moored_vessel,
    passing_vessel=passing_vessel,
    environment=environment
)

# Calculate forces at specific stagger
results = calculator.calculate_forces(stagger=0)
```

## 2. Import Path Issues

The module is located at:
- `src/digitalmodel/modules/marine_analysis/python_code_passing_ship/`

But the module entry point (`__main__.py`) expects to be run differently than documented.

## 3. Function Name Corrections

### Formulations Module:
- `sectional_area_curve` → actual function is `s1_function` and `s2_function`
- Missing functions that were documented

### Visualization Module:
- `plot_force_distribution` → actual function is `plot_forces`
- `create_comparison_plot` → actual function is `create_comparison_plots`

### Exporters Module:
- `export_to_json` → needs to be verified/implemented
- `export_to_csv` → needs to be verified/implemented

## 4. Configuration Model Name
- `ShipConfiguration` → actual class is `PassingShipConfig`

## 5. Missing Required Parameters
- `VesselConfig` requires `block_coefficient` parameter (not documented)

## Recommendations:

1. **Update README.md** with correct API usage examples
2. **Update api_reference.md** with actual function names
3. **Add missing block_coefficient** to all configuration examples
4. **Verify module entry point** works as documented
5. **Either implement missing functions** or update documentation to match actual implementation