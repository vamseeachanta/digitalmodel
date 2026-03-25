# OCIMF Environmental Loading Module

## Overview

The OCIMF Environmental Loading module provides comprehensive tools for calculating wind and current forces on vessels using OCIMF (Oil Companies International Marine Forum) coefficients.

## Features

- **Coefficient Database Management**: Load and manage OCIMF coefficient databases with 186+ vessel entries
- **2D Interpolation**: Smooth interpolation of coefficients based on heading and displacement using RBF (Radial Basis Function) methods
- **Force Calculations**: Calculate wind and current forces and moments on vessels
- **Comprehensive Visualizations**:
  - 3D surface plots of coefficient distributions
  - Polar diagrams for force coefficients
  - Force vector diagrams
  - Validation charts comparing calculations
  - Interpolation quality heatmaps

## Installation

The module is part of the `marine_engineering` package. Ensure all dependencies are installed:

```bash
pip install numpy pandas scipy matplotlib seaborn
# Optional for interactive 3D plots:
pip install plotly
```

## Quick Start

### 1. Basic Usage

```python
from marine_engineering.environmental_loading import (
    OCIMFDatabase,
    EnvironmentalForces,
    EnvironmentalConditions,
    VesselGeometry,
)

# Load OCIMF database
db = OCIMFDatabase('data/ocimf/ocimf_coefficients.csv')

# Get coefficients for specific heading and displacement
coeffs = db.get_coefficients(heading=45, displacement=250000)
print(f"CYw (lateral wind): {coeffs.CYw:.3f}")

# Calculate environmental forces
calc = EnvironmentalForces(db)

conditions = EnvironmentalConditions(
    wind_speed=20.0,  # m/s
    wind_direction=45.0,  # degrees
    current_speed=1.5,  # m/s
    current_direction=30.0  # degrees
)

geometry = VesselGeometry(
    loa=330.0,  # meters
    beam=60.0,  # meters
    draft=22.0  # meters
)

results = calc.calculate_total_forces(conditions, geometry, displacement=250000)
print(f"Total lateral force: {results.total_fy/1000:.1f} kN")
```

### 2. Generate Visualizations

```python
# 3D surface plot of coefficient
db.plot_coefficient_surface(
    'CYw',
    save_path='outputs/surface_CYw.png',
    interactive=False  # Set True for plotly interactive plot
)

# Polar diagram
db.plot_polar_diagram(
    displacement=250000,
    save_path='outputs/polar_250000.png'
)

# Force vector diagram
calc.plot_force_diagram(
    results,
    save_path='outputs/force_diagram.png'
)

# Interpolation quality heatmap
db.plot_interpolation_quality(
    coefficient_name='CYw',
    save_path='outputs/interp_quality.png'
)
```

### 3. Validation Against Excel

```python
# Compare Python calculations with Excel reference
excel_results = {
    'wind_fx': 150000,  # N
    'wind_fy': 850000,  # N
    'wind_mz': 58000000,  # N·m
    'current_fx': 2000000,  # N
    'current_fy': 9000000,  # N
    'current_mz': 620000000  # N·m
}

calc.plot_validation_chart(
    excel_results,
    results,
    save_path='outputs/validation.png'
)
```

## API Reference

### Classes

#### `OCIMFCoefficients`
Dataclass containing OCIMF coefficients:
- `CXw`: Wind force coefficient (longitudinal)
- `CYw`: Wind force coefficient (lateral)
- `CMw`: Wind moment coefficient
- `CXc`: Current force coefficient (longitudinal)
- `CYc`: Current force coefficient (lateral)
- `CMc`: Current moment coefficient

#### `OCIMFDatabase`
Database manager with interpolation capabilities.

**Methods:**
- `get_coefficients(heading, displacement, vessel_type=None)`: Get interpolated coefficients
- `plot_coefficient_surface(coefficient_name, save_path=None, interactive=False)`: 3D surface plot
- `plot_polar_diagram(displacement, save_path=None)`: Polar force diagram
- `plot_interpolation_quality(coefficient_name, save_path=None)`: Interpolation heatmap

#### `EnvironmentalForces`
Force calculation engine.

**Methods:**
- `calculate_wind_forces(conditions, geometry, displacement)`: Wind forces only
- `calculate_current_forces(conditions, geometry, displacement)`: Current forces only
- `calculate_total_forces(conditions, geometry, displacement)`: Total forces (wind + current)
- `plot_force_diagram(results, save_path=None)`: Force vector visualization
- `plot_validation_chart(excel_results, python_results, save_path=None)`: Comparison chart

#### `EnvironmentalConditions`
Environmental parameters:
- `wind_speed`: Wind speed (m/s)
- `wind_direction`: Wind direction (degrees, relative to vessel)
- `current_speed`: Current speed (m/s)
- `current_direction`: Current direction (degrees, relative to vessel)
- `air_density`: Air density (kg/m³, default=1.225)
- `water_density`: Water density (kg/m³, default=1025.0)

#### `VesselGeometry`
Vessel geometry parameters:
- `loa`: Length overall (m)
- `beam`: Beam (m)
- `draft`: Draft (m)
- `frontal_area_wind`: Projected frontal area for wind (m², auto-calculated if not provided)
- `lateral_area_wind`: Projected lateral area for wind (m², auto-calculated if not provided)
- `frontal_area_current`: Projected frontal area for current (m², auto-calculated if not provided)
- `lateral_area_current`: Projected lateral area for current (m², auto-calculated if not provided)

#### `EnvironmentalForceResults`
Force calculation results containing all force components and inputs.

## Examples

### Example 1: Create Sample Database

```python
from marine_engineering.environmental_loading import create_sample_database

create_sample_database(
    'data/ocimf/sample_db.csv',
    num_vessels=5,
    num_headings=13,
    num_displacements=3
)
```

### Example 2: Parametric Study

```python
import numpy as np
import matplotlib.pyplot as plt

# Study effect of wind speed
wind_speeds = np.linspace(10, 40, 15)
lateral_forces = []

for ws in wind_speeds:
    conditions = EnvironmentalConditions(
        wind_speed=ws,
        wind_direction=90,  # Beam wind
        current_speed=0,
        current_direction=0
    )
    results = calc.calculate_total_forces(conditions, geometry, 250000)
    lateral_forces.append(results.wind_fy / 1000)  # kN

plt.plot(wind_speeds, lateral_forces)
plt.xlabel('Wind Speed (m/s)')
plt.ylabel('Lateral Force (kN)')
plt.title('Wind Force vs Wind Speed (Beam Wind)')
plt.grid(True)
plt.show()
```

### Example 3: Multi-Vessel Comparison

```python
vessels = [
    {'name': 'VLCC', 'loa': 330, 'beam': 60, 'draft': 22, 'disp': 300000},
    {'name': 'Suezmax', 'loa': 275, 'beam': 48, 'draft': 16, 'disp': 180000},
    {'name': 'Aframax', 'loa': 245, 'beam': 42, 'draft': 14, 'disp': 120000},
]

for vessel in vessels:
    geom = VesselGeometry(loa=vessel['loa'], beam=vessel['beam'],
                         draft=vessel['draft'])
    res = calc.calculate_total_forces(conditions, geom, vessel['disp'])
    print(f"{vessel['name']}: Total Force = {res.total_fy/1000:.0f} kN")
```

## Testing

Run the comprehensive test suite:

```bash
# Run all tests
pytest tests/marine_engineering/environmental_loading/ -v

# Run with coverage
pytest tests/marine_engineering/environmental_loading/ --cov=marine_engineering.environmental_loading

# Run benchmarks only
pytest tests/marine_engineering/environmental_loading/ -m benchmark

# Run slow tests (chart generation)
pytest tests/marine_engineering/environmental_loading/ -m slow
```

## Demo Scripts

### Command Line Demo

```bash
python examples/ocimf_demo.py
```

This generates:
- 3D surface plots (CXw, CYw, CMw)
- Polar diagrams (250k and 300k tonnes)
- Interpolation quality heatmap
- Force vector diagram
- Validation chart

All outputs saved to: `examples/outputs/ocimf_demo/`

### Jupyter Notebook

```bash
jupyter notebook examples/ocimf_visualization_example.ipynb
```

Interactive notebook demonstrating:
- Database loading and management
- Coefficient interpolation
- All visualization types
- Parametric studies
- Multi-vessel comparisons

## CSV Database Format

OCIMF coefficient database CSV should have the following columns:

```csv
vessel_type,displacement,heading,CXw,CYw,CMw,CXc,CYc,CMc,vessel_name,loa,beam,draft
VLCC,250000,0,0.85,0.00,0.00,0.95,0.00,0.00,Sample VLCC 1,330,60,22
VLCC,250000,30,0.75,0.35,0.05,0.85,0.40,0.08,Sample VLCC 1,330,60,22
...
```

**Required columns:**
- `displacement`: Vessel displacement (tonnes)
- `heading`: Heading angle (degrees, 0-180)
- `CXw, CYw, CMw`: Wind coefficients
- `CXc, CYc, CMc`: Current coefficients

**Optional columns:**
- `vessel_type`: Vessel classification
- `vessel_name`: Vessel identifier
- `loa, beam, draft`: Vessel dimensions

## Theory and References

### Force Calculation Formulas

Wind forces:
```
Fx_wind = 0.5 × ρ_air × V_wind² × A_frontal × CXw
Fy_wind = 0.5 × ρ_air × V_wind² × A_lateral × CYw
Mz_wind = 0.5 × ρ_air × V_wind² × A_lateral × LOA × CMw
```

Current forces:
```
Fx_current = 0.5 × ρ_water × V_current² × A_frontal × CXc
Fy_current = 0.5 × ρ_water × V_current² × A_lateral × CYc
Mz_current = 0.5 × ρ_water × V_current² × A_lateral × LOA × CMc
```

### Interpolation Method

The module uses RBF (Radial Basis Function) interpolation with thin-plate spline kernel for smooth coefficient surfaces. This provides:
- C² continuity
- Exact interpolation at database points
- Smooth extrapolation (within reason)

### Coordinate System

- **Heading**: 0° = head-on, 90° = beam, 180° = stern-on
- **Forces**: Positive Fx = forward, Positive Fy = port
- **Moment**: Positive Mz = bow to port (yaw)

### References

1. OCIMF "Prediction of Wind and Current Loads on VLCCs" (1994)
2. OCIMF "Mooring Equipment Guidelines" (MEG4, 2018)
3. API RP 2SK "Design and Analysis of Stationkeeping Systems for Floating Structures"

## Performance

- Database loading: ~50ms for 195 entries
- Coefficient interpolation: ~1-2ms per query
- Force calculation: ~2-3ms
- Chart generation: 1-3 seconds per chart

## Limitations and Assumptions

1. **Port/Starboard Symmetry**: Headings are normalized to 0-180° assuming symmetric vessel
2. **Projected Areas**: Auto-calculated areas are simplified estimates. Provide actual values for accurate results
3. **Interpolation Bounds**: Extrapolation outside database range triggers warnings
4. **Static Analysis**: Does not account for vessel dynamics or motion
5. **Wind Profile**: Assumes uniform wind speed (no height variation)

## Troubleshooting

### Issue: Interpolation warnings

**Cause**: Requesting coefficients outside database range

**Solution**: Check heading/displacement bounds:
```python
print(f"Heading range: {db.headings.min()} - {db.headings.max()}°")
print(f"Displacement range: {db.displacements.min()} - {db.displacements.max()}t")
```

### Issue: Large force values

**Cause**: Incorrect units or projected areas

**Solution**:
- Ensure wind speed in m/s (not knots)
- Verify projected areas are in m² (not cm² or ft²)
- Check coefficient magnitudes (typically 0-1.5)

### Issue: Charts not generating

**Cause**: Missing matplotlib backend or directory

**Solution**:
```python
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend
from pathlib import Path
Path('outputs').mkdir(exist_ok=True)
```

## Contributing

To add new features:

1. Add methods to appropriate class (OCIMFDatabase or EnvironmentalForces)
2. Include comprehensive docstrings
3. Add unit tests in `tests/marine_engineering/environmental_loading/`
4. Update this README with examples

## License

MIT License - see main project LICENSE file

## Contact

For questions or issues, please open an issue on the project repository.

---

**Last Updated**: 2025-10-03
**Version**: 0.1.0
**Module**: `marine_engineering.environmental_loading`
