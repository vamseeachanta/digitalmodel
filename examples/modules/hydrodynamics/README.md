# Hydrodynamics Module Examples

## Overview

Examples for hydrodynamic analysis, wave loads, and hydrodynamic coefficient calculations.

## Files

### Python Scripts

**`generate_hydro_charts.py`** - Generate hydrodynamic coefficient charts
- RAO (Response Amplitude Operator) generation
- Added mass and damping coefficients
- Wave drift force calculations
- QTF (Quadratic Transfer Function) visualization

### Jupyter Notebooks

**`hydro_coefficients_example.ipynb`** - Interactive hydrodynamic analysis
- Step-by-step hydrodynamic coefficient calculation
- Visualization of frequency-dependent properties
- Wave force calculations
- Motion response predictions

## Usage

### Generate Hydrodynamic Charts

```bash
python examples/modules/hydrodynamics/generate_hydro_charts.py
```

### Run Interactive Notebook

```bash
jupyter notebook examples/modules/hydrodynamics/hydro_coefficients_example.ipynb
```

## Features

- ✅ RAO database management
- ✅ Added mass matrices (frequency-dependent)
- ✅ Radiation damping coefficients
- ✅ Wave drift forces (mean and slow-drift)
- ✅ Hydrodynamic pressure integration
- ✅ Motion predictions (6 DOF)

## Key Concepts

### Response Amplitude Operators (RAOs)

```python
# Example RAO generation
rao_data = {
    'surge': generate_surge_rao(periods, wave_directions),
    'heave': generate_heave_rao(periods, wave_directions),
    'pitch': generate_pitch_rao(periods, wave_directions)
}

# Visualize
plot_rao_polar(rao_data)
```

### Hydrodynamic Coefficients

```
A(ω) = Added mass (frequency-dependent)
B(ω) = Damping coefficient (frequency-dependent)
C = Hydrostatic stiffness
F(ω) = Wave excitation force
```

## Related Modules

- `../calm_buoy/` - CALM buoy hydrodynamics
- `../fpso/` - FPSO hydrodynamic analysis
- `../orcaflex/` - OrcaFlex hydrodynamic input

## Standards Referenced

- DNV-RP-C205 (Environmental conditions)
- ISO 19901-1 (Metocean design)
- ITTC (Hydrodynamic testing procedures)
