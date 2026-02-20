# Stress Analysis Module Examples

## Overview

Examples for structural stress analysis, von Mises stress calculations, and plate capacity.

## Files

### Python Scripts

**`plate_capacity_examples.py`** - Plate capacity and stress analysis
- Plate buckling analysis
- Stiffened panel capacity
- Ultimate strength calculations
- Code check procedures

### Advanced Examples

**`stress_examples/`** - Detailed stress analysis workflows
- `simple_demo.py` - Basic stress analysis introduction
- `stress_analysis_demo.py` - Comprehensive stress analysis
- `stress_strain_example.py` - Stress-strain relationships
- `vm_stress_example.py` - Von Mises stress calculations
- `nonlinear_example.py` - Nonlinear stress analysis

## Usage

### Plate Capacity Analysis

```bash
python examples/domains/stress/plate_capacity_examples.py
```

### Von Mises Stress Example

```bash
python examples/domains/stress/stress_examples/vm_stress_example.py
```

### Nonlinear Analysis

```bash
python examples/domains/stress/stress_examples/nonlinear_example.py
```

## Features

- ✅ Linear elastic stress analysis
- ✅ Von Mises stress calculations
- ✅ Principal stress determination
- ✅ Plate buckling analysis
- ✅ Stiffened panel capacity
- ✅ Nonlinear material behavior
- ✅ Ultimate strength assessment

## Example Workflow

```python
from assetutilities.stress import StressAnalyzer

# Define material properties
steel = Material(
    E=210e9,        # Young's modulus (Pa)
    nu=0.3,         # Poisson's ratio
    fy=355e6        # Yield strength (Pa)
)

# Analyze plate capacity
plate = Plate(
    thickness=20,   # mm
    width=1000,     # mm
    length=2000,    # mm
    material=steel
)

# Calculate capacities
buckling_capacity = plate.buckling_capacity()
ultimate_capacity = plate.ultimate_capacity()

# Check utilization
utilization = applied_load / ultimate_capacity
```

## Standards Referenced

- DNV-RP-C201 (Buckling strength)
- API RP 2A-WSD (Fixed platforms)
- EN 1993-1-5 (Plated structures)
- AISC (Steel construction manual)

## Related Modules

- `../fatigue/` - Fatigue analysis
- `../api_standards/` - API code checks
