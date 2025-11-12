# FPSO Mooring Module Examples

## Overview

Examples for FPSO (Floating Production Storage and Offloading) mooring analysis.

## Files

### Python Scripts

**`fpso_mooring_analysis.py`** - Comprehensive FPSO mooring analysis
- Spread mooring system design
- Static and dynamic analysis
- Mooring line tension calculations
- Offset analysis under environmental loads
- Export to various formats

### Jupyter Notebooks

**`fpso_mooring_analysis.ipynb`** - Interactive FPSO analysis notebook
- Step-by-step mooring design workflow
- Visualizations of mooring patterns
- Environmental load calculations
- Results visualization and reporting

## Usage

### Run Python Script

```bash
# From repository root
python examples/modules/fpso/fpso_mooring_analysis.py
```

### Run Jupyter Notebook

```bash
# Start Jupyter
jupyter notebook examples/modules/fpso/fpso_mooring_analysis.ipynb
```

## Features

- ✅ Spread mooring pattern (8-12 lines)
- ✅ Catenary mooring line analysis
- ✅ Environmental load calculations
- ✅ Static equilibrium analysis
- ✅ Dynamic response simulation
- ✅ Offset and excursion calculations
- ✅ Mooring line tension monitoring

## Example Output

```python
# Mooring system configuration
fpso = FPSO(
    length=330,
    beam=60,
    draft=22,
    displacement=300000
)

# Design spread mooring
mooring_system = design_spread_mooring(
    fpso=fpso,
    water_depth=1500,
    number_of_lines=12,
    chain_type="R4"
)

# Analyze under environmental loads
results = analyze_mooring_system(
    mooring_system=mooring_system,
    wave_hs=8.5,
    wind_speed=35,
    current_speed=2.0
)
```

## Related Modules

- `../mooring/` - Mooring line design
- `../calm_buoy/` - CALM buoy systems
- `../hydrodynamics/` - Hydrodynamic analysis

## Documentation

- `README_fpso_analysis.md` - Detailed FPSO analysis guide
- `QUICK_START_FPSO.md` - Quick start tutorial

## Standards Referenced

- API RP 2SK (Mooring design)
- API RP 2SM (Spread mooring)
- DNV-OS-E301 (Position mooring)
