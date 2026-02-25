# OCIMF Module Examples

## Overview

Examples for OCIMF (Oil Companies International Marine Forum) standards and guidelines.

## Files

### Python Scripts

**`ocimf_demo.py`** - OCIMF guidelines demonstration
- Mooring equipment sizing per MEG4
- Single Point Mooring (SPM) design per SMOG
- Hawser and mooring line selection
- Tanker mooring analysis

### Jupyter Notebooks

**`ocimf_visualization_example.ipynb`** - Interactive OCIMF analysis
- MEG4 equipment sizing workflow
- Mooring pattern visualization
- Load distribution analysis
- Equipment selection charts

## Usage

### Run OCIMF Demo

```bash
python examples/domains/ocimf/ocimf_demo.py
```

### Run Interactive Notebook

```bash
jupyter notebook examples/domains/ocimf/ocimf_visualization_example.ipynb
```

## Features

- ✅ MEG4 equipment sizing
- ✅ SMOG SPM operations
- ✅ Mooring line selection
- ✅ Hawser design
- ✅ Bollard pull calculations
- ✅ Fendering design
- ✅ Environmental criteria

## OCIMF Standards

### MEG4 (Mooring Equipment Guidelines 4th Edition)
- Mooring line sizing
- Winch capacity requirements
- Fairlead design
- Deck equipment specifications

### SMOG (Single Point Mooring Operations Guide)
- SPM system design
- CALM buoy operations
- Tanker approach procedures
- Emergency disconnect

## Example Workflow

```python
from assetutilities.ocimf import MEG4, SMOG

# Size mooring equipment per MEG4
tanker = Tanker(dwt=300000, type='VLCC')
environment = Environment(wave_hs=2.5, wind_speed=15)

meg4 = MEG4(tanker=tanker, environment=environment)
equipment = meg4.size_equipment()

print(f"Mooring lines: {equipment.num_lines}")
print(f"Line MBL: {equipment.line_mbl} kN")
print(f"Winch capacity: {equipment.winch_capacity} kN")

# SPM design per SMOG
spm = SMOG(
    buoy_type='CALM',
    water_depth=120,
    tanker=tanker
)

design = spm.design_system()
```

## Related Modules

- `../calm_buoy/` - CALM buoy SPM systems
- `../fpso/` - FPSO mooring
- `../mooring/` - General mooring design

## Standards Referenced

- OCIMF MEG4 (Mooring Equipment Guidelines)
- OCIMF SMOG (Single Point Mooring Operations)
- OCIMF OVID (Offshore Vessel Inspection Database)
