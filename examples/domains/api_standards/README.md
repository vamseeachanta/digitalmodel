# API Standards Module Examples

## Overview

Examples implementing API (American Petroleum Institute) standards for offshore structures.

## Files

### Python Scripts

**`apistd2rd_demo.py`** - API Std 2RD pipeline design demonstration
- Pipeline design per API Std 2RD
- Wall thickness calculations
- Pressure containment checks
- Installation design
- Hydrostatic testing

## Usage

### Run API 2RD Demo

```bash
python examples/domains/api_standards/apistd2rd_demo.py
```

## Input Files

**`../input_files/api_std_2rd/`** - Sample pipeline configurations
- `pipeline_example_basic.yml` - Basic pipeline design input

## Features

- ✅ API Std 2RD pipeline design
- ✅ Wall thickness calculations
- ✅ Pressure rating checks
- ✅ Material selection
- ✅ Corrosion allowances
- ✅ Installation loads
- ✅ Code compliance checking

## Example Workflow

```python
from assetutilities.api_standards import API2RD

# Define pipeline
pipeline = API2RD(
    diameter=16,            # inches
    design_pressure=1500,   # psi
    material='API 5L X65',
    environment='seawater'
)

# Calculate required wall thickness
t_req = pipeline.wall_thickness_required()

# Check pressure rating
pressure_rating = pipeline.pressure_rating(wall_thickness=t_req)

# Verify code compliance
compliance = pipeline.check_compliance()
```

## API Standards Covered

- **API Std 2RD** - Dynamic Risers for Floating Production Systems
- **API RP 2SK** - Mooring Design (see ../mooring/)
- **API RP 2SM** - Spread Mooring (see ../fpso/)
- **API RP 2A-WSD** - Fixed Platforms (see ../stress/)

## Related Modules

- `../reservoir/` - Reservoir and pipeline flow
- `../stress/` - Structural analysis
- `../mooring/` - Mooring design per API RP 2SK

## Standards Referenced

- API Std 2RD (Dynamic risers)
- API 5L (Line pipe specifications)
- API RP 1111 (Pipeline design)
