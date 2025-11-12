# Reservoir Analysis Module Examples

## Overview

Examples for reservoir engineering, production analysis, and field development.

## Files

### Python Scripts

**`reservoir_analysis_examples.py`** - Comprehensive reservoir analysis
- Production forecasting
- Decline curve analysis
- Material balance calculations
- Well performance analysis
- Field development optimization

## Input Files

**`../input_files/reservoir_analysis/`** - Sample field configurations
- `field_example_basic.yml` - Basic field development input

## Usage

### Run Reservoir Analysis

```bash
python examples/modules/reservoir/reservoir_analysis_examples.py
```

## Features

- ✅ Production forecasting
- ✅ Decline curve analysis (Arps)
- ✅ Material balance calculations
- ✅ Well performance curves
- ✅ PVT analysis
- ✅ Recovery factor estimation
- ✅ Field development planning

## Example Workflow

```python
from assetutilities.reservoir import ReservoirAnalyzer

# Define reservoir properties
reservoir = Reservoir(
    initial_pressure=4000,      # psi
    temperature=180,            # °F
    porosity=0.25,
    permeability=150,           # mD
    thickness=100,              # ft
    oil_gravity=35              # °API
)

# Production forecast
analyzer = ReservoirAnalyzer(reservoir)
forecast = analyzer.production_forecast(
    years=20,
    decline_rate=0.15,
    decline_type='exponential'
)

# Plot results
analyzer.plot_forecast(forecast)
```

## Analysis Types

### Decline Curve Analysis
- Exponential decline
- Hyperbolic decline
- Harmonic decline
- Type curves

### Material Balance
- Tank model
- Gas cap expansion
- Water influx
- Compaction drive

### Well Performance
- Inflow performance (IPR)
- Productivity index
- Skin factor
- Multiphase flow

## Related Modules

- `../api_standards/` - Pipeline design for production
- Integration with surface facilities

## Standards Referenced

- SPE (Society of Petroleum Engineers) guidelines
- API reservoir engineering standards
- Classic reservoir engineering (Craft & Hawkins)
