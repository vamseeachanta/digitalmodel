# VIV Analysis Module

**Version**: 1.0.0
**Status**: ✅ Production Ready
**Standards**: DNV-RP-C205, DNV-RP-F105, API RP 2A, DNV-RP-C203

## Overview

Vortex-Induced Vibration (VIV) analysis for risers and tubular members including natural frequency calculation, vortex shedding analysis, susceptibility screening, and fatigue assessment.

## Core Features Implemented

✅ **Natural Frequency Calculator** (`frequency_calculator.py`)
- Beam vibration equations for multiple boundary conditions
- Pinned-pinned, fixed-fixed, fixed-pinned, cantilever, tension-controlled
- Added mass effects from surrounding fluid
- Multiple mode calculation (1st, 2nd, 3rd, etc.)

✅ **Vortex Shedding Analyzer** (`vortex_shedding.py`)
- Strouhal number relationship: f_s = St * V / D
- Current profile handling (uniform, power law, custom)
- Reynolds number calculations

✅ **VIV Susceptibility Screening** (`screening.py`)
- Reduced velocity calculation: V_r = V / (f_n * D)
- Lock-in detection (V_r = 4-8 range)
- Safety factor evaluation per DNV-RP-C205

✅ **VIV Fatigue Calculator** (`fatigue.py`)
- Stress range from VIV amplitude
- S-N curve methodology (DNV curves)
- Cumulative damage calculation

## Quick Start

### CLI Usage

```bash
# Natural frequency calculation
viv-analysis natural-freq \
    --length 50 \
    --diameter 0.5 \
    --thickness 0.025 \
    --material steel \
    --boundary pinned-pinned \
    --n-modes 5

# VIV susceptibility screening
viv-analysis screening \
    --length 50 \
    --diameter 0.5 \
    --thickness 0.025 \
    --current 1.5 \
    --material steel \
    --output screening_result.json
```

### Python API

```python
from digitalmodel.viv_analysis import (
    TubularMember,
    MaterialProperties,
    BoundaryCondition,
    FrequencyCalculator,
    VIVScreening,
    STEEL_CARBON
)

# Create tubular member
member = TubularMember(
    name="Riser1",
    length=50.0,
    outer_diameter=0.5,
    wall_thickness=0.025,
    material=STEEL_CARBON,
    boundary_condition=BoundaryCondition.PINNED_PINNED
)

# Calculate natural frequencies
freq_calc = FrequencyCalculator()
results = freq_calc.calculate_multiple_modes(member, n_modes=5)

for result in results:
    print(f"Mode {result.mode_number}: {result.frequency:.4f} Hz")

# VIV screening
screening = VIVScreening()
viv_result = screening.screen_member(member, current_velocity=1.5, mode=1)

print(f"Reduced Velocity: {viv_result.reduced_velocity:.2f}")
print(f"Lock-in Status: {viv_result.lock_in_status}")
print(f"Safety Factor: {viv_result.safety_factor:.2f}")
print(f"Recommendation: {viv_result.recommendation}")
```

## Module Structure

```
viv_analysis/
├── __init__.py              # Module exports
├── models.py                # Data models and constants
├── frequency_calculator.py  # Natural frequency calculations
├── vortex_shedding.py      # Vortex shedding analyzer
├── screening.py            # VIV susceptibility screening
├── fatigue.py              # VIV fatigue damage
├── cli.py                  # Command-line interface
└── README.md               # This file
```

## Materials Available

- `steel` / `steel_carbon` - Carbon steel (E=207 GPa, ρ=7850 kg/m³)
- `steel_stainless` - Stainless steel (E=193 GPa, ρ=8000 kg/m³)
- `titanium` - Titanium (E=110 GPa, ρ=4500 kg/m³)

## Boundary Conditions

- `pinned-pinned` - Simply supported
- `fixed-fixed` - Clamped ends
- `fixed-pinned` - One clamped, one pinned
- `cantilever` - Fixed-free
- `tension-controlled` - For risers with axial tension

## Standards Compliance

- **DNV-RP-C205**: Environmental Conditions and Environmental Loads
- **DNV-RP-F105**: Free Spanning Pipelines
- **API RP 2A**: Fixed Offshore Platforms
- **DNV-RP-C203**: Fatigue Design of Offshore Steel Structures

## Status

**Implementation**: ✅ **Production Ready**
- All analysis classes functional (~1,900 lines)
- CLI with 2 commands operational
- Python API fully accessible
- Comprehensive unit tests (41 test methods, >92% coverage)
- CLI integration tests (18 test methods)
- Complete documentation (README + implementation summary)
- GitHub Actions CI/CD workflow (Ubuntu + Windows, Python 3.10/3.11)

## Contributing

The VIV Analysis Module is production-ready. Additional contributions welcome for:
1. Multi-mode VIV analysis
2. VIV suppression device modeling (strakes, fairings)
3. Reynolds-dependent Strouhal numbers
4. Time-domain VIV response prediction
5. Integration with CFD results
6. Advanced current profile models

---

**Maintained by**: Digital Model Development Team
**Last Updated**: 2026-01-04
**Version**: 1.0.0
