# Structural Analysis Module

**Version**: 1.0.0
**Status**: ✅ Production Ready
**Standards**: DNV-RP-C201, Eurocode 3, API RP 2A, ISO 19902

## Overview

Comprehensive structural analysis for offshore and marine structures including stress calculations, buckling checks, and capacity verification per international standards.

## Features

### Stress Calculations
- **Von Mises stress** - Equivalent stress for yield criterion
- **Principal stresses** - σ1, σ2, σ3 calculation
- **Maximum shear stress** - Tresca criterion
- **Beam bending stress** - Combined axial and bending
- **Pressure vessel stress** - Hoop and longitudinal
- **Torsional stress** - Circular and non-circular sections

### Buckling Analysis
- **Plate buckling** (DNV-RP-C201)
  - Elastic buckling stress
  - Johnson-Ostenfeld inelastic correction
  - Combined compression and shear
  - Multiple boundary conditions

- **Column buckling** (Eurocode 3)
  - Euler critical load
  - EC3 reduction factors
  - Buckling curves (a0, a, b, c, d)
  - Slenderness classification

### Capacity Verification
- **Tension capacity** - Gross section and net section checks
- **Combined loading** - Axial force + biaxial bending
- **Interaction formulas** - EC3 interaction checks
- **Safety factors** - Code-compliant partial factors

## Installation

The module is part of the digitalmodel package:

```bash
# Install from repository
uv pip install -e .

# Verify CLI registration
structural-analysis --version
```

## Quick Start

### CLI Usage

```bash
# Calculate Von Mises stress
structural-analysis stress \
    --sigma-x 150 \
    --sigma-y 50 \
    --tau-xy 30 \
    --material S355

# Check plate buckling
structural-analysis buckling-plate \
    --length 2000 \
    --width 1000 \
    --thickness 20 \
    --sigma-x 150 \
    --tau 30 \
    --material S355

# Check column buckling
structural-analysis buckling-column \
    --axial-force 2500000 \
    --area 15000 \
    --I-min 50000000 \
    --L-eff 8000 \
    --curve b \
    --material S355

# Check tension capacity
structural-analysis capacity \
    --tension \
    --axial-force 4000000 \
    --area-gross 15000 \
    --area-net 14000 \
    --material S355

# List available materials
structural-analysis list-materials
```

### Python API

```python
from digitalmodel.structural_analysis import (
    StressState,
    PlateBucklingAnalyzer,
    ColumnBucklingAnalyzer,
    MemberCapacityChecker,
    PlateGeometry,
    STEEL_S355
)

# Stress analysis
stress = StressState(sigma_x=150.0, sigma_y=50.0, tau_xy=30.0)
von_mises = stress.von_mises()
principals = stress.principal_stresses()
print(f"Von Mises: {von_mises:.2f} MPa")
print(f"Principals: σ1={principals[0]:.2f}, σ2={principals[1]:.2f}, σ3={principals[2]:.2f}")

# Plate buckling
analyzer = PlateBucklingAnalyzer(STEEL_S355)
plate = PlateGeometry(length=2000, width=1000, thickness=20)
result = analyzer.check_plate_buckling(
    plate=plate,
    sigma_x=150.0,
    tau=30.0,
    gamma_m=1.15
)
print(f"Utilization: {result.utilization:.2%}")
print(f"Status: {'PASS' if result.passes else 'FAIL'}")

# Column buckling
analyzer = ColumnBucklingAnalyzer(STEEL_S355)
result = analyzer.check_column_buckling(
    axial_force=2.5e6,
    area=15000,
    I_min=5e7,
    L_eff=8000,
    buckling_curve='b'
)
print(f"Safety Factor: {result.safety_factor:.2f}")

# Member capacity
checker = MemberCapacityChecker(STEEL_S355)
result = checker.check_tension_member(
    axial_force=4e6,
    area_gross=15000,
    area_net=14000
)
print(f"Capacity: {result.capacity/1e6:.2f} MN")
print(f"Governing: {result.governing_mode}")
```

## Available Materials

| Grade | Yield (MPa) | Ultimate (MPa) | E (MPa) | Application |
|-------|-------------|----------------|---------|-------------|
| **S275** | 275 | 430 | 210000 | General structures |
| **S355** | 355 | 510 | 210000 | Offshore platforms |
| **S420** | 420 | 520 | 210000 | High-strength applications |

## Standards Compliance

### DNV-RP-C201 (Plate Buckling)
- Elastic buckling stress calculation
- Johnson-Ostenfeld inelastic correction
- Combined loading interaction
- Material safety factors γM = 1.15

### Eurocode 3 (Steel Structures)
- Column buckling reduction factors
- Buckling curves for various cross-sections
- Member capacity verification
- Partial safety factors

### API RP 2A / ISO 19902
- Tubular member checks
- Fixed offshore platform design
- Safety factor requirements

## Module Structure

```
structural_analysis/
├── __init__.py          # Module exports
├── models.py            # Data models
├── stress_calculator.py # Stress calculations
├── buckling.py          # Buckling analysis
├── capacity.py          # Capacity checks
├── cli.py               # Command-line interface
└── README.md            # This file
```

## Testing

```bash
# Run all tests
pytest tests/modules/structural_analysis/ -v

# Run unit tests only
pytest tests/modules/structural_analysis/test_structural_analysis_unit.py -v

# Run CLI tests
pytest tests/modules/structural_analysis/test_structural_analysis_cli.py -v

# With coverage
pytest tests/modules/structural_analysis/ --cov=src/digitalmodel/modules/structural_analysis --cov-report=term
```

**Test Coverage:**
- 38 unit tests
- 18 CLI integration tests
- Code coverage > 90%
- Automated CI/CD with GitHub Actions

## Engineering Applications

### Offshore Platform Design
```python
# Check jacket leg
analyzer = ColumnBucklingAnalyzer(STEEL_S355)
result = analyzer.check_column_buckling(
    axial_force=10e6,      # 10 MN
    area=50000,            # mm²
    I_min=2e8,             # mm⁴
    L_eff=15000,           # 15m
    buckling_curve='b'
)
```

### Pressure Vessel Analysis
```python
from digitalmodel.structural_analysis import StressCalculator, STEEL_S420

calc = StressCalculator(STEEL_S420)

# Hoop stress
sigma_hoop = calc.hoop_stress(
    pressure=15.0,      # MPa
    radius=1.5,         # m
    thickness=0.025     # m
)

# Longitudinal stress
sigma_long = calc.longitudinal_stress(
    pressure=15.0,
    radius=1.5,
    thickness=0.025
)

# Check Von Mises
stress = StressState(sigma_x=sigma_long, sigma_y=sigma_hoop)
vm = stress.von_mises()
sf = STEEL_S420.yield_strength / vm
```

### Stiffened Panel Check
```python
# Check panel between stiffeners
plate = PlateGeometry(
    length=3000,    # Between stiffeners
    width=800,      # Stiffener spacing
    thickness=12
)

analyzer = PlateBucklingAnalyzer(STEEL_S355)
result = analyzer.check_plate_buckling(
    plate=plate,
    sigma_x=200,    # Compression
    sigma_y=50,     # Transverse
    tau=40,         # Shear
    gamma_m=1.15
)
```

## Limitations

- **Thin-walled assumptions**: Applicable for thickness ratios < 1/10
- **Linear elastic**: Does not include plasticity or large deflections
- **Simplified interactions**: Uses simplified interaction formulas
- **No FEA**: Analytical methods only, not finite element analysis

For detailed FEA or non-linear analysis, use specialized software.

## Best Practices

### 1. Verification
- Always verify results with hand calculations
- Check units carefully (N, mm, MPa)
- Compare with code examples

### 2. Safety Factors
- Use code-specified partial factors
- Document any deviations
- Consider consequence of failure

### 3. Load Combinations
- Check all applicable load cases
- Include dynamic amplification
- Account for fabrication tolerances

### 4. Documentation
- Reference applicable standards
- State assumptions clearly
- Include sensitivity studies

## Related Modules

- **fatigue_analysis** - Fatigue life assessment
- **signal_analysis** - Signal processing for loads
- **marine_analysis** - RAO and hydrodynamic analysis

## References

### Standards
- DNV-RP-C201: Buckling Strength of Plated Structures
- DNV-RP-C202: Buckling Strength of Shells
- Eurocode 3: Design of Steel Structures (EN 1993-1-1)
- API RP 2A-WSD: Planning, Designing and Constructing Fixed Offshore Platforms
- ISO 19902: Fixed Steel Offshore Structures

### Literature
- Timoshenko & Gere: Theory of Elastic Stability
- Young & Budynas: Roark's Formulas for Stress and Strain
- Bai & Jin: Marine Structural Design

## Support

For issues or questions:
- Check test examples for usage patterns
- Review skill documentation in `.claude/skills/structural-analysis/`
- Create an issue on GitHub

## Contributing

Contributions welcome! Please:
1. Follow existing code patterns
2. Add unit tests for new features
3. Update documentation
4. Run full test suite before submitting

---

**Maintained by**: Digital Model Development Team
**Last Updated**: 2026-01-04
**Version**: 1.0.0
