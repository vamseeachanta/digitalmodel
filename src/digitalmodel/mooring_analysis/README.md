# Mooring Analysis Module

**Version**: 1.0.0
**Status**: ✅ Production Ready
**Standards**: DNV-OS-E301, API RP 2SK, ABS Position Mooring

## Overview

Comprehensive mooring system design and analysis for offshore floating structures including CALM/SALM buoys, spread mooring configurations, catenary analysis, and safety factor verification per international standards.

## Features

### Mooring System Types
- **CALM (Catenary Anchor Leg Mooring)** - Multi-point catenary mooring for tanker terminals
- **SALM (Single Anchor Leg Mooring)** - Single point mooring with weathervaning
- **Spread Mooring** - Fixed heading systems for FPSO and semi-submersibles
- **Turret Mooring** - Internal/external turret systems

### Catenary Analysis
- **Analytical catenary equations** - Suspended line geometry and tensions
- **Horizontal/vertical stiffness** - Geometric and elastic stiffness calculation
- **Touchdown point analysis** - Grounded length and tension distribution
- **Iterative solvers** - Target tension and geometry solving

### Mooring Design
- **Safety factor verification** (DNV-OS-E301)
  - Intact condition: SF = 1.67 (dynamic)
  - Damaged condition: SF = 1.25 (one line failed)
  - Transient condition: SF = 1.05

- **Environmental loading**
  - Wave drift forces (Newman approximation)
  - Current forces (drag-based)
  - Wind forces (API/OCIMF methods)

- **Load case analysis**
  - Intact system verification
  - Damaged line (n-1) analysis
  - Multiple environmental conditions

### OrcaFlex Integration
- **YAML model generation** - Complete OrcaFlex model export
- **Line type definitions** - Chain, wire, polyester properties
- **Vessel configuration** - Vessel particulars and fairlead locations
- **Anchor configuration** - Anchor types and locations

## Installation

The module is part of the digitalmodel package:

```bash
# Install from repository
uv pip install -e .

# Verify CLI registration
mooring-analysis --version
```

## Quick Start

### CLI Usage

```bash
# Catenary analysis with horizontal tension
mooring-analysis catenary \
    --water-depth 100 \
    --line-length 500 \
    --line-weight 145 \
    --horizontal-tension 1000

# Catenary analysis with target fairlead tension
mooring-analysis catenary \
    --water-depth 100 \
    --line-length 500 \
    --line-weight 145 \
    --fairlead-tension 2000 \
    --output catenary_result.json

# CALM mooring design verification
mooring-analysis design \
    --system-type calm \
    --water-depth 100 \
    --n-lines 6 \
    --anchor-radius 400 \
    --material chain_r3_84 \
    --wave-hs 8.0 \
    --current-speed 1.2 \
    --check-damaged \
    --output design_result.json

# Spread mooring design
mooring-analysis design \
    --system-type spread \
    --water-depth 80 \
    --n-lines 8 \
    --material polyester_140 \
    --line-length 600

# List available materials
mooring-analysis list-materials
```

### Python API

```python
from digitalmodel.modules.mooring_analysis import (
    MooringSystem,
    MooringLine,
    MooringLineProperties,
    AnchorProperties,
    VesselParticulars,
    EnvironmentalConditions,
    CatenaryAnalyzer,
    MooringDesigner,
    DesignLoadCase,
    MooringType,
    LineType,
    ConditionType,
    CHAIN_R3_84MM,
    POLYESTER_140MM
)

# Catenary analysis
analyzer = CatenaryAnalyzer(water_depth=100.0)

chain = CHAIN_R3_84MM
chain.length = 500.0

result = analyzer.solve_catenary(
    line=chain,
    horizontal_tension=1000.0,
    vertical_height=100.0
)

print(f"Fairlead Tension: {result.fairlead_tension:.2f} kN")
print(f"Fairlead Angle: {result.fairlead_angle:.2f}°")
print(f"Horizontal Distance: {result.horizontal_distance:.2f} m")

# Calculate stiffness
stiffness = analyzer.calculate_stiffness(chain, result)
print(f"Horizontal Stiffness: {stiffness.horizontal_stiffness:.2f} kN/m")

# Mooring system design
vessel = VesselParticulars(
    vessel_type="tanker",
    length=280.0,
    beam=46.0,
    draft=17.5,
    displacement=150000.0,
    windage_area=6000.0
)

# Create mooring lines
lines = []
for i in range(6):
    angle = i * 60.0  # 6 lines at 60° spacing
    angle_rad = angle * np.pi / 180.0

    chain_segment = CHAIN_R3_84MM
    chain_segment.length = 450.0

    anchor = AnchorProperties(
        anchor_type="suction_pile",
        holding_capacity=5000.0,
        location=(400 * np.cos(angle_rad), 400 * np.sin(angle_rad), -100.0)
    )

    line = MooringLine(
        line_id=f"ML{i+1}",
        segments=[chain_segment],
        anchor=anchor,
        fairlead_location=(20 * np.cos(angle_rad), 20 * np.sin(angle_rad), -10.0),
        pretension=500.0
    )
    lines.append(line)

system = MooringSystem(
    system_type=MooringType.CALM,
    water_depth=100.0,
    lines=lines,
    vessel=vessel
)

# Design verification
designer = MooringDesigner(system)

env = EnvironmentalConditions(
    wave_hs=8.0,
    wave_tp=12.0,
    wave_direction=0.0,
    current_speed=1.2,
    current_direction=0.0,
    wind_speed=25.0,
    wind_direction=0.0,
    return_period=100
)

# Analyze intact condition
intact_case = DesignLoadCase(
    name="intact_100yr",
    condition=ConditionType.INTACT,
    environment=env
)

results = designer.analyze_intact_condition(intact_case)

for result in results:
    status = 'PASS' if result.passes else 'FAIL'
    print(f"{result.line_id}: SF={result.safety_factor:.2f} ({status})")

# Generate summary
summary = designer.generate_design_summary(results)
print(f"\nOverall Status: {summary['overall_status']}")
print(f"Min Safety Factor: {summary['min_safety_factor']:.2f}")
```

## Available Materials

| Material ID | Type | Diameter (mm) | MBL (kN) | Weight (kg/m) | EA (kN) |
|------------|------|---------------|----------|---------------|---------|
| **chain_r3_84** | Chain R3 | 84 | 8,500 | 145.0 | 850,000 |
| **chain_r3_102** | Chain R3 | 102 | 12,200 | 210.0 | 1,050,000 |
| **chain_r4_84** | Chain R4 | 84 | 9,800 | 145.0 | 850,000 |
| **wire_76** | Wire rope | 76 | 4,950 | 27.5 | 550,000 |
| **polyester_140** | Polyester | 140 | 7,200 | -8.0 | 180,000 |
| **polyester_180** | Polyester | 180 | 12,000 | -13.0 | 280,000 |

*Note: Negative weight indicates buoyant lines*

## Standards Compliance

### DNV-OS-E301 (Position Mooring)
- Safety factor requirements for ULS
- Intact and damaged conditions
- Dynamic load factors
- Material partial factors

### API RP 2SK (Station Keeping)
- Design criteria for floating systems
- Environmental load calculation methods
- Mooring line sizing guidelines
- Analysis requirements

### ABS (Position Mooring Systems)
- Material specifications
- Testing and certification
- Safety and reliability requirements

## Engineering Applications

### CALM Buoy Terminal

```python
# 8-line CALM mooring for tanker offloading
system = MooringSystem(
    system_type=MooringType.CALM,
    water_depth=120.0,
    lines=[...],  # 8 catenary mooring lines
    vessel=tanker_vessel
)

designer = MooringDesigner(system)

# 100-year environmental conditions
env_100yr = EnvironmentalConditions(
    wave_hs=9.5,
    wave_tp=13.5,
    current_speed=1.5,
    wind_speed=28.0,
    return_period=100
)

# Intact analysis
intact_results = designer.analyze_intact_condition(
    DesignLoadCase("intact_100yr", ConditionType.INTACT, env_100yr)
)

# Damaged analysis (one line failed)
for line_id in ["ML1", "ML2", "ML3"]:  # Check critical lines
    damaged_case = DesignLoadCase(
        f"damaged_{line_id}",
        ConditionType.DAMAGED,
        env_100yr,
        damaged_line_id=line_id
    )
    damaged_results = designer.analyze_damaged_condition(damaged_case)
```

### Spread Mooring FPSO

```python
# 12-line spread mooring for FPSO
system = MooringSystem(
    system_type=MooringType.SPREAD,
    water_depth=1500.0,
    lines=[...],  # 12 lines in 4 groups
    vessel=fpso_vessel
)

# Environmental loads
loads = designer.calculate_environmental_loads(env_100yr)
print(f"Total Environmental Force: {loads.total_force:.0f} kN")
```

### OrcaFlex Model Export

```python
from digitalmodel.modules.mooring_analysis import OrcaFlexModelGenerator

generator = OrcaFlexModelGenerator(system)

# Generate YAML model
generator.generate_model_yml('models/calm_mooring.yml')

# Or get model as dictionary
model_dict = generator.generate_model_dict()
```

## Limitations

- **Catenary assumptions**: Uniform weight, no current drag on line
- **Quasi-static analysis**: No dynamic simulation (use OrcaFlex for dynamics)
- **Simplified environmental loads**: Use diffraction analysis (AQWA/OrcaWave) for accurate forces
- **Single segment per line**: Multi-segment lines supported but analyzed as single equivalent

For detailed dynamic analysis, export to OrcaFlex and run time-domain simulations.

## Best Practices

### 1. Design Process
- Start with catenary analysis to size lines
- Verify safety factors for all load cases
- Check both intact and damaged conditions
- Include sensitivity studies for key parameters

### 2. Safety Factors
- Use DNV-OS-E301 safety factors as minimum
- Consider project-specific requirements
- Document any deviations from standards
- Account for manufacturing tolerances

### 3. Environmental Conditions
- Use site-specific metocean data
- Include directionality effects
- Check all return periods (1yr, 10yr, 100yr)
- Consider current profiles

### 4. Documentation
- Record all assumptions clearly
- State applicable standards
- Include sensitivity analyses
- Provide clear load case definitions

## Related Modules

- **catenary** - Advanced catenary analysis (riser systems)
- **orcaflex** - OrcaFlex model generation and execution
- **fatigue_analysis** - Mooring line fatigue assessment
- **structural_analysis** - Anchor/fairlead structure checks

## References

### Standards
- DNV-OS-E301: Position Mooring
- DNV-RP-E302: Design and Installation of Plate Anchors
- API RP 2SK: Design and Analysis of Stationkeeping Systems for Floating Structures
- ABS: Rules for Building and Classing Mobile Offshore Units
- ISO 19901-7: Stationkeeping Systems for Floating Offshore Structures

### Literature
- Faltinsen: Sea Loads on Ships and Offshore Structures
- API RP 2SM: Synthetic Fiber Ropes for Offshore Mooring
- OCIMF: Mooring Equipment Guidelines

## Support

For issues or questions:
- Check test examples for usage patterns
- Review skill documentation in `.claude/skills/mooring-design/`
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
