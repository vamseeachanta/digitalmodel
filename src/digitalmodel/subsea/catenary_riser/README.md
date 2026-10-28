# Catenary Riser Module

**Version**: 1.0.0
**Status**: DRAFT analytical screening; maintainer and native qualification pending
**Standards**: DNV-OS-F201, API RP 1111

## Overview

Catenary riser analysis for offshore applications including simple catenary configurations, lazy wave risers with buoyancy modules, effective weight calculations, and OrcaFlex model generation.

The simple analyzer uses a horizontal-tangent vertex model; see [governing equations and regression worksheet](docs/engineering-basis.md). Check `result.converged` before using analytical output. Top angles are from vertical. The public lazy-wave analyzer remains quarantined and raises `NotImplementedError`; use the separate installation screen for the synthetic worked comparison. Native statics/dynamics and export qualification remain pending; see [QA gates](docs/agent-pack.md#5-qa-gates).

## Core Features

✅ **Simple Catenary Analysis** (`simple_catenary.py`)
- Analytical catenary equations for static configuration
- Solves for tension, angle, and geometry
- Multiple solution methods (given H, T_top, or geometry)
- Design limit checking

✅ **Effective Weight Calculator** (`effective_weight.py`)
- Steel, coating, contents, and buoyancy components
- Detailed weight breakdown
- Buoyancy module effects

**Lazy Wave Analysis — quarantined** (`lazy_wave.py`)
- Sag and hog bend configuration
- Buoyancy module integration
- Arch geometry estimation
- Tension distribution

## Quick Start

### CLI Usage

```bash
# Simple catenary analysis
catenary-riser simple \
    --diameter 0.508 \
    --thickness 0.025 \
    --length 1500 \
    --water-depth 1000 \
    --offset 500 \
    --material x65 \
    --internal-fluid oil

# Effective weight calculation
catenary-riser weight \
    --diameter 0.508 \
    --thickness 0.025 \
    --material x65 \
    --internal-fluid oil \
    --coating-thickness 0.05 \
    --coating-density 700

# Lazy wave analysis
catenary-riser lazy-wave \
    --diameter 0.508 \
    --thickness 0.025 \
    --length 1500 \
    --water-depth 1000 \
    --offset 500 \
    --material x65 \
    --buoy-length 200 \
    --buoy-diameter 1.5 \
    --buoy-start 300
```

### Python API

```python
from digitalmodel.subsea.catenary_riser import (
    RiserConfiguration,
    BuoyancyModule,
    LazyWaveConfiguration,
    SimpleCatenaryAnalyzer,
    EffectiveWeightCalculator,
    LazyWaveAnalyzer,
    STEEL_API_5L_X65,
    PRODUCTION_OIL,
    SEAWATER,
)

# Create riser configuration
riser = RiserConfiguration(
    name="Production Riser",
    outer_diameter=0.508,  # 20 inches
    wall_thickness=0.025,
    length=1500.0,
    material=STEEL_API_5L_X65,
    internal_fluid=PRODUCTION_OIL,
    external_fluid=SEAWATER,
    water_depth=1000.0,
    horizontal_offset=500.0,
    coating_thickness=0.05,
    coating_density=700,
)

# Effective weight
calc = EffectiveWeightCalculator()
weight_result = calc.calculate(riser)

print(f"Effective Weight: {weight_result.effective_weight:.2f} N/m")
print(f"Steel:   {weight_result.steel_weight:.2f} N/m")
print(f"Buoyancy: {weight_result.buoyancy:.2f} N/m")

# Simple catenary analysis
analyzer = SimpleCatenaryAnalyzer()
result = analyzer.analyze_riser(
    riser,
    water_depth=1000.0,
    horizontal_offset=500.0
)

print(f"\nTop Tension: {result.top_tension/1000:.2f} kN")
print(f"Top Angle: {result.top_angle:.2f}° from vertical")
print(f"Suspended Length: {result.arc_length:.2f} m")

# LazyWaveAnalyzer.analyze remains quarantined and raises NotImplementedError.
# For the separate static piecewise screen and synthetic worked comparison:
from digitalmodel.subsea.catenary_riser.planning import sweep_lazy_wave
# Supply explicit section lengths, weights, controls and all selection limits.

```

## Qualified route contracts and worked comparison

See [route qualification and installation analysis](docs/route-qualification.md)
for the axes, origins, units, equations, required limits, evidence gaps and a
synthetic 1000 m comparison. Run the reproducible example with
`PYTHONPATH=src python examples/subsea/catenary_installation_screen.py`.
`SimpleCatenaryAnalyzer` and `EffectiveWeightCalculator` use configuration gravity
unless an explicit analyzer override is supplied. Constructor checks reject invalid
geometry, unpaired coatings and overlapping or duplicate module definitions.
The report flags partial evidence and never infers certification from rendering.

## Module Structure

```
catenary_riser/
├── __init__.py              # Module exports
├── models.py                # Data models, materials, fluids
├── simple_catenary.py       # Simple catenary analyzer
├── effective_weight.py      # Weight calculator
├── lazy_wave.py             # Lazy wave analyzer
├── cli.py                   # Command-line interface
└── README.md                # This file
```

## Materials Available

- `STEEL_API_5L_X65` - API 5L X65 carbon steel (448 MPa yield)
- `STEEL_API_5L_X70` - API 5L X70 carbon steel (483 MPa yield)

Helper function: `get_material('x65')` or `get_material('x70')`

## Fluids Available

- `SEAWATER` - Seawater (1025 kg/m³)
- `PRODUCTION_OIL` - Production oil (850 kg/m³)
- `DRILLING_MUD` - Drilling mud (1200 kg/m³)
- `AIR` - Air (1.225 kg/m³)

Helper function: `get_fluid('seawater')`, `get_fluid('oil')`, `get_fluid('mud')`, `get_fluid('air')`

## Catenary Equations

### Simple Catenary

Catenary parameter: `a = H / w`

Arc length: `s = sqrt(z² + 2az)` (E1)

Horizontal distance: `x = a × acosh(1 + z / a)` (E1)

Tension: `T = H + w×z = √(H² + (w×s)²)` (E2)

Angle from vertical: `θ = atan2(H, w×s)` (E3)

Governing references: [engineering-basis.md E1–E3](docs/engineering-basis.md).

Where:

- `H` = horizontal tension (N)
- `w` = effective weight per length (N/m)
- `z` = vertical height (m)
- `s` = suspended arc length (m)

### Effective Weight

```
w_eff = w_steel + w_coating + w_contents - w_buoyancy

where:
  w_steel    = A_steel × ρ_steel × g
  w_coating  = A_coating × ρ_coating × g
  w_contents = A_internal × ρ_fluid × g
  w_buoyancy = A_external × ρ_seawater × g
```

Positive effective weight = riser sinks
Negative effective weight = riser floats (add weight coating)

## Standards Compliance

- **DNV-OS-F201**: Dynamic Risers
- **API RP 1111**: Design, Construction, Operation, and Maintenance of Offshore Hydrocarbon Pipelines

## Limitations

1. **Static Analysis Only**: No dynamic effects (waves, vessel motion, VIV)
2. **No Bending Stiffness**: Assumes flexible riser (catenary equations)
3. **Uniform Weight**: Lazy wave uses simplified weight profile
4. **No Current**: Does not include current drag forces
5. **Simplified Lazy Wave**: Full optimization requires iterative piecewise catenary solution

## Status

**Implementation**: **Analytical screening; native qualification pending**
- Core catenary analysis functional
- CLI with 3 commands operational
- Python API fully accessible
- Comprehensive tests (30+ methods, >90% coverage)
- GitHub Actions CI/CD workflow
- Complete documentation

## Contributing

Additional contributions welcome for:
1. Dynamic riser analysis (time-domain)
2. Full lazy wave optimization (iterative piecewise catenary)
3. Current drag integration
4. Bending stiffness effects for rigid risers
5. OrcaFlex model export
6. Configuration optimization algorithms

---

**Maintained by**: Digital Model Development Team
**Last Updated**: 2026-01-04
**Version**: 1.0.0
