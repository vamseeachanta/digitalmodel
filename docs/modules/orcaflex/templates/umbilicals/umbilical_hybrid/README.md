# Umbilical Hybrid Template

## Overview

This template demonstrates the **validated hybrid approach** for OrcaFlex dynamic umbilical models, combining:
- **Object-level IncludeFile** for reusable library components (LineTypes)
- **BaseFile + IncludeFile** for parametric variations (water depth, umbilical type)

## Directory Structure

```
umbilical_hybrid/
├── README.md                         # This file
├── base/
│   └── umbilical_base.yml            # Base model (800m depth)
├── variations/
│   ├── deep_water_1200m.yml          # Deep water variation
│   └── steel_tube.yml                # Steel tube umbilical
└── cases/
    ├── case_deep_water.yml
    └── case_steel_tube.yml
```

## Umbilical Configuration

Dynamic umbilical from platform to subsea distribution unit:

```
Platform ─────┐
              │  Umbilical
              │  (catenary)
              │
              │
              │
              ╰──────────────╮
                             │
─────────────────────────────┴─── Seabed
                           SDU/Manifold
```

### Umbilical Functions:
- **Hydraulic control**: Valve actuation
- **Electrical power**: Subsea equipment
- **Chemical injection**: MEG, scale inhibitor
- **Fiber optic**: Communications/monitoring

## Usage

### Basic Analysis (Base Model)

```bash
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('base/umbilical_base.yml')
model.CalculateStatics()
"
```

### Parametric Study (BaseFile + Variation)

```yaml
# cases/case_deep_water.yml
BaseFile: ../base/umbilical_base.yml
IncludeFile: ../variations/deep_water_1200m.yml
```

## Configuration Summary

### Base Model (800m)
| Parameter | Value |
|-----------|-------|
| Water depth | 800 m |
| Umbilical type | Dynamic flexible |
| Length | 1200 m |
| Hangoff | Platform at -15m |

### Deep Water Variation (1200m)
| Parameter | Value |
|-----------|-------|
| Water depth | 1200 m |
| Length | 1800 m |

### Steel Tube Variation
| Parameter | Value |
|-----------|-------|
| Umbilical type | Steel tube |
| Higher stiffness, different dynamics |

## Library Components Used

- `library/line_types/umb_flex_dynamic.yml` - Dynamic flexible umbilical
- `library/line_types/umb_steel_tube.yml` - Steel tube umbilical
- `library/line_types/umb_static.yml` - Static umbilical

## Umbilical Types

| Type | Application | Characteristics |
|------|-------------|-----------------|
| Dynamic Flexible | Moving platforms | Low bending stiffness, fatigue resistant |
| Steel Tube | Fixed/compliant | Higher stiffness, pressure resistant |
| Static | Seabed sections | No dynamic loading |
| Hybrid | Mixed application | Combined properties |

## Design Considerations

### Dynamic Analysis
- Vessel motion induced fatigue
- VIV (vortex-induced vibration)
- Current loading
- Wave loading

### Critical Locations
- Hangoff point (I/J tube)
- Touch down point (TDP)
- Bend stiffener region

## Related Documentation

- Riser Templates: `templates/risers/`
- Pipeline Templates: `templates/pipelines/`
- Library: `docs/modules/orcaflex/library/`

