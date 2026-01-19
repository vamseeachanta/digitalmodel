# Turret Mooring Hybrid Template

## Overview

This template demonstrates the **validated hybrid approach** for OrcaFlex turret mooring models, combining:
- **Object-level IncludeFile** for reusable library components (LineTypes)
- **BaseFile + IncludeFile** for parametric variations (water depth, turret type)

## Directory Structure

```
turret_mooring_hybrid/
├── README.md                         # This file
├── base/
│   └── turret_mooring_base.yml       # Base model (300m, internal turret)
├── variations/
│   ├── deep_water_600m.yml           # Deep water variation
│   └── external_turret.yml           # External turret configuration
└── cases/
    ├── case_deep_water.yml
    └── case_external_turret.yml
```

## Turret Mooring Configuration

Internal turret mooring for FPSO:

```
                [Turret]
                   │
          ┌───────┴───────┐
          │               │
          │     FPSO      │──► Weathervaning
          │               │
          └───────────────┘

Mooring pattern (plan view):
         315°  0°   45°
           \   |   /
            \  |  /
             \ | /
   270° ───[TURRET]─── 90°
             / | \
            /  |  \
           /   |   \
         225° 180° 135°
```

### Key Features:
- **Weathervaning** - Vessel free to rotate about turret
- **Internal/External turret** - Two configuration options
- **Mooring at single point** - All lines to turret
- **Reduced environmental loads** - Vessel aligns with weather

## Usage

### Basic Analysis (Base Model)

```bash
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('base/turret_mooring_base.yml')
model.CalculateStatics()
"
```

### Parametric Study (BaseFile + Variation)

```yaml
# cases/case_deep_water.yml
BaseFile: ../base/turret_mooring_base.yml
IncludeFile: ../variations/deep_water_600m.yml
```

## Configuration Summary

### Base Model (300m, Internal Turret)
| Parameter | Value |
|-----------|-------|
| Water depth | 300 m |
| Number of legs | 8 |
| Mooring type | 84mm R4 chain |
| Line length | 1000 m |
| Anchor radius | 900 m |
| Turret position | Bow (X=130m) |

### Deep Water Variation (600m)
| Parameter | Value |
|-----------|-------|
| Water depth | 600 m |
| Line length | 2000 m |
| Anchor radius | 1500 m |

### External Turret Variation
| Parameter | Value |
|-----------|-------|
| Turret position | Forward of bow (X=160m) |
| Anchor radius | 930 m (adjusted) |

## Turret Types

### Internal Turret
- Located within hull moonpool
- Protected from environment
- Higher structural integration
- Common for harsh environments

### External Turret
- Mounted on cantilever at bow
- Simpler construction
- Lower weathervaning friction
- Common for benign environments

## Library Components Used

- `library/line_types/chain_84mm_r4.yml` - 84mm R4 studless chain

## Design Parameters

### Turret Mooring Geometry
| Parameter | Base (300m) | Deep Water (600m) |
|-----------|-------------|-------------------|
| Water depth | 300 m | 600 m |
| Anchor radius | 900 m | 1500 m |
| Line length | 1000 m | 2000 m |
| Length/depth ratio | 3.3 | 3.3 |

### 6D Buoy (Turret) Properties
| Parameter | Value |
|-----------|-------|
| Height | 10 m |
| Volume | 2000 m³ |
| Mass | 1,500 tonnes |
| Draft | ~15 m below waterline |

## Design Considerations

### Static Analysis
- Turret equilibrium under mooring loads
- Vessel weathervaning to minimize loads
- Line tensions at turret and anchors

### Dynamic Analysis
- Yaw motion damping
- Low-frequency motions
- Turret bearing loads
- Riser/umbilical interaction

### Critical Design Factors
- Turret bearing capacity
- Swivel/fluid transfer system
- Emergency disconnect
- Riser hang-off arrangement

## Comparison: Turret vs Spread Mooring

| Aspect | Turret | Spread |
|--------|--------|--------|
| Weathervaning | Yes | No |
| Environmental loads | Lower | Higher |
| Complexity | Higher | Lower |
| Heading | Variable | Fixed |
| Best for | Harsh environments | Benign conditions |

## Related Templates

- Spread Mooring: `templates/mooring_systems/spread_mooring_hybrid/`
- CALM Buoy: `templates/mooring_systems/calm_buoy_hybrid/`
- Library: `docs/modules/orcaflex/library/`
