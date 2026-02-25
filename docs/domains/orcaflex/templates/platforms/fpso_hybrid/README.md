# FPSO Hybrid Template

## Overview

This template demonstrates the **validated hybrid approach** for OrcaFlex FPSO platform models, combining:
- **Object-level IncludeFile** for reusable library components (LineTypes)
- **BaseFile + IncludeFile** for parametric variations (water depth, turret type)

## Directory Structure

```
fpso_hybrid/
├── README.md                         # This file
├── base/
│   └── fpso_base.yml                 # Base model (300m, internal turret)
├── variations/
│   ├── deep_water_1500m.yml          # Deep water configuration
│   └── external_turret.yml           # External turret variant
└── cases/
    └── case_deep_water.yml           # Base + deep water
```

## FPSO Configuration

FPSO with internal turret mooring:

```
                [Turret]
                   |
          +-------+-------+
          |               |
          |     FPSO      | --> Weathervaning
          |               |
          +---------------+

Mooring pattern (plan view):
         315   0   45
           \   |   /
            \  |  /
             \ | /
   270  ---[TURRET]---  90
             / | \
            /  |  \
           /   |   \
         225  180  135
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
model = OrcFxAPI.Model('base/fpso_base.yml')
model.CalculateStatics()
"
```

### Parametric Study (BaseFile + Variation)

```yaml
# cases/case_deep_water.yml
BaseFile: ../base/fpso_base.yml
IncludeFile: ../variations/deep_water_1500m.yml
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
| FPSO length | 280 m |

### Deep Water Variation (1500m)
| Parameter | Value |
|-----------|-------|
| Water depth | 1500 m |
| Mooring type | Chain-polyester-chain |
| Total line length | 2900 m |
| Anchor radius | 2700 m |

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

## Platform Properties

### 3DBuoy (Turret) Properties
| Parameter | Value |
|-----------|-------|
| Height | 10 m |
| Diameter | 15 m |
| Volume | 2000 m3 |
| Mass | 1,500 tonnes |
| Draft | ~15 m below waterline |

### Vessel Type Properties
| Parameter | Value |
|-----------|-------|
| Length | 280 m |
| Symmetry | xz plane |
| Surge positive | Forward |
| Sway positive | Port |
| Heave positive | Up |

## Library Components Used

- `library/line_types/chain_84mm_r4.yml` - 84mm R4 studless chain
- `library/line_types/polyester_200mm.yml` - 200mm polyester rope (deep water)

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

## Related Templates

- Semi-Submersible: `templates/platforms/semi_sub_hybrid/`
- TLP: `templates/platforms/tlp_hybrid/`
- Turret Mooring: `templates/mooring_systems/turret_mooring_hybrid/`
- Library: `docs/domains/orcaflex/library/`
