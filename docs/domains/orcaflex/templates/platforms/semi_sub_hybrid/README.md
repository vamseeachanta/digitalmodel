# Semi-Submersible Hybrid Template

## Overview

This template demonstrates the **validated hybrid approach** for OrcaFlex semi-submersible platform models, combining:
- **Object-level IncludeFile** for reusable library components (LineTypes)
- **BaseFile + IncludeFile** for parametric variations (water depth, mooring configuration)

## Directory Structure

```
semi_sub_hybrid/
├── README.md                         # This file
├── base/
│   └── semi_sub_base.yml             # Base model (500m, central hub mooring)
├── variations/
│   ├── deep_water_1500m.yml          # Deep water configuration
│   └── spread_moored.yml             # Spread mooring to columns
└── cases/
    └── case_deep_water.yml           # Base + deep water
```

## Semi-Submersible Configuration

4-column semi-submersible with catenary mooring:

```
Platform arrangement (plan view):

       +-----+           +-----+
       |Col 1|           |Col 2|
       +--+--+           +--+--+
          |                 |
          +-----------------+
                  |
          +-----------------+
          |                 |
       +--+--+           +--+--+
       |Col 3|           |Col 4|
       +-----+           +-----+

Mooring pattern (plan view):
         315   0   45
           \   |   /
            \  |  /
             \ | /
   270  ---[SEMI]---  90
             / | \
            /  |  \
           /   |   \
         225  180  135
```

### Key Features:
- **Fixed heading** - No weathervaning capability
- **Low motion response** - Deep draft columns
- **Symmetric mooring** - 8 lines in spread pattern
- **Stable platform** - Suitable for drilling and production

## Usage

### Basic Analysis (Base Model)

```bash
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('base/semi_sub_base.yml')
model.CalculateStatics()
"
```

### Parametric Study (BaseFile + Variation)

```yaml
# cases/case_deep_water.yml
BaseFile: ../base/semi_sub_base.yml
IncludeFile: ../variations/deep_water_1500m.yml
```

## Configuration Summary

### Base Model (500m, Central Hub)
| Parameter | Value |
|-----------|-------|
| Water depth | 500 m |
| Number of legs | 8 |
| Mooring type | 84mm R4 chain |
| Line length | 1500 m |
| Anchor radius | 1200 m |
| Column spacing | 60 m x 60 m |
| Hull length | 80 m |

### Deep Water Variation (1500m)
| Parameter | Value |
|-----------|-------|
| Water depth | 1500 m |
| Mooring type | Chain-polyester-chain |
| Total line length | 2750 m |
| Anchor radius | 2400 m |

### Spread Moored Variation
| Parameter | Value |
|-----------|-------|
| Connection points | 4 columns |
| Lines per column | 2 |
| Fairlead depth | -18 m |

## Mooring Configurations

### Central Hub (Base)
- All lines connect to single hub
- Simpler connection management
- Good for initial studies

### Spread Moored (Variation)
- Lines connect directly to columns
- More realistic fairlead locations
- Better load distribution
- Reduces yaw moment

## Platform Properties

### 3DBuoy (Mooring Hub) Properties
| Parameter | Value |
|-----------|-------|
| Height | 5 m |
| Diameter | 10 m |
| Volume | 500 m3 |
| Mass | 500 tonnes |
| Location | Below hull (-20m) |

### Vessel Type Properties
| Parameter | Value |
|-----------|-------|
| Length | 80 m |
| Symmetry | xz plane |
| Surge positive | Forward |
| Sway positive | Port |
| Heave positive | Up |

## Library Components Used

- `library/line_types/chain_84mm_r4.yml` - 84mm R4 studless chain
- `library/line_types/polyester_200mm.yml` - 200mm polyester rope (deep water)

## Design Considerations

### Static Analysis
- Hull equilibrium under mooring loads
- Line tensions at fairleads and anchors
- Column load distribution

### Dynamic Analysis
- Heave and pitch natural periods
- Low-frequency surge/sway motions
- Mooring line dynamics
- Riser interaction

### Critical Design Factors
- Air gap maintenance
- Column stability
- Deck clearance
- Riser porches

## Comparison: Semi-Sub vs FPSO

| Aspect | Semi-Sub | FPSO |
|--------|----------|------|
| Weathervaning | No | Yes |
| Motion response | Low | Moderate |
| Water depth | Moderate-Deep | Shallow-Deep |
| Storage capacity | Limited | High |
| Drilling capability | Excellent | Limited |

## Related Templates

- FPSO: `templates/platforms/fpso_hybrid/`
- TLP: `templates/platforms/tlp_hybrid/`
- Spread Mooring: `templates/mooring_systems/spread_mooring_hybrid/`
- Library: `docs/domains/orcaflex/library/`
