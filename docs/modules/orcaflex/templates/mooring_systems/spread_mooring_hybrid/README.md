# Spread Mooring Hybrid Template

## Overview

This template demonstrates the **validated hybrid approach** for OrcaFlex spread mooring models, combining:
- **Object-level IncludeFile** for reusable library components (LineTypes)
- **BaseFile + IncludeFile** for parametric variations (water depth, leg count)

## Directory Structure

```
spread_mooring_hybrid/
├── README.md                         # This file
├── base/
│   └── spread_mooring_base.yml       # Base model (200m, 8-leg)
├── variations/
│   ├── deep_water_500m.yml           # Deep water variation
│   └── twelve_leg.yml                # 12-leg configuration
└── cases/
    ├── case_deep_water.yml
    └── case_twelve_leg.yml
```

## Spread Mooring Configuration

8-leg spread mooring for FPSO (plan view):

```
         315°  0°   45°
           \   |   /
            \  |  /
             \ | /
   270° ------[FPSO]------ 90°
             / | \
            /  |  \
           /   |   \
         225° 180° 135°
```

### Key Features:
- **Symmetrical arrangement** - Equal distribution around vessel
- **Catenary configuration** - Chain on seabed provides holding capacity
- **Weathervaning limited** - Vessel heading fixed by mooring

## Usage

### Basic Analysis (Base Model)

```bash
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('base/spread_mooring_base.yml')
model.CalculateStatics()
"
```

### Parametric Study (BaseFile + Variation)

```yaml
# cases/case_deep_water.yml
BaseFile: ../base/spread_mooring_base.yml
IncludeFile: ../variations/deep_water_500m.yml
```

## Configuration Summary

### Base Model (200m, 8-leg)
| Parameter | Value |
|-----------|-------|
| Water depth | 200 m |
| Number of legs | 8 |
| Mooring type | 84mm R4 chain |
| Line length | 700 m |
| Anchor radius | 600 m |
| Fairlead depth | -10 m |

### Deep Water Variation (500m)
| Parameter | Value |
|-----------|-------|
| Water depth | 500 m |
| Line length | 1500 m |
| Anchor radius | 1200 m |

### 12-Leg Variation
| Parameter | Value |
|-----------|-------|
| Number of legs | 12 |
| Configuration | 4 clusters × 3 legs |
| Cluster spread | 30° per cluster |

## Library Components Used

- `library/line_types/chain_84mm_r4.yml` - 84mm R4 studless chain

## Design Parameters

### Mooring Geometry
| Parameter | Base (200m) | Deep Water (500m) |
|-----------|-------------|-------------------|
| Water depth | 200 m | 500 m |
| Anchor radius | 600 m | 1200 m |
| Line length | 700 m | 1500 m |
| Length/depth ratio | 3.5 | 3.0 |

### Chain Properties (84mm R4)
| Parameter | Value |
|-----------|-------|
| Diameter | 84 mm |
| Mass in air | 88 kg/m |
| MBL | ~7,000 kN |
| EA | 612 MN |

## Design Considerations

### Static Analysis
- Catenary equilibrium for all legs
- Vessel offset under mean environment
- Line tensions at fairleads and anchors

### Dynamic Analysis
- Vessel motion response (surge, sway, yaw)
- Line dynamics and fatigue
- Snap loading in extreme conditions

### Critical Design Factors
- Minimum line tension (avoid slack)
- Maximum tension (% of MBL)
- Anchor holding capacity
- Fairlead loads on vessel

## Mooring Leg Arrangements

### 8-Leg (Base)
- Standard for moderate environments
- 45° spacing between legs
- Balanced restoring forces

### 12-Leg (Variation)
- Harsh environment applications
- 4 clusters of 3 legs (30° spread)
- Higher redundancy

### 16-Leg (Not included)
- Ultra-harsh environments
- 4 clusters of 4 legs
- Maximum station-keeping

## Related Templates

- CALM Buoy: `templates/mooring_systems/calm_buoy_hybrid/`
- Turret Mooring: (planned)
- Library: `docs/modules/orcaflex/library/`
