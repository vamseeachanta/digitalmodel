# SALM (Single Anchor Leg Mooring) Hybrid Template

## Overview

This template demonstrates the **validated hybrid approach** for OrcaFlex SALM models, combining:
- **Object-level IncludeFile** for reusable library components (LineTypes)
- **BaseFile + IncludeFile** for parametric variations (water depth)

## Directory Structure

```
salm_hybrid/
├── README.md                         # This file
├── base/
│   └── salm_base.yml                 # Base model (50m depth)
├── variations/
│   ├── deep_water_100m.yml           # Deep water variation
│   └── wire_rope_leg.yml             # Wire rope line type
└── cases/
    ├── case_deep_water.yml           # Combined base + deep water
    └── case_wire_rope.yml            # Standalone wire rope model
```

## SALM Configuration

Single anchor leg mooring buoy (side view):

```
                    ╔════════╗
                    ║  SALM  ║ Buoy at surface
                    ║  Buoy  ║
                    ╚════╦═══╝
                         ║
                         ║  Single chain/wire leg
                         ║  (near-vertical)
                         ║
─────────────────────────╨───────────────────── Seabed
                      Anchor
```

### Key Characteristics:
- **Single anchor point** - Simplified mooring system
- **Near-vertical leg** - Short horizontal offset
- **Surface buoy** - 3DOF buoy for translational motion
- **Shallow water** - Typically 30-100m depth

## Usage

### Basic Analysis (Base Model)

```bash
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('base/salm_base.yml')
model.CalculateStatics()
"
```

### Parametric Study (BaseFile + Variation)

```yaml
# cases/case_deep_water.yml
BaseFile: ../base/salm_base.yml
IncludeFile: ../variations/deep_water_100m.yml
```

## Configuration Summary

### Base Model (50m)
| Parameter | Value |
|-----------|-------|
| Water depth | 50 m |
| Buoy mass | 45,000 kg |
| Buoy volume | 250 m³ |
| Chain type | 84mm R4 studless |
| Chain length | 55 m |
| Anchor offset | 15 m horizontal |

### Deep Water Variation (100m)
| Parameter | Value |
|-----------|-------|
| Water depth | 100 m |
| Buoy mass | 55,000 kg |
| Buoy volume | 300 m³ |
| Chain length | 110 m |
| Anchor offset | 25 m horizontal |

### Wire Rope Variation
| Parameter | Value |
|-----------|-------|
| Line type | 76mm wire rope |
| Other | Same as base |

## Library Components Used

- `library/line_types/chain_84mm_r4.yml` - 84mm R4 studless chain
- `library/line_types/wire_rope_76mm.yml` - 76mm wire rope (6x36 IWRC)

## Design Considerations

### SALM vs CALM Comparison
| Feature | SALM | CALM |
|---------|------|------|
| Anchor points | 1 | 4-8 |
| Water depth | 30-100m | 50-200m |
| Complexity | Simple | Complex |
| Station keeping | Limited | Good |
| Cost | Lower | Higher |

### Advantages
- Simple construction and installation
- Lower capital cost than multi-leg systems
- Suitable for temporary moorings

### Challenges
- Limited station keeping capability
- Higher buoy motion in waves
- Single point of failure

### Critical Locations
- Anchor connection point
- Buoy fairlead
- Chain/wire rope fatigue at bends

## Related Templates

- CALM Buoy: `templates/mooring_systems/calm_buoy_hybrid/`
- Spread Mooring: `templates/mooring_systems/spread_mooring_hybrid/`
- Turret Mooring: `templates/mooring_systems/turret_mooring_hybrid/`
- Library: `docs/domains/orcaflex/library/`
