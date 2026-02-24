# Top Tensioned Riser (TTR) Hybrid Template

## Overview

This template demonstrates the **validated hybrid approach** for OrcaFlex TTR models, combining:
- **Object-level IncludeFile** for reusable library components (LineTypes)
- **BaseFile + IncludeFile** for parametric variations (water depth)

## Directory Structure

```
ttr_hybrid/
├── README.md                         # This file
├── base/
│   └── ttr_base.yml                  # Base model (1500m depth)
├── variations/
│   ├── deep_water_2000m.yml          # Ultra deep water variation
│   └── ttr_12inch.yml                # 12-inch pipe variation
└── cases/
    ├── case_deep_water.yml           # Combined base + deep water
    └── case_12inch.yml               # Standalone 12-inch model
```

## TTR Configuration

Top Tensioned Riser from TLP/Spar to seabed (side view):

```
═══════════════════════════════ TLP/Spar Platform
      │╔═══╗│
      │║ T ║│  Top tensioner
      │║ T ║│
      │╚═╦═╝│
        ║
        ║     Riser (near-vertical)
        ║
        ║
        ║
────────╨────────────────────── Seabed
      Wellhead
```

### Key Characteristics:
- **Near-vertical configuration** - Direct seabed to platform
- **Top tension** - Maintained by tensioners or buoyancy
- **Platform types** - TLP, Spar, or compliant platforms
- **Deep water** - Typically 1000-3000m water depth

## Usage

### Basic Analysis (Base Model)

```bash
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('base/ttr_base.yml')
model.CalculateStatics()
"
```

### Parametric Study (BaseFile + Variation)

```yaml
# cases/case_deep_water.yml
BaseFile: ../base/ttr_base.yml
IncludeFile: ../variations/deep_water_2000m.yml
```

## Configuration Summary

### Base Model (1500m)
| Parameter | Value |
|-----------|-------|
| Water depth | 1500 m |
| Riser type | 9-inch X80 steel |
| Riser length | 1480 m |
| Platform draft | 25 m |

### Deep Water Variation (2000m)
| Parameter | Value |
|-----------|-------|
| Water depth | 2000 m |
| Riser length | 1980 m |
| Other | Same as base |

### 12-inch Variation
| Parameter | Value |
|-----------|-------|
| Pipe type | 12-inch X65 steel |
| Other | Same as base |

## Library Components Used

- `library/line_types/ttr_9inch_x80.yml` - 9-inch X80 high strength steel
- `library/line_types/scr_12inch_x65.yml` - 12-inch X65 steel (for 12-inch case)

## Design Considerations

### TTR vs SCR Comparison
| Feature | TTR | SCR |
|---------|-----|-----|
| Configuration | Vertical | Catenary |
| Platform motion | Limited heave | All DOF |
| Water depth | 1000-3000m | 500-2500m |
| Complexity | Higher | Lower |
| Platform type | TLP/Spar | FPSO/Semi |

### Advantages
- Direct vertical connection to wellhead
- Simplified flow assurance
- Dry tree completion possible
- Good for high pressure/temperature

### Challenges
- Requires compliant platform (TLP/Spar)
- Top tensioner system complexity
- Higher capital cost
- Platform heave limitations

### Critical Locations
- Top tensioner connection
- Keel joint (at platform keel)
- Stress joint at wellhead
- Tapered stress joint transitions

## Related Templates

- SCR: `templates/risers/scr_hybrid/`
- Lazy Wave: `templates/risers/lazy_wave_hybrid/`
- Pliant Wave: `templates/risers/pliant_wave_hybrid/`
- Steep Wave: `templates/risers/steep_wave_hybrid/`
- Library: `docs/modules/orcaflex/library/`
