# Steep Wave Riser Hybrid Template

## Overview

This template demonstrates the **validated hybrid approach** for OrcaFlex steep wave riser models, combining:
- **Object-level IncludeFile** for reusable library components (LineTypes)
- **BaseFile + IncludeFile** for parametric variations (water depth)

## Directory Structure

```
steep_wave_hybrid/
├── README.md                         # This file
├── base/
│   └── swr_base.yml                  # Base model (1000m depth)
├── variations/
│   └── deep_water_1500m.yml          # Deep water variation
└── cases/
    └── case_deep_water.yml           # Combined base + variation
```

## Steep Wave Riser Configuration

Steep wave riser from FPSO to seabed (side view):

```
FPSO ─┐
      │
      │  Upper section (steep angle)
      │
      ╰─╮  Buoyancy wave
        │
        │  Lower section
        │
────────┴────────────────────────── Seabed
     (short footprint)
```

### Key Characteristics:
- **Near-vertical approach** - Steep angle from FPSO
- **Short seabed footprint** - Minimal horizontal span
- **Congested areas** - Ideal for limited seabed space
- **Higher top tension** - Due to steep configuration

## Usage

### Basic Analysis (Base Model)

```bash
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('base/swr_base.yml')
model.CalculateStatics()
"
```

### Parametric Study (BaseFile + Variation)

```yaml
# cases/case_deep_water.yml
BaseFile: ../base/swr_base.yml
IncludeFile: ../variations/deep_water_1500m.yml
```

## Configuration Summary

### Base Model (1000m)
| Parameter | Value |
|-----------|-------|
| Water depth | 1000 m |
| Riser type | 10-inch X65 steel |
| Length | 1200 m |
| Horizontal span | 350 m |
| Hangoff depth | -20 m |

### Deep Water Variation (1500m)
| Parameter | Value |
|-----------|-------|
| Water depth | 1500 m |
| Length | 1800 m |
| Horizontal span | 500 m |

## Library Components Used

- `library/line_types/scr_10inch_x65.yml` - 10-inch X65 steel pipe

## Key Design Parameters

### Steep Wave Geometry
| Parameter | Base (1000m) | Deep Water (1500m) |
|-----------|--------------|---------------------|
| Water depth | 1000 m | 1500 m |
| Horizontal span | 350 m | 500 m |
| Line length | 1200 m | 1800 m |
| Span/depth ratio | 0.35 | 0.33 |

### Comparison with Other Risers
| Riser Type | Span/Depth | Top Tension | Footprint |
|------------|------------|-------------|-----------|
| SCR (Catenary) | 1.0-1.5 | Low | Large |
| Lazy Wave | 0.8-1.2 | Medium | Medium |
| Pliant Wave | 0.6-1.0 | Medium | Medium |
| **Steep Wave** | **0.3-0.5** | **High** | **Small** |

## Design Considerations

### Advantages
- Minimal seabed footprint
- Suitable for congested areas
- Less interaction with other lines

### Challenges
- Higher top tension at hangoff
- More sensitive to vessel motion
- Steeper departure angle

### Critical Locations
- Hangoff point (high tension)
- Sag bend region
- Touchdown point

## Related Templates

- SCR (Catenary): `templates/risers/scr_hybrid/`
- Lazy Wave: `templates/risers/lazy_wave_hybrid/`
- Pliant Wave: `templates/risers/pliant_wave_hybrid/`
- Library: `docs/modules/orcaflex/library/`
