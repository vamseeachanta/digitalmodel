# Pliant Wave Riser Hybrid Template

## Overview

This template demonstrates the **validated hybrid approach** for OrcaFlex pliant wave riser models, combining:
- **Object-level IncludeFile** for reusable library components (LineTypes)
- **BaseFile + IncludeFile** for parametric variations (water depth)

## Directory Structure

```
pliant_wave_hybrid/
├── README.md                         # This file
├── base/
│   └── pwr_base.yml                  # Base model (800m depth)
├── variations/
│   └── deep_water_1200m.yml          # Deep water variation
└── cases/
    └── case_deep_water.yml           # Combined base + variation
```

## Pliant Wave Riser Configuration

Pliant wave riser from FPSO to seabed anchor:

```
FPSO ─────────┐
              │  Upper catenary
              │
              ╰─────╮  Sag bend
                    │
              ╭─────╯  Hog bend (buoyancy)
              │
              │  Lower catenary
              │
──────────────┴─────────────────────── Seabed
            Anchor
```

### Key Differences from Lazy Wave:
- **More pronounced wave shape** - Larger sag/hog bends
- **Touchdown on seabed** - End anchored to seabed
- **Higher buoyancy requirements** - Creates sharper bends

## Usage

### Basic Analysis (Base Model)

```bash
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('base/pwr_base.yml')
model.CalculateStatics()
"
```

### Parametric Study (BaseFile + Variation)

```yaml
# cases/case_deep_water.yml
BaseFile: ../base/pwr_base.yml
IncludeFile: ../variations/deep_water_1200m.yml
```

## Configuration Summary

### Base Model (800m)
| Parameter | Value |
|-----------|-------|
| Water depth | 800 m |
| Riser type | 10-inch X65 steel |
| Length | 1100 m |
| Hangoff depth | -20 m (below FPSO) |

### Deep Water Variation (1200m)
| Parameter | Value |
|-----------|-------|
| Water depth | 1200 m |
| Length | 1700 m |
| Extended current profile | 5 depth points |

## Library Components Used

- `library/line_types/pwr_10inch.yml` - 10-inch X65 steel pipe

## Key Design Parameters

### Pliant Wave Geometry
| Parameter | Base (800m) | Deep Water (1200m) |
|-----------|-------------|---------------------|
| Horizontal span | 900 m | 1400 m |
| Line length | 1100 m | 1700 m |
| Length/span ratio | 1.22 | 1.21 |

### Static Analysis Settings
| Parameter | Value |
|-----------|-------|
| Statics method | Catenary |
| Min damping | 10 |
| Solution method | Implicit time domain |
| Time step | 0.1 s |

## Design Considerations

### Static Analysis
- Catenary starting shape
- Higher damping may be needed for wave shape
- Convergence sensitive to length/span ratio

### Dynamic Analysis
- VIV in sag/hog bend regions
- Fatigue at touchdown point
- Current loading on exposed sections

### Critical Locations
- Hangoff point (FPSO connection)
- Sag bend (maximum curvature)
- Hog bend (buoyancy transition)
- Touchdown point (seabed interaction)

## Related Templates

- Lazy Wave Riser: `templates/risers/lazy_wave_hybrid/`
- SCR (Catenary): `templates/risers/scr_hybrid/`
- Library: `docs/modules/orcaflex/library/`
