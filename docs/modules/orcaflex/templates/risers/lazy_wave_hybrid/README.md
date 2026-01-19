# Lazy Wave Riser Hybrid Template

## Overview

This template demonstrates the **validated hybrid approach** for OrcaFlex Lazy Wave Riser (LWR) models, combining:
- **Object-level IncludeFile** for reusable library components (LineTypes)
- **BaseFile + IncludeFile** for parametric variations (water depth, buoyancy length)

## Directory Structure

```
lazy_wave_hybrid/
├── README.md                         # This file
├── base/
│   └── lwr_base.yml                  # Base model (1000m water depth)
├── variations/
│   ├── deep_water_1500m.yml          # Deep water variation (1500m)
│   └── extended_buoyancy.yml         # Extended buoyancy section
└── cases/
    ├── case_deep_water.yml           # Case file: 1500m depth
    └── case_extended_buoyancy.yml    # Case file: extended buoyancy
```

## Lazy Wave Riser Configuration

A lazy wave riser consists of three main sections:

```
FPSO ─────┐
          │  Upper Catenary
          │  (no buoyancy)
          │
     Sag Bend ←─── Start of buoyancy section
          ╭─────╮
         ╱       ╲  Buoyancy Section
        ╱         ╲ (creates "wave" shape)
       ╱           ╲
  Hog Bend ←─── End of buoyancy section
          │
          │  Lower Catenary
          │  (no buoyancy)
          │
─────────┴───────── Seabed (Touchdown Point)
```

### Key Components:
1. **Upper Catenary**: From hangoff to sag bend (no buoyancy)
2. **Buoyancy Section**: Creates the wave shape (distributed buoyancy modules)
3. **Lower Catenary**: From hog bend to touchdown (no buoyancy)

### Benefits of Lazy Wave Configuration:
- Decouples vessel motion from touchdown point
- Reduces dynamic stress at touchdown
- Provides compliance for vessel offset
- Absorbs heave motion through wave shape

## Key Technical Findings

### 1. Multi-Section Line Configuration

Lazy wave risers require multiple line sections:

```yaml
LineType, Length, TargetSegmentLength:
  - [LWR_12inch, 400, 10]           # Upper catenary
  - [LWR_12inch_Buoyancy, 200, 5]   # Buoyancy section
  - [LWR_12inch, 600, 10]           # Lower catenary
```

### 2. Buoyancy Section Properties

The buoyancy section has:
- Larger OD (buoyancy modules attached)
- Same ID as base riser
- Lower mass per unit length (net buoyancy)

```yaml
# Buoyancy line type
OD: 0.85              # Increased due to modules
ID: 0.2909            # Same as base riser
MassPerUnitLength: 45 # Lower than displaced water
```

### 3. IncludeFile at Object Level Only

```yaml
# CORRECT - IncludeFile at object property level
LineTypes:
  - Name: LWR_12inch
    Category: General
    IncludeFile: ../../../../library/line_types/lwr_12inch.yml
```

## Usage

### Basic Analysis (Base Model)

```bash
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('base/lwr_base.yml')
model.CalculateStatics()
"
```

### Parametric Study (BaseFile + Variation)

```yaml
# cases/case_deep_water.yml
BaseFile: ../base/lwr_base.yml
IncludeFile: ../variations/deep_water_1500m.yml
```

```bash
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('cases/case_deep_water.yml')
model.CalculateStatics()
"
```

## Configuration Summary

### Base Model (1000m)
| Parameter | Value |
|-----------|-------|
| Water depth | 1000 m |
| Riser type | LWR 12-inch X65 |
| Upper catenary | 400 m |
| Buoyancy section | 200 m |
| Lower catenary | 600 m |
| Total length | 1200 m |

### Deep Water Variation (1500m)
| Parameter | Value |
|-----------|-------|
| Water depth | 1500 m |
| Upper catenary | 600 m |
| Buoyancy section | 300 m |
| Lower catenary | 900 m |
| Total length | 1800 m |

### Extended Buoyancy Variation
| Parameter | Value |
|-----------|-------|
| Buoyancy section | 350 m (vs 200m base) |
| Greater motion decoupling |

## Library Components Used

- `library/line_types/lwr_12inch.yml` - 12-inch X65 lazy wave riser (base)
- `library/line_types/lwr_12inch_buoyancy.yml` - 12-inch with buoyancy modules

## Design Considerations

### Buoyancy Section Sizing
- Length: Typically 15-25% of total riser length
- Net buoyancy: Sufficient to create wave shape
- Module spacing: Affects drag and VIV response

### Geometry Parameters
- Sag bend depth: Below vessel draft
- Hog bend elevation: Controls wave amplitude
- Touchdown distance: Affected by buoyancy section length

## Related Documentation

- SCR Template: `templates/risers/scr_hybrid/`
- CALM Buoy Template: `templates/mooring_systems/calm_buoy_hybrid/`
- Library: `docs/modules/orcaflex/library/`

