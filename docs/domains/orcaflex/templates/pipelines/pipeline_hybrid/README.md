# Pipeline Hybrid Template

## Overview

This template demonstrates the **validated hybrid approach** for OrcaFlex seabed pipeline models, combining:
- **Object-level IncludeFile** for reusable library components (LineTypes)
- **BaseFile + IncludeFile** for parametric variations (water depth, pipe size)

## Directory Structure

```
pipeline_hybrid/
├── README.md                         # This file
├── base/
│   └── pipeline_base.yml             # Base model (500m depth, 16")
├── variations/
│   ├── deep_water_1000m.yml          # Deep water variation
│   ├── pipe_20inch.yml               # 20-inch trunk line
│   └── pipe_12inch_flowline.yml      # 12-inch flowline
└── cases/
    ├── case_deep_water.yml
    ├── case_20inch.yml
    └── case_12inch_flowline.yml
```

## Pipeline Configuration

Seabed pipelines are modeled as lines with both ends anchored:

```
Platform/PLEM ──────────────────────── Manifold
     │                                      │
     ▼                                      ▼
  End A                                  End B
(Anchored)     ══════════════════     (Anchored)
               Pipeline on Seabed
─────────────────────────────────────────────────
                   Seabed
```

### Typical Applications:
- **Export lines**: Platform to shore/tanker
- **Flowlines**: Wellhead to manifold
- **Trunk lines**: Manifold to processing facility
- **Water/gas injection lines**

## Usage

### Basic Analysis (Base Model)

```bash
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('base/pipeline_base.yml')
model.CalculateStatics()
"
```

### Parametric Study (BaseFile + Variation)

```yaml
# cases/case_deep_water.yml
BaseFile: ../base/pipeline_base.yml
IncludeFile: ../variations/deep_water_1000m.yml
```

## Configuration Summary

### Base Model (500m, 16")
| Parameter | Value |
|-----------|-------|
| Water depth | 500 m |
| Pipeline type | 16-inch X65 |
| Pipeline length | 2000 m |
| Both ends | Anchored |

### Deep Water Variation (1000m)
| Parameter | Value |
|-----------|-------|
| Water depth | 1000 m |
| Pipeline length | 3000 m |

### Pipe Size Variations
| Variation | Size | Application |
|-----------|------|-------------|
| Base | 16" X65 | Standard export |
| 20" X70 | 20" X70 | High capacity trunk |
| 12" X65 | 12" X65 | Flowline |

## Library Components Used

- `library/line_types/pipe_16inch_x65.yml` - 16-inch X65 export pipeline
- `library/line_types/pipe_20inch_x70.yml` - 20-inch X70 trunk line
- `library/line_types/pipe_12inch_x65.yml` - 12-inch X65 flowline

## Design Considerations

### Seabed Interaction
- Normal stiffness: Resistance to vertical penetration
- Shear stiffness: Resistance to lateral/axial movement
- Friction coefficients: Important for lateral buckling

### Pipeline Sizing
| Size | Typical Use | Flow Capacity |
|------|-------------|---------------|
| 10-12" | Flowlines | Low-medium |
| 14-18" | Export lines | Medium-high |
| 20-24" | Trunk lines | High |

### Analysis Types
- **Static**: Equilibrium under weight and pressure
- **Dynamic**: Response to waves, currents, slugging
- **Lateral buckling**: Thermal expansion effects
- **Walking**: Cyclic thermal loading

## Related Documentation

- Riser Templates: `templates/risers/`
- Mooring Templates: `templates/mooring_systems/`
- Library: `docs/domains/orcaflex/library/`

