# SCR Hybrid Template

## Overview

This template demonstrates the **validated hybrid approach** for OrcaFlex SCR (Steel Catenary Riser) models, combining:
- **Object-level IncludeFile** for reusable library components (LineTypes)
- **BaseFile + IncludeFile** for parametric variations (water depth, riser size)

## Directory Structure

```
scr_hybrid/
├── README.md                    # This file
├── base/
│   └── scr_base.yml             # Base model (1200m water depth, 10-inch)
├── variations/
│   ├── deep_water_1500m.yml     # Deep water variation (1500m)
│   └── scr_12inch.yml           # 12-inch riser variation
└── cases/
    ├── case_deep_water.yml      # Case file: 1500m depth
    └── case_12inch.yml          # Case file: 12-inch riser
```

## Key Technical Findings

### 1. IncludeFile Works at Object Level Only

```yaml
# CORRECT - IncludeFile at object property level
LineTypes:
  - Name: SCR_10inch_X65
    Category: General                              # Required BEFORE IncludeFile
    IncludeFile: ../../../../../library/line_types/scr_10inch_x65.yml
```

### 2. Variations Must Include Complete Object Definitions

When using `BaseFile + IncludeFile` for variations, OrcaFlex **replaces** objects by name, it does NOT patch/merge properties.

```yaml
# variation.yml - Must include ALL line properties
Lines:
  - Name: SCR_Main
    EndAConnection: FPSO    # Must repeat ALL properties
    EndAX: -140
    EndAY: 0
    EndAZ: -20
    EndBConnection: Anchored
    EndBX: -2300            # Only this changed
    EndBZ: -1500            # Only this changed
    IncludedInStatics: Yes
    StaticsStep1: Catenary
    LineType, Length, TargetSegmentLength:
      - [SCR_10inch_X65, 2800, 10]    # Only this changed
```

### 3. Lines Must Use Multi-Column Format

```yaml
# CORRECT
LineType, Length, TargetSegmentLength:
  - [SCR_10inch_X65, 2200, 10]

# INCORRECT - Indexed format causes errors
LineType: SCR_10inch_X65
Length[1]: 2200
TargetSegmentLength[1]: 10
```

## Usage

### Basic Analysis (Base Model)

```bash
# Load and run base model directly
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('base/scr_base.yml')
model.CalculateStatics()
"
```

### Parametric Study (BaseFile + Variation)

```yaml
# cases/case_deep_water.yml
BaseFile: ../base/scr_base.yml
IncludeFile: ../variations/deep_water_1500m.yml
```

```bash
# Load combined case
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('cases/case_deep_water.yml')
model.CalculateStatics()
"
```

## Configuration Summary

### Base Model (1200m)
| Parameter | Value |
|-----------|-------|
| Water depth | 1200 m |
| Riser type | SCR 10-inch X65 |
| Riser length | 2200 m |
| Touchdown distance | ~1800 m |
| Hangoff angle | ~15° |

### Deep Water Variation (1500m)
| Parameter | Value |
|-----------|-------|
| Water depth | 1500 m |
| Riser length | 2800 m |
| Touchdown distance | ~2300 m |

### 12-inch Variation
| Parameter | Value |
|-----------|-------|
| Riser type | SCR 12-inch X65 |
| Higher capacity for larger flowrate |

## Library Components Used

- `library/line_types/scr_10inch_x65.yml` - 10-inch X65 steel catenary riser
- `library/line_types/scr_12inch_x65.yml` - 12-inch X65 steel catenary riser

## Riser Design Notes

### Catenary Geometry
- **Hangoff**: Top end connected to FPSO at draft level
- **Touchdown point (TDP)**: Where riser contacts seabed
- **Catenary equation**: y = a*cosh(x/a) where a = H/w (tension/weight)

### Typical Parameters
- Hangoff angle: 10-20° from vertical
- Top tension: 2-4 times riser weight
- TDP stress: Critical design parameter

## Related Documentation

- CALM Buoy Template: `templates/mooring_systems/calm_buoy_hybrid/`
- Library: `docs/domains/orcaflex/library/`
- Model Preparation Plan: `specs/modules/orcaflex/model-preparation/plan.md`

