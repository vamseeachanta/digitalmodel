# CALM Buoy Hybrid Template

## Overview

This template demonstrates the **validated hybrid approach** for OrcaFlex model management, combining:
- **Object-level IncludeFile** for reusable library components (LineTypes, BuoyTypes)
- **BaseFile + IncludeFile** for parametric variations

## Directory Structure

```
calm_buoy_hybrid/
├── README.md                    # This file
├── base/
│   └── calm_buoy_base.yml       # Base model (100m water depth)
├── variations/
│   └── deep_water_200m.yml      # Deep water variation (200m)
└── cases/
    └── case_deep_water.yml      # Case file combining base + variation
```

## Key Technical Findings (Validated January 2026)

### 1. IncludeFile Works at Object Level Only

```yaml
# CORRECT - IncludeFile at object property level
LineTypes:
  - Name: Chain_84mm
    Category: General                              # Required BEFORE IncludeFile
    IncludeFile: ../../../../library/line_types/chain_84mm_r4.yml

# INCORRECT - Section-level includes NOT SUPPORTED
- includefile: includes/01_general.yml           # Does NOT work
```

### 2. Variations Must Include Complete Object Definitions

When using `BaseFile + IncludeFile` for variations, OrcaFlex **replaces** objects by name, it does NOT patch/merge properties.

```yaml
# variation.yml - Must include ALL line properties
Lines:
  - Name: Mooring_1
    EndAConnection: CALM_Buoy    # Must repeat ALL properties
    EndAX: 6.0
    EndAY: 0
    EndAZ: -2.25
    EndBConnection: Anchored
    EndBX: 600                   # Only this changed
    EndBY: 0
    EndBZ: -200                  # Only this changed
    IncludedInStatics: Yes
    LineType, Length, TargetSegmentLength:
      - [Chain_84mm, 700, 10]    # Only this changed
```

### 3. Lines Must Use Multi-Column Format

```yaml
# CORRECT
LineType, Length, TargetSegmentLength:
  - [Chain_84mm, 350, 10]

# INCORRECT - Indexed format causes errors
LineType: Chain_84mm
Length[1]: 350
TargetSegmentLength[1]: 10
```

### 4. Library Files Must Have No Section Headers

```yaml
# library/line_types/chain_84mm_r4.yml
# CORRECT - Properties only
OD: 0.084
MassPerUnitLength: 0.088
EA: 612000
...

# INCORRECT - With section header
LineTypes:
  - OD: 0.084
    ...
```

## Usage

### Basic Analysis (Base Model)

```bash
# Load and run base model directly
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('base/calm_buoy_base.yml')
model.CalculateStatics()
"
```

### Parametric Study (BaseFile + Variation)

```yaml
# cases/case_deep_water.yml
BaseFile: ../base/calm_buoy_base.yml
IncludeFile: ../variations/deep_water_200m.yml
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

### Base Model (100m)
| Parameter | Value |
|-----------|-------|
| Water depth | 100 m |
| Number of legs | 6 |
| Anchor radius | 300 m |
| Line length | 350 m |
| Chain type | 84mm R4 |

### Deep Water Variation (200m)
| Parameter | Value |
|-----------|-------|
| Water depth | 200 m |
| Anchor radius | 600 m |
| Line length | 700 m |

## Library Components Used

- `library/line_types/chain_84mm_r4.yml` - 84mm R4 studless chain
- `library/line_types/hawser_hmpe_80mm.yml` - 80mm HMPE hawser
- `library/buoy_types/calm_12m_100m.yml` - 12m diameter CALM buoy

## Recommendations for New Models

1. **Use library IncludeFile** for equipment standardization
2. **Use flat YAML** for simple single-use models
3. **Use BaseFile + variation** only when complete object overrides are needed
4. **Don't batch convert existing flat files** - Keep them as reference examples

## Related Documentation

- Plan: `specs/modules/orcaflex/model-preparation/plan.md`
- Library: `docs/domains/orcaflex/library/`
- Original C06 Example: `docs/domains/orcaflex/examples/yml/C06/`
