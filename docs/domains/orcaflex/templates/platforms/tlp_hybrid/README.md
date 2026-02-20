# TLP Hybrid Template

## Overview

This template demonstrates the **validated hybrid approach** for OrcaFlex Tension Leg Platform (TLP) models, combining:
- **Object-level IncludeFile** for reusable library components (LineTypes)
- **BaseFile + IncludeFile** for parametric variations (water depth, platform size)

## Directory Structure

```
tlp_hybrid/
├── README.md                         # This file
├── base/
│   └── tlp_base.yml                  # Base model (1000m, 4-column)
├── variations/
│   ├── deep_water_1500m.yml          # Deep water configuration
│   └── mini_tlp.yml                  # Smaller TLP variant
└── cases/
    └── case_deep_water.yml           # Base + deep water
```

## TLP Configuration

4-column TLP with 8 tension legs:

```
Platform arrangement (plan view):

       +-----+           +-----+
       |Col 1|--[T]--[T]--|Col 2|
       +--+--+           +--+--+
          |                 |
          +-----------------+
                  |
          +-----------------+
          |                 |
       +--+--+           +--+--+
       |Col 3|--[T]--[T]--|Col 4|
       +-----+           +-----+

Tendon arrangement (side view):

  ========================== TLP Hull
    ||    ||    ||    ||
    ||    ||    ||    ||     Tendons (pretensioned)
    ||    ||    ||    ||
  ----[]----[]----[]----[]-- Seabed foundations
```

### Key Features:
- **Vertically compliant** - Heave restrained by tendon stiffness
- **Horizontally stiff** - Minimal surge/sway response
- **High pretension** - Tendons always in tension
- **Low heave/pitch/roll** - Ideal for dry tree risers

## Usage

### Basic Analysis (Base Model)

```bash
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('base/tlp_base.yml')
model.CalculateStatics()
"
```

### Parametric Study (BaseFile + Variation)

```yaml
# cases/case_deep_water.yml
BaseFile: ../base/tlp_base.yml
IncludeFile: ../variations/deep_water_1500m.yml
```

## Configuration Summary

### Base Model (1000m, 4-Column)
| Parameter | Value |
|-----------|-------|
| Water depth | 1000 m |
| Number of tendons | 8 (2 per corner) |
| Tendon type | 24-inch steel tube |
| Tendon length | 975 m |
| Column spacing | 60 m x 60 m |
| Hull length | 70 m |

### Deep Water Variation (1500m)
| Parameter | Value |
|-----------|-------|
| Water depth | 1500 m |
| Tendon length | 1475 m |
| Same tendon type | 24-inch steel tube |

### Mini-TLP Variation
| Parameter | Value |
|-----------|-------|
| Hull length | 45 m |
| Configuration | 3-column triangular |
| Number of tendons | 6 (2 per corner) |
| Column spacing | 20 m radius |

## TLP Types

### Conventional TLP (4-Column)
- Rectangular column arrangement
- 8 tendons (2 per corner)
- Large deck area
- Common for major developments

### Mini-TLP (3-Column)
- Triangular column arrangement
- 6 tendons (2 per corner)
- Smaller footprint
- Ideal for marginal fields
- Lower CAPEX

## Platform Properties

### 3DBuoy (Corner Connection) Properties
| Parameter | Base | Mini-TLP |
|-----------|------|----------|
| Height | 3 m | 2 m |
| Diameter | 8 m | 6 m |
| Volume | 200 m3 | 100 m3 |
| Mass | 200 tonnes | 100 tonnes |
| Location | -25 m | -18 m |

### Vessel Type Properties
| Parameter | Base | Mini-TLP |
|-----------|------|----------|
| Length | 70 m | 45 m |
| Symmetry | xz plane | xz plane |

## Library Components Used

- `library/line_types/tlp_tendon_24inch.yml` - 24-inch TLP tendon

## Design Considerations

### Static Analysis
- Tendon pretension balance
- Hull buoyancy vs tendon load
- Foundation reactions

### Dynamic Analysis
- Tendon springing (high frequency)
- Surge/sway offset
- Set-down effects
- Ringing response

### Critical Design Factors
- Tendon fatigue life
- Tendon slack prevention
- Deck clearance (air gap)
- Foundation capacity

## TLP Natural Periods

### Typical Values
| DOF | Period | Notes |
|-----|--------|-------|
| Surge | 60-120 s | Long period |
| Sway | 60-120 s | Long period |
| Heave | 2-4 s | Short (stiff) |
| Roll | 2-4 s | Short (stiff) |
| Pitch | 2-4 s | Short (stiff) |
| Yaw | 60-120 s | Long period |

## Comparison: TLP vs Semi-Sub vs FPSO

| Aspect | TLP | Semi-Sub | FPSO |
|--------|-----|----------|------|
| Heave motion | Very low | Moderate | High |
| Water depth | Deep | Moderate-Deep | Any |
| Dry trees | Excellent | Limited | No |
| Cost | High | Moderate | Moderate |
| Installation | Complex | Moderate | Simple |

## Related Templates

- FPSO: `templates/platforms/fpso_hybrid/`
- Semi-Submersible: `templates/platforms/semi_sub_hybrid/`
- TTR (Top Tensioned Riser): `templates/risers/ttr_hybrid/`
- Library: `docs/domains/orcaflex/library/`
