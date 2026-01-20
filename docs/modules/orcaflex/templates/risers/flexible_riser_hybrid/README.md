# Flexible Riser Hybrid Template

## Overview

This template provides OrcaFlex models for **flexible risers** using the validated hybrid approach:
- **Object-level IncludeFile** for reusable library components (LineTypes)
- **BaseFile + IncludeFile** for parametric variations (water depth)

## Flexible Riser Characteristics

Flexible risers are unbonded or bonded flexible pipes used for dynamic applications:
- **Multiple layer construction**: carcass, pressure sheath, tensile armor, outer sheath
- **Lower bending stiffness** than steel risers (allows tighter bend radii)
- **CompressionIsLimited**: flexible pipes cannot sustain compression
- **Common configurations**: lazy wave, steep S, free-hanging

### Typical Specifications
| Parameter | Range |
|-----------|-------|
| Internal diameter | 6-16 inches |
| Water depth | 100-2000m |
| Operating pressure | Up to 10,000 psi |
| Temperature | Up to 130 deg C |

## Directory Structure

```
flexible_riser_hybrid/
├── README.md                         # This file
├── base/
│   └── flexible_riser_base.yml       # Base model (1000m, 8-inch)
├── variations/
│   ├── deep_water_1500m.yml          # Deep water variation
│   └── large_bore_12inch.yml         # 12-inch (reference only)
└── cases/
    ├── case_deep_water.yml           # BaseFile + deep water
    └── case_large_bore.yml           # Standalone 12-inch model
```

## Lazy Wave Configuration

The base model uses a lazy wave configuration:

```
FPSO ─────┐
          │  Upper Catenary
          │  (Flex_8inch)
          │
     Sag Bend ←─── Start of buoyancy section
          ╭─────╮
         ╱       ╲  Buoyancy Section
        ╱         ╲ (Flex_8inch_Buoyancy)
       ╱           ╲
  Hog Bend ←─── End of buoyancy section
          │
          │  Lower Catenary
          │  (Flex_8inch)
          │
─────────┴───────── Seabed (Touchdown Point)
```

### Benefits of Lazy Wave Configuration
- Decouples vessel motion from touchdown point
- Reduces dynamic stress at touchdown
- Provides compliance for vessel offset
- Absorbs heave motion through wave shape

## Usage

### Basic Analysis (Base Model)

```python
import OrcFxAPI
model = OrcFxAPI.Model('base/flexible_riser_base.yml')
model.CalculateStatics()
```

### Deep Water Study (BaseFile + Variation)

```python
import OrcFxAPI
model = OrcFxAPI.Model('cases/case_deep_water.yml')
model.CalculateStatics()
```

### Large Bore Analysis (Standalone Model)

```python
import OrcFxAPI
# Note: Uses standalone model due to LineType name change limitation
model = OrcFxAPI.Model('cases/case_large_bore.yml')
model.CalculateStatics()
```

## Configuration Summary

### Base Model (1000m, 8-inch)
| Parameter | Value |
|-----------|-------|
| Water depth | 1000 m |
| Riser type | 8-inch flexible |
| Upper catenary | 350 m |
| Buoyancy section | 180 m |
| Lower catenary | 550 m |
| Total length | 1080 m |

### Deep Water Variation (1500m)
| Parameter | Value |
|-----------|-------|
| Water depth | 1500 m |
| Upper catenary | 550 m |
| Buoyancy section | 270 m |
| Lower catenary | 850 m |
| Total length | 1670 m |

### Large Bore Variation (12-inch)
| Parameter | Value |
|-----------|-------|
| Water depth | 1000 m |
| Riser type | 12-inch flexible |
| Upper catenary | 350 m |
| Buoyancy section | 180 m |
| Lower catenary | 550 m |
| Total length | 1080 m |

## Library Components Used

- `library/line_types/flex_8inch.yml` - 8-inch flexible pipe
- `library/line_types/flex_8inch_buoyancy.yml` - 8-inch with buoyancy modules
- `library/line_types/flex_12inch.yml` - 12-inch flexible pipe
- `library/line_types/flex_12inch_buoyancy.yml` - 12-inch with buoyancy modules

## Design Considerations

### LineType Variation Limitation

When changing LineType names (e.g., from 8-inch to 12-inch), the BaseFile + IncludeFile approach cannot be used because:
1. The Line object references the original LineType name
2. OrcaFlex replaces entire object definitions, not individual properties
3. The new LineType name would not be recognized by the Line

**Solution**: Use standalone models for diameter changes (see `case_large_bore.yml`).

### Buoyancy Section Sizing
- Length: Typically 15-25% of total riser length
- Net buoyancy: Sufficient to create wave shape
- Module spacing: Affects drag and VIV response

### Flexible Pipe Properties
- Lower EA than steel (allows axial strain)
- Much lower EI than steel (allows tight bends)
- Non-zero GJ for torsional behavior
- CompressionIsLimited: Yes (always)

## Related Templates

- Lazy Wave Riser (Steel): `templates/risers/lazy_wave_hybrid/`
- SCR (Steel Catenary): `templates/risers/scr_hybrid/`
- TTR (Top Tensioned): `templates/risers/ttr_hybrid/`
