# Subsea Jumper/Flying Lead Hybrid Template

## Overview

This template demonstrates the **validated hybrid approach** for OrcaFlex subsea jumper models, combining:
- **Object-level IncludeFile** for reusable library components (LineTypes)
- **BaseFile + IncludeFile** for parametric variations (water depth, pipe type)

Subsea jumpers are short flexible or rigid connections between subsea structures such as manifolds, trees, PLETs, and PLEMs.

## Directory Structure

```
jumper_hybrid/
|-- README.md                         # This file
|-- base/
|   |-- jumper_base.yml               # Base model (500m depth, 6" flexible)
|-- variations/
|   |-- deep_water_1500m.yml          # Deep water variation
|   |-- rigid_jumper.yml              # Rigid steel pipe variation
|-- cases/
    |-- case_deep_water.yml           # BaseFile + deep water variation
    |-- case_rigid.yml                # BaseFile + rigid variation
```

## Jumper Configuration

Subsea jumpers connect fixed subsea structures with an M-shaped (sag) configuration:

```
Manifold                                    PLET/Tree
   |                                            |
   v                                            v
End A                                        End B
(Anchored)                                (Anchored)
   |                                            |
   |         ___________________________        |
   |________/                           \_______|
          M-Shape Sag (~5-10m typical)

---------------------------------------------------
                     Seabed
```

### Key Characteristics:
- **Typical length**: 20-100m
- **Horizontal span**: 20-80m (creates natural sag)
- **Water depths**: 100-3000m
- **Hub height**: 2-5m above seabed
- **Configurations**: M-shape (sag), W-shape (multiple sags)

### Typical Applications:
- **Production jumpers**: Tree to manifold
- **Export jumpers**: Manifold to PLET
- **Water/gas injection**: Injection header to tree
- **Flying leads**: Umbilical termination to tree

## Usage

### Basic Analysis (Base Model)

```bash
python -c "
import OrcFxAPI
model = OrcFxAPI.Model('base/jumper_base.yml')
model.CalculateStatics()
"
```

### Parametric Study (BaseFile + Variation)

```yaml
# cases/case_deep_water.yml
BaseFile: ../base/jumper_base.yml
IncludeFile: ../variations/deep_water_1500m.yml
```

## Configuration Summary

### Base Model (500m, 6" Flexible)
| Parameter | Value |
|-----------|-------|
| Water depth | 500 m |
| Jumper type | 6-inch flexible |
| Horizontal span | 50 m |
| Total length | 60 m |
| Hub elevation | 3 m above seabed |
| Both ends | Anchored |
| Configuration | M-shape |

### Deep Water Variation (1500m)
| Parameter | Value |
|-----------|-------|
| Water depth | 1500 m |
| Hub elevation | 3 m above seabed |
| All other parameters | Same as base |

### Rigid Jumper Variation
| Parameter | Value |
|-----------|-------|
| Jumper type | 10-inch pipe bundle |
| Total length | 65 m (longer for stiffness) |
| Water depth | 500 m (from base) |

## Library Components Used

- `library/line_types/flex_6inch.yml` - 6-inch flexible composite pipe
- `library/line_types/pipe_10inch_bundle.yml` - 10-inch pipe-in-pipe bundle

## Design Considerations

### Flexible vs Rigid Jumpers
| Type | Advantages | Disadvantages |
|------|------------|---------------|
| Flexible | Easy installation, absorbs movement | Lower pressure rating |
| Rigid | High pressure/temperature | Complex installation |

### M-Shape Configuration
- Natural catenary sag between anchored ends
- Accommodates thermal expansion
- Reduces end reactions
- Typical sag: 5-15% of span

### Sizing Guidelines
| Application | Typical Size | Type |
|-------------|--------------|------|
| Production | 4-8" | Flexible |
| Export | 8-12" | Flexible/Rigid |
| Injection | 4-6" | Flexible |
| High pressure | 6-12" | Rigid |

### Analysis Types
- **Static**: Equilibrium configuration
- **Dynamic**: VIV, slugging, seismic
- **Installation**: Lowering through splash zone
- **Thermal**: Expansion/contraction cycles

## Related Documentation

- Pipeline Templates: `templates/pipelines/`
- Umbilical Templates: `templates/umbilicals/`
- Library: `docs/modules/orcaflex/library/`

