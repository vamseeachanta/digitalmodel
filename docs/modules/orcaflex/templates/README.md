# OrcaFlex Hybrid Templates

**Status**: ✅ 11 Validated Templates | 58 Automated Tests | 33 Library Components

Validated OrcaFlex model templates using the hybrid approach: object-level IncludeFile for library components + BaseFile/IncludeFile for parametric variations.

---

## Quick Start

```python
import OrcFxAPI

# Load a template directly
model = OrcFxAPI.Model('risers/scr_hybrid/base/scr_base.yml')

# Or load a case (base + variation)
model = OrcFxAPI.Model('risers/scr_hybrid/cases/case_deep_water.yml')

# Run analysis
model.CalculateStatics()
```

---

## Available Templates

### Risers (4 templates)

| Template | Water Depth | Description | Variations |
|----------|-------------|-------------|------------|
| [SCR Hybrid](risers/scr_hybrid/) | 1200m | Steel Catenary Riser, 10" X65 | 1500m deep, 12" pipe |
| [Lazy Wave Hybrid](risers/lazy_wave_hybrid/) | 1000m | Lazy Wave with buoyancy section | 1200m deep |
| [Pliant Wave Hybrid](risers/pliant_wave_hybrid/) | 800m | Pliant Wave configuration | 1200m deep |
| [TTR Hybrid](risers/ttr_hybrid/) | 1500m | Top Tensioned Riser | TBD |

### Pipelines (1 template)

| Template | Water Depth | Description | Variations |
|----------|-------------|-------------|------------|
| [Pipeline Hybrid](pipelines/pipeline_hybrid/) | 500m | Seabed pipeline, 16" X65 | 1000m deep, 20" trunk, 12" flowline |

### Umbilicals (1 template)

| Template | Water Depth | Description | Variations |
|----------|-------------|-------------|------------|
| [Umbilical Hybrid](umbilicals/umbilical_hybrid/) | 800m | Dynamic flexible umbilical | 1200m deep, steel tube |

### Mooring Systems (4 templates)

| Template | Water Depth | Description | Variations |
|----------|-------------|-------------|------------|
| [CALM Buoy Hybrid](mooring_systems/calm_buoy_hybrid/) | 100m | 6-leg CALM buoy mooring | 200m deep |
| [SALM Hybrid](mooring_systems/salm_hybrid/) | 150m | Single Anchor Leg Mooring | TBD |
| [Spread Mooring Hybrid](mooring_systems/spread_mooring_hybrid/) | 200m | 8-leg spread mooring for FPSO | 500m deep, 12-leg |
| [Turret Mooring Hybrid](mooring_systems/turret_mooring_hybrid/) | 300m | Internal turret with weathervaning | 600m deep, external turret |

---

## Template Structure

All hybrid templates follow this structure:

```
template_name_hybrid/
├── README.md                  # Template documentation
├── base/
│   └── *_base.yml            # Full model with library includes
├── variations/
│   └── *.yml                 # Override files (water depth, type, etc.)
└── cases/
    └── case_*.yml            # BaseFile + IncludeFile combinations
```

---

## Library Components

Templates use shared components from the library:

| Category | Count | Location |
|----------|-------|----------|
| Line Types | 21 | `../library/line_types/` |
| Buoy Types | 12 | `../library/buoy_types/` |

### Line Type Examples
- `chain_84mm_r4.yml` - 84mm R4 studless chain
- `scr_10inch_x65.yml` - 10" X65 steel riser
- `pwr_10inch.yml` - Pliant wave riser pipe
- `umb_flex_dynamic.yml` - Dynamic flexible umbilical

### Buoy Type Examples
- `calm_12m_100m.yml` - 12m CALM buoy for 100m depth
- `spm_10m.yml` - 10m single point mooring buoy

---

## Key Concepts

### Hybrid Approach

1. **Object-level IncludeFile**: Library components included at property level
   ```yaml
   LineTypes:
     - Name: Chain_84mm_R4
       Category: General
       IncludeFile: ../../../../library/line_types/chain_84mm_r4.yml
   ```

2. **BaseFile + IncludeFile**: Parametric variations
   ```yaml
   # case_deep_water.yml
   BaseFile: ../base/model_base.yml
   IncludeFile: ../variations/deep_water.yml
   ```

### Important Notes

- OrcaFlex **replaces** objects by name, does **not merge** properties
- Variation files must include **all** properties for overridden objects
- Library files have **no section headers** (properties only)
- Lines use multi-column format: `LineType, Length, TargetSegmentLength:`

---

## Validation

All templates are validated with:
- Static analysis convergence
- 58 automated pytest tests

Run validation:
```bash
uv run pytest tests/modules/orcaflex/test_hybrid_templates.py -v
```

### Test Coverage

| Test Category | Count |
|---------------|-------|
| Base model loading | 7 |
| Base model convergence | 7 |
| Case file convergence | 14 |
| Library validation | 24 |
| Structure conventions | 4 |
| **Total** | **58** |

---

## Creating New Templates

1. Copy an existing template as starting point
2. Modify base model for your configuration
3. Create variations for parametric studies
4. Create case files combining base + variations
5. Add README.md documentation
6. Run validation tests

### Template Checklist

- [ ] `base/*.yml` - Complete OrcaFlex model with library includes
- [ ] `variations/*.yml` - At least one variation file
- [ ] `cases/*.yml` - At least one case file
- [ ] `README.md` - Documentation with:
  - Configuration summary table
  - Usage examples
  - Design considerations

---

## Directory Structure

```
templates/
├── README.md                            # This file
├── risers/
│   ├── scr_hybrid/                     # ✅ Validated
│   ├── lazy_wave_hybrid/               # ✅ Validated
│   ├── pliant_wave_hybrid/             # ✅ Validated
│   └── ttr_hybrid/                     # ✅ Validated
├── pipelines/
│   └── pipeline_hybrid/                # ✅ Validated
├── umbilicals/
│   └── umbilical_hybrid/               # ✅ Validated
└── mooring_systems/
    ├── calm_buoy_hybrid/               # ✅ Validated
    ├── salm_hybrid/                    # ✅ Validated
    ├── spread_mooring_hybrid/          # ✅ Validated
    └── turret_mooring_hybrid/          # ✅ Validated
```

---

## Related Documentation

- [Library Components](../library/)
- [OrcaFlex Skills](../../../../.claude/skills/)
- [Template Tests](../../../../tests/modules/orcaflex/test_hybrid_templates.py)

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 2.0.0 | 2026-01-19 | Validated hybrid templates with test suite |
| 1.0.0 | 2026-01-02 | Initial template library structure |

---

**Version**: 2.0.0
**Last Updated**: 2026-01-19
**Status**: ✅ Production Ready
