# OrcaFlex Jumper Analysis Skill

## Description
Rigid and flexible jumper modelling in OrcaFlex — covers installation analysis (crane lift, lowering, landing), in-place analysis (VIV, fatigue, extreme response), and parametric studies across environmental headings and sea states.

## When to Use
- Jumper installation analysis (SZ/DZ, AHC on/off)
- Rigid jumper stress analysis and VIV screening
- Flexible jumper fatigue assessment
- Multi-section pipe modelling with buoyancy modules
- Three-point lift rigging design
- Parametric heading/sea-state studies

## Key Concepts

### Multi-Section Pipe
- Jumpers typically have 15-25 OrcaFlex line sections with alternating line types
- Line types: bare coated pipe, buoyancy modules, strake sections, insulation
- Each section has distinct OD, wall thickness, mass, and bending stiffness
- Section lengths vary: short connector sections (1-2m) to long pipe runs (50-100m)

### M-Shape Buoyancy Layout
- Mid-span buoyancy modules create characteristic M-shape in water column
- Module dimensions: typically 10m blocks, density ~0.694 te/m3
- Buoyancy placement defined by arc-length ranges along jumper
- Net buoyancy per module determines equilibrium shape

### Rigid End Connectors
- Modelled as separate short OrcaFlex lines with very high bending stiffness
- Typical: OCS 200-V connectors, OD ~1.8m, length ~0.5-1.0m
- `EndBxBendingStiffness: 1.0e+307` (effectively rigid)
- Connected to main jumper via end connections

### Installation Rigging Chain
The full lift system from vessel to jumper, modelled as linked OrcaFlex objects:
1. **Vessel** — Installation vessel with RAOs
2. **Crane Pedestal** — 6DBuoy at vessel crane location
3. **Crane Boom** — Constraint object (boom geometry)
4. **Crane Wire** — Winch object (main hoist)
5. **Sling** — Line from winch to masterlink
6. **Masterlink** — 3DBuoy (central connection point)
7. **Slings** — Lines from masterlink to spreader bar ends
8. **Spreader Bar** — 6DBuoy (~120ft / 36.6m)
9. **Lift Slings + Turnbuckles** — Lines from spreader bar to clamp points
10. **Clamps** — Attached to jumper at pickup arc lengths

### Three-Point Lift
- Spreader bar with asymmetric pickup at 3 arc-length positions
- Pickup points determined by jumper COG and weight distribution
- Typical: 5 clamps (10-inch jumper clamps, ~0.26 te each) at sling points
- COG calculation considers all KIT weights (~46 te total for 4 KITs)

### AHC System (Active Heave Compensation)
- Modelled via Winch + ExternalFunction (DLL)
- Two analysis variants: AHC-on (DZ) and AHC-off (SZ)
- ExternalFunction64.dll provides real-time heave compensation
- AHC reduces dynamic tension variation during lowering

### Dual-Zone Analysis
| Zone | Depth | Wave Theory | Typical Hs | Focus |
|------|-------|-------------|------------|-------|
| **SZ** (Splash Zone) | Surface | JONSWAP | 1.5m | Sling loads, vessel motion |
| **DZ** (Deep Zone) | Near seabed | Dean Stream | 2.0m | Landing loads, clearance |

### Coatings & Insulation
| Type | Density (te/m3) | Thickness (mm) | Purpose |
|------|----------------|----------------|---------|
| Insulation | 0.979 | 76.2 | Thermal |
| Buoyancy | 0.694 | 343 | Net uplift |
| Strake | 1.128 | 5 | VIV suppression |

### Two-Step Statics
- **Step 1**: User-specified starting positions (rigging geometry)
- **Step 2**: Full statics solve (catenary + equilibrium)
- Critical for installation models where initial geometry is non-trivial

### Parametric Studies
- Environmental headings: 0, 30, 60, 90, 120, 150, 165, 180 degrees
- Sea states: Hs = 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.5m
- Seeds: 20+ random seeds per (heading, Hs) combination
- Total runs per jumper: 200-1000+ simulations

## Model Library

### Available Models
| Model | Location | Description |
|-------|----------|-------------|
| Manifold-to-PLET | `docs/modules/orcaflex/jumper/manifold_to_plet/` | Full installation rigging |
| PLET-to-PLEM | `docs/modules/orcaflex/jumper/plet_to_plem/` | Shorter jumper variant |
| SUT/MM | `docs/modules/orcaflex/jumper/sut_mm/` | SZ/DZ/resonance variants |

### File Structure
```
docs/modules/orcaflex/jumper/<model>/
├── monolithic/          # Sanitized original OrcaFlex YAML
│   ├── DZ_AHCoff.yml
│   ├── SZ.yml
│   └── ...
└── spec.yml             # Extracted spec for modular builder
```

## Commands

### Generate from spec
```bash
uv run python -m digitalmodel.solvers.orcaflex.modular_generator --spec docs/modules/orcaflex/jumper/manifold_to_plet/spec.yml
```

### Validate round-trip
```bash
uv run python scripts/semantic_validate.py \
  --mono docs/modules/orcaflex/jumper/manifold_to_plet/monolithic/SZ.yml \
  --modular output/generated_model.yml
```

### Run benchmark
```bash
uv run python scripts/benchmark_model_library.py --library-only --three-way --skip-mesh
```

## Implementation Notes

- Jumper models use the `generic` field in `ProjectInputSpec` (not dedicated jumper schema)
- The `MonolithicExtractor` handles arbitrary OrcaFlex object types including 3DBuoys, 6DBuoys, Constraints, Winches
- Vessel RAO data round-trips through the extractor (verify RAO table sizes)
- ExternalFunction DLL references will cause benchmark failures — add to skip list
- For parametric studies, use the campaign generator pattern with seed/heading/Hs variations

## Related Skills
- `/orcaflex-installation-analysis` — General installation modelling
- `/orcaflex-model-generator` — Modular YAML generation from spec
- `/orcaflex-monolithic-to-modular` — Conversion workflow
- `/orcaflex-environment-config` — Environmental conditions setup
- `/orcaflex-vessel-setup` — Vessel RAO import and configuration
