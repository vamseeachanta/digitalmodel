# WRK-032 Research: Modular Pipeline Installation Input

> Phase 0 artifact | Generated: 2026-02-03

## Existing Architecture

### Schema Layer (`modular_generator/schema/`)

| Model | File | Key Fields |
|-------|------|------------|
| `ProjectInputSpec` | root.py | metadata, environment, pipeline (singular), equipment, simulation |
| `Pipeline` | pipeline.py | name, material, dimensions, coatings, segments[] |
| `Segment` | pipeline.py | type, length, segment_length, start_arc_length |
| `Equipment` | equipment.py | tugs, rollers, buoyancy_modules, ramps[], vessel, stinger, tensioner |
| `Rollers` | equipment.py | position[3], supports (int), support_type (str) |
| `Environment` | environment.py | water, seabed, waves, current, wind |
| `Simulation` | simulation.py | time_step, stages[], north_direction |

**Gap**: `ProjectInputSpec` supports exactly one pipeline + one equipment config. No campaign matrix. Rollers model is minimal (single position, no geometric parametrisation).

### Builder Layer (`modular_generator/builders/`)

17 files total. Execution order via `@BuilderRegistry.register(file, order=XX)`:

```
10 GeneralBuilder    → 01_general.yml
20 VarDataBuilder    → 02_var_data.yml
30 EnvironmentBuilder→ 03_environment.yml
35 VesselTypeBuilder → 04_vessel_types.yml (S-lay only)
40 LineTypeBuilder   → 05_linetypes.yml
45 VesselBuilder     → 06_vessels.yml (S-lay only)
50 SupportsBuilder   → 13_supports.yml
60 MorisonBuilder    → 14_morison.yml
70 ShapesBuilder     → 09_shapes.yml
80 BuoysBuilder      → 08_buoys.yml
90 LinesBuilder      → 07_lines.yml
95 WinchBuilder      → 11_winches.yml (S-lay only)
100 GroupsBuilder    → 10_groups.yml
```

**Key patterns**: Registry decorator, `BuilderContext` for cross-builder entity sharing, conditional generation via `should_generate()`.

### Existing Include-File Convention

From the CALM buoy modular spec (`specs/modules/orcaflex/modular-input-file/output/`):

**Naming**: `_XX[a-z]_<descriptive_name>[_variant].yml`

| Number | Section | Sub-Divisions |
|--------|---------|---------------|
| 01 | General | a=units, b=statics, c=dynamics, d=stages, e=view |
| 02 | VariableData | Single |
| 03 | Environment | a=sea, b=seabed, c=waves, d=current, e=wind |
| 04 | VesselTypes | Single |
| 05 | LineTypes | Single |
| 06 | Vessels/Buoys | + discretised variant |
| 07 | Lines | + discretised variant |
| 08 | Groups | + discretised variant |

Sections composed via OrcaFlex's `includefile` mechanism in a master YAML.

### Roller Generation (Current)

`BuoysBuilder._build_roller()` generates a 6D buoy with:
- 4 hardcoded support positions forming a V-shape
- Positions: `[17.5, 3.5, -1.6]`, `[-21.5, 2.5, -1.6]` (mirrored)
- Support count limited by `rollers.supports`
- Support type from `rollers.support_type`
- Zero mass, zero hydrodynamics (passive structure)

**Gap**: No parametric roller geometry. V-angle, diameter, friction, spacing between roller stations not configurable.

### Existing Input Examples

**Floating** (`docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/spec.yml`):
- 30" pipeline, X65, 5 tugs, 4-support roller, BMs at 3.9m spacing
- Single water depth (8m), single environment

**S-lay** (`docs/modules/orcaflex/pipeline/installation/s-lay/SB-SA/spec.yml`):
- 10" pipeline, Eclipse vessel, stinger R=150m, tensioner 490kN
- Single water depth, single environment

### Legacy Module (`orcaflex_installation.py`)

`OrcInstallation` class with 3 methods:
- `create_model_for_water_depth()` — iterates `delta_elevations`, updates 6DBuoys/3DBuoys InitialZ, adjusts line lengths
- `create_installation_depth_model()` — similar, for structure deployment
- `create_installation_depth_model2()` — variant handling full Line object redefinition

Uses `OrderedDict` + raw YAML manipulation. Not integrated with Pydantic schema.

**Reusable concept**: elevation-based parametric generation pattern. Will be absorbed into campaign matrix.

## What Needs to Change

1. **Schema**: Extend for campaign mode, roller arrangement, section registry
2. **Builders**: New `RollerArrangementBuilder` for multi-roller stations; extend `BuoysBuilder` and `LinesBuilder` for parametric support
3. **New module**: `CampaignGenerator` — reads campaign matrix, generates cartesian product of run configs, invokes `ModularModelGenerator` per combination
4. **Section composition**: Variable substitution engine for `${water_depth}` etc. in section templates
5. **Backward compatibility**: Single-run `ProjectInputSpec` still works as-is
