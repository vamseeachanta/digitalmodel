# OrcaFlex Property Routing Map

Property routing defines how engineering domain specifications are transformed into
OrcaFlex model objects. Each route maps domain concepts (pipe dimensions, mooring
chain grades, vessel hydrodynamics) to OrcaFlex YAML sections.

## Architecture

```
Engineering Spec ──► Domain Router ──► GenericModel dict ──► GenericModelBuilder ──► OrcaFlex YAML
                     (upstream)        (intermediate)        (existing)              (output)
```

Three routing paths converge on `ProjectInputSpec.generic` → `GenericModelBuilder`:

```
Route A: Pipeline ──► PipelineSpec ──► LineTypeBuilder ──────────► OrcaFlex YAML
                                       (dedicated builder)

Route B: Mooring  ──► MooringSystem ──► MooringRouter.route() ──► GenericModel dict ──► GenericModelBuilder
                                        (new router)

Route C: Vessel   ──► HullCatalog ──► VesselRouter.route() ──► GenericModel dict ──► GenericModelBuilder
                      + RAO files      (future)
```

---

## Route A — Pipeline (existing)

Pipeline routing uses dedicated builders with explicit property transformations.

### Source Schema
- `schema/pipeline.py` → `Pipeline.dimensions` (OD, wall thickness)
- `schema/pipeline.py` → `Pipeline.coatings` (coating stack with thickness, density)
- `schema/pipeline.py` → `Pipeline.material` (steel grade, yield stress, E, nu)

### Transformation
`LineTypeBuilder` (order=40) calculates OrcaFlex properties from engineering inputs:
- **OD**: Sum of pipe OD + 2× each coating thickness
- **ID**: Pipe inner diameter
- **MassPerUnitLength**: Steel mass + coating mass contributions
- **EA**: E × A_steel (axial stiffness from material + cross-section)
- **EI**: E × I_steel (bending stiffness from material + moment of inertia)

### Output Sections
- `LineTypes` → Homogeneous Pipe category with computed properties
- `Lines` → Pipeline route with sections, connections, seabed contact
- `Environment` → Water depth, wave, current, wind from spec

### Key Files
- `builders/linetype_builder.py` — coating stack → OD/mass calculation
- `builders/lines_builder.py` — route geometry → Line sections
- `builders/environment_builder.py` — metocean → Environment section

---

## Route B — Mooring Line (new, Phase 2B)

Mooring routing uses a domain router that transforms mooring engineering specs
into `GenericModel`-compatible dicts, which flow through the existing `GenericModelBuilder`.

### Source Schema
- `schema/mooring.py` → `MooringSystem.lines` (list of mooring lines)
- `schema/mooring.py` → `MooringSegment` (chain/wire/polyester with diameter, grade, length)
- `schema/mooring.py` → `MooringEndpoint` (anchor position, fairlead vessel connection)

### Transformation
`MooringRouter.route()` converts domain specs to OrcaFlex properties:

| Engineering Input | Router Logic | OrcaFlex Output |
|---|---|---|
| Chain grade (R3/R4/R5) + diameter | `CHAIN_DATABASE` lookup: MBL, mass, EA coefficients × d² | `LineTypes[].MassPerUnitLength`, `EA`, `AllowableTension` |
| Wire rope diameter + MBL | `WIRE_ROPE_DEFAULTS`: mass × d², EA = 0.45 × MBL | `LineTypes[].MassPerUnitLength`, `EA` |
| Segment type + diameter | Name generation: `chain_84mm_R3`, `wire_89mm` | `LineTypes[].Name` (deduplicated) |
| Segment list per line | Multi-section line definition | `Lines[].LineType, Length, NumberOfSegments` |
| Anchor position [x,y,z] | Direct mapping | `Lines[].EndAConnection=Fixed`, `EndAX/Y/Z` |
| Fairlead vessel + position | Vessel name reference | `Lines[].EndBConnection=<vessel>`, `EndBX/Y/Z` |
| Pretension (kN) | Creates winch with specified tension | `Winches[].WinchControl="Specified tension"` |

### Output Sections
- `line_types` → General category LineTypes with chain/wire properties
- `lines` → Multi-section Lines with anchor/fairlead connections
- `winches` → Pretension winches (when pretension specified)

### Material Database
Chain properties from DNV-OS-E302 / API 2SK (simplified):
- MBL (kN) = `mbl_coeff` × d² (d in mm)
- Mass (te/m) = `mass_coeff` × d²
- EA (kN) = `ea_coeff` × d² × 1000

### Key Files
- `schema/mooring.py` — Domain Pydantic models
- `routers/mooring_router.py` — `MooringRouter.route()` transformation
- `routers/base_router.py` — Abstract `BaseRouter` interface

---

## Route C — Vessel Hydrodynamics (future)

Vessel routing will link hull panel models and diffraction solver RAO outputs
to OrcaFlex VesselType definitions.

### Planned Source
- `hull_panel_catalog.yaml` → hull_id, dimensions, draft
- `DiffractionResults` → RAO data from OrcaWave/AQWA solver runs
- Existing: `hydrodynamics/diffraction/orcaflex_exporter.py` already exports VesselType YAML

### Planned Transformation
`VesselRouter` (future) will:
1. Look up hull geometry from catalog by hull_id
2. Load RAO data from solver output directory
3. Map hull dimensions + RAOs → VesselType properties
4. Generate vessel instance with mooring connection points

### Planned Output Sections
- `vessel_types` → VesselType with length, displacement RAOs
- `vessels` → Vessel instance with position, heading, motion settings

### Status
Not yet implemented. The `orcaflex_exporter.py` provides the export step;
the router would add the catalog lookup and schema validation layer.

---

## Cross-Reference: Object Inventory

See `object_inventory.yaml` in this directory for the complete machine-readable
inventory of all OrcaFlex object types, their properties, priority/skip
classifications, dormancy rules, and usage statistics from the spec library.
