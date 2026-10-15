# Modular generator knowledge pack

| Header               | Value                                                                  |
| -------------------- | ---------------------------------------------------------------------- |
| Module               | solvers/orcaflex/modular_generator                                     |
| Repository           | digitalmodel                                                           |
| Path                 | src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md |
| Pack revision        | 1                                                                      |
| Code revision        | fb8a214bdb622ed04f0b6473a9311695c9a5a314                               |
| Analysis family      | OrcaFlex riser and mooring; installation co-review                     |
| Quality manager role | Riser and mooring quality manager                                      |
| Status               | DRAFT                                                                  |

## 1. Purpose and scope

ModularModelGenerator validates a project specification, dispatches registered builders in dependency order, writes YAML includes, parameters and a master list, and invokes collision and post-generation checks. `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L81-L209]`

The root schema requires metadata and environment, supplies equipment and simulation defaults, and rejects both zero model branches and multiple branches among pipeline, riser, mooring and generic. `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py:L67-L118]`

The main path documented field by field below is pipeline installation; the other branches remain available through the root schema, with their detailed consumer audit deferred in section 10. `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py:L67-L118]`

The routing registry labels the broader OrcaFlex entry production but points at a different entry path; that parent label does not establish modular-generator maturity. `[docs/registry/module-routing.yaml:L195-L200]`

UNVERIFIED: module-specific maturity, target-version compatibility and engineering applicability envelopes are not established; section 10 tracks the missing evidence.

Generated dictionaries are distinct from native load and equilibrium results: integration tests separately instantiate a model, load data and calculate statics. `[tests/solvers/orcaflex/modular_generator/test_riser_generation.py:L420-L502]`

## 2. Inputs and outputs

Input YAML is loaded with yaml.safe_load before ProjectInputSpec validation; duplicate-key rejection is not implemented at this entry point. `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L149-L152]`

The include dumper disables aliases and binds the shared None representer; production include writes retain dictionary insertion order. `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L49-L62]` `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L183-L184]`

The General builder defaults to SI, but copies supplied generic user-unit declarations and gravity before Environment; it does not convert the engineering scalar inputs when changing that declaration. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/general_builder.py:L30-L51]`

Wave direction is documented as degrees from North, going to; direction is normalized modulo a full turn and nonzero phase is rejected rather than converted to a time origin. `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L72-L133]`

Current profile depth is documented positive downward below the surface. `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L195-L205]`

Stinger points use vessel-local coordinates: x is origin x minus radius times sine of arc-length/radius; z is origin z minus radius times one minus cosine. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_builder.py:L75-L122]`

UNVERIFIED: a complete coordinate-origin and phase-convention contract across raw vessel, line and buoy properties is not established; see section 10.

### Input field catalogue: main path

Each row records a declaration, rather than an independently qualified design limit. Required means no default. Factory names the default constructor. Bounds are declaration constraints; subsequent validator rules are listed separately. Nested records and text have no physical unit. A scalar without a declared unit remains explicitly unspecified.

#### ProjectInputSpec

Nested records dispatch to builders; one model branch is required. `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py:L67-L118]`

| Field       | Type / unit or declared meaning                         | Allowed field bounds | Default             | Declaration                                                                    |
| ----------- | ------------------------------------------------------- | -------------------- | ------------------- | ------------------------------------------------------------------------------ |
| metadata    | Metadata; Model metadata                                | type only            | required            | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py:L67-L67]` |
| environment | Environment; Environmental conditions                   | type only            | required            | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py:L68-L68]` |
| pipeline    | Pipeline or None; Pipeline definition                   | type only            | None                | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py:L69-L69]` |
| riser       | Riser or None; Riser definition                         | type only            | None                | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py:L70-L70]` |
| mooring     | MooringSystem or None; Mooring system definition        | type only            | None                | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py:L71-L71]` |
| generic     | GenericModel or None; Generic OrcaFlex model definition | type only            | None                | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py:L72-L74]` |
| equipment   | Equipment; Equipment configuration                      | type only            | factory: Equipment  | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py:L75-L77]` |
| simulation  | Simulation; Simulation parameters                       | type only            | factory: Simulation | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py:L78-L80]` |

#### Metadata

Identification and CLI summary; no physical data-name assignment in the generation loop. `[src/digitalmodel/solvers/orcaflex/modular_generator/cli.py:L104-L123]`

| Field       | Type / unit or declared meaning                             | Allowed field bounds | Default  | Declaration                                                                        |
| ----------- | ----------------------------------------------------------- | -------------------- | -------- | ---------------------------------------------------------------------------------- |
| name        | str; Unique model identifier                                | min_length=1         | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/metadata.py:L22-L22]` |
| description | str; Human-readable model description                       | type only            | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/metadata.py:L23-L23]` |
| structure   | str; Structure type (pipeline, riser, mooring, etc.)        | type only            | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/metadata.py:L24-L24]` |
| operation   | str; Operation type (installation/floating, in-place, etc.) | type only            | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/metadata.py:L25-L25]` |
| project     | str; Project code for traceability                          | type only            | ''       | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/metadata.py:L26-L26]` |
| version     | str; Schema version                                         | type only            | '1.0'    | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/metadata.py:L27-L27]` |
| author      | str or None; Author name or team                            | type only            | None     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/metadata.py:L28-L28]` |

#### Water

depth → WaterDepth when raw contains it, otherwise SeabedOriginDepth; density → Density. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L168-L246]`

| Field   | Type / unit or declared meaning                      | Allowed field bounds | Default  | Declaration                                                                           |
| ------- | ---------------------------------------------------- | -------------------- | -------- | ------------------------------------------------------------------------------------- |
| depth   | float; Water depth (m)                               | gt=0                 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L21-L21]` |
| density | float; Fluid density (te/m3), typical seawater 1.025 | gt=0                 | 1.025    | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L22-L26]` |

#### SeabedStiffness

normal → SeabedNormalStiffness; shear → SeabedShearStiffness. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L168-L246]`

| Field  | Type / unit or declared meaning   | Allowed field bounds | Default | Declaration                                                                           |
| ------ | --------------------------------- | -------------------- | ------- | ------------------------------------------------------------------------------------- |
| normal | float; Normal stiffness (kN/m/m2) | ge=0                 | 0       | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L38-L38]` |
| shear  | float; Shear stiffness (kN/m/m2)  | ge=0                 | 0       | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L39-L39]` |

#### Seabed

slope → SeabedSlope; stiffness → nested stiffness keys; friction_coefficient has no assignment in EnvironmentBuilder.build. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L168-L246]`

| Field                | Type / unit or declared meaning            | Allowed field bounds | Default  | Declaration                                                                           |
| -------------------- | ------------------------------------------ | -------------------- | -------- | ------------------------------------------------------------------------------------- |
| slope                | float; Seabed slope (degrees)              | ge=-90, le=90        | 0        | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L58-L58]` |
| stiffness            | SeabedStiffness; Soil stiffness parameters | type only            | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L59-L59]` |
| friction_coefficient | float; Seabed friction coefficient         | ge=0, le=1.5         | 0.3      | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L60-L62]` |

#### WaveTrain

name → Name for fresh trains; type → WaveType; direction → WaveDirection; height → WaveHeight/WaveHs; period → WavePeriod/WaveTz/WaveTp; gamma → conditional WaveGamma; phase must be zero. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L248-L362]`

| Field     | Type / unit or declared meaning                          | Allowed field bounds | Default  | Declaration                                                                            |
| --------- | -------------------------------------------------------- | -------------------- | -------- | -------------------------------------------------------------------------------------- |
| name      | str; unit not stated in declaration                      | type only            | 'Wave 1' | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L88-L88]`  |
| type      | str; Wave theory type                                    | type only            | 'airy'   | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L89-L89]`  |
| height    | float; Wave height Hs (m)                                | ge=0                 | 0        | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L90-L90]`  |
| period    | float; Wave period Tp (s)                                | gt=0                 | 8        | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L91-L91]`  |
| direction | float; Wave direction (deg from North)                   | type only            | 0        | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L92-L94]`  |
| phase     | float; Wave phase (deg); must be 0 — see class docstring | ge=0, lt=360         | 0        | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L95-L98]`  |
| gamma     | float; JONSWAP gamma (typically 1-7)                     | gt=1, le=7           | 3.3      | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L99-L101]` |

#### Waves

trains → WaveTrains; legacy flat fields wrap into trains[0]. `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L136-L165]`

| Field  | Type / unit or declared meaning | Allowed field bounds | Default                        | Declaration                                                                             |
| ------ | ------------------------------- | -------------------- | ------------------------------ | --------------------------------------------------------------------------------------- |
| trains | list[WaveTrain]; Wave trains    | type only            | factory: lambda: [WaveTrain()] | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L144-L147]` |

#### CurrentProfile

Helper record; Current.profile instead uses numeric depth/factor rows. `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L195-L225]`

| Field  | Type / unit or declared meaning                 | Allowed field bounds | Default  | Declaration                                                                             |
| ------ | ----------------------------------------------- | -------------------- | -------- | --------------------------------------------------------------------------------------- |
| depth  | float; Depth below surface (m)                  | ge=0                 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L204-L204]` |
| factor | float; Speed factor (fraction of surface speed) | ge=0                 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L205-L205]` |

#### Current

speed → RefCurrentSpeed in interpolated mode; direction → RefCurrentDirection; profile → CurrentDepth, CurrentFactor, CurrentRotation. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L168-L246]`

| Field     | Type / unit or declared meaning                           | Allowed field bounds | Default                     | Declaration                                                                             |
| --------- | --------------------------------------------------------- | -------------------- | --------------------------- | --------------------------------------------------------------------------------------- |
| speed     | float; Surface current speed (m/s)                        | ge=0                 | 0                           | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L218-L218]` |
| direction | float; Current direction (deg from North)                 | type only            | 0                           | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L219-L221]` |
| profile   | list[list[float]]; Current profile [[depth, factor], ...] | type only            | factory: lambda: [[0, 1.0]] | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L222-L225]` |

#### Wind

speed → WindSpeed unless dormant; direction → WindDirection; reference_height has no assignment in build. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L168-L246]`

| Field            | Type / unit or declared meaning        | Allowed field bounds | Default | Declaration                                                                             |
| ---------------- | -------------------------------------- | -------------------- | ------- | --------------------------------------------------------------------------------------- |
| speed            | float; Wind speed (m/s)                | ge=0                 | 0       | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L283-L283]` |
| direction        | float; Wind direction (deg from North) | type only            | 0       | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L284-L286]` |
| reference_height | float; Reference height (m)            | gt=0                 | 10      | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L287-L289]` |

#### Environment

Typed nested values → Environment; raw_properties → restricted overlay. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L168-L246]`

| Field          | Type / unit or declared meaning                                                          | Allowed field bounds | Default          | Declaration                                                                             |
| -------------- | ---------------------------------------------------------------------------------------- | -------------------- | ---------------- | --------------------------------------------------------------------------------------- |
| water          | Water; Water properties                                                                  | type only            | required         | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L324-L324]` |
| seabed         | Seabed; Seabed properties                                                                | type only            | required         | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L325-L325]` |
| waves          | Waves; Wave specification                                                                | type only            | factory: Waves   | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L326-L326]` |
| current        | Current; Current specification                                                           | type only            | factory: Current | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L327-L327]` |
| wind           | Wind; Wind specification                                                                 | type only            | factory: Wind    | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L328-L328]` |
| raw_properties | dict[str, Any]; Pass-through raw OrcaFlex Environment properties for round-trip fidelity | type only            | factory: dict    | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L329-L332]` |

#### Dimensions

outer_diameter → OD; wall_thickness participates in ID = OD minus twice wall thickness. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py:L97-L145]`

| Field          | Type / unit or declared meaning | Allowed field bounds | Default  | Declaration                                                                        |
| -------------- | ------------------------------- | -------------------- | -------- | ---------------------------------------------------------------------------------- |
| outer_diameter | float; Outer diameter (m)       | gt=0                 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L17-L17]` |
| wall_thickness | float; Wall thickness (m)       | gt=0                 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L18-L18]` |

#### Coating

thickness/density → LayerThickness, LayerMaterialDensity; weight-layer name → coating entry Name. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/vardata_builder.py:L37-L73]`

| Field     | Type / unit or declared meaning | Allowed field bounds | Default  | Declaration                                                                        |
| --------- | ------------------------------- | -------------------- | -------- | ---------------------------------------------------------------------------------- |
| thickness | float; Coating thickness (m)    | gt=0                 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L46-L46]` |
| density   | float; Coating density (te/m3)  | gt=0                 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L47-L47]` |
| name      | str or None; Coating identifier | type only            | None     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L48-L48]` |

#### Coatings

corrosion/weight → VariableData.Coatingsorlinings. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/vardata_builder.py:L37-L86]`

| Field     | Type / unit or declared meaning      | Allowed field bounds | Default       | Declaration                                                                        |
| --------- | ------------------------------------ | -------------------- | ------------- | ---------------------------------------------------------------------------------- |
| corrosion | Coating; Corrosion coating layer     | type only            | required      | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L80-L80]` |
| weight    | list[Coating]; Weight coating layers | type only            | factory: list | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L81-L83]` |

#### Segment

type/length/segment_length → LineType, Length, TargetSegmentLength; start_arc_length has no assignment in this segment-table loop. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/lines_builder.py:L148-L169]`

| Field            | Type / unit or declared meaning              | Allowed field bounds | Default  | Declaration                                                                          |
| ---------------- | -------------------------------------------- | -------------------- | -------- | ------------------------------------------------------------------------------------ |
| type             | str; Segment type identifier                 | min_length=1         | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L97-L97]`   |
| length           | float; Total segment length (m)              | gt=0                 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L98-L98]`   |
| segment_length   | float; Mesh segment length (m)               | gt=0                 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L99-L99]`   |
| start_arc_length | float or None; Start position from End A (m) | ge=0                 | None     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L100-L102]` |

#### Pipeline

name → Lines.Name; material → grade lookup; dimensions/coatings/segments → corresponding tables; youngs_modulus → E; poissons_ratio → PoissonRatio. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py:L97-L159]`

| Field          | Type / unit or declared meaning                 | Allowed field bounds | Default  | Declaration                                                                          |
| -------------- | ----------------------------------------------- | -------------------- | -------- | ------------------------------------------------------------------------------------ |
| name           | str; Pipeline name for OrcaFlex                 | min_length=1         | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L154-L154]` |
| material       | str; Material grade (e.g., X65)                 | type only            | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L155-L155]` |
| dimensions     | Dimensions; Cross-section dimensions            | type only            | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L156-L156]` |
| coatings       | Coatings; Coating system                        | type only            | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L157-L157]` |
| segments       | list[Segment]; Pipeline segments                | min_length=1         | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L158-L160]` |
| youngs_modulus | float or None; Young's modulus override (kN/m2) | gt=0                 | None     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L161-L163]` |
| poissons_ratio | float or None; Poisson's ratio override         | gt=0, lt=0.5         | None     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L164-L166]` |

#### TugProperties

mass/volume/height/moments_of_inertia → Mass/Volume/Height/MomentsOfInertia. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/tug_builder.py:L28-L58]`

| Field              | Type / unit or declared meaning                                 | Allowed field bounds | Default  | Declaration                                                                         |
| ------------------ | --------------------------------------------------------------- | -------------------- | -------- | ----------------------------------------------------------------------------------- |
| mass               | float; Tug mass (te)                                            | ge=0                 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L21-L21]` |
| volume             | float; Displaced volume (m3)                                    | ge=0                 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L22-L22]` |
| height             | float; Tug height for drawing (m)                               | gt=0                 | 6        | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L23-L23]` |
| moments_of_inertia | list[float] or None; Moments of inertia [Ixx, Iyy, Izz] (te.m2) | type only            | None     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L24-L26]` |

#### Tugs

count → 6DBuoy count; first_position/spacing → InitialPosition; properties → buoy property assignments. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/tug_builder.py:L24-L58]`

| Field          | Type / unit or declared meaning               | Allowed field bounds       | Default  | Declaration                                                                         |
| -------------- | --------------------------------------------- | -------------------------- | -------- | ----------------------------------------------------------------------------------- |
| count          | int; Number of tugs                           | ge=1, le=20                | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L40-L40]` |
| spacing        | float; Spacing between tugs (m)               | gt=0                       | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L41-L41]` |
| first_position | list[float]; First tug position [x, y, z] (m) | min_length=3, max_length=3 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L42-L44]` |
| properties     | TugProperties; Tug physical properties        | type only                  | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L45-L45]` |

#### Rollers

Legacy position/supports/support_type → one RollerStation via compatibility conversion. `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L386-L404]`

| Field        | Type / unit or declared meaning            | Allowed field bounds       | Default         | Declaration                                                                         |
| ------------ | ------------------------------------------ | -------------------------- | --------------- | ----------------------------------------------------------------------------------- |
| position     | list[float]; Roller position [x, y, z] (m) | min_length=3, max_length=3 | required        | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L66-L68]` |
| supports     | int; Number of supports                    | ge=1, le=20                | 4               | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L69-L69]` |
| support_type | str; Support type name                     | type only                  | 'Support type2' | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L70-L70]` |

#### RollerStation

Station geometry → roller buoy/support output; individual consumer audit deferred. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/roller_builder.py:L1-L161]`

| Field                | Type / unit or declared meaning                          | Allowed field bounds       | Default         | Declaration                                                                           |
| -------------------- | -------------------------------------------------------- | -------------------------- | --------------- | ------------------------------------------------------------------------------------- |
| position             | list[float]; [x, y, z] position (m)                      | min_length=3, max_length=3 | required        | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L84-L86]`   |
| support_count        | int; Supports per station                                | ge=1, le=20                | 4               | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L87-L89]`   |
| support_type         | str; Support type name                                   | type only                  | 'Support type2' | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L90-L92]`   |
| v_angle              | float or None; V-angle (deg), required for v_roller only | ge=30, le=180              | None            | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L93-L96]`   |
| diameter             | float; Roller drum diameter (m)                          | gt=0.05, le=5.0            | 0.5             | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L97-L99]`   |
| friction_coefficient | float; Roller friction coefficient                       | ge=0, le=1                 | 0.1             | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L100-L102]` |
| height_offset        | float; Vertical offset from reference (m)                | type only                  | 0.0             | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L103-L105]` |

#### RollerArrangement

type/stations → roller geometry; complete consumer map deferred. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/roller_builder.py:L1-L161]`

| Field           | Type / unit or declared meaning                       | Allowed field bounds | Default             | Declaration                                                                           |
| --------------- | ----------------------------------------------------- | -------------------- | ------------------- | ------------------------------------------------------------------------------------- |
| type            | RollerType; unit not stated in declaration            | type only            | RollerType.V_ROLLER | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L123-L123]` |
| stations        | list[RollerStation]; Ordered roller stations          | min_length=1         | required            | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L124-L126]` |
| spacing_pattern | str or None; 'uniform:<spacing_m>' or None for custom | type only            | None                | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L127-L129]` |

#### BuoyancyModuleProperties

mass/volume/height → Mass/Volume/Height of the BM buoy. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/buoyancy_builder.py:L34-L59]`

| Field  | Type / unit or declared meaning | Allowed field bounds | Default  | Declaration                                                                           |
| ------ | ------------------------------- | -------------------- | -------- | ------------------------------------------------------------------------------------- |
| mass   | float; Module mass (te)         | ge=0                 | 0.05     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L173-L173]` |
| volume | float; Displaced volume (m3)    | gt=0                 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L174-L174]` |
| height | float; Module height (m)        | gt=0                 | 2        | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L175-L175]` |

#### BuoyancyModules

spacing/start_arc_length/end_arc_length → attachment positions through the root helper; properties → BM buoy. `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py:L174-L205]`

| Field            | Type / unit or declared meaning                | Allowed field bounds | Default  | Declaration                                                                           |
| ---------------- | ---------------------------------------------- | -------------------- | -------- | ------------------------------------------------------------------------------------- |
| spacing          | float; Module spacing (m)                      | gt=0                 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L189-L189]` |
| properties       | BuoyancyModuleProperties; Module properties    | type only            | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L190-L190]` |
| start_arc_length | float or None; Start of BM zone (m from End A) | ge=0                 | None     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L191-L193]` |
| end_arc_length   | float or None; End of BM zone (m from End A)   | ge=0                 | None     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L194-L196]` |

#### Ramp

name/type/origin/size/rotation → Shapes via type-specific builders; full branch audit deferred. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/shapes_builder.py:L74-L182]`

| Field    | Type / unit or declared meaning                    | Allowed field bounds       | Default  | Declaration                                                                           |
| -------- | -------------------------------------------------- | -------------------------- | -------- | ------------------------------------------------------------------------------------- |
| name     | str; Shape name                                    | min_length=1               | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L225-L225]` |
| type     | str; Shape type (block, curved_plate, etc.)        | type only                  | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L226-L226]` |
| origin   | list[float] or None; Origin position [x, y, z] (m) | min_length=3, max_length=3 | None     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L227-L232]` |
| size     | list[float] or None; Dimensions [L, W, H] (m)      | min_length=3, max_length=3 | None     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L233-L239]` |
| rotation | list[float] or None; Rotation [rx, ry, rz] (deg)   | min_length=3, max_length=3 | None     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L240-L246]` |

#### VesselProperties

loa → Length and displacement estimate; beam/draft → estimate; displacement → Displacement; gyration_radii → MomentsOfInertia; depth/gmt/cog/rao_reference have no assignment in VesselTypeBuilder.build. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_type_builder.py:L32-L71]`

| Field          | Type / unit or declared meaning                    | Allowed field bounds       | Default  | Declaration                                                                           |
| -------------- | -------------------------------------------------- | -------------------------- | -------- | ------------------------------------------------------------------------------------- |
| loa            | float; Length overall (m)                          | gt=0                       | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L264-L264]` |
| beam           | float; Beam width (m)                              | gt=0                       | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L265-L265]` |
| depth          | float; Vessel depth (m)                            | gt=0                       | 9.5      | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L266-L266]` |
| draft          | float; Operational draft (m)                       | gt=0                       | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L267-L267]` |
| displacement   | float or None; Displacement (te)                   | ge=0                       | None     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L268-L268]` |
| gmt            | float; Transverse metacentric height (m)           | type only                  | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L269-L269]` |
| cog            | list[float]; Centre of gravity [x, y, z] (m)       | min_length=3, max_length=3 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L270-L272]` |
| gyration_radii | list[float]; Radii of gyration [Rxx, Ryy, Rzz] (m) | min_length=3, max_length=3 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L273-L275]` |
| rao_reference  | str or None; RAO data file reference               | type only                  | None     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L276-L276]` |

#### VesselMooring

drums/winch_capacity/pattern have no assignment in inspected VesselBuilder.build; complete mapping remains open. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_builder.py:L35-L73]`

| Field          | Type / unit or declared meaning          | Allowed field bounds | Default | Declaration                                                                           |
| -------------- | ---------------------------------------- | -------------------- | ------- | ------------------------------------------------------------------------------------- |
| drums          | int; Number of mooring drums/lines       | ge=1, le=16          | 8       | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L289-L289]` |
| winch_capacity | float; Winch capacity (kN)               | gt=0                 | 735.75  | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L290-L290]` |
| pattern        | str or None; Mooring pattern description | type only            | None    | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L291-L291]` |

#### Vessel

name → Name and type reference; properties → vessel type; position → InitialPosition; orientation[2] → InitialHeading; mooring has no assignment in build. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_builder.py:L35-L73]`

| Field       | Type / unit or declared meaning                     | Allowed field bounds       | Default   | Declaration                                                                           |
| ----------- | --------------------------------------------------- | -------------------------- | --------- | ------------------------------------------------------------------------------------- |
| name        | str; Vessel name                                    | min_length=1               | required  | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L297-L297]` |
| properties  | VesselProperties; Vessel physical properties        | type only                  | required  | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L298-L298]` |
| mooring     | VesselMooring or None; Mooring configuration        | type only                  | None      | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L299-L299]` |
| position    | list[float]; Initial position [x, y, z] (m)         | min_length=3, max_length=3 | [0, 0, 0] | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L300-L302]` |
| orientation | list[float]; Initial orientation [rx, ry, rz] (deg) | min_length=3, max_length=3 | [0, 0, 0] | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L303-L305]` |

#### StingerSection

length → total support-path arc; bend_radius has no read in _build_support_path. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_builder.py:L75-L131]`

| Field       | Type / unit or declared meaning | Allowed field bounds | Default  | Declaration                                                                           |
| ----------- | ------------------------------- | -------------------- | -------- | ------------------------------------------------------------------------------------- |
| length      | float; Section length (m)       | gt=0                 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L318-L318]` |
| bend_radius | float; Bend radius (m)          | gt=0                 | required | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L319-L319]` |

#### StingerRoller

arc_length/z_offset/support_type → SupportPosition/SupportType. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_builder.py:L111-L131]`

| Field        | Type / unit or declared meaning              | Allowed field bounds | Default         | Declaration                                                                           |
| ------------ | -------------------------------------------- | -------------------- | --------------- | ------------------------------------------------------------------------------------- |
| arc_length   | float; Arc length from stinger origin (m)    | ge=0                 | required        | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L325-L325]` |
| support_type | str; Support type name                       | type only            | 'Support type2' | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L326-L326]` |
| z_offset     | float; Vertical offset from stinger path (m) | type only            | 0               | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L327-L327]` |

#### Stinger

radius/sections/rollers/origin_position → support path; origin_orientation has no read in _build_support_path. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_builder.py:L75-L131]`

| Field              | Type / unit or declared meaning                     | Allowed field bounds       | Default       | Declaration                                                                           |
| ------------------ | --------------------------------------------------- | -------------------------- | ------------- | ------------------------------------------------------------------------------------- |
| radius             | float; Stinger radius (m)                           | gt=0                       | required      | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L333-L333]` |
| sections           | list[StingerSection]; Stinger sections              | type only                  | factory: list | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L334-L336]` |
| rollers            | list[StingerRoller]; Roller positions on stinger    | type only                  | factory: list | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L337-L339]` |
| origin_position    | list[float]; Stinger origin on vessel [x, y, z] (m) | min_length=3, max_length=3 | [0, 0, 0]     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L340-L342]` |
| origin_orientation | list[float]; Stinger orientation [rx, ry, rz] (deg) | min_length=3, max_length=3 | [180, 0, 0]   | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L343-L345]` |

#### Tensioner

name → Name; tension_value → StageTension; position_on_vessel → ConnectionPoint; stiffness/damping → Stiffness/Damping; capacity_kn has no read in build. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py:L35-L81]`

| Field              | Type / unit or declared meaning                       | Allowed field bounds       | Default     | Declaration                                                                           |
| ------------------ | ----------------------------------------------------- | -------------------------- | ----------- | ------------------------------------------------------------------------------------- |
| name               | str; Tensioner name                                   | type only                  | 'Tensioner' | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L358-L358]` |
| capacity_kn        | float; Maximum capacity (kN)                          | gt=0                       | required    | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L359-L359]` |
| stiffness          | float or None; Tensioner stiffness (kN/m)             | ge=0                       | None        | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L360-L360]` |
| damping            | float or None; Tensioner damping (kN.s/m)             | ge=0                       | None        | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L361-L361]` |
| position_on_vessel | list[float] or None; Position on vessel [x, y, z] (m) | min_length=3, max_length=3 | None        | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L362-L364]` |
| tension_value      | float; Applied tension (kN)                           | gt=0                       | required    | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L365-L365]` |

#### Equipment

Optional equipment records default to None; ramps default to an empty list; legacy rollers resolve to roller_arrangement. `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L368-L404]`

| Field              | Type / unit or declared meaning                                                   | Allowed field bounds | Default       | Declaration                                                                           |
| ------------------ | --------------------------------------------------------------------------------- | -------------------- | ------------- | ------------------------------------------------------------------------------------- |
| tugs               | Tugs or None; Tug configuration                                                   | type only            | None          | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L371-L371]` |
| rollers            | Rollers or None; Roller configuration                                             | type only            | None          | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L372-L372]` |
| roller_arrangement | RollerArrangement or None; Parametric roller arrangement (preferred over rollers) | type only            | None          | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L373-L375]` |
| buoyancy_modules   | BuoyancyModules or None; Buoyancy module configuration                            | type only            | None          | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L376-L378]` |
| ramps              | list[Ramp]; Ramp/shape definitions                                                | type only            | factory: list | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L379-L381]` |
| vessel             | Vessel or None; Installation vessel                                               | type only            | None          | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L382-L382]` |
| stinger            | Stinger or None; Stinger configuration                                            | type only            | None          | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L383-L383]` |
| tensioner          | Tensioner or None; Tensioner configuration                                        | type only            | None          | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L384-L384]` |

#### Simulation

time_step → ImplicitConstantTimeStep and TargetLogSampleInterval; stages → StageDuration; nonzero north_direction → NorthDirection; inner_time_step/implicit_constant have no assignment in GeneralBuilder.build. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/general_builder.py:L28-L81]`

| Field             | Type / unit or declared meaning              | Allowed field bounds | Default                  | Declaration                                                                          |
| ----------------- | -------------------------------------------- | -------------------- | ------------------------ | ------------------------------------------------------------------------------------ |
| time_step         | float; Outer time step (s)                   | gt=0, le=1           | 0.1                      | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/simulation.py:L20-L22]` |
| stages            | list[int or float]; Stage durations (s)      | min_length=1         | factory: lambda: [8, 16] | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/simulation.py:L23-L27]` |
| north_direction   | float; North direction (deg)                 | ge=0, lt=360         | 0                        | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/simulation.py:L28-L30]` |
| inner_time_step   | float or None; Inner time step (s)           | gt=0, le=0.1         | None                     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/simulation.py:L31-L33]` |
| implicit_constant | float or None; Implicit integration constant | ge=0, le=1           | None                     | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/simulation.py:L34-L36]` |

Current speed is limited to 0–5 m/s and wind speed to 0–80 m/s; current rows require nonnegative depth/factor and sorted depths; direction is normalized to [0, 360) degrees. `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L227-L308]`

Additional pipeline limits are wall thickness below half the diameter; coating thickness at most 0.5 m and density 0.5–8 te/m3; segment length at most 50,000 m; target mesh length 0.1–100 m, no greater than section length. `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L19-L135]`

The simulation validator requires each stage duration above 0 s and a time step of at least 0.001 s; field declarations separately cap it at 1 s. `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/simulation.py:L36-L55]`

Equipment validators default missing V-roller angles and reject reversed explicit buoyancy arc-length limits. `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py:L130-L210]`

UNVERIFIED: complete consumer maps for roller/shape branches and generic/riser/mooring/campaign fields remain open in section 10; declarations alone do not establish emitted behaviour.

### Output catalogue

| Output                | Contents / units                                                                                    | Source                                                                             |
| --------------------- | --------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------- |
| includes/*.yml        | Builder sections; model unit basis declared in General                                              | `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L154-L201]`      |
| inputs/parameters.yml | Water depth/density, available metocean scalars, stages and time step; not a model property section | `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L228-L246]`      |
| master.yml            | YAML 1.1 header and registry-ordered includefile directives                                         | `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L248-L262]`      |
| Variation YAML        | BaseFile reference plus ordered overrides; relative paths and no aliases                            | `[src/digitalmodel/solvers/orcaflex/modular_generator/writers/basefile.py:L1-L36]` |

## 3. Method and governing standards

| Builder / method                         | Emitted content and basis visible in code                                                                                                                                   | Evidence                                                                                         |
| ---------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------ |
| LineTypeBuilder.build                    | LineTypes from wall geometry, grade defaults and optional E/Poisson overrides; deduplicated segment types                                                                   | `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py:L88-L159]`    |
| LinesBuilder.build / _build_segment_data | Lines with connection tables, section lengths and mesh lengths; optional winch-wire restraint and buoyancy attachments                                                      | `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/lines_builder.py:L44-L169]`       |
| VesselTypeBuilder.build                  | VesselTypes with supplied displacement or a block-volume estimate; principal inertia = displacement times squared gyration radius; no published source cited by this method | `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_type_builder.py:L32-L71]`  |
| VesselBuilder / _build_support_path      | Vessels plus support points from circular-arc sine/cosine geometry; only heading is taken from the orientation vector                                                       | `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_builder.py:L35-L131]`      |
| BuoysBuilder.build                       | 6DBuoys/3DBuoys aggregated from roller, tug, module and end-buoy sub-builders; generated names registered in context                                                        | `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/buoys_builder.py:L47-L90]`        |
| TugBuilder.build                         | Initial positions advance from first_position by integer spacing along x; physical properties copied from input                                                             | `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/tug_builder.py:L24-L58]`          |
| BuoyancyBuilder.build                    | Attached lumped buoy with supplied mass/volume/height; hydrodynamic defaults are literals, not independent validation                                                       | `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/buoyancy_builder.py:L34-L82]`     |
| EndBuoyBuilder                           | End-buoy coordinates estimated from total pipeline length; marker placed at half-length; engineering generality not established                                             | `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/end_buoy_builder.py:L33-L100]`    |
| WinchBuilder.build                       | Winches join vessel to pipeline End A; specified-length then specified-tension stages; optional stiffness and damping                                                       | `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py:L35-L81]`        |
| EnvironmentBuilder.build                 | Defaults, restricted raw overlay and typed values; deterministic and spectral waves use different data names                                                                | `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L34-L417]` |
| GenericModelBuilder._merge_object        | Typed fields override property bags; explicit None survives; dormant keys removed; priority selectors ordered first                                                         | `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py:L343-L396]`    |
| merge_named_lists                        | Matching names replace the complete object; unmatched entries append; no recursive field merge                                                                              | `[src/digitalmodel/solvers/orcaflex/modular_generator/merge.py:L19-L74]`                         |
| VariableResolver.resolve                 | Tokens resolved and formatted; remaining tokens or invalid YAML rejected; no canonical re-dump of template text                                                             | `[src/digitalmodel/solvers/orcaflex/modular_generator/sections.py:L13-L76]`                      |

| Standard mention | Location and clause status                                                                                                                                                     | Evidence                                                                                                                                                                                    |
| ---------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| DNV-ST-F101      | Pipeline builder names this identifier and emits DNVSTF101 data names; edition, title and clause locator absent at this use; clause text not checked against the licensed copy | `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py:L15-L44]` `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py:L215-L233]` |
| DNV-OS-E302      | Mooring router attributes a chain breaking-load relation and grade database to it; edition, title and clause locator absent; clause text not checked against the licensed copy | `[src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py:L21-L30]`                                                                                                   |
| API RP 2SK       | Schema names API 2SK alongside chain grades, without RP qualifier, edition, title or clause; not itself a design check; clause text not checked against the licensed copy      | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/mooring.py:L19-L27]`                                                                                                           |
| API 5L           | Pipeline material docstring names this identifier, without edition, title or clause; clause text not checked against the licensed copy                                         | `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py:L139-L150]`                                                                                                        |

UNVERIFIED: complete bibliographic references, edition consistency and Citation-sidecar coverage are not established; no standard-derived coefficient or tabulated value is reproduced here. See section 10.

## 4. Validated worked examples

No validated example is registered by this pack. Fixture comparisons are regression evidence only. The planned independent jumper comparison remains tracked by [2424](https://github.com/vamseeachanta/digitalmodel/issues/2424), and library load/statics attestation by [2428](https://github.com/vamseeachanta/digitalmodel/issues/2428). These activities are not completed test cases.

| Regression example         | Test node ID                                                                                                                       | Input and expectation basis                           | Expected value / tolerance                                                                                     | Last offline result                                                                                                                           |
| -------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------- | -------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- |
| Spectral wave keys         | tests/solvers/orcaflex/modular_generator/builders/test_environment_builder.py::TestSingleWaveTrain::test_jonswap_uses_hs_tz        | Synthetic _make_env_spec helper in the test file      | WaveHs/WaveTz present; WaveHeight/WavePeriod absent; 0 incorrect keys (count); exact schema-mapping regression | PASS at d559d39cb29402d9891e628600ff319509a90938; `[tests/solvers/orcaflex/modular_generator/builders/test_environment_builder.py:L109-L119]` |
| Current padding            | tests/solvers/orcaflex/modular_generator/builders/test_environment_builder.py::TestCurrentProfile::test_single_level_padded_to_two | Synthetic _make_env_spec helper in the test file      | 2 profile rows (count), last depth equals input water depth (m); exact regression                              | PASS at d559d39cb29402d9891e628600ff319509a90938; `[tests/solvers/orcaflex/modular_generator/builders/test_environment_builder.py:L360-L373]` |
| Missing reference          | tests/solvers/orcaflex/modular_generator/test_post_validator.py::TestLineTypeReferences::test_missing_line_type_error              | In-memory dictionary in test body                     | Exactly 1 reference error (count); named absent LineType detected                                              | PASS at d559d39cb29402d9891e628600ff319509a90938; `[tests/solvers/orcaflex/modular_generator/test_post_validator.py:L137-L164]`               |
| Variation dependency order | tests/solvers/orcaflex/modular_generator/test_basefile_writer.py::test_row11_buoys_precede_lines                                   | Temporary base and override dictionaries in test body | 0 buoy-before-reference violations (count); exact ordering regression                                          | PASS at d559d39cb29402d9891e628600ff319509a90938; `[tests/solvers/orcaflex/modular_generator/test_basefile_writer.py:L170-L210]`              |

Offline result at d559d39cb29402d9891e628600ff319509a90938 (documentation edits only): the full module directory collected 817 nodes, with 772 passed, 0 failed and 45 skipped. The combined reporting/module command collected 833 nodes, with 788 passed, 0 failed and 45 skipped; reporting contributed 16 passed. The command used the project-environment form: PYTHONPATH=src uv run --no-sources --with pytest-timeout==2.4.0 python -m pytest tests/reporting/test_engine_golden.py tests/solvers/orcaflex/modular_generator/ -q -rA -p no:cacheprovider --basetemp=<external scratch>. No addopts override was used. The timeout plugin version is pinned by the lockfile; an initial collection attempt without it failed because the timeout marker was unregistered, before any test ran. `[uv.lock:L5572-L5574]`

The tests-contracts domain at the same revision collected 884 nodes: 823 passed, 0 failed and 61 existing skips; its routing-contract subset passed all 10 tests. Both commands were repeated after the pack and registry corrections at the same source revision: reporting/module 788 passed, 0 failed, 45 skipped; contracts 823 passed, 0 failed, 61 skipped; routing contract 10 passed. The command used the same project-environment form and timeout plugin, with the nine test roots declared for tests-contracts.

The original guarded imports were retained. test_riser_generation.py ran 21 offline tests and skipped 3 native tests; test_modular_vs_monolithic.py ran 10 offline tests and skipped 40 native tests. Native cases are NOT_RUN because OrcFxAPI is unavailable (43 pytest skips), rather than excluding either module and losing its offline cases. `[tests/solvers/orcaflex/modular_generator/test_riser_generation.py:L420-L428]` `[tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L25-L41]`

The two existing skips are fixture-path conditions in the integration tests, not evidence that the requested cases pass. `[tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py:L448-L497]`

UNVERIFIED: native load, statics, modal comparison and engineering benchmark results remain absent from this pack; the planned cases have no completed independent expected-versus-computed evidence here. See section 10.

## 5. QA gates

Checks below are acceptance procedures, not declarations of an existing implemented gate. Criteria are from AQG r0, revision 0, sections 4–10; generator-specific count criteria are defined here. Gate evidence shall be bound to input revision and fingerprint.

| Gate | Applicability / result                                               | Executable check procedure                                                                                                                                                                                                                                                                                                                                                                                                                                        | Numeric pass criterion and unit                                                                                                        | Required evidence                                                                   | Enforcement or gap                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| ---- | -------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| G0.1 | APPLICABLE                                                           | C01: strict UTF-8 parse of every input and composed include; duplicate mapping keys are errors                                                                                                                                                                                                                                                                                                                                                                    | 0 parse errors and 0 duplicate keys (count)                                                                                            | Parse/occurrence record per file                                                    | Safe-load entry is not strict duplicate rejection; [2090](https://github.com/vamseeachanta/digitalmodel/issues/2090); `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L149-L152]`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| G0.2 | APPLICABLE                                                           | C02: inspect scalar tokens in every emitted include and resolved custom template; default-valued null nodes shall serialize as ~                                                                                                                                                                                                                                                                                                                                  | 0 forbidden null spellings or unintended empty scalars (count)                                                                         | Token scan plus one None serialization sample                                       | _NoAliasDumper.add_representer binds _represent_none; _represent_none emits ~; templates are copied as resolved text and need the same scan; `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L49-L62]` `[src/digitalmodel/solvers/orcaflex/yaml_utils.py:L49-L52]` `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L264-L373]`                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| G0.3 | APPLICABLE                                                           | C03: walk ordered mappings, including raw WaveTrains; compare every selector/dependent pair with its documented prerequisite                                                                                                                                                                                                                                                                                                                                      | 0 dependent-before-selector or dormant-key assignments (count)                                                                         | Ordered-node violation list                                                         | EnvironmentBuilder.build initializes _DEFAULTS, preserving seabed/current/wind selector positions during overlay; GenericModelBuilder._merge_object orders priority keys; _build_wave_trains copies raw order without moving an existing WaveType; [2513](https://github.com/vamseeachanta/digitalmodel/issues/2513); `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L49-L87]` `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L126-L128]` `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L185-L198]` `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L248-L362]` `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py:L388-L396]` |
| G0.4 | APPLICABLE                                                           | C04: enumerate emitted object types and inject one invalid reference per generated reference-bearing type; compare with post-validator handlers; check target-version name-table membership and include existence                                                                                                                                                                                                                                                 | 0 unresolved references, 0 unknown names, 0 uncovered generated reference-bearing types (count); 100% type coverage                    | Generated/validated type matrix; mutation findings; versioned name-table comparison | Membership in _OBJECT_SECTIONS is not validation coverage; checks dispatch only specific reference handlers; [717](https://github.com/vamseeachanta/digitalmodel/issues/717); `[src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py:L36-L62]` `[src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py:L196-L204]`                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| G0.5 | APPLICABLE                                                           | C05: compare declared unit system and all emitted physical inputs with the approved design basis, including density, depth, gravity and section properties                                                                                                                                                                                                                                                                                                        | 0 unit mismatches (count); 0 unexplained normalized input differences (dimensionless fraction)                                         | Basis-to-output table with units                                                    | User unit declarations are preserved, not a unit-conversion proof; GeneralBuilder.build; `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/general_builder.py:L30-L51]`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| G0.6 | APPLICABLE                                                           | C06: compare each user-specified starting shape with that line's length, section sizes and end connections                                                                                                                                                                                                                                                                                                                                                        | Arc-length mismatch ≤0.1%; end-coordinate error ≤0.01 m; 0 section-count or size mismatches (count)                                    | Per-line shape/section/end comparison                                               | LinesBuilder._build_winch_wire_line selects User specified but emits no starting-shape table; generic property bags copy arbitrary properties without geometric validation; [2514](https://github.com/vamseeachanta/digitalmodel/issues/2514); `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/lines_builder.py:L282-L338]` `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py:L343-L396]`                                                                                                                                                                                                                                                                                                                                                                                  |
| G0.7 | APPLICABLE                                                           | C07: SHA-256 each transitively referenced text/model input; sort relative-path/digest records; hash the joined records and compare with manifest                                                                                                                                                                                                                                                                                                                  | 0 missing input digests and 0 manifest fingerprint mismatches (count)                                                                  | Per-input digests and independently recomputed aggregate                            | generate writes parameters/master/includes, without a digest manifest; [2515](https://github.com/vamseeachanta/digitalmodel/issues/2515); `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L154-L201]`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| G0.8 | APPLICABLE                                                           | C08: require immutable generator, template and design-basis revisions in output metadata                                                                                                                                                                                                                                                                                                                                                                          | 3 of 3 revision identifiers present (count), 0 unresolved revisions (count)                                                            | Provenance record bound to C07                                                      | _write_master records only source basename; [2515](https://github.com/vamseeachanta/digitalmodel/issues/2515); `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L248-L262]`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| G1   | APPLICABLE; NOT_RUN                                                  | C09: load ModularModelGenerator.generate master.yml in the matching OrcaFlex demo version where demonstrated compatible; otherwise use OrcFxAPI.Model.LoadData on the licensed route; compare objects with PostGenerationValidator._build_object_registry                                                                                                                                                                                                         | 0 load errors and 0 warnings (count); 0 object-count differences (count)                                                               | Native version, messages and manifest comparison                                    | No solver permitted in this pack preparation; planned attestation [2428](https://github.com/vamseeachanta/digitalmodel/issues/2428); AQG r0 revision 0, section 5; `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L190-L201]` `[tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L654-L675]` `[src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py:L247-L277]`                                                                                                                                                                                                                                                                                                                                                                                                 |
| G2   | APPLICABLE; NOT_RUN                                                  | C10: calculate statics of ModularModelGenerator.generate master.yml for SPEC_24IN through modular_output_dir (the integration fixture of issue 1944) and TestRiserModularGeneration.catenary_spec; verify LinesBuilder._build_winch_wire_line End B restraint for floating installations; require plan/elevation images, independent low-load catenary and directional offset sweep; reconcile component rows with PostGenerationValidator._build_object_registry | 0 unconverged models (count); tension difference ≤5%; angle difference ≤1 degree; ≥6 offset steps (count); 0 unchecked objects (count) | Native convergence record, images, hand comparison and component table              | Offline generation cannot establish equilibrium; [1944](https://github.com/vamseeachanta/digitalmodel/issues/1944); AQG r0 revision 0, section 6; `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L190-L201]` `[tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L49-L53]` `[tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L641-L675]` `[tests/solvers/orcaflex/modular_generator/test_riser_generation.py:L190-L210]` `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/lines_builder.py:L282-L338]` `[src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py:L247-L277]`                                                                                                                                                   |
| G3   | APPLICABLE; NOT_RUN                                                  | C11: run each required configuration/wave representative of generated master.yml from converged statics; compare GeneralBuilder.build General.ImplicitConstantTimeStep and StageDuration with Simulation.time_step and stages (production settings, stop after 1 s); compare each generated line tension with its C10 static value                                                                                                                                | 1 s duration; 0 errors/warnings/non-numeric results (count); tension change within ±25% of statics                                     | Native warning log, end image and simulation/wall-clock times                       | No native dynamics executed; `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/general_builder.py:L58-L74]` `[src/digitalmodel/solvers/orcaflex/modular_generator/schema/simulation.py:L20-L27]` `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L190-L201]`; AQG r0 revision 0, section 7; [2428](https://github.com/vamseeachanta/digitalmodel/issues/2428)                                                                                                                                                                                                                                                                                                                                                                                                                              |
| G4   | CONDITIONAL; NOT_APPLICABLE for the documented generated-vessel path | C12: when vessel data originate in a new diffraction run, require the diffraction record before import                                                                                                                                                                                                                                                                                                                                                            | 100% of newly imported vessel types have a matching record; free-floating balance error ≤1% of weight                                  | Per-type source/revision record and diffraction disposition                         | VesselTypeBuilder.build estimates properties without importing a diffraction result; no such result is qualified here; AQG r0 revision 0, sections 8 and 10; `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_type_builder.py:L32-L71]`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| G5   | APPLICABLE to issued analysis; NOT_RUN                               | C13: reconcile report appendix gate rows against every generated model and its fingerprint, deviations and independent sign-off                                                                                                                                                                                                                                                                                                                                   | 100% model coverage; 0 missing required records/images/signatures (count)                                                              | Machine-readable gate records and appendix reconciliation                           | This knowledge pack is not a signed analysis report; no appendix is generated by generate; AQG r0 revision 0, section 9; `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L154-L201]`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |

C04 uses the set of object types actually emitted for the selected specification as its denominator. The numerator shall contain only types with a demonstrated applicable validator check, not types merely registered as legal names. For non-reference-bearing types, record the applicable schema/property check or an explicit reason; do not count membership as reference validation.

The starting-shape, fingerprint and complete selector-pair procedures are defined checks with open implementation gaps, not gates claimed to pass. C02 is anchored to the None representer registration and shared scalar emitter. `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L49-L62]` `[src/digitalmodel/solvers/orcaflex/yaml_utils.py:L49-L52]` C03 is anchored to the raw dictionary copy and in-place WaveType assignment; C01 to the safe_load entry. `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L284-L301]` `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L149-L152]` No G1–G4 native acceptance is claimed.

Source anchors for the four required controls (function start lines are those in the citations): G0.2 _NoAliasDumper registration and _represent_none; G0.3 EnvironmentBuilder.build and GenericModelBuilder._merge_object, with [2513](https://github.com/vamseeachanta/digitalmodel/issues/2513) for raw wave trains; G0.6 [2514](https://github.com/vamseeachanta/digitalmodel/issues/2514); G0.7 [2515](https://github.com/vamseeachanta/digitalmodel/issues/2515). `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L49-L62]` `[src/digitalmodel/solvers/orcaflex/yaml_utils.py:L49-L52]`

## 6. Known failure modes and pitfalls

| Symptom                                                                           | Cause / qualification at the pin                                                                                                                                                                                                | Detection                                                                                                                     | Incident                                                                                        | Evidence                                                                                               | Date (source audit) |
| --------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------ | ------------------- |
| Two integration regressions silently skipped                                      | Fixture paths are fixed to an unrelated checkout layout                                                                                                                                                                         | Require 0 missing selected fixtures (count); retain existing skip outcome                                                     | [2517](https://github.com/vamseeachanta/digitalmodel/issues/2517)                               | `[tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py:L448-L497]`        | 2026-10-15          |
| Silent property loss through partial typed maps or unsupported pass-through modes | No blanket spec↔model parity follows from a successful generation                                                                                                                                                               | C04/C05; residual and typed-property comparison                                                                               | [2078](https://github.com/vamseeachanta/digitalmodel/issues/2078)                               | `[src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py:L391-L430]`                         | 2026-10-15          |
| Input changes after acceptance leave stale provenance                             | No SHA-256 input manifest is emitted                                                                                                                                                                                            | C07/C08; no detection check exists in generate                                                                                | [2515](https://github.com/vamseeachanta/digitalmodel/issues/2515); AQG r0 revision 0, section 1 | `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L154-L201]`                          | 2026-10-15          |
| Starting shape belongs to another line size                                       | No line/shape geometric acceptance step in the inspected generation path                                                                                                                                                        | C06; no detection check exists in that path                                                                                   | [2514](https://github.com/vamseeachanta/digitalmodel/issues/2514); AQG r0 revision 0, section 1 | `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/lines_builder.py:L282-L338]`            | 2026-10-15          |
| Reference registry gaps or unchecked generated object types                       | Current registry includes Constraints and standard built-ins, but membership is not complete property/reference validation                                                                                                      | C04; missing-line-type/link/constraint mutations                                                                              | [717](https://github.com/vamseeachanta/digitalmodel/issues/717)                                 | `[src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py:L25-L62]`                      | 2026-10-15          |
| Unqualified catenary reference                                                    | Conflicting fixture geometry is tracked by [2050](https://github.com/vamseeachanta/digitalmodel/issues/2050); CatenarySolver._length_error_for_a uses horizontal tension times length divided by axial stiffness for elongation | C10 shall use an independent reference; neither that solver path nor the disputed fixture is a qualified hand-check reference | [2050](https://github.com/vamseeachanta/digitalmodel/issues/2050)                               | `[src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L193-L201]`                           | 2026-10-15          |
| YAML null spelling or unresolved raw template null                                | Custom section text bypasses the canonical dumper                                                                                                                                                                               | C02; scan every output path                                                                                                   | AQG r0 revision 0, section 1; [2090](https://github.com/vamseeachanta/digitalmodel/issues/2090) | `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L264-L373]`                          | 2026-10-15          |
| Wave dependent key precedes selector                                              | Copying raw dictionaries retains existing key positions                                                                                                                                                                         | C03; raw WaveHeight-before-WaveType negative probe                                                                            | [2513](https://github.com/vamseeachanta/digitalmodel/issues/2513); AQG r0 revision 0, section 1 | `[src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L248-L362]`      | 2026-10-15          |
| Statics never converge in integration fixture                                     | Historical reported native non-convergence; not reproduced without a solver                                                                                                                                                     | C10; NOT_RUN here                                                                                                             | [1944](https://github.com/vamseeachanta/digitalmodel/issues/1944)                               | `[tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L25-L41]`                     | 2026-10-15          |
| No installed console entry for modular generation                                 | Module entry exists; project.scripts still has no modular-generator target                                                                                                                                                      | Read project.scripts: 0 matching targets; module main remains callable                                                        | [1641](https://github.com/vamseeachanta/digitalmodel/issues/1641)                               | `[pyproject.toml:L243-L264]` `[src/digitalmodel/solvers/orcaflex/modular_generator/__main__.py:L1-L6]` | 2026-10-15          |

The historical format findings describe earlier failures and are not a current native qualification record; their broad failure claims shall not be applied to this pin without reproduction. `[docs/domains/orcaflex/CRITICAL_YAML_FORMAT_FINDINGS.md:L1-L17]`

## 7. Lessons log

Append-only record; private dataset values are excluded.

| Date       | Entry                | Lesson                                                                                    | Evidence                                                                      | Pack change                         | Shared-log entry                                                               |
| ---------- | -------------------- | ----------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------- | ----------------------------------- | ------------------------------------------------------------------------------ |
| 2026-10-15 | Initial source audit | Post-generation errors are logged; collection collisions have a separate fail-closed path | `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L203-L226]` | Distinguish checks from enforcement | Not created; [2078](https://github.com/vamseeachanta/digitalmodel/issues/2078) |

## 8. Ownership

- Family quality manager: OrcaFlex riser and mooring quality manager.
- Deputy/co-review role: installation quality manager for installation builders.
- Producing role: module documentation contributor.
- Escalation: unresolved data, clause or tolerance questions go to the family quality manager; scope changes go to the maintainer.
- Clearance requires a second provider; DRAFT is retained pending that review.

## 9. Dependencies

The generator imports PyYAML, schema validation, builder context and registry, post-validation, mooring/vessel routers and the shared null representer. `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L5-L46]`

The extractor reads monolithic YAML and assembles metadata, environment, simulation and generic sections; reverse conversion therefore exists at this pin. `[src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py:L120-L168]`

The variation writer consumes builder ordering, generic section ordering and named-object membership to order references. `[src/digitalmodel/solvers/orcaflex/modular_generator/writers/basefile.py:L38-L124]`

The defaults template contains comments only; it is not an active default-value database. `[src/digitalmodel/solvers/orcaflex/modular_generator/templates/defaults.yml:L1-L5]`

The hybrid-template README validation and test totals are README claims, unverified by this pack; native evidence remains required for acceptance. `[docs/domains/orcaflex/templates/README.md:L1-L19]`

UNVERIFIED: dependency inventory rows, complete external include dependencies and target native version are not resolved here; see section 10.

Repository-wide method search, including scripts/: OrcaFlexModelGenerator.generate_from_template separately assembles component templates and writes a single YAML model. `[src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L206-L266]` The script process_model delegates modular writing to ModularModelGenerator rather than independently implementing it. `[scripts/generate_all_specs.py:L441-L450]` create_wave_file separately formats a WaveTrains text block, including selector-first WaveType and spectral keys; that implementation is a comparison point, not native qualification evidence. `[scripts/generate_env_files.py:L38-L76]` The script catenary computes a weight-only, inextensible touchdown screen, while the package CatenarySolver supplies an elastic residual; neither is promoted to a C10 reference by this search. `[scripts/orcaflex/prepare_slay_campaign.py:L24-L48]` `[src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L193-L201]`

## 10. Coverage and open items

| Open item                           | Missing evidence / work                                                                                                                                                                        | Responsible role       | Issue                                                                                                                                |
| ----------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| Consumer-map completion             | UNVERIFIED: remaining generic/riser/mooring/campaign fields, roller/shape branches, inherited fields, coordinate conventions and dependency inventory rows need a full consumer/residual audit | Module maintainer      | [2078](https://github.com/vamseeachanta/digitalmodel/issues/2078)                                                                    |
| Engineering qualification           | UNVERIFIED: module maturity, target-version compatibility and engineering envelopes remain unqualified; native load/statics and independent modal results are not supplied here                | Family quality manager | [2428](https://github.com/vamseeachanta/digitalmodel/issues/2428), [2424](https://github.com/vamseeachanta/digitalmodel/issues/2424) |
| Standards provenance                | UNVERIFIED: full titles, editions, clause locators, edition consistency and Citation-sidecar coverage require confirmation; clause text not checked against the licensed copy                  | Module maintainer      | [2516](https://github.com/vamseeachanta/digitalmodel/issues/2516)                                                                    |
| Strict YAML and validation coverage | Duplicate-key collapse and incomplete reference/property checks require bounded tests; logged findings are not fail-closed acceptance                                                          | Module maintainer      | [2090](https://github.com/vamseeachanta/digitalmodel/issues/2090), [717](https://github.com/vamseeachanta/digitalmodel/issues/717)   |
| Raw selector order                  | Existing WaveType is not moved ahead of raw dependent keys                                                                                                                                     | Module maintainer      | [2513](https://github.com/vamseeachanta/digitalmodel/issues/2513)                                                                    |
| Line starting shapes                | User-specified shape geometry is not checked against the owning line                                                                                                                           | Module maintainer      | [2514](https://github.com/vamseeachanta/digitalmodel/issues/2514)                                                                    |
| Input provenance                    | Fingerprint and generator/template/design-basis revision records are absent from the inspected output path                                                                                     | Module maintainer      | [2515](https://github.com/vamseeachanta/digitalmodel/issues/2515)                                                                    |
| Portable integration fixtures       | Two existing integration cases remain skipped because of fixed fixture paths                                                                                                                   | Module maintainer      | [2517](https://github.com/vamseeachanta/digitalmodel/issues/2517)                                                                    |
| Native statics defect               | The historical convergence problem remains unverified by this offline preparation                                                                                                              | Family quality manager | [1944](https://github.com/vamseeachanta/digitalmodel/issues/1944)                                                                    |
| Console entry                       | Installed console entry remains absent despite the module CLI                                                                                                                                  | Module maintainer      | [1641](https://github.com/vamseeachanta/digitalmodel/issues/1641)                                                                    |

Coverage: root generator, CLI/module entry, extractor entry and generic extraction, merge, variable substitution, validator dispatch/reference registry, registry/base-file ordering, environment, pipeline line/type, vessel/type, buoy orchestration and core buoy sub-builders were read as source. Six main-path schema files supplied the field catalogue. The defaults template and historical format/template documentation were inspected. The complete module test directory, including builders, integration, routers, schema and the guarded riser/comparison modules, was included in the offline run. Detailed auditing of the remaining branches and the complete library/template corpus is deferred under the consumer-map issue, rather than reported as completed.

No private sidecar was read or written; no sidecar entry is needed for the public code behaviour documented here. No shared-log record was published. Checks requiring native results remain NOT_RUN. These are scope and execution declarations, not engineering acceptance.
