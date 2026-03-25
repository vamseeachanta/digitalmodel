# OrcaFlex Modular Generator: Methods, Data Flow & Architecture

## 1. Three Methods of Creating OrcaFlex Models

There are three ways to define and run an OrcaFlex model in this system.
Each has a different source format, tooling, and use case.

```
 METHOD A: Monolithic          METHOD B: Spec-Driven (Modular)       METHOD C: Campaign
 (Legacy / Reference)          (Primary / Recommended)               (Parametric Studies)
 ________________________      ________________________________      ______________________________
|                        |    |                                |    |                              |
|  Single .dat or .yml   |    |  spec.yml  (human-authored)    |    |  campaign.yml + base spec    |
|  file (2000+ lines)    |    |  (100-200 lines)               |    |  (matrix of variations)      |
|________________________|    |________________________________|    |______________________________|
         |                              |                                      |
         v                              v                                      v
   OrcFxAPI.Model()             ModularModelGenerator               CampaignGenerator
         |                              |                                      |
         v                              v                                      v
   Statics/Dynamics             master.yml + includes/              N x (master.yml + includes/)
         |                              |                                      |
         v                              v                                      v
      Results                   OrcFxAPI.Model()                    OrcFxAPI.Model() x N
                                        |                                      |
                                        v                                      v
                                 Statics/Dynamics                   Statics/Dynamics x N
                                        |                                      |
                                        v                                      v
                                     Results                          Results Matrix
```

### Method A: Monolithic (Legacy / Reference)

A single `.dat` or `.yml` file containing the entire OrcaFlex model definition.
Exported directly from the OrcaFlex GUI or via `OrcFxAPI.SaveData()`.

```
 ┌─────────────────────────────┐
 │  A01 Pliant wave riser.dat  │  <-- 2114 lines, all properties
 │  (or .yml export)           │      in one flat file
 └─────────────┬───────────────┘
               │
               │  OrcFxAPI.Model(filepath)
               v
 ┌─────────────────────────────┐
 │    OrcFxAPI In-Memory       │
 │    Model Object             │
 │    (26 objects)             │
 └─────────────┬───────────────┘
               │
               │  model.CalculateStatics()
               v
 ┌─────────────────────────────┐
 │    Converged Results        │
 │    (tensions, bending, etc) │
 └─────────────────────────────┘
```

**Pros:** Direct, no tooling needed, exact OrcaFlex representation.
**Cons:** Not human-readable, no version control diffing, no parametric capability,
properties scattered across 2000+ lines, no validation before loading.

### Method B: Spec-Driven Modular (Primary / Recommended)

A concise `spec.yml` (100-200 lines) defines the engineering intent.
The `ModularModelGenerator` expands it into modular YAML files that OrcFxAPI loads.

```
 ┌──────────────────────────────┐
 │  spec.yml (182 lines)        │  <-- Human-authored, version-controlled
 │  - metadata                  │      Single source of truth
 │  - environment               │
 │  - riser (or pipeline)       │
 │  - simulation                │
 └──────────────┬───────────────┘
                │
                │  1. Pydantic validation
                v
 ┌──────────────────────────────┐
 │  ProjectInputSpec            │  <-- Schema-validated Python object
 │  (typed, cross-referenced)   │
 └──────────────┬───────────────┘
                │
                │  2. ModularModelGenerator.from_spec(spec)
                │     generator.generate(output_dir)
                v
 ┌──────────────────────────────┐
 │  output_dir/                 │  <-- Generated, not hand-edited
 │  ├── master.yml (13 lines)   │
 │  ├── includes/               │
 │  │   ├── 01_general.yml      │
 │  │   ├── 03_environment.yml  │
 │  │   ├── 04_riser_clumps.yml │
 │  │   ├── 05_riser_ltypes.yml │
 │  │   ├── 06_riser_vessels.yml│
 │  │   ├── 07_riser_lines.yml  │
 │  │   ├── 08_riser_links.yml  │
 │  │   └── 10_groups.yml       │
 │  └── inputs/                 │
 │      └── parameters.yml      │
 └──────────────┬───────────────┘
                │
                │  3. OrcFxAPI.Model()
                │     model.LoadData(master.yml)
                v
 ┌──────────────────────────────┐
 │  OrcFxAPI In-Memory Model    │
 │  (15 objects)                │
 └──────────────┬───────────────┘
                │
                │  4. model.CalculateStatics()
                v
 ┌──────────────────────────────┐
 │  Converged Results           │
 │  (tensions, bending, etc)    │
 └──────────────┬───────────────┘
                │
                │  5. Extract & save
                v
 ┌──────────────────────────────┐
 │  results.json                │
 │  (structured, machine-       │
 │   readable engineering data) │
 └──────────────────────────────┘
```

**Pros:** Human-readable, version-controllable, validated before generation,
parametric-ready, modular (edit one section without touching others).
**Cons:** Requires the generator tooling; not a native OrcaFlex format.

### Method C: Campaign (Parametric Studies)

A campaign file defines a parameter matrix over a base spec.
The `CampaignGenerator` produces N models from the combinations.

```
 ┌──────────────────────────┐    ┌────────────────────────────┐
 │  base spec.yml           │    │  campaign.yml              │
 │  (template model)        │    │  - water_depths: [80,100]  │
 └───────────┬──────────────┘    │  - environments: [1yr,10yr]│
             │                   │  - tensions: [50,100]      │
             │                   └──────────┬─────────────────┘
             └────────┬────────────────────┘
                      │
                      │  CampaignGenerator(campaign.yml)
                      │  gen.generate(output_dir)
                      v
 ┌────────────────────────────────────────────────────┐
 │  output_dir/                                       │
 │  ├── WD080_ENV1yr_T50/  (master.yml + includes/)   │
 │  ├── WD080_ENV1yr_T100/ (master.yml + includes/)   │
 │  ├── WD080_ENV10yr_T50/ (master.yml + includes/)   │
 │  ├── WD080_ENV10yr_T100/(master.yml + includes/)   │
 │  ├── WD100_ENV1yr_T50/  ...                        │
 │  └── ...                 (2 x 2 x 2 = 8 runs)     │
 └────────────────────────────────────────────────────┘
```


## 2. Data Flow: spec.yml to Results

### Detailed Pipeline (Method B)

```
  USER AUTHORS                    SYSTEM GENERATES                 ORCAFLEX COMPUTES
  ─────────────                   ─────────────────                ─────────────────

  ┌─────────┐    validate      ┌───────────────┐   build       ┌────────────────┐
  │spec.yml │───────────────>  │ProjectInputSpec│─────────────> │Modular YAML    │
  └─────────┘    (Pydantic)    └───────┬───────┘  (Builders)   │(master.yml +   │
                                       │                        │ includes/*.yml)│
                                       │                        └───────┬────────┘
                                       │                                │
                               ┌───────v───────┐                       │ LoadData()
                               │BuilderContext  │                       │
                               │(cross-refs)    │               ┌──────v─────────┐
                               └───────────────┘                │OrcFxAPI.Model  │
                                                                └──────┬─────────┘
                                                                       │
                                                          CalculateStatics()
                                                                       │
                                                                ┌──────v─────────┐
                                                                │Converged Model │
                                                                └──────┬─────────┘
                                                                       │
                                                          StaticResult()
                                                          RangeGraph()
                                                          TimeHistory()
                                                                       │
                                                                ┌──────v─────────┐
                                                                │results.json    │
                                                                │(tensions,      │
                                                                │ bending,       │
                                                                │ curvature)     │
                                                                └────────────────┘
```

### Builder Execution Order

Each builder reads the spec and writes one YAML file. Builders execute
in dependency order so downstream builders can reference upstream entities.

```
  spec.yml ──> ProjectInputSpec
                    │
                    ├──> GeneralBuilder        ──>  01_general.yml        (order=10)
                    ├──> EnvironmentBuilder     ──>  03_environment.yml    (order=30)
                    ├──> RiserClumpTypeBuilder  ──>  04_riser_clumps.yml   (order=38)
                    ├──> RiserLineTypeBuilder   ──>  05_riser_ltypes.yml   (order=50)
                    ├──> RiserVesselBuilder     ──>  06_riser_vessels.yml  (order=60)
                    ├──> RiserLinesBuilder      ──>  07_riser_lines.yml    (order=92)
                    ├──> RiserLinksBuilder      ──>  08_riser_links.yml    (order=93)
                    └──> GroupsBuilder          ──>  10_groups.yml         (order=100)
                                                         │
                    BuilderContext flows ─────────────────┘
                    downstream (typed fields:
                    line_type_names, riser_line_names,
                    riser_link_names, vessel_name, etc.)
```

### Cross-Builder Data Sharing

```
  RiserLineTypeBuilder                RiserLinesBuilder
  ┌──────────────────┐                ┌──────────────────┐
  │ Reads: spec.riser│                │ Reads: spec.riser│
  │  .line_types     │                │  .lines          │
  │                  │                │                  │
  │ Writes:          │   context      │ Reads from ctx:  │
  │  05_riser_lt.yml │──────────────> │  line_type_names │
  │                  │                │                  │
  │ Registers:       │                │ Writes:          │
  │  ctx.riser_      │                │  07_riser_lines  │
  │  line_type_names │                │                  │
  └──────────────────┘                │ Registers:       │
                                      │  ctx.riser_      │
                                      │  line_names      │
                                      └────────┬─────────┘
                                               │ context
                                               v
                                      ┌──────────────────┐
                                      │ RiserLinksBuilder │
                                      │ Reads from ctx:  │
                                      │  riser_line_names│
                                      │                  │
                                      │ Writes:          │
                                      │  08_riser_links  │
                                      └──────────────────┘
```


## 3. Single Source of Truth

### Where is the data held?

```
  ┌────────────────────────────────────────────────────────────────────┐
  │                    SINGLE SOURCE OF TRUTH                         │
  │                                                                    │
  │                        spec.yml                                    │
  │                                                                    │
  │  Everything downstream is DERIVED from this file.                  │
  │  It is the only file a user needs to author or version-control.    │
  └───────────────────────────────┬────────────────────────────────────┘
                                  │
                    ┌─────────────┴─────────────┐
                    │                           │
              ┌─────v──────┐            ┌───────v───────┐
              │  GENERATED │            │   COMPUTED    │
              │  (derived) │            │   (derived)   │
              │            │            │               │
              │ master.yml │            │ results.json  │
              │ includes/  │            │ benchmark.json│
              │ params.yml │            │ .sim files    │
              └────────────┘            └───────────────┘

  RULE: Never hand-edit generated files. Change spec.yml and regenerate.
```

### Data Ownership by File

```
  ┌──────────────────────────────────────────────────────────────────────────┐
  │ FILE               │ OWNER      │ ROLE                                  │
  ├──────────────────────────────────────────────────────────────────────────┤
  │ spec.yml           │ ENGINEER   │ Single source of truth. Defines the   │
  │                    │            │ complete model intent in 100-200 lines│
  │                    │            │ of human-readable YAML.               │
  ├──────────────────────────────────────────────────────────────────────────┤
  │ master.yml         │ GENERATOR  │ Entry point for OrcFxAPI. Lists       │
  │                    │            │ include files in dependency order.     │
  │                    │            │ DO NOT EDIT — regenerate from spec.    │
  ├──────────────────────────────────────────────────────────────────────────┤
  │ includes/*.yml     │ GENERATOR  │ OrcaFlex property sections. One file  │
  │                    │            │ per builder (environment, lines, etc). │
  │                    │            │ DO NOT EDIT — regenerate from spec.    │
  ├──────────────────────────────────────────────────────────────────────────┤
  │ parameters.yml     │ GENERATOR  │ Extracted simulation parameters for   │
  │                    │            │ post-processing tools.                │
  ├──────────────────────────────────────────────────────────────────────────┤
  │ results.json       │ RUNNER     │ Extracted engineering results from    │
  │                    │            │ converged OrcaFlex analysis.          │
  ├──────────────────────────────────────────────────────────────────────────┤
  │ monolithic .dat    │ REFERENCE  │ Legacy OrcaFlex export. Used only for │
  │                    │            │ benchmarking against modular output.  │
  └──────────────────────────────────────────────────────────────────────────┘
```

### Regeneration Guarantee

Any change to the model should be made in `spec.yml` only. The pipeline
guarantees deterministic output:

```
  Same spec.yml  ──>  Same master.yml + includes/  ──>  Same OrcaFlex results
                                                         (within solver tolerance)
```


## 4. Input Data Comparison: Monolithic vs Spec vs Modular

### File Size Comparison (Pliant Wave Riser)

```
  ┌────────────────────────────┬────────┬──────────────────────────────────┐
  │ Format                     │ Lines  │ Notes                            │
  ├────────────────────────────┼────────┼──────────────────────────────────┤
  │ Monolithic .yml (raw)      │  2114  │ Every OrcaFlex property, most    │
  │                            │        │ are defaults. Not human-readable │
  ├────────────────────────────┼────────┼──────────────────────────────────┤
  │ spec.yml (input)           │   182  │ Only engineering-relevant params │
  │                            │        │ Human-authored, reviewed         │
  ├────────────────────────────┼────────┼──────────────────────────────────┤
  │ Modular set (generated)    │   363  │ master.yml (13) + 8 includes    │
  │                            │        │ Machine-generated, not edited    │
  └────────────────────────────┴────────┴──────────────────────────────────┘

  Compression: spec.yml is 91% smaller than monolithic.
  Modular set is 83% smaller than monolithic.
```

### What Each Format Contains

```
  MONOLITHIC (.dat/.yml)           SPEC.YML                    MODULAR (generated)
  ─────────────────────            ──────────                  ───────────────────

  General settings .......... [implicit defaults] .......... 01_general.yml
  Line Types (ALL props) .... line_types (key params) ...... 05_riser_ltypes.yml
    - Name                     - name                         - Name
    - Category                 [derived: General]             - Category: General
    - OD, ID                   - outer_diameter               - OD, ID
    - MassPerUnitLength        - mass_per_length              - MassPerUnitLength
    - EA, EI, GJ               - axial_stiffness             - EA, EI, GJ
    - Cd, Ca, CdAxial          - bending_stiffness           - Cd, Ca, CdAxial
    - NormalDragLift...        [defaults applied]             [defaults from builder]
    - SeabedContact...         [defaults applied]             [defaults from builder]
    - Pen color, Thickness     [not in spec]                  [not generated]
    ~60 properties per type     ~12 properties per type        ~20 properties per type

  Vessel properties ......... vessel (key params) ........... 06_riser_vessels.yml
    - ALL RAO data              - name, position             - Name, position
    - ALL connection data       - orientation                - orientation
    - 100+ properties           - primary_motion             - motion settings
                                 ~8 properties                ~10 properties

  Line definitions .......... lines (key params) ............ 07_riser_lines.yml
    - Connection arrays         - end_a, end_b               - Connection arrays
    - ALL segment data          - sections                   - Segment arrays
    - Statics settings          - statics_method             - StaticsStep1/2
    - 50+ properties            - statics_step2              - ~25 properties
                                 ~20 properties

  Link definitions .......... links (key params) ............ 08_riser_links.yml
    - Connection arrays         - connections                - Connection arrays
    - Stiffness, length         - stiffness                  - Stiffness
    - Pen color                 - unstretched_length         - UnstretchedLength
    ~15 properties               ~7 properties                ~5 properties

  Environment (ALL) ......... environment (key params) ...... 03_environment.yml
    - Wave, current, wind       - wave height, period        - Wave, current, wind
    - Seabed model              - current speed, profile     - Seabed model
    - ALL spectral params       - water depth                - Selected params
    ~80 properties               ~15 properties               ~30 properties
```

### Key Differences

```
  ┌──────────────────────┬───────────────┬───────────────┬──────────────────┐
  │ Aspect               │ Monolithic    │ spec.yml      │ Modular Set      │
  ├──────────────────────┼───────────────┼───────────────┼──────────────────┤
  │ Author               │ OrcaFlex GUI  │ Engineer      │ Generator        │
  │ Lines                │ ~2000         │ ~180          │ ~360             │
  │ Human-readable       │ No            │ Yes           │ Partially        │
  │ Version-controllable │ Difficult     │ Easy          │ Not recommended  │
  │ Validated pre-load   │ No            │ Yes (Pydantic)│ No (derived)     │
  │ Parametric-ready     │ No            │ Yes           │ No (derived)     │
  │ Contains defaults    │ All 1000+     │ None          │ Relevant only    │
  │ Editable             │ OrcaFlex GUI  │ Text editor   │ Never            │
  │ Diffable (git)       │ Noisy         │ Clean         │ Noisy            │
  │ OrcaFlex-native      │ Yes           │ No            │ Yes (via master) │
  └──────────────────────┴───────────────┴───────────────┴──────────────────┘
```


## 5. CLI Entry Points

### Commands Available

```
  ┌──────────────────────────────────────────────────────────────────────────────┐
  │ COMMAND                          │ PURPOSE                                  │
  ├──────────────────────────────────────────────────────────────────────────────┤
  │ uv run python -m digitalmodel   │                                          │
  │   .solvers.orcaflex              │                                          │
  │   .modular_generator             │                                          │
  │                                  │                                          │
  │   validate -i spec.yml           │ Check spec against Pydantic schema       │
  │   generate -i spec.yml -o dir/   │ Generate modular YAML (no OrcaFlex run)  │
  │   campaign camp.yml -o dir/      │ Generate N models from parameter matrix  │
  │   campaign camp.yml --preview    │ Show matrix without generating           │
  ├──────────────────────────────────────────────────────────────────────────────┤
  │ uv run python scripts/           │                                          │
  │                                  │                                          │
  │   run_riser_analysis.py spec.yml │ End-to-end: spec -> model -> results     │
  │   run_riser_analysis.py --library│ Run all specs in library                 │
  │   benchmark_riser_library.py     │ Compare monolithic vs modular results    │
  │   mesh_sensitivity_riser.py      │ Coarse/medium/fine convergence matrix    │
  └──────────────────────────────────────────────────────────────────────────────┘
```

### End-to-End Runner Flowchart

```
  uv run python scripts/run_riser_analysis.py spec.yml [--output dir] [--dynamics]

  ┌─────────────────┐
  │ [1] Load spec   │  yaml.safe_load() -> ProjectInputSpec(**data)
  │     & validate  │  Pydantic validates types, ranges, cross-references
  └────────┬────────┘
           │
  ┌────────v────────┐
  │ [2] Generate    │  ModularModelGenerator.from_spec(spec)
  │     modular     │  generator.generate(output_dir / "modular")
  │                 │  -> master.yml + includes/*.yml
  └────────┬────────┘
           │
  ┌────────v────────┐
  │ [3] Load into   │  model = OrcFxAPI.Model()
  │     OrcFxAPI    │  model.LoadData(master.yml)
  └────────┬────────┘
           │
  ┌────────v────────┐
  │ [4] Run statics │  model.CalculateStatics()
  │                 │  -> converged equilibrium
  └────────┬────────┘
           │
  ┌────────v────────┐
  │ [5] Extract     │  line.StaticResult("Effective Tension", oeEndA)
  │     results     │  line.RangeGraph("Bend Moment")
  │                 │  link.StaticResult("Tension")
  └────────┬────────┘
           │
  ┌────────v────────┐
  │ [6] Save JSON   │  results.json with tensions, bending, curvature
  │     & print     │  per line and per link
  └─────────────────┘
```


## 6. Spec.yml Schema Structure (Single Source of Truth)

```yaml
# ┌─────────────────────────────────────────────────────────┐
# │  spec.yml — the ONLY file an engineer needs to author   │
# └─────────────────────────────────────────────────────────┘

metadata:                        # WHO and WHAT
  name: "a01_pliant_wave_riser"  #   Model identifier
  description: "Pliant wave..."  #   Human description
  structure: riser               #   "riser" or "pipeline"
  operation: production          #   Operational context
  project: "OrcaFlex A01"       #   Project reference

environment:                     # WHERE (sea conditions)
  water:
    depth: 100                   #   Water depth (m)
    density: 1.025               #   Seawater density (te/m3)
  seabed:
    stiffness: {normal: 100}     #   Soil stiffness (kN/m/m2)
  waves:
    type: dean_stream            #   Wave theory
    height: 6                    #   Hs (m)
    period: 7                    #   Tp (s)
  current:
    speed: 0.7                   #   Reference speed (m/s)
    profile: [[0,1.0],[100,0.3]] #   Depth-varying profile
  wind:
    speed: 0                     #   Wind speed (m/s)

riser:                           # WHAT (structural definition)
  vessel:                        #   Floating unit
    name: "FPSO"
    position: [0, -48, 0]
    orientation: [0, 0, 90]

  line_types:                    #   Material properties
    - name: "10in_product"
      outer_diameter: 0.3926
      inner_diameter: 0.254
      mass_per_length: 0.22135
      bending_stiffness: 361.85
      axial_stiffness: 650000

  clump_types:                   #   Discrete attachments
    - name: "Clamp Type"
      mass: 0.470
      volume: 0.060

  links:                         #   Tethers / spring-dampers
    - name: "Tether Pliant Simple"
      link_type: tether
      connections:
        - {object_name: Anchored, x: -116, y: 0, z: 1}
        - {object_name: "10in Pliant Simple", x: 0, y: 0, z: 176, z_relative_to: "End A"}
      unstretched_length: 9.9
      stiffness: 367000

  lines:                         #   Line definitions
    - name: "10in Pliant Simple"
      configuration: pliant_wave
      end_a: {type: vessel, name: FPSO, position: [33,2,-7.5]}
      end_b: {type: anchor, position: [-169,0,0.1963]}
      sections:                  #   Mesh definition
        - {line_type: "10in_product", length: 97.5, segment_length: 2}
        - {line_type: "10in_with_small_buoys", length: 65, segment_length: 2}
        - {line_type: "10in_product", length: 53, segment_length: 2}
      statics_step2: "Full statics"

simulation:                      # HOW LONG to run
  duration: 42
  time_step: 0.1
```


## 7. Benchmark Validation (2026-02-08)

The modular generator is validated against monolithic reference models.
All 4 riser configurations pass within engineering tolerance.

```
  ┌────────────────────────┬────────────┬────────────┬────────────┬────────┐
  │ Model                  │ Mono EA(kN)│ Mod EA(kN) │ Diff %     │ Status │
  ├────────────────────────┼────────────┼────────────┼────────────┼────────┤
  │ Catenary Riser         │    132.4   │    132.4   │   0.0%     │ PASS   │
  │ Lazy Wave (Distributed)│     90.7   │     90.5   │   0.2%     │ PASS   │
  │ Lazy Wave (Discrete)   │     90.7   │     90.7   │   0.0%     │ PASS   │
  │ Pliant Wave (+ tether) │    116.6   │    115.7   │   0.8%     │ PASS   │
  │ Steep Wave             │     87.3   │     87.6   │   0.3%     │ PASS   │
  └────────────────────────┴────────────┴────────────┴────────────┴────────┘

  Tolerance: 5% relative OR 10 kN absolute (tension)
             15% relative OR 5 kN.m absolute (bending)
```

### Mesh Sensitivity (Tension is mesh-insensitive)

```
  ┌────────────────────────┬───────────┬───────────┬───────────┬──────────┐
  │ Model                  │ Coarse 5m │ Medium 2m │ Fine 1m   │ Diff C/F │
  ├────────────────────────┼───────────┼───────────┼───────────┼──────────┤
  │ Catenary  MaxT (kN)    │   132.7   │   132.4   │   132.4   │   0.2%   │
  │ Lazy Wave MaxT (kN)    │    90.4   │    90.5   │    90.5   │   0.1%   │
  │ Pliant    MaxT (kN)    │   115.6   │   115.6   │   115.7   │   0.1%   │
  │ Steep     MaxT (kN)    │    87.5   │    87.6   │    87.6   │   0.0%   │
  ├────────────────────────┼───────────┼───────────┼───────────┼──────────┤
  │ Pliant    MaxB (kN.m)  │    25.2   │    31.1   │    34.8   │  27.6%   │
  │ Steep     MaxB (kN.m)  │    14.2   │    15.5   │    15.7   │   9.3%   │
  └────────────────────────┴───────────┴───────────┴───────────┴──────────┘

  Finding: Tension converges at coarse mesh. Bending requires fine mesh.
  Recommendation: Use medium (2m) for screening, fine (1m/0.5m) for production.
```
