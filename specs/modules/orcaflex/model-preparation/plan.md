# Plan: OrcaFlex Model Preparation from YAML Scratch

## Executive Summary

This plan outlines how to prepare OrcaFlex models in .yml format from scratch using the 18 available OrcaFlex skills and the established component library system. Based on analysis of **441 YAML model files** in the repository, this document captures proven patterns, naming conventions, and best practices.

---

## 1. Available OrcaFlex Skills for Model Preparation

### Core Model Building Skills
| Skill | Purpose | Phase |
|-------|---------|-------|
| **orcaflex-model-generator** | Generate complete models from templates using component assembly | Creation |
| **orcaflex-vessel-setup** | Configure 6-DOF vessels with RAOs and hydrodynamic properties | Setup |
| **orcaflex-environment-config** | Configure wave/current/wind/seabed conditions | Setup |
| **orcaflex-line-wizard** | Auto-calculate line tensions/lengths | Configuration |
| **orcaflex-rao-import** | Import RAOs from AQWA, OrcaWave, or CSV | Hydrodynamics |
| **orcaflex-file-conversion** | Convert between .yml/.dat/.sim formats | Conversion |

### Analysis & Optimization Skills
| Skill | Purpose | Phase |
|-------|---------|-------|
| **orcaflex-modeling** | Run simulations (static/dynamic) | Execution |
| **orcaflex-static-debug** | Troubleshoot convergence issues | Debugging |
| **orcaflex-mooring-iteration** | Optimize mooring pretensions | Optimization |
| **orcaflex-modal-analysis** | Extract natural frequencies/mode shapes | Analysis |
| **orcaflex-installation-analysis** | Model installation sequences | Specialized |

### Post-Processing Skills
| Skill | Purpose | Phase |
|-------|---------|-------|
| **orcaflex-post-processing** | Extract statistics, time series, range graphs | Results |
| **orcaflex-extreme-analysis** | Extract extreme values with linked statistics | Results |
| **orcaflex-operability** | Weather downtime and operability envelopes | Results |
| **orcaflex-visualization** | Generate interactive HTML reports | Reporting |
| **orcaflex-results-comparison** | Compare multiple simulations | Verification |
| **orcaflex-code-check** | Validate against DNV/API/ISO standards | Compliance |
| **orcaflex-batch-manager** | Parallel batch processing | Optimization |

---

## 2. YAML Structure for OrcaFlex Models

### Two Valid YAML Formats

**CRITICAL**: OrcaFlex supports two YAML formats. **Format B is now recommended** for all new models due to improved modularity, reusability, and parametric analysis support.

#### Format A: Modern Flat YAML (Legacy - for simple/quick models only)
```yaml
%YAML 1.1
# Type: Model
# Program: OrcaFlex 11.4
---
General:
  UnitsSystem: SI
  ImplicitConstantTimeStep: 0.1
  StageDuration: [30, 1000]

Environment:
  WaterDepth: 1200
  Density: 1.025
  SeabedType: Elastic

VesselTypes:
  - Name: FPSO_Generic
    Length: 280
    Draughts:
      - Name: Ballast
        Mass: 150000

LineTypes:
  - Name: Chain_R4_84mm
    OD: 0.252
    EA: 850e6

Vessels:
  - Name: FPSO
    VesselType: FPSO_Generic
    InitialPosition: [0, 0, 0]

Lines:
  - Name: Mooring_1
    LineType: Chain_R4_84mm
    Connection: [FPSO, Anchored]
```

#### Format B: Include-Based Structure (RECOMMENDED for All New Models)
```yaml
%YAML 1.1
# Type: Model
# Program: OrcaFlex 11.5
---
# Input Parameters (for parametric analysis)
_inputs:
  water_depth: 100
  hs: 3.5
  tp: 12.0
  current_speed: 1.2

# Include files - load in order
- includefile: includes/01_general.yml
- includefile: includes/02_var_data.yml
- includefile: includes/03_environment.yml
- includefile: includes/04_vessel_types.yml
- includefile: includes/05_line_types.yml
- includefile: includes/06_vessels.yml
- includefile: includes/07_lines.yml
- includefile: includes/08_buoys.yml
```

**Benefits of Format B:**
- **Modularity**: Reuse components across models
- **Parametric Analysis**: Input parameter blocks for easy variation
- **Version Control**: Track changes to specific sections
- **Maintainability**: Smaller, focused files
- **Template Library**: Available at `templates/mooring_systems/` and `templates/risers/`

**Conversion Tool**: Use `scripts/conversion/yaml_to_include.py` to convert flat YAML to include-based format.

**⚠️ WARNING**: Hybrid YAML (mixing formats) is NOT supported.

### Standard YAML Section Hierarchy
```yaml
%YAML 1.1
# Type: Model
# Program: OrcaFlex 11.4
---
General:              # Simulation settings (units, stages, timestep)
VariableData:         # Lookup tables (drag, stiffness curves)
Environment:          # Wave, current, wind, seabed
VesselTypes:          # Vessel templates (with RAOs/draughts)
LineTypes:            # Line templates (OD, ID, EA, EI, mass)
BuoyTypes:            # Buoy templates
ClumpTypes:           # Clump weight templates
WingTypes:            # Wing/strake templates
Vessels:              # Vessel instances
6DBuoys:              # 6-DOF buoy instances
3DBuoys:              # 3-DOF buoy instances
Lines:                # Line instances (moorings, risers)
Winches:              # Winch controls (tension, length)
Links:                # Link constraints
Shapes:               # Visual/anchor geometry
Constraints:          # Contact definitions
Groups:               # Object grouping
```

---

## 3. File Organization Structure (441 Files Analyzed)

### Primary Directory Categories
```
docs/modules/orcaflex/
├── examples/                    # 100+ OrcaFlex official examples
│   ├── raw/                     # Original .dat/.sim files
│   ├── yml/                     # Converted YAML versions
│   ├── catalog/                 # Searchable catalog indexes
│   └── gom_scr_design_study/    # Design study (configs/models/results)
│
├── mooring/                     # 80+ mooring system files
│   ├── buoy/                    # CALM, SPM, metocean buoys
│   ├── riser/                   # Mooring riser configurations
│   ├── semi/                    # Semi-submersible mooring
│   └── drillship-riser/         # Drillship mooring analysis
│
├── risers/                      # 60+ riser system files
│   ├── drilling/                # Drilling risers (TTR, BOP)
│   ├── production/              # Production risers (SCR, TTR, lazy wave)
│   ├── completion/              # Completion risers
│   ├── intervention/            # Intervention operations
│   └── disconnect/              # Disconnectable systems
│
├── templates/                   # Component libraries & templates
│   ├── components/
│   │   ├── vessels/             # Vessel type definitions
│   │   ├── lines/               # Line/pipe materials
│   │   ├── environment/         # Environmental profiles
│   │   ├── equipment/           # Equipment definitions
│   │   └── materials/           # Material specifications
│   └── risers/scr_catenary/     # SCR riser templates
│
├── pipeline/                    # 20+ pipeline files
│   ├── installation/            # Installation sequences
│   └── spanning/                # Spanning analysis
│
├── aqwa/                        # AQWA/OrcaWave integration
│   ├── to_orcaflex/             # AQWA RAO import
│   ├── to_orcawave/             # OrcaWave diffraction
│   └── multibody_test1/         # Multi-vessel interaction
│
├── support/                     # 70+ reference files
│   ├── base_file/               # Base model templates (dated)
│   ├── animations/              # Visualization post-processing
│   ├── naval_architecture/      # Hull form data
│   ├── orcawave/                # Hydrodynamic data
│   └── optimization/            # Design optimization studies
│
├── notes/                       # Analysis guidance
│   ├── pre_yml_file_management/ # Multi-file organization
│   ├── pre_environment/         # Environmental config
│   ├── anl_modal/               # Modal analysis
│   └── post_animations/         # Post-processing
│
└── structures/                  # Fixed structures
    ├── mudmat/                  # Mudmat interactions
    └── salvage/                 # Salvage operations
```

### Example Category Index (A01-L05, Z Series)
| Code | Category | Description |
|------|----------|-------------|
| **A01-A06** | Riser Systems | Catenary, lazy wave, steep wave, disconnecting turrets |
| **B01-B06** | Drilling/Intervention | Drilling risers, BOP, running tools |
| **C03-C10** | Mooring & Buoys | SPM, CALM, metocean, fenders, fish farms |
| **D02-D04** | Installation | Pull-in, lay-on, J-tube |
| **E01-E08** | Stingers & Pipelay | Explicit geometry, hinged, articulated, davits |
| **F01-F06** | Subsea Equipment | Lowering, manifolds, heave compensation |
| **G04** | Anchors | Anchor deployments |
| **H01-H03** | Floating Systems | Floating systems & transfers |
| **I01** | Streamer Arrays | Seismic streamers |
| **J01** | Deployment | Deployment with subsea units |
| **K01-K06** | Wind Energy | FOWT, spars, floating structures |
| **L01-L05** | Vessels | Default, spar, semi-sub, floating, jack-up |
| **Z02, Z09** | Advanced | Line-on-line, vessel time history |

---

## 4. VariableData Lookup Patterns

### Structure for Tabular Hydrodynamic Data
```yaml
VariableData:
  Dragcoefficient:
    - Name: Generic Drag Curve
      IndependentValue, DependentValue:
        - [10e3, 1.2]    # Reynolds number, Cd
        - [50e3, 1.2]
        - [240e3, 1.0]
        - [500e3, 0.8]
      Hysteretic: No

  Verticalvariationfactor:
    - Name: Current Profile
      IndependentValue, DependentValue:
        - [0, 1.0]       # Depth ratio, factor
        - [0.5, 0.8]
        - [1.0, 0.5]

  Constrainttranslationalstiffness:
    - Name: Fender Characteristic
      IndependentValue, DependentValue:
        - [0, 0]         # Deflection, Force
        - [0.5, 500]
        - [1.0, 2000]

  Axialstiffness:
    - Name: Hawser Stiffness
      IndependentValue, DependentValue:
        - [0, 0]         # Strain, Tension
        - [0.02, 500e3]
        - [0.05, 1200e3]
```

### Common Lookup Types
| VariableData Type | Use Case |
|-------------------|----------|
| `Dragcoefficient` | Reynolds number vs Cd curves |
| `Verticalvariationfactor` | Wind/current depth profiles |
| `Constrainttranslationalstiffness` | Fender force-deflection |
| `Linetypediameter` | Compliance profiles |
| `Axialstiffness` | Non-linear hawser/mooring properties |
| `Bendingstiffness` | Flex pipe bending characteristics |

---

## 5. CSV Component Lookup Tables

### Risers Catalog (`templates/components/lines/risers.csv`)
| RiserID | OD (m) | ID (m) | Wall (mm) | Material | Mass (kg/m) | EI (N.m²) | EA (N) | MaxTension (kN) |
|---------|--------|--------|-----------|----------|-------------|-----------|--------|-----------------|
| SCR_10inch_X65 | 0.273 | 0.249 | 12.0 | X65 | 78.5 | 4.2e7 | 7.8e9 | 3200 |
| SCR_12inch_X65 | 0.324 | 0.300 | 12.0 | X65 | 93.2 | 7.1e7 | 9.2e9 | 3800 |
| TTR_9inch_X80 | 0.229 | 0.209 | 10.0 | X80 | 55.4 | 2.8e7 | 6.5e9 | 4200 |
| LWR_12inch | 0.324 | 0.300 | 12.0 | X65 | 93.2 | 7.1e7 | 9.2e9 | 3800 |
| FLEX_6inch | 0.168 | 0.152 | 8.0 | Flex | 45.0 | 1.5e5 | 2.8e8 | 800 |

### Pipelines Catalog (`templates/components/lines/pipelines.csv`)
| PipeID | OD (m) | Wall (mm) | Coating (mm) | Material | MBL (kN) |
|--------|--------|-----------|--------------|----------|----------|
| 16inch_Export | 0.406 | 19.1 | 40 | X65 | 5200 |
| 20inch_Export | 0.508 | 22.2 | 50 | X65 | 6800 |
| PIP_12x20 | 0.508 | - | - | PIP | 4500 |

### Umbilicals Catalog (`templates/components/lines/umbilicals.csv`)
| UmbilicalID | OD (m) | Mass (kg/m) | MBL (kN) | Cores |
|-------------|--------|-------------|----------|-------|
| Static_6inch | 0.152 | 35.0 | 450 | 12 |
| Dynamic_4inch | 0.102 | 22.0 | 280 | 8 |

---

## 6. Naming Conventions (From 441 Files)

### File Naming Patterns
| Pattern | Example | Usage |
|---------|---------|-------|
| Descriptive | `A01 Catenary riser.yml` | Official examples |
| Water level | `model_hwl.yml`, `model_lwl.yml` | High/low/mid water level |
| Water depth | `water_depth_200m.yml` | Depth variants |
| Return period | `1yr_storm.yml`, `100yr_storm.yml` | Environmental severity |
| Heading | `000deg.yml`, `090deg.yml` | Environmental direction |
| Load condition | `_BAL.yml`, `_FUL.yml` | Ballast/full load |

### Object Naming Patterns
| Object Type | Convention | Examples |
|-------------|------------|----------|
| Vessels | Type prefix + ID | `FPSO_01`, `FST1`, `Drillship_A` |
| Buoys | Function + type | `CALM_Buoy`, `SPM_Main`, `Metocean_Buoy` |
| Lines | Function + number | `Mooring_1`, `Riser_SCR`, `Hawser_Bow` |
| Line Types | Material + size | `Chain_R4_84mm`, `Wire_6x36_76mm` |
| Anchors | Position + type | `Anchor_NE`, `Pile_01` |

---

## 7. Model Preparation Workflow (From Scratch)

### Phase 1: Requirements Definition
**Objective**: Define what model type and analysis scope

1. Select model type:
   - Mooring analysis (CALM, SALM, spread mooring)
   - Riser analysis (SCR, lazy wave, pliant wave)
   - Installation analysis (S-lay, J-lay, lowering)
   - Combined mooring + riser system

2. Define analysis objectives:
   - Static equilibrium only
   - Dynamic response (time domain)
   - Operability assessment
   - Fatigue analysis

3. Identify required inputs:
   - Vessel RAOs (from AQWA or OrcaWave)
   - Metocean data (Hs, Tp, current profiles)
   - Component specifications (MBL, EA, diameters)

### Phase 2: Component Assembly
**Skill**: `orcaflex-model-generator`

1. Select from component libraries:
   ```
   templates/components/
   ├── vessels/     # 20+ vessel specs (FPSO, drillship, pipelay)
   ├── lines/       # 50+ line catalogs (chains, wire, flex pipe)
   ├── materials/   # 15+ material grades
   ├── environment/ # 10+ metocean regions (GoM, North Sea)
   └── equipment/   # Buoys, connectors, fairleads
   ```

2. Create configuration YAML:
   ```yaml
   # config.yml
   model_type: mooring_analysis
   vessel:
     id: fpso_generic_spread
     draught: ballast
   mooring:
     type: spread_mooring
     num_legs: 8
     chain_grade: R4
   environment:
     region: gulf_of_mexico
     condition: 100_year_storm
   ```

3. Generate base model using lookup tables

### Phase 3: Vessel Setup
**Skill**: `orcaflex-vessel-setup`

1. Import vessel RAOs:
   - Use `orcaflex-rao-import` for AQWA/OrcaWave data
   - Define multiple draughts (ballast, laden, intermediate)

2. Configure vessel properties:
   ```yaml
   VesselTypes:
     - Name: FPSO_Generic
       Length: 280
       Draughts:
         - Name: Ballast
           Mass: 150000
           CentreOfMass: [140, 0, 8]
           RAOOrigin: [140, 0, 0]
           DisplacementRAOs:
             - RAODirection: 0
               RAOPeriodOrFrequency, RAOSurgeAmplitude, ...:
                 - [5, 0.1, ...]
   ```

3. Set initial position and orientation

### Phase 4: Environment Configuration
**Skill**: `orcaflex-environment-config`

1. Configure wave conditions:
   ```yaml
   Environment:
     WaterDepth: 1200
     WaterDensity: 1025
     SeabedType: Elastic
     SeabedNormalStiffness: 100

     WaveTrains:
       - Name: Swell
         WaveType: JONSWAP
         Hs: 3.5
         Tz: 9.5
         GammaJONSWAP: 2.4
         Direction: 180

     CurrentMethod: Interpolated
     RefCurrentSpeed: 1.2
     RefCurrentDirection: 180
   ```

2. Define current profiles using VariableData:
   ```yaml
   VariableData:
     Verticalvariationfactor:
       - Name: GOM Current Profile
         IndependentValue, DependentValue:
           - [0.0, 1.0]
           - [0.1, 0.9]
           - [0.5, 0.5]
           - [1.0, 0.2]
   ```

3. Configure seabed properties (friction, stiffness)

### Phase 5: Line Configuration
**Skill**: `orcaflex-line-wizard`

1. Define line types from catalog:
   ```yaml
   LineTypes:
     - Name: Studless_Chain_R4_84mm
       Category: Chain
       OD: 0.252
       MassPerUnitLength: 139.8
       EA: 850e6
       EI: 0
       GJ: 0
       Cdx: 2.4
       Cdy: 1.15
       Cmx: 2.0
       Cmy: 1.0
   ```

2. Create line instances with connections:
   ```yaml
   Lines:
     - Name: Mooring_Leg_1
       LineType: Studless_Chain_R4_84mm
       Length: 2500
       TargetSegmentLength: 10
       EndAConnection: FPSO
       EndAX: 10
       EndAY: 20
       EndAZ: -5
       EndBConnection: Anchored
       EndBX: 500
       EndBY: 400
       EndBZ: -150
   ```

3. Use Line Setup Wizard for auto-calculation of tensions/lengths

### Phase 6: Model Validation
**Skill**: `orcaflex-static-debug`

1. Run static analysis
2. Check for common issues:
   - Line catenary divergence
   - Vessel equilibrium problems
   - Anchor position errors
   - Buoy instability

3. Iterative debugging:
   - Adjust solver damping (1-100 range)
   - Fix connectivity issues
   - Resolve tension problems

### Phase 7: Analysis Execution
**Skill**: `orcaflex-modeling`

1. Configure two-stage analysis (common pattern):
   ```yaml
   General:
     StageDuration: [30, 1000]  # Stage 1: static, Stage 2: dynamic
     ImplicitConstantTimeStep: 0.1
   ```

2. Run simulations:
   - Static analysis first
   - Dynamic analysis with appropriate duration

3. Batch processing for multiple cases:
   - Use `orcaflex-batch-manager` for parallel execution

### Phase 8: Results Processing
**Skills**: `orcaflex-post-processing`, `orcaflex-extreme-analysis`

1. Extract key results:
   - Maximum tensions (mooring lines)
   - Vessel motions (6 DOF)
   - Structural utilization

2. Generate reports:
   - Interactive HTML visualizations
   - CSV data exports
   - Code compliance checks

---

## 8. Multi-Body & Advanced Patterns

### Two-Vessel Configurations (AQWA Integration)
```yaml
# Multi-vessel model structure
VesselTypes:
  - Name: FST1_Type
    Length: 280
    Draughts:
      - Name: Operating
        RAOOrigin: [140, 0, 0]
        DisplacementRAOs: [...]  # From AQWA

  - Name: FST2_Type
    Length: 180
    Draughts:
      - Name: Operating
        RAOOrigin: [90, 0, 0]
        DisplacementRAOs: [...]  # From AQWA

Vessels:
  - Name: FPSO
    VesselType: FST1_Type
    InitialPosition: [0, 0, 0]

  - Name: Shuttle_Tanker
    VesselType: FST2_Type
    InitialPosition: [100, 50, 0]
```

### AQWA RAO Import Pattern
```yaml
VesselTypes:
  - Name: FPSO_From_AQWA
    Draughts:
      - Name: Ballast
        RAOOrigin: [140, 0, 8.5]
        DisplacementRAOs:
          - RAODirection: 0
            RAOPeriodOrFrequency, RAOSurgeAmplitude, RAOSurgePhase, ...:
              - [3.0, 0.01, 0, 0.02, 0, ...]
              - [5.0, 0.05, 10, 0.08, 5, ...]
              # ... more periods
        LoadRAOs:
          - RAODirection: 0
            RAOPeriodOrFrequency, RAOForceX, RAOForceXPhase, ...:
              - [3.0, 100, 0, 50, 0, ...]
```

### Installation Sequence Stages
```yaml
General:
  # Stage 0: Build-up (ramp environmental loads)
  # Stage 1: Dynamic lowering simulation
  # Stage 2: Touchdown and stabilization
  StageDuration: [30, 300, 100]

Environment:
  # Use stage-specific loading
  WaveRampTime: 30  # Ramp over stage 0
```

---

## 9. Best Practices & Common Pitfalls

### ✅ DO (Best Practices)
1. **Use Include-Based Format B** for new models (master.yml + includes/)
2. **Use flat YAML (Format A)** only for simple, single-use models
3. **Store lookups in VariableData** - avoid hardcoding coefficients
4. **Two-stage analysis**: Static setup (30s) + Dynamic (1000s+)
5. **Use descriptive naming** following conventions above
6. **Reference CSV catalogs** for standard components
7. **Validate static first** before running dynamics
8. **Use relative paths** in include files for portability

### ❌ DON'T (Common Pitfalls)
1. **Hybrid YAML** - Mixing Format A and B is NOT supported
2. **Custom variables** outside standard sections - Not recognized
3. **Hardcoded coefficients** - Use VariableData lookups instead
4. **Skip static validation** - Always verify equilibrium first
5. **Long dynamic without ramp** - Use stage 0 for load buildup
6. **Absolute paths** in includes - Breaks portability

### Error Recovery Patterns
| Issue | Solution |
|-------|----------|
| Static divergence | Increase damping (10→50→100) |
| Line catenary failure | Check end positions, reduce pretension |
| Vessel drift | Verify mooring connections, check environment |
| Buoy instability | Add drag, reduce timestep |
| Memory overflow | Split into smaller batch runs |

---

## 10. Template-Based Approach (Recommended)

### Available Template: SCR Catenary (Fully Implemented)
Location: `docs/modules/orcaflex/templates/risers/scr_catenary/`

Files:
- `config_template.yml` - User configuration
- `model_template.yml` - Model structure
- `example_basic.yml` - Simple example
- `example_advanced.yml` - Full-featured example
- `README.md` - Usage guide

### Workflow for Template-Based Generation:

1. **Copy template configuration**:
   ```bash
   cp templates/risers/scr_catenary/config_template.yml my_config.yml
   ```

2. **Edit configuration with project specifics**:
   - Vessel ID → lookup from vessel library
   - Riser specifications → lookup from line catalog
   - Environment → lookup from metocean database

3. **Generate model**:
   ```
   /orcaflex-model-generator --config my_config.yml --output my_model.yml
   ```

4. **Validate and refine**:
   - Run static analysis
   - Debug any convergence issues
   - Optimize pretensions

---

## 11. Quick Reference: Model Creation Commands

### Skill Invocation Pattern
```
/orcaflex-model-generator    # Generate from templates
/orcaflex-vessel-setup       # Configure vessels
/orcaflex-environment-config # Set environment
/orcaflex-line-wizard        # Calculate line properties
/orcaflex-file-conversion    # Convert formats
/orcaflex-modeling           # Run simulations
/orcaflex-static-debug       # Troubleshoot convergence
/orcaflex-rao-import         # Import RAOs from AQWA/OrcaWave
/orcaflex-modal-analysis     # Extract natural frequencies
/orcaflex-code-check         # Validate against standards
```

---

## 12. Key Files for Reference

### Documentation
- `docs/modules/orcaflex/QUICKSTART_MODEL_GENERATOR.md`
- `docs/modules/orcaflex/universal_runner_guide.md`
- `docs/modules/orcaflex/HUMAN_FRIENDLY_YAML_FORMAT.md`
- `docs/modules/orcaflex/notes/pre_yml_file_management/yml_file_management.md`
- `docs/modules/orcaflex/support/base_file/basefile.md`

### Templates & Components
- `docs/modules/orcaflex/templates/components/` (component libraries)
- `docs/modules/orcaflex/templates/risers/scr_catenary/` (SCR template)
- `docs/modules/orcaflex/support/base_file/` (dated base templates)

### Examples (441 .yml files)
- `docs/modules/orcaflex/examples/yml/` (converted examples)
- `docs/modules/orcaflex/examples/gom_scr_design_study/` (design study)
- `docs/modules/orcaflex/mooring/buoy/` (mooring examples)
- `docs/modules/orcaflex/aqwa/to_orcaflex/` (AQWA integration)

### Python Tools
- `src/digitalmodel/agents/orcaflex/cli.py` (CLI interface)
- `src/digitalmodel/agents/orcaflex/generators/` (generation code)

---

## 13. Verification Steps

After model creation, verify by:

1. **Static convergence**: Model reaches equilibrium
2. **Line tensions**: Match expected pretensions (±5%)
3. **Vessel position**: At target coordinates (±1m)
4. **Environment**: Correct wave/current/wind conditions
5. **Code compliance**: Pass DNV/API checks using `/orcaflex-code-check`

---

## 14. Implementation Checklist

### To Create a New OrcaFlex Model from Scratch:

1. [ ] **Requirements** - Define model type and analysis scope
2. [ ] **Components** - Select vessel, lines, environment from libraries
3. [ ] **Configuration** - Create config.yml with project parameters
4. [ ] **Generation** - Use `/orcaflex-model-generator` to create base model
5. [ ] **Vessels** - Configure RAOs and draughts with `/orcaflex-vessel-setup`
6. [ ] **Environment** - Set metocean with `/orcaflex-environment-config`
7. [ ] **Lines** - Calculate properties with `/orcaflex-line-wizard`
8. [ ] **VariableData** - Add lookup tables for hydrodynamic coefficients
9. [ ] **Validation** - Run static analysis, debug with `/orcaflex-static-debug`
10. [ ] **Execution** - Run simulations with `/orcaflex-modeling`
11. [ ] **Results** - Post-process with `/orcaflex-post-processing`
12. [ ] **Compliance** - Verify against standards with `/orcaflex-code-check`

---

## 15. Repository Statistics

| Metric | Count |
|--------|-------|
| Total YAML files | 441 |
| Files with VesselTypes | 302 |
| Files with Lines | 324 |
| Template files | 4 (model, config, basic, advanced) |
| Example categories | 26 (A01-L05, Z02, Z09) |
| CSV lookup tables | 3 (risers, pipelines, umbilicals) |
| Max file size | ~42KB (mooring models) |
| Directory depth | 5 levels max |

---

## Summary

The OrcaFlex module provides a comprehensive toolkit of **18 skills** covering the complete model lifecycle from creation to compliance verification. Based on analysis of **441 YAML model files** (now converted to include-based format), the recommended approach is:

1. **Use Include-Based Format B** for new models (master.yml + includes/)
2. **Reference component libraries** (CSV catalogs) for vessels, lines, materials, equipment
3. **Follow naming conventions** for files and objects
4. **Store coefficients in VariableData** lookup tables
5. **Use two-stage analysis** (static setup + dynamic)
6. **Leverage skills** for specific tasks (setup, debug, analyze, validate)
7. **Review existing templates** at `templates/mooring_systems/` and `templates/risers/`
8. **Use conversion script** `scripts/conversion/yaml_to_include.py` for legacy flat files

**New Resources:**
- **115 converted example models**: `examples/converted/` (include-based format)
- **CALM Buoy Template**: `templates/mooring_systems/calm_buoy/`
- **Equipment CSVs**: `templates/components/equipment/` (buoys, connectors, fairleads)

The system enables engineers to generate valid OrcaFlex models efficiently using proven engineering standards and patterns extracted from 441 production-quality model files.
