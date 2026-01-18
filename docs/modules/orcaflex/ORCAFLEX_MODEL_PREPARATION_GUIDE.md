# OrcaFlex Model Preparation Guide: From YAML Scratch

## Executive Summary

This guide outlines how to prepare OrcaFlex models in .yml format from scratch using the 18 available OrcaFlex skills and the established component library system.

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

### Standard YAML Hierarchy
```yaml
%YAML 1.1
# Type: Model
# Program: OrcaFlex 11.4
---
General:              # Simulation settings (statics/dynamics)
VariableData:         # Lookup tables (drag, stiffness curves)
Environment:          # Wave, current, wind, seabed
VesselTypes:          # Vessel templates (with RAOs/draughts)
LineTypes:            # Line templates (OD, ID, EA, EI, mass)
Vessels:              # Vessel instances
6DBuoys:              # Buoy instances
Lines:                # Line instances (moorings, risers)
Winches:              # Winch controls (tension, length)
Shapes:               # Visual/anchor geometry
Constraints:          # Contact definitions
Groups:               # Object grouping
```

### Model Types in Repository (440 files)
- **Riser Systems**: 77 files (SCR, SLWR, drilling, completion)
- **Mooring Systems**: 72 files (CALM, SALM, spread, turret)
- **Installation Operations**: 22 files (S-lay, pull-in, davit)
- **Specialized Structures**: 9 files (mudmat, salvage)
- **Example Models**: 183 files (A01-L03 series)

---

## 3. Model Preparation Workflow (From Scratch)

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
           DisplacementRAOs: [imported RAO data]
   ```

3. Set initial position and orientation

### Phase 4: Environment Configuration
**Skill**: `orcaflex-environment-config`

1. Configure wave conditions:
   ```yaml
   Environment:
     WaveTrains:
       - Name: Swell
         WaveType: JONSWAP
         Hs: 3.5
         Tp: 12
         GammaJONSWAP: 2.4
   ```

2. Define current profiles (depth-varying)
3. Set wind loading (if required)
4. Configure seabed properties (friction, stiffness)

### Phase 5: Line Configuration
**Skill**: `orcaflex-line-wizard`

1. Define line types:
   ```yaml
   LineTypes:
     - Name: Studless_Chain_R4_84mm
       OD: 0.252
       MassPerUnitLength: 139.8
       EA: 850e6
       Category: Chain
   ```

2. Create line instances with connections:
   ```yaml
   Lines:
     - Name: Mooring_Leg_1
       LineType: Studless_Chain_R4_84mm
       TopEnd: Vessel
       Connection:
         - [FPSO_Generic, 10, 20, -5]  # Start
         - [Anchored, 500, 400, -150]  # End
   ```

3. Use Line Setup Wizard for auto-calculation of:
   - Line lengths to achieve target tensions
   - Tensions for given line lengths

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

1. Configure analysis stages:
   - Stage 0: Build up (ramp loads)
   - Stage 1: Dynamic simulation

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

## 4. Template-Based Approach (Recommended)

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

## 5. Quick Reference: Skill Invocation

### Skill Commands
```
/orcaflex-model-generator    # Generate from templates
/orcaflex-vessel-setup       # Configure vessels
/orcaflex-environment-config # Set environment
/orcaflex-line-wizard        # Calculate line properties
/orcaflex-file-conversion    # Convert formats
/orcaflex-modeling           # Run simulations
/orcaflex-static-debug       # Troubleshoot convergence
/orcaflex-mooring-iteration  # Optimize pretensions
/orcaflex-modal-analysis     # Frequency analysis
/orcaflex-post-processing    # Extract results
/orcaflex-extreme-analysis   # Get extreme values
/orcaflex-operability        # Weather downtime analysis
/orcaflex-visualization      # Generate reports
/orcaflex-results-comparison # Compare simulations
/orcaflex-code-check         # DNV/API/ISO compliance
/orcaflex-batch-manager      # Parallel batch runs
```

---

## 6. Key Files for Reference

### Documentation
- `docs/modules/orcaflex/QUICKSTART_MODEL_GENERATOR.md`
- `docs/modules/orcaflex/universal_runner_guide.md`
- `docs/modules/orcaflex/HUMAN_FRIENDLY_YAML_FORMAT.md`

### Templates
- `docs/modules/orcaflex/templates/components/` (component libraries)
- `docs/modules/orcaflex/templates/risers/scr_catenary/` (SCR template)

### Examples (440 .yml files)
- `docs/modules/orcaflex/examples/raw/` (original examples)
- `docs/modules/orcaflex/examples/gom_scr_design_study/` (design study)
- `docs/modules/orcaflex/mooring/buoy/` (mooring examples)

### Python Tools
- `src/digitalmodel/agents/orcaflex/cli.py` (CLI interface)
- `src/digitalmodel/agents/orcaflex/generators/` (generation code)

---

## 7. Verification Checklist

After model creation, verify:

- [ ] **Static convergence**: Model reaches equilibrium
- [ ] **Line tensions**: Match expected pretensions
- [ ] **Vessel position**: At target coordinates
- [ ] **Environment**: Correct wave/current/wind conditions
- [ ] **Code compliance**: Pass DNV/API checks using `/orcaflex-code-check`

---

## 8. Implementation Workflow Summary

### To Create a New OrcaFlex Model from Scratch:

1. [ ] **Requirements** - Define model type and analysis scope
2. [ ] **Components** - Select vessel, lines, environment from libraries
3. [ ] **Configuration** - Create config.yml with project parameters
4. [ ] **Generation** - Use `/orcaflex-model-generator` to create base model
5. [ ] **Vessels** - Configure RAOs and draughts with `/orcaflex-vessel-setup`
6. [ ] **Environment** - Set metocean with `/orcaflex-environment-config`
7. [ ] **Lines** - Calculate properties with `/orcaflex-line-wizard`
8. [ ] **Validation** - Run static analysis, debug with `/orcaflex-static-debug`
9. [ ] **Execution** - Run simulations with `/orcaflex-modeling`
10. [ ] **Results** - Post-process with `/orcaflex-post-processing`
11. [ ] **Compliance** - Verify against standards with `/orcaflex-code-check`

---

## Summary

The OrcaFlex module provides a comprehensive toolkit of **18 skills** covering the complete model lifecycle from creation to compliance verification. The recommended approach is:

1. **Use templates** from `docs/modules/orcaflex/templates/` as starting points
2. **Reference component libraries** for vessels, lines, materials, environments
3. **Follow the standard YAML hierarchy** for model structure
4. **Leverage skills** for specific tasks (setup, debug, analyze, validate)
5. **Review existing examples** (440 .yml files) for patterns and best practices

The system enables engineers to generate valid OrcaFlex models efficiently using proven engineering standards.
