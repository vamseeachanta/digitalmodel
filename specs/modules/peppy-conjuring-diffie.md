# Implementation Plan: OrcaFlex Include-Based Model System

## Status: COMPLETED (January 2026)

## Objective (Revised)
1. ~~Convert all 114 existing flat YAML files~~ → Keep flat files as reference
2. Create hybrid templates using object-level IncludeFile for equipment reuse
3. Establish library of reusable components (line types, buoy types)

## Scope (Revised)
- **Phase 1**: ~~Convert 114 YAML files~~ → DEPRIORITIZED (flat files work fine)
- **Phase 2**: Create CALM buoy + SCR riser hybrid templates using library components

## Deliverables (Completed)

**Library Components (33 files)**:
- 21 line types (risers, pipelines, umbilicals, chains, hawsers)
- 12 buoy types (CALM, SPM, metocean, spar, navigation, pickup)
- Library generator with 21 unit tests

**CALM Buoy Hybrid Template (5 files)**:
- Base model (100m water depth, 6-leg mooring)
- Deep water variation (200m)
- Case file combining base + variation
- README with key findings

**SCR Hybrid Template (6 files)**:
- Base model (1200m water depth, 10-inch X65)
- Deep water variation (1500m)
- 12-inch riser variation
- Case files for each variation
- README with documentation

---

## Phase 1: Convert to Include-Based Format

### Target Files (114 total)

| Category | Count | Location |
|----------|-------|----------|
| Mooring/Buoy | 49 | `docs/modules/orcaflex/mooring/buoy/` |
| Examples (A01-M01) | 65 | `docs/modules/orcaflex/examples/yml/` |

### Include-Based Structure (per model)

Each flat YAML file will be split into:
```
model_name/
├── master.yml              # Main file with includes + input parameters
├── includes/
│   ├── 01_general.yml      # General settings, units, stages
│   ├── 02_var_data.yml     # VariableData lookup tables
│   ├── 03_environment.yml  # Wave, current, wind, seabed
│   ├── 04_vessel_types.yml # VesselTypes definitions
│   ├── 05_line_types.yml   # LineTypes definitions
│   ├── 06_vessels.yml      # Vessel instances
│   ├── 07_lines.yml        # Line instances
│   └── 08_buoys.yml        # Buoy instances (if applicable)
└── inputs/
    └── parameters.yml      # Input parameter definitions for parametric runs
```

### Master File Format

```yaml
# Master file with include references and input parameter blocks
%YAML 1.1
# Type: Model
# Program: OrcaFlex 11.4
---
# Input Parameters (for parametric analysis)
_inputs:
  water_depth: 1200        # m
  hs: 3.5                  # m, significant wave height
  tp: 12.0                 # s, peak period
  current_speed: 1.2       # m/s
  vessel_draft: ballast    # ballast | laden | intermediate

# Include files
- includefile: includes/01_general.yml
- includefile: includes/02_var_data.yml
- includefile: includes/03_environment.yml
- includefile: includes/04_vessel_types.yml
- includefile: includes/05_line_types.yml
- includefile: includes/06_vessels.yml
- includefile: includes/07_lines.yml
```

### Conversion Tasks

#### Task 1.1: Create Include Structure Template
Create the standard include file templates that will be used for all conversions.

#### Task 1.2: Convert CALM Buoy Examples (Priority)
| File | Source |
|------|--------|
| C05 Single Point Mooring | `examples/yml/C05/` |
| C06 CALM Buoy | `examples/yml/C06/` |
| C06 Discretised CALM Buoy | `examples/yml/C06/` |
| spm-proj-1 BaseModel | `mooring/buoy/spm-proj-1/` |
| spm-proj-2 BaseModel | `mooring/buoy/spm-proj-2/` |

#### Task 1.3: Convert Riser Examples (A01-A06)
| File | Source |
|------|--------|
| A01 Catenary riser | `examples/yml/A01/` |
| A01 Lazy wave riser | `examples/yml/A01/` |
| A01 Pliant wave riser | `examples/yml/A01/` |
| A01 Steep wave riser | `examples/yml/A01/` |
| A02 Lazy S (detailed) | `examples/yml/A02/` |

#### Task 1.4: Convert Remaining Examples
Batch convert remaining 104 files using automated script.

---

## Phase 2: CALM Buoy Template

### Task 2.1: Create Equipment CSV Files
**Location:** `docs/modules/orcaflex/templates/components/equipment/`

#### 2.1.1 buoys.csv
```csv
BuoyID,Type,Diameter_m,Height_m,Mass_kg,Buoyancy_kN,Cd,Cm,Description
CALM_12m,CALM,12.0,4.5,85000,2500,0.8,1.2,Standard CALM buoy for tanker operations
CALM_15m,CALM,15.0,5.5,120000,4200,0.8,1.2,Large CALM buoy for VLCC operations
SPM_10m,SPM,10.0,3.5,55000,1800,0.75,1.15,Single point mooring buoy
METOCEAN_3m,Metocean,3.0,2.0,2500,120,0.6,1.0,Environmental monitoring buoy
SPAR_6m,Spar,6.0,15.0,45000,950,0.7,1.1,Spar-type surface buoy
```

#### 2.1.2 connectors.csv
```csv
ConnectorID,Type,SWL_kN,MBL_kN,Mass_kg,Length_m,Description
HAWSER_CONN_150T,Hawser,1500,3000,450,2.5,Tanker hawser connector
CHAIN_STOPPER_200T,ChainStopper,2000,4000,850,1.8,Chain stopper for mooring
QUICK_RELEASE_100T,QuickRelease,1000,2000,320,1.2,Emergency disconnect
SWIVEL_150T,Swivel,1500,3000,550,1.5,Swivel for CALM buoy
SHACKLE_200T,Shackle,2000,4000,180,0.5,Anchor shackle
```

#### 2.1.3 fairleads.csv
```csv
FairleadID,Type,Angle_deg,Radius_m,Capacity_kN,Mass_kg,Description
VESSEL_BOW,Bow,45,15.0,3000,2500,Bow fairlead for FPSO
VESSEL_STERN,Stern,135,12.0,2500,2200,Stern fairlead
BUOY_CHAIN,BuoyChain,90,6.0,4000,1800,CALM buoy chain fairlead
TURRET_INTERNAL,Turret,0,8.0,5000,3500,Internal turret fairlead
GUIDE_ROLLER,Roller,0,0.5,1500,450,Guide roller fairlead
```

### Task 2.2: Create CALM Buoy Mooring Template (Include-Based)
**Location:** `docs/modules/orcaflex/templates/mooring_systems/calm_buoy/`

#### 2.2.1 README.md
Usage guide explaining:
- CALM buoy configuration options
- Required inputs (vessel RAOs, metocean data)
- Example usage

#### 2.2.2 config_template.yml
```yaml
# CALM Buoy Mooring System Configuration
model_name: "CALM_Buoy_Mooring"
description: "Catenary Anchor Leg Mooring buoy system"

# Buoy Configuration
buoy:
  type: CALM_12m              # From buoys.csv
  draft: 2.5                  # m
  initial_position: [0, 0, 0] # [x, y, z] in m

# Mooring Legs (typically 6-8)
mooring:
  num_legs: 6
  leg_type: catenary
  chain:
    type: Studless_Chain_R4_84mm  # From lines catalog
    length: 800                    # m per leg
  anchor:
    radius: 750                    # m from buoy center
    depth: 100                     # m water depth

# Hawser Connection
hawser:
  type: HMPE_Rope_80mm
  length: 80                  # m
  pretension: 50              # kN

# Environment
environment:
  region: gulf_of_mexico      # Lookup from metocean.csv
  condition: 100yr_storm
  water_depth: 100

# Analysis Settings
analysis:
  static_duration: 30         # s
  dynamic_duration: 3600      # s (1 hour)
  timestep: 0.1               # s
```

#### 2.2.3 master.yml + includes/
Complete OrcaFlex model structure with:
- General settings
- Environment configuration
- BuoyTypes and Buoys (CALM buoy)
- LineTypes (chain, hawser)
- Lines (6-8 mooring legs + hawser)
- Shapes (anchor markers)

#### 2.2.4 example_basic/
Working example with 6-leg CALM buoy in 100m water depth

#### 2.2.5 example_advanced/
Full example with:
- Multiple environmental headings
- Tanker approach scenarios
- Hawser connection variants

---

## Files to Create/Modify

### Phase 1: Conversion Output (~800 files)

**Per model conversion creates:**
```
model_name/
├── master.yml           # 1 file
├── includes/           # 5-8 files
│   ├── 01_general.yml
│   ├── 02_var_data.yml
│   ├── 03_environment.yml
│   ├── 04_vessel_types.yml
│   ├── 05_line_types.yml
│   ├── 06_vessels.yml
│   ├── 07_lines.yml
│   └── 08_buoys.yml
└── inputs/
    └── parameters.yml   # 1 file
```

**Estimated total:** 114 models × ~7 files = ~800 new files

### Phase 2: New Template Files (15+ files)

| File | Location | Purpose |
|------|----------|---------|
| `buoys.csv` | `templates/components/equipment/` | Buoy specifications |
| `connectors.csv` | `templates/components/equipment/` | Connector catalog |
| `fairleads.csv` | `templates/components/equipment/` | Fairlead catalog |
| `README.md` | `templates/mooring_systems/calm_buoy/` | Usage guide |
| `config_template.yml` | `templates/mooring_systems/calm_buoy/` | Configuration |
| `master.yml` | `templates/mooring_systems/calm_buoy/` | Master file |
| `includes/*.yml` | `templates/mooring_systems/calm_buoy/includes/` | 7 include files |
| `example_basic/` | `templates/mooring_systems/calm_buoy/` | Basic example (include-based) |
| `example_advanced/` | `templates/mooring_systems/calm_buoy/` | Advanced example (include-based) |

### Plan File to Update
| File | Change |
|------|--------|
| `specs/modules/orcaflex/model-preparation/plan.md` | Change recommended format from A (Flat) to B (Include-Based) |

### Reference Files (read-only)
| File | Purpose |
|------|---------|
| `docs/modules/orcaflex/mooring/buoy/*.yml` | Existing CALM buoy examples (to be converted) |
| `templates/risers/scr_catenary/*` | Template structure reference |
| `templates/components/lines/risers.csv` | CSV format reference |

---

## Verification Steps

### Phase 1 Verification
1. **Include Syntax**: All include files load correctly via master.yml
2. **YAML Validation**: Run `/orcaflex-file-conversion` on each converted model
3. **Equivalence Check**: Converted include-based model produces same results as original flat file
4. **Parameter Extraction**: Input parameters correctly identified in parameters.yml

### Phase 2 Verification
1. **CSV Validation**: Ensure all CSVs have consistent column headers and realistic values
2. **Template Validation**: Run `/orcaflex-file-conversion` on CALM template
3. **Static Analysis**: Use `/orcaflex-static-debug` to verify model converges
4. **Cross-Reference**: Compare generated model against original CALM examples

---

## Success Criteria

### Phase 1 - DEPRIORITIZED
**Decision**: Keep flat YAML files as reference examples. Hybrid approach only for new models.

- [x] Validated that section-level includes do NOT work in OrcaFlex
- [x] Documented object-level IncludeFile approach
- [ ] ~~All 114 flat YAML files converted~~ (Deprioritized - flat files kept)

### Phase 2 - COMPLETED
- [x] Equipment CSV files exist with 5+ entries each
- [x] CALM buoy hybrid template created (base + variations)
- [x] Template loads successfully in OrcaFlex
- [x] Static convergence validated
- [x] Template documented in README.md
- [x] Model-preparation plan updated

### Additional Accomplishments
- [x] Library generator fixed and tested (21 unit tests)
- [x] 33 library components generated (21 line types, 12 buoy types)
- [x] SCR hybrid template created with variations
- [x] Key findings documented (IncludeFile at object level only)

---

## Estimated Scope

### Phase 1: Conversion (Large)
| Item | Count | Notes |
|------|-------|-------|
| Models to convert | 114 | 49 mooring/buoy + 65 examples |
| Files per model | ~7 | master.yml + 5-7 includes + parameters.yml |
| **Total Phase 1** | ~800 files | Automated via Python script |

### Phase 2: CALM Template (Small)
| Item | Effort |
|------|--------|
| Equipment CSVs (3 files) | ~30 rows of data |
| CALM template (5 files) | ~500 lines YAML |
| Documentation | ~100 lines markdown |
| **Total Phase 2** | ~8 files, ~700 lines |

### Combined Total
| Phase | Files | Complexity |
|-------|-------|------------|
| Phase 1 | ~800 | Automated conversion |
| Phase 2 | ~8 | Manual creation |
| **Total** | ~808 files | Mostly automated |

---

## Alternative Approaches Considered

1. **Start with simpler template** (e.g., single mooring line) - Rejected: CALM is more useful
2. **Enhance Python generators first** - Deferred: Templates provide immediate value
3. **Create all mooring templates at once** - Rejected: Better to validate one first

---

## Cross-Review Workflow

Per the **Cross-Review Policy** (`.claude/skills/workflows/cross-review-policy/`), all Claude work must be reviewed by OpenAI Codex before presenting to user.

### Review Determination

| File Type | Category | Review Required? |
|-----------|----------|-----------------|
| `*.csv` | Configuration/Data | ⚠️ Edge case - data files |
| `*.yml` (templates) | Configuration | ⚠️ Edge case - config files |
| `README.md` | Documentation | ❌ Exempt |

**Policy Exceptions (from CROSS_REVIEW_POLICY.md):**
- Documentation-only changes (pure markdown/text)
- Configuration files (non-code config)

### Required Workflow (User Selected: Full Review)

1. **Create files** - Implement CSV and YAML templates
2. **Commit with AI signature** - Include `Co-Authored-By: Claude`
3. **Codex review** - Mandatory review via cross-review loop (max 3 iterations)
4. **Implement feedback** - Address Codex suggestions
5. **Validate with OrcaFlex** - Run `/orcaflex-file-conversion` to verify YAML syntax
6. **Present to user** - Only after Codex approval or 3 iterations complete

### Review Commands

Scripts located at: `D:/workspace-hub/scripts/ai-review/`

```bash
# After commit, trigger review loop
D:/workspace-hub/scripts/ai-review/cross-review-loop.sh --max-iterations 3

# Check review status
D:/workspace-hub/scripts/ai-review/review-manager.sh list

# View specific review
D:/workspace-hub/scripts/ai-review/review-manager.sh show <review_id>

# After implementing feedback, re-submit
D:/workspace-hub/scripts/ai-review/review-manager.sh implement <review_id>
```

---

## Next Phase Preview

After CALM buoy template is complete, the logical next steps are:
1. Create spread mooring template (uses same equipment CSVs)
2. Add Python CSV loader to generators
3. Create turret mooring template
4. Add template validation layer
