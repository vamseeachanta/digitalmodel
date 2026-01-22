# Plan: OrcaFlex Modular Model Generator from Project Spec

## Overview

Create a system that generates complete OrcaFlex modular input models from a high-level project-specific YAML input file. The input captures engineering intent, and the generator produces the full modular structure.

## Goal

**Input**: Single human-readable YAML spec file defining model parameters
**Output**: Complete modular OrcaFlex structure (master.yml + includes/ + inputs/)

---

## Output Folders (Explicit)

### This Work (30in Pipeline Floating Installation)

| Item | Path |
|------|------|
| **Input spec** | `docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/spec.yml` |
| **Generated output** | `docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/generated/` |
| **Reference (existing)** | `docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/modular/` |

### Generator Code

| Item | Path |
|------|------|
| **Module** | `src/digitalmodel/modules/orcaflex/modular_generator/` |
| **Skill** | `.claude/skills/orcaflex-modular-generator/` |

---

## Model Taxonomy

This is **1 model** in a taxonomy of Structure × Operation types:

### Structure Types

| Structure | Description | Examples |
|-----------|-------------|----------|
| **Pipeline** | Subsea pipelines | 30", 24", 16" flowlines |
| **Riser** | Vertical/catenary risers | SCR, TTR, lazy wave |
| **Mooring** | Mooring systems | CALM buoy, spread mooring |
| **Vessel** | Floating vessels | FPSO, drillship, installation vessel |
| **Umbilical** | Power/control cables | Dynamic umbilicals |
| **Subsea** | Subsea structures | Manifolds, PLEMs, templates |

### Operation Types

| Operation | Description | Structures |
|-----------|-------------|------------|
| **Installation** | Installation analysis | Pipeline, riser, subsea |
| ├─ floating | Floating installation | Pipeline |
| ├─ s-lay | S-lay installation | Pipeline |
| ├─ j-lay | J-lay installation | Pipeline, riser |
| └─ lowering | Subsea lowering | Subsea |
| **In-place** | Operating condition | All |
| **Spanning** | Free span analysis | Pipeline |
| **Fatigue** | Fatigue assessment | Riser, mooring |
| **Extreme** | Extreme response | All |

### Current Model Position

```
Structure: Pipeline
Operation: Installation → Floating
Example:   30in_pipeline (5 tugs, buoyancy modules)
```

---

## Future Structures Plan

### Priority 1: Pipeline Operations (Near-term)

| Model | Structure | Operation | Status |
|-------|-----------|-----------|--------|
| 30in_pipeline | Pipeline | Installation/Floating | **This plan** |
| s-lay_pipeline | Pipeline | Installation/S-lay | Future |
| spanning_pipeline | Pipeline | Spanning | Future |

### Priority 2: Mooring Systems (Next)

| Model | Structure | Operation | Notes |
|-------|-----------|-----------|-------|
| calm_buoy | Mooring | In-place | CALM buoy system |
| spread_mooring | Mooring | In-place | Multi-line spread |
| salm_buoy | Mooring | In-place | SALM configuration |

### Priority 3: Risers (Future)

| Model | Structure | Operation | Notes |
|-------|-----------|-----------|-------|
| scr_catenary | Riser | In-place | Steel catenary riser |
| lazy_wave | Riser | In-place | Lazy wave config |
| ttr | Riser | In-place | Top-tensioned riser |

### Priority 4: Complex Operations (Future)

| Model | Structure | Operation | Notes |
|-------|-----------|-----------|-------|
| fpso_mooring | Vessel+Mooring | In-place | Full FPSO system |
| installation_vessel | Vessel | Installation | Pipelay vessel |

---

## First Example: 30in Pipeline Floating Installation

### Input Spec

```yaml
# docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/spec.yml
metadata:
  name: "30in_pipeline_installation"
  description: "30-inch pipeline floating installation with 5 tugs"
  structure: pipeline
  operation: installation/floating
  project: "SESA"

environment:
  water:
    depth: 8                      # m
    density: 1.03                 # te/m3
  seabed:
    slope: 0.57                   # deg
    stiffness: {normal: 1000, shear: 100}
  waves:
    type: dean_stream
    height: 0                     # Hs (m)
    period: 8                     # Tp (s)
    direction: 180
  current:
    speed: 1                      # m/s at surface
    direction: 270
    profile: [[0, 0.46], [6, 0.39], [15, 0.33]]
  wind:
    speed: 8.87
    direction: 270

pipeline:
  name: "30'' Line"
  material: X65
  dimensions:
    outer_diameter: 0.762         # 30"
    wall_thickness: 0.0254
  coatings:
    corrosion: {thickness: 0.0042, density: 0.95}
    weight:
      - {name: CWC120, thickness: 0.12, density: 2.978}
      - {name: CWC90, thickness: 0.09, density: 2.978}
  segments:
    - {type: X65+coating+CWC120, length: 1894.4, segment_length: 3.9}
    - {type: X65+coating+CWC90, length: 3005.6, segment_length: 5.4}

equipment:
  tugs:
    count: 5
    spacing: 816.6
    first_position: [716.7, -20, 0]
    properties: {mass: 30, volume: 100}
  rollers:
    position: [5, 0, -2]
    supports: 4
  buoyancy_modules:
    spacing: 3.9
    properties: {mass: 0.05, volume: 4.91}
  ramps:
    - {name: "Ramp inclined", type: block, origin: [-126.35, -25, -5]}
    - {name: "Ramp-curve", type: curved_plate}

simulation:
  time_step: 0.1
  stages: [8, 16]
  north_direction: 70
```

### Generated Output

```
docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/generated/
├── master.yml                    # Entry point with includes
├── includes/
│   ├── 01_general.yml           # Simulation settings
│   ├── 02_var_data.yml          # Coating definitions
│   ├── 03_environment.yml       # Water, waves, current, wind
│   ├── 05_line_types.yml        # X65+coating+CWC* definitions
│   ├── 07_lines.yml             # Pipeline segments
│   ├── 08_buoys.yml             # Tugs, rollers, BM, 6D buoys
│   ├── 09_shapes.yml            # Ramps
│   ├── 10_groups.yml            # Model organization
│   ├── 13_supports.yml          # Support types
│   └── 14_morison.yml           # Morison elements
└── inputs/
    └── parameters.yml           # Extracted key parameters
```

---

## Implementation

### File Structure

```
src/digitalmodel/modules/orcaflex/modular_generator/
├── __init__.py                   # ModularModelGenerator class
├── schema.py                     # Pydantic input validation
├── builders/
│   ├── __init__.py
│   ├── base.py                   # BaseBuilder ABC
│   ├── general_builder.py
│   ├── environment_builder.py
│   ├── vardata_builder.py
│   ├── linetype_builder.py
│   ├── lines_builder.py
│   ├── buoys_builder.py
│   ├── shapes_builder.py
│   ├── groups_builder.py
│   ├── supports_builder.py
│   └── morison_builder.py
├── templates/
│   └── defaults.yml              # Default OrcaFlex values
└── cli.py                        # Command-line interface
```

### Core Classes

**ModularModelGenerator** (`__init__.py`)
- `generate(input_file, output_dir)` → creates full modular structure
- Orchestrates builders in dependency order
- Generates master.yml and parameters.yml

**ProjectInputSpec** (`schema.py`)
- Pydantic model for input validation
- Type checking, range validation
- Engineering constraints (wall_thickness < OD/2)

**BaseBuilder** (`builders/base.py`)
- Abstract base for section builders
- `build()` → returns section dict
- `get_generated_entities()` → for cross-builder references

---

## Tasks

### Phase 0: TDD Foundation (MANDATORY per CLAUDE.md)
- [ ] Create `tests/modules/orcaflex/modular_generator/` structure
- [ ] Add `conftest.py` with shared fixtures loading reference files
- [ ] Define OrcaFlex API mock strategy (pytest-mock for Model class)
- [ ] Write failing tests for schema validation (field_validators)
- [ ] Write failing tests for each builder output structure
- [ ] Add pytest parametrization for edge cases (missing optional fields, empty segments)
- [ ] Add integration test task for full generation pipeline

### Phase 1: Core Framework
- [ ] Create `modular_generator/` module structure
- [ ] Implement `schema.py` with Pydantic models:
  - Add engineering constraints via `@field_validator`
  - Define optional fields with `Field(default=...)` for non-required parameters
  - Validate that omitted optional fields produce valid OrcaFlex output
- [ ] Implement `BaseBuilder` abstract class
- [ ] Implement `ModularModelGenerator` orchestrator:
  - Implement explicit `INCLUDE_ORDER` constant (not sorted())
  - Implement context dict passing mechanism for entity sharing
- [ ] Create `templates/defaults.yml` with OrcaFlex default values

### Phase 2: Builders (Pipeline-specific)
- [ ] `general_builder.py` - simulation settings
- [ ] `environment_builder.py` - water, waves, current, wind (map to OrcaFlex enums)
- [ ] `vardata_builder.py` - coating/lining definitions
- [ ] `linetype_builder.py` - pipe type properties (handle variable segment type naming)
- [ ] `supports_builder.py` - support contact types
- [ ] `morison_builder.py` - hydrodynamic elements

### Phase 3: Complex Builders
- [ ] `buoys_builder.py` - tugs, rollers, BM (auto-positioning):
  - Generate 6DBuoys list (tugs, rollers with all 50+ properties)
  - Generate 3DBuoys list (inline buoyancy modules)
  - Handle variable roller count and support positions
  - Generate BM for each segment at specified spacing
- [ ] `lines_builder.py` - pipeline segments, connections:
  - Implement attachment generation with arc-length calculation
  - Handle End A/End B connection references
  - Generate segment array with proper mesh sizing
- [ ] `shapes_builder.py` - ramps, fixed geometry
- [ ] `groups_builder.py` - model hierarchy (consume all entity names from context)

### Phase 4: Integration & Validation
- [ ] CLI interface (`cli.py`)
- [ ] Create `spec.yml` from existing 30in_pipeline model
- [ ] Generate to `generated/` folder
- [ ] Implement semantic YAML comparison utility:
  - Compare dictionary structures (not raw text)
  - Use tolerance for floating-point values (rtol=1e-5)
  - Ignore non-functional differences (comments, key ordering)
- [ ] Compare with reference `modular/` folder using semantic comparison
- [ ] Validate with OrcaFlex API (`OrcFxAPI.Model()`)

### Phase 5: Future Structure Support (Separate Stories)
- [ ] Mooring schema extensions (CALM buoy components)
- [ ] Riser schema extensions (catenary parameters)
- [ ] Vessel schema extensions (RAO import)

---

## Critical Files

| File | Purpose |
|------|---------|
| `src/digitalmodel/modules/orcaflex/modular_generator/__init__.py` | Main generator class |
| `src/digitalmodel/modules/orcaflex/modular_generator/schema.py` | Input validation |
| `src/digitalmodel/modules/orcaflex/modular_generator/builders/lines_builder.py` | Most complex - segments/attachments |
| `docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/modular/` | Reference structure |
| `docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/spec.yml` | Input spec (to create) |
| `docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/generated/` | Output folder |

---

## Include Ordering (Critical)

OrcaFlex requires specific dependency order:

```
01_general.yml      → Simulation settings first
02_var_data.yml     → Material definitions
03_environment.yml  → Environment before objects
05_line_types.yml   → Types before instances
13_supports.yml     → Supports before buoys use them
14_morison.yml      → Morison elements
09_shapes.yml       → Shapes before lines touch them
08_buoys.yml        → Buoys before lines connect
07_lines.yml        → Lines reference types/buoys
10_groups.yml       → Groups reference all objects
```

---

## Verification

1. **Schema validation**: Load spec with Pydantic, check for errors
2. **Generation**: Run generator, verify all files created
3. **Comparison**: Diff `generated/` vs reference `modular/`
4. **OrcaFlex load**: `OrcFxAPI.Model('master.yml')` succeeds
5. **Static analysis**: Model converges in statics

```bash
# Generate
uv run python -m digitalmodel.modules.orcaflex.modular_generator generate \
    --input docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/spec.yml \
    --output docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/generated/

# Validate
uv run python -c "import OrcFxAPI; m = OrcFxAPI.Model('docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/generated/master.yml'); print('OK')"
```

---

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| Pydantic for schema | Strong typing, clear errors |
| Builder pattern | Testable, extensible, separation of concerns |
| YAML templates for defaults | Easy updates without code changes |
| Engineering-focused input | Captures intent, not OrcaFlex syntax |
| Structure × Operation taxonomy | Scalable to other model types |
| spec.yml alongside model | Keeps input with output for traceability |

---

## Plan Review Requirements

**Cross-Review (MANDATORY per workspace CLAUDE.md)**

This plan must be reviewed by external AI agents before implementation:

| Review | Agent | Iterations | Criteria |
|--------|-------|------------|----------|
| 1 | OpenAI Codex | 3 minimum | No comments OR all addressed |
| 2 | Google Gemini | 3 minimum | No comments OR all addressed |

### Review Checklist

- [ ] Schema design completeness
- [ ] Builder architecture soundness
- [ ] Dependency ordering correctness
- [ ] File organization consistency
- [ ] Validation approach adequacy
- [ ] Future extensibility for other structures

### Review Status

| Reviewer | Iteration | Status | Comments |
|----------|-----------|--------|----------|
| Codex | 1 | APPROVED WITH COMMENTS | See `review-codex-iteration-1.md` |
| Codex | 2 | APPROVED | See `review-codex-iteration-2.md` |
| Codex | 3 | **APPROVED** | See `review-codex-iteration-3.md` - FINAL |
| Gemini | 1 | APPROVED WITH COMMENTS | See `review-gemini-iteration-1.md` |
| Gemini | 2 | NEEDS REVISION | See `review-gemini-iteration-2.md` |
| Gemini | 3 | **APPROVED** | See `review-gemini-iteration-3.md` - FINAL |

**Cross-Review Complete**: Both reviewers approved after 3 iterations. Plan ready for implementation.

---

## Iteration 1 Review Findings (BLOCKING Issues)

### Issue 1: Include Ordering (BLOCKING)
**Problem**: Plan shows dependency order but implementation would use alphabetical sorting.
**Resolution**: Use explicit INCLUDE_ORDER list, not sorted():
```python
INCLUDE_ORDER = [
    '01_general.yml', '02_var_data.yml', '03_environment.yml',
    '05_line_types.yml', '13_supports.yml', '14_morison.yml',
    '09_shapes.yml', '08_buoys.yml', '07_lines.yml', '10_groups.yml',
]
```

### Issue 2: Buoy Output Structure (BLOCKING)
**Problem**: Would output `{'Buoys': [...]}` but OrcaFlex requires `{'6DBuoys': [...], '3DBuoys': [...]}`
**Resolution**: Use correct OrcaFlex structure with separate 6D and 3D buoy lists.

### Issue 3: No TDD Tasks (BLOCKING per CLAUDE.md)
**Problem**: TDD is mandatory but plan has no test tasks.
**Resolution**: Added Phase 0 (see updated Tasks section below).

### Issue 4: Cross-Builder Entity Sharing (HIGH)
**Problem**: Builders need to share entity names for cross-references.
**Resolution**: Implement context dict passed through all builders:
```python
context = {}
for builder in builders:
    data = builder.build(context)
    context.update(builder.get_generated_entities())
```

---

## Iteration 2 Review Status

**All iteration 1 BLOCKING issues incorporated into main Tasks section.**

Tasks section now includes:
- Phase 0: Complete TDD foundation with test structure and mock strategy
- Phase 1: Context mechanism and INCLUDE_ORDER implementation
- Phase 3: Detailed buoys_builder with 6DBuoys/3DBuoys separation
- Phase 3: Attachment arc-length calculation for lines_builder
- Phase 4: Semantic YAML comparison utility
