# Plan: OrcaFlex Model Library — Spec → Modular → Analysis Validation

> **Status**: IN PROGRESS — Building validated model library
> **Depends on**: `cryptic-roaming-gem.md` (modular vs monolithic test framework)

## Objective

Establish a comprehensive library of OrcaFlex input files with three validated states for each model:
1. **spec.yml** — Declarative specification (modular generator input)
2. **monolithic/** — Original/converted single-file model (baseline)
3. **modular/** — Generated modular output (validation target)

Each model progresses through: **Spec Creation → Generation → Static Analysis → Postprocess Validation**

---

## Library Structure

```
docs/modules/orcaflex/library/
├── tier1_instant/                    # Pre-computed .sim, <2s load
│   ├── c07_metocean_buoy/
│   │   ├── spec.yml                  # Declarative specification
│   │   ├── monolithic/
│   │   │   └── C07 Metocean Buoy.dat # Original model
│   │   ├── modular/
│   │   │   ├── master.yml            # Generated entry point
│   │   │   └── includes/             # Generated components
│   │   └── validation/
│   │       ├── statics.sim           # Pre-computed statics
│   │       └── results.json          # Extracted reference values
│   ├── c05_single_point_mooring/
│   ├── c06_calm_buoy/
│   └── c08_fish_farm/
│
├── tier2_fast/                       # Quick statics <30s
│   ├── a01_catenary_riser/
│   │   ├── spec.yml
│   │   ├── monolithic/
│   │   ├── modular/
│   │   └── validation/
│   ├── a01_lazy_wave_riser/
│   ├── d02_pull_in_analysis/
│   └── simple_fender_model/
│
├── tier3_medium/                     # Medium complexity 1-5min
│   ├── pipeline_spanning/
│   ├── l01_default_vessel/
│   └── 24in_pipeline_coarse/         # 10m mesh variant
│
└── tier4_production/                 # Full production models >10min
    ├── 24in_pipeline/
    └── 30in_pipeline/
```

---

## Model Catalog

### Tier 1: Instant Load (<2 seconds)

| ID | Model | Source | Lines | Est. Size | Priority |
|----|-------|--------|-------|-----------|----------|
| T1-01 | C07 Metocean Buoy | `examples/raw/C07/` | 3 mooring lines | 3.1M | HIGH |
| T1-02 | C05 Single Point Mooring | `examples/raw/C05/` | 1 riser | 10M | MEDIUM |
| T1-03 | C06 CALM Buoy | `examples/raw/C06/` | Multiple hawsers | 9-11M | MEDIUM |
| T1-04 | C08 Fish Farm | `examples/raw/C08/` | Net cage lines | 12M | LOW |

### Tier 2: Fast Statics (<30 seconds)

| ID | Model | Source | Elements | Est. Time | Priority |
|----|-------|--------|----------|-----------|----------|
| T2-01 | A01 Catenary Riser | `examples/raw/A01/` | ~200 | <10s | HIGH |
| T2-02 | A01 Lazy Wave Riser | `examples/raw/A01/` | ~300 | <15s | HIGH |
| T2-03 | Simple Fender Model | `examples/raw/C09/` | ~50 | <5s | MEDIUM |
| T2-04 | D02 Pull-in Analysis | `examples/raw/D02/` | ~150 | <10s | MEDIUM |
| T2-05 | A01 Pliant Wave Riser | `examples/raw/A01/` | ~250 | <12s | LOW |
| T2-06 | A01 Steep Wave Riser | `examples/raw/A01/` | ~200 | <10s | LOW |

### Tier 3: Medium Complexity (1-5 minutes)

| ID | Model | Source | Elements | Est. Time | Priority |
|----|-------|--------|----------|-----------|----------|
| T3-01 | Pipeline Spanning | `pipeline/spanning/` | ~500 | <60s | HIGH |
| T3-02 | L01 Default Vessel | `examples/raw/L01/` | ~400 | <45s | MEDIUM |
| T3-03 | 24in Pipeline Coarse | `24in_pipeline/spec_qa.yml` | ~490 | ~2min | HIGH |

### Tier 4: Production (>10 minutes)

| ID | Model | Source | Elements | Est. Time | Priority |
|----|-------|--------|----------|-----------|----------|
| T4-01 | 24in Floating Pipeline | `24in_pipeline/spec.yml` | ~9800 | ~15min | HIGH |
| T4-02 | 30in Floating Pipeline | `30in_pipeline/` | ~12000 | ~20min | HIGH |

---

## Pipeline for Each Model

### Phase 1: Spec Creation

Create `spec.yml` from the original model:

```yaml
# spec.yml - Declarative specification for model generation
metadata:
  name: "A01 Catenary Riser"
  source: "docs/modules/orcaflex/examples/raw/A01/A01 Catenary riser.dat"
  tier: 2
  expected_statics_time: "<10s"

environment:
  water_depth: 500  # m
  current_speed: 0.5  # m/s

vessel:
  name: "FPSO"
  draft: 15  # m

line:
  name: "Riser"
  type: catenary
  total_length: 800  # m
  segments:
    - type: flex_joint
      length: 2
    - type: riser_pipe
      length: 798
      segment_length: 4  # OrcaFlex mesh target (m)

connections:
  end_a:
    type: vessel
    vessel_name: "FPSO"
    position: [0, 0, -15]
  end_b:
    type: anchor
    position: [700, 0, -500]
```

### Phase 2: Generation

```bash
# Generate modular output from spec
uv run python -m digitalmodel.solvers.orcaflex.modular_generator \
  --spec spec.yml \
  --output modular/
```

Output structure:
```
modular/
├── master.yml          # Entry point with include directives
└── includes/
    ├── environment.yml
    ├── vessel.yml
    ├── line_types.yml
    ├── lines.yml
    └── connections.yml
```

### Phase 3: Static Analysis

```python
import OrcFxAPI

# Load and run statics on both models
monolithic = OrcFxAPI.Model("monolithic/model.dat")
monolithic.CalculateStatics()

modular = OrcFxAPI.Model()
modular.LoadData("modular/master.yml")
modular.CalculateStatics()
```

### Phase 4: Postprocess Validation

Extract and compare engineering results:

```python
@dataclass
class ValidationResults:
    model_name: str
    statics_converged: bool
    statics_time_seconds: float
    end_tensions: dict[str, float]      # {line_name: tension_kN}
    end_bending_moments: dict[str, float]
    max_tensions: dict[str, float]
    total_line_length: dict[str, float]
    object_count: int
```

Validation checks:
1. **Convergence**: Both models converge statics
2. **Tension match**: End tensions within 5% / 10 kN
3. **Bending match**: End bending within 15% / 5 kN.m
4. **Length match**: Total line length within 0.01%
5. **Object count**: Modular has expected components

---

## Implementation Phases

### Phase A: Foundation (Current)

**Status**: COMPLETE
- [x] Test framework in `test_modular_vs_monolithic.py`
- [x] Helper functions for result extraction
- [x] Tolerance-based comparison logic
- [x] TestQuickQA with spec_qa.yml (10m mesh)
- [x] TestInstantRiser with A01 Catenary Riser

### Phase B: Tier 2 Fast Models

**Status**: IN PROGRESS

Priority order (by statics convergence reliability):
1. **T2-01 A01 Catenary Riser** ✓ Implemented in test suite
2. **T2-03 Simple Fender Model** — Next
3. **T2-04 D02 Pull-in Analysis**
4. **T2-02 A01 Lazy Wave Riser**

For each model:
1. Create `spec.yml` (if not monolithic-only)
2. Generate modular output
3. Add test class to `test_modular_vs_monolithic.py`
4. Pre-compute statics.sim for CI/CD

### Phase C: Tier 1 Pre-computed Models

Add tests for existing .sim files:
1. Load monolithic .sim
2. Extract reference values
3. Store in `validation/results.json`
4. Compare modular generation against reference

### Phase D: Production Models

- Fix convergence issue with 24in/30in pipeline models
- Investigate solver settings from working `vessel_end_winch.sim`
- Generate pre-computed statics.sim once convergence is achieved

---

## Test Classes to Add

```python
# test_modular_vs_monolithic.py additions

class TestT2SimpleFender:
    """T2-03: Simple Fender Model validation (<5s statics)."""

class TestT2PullInAnalysis:
    """T2-04: D02 Pull-in Analysis validation (<10s statics)."""

class TestT1MetoceanBuoy:
    """T1-01: C07 Metocean Buoy from pre-computed .sim."""

class TestT1CALMBuoy:
    """T1-03: C06 CALM Buoy from pre-computed .sim."""
```

---

## Mesh Coarsening Strategy

For large production models, create coarse variants for quick QA:

| Level | Segment Length | Elements (4900m) | Est. Time | Use Case |
|-------|---------------|------------------|-----------|----------|
| **Ultra-coarse** | 50m | ~98 | <30s | Smoke test |
| **Coarse** | 10m | ~490 | ~2 min | Quick QA |
| **Medium** | 2m | ~2450 | ~5 min | Comparison |
| **Fine** | 0.5m | ~9800 | ~15 min | Production |

Implementation: Separate spec files with different `segment_length` values.

---

## Files to Create/Modify

### Immediate (Phase B)

| Action | File | Description |
|--------|------|-------------|
| **Create** | `tests/.../test_tier2_fast_models.py` | Dedicated test file for Tier 2 models |
| **Modify** | `tests/.../test_modular_vs_monolithic.py` | Add more fast model test classes |
| **Create** | Per-model `spec.yml` files | As models are converted |

### Library Structure

| Action | Path | Description |
|--------|------|-------------|
| **Create** | `docs/modules/orcaflex/library/` | New library root directory |
| **Move** | Tier 1-4 subdirectories | Organized model storage |
| **Create** | `validation/results.json` per model | Reference extraction results |

---

## Verification Criteria

For each model in the library:

1. **spec.yml exists** and validates against schema
2. **monolithic/ loads** in OrcFxAPI without error
3. **modular/ generates** from spec without error
4. **Statics converge** on both models
5. **Results match** within defined tolerances
6. **Pre-computed .sim** available for CI/CD (optional for Tier 3-4)

---

## Known Issues

### 24in/30in Pipeline Convergence

Both modular AND monolithic 24in pipeline models fail to converge:
```
OrcFxAPI.DLLError: Error code: 27
Static calculation failed (Whole system statics: Not converged.)
```

**Investigation needed**:
- Compare `vessel_end_winch.yml` (which converges) vs base YAML
- Identify solver settings that enable convergence
- Apply findings to spec.yml generation

---

## Progress Tracking

| Model | Spec | Monolithic | Modular | Statics (mono) | Statics (mod) |
|-------|------|------------|---------|----------------|---------------|
| A01 Catenary Riser | ✓ | ✓ | ✓ | ✓ (0.27s) | ✓ (0.15s) |
| A01 Lazy Wave Riser | ✓ | ✓ | ✓ | ✓ (0.84s) | ✓ (0.50s) |
| A01 Pliant Wave Riser | ✓ | ✓ | ✓ | ✓ (1.05s) | ✓ (0.24s) |
| A01 Steep Wave Riser | ✓ | ✓ | ✓ | ✓ (1.14s) | ✓ (0.14s) |

**Convergence Fix (2026-02-05)**: All 4 A01 riser models now converge statics:
- **Root cause**: `RiserClumpTypeBuilder` was generating `6DBuoys` (6-DOF rigid bodies) instead of `ClumpTypes` (inline attachments). Each 6DBuoy adds 6 DOFs; with 51 modules, this created 306 unconstrained DOFs causing singular Jacobian.
- **Fix 1**: Rewrote builder to generate `ClumpTypes:` section with correct OrcaFlex format (Name, Mass, Volume, Height, AlignWith, DragArea[3], Cd[3], Ca[3])
- **Fix 2**: Changed attachment format from dict-based to multi-column: `AttachmentType, Attachmentx, Attachmenty, Attachmentz, AttachmentzRelativeTo`
- **Fix 3**: Added `StaticsStep2: Full statics` support (schema + builder)
- **Fix 4**: Lazy wave spec updated with correct monolithic parameters (2.5m spacing, 0.625m segments, 20 modules from 81.25-128.75m)
- **Fix 5**: Pliant wave spec rewritten with correct monolithic parameters using "Simple" single-line variant (vessel [0,-48,0] heading 90, OD=0.3926, EI=361.85, EA=650e3)

**Generator Extension (2026-02-05)**: Riser support added to ModularModelGenerator:
- New schema: `RiserLineType`, `RiserLine`, `EndConnection`, `RiserVessel`, etc.
- New builders: `RiserLineTypeBuilder`, `RiserLinesBuilder`, `RiserVesselBuilder`, `RiserClumpTypeBuilder`
- Pipeline builders skip for riser models via `should_generate()` checks
- All 4 A01 riser specs updated to match Pydantic schema format
- 14 new tests in `test_riser_generation.py` (all passing)
- OrcFxAPI load + statics verified for all 4 models

### Library Locations

| Model | Library Path |
|-------|--------------|
| A01 Catenary Riser | `docs/modules/orcaflex/library/tier2_fast/a01_catenary_riser/` |
| A01 Lazy Wave Riser | `docs/modules/orcaflex/library/tier2_fast/a01_lazy_wave_riser/` |
| A01 Pliant Wave Riser | `docs/modules/orcaflex/library/tier2_fast/a01_pliant_wave_riser/` |
| A01 Steep Wave Riser | `docs/modules/orcaflex/library/tier2_fast/a01_steep_wave_riser/` |
| C09 Fenders | TODO | ✓ | TODO | ✓ (10s) | ✓ |
| D02 Pull-in | TODO | ✓* | TODO | SKIP | - |
| 24in QA (10m mesh) | ✓ | ✓ | ✓ | SKIP** | ✓ |
| 30in Pipeline | - | ✓ | ✓ | ✓ (.sim) | ✓ |

*D02 YAML has version compatibility issue (OrcaFlex 11.5e properties)
**SKIP = Non-convergence (known issue, affects monolithic too)

### Test Summary (2026-02-05)

```
TestInstantRiser (A01 Catenary):     5 passed, 0 skipped
TestLazyWaveRiser (A01 Lazy Wave):   5 passed, 0 skipped
TestFenderModel (C09 Fenders):       4 passed, 0 skipped
TestPullInAnalysis (D02 Pull-in):    1 passed, 4 skipped (version)
TestCrossValidation30in:             3 passed
---
Validation Total:                   18 passed, 4 skipped in 26.61s

TestRiserSchemaValidation:           8 passed
TestRiserSpecLoading:                2 passed
TestRiserModularGeneration:          4 passed
TestRiserOrcaFlexLoad:               1 passed
---
Riser Generation Total:             14 passed in 4.87s

Statics convergence (all 4 models):
  a01_catenary_riser      CONVERGED  (0.15s)
  a01_lazy_wave_riser     CONVERGED  (0.50s)
  a01_pliant_wave_riser   CONVERGED  (0.24s)
  a01_steep_wave_riser    CONVERGED  (0.14s)
```
