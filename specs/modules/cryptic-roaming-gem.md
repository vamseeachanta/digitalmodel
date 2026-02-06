# Plan: OrcaFlex Analysis Validation — Modular vs Monolithic

> **Status**: COMPLETED — Tests implemented in `test_modular_vs_monolithic.py`
> **Superseded by**: `specs/modules/orcaflex-qa-fast-models.md` (QA strategy with fast models)

## Objective

Validate that the modular generator output produces numerically equivalent OrcaFlex results compared to the monolithic baseline. Run static analysis on both the generated modular model and the monolithic base, extract engineering results (effective tension, bending moment at line ends), and compare within tolerances.

---

## Background

The modular generator (WRK-032) produces structurally valid OrcaFlex YAML, verified by 239 passing tests. However, **no OrcaFlex solver validation** has been performed — we have not confirmed that the generated model produces the same engineering results as the original monolithic model.

**Key finding**: OrcFxAPI natively resolves `includefile` directives, so `master.yml` can be loaded directly without conversion.

---

## Comparison Strategy

### What we compare

| Model | Source | Line Name |
|-------|--------|-----------|
| **Monolithic baseline** | `docs/.../24in_pipeline/monolithic/basefile/D24inL4900mBuoy7kNSpacing1500mm.yml` | `pipeline` |
| **Modular (generated)** | Generated from `docs/.../24in_pipeline/spec.yml` via `ModularModelGenerator` | `pipeline` |

### Known structural differences (not bugs)

| Difference | Monolithic | Modular | Impact |
|------------|-----------|---------|--------|
| FE segments | 6 sections (0.1–0.5m mesh) | 1 section (0.5m mesh) | Minor mesh sensitivity at boundaries |
| Winch wire | Present (165m, OD=0.08m) | Absent (schema gap) | Small lateral restraint difference at vessel end |
| Buoy naming | `buoy1`, `buoy2`, etc. | Generator naming convention | No numerical impact |
| Ramp-curve profile | 2-point diameter table | Simplified parameters | Minor contact force difference |

### Tolerances

| Quantity | Relative | Absolute | Rationale |
|----------|----------|----------|-----------|
| End A/B effective tension | 5% | 10 kN | End forces from global equilibrium, less mesh-sensitive |
| End A/B bending moment | 15% | 5 kN·m | Bending is mesh-sensitive near boundary conditions |
| Pipeline total length | 0.01% | 0.1 m | Pure geometry check |
| Static convergence | — | Both must converge | Binary pass/fail |

---

## Phases

### Phase A: Smoke Test — Load & Statics

Can the modular output load in OrcFxAPI and converge statics?

**Tests** (~5):
1. Generate modular output from 24in spec.yml to `tmp_path`
2. Load modular `master.yml` via `OrcFxAPI.Model()` — no exception
3. Load monolithic baseline via `OrcFxAPI.Model()` — no exception
4. `model.CalculateStatics()` converges on modular model
5. `model.CalculateStatics()` converges on monolithic model

### Phase B: Static Result Extraction

Extract engineering results from both models after statics.

**OrcFxAPI calls**:
```python
line = model["pipeline"]
end_a_tension = line.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
end_b_tension = line.StaticResult("Effective Tension", OrcFxAPI.oeEndB)
end_a_bending = line.StaticResult("Bend Moment", OrcFxAPI.oeEndA)
end_b_bending = line.StaticResult("Bend Moment", OrcFxAPI.oeEndB)
```

**Tests** (~4):
1. Extract modular static results — values are finite and reasonable
2. Extract monolithic static results — values are finite and reasonable
3. Modular pipeline length ≈ 4900m
4. Monolithic pipeline length ≈ 4900m

### Phase C: Comparison

Compare modular results against monolithic within tolerances.

**Tests** (~5):
1. End A effective tension within 5% / 10 kN
2. End B effective tension within 5% / 10 kN
3. End A bending moment within 15% / 5 kN·m
4. End B bending moment within 15% / 5 kN·m
5. Object count difference explained (monolithic has winch wire objects)

### Phase D: 30in Cross-Validation (bonus)

The 30in pipeline already has `monolithic.sim` and `modular/master.sim`. Load both and compare.

**Tests** (~3):
1. Load 30in monolithic.sim — extract End A/B tension
2. Load 30in modular master.sim — extract End A/B tension
3. Compare within 5% tolerance

---

## Implementation

### File to create

**`tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py`**

~17 tests across 4 test classes, all gated by:
```python
try:
    import OrcFxAPI
    ORCAFLEX_AVAILABLE = True
except ImportError:
    ORCAFLEX_AVAILABLE = False

requires_orcaflex = pytest.mark.skipif(
    not ORCAFLEX_AVAILABLE, reason="OrcFxAPI not available"
)
```

### Fixtures (session-scoped for efficiency)

```python
@pytest.fixture(scope="session")
def modular_output_dir(tmp_path_factory):
    """Generate modular output from 24in spec once per session."""
    ...

@pytest.fixture(scope="session")
def modular_model(modular_output_dir):
    """Load modular master.yml and run statics."""
    ...

@pytest.fixture(scope="session")
def monolithic_model():
    """Load monolithic baseline and run statics."""
    ...
```

### Helper function

```python
@dataclass
class StaticEndResults:
    line_name: str
    end_a_tension: float  # kN
    end_b_tension: float  # kN
    end_a_bending: float  # kN·m
    end_b_bending: float  # kN·m
    object_count: int

def extract_static_end_results(model, line_name: str) -> StaticEndResults:
    """Extract static results at line ends from a converged OrcaFlex model."""
    ...
```

### Key infrastructure to reuse

| Component | Location | Usage |
|-----------|----------|-------|
| `ModularModelGenerator` | `src/.../modular_generator/__init__.py` | Generate modular output from spec |
| `Level2OrcaFlexValidator` pattern | `src/.../modular_input_validation/level_2_orcaflex.py` | OrcFxAPI availability check, model loading |
| `postProcess.py` patterns | `src/.../post_results/postProcess.py` | RangeGraph / StaticResult extraction API |
| 24in spec fixture | `tests/.../test_24in_spec.py` | Path constants, spec loading pattern |

### Path constants

```python
PIPELINE_24IN = Path("docs/modules/orcaflex/pipeline/installation/floating/24in_pipeline")
SPEC_24IN = PIPELINE_24IN / "spec.yml"
MONOLITHIC_24IN = PIPELINE_24IN / "monolithic/basefile/D24inL4900mBuoy7kNSpacing1500mm.yml"

PIPELINE_30IN = Path("docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline")
MONOLITHIC_30IN_SIM = PIPELINE_30IN / "monolithic.sim"
MODULAR_30IN_SIM = PIPELINE_30IN / "modular/master.sim"
```

---

## Verification

1. All tests pass with OrcFxAPI available (Phases A–D)
2. All tests skip gracefully without OrcFxAPI (`requires_orcaflex` marker)
3. Existing 239 modular generator tests still pass (zero regressions)
4. Comparison results printed to stdout for engineering review
5. Any tolerance failures investigated and documented as known differences

## Files to create/modify

| Action | File |
|--------|------|
| **Create** | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` |

No source code modifications needed.
