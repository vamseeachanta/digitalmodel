---
title: "OFFPIPE Integration Module — Pipelay Cross-Validation Against OrcaFlex"
description: "Full OFFPIPE module for independent cross-validation of OrcaFlex pipelay analysis. MVP scoped to S-lay static analysis."
version: "0.2.0"
module: offpipe
session:
  id: "dazzling-snuggling-thunder"
  agent: "claude-opus-4-5"
review:
  cross_review_mandatory: true
  iterations: 3
  reviewers:
    - agent: feasibility-reviewer
      status: complete
      findings: 6
    - agent: architecture-reviewer
      status: complete
      findings: 5
    - agent: testing-reviewer
      status: complete
      findings: 5
work_item: WRK-075
---

# OFFPIPE Integration Module — Pipelay Cross-Validation Against OrcaFlex

## Executive Summary

Full OFFPIPE module for independent cross-validation of OrcaFlex pipelay analysis. OFFPIPE uses a beam-column FEM formulation vs OrcaFlex's lumped-mass/spring approach, making it the most practical independent verification tool for pipelay analysis. MVP scoped to S-lay static analysis.

**User provides**: OFFPIPE documentation + example files + matching OrcaFlex scenario.

## Cross-Review Findings (v0.2.0)

### Feasibility Review
1. **CRITICAL**: No OrcaFlex pipelay result extractor exists — must be built in `orcaflex/pipelay_results.py`
2. **HIGH**: engine.py registration missing from original plan — added as Phase 1
3. **MEDIUM**: Scope too large for single pass — MVP scoped to S-lay static only
4. **MEDIUM**: Tolerance thresholds unvalidated — need relative + absolute per parameter
5. **LOW**: No error handling for malformed OFFPIPE files
6. **LOW**: Input generator premature — deferred to Phase 6

### Architecture Review
1. **HIGH**: Premature sub-packaging — flatten to match diffraction pattern (all .py at same level)
2. **HIGH**: OrcaFlex pipelay converter should live in `orcaflex/` for reuse across modules
3. **MEDIUM**: Unified pipelay schema undefined — must be explicit dataclasses
4. **MEDIUM**: Router pattern must match existing convention exactly (thin class, if/elif dispatch)
5. **LOW**: YAML tolerance config should support per-parameter relative + absolute thresholds

### Testing Review
1. **HIGH**: Original plan not TDD-compliant — every phase must lead with test files
2. **HIGH**: No test data strategy — need conftest.py, fixtures/, golden references
3. **MEDIUM**: No edge cases defined — empty files, partial results, mismatched arc lengths
4. **MEDIUM**: Tolerance validation needs both relative and absolute thresholds
5. **LOW**: No pytest markers for slow/integration tests

## Module Structure (Flat — matches diffraction pattern)

```
src/digitalmodel/offpipe/
├── __init__.py                   # Exports, __all__, version metadata
├── offpipe.py                    # Router class (thin wrapper, if/elif dispatch)
├── output_parser.py              # Parse OFFPIPE S-lay static result files
├── input_generator.py            # Phase 6 (deferred) — generate OFFPIPE input decks
├── offpipe_converter.py          # OFFPIPE results → unified PipelayResults schema
├── pipelay_comparator.py         # Cross-validation comparison engine
├── tolerance_config.py           # Per-parameter configurable tolerances (YAML)
├── pipelay_schemas.py            # Unified pipelay data schema (dataclasses)
├── benchmark_report.py           # Plotly HTML interactive reports

src/digitalmodel/orcaflex/
├── pipelay_results.py            # NEW: Extract pipelay results via OrcFxAPI
```

## Test Structure

```
tests/offpipe/
├── conftest.py                   # Fixtures, golden data loaders, pytest markers
├── fixtures/
│   ├── offpipe_slay_static.out   # Golden OFFPIPE output file
│   ├── offpipe_slay_expected.json # Parsed golden reference
│   └── orcaflex_slay_results.json # Matching OrcaFlex reference
├── test_output_parser.py         # Parser tests (Phase 2)
├── test_pipelay_schemas.py       # Schema validation tests (Phase 2)
├── test_offpipe_converter.py     # Converter tests (Phase 3)
├── test_orcaflex_pipelay.py      # OrcaFlex extractor tests (Phase 3)
├── test_pipelay_comparator.py    # Comparison engine tests (Phase 4)
├── test_tolerance_config.py      # Tolerance config tests (Phase 4)
├── test_benchmark_report.py      # Report generation tests (Phase 5)
├── test_offpipe_router.py        # Router + engine.py tests (Phase 1)
└── test_integration.py           # End-to-end cross-validation (Phase 5)
```

## Unified Pipelay Schema

```python
@dataclass
class PipelayMetadata:
    water_depth: float
    pipe_od: float
    pipe_wt: float
    lay_method: str           # "s-lay", "j-lay"
    source_tool: str          # "offpipe", "orcaflex"

@dataclass
class PipeProfile:
    arclength: np.ndarray     # Arc length along pipe (m)
    stress_vonmises: np.ndarray
    axial_force: np.ndarray
    bending_moment: np.ndarray
    curvature: np.ndarray
    tension: np.ndarray
    deflection_z: np.ndarray

@dataclass
class PipelayResults:
    metadata: PipelayMetadata
    sagbend: PipeProfile
    overbend: PipeProfile
    suspended: PipeProfile
    summary: dict             # Key scalars: max stress, min tension, etc.
```

## Tolerance Configuration

```yaml
# offpipe/tolerances.yaml
tolerances:
  stress_vonmises:
    relative: 0.10    # 10% relative tolerance
    absolute: 5.0     # 5 MPa absolute tolerance
  curvature:
    relative: 0.10
    absolute: 0.001   # 1/km absolute
  tension:
    relative: 0.10
    absolute: 5.0     # 5 kN absolute
  bending_moment:
    relative: 0.10
    absolute: 2.0     # 2 kN·m absolute
  axial_force:
    relative: 0.10
    absolute: 5.0     # 5 kN absolute
  deflection_z:
    relative: 0.15
    absolute: 0.5     # 0.5 m absolute

classification:
  EXCELLENT: 0.05     # ≤5% max deviation
  GOOD: 0.10          # ≤10%
  FAIR: 0.20          # ≤20%
  POOR: 1.0           # >20%
```

## Router Pattern

```python
class Offpipe:
    def __init__(self):
        pass

    def router(self, cfg: dict) -> dict:
        name = cfg["calculation"]["name"]
        if name == "parse_output":
            cfg = output_parser.run(cfg)
        elif name == "generate_input":
            cfg = input_generator.run(cfg)
        elif name == "cross_validate":
            cfg = pipelay_comparator.run(cfg)
        elif name == "benchmark_report":
            cfg = benchmark_report.run(cfg)
        else:
            raise ValueError(f"Unknown calculation: {name}")
        return cfg
```

## Implementation Phases

### Phase 0: Documentation Analysis (GATE)
**Blocked until user provides OFFPIPE docs + example files + matching OrcaFlex scenario.**

- [ ] Analyze OFFPIPE output file format from documentation
- [ ] Identify all parseable fields and their units
- [ ] Map OFFPIPE fields to unified schema fields
- [ ] Document any fields that don't have OrcaFlex equivalents
- [ ] Validate that user's example files match documentation format

### Phase 1: Scaffold + Router + engine.py Registration
Tests first: `test_offpipe_router.py`

- [ ] Create `offpipe/__init__.py` with exports
- [ ] Create `offpipe/offpipe.py` with router class (thin, if/elif dispatch)
- [ ] Register `offpipe` basename in `engine.py`
- [ ] Create `tests/offpipe/conftest.py` with fixtures and markers
- [ ] Verify YAML routing works end-to-end

### Phase 2: Output Parser + Schema
Tests first: `test_output_parser.py`, `test_pipelay_schemas.py`

- [ ] Define `pipelay_schemas.py` (PipelayMetadata, PipeProfile, PipelayResults)
- [ ] Implement `output_parser.py` — parse OFFPIPE S-lay static output
- [ ] Validate against golden reference file
- [ ] Handle edge cases: empty files, partial results, encoding issues
- [ ] Store golden test data in `tests/offpipe/fixtures/`

### Phase 3: OrcaFlex Pipelay Extraction + Converter
Tests first: `test_orcaflex_pipelay.py`, `test_offpipe_converter.py`

- [ ] Create `orcaflex/pipelay_results.py` — extract pipelay results via OrcFxAPI
- [ ] Implement `offpipe_converter.py` — OFFPIPE parsed data → PipelayResults
- [ ] Both converters produce identical PipelayResults schema
- [ ] Arc-length interpolation for mismatched grids
- [ ] pytest.skip if OrcFxAPI not available

### Phase 4: Comparison Engine + Tolerances
Tests first: `test_pipelay_comparator.py`, `test_tolerance_config.py`

- [ ] Implement `tolerance_config.py` — load YAML, per-parameter relative+absolute
- [ ] Implement `pipelay_comparator.py` — compare two PipelayResults
- [ ] Per-parameter deviation calculation (relative and absolute)
- [ ] Agreement classification: EXCELLENT/GOOD/FAIR/POOR
- [ ] Summary statistics: max deviation, mean deviation, % within tolerance

### Phase 5: Benchmark Report + Integration
Tests first: `test_benchmark_report.py`, `test_integration.py`

- [ ] Implement `benchmark_report.py` — interactive Plotly HTML report
- [ ] Overlay plots: OFFPIPE vs OrcaFlex per parameter per region
- [ ] Agreement classification badges per parameter
- [ ] Summary table with all deviations
- [ ] End-to-end integration test: parse → convert → compare → report

### Phase 6: Input Generator + Skills (Deferred)
- [ ] `input_generator.py` — generate OFFPIPE input decks from unified config
- [ ] Create skill: `offpipe-analysis`
- [ ] Create skill: `offpipe-orcaflex-benchmark`

## engine.py Registration

Add to the existing basename dispatch in `engine.py`:

```python
elif basename == "offpipe":
    from digitalmodel.offpipe.offpipe import Offpipe
    cfg = Offpipe().router(cfg)
```

## Acceptance Criteria

- [ ] OFFPIPE S-lay output files parsed correctly (validated against golden reference)
- [ ] OrcaFlex pipelay results extracted via OrcFxAPI
- [ ] Cross-validation comparison with configurable per-parameter tolerances
- [ ] Interactive Plotly HTML benchmark report with agreement classification
- [ ] All tests pass via `uv run pytest tests/offpipe/` (TDD, no mocks, real data)
- [ ] engine.py routes `offpipe` basename correctly
- [ ] Skills created: `offpipe-analysis`, `offpipe-orcaflex-benchmark` (Phase 6)

## Blocked On

- [ ] User provides OFFPIPE documentation (PDF/manual)
- [ ] User provides example S-lay input deck
- [ ] User provides corresponding OFFPIPE output file(s)
- [ ] User provides matching OrcaFlex model + simulation results

---

*Cross-reviewed by 3 independent agents (feasibility, architecture, testing). All 16 findings incorporated into v0.2.0.*
