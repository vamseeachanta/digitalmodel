# WRK-049: Dynacard Module Way Forward

## Metadata
- **id**: WRK-049
- **title**: Determine dynacard module way forward
- **version**: 1.0
- **module**: marine_ops/artificial_lift/dynacard
- **session.agent**: orchestrator
- **review**: pending

---

## Decision: CONTINUE development

### Rationale

The dynacard module is **feature-complete, well-tested, and architecturally sound**. It should continue as a core module in digitalmodel. There is no case for deprecation or pivot.

**Evidence supporting CONTINUE:**

| Criterion | Finding |
|-----------|---------|
| **Code volume** | 21 source files, 5,633 lines — substantial investment |
| **Test coverage** | 15 test files, 4,755 lines of tests — 84% test/code ratio |
| **Test status** | **317 tests, ALL PASSING** |
| **Architecture** | Clean separation: models → physics → calculators → diagnostics → workflow |
| **Type safety** | Full Pydantic v2 models, 30+ data classes |
| **Error handling** | Custom 6-type exception hierarchy |
| **Extensibility** | Generic `BaseCalculator[T]` pattern for all calculators |
| **Standards compliance** | API RP 11L, API 11E, Gibbs 1963 references |
| **Dual solvers** | Gibbs (frequency) + Finite Difference (time-domain) with comparison |
| **Real data validation** | Parametrized tests against 5 anonymized production wells |
| **Parity testing** | Solver parity test against legacy gold-standard outputs |

---

## Current State Assessment

### Strengths
1. **Complete analysis pipeline**: Surface card → downhole card → P1-P7 calculations → AI diagnostics
2. **12 specialized calculators**: gear box loading, power consumption, lift capacity, load analysis, pump efficiency, geometry, rod buckling, ideal card, torque balance, corners, physics, diagnostics
3. **Per-calculator test suites**: Each calculator has its own dedicated test file (226-484 lines each)
4. **Production data testing**: Tests run against cleansed field data, not just synthetic
5. **Workflow orchestrator** with router pattern for engine.py integration

### Gaps Identified
1. **Base class and solver coverage**: `base.py` (45%), `solver.py` (46%), `corners.py` (69%) have lower coverage
2. **No README**: Module directory lacks a top-level README.md
3. **AI diagnostics**: Threshold-based pattern matching (6 failure modes) — functional but basic
4. **Finite difference solver**: Less exercised in tests than Gibbs solver
5. **Solver parity test**: Depends on external reference file (skipped when unavailable)
6. **No CLI entry point**: Not registered in pyproject.toml scripts

---

## Recommended Next Steps (Priority Order)

### P1 — Improve test coverage for under-tested files
- **`solver.py`** (46% coverage): Test `router()` method, `compare_solvers()`, error paths
- **`base.py`** (45% coverage): Test validation helpers, edge cases in abstract methods
- **`corners.py`** (69% coverage): Test corner detection with various card shapes
- **`calculations.py`** (78%): Cover remaining branches (empty data, edge values)
- Target: bring all files above 85%

**Files to modify:**
- `tests/marine_ops/artificial_lift/dynacard/` — add/extend test files
- `tests/marine_ops/artificial_lift/test_dynacard.py` — extend workflow tests

### P2 — Add module README
- Create `src/digitalmodel/marine_ops/artificial_lift/dynacard/README.md`
- Document: purpose, quickstart, API overview, calculator catalog, solver comparison

### P3 — Register CLI entry point
- Add dynacard to `pyproject.toml` `[project.scripts]` section
- Enable `uv run digitalmodel-dynacard` for field use

### P4 — Improve AI diagnostics (future)
- Move from threshold-based to learning-based classification
- Add more failure modes beyond the current 6
- This is a larger effort and can be a separate work item

---

## Acceptance Criteria Resolution

| Criterion | Status | Evidence |
|-----------|--------|---------|
| Review current state | DONE | 21 files, 5,633 LOC, 317 passing tests |
| Identify gaps/issues | DONE | Coverage gaps in solver.py/base.py/corners.py, no README, no CLI |
| Assess use cases | DONE | SRP diagnostics, field troubleshooting, production reporting |
| Decision documented | DONE | **CONTINUE** — module is mature and valuable |
| If continuing: next steps | DONE | P1-P4 prioritized above |

---

## Implementation Plan (P1: Test Coverage)

This is the only step that requires code changes. P2-P4 can be separate work items.

### Step 1: Extend `solver.py` tests
- **File**: `tests/marine_ops/artificial_lift/test_dynacard.py`
- **Add tests for**: `router()` with valid config dict, `compare_solvers()`, solver method switching, missing context handling
- **Reuse**: existing `sample_context` fixture

### Step 2: Extend `base.py` tests
- **File**: New `tests/marine_ops/artificial_lift/dynacard/test_base.py`
- **Add tests for**: validation helper methods, array length checks, missing data errors
- **Reuse**: existing Pydantic models from `models.py`

### Step 3: Extend `corners.py` tests
- **File**: New `tests/marine_ops/artificial_lift/dynacard/test_corners.py`
- **Add tests for**: `CornerDetector`, `calculate_corners()`, `get_corner_loads()` with various card shapes
- **Reuse**: existing `sample_context` fixture and card generation patterns

### Step 4: Fill `calculations.py` branch coverage
- **File**: Extend existing test through `test_dynacard.py` or new dedicated file
- **Add tests for**: empty arrays, zero-value edge cases, boundary conditions

---

## Verification

```bash
# Run all dynacard tests
uv run pytest tests/marine_ops/artificial_lift/ -v

# Check coverage for dynacard module specifically
uv run pytest tests/marine_ops/artificial_lift/ --cov=src/digitalmodel/marine_ops/artificial_lift/dynacard --cov-report=term-missing

# Verify no regressions
uv run pytest tests/marine_ops/artificial_lift/ -x --tb=short
```

---

## Work Queue Update

After implementation, update WRK-049:
- Status: `completed`
- Decision: CONTINUE
- Create follow-up items for P2 (README), P3 (CLI), P4 (AI diagnostics) if desired
