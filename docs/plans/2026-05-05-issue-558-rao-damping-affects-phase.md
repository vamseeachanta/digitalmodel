# Plan: digitalmodel #558 — `test_damping_affects_phase` phase sign convention

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/558
**Status:** plan-review
**Tier:** T1 (single convention alignment)

## Root cause
- Test: `tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py::TestHydroRAOIntegration::test_damping_affects_phase`
- Failure: `Phase at resonance 138.4° should be near -90°` — phase-sign convention mismatch.
- 138.4° ≡ -221.6° ≡ 138.4° (mod 360); does not equal -90° under either lead-positive or lag-negative convention without wrap. Likely the source uses the `e^{i(omega t + phi)}` (lead-positive) convention while the test expects `e^{i(omega t - phi)}` (lag-negative) — at resonance, response lags forcing by 90° under critical damping.

## Plan
### Task 1 — Reproduce
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py::TestHydroRAOIntegration::test_damping_affects_phase -xvs
```

### Task 2 — Identify canonical convention
Inspect docstring / module header of the RAO source under `src/digitalmodel/marine_ops/marine_analysis/` (likely `unified_rao_reader.py` or `hydro/rao_*.py`):
- Look for any "phase convention" / "sign convention" / "ITTC" / "DNV" comment.
- Check whether the same module defines a `convert_phase()` or `unwrap()` helper indicating the intended convention.
- If undocumented: pick lag-negative (DNV / ITTC ship-hydrodynamics standard); add a docstring stating it.

### Task 3 — Fix
- If source uses lead-positive: negate phase in source (`phase = -phase`) at the integration boundary, OR
- If test is wrong: update assertion expected value to `+90°` (or unwrapped equivalent).

Smaller-blast-radius preference: change the test if the source convention is documented. Change the source only if the docstring claims lag-negative but emits lead-positive.

### Task 4 — Verify
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py::TestHydroRAOIntegration::test_damping_affects_phase -xvs
```

### Task 5 — Confirm no regressions
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py -x
```

## Acceptance Criteria
- The single test passes.
- Sibling RAO tests (#559, #560) remain unaffected (they may share the same convention — verify before this fix).
- PR body identifies the canonical convention chosen and cites the source-module docstring (added or pre-existing).

## Open questions
- Does the same convention bug affect #559 / #560? If yes, batch all three under this plan and close the others as duplicates.
