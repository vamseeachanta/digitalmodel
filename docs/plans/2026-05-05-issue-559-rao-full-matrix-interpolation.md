# Plan: digitalmodel #559 — `test_full_matrix_interpolation` strict-greater on equal floats

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/559
**Status:** plan-review
**Tier:** T1 (single-line test assertion fix)

## Root cause
- Test: `tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py::TestHydroRAOIntegration::test_full_matrix_interpolation`
- Failure: `Added mass should be diagonally dominant at (3,1): assert 624146.29 > 624146.29`.
- Diagonal dominance check uses strict `>` against floats that compare arithmetically equal at this matrix entry. Should use `>=` (or `pytest.approx` with tolerance).

## Plan
### Task 1 — Reproduce
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py::TestHydroRAOIntegration::test_full_matrix_interpolation -xvs
```

### Task 2 — Fix
Edit `tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py`:
- Locate the `Added mass should be diagonally dominant` assertion.
- Change `>` to `>=`. (Or, if the test author intended strict dominance and floats should not be equal except at degenerate fixtures, perturb the off-diagonal fixture by epsilon — but this expands scope. `>=` is the safe surgical fix.)

### Task 3 — Verify
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py::TestHydroRAOIntegration::test_full_matrix_interpolation -xvs
```

### Task 4 — Confirm no regressions
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py -x
```

## Acceptance Criteria
- The single test passes.
- No regression in sibling `test_hydro_rao_integration.py` tests.
- The assertion comment is updated to clarify "diagonally dominant or equal" if `>=` is chosen.
