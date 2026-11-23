# Plan: digitalmodel #557 — `test_boundary_warnings` DID NOT WARN

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/557
**Status:** plan-review
**Tier:** T1 (single source-side restoration OR test relaxation)

## Root cause
- Test: `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py::TestOCIMFDatabase::test_boundary_warnings`
- Failure: `pytest.warns(UserWarning)` — DID NOT WARN. Source no longer emits the boundary warning when the OCIMF interpolator is queried out-of-range.

## Plan
### Task 1 — Reproduce
```
uv run pytest tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py::TestOCIMFDatabase::test_boundary_warnings -xvs
```

### Task 2 — Determine direction (source vs test)
Run `git blame` on the OCIMF interpolator (likely under `src/digitalmodel/marine_ops/environmental_loading/`) for the `warnings.warn(...)` call:
- If recently removed in a refactor: **(a) regression** — restore the warning emission. Source fix.
- If intentionally replaced with clamping behavior: **(b) test fix** — replace `pytest.warns(UserWarning)` with assertion on clamped output.

```
git log --oneline -p -S "boundary" -- src/digitalmodel/marine_ops/ | head -100
git log --oneline -S "warnings.warn" -- src/digitalmodel/marine_ops/environmental_loading/ | head -20
```

### Task 3 — Fix
- If (a): re-add `warnings.warn("...out of range...", UserWarning)` at the boundary-check site in the interpolator.
- If (b): replace test body with assertion that the clamped output equals the boundary value.

### Task 4 — Verify
```
uv run pytest tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py::TestOCIMFDatabase::test_boundary_warnings -xvs
```

### Task 5 — Confirm no regressions
```
uv run pytest tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py -x
```

## Acceptance Criteria
- The single test passes.
- Sibling `test_ocimf.py` tests (esp. #556) remain green.
- PR body cites the git-blame finding that determined direction (a) vs (b).

## Open questions
- Was the warning intentionally removed? Git blame must answer before editing — do not silently choose either path.
