# Plan: digitalmodel #556 — `test_get_coefficients_interpolation` CYw bound

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/556
**Status:** plan-review
**Tier:** T1 (single assertion fix or single source helper)

## Root cause
- Test: `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py::TestOCIMFDatabase::test_get_coefficients_interpolation`
- Assertion `0 <= CYw <= 1.5` fails for `CYw = -3.56`. The sample OCIMF DB has signed lateral-wind coefficients; the assertion treats them as magnitudes.
- Per OCIMF MEG4 sign convention, lateral wind force coefficients can be negative (sign indicates direction).

## Plan
### Task 1 — Reproduce
```
uv run pytest tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py::TestOCIMFDatabase::test_get_coefficients_interpolation -xvs
```

### Task 2 — Verify direction (test vs source)
Open the OCIMF DB module under `src/digitalmodel/marine_ops/environmental_loading/` (or sibling) and confirm the coefficient values stored match MEG4 convention (signed). Cross-check against any docstring stating sign convention.
- If source emits signed values per MEG4: this is a **test fix** — change assertion bounds.
- If source previously emitted magnitudes and recently switched: this is a **source fix** (out of scope for this single-test plan; escalate).

Hypothesis (per issue body): test fix — bound is wrong.

### Task 3 — Fix
Edit `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py`:
- Change `0 <= CYw <= 1.5` to `abs(CYw) <= 3.6` (or per OCIMF reference range).
- Add a one-line comment citing MEG4 sign convention.

### Task 4 — Verify
```
uv run pytest tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py::TestOCIMFDatabase::test_get_coefficients_interpolation -xvs
```

### Task 5 — Confirm no regressions
```
uv run pytest tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py -x
```

## Acceptance Criteria
- The single test passes.
- Sibling tests in `test_ocimf.py` continue to pass (esp. #557 `test_boundary_warnings`).
- The new bound has a comment citing the OCIMF MEG4 sign convention.

## Open questions
- Confirm whether 3.56 magnitude is genuinely physical or a fixture artifact — if the latter, fix the fixture instead of relaxing the assertion.
