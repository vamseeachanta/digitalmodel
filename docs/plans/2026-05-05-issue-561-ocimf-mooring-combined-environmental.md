# Plan: digitalmodel #561 — `test_combined_environmental_forces` wrong test premise

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/561
**Status:** plan-review
**Tier:** T1 (test-design fix)

## Root cause
- Test: `tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py::TestOCIMFMooringIntegration::test_combined_environmental_forces`
- Failure: `Wind lateral force should exceed current: 526653 > 4059000` is false. At the test's wind=20 m/s and current=1.5 m/s, current force genuinely dominates because `lateral_area_current = 7040 m²` ≫ `lateral_area_wind = 3200 m²`. The test premise is physically wrong for this vessel geometry.

## Plan
### Task 1 — Reproduce
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py::TestOCIMFMooringIntegration::test_combined_environmental_forces -xvs
```

### Task 2 — Decide fix shape
Two options:
- **(a) Invert the assertion:** assert `current > wind` for this geometry. Document why in a comment. Lowest-friction.
- **(b) Change the fixture vessel geometry:** reduce `lateral_area_current` below `lateral_area_wind` (or change wind speed) so wind genuinely dominates and the original assertion holds. More accurately reflects the test's stated intent.

Issue body recommends (a) or (b); pick (b) if the test name explicitly intends "wind dominates" as the contract being tested.

### Task 3 — Fix
Edit `tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py`:
- Apply chosen option in the `test_combined_environmental_forces` body.
- Add a comment explaining the geometry's force balance.

### Task 4 — Verify
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py::TestOCIMFMooringIntegration::test_combined_environmental_forces -xvs
```

### Task 5 — Confirm no regressions
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py -x
```

## Acceptance Criteria
- The single test passes.
- No regression in sibling `test_ocimf_mooring_integration.py` tests (esp. #564 `test_environmental_forces_to_mooring_tension`, which shares the fixture vessel).
- The fix path (a vs b) is documented in the test body comment.

## Open questions
- Does fixing this vessel-geometry assumption auto-resolve #564? Likely yes — verify before treating #564 as independent.
