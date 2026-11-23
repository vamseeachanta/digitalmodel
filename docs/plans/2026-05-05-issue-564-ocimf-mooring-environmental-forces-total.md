# Plan: digitalmodel #564 — `test_environmental_forces_to_mooring_tension` needs investigation

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/564
**Status:** plan-review
**Tier:** T1 (likely auto-resolves with #561; investigation-first)

## Root cause
- Test: `tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py::TestOCIMFMooringIntegration::test_environmental_forces_to_mooring_tension`
- Triage logged FAILED status only, no traceback. Hypothesis: shares the same vessel-geometry / force-magnitude assumption issue with #561, since both tests use the same fixture vessel.

## Plan
### Task 1 — Reproduce and capture traceback
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py::TestOCIMFMooringIntegration::test_environmental_forces_to_mooring_tension -xvs 2>&1 | tee /tmp/issue-564-repro.log
```

### Task 2 — Determine if duplicate of #561
After capturing the traceback:
- If failure is on the same assertion class (wind > current OR a downstream cascade from those forces): close as duplicate of #561 once #561 lands.
- If a distinct defect at the mooring-tension computation step (`forces → mooring tensions`): file fresh, targeted fix.

### Task 3 — Fix (depends on Task 2)
- If duplicate of #561: no separate fix; verify after #561 lands.
- If novel: targeted edit at the mooring-tension coupling step in `src/digitalmodel/marine_ops/...`, or at the test assertion bound.

### Task 4 — Verify
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py::TestOCIMFMooringIntegration::test_environmental_forces_to_mooring_tension -xvs
```

### Task 5 — Confirm no regressions
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py -x
```

## Acceptance Criteria
- The single test passes.
- No regression in sibling `test_ocimf_mooring_integration.py` tests.
- PR body documents whether root cause was duplicate of #561 or novel.

## Open questions
- Recommended order: implement #561 first → re-run #564 → if green, close as duplicate. Only fix #564 directly if still failing post-#561.
