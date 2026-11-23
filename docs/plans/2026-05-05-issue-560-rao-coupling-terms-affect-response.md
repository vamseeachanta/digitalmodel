# Plan: digitalmodel #560 — `test_coupling_terms_affect_response` needs investigation

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/560
**Status:** plan-review
**Tier:** T2 (investigation-first; size depends on root cause discovered)

## Root cause
- Test: `tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py::TestHydroRAOIntegration::test_coupling_terms_affect_response`
- Triage logged FAILED status only, no traceback captured. Root cause unknown until live repro.
- Hypothesis (from issue body): may share the RAO-integration cluster root cause with #558 (phase sign convention) and #559 (strict-greater on equal floats), since all three live in the same test class against the same coupling-matrix path.

## Plan
### Task 1 — Reproduce and capture traceback
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py::TestHydroRAOIntegration::test_coupling_terms_affect_response -xvs 2>&1 | tee /tmp/issue-560-repro.log
```

### Task 2 — Classify root cause
After capturing the traceback:
- If assertion failure on phase / sign: likely shares root cause with #558 — close #560 as duplicate after #558 lands (verify first).
- If assertion failure on strict comparison of equal floats: likely shares root cause with #559 — same.
- If a distinct coupling-matrix indexing or computation defect: file as a fresh source-side or test-side fix.

### Task 3 — Fix (depends on Task 2)
- If duplicate of #558 or #559: do nothing in this plan; mark issue as duplicate, close after parent fix lands and verify test passes.
- If novel: targeted edit at the failing assertion (test side) or the coupling-application path in `src/digitalmodel/marine_ops/marine_analysis/` (source side), depending on which is wrong.

### Task 4 — Verify
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py::TestHydroRAOIntegration::test_coupling_terms_affect_response -xvs
```

### Task 5 — Confirm no regressions
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py -x
```

## Acceptance Criteria
- The single test passes.
- No regression in sibling `test_hydro_rao_integration.py` tests.
- Root cause documented in PR body (novel defect OR duplicate of #558/#559 with verification).

## Open questions
- Run #558 and #559 fixes first — this test may auto-resolve. Implement order: #558 → #559 → re-run #560 → only fix #560 directly if still failing.
