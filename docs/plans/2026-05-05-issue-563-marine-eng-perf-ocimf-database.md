# Plan: digitalmodel #563 — `test_ocimf_database_performance` needs investigation

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/563
**Status:** plan-review
**Tier:** T1 (likely auto-resolves with #562; investigation-first)

## Root cause
- Test: `tests/marine_ops/marine_engineering/integration/test_marine_eng_performance.py::TestPerformanceBenchmarks::test_ocimf_database_performance`
- Triage logged FAILED status only, no traceback. Hypothesis: shares the JSON-serialization defect (numpy.bool_) with #562 since both tests live in the same `TestPerformanceBenchmarks` class and likely use the same summary-writer helper.

## Plan
### Task 1 — Reproduce and capture traceback
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_marine_eng_performance.py::TestPerformanceBenchmarks::test_ocimf_database_performance -xvs 2>&1 | tee /tmp/issue-563-repro.log
```

### Task 2 — Determine if duplicate of #562
After capturing the traceback:
- If error is `TypeError: Object of type bool_ is not JSON serializable` (or sibling numpy-coercion error): close #563 as duplicate of #562 — the #562 fix will auto-resolve it.
- If a distinct defect (e.g., timeout, assertion mismatch on a perf metric): file fix targeted at that root cause.

### Task 3 — Fix (depends on Task 2)
- If duplicate of #562: no separate fix; verify after #562 lands.
- If novel: targeted edit at the failing assertion or source helper.

### Task 4 — Verify
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_marine_eng_performance.py::TestPerformanceBenchmarks::test_ocimf_database_performance -xvs
```

### Task 5 — Confirm no regressions
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_marine_eng_performance.py -x
```

## Acceptance Criteria
- The single test passes.
- No regression in sibling `test_marine_eng_performance.py` tests.
- PR body documents whether root cause was duplicate of #562 or novel.

## Open questions
- Recommended order: implement #562 first → re-run #563 → if green, close #563 as duplicate. Only fix #563 directly if still failing post-#562.
