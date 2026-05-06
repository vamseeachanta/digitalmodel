# Plan: digitalmodel #562 — `test_complete_workflow_performance` numpy.bool_ JSON serialization

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/562
**Status:** plan-review
**Tier:** T1 (single targeted source edit)

## Root cause
- Test: `tests/marine_ops/marine_engineering/integration/test_marine_eng_performance.py::TestPerformanceBenchmarks::test_complete_workflow_performance`
- Failure: `TypeError: Object of type bool_ is not JSON serializable` — numpy bool in summary dict fails `json.dump`.
- Source emits a `dict` containing `numpy.bool_` value(s). Python's stdlib JSON encoder accepts only native `bool`.
- Likely shares the source-side performance-benchmark summary writer with #563 (`test_ocimf_database_performance`).

## Plan
### Task 1 — Reproduce
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_marine_eng_performance.py::TestPerformanceBenchmarks::test_complete_workflow_performance -xvs
```

### Task 2 — Identify the writer
Locate the summary-writer site under `src/digitalmodel/marine_ops/` (likely a benchmarking helper that does `json.dump(summary, fh)`). Trace how `numpy.bool_` enters the dict — typically from a comparison like `np.array(...) > threshold`, which yields `np.bool_`.

### Task 3 — Fix
Pick one (preferred order):
- **(a) Coerce at write site:** `json.dump(summary, fh, default=lambda o: bool(o) if isinstance(o, np.bool_) else (int(o) if isinstance(o, np.integer) else (float(o) if isinstance(o, np.floating) else str(o))))`. Most robust — also covers numpy ints/floats.
- **(b) Coerce at insertion:** wrap `bool(...)` around the offending value(s) before insertion into the dict.

Option (a) is preferred since it generalizes — also pre-empts a future `np.int64`/`np.float64` recurrence.

### Task 4 — Verify
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_marine_eng_performance.py::TestPerformanceBenchmarks -xvs
```
This should also clear #563 if it shares the same writer.

### Task 5 — Confirm no regressions
```
uv run pytest tests/marine_ops/marine_engineering/integration/test_marine_eng_performance.py -x
```

## Acceptance Criteria
- `test_complete_workflow_performance` passes.
- The summary-writer's JSON output contains a native `bool` (or properly-coerced numeric) for the previously-failing field.
- No regression in sibling `test_marine_eng_performance.py` tests.

## Open questions
- After this fix, re-run #563 — likely auto-resolves. If yes, close #563 as duplicate.
