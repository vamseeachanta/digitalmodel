# Plan: digitalmodel #565 — `test_csv_to_visualization_workflow` needs investigation

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/565
**Status:** plan-review
**Tier:** T2 (investigation-first; integration test spans CSV→DB→plot)

## Root cause
- Test: `tests/marine_ops/marine_engineering/test_hydro_coefficients.py:444 TestIntegration::test_csv_to_visualization_workflow`
- Triage logged FAILED status only, no traceback. End-to-end integration test (`csv → CoefficientDatabase → plot`); failure surface includes the CSV parser, the database API, and the matplotlib plotter.
- Possible overlap with sibling Kramers-Kronig defect (numpy.bool_ pattern) addressed in #566 R7 if the workflow inspects validator output.

## Plan
### Task 1 — Reproduce and capture traceback
```
uv run pytest tests/marine_ops/marine_engineering/test_hydro_coefficients.py::TestIntegration::test_csv_to_visualization_workflow -xvs 2>&1 | tee /tmp/issue-565-repro.log
```

### Task 2 — Localize failure stage
From the traceback, identify which stage failed:
- **CSV parse stage:** check fixture CSV content vs parser expectations (column headers, types).
- **Database insert stage:** check `CoefficientDatabase` API surface for required fields.
- **Plot rendering stage:** check matplotlib backend (set `MPLBACKEND=Agg` if a display is required), and any output-file path expectations.

### Task 3 — Fix (depends on Task 2)
Targeted edit at the failing stage:
- Source-side if the integration path has a real defect (e.g., `np.bool_` leaking into a check, missing matplotlib non-interactive backend setup, schema drift in the CSV loader).
- Test-side if the fixture is stale or the expected output has changed.

### Task 4 — Verify
```
uv run pytest tests/marine_ops/marine_engineering/test_hydro_coefficients.py::TestIntegration::test_csv_to_visualization_workflow -xvs
```

### Task 5 — Confirm no regressions
```
uv run pytest tests/marine_ops/marine_engineering/test_hydro_coefficients.py -x
```

## Acceptance Criteria
- The single test passes.
- No regression in sibling `test_hydro_coefficients.py` tests, including the Kramers-Kronig validator suite (covered by #566 R7).
- PR body identifies the failure stage (parse/insert/plot) and the chosen fix.

## Open questions
- Run #566 R7 (numpy.bool_ in Kramers-Kronig) first if the traceback shows it on the workflow path — may auto-resolve.
