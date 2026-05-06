# Plan: digitalmodel #555 — `_generate_chain_database` `diameters[:20]` excludes 76mm

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/555
**Status:** plan-review
**Tier:** T1 (single off-by-one slice + count-assertion update, mirrored in two files)

## Root cause (verified at HEAD `08e9c33`)
- `tests/marine_ops/marine_engineering/legacy/test_component_database.py:236`: `for diameter in diameters[:20]:` truncates at 64mm.
- The `diameters` array (lines 228–231): `[16, 19, 22, 24, 28, 32, 34, 36, 38, 40, 44, 46, 48, 50, 52, 56, 58, 60, 62, 64, 66, 68, 70, 76, ...]` — 76mm sits at index 23.
- Four tests query `76mm R4 Stud Link` and fail with `ValueError: Chain not found`:
  - `test_get_chain_by_specification`
  - `test_find_chain_with_safety_factor`
  - `test_database_performance`
  - `test_weight_scales_with_diameter_squared`
- A second copy of the same bug exists at `tests/marine_ops/marine_engineering/test_component_database.py:236` (non-legacy path).

## Plan
### Task 1 — Reproduce
```
uv run pytest tests/marine_ops/marine_engineering/legacy/test_component_database.py::TestComponentDatabase::test_get_chain_by_specification -xvs
```

### Task 2 — Decide intent (sub-decision flagged in issue)
Three options; pick before editing:
1. **"First 20 standard sizes"** — array order is wrong (76mm should be in first 20). Reorder array, keep `[:20]` slice.
2. **"Up to and including 76mm"** — extend slice to `[:24]`. Generates 24 × 3 grades = 72 components; update count assertion from 60 to 72.
3. **"All standard sizes"** — drop slice. Generates 49 × 3 = 147 components; update count assertion to 147.

Issue body recommends intent (2) or (3) since the test queries require 76mm.

### Task 3 — Fix
Edit both files in lockstep (legacy + non-legacy paths must stay consistent):
- `tests/marine_ops/marine_engineering/legacy/test_component_database.py:236` — apply chosen slice.
- `tests/marine_ops/marine_engineering/test_component_database.py:236` — same edit.
- Update the `Loaded N chain components` print on line 219 (both files).
- Update any `len(chain_db) == 60` (or similar) assertion to the new component count.

### Task 4 — Verify
```
uv run pytest tests/marine_ops/marine_engineering/legacy/test_component_database.py tests/marine_ops/marine_engineering/test_component_database.py -xvs
```

### Task 5 — Confirm no regressions
```
uv run pytest tests/marine_ops/marine_engineering/ -x -k "component or chain"
```

## Acceptance Criteria
- The four named tests pass in both legacy and non-legacy paths.
- "Loaded N chain components" print and `len(...)`-style assertions match the new count.
- No regression in other `test_component_database.py` tests.

## Open questions
- Confirm intent (1)/(2)/(3) before editing — the issue body flags this as a sub-decision; pick the one that minimizes downstream test churn.
