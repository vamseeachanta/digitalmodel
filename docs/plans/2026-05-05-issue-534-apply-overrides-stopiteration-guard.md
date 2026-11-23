# Plan: digitalmodel #534 — _apply_overrides direct-call StopIteration guard

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/534
**Status:** plan-review
**Tier:** T3

## Context

`_apply_overrides(spec, combo, matrix)` at `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py:419-431` uses bare `next(...)` to look up the matching environment/soil variation. When called via the normal `combinations()` emission path the lookup is always satisfied, but the function is also a direct test surface (`TestApplyOverridesWithSweeps`) and a module-level helper that hand-built combos can pass invalid names to. In that case the bare generator raises `StopIteration` instead of a helpful `ValueError`.

Code-review follow-up from #511 / PR #533, defect ID **m2**, marked DEFERRED in commit `1d96aa63`. Sibling defects: #535 (apply_dotted_override error chaining, m3), #536 (per-iteration validate perf, m5), #537 (manifest empty-runs, m7).

## Plan

1. **Confirm call sites.** Read `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py:415-445` to verify the two `next(...)` lookups (env at L421-ish, soil at L431-ish). Run `git grep -n '_apply_overrides\\|next(e for e\\|next(s for s' src/ tests/` to enumerate direct callers and confirm the test-surface claim.

2. **Replace both bare `next()` calls with default + raise.** For each branch:
   ```python
   env_var = next((e for e in matrix.environments if e.name == env_name), None)
   if env_var is None:
       raise ValueError(f"environment {env_name!r} not in matrix.environments")
   ```
   Apply the same pattern to the `soil` branch with parallel error message. Preserve all surrounding logic.

3. **Add unit tests in `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py`.** Two new tests:
   - `test_apply_overrides_unknown_environment_raises_valueerror` — hand-build a combo with `environment="not-in-matrix"` and assert `ValueError` (not `StopIteration`).
   - `test_apply_overrides_unknown_soil_raises_valueerror` — same shape for the soil branch.
   Confirm error messages include the offending name (use `match=` kwarg in `pytest.raises`).

4. **Smoke check.** `uv run pytest tests/solvers/orcaflex/modular_generator/schema/test_campaign.py -v` then a broader `uv run pytest tests/solvers/orcaflex/modular_generator/ -q` to confirm no regression in sibling test files. The combinations-emission path should be untouched (default path always finds a match).

## Acceptance Criteria

- [ ] `_apply_overrides` raises `ValueError` (not `StopIteration`) for unknown env name
- [ ] `_apply_overrides` raises `ValueError` (not `StopIteration`) for unknown soil name
- [ ] Both error messages include the offending name (assertable via `pytest.raises(match=...)`)
- [ ] Two new unit tests pass; pre-existing `TestApplyOverridesWithSweeps` tests continue to pass
- [ ] No changes to function signature or normal-path return value
