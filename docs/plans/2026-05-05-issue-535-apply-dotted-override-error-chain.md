# Plan: digitalmodel #535 — apply_dotted_override should chain ValidationError with dotted-path context

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/535
**Status:** plan-review
**Tier:** T3

## Context

`apply_dotted_override(spec, dotted, value)` at `src/digitalmodel/solvers/orcaflex/modular_generator/schema/_overrides.py` round-trips through `model_dump()` → `_set_nested_safe()` → `ProjectInputSpec.model_validate(d)`. When `model_validate` raises a Pydantic `ValidationError` (e.g., a sweep injects `"not-a-float"` into a float field), the error message lists the field path inside `ProjectInputSpec` but never names the **sweep dotted path** that triggered the failure.

For multi-sweep campaigns (N sweeps × M values), users currently get a Pydantic stack trace with no hint of which sweep parameter is bad. Chaining the error fixes that without losing diagnostic detail.

Code-review follow-up from #511 / PR #533, defect ID **m3**, marked DEFERRED in commit `1d96aa63`. Sibling defects: #534, #536, #537.

## Plan

1. **Confirm current contract.** Read `src/digitalmodel/solvers/orcaflex/modular_generator/schema/_overrides.py` (53 lines, small file) — locate `apply_dotted_override`. Run `git grep -n "apply_dotted_override\\|test_apply_dotted_override_pydantic_validates" src/ tests/` to enumerate callers and existing tests, including `test_apply_dotted_override_pydantic_validates_type_error` named in the issue.

2. **Wrap `model_validate` with chained `ValueError`.** Replace the body:
   ```python
   try:
       return ProjectInputSpec.model_validate(d)
   except ValidationError as exc:
       raise ValueError(
           f"Sweep parameter {dotted!r} produced invalid spec for value {value!r}"
       ) from exc
   ```
   The `from exc` chain preserves `__cause__` so the original Pydantic error remains accessible for callers that need field-level detail. Add the `ValidationError` import if not already present.

3. **Update `test_apply_dotted_override_pydantic_validates_type_error`.** Change assertion to:
   - assert raised exception type is `ValueError` (top-level)
   - assert `dotted` path appears in `str(exc.value)`
   - assert `value` repr appears in `str(exc.value)`
   - assert `exc.value.__cause__` is a Pydantic `ValidationError`
   This locks both the new top-level message AND the preservation of the original chain.

4. **Add a multi-sweep regression test** (new) in the same file: build a 2-sweep campaign where one sweep value fails Pydantic validation, call `apply_dotted_override` directly, and assert the error message names the failing sweep's dotted path (not the other sweep's). Catches a future bug where the wrong dotted path is reported.

5. **Smoke check.** `uv run pytest tests/solvers/orcaflex/modular_generator/ -q -k "dotted or override"` then full `tests/solvers/orcaflex/modular_generator/` run.

## Acceptance Criteria

- [ ] Top-level error type is `ValueError` (not `ValidationError`) when validation fails
- [ ] Top-level message contains both the dotted path and the offending value
- [ ] `exc.__cause__` is the original Pydantic `ValidationError` — full field-level detail preserved
- [ ] Updated `test_apply_dotted_override_pydantic_validates_type_error` passes with all four assertions above
- [ ] New multi-sweep regression test passes
- [ ] `_overrides.py` non-error path is unchanged (no perf hit on success path)
