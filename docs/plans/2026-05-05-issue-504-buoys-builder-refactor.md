# Plan: digitalmodel #504 — Buoys builder refactor: split into focused builders

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/504
**Status:** plan-review (carrying `status:working`)
**Tier:** T3

## Context

The issue body states `buoys_builder.py` is 611 lines handling 4 responsibilities. Current state on `main` shows the refactor is **partially complete**: `buoys_builder.py` is now 82 lines, `buoyancy_builder.py` (82 lines) and `end_buoy_builder.py` (100 lines) already exist as siblings. Missing: `tug_builder.py` and `roller_builder.py`. The `buoys_builder.py` shell still contains tug + roller logic and needs its own decomposition before it can be deleted.

Reference: `src/digitalmodel/solvers/orcaflex/modular_generator/builders/{base.py,registry.py,context.py}` define the `BaseBuilder` contract (`should_generate()`, `build()`) and the `BuilderRegistry` ordering. New builders must register at the same order slot the old monolith occupied to preserve YAML-output ordering.

## Plan

1. **Audit current `buoys_builder.py`.** Read all 82 lines. Identify which sections still cover tug 6D-buoy generation vs roller arrangements. Capture the spec-key inputs each one reads (likely `tugs:` and `rollers:` under the spec root). Confirm `should_generate()` currently returns `True` if either tugs or rollers is present.

2. **Extract `tug_builder.py`.** New file `src/digitalmodel/solvers/orcaflex/modular_generator/builders/tug_builder.py`. Subclass `BaseBuilder`. `should_generate()` returns `True` only when `self.context.spec.tugs` is non-empty. `build()` returns the `{"6DBuoys": [...]}` dict for tugs only. Mirror the structure of `end_buoy_builder.py` for stylistic consistency.

3. **Extract `roller_builder.py`.** Same pattern: new `roller_builder.py`, `should_generate()` keyed on `self.context.spec.rollers`, `build()` returns the roller arrangement dict.

4. **Register and demote `buoys_builder`.** Edit `src/digitalmodel/solvers/orcaflex/modular_generator/builders/registry.py` to register `TugBuilder` and `RollerBuilder` at the same priority that `BuoysBuilder` currently occupies (preserve emit order). Replace `BuoysBuilder` with a thin deprecation shim that re-exports the four split builders, OR delete it if no external callers reference it. Confirm via `git grep "BuoysBuilder\b" --` before deleting.

5. **Test against modular-vs-monolithic regression.** Run `uv run pytest tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py -xvs`. This 1,193-line test is the canonical fidelity gate. Output YAML from any spec touching tugs or rollers must remain byte-equivalent. If diff appears, narrow with `--collect-only -k tug` then `-k roller`.

## Acceptance Criteria

- [ ] `tug_builder.py` and `roller_builder.py` exist; each <150 lines; each has its own `should_generate()` keyed on a single spec section
- [ ] `BuilderRegistry` registers the two new builders at the same emit priority as the old `BuoysBuilder`; YAML emit order unchanged
- [ ] `buoys_builder.py` is either deleted (no callers) or reduced to a deprecation shim re-exporting the four focused builders
- [ ] `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` passes
- [ ] `git grep "buoys_builder\|BuoysBuilder" -- '*.py'` shows no live callers outside the deprecation shim

## Open questions

1. Are tugs and rollers currently keyed on separate top-level spec sections, or sub-keys under a `marine_ops:` block? Confirm by grepping fixture specs before drafting `should_generate()`.
