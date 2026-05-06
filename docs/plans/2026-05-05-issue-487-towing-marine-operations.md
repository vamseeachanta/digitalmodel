# Plan: digitalmodel #487 — Towing and marine operations module

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/487
**Status:** plan-review
**Tier:** T3 (new submodule)
**Priority:** low

## Context

Towing/towline analysis has no native Python solver in digitalmodel. Existing `src/digitalmodel/marine_ops/installation/jumper_lift.py` shows the established marine-ops structure (Pydantic configs, scenario classes, OrcaFlex bridge). New `marine_ops/towing/` will follow the same pattern. Sibling dependency: #493 (river current profiles) provides the environmental input to drift-force calculations.

DNV-ST-N001 (Marine operations, general) governs design checks; specific compliance gates (SF 3.0 dynamic, 1.67 static for towlines) flow from §11.

## Plan

1. **Create submodule skeleton.** New `src/digitalmodel/marine_ops/towing/` with `__init__.py`, `towline_analysis.py`, `bollard_pull.py`, `drift_force.py`, `config_optimizer.py`, `dnv_st_n001_checks.py`, plus `fixtures/barge_tug_500t_pull.yml` (representative 80 m × 24 m barge + 80 t bollard-pull tug + 600 m wire towline).

2. **Static towline catenary (`towline_analysis.py`).** Reuse `subsea/catenary_riser/legacy/catenary_equation.py` if its API is clean; otherwise reimplement specifically for towlines (no submerged weight nuance — towline is in air for tugs and surface). Output: top tension, sag, scope, line-shape coordinates. Validate against an analytic 2-point catenary case.

3. **Bollard pull and drift force (`bollard_pull.py`, `drift_force.py`).** `BollardPullCheck(tug_capacity, env_loads).check()` returns required vs available pull and a margin. `drift_force.compute(vessel, current_profile, wind_profile)`: surface drag from current (consume #493 `RiverCurrentProfile` if available, else accept a uniform current), wind drag using OCIMF coefficients (reuse `hydrodynamics/ocimf_loading.py`).

4. **Config optimizer and DNV checks (`config_optimizer.py`, `dnv_st_n001_checks.py`).** Optimizer: brute-force grid over `(bridle_angle, line_length, line_diameter)` to minimize towline tension subject to clearance and scope constraints. DNV checks: SF on towline MBL, fairlead reaction, bridle angle bounds. Each check returns `Result(pass, reason, margin)`.

5. **Tests + worked example.** `tests/marine_ops/towing/` covering each module. Worked-example fixture-driven script under `examples/marine_ops/towing/towing_scenario.py` that runs end-to-end and prints DNV verdict. Smoke: `uv run pytest tests/marine_ops/towing/ -x` and `uv run python examples/marine_ops/towing/towing_scenario.py`.

## Acceptance Criteria

- [ ] `src/digitalmodel/marine_ops/towing/` submodule with 5 core modules + 1 fixture exists
- [ ] Static catenary solver matches analytic 2-point case within 0.5%
- [ ] `BollardPullCheck` produces a clear verdict and margin on the fixture under benign and storm environmental sets
- [ ] `compute_drift_force` matches an OCIMF tabulated case within 5%
- [ ] DNV-ST-N001 SF checks return PASS on the fixture and FAIL when the towline is downsized 20%
- [ ] All tests pass; DNV-ST-N001 citation emitted

## Open questions

1. Is current_profile expected to be the `RiverCurrentProfile` from #493, or a generic `metocean.CurrentProfile`? If #493 lands later, accept a `Callable[[float], float]` as the env interface and adapt later — note this in module docstring.
2. Does `subsea/catenary_riser/legacy/catenary_equation.py` have a stable API that can be reused, or is it considered legacy-only? Default: reimplement to keep towing self-contained.
