# Plan: digitalmodel #490 — Capping stack analysis (API 17W)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/490
**Status:** plan-review
**Tier:** T3 (new submodule)
**Priority:** low

## Context

Post-Macondo, capping stacks (API 17W) are mandatory for deepwater wells as the emergency blowout containment system. For field-development emergency response planning we need to model deployment scenarios, wellhead-interface compatibility, and containment pressure capacity vs reservoir conditions. No code exists today.

Adjacent: future tree module (#484) provides the wellhead interface this module checks against; future ROV module (#491) provides deployment heavy-lift modeling.

## Plan

1. **Create submodule skeleton.** New `src/digitalmodel/subsea/capping_stack/` with `__init__.py`, `deployment.py`, `interface_check.py`, `containment_capacity.py`, plus `fixtures/generic_15ksi_capping_stack.yml` (15,000 psi WP, 7-1/16" bore, 30 t in air).

2. **Deployment (`deployment.py`).** Class `DeploymentScenario` with fields: `vessel_type`, `crane_capacity_te`, `water_depth`, `hs_limit`, `target_well_id`. Method `feasibility()` returns `(feasible: bool, governing_constraint: str)`. Constraints: lift weight vs crane SWL, lower-line tension under current loading, sea-state limit. Reuse `hydrodynamics/` inputs for current.

3. **Interface check (`interface_check.py`).** `check_compatibility(capping_stack_spec, wellhead_spec)`: bore diameter match, pressure rating, connector class (e.g., H4 18-3/4", 15K). Return structured `Result(pass: bool, reasons: list[str])`. Hard-code the canonical wellhead/capping-stack connector classes from API 17D / 17W as an enum.

4. **Containment capacity (`containment_capacity.py`).** Compute the maximum reservoir flow the capping stack can choke off, given choke valve Cv, downstream containment line pressure, and reservoir IPR. Function `containment_envelope(stack, reservoir)` returns a pressure-vs-flow curve and an `is_contained: bool` for a given reservoir scenario.

5. **Tests and smoke.** `tests/subsea/capping_stack/` covering: fixture-deploy feasibility under benign and storm conditions, interface match (15K-15K) vs mismatch (10K-15K), envelope pass/fail vs a representative reservoir IPR. Smoke: `uv run pytest tests/subsea/capping_stack/ -x`. Add module-level docstring referencing API 17W with citation per `.claude/rules/calc-citation-contract.md`.

## Acceptance Criteria

- [ ] `src/digitalmodel/subsea/capping_stack/` submodule with 3 core modules + 1 fixture exists
- [ ] `DeploymentScenario.feasibility()` returns a deterministic verdict on the fixture under benign and storm seastates
- [ ] `check_compatibility()` returns structured pass/fail with at least 3 reason categories (bore, pressure, connector)
- [ ] `containment_envelope()` returns a pressure-flow curve and binary verdict; matches a hand-checked single-point case within 5%
- [ ] All tests pass; API 17W citation emitted

## Open questions

1. Source for canonical 17W connector dimensions and pressure ratings — public summary tables or licensed standard?
2. Does this plan need to integrate with #484 (trees) before merge, or is the wellhead interface modeled inline as a Pydantic model and reconciled later? Default: inline now, reconcile on #484 landing.
