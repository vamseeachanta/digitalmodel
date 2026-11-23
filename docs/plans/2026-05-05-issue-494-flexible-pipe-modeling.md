# Plan: digitalmodel #494 — Flexible pipe modeling: API 17B/17J compliance

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/494
**Status:** plan-review
**Tier:** T3 (new submodule, multi-file)
**Priority:** low (`priority:low`)

## Context

Subsea production systems use unbonded flexible pipes (API 17B operational practice, API 17J unbonded-pipe specification) for dynamic riser and flowline applications. Current `src/digitalmodel/subsea/catenary_riser/` provides catenary and lazy-wave geometry (`simple_catenary.py`, `lazy_wave.py`, `effective_weight.py`) but treats the pipe as a homogeneous beam with no layered cross-section. Subsea cross-section infrastructure already exists for cables/umbilicals at `src/digitalmodel/subsea/cross_sections/` (schema, validation, visualization, fixtures) — flexible-pipe layers are a natural extension of that pattern, not a greenfield module.

A new sibling submodule `src/digitalmodel/subsea/flexible_pipes/` will host layer modeling, API 17B fatigue, bend stiffener sizing, and API 17J design checks. Integration with `catenary_riser/simple_catenary.py` happens via a thin adapter that exposes the layered effective bending stiffness back to the catenary solver.

## Plan

1. **Create submodule skeleton.** New directory `src/digitalmodel/subsea/flexible_pipes/` with `__init__.py`, `cli.py`, `cross_section.py`, `bend_stiffener.py`, `fatigue_17B.py`, `design_checks_17J.py`. Mirror the layout of `subsea/cross_sections/` (schema-first, then validation, then reporting). Add a fixture directory `fixtures/` with one canonical 6"-ID flexible riser cross-section YAML (carcass + pressure sheath + 2 tensile armor pairs + outer sheath).

2. **Layer cross-section model (`cross_section.py`).** Pydantic schema for layers in canonical order: carcass, pressure sheath, zeta layer, pressure armor, anti-wear tape, tensile armor pair (cross-wound), insulation, outer sheath. Compute equivalent EA, EI, mass/length, and crush capacity by summing per-layer contributions. Reference: API 17B §5 (cross-section design) — add citation per `.claude/rules/calc-citation-contract.md`.

3. **API 17B fatigue (`fatigue_17B.py`).** Rainflow-count tensile-armor stress histories (use existing `signal_processing/signal_analysis/fatigue/`) and apply a tensile-armor S-N curve. Output cumulative damage and predicted fatigue life. Hand-off interface: function takes a `pd.DataFrame` of (time, top-tension, curvature) and returns `{damage, life_years, governing_layer}`.

4. **Bend stiffener sizing (`bend_stiffener.py`) and 17J design checks (`design_checks_17J.py`).** Bend stiffener: Boef-style stiffness taper to limit curvature at top connection, given vessel motions and end-of-life mean tension. Design checks: minimum bend radius (storage and dynamic), tensile capacity, burst, collapse — fail-fast `Result` dataclass with pass/fail per check.

5. **Integration adapter and tests.** New `src/digitalmodel/subsea/catenary_riser/flexible_pipe_adapter.py`: wraps a `FlexiblePipeCrossSection` and yields the EI/EA/weight per length that `simple_catenary` expects. Add `tests/subsea/flexible_pipes/test_cross_section.py`, `test_fatigue_17B.py`, `test_bend_stiffener.py`, `test_design_checks_17J.py` — at least one happy-path and one boundary case each. Smoke: `uv run pytest tests/subsea/flexible_pipes/ -x`.

## Acceptance Criteria

- [ ] `src/digitalmodel/subsea/flexible_pipes/` exists with the four core modules listed and a fixtures directory
- [ ] `FlexiblePipeCrossSection` computes EA, EI, mass/length deterministically from a layer YAML and matches a hand-calculated reference within 1%
- [ ] `compute_17B_fatigue_damage()` returns Miner's-rule damage on a synthetic stress history and matches an analytic check within 5%
- [ ] `flexible_pipe_adapter.py` round-trips a flexible cross-section through `simple_catenary.solve` with no API changes to the catenary solver
- [ ] All new tests pass via `uv run pytest tests/subsea/flexible_pipes/ -x`
- [ ] At least one citation emitted per `.claude/rules/calc-citation-contract.md` (API 17B or 17J)

## Open questions

1. Is there an existing in-repo S-N curve appropriate for tensile-armor wires (likely DNV-RP-C203 Class C) that we should adopt, or do we need to vendor a new SN curve into `signal_processing/signal_analysis/fatigue/curves.py`?
2. Confirm `priority:low` allows scoping out worked-example documentation to a follow-up issue.
