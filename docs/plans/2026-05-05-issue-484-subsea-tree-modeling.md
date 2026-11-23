# Plan: digitalmodel #484 — Subsea tree modeling module (API 17D)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/484
**Status:** plan-review
**Tier:** T3 (new submodule)
**Priority:** medium

## Context

Subsea trees (API 17D) are the direct interface between reservoir and production flowline — every subsea field development starts here. They control flow paths via chokes and valves, regulate pressure, and provide condition monitoring. No tree-modeling code exists in digitalmodel today. This issue is upstream of #485 (manifolds) and #490 (capping stack interface). Existing flowline integration target: `src/digitalmodel/subsea/pipeline/`.

API 17D + API 6A govern wellhead and tree equipment. Common tree types: single-bore (vertical xmas tree), dual-bore (horizontal xmas tree), multiplex (HXT with multiplexed control). Choke valves are the dominant flow-regulation mechanism — sized via Cv at design conditions.

## Plan

1. **Create submodule skeleton.** New `src/digitalmodel/subsea/trees/` with `__init__.py`, `tree_catalog.py`, `pressure_control.py`, `flow_capacity.py`, `design_checks.py`, plus `fixtures/canonical_15ksi_xmas_tree.yml` (5-1/8" 15K vertical xmas tree, single-bore, 7-1/16" tubing hanger).

2. **Tree catalog (`tree_catalog.py`).** Pydantic models: `TreeType` enum (vertical_xmas, horizontal_xmas, multiplex), `TreeSpec` (working pressure, bore size, connector class, tubing-hanger profile, valve count). Static registry of canonical 5K/10K/15K/20K dimensions sourced from API 17D / 6A common-class tables.

3. **Pressure control (`pressure_control.py`).** Choke valve pressure-drop calc using Fisher Cv formula: `Q = Cv * sqrt(ΔP / SG)` for liquid, gas-corrected for compressible flow. Method `pressure_drop(flow_rate, choke_position)` returns ΔP across the choke for a given upstream condition.

4. **Flow capacity and design checks (`flow_capacity.py`, `design_checks.py`).** Flow capacity: maximum production rate at fully-open choke, given upstream and downstream pressures. Design checks per API 17D: working-pressure vs reservoir SI test pressure, temperature class, material class (CC, DD, EE per API 17D §4.2), connector class. Each check returns `Result(pass, reason, margin)`.

5. **Integration with flowline + tests.** Add an optional adapter `src/digitalmodel/subsea/trees/flowline_adapter.py` exposing a `TreeOutletState(pressure, temperature, flow, composition)` to feed `subsea/pipeline/pipeline_pressure.py`. Tests in `tests/subsea/trees/` covering catalog lookup, choke ΔP vs Fisher analytic, capacity envelope, and design checks (≥15 tests total per issue body). Smoke: `uv run pytest tests/subsea/trees/ -x`.

## Acceptance Criteria

- [ ] `src/digitalmodel/subsea/trees/` exists with 4 core modules + 1 fixture + 1 adapter
- [ ] Tree catalog lookup returns deterministic specs for 5K/10K/15K/20K vertical and horizontal xmas trees
- [ ] Choke ΔP matches Fisher Cv analytic on a known case within 1% for liquid and 3% for gas
- [ ] Design checks return PASS on the 15K fixture and FAIL when working pressure is reduced to 10K with reservoir at 12K
- [ ] At least 15 tests in `tests/subsea/trees/` and all pass via `uv run pytest tests/subsea/trees/ -x`
- [ ] API 17D citation emitted in module docstring

## Open questions

1. Source for canonical material-class temperature/CO2/H2S envelopes — public API summaries or licensed standard? Document choice with citation.
2. Multiplex tree control hardware — is that scoped to #488 (control systems) and out of scope here, with this module exposing only the tree mechanical interfaces? Default: yes, defer multiplex to #488.
