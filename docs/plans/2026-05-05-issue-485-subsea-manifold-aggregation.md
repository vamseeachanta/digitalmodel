# Plan: digitalmodel #485 — Subsea manifold aggregation module (API 17P)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/485
**Status:** plan-review
**Tier:** T3 (new submodule)
**Priority:** medium

## Context

Subsea manifolds (API 17P) are the hub that gathers production from multiple trees and balances flow upstream of the flowline/riser. Multi-well field development needs flow aggregation, pressure balancing, chemical-injection routing, and design-envelope verification. Direct upstream integration: future tree module (#484); direct downstream: existing `src/digitalmodel/subsea/pipeline/`.

Pattern to follow: existing `src/digitalmodel/subsea/cross_sections/` — schema-first, fixtures, validation, reporting. Reuse `subsea/pipeline/pipeline_pressure.py` for the manifold→flowline pressure handoff.

## Plan

1. **Create submodule skeleton.** New `src/digitalmodel/subsea/manifolds/` with `__init__.py`, `manifold_catalog.py`, `flow_aggregator.py`, `pressure_balance.py`, `design_checks.py`, `chemical_injection.py`, plus a fixture `fixtures/four_slot_production_manifold.yml` (4 tree slots, 1 chemical-injection manifold, design pressure 10K psi, 36" header).

2. **Catalog and aggregator (`manifold_catalog.py`, `flow_aggregator.py`).** `ManifoldType` enum (standard, template, multi-slot, CIM). Pydantic `ManifoldSpec` keyed on type with shared and type-specific fields. `aggregate_flow(slot_flows)` sums per-slot mass-flow and returns total + per-component composition (assume per-slot composition + flow rate as inputs).

3. **Pressure balance (`pressure_balance.py`).** Solve the well-to-well pressure balance: each slot has an inlet pressure from the tree → manifold header pressure ↑ → flowline pressure ↓. Use a linear-system solver with choke valves as the balancing variable. Return per-slot choke setting that yields target header pressure within tolerance.

4. **Design checks and chemical injection (`design_checks.py`, `chemical_injection.py`).** Design checks: header pressure vs MAOP, slot-spacing vs ROV access, structural reactions per API 17P §5. Chemical-injection: per-tree dosage rates, residence time in the CIM, mixing-zone pressure-drop. Each check returns `Result(pass, reason, margin)`.

5. **Tree → manifold → flowline integration smoke.** Hand-coded scenario in `examples/subsea/manifolds/multi_well_field.py`: 4 trees → manifold → 8 km 12" flowline. Run the chain: tree pressures → manifold balance → flowline pressure drop. Tests in `tests/subsea/manifolds/` covering each module + integration. Smoke: `uv run pytest tests/subsea/manifolds/ -x`.

## Acceptance Criteria

- [ ] `src/digitalmodel/subsea/manifolds/` exists with 5 core modules + 1 fixture
- [ ] `aggregate_flow()` mass-balances on a 4-slot fixture (sum of slot flows = header flow within 1e-9 te/s)
- [ ] `pressure_balance()` converges to within 0.1 bar tolerance on a 4-slot case with mixed reservoir pressures
- [ ] Design checks return PASS on fixture and FAIL when MAOP is reduced 20%
- [ ] Manifold → pipeline integration runs end-to-end on the example script
- [ ] All tests pass; API 17P citation emitted

## Open questions

1. Should the manifold module gate-check the tree (#484) interface dimensions before #484 lands, or assume any compatible Pydantic model? Default: define an internal `ManifoldTreeInterface` Pydantic schema now, reconcile with #484 on its merge.
2. CIM (Chemical Injection Manifold) — separate fixture or sub-section under the main manifold spec? Default: sub-section, single canonical YAML.
