# Plan: digitalmodel #582 — API RP 2I In-service Inspection of Marine Structures

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/582
**Status:** plan-review
**Tier:** T2 (data-model + workflow surface; minimal numeric calculation)

## Context
- **Standard / publisher / revision.** API RP 2I, 3rd Edition (March 2008, Reaffirmed 2014). Title: *In-service Inspection of Mooring Hardware for Floating Structures*. (Note: the issue title says "Marine Structures" but the actual API RP 2I scope is **mooring hardware** specifically; flag this to the user.)
- **Why it matters.** No in-service inspection module exists in `src/digitalmodel/marine_ops/` or `src/digitalmodel/asset_integrity/` — `grep -ril "API RP 2I\|in.service" src/digitalmodel/` returns zero hits. RP 2I sets inspection intervals, technique selection (visual / NDT / dimensional), and acceptance criteria for chain, wire, connectors, fairleads, stoppers, and turret bearings on FPSOs and other floating structures. Pairs naturally with `src/digitalmodel/orcaflex/mooring_design.py` (design side) and `src/digitalmodel/structural/offshore_resilience/structural_health.py` (SHM side).
- **Dependencies on other issues.** Soft alignment with #574 family (DNV-ST-0126 / API RP 2SIM SHM citations are adjacent). Needs a wiki resolver page for `api-rp-2i.md` (workspace-hub) — small addition, in scope.

## Scope
- New module `src/digitalmodel/asset_integrity/inservice_inspection/api_rp_2i.py`:
  - `MooringHardwareType` enum: `CHAIN`, `WIRE_ROPE`, `SYNTHETIC_ROPE`, `CONNECTOR_HLINK`, `CONNECTOR_DLOCK`, `FAIRLEAD`, `CHAIN_STOPPER`, `TURRET_BEARING`.
  - `InspectionTechnique` enum: `GVI` (general visual), `CVI` (close visual), `MPI` (magnetic particle), `UT` (ultrasonic), `DIMENSIONAL`, `LOAD_TEST`.
  - `InspectionInterval` Pydantic model: `interval_months_min`, `interval_months_routine`, `risk_factor_uplift`. Populated from RP 2I Table 4-1 (chain), Table 4-2 (wire), Table 4-3 (connectors), Table 4-4 (fairleads).
  - `select_inspection_techniques(hardware_type, age_years, last_findings) -> list[InspectionTechnique]` — implements §4 decision tree.
  - `compute_next_inspection_due(component, history) -> date` — §3.4 risk-based interval logic.
  - `evaluate_findings(component, findings) -> InspectionVerdict` — §5 acceptance criteria; outputs `ACCEPT`, `MONITOR`, `REPAIR`, `REPLACE`.
  - `wear_allowance_check(diameter_measured_mm, diameter_nominal_mm, hardware_type) -> WearVerdict` — §5.2.2 wear limits (e.g., 12% chain link diameter loss → reject).
  - Citations to RP 2I (§3, §4, §5) emitted via `digitalmodel.citations`.
- New module `src/digitalmodel/asset_integrity/inservice_inspection/__init__.py`.
- `tests/asset_integrity/inservice_inspection/test_api_rp_2i.py`:
  - Each hardware type round-trips through technique selection.
  - `wear_allowance_check` returns `REJECT` for chain at exactly 12% diameter loss.
  - Inspection-interval test for new vs aged hardware.
  - Citation resolution test.
- `knowledge/wikis/engineering-standards/wiki/standards/api-rp-2i.md` (workspace-hub).
- `docs/domains/articles/api_rp_2i_inspection_program.md` — explainer linking to `mooring_design.py` (design) and `structural_health.py` (SHM).
- **Non-goals.** Risers (RP 2RD, separate). Topsides (RP 583, RP 572 → see #583). Subsea production hardware (DNV-RP-F116, separate). FFS / fitness-for-service for accepted-with-defect components (API 579, already in repo). Inspection-data-management UI.

## Deliverables
- `src/digitalmodel/asset_integrity/inservice_inspection/__init__.py`
- `src/digitalmodel/asset_integrity/inservice_inspection/api_rp_2i.py`
- `tests/asset_integrity/inservice_inspection/test_api_rp_2i.py`
- `tests/asset_integrity/inservice_inspection/fixtures/mooring_history.csv`
- `knowledge/wikis/engineering-standards/wiki/standards/api-rp-2i.md` (workspace-hub)
- `docs/domains/articles/api_rp_2i_inspection_program.md`

## Approach
1. **Add wiki resolver page.** Mirror `api-rp-2sk.md` frontmatter. Smoke check: `grep -c "code_id: api-rp-2i" knowledge/wikis/engineering-standards/wiki/standards/api-rp-2i.md`.
2. **Enums + interval tables.** Lift directly from RP 2I Tables 4-1 to 4-4 as Python dicts. Smoke check: `cd digitalmodel && uv run python -c "from digitalmodel.asset_integrity.inservice_inspection.api_rp_2i import InspectionInterval; print('ok')"`.
3. **Technique selector + interval calc.** Implement §4 decision tree. Smoke check: `cd digitalmodel && uv run pytest tests/asset_integrity/inservice_inspection/test_api_rp_2i.py::test_technique_selection_chain -q`.
4. **Wear allowance.** Implement §5.2 limits. Smoke check: `cd digitalmodel && uv run pytest tests/asset_integrity/inservice_inspection/test_api_rp_2i.py::test_wear_allowance_chain_12pct -q`.
5. **Citation wiring.** Emit citations from each public function. Smoke check: `cd digitalmodel && uv run pytest tests/asset_integrity/inservice_inspection/ -q`.
6. **Article + cross-link** with `mooring_design.py` and `structural_health.py` docstrings.

## Open questions
- **Issue-title scope mismatch.** Title says "Marine Structures" but RP 2I is **mooring-hardware-specific**. Confirm scope is mooring hardware only; if user wants all marine structures, the relevant API codes are RP 2SIM / RP 583 / RP 572 (separate issues). **Needs user input.**
- **Edition.** RP 2I 3rd Ed. 2008 reaffirmed 2014. A 4th Ed. is rumored but not published as of 2026-05. Default: target 3rd Ed. (2008/R2014).
- **Risk-based interval extension.** §3.4 allows interval extension on a documented risk basis. v1 should expose `risk_factor_uplift` but default to 1.0 (no extension). Confirm.
- **Synthetic-rope inspection.** RP 2I §4.2.3 covers synthetics lightly; the deeper standard is RP 2SM (#584). Default: cross-link from this module to #584 once #584 lands.

## Acceptance Criteria
- [ ] All seven hardware types in `MooringHardwareType` round-trip through `select_inspection_techniques`.
- [ ] `wear_allowance_check` for chain returns `REJECT` at ≥12% diameter loss (per RP 2I §5.2.2).
- [ ] `compute_next_inspection_due` produces a date consistent with RP 2I Table 4-1 routine intervals for new chain.
- [ ] Citations emit for §3, §4, §5 and resolve via `api-rp-2i.md`.
- [ ] Full test suite green: `cd digitalmodel && uv run pytest tests/asset_integrity/inservice_inspection/ -q`.
