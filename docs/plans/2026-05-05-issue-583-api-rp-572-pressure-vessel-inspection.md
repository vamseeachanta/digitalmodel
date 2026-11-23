# Plan: digitalmodel #583 — API RP 572 Inspection of Pressure Vessels

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/583
**Status:** plan-review
**Tier:** T2 (well-bounded inspection workflow; mostly data-model + decision logic)

## Context
- **Standard / publisher / revision.** API RP 572, 4th Edition (November 2016) is the current public release. Issue body cites "2nd Ed (2001)" which is superseded — flag this as Open Question. Title: *Inspection Practices for Pressure Vessels*.
- **Why it matters.** `grep -ril "API RP 572\|API_RP_572" src/digitalmodel/` returns zero hits. The wider API RP 5xx inspection family is absent (RP 510 in-service vessel inspection / RP 570 piping / RP 571 damage mechanisms / RP 572 vessel inspection / RP 580 RBI / RP 581 RBI). RP 572 is the **practitioner-level companion to RP 510**, providing inspection technique selection, interval logic, and damage-mechanism × inspection-technique cross-walks for pressure vessels (separators, scrubbers, KO drums, columns, reactors). Adjacent existing module: `src/digitalmodel/asset_integrity/` (API 579 FFS, BS 7910). RP 572 outputs feed those FFS engines.
- **Dependencies on other issues.** Independent. Adjacent: #582 (RP 2I in-service inspection mooring hardware) — share inspection-data patterns. Needs new wiki resolver `api-rp-572.md` (workspace-hub).

## Scope
- New module `src/digitalmodel/asset_integrity/inservice_inspection/api_rp_572.py`:
  - `VesselType` enum: `SEPARATOR`, `SCRUBBER`, `KO_DRUM`, `COLUMN`, `REACTOR`, `HEAT_EXCHANGER_SHELL`, `STORAGE_TANK_PRESSURIZED`.
  - `DamageMechanism` enum: `GENERAL_CORROSION`, `LOCALIZED_CORROSION`, `EROSION`, `FATIGUE_CRACKING`, `SCC_CHLORIDE`, `SCC_CAUSTIC`, `HIC_SOHIC`, `CREEP`, `BRITTLE_FRACTURE`, `MIC`, `EXTERNAL_CORROSION_CUI` — drawn from API RP 571 cross-reference per RP 572 §6.
  - `InspectionTechnique` enum: `EXTERNAL_VISUAL`, `INTERNAL_VISUAL`, `UT_THICKNESS`, `UT_PHASED_ARRAY`, `MPI`, `LPI`, `RT`, `PMI`, `EDDY_CURRENT`, `ACOUSTIC_EMISSION`.
  - `InspectionPlan` Pydantic model — vessel + service + damage mechanisms + technique list + intervals.
  - `select_techniques(vessel_type, service_type, damage_mechanisms) -> list[InspectionTechnique]` — RP 572 §7 cross-walk.
  - `compute_inspection_interval(corrosion_rate_mpy, remaining_life_years) -> int` — RP 510 §6 half-life rule (RP 572 references this).
  - `evaluate_thickness_findings(measurements, t_min) -> ThicknessVerdict` — basic min-thickness check; deeper FFS handed off to existing `asset_integrity/API579.py`.
  - Citations to RP 572 (§6 damage mechanisms, §7 technique selection, §8 reporting) via `digitalmodel.citations`.
- `tests/asset_integrity/inservice_inspection/test_api_rp_572.py`:
  - Damage-mechanism × technique cross-walk for an amine separator (SCC + general corrosion → MPI + UT recommended).
  - Half-life interval calc for 5 mpy corrosion rate / 100 mil remaining life → 10-year interval.
  - Citation resolution.
- `knowledge/wikis/engineering-standards/wiki/standards/api-rp-572.md` (workspace-hub).
- `docs/domains/articles/api_rp_572_vessel_inspection.md`.
- **Non-goals.** No FFS calculations (deferred to existing `asset_integrity/API579.py`). No risk-based-inspection (RP 580/581 — separate). No RP 510 (pressure-vessel rerating) — that is a separate code; this PR cross-references but does not implement. No pressure-relief device inspection (RP 576 — separate). No fired-equipment (RP 573).

## Deliverables
- `src/digitalmodel/asset_integrity/inservice_inspection/api_rp_572.py`
- `src/digitalmodel/asset_integrity/inservice_inspection/__init__.py` — extend exports (shared with #582)
- `tests/asset_integrity/inservice_inspection/test_api_rp_572.py`
- `tests/asset_integrity/inservice_inspection/fixtures/amine_separator_history.yml`
- `knowledge/wikis/engineering-standards/wiki/standards/api-rp-572.md` (workspace-hub)
- `docs/domains/articles/api_rp_572_vessel_inspection.md`

## Approach
1. **Add wiki resolver page.** Frontmatter mirroring `api-rp-2a-wsd.md`. Smoke check: `head -20 knowledge/wikis/engineering-standards/wiki/standards/api-rp-572.md`.
2. **Enums + cross-walk table.** RP 572 §7 cross-walk lifted into a Python dict-of-sets. Smoke check: `cd digitalmodel && uv run python -c "from digitalmodel.asset_integrity.inservice_inspection.api_rp_572 import select_techniques, DamageMechanism; print(select_techniques('SEPARATOR','sour',[DamageMechanism.SCC_CHLORIDE]))"`.
3. **Half-life interval.** Implement RP 510 §6 (referenced by RP 572). Smoke check: `cd digitalmodel && uv run pytest tests/asset_integrity/inservice_inspection/test_api_rp_572.py::test_half_life_interval -q`.
4. **Thickness check.** Min-thickness verdict; flag deeper FFS via `asset_integrity/API579.py` link. Smoke check: `cd digitalmodel && uv run pytest tests/asset_integrity/inservice_inspection/test_api_rp_572.py::test_thickness_verdict -q`.
5. **Citations + article.** Emit + cross-link. Smoke check: `cd digitalmodel && uv run pytest tests/asset_integrity/inservice_inspection/test_api_rp_572.py -q`.

## Open questions
- **Edition.** Issue body says "2nd Ed 2001"; current public revision is **4th Ed. (Nov 2016)** with a 5th Ed. circulated 2024. Default: target **4th Ed. 2016**. **Needs user input** if 2nd Ed. is contractually required (e.g., legacy plant grandfathered to that revision).
- **API RP 571 dependency.** RP 572 §6 cross-references RP 571 damage mechanisms heavily. v1 includes `DamageMechanism` enum — full RP 571 numerical models (e.g., Larson-Miller for creep) deferred to a follow-up issue.
- **API RP 510 boundary.** RP 510 (vessel rerating + interval rules) is the parent code. v1 implements only the subset of §6 interval logic that RP 572 references; full RP 510 is a separate issue.
- **PMI policy.** Should `select_techniques` always include PMI for alloy verification on first inspection? Default: yes for stainless / nickel-alloy services; no for carbon-steel.

## Acceptance Criteria
- [ ] `select_techniques` returns ≥2 techniques for an amine separator with SCC + general corrosion damage mechanisms.
- [ ] Half-life interval calc for 5 mpy / 100 mil → 10 years (within ±0.5 year).
- [ ] Citations emit for RP 572 §6, §7, §8 and resolve via `api-rp-572.md`.
- [ ] Thickness verdict integrates cleanly with `asset_integrity/API579.py` (test fixture demonstrates the call path).
- [ ] Full test suite green: `cd digitalmodel && uv run pytest tests/asset_integrity/inservice_inspection/test_api_rp_572.py -q`.
