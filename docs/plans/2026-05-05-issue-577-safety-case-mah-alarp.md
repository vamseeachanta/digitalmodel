# Plan: digitalmodel #577 — Safety Case / MAH ALARP framework module

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/577
**Status:** plan-review
**Tier:** T3 (new framework crossing four standards, no precedent in repo)

## Context
- **Standards / publishers / revisions.** NORSOK Z-013 (2010, Rev 3) — Risk and emergency-preparedness analysis. NORSOK S-001 (2018, Ed.5) — Technical safety. UK HSE SCR-2015 — Offshore Installations (Safety Case) Regulations 2015. ISO 17776 (2016) — Major-accident hazard identification.
- **Why it matters.** `grep -rilE 'safety case|alarp|qra|simops' src/digitalmodel/` returns zero hits today. The post explicitly calls Safety Cases + ALARP + QRA out as O&G-mature frameworks the FOWT sector should adopt. This module surface is broader than FOWT — applicable to any floating asset (FPSO, FLNG, FOWT). The gap blocks any future "regulator-ready" output from digitalmodel.
- **Dependencies on other issues.** Depends on #574 expansion for NORSOK Z-013 wiki resolver page. The seven pages in #574 cover FOWT-specific codes; #577 will need three additional resolver pages (NORSOK Z-013, NORSOK S-001, ISO 17776 — UK HSE SCR-2015 is a regulation, not a standard, so it gets a `regulations/` page instead of `standards/`). **This plan should request those three additional pages be added to #574 scope before #577 implementation begins.**

## Scope
- New package `src/digitalmodel/asset_integrity/safety_case/`:
  - `mah_register.py` — `MajorAccidentHazard` Pydantic model (id, description, frequency, consequence, barriers, owners). `MAHRegister` container with CSV ingest + Pydantic validation.
  - `alarp_demonstration.py` — `ALARPDemonstration` model implementing the gross-disproportion CBA per UK HSE guidance: `cost_of_risk_reduction`, `value_of_statistical_life`, `disproportion_factor`. `evaluate_alarp(measure, baseline) -> ALARPVerdict`.
  - `qra_mooring_failure.py` — Mooring-line break QRA composing with `orcaflex/mooring_design.py` outputs. `MooringFailureFrequency` × `MooringFailureConsequence` → annual-loss expectation. Chain / wire / poly defect-rate baselines pulled from public failure-rate datasets (HSE OREDA, DNV joint-industry studies).
  - `qra_vessel_collision.py` — Passing-vessel collision QRA composing with `src/digitalmodel/hydrodynamics/passing_ship/calculator.py`. `VesselCollisionFrequency` from traffic density × encounter geometry; consequence pulled from existing module.
  - `__init__.py` — public API surface.
- `tests/asset_integrity/safety_case/test_mah_register.py` — CSV round-trip, Pydantic validation errors on malformed rows.
- `tests/asset_integrity/safety_case/test_alarp_demonstration.py` — CBA fixture with two measures, asserts gross-disproportion threshold flips correctly.
- `tests/asset_integrity/safety_case/test_qra_mooring.py` — fixture chain mooring with seeded failure rate, asserts ALE within ±5% of hand calc.
- `tests/asset_integrity/safety_case/test_qra_vessel_collision.py` — fixture passing-ship scenario, asserts frequency calculation.
- `docs/domains/articles/safety_case_alarp_framework.md` — explainer with Z-013 framework diagram and ALARP triangle.
- **Non-goals.** No bowtie diagram generation (visual; out of scope). No emergency-preparedness analysis (NORSOK Z-013 §6 — separate follow-up). No QRA for fire/explosion/blowout (NORSOK S-001 §11 territory; separate follow-up). No regulator-submission packaging (administrative; out of scope). Only **mooring-failure** and **vessel-collision** QRAs in this PR.

## Deliverables
- `src/digitalmodel/asset_integrity/safety_case/__init__.py` — public API
- `src/digitalmodel/asset_integrity/safety_case/mah_register.py`
- `src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py`
- `src/digitalmodel/asset_integrity/safety_case/qra_mooring_failure.py`
- `src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py`
- `tests/asset_integrity/safety_case/` — four test files + a `fixtures/` directory
- `docs/domains/articles/safety_case_alarp_framework.md`
- `knowledge/wikis/engineering-standards/wiki/standards/norsok-z-013.md` (request added to #574 scope)
- `knowledge/wikis/engineering-standards/wiki/standards/norsok-s-001.md` (request added to #574 scope)
- `knowledge/wikis/engineering-standards/wiki/standards/iso-17776.md` (request added to #574 scope)
- `knowledge/wikis/engineering-standards/wiki/regulations/uk-hse-scr-2015.md` (new regulations subdirectory)

## Approach
1. **Coordinate with #574.** Before any code: comment on #574 requesting NORSOK Z-013, NORSOK S-001, ISO 17776 standards-page additions plus a new `regulations/uk-hse-scr-2015.md` resolver. Smoke check: `gh issue view 574 --comments`.
2. **MAH register.** Implement Pydantic models + CSV ingest first (lowest dependency). Smoke check: `cd digitalmodel && uv run pytest tests/asset_integrity/safety_case/test_mah_register.py -q`.
3. **ALARP CBA.** Add gross-disproportion calc; cite UK HSE R2P2 numerical thresholds. Smoke check: `cd digitalmodel && uv run pytest tests/asset_integrity/safety_case/test_alarp_demonstration.py -q`.
4. **Mooring QRA.** Compose with `orcaflex.mooring_design`. Smoke check: `cd digitalmodel && uv run pytest tests/asset_integrity/safety_case/test_qra_mooring.py -q`.
5. **Vessel-collision QRA.** Compose with `hydrodynamics.passing_ship`. Smoke check: `cd digitalmodel && uv run pytest tests/asset_integrity/safety_case/test_qra_vessel_collision.py -q`.
6. **Citation wiring + integration test.** All four sub-modules emit Citations. Run the full `tests/asset_integrity/safety_case/` suite. Smoke check: `cd digitalmodel && uv run pytest tests/asset_integrity/safety_case/ -q`.

## Open questions
- **Failure-rate baseline source.** OREDA and DNV-JIP failure rates are licensed datasets. Default: ship public-domain HSE rates only and document the uplift path. **Needs user input** on whether to expose proprietary-dataset hooks.
- **Value of Statistical Life.** UK HSE guidance uses ~£2M; US EPA uses ~$11M. Default: parametric — caller supplies VoSL; v1 defaults to UK HSE figure. Confirm.
- **Scope of vessel collision.** Limit to passing-ship (existing module) or extend to drifting/powered-vessel collision (NORSOK Z-013 §5.3.4)? Default: passing-ship only for v1; flag drifting as follow-up.
- **CSV schema.** Should the MAH register accept ECCAIRS / CCPS taxonomy fields, or stay minimal? Default: minimal; taxonomy import is a follow-up.

## Acceptance Criteria
- [ ] MAH register supports CSV ingest with Pydantic validation; malformed rows raise `MAHValidationError` with the offending field path.
- [ ] ALARP CBA produces a `grossly_disproportionate: bool` verdict and exposes the disproportion factor.
- [ ] Mooring-line-break QRA composes cleanly with `orcaflex/mooring_design.py` outputs (test fixture demonstrates the call path).
- [ ] Vessel-collision QRA composes cleanly with `hydrodynamics/passing_ship/calculator.py` (test fixture demonstrates the call path).
- [ ] Citations for NORSOK Z-013, UK HSE SCR-2015, ISO 17776, NORSOK S-001 all resolve via the wiki resolver pages added under the expanded #574 scope.
