# Plan: digitalmodel #576 — FOWT watch-circle envelope vs dynamic-cable curvature

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/576
**Status:** plan-review
**Tier:** T2 (extension of an existing pilot module, well-scoped formula set)

## Context
- **Standards / publisher / revision.** DNV-RP-0360 (2024-03) — dynamic subsea power cables. Adjacent codes already in repo: DNV-OS-E301 (position mooring), API RP 2SK (mooring). MBR / fatigue-bend tolerance is the FOWT-distinguishing constraint.
- **Why it matters.** `src/digitalmodel/orcaflex/mooring_design.py` is the citation pilot and computes mooring offset / watch circle for O&G floaters. It does **not** check that the watch-circle envelope respects the dynamic export-cable Minimum Bend Radius (MBR) and fatigue-bend envelope, which is the primary FOWT mooring sizing driver. Without this check, a passing mooring design can still violate cable warranty.
- **Dependencies on other issues.** Hard dependency on #574 (DNV-RP-0360 wiki page). Soft dependency on #575 (consumer for full coupled simulation), but this issue can be implemented standalone against the existing `mooring_design.py` since the cable check is a static-envelope calculation.

## Scope
- Extend `src/digitalmodel/orcaflex/mooring_design.py` (preferred — keeps the citation pilot in one place) with:
  - `WatchCircleConstraint` Pydantic model carrying `current_offset_limit_m`, `mbr_limit_m`, `fatigue_envelope_m`, `cable_top_hangoff_xyz`, `seabed_touchdown_xyz`, `cable_outer_diameter_m`, `manufacturer_min_bend_radius_m`.
  - `DynamicCableProfile` model — top hang-off, sag bend, touchdown geometry; provides `bend_radius_at(arc_length)`.
  - `check_watch_circle_vs_cable(offset_envelope, cable_config) -> WatchCircleResult` returning `pass: bool`, `slack_margin_m`, `governing_constraint: Literal["MBR","fatigue","current"]`, plus a `Citation(code_id="dnv-rp-0360", ...)` instance.
  - Update existing `mooring_design.py` module docstring References block to include DNV-RP-0360.
- New tests in `tests/orcaflex/test_mooring_design_fowt.py` covering:
  - Catenary mooring × spar floater × benign metocean → pass with positive slack.
  - Taut mooring × semi-sub × extreme storm offset → MBR-governed fail.
  - Hybrid (chain-poly-chain) × TLP × intermediate → fatigue-envelope governed.
- New fixture file `tests/orcaflex/fixtures/fowt_cable_profiles.yml` with three reference cable configs (one per mooring pattern).
- **Non-goals.** No fatigue life calculation (the `fatigue_envelope_m` is **input** to this module — it comes from a separate DNV-RP-0360 §6 fatigue analysis that is out of scope for this PR). No bend-stiffener sizing. No cable installation analysis. No re-implementation of catenary equations (already in `mooring_design.py`).

## Deliverables
- `src/digitalmodel/orcaflex/mooring_design.py` — extended with `WatchCircleConstraint`, `DynamicCableProfile`, `check_watch_circle_vs_cable`. Module docstring References block adds DNV-RP-0360.
- `tests/orcaflex/test_mooring_design_fowt.py` — new test file with three pattern × floater combinations.
- `tests/orcaflex/fixtures/fowt_cable_profiles.yml` — fixture data.
- `docs/domains/articles/fowt_watch_circle_cable_check.md` — short explainer; cross-link from `mooring_design.py` docstring.

## Approach
1. **Read the existing pilot.** `wc -l src/digitalmodel/orcaflex/mooring_design.py` and skim — confirm the citation-emission pattern already in place for `DNV-OS-E301` so the new DNV-RP-0360 emission follows it byte-for-byte. Smoke check: `grep -n "Citation" src/digitalmodel/orcaflex/mooring_design.py`.
2. **Add models.** Append `WatchCircleConstraint` and `DynamicCableProfile` Pydantic classes. Smoke check: `cd digitalmodel && uv run python -c "from digitalmodel.orcaflex.mooring_design import WatchCircleConstraint, DynamicCableProfile; print('ok')"`.
3. **Implement the check.** `check_watch_circle_vs_cable` computes slack vs all three constraints, returns the governing one. Smoke check: `cd digitalmodel && uv run pytest tests/orcaflex/test_mooring_design_fowt.py::test_catenary_spar_pass -q`.
4. **Citation wiring.** Emit `Citation(code_id="dnv-rp-0360", clause="§5", revision="2024-03", ...)`. Smoke check: `cd digitalmodel && uv run pytest tests/orcaflex/test_mooring_design_fowt.py -q -k citation`.
5. **Round-trip test.** Add a test that fails closed with `CitationResolutionError` if the wiki page is missing. Smoke check: `cd digitalmodel && uv run pytest tests/orcaflex/test_mooring_design_fowt.py::test_fail_closed_when_wiki_missing -q`.

## Open questions
- **MBR source.** Should the manufacturer MBR ship as a hard-coded library entry (per cable type) or stay caller-supplied? Default: caller-supplied for v1 with a small reference-cable library in the fixture file; cable-property DB is a follow-up.
- **Watch-circle envelope shape.** Is a circular envelope sufficient or do we need a polygon (offset varies with current heading)? Default: circular for v1; document polygon as follow-up.
- **Fatigue envelope source.** This module accepts `fatigue_envelope_m` as input; does the user want a future PR to compute it from DNV-RP-0360 §6? Flag as out-of-scope follow-up.

## Acceptance Criteria
- [ ] `check_watch_circle_vs_cable` produces fail-closed result when offset > cable MBR-derived envelope, with `governing_constraint` correctly identifying MBR vs fatigue vs current.
- [ ] Citation emission uses `dnv-rp-0360.md` wiki page (depends on #574); test asserts resolution succeeds.
- [ ] Unit tests cover all three patterns (catenary, taut, hybrid) × at least two floater types from `HostType` (spar / semi / TLP).
- [ ] `mooring_design.py` module docstring References block lists DNV-RP-0360.
- [ ] No regression in existing `tests/orcaflex/test_mooring_design.py`; full file passes `uv run pytest tests/orcaflex/ -q`.
