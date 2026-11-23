# Plan: digitalmodel #584 — API RP 2SM Synthetic Fiber Rope Mooring

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/584
**Status:** plan-review
**Tier:** T2 (extends an existing pilot; well-bounded clause set)

## Context
- **Standard / publisher / revision.** API RP 2SM, 1st Edition (March 2001) + Addendum 1 (December 2007). Title: *Design, Manufacture, Installation, and Maintenance of Synthetic Fiber Ropes for Offshore Mooring*. The 2nd Edition has been in committee draft since circa 2014; 1st Ed. (2001/2007) remains the cited public revision.
- **Why it matters.** Existing surface in repo: `src/digitalmodel/orcaflex/mooring_design.py` already includes `SegmentMaterial.POLYESTER` / `HMPE` / `NYLON` enum members and a chain/wire/poly material-property library (lines ~30–50 visible in survey). However, the synthetic-specific axial-stiffness (storm vs post-installation), creep, abrasion, and torque-balance properties demanded by RP 2SM are not modelled — the library exposes only break load + linear density + axial stiffness as a single number. RP 2SM is the controlling standard for any FOWT / FPSO using polyester taut moorings (most current Brazil, GoM, and West-of-Shetland deepwater FPSO mooring systems). Adjacent: existing data clients in `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py`. README at `src/digitalmodel/subsea/mooring_analysis/README.md` already mentions API RP 2SM in passing.
- **Dependencies on other issues.** Soft alignment with #582 (RP 2I in-service inspection — synthetic ropes per RP 2I §4.2.3 cross-link to RP 2SM). Needs new wiki resolver `api-rp-2sm.md` (workspace-hub).

## Scope
- Extend `src/digitalmodel/orcaflex/mooring_design.py` material library:
  - Replace single-stiffness scalar for synthetic materials with a `SyntheticRopeStiffness` Pydantic model carrying `static_stiffness_kn`, `dynamic_storm_stiffness_kn`, `post_installation_stiffness_kn`, `mean_load_factor` (the "ML" axial stiffness uplift coefficient per RP 2SM §5.4).
  - Add `SyntheticRopeProperties` model: `creep_rate_pct_per_decade`, `abrasion_resistance_class`, `torque_match_class`, `bend_over_sheave_min_dia_ratio`, `axial_compression_fatigue_resistance`. Numbers populated from RP 2SM Table 5-1 / Table 5-2 ranges per fibre type (polyester, HMPE, aramid, nylon).
- New module `src/digitalmodel/orcaflex/synthetic_rope_design.py`:
  - `select_rope_material(service, water_depth_m, target_life_years, motion_envelope) -> SegmentMaterial` — RP 2SM §4 decision tree.
  - `axial_stiffness_for_load_history(material, load_history) -> float` — implements RP 2SM §5.4 mean-load + dynamic-stiffness equations.
  - `creep_elongation(material, mean_load_pct_mbl, service_years) -> float` — RP 2SM §5.5 creep model.
  - `axial_compression_fatigue_check(material, low_tension_events) -> CompressionFatigueVerdict` — §5.6 axial-compression-fatigue acceptance (the well-known polyester failure mode).
  - `qa_program(material, manufacturer_spec) -> list[QAItem]` — §7 manufacturing / §8 installation QA checklist.
  - Citations to RP 2SM (§4, §5, §7, §8) emitted via `digitalmodel.citations`.
- `tests/orcaflex/test_synthetic_rope_design.py`:
  - Polyester taut mooring × 2000 m WD × 25-year life → `select_rope_material` returns `POLYESTER`.
  - Mean-load 20% MBL polyester creep over 25 years matches RP 2SM §5.5 example within ±10%.
  - Axial-compression fatigue check with 10⁶ low-tension events on polyester → flag CRITICAL.
  - Citation resolution.
- `tests/orcaflex/fixtures/synthetic_rope_load_history.csv`.
- `knowledge/wikis/engineering-standards/wiki/standards/api-rp-2sm.md` (workspace-hub).
- `docs/domains/articles/api_rp_2sm_synthetic_mooring.md`.
- **Non-goals.** No FOWT-specific tendon design (TLP territory; separate). No installation-tension monitoring (out of scope). No bend-stiffener / bend-restrictor design. No detailed manufacturing tolerance (§7) acceptance criteria — checklist only. No IceWise / dynamic-positioning interaction. The existing `data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py` is **not** modified — it stays a data ingest layer; RP 2SM logic lives separately.

## Deliverables
- `src/digitalmodel/orcaflex/mooring_design.py` — material library extended with `SyntheticRopeStiffness` + `SyntheticRopeProperties`
- `src/digitalmodel/orcaflex/synthetic_rope_design.py` — new module
- `src/digitalmodel/orcaflex/__init__.py` — re-export new symbols
- `tests/orcaflex/test_synthetic_rope_design.py`
- `tests/orcaflex/fixtures/synthetic_rope_load_history.csv`
- `knowledge/wikis/engineering-standards/wiki/standards/api-rp-2sm.md` (workspace-hub)
- `docs/domains/articles/api_rp_2sm_synthetic_mooring.md`

## Approach
1. **Add wiki resolver page.** Frontmatter following `api-rp-2sk.md`. Smoke check: `head -20 knowledge/wikis/engineering-standards/wiki/standards/api-rp-2sm.md`.
2. **Extend material library.** Add Pydantic models in `mooring_design.py`. Smoke check: `cd digitalmodel && uv run python -c "from digitalmodel.orcaflex.mooring_design import SyntheticRopeProperties; print('ok')"`.
3. **Selector + stiffness model.** Implement §4 decision tree + §5.4 stiffness equations. Smoke check: `cd digitalmodel && uv run pytest tests/orcaflex/test_synthetic_rope_design.py::test_select_polyester_taut -q`.
4. **Creep + axial-compression fatigue.** §5.5 + §5.6 implementations. Smoke check: `cd digitalmodel && uv run pytest tests/orcaflex/test_synthetic_rope_design.py::test_creep_polyester -q && uv run pytest tests/orcaflex/test_synthetic_rope_design.py::test_axial_compression_fatigue -q`.
5. **QA checklist.** §7/§8 enumerated as `QAItem` data. Smoke check: `cd digitalmodel && uv run pytest tests/orcaflex/test_synthetic_rope_design.py::test_qa_program -q`.
6. **Citation wiring + article.** Emit + cross-link from `mooring_design.py` docstring References block.

## Open questions
- **Edition.** 1st Ed. (2001) + Addendum 1 (2007) is the current public revision; 2nd Ed. has been in draft for ~10 years. Default: target 1st Ed./Addendum 1. **Needs user input** if a draft 2nd Ed. is available to cite.
- **Bend-over-sheave installation tension.** §6.2 covers installation handling; v1 captures the limit ratio only, full installation analysis deferred. Confirm.
- **Hybrid moorings.** Polyester sandwiched between chain segments is the FPSO-typical layout. v1 covers the synthetic segment in isolation; full hybrid stiffness composition (chain + poly + chain → effective system stiffness) lives in #576 / `mooring_design.py`. Confirm boundary.
- **Aramid / HMPE FOWT use.** HMPE is increasingly proposed for FOWT taut moorings (lower stretch). RP 2SM 1st Ed. covers aramid + HMPE less thoroughly than polyester. Default: cover all three but flag the HMPE coverage as weaker. Confirm.

## Acceptance Criteria
- [ ] `SyntheticRopeStiffness` model exposes static, dynamic-storm, and post-installation stiffness separately (closes the single-scalar gap in `mooring_design.py`).
- [ ] `select_rope_material` returns `POLYESTER` for the canonical 2000 m WD × 25-year FPSO scenario.
- [ ] Polyester creep at 20% MBL × 25 years matches RP 2SM §5.5 example within ±10%.
- [ ] Axial-compression fatigue check correctly flags 10⁶ low-tension events on polyester as CRITICAL.
- [ ] Citations for RP 2SM §4, §5, §7, §8 emit and resolve via `api-rp-2sm.md`.
- [ ] Full test suite green: `cd digitalmodel && uv run pytest tests/orcaflex/test_synthetic_rope_design.py -q`; no regression in `tests/orcaflex/test_mooring_design.py`.
