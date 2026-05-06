# Plan: digitalmodel #579 — DNV-RP-F106 Factory Applied External Pipeline Coatings

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/579
**Status:** plan-review
**Tier:** T2 (extension of mature `cathodic_protection/` module; bounded clause set)

## Context
- **Standard / publisher / revision.** DNV-RP-F106 (2021-09 latest; superseded the 2003 Ed. cited in WRK-494). Title: *Factory applied external pipeline coatings for corrosion control*. Publisher: DNV.
- **Why it matters.** The repo has strong cathodic-protection coverage already (`src/digitalmodel/cathodic_protection/` ships `dnv_rp_b401.py`, `coating.py`, `pipeline_cp.py`, plus 17 sibling files). DNV-RP-F106 is the upstream coating selection / qualification standard that pairs with B401 — it specifies coating types (3LPE, 3LPP, FBE, asphalt enamel), thickness ranges, holiday-detection limits, and breakdown factors that B401 already references. Today `coating.py` carries breakdown-factor numbers without a resolvable F106 citation. Implementing F106 closes a citation-contract gap and gives selection logic for upstream pipeline-engineering callers.
- **Dependencies on other issues.** Needs a wiki resolver page at `knowledge/wikis/engineering-standards/wiki/standards/dnv-rp-f106.md` (workspace-hub) — one-line addition, can be in this PR's scope. Soft dependency on `dnv_rp_b401.py` (existing) for the breakdown-factor cross-link.

## Scope
- New module `src/digitalmodel/cathodic_protection/dnv_rp_f106.py`:
  - `CoatingType` enum: `FBE`, `THREE_LAYER_PE` (3LPE), `THREE_LAYER_PP` (3LPP), `ASPHALT_ENAMEL`, `COAL_TAR_EPOXY`, `POLYCHLOROPRENE`, `POLYURETHANE`.
  - `CoatingProperties` Pydantic model: `nominal_thickness_mm`, `min_thickness_mm`, `holiday_detection_voltage_v`, `service_temp_min_c`, `service_temp_max_c`, `breakdown_factor_initial`, `breakdown_factor_mean`, `breakdown_factor_final`.
  - `COATING_LIBRARY: dict[CoatingType, CoatingProperties]` populated from F106 Table 5-1 / Table 5-2 ranges.
  - `select_coating(service_temp_c, mechanical_protection_required, expected_life_years) -> CoatingType` — the F106 §5 selection workflow.
  - `validate_thickness(coating_type, measured_thickness_mm) -> ValidationResult` — implements §5.4 acceptance.
  - `holiday_detection_voltage(coating_type, thickness_mm) -> float` — §6.3 jeeping voltage formula.
  - Citations to F106 (§5, §6) emitted via `digitalmodel.citations`.
- Update `src/digitalmodel/cathodic_protection/coating.py` module docstring to cross-reference F106 alongside B401 for breakdown factors. **No formula changes** in this PR — only the docstring + a `Citation` emission swap.
- `tests/cathodic_protection/test_dnv_rp_f106.py`:
  - Each coating in the library round-trips through `validate_thickness`.
  - `select_coating` for hot pipeline (>110 °C) returns `THREE_LAYER_PP` or `FBE` (per F106 §5.2 temperature ladder).
  - `holiday_detection_voltage(FBE, 0.5)` matches F106 §6.3 worked example.
  - Citation resolution succeeds.
- New wiki page `knowledge/wikis/engineering-standards/wiki/standards/dnv-rp-f106.md` (workspace-hub).
- **Non-goals.** No internal pipeline coatings (that is DNV-RP-F107 territory). No field-applied coatings (DNV-RP-F102). No coating defect mechanics / FFS for damaged coatings (B401 + ISO 21809 territory; out of scope). No CP design recalculation — `dnv_rp_b401.py` stays as-is.

## Deliverables
- `src/digitalmodel/cathodic_protection/dnv_rp_f106.py` — new module
- `src/digitalmodel/cathodic_protection/coating.py` — docstring + citation cross-reference update only
- `src/digitalmodel/cathodic_protection/__init__.py` — re-export new public symbols
- `tests/cathodic_protection/test_dnv_rp_f106.py` — new test file
- `knowledge/wikis/engineering-standards/wiki/standards/dnv-rp-f106.md` — wiki resolver (workspace-hub)
- `docs/domains/articles/dnv_rp_f106_coating_selection.md` — short explainer

## Approach
1. **Add the wiki resolver page** in workspace-hub before any code work, mirroring `dnv-rp-b401.md` frontmatter. Smoke check: `cat knowledge/wikis/engineering-standards/wiki/standards/dnv-rp-f106.md | head -20` and `grep -c "code_id: dnv-rp-f106" $_`.
2. **Implement enum + library + Pydantic models.** Smoke check: `cd digitalmodel && uv run python -c "from digitalmodel.cathodic_protection.dnv_rp_f106 import CoatingType, COATING_LIBRARY; print(len(COATING_LIBRARY))"`.
3. **Selection workflow.** Implement `select_coating` using F106 §5.2 temperature ladder + §5.3 mechanical-protection switch. Smoke check: `cd digitalmodel && uv run pytest tests/cathodic_protection/test_dnv_rp_f106.py::test_select_coating_high_temp -q`.
4. **Holiday-detection voltage.** Implement §6.3 formula. Smoke check: `cd digitalmodel && uv run pytest tests/cathodic_protection/test_dnv_rp_f106.py::test_holiday_detection_voltage_fbe -q`.
5. **Citation cross-reference.** Update `coating.py` docstring + emission. Smoke check: `cd digitalmodel && uv run pytest tests/cathodic_protection/ -q`.
6. **Article.** Write the explainer.

## Open questions
- **Revision.** WRK-494 cites the 2003 Ed.; current public revision is 2021-09 (latest 2024 amendment exists). Default: lock to **2021-09** as the wiki revision and document 2003 as superseded. **Needs user input** if there's a contractual reason to stay on 2003.
- **3LPP vs 3LPE temperature crossover.** Different DNV editions give 110 °C vs 115 °C as the FBE→3LPP transition. Default: 110 °C with `>=` (conservative).
- **Coating library extensibility.** Should `COATING_LIBRARY` be loadable from YAML to let projects override? Default: hard-coded in v1; YAML override is a follow-up.

## Acceptance Criteria
- [ ] `CoatingType` enum covers at least the seven types listed in F106 §5.
- [ ] `select_coating` returns a defensible coating for hot (`>=110 °C`), warm (`60–110 °C`), and cold (`<60 °C`) services.
- [ ] `validate_thickness` rejects below-minimum measurements and emits a `Citation(code_id="dnv-rp-f106", clause="§5.4", ...)`.
- [ ] Wiki page `dnv-rp-f106.md` exists and resolves via `digitalmodel.citations.schema`.
- [ ] All tests pass: `cd digitalmodel && uv run pytest tests/cathodic_protection/test_dnv_rp_f106.py -q`.
