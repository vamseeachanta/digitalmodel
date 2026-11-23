# Cathodic Protection — Technical Quality Review (2026-09-24)

> Scope: everything under `src/digitalmodel/cathodic_protection/`, the legacy solver under `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/`, the six `examples/workflows/cathodic-protection*` demos, CP reporting, tests, fixtures, and `docs/domains/cathodic_protection/`.
> Baseline: origin/main `c7b5766d` (local checkout `87d56cac`; CP paths are byte-identical between the two).
> Standards applied: `workspace-hub/AGENTS.md`, `.claude/standards/{code-style,testing,best-practices,input-file-architecture}.md`, `.claude/quality-gates.yaml` (origin/main), `docs/domains/project-docs/HTML_REPORTING_STANDARDS.md`, `TABLE_FORMATTING_STANDARDS.md`.
> Method: four independent adversarial reviewers (B401/anodes; pipeline/ICCP/corrosion; workflows/legacy/reporting; tests/docs). Each blocker was re-verified by the orchestrator against code and, where available, against the repo's own DNV-RP-B401 dataset CSVs in `llm-wiki/wikis/engineering-standards/wiki/datasets/dnv-rp-b401/`.

## 1. Verdict

**Not release-grade. Block for client use; keep as beta.** The suite is green (480 tests locally, 549 on origin/main, 0 skips) and the new package is cleanly structured (pydantic I/O in 13 of 19 modules, 77–100 % line coverage per module). But the numbers behind it are not trustworthy yet:

- Several DNV-RP-B401 constants are wrong or invented and are locked in by tests.
- Eight blocker-class physics/unit errors exist in the pipeline, corrosion-rate, ICCP, fuel-system and interference modules.
- The YAML engine routes every CP workflow to the **legacy** solver, whose B401 tables are inverted; the new package is unreachable from `python -m digitalmodel`.
- Four of six shipped demo workflows return `adequate: false` and still exit 0.
- The `edition` argument ("2017"/"2021") changes no value anywhere.

What is sound and edition-stable: the B401 core relations (I = A·i·f_c; M = I·t·8760/(u·ε); long-slender Dwight resistance; ΔE = 0.25 V for Al-Zn-In; ε = 2000 Ah/kg; u = 0.90/0.85/0.80), F103 Eq. 14 protected length, the Dwight vertical-rod formula, NACE SP0169 potential criteria, and `dnv_rp_f106.py` (the one module that meets the full repo bar: typed I/O, validation, citation sidecar).

## 2. Gate compliance (repo engineering standards)

| Gate / standard | Status | Evidence |
|---|---|---|
| `tests-cathodic-protection` (both roots, #1923) | PASS | 480 passed in 10 s locally; 549 on origin/main |
| Coverage ≥ 60 % fail / ≥ 80 % warn | WARN | new package 67–100 % per module; legacy `cathodic_protection.py` 62.5 %; `cp_DNV_RP_F103_2010.py` 9.6 % (no callers) |
| `ruff check` (blocking gate) | FAIL | 10 findings in the package: 6× F401 unused import, 2× F841 unused variable (`anode_depletion.py:269`, `marine_structure_cp.py:344`), 2× E741 (`stray_current.py:167,223`) |
| Complexity (C901 ≥ 10 warn) | WARN | `cp_reporting.generate_assessment_report` = 11 |
| mypy | FAIL | `pipeline_cp.py:135` missing named arg `recommended_anode_spacing_m`; `:371` returns Any |
| Citation registry (`digitalmodel.citations`) | PARTIAL | only `coating.py` and `dnv_rp_f106.py` emit `Citation`; 17 modules cite by docstring string only |
| TDD / doc-verified tests | PARTIAL | ~120 of 549 tests trace to a table or an abstracted calc; ~225 recompute the formula inline; ~140 are range/metadata checks |
| Testing standard naming | FAIL | `test_anode_depletion_new.py` |
| Fixture consumption (testing.md) | FAIL | 10 YAML vectors in `tests/fixtures/test_vectors/cathodic_protection/` are read by nothing |
| HTML reporting standard | FAIL | `cp_html_report.py` loads Plotly from CDN, no header/footer/data-source blocks; the B401 calc-sheet HTML has no interactive plot |
| Table formatting standard | FAIL | 20 of 21 domain docs carry units inside table cells |
| Public hygiene (#2155/#2167) | FAIL | operator/contractor names, a named project, contractor document numbers and 14 private absolute paths in four analysis notes and `standards-inventory.md`; PR #2167 does not touch them |
| Code style standard itself | n/a | `.claude/standards/code-style.md` is Ruby/HTML-flavoured (2-space, single quotes); Python modules correctly follow black/88. The standard needs a Python section. |

## 3. Blockers (technical correctness)

All items were independently verified by the orchestrator unless marked "reviewer probe".

| # | Location | Defect | Evidence | Fix |
|---|---|---|---|---|
| B1 | `marine_structure_cp.py:52-73` | B401 Table 10-1 **final** current densities wrong; >30 m depth bands missing; a "tidal" row that B401 does not have | Code 90/100/120/150 mA/m²; Table 10-1 (repo CSV) 100/110/130/170 for 0–30 m; >30–100 m temperate mean is 80 not 100. `test_marine_structure_cp.py:39` locks "final=120" | Key the table on (climate, depth band); drop tidal row; parametrise tests from the wiki CSV |
| B2 | `coating.py:52-62, 400-436` | Coating a/b constants match neither B401 Table 10-4 nor F103 Table 5-3, yet are emitted as a B401 `Citation` (`section="§10.7 / Tables 10-2 and 10-4"`; 10-2 is current densities). "2 %/°C" and "0.5 %/100 m" corrections attributed to a §10.7.4 that does not exist | Table 10-4: Cat I a=0.10 b=0.10; II a=0.05 b=0.025/0.015; III a=0.02 b=0.012/0.008. Code FBE (0.02,0.003), 3LPE (0.01,0.002). `test_coating.py:84` expects an FBE life of 160 yr | Split into `b401_paint_breakdown` and `f103_linepipe_breakdown`, each with a cited table; delete uncited corrections |
| B3 | legacy `cp_DNV_RP_B401_2021.py:14-35` (used by jacket/monopile/manifold demos) | Current-density table is a coated/bare split B401 does not have (temperate coated 0.050 vs Table 10-1 mean 0.100), includes an **atmospheric** 0.010 A/m² row (CP does not act in air), and inverts coating categories (Cat I a=0.05, Cat III a=0.25 vs Table 10-4 Cat I a=0.10, Cat III a=0.02). Docstring cites "Table 3-1 / Sec 3.4.6 / 4.9", which do not exist. 59 tests in `tests/specialized/.../test_cathodic_protection_b401.py` assert these values | Jacket demo returns f_cf = 0.55 where Table 10-4 Cat III/25 yr gives 0.32 | Replace tables with Table 10-1/10-4, re-baseline the 59 tests; this repeats the WRK-279 (#411) fabricated-reference pattern |
| B4 | `_edition.py` + every function in `dnv_rp_b401.py`, `anode_sizing.py:295`, `coating.py:532`, `marine_cp.py:606`, `marine_structure_cp.py:191` | `edition` is a no-op: each function does `_ = normalize_edition(...)`. `test_edition_api_foundation.py:129-138` asserts 2017 == 2021. Result objects carry "DNV-RP-B401 (May 2021)" and `citation.revision="2011"` at once. "May 2021" should be verified against the PDF (reviewers recall the 2021 edition is dated September 2021). Omitting the argument emits a `UserWarning` on every call | — | Make edition a real key into per-edition tables and citation sections, or remove the parameter |
| B5 | `corrosion_rate.py:108-114` | Faraday mm/yr-per-mA/m² factors are 10× too high (µm confused with mm) | Hand calc Fe: 55.85e-3/(2·96485)/7870 · 1e-3 A/m² · 3.156e7 s = 0.00116 mm/yr; code 0.0116 | Divide all five by 10; add a Faraday unit test |
| B6 | `api_rp_1632.py:33` | Mg H-1 capacity 500 A·h/kg is the per-pound figure | Practical Mg H-1 ≈ 500 A·h/lb ≈ 1100 A·h/kg; anode life under-predicted 2.2× | 1100, cited |
| B7 | `pipeline_cp.py:195-226` | Holiday-test voltage 3–65× too low; parameter named `wall_thickness_mm` but is coating DFT; contradicts `dnv_rp_f106.holiday_detection_voltage` | 0.4 mm FBE → 100 V; NACE SP0490 525·√t(mils) → 2083 V. `test_pipeline_cp.py:89-106` asserts the wrong values | Delete and route to `dnv_rp_f106` |
| B8 | `iccp_design.py:153-164` | Rectifier sizing silently caps at 120 V / 200 A | 300 A into 5 Ω needs ~2250 V; returns 120 V/200 A with no flag | Raise, or return `exceeds_standard_range=True` and unit count |
| B9 | `iccp_design.py:222-263` | Anode mass uses cast-iron density for all materials; "Sunde's formula" is not Sunde's; ×0.6 deep-well factor and consumption rates uncited | reviewer probe: Pt/Ti life 397 608 yr; MMO 2386 yr | Per-material density; MMO/Pt life by coating wear; real Sunde with spacing term |
| B10 | `stray_current.py:163-178, 231-240` | DC model equates remote point-source earth potential with pipe-to-soil shift (always "critical"); AC model has an unexplained `/1000`, goes negative for d > 658 m, and multiplies V/R by circumference and calls it a density | reviewer probe: 100 A at 50 m → 15.9 V "shift"; 1 kA/50 m/ρ=50/1 km → 0.16 V "low" where Carson gives ~160 V | EN 50162 Table 1 resistivity-banded DC shift limits; ISO 18086 AC coupon-density method (30 mA/m² criterion) |
| B11 | `fuel_system_cp.py:173-189` | "Protection check" = −0.55 − I·R is not a physical model; passes at −5.55 V CSE | reviewer probe | Remove; protection is verified by survey (SP0169 §6) |
| B12 | `corrosion_rate.py:263-278` | Galvanic model is ohmic-only with no polarisation; unknown materials silently default | reviewer probe: CS/SS 1:1 seawater → 264 mm/yr | Polarisation-resistance or tabulated (BS PD 6484) model; raise on unknown material |
| B13 | `engine.py:13, 395` | `basename: cathodic_protection` dispatches to the legacy solver only; the new package cannot be reached from any YAML | run of all six demos | Thin adapter from the engine onto the new package; legacy behind `*_legacy` keys |
| B14 | legacy `cathodic_protection.py:1148-1153`, B401 wrapper | Anode count returned as float (184.257); `adequate=false` / `initial_meets_demand=false` never fail the run | ships, jacket, monopile demos all exit 0 with failing designs; fpso demo returns mass only | `ceil()`; `status: FAIL` + warning; durable-workflow tests assert adequacy |

## 4. Major findings

**B401 design loop (new package)**
- No initial/final anode current-output check (B401 §7.8). Anode count is mass-only in `marine_structure_cp.py:224-228`, `marine_cp.py:629-633`, `anode_sizing.py:316-331`. Hand check (North Sea jacket, 8000 m² bare, 25 yr, 200 kg anodes): mass gives 487 anodes, initial current needs 506; the module returns 487 with no flag. Final demand should use 130 mA/m², code uses 120.
- Bracelet/flush resistance formulas are not B401 Table 10-7 (0.315·ρ/√A and ρ/(2S)); `flush_anode_resistance` (`dnv_rp_b401.py:305-353`) takes width/height then ignores them, uses inch/Ω·cm units, and calls a half-space slender-body expression "McCoy". No L ≥ 4r validity check for the long-slender formula.
- `marine_structure_cp.py:83,204,271`: `fc = cbf if cbf > 0 else 1.0` treats a perfect coating (f_c = 0) or an omitted value as bare steel.
- `anode_depletion.py:135-147, 206-218`: utilisation factor double-counted (consumed mass = I·t·8760/(ε·u) is the gross requirement, not metal consumed); `end_of_life_year` 18.5 yr vs the same profile's remaining-mass zero at 20.5 yr.
- `marine_cp.py:364-377, 472-526`: a second, continuous temperature/depth density model that B401 does not define, with an uncited `CALCAREOUS_REDUCTION_FACTOR = 0.60`; two classes named `MarineCPResult`.
- `dnv_rp_b401.py`: no input validation (r_a > 4L/e gives negative resistance and negative current silently; `number_of_anodes(x, 0)` raises a bare `ZeroDivisionError`). Docstring says "2005/2017" while the edition helper accepts only 2017/2021.

**Pipeline / onshore**
- `pipeline_cp.py:345-371` `soil_resistivity_correction` k = (50/ρ)^0.3 "per ISO 15589-1" does not exist in that standard (ISO 15589-1 varies the potential criterion with resistivity, not current density).
- `pipeline_cp.py:38-53`: `CURRENT_DENSITY_TABLE` uncited; −0.950 V anaerobic and −1.2 V limit attributed to SP0169 §6.2 are ISO 15589-1:2015 Table 1; "API RP 1169" is an inspector-certification document, not a CP standard; the 100 mV polarisation-shift criterion is defined but never applied.
- `iso_15589_2.py`: pinned to the superseded 2004 edition; the current densities are B401 Table 10-1 numbers, not ISO 15589-2 Table 1; no −0.90 V SRB criterion; anode-resistance form is in neither standard.
- `api_rp_1632.py:105-131`: `I = E/(2·R_a)` halves anode output on an uncited "R_e ≈ R_a" assumption.
- `corrosion_rate.py:147-224`: NORSOK M-506 K_t values wrong (0.42/3.4 vs Table 15 °C 1.59, 60 °C 10.70; 3× under-prediction); de Waard–Milliams pH correction anchored at pH 4 instead of pH_sat(T, pCO2).
- There is **no DNV-RP-F103 bracelet-anode module** in the new package; F103 content is split across `dnv_rp_b401.protected_length`, `anode_sizing` (u = 0.80) and the uncalled legacy `cp_DNV_RP_F103_2010.py`. Legacy F103 attenuation in `cathodic_protection.py:571-743` is home-brew (coating "resistance_ohm_m2" treated as Ω·m, hard-coded steel ρ 1.7e-7 while geometry uses 2e-7, "enhanced" attenuation always 0.0). Legacy F103 Table 5-1 bands (≤50/50–80/80–120 °C) and FBE a = 0.010 should be verified against the PDF (reviewers recall ≤25/25–50/50–80/80–120 °C and FBE a = 0.030).

**Survey / monitoring / reporting**
- `cp_survey.py:156,169`: falls back to ON potential when OFF is missing, then applies the IR-free −850 mV criterion; overprotected points (< −1.20 V CSE) counted as protected. DCVG `distance_m` hard-coded 0.0.
- `cp_monitoring.py:47-79`: Ag/AgCl at +222 mV vs SHE (saturated-KCl value; seawater electrode ≈ +250 mV), which breaks the −0.80 Ag/AgCl ≈ −0.85 CSE mapping.
- `visualization/reporting/cp_html_report.py:64, 288-294, 333`: fabricates `attenuation_length_m = spacing × 10`; reads keys no longer in the F103 schema; Plotly from CDN; no edition/citation in the report.
- `cp_reporting.py:149`: cites "SP0169 §6.2.2.1" (criterion is §6.2.2.1.1); no edition field on `ComplianceCheck`.
- Legacy ABS-ships path ignores its own schema (`environment.seawater.max_temperature`, `anode.anode_current_capacity` silently unused) and extrapolates Al capacity above 2000 Ah/kg for T < 20 °C, which Table 10-6 forbids.

**Duplication (DRY, best-practices.md)**
- Current demand implemented 7×; anode mass 7×; coating a + b·t 6×; Dwight/McCoy resistance in four variants with three different flush formulas; three `CoatingType` enums (`__init__.py` exports the fuel-system one unprefixed); `pipeline_current_demand` vs `calculate_pipeline_current_demand` with different spacing clamps (2000 vs 5000 m).
- Divergent constants between legacy and new code: temperate mean density 0.050 vs 0.100 A/m²; splash 0.100/0.200 vs 0.0; stand-off utilisation 0.85 vs 0.90; steel ρ 1.7e-7 vs 2.0e-7; four different B401 edition strings.
- `cp_DNV_RP_F103_2010.py` (378 lines) has no caller. `infrastructure/common/*` are deprecation shims still imported by `scripts/generate_cp_report.py` and `scripts/run_cp_test.py`.

## 5. Tests

- Green, deterministic, no skips, no hard-coded paths, every public function referenced at least once. That is the good news.
- `test_dnv_rp_b401_doc_verified.py` is verified against `calc-008` (an abstracted client SLHR report under B401:2005), not against DNV worked examples; values at lines 230–289 trace to nothing in the repo; F103 tests live inside a "B401" file.
- `test_edition_divergence_baseline.py:33-61`: two tests compute ratios from `math` and assert on their own arithmetic (no source import). `:89-101` locks in `ValueError("not IMPLEMENTED")` for YAML keys the docs advertise.
- `tests/specialized/cathodic_protection/conftest.py:5-33`: unused fixtures with Al capacity 2750 Ah/kg (that is the density).
- ~90 assertions are `> 0` / `is not None` / range; de Waard–Milliams tested only as `0.5 < rate < 20`.
- No property-based tests (hypothesis is installed), no negative-input tests for the B401/F103 kernels.
- `tests/DOMAINS.md` cathodic row omits `tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py` and `tests/benchmarks/test_cp_benchmarks.py` (CI's `detect_touched_domains.py` already routes them).
- Local checkout still has the `collect_ignore` entries that origin/main removed in #1923; a `git pull` closes that.

## 6. Documentation

- Ten of eleven `calc-0xx` Python examples pass `calculation_type` keys the router rejects (`DNV_RP_B401_2021`, `DNVGL_RP_B401_2017`, `ABS_CP_SHIPS_2017`, `DNVGL_RP_F103_2016`, …). Accepted keys are only `ABS_gn_ships_2018`, `DNV_RP_F103_2010`, `ABS_gn_offshore_2018`, `DNV_RP_B401_offshore`. No calc doc is reproduced by a test.
- `_index.md` is a 2025-07 stub ("Total Files: 5") linking to a non-existent `cal/` directory; 30+ files unlisted.
- `example-03-...-placeholder.md` says "NOT YET IMPLEMENTED — see WRK-272" although that work closed and the offshore route exists.
- `example-01` / `example-02` exist in two overlapping versions each; the ship file is named "abs-2018" for a Dec-2017 guidance note (the router key repeats the error).
- `docs/marketing/cathodic-protection-brochure.md` claims "validated against worked examples" and "full test coverage, traceable to standards"; §5 above does not support that. Its Example 1 shows a 10 km line protected by one bracelet anode with a 272 m protected length, which reads as a failed design.
- Public hygiene: four 2026-01 analysis notes and `standards-inventory.md` carry an operator name, a contractor name, a named project, contractor document numbers and private `/mnt/...` paths. Not covered by PR #2167; belongs under #2155.
- `docs/plans/2026-05-05-issue-573-...md` and issue #573 are still open although `f1a1b05f` (2026-06-11) fixed the 16 failures.

## 7. Improvements and enhancements (recommended order)

1. **Constants first.** Create `b401_tables.py` and `f103_tables.py`: edition-keyed, each value returned as a `Citation`-bearing record via `citations.registry` (the `get_dnv_f103_reference` pattern), populated from the standards and unit-tested against the wiki dataset CSVs so constants cannot drift. Fix B1, B2, B3 here.
2. **One kernel.** `_kernels.py` for current demand, anode mass, coating breakdown, Dwight/McCoy/F103 resistances. Every standard module composes the kernel with its own cited constants. Removes the 7× duplicates and the four flush-formula variants.
3. **Full B401 §7 design loop.** Zones auto-split into depth bands; initial/mean/final demand; N = max(N_mass, N_initial, N_final) with depleted final geometry; governing case reported. Add the jacket hand-calc (Appendix A2) as a regression test.
4. **New `dnv_rp_f103.py`** (editions 2010/2016): Tables 5-1/5-3, bracelet R = 0.315·ρ/√A, Eq. 14 attenuation (moved out of the B401 module), `BraceletDesignResult`. Retire the two legacy F103 implementations.
5. **Engine adapter.** Route `basename: cathodic_protection` to the new package; keep legacy as `*_legacy`; make adequacy a `PASS/FAIL` status that the durable-workflow tests assert. Ship the six demos as passing designs.
6. **Report pack.** Give CP the hull-girder/foam-system treatment: `results/*.csv`, an offline Plotly HTML (demand vs time, mass vs count, adequacy), `citations[]` and edition in header/footer, READMEs for the four demo directories without one.
7. **Fix the physics blockers** B5–B12 as listed, each with a hand-derived expected value.
8. **Tests.** Retitle the doc-verified file; make the divergence baseline call code or delete it; merge `test_anode_depletion_new.py`; consume or delete the ten YAML vectors; add hypothesis invariants (mass ∝ I·T, R ∝ ρ) and `pytest.raises` for non-positive inputs; one published-value check per model (dW-M nomogram, M-506 example).
9. **Lint/type.** Clear the 10 ruff findings and 2 mypy errors; split `generate_assessment_report`.
10. **Docs.** Regenerate `_index.md`; executable-examples test over calc-001..011 with corrected keys; one example per standard; units to headers; soften the brochure; redact the five files under #2155; close #573.

## 8. Further analysis and new structure types

The framework already has the right shape for this (typed inputs, zones, anode geometry, depletion, survey, reporting), so extending it is cheap **once items 1–5 above land**. Anything added before that inherits the wrong tables.

Structure types not yet modelled, in rough order of commercial relevance to the existing offshore/marine client base:

| Candidate | Governing references | What exists to build on | Gap |
|---|---|---|---|
| Offshore-wind monopile internals (closed compartment, acidification, ICCP vs SACP) and transition-piece externals | DNV-RP-B401, DNV-ST-0126, DNV-RP-0416 | monopile demo (externals only) | internal-compartment demand, oxygen depletion, ICCP anode strings |
| Hull ICCP (FPSO, drillship, FSO) with reference-cell placement and shaft grounding | DNV-RP-B401 ICCP sections, ABS GN Ships, class rules | `iccp_design.py` (onshore groundbeds only) | seawater ICCP anode types (MMO, Pt/Ti), dielectric shield sizing, hull current distribution |
| Subsea structures with sled/retrofit anodes (PLET/FLET, manifolds, retrofit clamps) | DNV-RP-B401, F103 | manifold demo, `retrofit_assessment` | retrofit anode sled geometry, cable resistance, electrical continuity checks |
| Flexible risers and mooring chain (armour wires, calcareous deposits, hydrogen) | API 17B/17J, DNV-RP-B401, ISO 15589-2 | `dnv_rp_b401`, `anode_depletion` | high-strength steel potential limits (≤ −1.10 V), chain-link area models |
| Quay walls, sheet piles, jetties | ISO 12473, BS EN 12954, DNV-RP-B401 | zone model | soil/seawater dual-electrolyte demand, tidal cycling |
| Tank internals and well casings | NACE SP0388/SP0575, API 651, NACE SP0186 | `fuel_system_cp` | water-bottom demand, casing attenuation |
| Reinforced-concrete CP | ISO 12696 | none | new module |
| AC interference | ISO 18086, NACE SP21424 | `stray_current` (needs rewrite) | coupon density method, mitigation sizing |

Analyses that the current data model supports with modest additions:

- Potential-attenuation fields on long pipelines and large structures with a boundary-element or 1-D transmission-line solver, reusing the existing `gmsh_meshing` module for geometry. This would replace the fabricated `spacing × 10` attenuation length in the HTML report with a computed one.
- Uncertainty and sensitivity sweeps over current density, coating breakdown, resistivity and anode capacity using the repo's uncertainty core (#1427), giving P50/P90 anode mass instead of a single number.
- Life extension and risk-based inspection: join `anode_depletion` with `cp_survey` trend data (drop-cell, ROV stab, CIPS) to forecast remaining life per zone and schedule inspections. Ties directly to epics #1834 and #1845.
- Retrofit optimisation: minimise added anode mass subject to final-current adequacy across zones.
- Standards crosswalk: B401 2005/2010/2017/2021 and F103 2010/2016 side-by-side on the same input, which is what the `edition` argument was meant to deliver.

## 9. Issue set (filed 2026-09-25 under epic #2206)

| # | Issue | Decision | Order |
|---|---|---|---|
| #2207 | B401/F103 constants from cited, edition-keyed table modules; re-baseline 59 locked tests (B1–B3) | D2, D6 | 1 |
| #2209 | Faraday, Mg capacity, holiday voltage, rectifier cap fixed; stray current, fuel-system check, galvanic, ICCP anode life quarantined (B5–B12) | D7 | parallel with 1 |
| #2208 | `edition` functional for B401 2005/2010/2017/2021 and F103 2010/2016 (B4) | D3 | after 1 |
| #2211 | `dnv_rp_f103.py`, single kernel, full B401 §7 design loop | — | after 1 |
| #2210 | Engine adapter, PASS/FAIL adequacy, legacy behind `*_legacy`, demos retired (B13, B14) | D4, D5 | after #2211 |
| #2212 | Standard HTML/PDF reporting engine for client deliverables, CP first consumer | D9 | plan now, implement after #2210 |
| #2213 | Test hygiene, doc-verified provenance, hypothesis, lint/type gates | D6 | alongside |
| #2214 | Docs: executable examples, index, duplicates, brochure claims removed, #573 plan marked done | D11, D12 | any time |
| #2155 | Redaction of the five identifier-bearing CP docs (comment posted) | D8 | existing issue |
| #573 | Closed 2026-09-25 with reference to f1a1b05f | D12 | done |

## 10. Owner decisions (recorded 2026-09-25)

Answers were recorded on `technical-quality-review-2026-09-25-human-decisions.html` between 14:09 and 14:14 UTC and restated in epic #2206. D1–D4, D6–D8 and D10–D12 approved the recommendations. D5 replaced "passing demos" with "no more demos; end deliverables become HTML/PDF". D9 replaced the CP-only report pack with one comprehensive reporting code/HTML that handles client reporting in a standard way. D11 removes the brochure claims rather than softening them. The owner's research direction for D1–D3: keep as many options open for clients as possible (editions, regions, standards). Section 8 candidates are to be sequenced by end-deliverable demand rather than by a fixed roadmap.

## Appendix A. Hand calculations used for verification

**A1. Faraday factor, carbon steel.** 1 mA/m² × 55.85 g/mol ÷ (2 × 96 485 C/mol) ÷ 7870 kg/m³ × 3.156×10⁷ s/yr = 1.16 µm/yr = 0.00116 mm/yr. Code: 0.0116.

**A2. North Sea jacket, B401 (0–30 m, temperate).** 8000 m² bare, 25 yr, Al-Zn-In 2000 Ah/kg, u = 0.90, 200 kg anodes L = 2.0 m, ρ = 0.30 Ω·m.
I_mean = 8000 × 0.100 = 800 A; M = 800 × 25 × 8760 / (0.9 × 2000) = 97 333 kg; N_mass = 487.
r_eq = √(200/(π × 2 × 2750)) = 0.1076 m; R_a = 0.30/(2π × 2) × (ln(8/0.1076) − 1) = 0.0790 Ω; I_a = 0.25/0.0790 = 3.16 A.
I_initial = 8000 × 0.200 = 1600 A → N_initial = 506 > 487. Module returns 487, no flag.

**A3. 10 km × 12.75 in FBE flowline, F103, non-buried ≤ 25 °C, 25 yr.** A = 10 176 m²; i_cm = 0.050 A/m²; f_cm = 0.030 + 0.0003 × 12.5 = 0.03375; I_cm = 17.2 A; M = 17.2 × 25 × 8760 / (2000 × 0.80) = 2350 kg; 59 bracelets at 40 kg, ~170 m spacing. `dnv_rp_b401.anode_mass_requirement` reproduces 2350.3 kg; `pipeline_cp.design_pipeline_cp` on the same pipe returns 2.87 A / 6 anodes / 1667 m because it is an onshore spacing heuristic, not pipeline CP design.

**A4. Holiday voltage, 0.4 mm FBE.** NACE SP0490: 525 × √15.7 mils = 2083 V. Code: 5 × √400 = 100 V.
