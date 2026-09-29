# Cathodic Protection

## Purpose

Design and assessment of galvanic (sacrificial anode) and impressed-current cathodic
protection for submarine pipelines, ship hulls, fixed offshore structures and subsea
equipment, against DNV-RP-B401, DNV-RP-F103, ABS Guidance Notes (ships, offshore),
ISO 15589-2 and API RP 1632. This directory holds the domain notes, the worked examples that
run through the code, the analysis notes and the technical reviews. The code lives in two
places (below); the worked examples are the contract between the two.

## Use status

Owner decision 2026-09-27 (epic #2206, D1 revisited). The domain is approved for client
use on the DNV-RP-B401 offshore and DNV-RP-F103 bracelet routes only, and only subject to
an engineer-of-record (EOR) check of every client deliverable. The four quarantined models
stay experimental. ABS ships is blocked by default after the 2026-09-27 benchmark
([#2259](https://github.com/vamseeachanta/digitalmodel/issues/2259),
[#1852](https://github.com/vamseeachanta/digitalmodel/issues/1852)) found mean demand
understated by about a third and final demand by about half. ABS offshore remains legacy,
uncited and not for client use without an independent check.

| Route / model | Status | Basis |
|---------------|--------|-------|
| `DNV_RP_B401_offshore` (engine adapter) | Client use with EOR check | Cited, edition-keyed B401 tables; seawater, buried and concrete-reinforcement zones; independent seawater/sediment anode families; riser-base component compositions; sequential temporary, wet-storage, operating and retrofit assessments; initial / mean / final checks per family |
| `DNV_RP_F103` (engine adapter; runs `design_data.edition`, default 2019) | Client use with EOR check | Cited, edition-keyed F103 tables (`f103_tables.py`); Eq. 14 protected-length check |
| `DNV_RP_F103_2010` (engine adapter; deprecated alias of `DNV_RP_F103` pinned to edition 2010, `DeprecationWarning`; a different `design_data.edition` raises) | Client use with EOR check | As `DNV_RP_F103`, edition 2010; reproduces results from before the 2019 default |
| `ABS_gn_ships_2018` (engine adapter) | Experimental, known understatement; not for design use | Raises `ExperimentalModelError` unless `inputs.design_data.experimental: true` (Boolean); opt-in preserves the legacy calculation. Benchmark 2026-09-27: mean demand about a third low, final about half low ([#2259](https://github.com/vamseeachanta/digitalmodel/issues/2259), [#1852](https://github.com/vamseeachanta/digitalmodel/issues/1852)) |
| `ABS_gn_offshore_2018` | Legacy, uncited: not for client use without an independent check | Legacy solver (`base_solvers/hydrodynamics/cathodic_protection.py`), tables not cited |
| `DNV_RP_B401_offshore_legacy`, `DNV_RP_F103_2010_legacy` | Legacy, uncited: not for client use without an independent check | Deprecated legacy solver paths |
| Stray current (`stray_current.assess_stray_current` / `design_drainage_bond`), fuel-system protection check (`fuel_system_cp.check_protection`), galvanic corrosion (`corrosion_rate.galvanic_corrosion`), ICCP anode life (`iccp_design.anode_bed_design` `estimated_life_years`) | Experimental | Quarantined (#2209): the first three raise `ExperimentalModelError` and the ICCP life is `None` unless called with `experimental=True` |

The status is machine-visible: every engine-adapter route writes
`results["status"]["use_status"]` (`client-use-with-eor-check`,
`legacy-uncited-independent-check-required`, or `experimental-known-understatement`), and the `cathodic_protection.anode_design`
report states it in the Adequacy section and in the route's status detail, so every HTML/PDF
deliverable carries it. `docs/registry/module-routing.yaml` keeps `maturity: beta` (its scale
has no "client use with conditions" value).

For a phased riser-base assessment, add
`inputs.riser_base_assessment` to the existing `DNV_RP_B401_offshore` input.
Its `components[]` flatten to the same cited B401 zones and named anode families
as the ordinary route. `phases[]` consume each installed family's mass in order;
wet-storage consumption therefore reduces the usable mass entering operation.
`retrofit` reports remaining physical/usable mass and additional counts governed
by mass and initial/final output. Component grouping, phase duration/demand,
retrofit inference and an owner-accepted output shortfall are project-practice
inputs with non-empty reasons. Acceptance records never convert the engineering
result from `FAIL` to `PASS`.

Evidence application (2026-09-29, [#2264](https://github.com/vamseeachanta/digitalmodel/issues/2264)):
provisional records and report provenance now show evidence class and source. EN 50162's
middle resistivity band includes 200 ohm-m; graphite lifetime requires measured mass.
The [verification checklists](standards-inventory.md#8-standards-verification-checklist--evidence-application)
record remaining gaps per value. These observations do not qualify experimental models.

## Module map

### `src/digitalmodel/cathodic_protection/` (package)

| Module | One line |
|--------|----------|
| `__init__.py` | Package entry; re-exports the edition helpers and the calculators below |
| `_edition.py` | DNV-RP-B401 (default 2021) and DNV-RP-F103 (`"2010"`, `"2019"`; default 2019 since 2026-09-27, was 2010) edition normalisation |
| `anode_depletion.py` | Anode consumption tracking, remaining-life and inspection-interval estimates |
| `anode_sizing.py` | Sacrificial anode sizing per DNV-RP-B401: stand-off, bracelet, flush-mount; McCoy and Dwight resistance |
| `b401_anode_families.py` | Independent B401 anode-family mass, resistance and initial/final output checks using each family's environment and electrolyte |
| `b401_family_route.py` | Engine mapping for named zone-to-family assignments, validation and overall governing margin |
| `b401_family_report.py` | Report tables for explicit zone area basis and per-family electrochemistry/checks |
| `b401_structures_phases.py` | Sequential installed-mass ledgers for temporary, wet-storage and operating phases, retrofit sizing, and fail-preserving accepted-shortfall records |
| `b401_structures_phases_retrofit.py` | Existing-system remaining-mass, depleted-output and combined existing-plus-proposed retrofit checks |
| `b401_structures_phases_schema.py` | Riser-base, foundation, mudmat and hatch-cover composition validation plus edition-specific B401 rule citations |
| `b401_structures_phases_report.py` | Report tables for component composition, phase balances, retrofit results and owner shortfall dispositions |
| `api_rp_1632.py` | API RP 1632 galvanic CP of underground tanks and piping |
| `coating.py` | Coating breakdown factors (initial, mean, final) and coating-life estimates per DNV-RP-B401 |
| `corrosion_rate.py` | CO2 / H2S / galvanic corrosion-rate models (de Waard-Milliams, Norsok M-506) |
| `cp_monitoring.py` | Reference electrodes, data loggers, remote monitoring and alarm thresholds |
| `cp_reporting.py` | CP assessment report generation: compliance checks, survey comparison, recommendations |
| `report_adapters.py` | `report:` consumers of the standard HTML/PDF engine (`anode_design` for every engine-adapter route, `assessment` for `CPAssessmentReport`); see [reporting/standard-report-engine.md](../reporting/standard-report-engine.md) (CP consumer section) |
| `cp_survey.py` | Survey data interpretation: potential mapping, attenuation curves, DCVG/ACVG, CIS |
| `dnv_rp_b401.py` | DNV-RP-B401 (2005/2017) sacrificial anode design chain incl. F103 protected length |
| `dnv_rp_f106.py` | DNV-RP-F106 factory-applied external pipeline coating selection and inspection subset |
| `fuel_system_cp.py` | Impressed-current CP for buried generator fuel piping (extends API RP 1632) |
| `iccp_design.py` | ICCP system design: rectifier sizing, anode beds, cable sizing, monitoring points |
| `iso_15589_2.py` | ISO 15589-2 offshore pipeline galvanic CP design |
| `marine_cp.py` | Multi-zone marine CP: current density by temperature and depth, calcareous correction |
| `marine_structure_cp.py` | Zone-based CP for platforms, jackets, monopiles and subsea structures; retrofit assessment |
| `pipeline_cp.py` | Pipeline CP per NACE SP0169 / ISO 15589-1: current density, anode spacing, interference |
| `stray_current.py` | AC/DC interference assessment and mitigation (drainage bonds, polarisation cells) |

### Legacy solver (router used by the worked examples)

`src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` —
`CathodicProtection.router(cfg)` accepts exactly four `inputs.calculation_type` keys:

| Router key | Standard | Helper module |
|------------|----------|---------------|
| `ABS_gn_ships_2018` | ABS Guidance Notes on Cathodic Protection of Ships, December 2017 (key misnamed; not renamed) | in-file |
| `DNV_RP_F103_2010` | DNV-RP-F103 October 2010 (2016 tables not yet in the repo); legacy-router key only, the engine-adapter key is `DNV_RP_F103` | in-file; `cp_DNV_RP_F103_2010.py` is an older standalone |
| `ABS_gn_offshore_2018` | ABS Guidance Notes on Cathodic Protection of Offshore Structures, December 2018 | in-file |
| `DNV_RP_B401_offshore` | DNV-RP-B401; `design_data.edition` selects 2005 / 2010 / 2017 / 2021 (requires #2207 or later); reports initial / mean / final demand, `governing_case`, `recommended_anode_count`, `provenance` and `citations` | `cp_DNV_RP_B401_2021.py` on `b401_tables.py` |

Related: `cp_sacrificial_anode_b401.py` (shared B401 sizing equations), `cp_astm_g42.py`,
`cp_astm_g80.py`. `digitalmodel.infrastructure.common.cathodic_protection` is a deprecated
import shim for the same class.

### Tables added by #2207

`src/digitalmodel/cathodic_protection/b401_tables.py` and `f103_tables.py` — cited,
edition-keyed DNV-RP-B401 (Tables 10-1 / 10-2 by climate × depth band, Table 10-3 / A-3 /
8-3 by reinforcement-steel area, Table 10-4 by category × depth band, Sec. 6.3 buried,
and Tables 10-6 / 10-7 / 10-8) and DNV-RP-F103 table modules. Concrete reinforcement
demand holds the single Table x-3 mean density constant across the three sizing phases;
the table does not provide separate initial/final values. Named anode families select the
Table x-6 capacity and closed-circuit potential by seawater/sediment environment and, for
2021, by anode surface temperature.

## Worked examples (`examples/`)

Every ```python block in these files is executed by
`tests/cathodic_protection/test_worked_examples.py`; blocks tagged `# not-runnable: <reason>`
are skipped.

| File | One line |
|------|----------|
| [`example-01-pipeline-dnv-f103-2010.md`](examples/example-01-pipeline-dnv-f103-2010.md) | Submarine pipeline, 3LPE, 10 km, DNV-RP-F103:2010; Variant B: FBE 24-inch line with the anode spacing check |
| [`example-02-ship-hull-abs-gn-ships-2017.md`](examples/example-02-ship-hull-abs-gn-ships-2017.md) | FST hull, 100 % coated, ABS GN Ships 2017; Variant B: 95 % coated hull with the disbonding term |
| [`example-03-platform-abs-offshore-2018.md`](examples/example-03-platform-abs-offshore-2018.md) | Fixed jacket, tropical, 50 m, ABS GN Offshore 2018 |
| [`example-03-platform-dnv-b401-offshore.md`](examples/example-03-platform-dnv-b401-offshore.md) | Fixed jacket, temperate 0–30 m, Category III, 25 yr, `DNV_RP_B401_offshore` route |
| [`calc-001-abs-gn-ships-2017-fst-hull.md`](examples/calc-001-abs-gn-ships-2017-fst-hull.md) | Abstracted FST hull anode design, ABS ships method, 5-yr with 15/25-yr sensitivities |
| [`calc-002-dnv-f103-2010-anode-depth-salinity.md`](examples/calc-002-dnv-f103-2010-anode-depth-salinity.md) | Depth/salinity resistivity profile feeding calc-001 (not runnable; resistivity helper pending) |
| [`calc-003-dnvgl-f103-2016-flowline-flet-to-flet.md`](examples/calc-003-dnvgl-f103-2016-flowline-flet-to-flet.md) | Deepwater flowlines protected from FLET anode banks, DNVGL-RP-F103:2016 (routed through the 2010 route) |
| [`calc-004-dnvgl-b401-subsea-riser-base.md`](examples/calc-004-dnvgl-b401-subsea-riser-base.md) | Subsea riser base structures, DNVGL-RP-B401:2017, 27 yr |
| [`calc-005-dnvgl-b401-riser-base-foundation.md`](examples/calc-005-dnvgl-b401-riser-base-foundation.md) | Riser base foundations (mud mat + hatch covers), DNVGL-RP-B401:2017 |
| [`calc-006-dnvgl-b401-walking-mitigation.md`](examples/calc-006-dnvgl-b401-walking-mitigation.md) | Runnable de-identified mattress/pipe-clamp composition with seawater, reinforcement and buried zones plus separate seawater/sediment anode families |
| [`calc-007-dnvgl-b401-f103-design-basis-all.md`](examples/calc-007-dnvgl-b401-f103-design-basis-all.md) | Field CP design basis (B401:2017 + F103:2016 + ISO 15589-2); reference data, not routed |
| [`calc-008-dnv-b401-2005-slhr-deepwater.md`](examples/calc-008-dnv-b401-2005-slhr-deepwater.md) | Single-leg hybrid risers, DNV-RP-B401:2005, 22 yr, tropical |
| [`calc-009-dnv-b401-2005-fsr-temporary.md`](examples/calc-009-dnv-b401-2005-fsr-temporary.md) | Temporary free-standing riser, 6-month life, DNV-RP-B401:2005 / NACE SP0176 |
| [`calc-010-dnv-b401-2021-fst-design-philosophy.md`](examples/calc-010-dnv-b401-2021-fst-design-philosophy.md) | FST hull CP philosophy and SACP sensitivity, DNV-RP-B401:2021, 25/40 yr |
| [`calc-011-abs-gn-ships-2017-fst-cp-specification.md`](examples/calc-011-abs-gn-ships-2017-fst-cp-specification.md) | FST hull CP specification, ABS GN Ships 2017, 5 yr, 0.325 Ω·m |

## Domain notes

| File | One line |
|------|----------|
| [`literature.md`](literature.md) | Introduction, salinity/resistivity data, standards on file, references, vendors |
| [`abs_ship_sacrificial_anode.md`](abs_ship_sacrificial_anode.md) | Sacrificial anode methodology per the ABS ships Guidance Notes (sections 4–7) |
| [`ship_cp_requirements_abs.md`](ship_cp_requirements_abs.md) | Design-data checklist for ship CP per ABS (vessel, environment, coating, anodes, ICCP) |
| [`anode_material.md`](anode_material.md) | Aluminium vs zinc vs magnesium anodes and salinity |
| `cp_calculation.puml` / `.png`, `cp_highlevel_workflow.puml` / `.png`, `fst_cp_work.puml` / `.png` | Workflow diagrams |
| `cp_ship_typical_design_currents.PNG`, `sa_slender_anode.png`, `image.png` | Figures referenced by the notes |
| `references/` | Reference figures |

## Analysis notes (identifiers redacted under #2155 and #2167)

- Contractor CP comparison note: removed from the public tree under #2167 (a private copy is kept)
- [`COATING_COMPARISON_ANALYSIS.md`](COATING_COMPARISON_ANALYSIS.md)
- [`CP_ANALYSIS_FINDINGS.md`](CP_ANALYSIS_FINDINGS.md)
- [`cp_bug_fixes_summary.md`](cp_bug_fixes_summary.md)
- [`standards-inventory.md`](standards-inventory.md) (standards inventory)

## Reviews

- [`technical-quality-review-2026-09-24.md`](technical-quality-review-2026-09-24.md) — technical-quality review of the CP code and docs
- [`session-handoff-2026-09-27.md`](session-handoff-2026-09-27.md) — what the 2026-09 review and remediation delivered, current use status, open items
  (section 6 is the source of the #2214 documentation work)
- [`technical-quality-review-2026-09-25-human-decisions.html`](technical-quality-review-2026-09-25-human-decisions.html) — owner decisions D1–D12 on that
  review (D11: brochure removed; D12: examples/index rework)

## Tests and fixtures

| Location | Covers |
|----------|--------|
| `tests/cathodic_protection/` | Package modules (one `test_<module>.py` each), B401 edition foundation and divergence baseline, riser-base phased/retrofit route tests, `test_worked_examples.py` (this directory's examples) |
| `tests/specialized/cathodic_protection/` | Legacy router: ABS ships and ABS offshore 2018 calculations; `conftest.py` fixtures |
| `tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py` | Legacy router: DNV-RP-F103:2010 (calibrated by `f1a1b05f`, #573) |
| `tests/benchmarks/test_cp_benchmarks.py` | CP benchmarks |
| `tests/fixtures/test_vectors/cathodic_protection/*.yaml` | Test vectors: B401 anode count/mass/resistance, coating breakdown, current demand, sacrificial anode; F103 protected length; ISO 15589-2 pipeline; API RP 1632; fuel-system CP |

## Plans

- [`docs/plans/2026-09-25-issue-2214-cp-docs-examples-index-brochure.md`](../../plans/2026-09-25-issue-2214-cp-docs-examples-index-brochure.md) (this rework)
- [`docs/plans/2026-05-05-issue-573-dnv-rp-f103-cathodic-protection-calibration.md`](../../plans/2026-05-05-issue-573-dnv-rp-f103-cathodic-protection-calibration.md) (closed)
