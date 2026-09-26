# Cathodic Protection

## Purpose

Design and assessment of galvanic (sacrificial anode) and impressed-current cathodic
protection for submarine pipelines, ship hulls, fixed offshore structures and subsea
equipment, against DNV-RP-B401, DNV-RP-F103, ABS Guidance Notes (ships, offshore),
ISO 15589-2 and API RP 1632. This directory holds the domain notes, the worked examples that
run through the code, the analysis notes and the technical reviews. The code lives in two
places (below); the worked examples are the contract between the two.

## Module map

### `src/digitalmodel/cathodic_protection/` (package)

| Module | One line |
|--------|----------|
| `__init__.py` | Package entry; re-exports the edition helpers and the calculators below |
| `_edition.py` | DNV-RP-B401 edition normalisation (`"2017"`, `"2021"`; default 2021) |
| `anode_depletion.py` | Anode consumption tracking, remaining-life and inspection-interval estimates |
| `anode_sizing.py` | Sacrificial anode sizing per DNV-RP-B401: stand-off, bracelet, flush-mount; McCoy and Dwight resistance |
| `api_rp_1632.py` | API RP 1632 galvanic CP of underground tanks and piping |
| `coating.py` | Coating breakdown factors (initial, mean, final) and coating-life estimates per DNV-RP-B401 |
| `corrosion_rate.py` | CO2 / H2S / galvanic corrosion-rate models (de Waard-Milliams, Norsok M-506) |
| `cp_monitoring.py` | Reference electrodes, data loggers, remote monitoring and alarm thresholds |
| `cp_reporting.py` | CP assessment report generation: compliance checks, survey comparison, recommendations |
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
| `DNV_RP_F103_2010` | DNV-RP-F103 October 2010 (2016 tables not yet in the repo) | in-file; `cp_DNV_RP_F103_2010.py` is an older standalone |
| `ABS_gn_offshore_2018` | ABS Guidance Notes on Cathodic Protection of Offshore Structures, December 2018 | in-file |
| `DNV_RP_B401_offshore` | DNV-RP-B401; `design_data.edition` selects 2005 / 2010 / 2017 / 2021 (requires #2207 or later); reports initial / mean / final demand, `governing_case`, `recommended_anode_count`, `provenance` and `citations` | `cp_DNV_RP_B401_2021.py` on `b401_tables.py` |

Related: `cp_sacrificial_anode_b401.py` (shared B401 sizing equations), `cp_astm_g42.py`,
`cp_astm_g80.py`. `digitalmodel.infrastructure.common.cathodic_protection` is a deprecated
import shim for the same class.

### Tables added by #2207

`src/digitalmodel/cathodic_protection/b401_tables.py` and `f103_tables.py` — cited,
edition-keyed DNV-RP-B401 (Tables 10-1 / 10-2 by climate × depth band, Table 10-4 by
category × depth band, Sec. 6.3 buried, Tables 10-6 / 10-7 / 10-8) and DNV-RP-F103 table
modules, wired into the package and the legacy B401 route. Each edition carries a provenance
flag (`verified-2011-tables` for 2005 / 2010, `inherited-2011-unverified` for 2017 / 2021). The
worked-example "Reproduction notes" record the values the route returns with these tables.

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
| [`calc-006-dnvgl-b401-walking-mitigation.md`](examples/calc-006-dnvgl-b401-walking-mitigation.md) | Pipe clamp mattress anodes for walking mitigation, DNV-RP-B401:2021 |
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

## Analysis notes (being redacted under #2155 — filenames only)

- [`saipem_cp_comparison_analysis.md`](saipem_cp_comparison_analysis.md)
- [`COATING_COMPARISON_ANALYSIS.md`](COATING_COMPARISON_ANALYSIS.md)
- [`CP_ANALYSIS_FINDINGS.md`](CP_ANALYSIS_FINDINGS.md)
- [`cp_bug_fixes_summary.md`](cp_bug_fixes_summary.md)
- [`standards-inventory.md`](standards-inventory.md) (standards inventory)

## Reviews

- [`technical-quality-review-2026-09-24.md`](technical-quality-review-2026-09-24.md) — technical-quality review of the CP code and docs
  (section 6 is the source of the #2214 documentation work)
- [`technical-quality-review-2026-09-25-human-decisions.html`](technical-quality-review-2026-09-25-human-decisions.html) — owner decisions D1–D12 on that
  review (D11: brochure removed; D12: examples/index rework)

## Tests and fixtures

| Location | Covers |
|----------|--------|
| `tests/cathodic_protection/` | Package modules (one `test_<module>.py` each), B401 edition foundation and divergence baseline, `test_worked_examples.py` (this directory's examples) |
| `tests/specialized/cathodic_protection/` | Legacy router: ABS ships and ABS offshore 2018 calculations; `conftest.py` fixtures |
| `tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py` | Legacy router: DNV-RP-F103:2010 (calibrated by `f1a1b05f`, #573) |
| `tests/benchmarks/test_cp_benchmarks.py` | CP benchmarks |
| `tests/fixtures/test_vectors/cathodic_protection/*.yaml` | Test vectors: B401 anode count/mass/resistance, coating breakdown, current demand, sacrificial anode; F103 protected length; ISO 15589-2 pipeline; API RP 1632; fuel-system CP |

## Plans

- [`docs/plans/2026-09-25-issue-2214-cp-docs-examples-index-brochure.md`](../../plans/2026-09-25-issue-2214-cp-docs-examples-index-brochure.md) (this rework)
- [`docs/plans/2026-05-05-issue-573-dnv-rp-f103-cathodic-protection-calibration.md`](../../plans/2026-05-05-issue-573-dnv-rp-f103-cathodic-protection-calibration.md) (closed)
