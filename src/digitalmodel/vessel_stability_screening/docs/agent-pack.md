# Vessel stability screening knowledge pack

| Header | Value |
|---|---|
| Module | `vessel_stability_screening` |
| Repository | `digitalmodel` |
| Package scope | `src/digitalmodel/vessel_stability_screening/` |
| Pack revision | 0 |
| Code revision | `fd5e419b667a28fe411e1b345b40d65da98f248f` |
| Analysis family | Engineering calculations (EC); AQG r0, Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

Citations below resolve at the code revision unless a separate map revision is explicitly given.

## 1. Purpose and scope

`router` builds a loading condition, solves table-based equilibrium, evaluates intact criteria, optionally evaluates lifting, fluid-KG limits and damage cases, and returns results with CSV locations. [src/digitalmodel/vessel_stability_screening/workflow.py:L185-L352]

The package is a screening workflow; the governing stability booklet or class/regulatory submittal and licensed cross-check remain separate. [src/digitalmodel/vessel_stability_screening/workflow.py:L167-L172]

Registry maturity is development; the listed test routes include both core calculation files and both workflow files. [docs/registry/module-routing.yaml:L619-L624]

Scope includes the two Python files in this package and their delegated intact/damage calculations; the latter remain upstream dependencies, outside the package boundary used for implementation-map counting. [src/digitalmodel/vessel_stability_screening/__init__.py:L1-L5] [src/digitalmodel/vessel_stability_screening/workflow.py:L130-L163]

Hydrostatics are supplied tables, with no extrapolation; added-weight flooding reuses the intact table and approximates free communication through floodwater free-surface moment rather than a lost-buoyancy solution. [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L123-L168] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L15-L29]

## 2. Inputs and outputs

The API and CSV fields use metres, tonnes, degrees and metre-radians, with MCT in tonne-metres per centimetre; longitudinal positions start at the aft perpendicular, positive forward, and trim is positive by the stern. [src/digitalmodel/vessel_stability_screening/workflow.py:L119-L120] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L39-L42]

UNVERIFIED: vertical datum and transverse positive direction are not established for every external input format; no phase convention is established. Maintainer clarification: issue pending (1).

| Input | Unit | Range / requirement | Default | Source and evidence |
|---|---|---|---|---|
| `hydrostatics.table` or `.csv` | draft/KM/LCB/LCF: m; displacement: t; MCT: t m/cm; TPC: t/cm | At least two rows; positive required draft, displacement and KM; displacement strictly increases with draft | No required-field defaults; optional fields absent | Inline mapping or CSV relative to configuration directory; [src/digitalmodel/vessel_stability_screening/workflow.py:L358-L394] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L89-L146] |
| `loading_condition.items` | weight: t; VCG/LCG: m; FSM: t m | Nonempty; weight positive; FSM nonnegative | FSM zero; generated item/condition labels | Config; [src/digitalmodel/vessel_stability_screening/workflow.py:L397-L435] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L208-L225] |
| Item `free_surface` | length/breadth: m; density: t/m³ | Positive dimensions and density; mutually exclusive with nonzero explicit FSM | None | Config; [src/digitalmodel/vessel_stability_screening/workflow.py:L412-L425] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L198-L204] |
| `vessel` | LBP: m; immersion angles: deg | Optional; LBP checked positive when end drafts are computed | Absent | Config; [src/digitalmodel/vessel_stability_screening/workflow.py:L186-L190] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L306-L313] |
| `gz` | heel: deg; KN/GZ: m; grid displacement: t | Exactly one of KN, GZ or KN grid; at least three points, starting at zero heel and strictly increasing | Absent | Config; [src/digitalmodel/vessel_stability_screening/workflow.py:L438-L470] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L331-L345] |
| `criteria.imo_intact` | areas: m rad; arms/GM: m; angle: deg | Optional overrides of cited defaults | Enabled; default thresholds referenced without reproducing them | Config; [src/digitalmodel/vessel_stability_screening/workflow.py:L198-L208] [src/digitalmodel/vessel_stability_screening/workflow.py:L477-L490] |
| `criteria.weather_cfr_170_170` | pressure: t/m²; area: m²; lever: m; angle: deg | Positive pressure, area and lever; angle between zero and a right angle; citation required | Optional; angle default at cited code location | Config; [src/digitalmodel/vessel_stability_screening/workflow.py:L493-L514] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L575-L596] |
| `lifting` | hook: t; outreach: m; moment: t m; angle: deg; area ratio: dimensionless | Direct moment or hook/outreach; cited criteria required | Optional; residual-range default at cited code location | Config; [src/digitalmodel/vessel_stability_screening/workflow.py:L517-L556] |
| `max_kg` | tolerance: m | Requires KN input; pre-corrected GZ cannot be rebased | Optional; enabled when supplied; tolerance 0.0001 m | Config; [src/digitalmodel/vessel_stability_screening/workflow.py:L559-L594] |
| `damage_cases` | density: t/m³; volume: m³; positions: m; FSM: t m; heel: deg | Nonempty; exactly one of compartments or direct condition; cited criteria | Optional; imported seawater-density default; transverse moment zero for direct condition | Config; [src/digitalmodel/vessel_stability_screening/workflow.py:L609-L725] |
| Flooded compartment | permeability: dimensionless; volume: m³ | Volume positive; permeability greater than zero and at most unity; free communication requires positive FSM | FSM and TCG zero; free communication false; rectangular-tank density inherits floodwater density | Config; [src/digitalmodel/vessel_stability_screening/workflow.py:L778-L806] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L91-L137] |
| Direct damaged condition | displacement: t; GM/KG/draft/trim: m; heel: deg | Positive displacement; heel magnitude nonnegative; positive KG if supplied; KG required with KN | Optional properties absent | Config; [src/digitalmodel/vessel_stability_screening/workflow.py:L809-L853] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L140-L164] |
| `output_dir` and config provenance | path | Relative directory resolved against config directory, falling back to current directory | `results`; basename-derived stem | Config; [src/digitalmodel/vessel_stability_screening/workflow.py:L994-L1012] |

| Output | Unit / interpretation | Availability and evidence |
|---|---|---|
| Loading-condition and equilibrium CSVs / result blocks | Weight t; centres, draft, GM, FSC and trim m; FSM t m | Always; individual and total loading rows; unavailable trim/end drafts remain absent values; [src/digitalmodel/vessel_stability_screening/workflow.py:L263-L280] [src/digitalmodel/vessel_stability_screening/workflow.py:L887-L930] |
| Criteria CSV / `criteria_results` | Value, required, unit, comparison, margin, status and citation | When intact/lifting criteria exist; [src/digitalmodel/vessel_stability_screening/workflow.py:L281-L284] [src/digitalmodel/vessel_stability_screening/workflow.py:L933-L944] |
| GZ, lifting and KG-limit blocks | deg, m, t m, m rad; ratios dimensionless | Conditional; lifting adds heeling/net arms; KG rows include individual limits and governing key; [src/digitalmodel/vessel_stability_screening/workflow.py:L285-L292] [src/digitalmodel/vessel_stability_screening/workflow.py:L947-L976] |
| Damage-case / damage-criteria CSVs and blocks | Per-case displacement, draft, trim, GM, heel, range, area, ratio, status and cited criteria | When damage cases exist; [src/digitalmodel/vessel_stability_screening/workflow.py:L293-L299] [src/digitalmodel/vessel_stability_screening/workflow.py:L727-L745] |
| Summary CSV and `screening_status` | One summary row; pass only if all evaluated intact and damage checks pass | At least one criterion or damage result required; [src/digitalmodel/vessel_stability_screening/workflow.py:L235-L260] [src/digitalmodel/vessel_stability_screening/workflow.py:L301-L352] |

## 3. Method and governing standards

| Method | Implementation and reference |
|---|---|
| Hydrostatic interpolation | `HydrostaticTable` sorts by draft, rejects non-increasing displacement, interpolates linearly and rejects queries outside the table; `_hydrostatics` parses rows; [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L123-L192] [src/digitalmodel/vessel_stability_screening/workflow.py:L358-L394] |
| Loading and free surface | `rectangular_fsm_t_m` computes density × length × breadth cubed / 12; `build_loading_condition` sums weights and moments, FSC = total FSM / displacement, fluid KG = solid KG + FSC; [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L198-L204] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L242-L264] |
| Equilibrium | `solve_equilibrium` gets draft/KM at displacement, subtracts solid/fluid KG, computes trim from displacement × (LCB − LCG) / MCT and converts centimetres to metres; end drafts use LCF/LBP; [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L283-L324] |
| GZ and KN | `gz_from_kn` subtracts fluid KG × sine of heel; `kn_at_displacement` interpolates KN grids; `GZCurve` delegates interpolation and area integration to `damage_stability` helpers; [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L331-L409] |
| Intact criteria | `_imo_criteria` passes overrides to `ImoIntactCriteria`; `imo_intact_criteria` evaluates areas, righting arm, maximum-arm angle and fluid GM, with downflooding capping; default citation: IMO IS Code 2008 (MSC.267(85)), edition 2008, Part A 2.2.1–2.2.4 (general intact criteria, as named in code); clause text not checked against the licensed copy; [src/digitalmodel/vessel_stability_screening/workflow.py:L477-L490] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L470-L568] |
| Weather | `_weather` passes inputs to `weather_criterion_cfr_170_170`; required GM uses pressure × windage area × lever / (displacement × tangent of limiting heel); sample citation: 46 CFR 170.170, edition 2024, Weather criterion (170.170(a), as named in code); clause text not checked against the licensed copy; [src/digitalmodel/vessel_stability_screening/workflow.py:L493-L514] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L599-L625] [tests/naval_architecture/test_vessel_stability_screening.py:L133-L141] |
| Lifting | `_lifting` constructs a direct or hook-derived moment; `equilibrium_heel_deg` finds the first GZ/cosine-arm intersection; `lifting_check` checks heel and optional residual energy ratio; sample citation: 46 CFR Part 173 Subpart B, edition 2024, 173.005-series (lifting/crane heel, as named in code; applicability remains to be confirmed); clause text not checked against the licensed copy; [src/digitalmodel/vessel_stability_screening/workflow.py:L517-L556] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L632-L693] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L714-L832] [tests/naval_architecture/test_vessel_stability_screening.py:L144-L149] |
| KG limits | `_max_kg` delegates per-criterion bisection on fluid KG between zero and KM, including no feasible limit and KM-capped cases; [src/digitalmodel/vessel_stability_screening/workflow.py:L559-L594] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L884-L982] |
| Added-weight damage | `_damage_cases` calls `added_weight_equilibrium`, then optional `damage_gz_analysis` and `evaluate_damage_case`; floodwater mass = density × permeability × volume, heel from transverse moment / (damaged displacement × fluid GM); [src/digitalmodel/vessel_stability_screening/workflow.py:L655-L725] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L132-L137] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L195-L254] |
| Direct damage and survival | `_direct_condition` accepts supplied damaged properties; optional damaged KN requires damaged fluid KG; all survival thresholds are config-supplied with citation; sample citation: IMO MODU Code 2009 (A.1023(26)), edition 2009, 3.6 (damage criteria, as named in the fixture); clause text not checked against the licensed copy; [src/digitalmodel/vessel_stability_screening/workflow.py:L758-L775] [src/digitalmodel/vessel_stability_screening/workflow.py:L809-L853] [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L32-L36] |
| Alternative damage citation | 46 CFR Part 174, edition 2024, subpart C (test citation; clause title and vessel-class applicability unverified); clause text not checked against the licensed copy; no Part 174 thresholds are selected automatically; [tests/naval_architecture/test_damage_stability_screening.py:L346-L353] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L436-L481] |

The local `Citation` is an inline standard/edition/clause/note record; `_citation` stringifies these fields and CSV rows contain its label, without the shared citation sidecar/resolver fields. [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L63-L83] [src/digitalmodel/vessel_stability_screening/workflow.py:L867-L881] [src/digitalmodel/vessel_stability_screening/workflow.py:L933-L944]

UNVERIFIED: statutory applicability, exact clause titles for the generic lifting/damage citations, licensed threshold correctness and shared citation-contract conformance require source and contract review; issue pending (2). The inspected code/docs consistently name IS Code 2008 and MODU Code 2009; no conflicting edition was found in these locations. [src/digitalmodel/vessel_stability_screening/workflow.py:L8-L18] [docs/domains/ship-design/vessel-stability-screening.md:L9-L16]

## 4. Validated worked examples

The following four validated cases use synthetic closed-form expectations written in the tests, independently of production-function output; they establish these equations and fixtures, rather than statutory compliance or licensed-program agreement. [tests/naval_architecture/test_vessel_stability_screening.py:L5-L43] [tests/naval_architecture/test_damage_stability_screening.py:L5-L68]

| ID / kind | Source and input | Expected value and tolerance basis | Existing test node ID | Last offline result |
|---|---|---|---|---|
| V1 validated: loading moments | Synthetic weight items; arithmetic totals and virtual rise derivation; [tests/naval_architecture/test_vessel_stability_screening.py:L15-L24] [tests/naval_architecture/test_vessel_stability_screening.py:L114-L121] | Displacement 8200 t; KG 40200/8200 m within 1e-9 m; FSC 0.05 m within 1e-12 m; exact synthetic arithmetic tolerances in test | `tests/naval_architecture/test_vessel_stability_screening.py::test_loading_condition_hand_totals` | PASS at code pin, 2026-10-18 UTC; [tests/naval_architecture/test_vessel_stability_screening.py:L205-L212] |
| V2 validated: equilibrium | Same synthetic items and box table; independent moment/MCT derivation; [tests/naval_architecture/test_vessel_stability_screening.py:L20-L27] | Draft 4 m; fluid GM 5.3808943 m, trim 0.2341463 m, aft draft 4.1170732 m and forward draft 3.8829268 m within 1e-6 m; rounded hand-value tolerances | `tests/naval_architecture/test_vessel_stability_screening.py::test_equilibrium_hand_values` | PASS at code pin, 2026-10-18 UTC; [tests/naval_architecture/test_vessel_stability_screening.py:L233-L243] |
| V3 validated: GZ integration | Synthetic wall-sided KN table and independent analytic integral, including smooth extension beyond deck-edge immersion; [tests/naval_architecture/test_vessel_stability_screening.py:L28-L37] | Areas 0.8072556, 1.5566685 and 0.7494129 m rad for the three intervals in the test; tolerance 0.5% for coarse-table trapezoidal integration | `tests/naval_architecture/test_vessel_stability_screening.py::test_gz_areas_match_wall_sided_analytic` | PASS at code pin, 2026-10-18 UTC; [tests/naval_architecture/test_vessel_stability_screening.py:L267-L271] |
| V4 validated: flooded equilibrium | Synthetic compartment, flooded mass/moment arithmetic and interpolated intact table; [tests/naval_architecture/test_damage_stability_screening.py:L13-L32] | Floodwater 410 t; draft 4.2 m within 1e-9 m; GM 5.2357724 m and trim 0.7141463 m within 1e-6 m; heel 2.0835 deg within 0.0005 deg; test rounding tolerances | `tests/naval_architecture/test_damage_stability_screening.py::test_added_weight_equilibrium_hand_values` | PASS at code pin, 2026-10-18 UTC; [tests/naval_architecture/test_damage_stability_screening.py:L204-L221] |
| R1 regression: intact export | Router fixture; generated report tables and summary, without independent export oracle; [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L56-L119] | Six CSV files; nine criteria rows with required columns; summary status pass; exact counts (files/rows) | `tests/naval_architecture/test_vessel_stability_screening_workflow.py::test_router_writes_report_pack_ready_csvs` | PASS at code pin, 2026-10-18 UTC; [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L193-L221] |
| R2 regression: damage export | Router damage fixture; supplied properties and generated summaries; [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L69-L132] | Two damage CSV files; two case rows and ten criteria rows; zero failed cases; exact counts (files/rows/cases) | `tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py::test_router_writes_damage_csvs_and_summary` | PASS at code pin, 2026-10-18 UTC; [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L217-L241] |

Execution record: the four test files yielded 93 collected, 93 passed, zero failed, zero skipped and zero NOT_RUN before documentation changes; individual node results above come from that run, with the source cases at the code pin. [tests/naval_architecture/test_vessel_stability_screening.py:L153-L505] [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L122-L360] [tests/naval_architecture/test_damage_stability_screening.py:L179-L513] [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L177-L409]

After-change execution record: the same four source files and command form again yielded 93 collected, 93 passed, zero failed, zero skipped and zero NOT_RUN on the uncommitted documentation tree at the same code pin. [tests/naval_architecture/test_vessel_stability_screening.py:L153-L505] [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L122-L360] [tests/naval_architecture/test_damage_stability_screening.py:L179-L513] [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L177-L409]

The initial run used the project environment; the later run used a scratch project environment with the declared `pytest-timeout` dependency available, needed for the separate contracts-domain collection check. [pyproject.toml:L166-L181]

Reproduction uses `PYTHONPATH=src uv run --no-sources python -m pytest` followed by those four `tests/naval_architecture/` paths, `-q -rA -p no:cacheprovider` and a scratch `--basetemp` directory; no addopts override was used.

## 5. QA gates

Gate applicability follows AQG r0 section 10, Python calculations, and section 9 (G5).

| Gate | Applicability | Check IDs / required evidence |
|---|---|---|
| G0 inputs, units, citations | Applicable | C01, C02, C08, C09, C10: cited-input rejection, unit arithmetic and option/edge-case checks |
| G1 imports and tests | Applicable | C03: Python import/router route and all four offline test files; this workflow writes CSV, with no target-program deck load route; [src/digitalmodel/vessel_stability_screening/workflow.py:L274-L299] |
| G2 hand or published check | Applicable | C04, C05, C06: independent synthetic moments, wall-sided integral and floodwater arithmetic |
| G3 short dynamics | NOT_APPLICABLE | Deterministic table calculation, no time integration or dynamics run in `router`; [src/digitalmodel/vessel_stability_screening/workflow.py:L185-L260] |
| G4 diffraction | NOT_APPLICABLE | Supplied hydrostatic/GZ tables, no panel-mesh or radiation/diffraction computation in `router`; [src/digitalmodel/vessel_stability_screening/workflow.py:L192-L208] |
| G5 QA appendix | Applicable | C07: CSV/schema/citation evidence; signed, fingerprint-bound appendix coverage remains an open gap, issue pending (2); [src/digitalmodel/vessel_stability_screening/workflow.py:L933-L944] |

| Check ID | Executable check and numeric pass criterion / unit | Evidence and gate |
|---|---|---|
| C01 | Run the three workflow tests rejecting uncited weather, lifting and damage criteria; exactly 3 tests pass (tests), each requiring a ValueError | G0; `tests/naval_architecture/test_vessel_stability_screening_workflow.py::test_router_uncited_weather_criterion_rejected`, `tests/naval_architecture/test_vessel_stability_screening_workflow.py::test_router_uncited_lifting_criteria_rejected`, `tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py::test_router_damage_rejects_uncited_criteria`; [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L309-L337] [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L326-L330] |
| C02 | `rectangular_fsm_t_m(12, 8, 0.85)` equals 435.2 t m with pytest default relative tolerance 1e-6; equivalent workflow FSC equals 435.2/8200 m within 1e-9 m | G0; `tests/naval_architecture/test_vessel_stability_screening.py::test_rectangular_fsm_hand_value`, `tests/naval_architecture/test_vessel_stability_screening_workflow.py::test_router_free_surface_tank_dimensions`; [tests/naval_architecture/test_vessel_stability_screening.py:L215-L217] [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L267-L289] |
| C03 | Four module files: 93 tests pass; 0 failed, skipped or NOT_RUN tests; package router identity equals imported workflow router (1 identity assertion) | G1; `tests/naval_architecture/test_vessel_stability_screening_workflow.py::test_router_engine_arm_routes_basename`; [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L356-L360] |
| C04 | `build_loading_condition` and `solve_equilibrium`: V1/V2 expected mass and centre/GM/trim values within their listed tolerances in t and m; 2 tests pass | G2; V1 and V2 node IDs; [tests/naval_architecture/test_vessel_stability_screening.py:L205-L243] |
| C05 | `GZCurve.area_m_rad` matches all 3 analytic areas within 0.5% (m rad) | G2; V3 node ID; [tests/naval_architecture/test_vessel_stability_screening.py:L267-L271] |
| C06 | `added_weight_equilibrium`: V4 mass, draft, GM, trim and heel meet the listed t, m and deg tolerances; 1 test passes | G2; V4 node ID; [tests/naval_architecture/test_damage_stability_screening.py:L204-L221] |
| C07 | `_write_csv` / `router`: R1 and R2 yield 8 specified CSV files in total, 9 intact/lift and 10 damage criteria rows, 2 damage-case rows; the separate cited-row tests require 0 empty citation strings | G5; R1/R2 plus `tests/naval_architecture/test_vessel_stability_screening_workflow.py::test_router_criteria_table_cited_and_passing`, `tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py::test_router_damage_criteria_rows_cited`; [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L140-L158] [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L199-L241] |
| C08 | `HydrostaticTable` out-of-range access and `router` missing KN, missing criteria and unknown damage fields each raise; exactly 5 tests pass (tests) | G0; `tests/naval_architecture/test_vessel_stability_screening.py::test_table_rejects_out_of_range`, `tests/naval_architecture/test_vessel_stability_screening_workflow.py::test_router_max_kg_with_gz_table_rejected`, `tests/naval_architecture/test_vessel_stability_screening_workflow.py::test_router_no_criteria_rejected`, `tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py::test_router_damage_rejects_unknown_criteria_field`, `tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py::test_router_gz_thresholds_without_gz_rejected`; [tests/naval_architecture/test_vessel_stability_screening.py:L181-L185] [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L260-L264] [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L346-L349] [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L340-L344] [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L381-L385] |
| C09 | FSM-dominant intact and damaged cases produce negative fluid GM (m) and fail status; damaged failed-case count exactly 1 case | G0; `tests/naval_architecture/test_vessel_stability_screening_workflow.py::test_router_fsm_dominance_fails_gm`, `tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py::test_router_negative_post_damage_gm_fails_screening`; [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L292-L306] [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L252-L280] |
| C10 | Optional/direct routes: direct GZ produces exactly 0 KG-limit rows; damage-only route exactly 0 intact criteria rows and 2 cases; direct supplied heel retained within pytest default relative tolerance 1e-6 deg; fallback angle 25 deg; margin-line cap 20 deg; exactly 5 tests pass | G0; `tests/naval_architecture/test_vessel_stability_screening_workflow.py::test_router_direct_gz_table_skips_max_kg_requirement`, `tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py::test_router_damage_only_run_without_intact_criteria`, `tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py::test_router_direct_case_uses_supplied_heel`, `tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py::test_router_vessel_level_downflooding_is_fallback`, `tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py::test_router_margin_line_immersion_caps_limit`; [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L240-L257] [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L186-L196] [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L244-L249] [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L295-L320] |

These checks cover their named fixtures; malformed external numeric formats, nonfinite values, truthy strings and all default combinations lack complete adversarial coverage, issue pending (1). [src/digitalmodel/vessel_stability_screening/workflow.py:L982-L991] [src/digitalmodel/vessel_stability_screening/workflow.py:L473-L474] [src/digitalmodel/vessel_stability_screening/workflow.py:L803-L803]

## 6. Known failure modes and pitfalls

Sweep commands, applied to all package Python files, with source read through the enclosing definitions:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|1970|float\(|int\(|0\.0|1e-4|14\.0|40\.0|density|Units|cache' src/digitalmodel/vessel_stability_screening --glob '*.py'
rg -n 'or \{\}|or \[\]|get\(|bool\(|return|except|continue|unit|cache|1970|0\.05|5%' src/digitalmodel/vessel_stability_screening --glob '*.py'
```

sweep: 54 found

The count records candidate sites, including ordinary optional behaviour and contextual rethrows; it is not a count of confirmed numerical defects. Numeric-conversion and default gaps share one boundary-coverage draft; citation gaps share the citation-contract draft. [src/digitalmodel/vessel_stability_screening/workflow.py:L358-L881]

| Site | Symptom / classification | Detection and tracking |
|---|---|---|
| S119 | Unit assumptions: CSV/API values have no automatic unit or frame conversion; mis-scaled input can look plausible. | C02; incomplete format/frame checks, issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L119-L120] |
| S186 | Missing or falsey workflow settings become an empty mapping; required hydrostatics later raise. | C08; missing-hydrostatics test also in C03 [src/digitalmodel/vessel_stability_screening/workflow.py:L186-L186] |
| S187 | Falsey vessel settings become an empty mapping, omitting end drafts and immersion limits. | no detection check exists; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L187-L187] |
| S198 | Falsey criteria/IMO mappings become defaults; the enabled switch defaults true. | no detection check exists for malformed falsey input; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L198-L199] |
| S325 | Absent lift/governing KG uses None summary placeholders, not a failed run. | C10 for disabled KG; no detection check exists for every summary placeholder; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L325-L330] |
| S344 | Absent lifting becomes None in returned data. | no detection check exists; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L344-L344] |
| S379 | Unknown inline hydrostatic columns raise, but unknown CSV columns are ignored. | no detection check exists for extra CSV columns; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L379-L382] |
| S386 | Blank hydrostatic cells continue without assignment; optional fields disappear, so trim can remain unavailable. | no detection check exists for blank CSV optional cells; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L386-L388] |
| S389 | CSV/inline numeric conversion uses float directly: decimal/thousands separators are not normalised; nonfinite strings are not checked here. | no detection check exists for numeric grammar/nonfinite values; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L389-L389] |
| S412 | Missing loading FSM defaults zero; float conversion has no locale or finiteness guard. | C02/C09 exercise supplied FSM only; no detection check exists for omitted/invalid FSM; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L412-L412] |
| S445 | Missing intact GZ returns (None, None); other criterion paths may still run. | C08/C10 [src/digitalmodel/vessel_stability_screening/workflow.py:L445-L445] |
| S448 | Intact heel values use float; empty/missing heel list raises. | no detection check exists for numeric grammar/nonfinite values; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L448-L448] |
| S458 | Direct intact GZ uses float without locale or finiteness guard. | C10 for valid table; no detection check exists for malformed values; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L458-L458] |
| S461 | Intact KN uses float without locale or finiteness guard. | C04/C05 for valid values; no detection check exists for malformed values; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L461-L461] |
| S466 | Cross-curve displacements use float; missing list becomes empty before downstream indexing. | no detection check exists for empty or malformed grid displacement; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L466-L466] |
| S467 | Cross-curve KN cells use float; missing matrix becomes empty. | no detection check exists for empty or malformed matrix; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L467-L467] |
| S473 | bool(enabled) accepts nonempty strings as true, including a textual false. | no detection check exists; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L473-L474] |
| S488 | IMO numeric overrides use float without locale or finiteness guard. | no detection check exists for malformed overrides; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L488-L488] |
| S489 | Omitted override citation preserves the default citation even with custom thresholds. | no detection check exists for provenance of changed thresholds; issue pending (2) [src/digitalmodel/vessel_stability_screening/workflow.py:L489-L490] |
| S494 | Missing criteria mapping becomes empty and absent weather returns None. | C08 guards globally empty evaluation; individual optional weather absence has no detection check exists; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L494-L497] |
| S512 | Weather limiting angle has an implicit default and direct float conversion; default location is cited without reproducing a statutory value. | no detection check exists for omitted/malformed angle; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L512-L512] |
| S520 | Absent lift returns (None, None). | C10 [src/digitalmodel/vessel_stability_screening/workflow.py:L520-L520] |
| S526 | Direct lift moment uses float without locale or finiteness guard. | no detection check exists for malformed moment; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L526-L526] |
| S549 | Absent residual-area-ratio threshold becomes None; supplied ratio uses float. | no detection check exists for malformed ratio; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L549-L549] |
| S550 | Residual-range limit uses an implicit default and float conversion. | no detection check exists for default provenance/malformed limit; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L550-L552] |
| S571 | Absent/disabled max-KG returns empty limits and no governing result. | C10 [src/digitalmodel/vessel_stability_screening/workflow.py:L571-L572] |
| S592 | KG bisection tolerance defaults 0.0001 m, parsed with float; negative/zero/nonfinite values are not checked in this adapter. | no detection check exists for malformed tolerance; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L592-L592] |
| S611 | Absent damage cases return two empty lists; this is intentional optional behaviour. | C03 runs intact-only cases; no detection check exists for malformed falsey damage settings; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L611-L611] |
| S619 | No shared damage criteria stores None; missing per-case criteria later raise. | C01/C08; missing-criteria test in C03 [src/digitalmodel/vessel_stability_screening/workflow.py:L619-L622] |
| S625 | Floodwater density inherits the upstream seawater constant and uses float; no format/unit conversion occurs. | C06 for supplied/default synthetic density; no detection check exists for malformed density; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L625-L625] |
| S689 | Direct damaged conditions set transverse heeling moment zero; supplied heel anchors no-arm analysis. | C10 [src/digitalmodel/vessel_stability_screening/workflow.py:L689-L689] |
| S693 | Direct-condition floodwater mass is None rather than a computed mass. | C10 [src/digitalmodel/vessel_stability_screening/workflow.py:L693-L693] |
| S698 | Without damaged GZ, analysis remains None; configured GZ-dependent thresholds later raise. | C08 [src/digitalmodel/vessel_stability_screening/workflow.py:L698-L698] |
| S705 | Absent wind moment leaves wind arm None; otherwise moment/displacement supplies constant arm. | C03 includes wind-arm test; no detection check exists for every omission combination; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L705-L705] |
| S768 | Damage thresholds use float only when present and not None. | C08 for unknown keys; no detection check exists for numeric grammar/nonfinite values; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L768-L770] |
| S774 | DamageCriteria ValueError is re-raised with context; it is not swallowed. | C08 [src/digitalmodel/vessel_stability_screening/workflow.py:L774-L775] |
| S781 | Flooded-compartment FSM defaults zero and uses float. | C03 includes free-communication validation; no detection check exists for malformed FSM; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L781-L781] |
| S792 | Rectangular floodwater tank density defaults to case density and uses float. | C03 includes floodwater-dimensions test; no detection check exists for malformed density; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L792-L792] |
| S801 | Compartment TCG defaults zero and uses float, potentially suppressing a transverse moment when omitted. | no detection check exists for omitted/malformed TCG; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L801-L801] |
| S803 | bool(free_communication) treats nonempty text as true, while omission defaults false. | no detection check exists for textual flags/omission; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L803-L803] |
| S805 | FloodedCompartment ValueError is re-raised with context. | C03 includes compartment validation; no swallowed exception [src/digitalmodel/vessel_stability_screening/workflow.py:L805-L806] |
| S823 | DirectDamageCondition errors are propagated or re-raised with context. | no detection check exists for every contextual error branch; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L823-L826] |
| S831 | Absent damaged GZ returns None. | C08/C09 [src/digitalmodel/vessel_stability_screening/workflow.py:L831-L831] |
| S834 | Damaged heel list uses float; absence/empty list raises. | no detection check exists for malformed numeric text; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L834-L834] |
| S846 | Direct damaged GZ values use float without locale or finiteness guard. | C10 for supplied valid curve; no detection check exists for malformed values; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L846-L846] |
| S853 | Damaged KN values use float without locale or finiteness guard. | C06 for valid curve; no detection check exists for malformed values; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L853-L853] |
| S859 | Missing case downflooding falls back to vessel value; absent limits return None. | C10 for fallback/capping; C08 for missing required damage input [src/digitalmodel/vessel_stability_screening/workflow.py:L859-L864] |
| S874 | Citation fields are stringified; a nonstring None can become nonempty text rather than missing metadata. | C01 detects missing mapping; no detection check exists for nonstring citation fields; issue pending (2) [src/digitalmodel/vessel_stability_screening/workflow.py:L874-L878] |
| S880 | Citation ValueError is re-raised with context. | C01 [src/digitalmodel/vessel_stability_screening/workflow.py:L880-L881] |
| S952 | Absent intact curve makes exported GZ rows empty, so GZ CSV is omitted. | C10 [src/digitalmodel/vessel_stability_screening/workflow.py:L952-L953] |
| S982 | Required numeric helper raises on None then float-converts other text directly; no locale/finiteness check. | C08 for required-input guards; no detection check exists for all numeric boundaries; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L982-L986] |
| S989 | Optional numeric helper returns None on omission and float-converts supplied text. | C10 for optional valid inputs; no detection check exists for malformed numeric text; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L989-L991] |
| S994 | Relative output paths resolve through supplied config directory/file or current directory; implicit output location can vary. | no detection check exists for every path fallback; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L994-L1006] |
| S1029 | Non-repository output path catches ValueError and returns an absolute display path; this is an explicit path fallback, not a computation fallback. | no detection check exists for public-safe display paths; issue pending (1) [src/digitalmodel/vessel_stability_screening/workflow.py:L1029-L1030] |

No cache-write path, integer conversion, epoch fallback or error-path empty-result cache occurs in the package; the identified empty-return paths are optional output/feature paths, and the four contextual exception handlers rethrow. [src/digitalmodel/vessel_stability_screening/workflow.py:L358-L1030]

| Additional pitfall | Cause and detection | Tracking / date |
|---|---|---|
| Negative GM or excessive free surface must fail the screen | C09 checks both intact and damage failure propagation; [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L292-L306] [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L252-L280] | Preventive coverage, no incident assigned; code pin |
| A hydrostatic table is not a licensed stability result | #2089 seeks a separately qualified native-output/canary route; no detection check exists here for licensed-program agreement; the workflow only consumes supplied data; [docs/plans/2026-09-11-issue-2089-ghs-canary.md:L14-L20] | #2089; plan dated 2026-09-11 |
| Fixture prose can misidentify the governing KG criterion | Intact workflow docstring names a GM-governed limit while its actual configured lifting test expects lifting to govern; C03 executes the assertion but no detection check exists for contradictory prose; [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L9-L11] [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L171-L190] | issue pending (3); code pin |
| Inline citations are not a resolved shared sidecar or signed QA appendix | Fields are strings serialized into criterion rows; no detection check exists for shared resolver or full appendix conformance; [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L63-L83] [src/digitalmodel/vessel_stability_screening/workflow.py:L933-L944] | issue pending (2); code pin |

## 7. Lessons log

Append future entries with date, change reference, lesson, evidence, pack change and shared-log reference; use public evidence or an existing private-sidecar entry identifier only.

| Date | Change reference | Lesson | Evidence | Pack change | Shared-log reference |
|---|---|---|---|---|---|
| 2026-10-18 | Initial pack | Keep fixture arithmetic, export regression and licensed-program evidence distinct | Synthetic router test asserts supplied direct properties; licensed native evidence is a separate planned comparison; [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L186-L196] [docs/plans/2026-09-11-issue-2089-ghs-canary.md:L59-L65] | Sections 4, 6 and 9 separate these evidence types | Not created; linked follow-up #2089 |

## 8. Ownership

Review contract: engineering calculations quality manager, supported by a deputy engineering calculations quality reviewer on a provider independent of the producer. Producing discipline: engineering calculations. Maintainers assign people and resolve unassigned review responsibility before clearance. Escalate disputed method-family classifications, tolerance changes, source ambiguity and missing reference inputs to the family quality manager; maintainers resolve cross-family scope. These are review requirements, not a record of completed review. (AQG r0 section 11; module knowledge-pack lifecycle, revision 0, section 4.)

## 9. Dependencies and implementation inventory

| Dependency / consumer | Contract and inventory evidence |
|---|---|
| `naval_architecture` intact/damage core | Imports table, loading, GZ, criteria, KG-limit and flooding implementations; upstream registry domain is development; [src/digitalmodel/vessel_stability_screening/workflow.py:L130-L163] [docs/registry/module-routing.yaml:L247-L261] |
| `damage_stability` helpers | `GZCurve` calls shared GZ interpolation and integration; [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L347-L357] [src/digitalmodel/naval_architecture/damage_stability.py:L27-L75] |
| Engine route | Basename route imports and calls this router; [src/digitalmodel/engine.py:L519-L524] |
| CSV/report consumer | CSV outputs carry criteria citations and report-ready result columns; no report-pack call is present in `router`; [src/digitalmodel/vessel_stability_screening/workflow.py:L262-L352] |
| Standard library | CSV reading/writing, dataclass conversion and filesystem paths; no solver import in package imports; [src/digitalmodel/vessel_stability_screening/workflow.py:L125-L163] |
| GHS: package occurrence 1 | Docstring identifies a separate licensed cross-check, with no version specified; read as text only; [src/digitalmodel/vessel_stability_screening/workflow.py:L15-L18] |
| GHS: package occurrence 2 | Schema's direct-damage example labels supplied conditions as coming from the licensed tool; no invocation or version; read as text only; [src/digitalmodel/vessel_stability_screening/workflow.py:L101-L108] |
| GHS: package occurrence 3 | Governance string repeats the separate licensed cross-check; no invocation or version; read as text only; [src/digitalmodel/vessel_stability_screening/workflow.py:L167-L172] |

The related upstream intact core names GHS as table source and cross-check, and the upstream damage core names it as direct-condition source and cross-check; these are dependency statements rather than execution evidence. [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L7-L9] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L34-L37] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L11-L14] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L46-L48]

Implementation-map audit: reference `origin/codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. This section's map citation uses that map commit, rather than the code pin. [docs/registry/implementation-map.yaml:L1-L3]

Map-family membership was computed from every family's `implementations[].path` prefix against the package header path: zero in-package rows, zero module map families, empty required inventory, zero counted families. Counted-family grep: `NOT_APPLICABLE`, because no family has a row in the package; no search signal qualifies for a counted-family search at this revision. [docs/registry/implementation-map.yaml:L345-L26502]

Reproduce the membership calculation with Python `yaml.safe_load` on the named map, then filter each family's `implementations` by `row["path"].startswith("src/digitalmodel/vessel_stability_screening/")`; ranking and case-sensitive Python `re` searches become required when in-package rows exist. No subpackage is excluded.

A separate repository-wide search, including `scripts/`, identifies the explicit core implementations below; these are outside the package and belong in the map follow-up rather than counted-family misses. [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L198-L204] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L195-L254] [src/digitalmodel/naval_architecture/damage_stability.py:L87-L149]

```sh
git grep -n -E 'vessel_stability_screening|damage_stability_screening|rectangular_fsm_t_m|weather_criterion_cfr_170_170|max_kg_screening' CODE_PIN -- '*.py'
git grep -n -E 'FSC|free.surface|KN.*KG|weather.*170|intact.stability|maximum.KG|added.weight|damage.stability' CODE_PIN -- '*.py'
```

| Related implementation | Classification and inventory follow-up |
|---|---|
| `naval_architecture/vessel_stability_screening.py`: `rectangular_fsm_t_m`, `build_loading_condition`, `HydrostaticTable`, `solve_equilibrium`, `GZCurve`, `gz_from_kn`, `kn_at_displacement`, `imo_intact_criteria`, `weather_criterion_cfr_170_170`, `equilibrium_heel_deg`, `lifting_check`, `max_kg_screening`, `governing_kg_limit` | Actual upstream calculations, described in section 3; add stability/loading/criteria families and their workflow adapters to map follow-up, issue pending (4); [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L198-L324] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L331-L409] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L493-L568] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L599-L982] |
| `naval_architecture/damage_stability_screening.py`: `FloodedCompartment.floodwater_t`, `added_weight_equilibrium`, `damage_gz_analysis`, `evaluate_damage_case` | Upstream flooding/survival calculations, described in section 3; issue pending (4); [src/digitalmodel/naval_architecture/damage_stability_screening.py:L132-L137] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L195-L254] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L277-L371] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L508-L665] |
| `naval_architecture/damage_stability.py`: `interpolate_gz`, `gz_area_under_curve`, `check_imo_intact_stability`, `lost_buoyancy_sinkage`, `flooded_gm` | GZ helpers plus another intact-criteria and flooding route; outside-package map gap/follow-up, not a counted miss; issue pending (4); [src/digitalmodel/naval_architecture/damage_stability.py:L27-L182] |
| `naval_architecture/stability.py`: `gz_from_cross_curves`, `gz_with_tcg_correction`, `free_surface_correction` | Independent GZ/free-surface formulas in imperial units; the cited variant delegates to the base GZ function; outside-package map follow-up, issue pending (4); [src/digitalmodel/naval_architecture/stability.py:L26-L122] |
| `naval_architecture/floating_platform_stability.py`: `compute_gm`, `compute_gz_curve`, `compute_area_under_gz`, `compute_wind_heel`, `check_intact_stability` | Additional GM/GZ/wind/criteria implementations; outside-package map follow-up, issue pending (4); [src/digitalmodel/naval_architecture/floating_platform_stability.py:L112-L330] |
| `naval_architecture/loading_computer.py`: `solve_equilibrium`, `gz_wall_sided`, `intact_stability`, `damage_stability` | Box-hull equilibrium, wall-sided GZ and damage route, partly delegates shared criteria; outside-package map follow-up, issue pending (4); [src/digitalmodel/naval_architecture/loading_computer.py:L205-L367] |
| `naval_architecture/workflow_calculations.py`: `run_damage_stability`, `run_platform_stability` | Adapters delegate to the listed damage/platform calculations; helpers rather than independent formulas; [src/digitalmodel/naval_architecture/workflow_calculations.py:L10-L80] |
| Current package `router`, parsers and row builders | Delegates calculations to the listed core, serializes outputs and constructs input records; `_limit_angle` selects supplied immersion limits, not a second stability solver; [src/digitalmodel/vessel_stability_screening/workflow.py:L185-L352] [src/digitalmodel/vessel_stability_screening/workflow.py:L358-L864] |

The broad repository search returned 668 matching lines in 155 Python paths, including 11 script paths; incidental CFD, documentation-generation and test vocabulary hits were retained as discovery evidence, without treating the broad search as an exhaustive implementation audit. Remaining map classification is issue pending (4).

Map follow-up must name every section-3 method family: hydrostatic interpolation, loading/free-surface moments, equilibrium/trim, GZ/KN interpolation/integration, intact criteria, weather criteria, lifting heel/energy, fluid-KG limits, added-weight flooding, direct damaged-condition screening and survival criteria. Those families are not counted until the map has corresponding in-package rows; issue pending (4). [src/digitalmodel/vessel_stability_screening/workflow.py:L185-L253] [src/digitalmodel/vessel_stability_screening/workflow.py:L609-L725]

## 10. Coverage and open items

Package files read in full: `__init__.py` and `workflow.py`, including their embedded schema and all parsing/output paths; the package contains no further Python subpackage. [src/digitalmodel/vessel_stability_screening/__init__.py:L1-L5] [src/digitalmodel/vessel_stability_screening/workflow.py:L1-L1030]

| Read surface | Scope / evidence |
|---|---|
| `docs/domains/ship-design/vessel-stability-screening.md` | Method, governance, output and synthetic-fixture claims; derived test counts supersede historical prose; [docs/domains/ship-design/vessel-stability-screening.md:L1-L184] |
| `docs/maps/digitalmodel-operator-map.md` | Matching module row; [docs/maps/digitalmodel-operator-map.md:L106-L106] |
| `docs/plans/2026-09-11-issue-2089-ghs-canary.md` and `.html` | Discovery documents naming this workflow; separate native-evidence boundary, without execution; [docs/plans/2026-09-11-issue-2089-ghs-canary.md:L14-L20] [docs/plans/2026-09-11-issue-2089-ghs-canary.html:L14-L20] |
| `docs/registry/module-routing.yaml` | Existing module route and maturity; [docs/registry/module-routing.yaml:L619-L624] |
| Base configuration | Default configuration only, no worked input condition; [src/digitalmodel/base_configs/modules/vessel_stability_screening/vessel_stability_screening.yml:L1-L19] |
| Core and workflow tests | 35 intact core + 17 intact workflow + 21 damage core + 20 damage workflow = 93 test definitions; all run, source-derived rather than inventory count; [tests/naval_architecture/test_vessel_stability_screening.py:L153-L505] [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L122-L360] [tests/naval_architecture/test_damage_stability_screening.py:L179-L513] [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L177-L409] |
| Supporting calculations and citation contract | Intact/damage core and shared GZ helpers; local Citation inspected; shared calculation-citation contract read separately to identify the inline/sidecar distinction; [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L63-L83] [src/digitalmodel/naval_architecture/damage_stability.py:L27-L75] |

No package README or package-local test folder was found in the pinned package tree; its test routes are external. [docs/registry/module-routing.yaml:L619-L624]

| Open item | Responsible role | Tracking |
|---|---|---|
| External vertical/transverse datum (the section-2 UNVERIFIED claim), numeric grammar, unit declaration and complete default/optional-path validation; silent-sweep boundary gaps in section 6 | Maintainer, engineering calculations | issue pending (1) |
| Exact statutory titles (the section-3 UNVERIFIED claim), clause applicability, licensed thresholds and shared citation-sidecar/full G5 appendix conformance; review override provenance and nonstring citation fields | Maintainer, engineering calculations | issue pending (2) |
| Contradictory governing-KG fixture prose; retain executable assertion as evidence, not its conflicting docstring | Maintainer, engineering calculations | issue pending (3) |
| Stability-method map coverage, including helpers and other implementations in section 9 | Maintainer, engineering calculations | issue pending (4) |
| Missing licensed-program comparison, runtime/version qualification and accepted native fixture; no detection check exists in these module tests; no full-vessel statutory validation claimed | Maintainer, engineering calculations | #2089; [docs/plans/2026-09-11-issue-2089-ghs-canary.md:L59-L65] |

UNVERIFIED claims are consolidated into the first two open items; no private dataset, client-derived value or sidecar entry is used. Synthetic evidence remains limited to the documented box-table assumptions and the smooth extension beyond deck-edge immersion. [tests/naval_architecture/test_vessel_stability_screening.py:L5-L37]
