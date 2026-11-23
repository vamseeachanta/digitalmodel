# Assessment knowledge pack

| Header | Value |
|---|---|
| Module | asset_integrity/assessment |
| Repository | digitalmodel |
| Package path | `src/digitalmodel/asset_integrity/assessment` |
| Pack revision | 0, 2026-10-16 |
| Code revision | `bb18c1a346b31b4ce975befa41fc84d9a5ff29e5` |
| Analysis family | Engineering calculations (EC); AQG r0, Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The canonical metal-loss route is `assess_component(FFSComponent, grid)`: parsing, morphology routing, required-wall screening, RSF assessment, decision and measurement sufficiency return one `FFSAssessmentResult`. [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L137-L229]

The package also implements pit-field characterization, a dedicated synthetic pitting workflow, flat-plate surface-crack FAD calculations and a Level 3 handoff; the handoff assembles data and recommendations rather than executing advanced analysis. [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L139-L168] [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L242-L299] [src/digitalmodel/asset_integrity/assessment/level3_escalation.py:L180-L201]

The registry calls the containing asset-integrity domain stable; this is a routing maturity label, while the source audit withholds complete Part 4/5 qualification from these simplified screens. [docs/registry/module-routing.yaml:L14-L23] [docs/domains/asset-integrity/uniform-loss-source-audit-2026-10-06.md:L42-L52]

Scope includes all Python definitions in this package; the legacy `common` engine, structural/pipeline calculators and composite-repair design remain dependencies rather than implementations covered here. [src/digitalmodel/asset_integrity/assessment/__init__.py:L15-L66] [docs/domains/asset-integrity/ffs-architecture.md:L5-L20] [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L271-L276]

## 2. Inputs and outputs

### Inputs

| Input and route | Unit / convention | Range enforced and default | Evidence |
|---|---|---|---|
| `GridParser.from_dataframe/from_csv/from_numpy` | Remaining wall in inches internally; mm converted; correction factor applied first | Empty DataFrame and unknown units rejected; numeric conversion uses `astype(float)`; no general finite/positive-wall guard | [src/digitalmodel/asset_integrity/assessment/grid_parser.py:L34-L83] [src/digitalmodel/asset_integrity/assessment/grid_parser.py:L111-L123] [src/digitalmodel/asset_integrity/assessment/grid_parser.py:L125-L155] |
| Grid coordinates | Rows axial, columns circumferential; index spacing interpreted as inches downstream | No spatial origin, load phase or global coordinate frame input; minimum positive index gap used, with unit-spacing fallback | [src/digitalmodel/asset_integrity/assessment/grid_parser.py:L3-L15] [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L278-L291] |
| `FFSComponent` geometry / pressure / strength | OD and nominal wall in inches; pressure and SMYS in psi | Required geometry/pressure; SMYS optional in data model but needed by B31 screening; no dataclass physical-range validation | [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L55-L68] [src/digitalmodel/asset_integrity/assessment/level1_screener.py:L160-L167] |
| Component corrosion rate and FCA | in/year and inches | Both default to zero; RSFa uses the declared code default; coordinator FCA handling differs from dedicated workflow | [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L59-L68] [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L181-L220] |
| `Level1Screener` code/factors | `B31.8`, `B31.4`, `ASME_VIII_DIV1`; stress in psi | Named code checked; declared B31 and weld factors and allowable-stress fallback are assumptions requiring a design basis | [src/digitalmodel/asset_integrity/assessment/level1_screener.py:L60-L97] |
| `Level2Engine` | GML or LML; all lengths inches, RSFa dimensionless | Type checked; reference wall is supplied `t_min_in`; nonpositive reference has special fallbacks | [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L75-L93] [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L126-L140] [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L165-L191] |
| Dedicated pitting `_read_input` | OD, nominal wall, required wall, axial pitch, FCA in inches | OD > twice WT; required wall positive and <= WT; FCA nonnegative and < WT; finite rectangular numeric readings in (0, WT]; no strings/booleans; no relaxation of declared screening floors | [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L17-L44] |
| Pitting pitch | Same axial and circumferential pitch; row and column coordinates constructed from explicit pitch | Positive finite scalar required in workflow; low-level API instead infers index spacing | [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L17-L44] [src/digitalmodel/asset_integrity/assessment/pitting.py:L152-L156] [src/digitalmodel/asset_integrity/assessment/pitting.py:L191-L206] |
| Shared `decide` and derating callables | Margin dimensionless; remaining life years or time Quantity; pressure/force limits unit tagged | Unknown asset classes and wrong dimensions rejected; nonfinite margin escalates; default bands are shared policy assumptions | [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L102-L108] [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L182-L227] [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L339-L369] [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L395-L414] |
| `assess_crack_like_flaw` | a, c, t, half-width in mm; membrane/bending stress and elastic modulus MPa; toughness MPa sqrt(m); Charpy J | Surface flaw: 0 < a < t, a/c <= 1, c/half-width < 0.5; c is half-length; front angles identify deepest/surface points; no full finite-material validation | [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L117-L145] [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L242-L285] |
| `FFSReport` and `store_report` | HTML string; optional file output is UTF-8 | Date defaults to current date; report persistence only with `output_dir`, relative to supplied configuration directory | [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L79-L125] [src/digitalmodel/asset_integrity/assessment/screening_report.py:L44-L54] |

### Outputs

| Output | Quantity / meaning / limitations | Evidence |
|---|---|---|
| `FFSAssessmentResult` | Stage dictionaries, thickness in inches, RSF and Folias dimensionless, life years, rerated pressure psi, sufficiency action; `passes` means ACCEPT or MONITOR | [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L71-L134] |
| `Level1Screener.evaluate` | Required/measured wall and signed margin in inches; nonnegative margin passes | [src/digitalmodel/asset_integrity/assessment/level1_screener.py:L119-L142] |
| `Level2Engine.evaluate` | RSF, RSFa, minimum/mean wall, Folias; LML also flaw length, thickness ratio, shell parameter and applicability | [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L99-L120] [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L215-L227] |
| `PitFieldCharacterization` | Four-connected pit count, depth ratios, nearest-centroid spacing, pitted-region dimensions; spacing assumes equal pitches | [src/digitalmodel/asset_integrity/assessment/pitting.py:L87-L125] [src/digitalmodel/asset_integrity/assessment/pitting.py:L152-L240] |
| Dedicated pitting result | Characterization, two levels, final decision, HTML, optional path; `qualification=synthetic_formula_anchor_only`; no life or pressure rerating | [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L118-L145] [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L149-L168] |
| Crack result and critical depth | Kr/Lr/envelope, acceptance boolean, load-line margin, K and reference stress; critical-depth bisection returns mm | [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L223-L239] [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L280-L347] |
| Sufficiency / escalation | Reasons and concrete extra-reading actions; handoff echoes geometry, grid summary, loads, material and results | [src/digitalmodel/asset_integrity/assessment/measurement_sufficiency.py:L47-L66] [src/digitalmodel/asset_integrity/assessment/level3_escalation.py:L270-L301] |

## 3. Method and governing standards

All licensed-standard references below describe the source's declared basis: clause text not checked against the licensed copy; no tabulated standard values are reproduced. [docs/domains/asset-integrity/uniform-loss-source-audit-2026-10-06.md:L170-L179]

| Method / implementing definition | Declared reference, edition and clause locator | Bound on interpretation |
|---|---|---|
| `Level1Screener._t_min_b31` and `_t_min_asme8_ug27` | ASME B31.8, 2022, pressure design; ASME B31.4, 2022, pressure design; BPVC Section VIII Division 1, 2023, UG-27(c)(1) | Code-required wall arithmetic, not complete API 579 applicability/procedure; outside radius used by VIII approximation. [src/digitalmodel/asset_integrity/assessment/level1_screener.py:L25-L28] [src/digitalmodel/asset_integrity/assessment/level1_screener.py:L148-L188] [docs/domains/asset-integrity/uniform-loss-source-audit-2026-10-06.md:L42-L46] |
| `Level2Engine._evaluate_gml` | API 579-1/ASME FFS-1, Fitness-For-Service, 2021, Part 4 general metal loss, Level 2 | Valid-cell mean divided by required wall and capped; this source audit does not qualify it as complete Part 4. [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L126-L150] [docs/domains/asset-integrity/uniform-loss-source-audit-2026-10-06.md:L44-L46] |
| `Level2Engine._evaluate_lml`, `_folias_factor`, `_shell_parameter` | API 579-1/ASME FFS-1, Fitness-For-Service, 2021, Part 5 local metal loss; source also cites Part 4 Folias table | Flaw length counts degraded rows rather than bounding span; required wall is reference; no width term; coefficient/source binding remains unresolved in #2287. [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L157-L227] [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L233-L276] [docs/domains/asset-integrity/uniform-loss-source-audit-2026-10-06.md:L13-L22] |
| `FFSRouter.classify` | API 579-1/ASME FFS-1, 2021, Parts 4/5/6 morphology | Explicit heuristic thresholds and force override; not a complete standard applicability check. [src/digitalmodel/asset_integrity/assessment/ffs_router.py:L43-L55] [src/digitalmodel/asset_integrity/assessment/ffs_router.py:L95-L132] |
| `characterize_pit_field`, `screen_pitting_level1`, `assess_pitting_level2_equivalent_lta` | API 579-1/ASME FFS-1, Fitness-For-Service, 2021, Part 6 pitting; Part 5 equivalent local thin area | Characterization plus a closed-form screen and delegated simplified LML estimate; no pit-couple charts; low-level wording claims a bound, while the dedicated workflow explicitly withholds that qualification. [src/digitalmodel/asset_integrity/assessment/pitting.py:L24-L58] [src/digitalmodel/asset_integrity/assessment/pitting.py:L367-L386] [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L64-L70] |
| `decide`, `remaining_life`, `scaled_limit`, `reduced_mawp` | API 579-1/ASME FFS-1, 2021, assessment flowchart / rerating; ASME PCC-2, 2022, repair methods | Policy tree, linear thickness-margin/rate life, and callable limit scaling; quantity tags checked independently of physical validity. [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L38-L51] [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L230-L243] [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L260-L291] [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L413-L449] |
| Shared action labels | API RP 2SK, mooring; API RP 2SIM, jackets | Named only as reference families in `ffs_decision`; no standard-specific strength equation or clause/edition is supplied by that action map. [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L48-L51] [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L157-L179] |
| Repair recommendation integration | ISO 24817, composite repairs for pipework; ASME PCC-2, non-metallic composite repair articles | Coordinator delegates to an external candidate-screening helper; no laminate design; edition and precise clause not recorded here. [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L271-L276] [src/digitalmodel/asset_integrity/composite_repair.py:L1-L19] |
| `fad_curve_option1`, `lr_max`, `reference_stress_surface_flaw`, `kmat_from_charpy` | BS 7910, Guide to methods for assessing the acceptability of flaws in metallic structures, 2013+A1:2015, Option 1 failure-assessment curve, Annex P reference stress, Annex J toughness correlation | The assessment uses Option 1; API Part 9 Level 2 is a separate function, with a 2016 source label. [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L64-L111] [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L179-L217] [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L280-L286] [src/digitalmodel/codes.py:L54-L57] |
| `newman_raju_k`, `critical_flaw_depth_mm` | Newman and Raju (1981), finite-plate semi-elliptical surface-flaw solution, as cited in code | Membrane plus bending; deepest and surface points; critical depth bisects the assessment with geometry/depth caps; no cylinder-specific K solution. [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L34-L45] [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L117-L173] [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L302-L347] |
| `MeasurementSufficiency.evaluate`; Level 3 trigger/handoff | API 579-1/ASME FFS-1, Part 4 measurement practice and higher-level assessment | Sufficiency thresholds are configurable engineering defaults, not code mandates; handoff does not execute FEA. [src/digitalmodel/asset_integrity/assessment/measurement_sufficiency.py:L26-L31] [src/digitalmodel/asset_integrity/assessment/level3_escalation.py:L22-L40] |

Edition/provenance defect: `codes.BS_7910` and the code register say 2019, whereas the canonical Option 1 definition says 2013+A1:2015; API labels also differ between the 2016 crack curve and the coordinator/report's 2021 basis; these are not silently reconciled (#2157, #2175, issue pending (2)). [src/digitalmodel/codes.py:L52-L57] [docs/domains/codes-register.md:L19-L20] [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L64-L105] [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L509-L514]

`FFSAssessmentResult.code_reference` is a code-label string, not a per-equation `Citation` sidecar; the report methods assemble text references, while the pitting result exposes qualification and limitations. [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L96-L96] [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L109-L134] [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L509-L559] [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L139-L168]

UNVERIFIED: U1 — Exact-edition normative qualification of these formulas and completeness of their licensed-standard procedures; follow #2287, #2175 and issue pending (2).

UNVERIFIED: U2 — Independent published-case qualification of the equivalent-LTA pitting representation; follow #2182 and #2345.

UNVERIFIED: U3 — Editions and clause-level bindings for API RP 2SK, API RP 2SIM and ISO 24817 integration; follow issue pending (2).

## 4. Validated worked examples

Validated: 0; the pitting record explicitly limits its formula agreement to workflow wiring and withholds published-case qualification; the Folias validation record similarly withholds complete Part 5 qualification. [docs/domains/asset-integrity/pitting-validation-2026-10-09.md:L15-L20] [docs/domains/asset-integrity/pitting-validation-2026-10-09.md:L41-L54] [docs/domains/asset-integrity/ffs-validation-record-2026-06-27.md:L13-L20]

Regression examples (PASS means the observed offline test result at the code pin, not standard qualification):

| ID / input and source | Expected output and tolerance basis | Existing test node ID | BASE / AFTER |
|---|---|---|---|
| R1, synthetic mm grid in test | 0.500 inch after conversion; relative tolerance 1e-4, the test assertion | `tests/asset_integrity/test_ffs_assessment.py::TestGridParserFromDataFrame::test_mm_to_inch_conversion` | PASS / PASS. [tests/asset_integrity/test_ffs_assessment.py:L100-L104] |
| R2, synthetic unequal-NaN grid in test | Mean wall 0.500 inch; relative tolerance 1e-12, arithmetic check | `tests/asset_integrity/test_ffs_1094_quickwins.py::test_gml_t_am_is_area_average_not_mean_of_column_means` | PASS / PASS. [tests/asset_integrity/test_ffs_1094_quickwins.py:L17-L30] |
| R3, synthetic pitting configuration in test | Five pits; RSF from explicit closed form in the test, relative tolerance 1e-6 / absolute 1e-12 from pytest default; Level 1 fail, Level 2 estimate exercised | `tests/asset_integrity/test_pitting_workflow.py::test_pitted_grid_routes_and_workflow_returns_formula_anchor` | PASS / PASS. [tests/asset_integrity/test_pitting_workflow.py:L12-L36] |
| R4, synthetic-input-v1 durable workflow | RSF equals committed formula anchor with pytest default tolerance; report output exists; input file is registry-owned | `tests/workflows/test_durable_workflows.py::test_workflow_registry[api579-pitting-screen]` | PASS / PASS, temporary copy. [docs/registry/workflows.yaml:L1275-L1285] [tests/workflows/test_durable_workflows.py:L85-L92] [tests/workflows/test_durable_workflows.py:L1723-L1724] |

The crack test's legacy comparison delegates to the same canonical function, so it is not an independent validation reference; data-derived flaw cases are deliberately not reproduced in this public pack. [tests/asset_integrity/test_crack_fad.py:L36-L55] [tests/asset_integrity/test_crack_fad.py:L123-L140]

## 5. QA gates

AQG r0, section 10, Python calculations, supplies G0/G1/G2; G3 and G4 are NOT_APPLICABLE to this offline calculation package, and G5 applies to evidence and report output.

| Gate | Applicability / executable checks | Numeric pass criterion and unit | Evidence |
|---|---|---|---|
| G0 | C01: `GridParser.from_dataframe`, R1 | Converted wall differs from 0.500 inch by <= 0.00005 inch | R1 assertion. [tests/asset_integrity/test_ffs_assessment.py:L100-L104] |
| G0 | C02: pitting `_read_input` rejects invalid grids | 4/4 invalid inputs raise ValueError; 0 invalid grids accepted (cases) | `tests/asset_integrity/test_pitting_workflow.py::test_invalid_measurements_rejected`. [tests/asset_integrity/test_pitting_workflow.py:L49-L54] |
| G0 | C03: `MeasurementSufficiency.evaluate`, sparse grid | 9 readings produce TAKE_MORE and exactly 6 added readings (count) | `tests/asset_integrity/test_measurement_sufficiency.py::test_take_more_too_few_readings`. [tests/asset_integrity/test_measurement_sufficiency.py:L65-L75] |
| G0 | C04: `newman_raju_k` validity errors | 2/2 out-of-domain geometries rejected; 0 false passes (cases) | `tests/asset_integrity/test_crack_fad.py::test_newman_raju_validity_errors`. [tests/asset_integrity/test_crack_fad.py:L95-L99] |
| G1 | C11: imports and collected assessment tests, with frozen baseline failure identities | 0 new failures across 472 collected cases; 1/1 copied durable workflow case passes (cases) | Test record in section 10; baseline defect in section 6. [tests/asset_integrity/test_fad_curves.py:L159-L162] [tests/asset_integrity/test_fad_curves.py:L195-L198] |
| G2 | C21: `_evaluate_gml` arithmetic, R2 | Mean error <= 5e-13 inch; column-mean bias must not reproduce | R2 assertion. [tests/asset_integrity/test_ffs_1094_quickwins.py:L17-L30] |
| G2 | C22: pitting workflow independent recomputation of the implemented relation, R3 | RSF error <= max(1e-12, 1e-6 times absolute reference), dimensionless | R3 assertion; arithmetic only, method qualification remains U2. [tests/asset_integrity/test_pitting_workflow.py:L29-L36] |
| G2 | C23: `lr_max`, both FAD curves versus written closed forms | Cutoff absolute error <= 1e-12; curve absolute error <= 1e-9, dimensionless | `tests/asset_integrity/test_fad_curves.py::test_lr_max_hand_value`, `tests/asset_integrity/test_fad_curves.py::test_option1_matches_hand_values`, `tests/asset_integrity/test_fad_curves.py::test_api579_level2_matches_hand_values`. [tests/asset_integrity/test_fad_curves.py:L40-L77] |
| G3 | NOT_APPLICABLE — no dynamic integration in the covered route | 0 dynamic runs required (runs) | Route stages. [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L157-L229] |
| G4 | NOT_APPLICABLE — no diffraction model or mesh in the covered route | 0 diffraction runs required (runs) | Route stages. [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L157-L229] |
| G5 | C51: `FFSReport.generate_screening_html` and persisted result | 0 byte differences on readback; 0 unescaped script tags (count) | `tests/asset_integrity/test_pitting_workflow.py::test_report_write_and_escaping`. [tests/asset_integrity/test_pitting_workflow.py:L57-L64] |
| G5 | C52: pitting qualification and limit statements | Exact qualification token present (1); unsupported bound phrase absent (0 occurrences) | `tests/asset_integrity/test_pitting_workflow.py::test_report_does_not_claim_a_qualified_pitting_bound`, `tests/asset_integrity/test_pitting_workflow.py::test_pitting_result_marks_published_case_qualification_unestablished`. [tests/asset_integrity/test_pitting_workflow.py:L180-L198] |
| G5 | C53: `FFSReport.generate_html` embeds supplied sufficiency | 3/3 required section/action tokens present (count) | `tests/asset_integrity/test_measurement_sufficiency.py::test_ffs_report_includes_sufficiency_section`. [tests/asset_integrity/test_measurement_sufficiency.py:L200-L222] |

These 11 checks cover executable component behavior; they do not supply an input-fingerprint gate record or independent signature for the generated report, which remains issue pending (3). [src/digitalmodel/asset_integrity/assessment/screening_report.py:L21-L54] [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L149-L199]

## 6. Known failure modes and pitfalls

Silent-failure and assumption sweep (rows enumerate distinct sites or one coupled branch family; deliberate sentinels and data-holder defaults are retained with their disposition):

sweep: 83 found

Commands run against all package Python files at the code pin (the expression also finds deliberate non-error defaults):
```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|default|float\(|int\(|units|spacing|cache' src/digitalmodel/asset_integrity/assessment --glob '*.py'
```
The two decision lookup handlers re-raise unknown values instead of swallowing errors. [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L182-L189] [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L463-L469]

The inspection classified each site below; data-holder empty-list/dict factories initialize records rather than recover from errors, integer casts of array counts are internal reductions, and the package has no `continue` record-skip statement or empty-result cache writer. [src/digitalmodel/asset_integrity/assessment/pitting.py:L87-L125] [src/digitalmodel/asset_integrity/assessment/measurement_sufficiency.py:L47-L66] [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L71-L100] [src/digitalmodel/asset_integrity/assessment/level3_escalation.py:L54-L76]

| Site | Symptom / cause or intentional disposition | Detection and follow-up |
|---|---|---|
| S01 | Default input is inches, with multiplicative correction; a missing unit/factor changes all downstream wall values. | C01; #2339. [src/digitalmodel/asset_integrity/assessment/grid_parser.py:L34-L50] |
| S02 | External cells are converted with astype(float); locale separators are not normalized and finite positivity is not enforced. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/grid_parser.py:L70-L70] |
| S03 | Optional all-NaN row removal changes the coordinate lattice; no post-drop empty-grid check. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/grid_parser.py:L80-L81] |
| S04 | CSV leading/footer skips and index/header choices intentionally discard records; no discarded-record audit. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/grid_parser.py:L86-L123] |
| S05 | NumPy route defaults to inches and correction factor; it rejects dimensionality but not finite/positive values. | C01 covers conversion only; issue pending (1). [src/digitalmodel/asset_integrity/assessment/grid_parser.py:L125-L155] |
| S06 | Minimum ignores NaN; an all-NaN grid yields an unknown scalar. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/grid_parser.py:L161-L164] |
| S07 | Maximum ignores NaN; an all-NaN grid yields an unknown scalar. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/grid_parser.py:L166-L169] |
| S08 | Mean is a mean of column means and is biased with unequal missing-cell counts, unlike Level2Engine. | no detection check exists for GridParser mean; #1094. [src/digitalmodel/asset_integrity/assessment/grid_parser.py:L171-L174] |
| S09 | Fixed loss/pitting thresholds determine morphology without a complete standard applicability assessment. | no detection check exists for qualification; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_router.py:L43-L53] |
| S10 | force_type bypasses morphology and returns no pitting metrics. | no detection check exists for override suitability; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_router.py:L95-L111] |
| S11 | Valid-cell denominator is clamped; nonpositive nominal thickness gives zero background ratio. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/ffs_router.py:L148-L152] |
| S12 | No valid cells returns zero degraded fraction, which can route as LML. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/ffs_router.py:L174-L179] |
| S13 | Default B31 and weld factors embed a design basis that is not selected from the component conditions. | no detection check exists for basis; #2287. [src/digitalmodel/asset_integrity/assessment/level1_screener.py:L35-L37] |
| S14 | Missing allowable stress falls back to a yield-strength fraction. | no detection check exists for allowable-stress suitability; #2287. [src/digitalmodel/asset_integrity/assessment/level1_screener.py:L91-L97] |
| S15 | B31 temperature and longitudinal-joint factors are fixed in dispatch. | no detection check exists for actual conditions; #2287. [src/digitalmodel/asset_integrity/assessment/level1_screener.py:L103-L116] |
| S16 | B31 formula checks missing strength, but not positive finite geometry, pressure or factor. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/level1_screener.py:L160-L167] |
| S17 | Default RSFa and geometry values are accepted without physical-range validation. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L75-L93] |
| S18 | GML has no explicit all-missing-grid guard before averaging and verdict. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L133-L140] |
| S19 | Nonpositive reference wall uses infinity before capping. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L137-L137] |
| S20 | GML clips RSF to unity, hiding over-reference wall in the ratio alone. | C21 checks the mean, not invalid inputs; #2287. [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L138-L138] |
| S21 | LML uses required wall as reference and fixed nominal-loss threshold; outside-flaw evidence/FCA is not checked. | no detection check exists on this public engine; #2345. [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L169-L174] |
| S22 | Counts degraded rows, not their bounding span; extent uses inferred pitch. | no detection check exists for separated-row gaps; #2287. [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L176-L182] |
| S23 | Nonpositive reference wall substitutes unity thickness ratio, then caps. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L189-L191] |
| S24 | Nonpositive denominator becomes zero RSF rather than a geometry error. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L205-L211] |
| S25 | Invalid inside diameter/reference wall returns unity Folias factor. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L253-L254] |
| S26 | Folias floor/cap restricts numeric values; out-of-range flag depends on Rt entering the calculation. | no detection check exists for source qualification; #1094. [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L258-L266] |
| S27 | Degenerate geometry returns zero shell parameter. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L269-L276] |
| S28 | Index float conversion swallows TypeError/ValueError and substitutes unit pitch; no locale-number parsing. | no detection check exists; #2339. [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L281-L284] |
| S29 | Single index or empty index substitutes unit pitch. | no detection check exists for direct API; #2339. [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L285-L286] |
| S30 | Nonpositive gaps are filtered; if none remain uses unit pitch; otherwise minimum positive gap hides irregular pitch. | no detection check exists; #2339. [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L287-L291] |
| S31 | Nonfinite readings are excluded from pit population and count. | no detection check exists for excluded-data disclosure; issue pending (1). [src/digitalmodel/asset_integrity/assessment/pitting.py:L152-L159] |
| S32 | No pits or no valid cells produces zero depths/counts and nominal-wall ligament/mean; spacing is None. | no detection check exists for empty low-level input; #2339. [src/digitalmodel/asset_integrity/assessment/pitting.py:L161-L186] |
| S33 | Pit-centroid distances use axial pitch for both axes; one pit leaves spacing None. | no detection check exists for unequal physical pitch; #2339. [src/digitalmodel/asset_integrity/assessment/pitting.py:L191-L206] |
| S34 | Missing nonpit background substitutes mean pitted wall. | no detection check exists on direct API; #2345. [src/digitalmodel/asset_integrity/assessment/pitting.py:L230-L232] |
| S35 | No detected pits returns Level 1 ACCEPT without measured-wall/FCA comparison. | no detection check exists on direct API; #2345. [src/digitalmodel/asset_integrity/assessment/pitting.py:L280-L292] |
| S36 | No detected pits returns Level 2 ACCEPT with unity RSF and no equivalent region. | no detection check exists on direct API; #2345. [src/digitalmodel/asset_integrity/assessment/pitting.py:L339-L356] |
| S37 | Equivalent grid forces at least one row; a single row loses physical pitch in the engine. | no detection check exists on direct API; #2339. [src/digitalmodel/asset_integrity/assessment/pitting.py:L358-L365] |
| S38 | Convenience assess_pitting applies FCA only to Level 1, not the delegated Level 2 wall. | no detection check exists on this caller; #2345. [src/digitalmodel/asset_integrity/assessment/pitting.py:L408-L418] |
| S39 | Corrosion rate and FCA default to zero; unprovided rate can become unbounded life. | no detection check exists for missing-vs-zero rate; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L59-L68] |
| S40 | Coordinator supplies FCA to pit Level 1 only; raw wall reaches LML/GML/Level 2 and life. | no detection check exists for FCA ownership; #2345. [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L181-L220] |
| S41 | Missing pressure becomes NaN; missing Folias becomes unity; missing life becomes NaN; scalar casts do not validate physical evidence. | no detection check exists; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L231-L250] |
| S42 | Repair context defaults to no leak/through-wall/axial load, oil, and practice temperature/life unless overridden. | no detection check exists for missing operating context; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L299-L309] |
| S43 | Adapter defaults to inches when metadata is absent. | C01 covers conversion only; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_workflow.py:L41-L48] |
| S44 | List cells coerce to float, including numeric strings; locale separators have no explicit policy. | no detection check exists; issue pending (1). [src/digitalmodel/asset_integrity/assessment/ffs_workflow.py:L57-L66] |
| S45 | Shared monitor/floor/life bands are default policy across asset classes. | no detection check exists for asset-specific basis; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L102-L108] |
| S46 | Wall already below required returns zero life; zero rate returns infinity; negative/nonfinite rate has no physical guard. | no detection check exists for invalid-rate branch; issue pending (1). [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L230-L243] |
| S47 | scaled_limit returns None for nonpositive allowable or nonfinite margin; reduced_mawp propagates it. | no detection check exists for missing-limit explanation; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L260-L291] |
| S48 | Absent applicability means an all-clear record, not a demonstrated applicability audit. | no detection check exists for absent-vs-checked evidence; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L371-L376] |
| S49 | Nonfinite margin or raised applicability flags explicitly ESCALATE; not a swallowed failure. | C11 runs applicability/decision regressions; #1094. [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L395-L411] |
| S50 | Only margin is checked finite; life/allowable and caller screening_pass can influence decisions without a complete finite-value guard. | no detection check exists for all invalid scalar combinations; issue pending (1). [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L413-L449] |
| S51 | Reading count, scatter, UT tolerance and profile count have configurable defaults without constructor range checks. | C03 covers default count behavior; #2339. [src/digitalmodel/asset_integrity/assessment/measurement_sufficiency.py:L90-L101] |
| S52 | Severe-RSF branch takes precedence over data gaps; confidence high is a policy action, not measurement accuracy. | no detection check exists for confidence interpretation; #2339. [src/digitalmodel/asset_integrity/assessment/measurement_sufficiency.py:L134-L156] |
| S53 | Empty data gives zero mean/std; validity counts exclude NaN but not infinity. | no detection check exists for all-nonfinite input; issue pending (1). [src/digitalmodel/asset_integrity/assessment/measurement_sufficiency.py:L278-L281] |
| S54 | Single reading or nonpositive mean substitutes zero COV; empty data uses None. | no detection check exists for invalid-value scatter; issue pending (1). [src/digitalmodel/asset_integrity/assessment/measurement_sufficiency.py:L282-L282] |
| S55 | Profile cells use fixed nominal threshold and ignore NaN through false comparisons. | C03 covers counts only; #2339. [src/digitalmodel/asset_integrity/assessment/measurement_sufficiency.py:L284-L287] |
| S56 | No readings leaves minimum/location None and boundary/degraded flags false. | no detection check exists for empty-grid reporting; issue pending (1). [src/digitalmodel/asset_integrity/assessment/measurement_sufficiency.py:L289-L303] |
| S57 | External result scalars cast to float; missing RSF/allowable metadata becomes zero; a missing margin suppresses tolerance check. | no detection check exists for missing-result dictionaries; issue pending (1). [src/digitalmodel/asset_integrity/assessment/measurement_sufficiency.py:L305-L320] |
| S58 | Severe-loss escalation floor is a fixed default shared with sufficiency. | no detection check exists for default-basis suitability; #2339. [src/digitalmodel/asset_integrity/assessment/level3_escalation.py:L47-L51] |
| S59 | Missing RSF/RSFa becomes zero through float conversion; NaN can evade comparisons. | no detection check exists for NaN dictionaries; issue pending (1). [src/digitalmodel/asset_integrity/assessment/level3_escalation.py:L120-L125] |
| S60 | Handoff repeats default-zero scalar conversion to select recommendations. | no detection check exists for invalid scalar recommendations; issue pending (1). [src/digitalmodel/asset_integrity/assessment/level3_escalation.py:L203-L215] |
| S61 | Missing grid_summary becomes empty dict; handoff echoes caller dictionaries without completeness validation. | no detection check exists for completeness; issue pending (3). [src/digitalmodel/asset_integrity/assessment/level3_escalation.py:L270-L278] |
| S62 | Zero bending, very wide plate and deepest-point defaults are explicit load/geometry assumptions; finite inputs are not exhaustively checked. | C04 checks two geometry violations only; issue pending (1). [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L117-L145] |
| S63 | FAD returns zero beyond collapse cutoff; deliberate physical-domain sentinel, not an error. | C23 covers cutoff; #2160. [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L79-L111] |
| S64 | Charpy correlation has default thickness and a positive-input check, but no transition-temperature/material guard. | no detection check exists for correlation suitability; #2157. [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L205-L217] |
| S65 | Missing toughness uses Charpy with note; nonpositive Kr yields infinite margin; collapse overrides to zero. | no detection check exists for negative-loading interpretation; #2157. [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L262-L291] |
| S66 | Critical depth returns zero for failing small depth or a cap smaller than the tolerance interval; no distinct reason; defaults cap depth and tolerance. | no detection check exists for indistinguishable zero outcomes; issue pending (1). [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L302-L347] |
| S67 | Missing date/ID/RSFa/life/criterion get display defaults; truthy inside diameter selection replaces explicit zero. | no detection check exists for missing-data provenance; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L123-L134] |
| S68 | Missing sufficiency attributes become UNKNOWN/empty lists/strings; count int conversion accepts external report metadata. | C53 covers populated case only; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L209-L227] |
| S69 | Missing recommendation fields become empty candidates/exclusions/basis; interval converted to float without locale policy. | no detection check exists for incomplete recommendations; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L253-L274] |
| S70 | zip pairs flags and notes to shortest list, potentially omitting a flag. | no detection check exists for unequal lists; issue pending (3). [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L319-L323] |
| S71 | NaN remaining life is displayed as infinity/no corrosion, indistinguishable from zero-rate infinity. | no detection check exists for NaN report life; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L357-L363] |
| S72 | Report defaults Folias to unity and uses final decision verdict in Level 2 display. | no detection check exists for omitted Folias; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L453-L475] |
| S73 | Nonfinite life is described as unlimited/no further corrosion, including NaN. | no detection check exists for NaN report life; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L486-L505] |
| S74 | Grid appendix truncates rows/columns and rounds values; omission note is present, but full raw grid is not embedded. | no detection check exists for full-evidence retention; issue pending (3). [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L573-L597] |
| S75 | FCA defaults to zero, acceptance controls to declared constants; finite numeric input validation rejects text and booleans. | C02; #2339. [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L17-L44] |
| S76 | No pits or consumed ligament yields not-evaluated/None; single-row extent is repaired only in this adapter. | C11 runs dedicated edge-case regressions; #2339. [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L47-L72] |
| S77 | Absent Level 2 applicability defaults all-clear before explicit pit/background/ligament/nonuniform-depth flags. | C11 runs workflow guards; #2345. [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L75-L98] |
| S78 | NaN used as unsupported decision token and converted to None for output; no physical life/rerating inferred. | C52 checks qualification, not all token branches; #2339. [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L112-L135] |
| S79 | Scalar helper accepts numeric types only, casts to float and rejects nonfinite/invalid sign; optional defaults require an input basis. | C02 covers workflow invalid grids only; #2339. [src/digitalmodel/asset_integrity/assessment/screening_report.py:L10-L18] |
| S80 | None rendered not evaluated; absent limitations are caller responsibility; HTML is escaped. | C51; issue pending (3). [src/digitalmodel/asset_integrity/assessment/screening_report.py:L21-L41] |
| S81 | No output_dir returns in-memory result without persistence; write errors propagate; no empty-result cache write exists here. | C51 covers explicit write route; issue pending (3). [src/digitalmodel/asset_integrity/assessment/screening_report.py:L44-L54] |
| S82 | Enum returns None for an unrecognized alias, allowing Enum to raise; not a swallowed error. | C11; #2205. [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L86-L90] |
| S83 | Compatibility wrappers return None for absent pressure and delegate life/rerating; no second formula or swallowed exception. | C11; #2339. [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L553-L568] |

Other confirmed defects and historical leads:

| Finding | Existing issue / detection / date | Evidence |
|---|---|---|
| Five legacy FAD tests fail before and after documentation edits: Python 3.12 invalid escape under warnings-as-errors while importing common/data | issue pending (4); C11 detects the same five baseline failures; observed 2026-10-16 | [src/digitalmodel/asset_integrity/common/data.py:L214-L217] [tests/asset_integrity/test_fad_curves.py:L159-L162] [tests/asset_integrity/test_fad_curves.py:L195-L206] |
| BS 7910 edition mismatch; API crack curve and report expose different edition bases | issue pending (2), #2157, #2175; no detection check exists for edition consistency | [src/digitalmodel/codes.py:L52-L57] [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L64-L105] [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L509-L514] |
| Low-level pitting claims a qualified bound while workflow withholds it | #2345 and #2182; C52 detects workflow overstatement only; no detection check exists for low-level basis wording | [src/digitalmodel/asset_integrity/assessment/pitting.py:L77-L84] [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L64-L70] |
| Generic HTML methodology still says Part 6 is out of scope even though coordinator routes PITTING | #2339; no detection check exists for that report inconsistency | [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L548-L552] [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L175-L197] |
| Historical coordinator, sufficiency, pitting, crack and shared-vocabulary issues are feature leads, not proof of incidents | #1058, #1059, #1269, #1270, #2205; C01/C03/C04/C11/C52 cover implemented behavior; missing Part 6 charts remain U2 | [docs/domains/asset-integrity/ffs-architecture.md:L3-L20] [tests/asset_integrity/test_pitting.py:L3-L15] [tests/asset_integrity/test_crack_fad.py:L1-L10] [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L3-L8] |

## 7. Lessons log

| Date | Work reference | Lesson and evidence | Pack change | Shared-log entry |
|---|---|---|---|---|
| 2026-10-16 | Initial documentation | A passed formula anchor does not establish pitting failure-mechanism qualification. [docs/domains/asset-integrity/pitting-validation-2026-10-09.md:L15-L20] [docs/domains/asset-integrity/pitting-validation-2026-10-09.md:L48-L54] | Regression classification and route-specific limits | Proposed entry, issue pending (3) |

## 8. Ownership

Proposed review responsibility: engineering calculations quality manager; deputy: structural calculations quality manager; producing discipline: asset integrity calculations; escalation: the maintainer for missing sources, conflicting editions or revised acceptance criteria. Appointment and clearance remain issue pending (3).

## 9. Dependencies and other implementations

Upstream dependencies include NumPy/pandas grid handling, SciPy connected-component labeling, unit-tagged limits, applicability records, code-reference labels and the composite-repair helper; downstream the FFS workflow serializes the coordinator and the dedicated pitting adapter builds an HTML report. [src/digitalmodel/asset_integrity/assessment/pitting.py:L63-L71] [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L31-L50] [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L61-L65] [src/digitalmodel/asset_integrity/assessment/ffs_workflow.py:L41-L54] [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L139-L168]

### Implementation-map inventory and counted families

Implementation-map source: `docs/registry/implementation-map.yaml` on `codex/implmap-work`, map commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; section-9 map locators resolve at that separate map pin. [docs/registry/implementation-map.yaml:L1-L6]

Audit method: parse the map with `yaml.safe_load`, inspect every family's `implementations`, and count row paths beginning `src/digitalmodel/asset_integrity/assessment/`; all 13 family counts are zero. [docs/registry/implementation-map.yaml:L1-L53690]

Inventory: empty — none of the map's implementation rows has a path inside this package; map families: 0, counted families: 0, further counted-family implementations: 0; counted-family grep: NOT_APPLICABLE because there is no in-package map family (issue pending (5)). [docs/registry/implementation-map.yaml:L1-L53690]

Map follow-up: add required-wall screening, GML/LML RSF and Folias, morphology/pit-field/equivalent-LTA assessment, FAD/stress-intensity/reference-stress/toughness/critical-depth methods, shared decisions/life/rerating, sufficiency/escalation and report evidence; these section-3 families are not counted until map rows are added inside the package (issue pending (5)). [src/digitalmodel/asset_integrity/assessment/level1_screener.py:L148-L188] [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L126-L227] [src/digitalmodel/asset_integrity/assessment/pitting.py:L128-L150] [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L19-L45] [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L230-L291] [src/digitalmodel/asset_integrity/assessment/measurement_sufficiency.py:L104-L123] [src/digitalmodel/asset_integrity/assessment/screening_report.py:L21-L54]

### Repository-wide searches, including scripts

```sh
rg -n 'Folias|folias|remaining_strength_factor|RSF' src scripts --glob '*.py'
rg -n 'fad_curve|BS7910_2013_option_1|newman_raju|kmat_from_charpy' src scripts --glob '*.py'
rg -n 'Level2Engine|assess_pitting|assess_component|reduced_mawp' src scripts --glob '*.py'
```

| Search / implementation | Relationship and limit | Evidence |
|---|---|---|
| M1 | Separate RSF calculator computes wall ratios, and the circumferential helper computes a separate factor; neither is a substitute qualification reference for this package | [src/digitalmodel/asset_integrity/rsf_calculations.py:L21-L63] [docs/domains/asset-integrity/uniform-loss-source-audit-2026-10-06.md:L13-L22] |
| M2 | Common FAD builder delegates to the canonical curve; fracture router and other structural fracture calculations are outside this package | [src/digitalmodel/asset_integrity/common/fad.py:L1-L35] [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L14-L17] [src/digitalmodel/asset_integrity/assessment/level3_escalation.py:L36-L40] |
| M3 | Coordinator, convenience and workflow callers share implementations but differ in preconditions/FCA handling; S21/S34–S40 distinguish them | [src/digitalmodel/asset_integrity/assessment/pitting.py:L408-L418] [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L175-L220] [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L47-L98] |

The named durable workflow is registered as offline and locates its in-memory pitting result; inventory-row test counts are replaced by the collected evidence in section 10. [docs/registry/workflows.yaml:L1275-L1285]

## 10. Coverage and open items

### Source and documentation coverage

All 15 package Python files were inspected, including exports and the workflow/report subpaths; source scope is the package header path, with no nested Python subpackage excluded. [src/digitalmodel/asset_integrity/assessment/__init__.py:L15-L66]

- `__init__.py`. [src/digitalmodel/asset_integrity/assessment/__init__.py:L1-L92]
- `crack_fad.py`. [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L1-L347]
- `ffs_coordinator.py`. [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L1-L321]
- `ffs_decision.py`. [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L1-L568]
- `ffs_report.py`. [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L1-L597]
- `ffs_router.py`. [src/digitalmodel/asset_integrity/assessment/ffs_router.py:L1-L179]
- `ffs_workflow.py`. [src/digitalmodel/asset_integrity/assessment/ffs_workflow.py:L1-L66]
- `grid_parser.py`. [src/digitalmodel/asset_integrity/assessment/grid_parser.py:L1-L174]
- `level1_screener.py`. [src/digitalmodel/asset_integrity/assessment/level1_screener.py:L1-L188]
- `level2_engine.py`. [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L1-L291]
- `level3_escalation.py`. [src/digitalmodel/asset_integrity/assessment/level3_escalation.py:L1-L301]
- `measurement_sufficiency.py`. [src/digitalmodel/asset_integrity/assessment/measurement_sufficiency.py:L1-L321]
- `pitting.py`. [src/digitalmodel/asset_integrity/assessment/pitting.py:L1-L426]
- `pitting_workflow.py`. [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L1-L168]
- `screening_report.py`. [src/digitalmodel/asset_integrity/assessment/screening_report.py:L1-L54]

Focused discovery used `rg -l 'asset_integrity[./]assessment|api579-pitting-screen|ffs_coordinator|Level1Screener|Level2Engine|MeasurementSufficiency' docs --glob '*.md'`; supporting source passages, registry/workflow entries and relevant standard references were read:
- `docs/domains/asset-integrity/ffs-architecture.md`. [docs/domains/asset-integrity/ffs-architecture.md:L5-L44]
- `docs/domains/asset-integrity/ffs-readiness-review-2026-09-25.md`. [docs/domains/asset-integrity/ffs-readiness-review-2026-09-25.md:L21-L74]
- `docs/domains/asset-integrity/ffs-validation-record-2026-06-27.md`. [docs/domains/asset-integrity/ffs-validation-record-2026-06-27.md:L13-L36]
- `docs/domains/asset-integrity/pitting-validation-2026-10-09.md`. [docs/domains/asset-integrity/pitting-validation-2026-10-09.md:L1-L74]
- `docs/domains/asset-integrity/uniform-loss-source-audit-2026-10-06.md`. [docs/domains/asset-integrity/uniform-loss-source-audit-2026-10-06.md:L13-L22]
- `docs/domains/codes-register.md`. [docs/domains/codes-register.md:L14-L20]
- `docs/domains/codes-provenance-2026-06-28.md`. [docs/domains/codes-provenance-2026-06-28.md:L7-L40]
- `docs/plans/2026-09-24-issue-2157-crack-fe-assessment.md`. [docs/plans/2026-09-24-issue-2157-crack-fe-assessment.md:L30-L59]
- `docs/plans/2026-09-25-issue-1057-ffs-offering-program.md`. [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L15-L28]
- `docs/plans/2026-10-06-issue-2287-uniform-local-loss-screening.md`. [docs/plans/2026-10-06-issue-2287-uniform-local-loss-screening.md:L1-L30]
Additional non-Markdown discovery hits are `docs/capability-map/capabilities-added.yml`, `docs/capability-map/capabilities-sections.yml`, and `docs/domains/asset-integrity/surface-crack-calculix-results.json`; broader capability surfaces and historical solver results are discovery evidence, with further qualification assigned to issue pending (3). [scripts/studies/surface_crack_post.py:L1-L5] [tests/solvers/calculix/test_surface_crack_spike.py:L20-L27]

The containing package pack is a dependency reference; long planning documents were read at relevant passages, without claiming execution of their proposed work. [src/digitalmodel/asset_integrity/docs/agent-pack.md:L1-L20]

### Offline test evidence

Execution record at the code pin: `PYTHONPATH=src uv run --no-sources python -m pytest <listed paths> -q -rA -p no:cacheprovider --basetemp=<scratch case>`; default pytest configuration retained, no addopts override, no extra worker, no solver execution. The durable test ran from a temporary copy because it writes report/log files.

| Command scope | BASE run / passed / failed / skipped / NOT_RUN | AFTER run / passed / failed / skipped / NOT_RUN |
|---|---|---|
| Twenty discovered test files below | 472 / 467 / 5 / 0 / 0 | 472 / 467 / 5 / 0 / 0 |
| Copied pitting durable node R4 | 1 / 1 / 0 / 0 / 0 | 1 / 1 / 0 / 0 / 0 |

Additional routing/contracts validation: the complete contracts domain collected 1,115 cases, with 1,051 passed, 3 failed and 61 existing skips; the missing declared timeout plugin was then supplied as `--with pytest-timeout==2.4.0` for the affected existing-pack node, which passed. The two remaining baseline failures are the string-addressed-dependency positive control and source-parse guard, both affected by unchanged invalid escape literals; this pack's node-ID collection and all 10 routing tests passed. [tests/contracts/test_string_addressed_dependencies.py:L517-L531] [tests/contracts/test_string_addressed_dependencies.py:L620-L643] [tests/test_no_corrupted_py_files.py:L14-L24] [tests/docs/test_digitalmodel_routing_contract.py:L40-L52]

The routing YAML loads with the existing permissive parser, and parsed equality after removing only the new assessment registration holds; strict parsing finds a pre-existing duplicate `solvers.submodules` key at the code pin, issue pending (6). [docs/registry/module-routing.yaml:L303-L322]

A supplemental external crack-reference file was discovered during dependency review and run on the unchanged code pin after the documentation draft: `tests/solvers/calculix/test_surface_crack_spike.py` collected 10 cases, 9 passed and 1 skipped through its existing binary guard. The native case `tests/solvers/calculix/test_surface_crack_spike.py::test_native_displacement_and_stress_readback` is NOT_RUN because the CalculiX binary is absent; no native solve was started. Its solver-free reference wrapper delegates to this package's crack function and is regression evidence only. [tests/solvers/calculix/test_surface_crack_spike.py:L46-L57] [tests/solvers/calculix/test_surface_crack_spike.py:L81-L105]

The five failures are four parameterizations of `tests/asset_integrity/test_fad_curves.py::test_three_legacy_entry_points_return_canonical_kr` and `tests/asset_integrity/test_fad_curves.py::test_get_k_r_allowable_beyond_cutoff_is_zero`; baseline source imports common/data, whose invalid escape is rejected under Python 3.12 warnings-as-errors (issue pending (4)).
[tests/asset_integrity/test_fad_curves.py:L150-L162] [tests/asset_integrity/test_fad_curves.py:L195-L206] [src/digitalmodel/asset_integrity/common/data.py:L214-L217]

Test file inventory (counts are collected parameterized cases; tests beyond this package exercise its direct consumers):
- `tests/asset_integrity/test_applicability.py`: 36 cases. [tests/asset_integrity/test_applicability.py:L1-L20]
- `tests/asset_integrity/test_code_provenance.py`: 11 cases. [tests/asset_integrity/test_code_provenance.py:L1-L20]
- `tests/asset_integrity/test_composite_repair.py`: 8 cases. [tests/asset_integrity/test_composite_repair.py:L1-L20]
- `tests/asset_integrity/test_crack_fad.py`: 11 cases. [tests/asset_integrity/test_crack_fad.py:L1-L20]
- `tests/asset_integrity/test_fad_curves.py`: 28 cases. [tests/asset_integrity/test_fad_curves.py:L1-L20]
- `tests/asset_integrity/test_ffs_1094_quickwins.py`: 6 cases. [tests/asset_integrity/test_ffs_1094_quickwins.py:L1-L20]
- `tests/asset_integrity/test_ffs_assessment.py`: 70 cases. [tests/asset_integrity/test_ffs_assessment.py:L1-L20]
- `tests/asset_integrity/test_ffs_coordinator.py`: 11 cases. [tests/asset_integrity/test_ffs_coordinator.py:L1-L20]
- `tests/asset_integrity/test_ffs_decision_asset_classes.py`: 82 cases. [tests/asset_integrity/test_ffs_decision_asset_classes.py:L1-L20]
- `tests/asset_integrity/test_ffs_lookup.py`: 13 cases. [tests/asset_integrity/test_ffs_lookup.py:L1-L20]
- `tests/asset_integrity/test_ffs_validation.py`: 11 cases. [tests/asset_integrity/test_ffs_validation.py:L1-L20]
- `tests/asset_integrity/test_level3_escalation.py`: 14 cases. [tests/asset_integrity/test_level3_escalation.py:L1-L20]
- `tests/asset_integrity/test_measurement_sufficiency.py`: 15 cases. [tests/asset_integrity/test_measurement_sufficiency.py:L1-L20]
- `tests/asset_integrity/test_pitting.py`: 36 cases. [tests/asset_integrity/test_pitting.py:L1-L20]
- `tests/asset_integrity/test_pitting_workflow.py`: 35 cases. [tests/asset_integrity/test_pitting_workflow.py:L1-L20]
- `tests/asset_integrity/test_rbi_screening.py`: 27 cases. [tests/asset_integrity/test_rbi_screening.py:L1-L20]
- `tests/asset_integrity/test_real_inspection_fixtures.py`: 7 cases. [tests/asset_integrity/test_real_inspection_fixtures.py:L1-L20]
- `tests/asset_integrity/test_rerate_wiring.py`: 6 cases. [tests/asset_integrity/test_rerate_wiring.py:L1-L20]
- `tests/asset_integrity/test_riser_joint_ffs.py`: 12 cases. [tests/asset_integrity/test_riser_joint_ffs.py:L1-L20]
- `tests/materials/test_ffs_material_library.py`: 33 cases. [tests/materials/test_ffs_material_library.py:L1-L20]

### Open-item register

Every item below is assigned to the maintainer for triage; draft references denote issue text prepared for issue creation, not existing issue numbers.

| Item | State / bound / follow-up | Evidence |
|---|---|---|
| U1 | UNVERIFIED normative qualification: #2287 for wall-loss procedure/source, #2175 for Part 9; edition/citation gaps issue pending (2) | [docs/domains/asset-integrity/uniform-loss-source-audit-2026-10-06.md:L170-L179] [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L95-L105] |
| U2 | UNVERIFIED published pitting bound/benchmark: #2182, #2345; published case and pit charts not established | [docs/domains/asset-integrity/pitting-validation-2026-10-09.md:L41-L71] |
| U3 | UNVERIFIED edition/clause bindings for API RP 2SK, API RP 2SIM and ISO 24817: issue pending (2) | [src/digitalmodel/asset_integrity/assessment/ffs_decision.py:L48-L51] [src/digitalmodel/asset_integrity/composite_repair.py:L16-L19] |
| Input validity and defaults | The input and missing-data rows in section 6: finite, sign, locale, empty-grid and material-domain checks need discriminating cases, issue pending (1); route-specific defaults also #2339 | [src/digitalmodel/asset_integrity/assessment/grid_parser.py:L60-L83] [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L269-L291] [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L302-L347] |
| Reference wall and FCA | Direct/convenience/coordinator paths need preconditions independently of dedicated workflow: #2345 | [src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py:L175-L220] [src/digitalmodel/asset_integrity/assessment/pitting_workflow.py:L75-L98] |
| Extent and averaging | GridParser mean differs from engine mean (#1094); degraded-row counting and omitted width/source qualification #2287 | [src/digitalmodel/asset_integrity/assessment/grid_parser.py:L171-L174] [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L131-L138] [src/digitalmodel/asset_integrity/assessment/level2_engine.py:L172-L182] |
| Report evidence, metadata and stewardship | Complete gate/fingerprint/signature evidence, flag-list cardinality, full-grid retention, reviewer/deputy appointment and shared-log entry: issue pending (3); report-default semantics #2339 | [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L319-L323] [src/digitalmodel/asset_integrity/assessment/ffs_report.py:L573-L597] [src/digitalmodel/asset_integrity/assessment/screening_report.py:L21-L54] |
| Native crack readback | NOT_RUN: CalculiX binary absent; the existing native test requires a solver, while its 9 solver-free cases passed; further native qualification #2157 | [tests/solvers/calculix/test_surface_crack_spike.py:L81-L105] |
| Baseline failures | Invalid escape in unchanged common/data blocks five legacy FAD tests: issue pending (4) | [src/digitalmodel/asset_integrity/common/data.py:L214-L217] |
| Validation baseline | Strict routing YAML duplicate key needs a separate repair, issue pending (6); invalid escape parsing affects 13 unchanged source files and the dependency scanner, issue pending (4) | [docs/registry/module-routing.yaml:L303-L322] [tests/contracts/test_string_addressed_dependencies.py:L517-L531] [tests/test_no_corrupted_py_files.py:L14-L24] |
| Map coverage | Empty in-package map inventory; all section-3 method families require map additions: issue pending (5) | [docs/registry/implementation-map.yaml:L1-L53690] |
| Historical plans and data boundaries | Historical test/maturity claims were not adopted; no private sidecar was read or written, no client-derived values reproduced; further case qualification/anonymized discovery inventory belongs to issue pending (3) | [docs/domains/asset-integrity/ffs-readiness-review-2026-09-25.md:L23-L47] [tests/asset_integrity/test_pitting.py:L7-L15] |
