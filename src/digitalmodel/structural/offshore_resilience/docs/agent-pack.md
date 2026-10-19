# Offshore resilience knowledge pack

| Header | Value |
|---|---|
| Module | structural/offshore_resilience |
| Repository | digitalmodel |
| Package path | `src/digitalmodel/structural/offshore_resilience` |
| Pack revision | 0 |
| Code revision | `c099a5e1317e6bed63d876252d87cbb12c774372` |
| Analysis family | Engineering calculations (EC); AQG r0 Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT; clearance pending |

## 1. Purpose and scope

The public API exposes platform concept records, cost comparison, installation checklists, monitoring records/templates, decommissioning flags and lifecycle/tieback scores. [src/digitalmodel/structural/offshore_resilience/__init__.py:L25-L98]

The implemented paths perform arithmetic, classify concepts, compare checklist inputs with stored bounds and construct records; these paths do not perform structural stress, fatigue accumulation or dynamic-response analysis. [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L93-L119] [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L144-L204] [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L88-L138] [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L187-L208] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L225-L272]

The routing registry marks the structural domain stable; it has no separate offshore-resilience maturity field at the code pin. [docs/registry/module-routing.yaml:L400-L418]

Treat results as screening and configuration evidence; do not use generic defaults as design approval criteria until the reference-basis gaps in section 10 are closed. [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L179-L184] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L250-L265]

## 2. Inputs and outputs

Input ranges below describe implemented validation, rather than certified applicability. [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L93-L104]

| Input | Unit / convention | Allowed range and default | Source / evidence |
|---|---|---|---|
| MinimumFacilityPlatform | Weights/capacity tonnes; depth ft; life years; slots count | All fields required; platform_type restricted; depth minimum strictly below maximum; other physical bounds not enforced | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L60-L104] |
| ModularCostComparison | CAPEX and savings USD; window days | Required; conventional CAPEX positive; other values unchecked | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L127-L149] |
| lifecycle_value_index | Dimensionless difficulty, flexibility, complexity, headroom | Required; inclusive scale defined in code; no integral-type check | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L215-L247] |
| decom_flags_for_design_choices | Boolean choices, removal-strategy text, jacket-piece count | Required; documented cut/pull/leave_in_situ; unknown text or invalid counts have no rejection branch | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L72-L156] |
| score_tieback_accommodation | Slots count, PipelineLayout enum, mudmat percent of existing area | Required; negative margin rejected; slots only capped above | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L163-L208] |
| marginal_field_lifecycle_score | Four dimensionless scale inputs and tieback fraction | Required; tieback constrained to [0, 1]; scale validation delegated | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L215-L262] |
| ChecklistItem.set_value | Same unit as item.unit | Inclusive min/max; initial actual_value None and status PENDING; no conversion | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L63-L94] |
| Checklist factories | Structure fixed/floating; foundation MONOPILE/JACKET | Required selector; each item carries its bounds and unit; structure selector explicitly validated | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L145-L164] [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L248-L352] |
| StructuralHealthRecord | Datetime documented as timezone-aware UTC; engineering-unit value | Required; checks alert/severity consistency only | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L60-L96] |
| SensorConfig / SensorTemplate | Sampling Hz; threshold unit absent from schema | Required config fields; critical must exceed warning; template sensors default empty | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L103-L149] |
| build_alert_thresholds | Life years; budget fraction; AssetType | Required; documented budget domain not validated before division | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L225-L265] |

| Output | Unit / interpretation | Calculation / evidence |
|---|---|---|
| total_weight_tonnes, crane_installable, water_depth_midpoint_ft | Tonnes, boolean, ft | Sum, inclusive capacity comparison, arithmetic midpoint; no dynamic lift factor | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L106-L119] |
| net_savings_usd, savings_fraction | USD, dimensionless | Conventional minus minimum-facility plus avoided lift cost; fraction divides by conventional | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L151-L167] |
| BseePlatformClass | Enum | Approximate depth/weight branch classification | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L174-L204] |
| Lifecycle and tieback scores | Dimensionless | Normalised heuristic scores; composite dictionary also returns recommendation | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L249-L259] [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L193-L208] [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L264-L281] |
| DecomFlag list | Categorical severity/mitigation | Empty when no branch appends a flag | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L98-L156] |
| Checklist summary, completion_ratio, is_complete | Counts, fraction, boolean | Completion ratio measures passed fraction, not entered fraction | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L108-L138] |
| SensorTemplate | List of SensorConfig | Fixed, floating and wind factory records, not live measurement processing | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L152-L192] |
| AlertThresholdConfig | Fatigue fraction/year, acceleration g, frequency drift percent | Budget/life scaling and literal triggers | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L199-L272] |

These API implementations have no file-format reader/writer or coordinate transform; sensor locations are descriptive strings, and wind tilt labels use fore-aft/side-side axes without a declared origin or global transform. [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L93-L259] [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L88-L352] [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L72-L281] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L86-L272] [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L174-L197]

## 3. Method and governing standards

| Method | Implementation and reference basis | Evidence |
|---|---|---|
| Concept mass, capacity and depth | Add weights, compare capacity, average depth endpoints; elementary screening arithmetic | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L106-L119] |
| Cost comparison | Net difference plus avoided lift cost; fraction of conventional CAPEX | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L151-L167] |
| Approximate classification | API RP 2A-WSD, Section 1.7, Platform Classification is named; edition unspecified; clause text not checked against the licensed copy | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L8-L12] [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L174-L204] |
| Lifecycle value | Reverse difficulty/complexity, normalise flexibility/headroom, average components | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L238-L259] |
| Decommissioning flags | Qualitative branches; API RP 2A-WSD, Appendix A, Grouted pile connections, edition unspecified; clause text not checked against the licensed copy; BSEE NTL-2016-N07 and OSPAR Decision 98/3 also named, without numeric compliance checks | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L9-L14] [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L98-L156] |
| Tieback and composite | Average capped slot/margin scores and topology lookup; weight lifecycle/tieback before recommendation branches | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L193-L208] [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L264-L281] |
| Installation criteria | Inclusive scalar comparison with pre-populated bounds; no load-dependent adequacy calculation | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L88-L94] [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L164-L236] |
| Monitoring configuration | DNV-ST-0126, Support Structures for Wind Turbines, and API RP 2SIM, Structural Integrity Management named without edition or clause; clause text not checked against the licensed copy | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L9-L13] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L131-L192] |
| Alert thresholds | Annual budget pace times warning/critical factors and literal triggers; no threshold-exceedance evaluator or Citation sidecar emitted by this function | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L225-L272] |

The standards-page plan records DNV-ST-0126, Support structures for wind turbines (2021-07), and API RP 2SIM, Structural Integrity Management of Fixed Offshore Structures (2014, R2020); the module docstring omits editions, which establishes a traceability gap rather than conflicting implemented editions. [docs/plans/2026-05-05-issue-574-fowt-wiki-standards-pages.md:L8-L16] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L9-L13]

The separate structural plan identifies API RP 2A-WSD as Planning, Designing, and Constructing Fixed Offshore Platforms — Working Stress Design, 22nd edition (2014), reaffirmed 2025; this metadata does not identify the edition used by this package. [docs/plans/2026-05-05-issue-581-api-rp-2a-wsd-structural.md:L8-L9]

UNVERIFIED: U1 — engineering validity and standards-clause provenance of classification, checklist and monitoring defaults; issue pending (3), with standards-page follow-up #574. [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L45-L48] [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L164-L236] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L250-L272]

UNVERIFIED: U2 — calibrated measurement units, sign/phase conventions and reference frames for sensor thresholds; issue pending (3). [src/digitalmodel/structural/offshore_resilience/structural_health.py:L103-L137] [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L22-L197]

## 4. Validated worked examples and regression examples

Independent engineering validation: 0 cases credited; the following 6 cases are regression examples with expectations encoded in repository tests, not sourced engineering benchmarks. [tests/structural/offshore_resilience/test_minimum_facility.py:L46-L104] [tests/structural/offshore_resilience/test_minimum_facility.py:L175-L205] [tests/structural/offshore_resilience/test_lifecycle_planning.py:L93-L118] [tests/structural/offshore_resilience/test_installation_checklist.py:L39-L49] [tests/structural/offshore_resilience/test_structural_health.py:L212-L224]

All node results below were PASS before and after the documentation change at the header code revision; no solver, licence or external reference-data route was needed. Test command: `PYTHONPATH=src uv run --no-sources python -m pytest tests/structural/offshore_resilience -q -rA -p no:cacheprovider --basetemp=<scratch>`; baseline 72 run/72 passed/0 failed/0 skipped/0 NOT_RUN, after 72/72/0/0/0. These are execution records, not evidence that the screening assumptions satisfy a standard.

| Case / class | Source and inline input | Expected value / tolerance basis | Existing test node ID | Offline result |
|---|---|---|---|---|
| R1 mass sum; regression | 180.0 t + 320.0 t; [tests/structural/offshore_resilience/test_minimum_facility.py:L46-L59] | 500.0 t; pytest.approx default | `tests/structural/offshore_resilience/test_minimum_facility.py::TestMinimumFacilityPlatform::test_total_weight_tonnes` | PASS at BASE and after |
| R2 depth midpoint; regression | 60.0 ft and 300.0 ft; [tests/structural/offshore_resilience/test_minimum_facility.py:L91-L104] | 180.0 ft; pytest.approx default | `tests/structural/offshore_resilience/test_minimum_facility.py::TestMinimumFacilityPlatform::test_water_depth_midpoint` | PASS at BASE and after |
| R3 savings fraction; regression | Inline USD costs in the cited test; [tests/structural/offshore_resilience/test_minimum_facility.py:L186-L195] | 0.4 dimensionless; pytest.approx default | `tests/structural/offshore_resilience/test_minimum_facility.py::TestModularCostComparison::test_savings_fraction` | PASS at BASE and after |
| R4 tieback; regression | 4 slots, LOOP, 30.0 % margin; [tests/structural/offshore_resilience/test_lifecycle_planning.py:L93-L100] | Score >= 0.75 dimensionless; inequality in test | `tests/structural/offshore_resilience/test_lifecycle_planning.py::TestScoreTiebackAccommodation::test_high_score_for_well_designed_layout` | PASS at BASE and after |
| R5 scalar checklist; regression | 1.8 m inside [0.0, 2.5] m; [tests/structural/offshore_resilience/test_installation_checklist.py:L39-L49] | PASS; 0 status mismatches | `tests/structural/offshore_resilience/test_installation_checklist.py::TestChecklistItem::test_item_passes_when_value_in_range` | PASS at BASE and after |
| R6 budget monotonicity; regression | 25 years; two synthetic budget fractions; [tests/structural/offshore_resilience/test_structural_health.py:L212-L224] | Lower budget gives lower rate (fraction/year); 0 order violations | `tests/structural/offshore_resilience/test_structural_health.py::TestBuildAlertThresholds::test_budget_fraction_respected_in_fatigue_warning` | PASS at BASE and after |

## 5. QA gates and executable checks

Gate applicability follows AQG r0, section 10, Python calculations; G5 follows AQG r0, section 9, QA appendix. Numeric criteria below check the module's implemented behavior, not licensed design limits.

| Gate | Applicability / route | Check IDs |
|---|---|---|
| G0 | Applicable: caller inputs, units and source citations | C01, C02, C03, C04 |
| G1 | Applicable: Python import and offline tests; no target-program deck is written by these paths | C11 |
| G2 | Applicable: independent arithmetic hand checks of selected outputs; standards validation remains open | C21, C22, C23 |
| G3 | NOT_APPLICABLE: no dynamic time integration in the described calculations | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L106-L119] [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L257-L281] |
| G4 | NOT_APPLICABLE: no diffraction body, mesh or hydrodynamic coefficients in this API | [src/digitalmodel/structural/offshore_resilience/__init__.py:L66-L98] |
| G5 | Applicable: preserve pin, command, outcomes, citation audit and limitations with the calculation evidence | C51 |

| Check | Executable operation and evidence | Numeric pass criterion / unit | Gate |
|---|---|---|---|
| C01 | Run invalid platform type/depth, scale and negative-margin tests; preserve pytest node results | 0 missed expected ValueError exceptions, count; does not test non-finite inputs | G0; [tests/structural/offshore_resilience/test_minimum_facility.py:L106-L134] [tests/structural/offshore_resilience/test_minimum_facility.py:L243-L251] [tests/structural/offshore_resilience/test_lifecycle_planning.py:L120-L126] |
| C02 | Run ChecklistItem range/default cases, all-pass checklists and summary tests | 0 status/count mismatches, count; all-pass completion_ratio = 1.0 dimensionless | G0; [tests/structural/offshore_resilience/test_installation_checklist.py:L39-L73] [tests/structural/offshore_resilience/test_installation_checklist.py:L116-L121] |
| C03 | Run alert/severity consistency and SensorConfig threshold-order tests | 0 missed expected exceptions, count | G0; [tests/structural/offshore_resilience/test_structural_health.py:L66-L117] |
| C04 | Resolve every bracketed citation with git show at its stated pin, checking line bounds | 0 missing files/ranges, count; 10/10 manually inspected citations | G0; citation format demonstrated by [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L106-L119] |
| C11 | Run the complete module test directory through the command in section 4 | 72 passed, 0 failed, 0 skipped, 0 NOT_RUN, tests; package import must succeed | G1; [tests/structural/offshore_resilience/test_minimum_facility.py:L11-L20] |
| C21 | Hand compute 180 + 320; compare MinimumFacilityPlatform.total_weight_tonnes with 500 | Absolute error <= 1e-9 t; save arithmetic assertion record | G2; [tests/structural/offshore_resilience/test_minimum_facility.py:L46-L59] |
| C22 | Hand compute (60 + 300)/2; compare water_depth_midpoint_ft with 180 | Absolute error <= 1e-9 ft; save arithmetic assertion record | G2; [tests/structural/offshore_resilience/test_minimum_facility.py:L91-L104] |
| C23 | Hand compute (40 - 28 + 4)/40 from the inline cost case, in millions of USD; compare savings_fraction | Absolute error <= 1e-12 dimensionless; save arithmetic assertion record | G2; [tests/structural/offshore_resilience/test_minimum_facility.py:L186-L195] |
| C51 | Check the evidence record for all nine check IDs, code pin, commands, both test outcomes and limits | 9/9 records; 0 missing outcomes, count; this pack is not a signed report appendix | G5; output quantities documented in [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L106-L119] [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L151-L167] |

## 6. Known failure modes and pitfalls

The sweep counts source sites with defaults, assumptions or unchecked inputs; a counted site need not be a confirmed runtime defect. Each site below records the observed risk and either an executable detection check from section 5 or the absence of one. Code fixes are outside this pack.

Sweep commands, run against every package Python file at the header code pin:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|unit|cache|default' src/digitalmodel/structural/offshore_resilience -g '*.py'
rg -n 'threshold|margin|score|pace|rate|weight|depth|timestamp|min_value|max_value|sampling|return|raise' src/digitalmodel/structural/offshore_resilience -g '*.py'
```

No swallowed exception, continue, error-path None/list/dict return, external-text numeric conversion or cache write was found in the complete source read-through; `float()` normalises an internal scale span, not external text. Empty flag lists and initial empty containers are intentional but listed below to prevent interpreting absence as approval. [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L238-L259] [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L98-L156] [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L105-L138] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L144-L149]

| Site | Symptom / cause | Detection / follow-up | Pinned evidence |
|---|---|---|---|
| S01 | Approximate floating-depth threshold has no independently checked reference basis | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L46-L46] |
| S02 | Approximate large-fixed depth threshold lacks a checked reference basis | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L47-L47] |
| S03 | Approximate topsides-weight threshold lacks a checked reference basis | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L48-L48] |
| S04 | Physical weights/capacity/life/slots accept negative or non-finite values; ft/tonnes assumed without conversion | no detection check exists; issue pending (1) | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L83-L104] |
| S05 | Depth ordering is checked, but NaN can evade the comparison | no detection check exists; issue pending (1) | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L99-L104] |
| S06 | Only conventional CAPEX positivity is checked; NaN and invalid other costs/window pass | no detection check exists; issue pending (1) | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L139-L149] |
| S07 | Negative/NaN classifier inputs reach a plausible classification | no detection check exists; issue pending (1) | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L197-L204] |
| S08 | Scale range validation accepts fractional scale inputs | no detection check exists; issue pending (1) | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L238-L247] |
| S09 | Normalisation span and equal weights are heuristic assumptions, not a standards validation | C21-C23 cover arithmetic only; no detection check exists for this heuristic; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L249-L259] |
| S10 | An initially empty flag list can remain empty for unrecognised choices | no detection check exists; issue pending (1) | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L98-L98] |
| S11 | Unknown pile-removal text produces no pile-removal flag | no detection check exists; issue pending (1) | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L115-L140] |
| S12 | Non-positive jacket-piece count produces no jacket-lift flag | no detection check exists; issue pending (1) | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L142-L156] |
| S13 | Slot score clamps only above; negative counts can produce a negative score | no detection check exists; issue pending (1) | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L193-L194] |
| S14 | Fixed topology scores have no independent engineering calibration | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L197-L202] |
| S15 | Percentage units and upper saturation assumed; NaN is not rejected | no detection check exists; issue pending (1) | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L204-L205] |
| S16 | Equal-weight tieback composite has no independent engineering calibration | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L207-L208] |
| S17 | Composite weights are fixed policy assumptions | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L264-L265] |
| S18 | Recommendation cutoff is a fixed policy assumption | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L268-L269] |
| S19 | Recommendation cutoff is a fixed policy assumption | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L270-L271] |
| S20 | Recommendation cutoff is a fixed policy assumption | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L272-L273] |
| S21 | Initial PENDING status requires explicit caller completion | C02; pending counts exercised; issue pending (2) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L85-L85] |
| S22 | Initial None actual value requires explicit caller input | C02; initial pending path exercised; issue pending (2) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L86-L86] |
| S23 | Empty items accepted as a container default | no detection check exists; issue pending (2) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L106-L106] |
| S24 | Scalar bounds do not compare crane capacity with structure weight or verify units | no detection check exists; issue pending (2) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L88-L94] |
| S25 | Empty completion_ratio silently returns zero | no detection check exists; issue pending (2) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L121-L125] |
| S26 | Empty checklist is_complete is true despite zero completion ratio | no detection check exists; issue pending (2) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L128-L130] |
| S27 | crane_capacity_tonnes: literal screening range; caller unit t assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L166-L173] |
| S28 | vessel_deck_area_m2: literal screening range; caller unit m2 assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L174-L181] |
| S29 | max_hs_lift_m: literal screening range; caller unit m assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L183-L190] |
| S30 | required_window_days: literal screening range; caller unit days assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L191-L198] |
| S31 | max_driving_energy_kj: literal screening range; caller unit kJ assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L200-L207] |
| S32 | soil_refusal_blowcount_per_m: literal screening range; caller unit blows/m assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L208-L215] |
| S33 | crane_capacity_tonnes: literal screening range; caller unit t assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L265-L272] |
| S34 | max_hs_piling_m: literal screening range; caller unit m assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L274-L281] |
| S35 | metacentric_height_m: literal screening range; caller unit m assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L220-L227] |
| S36 | max_hs_towout_m: literal screening range; caller unit m assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L228-L235] |
| S37 | pile_run_penetration_m: literal screening range; caller unit m assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L287-L297] |
| S38 | soil_plug_factor: literal screening range; caller unit - assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L298-L305] |
| S39 | hammer_rated_energy_kj: literal screening range; caller unit kJ assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L307-L314] |
| S40 | blow_count_at_target_depth: literal screening range; caller unit blows/0.25m assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L315-L322] |
| S41 | max_driving_energy_kj: literal screening range; caller unit kJ assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L327-L334] |
| S42 | fatigue_damage_during_installation: literal screening range; caller unit - assumed; provenance unverified | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L336-L346] |
| S43 | UTC/timezone requirement and finite measurement value not enforced | no detection check exists; issue pending (1) | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L66-L96] |
| S44 | Sampling-rate positivity and finite thresholds not checked; NaN bypasses ordering | no detection check exists; issue pending (1) | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L124-L137] |
| S45 | SensorTemplate permits an empty sensor list by default | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L149-L149] |
| S46 | ACC_DECK_NE: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L26-L33] |
| S47 | ACC_DECK_SW: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L34-L41] |
| S48 | STR_NODE_A: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L43-L50] |
| S49 | STR_NODE_B: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L51-L58] |
| S50 | CP_MUD_N: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L60-L67] |
| S51 | CP_MID_S: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L68-L75] |
| S52 | MRU_SURGE: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L84-L91] |
| S53 | MRU_HEAVE: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L92-L99] |
| S54 | RISER_TEN_01: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L101-L108] |
| S55 | RISER_TEN_02: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L109-L116] |
| S56 | MOOR_LINE_01: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L118-L125] |
| S57 | MOOR_LINE_04: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L126-L133] |
| S58 | MOOR_LINE_07: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L134-L141] |
| S59 | BLD1_ROOT_FLAPWISE: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L152-L159] |
| S60 | BLD1_ROOT_EDGEWISE: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L160-L167] |
| S61 | TWR_ACC_MID: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L169-L176] |
| S62 | FND_TILT_X: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L178-L185] |
| S63 | FND_TILT_Y: literal sampling/alert defaults; threshold unit and calibration absent from schema | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L186-L193] |
| S64 | Annual pace divides by unchecked life; zero raises, negative budget/life yields invalid thresholds | no detection check exists; issue pending (1) | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L250-L251] |
| S65 | Warning multiplier is a fixed assumption | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L253-L254] |
| S66 | Critical multiplier is a fixed assumption | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L255-L255] |
| S67 | Fixed/floating storm trigger is a fixed acceleration assumption in g | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L259-L260] |
| S68 | Unknown asset type silently takes wind storm trigger | no detection check exists; issue pending (1) | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L261-L262] |
| S69 | Frequency drift percent is a fixed assumption | no detection check exists; issue pending (3) | [src/digitalmodel/structural/offshore_resilience/structural_health.py:L264-L265] |

sweep: 69 found

The issue search found #574 concerning missing standards resolver pages, a provenance lead rather than an observed calculation incident; current monitoring construction does not emit a Citation. No detection check exists here for resolving those standards pages; the separate standards-page work remains #574. [docs/plans/2026-05-05-issue-574-fowt-wiki-standards-pages.md:L8-L16] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L225-L272]

## 7. Lessons log

| Date | Entry | Lesson / evidence | Pack change | Shared-log reference |
|---|---|---|---|---|
| 2026-10-18 | Initial source audit | A passing scalar checklist does not establish capacity against a structure load; the described load requirement is absent from the comparator. [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L88-L94] [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L166-L173] | Record the limitation in sections 5 and 6 | issue pending (2) |
| 2026-10-18 | Initial provenance audit | Named standards in monitoring docstrings are not calculation-output citations; the returned threshold object contains numeric fields only. [src/digitalmodel/structural/offshore_resilience/structural_health.py:L9-L13] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L219-L222] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L267-L272] | Separate standards claims from regression evidence | #574; issue pending (3) |

## 8. Ownership

Review responsibility: engineering calculations quality manager; deputy role: deputy engineering calculations quality manager; producing role: engineering calculations contributor. Ambiguous clauses, unpassed checks and tolerance changes go to the family quality manager; unresolved scope or evidence decisions go to the maintainer. These are this pack's review requirements, not a record of completed approval.

UNVERIFIED: U3 — appointment of the deputy and completion of independent clearance; issue pending (5). Status remains DRAFT.

## 9. Dependencies and implementation inventory

Direct calculation dependencies are Python dataclasses, enum and typing; monitoring also uses datetime, and sensor construction delegates to local template helpers. The composite lifecycle function imports lifecycle_value_index locally; package exports are the downstream entry surface, not evidence of an external consumer. [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L14-L18] [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L248-L262] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L15-L20] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L181-L192] [src/digitalmodel/structural/offshore_resilience/__init__.py:L66-L98]

No solver version, file dataset or external numeric library is required by those calculation paths; generic inline regression inputs are the test data in section 4. [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L14-L259] [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L16-L281] [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L20-L352] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L15-L272] [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L7-L197]

Implementation-map audit record: source ref `origin/codex/implmap-work`, map commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, map data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. For map citations in this section only, the pin is the map commit rather than the header code revision. [docs/registry/implementation-map.yaml:L1-L4]

| Inventory measure | Recorded result |
|---|---|
| Accepted map families with an implementation path inside the package | 0 |
| Full inventory for those families, including implementations elsewhere | Empty |
| Counted families after descending in-package-row count and map-order tie break | 0 |
| Counted-family grep | NOT_APPLICABLE: no accepted map family has an in-package implementation row |
| Further second implementations counted by that grep | NOT_APPLICABLE; no uniqueness claim |
| A8 inventory outcome | PASS with empty inventory |

Reproduce by parsing the pinned YAML and selecting each family for which any `implementations` row has `path.startswith('src/digitalmodel/structural/offshore_resilience/')`; the selection is empty. Candidate entries in rejected/review-needed lists are not implementation rows. Monitoring is rejected as a non-implementation match, lifecycle scores are review-needed wind-force name matches, and the depth midpoint is a review-needed wave-kinematics name match. [docs/registry/implementation-map.yaml:L32027-L32031] [docs/registry/implementation-map.yaml:L45724-L45737] [docs/registry/implementation-map.yaml:L53641-L53647]

For any future counted family, rank by in-package rows, retain map order for ties, take at most three, omit a signal only when Python re matches the package-relative path case-insensitively, and use remaining patterns unchanged and case-sensitively on pinned Python files; assign hits to enclosing definitions, treating module statements as one definition. This is the reproducibility procedure, not a claim that a currently uncounted family was tested.

Whole-repository supplementary search at the code pin, including scripts, used the following command and produced 96 matching lines, all in this package and its four test files; manual classification found imports, exports, documentation and test calls rather than a second definition outside the package. These results do not change the counted-family outcome. [src/digitalmodel/structural/offshore_resilience/__init__.py:L25-L98] [tests/structural/offshore_resilience/test_lifecycle_planning.py:L8-L15]

```sh
git grep -n -E 'classify_bsee_platform|lifecycle_value_index|score_tieback_accommodation|marginal_field_lifecycle_score|decom_flags_for_design_choices|build_alert_thresholds|build_sensor_template|build_structure_checklist|build_wind_foundation_checklist' <code-pin> -- '*.py'
```

Map follow-up: add families for platform classification, mass/capacity/depth arithmetic, cost comparison, lifecycle/tieback scoring, decommissioning flags, checklist criteria and monitoring templates/alert calculations before counting them; issue pending (4). Their implementations are listed in section 3, including helper delegation rather than duplicate calculations. [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L106-L259] [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L72-L281] [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L88-L352] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L152-L272]

## 10. Coverage and open items

Read-through record: every package Python file, all four module test files and their test-package initializer were read at the code pin; no package subfolder contains another implementation. This is an inventory/read record, not a claim of independent engineering validation.

| Coverage | Files read / evidence |
|---|---|
| Package API and platform concepts | `__init__.py`; `minimum_facility.py` [src/digitalmodel/structural/offshore_resilience/__init__.py:L1-L100] [src/digitalmodel/structural/offshore_resilience/minimum_facility.py:L1-L259] |
| Checklists and lifecycle | `installation_checklist.py`; `lifecycle_planning.py` [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L1-L352] [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L1-L281] |
| Monitoring, including private helper | `structural_health.py`; `_sensor_templates.py` [src/digitalmodel/structural/offshore_resilience/structural_health.py:L1-L272] [src/digitalmodel/structural/offshore_resilience/_sensor_templates.py:L1-L197] |
| All module tests | `tests/structural/offshore_resilience/__init__.py`, `test_minimum_facility.py`, `test_lifecycle_planning.py`, `test_installation_checklist.py`, `test_structural_health.py`; complete collected node list retained in test evidence [tests/structural/offshore_resilience/test_minimum_facility.py:L1-L251] [tests/structural/offshore_resilience/test_lifecycle_planning.py:L1-L174] [tests/structural/offshore_resilience/test_installation_checklist.py:L1-L181] [tests/structural/offshore_resilience/test_structural_health.py:L1-L224] |
| Registry and capability declaration | Structural routing entry and structural-health capability [docs/registry/module-routing.yaml:L400-L418] [docs/capability-map/capabilities-added.yml:L71-L71] |
| Additional standard-identification reading | Structural API RP 2A-WSD plan, read for title and edition metadata only [docs/plans/2026-05-05-issue-581-api-rp-2a-wsd-structural.md:L8-L9] |
| Module-name documentation discoveries | Standards-page plan; marine-inspection plan; fitness-for-service plan; implementation map and capability declaration [docs/plans/2026-05-05-issue-574-fowt-wiki-standards-pages.md:L8-L16] [docs/plans/2026-05-05-issue-582-api-rp-2i-marine-inspection.md:L8-L10] [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L17-L21] [docs/registry/implementation-map.yaml:L32003-L32007] [docs/capability-map/capabilities-added.yml:L71-L71] |

Recount record: 72 collected module tests (18 platform/cost, 19 checklist, 14 lifecycle, 21 monitoring); all 72 passed at BASE and after. The complete suite, not only the six examples, was run in both cases.

| Open item | Responsible role | Tracking / evidence |
|---|---|---|
| Numeric, enum, finiteness and timezone validation gaps from section 6 | Engineering calculations maintainer | issue pending (1); [src/digitalmodel/structural/offshore_resilience/lifecycle_planning.py:L187-L208] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L86-L96] |
| Empty completion and load-dependent checklist adequacy | Engineering calculations maintainer | issue pending (2); [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L121-L130] [src/digitalmodel/structural/offshore_resilience/installation_checklist.py:L166-L173] |
| U1: standards provenance, editions/clauses and calculation-output sidecars; do not treat defaults as certified limits | Engineering calculations quality manager | #574 for resolver-page work; issue pending (3) for module basis; [docs/plans/2026-05-05-issue-574-fowt-wiki-standards-pages.md:L8-L16] [src/digitalmodel/structural/offshore_resilience/structural_health.py:L250-L272] |
| U2: monitoring units, calibration, reference frames and sign/phase conventions | Engineering calculations quality manager | issue pending (3); [src/digitalmodel/structural/offshore_resilience/structural_health.py:L103-L137] |
| Independent engineering benchmark absent from credited examples; retain regression classification | Engineering calculations quality manager | issue pending (3); [tests/structural/offshore_resilience/test_minimum_facility.py:L175-L195] |
| Add all section-3 method families to the implementation map | Engineering calculations maintainer | issue pending (4); section 9 audit record |
| U3: deputy appointment, independent clearance and replacement of draft references with opened issue numbers | Maintainer | issue pending (5); DRAFT status in header |

The calculation-citation contract was readable as a reference requirement; no standards text was opened and no licensed clause or tabulated value is certified by this pack. No private dataset was read or used; no sidecar entry is claimed. These are evidence-scope declarations.
