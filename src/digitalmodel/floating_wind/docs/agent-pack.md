# Floating-wind module knowledge pack

| Header | Value |
|---|---|
| Module | `floating_wind` |
| Repository | `digitalmodel` |
| Package | `src/digitalmodel/floating_wind` |
| Pack revision | 0, 2026-10-16 |
| Code revision | `b873edb364f0c55bf7789a7ae960ce9035233147` |
| Map reference | `codex/implmap-work`, `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; data revision `270c8759c374b67a7f032953e7fcdb14e0224461` |
| Analysis family | Engineering calculations (`EC`); AQG r0, Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT; independent clearance pending |

All repository citations use the code revision above; the implementation map at that revision is byte-identical to the map reference above. [docs/registry/implementation-map.yaml:L1-L4]

## 1. Purpose and scope

The package sizes semi-submersible, spar, tension-leg and barge concepts, screens variants using stability, motion, modal and optional mooring proxies, and computes economics and qualification overlays. [src/digitalmodel/floating_wind/floaters.py:L1-L33] [src/digitalmodel/floating_wind/screening.py:L1-L23] [src/digitalmodel/floating_wind/__init__.py:L23-L94]

The routing registry classifies the domain as beta and routes its key tests to `tests/floating_wind/`. [docs/registry/module-routing.yaml:L175-L180]

The closed-form tier is preliminary concept screening; site significant wave height is stored but the motion calculation uses peak period, natural periods and damping rather than a dimensional wave-response calculation. [src/digitalmodel/floating_wind/floaters.py:L22-L26] [src/digitalmodel/floating_wind/screening.py:L55-L67] [src/digitalmodel/floating_wind/screening.py:L131-L145]

The solver tier prepares analytic meshes or supplied-mesh placeholders, a diffraction specification and a motion/mooring configuration; `dispatch_case` only returns an unchanged case or changes its status to `dispatched`. A completed diffraction or coupled analysis is not established by that status. [src/digitalmodel/floating_wind/solver_tier.py:L172-L184] [src/digitalmodel/floating_wind/solver_tier.py:L292-L349]

The economics model holds annual energy constant across operating life and treats depth, distance and fabrication region as provenance rather than deriving costs from them. [src/digitalmodel/floating_wind/economics.py:L16-L30] [src/digitalmodel/floating_wind/economics.py:L148-L181]

## 2. Inputs and outputs

### Inputs

| Input | Unit | Allowed range | Default | Source and evidence |
|---|---|---|---|---|
| `TurbineTopside`: RNA/tower mass, hub height, tower-base height | t; m above still water | Masses and hub height > 0; tower-base height ≥ 0 | Masses/hub required; tower-base height has a code default | Caller or reference topside; [src/digitalmodel/floating_wind/floaters.py:L88-L131] |
| Semi: column count, diameter/radius, draft, pontoon height/volume, steel mass | count; m; m³; t | Count 3–6; other dimensions/mass > 0; pontoon height < draft | Count and centroid/added-mass ratios have code defaults | `SemiSubmersible`; [src/digitalmodel/floating_wind/floaters.py:L315-L349] |
| Spar: diameter, draft, steel mass | m; t | > 0; centroid fraction > 0 and ≤ 1; added-mass ratios ≥ 0 | Dimensions/mass required; coefficient defaults in code | `Spar`; [src/digitalmodel/floating_wind/floaters.py:L392-L407] |
| TLP: column/pontoon geometry, steel mass, tendon count, EA, free length | m; m³; t; count; N | Positive dimensions, mass, EA and free length; columns 3–6; tendons ≥ 3; pontoon height < draft | Column/tendon counts and steel-centroid ratio have code defaults | `TLP`; [src/digitalmodel/floating_wind/floaters.py:L440-L477] |
| Barge: length, beam, draft, steel mass, block/waterplane coefficients | m; t; dimensionless | Dimensions/mass > 0; shape coefficients > 0 and ≤ 1 | Shape, centroid and added-mass coefficients default in code | `Barge`; [src/digitalmodel/floating_wind/floaters.py:L537-L555] |
| `LoadCase`: significant height, peak period, steady load | m; s; kN | Height/period > 0; load ≥ 0 | Steady load 0.0 kN | Caller; [src/digitalmodel/floating_wind/screening.py:L55-L67] |
| `ScreeningCriteria`: GM, DAF, damping, wave band, stiffness, offset | m; dimensionless; s; kN/m; m | DAF > 1; damping > 0 and < 1; positive stiffness/ordered band are not validated | Code thresholds; mooring fields `None` | Caller; [src/digitalmodel/floating_wind/screening.py:L70-L89] |
| Financial parameters: discount, escalation, life | fractions/year; years | Discount > 0 and < 1; escalation ≥ 0 and < 1; life integer > 0 | Escalation 0.0; other fields required | `FinancialParameters`; [src/digitalmodel/floating_wind/economics.py:L86-L101] |
| Rating, turbine count, capacity factor; CAPEX, OPEX, DECOMEX | MW; count; fraction; USD/kW; USD/kW/year; USD/kW | Rating/count > 0; capacity factor > 0 and ≤ 1; costs ≥ 0 | DECOMEX 0.0 USD/kW; other costs required | Caller or packaged YAML; [src/digitalmodel/floating_wind/economics.py:L107-L182] |
| Reliability fraction, failure-related OPEX share, availability loss | dimensionless | First two in [0,1]; loss in [0,1) | Calibrated OPEX/availability defaults in code | `ReliabilityScenario`; [src/digitalmodel/floating_wind/reliability.py:L42-L71] |
| Scaling shares and reference capacity | dimensionless; MW | Shares in [0,1], sum ≤ 1 with code tolerance; reference capacities > 0 | Missing components retain their costs | `ComponentScaling`, `EconomiesOfScale`; [src/digitalmodel/floating_wind/scaling.py:L94-L146] [src/digitalmodel/floating_wind/scaling.py:L167-L174] |
| Maturity scores, criterion weights, verdict thresholds | dimensionless | Scores/thresholds in [0,1]; weights > 0; unique keys and ordered thresholds | Missing scores become zero | `ConceptMaturity`, `QualificationCriteria`; [src/digitalmodel/floating_wind/qualification.py:L78-L168] |
| Learning ratio, cumulative/reference deployment, discount; collaboration uplifts | dimensionless; consistent unit count | Learning > 0 and ≤ 1; deployment > 0; discount ≥ 0 and < 1; uplifts > 0 and ≤ 1 | Packaged learning/program inputs; zero default discount | [src/digitalmodel/floating_wind/standardization.py:L98-L148] [src/digitalmodel/floating_wind/collaboration.py:L65-L100] |
| Frequency/heading grid, mesh filename, depth/density | rad/s; degrees; m; kg/m³ | No validation in `FrequencyGrid` or `build_orcawave_spec` | Fixed grid and environment defaults | Caller; [src/digitalmodel/floating_wind/solver_tier.py:L59-L64] [src/digitalmodel/floating_wind/solver_tier.py:L190-L238] |

### Outputs and conventions

| Output | Unit / convention | Behaviour and evidence |
|---|---|---|
| `FloaterProperties` | Mass t; lengths m; area m²; periods s | Displacement, steel/ballast/topside split, KB/BM/KG/GM, gyration radius, feasibility and notes; [src/digitalmodel/floating_wind/floaters.py:L137-L172] |
| Vertical coordinates | z positive above keel for KB/KG; draft from keel to waterline | Solver CoG is translated to waterline datum using KG − draft; [src/digitalmodel/floating_wind/floaters.py:L28-L33] [src/digitalmodel/floating_wind/solver_tier.py:L199-L209] |
| Screening table | GM/offset margins m; modal margin s; DAF dimensionless | `CheckResult` records value, limit, boolean and signed margin; the minimum raw margin governs despite differing units; [src/digitalmodel/floating_wind/screening.py:L92-L128] [src/digitalmodel/floating_wind/screening.py:L148-L194] [src/digitalmodel/floating_wind/screening.py:L271-L273] |
| Trade-space records/front/correlations | Response units retained; Pearson r dimensionless | Feasibility filters and nonfinite-to-None conversion affect the available records; [src/digitalmodel/floating_wind/tradespace.py:L43-L92] [src/digitalmodel/floating_wind/tradespace.py:L172-L200] |
| `LCOEResult` | USD/MWh; USD; MWh | Discounted cost and energy components are returned without changing the caller's currency basis; [src/digitalmodel/floating_wind/economics.py:L215-L232] [src/digitalmodel/floating_wind/economics.py:L265-L295] |
| GDF and diffraction specification | Vertices m; waterline z=0; mass kg; frequency rad/s; heading degrees | GDF header includes length/gravity/symmetry; spec rounds mass, CoG and radii; panel orientation and physical loadability are not established by count tests; [src/digitalmodel/floating_wind/solver_tier.py:L59-L64] [src/digitalmodel/floating_wind/solver_tier.py:L80-L170] [src/digitalmodel/floating_wind/solver_tier.py:L211-L260] |
| JSON summaries / HTML data sheets | JSON result fields; formatted output units from model | Output paths are optional; concept JSON converts NaN to null and either infinity to the string `inf`; [src/digitalmodel/floating_wind/workflow.py:L97-L133] [src/digitalmodel/floating_wind/economics_workflow.py:L101-L135] |

UNVERIFIED: A common horizontal-axis, heading-origin and phase convention compatible with both target solvers is not established by these generators; see section 10 item U1. [src/digitalmodel/floating_wind/solver_tier.py:L59-L64] [src/digitalmodel/floating_wind/solver_tier.py:L211-L286]

## 3. Method and governing standards

| Method | Implementing definition / basis | Limits |
|---|---|---|
| Mass balance, CG, GM and uncoupled periods | `_assemble`, `_heave_period`, `_pitch_period`: ballast closes displacement minus lightweight; GM=KB+BM−KG; period from mass/restoring stiffness. [src/digitalmodel/floating_wind/floaters.py:L178-L309] | Negative ballast is clamped and flagged infeasible; ballast-centroid and added-mass assumptions require study-specific evidence. |
| Archetype hydrostatics | `SemiSubmersible.properties`, `Spar.properties`, `TLP.properties`, `Barge.properties`; analytic volume/waterplane inertia; TLP uses tendon axial stiffness. [src/digitalmodel/floating_wind/floaters.py:L351-L389] [src/digitalmodel/floating_wind/floaters.py:L409-L534] [src/digitalmodel/floating_wind/floaters.py:L557-L583] | Barge uses the beam-governed transverse inertia for the response named pitch; these are screening approximations. [src/digitalmodel/floating_wind/floaters.py:L561-L570] |
| DAF, stability, modal and steady offset | `_daf`, `_stability_check`, `_motion_check`, `_modal_check`, `_mooring_check`; DAF uses Tn/Tp, offset uses load/stiffness. [src/digitalmodel/floating_wind/screening.py:L106-L194] | No dimensional motion amplitude or nonlinear line solution is computed in these definitions. [src/digitalmodel/floating_wind/screening.py:L131-L194] |
| Variant generation and selection | `screen_concept` uses `ParametricStudy`; `_screen_one` aggregates the worst per-load-case motion/mooring check. [src/digitalmodel/floating_wind/screening.py:L243-L333] | Governing raw margins mix units. [src/digitalmodel/floating_wind/screening.py:L271-L273] |
| Pareto and correlation | `_dominates`, `pareto_front`, `_variance`, `_pearson`, `correlation_map`; feasible records by default. [src/digitalmodel/floating_wind/tradespace.py:L95-L200] [src/digitalmodel/floating_wind/tradespace.py:L215-L231] | Missing objectives affect dominance; constant/incomplete series are omitted. [src/digitalmodel/floating_wind/tradespace.py:L102-L109] [src/digitalmodel/floating_wind/tradespace.py:L186-L193] |
| Discounted LCOE | `annual_energy_production_mwh`, `capex_total_usd`, `opex_year_usd`, `decomex_total_usd`, `_discount_factor`, `compute_lcoe`; CAPEX at start, escalated OPEX, terminal DECOMEX, common discount factor for energy/cost. [src/digitalmodel/floating_wind/economics.py:L16-L30] [src/digitalmodel/floating_wind/economics.py:L238-L295] | Cost bases are inputs; calibration is not independent validation. [src/digitalmodel/floating_wind/economics.py:L297-L305] |
| Economic overlays | `apply_reliability`, `ComponentScaling.factor`, `scale_economics`, `LearningCurve.factor`, `StandardizationDiscount.factor`, `apply_standardization`, `apply_change`, `run_sweep`, `variant_lcoe`; wrapper functions delegate to the core. [src/digitalmodel/floating_wind/reliability.py:L74-L104] [src/digitalmodel/floating_wind/scaling.py:L113-L208] [src/digitalmodel/floating_wind/standardization.py:L122-L205] [src/digitalmodel/floating_wind/sweep.py:L115-L160] [src/digitalmodel/floating_wind/tradespace_economics.py:L55-L144] | `model_copy(update=...)` does not rerun the field validators; zero base LCOE causes a percentage denominator failure. [src/digitalmodel/floating_wind/sweep.py:L115-L157] |
| Qualification / collaboration | `trl_to_maturity`, `score_concept`, `rank_concepts`, `total_uplift`, `uplift_scores`; weighted mean, zero for missing scores, capped additive maturity uplifts. [src/digitalmodel/floating_wind/qualification.py:L63-L67] [src/digitalmodel/floating_wind/qualification.py:L156-L184] [src/digitalmodel/floating_wind/collaboration.py:L83-L100] | A score is an input-based assessment, not standards compliance. [src/digitalmodel/floating_wind/qualification.py:L15-L20] |
| Mesh/specification/configuration and response fold | `_gdf_text`, `box_gdf`, `cylinder_gdf`, `build_panel_mesh`, `build_orcawave_spec`, `build_orcaflex_motion_config`, `prepare_variant`, `dispatch_case`, `fold_rao_results`. [src/digitalmodel/floating_wind/solver_tier.py:L88-L390] | `prepare_shortlist` delegates; spec flags irregular-frequency removal but computes no lid treatment; supplied RAO values have no enforced unit contract. [src/digitalmodel/floating_wind/solver_tier.py:L241-L246] [src/digitalmodel/floating_wind/solver_tier.py:L328-L375] |

### Named standards: reference identification, not compliance

| Reference as named | Use located | Edition / clause status |
|---|---|---|
| DNV-RP-A203, Technology Qualification | Qualification scorer and collaboration rationale. [src/digitalmodel/floating_wind/qualification.py:L22-L26] [src/digitalmodel/floating_wind/collaboration.py:L27-L30] | UNVERIFIED: edition and clause/table are absent at these sites; clause text not checked against the licensed copy; U2. |
| DNV-OS-C301 | Floater/report references and stability-check basis. [src/digitalmodel/floating_wind/floaters.py:L35-L42] [src/digitalmodel/floating_wind/report.py:L37-L39] [src/digitalmodel/floating_wind/screening.py:L119-L128] | UNVERIFIED: full title, edition and clause/table are not identified by the module; clause text not checked against the licensed copy; U2. |
| DNV-RP-0286 | Floater/report references. [src/digitalmodel/floating_wind/floaters.py:L35-L42] [src/digitalmodel/floating_wind/report.py:L115-L121] | UNVERIFIED: module edition and clause are absent; related documentation records a different level of edition detail, not a proven conflict; clause text not checked against the licensed copy; U2. |
| API RP 17N, Recommended Practice for Subsea Production System Reliability, Technical Risk, and Integrity Management | Reliability-process rationale; qualification description also names API 17N. [src/digitalmodel/floating_wind/reliability.py:L23-L31] [src/digitalmodel/floating_wind/qualification.py:L15-L20] | UNVERIFIED: edition and clause/table are absent; clause text not checked against the licensed copy; U2. |

No standard table values are adopted here. UNVERIFIED: standards-to-equation applicability, coefficient provenance and emitted `Citation` sidecars are not established for this package; U2. [src/digitalmodel/floating_wind/qualification.py:L51-L67] [src/digitalmodel/floating_wind/screening.py:L70-L89] [src/digitalmodel/floating_wind/floaters.py:L68-L71]

## 4. Validated worked examples and regression examples

The analytical cases below validate the stated algebraic quantity only; physical validation of a complete coupled floating-wind concept is not established by these tests. [tests/floating_wind/test_floaters.py:L1-L5] [tests/floating_wind/test_solver_tier.py:L1-L5]

Execution record at the pinned code revision: 135 module tests passed at BASE, 135 passed in the isolated BASE snapshot and 135 passed AFTER the documentation changes; failures, skips and NOT_RUN module nodes were each 0. All ten case nodes below passed in every run. The command form was `PYTHONPATH=src uv run --no-sources python -m pytest tests/floating_wind -q -rA -p no:cacheprovider --basetemp=<scratch>` without an addopts override. Native solver loading and solving were not run. [tests/floating_wind/test_solver_tier.py:L1-L5]

| Case / kind | Independent basis or regression source | Inputs; expected result; tolerance | Existing test node / result |
|---|---|---|---|
| V1 analytical validation | Closed-form circular waterplane: I/V=(πD⁴/64)/(πD²T/4)=D²/(16T), independent algebraic reduction | D=20 m, T=120 m; BM=20²/(16×120) m; relative tolerance 1e-6 | `tests/floating_wind/test_floaters.py::test_well_sized_spar_is_stable_with_long_heave`; PASS (BASE and AFTER). [tests/floating_wind/test_floaters.py:L64-L74] |
| V2 analytical validation | Weighted-mean arithmetic (3×1+1×0)/4 | Weights 3 and 1; scores 1 and 0; result 0.75 dimensionless; pytest default relative tolerance 1e-6 | `tests/floating_wind/test_qualification.py::test_weighted_mean_is_correct`; PASS (BASE and AFTER). [tests/floating_wind/test_qualification.py:L67-L79] |
| V3 analytical validation | Static linear spring F/k | Governing force 3000 kN / stiffness 400 kN/m = 7.5 m; pytest default relative tolerance 1e-6 | `tests/floating_wind/test_screening.py::test_mooring_check_only_when_criteria_given`; PASS (BASE and AFTER). [tests/floating_wind/test_screening.py:L104-L128] |
| R1 regression | Semi mass/GM identities and broad period bands; no independently opened benchmark | Existing fixture; positive GM; total mass equals displacement; heave 15–28 s and pitch 18–40 s | `tests/floating_wind/test_floaters.py::test_semi_is_stable_and_in_plausible_period_band`; PASS (BASE and AFTER). [tests/floating_wind/test_floaters.py:L35-L61] |
| R2 regression | Specification uses the module's own properties | Mass=displacement×1000 kg/t within relative 1e-6; CG=KG−draft within 1e-3 m | `tests/floating_wind/test_solver_tier.py::test_orcawave_spec_inertia_from_properties`; PASS (BASE and AFTER). [tests/floating_wind/test_solver_tier.py:L111-L129] |
| R3 regression | Committed sizing workflow | 3 archetypes; 12 variants; passing spar shortlist; 0 passing barge variants; JSON readback | `tests/floating_wind/test_workflow.py::test_floating_wind_sizing_workflow_offline`; PASS (BASE and AFTER). [tests/floating_wind/test_workflow.py:L31-L51] |
| R4 regression / calibration | Packaged economics input; published basis not independently opened | 495 MW; LCOE anchor 184 USD/MWh ±2.0%; cost split reconstruction relative 1e-3 | `tests/floating_wind/test_economics_workflow.py::test_router_reproduces_base_case`; PASS (BASE and AFTER). [tests/floating_wind/test_economics_workflow.py:L41-L60] |
| R5 regression | Analytic panel bookkeeping, not mesh normals or solver validation | Box 60×60×12 m at 6×6×3 divisions; 108 panels; exactly 4 vertices per panel | `tests/floating_wind/test_solver_tier.py::test_box_gdf_is_well_formed`; PASS (BASE and AFTER). [tests/floating_wind/test_solver_tier.py:L80-L94] |
| R6 regression | Learning-curve identity for supplied calibration inputs | Reference 100 units, progress ratio 0.90; doubling factor 0.90, two doublings 0.81; pytest default relative tolerance 1e-6 | `tests/floating_wind/test_standardization.py::test_wrights_law_doubling`; PASS (BASE and AFTER). [tests/floating_wind/test_standardization.py:L40-L46] |
| R7 regression | Data-sheet generation and readback | 1 HTML file; document declaration and supplied trade-space section present | `tests/floating_wind/test_report.py::test_writes_html_file`; PASS (BASE and AFTER). [tests/floating_wind/test_report.py:L157-L164] |

UNVERIFIED: The economics presentation targets and literature-calibrated coefficients have not been checked against independently opened primary sources; U3. [src/digitalmodel/base_configs/modules/floating_wind_economics/economies_of_scale.yml:L3-L17] [examples/workflows/floating-wind-economics/input.yml:L1-L6]

## 5. QA gates and executable checks

AQG r0, section 10, Python calculations defines G0 inputs/units/citations, G1 imports/tests, G2 hand or published checks, G3 and G4 NOT_APPLICABLE, and section 9 applies G5 evidence to every family (AQG r0, sections 9–11).

| Gate | Applicability and module route |
|---|---|
| G0 | Applicable: model input guards C01–C03; conventions and unchecked provenance are recorded in sections 2, 3 and 6. [tests/floating_wind/test_floaters.py:L123-L133] [tests/floating_wind/test_economics.py:L107-L129] [tests/floating_wind/test_scaling.py:L129-L140] |
| G1 | Applicable: import through all module tests (C05), analytic preparation/specification checks and optional-output checks; preparation is the offline route. OrcaWave route: `build_orcawave_spec` → `prepare_variant` → written diffraction specification; `run_orcawave` execution remains comment-only. OrcaFlex route: `build_orcaflex_motion_config` → skeleton dictionary; a native model writer/loader is not implemented here. [src/digitalmodel/floating_wind/solver_tier.py:L292-L325] Native OrcaWave loading of the generated specification and OrcaFlex loading of a complete motion model remain NOT_RUN: solver interface/licence prohibited; version, warnings and native loadability are not established. [tests/floating_wind/test_solver_tier.py:L1-L5] [src/digitalmodel/floating_wind/solver_tier.py:L263-L286] [src/digitalmodel/floating_wind/solver_tier.py:L335-L349] |
| G2 | Applicable: C09–C11 are independent algebraic checks; the other checks exercise model invariants and documented fallbacks. [tests/floating_wind/test_floaters.py:L64-L74] [tests/floating_wind/test_screening.py:L104-L128] [tests/floating_wind/test_qualification.py:L67-L79] |
| G3 | NOT_APPLICABLE to the Python-calculations family (AQG r0, section 10); `dispatch_case` supplies no dynamics run. [src/digitalmodel/floating_wind/solver_tier.py:L335-L349] |
| G4 | NOT_APPLICABLE to the Python-calculations family (AQG r0, section 10); generated meshes are preparation artifacts, not a cleared diffraction analysis. [tests/floating_wind/test_solver_tier.py:L1-L5] |
| G5 | Applicable: C12/C13/C23 test report rendering and supplied provenance. A signed, fingerprint-linked AQG appendix is not established by those interface checks; follow-up #1019. [src/digitalmodel/floating_wind/report.py:L329-L386] [src/digitalmodel/floating_wind/economics_report.py:L178-L215] |

Every node below is executable with `PYTHONPATH=src uv run --no-sources python -m pytest <node> -q -rA -p no:cacheprovider --basetemp=<scratch>`; output-writing workflows require a temporary repository copy. The numeric criteria describe existing assertions, not new normative design limits. [tests/floating_wind/test_workflow.py:L18-L35] [tests/floating_wind/test_solver_tier.py:L80-L129]

| Check | Gate | Function/output and numeric pass criterion (unit) | Executable evidence |
|---|---|---|---|
| C01 | G0 | Pontoon height equal to draft produces 1 validation error; invalid concepts accepted: 0 count | `tests/floating_wind/test_floaters.py::test_pontoon_height_must_be_less_than_draft`; [tests/floating_wind/test_floaters.py:L122-L132] |
| C02 | G0 | Discount rate outside (0,1) produces 1 validation error; accepted invalid cases: 0 count | `tests/floating_wind/test_economics.py::test_invalid_discount_rate_rejected`; [tests/floating_wind/test_economics.py:L109-L129] |
| C03 | G0 | Scaling shares summing above 1 produce 1 validation error; accepted invalid shares: 0 count | `tests/floating_wind/test_scaling.py::test_component_scaling_fraction_guard`; [tests/floating_wind/test_scaling.py:L132-L134] |
| C04 | G2 | Partial maturity score < full score at supplied 0.8 dimensionless inputs | `tests/floating_wind/test_qualification.py::test_missing_criterion_counts_as_zero`; [tests/floating_wind/test_qualification.py:L59-L63] |
| C05 | G1 | All module nodes execute offline; failures 0 count, skips 0 count; baseline collection 135 cases | `tests/floating_wind/` (all 15 files); [docs/registry/module-routing.yaml:L175-L180] |
| C06 | G1 | Box GDF has 108 panels and exactly 4 vertices/panel; mismatch 0 count | `tests/floating_wind/test_solver_tier.py::test_box_gdf_is_well_formed`; [tests/floating_wind/test_solver_tier.py:L83-L87] |
| C07 | G1 | Generated mass relative error ≤1e-6 kg/kg and CG error ≤0.001 m against properties | `tests/floating_wind/test_solver_tier.py::test_orcawave_spec_inertia_from_properties`; [tests/floating_wind/test_solver_tier.py:L106-L121] |
| C08 | G1 | Absent output config produces 0 output-path keys (count); test does not inventory the filesystem | `tests/floating_wind/test_economics_workflow.py::test_router_without_output_is_noop_on_disk`; [tests/floating_wind/test_economics_workflow.py:L97-L104] |
| C09 | G2 | Spar BM relative error ≤1e-6 m/m against D²/(16T) | `tests/floating_wind/test_floaters.py::test_well_sized_spar_is_stable_with_long_heave`; [tests/floating_wind/test_floaters.py:L62-L70] |
| C10 | G2 | Supplied mooring force/stiffness gives 7.5 m offset at pytest relative tolerance 1e-6 m/m | `tests/floating_wind/test_screening.py::test_mooring_check_only_when_criteria_given`; [tests/floating_wind/test_screening.py:L100-L123] |
| C11 | G2 | Weighted maturity =0.75 dimensionless at pytest relative tolerance 1e-6 | `tests/floating_wind/test_qualification.py::test_weighted_mean_is_correct`; [tests/floating_wind/test_qualification.py:L66-L77] |
| C12 | G5 | 1 HTML file written and read back; document declaration and supplied trade-space section present (count ≥1 each) | `tests/floating_wind/test_report.py::test_writes_html_file`; [tests/floating_wind/test_report.py:L157-L164] |
| C13 | G5 | Trade-space heading occurrences =0 count when optional data are absent | `tests/floating_wind/test_report.py::test_tradespace_section_omitted_when_absent`; [tests/floating_wind/test_report.py:L117-L122] |
| C14 | G2 | Adjusted capacity factor =1.0 dimensionless at pytest relative tolerance 1e-6 | `tests/floating_wind/test_reliability.py::test_availability_gain_capped_at_full_cf`; [tests/floating_wind/test_reliability.py:L59-L64] |
| C15 | G2 | Absent criterion plus supplied uplift =0.15 dimensionless at pytest relative tolerance 1e-6 | `tests/floating_wind/test_collaboration.py::test_uplift_adds_absent_criterion_from_zero`; [tests/floating_wind/test_collaboration.py:L57-L62] |
| C16 | G2 | Pure per-MW scaling LCOE relative error ≤1e-9 USD/MWh per USD/MWh against base | `tests/floating_wind/test_scaling.py::test_custom_scaling_overrides_defaults`; [tests/floating_wind/test_scaling.py:L123-L129] |
| C17 | G2 | Year-1 and year-10 OPEX equal at zero escalation; relative error ≤1e-6 USD/USD | `tests/floating_wind/test_economics.py::test_zero_inflation_is_constant_opex`; [tests/floating_wind/test_economics.py:L95-L98] |
| C18 | G2 | Infeasible concept has 0.0 t ballast, false feasibility and ≥1 warning note (count) | `tests/floating_wind/test_floaters.py::test_infeasible_when_lightweight_exceeds_buoyancy`; [tests/floating_wind/test_floaters.py:L113-L119] |
| C19 | G2 | Negative GM m yields infinite pitch period s and ≥1 instability note (count) | `tests/floating_wind/test_floaters.py::test_slim_spar_with_heavy_topside_is_statically_unstable`; [tests/floating_wind/test_floaters.py:L73-L79] |
| C20 | G2 | Fixed draft column appears 0 times in correlation keys (count) | `tests/floating_wind/test_tradespace.py::test_correlation_drops_constant_columns`; [tests/floating_wind/test_tradespace.py:L96-L99] |
| C21 | G1 | Mooring result count =0 with both inputs absent and =1 with both supplied | `tests/floating_wind/test_screening.py::test_mooring_check_only_when_criteria_given`; [tests/floating_wind/test_screening.py:L100-L123] |
| C22 | G1 | Semi preparation returns 1 needs-supplied-mesh status; mesh paths=0 count; placeholder spec present | `tests/floating_wind/test_solver_tier.py::test_prepare_semi_needs_supplied_mesh`; [tests/floating_wind/test_solver_tier.py:L134-L141] |
| C23 | G5 | Caller-supplied tier, tool and version appear ≥1 time each (count); this is a rendering test, not solve attestation | `tests/floating_wind/test_report.py::test_high_fidelity_provenance_stamp`; [tests/floating_wind/test_report.py:L133-L142] |
| C24 | G1 | 1 prepared status without licence and 1 dispatched status when intent flag is true; no solver execution asserted | `tests/floating_wind/test_solver_tier.py::test_dispatch_is_fail_closed_without_license`; [tests/floating_wind/test_solver_tier.py:L150-L157] |
| C25 | G2 | Summed supplied technology uplift =0.30 dimensionless at pytest relative tolerance 1e-6 | `tests/floating_wind/test_collaboration.py::test_total_uplift_sums_per_criterion`; [tests/floating_wind/test_collaboration.py:L31-L36] |
| C26 | G2 | Updated capacity =500 MW; turbine/mooring costs unchanged at pytest relative tolerance 1e-6 USD/kW per USD/kW | `tests/floating_wind/test_scaling.py::test_scale_economics_updates_size`; [tests/floating_wind/test_scaling.py:L66-L76] |

## 6. Known failure modes and pitfalls

sweep: 49 found

The count denotes review sites, including intentional defaults and optional behaviour, rather than confirmed defects. Each site below identifies its detection boundary; a passing neighbouring test does not establish an untested failure case.

Sweep executed at the code pin on all 17 package Python files, supplemented by full source reading:

```text
rg -n 'except|continue|return None|return \[\]|return \{\}|\b0(?:\.0)?\b|0\.05|1970|float\(|int\(|units?|cache|OrcaFlex|OrcaWave|DNV|API' src/digitalmodel/floating_wind
```

| Site | Symptom / cause | Detection in section 5 | Tracking / date |
|---|---|---|---|
| S01 | Missing package metadata becomes unknown; specific cause is lost. [src/digitalmodel/floating_wind/report.py:L78-L81] | no detection check exists | issue pending (5); 2026-10-16 |
| S02 | Broad exception becomes unknown; unexpected metadata errors are swallowed. [src/digitalmodel/floating_wind/report.py:L75-L83] | no detection check exists | issue pending (5); 2026-10-16 |
| S03 | None/NaN becomes a dash; negative infinity renders with the positive infinity glyph. [src/digitalmodel/floating_wind/report.py:L150-L156] | no detection check exists | issue pending (1); 2026-10-16 |
| S04 | NaN becomes null and either infinity becomes inf; invalid state/sign is lost. [src/digitalmodel/floating_wind/workflow.py:L113-L124] | no detection check exists | issue pending (1); 2026-10-16 |
| S05 | Missing summary path returns None intentionally; omission lacks a targeted assertion. [src/digitalmodel/floating_wind/workflow.py:L126-L133] | no detection check exists | issue pending (5); 2026-10-16 |
| S06 | Missing summary path returns None intentionally. [src/digitalmodel/floating_wind/economics_workflow.py:L113-L119] | C08 | issue pending (5); 2026-10-16 |
| S07 | Missing HTML path returns None intentionally. [src/digitalmodel/floating_wind/economics_workflow.py:L121-L135] | C08 | issue pending (5); 2026-10-16 |
| S08 | Missing either mooring input removes the check; single-missing-input coverage is absent. [src/digitalmodel/floating_wind/screening.py:L180-L194] | C21 (both absent/both supplied); no detection check exists for one missing input | issue pending (5); 2026-10-16 |
| S09 | Semi/TLP returns no analytic mesh and needs_supplied_mesh; TLP-specific assertion is absent. [src/digitalmodel/floating_wind/solver_tier.py:L172-L184] | C22 (semi only); no detection check exists for TLP routing | #2435; 2026-10-16 |
| S10 | Nonfinite periods become None and cease to be numerical metric responses. [src/digitalmodel/floating_wind/tradespace.py:L43-L65] | no detection check exists | issue pending (1); 2026-10-16 |
| S11 | No feasible records returns an empty correlation dictionary without a reason field. [src/digitalmodel/floating_wind/tradespace.py:L182-L184] | no detection check exists | issue pending (5); 2026-10-16 |
| S12 | No feasible variants returns an empty LCOE list without a reason field. [src/digitalmodel/floating_wind/tradespace_economics.py:L93-L97] | no detection check exists | issue pending (5); 2026-10-16 |
| S13 | Fewer than two samples becomes zero variance; correlation column is omitted. [src/digitalmodel/floating_wind/tradespace.py:L215-L220] | C20 (constant columns only); no detection check exists for one-record input | issue pending (5); 2026-10-16 |
| S14 | Zero standard deviation returns internal NaN; not external-text parsing. [src/digitalmodel/floating_wind/tradespace.py:L223-L231] | no detection check exists | issue pending (5); 2026-10-16 |
| S15 | Continue treats a missing competing objective as worse; intentional dominance flow. [src/digitalmodel/floating_wind/tradespace.py:L102-L109] | no detection check exists | issue pending (5); 2026-10-16 |
| S16 | Continue excludes metadata columns; intentional algorithmic filtering. [src/digitalmodel/floating_wind/tradespace.py:L203-L212] | no detection check exists | issue pending (5); 2026-10-16 |
| S17 | Continue retains unconfigured CAPEX costs; intentional per-MW default. [src/digitalmodel/floating_wind/scaling.py:L167-L174] | C26 | issue pending (5); 2026-10-16 |
| S18 | Default zero OPEX escalation; omission and explicit zero have no separate status. [src/digitalmodel/floating_wind/economics.py:L93-L98] | C17 (explicit zero only); no detection check exists for omission | issue pending (5); 2026-10-16 |
| S19 | Omitted decommissioning cost becomes zero. [src/digitalmodel/floating_wind/economics.py:L180-L182] | no detection check exists | issue pending (5); 2026-10-16 |
| S20 | Missing pitch gyration radius defaults to zero in manually built properties. [src/digitalmodel/floating_wind/floaters.py:L159-L162] | no detection check exists | issue pending (5); 2026-10-16 |
| S21 | Default zero steady load produces zero offset even when mooring bounds are supplied. [src/digitalmodel/floating_wind/screening.py:L62-L67] | no detection check exists | issue pending (5); 2026-10-16 |
| S22 | Per-unit scaling share defaults to zero; intended identity. [src/digitalmodel/floating_wind/scaling.py:L100-L100] | C16 | issue pending (5); 2026-10-16 |
| S23 | Per-farm scaling share defaults to zero; intended identity. [src/digitalmodel/floating_wind/scaling.py:L101-L101] | C16 | issue pending (5); 2026-10-16 |
| S24 | Default zero discount is an identity; omission has no direct assertion. [src/digitalmodel/floating_wind/standardization.py:L134-L136] | no detection check exists | issue pending (5); 2026-10-16 |
| S25 | Missing maturity scores count as zero by design. [src/digitalmodel/floating_wind/qualification.py:L160-L168] | C04 | issue pending (5); 2026-10-16 |
| S26 | Missing accumulated uplift begins at zero; summation identity. [src/digitalmodel/floating_wind/collaboration.py:L83-L89] | C25 | issue pending (5); 2026-10-16 |
| S27 | Missing criterion begins at zero and receives supplied uplift. [src/digitalmodel/floating_wind/collaboration.py:L91-L100] | C15 | issue pending (5); 2026-10-16 |
| S28 | Fixed default damping is a calibration assumption, not measured site evidence. [src/digitalmodel/floating_wind/screening.py:L77-L79] | no detection check exists | issue pending (3); 2026-10-16 |
| S29 | Ballast centroid is a fixed fraction of draft, affecting KG and GM. [src/digitalmodel/floating_wind/floaters.py:L253-L260] | no detection check exists | issue pending (3); 2026-10-16 |
| S30 | Negative ballast clamps to zero with infeasible flag and note. [src/digitalmodel/floating_wind/floaters.py:L239-L249] | C18 | issue pending (5); 2026-10-16 |
| S31 | Nonpositive total mass substitutes hull CG; normally unreachable via positive public input models. [src/digitalmodel/floating_wind/floaters.py:L251-L263] | no detection check exists | issue pending (5); 2026-10-16 |
| S32 | Nonpositive total mass substitutes zero gyration radius. [src/digitalmodel/floating_wind/floaters.py:L275-L277] | no detection check exists | issue pending (5); 2026-10-16 |
| S33 | Missing or zero tendon period becomes NaN via truthiness. [src/digitalmodel/floating_wind/floaters.py:L279-L281] | no detection check exists | issue pending (1); 2026-10-16 |
| S34 | Nonpositive waterplane area returns infinite heave period. [src/digitalmodel/floating_wind/floaters.py:L178-L188] | no detection check exists | issue pending (1); 2026-10-16 |
| S35 | Nonpositive GM returns infinite pitch period as instability sentinel. [src/digitalmodel/floating_wind/floaters.py:L191-L210] | C19 | issue pending (1); 2026-10-16 |
| S36 | Every nonfinite natural period becomes ordinary DAF 1.0. [src/digitalmodel/floating_wind/screening.py:L106-L116] | no detection check exists | issue pending (1); 2026-10-16 |
| S37 | No finite periods produces passing modal status and infinite margin. [src/digitalmodel/floating_wind/screening.py:L148-L176] | no detection check exists | issue pending (1); 2026-10-16 |
| S38 | Geometry is metres and masses tonnes; period helpers convert to kg using fixed density/gravity. [src/digitalmodel/floating_wind/floaters.py:L28-L33] [src/digitalmodel/floating_wind/floaters.py:L68-L71] [src/digitalmodel/floating_wind/floaters.py:L184-L209] | C09 (BM algebra only); no detection check exists for unit rejection | issue pending (3); 2026-10-16 |
| S39 | kN divided by kN/m assumes metres; zero/negative stiffness lacks input guards. [src/digitalmodel/floating_wind/screening.py:L180-L194] | no detection check exists for invalid stiffness; C10 covers positive inputs only | issue pending (2); 2026-10-16 |
| S40 | MW/kW and currency-per-kW bases are assumed; fixed hours/year and constant AEP are used. [src/digitalmodel/floating_wind/economics.py:L159-L192] [src/digitalmodel/floating_wind/economics.py:L72-L72] [src/digitalmodel/floating_wind/economics.py:L238-L258] | no detection check exists | issue pending (3); 2026-10-16 |
| S41 | Environment defaults, tonne-to-kg and keel-to-waterline transforms; pitch gyration reused for all axes. [src/digitalmodel/floating_wind/solver_tier.py:L190-L238] | C07 (mass/vertical CG/pitch radius only); no detection check exists for environment or roll/yaw basis | #2435; 2026-10-16 |
| S42 | Grid assumes rad/s and degrees; GDF length/gravity/symmetry conventions need independent checks. [src/digitalmodel/floating_wind/solver_tier.py:L59-L93] | C06 (count only); no detection check exists for physical units or normals | #2435; 2026-10-16 |
| S43 | Hull VCG and added-mass/inertia coefficients have fixed defaults; spar/barge defaults also apply. [src/digitalmodel/floating_wind/floaters.py:L338-L342] [src/digitalmodel/floating_wind/floaters.py:L403-L407] [src/digitalmodel/floating_wind/floaters.py:L548-L555] | no detection check exists | issue pending (3); 2026-10-16 |
| S44 | TLP periods use fixed added-mass and gyration coefficients; ordinary added-mass period route is bypassed. [src/digitalmodel/floating_wind/floaters.py:L495-L526] | no detection check exists | issue pending (3); 2026-10-16 |
| S45 | OPEX/availability defaults are calibrated assumptions; capacity factor is capped. [src/digitalmodel/floating_wind/reliability.py:L47-L94] | C14 (cap only); no detection check exists for independent default calibration | issue pending (3); 2026-10-16 |
| S46 | TRL denominator is a code default; zero alternative denominator is not guarded. [src/digitalmodel/floating_wind/qualification.py:L51-L67] | no detection check exists | issue pending (2); 2026-10-16 |
| S47 | Learning/reference deployment defaults require a consistent deployment unit. [src/digitalmodel/floating_wind/standardization.py:L101-L128] | no detection check exists | issue pending (3); 2026-10-16 |
| S48 | Absent optional trade-space information returns empty text intentionally. [src/digitalmodel/floating_wind/report.py:L288-L290] | C13 | issue pending (5); 2026-10-16 |
| S49 | Empty optional sensitivity rows omit the section; no direct omission assertion. [src/digitalmodel/floating_wind/economics_report.py:L160-L162] | no detection check exists | issue pending (5); 2026-10-16 |

No external-text integer/float conversion, epoch-date sentinel, empty-result cache write or record-skipping continue was identified in the package sweep; `float("nan")` is an internal sentinel and `_split` returns a top-level-field discriminator rather than an error result. [src/digitalmodel/floating_wind/tradespace.py:L203-L231] [src/digitalmodel/floating_wind/sweep.py:L98-L105]

| Additional confirmed limitation | Detection | Tracking |
|---|---|---|
| Dispatch does not launch a solver; opening documentation overstates execution. [src/digitalmodel/floating_wind/solver_tier.py:L22-L24] [src/digitalmodel/floating_wind/solver_tier.py:L335-L349] | C24 verifies statuses only; no detection check exists for completed solver execution | #2435 |
| RAO-derived motion value has no enforced quantity/unit conversion to the DAF limit. [src/digitalmodel/floating_wind/solver_tier.py:L352-L375] | no detection check exists | #2435 |
| Motion config is a skeleton with no line geometry/stiffness/connections. [src/digitalmodel/floating_wind/solver_tier.py:L263-L286] | no detection check exists | #2435 |
| Unvalidated copy updates permit invalid transformed economic inputs; zero base LCOE divides by zero in sensitivity percentage. [src/digitalmodel/floating_wind/sweep.py:L115-L157] [src/digitalmodel/floating_wind/scaling.py:L149-L185] | no detection check exists | issue pending (2) |
| Governing minimum mixes metres, seconds and dimensionless margins. [src/digitalmodel/floating_wind/screening.py:L271-L273] | Existing smallest-margin test reproduces the ordering; no detection check exists for comparable severity | issue pending (2) |
| Metadata fallback and supplied provenance do not attest a signed AQG appendix or source fingerprint. [src/digitalmodel/floating_wind/report.py:L72-L121] [src/digitalmodel/floating_wind/economics_report.py:L178-L184] | C23 tests supplied text only; no detection check exists for attestation | #1019 |
| GDF box opposite faces use equal winding; cylinder bottom fan orientation and length scaling require independent physical qualification. [src/digitalmodel/floating_wind/solver_tier.py:L95-L170] | C06 verifies counts only; no detection check exists for orientation or native dimensional readback | #2435 |

Issue discovery includes the completed sizing/economics feature issues and the still-open solver-chain/report-provenance work; closure of a feature issue does not establish native execution or independent engineering validation. [src/digitalmodel/floating_wind/solver_tier.py:L335-L349] [src/digitalmodel/floating_wind/report.py:L86-L121]


## 7. Lessons log

| Date | Revision | Lesson / evidence | Pack change | Shared record |
|---|---|---|---|---|
| 2026-10-16 | 0 | Prepared/dispatched status does not establish a native solve. [src/digitalmodel/floating_wind/solver_tier.py:L335-L349] | Sections 1, 5, 6 and 9 distinguish preparation from execution | #2435 |
| 2026-10-16 | 0 | Nonfinite period handling can create ordinary passing proxies. [src/digitalmodel/floating_wind/screening.py:L106-L176] | Section 6 lists the individual fallback paths | issue pending (1) |
| 2026-10-16 | 0 | A calibration fit is not independent physical validation. [src/digitalmodel/base_configs/modules/floating_wind_economics/economies_of_scale.yml:L3-L17] | Section 4 separates analytical identities from regression cases | issue pending (3) |

## 8. Ownership

Engineering calculations quality manager: family review role; deputy: engineering calculations deputy quality manager; producing role: engineering calculation/documentation agent. These are review responsibilities for this draft, not a record of completed clearance (AQG r0, section 11).

UNVERIFIED: The deputy assignment and shared-log/index entry are not established; the maintainer will confirm them under section 10 item U6, issue pending (6).

Questions about an A8 implementation/helper classification go to the family quality manager; standards interpretation, changed tolerance and contradictory engineering inputs require that role's disposition before clearance (AQG r0, section 11; A8 completeness criterion stated below).

## 9. Dependencies and implementation inventory

### Package and solver interfaces

| Interface | Relationship / evidence |
|---|---|
| Pydantic and YAML | Model validation and configuration parsing; imports and use in model constructors/loaders. [src/digitalmodel/floating_wind/economics.py:L48-L55] [src/digitalmodel/floating_wind/economics.py:L194-L209] |
| Parametric study engine | `ParameterSweep`/`ParametricStudy` generate variant configurations. [src/digitalmodel/floating_wind/screening.py:L43-L43] [src/digitalmodel/floating_wind/screening.py:L308-L333] |
| Reporting backbone | Concept and economic data sheets use `ReportBackbone` and `ReportRenderer`. [src/digitalmodel/floating_wind/report.py:L362-L386] [src/digitalmodel/floating_wind/report.py:L460-L505] [src/digitalmodel/floating_wind/economics_report.py:L199-L284] |
| Packaged configuration | Economics, scaling, learning, collaboration and qualification loaders reference base configuration files. [src/digitalmodel/floating_wind/economics.py:L75-L81] [src/digitalmodel/floating_wind/scaling.py:L73-L79] [src/digitalmodel/floating_wind/standardization.py:L72-L78] [src/digitalmodel/floating_wind/collaboration.py:L56-L62] [src/digitalmodel/floating_wind/qualification.py:L54-L60] |
| OrcaWave and OrcaFlex | Named target programs; mesh/specification/configuration preparation and result-fold only. A target program version and completed solver result are not established. [src/digitalmodel/floating_wind/solver_tier.py:L190-L390] |

Every textual occurrence naming either target solver in the package is covered by these ranges; the names are read as text, not as evidence of a loaded interface or a completed run. [src/digitalmodel/floating_wind/__init__.py:L1-L7] [src/digitalmodel/floating_wind/floaters.py:L22-L26] [src/digitalmodel/floating_wind/screening.py:L20-L29] [src/digitalmodel/floating_wind/screening.py:L43-L43] [src/digitalmodel/floating_wind/workflow.py:L8-L12] [src/digitalmodel/floating_wind/report.py:L20-L24] [src/digitalmodel/floating_wind/report.py:L88-L116] [src/digitalmodel/floating_wind/solver_tier.py:L1-L30] [src/digitalmodel/floating_wind/solver_tier.py:L47-L51] [src/digitalmodel/floating_wind/solver_tier.py:L76-L76] [src/digitalmodel/floating_wind/solver_tier.py:L187-L390] [src/digitalmodel/floating_wind/economics_workflow.py:L8-L11]

### Counted map families

The package has one map row in each of two families, ranked by in-package row count with map order breaking the tie; both families are counted. The table below enumerates every implementation of those families, including entries outside the package and under scripts. Each inventory ordinal identifies exactly one map record through its line citation and source-file digest; original locators and symbols remain in the cited record rather than reproducing identifying discovery names. [docs/registry/implementation-map.yaml:L1-L4]

#### orcaflex-text-model-writers

In-package rows: 1; total inventory rows: 440. [docs/registry/implementation-map.yaml:L4933-L4935]

| Ordinal | Scope | Source file SHA-256 | Map record |
|---|---|---|---|
| 1 | external | `b61e48165fe3db5377aba04f7f87d6f56387d3bbba5641c926a5bb7cde281615` | [docs/registry/implementation-map.yaml:L4940-L4949] |
| 2 | external | `2d44837a4b9f273fa3d785464ceb9c0943e65c4f1f18ce3e50a04fe9a59b4354` | [docs/registry/implementation-map.yaml:L4950-L4958] |
| 3 | external | `9f1d4c574db162beb83c75e1e4f9d03add340c6f7a5b97a75bde7f68b9655497` | [docs/registry/implementation-map.yaml:L4959-L4968] |
| 4 | external | `adeeee4b1565fde355c6411b2ae91ae1e3d69cdc85bf5e5703633e78dbb3c4e3` | [docs/registry/implementation-map.yaml:L4969-L4978] |
| 5 | external | `51545973c3c399e9d9cf8eb150a30d1ae96274e3de1c43752c2e7439fca48be2` | [docs/registry/implementation-map.yaml:L4979-L4988] |
| 6 | external | `57d9cf9869101fd526ccc4c1609285d5c11f99cf6565aec3293969a80e27a446` | [docs/registry/implementation-map.yaml:L4989-L4998] |
| 7 | external | `a454bb54960298edfcd256117221d0ce6a765291e74d7401d7c6de4fb910c3c4` | [docs/registry/implementation-map.yaml:L4999-L5008] |
| 8 | external | `a454bb54960298edfcd256117221d0ce6a765291e74d7401d7c6de4fb910c3c4` | [docs/registry/implementation-map.yaml:L5009-L5020] |
| 9 | external | `c9f4d83e9174c4ac8805d9123aac7f4039940d44fcab2b4c6c4cef678b5446f4` | [docs/registry/implementation-map.yaml:L5021-L5030] |
| 10 | external | `51d61464bd8eb940b02ffcef35c71dda0df1bf032aee33f58fcec7bfb167d1a7` | [docs/registry/implementation-map.yaml:L5031-L5040] |
| 11 | external | `4c32da4d29f5eaf67f14ace27854b4528c903b26144abbff39f4e20b14db2a00` | [docs/registry/implementation-map.yaml:L5041-L5050] |
| 12 | external | `31998ae3f21e5af856141ae96e3f297a71d9fe8f08ad221e84c0005d2654f184` | [docs/registry/implementation-map.yaml:L5051-L5060] |
| 13 | external | `0e0774d6ce720b334c441f0a5ac5945ea97b854fd65070302a9ec6eab215efe5` | [docs/registry/implementation-map.yaml:L5061-L5070] |
| 14 | external | `4b78eb981be8181a63a9051a270cf03594f23aaf84eff254176246d498dc8e72` | [docs/registry/implementation-map.yaml:L5071-L5080] |
| 15 | external | `49f8483d6936371f3de1f0fec307a5e0b714b1d2be3dc3ea9d0f913595f66f5e` | [docs/registry/implementation-map.yaml:L5081-L5090] |
| 16 | external | `4894c9f1115ee0bf1d88bba6e8c44f133442bc6fbd0285eca9d06ee9a410d052` | [docs/registry/implementation-map.yaml:L5091-L5100] |
| 17 | external | `df012c8dd2361fd38e6dc282a5daf8826ef7c319266f5e25f7056b6ee6037089` | [docs/registry/implementation-map.yaml:L5101-L5110] |
| 18 | external | `182b3350bf69b1657724b8668f260de602b73563be89651b42bb60fc31d40f89` | [docs/registry/implementation-map.yaml:L5111-L5120] |
| 19 | external | `182b3350bf69b1657724b8668f260de602b73563be89651b42bb60fc31d40f89` | [docs/registry/implementation-map.yaml:L5121-L5132] |
| 20 | external | `182b3350bf69b1657724b8668f260de602b73563be89651b42bb60fc31d40f89` | [docs/registry/implementation-map.yaml:L5133-L5142] |
| 21 | external | `8f921418366ef24b99f7badd82941bcc3ca5bef5e13242b78fc4dda1e495614b` | [docs/registry/implementation-map.yaml:L5143-L5151] |
| 22 | external | `8f921418366ef24b99f7badd82941bcc3ca5bef5e13242b78fc4dda1e495614b` | [docs/registry/implementation-map.yaml:L5152-L5161] |
| 23 | external | `8f921418366ef24b99f7badd82941bcc3ca5bef5e13242b78fc4dda1e495614b` | [docs/registry/implementation-map.yaml:L5162-L5171] |
| 24 | external | `8f921418366ef24b99f7badd82941bcc3ca5bef5e13242b78fc4dda1e495614b` | [docs/registry/implementation-map.yaml:L5172-L5181] |
| 25 | external | `8f921418366ef24b99f7badd82941bcc3ca5bef5e13242b78fc4dda1e495614b` | [docs/registry/implementation-map.yaml:L5182-L5191] |
| 26 | external | `f284bdd745695f3988f61437dbc6a261aa20b1386afb78583866013819a3835a` | [docs/registry/implementation-map.yaml:L5192-L5201] |
| 27 | external | `f284bdd745695f3988f61437dbc6a261aa20b1386afb78583866013819a3835a` | [docs/registry/implementation-map.yaml:L5202-L5211] |
| 28 | external | `f284bdd745695f3988f61437dbc6a261aa20b1386afb78583866013819a3835a` | [docs/registry/implementation-map.yaml:L5212-L5223] |
| 29 | external | `493c3b19343e92d7265ef25be3eb943080d1d5d7cb86c1225622850b8ac29ec1` | [docs/registry/implementation-map.yaml:L5224-L5233] |
| 30 | external | `493c3b19343e92d7265ef25be3eb943080d1d5d7cb86c1225622850b8ac29ec1` | [docs/registry/implementation-map.yaml:L5234-L5243] |
| 31 | external | `26d2feef6eea371f7b9c698b335feffb058e47a92d42d704465b2e832987b9fc` | [docs/registry/implementation-map.yaml:L5244-L5253] |
| 32 | external | `af582d80fcbf774696e54827aecd93553b954120c2fed5e46b77773b25e89f73` | [docs/registry/implementation-map.yaml:L5254-L5263] |
| 33 | external | `378a7e8c3eca6a364b3beaee394cf8c7a570408042bca59b0c27d55b1c8f23a3` | [docs/registry/implementation-map.yaml:L5264-L5273] |
| 34 | external | `7fe7507a2527113bed9fe21f157b601f6d1b11617f6c90ac4ed635b977ec08d4` | [docs/registry/implementation-map.yaml:L5274-L5283] |
| 35 | external | `994537a47865e03b02fa7631028493bb1e7a4100b756ad098a8f5bf2df4c8ebf` | [docs/registry/implementation-map.yaml:L5284-L5293] |
| 36 | external | `6a3a001d5a7d9af5b97c393c89247652354ce995ea82a8af948cfaeecb699f26` | [docs/registry/implementation-map.yaml:L5294-L5303] |
| 37 | external | `6a3a001d5a7d9af5b97c393c89247652354ce995ea82a8af948cfaeecb699f26` | [docs/registry/implementation-map.yaml:L5304-L5313] |
| 38 | external | `b82b9a15db604f8f6c9f660967d356abfb762fe208d65d513174bea32ad81c7f` | [docs/registry/implementation-map.yaml:L5314-L5323] |
| 39 | external | `ce2e16fa2a2b0d7978ba310e3489427cfa1abee37845006df0faa4b0fbcef2f3` | [docs/registry/implementation-map.yaml:L5324-L5333] |
| 40 | external | `7ff1c48d14f969c9e713587fd8bc7acc91902ced3135797f98fbd9548c321b6a` | [docs/registry/implementation-map.yaml:L5334-L5343] |
| 41 | external | `7ff1c48d14f969c9e713587fd8bc7acc91902ced3135797f98fbd9548c321b6a` | [docs/registry/implementation-map.yaml:L5344-L5353] |
| 42 | external | `7ff1c48d14f969c9e713587fd8bc7acc91902ced3135797f98fbd9548c321b6a` | [docs/registry/implementation-map.yaml:L5354-L5363] |
| 43 | external | `5c7221c82de965b083521ade97b5b03364a218a825598a7b75b8425887542f05` | [docs/registry/implementation-map.yaml:L5364-L5372] |
| 44 | external | `5c7221c82de965b083521ade97b5b03364a218a825598a7b75b8425887542f05` | [docs/registry/implementation-map.yaml:L5373-L5382] |
| 45 | external | `5c7221c82de965b083521ade97b5b03364a218a825598a7b75b8425887542f05` | [docs/registry/implementation-map.yaml:L5383-L5392] |
| 46 | external | `4d1272e9ad0c08586683d884bf5b1a3778333ef6b18dcbfd2058fe63b6e75142` | [docs/registry/implementation-map.yaml:L5393-L5402] |
| 47 | external | `196f0a82b15e2c19f4a7173ef08b6d485b83b2868337e7acf995474cbfb55a19` | [docs/registry/implementation-map.yaml:L5403-L5415] |
| 48 | external | `196f0a82b15e2c19f4a7173ef08b6d485b83b2868337e7acf995474cbfb55a19` | [docs/registry/implementation-map.yaml:L5416-L5425] |
| 49 | external | `196f0a82b15e2c19f4a7173ef08b6d485b83b2868337e7acf995474cbfb55a19` | [docs/registry/implementation-map.yaml:L5426-L5437] |
| 50 | external | `196f0a82b15e2c19f4a7173ef08b6d485b83b2868337e7acf995474cbfb55a19` | [docs/registry/implementation-map.yaml:L5438-L5447] |
| 51 | external | `196f0a82b15e2c19f4a7173ef08b6d485b83b2868337e7acf995474cbfb55a19` | [docs/registry/implementation-map.yaml:L5448-L5457] |
| 52 | external | `9aba0526d958bc8813e99eccd391ac021df8c88b7defe709b71ad7ba1e805d53` | [docs/registry/implementation-map.yaml:L5458-L5467] |
| 53 | external | `9aba0526d958bc8813e99eccd391ac021df8c88b7defe709b71ad7ba1e805d53` | [docs/registry/implementation-map.yaml:L5468-L5477] |
| 54 | external | `9aba0526d958bc8813e99eccd391ac021df8c88b7defe709b71ad7ba1e805d53` | [docs/registry/implementation-map.yaml:L5478-L5487] |
| 55 | external | `1c742a287029103f6bed53ec36720ec98c96a3b63a99d2f1665eae33fb8a2bd4` | [docs/registry/implementation-map.yaml:L5488-L5497] |
| 56 | external | `c2d510170609e864f2190ab792ddbc816ab2eb72423cc9d374a0464c3f7bb9d3` | [docs/registry/implementation-map.yaml:L5498-L5507] |
| 57 | external | `c2d510170609e864f2190ab792ddbc816ab2eb72423cc9d374a0464c3f7bb9d3` | [docs/registry/implementation-map.yaml:L5508-L5519] |
| 58 | external | `c2d510170609e864f2190ab792ddbc816ab2eb72423cc9d374a0464c3f7bb9d3` | [docs/registry/implementation-map.yaml:L5520-L5531] |
| 59 | external | `1b4641eb349fc206afcdf83cd289134d37811189d654619f809137523556ef83` | [docs/registry/implementation-map.yaml:L5532-L5540] |
| 60 | external | `1b4641eb349fc206afcdf83cd289134d37811189d654619f809137523556ef83` | [docs/registry/implementation-map.yaml:L5541-L5550] |
| 61 | external | `411d0aef730852d7d3c8381bc42c3e7730738af508004585c682cf5ba5412641` | [docs/registry/implementation-map.yaml:L5551-L5560] |
| 62 | external | `1fee8bc5050579a5009434d7acf7ed13b9bf2db0338a0eb6024baf88b9524e65` | [docs/registry/implementation-map.yaml:L5561-L5570] |
| 63 | external | `1fee8bc5050579a5009434d7acf7ed13b9bf2db0338a0eb6024baf88b9524e65` | [docs/registry/implementation-map.yaml:L5571-L5580] |
| 64 | external | `1fee8bc5050579a5009434d7acf7ed13b9bf2db0338a0eb6024baf88b9524e65` | [docs/registry/implementation-map.yaml:L5581-L5593] |
| 65 | external | `0731c28152605764bcc0ad14a7a8adc1d86dd7c3b0a1165561e355aa464d9388` | [docs/registry/implementation-map.yaml:L5594-L5603] |
| 66 | external | `0731c28152605764bcc0ad14a7a8adc1d86dd7c3b0a1165561e355aa464d9388` | [docs/registry/implementation-map.yaml:L5604-L5615] |
| 67 | external | `0731c28152605764bcc0ad14a7a8adc1d86dd7c3b0a1165561e355aa464d9388` | [docs/registry/implementation-map.yaml:L5616-L5628] |
| 68 | external | `5bc90a11a476560633a33c7f0be7e568710ad923df33155802166d456790255a` | [docs/registry/implementation-map.yaml:L5629-L5638] |
| 69 | external | `0faf34a3d496104f787bc1d1f6544b5e0167e2c39ca2e7979b3d01361872f02e` | [docs/registry/implementation-map.yaml:L5639-L5648] |
| 70 | external | `0cb0ab0b0aa881427e574aa54ed74d75a14180e54ac5235bd3514d15778e67a9` | [docs/registry/implementation-map.yaml:L5649-L5658] |
| 71 | external | `7fe765c7f06878decb92ea96b703388bcd064e3caa705172605f23514f36c16b` | [docs/registry/implementation-map.yaml:L5659-L5667] |
| 72 | external | `20f87f8eebc5a6e0d0f77d8be88f73088c9a023c800f67111aff348aee0016d4` | [docs/registry/implementation-map.yaml:L5668-L5677] |
| 73 | external | `0cae4ecfa2688b40b62256270e2d60ea8ba3ee251a4a9aa5e6d292e9da7a3651` | [docs/registry/implementation-map.yaml:L5678-L5687] |
| 74 | external | `0cae4ecfa2688b40b62256270e2d60ea8ba3ee251a4a9aa5e6d292e9da7a3651` | [docs/registry/implementation-map.yaml:L5688-L5697] |
| 75 | external | `0bb29d89cab0016ec669783b1b2b457c2495437b625b8b26e409c80f873d8122` | [docs/registry/implementation-map.yaml:L5698-L5707] |
| 76 | external | `ff714349c70c69cd6e0d2708d8b14232b80d0980c1e0ff3965266625a29e5990` | [docs/registry/implementation-map.yaml:L5708-L5717] |
| 77 | external | `ff714349c70c69cd6e0d2708d8b14232b80d0980c1e0ff3965266625a29e5990` | [docs/registry/implementation-map.yaml:L5718-L5727] |
| 78 | external | `ff714349c70c69cd6e0d2708d8b14232b80d0980c1e0ff3965266625a29e5990` | [docs/registry/implementation-map.yaml:L5728-L5737] |
| 79 | external | `2ec45f51af70af127c1db398280381e275bf94414db26351ae59476d1bbe8edb` | [docs/registry/implementation-map.yaml:L5738-L5749] |
| 80 | external | `2ec45f51af70af127c1db398280381e275bf94414db26351ae59476d1bbe8edb` | [docs/registry/implementation-map.yaml:L5750-L5759] |
| 81 | external | `e4da4bb8d62929b19cf7f92ea318e31a2364664c63e55368803f41e13907ed47` | [docs/registry/implementation-map.yaml:L5760-L5769] |
| 82 | external | `3109a01b0d09d4fb4fe4057de6e1316d127271442fb7c4fc645333faef08652d` | [docs/registry/implementation-map.yaml:L5770-L5781] |
| 83 | external | `3109a01b0d09d4fb4fe4057de6e1316d127271442fb7c4fc645333faef08652d` | [docs/registry/implementation-map.yaml:L5782-L5791] |
| 84 | external | `25fba0b193ce503130ae509dfa4de6f34197ec3a462f3763c45c763e23756830` | [docs/registry/implementation-map.yaml:L5792-L5801] |
| 85 | external | `25fba0b193ce503130ae509dfa4de6f34197ec3a462f3763c45c763e23756830` | [docs/registry/implementation-map.yaml:L5802-L5811] |
| 86 | external | `ad9e4f4a4c70b3f5515155f6b4980e2ec5fba114a0c733fb4fefd0631187153f` | [docs/registry/implementation-map.yaml:L5812-L5823] |
| 87 | external | `a5b2b9771a5664ef748f852d08ba9b3103814f63b1d9fd81fb339df20dfd2b40` | [docs/registry/implementation-map.yaml:L5824-L5833] |
| 88 | external | `7e7a1aca24bfd34faeabe89c7bf55ffed62313997df5c5705560657a9b70c8c0` | [docs/registry/implementation-map.yaml:L5834-L5843] |
| 89 | external | `f3596dbf048f630d0d927f13dffa0714e342875a5e3dfdedd7acbb7bebee6744` | [docs/registry/implementation-map.yaml:L5844-L5853] |
| 90 | external | `b561606ac62c4fba6be68a8b2262ca6e58136ebd03d758f9a7a9e45a0cf5810f` | [docs/registry/implementation-map.yaml:L5854-L5863] |
| 91 | external | `65bb1443ec3e98629b897c12aefbdab7f7fe444c1e14dc482b750bd1dcba8355` | [docs/registry/implementation-map.yaml:L5864-L5872] |
| 92 | external | `65bb1443ec3e98629b897c12aefbdab7f7fe444c1e14dc482b750bd1dcba8355` | [docs/registry/implementation-map.yaml:L5873-L5882] |
| 93 | external | `3ea4c14fad066254842f62d81e8f2d9036f40f4ea51bf1dd7e8ea0f525703f17` | [docs/registry/implementation-map.yaml:L5883-L5891] |
| 94 | external | `0f59cd67ba151f334d832eb25d131da06d2d69ed18fa9309b4e17d4b4d0d1dae` | [docs/registry/implementation-map.yaml:L5892-L5901] |
| 95 | external | `8751e287495486cbcce5d3bd41e6fae1da45805c1c5a90b1b961eb64a024e38f` | [docs/registry/implementation-map.yaml:L5902-L5911] |
| 96 | external | `1e9de587b79eee3b945ae88ba8d285e4024cbb279551cd3741ba73fa4a612582` | [docs/registry/implementation-map.yaml:L5912-L5921] |
| 97 | external | `1e6f72e47ba335e41a48c9d2cb3f759667d7d8e0ab19a1caf2a37e1b8ef51bff` | [docs/registry/implementation-map.yaml:L5922-L5933] |
| 98 | external | `6ed0947fc14c9ebabb67021257efb9a53287f9e27d8d613b268fd2e81a0ffc7b` | [docs/registry/implementation-map.yaml:L5934-L5943] |
| 99 | external | `c07682b70f529c27575942edf4483145d8b8ad2c0c6c62b81a97b892cf3dc632` | [docs/registry/implementation-map.yaml:L5944-L5953] |
| 100 | external | `c07682b70f529c27575942edf4483145d8b8ad2c0c6c62b81a97b892cf3dc632` | [docs/registry/implementation-map.yaml:L5954-L5965] |
| 101 | external | `b4359396225bf6a48cddb3c5b3eca4400a37413544d4bc7a9db65d0cd91abaa9` | [docs/registry/implementation-map.yaml:L5966-L5975] |
| 102 | external | `b4359396225bf6a48cddb3c5b3eca4400a37413544d4bc7a9db65d0cd91abaa9` | [docs/registry/implementation-map.yaml:L5976-L5985] |
| 103 | external | `b4359396225bf6a48cddb3c5b3eca4400a37413544d4bc7a9db65d0cd91abaa9` | [docs/registry/implementation-map.yaml:L5986-L5994] |
| 104 | external | `94282a1c559bb23f52bd7454a022a80a11f9f6db35285d28d0600eccd5ca3cb6` | [docs/registry/implementation-map.yaml:L5995-L6004] |
| 105 | external | `63cf2d628a398a8edcd5b08e8643b0639008e5e460327f16c306e67fff045268` | [docs/registry/implementation-map.yaml:L6005-L6014] |
| 106 | external | `fb0e609658125f47fb635954ebc7406c3e0f3a1708f5b6946032459081a3293c` | [docs/registry/implementation-map.yaml:L6015-L6024] |
| 107 | external | `e24afe379d6ca435241651399ccd55049b8fe6a872854f1c2f8ba5b5df794f8b` | [docs/registry/implementation-map.yaml:L6025-L6034] |
| 108 | external | `58d418e6179d8171e558d62128a26dad311a568011c9f7cf3703bf38adf82bb3` | [docs/registry/implementation-map.yaml:L6035-L6044] |
| 109 | external | `8d924e92f27e5d1e4f9cceb6c152e0879ede12ba5dd8b0dad7f94c030263cc69` | [docs/registry/implementation-map.yaml:L6045-L6054] |
| 110 | external | `df16cb1a65954fc653779d87bdfe61b703ebcd26ae846bb3f728b9408c6a5794` | [docs/registry/implementation-map.yaml:L6055-L6064] |
| 111 | external | `e13db23b9225a6fd454d1207fbcd2aab1c83291fde0488ae7f2bf2633066d951` | [docs/registry/implementation-map.yaml:L6065-L6074] |
| 112 | external | `e13db23b9225a6fd454d1207fbcd2aab1c83291fde0488ae7f2bf2633066d951` | [docs/registry/implementation-map.yaml:L6075-L6084] |
| 113 | external | `e13db23b9225a6fd454d1207fbcd2aab1c83291fde0488ae7f2bf2633066d951` | [docs/registry/implementation-map.yaml:L6085-L6096] |
| 114 | external | `e13db23b9225a6fd454d1207fbcd2aab1c83291fde0488ae7f2bf2633066d951` | [docs/registry/implementation-map.yaml:L6097-L6109] |
| 115 | external | `e13db23b9225a6fd454d1207fbcd2aab1c83291fde0488ae7f2bf2633066d951` | [docs/registry/implementation-map.yaml:L6110-L6119] |
| 116 | external | `f39e9f8e874f9aa801edec9b0d0d4da98fbec4020f1c2c35b30616961381bbdb` | [docs/registry/implementation-map.yaml:L6120-L6129] |
| 117 | external | `f39e9f8e874f9aa801edec9b0d0d4da98fbec4020f1c2c35b30616961381bbdb` | [docs/registry/implementation-map.yaml:L6130-L6141] |
| 118 | external | `f39e9f8e874f9aa801edec9b0d0d4da98fbec4020f1c2c35b30616961381bbdb` | [docs/registry/implementation-map.yaml:L6142-L6153] |
| 119 | external | `b3779964c2a1aaa1629487b83c66ffaea413a4c641f6e57ee79ccfc4ddf86bf5` | [docs/registry/implementation-map.yaml:L6154-L6163] |
| 120 | external | `62f3abf7b69a9250314e131d0eb9940ebfa3c482995a5748dd17e2839e27c658` | [docs/registry/implementation-map.yaml:L6164-L6173] |
| 121 | external | `0e9a95cfd0321ef5b914a2967144b88531bac9af89a3840408257b5efac51070` | [docs/registry/implementation-map.yaml:L6174-L6183] |
| 122 | external | `0e9a95cfd0321ef5b914a2967144b88531bac9af89a3840408257b5efac51070` | [docs/registry/implementation-map.yaml:L6184-L6193] |
| 123 | external | `0e9a95cfd0321ef5b914a2967144b88531bac9af89a3840408257b5efac51070` | [docs/registry/implementation-map.yaml:L6194-L6203] |
| 124 | package | `40b8a37903f8d559cd3661967a6c49f069fdd3a02ef242ec2efb6a2e91d14e3f` | [docs/registry/implementation-map.yaml:L6204-L6213] |
| 125 | external | `f19ba168c5a242900e1a85417a35a89397706910630a93500667dd4e313b74f7` | [docs/registry/implementation-map.yaml:L6214-L6223] |
| 126 | external | `4c0599b2d137847e1ee6ba49f2517c96f431e69cfb63549f09648b7f482ae45f` | [docs/registry/implementation-map.yaml:L6224-L6233] |
| 127 | external | `c9eead71d35310ba3fc7e3a560fa94c9eb2d9e8666de3ddd1400c4eb3d9d5382` | [docs/registry/implementation-map.yaml:L6234-L6243] |
| 128 | external | `c6c10dc97c393f5f92605b9d469d8a5cea77e1a18bd6c08383a60045540d2b2d` | [docs/registry/implementation-map.yaml:L6244-L6253] |
| 129 | external | `23d60ce0043ad300995d488ba780c59a2705d61f1374297581bfce5986279dc4` | [docs/registry/implementation-map.yaml:L6254-L6263] |
| 130 | external | `b8ebfd8216187233cdcb6d3cbe4541afd2d89afc59504b0206f9c12c04ceb10c` | [docs/registry/implementation-map.yaml:L6264-L6273] |
| 131 | external | `0b3f3858b91fa8c4f28731158ff8e1e6e0a7361c87aa7667332a52f178826d52` | [docs/registry/implementation-map.yaml:L6274-L6283] |
| 132 | external | `709df020fbbf303022c61d02a6e1615ff01b404608fc256313ff92a2a73fa2f7` | [docs/registry/implementation-map.yaml:L6284-L6293] |
| 133 | external | `0a8026e1597e69b75ae70944857962aeddd93fd836e10ebced556c87587d2c3e` | [docs/registry/implementation-map.yaml:L6294-L6303] |
| 134 | external | `0a8026e1597e69b75ae70944857962aeddd93fd836e10ebced556c87587d2c3e` | [docs/registry/implementation-map.yaml:L6304-L6313] |
| 135 | external | `9bfade35665bacd49cdbaca316cc2ed4db86eb0e00aef48114475861cba8884e` | [docs/registry/implementation-map.yaml:L6314-L6323] |
| 136 | external | `9bfade35665bacd49cdbaca316cc2ed4db86eb0e00aef48114475861cba8884e` | [docs/registry/implementation-map.yaml:L6324-L6333] |
| 137 | external | `9bfade35665bacd49cdbaca316cc2ed4db86eb0e00aef48114475861cba8884e` | [docs/registry/implementation-map.yaml:L6334-L6343] |
| 138 | external | `9bfade35665bacd49cdbaca316cc2ed4db86eb0e00aef48114475861cba8884e` | [docs/registry/implementation-map.yaml:L6344-L6353] |
| 139 | external | `9bfade35665bacd49cdbaca316cc2ed4db86eb0e00aef48114475861cba8884e` | [docs/registry/implementation-map.yaml:L6354-L6363] |
| 140 | external | `9bfade35665bacd49cdbaca316cc2ed4db86eb0e00aef48114475861cba8884e` | [docs/registry/implementation-map.yaml:L6364-L6373] |
| 141 | external | `9bfade35665bacd49cdbaca316cc2ed4db86eb0e00aef48114475861cba8884e` | [docs/registry/implementation-map.yaml:L6374-L6383] |
| 142 | external | `270a48d2645fbf752f6a5aeba2c2e545a5ea5af12390f3f934a415ff92b133a2` | [docs/registry/implementation-map.yaml:L6384-L6393] |
| 143 | external | `270a48d2645fbf752f6a5aeba2c2e545a5ea5af12390f3f934a415ff92b133a2` | [docs/registry/implementation-map.yaml:L6394-L6403] |
| 144 | external | `639ca1953da8cc44dd7e0ed93441530923db0b60f2245e7b07812f809d3f7f3b` | [docs/registry/implementation-map.yaml:L6404-L6413] |
| 145 | external | `639ca1953da8cc44dd7e0ed93441530923db0b60f2245e7b07812f809d3f7f3b` | [docs/registry/implementation-map.yaml:L6414-L6423] |
| 146 | external | `6ee3b6d5f26f5d260725489eab7e05c8cbc5be77687c80a7127bbd5baf420915` | [docs/registry/implementation-map.yaml:L6424-L6436] |
| 147 | external | `6ee3b6d5f26f5d260725489eab7e05c8cbc5be77687c80a7127bbd5baf420915` | [docs/registry/implementation-map.yaml:L6437-L6448] |
| 148 | external | `6ee3b6d5f26f5d260725489eab7e05c8cbc5be77687c80a7127bbd5baf420915` | [docs/registry/implementation-map.yaml:L6449-L6458] |
| 149 | external | `6ee3b6d5f26f5d260725489eab7e05c8cbc5be77687c80a7127bbd5baf420915` | [docs/registry/implementation-map.yaml:L6459-L6468] |
| 150 | external | `6ee3b6d5f26f5d260725489eab7e05c8cbc5be77687c80a7127bbd5baf420915` | [docs/registry/implementation-map.yaml:L6469-L6478] |
| 151 | external | `6ee3b6d5f26f5d260725489eab7e05c8cbc5be77687c80a7127bbd5baf420915` | [docs/registry/implementation-map.yaml:L6479-L6490] |
| 152 | external | `35a1404e6afe3c3d413094cf7a38f443e2186439fd37566c9b994af5f78602c2` | [docs/registry/implementation-map.yaml:L6491-L6500] |
| 153 | external | `35a1404e6afe3c3d413094cf7a38f443e2186439fd37566c9b994af5f78602c2` | [docs/registry/implementation-map.yaml:L6501-L6510] |
| 154 | external | `35a1404e6afe3c3d413094cf7a38f443e2186439fd37566c9b994af5f78602c2` | [docs/registry/implementation-map.yaml:L6511-L6520] |
| 155 | external | `35a1404e6afe3c3d413094cf7a38f443e2186439fd37566c9b994af5f78602c2` | [docs/registry/implementation-map.yaml:L6521-L6530] |
| 156 | external | `35a1404e6afe3c3d413094cf7a38f443e2186439fd37566c9b994af5f78602c2` | [docs/registry/implementation-map.yaml:L6531-L6540] |
| 157 | external | `35a1404e6afe3c3d413094cf7a38f443e2186439fd37566c9b994af5f78602c2` | [docs/registry/implementation-map.yaml:L6541-L6550] |
| 158 | external | `35a1404e6afe3c3d413094cf7a38f443e2186439fd37566c9b994af5f78602c2` | [docs/registry/implementation-map.yaml:L6551-L6560] |
| 159 | external | `921910eded3a599f99bac6706f4ce52d2c903b7b79894d5e2db9a5da9a942134` | [docs/registry/implementation-map.yaml:L6561-L6570] |
| 160 | external | `921910eded3a599f99bac6706f4ce52d2c903b7b79894d5e2db9a5da9a942134` | [docs/registry/implementation-map.yaml:L6571-L6580] |
| 161 | external | `921910eded3a599f99bac6706f4ce52d2c903b7b79894d5e2db9a5da9a942134` | [docs/registry/implementation-map.yaml:L6581-L6590] |
| 162 | external | `921910eded3a599f99bac6706f4ce52d2c903b7b79894d5e2db9a5da9a942134` | [docs/registry/implementation-map.yaml:L6591-L6600] |
| 163 | external | `921910eded3a599f99bac6706f4ce52d2c903b7b79894d5e2db9a5da9a942134` | [docs/registry/implementation-map.yaml:L6601-L6610] |
| 164 | external | `921910eded3a599f99bac6706f4ce52d2c903b7b79894d5e2db9a5da9a942134` | [docs/registry/implementation-map.yaml:L6611-L6620] |
| 165 | external | `921910eded3a599f99bac6706f4ce52d2c903b7b79894d5e2db9a5da9a942134` | [docs/registry/implementation-map.yaml:L6621-L6630] |
| 166 | external | `25b147cfce060355711256023b2ec7356675ff3d840b672e58f98d8fb33db196` | [docs/registry/implementation-map.yaml:L6631-L6640] |
| 167 | external | `25b147cfce060355711256023b2ec7356675ff3d840b672e58f98d8fb33db196` | [docs/registry/implementation-map.yaml:L6641-L6650] |
| 168 | external | `25b147cfce060355711256023b2ec7356675ff3d840b672e58f98d8fb33db196` | [docs/registry/implementation-map.yaml:L6651-L6659] |
| 169 | external | `4620d37dec29111e3771e9b2da78166971e9d9faebe6a738961e8a5f47846d7f` | [docs/registry/implementation-map.yaml:L6660-L6672] |
| 170 | external | `70068280efe88765f7e93b47f18112363c609495d2a1a947f1e33b654b422310` | [docs/registry/implementation-map.yaml:L6673-L6682] |
| 171 | external | `10a32bd4fd6b3bf570b1864c1e06d12ce031c8f72368e76ff2aadc8f42c4322a` | [docs/registry/implementation-map.yaml:L6683-L6692] |
| 172 | external | `10a32bd4fd6b3bf570b1864c1e06d12ce031c8f72368e76ff2aadc8f42c4322a` | [docs/registry/implementation-map.yaml:L6693-L6704] |
| 173 | external | `5d3fa4216222f1418f8de46bc507200aaeb91e40b199a373215e099fd160a07a` | [docs/registry/implementation-map.yaml:L6705-L6714] |
| 174 | external | `718e8aac4384a242b7d39ae3dcb493cd44b97607ab3cae063efa6416054df79c` | [docs/registry/implementation-map.yaml:L6715-L6724] |
| 175 | external | `718e8aac4384a242b7d39ae3dcb493cd44b97607ab3cae063efa6416054df79c` | [docs/registry/implementation-map.yaml:L6725-L6734] |
| 176 | external | `718e8aac4384a242b7d39ae3dcb493cd44b97607ab3cae063efa6416054df79c` | [docs/registry/implementation-map.yaml:L6735-L6744] |
| 177 | external | `718e8aac4384a242b7d39ae3dcb493cd44b97607ab3cae063efa6416054df79c` | [docs/registry/implementation-map.yaml:L6745-L6754] |
| 178 | external | `718e8aac4384a242b7d39ae3dcb493cd44b97607ab3cae063efa6416054df79c` | [docs/registry/implementation-map.yaml:L6755-L6764] |
| 179 | external | `718e8aac4384a242b7d39ae3dcb493cd44b97607ab3cae063efa6416054df79c` | [docs/registry/implementation-map.yaml:L6765-L6774] |
| 180 | external | `c36d23a4c1d9ee37b415145a1220490c3487d776c8ba3dcbdeb7b6c28ec7c41b` | [docs/registry/implementation-map.yaml:L6775-L6784] |
| 181 | external | `fe321c5b495f9b42bc1408f16fc93d1198086fdadebba902899ac437589c6494` | [docs/registry/implementation-map.yaml:L6785-L6794] |
| 182 | external | `1dbe1735ebcae88b929f98387f473a76c9274b95d3bf96bd285795393f577a54` | [docs/registry/implementation-map.yaml:L6795-L6804] |
| 183 | external | `5561cd463af3b7db4285b8d24f81cbd1da2a5ae4e933eaabe3c72bd7ff9ecf96` | [docs/registry/implementation-map.yaml:L6805-L6814] |
| 184 | external | `9b023f6ce3888b5386076c7e50693d1f75c5c6167ab129b0af3b981271686919` | [docs/registry/implementation-map.yaml:L6815-L6824] |
| 185 | external | `7a8d178102a5a511a74478ec8825a7e34500fdf2a413e8eb28c9d119d3901151` | [docs/registry/implementation-map.yaml:L6825-L6834] |
| 186 | external | `06a7ca2e257470d1200e9f2820bde950203b547db7bca5d462b56073778ae0f7` | [docs/registry/implementation-map.yaml:L6835-L6844] |
| 187 | external | `9dc0b4d0d2629688bac1c737496e5af97f7e02da9fbe37042db3d3efa0397038` | [docs/registry/implementation-map.yaml:L6845-L6854] |
| 188 | external | `7c17d2b8cd3b30a18195799f68d2cf4a64bc6c200460ce12f22531049d27127f` | [docs/registry/implementation-map.yaml:L6855-L6864] |
| 189 | external | `7c17d2b8cd3b30a18195799f68d2cf4a64bc6c200460ce12f22531049d27127f` | [docs/registry/implementation-map.yaml:L6865-L6874] |
| 190 | external | `584fdbdbed9fb8abf82b716a8ce54570b4efb50259cbd67192f57d67440bdb2e` | [docs/registry/implementation-map.yaml:L6875-L6884] |
| 191 | external | `68b2b20d015de71cfe4962e3b48f0699679874726da5661ff396dd2099f83205` | [docs/registry/implementation-map.yaml:L6885-L6894] |
| 192 | external | `0f43b76138a6e229be46431947a200db2cb184e5c751786f95cc9f98700c084e` | [docs/registry/implementation-map.yaml:L6895-L6904] |
| 193 | external | `0f43b76138a6e229be46431947a200db2cb184e5c751786f95cc9f98700c084e` | [docs/registry/implementation-map.yaml:L6905-L6914] |
| 194 | external | `7f5de88242c86c4d6c9deb0d07ae863345076649a375ed2c5ba1b9a1884cc6ee` | [docs/registry/implementation-map.yaml:L6915-L6927] |
| 195 | external | `7f5de88242c86c4d6c9deb0d07ae863345076649a375ed2c5ba1b9a1884cc6ee` | [docs/registry/implementation-map.yaml:L6928-L6937] |
| 196 | external | `2219e6bfe94dac89ecfffa777ede8344fcb83b8597396dd10750b3331e004b4c` | [docs/registry/implementation-map.yaml:L6938-L6947] |
| 197 | external | `70703f07fb1f3c95a799256c9dd83ebbbaf576601809052313377389310a89c5` | [docs/registry/implementation-map.yaml:L6948-L6957] |
| 198 | external | `70703f07fb1f3c95a799256c9dd83ebbbaf576601809052313377389310a89c5` | [docs/registry/implementation-map.yaml:L6958-L6967] |
| 199 | external | `ad70dd032b272309e58e4a2b896dca80962370ddfd68ffaaa0fac7886d4fd57d` | [docs/registry/implementation-map.yaml:L6968-L6977] |
| 200 | external | `ad70dd032b272309e58e4a2b896dca80962370ddfd68ffaaa0fac7886d4fd57d` | [docs/registry/implementation-map.yaml:L6978-L6987] |
| 201 | external | `da3be683eda4db860cc28722627d8159ab6578ea6b099b671edcad59e6175b38` | [docs/registry/implementation-map.yaml:L6988-L6997] |
| 202 | external | `da3be683eda4db860cc28722627d8159ab6578ea6b099b671edcad59e6175b38` | [docs/registry/implementation-map.yaml:L6998-L7007] |
| 203 | external | `2eab2409002076d6794cfdacce365510bc0bd7acfe957122f829618e5ea528b4` | [docs/registry/implementation-map.yaml:L7008-L7017] |
| 204 | external | `5c34417b7437d2a1e2e533d56531d7dc41d1a17dc32071e675f596761b6a82b4` | [docs/registry/implementation-map.yaml:L7018-L7031] |
| 205 | external | `5c34417b7437d2a1e2e533d56531d7dc41d1a17dc32071e675f596761b6a82b4` | [docs/registry/implementation-map.yaml:L7032-L7041] |
| 206 | external | `5c34417b7437d2a1e2e533d56531d7dc41d1a17dc32071e675f596761b6a82b4` | [docs/registry/implementation-map.yaml:L7042-L7051] |
| 207 | external | `74f425c7434405dc579ef1c673531381fcd70903599d727e94b6b8e2d2ad249a` | [docs/registry/implementation-map.yaml:L7052-L7061] |
| 208 | external | `7d0c429cf34635a565d53325c538355cebef9f7e319c73a5a0fc3889523f4f5a` | [docs/registry/implementation-map.yaml:L7062-L7071] |
| 209 | external | `7d0c429cf34635a565d53325c538355cebef9f7e319c73a5a0fc3889523f4f5a` | [docs/registry/implementation-map.yaml:L7072-L7081] |
| 210 | external | `fee4659cc897507c332c78b823fa7dadc8f4bff56cf6ad560540c16f99e09660` | [docs/registry/implementation-map.yaml:L7082-L7091] |
| 211 | external | `fee4659cc897507c332c78b823fa7dadc8f4bff56cf6ad560540c16f99e09660` | [docs/registry/implementation-map.yaml:L7092-L7101] |
| 212 | external | `75fd1223bd5efce362205e0ea7ab06500b9c5f9bc765ed06d70971089a10dfaf` | [docs/registry/implementation-map.yaml:L7102-L7111] |
| 213 | external | `75fd1223bd5efce362205e0ea7ab06500b9c5f9bc765ed06d70971089a10dfaf` | [docs/registry/implementation-map.yaml:L7112-L7121] |
| 214 | external | `40c792152ad143f4ab9501c575fb96617d3b9389a766b05a8d61048e4eae3aaf` | [docs/registry/implementation-map.yaml:L7122-L7131] |
| 215 | external | `40c792152ad143f4ab9501c575fb96617d3b9389a766b05a8d61048e4eae3aaf` | [docs/registry/implementation-map.yaml:L7132-L7141] |
| 216 | external | `a0821b13287a69aa71101f5360c21428fa437dfcde38c1d766f300fcf7641de8` | [docs/registry/implementation-map.yaml:L7142-L7153] |
| 217 | external | `a0821b13287a69aa71101f5360c21428fa437dfcde38c1d766f300fcf7641de8` | [docs/registry/implementation-map.yaml:L7154-L7163] |
| 218 | external | `fa5488800062120096d12fff2e7e8bc58adbd99b1a5efbcccf0dc9ec12e07be3` | [docs/registry/implementation-map.yaml:L7164-L7176] |
| 219 | external | `fa5488800062120096d12fff2e7e8bc58adbd99b1a5efbcccf0dc9ec12e07be3` | [docs/registry/implementation-map.yaml:L7177-L7186] |
| 220 | external | `fa5488800062120096d12fff2e7e8bc58adbd99b1a5efbcccf0dc9ec12e07be3` | [docs/registry/implementation-map.yaml:L7187-L7196] |
| 221 | external | `fa5488800062120096d12fff2e7e8bc58adbd99b1a5efbcccf0dc9ec12e07be3` | [docs/registry/implementation-map.yaml:L7197-L7207] |
| 222 | external | `d26046cafb43c33a72a5d8af17dde9fa052f907d0c0ceceef5c206acd2f697d2` | [docs/registry/implementation-map.yaml:L7208-L7217] |
| 223 | external | `dd6be7ff0d3681881fc0d20556a6da5ebad050879cc8470709d4c85909893782` | [docs/registry/implementation-map.yaml:L7218-L7227] |
| 224 | external | `dfe4d0e41c651005f9b6050c3adb2d08beb13ab47bf0a13100fbf94b20d9dfc8` | [docs/registry/implementation-map.yaml:L7228-L7237] |
| 225 | external | `4b4c882db15f413b326d8a20ee5913d31f04126f3e8d0e10f5cccb275ae4714a` | [docs/registry/implementation-map.yaml:L7238-L7247] |
| 226 | external | `ec438cacad27fa6db3f61a7124990a7a8784a531b3222f159c14fac4a5861d81` | [docs/registry/implementation-map.yaml:L7248-L7257] |
| 227 | external | `93e067adcfb9b43f6207dbe09eba352eba6f9a7dc9cb34b8ffa5cf71a146a7af` | [docs/registry/implementation-map.yaml:L7258-L7269] |
| 228 | external | `93e067adcfb9b43f6207dbe09eba352eba6f9a7dc9cb34b8ffa5cf71a146a7af` | [docs/registry/implementation-map.yaml:L7270-L7279] |
| 229 | external | `9fa56f1086f3e4db56bdd0d383630de47c04f8fd9f9618da07d379adca2c1b54` | [docs/registry/implementation-map.yaml:L7280-L7289] |
| 230 | external | `bad58fc5a165cf12d74040e61d74b08da034d5624491d32f851d61241115a2c8` | [docs/registry/implementation-map.yaml:L7290-L7299] |
| 231 | external | `a780223de23f903de74aa4fe20abd8ca068d855215a77fb4816fb74f65ef2590` | [docs/registry/implementation-map.yaml:L7300-L7309] |
| 232 | external | `45ac94500bed345150806043acfbab0eb6f8e40ea6e415a50942194db08438d9` | [docs/registry/implementation-map.yaml:L7310-L7319] |
| 233 | external | `277d9a7a714414d7550f65397881adc326052d2233d0d1096609839d33feb52b` | [docs/registry/implementation-map.yaml:L7320-L7329] |
| 234 | external | `277d9a7a714414d7550f65397881adc326052d2233d0d1096609839d33feb52b` | [docs/registry/implementation-map.yaml:L7330-L7339] |
| 235 | external | `4928551191073b37905ce51008de21cc637cba9b300878362582c97127f2845d` | [docs/registry/implementation-map.yaml:L7340-L7349] |
| 236 | external | `4928551191073b37905ce51008de21cc637cba9b300878362582c97127f2845d` | [docs/registry/implementation-map.yaml:L7350-L7359] |
| 237 | external | `4928551191073b37905ce51008de21cc637cba9b300878362582c97127f2845d` | [docs/registry/implementation-map.yaml:L7360-L7369] |
| 238 | external | `4928551191073b37905ce51008de21cc637cba9b300878362582c97127f2845d` | [docs/registry/implementation-map.yaml:L7370-L7379] |
| 239 | external | `944cd5202670ce9bcd39e31921628a71c52cac918479f881e6dcb29fb04b13d0` | [docs/registry/implementation-map.yaml:L7380-L7389] |
| 240 | external | `944cd5202670ce9bcd39e31921628a71c52cac918479f881e6dcb29fb04b13d0` | [docs/registry/implementation-map.yaml:L7390-L7399] |
| 241 | external | `944cd5202670ce9bcd39e31921628a71c52cac918479f881e6dcb29fb04b13d0` | [docs/registry/implementation-map.yaml:L7400-L7409] |
| 242 | external | `944cd5202670ce9bcd39e31921628a71c52cac918479f881e6dcb29fb04b13d0` | [docs/registry/implementation-map.yaml:L7410-L7419] |
| 243 | external | `b9989424faa5cf3cc6f62ca2600a59a3bb8967eba5ba7d6fd01faad0c78bf2c2` | [docs/registry/implementation-map.yaml:L7420-L7429] |
| 244 | external | `b9989424faa5cf3cc6f62ca2600a59a3bb8967eba5ba7d6fd01faad0c78bf2c2` | [docs/registry/implementation-map.yaml:L7430-L7439] |
| 245 | external | `dc65d316830ea493f6c24e836efd2b099ce54aba182030f2bf8b41e8b9151c1e` | [docs/registry/implementation-map.yaml:L7440-L7449] |
| 246 | external | `6a6bc51fc38ae4bc99010ca9dfb820f49a2c7975e6ed20242c01f9374b392c4b` | [docs/registry/implementation-map.yaml:L7450-L7459] |
| 247 | external | `ced78472e8c301c4f63aad6081b9b411ef17de67dec7b6d5ba0f352aa50f32da` | [docs/registry/implementation-map.yaml:L7460-L7469] |
| 248 | external | `d565d7485e0e1b63f1720ee95881024f4c282d504b6106c623d0bcb797f55279` | [docs/registry/implementation-map.yaml:L7470-L7479] |
| 249 | external | `caec588c21001b8730b3a026e62a90270e2daf390a30ea0347af9acacc2de596` | [docs/registry/implementation-map.yaml:L7480-L7489] |
| 250 | external | `caec588c21001b8730b3a026e62a90270e2daf390a30ea0347af9acacc2de596` | [docs/registry/implementation-map.yaml:L7490-L7499] |
| 251 | external | `6267aadeefff29e0d72a11b4a8bfe3dd78449a56ee78f2731417b7fc0c246c8c` | [docs/registry/implementation-map.yaml:L7500-L7509] |
| 252 | external | `6267aadeefff29e0d72a11b4a8bfe3dd78449a56ee78f2731417b7fc0c246c8c` | [docs/registry/implementation-map.yaml:L7510-L7519] |
| 253 | external | `0c22f1a535e99ccda72abcd0909114d4bd0c4bd6b689fdf53c981a10c62bc42b` | [docs/registry/implementation-map.yaml:L7520-L7529] |
| 254 | external | `09d3aa9dc0f1a1861913245885214c3e51fd8fe8dcdd78e2ddfb6413ed3da107` | [docs/registry/implementation-map.yaml:L7530-L7539] |
| 255 | external | `0c712bb2fb4f241a2e51df4788b5d196a72c97223494fc2366d5f0649c073c0f` | [docs/registry/implementation-map.yaml:L7540-L7551] |
| 256 | external | `0c712bb2fb4f241a2e51df4788b5d196a72c97223494fc2366d5f0649c073c0f` | [docs/registry/implementation-map.yaml:L7552-L7561] |
| 257 | external | `0c712bb2fb4f241a2e51df4788b5d196a72c97223494fc2366d5f0649c073c0f` | [docs/registry/implementation-map.yaml:L7562-L7571] |
| 258 | external | `0c712bb2fb4f241a2e51df4788b5d196a72c97223494fc2366d5f0649c073c0f` | [docs/registry/implementation-map.yaml:L7572-L7581] |
| 259 | external | `0c712bb2fb4f241a2e51df4788b5d196a72c97223494fc2366d5f0649c073c0f` | [docs/registry/implementation-map.yaml:L7582-L7591] |
| 260 | external | `da7819a575370b8848c8739026632e58f33ae6e88618194768954e5d22e2eea8` | [docs/registry/implementation-map.yaml:L7592-L7603] |
| 261 | external | `da7819a575370b8848c8739026632e58f33ae6e88618194768954e5d22e2eea8` | [docs/registry/implementation-map.yaml:L7604-L7613] |
| 262 | external | `9983e53e8f91b1d3e97d8738c1bee66d12d0d71a6c3606ebe0f63e0cc5dcf801` | [docs/registry/implementation-map.yaml:L7614-L7626] |
| 263 | external | `9983e53e8f91b1d3e97d8738c1bee66d12d0d71a6c3606ebe0f63e0cc5dcf801` | [docs/registry/implementation-map.yaml:L7627-L7636] |
| 264 | external | `9983e53e8f91b1d3e97d8738c1bee66d12d0d71a6c3606ebe0f63e0cc5dcf801` | [docs/registry/implementation-map.yaml:L7637-L7646] |
| 265 | external | `9983e53e8f91b1d3e97d8738c1bee66d12d0d71a6c3606ebe0f63e0cc5dcf801` | [docs/registry/implementation-map.yaml:L7647-L7656] |
| 266 | external | `9983e53e8f91b1d3e97d8738c1bee66d12d0d71a6c3606ebe0f63e0cc5dcf801` | [docs/registry/implementation-map.yaml:L7657-L7666] |
| 267 | external | `82548412c57fb2a2bea88bb267e134b72f85950468c22c0abd82ae6bb40df99d` | [docs/registry/implementation-map.yaml:L7667-L7676] |
| 268 | external | `82548412c57fb2a2bea88bb267e134b72f85950468c22c0abd82ae6bb40df99d` | [docs/registry/implementation-map.yaml:L7677-L7686] |
| 269 | external | `82548412c57fb2a2bea88bb267e134b72f85950468c22c0abd82ae6bb40df99d` | [docs/registry/implementation-map.yaml:L7687-L7696] |
| 270 | external | `b9576b0cb62b447ed777cf1a6e7cc3eaf73d73a6b5360bdc1b909d5329e023e2` | [docs/registry/implementation-map.yaml:L7697-L7706] |
| 271 | external | `b9576b0cb62b447ed777cf1a6e7cc3eaf73d73a6b5360bdc1b909d5329e023e2` | [docs/registry/implementation-map.yaml:L7707-L7716] |
| 272 | external | `1ca50d244025c322c83c59c5d854a6f42fdf962a7e595819f4e61ea19f9239b8` | [docs/registry/implementation-map.yaml:L7717-L7729] |
| 273 | external | `1ca50d244025c322c83c59c5d854a6f42fdf962a7e595819f4e61ea19f9239b8` | [docs/registry/implementation-map.yaml:L7730-L7739] |
| 274 | external | `1ca50d244025c322c83c59c5d854a6f42fdf962a7e595819f4e61ea19f9239b8` | [docs/registry/implementation-map.yaml:L7740-L7749] |
| 275 | external | `b86cf2bec62372721bf6bf66b450fe884e82c728a70d79e89d0e07394dd3b453` | [docs/registry/implementation-map.yaml:L7750-L7762] |
| 276 | external | `b86cf2bec62372721bf6bf66b450fe884e82c728a70d79e89d0e07394dd3b453` | [docs/registry/implementation-map.yaml:L7763-L7774] |
| 277 | external | `b86cf2bec62372721bf6bf66b450fe884e82c728a70d79e89d0e07394dd3b453` | [docs/registry/implementation-map.yaml:L7775-L7784] |
| 278 | external | `b86cf2bec62372721bf6bf66b450fe884e82c728a70d79e89d0e07394dd3b453` | [docs/registry/implementation-map.yaml:L7785-L7794] |
| 279 | external | `b86cf2bec62372721bf6bf66b450fe884e82c728a70d79e89d0e07394dd3b453` | [docs/registry/implementation-map.yaml:L7795-L7804] |
| 280 | external | `b86cf2bec62372721bf6bf66b450fe884e82c728a70d79e89d0e07394dd3b453` | [docs/registry/implementation-map.yaml:L7805-L7816] |
| 281 | external | `204514959585767d3c5f6f424443dd6004835b3b13a380bef330da65ad0a65bd` | [docs/registry/implementation-map.yaml:L7817-L7826] |
| 282 | external | `169c6ceb806ac0629bae99522f452ee10b55fdd19f6426e05b3267e767670718` | [docs/registry/implementation-map.yaml:L7827-L7836] |
| 283 | external | `f8facd383a45910c92d99f37863f57c9a9508a6de1d50b63da18bbdafca9171d` | [docs/registry/implementation-map.yaml:L7837-L7846] |
| 284 | external | `e5a8dc8537278e5b88ba133cd3255028bc39ad9478cde1697bf429141421d2f2` | [docs/registry/implementation-map.yaml:L7847-L7856] |
| 285 | external | `e5a8dc8537278e5b88ba133cd3255028bc39ad9478cde1697bf429141421d2f2` | [docs/registry/implementation-map.yaml:L7857-L7866] |
| 286 | external | `e5a8dc8537278e5b88ba133cd3255028bc39ad9478cde1697bf429141421d2f2` | [docs/registry/implementation-map.yaml:L7867-L7878] |
| 287 | external | `e5a8dc8537278e5b88ba133cd3255028bc39ad9478cde1697bf429141421d2f2` | [docs/registry/implementation-map.yaml:L7879-L7888] |
| 288 | external | `e5a8dc8537278e5b88ba133cd3255028bc39ad9478cde1697bf429141421d2f2` | [docs/registry/implementation-map.yaml:L7889-L7898] |
| 289 | external | `a424fb7870b27134656253d0074f90743bfa7be56b8722a055a8128baa1364d0` | [docs/registry/implementation-map.yaml:L7899-L7908] |
| 290 | external | `d4132e42920d9bf4c5dbda9a363ea95516111c13752b16a2065e340c138dacd2` | [docs/registry/implementation-map.yaml:L7909-L7921] |
| 291 | external | `d4132e42920d9bf4c5dbda9a363ea95516111c13752b16a2065e340c138dacd2` | [docs/registry/implementation-map.yaml:L7922-L7931] |
| 292 | external | `d4132e42920d9bf4c5dbda9a363ea95516111c13752b16a2065e340c138dacd2` | [docs/registry/implementation-map.yaml:L7932-L7941] |
| 293 | external | `d4132e42920d9bf4c5dbda9a363ea95516111c13752b16a2065e340c138dacd2` | [docs/registry/implementation-map.yaml:L7942-L7951] |
| 294 | external | `d4132e42920d9bf4c5dbda9a363ea95516111c13752b16a2065e340c138dacd2` | [docs/registry/implementation-map.yaml:L7952-L7961] |
| 295 | external | `d4132e42920d9bf4c5dbda9a363ea95516111c13752b16a2065e340c138dacd2` | [docs/registry/implementation-map.yaml:L7962-L7971] |
| 296 | external | `d4132e42920d9bf4c5dbda9a363ea95516111c13752b16a2065e340c138dacd2` | [docs/registry/implementation-map.yaml:L7972-L7981] |
| 297 | external | `7ee449011ee9ab6c88590d9a217be7a6a66c03a6359e419862c372a516db5338` | [docs/registry/implementation-map.yaml:L7982-L7991] |
| 298 | external | `db0dbb44afa4418353fc730a670655e9cdaa5a9d5a7789891d893ec2dcf16055` | [docs/registry/implementation-map.yaml:L7992-L8001] |
| 299 | external | `c3c72b449cf3ff0495f4a96839c55a52091147ab74680b25116bf403eac522ee` | [docs/registry/implementation-map.yaml:L8002-L8011] |
| 300 | external | `6f9a80743d74183cb5816ce25e4494cf857299fdaa3dc117e957a7e3918cace1` | [docs/registry/implementation-map.yaml:L8012-L8021] |
| 301 | external | `5bdefebcb07b01c91b10faa5c9856dff9fcec2606813289274c927b5554523a4` | [docs/registry/implementation-map.yaml:L8022-L8031] |
| 302 | external | `8371ba396a2bea75d555db008e08262b0902912f1694c72e40d7e2fc404a9cf2` | [docs/registry/implementation-map.yaml:L8032-L8043] |
| 303 | external | `8371ba396a2bea75d555db008e08262b0902912f1694c72e40d7e2fc404a9cf2` | [docs/registry/implementation-map.yaml:L8044-L8053] |
| 304 | external | `129aa40a7d3c3b58ed62876f1e56cde9e30445531e066e18123e43658c15aaf0` | [docs/registry/implementation-map.yaml:L8054-L8063] |
| 305 | external | `129aa40a7d3c3b58ed62876f1e56cde9e30445531e066e18123e43658c15aaf0` | [docs/registry/implementation-map.yaml:L8064-L8073] |
| 306 | external | `129aa40a7d3c3b58ed62876f1e56cde9e30445531e066e18123e43658c15aaf0` | [docs/registry/implementation-map.yaml:L8074-L8083] |
| 307 | external | `33106dbcdeb98112901d32788c2be94eae6f4db9d436b2a6edd8af6292b33b74` | [docs/registry/implementation-map.yaml:L8084-L8093] |
| 308 | external | `33106dbcdeb98112901d32788c2be94eae6f4db9d436b2a6edd8af6292b33b74` | [docs/registry/implementation-map.yaml:L8094-L8103] |
| 309 | external | `33106dbcdeb98112901d32788c2be94eae6f4db9d436b2a6edd8af6292b33b74` | [docs/registry/implementation-map.yaml:L8104-L8113] |
| 310 | external | `b204a635623d0296cfb57cc575f128d28bf7bcecc35289ded4dbb4ef683bb9d8` | [docs/registry/implementation-map.yaml:L8114-L8123] |
| 311 | external | `888d40b1de24010459af9b42d98ac85744e7f43c3bb9954e0115fccaae6ca121` | [docs/registry/implementation-map.yaml:L8124-L8133] |
| 312 | external | `e3d0406af02a2a5b3d423a1fea7bc48c30102cbf753b140cd4670a376c53da97` | [docs/registry/implementation-map.yaml:L8134-L8143] |
| 313 | external | `e3d0406af02a2a5b3d423a1fea7bc48c30102cbf753b140cd4670a376c53da97` | [docs/registry/implementation-map.yaml:L8144-L8155] |
| 314 | external | `b0e676a83e9237e5d0f359a8c37fc58b1d3331b54f0eaeb1a0e6e2b7d0176210` | [docs/registry/implementation-map.yaml:L8156-L8165] |
| 315 | external | `b0e676a83e9237e5d0f359a8c37fc58b1d3331b54f0eaeb1a0e6e2b7d0176210` | [docs/registry/implementation-map.yaml:L8166-L8175] |
| 316 | external | `459e6434e28459cfdd6f7eea9c2f67695d846fa67120cd97a7e6ef062fee34a8` | [docs/registry/implementation-map.yaml:L8176-L8185] |
| 317 | external | `459e6434e28459cfdd6f7eea9c2f67695d846fa67120cd97a7e6ef062fee34a8` | [docs/registry/implementation-map.yaml:L8186-L8195] |
| 318 | external | `62b46c533f6541861f370a166521f659c9f112e719ea17af873bd895e46cfbee` | [docs/registry/implementation-map.yaml:L8196-L8205] |
| 319 | external | `62b46c533f6541861f370a166521f659c9f112e719ea17af873bd895e46cfbee` | [docs/registry/implementation-map.yaml:L8206-L8215] |
| 320 | external | `538249017dc6ddde0a67debe275dda2b01a768c0cc3134757c30d737ca398812` | [docs/registry/implementation-map.yaml:L8216-L8227] |
| 321 | external | `538249017dc6ddde0a67debe275dda2b01a768c0cc3134757c30d737ca398812` | [docs/registry/implementation-map.yaml:L8228-L8237] |
| 322 | external | `6c321f6f00ff56f0b436bcdfaf3a55739c28586b0f9a1b17bcec037cfdb16b28` | [docs/registry/implementation-map.yaml:L8238-L8247] |
| 323 | external | `106c5e8d24b33b2455447bc9ee1bc6b909f0064cf55b94b33fef29ab315d2793` | [docs/registry/implementation-map.yaml:L8248-L8257] |
| 324 | external | `074c57c7608db8162536a5dadb519c3cf9faf3308e25121474b78623006a0856` | [docs/registry/implementation-map.yaml:L8258-L8267] |
| 325 | external | `62e4cee9f25bb1b629787b1048d6c064062feb84c31a5283076c6b26441aaa12` | [docs/registry/implementation-map.yaml:L8268-L8279] |
| 326 | external | `41dbb41af843a53ece0b711164adf67a7790f189f88de49f4dc056616080de41` | [docs/registry/implementation-map.yaml:L8280-L8290] |
| 327 | external | `41dbb41af843a53ece0b711164adf67a7790f189f88de49f4dc056616080de41` | [docs/registry/implementation-map.yaml:L8291-L8300] |
| 328 | external | `41dbb41af843a53ece0b711164adf67a7790f189f88de49f4dc056616080de41` | [docs/registry/implementation-map.yaml:L8301-L8310] |
| 329 | external | `41dbb41af843a53ece0b711164adf67a7790f189f88de49f4dc056616080de41` | [docs/registry/implementation-map.yaml:L8311-L8323] |
| 330 | external | `e19d85ed3bd7241e6c4bed3c549371e2cfebf36672c2fd351570889563e0e8c8` | [docs/registry/implementation-map.yaml:L8324-L8333] |
| 331 | external | `e19d85ed3bd7241e6c4bed3c549371e2cfebf36672c2fd351570889563e0e8c8` | [docs/registry/implementation-map.yaml:L8334-L8343] |
| 332 | external | `2d81029862cdcfbb480238ab4a5ad07e22d4fc3f1558a176df77d6a0c72634bc` | [docs/registry/implementation-map.yaml:L8344-L8353] |
| 333 | external | `765aabac5719a0c9e0ffc05f04103c1bd70e7e2755183f674d1f99f25c2fad58` | [docs/registry/implementation-map.yaml:L8354-L8363] |
| 334 | external | `c0e80dbb8816f96cd155ae7b093d9fbdaaf227347b98df4692b145a170ea5187` | [docs/registry/implementation-map.yaml:L8364-L8373] |
| 335 | external | `c71ad075b93112842c7218eadc3a6db64ad041a0b57a11902f36194f6f2b11ed` | [docs/registry/implementation-map.yaml:L8374-L8383] |
| 336 | external | `daa62bce773b294df8652647d8e6621b082cb71ba6ec15b5334e8ed194772f9f` | [docs/registry/implementation-map.yaml:L8384-L8393] |
| 337 | external | `ae53eee6c095f153455c02a321785ad81f5996089a8b56944872de01e5727961` | [docs/registry/implementation-map.yaml:L8394-L8403] |
| 338 | external | `95f8686a1817523d28ce407018f872de04e5f3e51d13050ddf99e97f560141cd` | [docs/registry/implementation-map.yaml:L8404-L8413] |
| 339 | external | `95f8686a1817523d28ce407018f872de04e5f3e51d13050ddf99e97f560141cd` | [docs/registry/implementation-map.yaml:L8414-L8423] |
| 340 | external | `95f8686a1817523d28ce407018f872de04e5f3e51d13050ddf99e97f560141cd` | [docs/registry/implementation-map.yaml:L8424-L8435] |
| 341 | external | `37b85de7f0bc7c96d50d463f9e959a5de0911f7a54f75a4a27ff6198db3d330c` | [docs/registry/implementation-map.yaml:L8436-L8445] |
| 342 | external | `457380f881bf7fc1b578c940a057e6ade961baae452dca620469216082a6ccd7` | [docs/registry/implementation-map.yaml:L8446-L8457] |
| 343 | external | `457380f881bf7fc1b578c940a057e6ade961baae452dca620469216082a6ccd7` | [docs/registry/implementation-map.yaml:L8458-L8470] |
| 344 | external | `e5da79b3959a027605fa418f0ae5c4a2d18ebada20c50327bdbe9d6f8ef3ac8d` | [docs/registry/implementation-map.yaml:L8471-L8480] |
| 345 | external | `e5da79b3959a027605fa418f0ae5c4a2d18ebada20c50327bdbe9d6f8ef3ac8d` | [docs/registry/implementation-map.yaml:L8481-L8492] |
| 346 | external | `fc6a9f15290d35f8cfc734c72d06df862494602b4ae93028e2c546d35097ab00` | [docs/registry/implementation-map.yaml:L8493-L8505] |
| 347 | external | `fc6a9f15290d35f8cfc734c72d06df862494602b4ae93028e2c546d35097ab00` | [docs/registry/implementation-map.yaml:L8506-L8517] |
| 348 | external | `fc6a9f15290d35f8cfc734c72d06df862494602b4ae93028e2c546d35097ab00` | [docs/registry/implementation-map.yaml:L8518-L8527] |
| 349 | external | `ff47310ed1a3615c1b3c471d4ce9d95281ca0ba1f5130913814de5b821d0f717` | [docs/registry/implementation-map.yaml:L8528-L8537] |
| 350 | external | `ff47310ed1a3615c1b3c471d4ce9d95281ca0ba1f5130913814de5b821d0f717` | [docs/registry/implementation-map.yaml:L8538-L8547] |
| 351 | external | `ff47310ed1a3615c1b3c471d4ce9d95281ca0ba1f5130913814de5b821d0f717` | [docs/registry/implementation-map.yaml:L8548-L8560] |
| 352 | external | `ff47310ed1a3615c1b3c471d4ce9d95281ca0ba1f5130913814de5b821d0f717` | [docs/registry/implementation-map.yaml:L8561-L8572] |
| 353 | external | `ff47310ed1a3615c1b3c471d4ce9d95281ca0ba1f5130913814de5b821d0f717` | [docs/registry/implementation-map.yaml:L8573-L8582] |
| 354 | external | `ff47310ed1a3615c1b3c471d4ce9d95281ca0ba1f5130913814de5b821d0f717` | [docs/registry/implementation-map.yaml:L8583-L8596] |
| 355 | external | `ff47310ed1a3615c1b3c471d4ce9d95281ca0ba1f5130913814de5b821d0f717` | [docs/registry/implementation-map.yaml:L8597-L8608] |
| 356 | external | `ff47310ed1a3615c1b3c471d4ce9d95281ca0ba1f5130913814de5b821d0f717` | [docs/registry/implementation-map.yaml:L8609-L8619] |
| 357 | external | `ff47310ed1a3615c1b3c471d4ce9d95281ca0ba1f5130913814de5b821d0f717` | [docs/registry/implementation-map.yaml:L8620-L8632] |
| 358 | external | `ff47310ed1a3615c1b3c471d4ce9d95281ca0ba1f5130913814de5b821d0f717` | [docs/registry/implementation-map.yaml:L8633-L8647] |
| 359 | external | `ff47310ed1a3615c1b3c471d4ce9d95281ca0ba1f5130913814de5b821d0f717` | [docs/registry/implementation-map.yaml:L8648-L8657] |
| 360 | external | `21b42f5bcba169fb721854a56ea7cccee2954501226b9068ad26c59ca9c8ca33` | [docs/registry/implementation-map.yaml:L8658-L8667] |
| 361 | external | `21b42f5bcba169fb721854a56ea7cccee2954501226b9068ad26c59ca9c8ca33` | [docs/registry/implementation-map.yaml:L8668-L8677] |
| 362 | external | `21b42f5bcba169fb721854a56ea7cccee2954501226b9068ad26c59ca9c8ca33` | [docs/registry/implementation-map.yaml:L8678-L8687] |
| 363 | external | `21b42f5bcba169fb721854a56ea7cccee2954501226b9068ad26c59ca9c8ca33` | [docs/registry/implementation-map.yaml:L8688-L8701] |
| 364 | external | `21b42f5bcba169fb721854a56ea7cccee2954501226b9068ad26c59ca9c8ca33` | [docs/registry/implementation-map.yaml:L8702-L8714] |
| 365 | external | `21b42f5bcba169fb721854a56ea7cccee2954501226b9068ad26c59ca9c8ca33` | [docs/registry/implementation-map.yaml:L8715-L8724] |
| 366 | external | `21b42f5bcba169fb721854a56ea7cccee2954501226b9068ad26c59ca9c8ca33` | [docs/registry/implementation-map.yaml:L8725-L8739] |
| 367 | external | `f906f9812bd7d6b7ba8ba3cd16bba9855f709feec53f77efa8646ef0b663602d` | [docs/registry/implementation-map.yaml:L8740-L8749] |
| 368 | external | `f906f9812bd7d6b7ba8ba3cd16bba9855f709feec53f77efa8646ef0b663602d` | [docs/registry/implementation-map.yaml:L8750-L8759] |
| 369 | external | `f906f9812bd7d6b7ba8ba3cd16bba9855f709feec53f77efa8646ef0b663602d` | [docs/registry/implementation-map.yaml:L8760-L8773] |
| 370 | external | `f8168da63d3af62f45f7e8e65e4fed2d151910600ef81751052d99de0a5283ea` | [docs/registry/implementation-map.yaml:L8774-L8783] |
| 371 | external | `b55829f21be631d8e956b8f5f3cdab02f41b8499737b40f309637247b0bce9a3` | [docs/registry/implementation-map.yaml:L8784-L8793] |
| 372 | external | `374ec32dfcd700baadf83ffe23d49549c3699436d3a62b1a449c1a3016980644` | [docs/registry/implementation-map.yaml:L8794-L8803] |
| 373 | external | `09456ba60a898dbc464ca6ac7390a94f48b64b7e81d31647bc315503b0209ff4` | [docs/registry/implementation-map.yaml:L8804-L8813] |
| 374 | external | `8e92e43a560c821c80555fb693fa296d9af54fd44f7f9ada032a9e27b0232ffc` | [docs/registry/implementation-map.yaml:L8814-L8823] |
| 375 | external | `021c661e40f19277338cdf80883f539d25fd9f5265db529006ee0d0e2b43d0b0` | [docs/registry/implementation-map.yaml:L8824-L8833] |
| 376 | external | `021c661e40f19277338cdf80883f539d25fd9f5265db529006ee0d0e2b43d0b0` | [docs/registry/implementation-map.yaml:L8834-L8843] |
| 377 | external | `021c661e40f19277338cdf80883f539d25fd9f5265db529006ee0d0e2b43d0b0` | [docs/registry/implementation-map.yaml:L8844-L8856] |
| 378 | external | `93044174cc0a9c797494d16ecccce116cf2adea24448862c02891730f5b2ca3a` | [docs/registry/implementation-map.yaml:L8857-L8866] |
| 379 | external | `e1e194ab9d4cfae1252332fff1377e0e84a792e6acf1bf9ea66868123b58062c` | [docs/registry/implementation-map.yaml:L8867-L8876] |
| 380 | external | `478d442856c41bcca77713a0b00dd7052c29eaeff0f39b4c9e3dc79b942ab68d` | [docs/registry/implementation-map.yaml:L8877-L8886] |
| 381 | external | `f832aaed294c68e0f1d1db5d457e282772fc5b09d849f439b85670be6087e0a3` | [docs/registry/implementation-map.yaml:L8887-L8896] |
| 382 | external | `8f63f2b8696ea7941d889189059f34e8a5df555fe30509ed299936aa13a9294d` | [docs/registry/implementation-map.yaml:L8897-L8908] |
| 383 | external | `8f63f2b8696ea7941d889189059f34e8a5df555fe30509ed299936aa13a9294d` | [docs/registry/implementation-map.yaml:L8909-L8918] |
| 384 | external | `88d194b329b41f64c2df4ab49dedc4ac42ea389df71d1b28419554bd864fe324` | [docs/registry/implementation-map.yaml:L8919-L8928] |
| 385 | external | `a518ea46f6c810abf2f5a3b43c2d589f9214b14e0a9d60046f757aa68b3f2e4e` | [docs/registry/implementation-map.yaml:L8929-L8938] |
| 386 | external | `caaa3e8da45b7d13d8b1f8a176a9a383444e9125a5fb0d1adef152b6a3ac00d6` | [docs/registry/implementation-map.yaml:L8939-L8948] |
| 387 | external | `f541e81e416a5e7376654cd48c1135ce160bc94624b819e2f1c053aec266de9e` | [docs/registry/implementation-map.yaml:L8949-L8958] |
| 388 | external | `bcf7278232193096a7334f80ac71e970470b844504101c3460d69ad603a08cda` | [docs/registry/implementation-map.yaml:L8959-L8970] |
| 389 | external | `0df86ea8b72e98091907e760fc5735f53524799f9c4044fe09cf45ad93cf50a5` | [docs/registry/implementation-map.yaml:L8971-L8980] |
| 390 | external | `34f122497df7f579b097cb677939ec9a0e3fd56d5c14d0b836168733130f6937` | [docs/registry/implementation-map.yaml:L8981-L8990] |
| 391 | external | `34f122497df7f579b097cb677939ec9a0e3fd56d5c14d0b836168733130f6937` | [docs/registry/implementation-map.yaml:L8991-L9000] |
| 392 | external | `b4a2586689a0b37ca0e0324077487f6f539eb0f951bbac1b9300e31521bb0e42` | [docs/registry/implementation-map.yaml:L9001-L9010] |
| 393 | external | `b4a2586689a0b37ca0e0324077487f6f539eb0f951bbac1b9300e31521bb0e42` | [docs/registry/implementation-map.yaml:L9011-L9020] |
| 394 | external | `ee52b4b1ed5bc5833711f9d22e0769aaa9dceec6af1a31ea27eb345240c83f90` | [docs/registry/implementation-map.yaml:L9021-L9030] |
| 395 | external | `ee52b4b1ed5bc5833711f9d22e0769aaa9dceec6af1a31ea27eb345240c83f90` | [docs/registry/implementation-map.yaml:L9031-L9040] |
| 396 | external | `ee52b4b1ed5bc5833711f9d22e0769aaa9dceec6af1a31ea27eb345240c83f90` | [docs/registry/implementation-map.yaml:L9041-L9050] |
| 397 | external | `93e08a42efae81caf110022488f97ae047301d989709b697f4b293fecb7f31a1` | [docs/registry/implementation-map.yaml:L9051-L9060] |
| 398 | external | `af8948a652faf9e8b0e2608bef7f6837584ecee779b61320d4065b8df36766e9` | [docs/registry/implementation-map.yaml:L9061-L9070] |
| 399 | external | `176779988dfb0f78a7c26d1c427b463a5808fdd78c1eec59e8443fed0a4ec44e` | [docs/registry/implementation-map.yaml:L9071-L9080] |
| 400 | external | `3d51d2029a96fcb291ac4a1f9392b4f87bd4f224b2812b26b25c272b69814b81` | [docs/registry/implementation-map.yaml:L9081-L9090] |
| 401 | external | `8977107592f2d91bad636fdc94b3b88c05ffa5d6d30eb3b5d5f183914249c76c` | [docs/registry/implementation-map.yaml:L9091-L9100] |
| 402 | external | `8977107592f2d91bad636fdc94b3b88c05ffa5d6d30eb3b5d5f183914249c76c` | [docs/registry/implementation-map.yaml:L9101-L9110] |
| 403 | external | `fee15a1228b84c5f6b71cee05d005a19ace05bf0e5d5ff10693c3eeff5bea0a8` | [docs/registry/implementation-map.yaml:L9111-L9120] |
| 404 | external | `4aaeefe433acfc148ceeb71d483dd4c4fe5b27339f6d7861256172d841ca65f4` | [docs/registry/implementation-map.yaml:L9121-L9130] |
| 405 | external | `b56dd86127a8b80bb0883ddb0bfcd6551a89beb1fec27186093cdc455bb6fb90` | [docs/registry/implementation-map.yaml:L9131-L9140] |
| 406 | external | `b56dd86127a8b80bb0883ddb0bfcd6551a89beb1fec27186093cdc455bb6fb90` | [docs/registry/implementation-map.yaml:L9141-L9152] |
| 407 | external | `33efeb84239b702670e8a6eea9aca068aeabb22023d8a1bc09a86467a3f10729` | [docs/registry/implementation-map.yaml:L9153-L9162] |
| 408 | external | `33efeb84239b702670e8a6eea9aca068aeabb22023d8a1bc09a86467a3f10729` | [docs/registry/implementation-map.yaml:L9163-L9172] |
| 409 | external | `d0ad0adcb01ad7aeb0e766c03a556ec8879bf743faffde8785d460fe777a5768` | [docs/registry/implementation-map.yaml:L9173-L9182] |
| 410 | external | `1a0355cdff10d1991579cc16b3c8f4ceac8256eed1f925a02c0cdd9c7c96e5ad` | [docs/registry/implementation-map.yaml:L9183-L9192] |
| 411 | external | `66f0dd6d7fc31e9abf6ae51bf88bafc5af5dd5c198af87c46e7306b517059411` | [docs/registry/implementation-map.yaml:L9193-L9202] |
| 412 | external | `741040af2ee654e70babcf515dbd5397402d60d1a30ad7a21008b23ab09aec6a` | [docs/registry/implementation-map.yaml:L9203-L9212] |
| 413 | external | `aa3aa9f5f38e1d0747c5484d15344e06854dea60775c25d0147039092f72f5ea` | [docs/registry/implementation-map.yaml:L9213-L9222] |
| 414 | external | `aa3aa9f5f38e1d0747c5484d15344e06854dea60775c25d0147039092f72f5ea` | [docs/registry/implementation-map.yaml:L9223-L9232] |
| 415 | external | `cbfca9a8e6b60b82275aca3771fba500065db794c1fa6a78d59a548a4fd444d1` | [docs/registry/implementation-map.yaml:L9233-L9242] |
| 416 | external | `67d3ab96fa8fba1c4302f54cfacb2eced014eed5ee2f261866efd301e5053ca7` | [docs/registry/implementation-map.yaml:L9243-L9252] |
| 417 | external | `fe661ac70cded629d394d700d7c0e50c64493b32c652ba517b196f5c6ca20d7e` | [docs/registry/implementation-map.yaml:L9253-L9262] |
| 418 | external | `efed3c4902ab5166f78fd284fcf4a443f2128e278b2709034f32bfc644253315` | [docs/registry/implementation-map.yaml:L9263-L9272] |
| 419 | external | `743d270155eb32de32f21b3d41ffd96ee7ce40ac318d74ad088768253cd85b25` | [docs/registry/implementation-map.yaml:L9273-L9282] |
| 420 | external | `baff34ca75b8621f4913dde7d90f875835703edc2f61e5415404554688a65dd1` | [docs/registry/implementation-map.yaml:L9283-L9292] |
| 421 | external | `baff34ca75b8621f4913dde7d90f875835703edc2f61e5415404554688a65dd1` | [docs/registry/implementation-map.yaml:L9293-L9302] |
| 422 | external | `50d08061617a64d413331da3a00289ea05c7d52d2366b7b318f8f9acd5595760` | [docs/registry/implementation-map.yaml:L9303-L9312] |
| 423 | external | `50d08061617a64d413331da3a00289ea05c7d52d2366b7b318f8f9acd5595760` | [docs/registry/implementation-map.yaml:L9313-L9322] |
| 424 | external | `50d08061617a64d413331da3a00289ea05c7d52d2366b7b318f8f9acd5595760` | [docs/registry/implementation-map.yaml:L9323-L9332] |
| 425 | external | `50d08061617a64d413331da3a00289ea05c7d52d2366b7b318f8f9acd5595760` | [docs/registry/implementation-map.yaml:L9333-L9342] |
| 426 | external | `50d08061617a64d413331da3a00289ea05c7d52d2366b7b318f8f9acd5595760` | [docs/registry/implementation-map.yaml:L9343-L9352] |
| 427 | external | `50d08061617a64d413331da3a00289ea05c7d52d2366b7b318f8f9acd5595760` | [docs/registry/implementation-map.yaml:L9353-L9362] |
| 428 | external | `ebcc9455cc8f2dcea0fa0a891234ee718ecae5ff1ef1aa60046d5f8b46a0827d` | [docs/registry/implementation-map.yaml:L9363-L9372] |
| 429 | external | `7d455d095a24fc8d39b07ea78ed3a6e5eb9f132429ace51dd92f559a8fc37f2b` | [docs/registry/implementation-map.yaml:L9373-L9382] |
| 430 | external | `b2a15a8d927970d479e47c3b97bc1c395bf08bc54943fa7038db017a02be1250` | [docs/registry/implementation-map.yaml:L9383-L9392] |
| 431 | external | `fb310a4480bec3c620dcfb38dc50d3ae231aceed5dd7c82cd983c4332d15ab18` | [docs/registry/implementation-map.yaml:L9393-L9402] |
| 432 | external | `b6c888a5b40403a43b76e633eceed0ca3cbc3570d134b49a4f8024fe1cdb26d6` | [docs/registry/implementation-map.yaml:L9403-L9412] |
| 433 | external | `b6c888a5b40403a43b76e633eceed0ca3cbc3570d134b49a4f8024fe1cdb26d6` | [docs/registry/implementation-map.yaml:L9413-L9422] |
| 434 | external | `b6c888a5b40403a43b76e633eceed0ca3cbc3570d134b49a4f8024fe1cdb26d6` | [docs/registry/implementation-map.yaml:L9423-L9432] |
| 435 | external | `b6c888a5b40403a43b76e633eceed0ca3cbc3570d134b49a4f8024fe1cdb26d6` | [docs/registry/implementation-map.yaml:L9433-L9442] |
| 436 | external | `b6c888a5b40403a43b76e633eceed0ca3cbc3570d134b49a4f8024fe1cdb26d6` | [docs/registry/implementation-map.yaml:L9443-L9452] |
| 437 | external | `c65bcb95f2ea163eec21921ab640620eb36e0e0fb1667966375c12f781c0abea` | [docs/registry/implementation-map.yaml:L9453-L9462] |
| 438 | external | `bf933533feb60ab1bb70c61ca54fdac26820406edfa6277ca72de64da1cbcf25` | [docs/registry/implementation-map.yaml:L9463-L9472] |
| 439 | external | `eef896565a9dd5304f6c013d9d220955de28b2825624e7f97f655dd370dc3bff` | [docs/registry/implementation-map.yaml:L9473-L9482] |
| 440 | external | `c6846c0b56ad502651c49d587d8c01ab03d820ed0d6cbc8fade9055c6c6baa29` | [docs/registry/implementation-map.yaml:L9483-L9493] |
#### irregular-frequency-treatment

In-package rows: 1; total inventory rows: 31. [docs/registry/implementation-map.yaml:L13256-L13258]

| Ordinal | Scope | Source file SHA-256 | Map record |
|---|---|---|---|
| 1 | external | `536e001cf8648ea997a7a221c302e1dfcfe1d018f86f3f3efd303f356af767a9` | [docs/registry/implementation-map.yaml:L13263-L13272] |
| 2 | external | `15fa466cb0adc4dd962aaf5dd12046dc74e58133aeac8a0d81ce8c667f495f08` | [docs/registry/implementation-map.yaml:L13273-L13282] |
| 3 | external | `f5748c561c9ccbcda16c931463a7bf3255894aadb950834781633d14fe53eeb8` | [docs/registry/implementation-map.yaml:L13283-L13292] |
| 4 | external | `f5748c561c9ccbcda16c931463a7bf3255894aadb950834781633d14fe53eeb8` | [docs/registry/implementation-map.yaml:L13293-L13302] |
| 5 | external | `131858b1697dab2e7e9f45e7fb385d42ddf06accd970695bd045149e311c4d8a` | [docs/registry/implementation-map.yaml:L13303-L13312] |
| 6 | external | `131858b1697dab2e7e9f45e7fb385d42ddf06accd970695bd045149e311c4d8a` | [docs/registry/implementation-map.yaml:L13313-L13322] |
| 7 | external | `131858b1697dab2e7e9f45e7fb385d42ddf06accd970695bd045149e311c4d8a` | [docs/registry/implementation-map.yaml:L13323-L13332] |
| 8 | package | `40b8a37903f8d559cd3661967a6c49f069fdd3a02ef242ec2efb6a2e91d14e3f` | [docs/registry/implementation-map.yaml:L13333-L13342] |
| 9 | external | `4275dd465421e16a6b0be99dfa13ec816b6ed5ef7e211edf3d1250bfcab400f0` | [docs/registry/implementation-map.yaml:L13343-L13352] |
| 10 | external | `09b106066b5f87f2035a8e9691d0e22aa7d3f4456abada6a65d0c9760c223227` | [docs/registry/implementation-map.yaml:L13353-L13362] |
| 11 | external | `ec7bcf7fa6c81ebceeeb26f4b3d9adb09cc0dc76799021ce1c68c3cd1b39afdc` | [docs/registry/implementation-map.yaml:L13363-L13372] |
| 12 | external | `ec7bcf7fa6c81ebceeeb26f4b3d9adb09cc0dc76799021ce1c68c3cd1b39afdc` | [docs/registry/implementation-map.yaml:L13373-L13382] |
| 13 | external | `c6c10dc97c393f5f92605b9d469d8a5cea77e1a18bd6c08383a60045540d2b2d` | [docs/registry/implementation-map.yaml:L13383-L13392] |
| 14 | external | `c6c10dc97c393f5f92605b9d469d8a5cea77e1a18bd6c08383a60045540d2b2d` | [docs/registry/implementation-map.yaml:L13393-L13402] |
| 15 | external | `c6c10dc97c393f5f92605b9d469d8a5cea77e1a18bd6c08383a60045540d2b2d` | [docs/registry/implementation-map.yaml:L13403-L13412] |
| 16 | external | `86bc16d26b65e069b6208f590b1e29b15efa7ceb6c8194f52ac1a251a012a724` | [docs/registry/implementation-map.yaml:L13413-L13422] |
| 17 | external | `86bc16d26b65e069b6208f590b1e29b15efa7ceb6c8194f52ac1a251a012a724` | [docs/registry/implementation-map.yaml:L13423-L13434] |
| 18 | external | `c380d3cde99b550cf4b5216c7f8628a7b682d1ea5c580e39840ac8ac8fb28879` | [docs/registry/implementation-map.yaml:L13435-L13444] |
| 19 | external | `c380d3cde99b550cf4b5216c7f8628a7b682d1ea5c580e39840ac8ac8fb28879` | [docs/registry/implementation-map.yaml:L13445-L13454] |
| 20 | external | `c380d3cde99b550cf4b5216c7f8628a7b682d1ea5c580e39840ac8ac8fb28879` | [docs/registry/implementation-map.yaml:L13455-L13467] |
| 21 | external | `b8ebfd8216187233cdcb6d3cbe4541afd2d89afc59504b0206f9c12c04ceb10c` | [docs/registry/implementation-map.yaml:L13468-L13477] |
| 22 | external | `b8ebfd8216187233cdcb6d3cbe4541afd2d89afc59504b0206f9c12c04ceb10c` | [docs/registry/implementation-map.yaml:L13478-L13487] |
| 23 | external | `a2b25f4cb940465146ad24fd2fb79961c06624666be59bd46d345301d2a227bc` | [docs/registry/implementation-map.yaml:L13488-L13497] |
| 24 | external | `a2b25f4cb940465146ad24fd2fb79961c06624666be59bd46d345301d2a227bc` | [docs/registry/implementation-map.yaml:L13498-L13507] |
| 25 | external | `dc4f7582039fde90c702aacf5e853dfea84a03b7a8ce2667395c9f65a2ef6ef2` | [docs/registry/implementation-map.yaml:L13508-L13517] |
| 26 | external | `dc4f7582039fde90c702aacf5e853dfea84a03b7a8ce2667395c9f65a2ef6ef2` | [docs/registry/implementation-map.yaml:L13518-L13527] |
| 27 | external | `d82489f96afef321e2fda9c06a4a04091b3e4bff047f3bf9beeeea7d6f5106d2` | [docs/registry/implementation-map.yaml:L13528-L13537] |
| 28 | external | `ae4597f317b28365750eccfbde1a6f02334184b1eb30bd16a565be5f0ab05a2d` | [docs/registry/implementation-map.yaml:L13538-L13547] |
| 29 | external | `38b3fd76a0c26279b17356d3c50de2d4563a35c45259df39a5a243dabeb7651e` | [docs/registry/implementation-map.yaml:L13548-L13557] |
| 30 | external | `96df39d1bd80c00970b8cc195ba6efcdbcfbe765b00b5ae79313f4ba9cc2ff67` | [docs/registry/implementation-map.yaml:L13558-L13567] |
| 31 | external | `0c4a50562ee319aba526db099fd7012cc896d21d6b40009eca7c6d5aaf554315` | [docs/registry/implementation-map.yaml:L13568-L13577] |

### Exact package grep and definition classification

Search uses each counted family's stored `search_signals[].pattern` unchanged with Python `re`, case-sensitive, on the pinned package's `.py` files. A signal matching `floating_wind` case-insensitively would be removed as a package-path vocabulary signal; neither counted family has such a signal. Hits are assigned to their innermost enclosing function/class, with module statements grouped as one definition. The stored patterns, including their escaping and broad `lid` vocabulary matches, are retained rather than corrected. [docs/registry/implementation-map.yaml:L1-L4]

#### Grep: orcaflex-text-model-writers

19 definitions hit.

| Definition | Classification / reason | Evidence |
|---|---|---|
| `__init__.py::<module>` | Module documentation/import/configuration-locator hit; no additional writer definition | [src/digitalmodel/floating_wind/__init__.py:L1-L151] |
| `collaboration.py::<module>` | Module documentation/import/configuration-locator hit; no additional writer definition | [src/digitalmodel/floating_wind/collaboration.py:L1-L140] |
| `economics.py::<module>` | Module documentation/import/configuration-locator hit; no additional writer definition | [src/digitalmodel/floating_wind/economics.py:L1-L305] |
| `economics_workflow.py::<module>` | Module documentation/import/configuration-locator hit; no additional writer definition | [src/digitalmodel/floating_wind/economics_workflow.py:L1-L135] |
| `economics_workflow.py::_write_summary` | Helper: serializes JSON summaries, not a native text-model algorithm | [src/digitalmodel/floating_wind/economics_workflow.py:L113-L119] |
| `floaters.py::<module>` | Module documentation/import/configuration-locator hit; no additional writer definition | [src/digitalmodel/floating_wind/floaters.py:L1-L606] |
| `qualification.py::<module>` | Module documentation/import/configuration-locator hit; no additional writer definition | [src/digitalmodel/floating_wind/qualification.py:L1-L184] |
| `report.py::<module>` | Module documentation/import/configuration-locator hit; no additional writer definition | [src/digitalmodel/floating_wind/report.py:L1-L505] |
| `report.py::ConceptProvenance` | Data holder / provenance, not a computation of the family result | [src/digitalmodel/floating_wind/report.py:L86-L121] |
| `scaling.py::<module>` | Module documentation/import/configuration-locator hit; no additional writer definition | [src/digitalmodel/floating_wind/scaling.py:L1-L208] |
| `screening.py::<module>` | Module documentation/import/configuration-locator hit; no additional writer definition | [src/digitalmodel/floating_wind/screening.py:L1-L346] |
| `solver_tier.py::<module>` | Module documentation/import/configuration-locator hit; no additional writer definition | [src/digitalmodel/floating_wind/solver_tier.py:L1-L390] |
| `solver_tier.py::SolverCase` | Data holder / provenance, not a computation of the family result | [src/digitalmodel/floating_wind/solver_tier.py:L68-L77] |
| `solver_tier.py::build_orcaflex_motion_config` | Own configuration builder already listed in section 3; no further unlisted implementation | [src/digitalmodel/floating_wind/solver_tier.py:L263-L286] |
| `solver_tier.py::prepare_variant` | Listed map definition; writes mesh/spec and delegates motion-config construction | [src/digitalmodel/floating_wind/solver_tier.py:L292-L325] |
| `solver_tier.py::fold_rao_results` | Helper: consumes supplied responses; no writer algorithm | [src/digitalmodel/floating_wind/solver_tier.py:L352-L390] |
| `standardization.py::<module>` | Module documentation/import/configuration-locator hit; no additional writer definition | [src/digitalmodel/floating_wind/standardization.py:L1-L205] |
| `workflow.py::<module>` | Module documentation/import/configuration-locator hit; no additional writer definition | [src/digitalmodel/floating_wind/workflow.py:L1-L133] |
| `workflow.py::_write_summary` | Helper: serializes JSON summaries, not a native text-model algorithm | [src/digitalmodel/floating_wind/workflow.py:L126-L133] |
#### Grep: irregular-frequency-treatment

29 definitions hit.

| Definition | Classification / reason | Evidence |
|---|---|---|
| `collaboration.py::<module>` | Documentation/import/style-string match, including incidental lid substrings; no treatment | [src/digitalmodel/floating_wind/collaboration.py:L1-L140] |
| `collaboration.py::from_mapping` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/collaboration.py:L103-L104] |
| `economics.py::<module>` | Documentation/import/style-string match, including incidental lid substrings; no treatment | [src/digitalmodel/floating_wind/economics.py:L1-L305] |
| `economics.py::from_mapping` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/economics.py:L195-L197] |
| `economics_report.py::<module>` | Documentation/import/style-string match, including incidental lid substrings; no treatment | [src/digitalmodel/floating_wind/economics_report.py:L1-L284] |
| `floaters.py::<module>` | Documentation/import/style-string match, including incidental lid substrings; no treatment | [src/digitalmodel/floating_wind/floaters.py:L1-L606] |
| `floaters.py::FloaterProperties` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/floaters.py:L137-L172] |
| `floaters.py::_assemble` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/floaters.py:L213-L309] |
| `floaters.py::SemiSubmersible` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/floaters.py:L315-L389] |
| `floaters.py::TLP` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/floaters.py:L440-L534] |
| `qualification.py::<module>` | Documentation/import/style-string match, including incidental lid substrings; no treatment | [src/digitalmodel/floating_wind/qualification.py:L1-L184] |
| `qualification.py::QualificationCriteria` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/qualification.py:L86-L130] |
| `qualification.py::from_mapping` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/qualification.py:L122-L123] |
| `qualification.py::ConceptMaturity` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/qualification.py:L133-L146] |
| `reliability.py::<module>` | Documentation/import/style-string match, including incidental lid substrings; no treatment | [src/digitalmodel/floating_wind/reliability.py:L1-L104] |
| `report.py::<module>` | Documentation/import/style-string match, including incidental lid substrings; no treatment | [src/digitalmodel/floating_wind/report.py:L1-L505] |
| `scaling.py::<module>` | Documentation/import/style-string match, including incidental lid substrings; no treatment | [src/digitalmodel/floating_wind/scaling.py:L1-L208] |
| `scaling.py::ComponentScaling` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/scaling.py:L94-L119] |
| `scaling.py::EconomiesOfScale` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/scaling.py:L122-L146] |
| `scaling.py::from_mapping` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/scaling.py:L138-L139] |
| `screening.py::VariantScreening` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/screening.py:L197-L210] |
| `solver_tier.py::build_orcawave_spec` | Listed map definition; sets treatment option only, no independent treatment algorithm | [src/digitalmodel/floating_wind/solver_tier.py:L190-L260] |
| `standardization.py::<module>` | Documentation/import/style-string match, including incidental lid substrings; no treatment | [src/digitalmodel/floating_wind/standardization.py:L1-L205] |
| `standardization.py::LearningCurve` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/standardization.py:L98-L128] |
| `standardization.py::StandardizationDiscount` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/standardization.py:L131-L148] |
| `standardization.py::default_learning_curve` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/standardization.py:L198-L205] |
| `sweep.py::<module>` | Documentation/import/style-string match, including incidental lid substrings; no treatment | [src/digitalmodel/floating_wind/sweep.py:L1-L160] |
| `sweep.py::LeverChange` | Unrelated model/validation/loader definition or comment match; no irregular-frequency treatment | [src/digitalmodel/floating_wind/sweep.py:L61-L79] |
| `tradespace_economics.py::<module>` | Documentation/import/style-string match, including incidental lid substrings; no treatment | [src/digitalmodel/floating_wind/tradespace_economics.py:L1-L144] |

Further unlisted second implementations after the section-3 inventory: 0 across the two counted families; the A8 threshold is at most 1. This result concerns map completeness, not engineering qualification or the correctness of the map's family assignments.

Whole-repository supplementary search, including scripts, was also executed at the code pin:

```text
git grep -n -E 'def (compute_lcoe|trl_to_maturity|_heave_period|_pitch_period|_dominates)|LCOE|KB.*BM.*KG|k_heave|def.*pearson' <code-pin> -- '*.py'
```

The supplementary search returned 159 lines, including 7 under scripts; these are discovery matches rather than classified additional implementations. They do not change the counted-family result. Map follow-up, issue pending (4): hydrostatic sizing, rigid-body periods, screening criteria, Pareto/correlation, discounted LCOE, reliability, scaling, learning and qualification lack in-package map rows and will count only after such rows are added. [src/digitalmodel/floating_wind/floaters.py:L178-L583] [src/digitalmodel/floating_wind/economics.py:L238-L295] [src/digitalmodel/floating_wind/qualification.py:L157-L184]

The map labels `prepare_variant` as a text-model writer and `build_orcawave_spec` as irregular-frequency treatment; their bodies serialize a diffraction specification or set an option. Reclassification and external repository discoveries belong to the same map follow-up. [src/digitalmodel/floating_wind/solver_tier.py:L241-L246] [src/digitalmodel/floating_wind/solver_tier.py:L292-L325]

## 10. Coverage and open items

### Reading inventory

All 17 package Python files and all 15 module test files were inspected at the code pin; the package contained no README, pre-existing docs folder or nested Python subpackage. The source and test inventories below enumerate the retained reading scope.

| Source read | Evidence span |
|---|---|
| `src/digitalmodel/floating_wind/__init__.py` | [src/digitalmodel/floating_wind/__init__.py:L1-L151] |
| `src/digitalmodel/floating_wind/collaboration.py` | [src/digitalmodel/floating_wind/collaboration.py:L1-L140] |
| `src/digitalmodel/floating_wind/economics.py` | [src/digitalmodel/floating_wind/economics.py:L1-L305] |
| `src/digitalmodel/floating_wind/economics_report.py` | [src/digitalmodel/floating_wind/economics_report.py:L1-L284] |
| `src/digitalmodel/floating_wind/economics_workflow.py` | [src/digitalmodel/floating_wind/economics_workflow.py:L1-L135] |
| `src/digitalmodel/floating_wind/floaters.py` | [src/digitalmodel/floating_wind/floaters.py:L1-L606] |
| `src/digitalmodel/floating_wind/qualification.py` | [src/digitalmodel/floating_wind/qualification.py:L1-L184] |
| `src/digitalmodel/floating_wind/reliability.py` | [src/digitalmodel/floating_wind/reliability.py:L1-L104] |
| `src/digitalmodel/floating_wind/report.py` | [src/digitalmodel/floating_wind/report.py:L1-L505] |
| `src/digitalmodel/floating_wind/scaling.py` | [src/digitalmodel/floating_wind/scaling.py:L1-L208] |
| `src/digitalmodel/floating_wind/screening.py` | [src/digitalmodel/floating_wind/screening.py:L1-L346] |
| `src/digitalmodel/floating_wind/solver_tier.py` | [src/digitalmodel/floating_wind/solver_tier.py:L1-L390] |
| `src/digitalmodel/floating_wind/standardization.py` | [src/digitalmodel/floating_wind/standardization.py:L1-L205] |
| `src/digitalmodel/floating_wind/sweep.py` | [src/digitalmodel/floating_wind/sweep.py:L1-L160] |
| `src/digitalmodel/floating_wind/tradespace.py` | [src/digitalmodel/floating_wind/tradespace.py:L1-L231] |
| `src/digitalmodel/floating_wind/tradespace_economics.py` | [src/digitalmodel/floating_wind/tradespace_economics.py:L1-L144] |
| `src/digitalmodel/floating_wind/workflow.py` | [src/digitalmodel/floating_wind/workflow.py:L1-L133] |

| Test file read | Evidence span |
|---|---|
| `tests/floating_wind/test_collaboration.py` | [tests/floating_wind/test_collaboration.py:L1-L106] |
| `tests/floating_wind/test_economics.py` | [tests/floating_wind/test_economics.py:L1-L129] |
| `tests/floating_wind/test_economics_workflow.py` | [tests/floating_wind/test_economics_workflow.py:L1-L123] |
| `tests/floating_wind/test_floaters.py` | [tests/floating_wind/test_floaters.py:L1-L141] |
| `tests/floating_wind/test_qualification.py` | [tests/floating_wind/test_qualification.py:L1-L116] |
| `tests/floating_wind/test_reliability.py` | [tests/floating_wind/test_reliability.py:L1-L81] |
| `tests/floating_wind/test_report.py` | [tests/floating_wind/test_report.py:L1-L164] |
| `tests/floating_wind/test_scaling.py` | [tests/floating_wind/test_scaling.py:L1-L151] |
| `tests/floating_wind/test_screening.py` | [tests/floating_wind/test_screening.py:L1-L152] |
| `tests/floating_wind/test_solver_tier.py` | [tests/floating_wind/test_solver_tier.py:L1-L176] |
| `tests/floating_wind/test_standardization.py` | [tests/floating_wind/test_standardization.py:L1-L120] |
| `tests/floating_wind/test_sweep.py` | [tests/floating_wind/test_sweep.py:L1-L123] |
| `tests/floating_wind/test_tradespace.py` | [tests/floating_wind/test_tradespace.py:L1-L111] |
| `tests/floating_wind/test_tradespace_economics.py` | [tests/floating_wind/test_tradespace_economics.py:L1-L140] |
| `tests/floating_wind/test_workflow.py` | [tests/floating_wind/test_workflow.py:L1-L60] |

| Related documentation discovered | Reading boundary / evidence |
|---|---|
| `docs/maps/digitalmodel-operator-map.md` | Relevant sections inspected; [docs/maps/digitalmodel-operator-map.md:L48-L51] |
| `docs/benchmarks/catenary_closed_form/README.md` | Relevant sections inspected; [docs/benchmarks/catenary_closed_form/README.md:L129-L132] |
| `docs/registry/implementation-map.md` | Relevant sections inspected; [docs/registry/implementation-map.md:L19-L22] |
| `docs/capability-map/capabilities-ia-spec-1444.md` | Relevant sections inspected; [docs/capability-map/capabilities-ia-spec-1444.md:L51-L54] |
| `docs/domains/articles/ENG_FloatingWindPower.md` | Relevant sections inspected; [docs/domains/articles/ENG_FloatingWindPower.md:L1-L4] |
| `docs/research/mooring-industry-practice.md` | Matching sections only; remainder open under issue pending (6); [docs/research/mooring-industry-practice.md:L18-L21] |
| `docs/research/ffs-mooring-chain-degradation-screen.md` | Matching sections only; remainder open under issue pending (6); [docs/research/ffs-mooring-chain-degradation-screen.md:L77-L80] |
| `docs/research/hydrodynamics-industry-practice.md` | Matching sections only; remainder open under issue pending (6); [docs/research/hydrodynamics-industry-practice.md:L40-L43] |
| `docs/plans/2026-05-05-issue-574-fowt-wiki-standards-pages.md` | Relevant sections inspected; [docs/plans/2026-05-05-issue-574-fowt-wiki-standards-pages.md:L1-L4] |
| `docs/research/mooring-academic-references.md` | Matching sections only; remainder open under issue pending (6); [docs/research/mooring-academic-references.md:L28-L31] |
| `docs/domains/wind/wind_turbines.md` | Relevant sections inspected; [docs/domains/wind/wind_turbines.md:L11-L14] |
| `docs/domains/ship-design/foil.md` | Relevant sections inspected; [docs/domains/ship-design/foil.md:L3-L6] |
| `docs/domains/wind/wind.md` | Relevant sections inspected; [docs/domains/wind/wind.md:L15-L18] |
| `docs/domains/wind/moorings.md` | Relevant sections inspected; [docs/domains/wind/moorings.md:L1-L4] |
| `docs/domains/marine-engineering/marine_3d_models_catalog.md` | Matching sections only; remainder open under issue pending (6); [docs/domains/marine-engineering/marine_3d_models_catalog.md:L131-L134] |
| `docs/domains/mooring/moorings.md` | Relevant sections inspected; [docs/domains/mooring/moorings.md:L1-L1] |
| `docs/domains/hydrodynamics/2026-10-13-fw-loadcases.md` | Relevant sections inspected; [docs/domains/hydrodynamics/2026-10-13-fw-loadcases.md:L19-L22] |
| `docs/domains/project-docs/IMPLEMENTATION_PHASE4_SUMMARY.md` | Matching sections only; remainder open under issue pending (6); [docs/domains/project-docs/IMPLEMENTATION_PHASE4_SUMMARY.md:L16-L19] |
| `docs/domains/project-docs/USER_DOCUMENTATION_COMPLETE.md` | Matching sections only; remainder open under issue pending (6); [docs/domains/project-docs/USER_DOCUMENTATION_COMPLETE.md:L77-L80] |
| `docs/domains/project-docs/IMPLEMENTATION_PHASE3_SUMMARY.md` | Matching sections only; remainder open under issue pending (6); [docs/domains/project-docs/IMPLEMENTATION_PHASE3_SUMMARY.md:L259-L262] |
| `docs/domains/orcaflex/examples/ORCAFLEX_MODELS.md` | Matching sections only; remainder open under issue pending (6); [docs/domains/orcaflex/examples/ORCAFLEX_MODELS.md:L179-L182] |
| `docs/domains/orcaflex/wind/fowt-worked-example/external-files-inventory.md` | Relevant sections inspected; [docs/domains/orcaflex/wind/fowt-worked-example/external-files-inventory.md:L1-L4] |
| `docs/domains/orcaflex/wind/fowt-worked-example/load-case-table.md` | Relevant sections inspected; [docs/domains/orcaflex/wind/fowt-worked-example/load-case-table.md:L1-L4] |
| `docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md` | Relevant sections inspected; [docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md:L1-L4] |

The two workflow input files and six base configurations were read; their numerical calibration inputs are not newly established public benchmark data. [examples/workflows/floating-wind-sizing/input.yml:L1-L69] [examples/workflows/floating-wind-economics/input.yml:L1-L51] [src/digitalmodel/base_configs/modules/floating_wind_sizing/floating_wind_sizing.yml:L1-L8] [src/digitalmodel/base_configs/modules/floating_wind_qualification/floating_wind_qualification.yml:L1-L25] [src/digitalmodel/base_configs/modules/floating_wind_economics/floating_wind_economics.yml:L1-L42] [src/digitalmodel/base_configs/modules/floating_wind_economics/collaboration.yml:L1-L27] [src/digitalmodel/base_configs/modules/floating_wind_economics/economies_of_scale.yml:L1-L33] [src/digitalmodel/base_configs/modules/floating_wind_economics/standardization.yml:L1-L21]

The related standards note records DNV-RP-0286 edition 2019-05 amended 2021-10 and section titles, while the earlier plan names 2019-05 without amendment; this is incomplete alignment rather than a demonstrated conflicting edition. These are repository-recorded metadata, not source pages reopened for this pack. [docs/domains/hydrodynamics/2026-10-13-fw-loadcases.md:L47-L49] [docs/plans/2026-05-05-issue-574-fowt-wiki-standards-pages.md:L8-L15]

The proposed coupled worked-example worksheet does not supply calculated motions, periods or loads and is excluded from section 4. [docs/domains/orcaflex/wind/fowt-worked-example/load-case-table.md:L60-L87]

### Open-item register

Every item is assigned to the maintainer for issue creation/routing; the engineering calculations quality manager will decide engineering disposition. Existing issue numbers are references only; draft numbers require issue creation before independent acceptance of A7.

| Item | Missing evidence / required next check | Tracking |
|---|---|---|
| U1 | UNVERIFIED: solver horizontal frame, heading/phase convention, target version, native loadability, mesh orientation/length scale and RAO-to-DAF unit conversion; require recorded native readback and independent mesh/response checks | #2435 |
| U2 | UNVERIFIED: complete standard titles/editions/clauses, applicability, coefficient provenance and Citation sidecars; clause text not checked against the licensed copy | #574; issue pending (3) |
| U3 | UNVERIFIED: independently opened economic primary-source benchmarks and coefficient calibration evidence; current economic comparisons remain regression/calibration | issue pending (3) |
| U4 | Independent coupled/hydrodynamic worked example and result evidence are missing; analytical validation of three quantities does not qualify a complete concept | #2435; #2423 |
| U5 | Missing adversarial checks identified in section 6: nonfinite responses, transformed numeric domains, zero denominators, omitted inputs and empty/sentinel outputs | issue pending (1); issue pending (2); issue pending (5) |
| U6 | UNVERIFIED: deputy assignment and shared knowledge-log/index entry; partial documentary reading and absent package README also require disposition | issue pending (6) |
| U7 | Map follow-up: add missing calculation families, classify supplementary repository hits and correct option/serialization family assignments | issue pending (4) |
| U8 | Signed, fingerprint-linked report appendix and reliable metadata provenance are not established by rendering tests | #1019 |
| U9 | Baseline registry defect outside this package: duplicate `submodules` keys at pinned lines 305 and 313 can hide preceding routing entries; those blocks are unchanged by this pack. [docs/registry/module-routing.yaml:L305-L320] | issue pending (8) |

No client-derived dataset is adopted in this pack. No private-sidecar entry is named because no such entry identifier forms part of the calculation evidence reviewed here.
