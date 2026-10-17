# On-bottom stability knowledge pack

| Header | Value |
|---|---|
| Module | subsea/on_bottom_stability |
| Repository | digitalmodel |
| Package | src/digitalmodel/subsea/on_bottom_stability |
| Pack revision | 0; 2026-10-16 |
| Code pin | 7aad603be1c6136106dbb2381ce5096b050654f8 |
| Analysis family | Engineering calculations; EC |
| Quality review role | Engineering calculations quality manager |
| Status | DRAFT; independent review pending |

Evidence convention: all repository citations, including the implementation-map citations in section 9, resolve at the header code pin.
Numerical checks below specify this pack's checks; they do not establish compliance with a licensed standard.

## 1. Purpose and scope

The F109 surface computes hydrodynamic force, lift, submerged weight and absolute/generalized lateral utilisation; the package exposes both canonical and simplified APIs, with simplified signatures winning overlapping package exports. [src/digitalmodel/subsea/on_bottom_stability/__init__.py:L1-L22]
The simplified lateral surface delegates to generalized stability with passive resistance fixed to zero; the separate vertical check uses the weight-to-uplift criterion. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L291-L360] [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L398-L429]
The E305 surface builds layered weights, evaluates simultaneous regular-wave/current demand, integrates a diameter-averaged current profile and bisects a caller's concrete-thickness bracket. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L85-L108] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L194-L241] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L244-L257] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L309-L346]
The workflow requires two distinct case roles per segment and labels its result unqualified; edition-specific equivalence, the DNV-OS-F101 comparator (*Submarine pipeline systems*; edition not established) and private-workbook reproduction remain unestablished. [src/digitalmodel/subsea/on_bottom_stability/e305_workflow.py:L28-L35] [src/digitalmodel/subsea/on_bottom_stability/e305_workflow.py:L130-L152]
The existing routing registry marks the parent subsea domain stable; it does not separately assess this calculation package's maturity. [docs/registry/module-routing.yaml:L351-L363]
No lateral-displacement time history is computed by the listed force/ratio APIs or the E305 case result. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L18-L33] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L154-L180]

## 2. Inputs and outputs

### Inputs

| Surface / names | Units and convention | Range / default / source |
|---|---|---|
| Canonical `hydrodynamic_force_per_meter`: density, total coated diameter, velocity, acceleration, drag/inertia coefficients | kg/m³, m, m/s, m/s², dimensionless; velocity and acceleration signed along one scalar load direction | Caller values; coefficient defaults reference the code's smooth-pipe constants; no finite-value or physical-domain validation in the function. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L53-L87] |
| Canonical `lift_force_per_meter` | Density first, diameter second, velocity third; lift N/m | Caller coefficient or code default; squared velocity; no coefficient-domain validation. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L90-L116] |
| Canonical `submerged_weight_per_meter`: steel OD, wall, coating thickness and four densities | m and kg/m³; OD means steel OD here, although the load routine uses coated OD | All required; inner diameter is steel OD minus twice wall; gravity is fixed in code; no geometry validation. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L119-L184] |
| Canonical stability: `W_s_N_m`, `F_H_N_m`, `F_L_N_m`, `mu_soil`, `F_R_N_m`, `gamma_SC` | N/m; friction and factor dimensionless | Passive resistance required only by generalized check; safety-factor default references code constant; positive weight/capacity branches are explicit. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L187-L250] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L254-L317] |
| Simplified weight / loads / individual force wrappers | m, kg/m³, m/s, m/s²; force output N/m | Weight geometry/densities required; combined loads have a lift-coefficient default; individual drag/lift/inertia coefficients required. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L121-L211] [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L214-L288] |
| Simplified `check_lateral_stability`, `check_vertical_stability` | kN/m inputs; required lateral weight remains N/m | Positive submerged weight enforced; lateral wrapper converts each force by the N/kN scale; safety-factor default references code constant. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L363-L429] |
| E305 `Pipe` | All lengths m, densities kg/m³, extra structural mass kg/m, absorption and joint fractions dimensionless | All fields required; finite nonnegative fields, positive steel OD/wall/steel and water density, positive bore, corrosion allowance less than wall, joint fraction at most one. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L32-L65] |
| E305 `Case` | Velocities m/s, period s, phase rad; factors/coefficients dimensionless | Role and reduction convention required; velocities/coefficients finite nonnegative; period, R, Fw, friction and factors positive; duplicate-current default false, phase default None, phase-count default 720 with minimum 4. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L111-L151] |
| E305 gravity / sea state / reference current | Gravity m/s² explicit; Hs/depth/elevation/reference height/roughness m; Tp s; reference velocity m/s | Sea-state helper checks elevation below depth and exact helper gravity; a zero wave height has a distinct branch; current profile requires positive height/diameter/roughness. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L244-L277] |
| External table JSON | Roughness and grain-size axis m; Fw axes K and M dimensionless; remaining factor outputs dimensionless | Explicit file or `E305_TABLES_FILE`; exact four-table set, ordered axes, bounded input, no extrapolation; R/clay axis semantics supplied externally. [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L58-L110] [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L113-L121] [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L152-L175] |
| Cached workbook fixture | Schema-versioned SI pipe/case data and independent expected weights/ratios | External workbook, cell map and output; cached results required, duplicate JSON keys/nonfinite results rejected; source digests required. [src/digitalmodel/subsea/on_bottom_stability/e305_fixture.py:L23-L89] [src/digitalmodel/subsea/on_bottom_stability/e305_fixture.py:L99-L155] |

### Outputs

| Surface | Output / units / convention |
|---|---|
| Canonical functions | Scalar loads and submerged weight in N/m; positive weight means sinking; `StabilityResult(utilisation, is_stable, details)` carries dimensionless ratio and intermediate forces. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L18-L33] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L152-L184] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L218-L250] |
| Simplified functions | Weight and component-load dataclasses in N/m; `utilization` has `utilisation` alias; required weight defaults to zero only on direct construction of the result container. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L67-L118] [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L356-L360] |
| E305 weight | Component/total mass kg/m, dry/buoyancy/submerged forces N/m, maximum active local hydrodynamic diameter m and supplied gravity m/s². [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L68-L108] |
| E305 case | Required/available weight N/m, dimensionless utilization/safety factor, simultaneous phase rad and component forces N/m; undefined ratios are None; qualification remains false. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L154-L180] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L221-L241] |
| E305 inversion | Thickness/bracket m, residual N/m, convergence/status/iteration count; unsuccessful brackets or iteration limits return no thickness. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L280-L290] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L325-L346] |
| E305 workflow / fixture comparison | JSON, CSV and SHA-256 manifest; JSON refuses nonfinite numbers, CSV columns carry SI suffixes and qualification notes; fixture comparison returns actual, expected and relative-error rows. [src/digitalmodel/subsea/on_bottom_stability/e305_workflow.py:L98-L127] [src/digitalmodel/subsea/on_bottom_stability/e305_fixture.py:L158-L178] |

The APIs use scalar directions rather than a global coordinate origin or spatial reference frame; signed drag/inertia and nonnegative squared-velocity lift are explicit in the code. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L85-L87] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L116-L116] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L199-L205]
For E305, phase uses cosine velocity and negative sine acceleration, corrosion removes internal steel, field joints are length-weighted, and extra structural mass is added to weight rather than hydrodynamic inertia. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L85-L108] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L194-L205]

## 3. Method and governing standards

Standards references here describe repository attributions; clause text not checked against the licensed copy.
No coefficient or safety-factor value attributed to a licensed table is reproduced here.

| Method / implementation | Reference, edition, clause and provenance limit |
|---|---|
| `dnv_rp_f109.hydrodynamic_force_per_meter`, `lift_force_per_meter` | DNV-RP-F109, *On-Bottom Stability Design of Submarine Pipelines*, October 2021; source labels §3.2.1; clause title absent; formula and defaults at the cited code, without a `Citation` sidecar. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L1-L10] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L36-L116] |
| `dnv_rp_f109.submerged_weight_per_meter` | Same edition; source labels §2.2, clause title absent; annular steel/coating/content mass less displaced water, multiplied by fixed gravity. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L119-L184] |
| `dnv_rp_f109.absolute_stability_check` | Same edition; source labels §4.3.1, clause title absent; factored horizontal-plus-frictional-lift demand over frictional weight capacity; nonpositive weight/capacity gives infinite utilisation. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L187-L250] |
| `dnv_rp_f109.generalized_stability_check` | Same edition; source labels §4.3.2, clause title absent; factored horizontal demand over frictional effective weight plus passive resistance; nonpositive capacity gives infinite utilisation. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L254-L317] |
| `simplified.submerged_weight`, `hydrodynamic_loads`, `drag_force_per_meter`, `lift_force_per_meter`, `inertia_force_per_meter` | Own annular/Morison arithmetic; no separate edition or resolved sidecar; combined drag is signed while individual drag is squared. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L121-L288] |
| `simplified.lateral_stability_check`, `check_lateral_stability`, `check_vertical_stability` | Lateral delegates to generalized with zero passive resistance and computes required weight; kN wrapper scales inputs; vertical computes factored lift/weight independently; manifest attributes uplift to §3.4 without a clause title. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L291-L429] [src/digitalmodel/subsea/on_bottom_stability/manifest.yaml:L66-L90] |
| E305 `_annulus`, `weight_build_up` | Geometry closed form and explicit layer assumptions, including length-weighted buoyancy and maximum active local diameter; no claim of licensed geometry calibration. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L81-L108] |
| E305 `dimensionless_ratios`, `_phase_demand`, `evaluate_case` | DNV RP E305, *On-Bottom Stability Design of Submarine Pipelines*, 1988, clause “Simplified Static Stability Analysis” is named by the workflow README; demand maximizes simultaneous component demand at sampled phases, then applies target safety factor; gamma_st is recorded only. [examples/workflows/on-bottom-stability-e305/README.md:L9-L14] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L183-L241] |
| E305 `diameter_averaged_current`, `wave_from_sea_state` | Shifted-log profile integrated over diameter; equivalence to the named E305 Appendix A remains unestablished; spectral kinematics delegate to the free-span helper with its fixed gravity. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L244-L277] |
| E305 `_forward_margin`, `inverse_concrete_thickness` | Rebuild geometry and both roles per trial, bisect bracket, retain feasible endpoint; no global minimum guarantee and no DNV-OS-F101 qualification. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L280-L346] |
| E305 table `_number`, `_axis`, `_flatten`, `_table`, `_bracket`, `TableSet.lookup`, `load_tables` | Numeric/schema validation, one/two-dimensional linear interpolation, no extrapolation; E305 1988 `Citation` metadata resolves at load and lookup; interpolation suitability depends on external data basis. [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L4-L8] [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L30-L175] |
| Workflow `_cases`, `_make_case`, `_segment`, `_json_bytes`, `_csv_bytes`, `_write_outputs`, `router` | Construct/route cases, table lookups, serialize results and digest outputs; no alternate demand equation in these helpers. [src/digitalmodel/subsea/on_bottom_stability/e305_workflow.py:L28-L164] |
| Fixture `_object`, `_reject_constant`, `_read_json`, `_expected_case`, `validate_fixture`, `load_fixture`, `_read_tree`, `_outside_repository`, `convert_workbook`, `compare_fixture` | Validate/copy cached external data, bind read-byte digests and compute comparison errors; no solver launch or recalculation of workbook formulas. [src/digitalmodel/subsea/on_bottom_stability/e305_fixture.py:L23-L178] |

Edition conflict: code names October 2021, older design lists name 2010-10, while the standards inventory records 2021-05 amended 2025-09; these are conflicting recorded editions, not reconciled standard requirements; tracked by #1813. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L1-L10] [docs/domains/pipelines/_design.md:L10-L10] [docs/domains/pipelines/_subsea_pipelines.md:L41-L41] [docs/standards/subsea-pipelines-inventory.yaml:L643-L678]
DNV-OS-F101 is listed as the former designation of DNV-ST-F101, *Submarine pipeline systems*; the edition applicable to the unestablished comparator is not established. [docs/standards/subsea-pipelines-inventory.yaml:L250-L266]
The F109 manifest carries clause numbers without clause titles and the canonical/simplified numeric routines return no `Citation` sidecar; the external E305 tables enforce one, while typed parity permits an empty tuple; provenance follow-up is #1201. [src/digitalmodel/subsea/on_bottom_stability/manifest.yaml:L1-L90] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L218-L250] [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L356-L360] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L208-L240] [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L102-L110]
UNVERIFIED: U1 — E305 edition-specific demand/profile equivalence and external interpolation suitability; the repository itself records these limits; issue pending (1). [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L4-L13] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L244-L250] [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L4-L8]
UNVERIFIED: U2 — DNV-OS-F101 (*Submarine pipeline systems*; edition not established) comparison basis and private-workbook reproduction; the workflow records both as unestablished; issue pending (1). [src/digitalmodel/subsea/on_bottom_stability/e305_workflow.py:L144-L149]

## 4. Validated worked examples and regression examples

These are arithmetic examples with synthetic or zero-load inputs; they do not validate licensed coefficients or edition-specific compliance. [tests/subsea/on_bottom_stability/test_e305_parity.py:L4-L18] [tests/subsea/on_bottom_stability/test_e305_weight.py:L4-L4] [tests/subsea/on_bottom_stability/test_e305_tables.py:L4-L4]
The tolerance column defines reproducible pack criteria; the cited test nodes also passed their own assertions at the code pin.

| ID / kind | Independent basis or regression basis; input | Expected / tolerance | Existing test node / offline result |
|---|---|---|---|
| V1 validated | Synthetic closed-form current-only demand; input builder `case()` and `assess()` | Required weight 120.0 N/m; explicitly duplicated current 480.0 N/m; absolute error at most 0.001 N/m; arithmetic allowance | `tests/subsea/on_bottom_stability/test_e305_parity.py::test_current_counted_once_or_explicitly_duplicated` — PASS at BASE and AFTER. [tests/subsea/on_bottom_stability/test_e305_parity.py:L11-L38] |
| V2 validated | Closed-form integral of the shifted logarithmic profile; inline synthetic unit geometry | `2*(2*ln(2)-1)/ln(2)` m/s; absolute error at most 1e-6 m/s; integration arithmetic allowance | `tests/subsea/on_bottom_stability/test_e305_kinematics.py::test_diameter_average_is_an_integral_not_a_centre_value` — PASS at BASE and AFTER. [tests/subsea/on_bottom_stability/test_e305_kinematics.py:L11-L21] |
| V3 validated | Closed-form concentric-annulus areas and length fractions, inline synthetic `Pipe` | Submerged weight `10*(sum(masses)-displaced_area)` N/m using the independent mass ledger in the test; absolute error at most 1e-6 N/m; geometry arithmetic allowance | `tests/subsea/on_bottom_stability/test_e305_weight.py::test_weight_build_up_synthetic_annuli_and_gravity` — PASS at BASE and AFTER. [tests/subsea/on_bottom_stability/test_e305_weight.py:L11-L39] |
| R1 regression | Canonical zero-flow branch, inline geometry; exercises API defaults without qualifying their values | Hydrodynamic force zero N/m; absolute error at most 1e-6 N/m, explicit test tolerance | `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestHydrodynamicForce::test_zero_velocity_and_acceleration_gives_zero` — PASS at BASE and AFTER. [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L36-L41] |
| R2 regression | Thickness inversion checked by the module's own forward functions; inline synthetic builder | Bracket width at most 1e-7 m; forward utilization within 1e-4 of one, dimensionless, residual at least zero N/m | `tests/subsea/on_bottom_stability/test_e305_inverse.py::test_inverse_concrete_thickness_recomputes_diameter_and_forward_margin` — PASS at BASE and AFTER. [tests/subsea/on_bottom_stability/test_e305_inverse.py:L13-L48] |
| R3 regression | Synthetic interpolation and citation mutation, not standard calibration | Fw 6.0 dimensionless at the test coordinates; pack arithmetic tolerance 1e-6 dimensionless; out-of-domain and changed citation rejected | `tests/subsea/on_bottom_stability/test_e305_tables.py::test_synthetic_tables_present_interpolation_and_citation_sidecar` — PASS at BASE and AFTER. [tests/subsea/on_bottom_stability/test_e305_tables.py:L15-L44] |
| R4 regression | Registered synthetic two-segment batch; direct router and standalone entry point | Zero differing output bytes on rerun; zero SHA-256 mismatches; qualification false for the two-role results | `tests/subsea/on_bottom_stability/test_e305_workflow.py::test_e305_registration_and_registered_batch_are_reproducible` — PASS at BASE and AFTER. [tests/subsea/on_bottom_stability/test_e305_workflow.py:L24-L69] |

Offline execution record: the focused command covers 94 collected items, 93 PASS, zero FAIL, one raw SKIP mapped to NOT_RUN for the missing private reference fixture; the package subset is 60 collected items, 59 PASS and that one SKIP. [tests/subsea/on_bottom_stability/test_e305_fixture.py:L101-L113]
The additional 34 passing tests comprise the simplified public surface and the geotechnical compatibility file, including unrelated compatibility checks. [tests/test_on_bottom_stability.py:L1-L207] [tests/geotechnical/test_geotechnical.py:L18-L194]
The complete node inventory and commands are recorded in section 10.

## 5. QA gates and executable checks

Gate applicability follows AQG r0, section 10, “Python calculations”; G5 applies to every family (AQG r0, sections 9–10).
The following are pack checks, not a clearance signature.

| Gate | Applicability / route | Module-specific check IDs |
|---|---|---|
| G0 | Applicable: scalar SI contracts, external JSON/citation validation and wrapper scales | C01, C08, C10, C11, C13 |
| G1 | Applicable: Python imports and offline tests through direct APIs/router; no target-program deck is written by these APIs | C02; direct import route exercised by the module tests. [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L12-L24] [tests/subsea/on_bottom_stability/test_e305_workflow.py:L36-L46] |
| G2 | Applicable: synthetic closed forms and explicit regression bounds | C03–C07, C12, C15 |
| G3 | NOT_APPLICABLE: no time-integration state or dynamic solver launch in the case evaluation | [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L194-L241] |
| G4 | NOT_APPLICABLE: no body mesh/diffraction result is produced by the case result | [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L154-L180] |
| G5 | Applicable: reproducible JSON/CSV evidence, manifests and visible qualification limits; report appendix generation is outside this workflow | C09, C14. [examples/workflows/on-bottom-stability-e305/README.md:L108-L117] |

For test-based checks, execute the referenced node with the section-10 command form; section-4 examples use the tolerances stated there.

| ID | Gate | Executable check / numeric pass criterion and unit | Evidence / observed status |
|---|---|---|---|
| C01 | G0 | `Case.__post_init__`: seven parameter changes yield seven ValueError outcomes; zero accepted invalid cases, count | Seven PASS nodes. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L132-L151] [tests/subsea/on_bottom_stability/test_e305_parity.py:L85-L90] |
| C02 | G1 | Package `__all__` export probe: 0 import errors, 0 missing exported names, count; all 26 declared names resolve; `e305_workflow.router` also runs through registered basename `on_bottom_stability_e305` | Export probe and both registered workflow nodes PASS at BASE and AFTER; the shared engine asserts the basename and result contract. [src/digitalmodel/subsea/on_bottom_stability/__init__.py:L25-L91] [tests/workflows/test_durable_workflows.py:L57-L90] [tests/workflows/test_durable_workflows.py:L793-L822] |
| C03 | G2 | V1 `evaluate_case`: expected 120.0 / 480.0 N/m, error at most 0.001 N/m | V1 PASS; explicit pack arithmetic probe PASS. [tests/subsea/on_bottom_stability/test_e305_parity.py:L29-L38] |
| C04 | G2 | V2 `diameter_averaged_current`: closed-form integral error at most 1e-6 m/s | V2 PASS; explicit pack arithmetic probe PASS. [tests/subsea/on_bottom_stability/test_e305_kinematics.py:L11-L21] |
| C05 | G2 | V3 `weight_build_up`: component mass and buoyancy/submerged-force ledger error at most 1e-6 N/m after force conversion | V3 PASS; explicit pack arithmetic probe PASS. [tests/subsea/on_bottom_stability/test_e305_weight.py:L11-L39] |
| C06 | G2 | `test_single_wave_instant_and_simultaneous_phase_scan`: four-phase inertial demand equals `7.5*pi` N/m within 1e-6 N/m; selected zero phase gives zero N/m | PASS; synthetic inertia input cancels annular area. [tests/subsea/on_bottom_stability/test_e305_parity.py:L41-L51] |
| C07 | G2 | R2 `inverse_concrete_thickness`: converged bracket at most 1e-7 m and utilization within 1e-4 of one; zero incorrect unbracketed/iteration-limit status outcomes, count | R2 PASS; forward check is regression, not an independent design reference. [tests/subsea/on_bottom_stability/test_e305_inverse.py:L13-L48] |
| C08 | G0 | `load_tables` / `lookup`: nine malformed variants rejected, missing configuration/file rejected, citation mutation rejected, zero extrapolated results, count; synthetic Fw within 1e-6 dimensionless of 6.0 | Eleven table tests PASS. [tests/subsea/on_bottom_stability/test_e305_tables.py:L15-L95] |
| C09 | G5 | R4 `_write_outputs`: zero differing bytes, zero digest mismatches, count; exactly two segment rows each carrying both case roles | R4 PASS; manifest and serialized result retained in temporary test outputs. [tests/subsea/on_bottom_stability/test_e305_workflow.py:L48-L70] |
| C10 | G0 | Fixture conversion test: six expected/actual rows; relative error less than 1e-12 dimensionless; present malformed fixture and literal expected-answer injection rejected, zero accepted bad cases, count | Two public fixture tests PASS; independent private-data check NOT_RUN. [tests/subsea/on_bottom_stability/test_e305_fixture.py:L52-L98] [tests/subsea/on_bottom_stability/test_e305_fixture.py:L101-L113] |
| C11 | G0 | `wave_from_sea_state`: one inconsistent-gravity input rejected, count; supported output matches free-span helper with relative tolerance 1e-6 dimensionless on velocity/period | PASS; helper comparison is regression. [tests/subsea/on_bottom_stability/test_e305_kinematics.py:L24-L32] |
| C12 | G2 | Canonical `absolute_stability_check` zero weight and generalized zero capacity: infinite utilization and false stable flag; zero finite/stable sentinel outcomes and zero accepted simplified zero-weight calls, count | Existing zero-weight and zero-capacity tests PASS; simplified zero-weight calls raise. [tests/test_on_bottom_stability.py:L129-L138] [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L206-L215] [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L275-L285] |
| C13 | G0 | Hand probe: `check_lateral_stability` kN/m versus `lateral_stability_check` N/m; same explicit synthetic factor and expected generalized ratio; error at most 1e-12 dimensionless, required-weight scale error at most 1e-9 N/m | Pack unit-scale probe PASS; wrapper implementation traced. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L337-L395] |
| C14 | G5 | `test_e305_outputs_state_gamma_is_recorded_only`: zero changed demands after altering gamma_st, count; note present in JSON and CSV | PASS; this records the limitation instead of qualifying the unused factor. [tests/subsea/on_bottom_stability/test_e305_workflow.py:L211-L227] |
| C15 | G2 | `test_reduction_convention_and_dimensionless_ratios_are_explicit`: reduced current demand 30.0 N/m within 0.001 N/m; K and M within 1e-6 dimensionless of 8.0 and 1.0; zero-wave M is None | PASS; no interpolation through undefined M. [tests/subsea/on_bottom_stability/test_e305_parity.py:L73-L82] [tests/subsea/on_bottom_stability/test_e305_workflow.py:L150-L156] |

## 6. Known failure modes and pitfalls

sweep: 50 found

This count covers 50 reviewed behavior/default/unit/error-path sites, including guarded paths; it is not a count of 50 proven silent failures.
The seven package Python files were searched with these exact commands, followed by reading every definition and its input/output assumptions:

```sh
rg -n --glob '*.py' -e 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|float\(|int\(|units|default|cache' src/digitalmodel/subsea/on_bottom_stability
rg -n --glob '*.py' -e 'default|= None|= \(\)|= False|= 0|else None|else 0|except|continue|return None|return \[\]|return \{\}|float\(|int\(' src/digitalmodel/subsea/on_bottom_stability
```

No swallowed exception, record-skipping `continue`, literal error-path empty-list/dict return, text-to-int conversion, epoch default or empty-result cache write occurs in the inspected package; each exception handler re-raises and the external float conversion first validates numeric type. [src/digitalmodel/subsea/on_bottom_stability/e305_fixture.py:L99-L118] [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L30-L35] [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L102-L108] [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L152-L175]

| Site | Symptom / cause / classification | Detection check / follow-up | Observed |
|---|---|---|---|
| S01 | F109 hydrodynamic coefficient defaults are silently selected when omitted; their table attribution is not a resolved sidecar. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L36-L45] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L53-L116] | no detection check exists; #1201. | 2026-10-16 |
| S02 | F109 safety-factor default is silently selected by both stability signatures. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L48-L50] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L187-L193] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L254-L261] | no detection check exists; #1201. | 2026-10-16 |
| S03 | Canonical weight fixes gravity and assumes concentric SI annuli without geometry validation. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L157-L184] | no detection check exists for invalid geometry; issue pending (2). | 2026-10-16 |
| S04 | Canonical signed load and unchecked coefficients can give nonphysical demand or stable verdicts. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L85-L87] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L116-L116] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L236-L249] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L298-L315] | no detection check exists; issue pending (2). | 2026-10-16 |
| S05 | Absolute nonpositive weight is converted to zero capacity and infinite utilization, not an exception. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L226-L234] | C12; guarded branch. | 2026-10-16 |
| S06 | Absolute nonpositive frictional capacity yields infinite utilization; no explicit finite-input validation. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L236-L242] | no detection check exists for all coefficient/nonfinite boundaries; issue pending (2). | 2026-10-16 |
| S07 | Generalized nonpositive effective capacity yields infinite utilization and false stable flag. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L298-L309] | C12; guarded branch. | 2026-10-16 |
| S08 | Generalized check tests capacity, not positive weight independently; passive resistance may offset buoyancy. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L298-L315] | no detection check exists; issue pending (2). | 2026-10-16 |
| S09 | Simplified gravity is a module constant; its lift-coefficient default is table-attributed without a sidecar. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L62-L64] [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L171-L179] | no detection check exists for provenance; #1201. | 2026-10-16 |
| S10 | Direct simplified result construction defaults required weight to zero without computing it. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L86-L104] | no detection check exists; issue pending (2). | 2026-10-16 |
| S11 | Simplified weight assumes SI concentric geometry; invalid wall/bore or negative densities are not rejected. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L121-L162] | no detection check exists; issue pending (2). | 2026-10-16 |
| S12 | Combined hydrodynamic drag is signed, while the individual drag helper squares velocity. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L198-L204] [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L214-L234] | no detection check exists for negative individual drag; issue pending (2). | 2026-10-16 |
| S13 | Individual lift/inertia helpers trust SI values and supplied coefficients without boundary checks. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L237-L288] | no detection check exists for invalid inputs; issue pending (2). | 2026-10-16 |
| S14 | Simplified lateral weight at or below zero raises rather than returning the canonical sentinel. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L331-L335] [tests/test_on_bottom_stability.py:L129-L138] | C12 covers canonical behavior; simplified zero-weight test covers the exception. | 2026-10-16 |
| S15 | Simplified lateral sets passive resistance to zero; callers cannot claim soil-resistance credit through it. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L337-L344] | C13; explicit delegation and unit probe. | 2026-10-16 |
| S16 | Nonpositive friction makes required weight infinite; finite-positive friction itself is not enforced. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L348-L359] | no detection check exists; issue pending (2). | 2026-10-16 |
| S17 | The kN wrapper multiplies inputs while retaining required weight in N/m; mixing interfaces changes scale. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L370-L395] | C13; SI/kN contract. | 2026-10-16 |
| S18 | Vertical check enforces positive weight only; lift and factor signs/finiteness are unchecked. [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L398-L429] | no detection check exists for those inputs; issue pending (2). | 2026-10-16 |
| S19 | E305 Pipe validates finite/nonnegative fields, bore, corrosion and joint fraction; inputs carry explicit SI assumptions. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L32-L65] | no detection check exists for every Pipe boundary; issue pending (2). | 2026-10-16 |
| S20 | E305 Case validates velocities/coefficient and factor domains, role and reduction convention. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L111-L151] | C01; seven explicit invalid variants only. | 2026-10-16 |
| S21 | Duplicate current defaults false, changing demand if explicitly enabled. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L128-L128] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L194-L205] | C03; both modes tested. | 2026-10-16 |
| S22 | Phase defaults None and resolution defaults 720; sampled maximum is not a proved continuous maximum. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L129-L151] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L221-L226] | C06 covers a four-phase synthetic case; no resolution-convergence detection check exists; issue pending (2). | 2026-10-16 |
| S23 | Case results default to empty citations, unestablished comparator and false qualification. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L178-L180] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L208-L215] | C09 and C14 cover visible limits; typed parity uses no table provenance. | 2026-10-16 |
| S24 | Zero wave velocity returns undefined M; table mode refuses that case. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L183-L191] [src/digitalmodel/subsea/on_bottom_stability/e305_workflow.py:L64-L69] | C15; zero-wave table rejection. | 2026-10-16 |
| S25 | Nonpositive available weight returns None utilization and false demand-pass. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L230-L235] | no detection check exists; issue pending (2). | 2026-10-16 |
| S26 | Zero demand returns None safety factor even with positive available weight. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L230-L235] | no detection check exists for this sentinel; issue pending (2). | 2026-10-16 |
| S27 | Layer model removes corrosion internally and treats extra mass as weight, with length-fraction buoyancy. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L85-L108] | C05; explicit geometry/mass convention. | 2026-10-16 |
| S28 | Inactive body/joint diameters use zero before maximum selection. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L94-L108] | no detection check exists for both endpoint joint fractions; issue pending (2). | 2026-10-16 |
| S29 | gamma_st is accepted and returned but does not scale demand. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L227-L240] | C14; zero demand changes expected when only gamma_st changes. | 2026-10-16 |
| S30 | Shifted log current assumes supplied reference height and roughness convention, not confirmed E305 equivalence. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L244-L257] | C04 checks integration only; issue pending (1). | 2026-10-16 |
| S31 | Spectral wrapper requires the upstream fixed gravity and assumes pipe elevation from bed below depth. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L260-L273] | C11; inconsistent gravity raises. | 2026-10-16 |
| S32 | Zero Hs returns zero velocity and the input period without consulting the spectral helper. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L274-L277] | no detection check exists for this branch; issue pending (2). | 2026-10-16 |
| S33 | Inverse defaults iteration bound and numerical thickness tolerance, not standard-derived limits. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L309-L324] | C07; bracket and iteration-limit outcomes. | 2026-10-16 |
| S34 | Inverse feasible lower endpoint can return zero iterations without searching for a smaller root. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L325-L334] | C07 covers lower_feasible; no exact-endpoint detection check exists; issue pending (2). | 2026-10-16 |
| S35 | Inverse failed bracket or iteration exhaustion returns None thickness and false convergence. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L331-L346] | C07; no accepted feasible thickness on those two paths. | 2026-10-16 |
| S36 | Table float conversion accepts only already numeric finite non-boolean input, so locale-formatted text is rejected. [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L30-L35] | C08 checks malformed data; no locale-string-specific detection check exists; issue pending (2). | 2026-10-16 |
| S37 | Axis/output SI metadata is enforced, but R/clay coordinate semantics remain provider-defined. [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L58-L110] | C08 checks malformed units only; issue pending (1). | 2026-10-16 |
| S38 | Invalid table Citation construction is caught and re-raised as TableError. [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L102-L108] | C08; exception is not swallowed. | 2026-10-16 |
| S39 | Missing named table uses a None lookup sentinel then raises; no fallback coefficient. [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L132-L136] | no detection check exists for an unknown lookup name; issue pending (2). | 2026-10-16 |
| S40 | Interpolation initializes value and offset to zero and corner weight to one; these are accumulators, not missing-result fallbacks. [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L140-L149] | C08; synthetic interpolation. | 2026-10-16 |
| S41 | Table file defaults to environment lookup, bounds read size, and re-raises unreadable-file exceptions. [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L152-L163] | C08; absent file/configuration rejected. | 2026-10-16 |
| S42 | Malformed table JSON exceptions are caught and re-raised. [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L164-L172] | C08; duplicate-key and malformed variants. | 2026-10-16 |
| S43 | Wiki root defaults None and passes that selection to citation resolution. [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L152-L175] | no detection check exists for every default-root configuration; issue pending (2). | 2026-10-16 |
| S44 | Fixture JSON builds an initially empty mapping but rejects duplicate keys; nonfinite constants are errors. [src/digitalmodel/subsea/on_bottom_stability/e305_fixture.py:L23-L37] | no detection check exists for fixture duplicate-key input; issue pending (2). | 2026-10-16 |
| S45 | Fixture SI inputs and expected results must cover both roles, positive available weight and source digests; comparison gravity is fixed by schema. [src/digitalmodel/subsea/on_bottom_stability/e305_fixture.py:L40-L89] | C10; present malformed fixture rejected. | 2026-10-16 |
| S46 | Cached workbook cell locator exceptions are re-raised; missing cached cells fail instead of producing empty results. [src/digitalmodel/subsea/on_bottom_stability/e305_fixture.py:L99-L118] | no detection check exists for every locator/cache-missing branch; issue pending (2). | 2026-10-16 |
| S47 | Fixture comparison of zero expected value emits zero only for exact agreement and infinity otherwise. [src/digitalmodel/subsea/on_bottom_stability/e305_fixture.py:L169-L178] | no detection check exists for both zero-target branches; issue pending (2). | 2026-10-16 |
| S48 | Typed workflow cases start with empty lookup mappings; tables are absent only in explicit parity mode. [src/digitalmodel/subsea/on_bottom_stability/e305_workflow.py:L38-L50] [src/digitalmodel/subsea/on_bottom_stability/e305_workflow.py:L130-L151] | C09; C08 catches missing tables in table mode. | 2026-10-16 |
| S49 | Table source digest/citations default to None/empty in parity mode; output writing is optional rather than an evidence requirement. [src/digitalmodel/subsea/on_bottom_stability/e305_workflow.py:L150-L164] | C09 checks enabled output; no absent-output detection check exists; issue pending (2). | 2026-10-16 |
| S50 | Strict JSON serialization rejects nonfinite values; CSV serializes optional ratios without imposing a unit-aware schema. [src/digitalmodel/subsea/on_bottom_stability/e305_workflow.py:L98-L125] | C09 covers reproducibility; no undefined-ratio CSV detection check exists; issue pending (2). | 2026-10-16 |

Incident/reference reconciliation: the issue searches found capability and audit leads rather than four confirmed module incidents; the reproducible stale-import defect is #1923, and the duplicate-surface risk is covered by the existing audit #1817 and the delegation cited above. [tests/conftest.py:L52-L59] [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L337-L344]
The standard-edition discrepancy is tracked by #1813; inline parameter provenance by #1201; physical soil-model scope by #1838; missing displacement/dynamic capability remains a coverage item under #1812/#1835 rather than a result of the scalar checks. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L1-L10] [docs/standards/subsea-pipelines-inventory.yaml:L643-L678] [src/digitalmodel/subsea/on_bottom_stability/manifest.yaml:L1-L90] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L254-L317] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L154-L180]

| Confirmed defect or qualification limit | Detection / issue |
|---|---|
| Legacy pipeline test imports an absent module and is normally ignored; explicit BASE/AFTER invocation produces `ModuleNotFoundError: No module named 'digitalmodel.subsea.pipeline.on_bottom_stability'` | Explicit test-file collection; no runtime detection check exists; #1923. [tests/subsea/pipeline/test_on_bottom_stability.py:L17-L32] [tests/conftest.py:L52-L59] |
| F109 code/older docs/standards inventory record different editions | Read and compare the three cited metadata entries; no automated detection check exists; #1813. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L1-L10] [docs/domains/pipelines/_design.md:L10-L10] [docs/standards/subsea-pipelines-inventory.yaml:L643-L678] |
| Signed horizontal load can produce a negative utilization reported stable by scalar stability checks | No detection check exists in the shipped tests; issue pending (2); preserve the signed-load caveat until a physical-direction contract is established. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L236-L249] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L298-L315] [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L337-L359] |
| E305 standard equivalence, DNV-OS-F101 comparator and private reference remain unestablished | C10 can detect private parity once data exists; no edition-equivalence/comparator detection check exists; issue pending (1). [examples/workflows/on-bottom-stability-e305/README.md:L3-L14] [tests/subsea/on_bottom_stability/test_e305_fixture.py:L101-L113] |

## 7. Lessons log

Append-only log; future entries use date, anonymous change reference, lesson, evidence, pack change and shared-log reference.

| Date | Change reference | Lesson | Evidence | Pack change | Shared-log reference |
|---|---|---|---|---|---|
| 2026-10-16 | Initial documentation | A forward self-check is a regression example; it does not supply independent qualification | [tests/subsea/on_bottom_stability/test_e305_inverse.py:L13-L48] | Inverse example classified R2; limits retained | Not assigned |
| 2026-10-16 | Initial documentation | Synthetic tables check mechanics and provenance resolution, not licensed calibration | [tests/subsea/on_bottom_stability/test_e305_tables.py:L4-L44] | Table example classified R3 | Not assigned |

## 8. Ownership

Proposed review routing for this pack: engineering calculations quality manager; deputy engineering calculations quality manager; producing discipline: engineering calculations; escalation of ambiguous clauses, tolerances or cross-family changes to the maintainer through the family quality review.
UNVERIFIED: U3 — named assignments and deputy availability are not established by repository evidence; issue pending (4).
No clearance or signature is asserted by this draft.

## 9. Dependencies and implementation inventory

The F109/simplified calculations use standard-library math and result containers; the E305 calculation layer also imports `Citation`, tables validate it through the citation resolver, fixtures lazily import openpyxl, and spectral inputs delegate to `subsea.pipeline.free_span.wave_velocity`. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L12-L15] [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L51-L59] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L16-L22] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L260-L277] [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L23-L23] [src/digitalmodel/subsea/on_bottom_stability/e305_fixture.py:L126-L139]
The downstream F109 pipeline workflow delegates weight, force and stability to the package rather than providing a second criterion. [src/digitalmodel/subsea/pipeline/integrity_workflows.py:L7-L71]
The E305 direct batch route writes JSON/CSV/manifest evidence; neither that route nor the scalar APIs require a solver version in their input/result contracts. [src/digitalmodel/subsea/on_bottom_stability/e305_workflow.py:L130-L164] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L53-L60] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L187-L193]
Inventory-row IDs for these dependency modules are not asserted; source paths above are the dependency identifiers used by this pack; inventory reconciliation remains #1818.

Implementation-map pin: the header code pin `7aad603be1c6136106dbb2381ce5096b050654f8`; embedded data revision: `270c8759c374b67a7f032953e7fcdb14e0224461`. [docs/registry/implementation-map.yaml:L1-L5]
The code-pin map is byte-identical to the previously cited map; its data revision, inventory rows and citation ranges are unchanged.
The inventory below reproduces all 165 implementation rows of the two families with rows inside the package, including external implementations and helper/example rows; it records map classification rather than newly validating external implementations. [docs/registry/implementation-map.yaml:L1-L5]

Counted-family order is `dnv-rp-e305-on-bottom-stability` (23 in-package rows), then `wind-and-current-force-routes` (9); there are only two applicable map families.
The family tables below provide the row-level citations for those counts and their complete inventories.

A8 search record: Python `re`, case-sensitive, against every `.py` file inside the header package at the code pin; each hit is assigned to its innermost enclosing function/class, with one module-level definition.
The JSON below records the exact map patterns as JSON strings; decode the strings before using them as Python regular expressions.

```json
[
  {
    "family": "dnv-rp-e305-on-bottom-stability",
    "retained": [
      {
        "search": "vocabulary",
        "pattern": "generalised.stability|generalized.stability|dynamic.stability|hydrodynamic.force|soil.resistance"
      },
      {
        "search": "constants_or_keys",
        "pattern": "lateral_resistance|submerged_weight|passive_resistance|soil_friction|stability_parameter"
      },
      {
        "search": "algorithm",
        "pattern": "E305|e305|on_bottom_stability|weight_build_up|hydrodynamic_loads|lateral_stability|vertical_stability|submerged_weight|passive_resistance|soil_resistance|stability_parameter|weight_demand|WeightResult|WeightDemand|demand|lateral|hydrodynamic|buoyancy|force|_annulus|weight_build_up|math\\.pi|wave_velocity_m_s|reference_current_m_s|evaluate_case|weight_build_up"
      }
    ],
    "excluded_path_matches": []
  },
  {
    "family": "wind-and-current-force-routes",
    "retained": [
      {
        "search": "algorithm",
        "pattern": "(?:0\\.5|1\\s*/\\s*2).*(?:rho|density).*(?:cd|drag|area)|(?:wind|current|drag|lift)_forces?|calculate_(?:wind|current)|OCIMF|current_profile|wind_height_coefficient"
      },
      {
        "search": "vocabulary",
        "pattern": "OCIMF|wind.load|current.load|wind.force|current.force|drag.coefficient|current.profile"
      },
      {
        "search": "constants_or_keys",
        "pattern": "Cxw|Cyw|CXw|CYw|CXc|CYc|drag_coefficient|lift_coefficient|wind_speed|current_velocity"
      }
    ],
    "excluded_path_matches": []
  }
]
```

| Counted family | Signals searched, case-sensitive | Hit lines / distinct definitions | Classification / further second implementations |
|---|---|---|---|
| dnv-rp-e305-on-bottom-stability | Every family signal, including the complete algorithm pattern | 231 / 42 | Compute/wrapper definitions are already inventoried or listed in section 3; additional validation/data holders include `Case.__post_init__`, `CaseResult` and `SubmergedWeightResult`; 0 further second implementations. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L111-L180] [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L67-L118] |
| wind-and-current-force-routes | Every family signal | 24 / 9 | Seven functions already inventoried or listed in section 3; two module definitions are exports/docstrings/constants; 0 further second implementations. [src/digitalmodel/subsea/on_bottom_stability/__init__.py:L1-L91] [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L1-L64] |

The narrower vocabulary/key search alone yields 48 hit lines and 14 definitions. Its E305 named hit definitions are `absolute_stability_check`, `generalized_stability_check`, `hydrodynamic_force_per_meter`, `submerged_weight_per_meter`, `_forward_margin`, `_segment`, `check_lateral_stability`, `check_vertical_stability`, `lateral_stability_check`, and `submerged_weight`; their implementations are cited in section 3.
The wind/current named hits are canonical lift, `_phase_demand`, simplified lateral/check-lateral, drag, combined hydrodynamic loads and simplified lift; their implementations are cited in section 3.
The full-signal search includes class/module declarations and validation, serialization and table helpers already described in sections 2, 3 and 9; these matches introduce no additional calculation implementation.
A8 result: PASS — complete inventory and zero additional in-package second implementations after accounting for sections 3 and 9.

Repository-wide follow-up search, including `scripts/`:

```sh
rg -n --hidden --glob '*.py' --glob '!.git/**' --glob '!.venv/**' 'generalized_stability_check|absolute_stability_check|lateral_stability_check|weight_build_up|def .*stability|def .*submerged_weight|def .*hydrodynamic_loads' src scripts tests examples
```

The search record contains 162 matching lines in 75 files, including two script files; broad stability-name hits include other engineering families and are not automatically second on-bottom implementations.
The pipeline delegate is the relevant external load route; a script named for test stability is test orchestration rather than a pipe-stability criterion. [src/digitalmodel/subsea/pipeline/integrity_workflows.py:L7-L71] [scripts/python/digitalmodel/tools/check_stability_analysis.py:L1-L35]
Map follow-up: add/reconcile inverse-thickness search coverage, linear interpolation/citation validation, cached-workbook conversion and spectral-wave delegation as distinct method families when they gain in-package map rows; these do not alter the counted-family result; #1818. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L244-L277] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L309-L346] [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L132-L175] [src/digitalmodel/subsea/on_bottom_stability/e305_fixture.py:L126-L178]

### Map family: dnv-rp-e305-on-bottom-stability

In-package rows: 23; full map inventory: 30 rows. [docs/registry/implementation-map.yaml:L25896-L26252]

| Map row | Path | Definition | Map relation | Evidence |
|---|---|---|---|---|
| 1 | `src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py` | `_annulus` | `independent` | [docs/registry/implementation-map.yaml:L25903-L25912] |
| 2 | `src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py` | `weight_build_up` | `independent` | [docs/registry/implementation-map.yaml:L25913-L25924] |
| 3 | `src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py` | `dimensionless_ratios` | `independent` | [docs/registry/implementation-map.yaml:L25925-L25934] |
| 4 | `src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py` | `_phase_demand` | `independent` | [docs/registry/implementation-map.yaml:L25935-L25944] |
| 5 | `src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py` | `evaluate_case` | `independent` | [docs/registry/implementation-map.yaml:L25945-L25957] |
| 6 | `src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py` | `diameter_averaged_current` | `independent` | [docs/registry/implementation-map.yaml:L25958-L25967] |
| 7 | `src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py` | `_forward_margin` | `independent` | [docs/registry/implementation-map.yaml:L25968-L25980] |
| 8 | `src/digitalmodel/subsea/on_bottom_stability/e305_tables.py` | `_table` | `independent` | [docs/registry/implementation-map.yaml:L25981-L25990] |
| 9 | `src/digitalmodel/subsea/on_bottom_stability/e305_tables.py` | `load_tables` | `independent` | [docs/registry/implementation-map.yaml:L25991-L26002] |
| 10 | `src/digitalmodel/subsea/on_bottom_stability/e305_workflow.py` | `_segment` | `independent` | [docs/registry/implementation-map.yaml:L26003-L26013] |
| 11 | `src/digitalmodel/subsea/on_bottom_stability/e305_workflow.py` | `_write_outputs` | `independent` | [docs/registry/implementation-map.yaml:L26014-L26023] |
| 12 | `src/digitalmodel/subsea/on_bottom_stability/e305_workflow.py` | `router` | `independent` | [docs/registry/implementation-map.yaml:L26024-L26037] |
| 13 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `lateral_stability_check` | `caller` | [docs/registry/implementation-map.yaml:L26038-L26049] |
| 14 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `check_lateral_stability` | `caller` | [docs/registry/implementation-map.yaml:L26050-L26060] |
| 15 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `check_vertical_stability` | `independent` | [docs/registry/implementation-map.yaml:L26061-L26070] |
| 16 | `src/digitalmodel/subsea/pipeline/integrity_workflows.py` | `run_on_bottom_stability` | `independent` | [docs/registry/implementation-map.yaml:L26071-L26080] |
| 17 | `tests/subsea/on_bottom_stability/synthetic_e305_tables.py` | `make_synthetic_tables` | `test_helper` | [docs/registry/implementation-map.yaml:L26081-L26090] |
| 18 | `tests/subsea/on_bottom_stability/test_e305_fixture.py` | `synthetic_workbook_and_map` | `test_helper` | [docs/registry/implementation-map.yaml:L26091-L26100] |
| 19 | `tests/subsea/on_bottom_stability/test_e305_fixture.py` | `test_synthetic_workbook_conversion_is_external_cached_and_reproducible` | `test_helper` | [docs/registry/implementation-map.yaml:L26101-L26112] |
| 20 | `tests/subsea/on_bottom_stability/test_e305_workflow.py` | `configuration` | `test_helper` | [docs/registry/implementation-map.yaml:L26113-L26122] |
| 21 | `tests/subsea/on_bottom_stability/test_e305_workflow.py` | `table_configuration` | `test_helper` | [docs/registry/implementation-map.yaml:L26123-L26134] |
| 22 | `tests/subsea/pipeline/test_on_bottom_stability.py` | `TestSubmergedWeight.test_bare_pipe` | `test_helper` | [docs/registry/implementation-map.yaml:L26135-L26144] |
| 23 | `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py` | `hydrodynamic_force_per_meter` | `independent` | [docs/registry/implementation-map.yaml:L26145-L26156] |
| 24 | `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py` | `lift_force_per_meter` | `independent` | [docs/registry/implementation-map.yaml:L26157-L26167] |
| 25 | `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py` | `submerged_weight_per_meter` | `independent` | [docs/registry/implementation-map.yaml:L26168-L26179] |
| 26 | `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py` | `absolute_stability_check` | `independent` | [docs/registry/implementation-map.yaml:L26180-L26191] |
| 27 | `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py` | `generalized_stability_check` | `independent` | [docs/registry/implementation-map.yaml:L26192-L26203] |
| 28 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `submerged_weight` | `independent` | [docs/registry/implementation-map.yaml:L26204-L26215] |
| 29 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `lift_force_per_meter` | `independent` | [docs/registry/implementation-map.yaml:L26216-L26226] |
| 30 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `inertia_force_per_meter` | `independent` | [docs/registry/implementation-map.yaml:L26227-L26237] |

### Map family: wind-and-current-force-routes

In-package rows: 9; full map inventory: 135 rows. [docs/registry/implementation-map.yaml:L15393-L16806]

| Map row | Path | Definition | Map relation | Evidence |
|---|---|---|---|---|
| 1 | `examples/demos/gtm/demo_03_deepwater_mudmat_installation.py` | `calc_splash_zone` | `example` | [docs/registry/implementation-map.yaml:L15400-L15409] |
| 2 | `examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py` | `calc_phase_3_splash` | `example` | [docs/registry/implementation-map.yaml:L15410-L15419] |
| 3 | `examples/demos/gtm/results_store_demo05.py` | `<module>` | `example` | [docs/registry/implementation-map.yaml:L15420-L15428] |
| 4 | `examples/demos/gtm/tests/test_demo_05_results_store.py` | `_good_case` | `example` | [docs/registry/implementation-map.yaml:L15429-L15438] |
| 5 | `examples/domains/fpso/fpso_mooring_analysis.py` | `calculate_environmental_forces` | `example` | [docs/registry/implementation-map.yaml:L15439-L15448] |
| 6 | `examples/domains/fpso/fpso_mooring_analysis.py` | `main` | `example` | [docs/registry/implementation-map.yaml:L15449-L15460] |
| 7 | `examples/domains/ocimf/ocimf_demo.py` | `main` | `example` | [docs/registry/implementation-map.yaml:L15461-L15470] |
| 8 | `examples/domains/workflows/01_spread_mooring_8point.py` | `calculate_environmental_forces` | `example` | [docs/registry/implementation-map.yaml:L15471-L15480] |
| 9 | `examples/domains/workflows/03_calm_buoy.py` | `analyze_calm_buoy` | `example` | [docs/registry/implementation-map.yaml:L15481-L15490] |
| 10 | `examples/domains/workflows/05_jackup_installation.py` | `analyze_jackup_mooring` | `example` | [docs/registry/implementation-map.yaml:L15491-L15500] |
| 11 | `examples/domains/workflows/06_floating_wind_turbine.py` | `analyze_floating_wind_mooring` | `example` | [docs/registry/implementation-map.yaml:L15501-L15510] |
| 12 | `examples/domains/workflows/07_wave_energy_converter.py` | `analyze_wec_mooring` | `example` | [docs/registry/implementation-map.yaml:L15511-L15520] |
| 13 | `examples/domains/workflows/08_aquaculture_mooring.py` | `analyze_aquaculture_mooring` | `example` | [docs/registry/implementation-map.yaml:L15521-L15530] |
| 14 | `examples/domains/workflows/09_spm_terminal.py` | `analyze_spm_terminal` | `example` | [docs/registry/implementation-map.yaml:L15531-L15540] |
| 15 | `examples/domains/workflows/10_dp_backup_mooring.py` | `analyze_dp_backup_mooring` | `example` | [docs/registry/implementation-map.yaml:L15541-L15550] |
| 16 | `scripts/capabilities/build_onepagers.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L15551-L15559] |
| 17 | `scripts/python/digitalmodel/legacy/extract_ocimf_database.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L15560-L15568] |
| 18 | `scripts/python/digitalmodel/legacy/extract_ocimf_database.py` | `extract_ocimf_database` | `independent` | [docs/registry/implementation-map.yaml:L15569-L15578] |
| 19 | `scripts/python/digitalmodel/legacy/generate_integration_charts.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L15579-L15587] |
| 20 | `scripts/python/digitalmodel/legacy/generate_ocimf_report.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L15588-L15597] |
| 21 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler._setup_test_data` | `independent` | [docs/registry/implementation-map.yaml:L15598-L15607] |
| 22 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_ocimf_lookup` | `independent` | [docs/registry/implementation-map.yaml:L15608-L15617] |
| 23 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.run_all` | `independent` | [docs/registry/implementation-map.yaml:L15618-L15629] |
| 24 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L15630-L15639] |
| 25 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.generate_excel_reference_data` | `independent` | [docs/registry/implementation-map.yaml:L15640-L15649] |
| 26 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.export_statistics_to_csv` | `independent` | [docs/registry/implementation-map.yaml:L15650-L15659] |
| 27 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.export_json_summary` | `independent` | [docs/registry/implementation-map.yaml:L15660-L15669] |
| 28 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.run_complete_validation` | `independent` | [docs/registry/implementation-map.yaml:L15670-L15684] |
| 29 | `scripts/python/digitalmodel/legacy/visualize_ocimf_data.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L15685-L15693] |
| 30 | `scripts/python/digitalmodel/legacy/visualize_ocimf_data.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L15694-L15703] |
| 31 | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | `make_polar_overlay` | `independent` | [docs/registry/implementation-map.yaml:L15704-L15713] |
| 32 | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | `parse_args` | `independent` | [docs/registry/implementation-map.yaml:L15714-L15723] |
| 33 | `src/digitalmodel/ansys/pressure_vessel.py` | `PressureVesselGenerator.generate_wind_load` | `independent` | [docs/registry/implementation-map.yaml:L15724-L15733] |
| 34 | `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py` | `SyntheticRopeDatabaseClient._generate_rope_spec` | `independent` | [docs/registry/implementation-map.yaml:L15734-L15743] |
| 35 | `src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L15744-L15753] |
| 36 | `src/digitalmodel/drilling_riser/drift_off.py` | `environmental_force_n` | `independent` | [docs/registry/implementation-map.yaml:L15754-L15763] |
| 37 | `src/digitalmodel/drilling_riser/envelope.py` | `CurrentProfile.drag_load_n_per_m` | `independent` | [docs/registry/implementation-map.yaml:L15764-L15773] |
| 38 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `as_analysed_sections` | `independent` | [docs/registry/implementation-map.yaml:L15774-L15783] |
| 39 | `src/digitalmodel/dynmoor_mooring/workflow.py` | `router` | `independent` | [docs/registry/implementation-map.yaml:L15784-L15793] |
| 40 | `src/digitalmodel/engine.py` | `engine` | `caller` | [docs/registry/implementation-map.yaml:L15794-L15806] |
| 41 | `src/digitalmodel/hydrodynamics/cli.py` | `ocimf_wind_cmd` | `independent` | [docs/registry/implementation-map.yaml:L15807-L15816] |
| 42 | `src/digitalmodel/hydrodynamics/cli.py` | `ocimf_current_cmd` | `independent` | [docs/registry/implementation-map.yaml:L15817-L15826] |
| 43 | `src/digitalmodel/hydrodynamics/cli.py` | `combined_env_cmd` | `independent` | [docs/registry/implementation-map.yaml:L15827-L15836] |
| 44 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_total_viv_fatigue` | `independent` | [docs/registry/implementation-map.yaml:L15837-L15846] |
| 45 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.tabulate_viv_results` | `independent` | [docs/registry/implementation-map.yaml:L15847-L15856] |
| 46 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_total_viv_fatigue` | `independent` | [docs/registry/implementation-map.yaml:L15857-L15866] |
| 47 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.tabulate_viv_results` | `independent` | [docs/registry/implementation-map.yaml:L15867-L15876] |
| 48 | `src/digitalmodel/infrastructure/utils/ETL_components.py` | `ETL_components.transform_viv_current_reformat_1` | `independent` | [docs/registry/implementation-map.yaml:L15877-L15886] |
| 49 | `src/digitalmodel/installation/installation_pamphlet/calculations.py` | `compute_splashzone_envelope` | `independent` | [docs/registry/implementation-map.yaml:L15887-L15896] |
| 50 | `src/digitalmodel/installation/jumper_installation/calculations.py` | `calc_phase_3_splash` | `independent` | [docs/registry/implementation-map.yaml:L15897-L15906] |
| 51 | `src/digitalmodel/marine_ops/marine_analysis/__main__.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L15907-L15916] |
| 52 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `OCIMFDatabase.get_coefficients` | `independent` | [docs/registry/implementation-map.yaml:L15917-L15926] |
| 53 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `EnvironmentalForces.calculate_total_forces` | `independent` | [docs/registry/implementation-map.yaml:L15927-L15936] |
| 54 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `create_sample_database` | `independent` | [docs/registry/implementation-map.yaml:L15937-L15946] |
| 55 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/workflow.py` | `OCIMFWorkflow.router` | `independent` | [docs/registry/implementation-map.yaml:L15947-L15956] |
| 56 | `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L15957-L15965] |
| 57 | `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py` | `extract_ocimf_database` | `copy` | [docs/registry/implementation-map.yaml:L15966-L15975] |
| 58 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler._setup_test_data` | `copy` | [docs/registry/implementation-map.yaml:L15976-L15987] |
| 59 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_ocimf_lookup` | `copy` | [docs/registry/implementation-map.yaml:L15988-L15997] |
| 60 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.run_all` | `copy` | [docs/registry/implementation-map.yaml:L15998-L16009] |
| 61 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `main` | `copy` | [docs/registry/implementation-map.yaml:L16010-L16019] |
| 62 | `src/digitalmodel/marine_ops/marine_analysis/reporting/ocimf_interactive_report.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L16020-L16029] |
| 63 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.generate_excel_reference_data` | `copy` | [docs/registry/implementation-map.yaml:L16030-L16039] |
| 64 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.export_statistics_to_csv` | `copy` | [docs/registry/implementation-map.yaml:L16040-L16049] |
| 65 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.export_json_summary` | `copy` | [docs/registry/implementation-map.yaml:L16050-L16059] |
| 66 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.run_complete_validation` | `copy` | [docs/registry/implementation-map.yaml:L16060-L16074] |
| 67 | `src/digitalmodel/marine_ops/marine_analysis/visualization/integration_charts.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L16075-L16083] |
| 68 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `_output_dir` | `independent` | [docs/registry/implementation-map.yaml:L16084-L16093] |
| 69 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `_cli_output_dir` | `independent` | [docs/registry/implementation-map.yaml:L16094-L16103] |
| 70 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `load_data` | `independent` | [docs/registry/implementation-map.yaml:L16104-L16113] |
| 71 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L16114-L16126] |
| 72 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `OCIMFDatabase.get_coefficients` | `copy` | [docs/registry/implementation-map.yaml:L16127-L16136] |
| 73 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `EnvironmentalForces.calculate_total_forces` | `copy` | [docs/registry/implementation-map.yaml:L16137-L16146] |
| 74 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `create_sample_database` | `copy` | [docs/registry/implementation-map.yaml:L16147-L16156] |
| 75 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._environmental_forces` | `independent` | [docs/registry/implementation-map.yaml:L16157-L16168] |
| 76 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._ocimf_database_path` | `independent` | [docs/registry/implementation-map.yaml:L16169-L16178] |
| 77 | `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` | `_resolve_arrow_direction_in_body_frame` | `independent` | [docs/registry/implementation-map.yaml:L16179-L16188] |
| 78 | `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` | `polar_force_overlay` | `independent` | [docs/registry/implementation-map.yaml:L16189-L16198] |
| 79 | `src/digitalmodel/naval_architecture/maneuvering_envelope.py` | `threshold_speed_for_steerage` | `independent` | [docs/registry/implementation-map.yaml:L16199-L16208] |
| 80 | `src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py` | `run_maneuvering_envelope_sweep` | `independent` | [docs/registry/implementation-map.yaml:L16209-L16218] |
| 81 | `src/digitalmodel/orcaflex/installation_analysis.py` | `SplashZoneInput.calculate_splash_zone_loads` | `independent` | [docs/registry/implementation-map.yaml:L16219-L16228] |
| 82 | `src/digitalmodel/orcawave/drift_forces.py` | `compute_wind_current_drift` | `independent` | [docs/registry/implementation-map.yaml:L16229-L16238] |
| 83 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py` | `KnowledgeIntegrator._extract_configurations` | `independent` | [docs/registry/implementation-map.yaml:L16239-L16248] |
| 84 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder.build` | `independent` | [docs/registry/implementation-map.yaml:L16249-L16258] |
| 85 | `src/digitalmodel/solvers/orcaflex/reporting/extractors/loads_extractor.py` | `extract_loads` | `independent` | [docs/registry/implementation-map.yaml:L16259-L16268] |
| 86 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `calculate_dual_chute_drag` | `independent` | [docs/registry/implementation-map.yaml:L16269-L16280] |
| 87 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `assess_all_load_cases` | `caller` | [docs/registry/implementation-map.yaml:L16281-L16293] |
| 88 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `simulate_deployment_transient` | `independent` | [docs/registry/implementation-map.yaml:L16294-L16304] |
| 89 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `export_results_yaml` | `independent` | [docs/registry/implementation-map.yaml:L16305-L16314] |
| 90 | `src/digitalmodel/structural/parachute/parachute_drag.py` | `calculate_drag_force` | `independent` | [docs/registry/implementation-map.yaml:L16315-L16324] |
| 91 | `src/digitalmodel/subsea/mooring_analysis/cli.py` | `design_verification` | `independent` | [docs/registry/implementation-map.yaml:L16325-L16334] |
| 92 | `src/digitalmodel/subsea/mooring_analysis/designer.py` | `MooringDesigner.calculate_environmental_loads` | `independent` | [docs/registry/implementation-map.yaml:L16335-L16344] |
| 93 | `src/digitalmodel/subsea/mooring_analysis/dynmoor.py` | `build_wind_model.force` | `independent` | [docs/registry/implementation-map.yaml:L16345-L16354] |
| 94 | `src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py` | `_phase_demand` | `independent` | [docs/registry/implementation-map.yaml:L16355-L16364] |
| 95 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `hydrodynamic_loads` | `independent` | [docs/registry/implementation-map.yaml:L16365-L16374] |
| 96 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `drag_force_per_meter` | `independent` | [docs/registry/implementation-map.yaml:L16375-L16384] |
| 97 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `lateral_stability_check` | `caller` | [docs/registry/implementation-map.yaml:L16385-L16396] |
| 98 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `check_lateral_stability` | `caller` | [docs/registry/implementation-map.yaml:L16397-L16408] |
| 99 | `src/digitalmodel/subsea/pipeline/integrity_workflows.py` | `run_on_bottom_stability` | `independent` | [docs/registry/implementation-map.yaml:L16409-L16418] |
| 100 | `src/digitalmodel/subsea/viv_analysis/screening.py` | `VIVScreening.screen_with_current_profile` | `independent` | [docs/registry/implementation-map.yaml:L16419-L16428] |
| 101 | `src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py` | `viv_analysis` | `independent` | [docs/registry/implementation-map.yaml:L16429-L16438] |
| 102 | `src/digitalmodel/subsea/viv_analysis/vortex_shedding.py` | `VortexSheddingAnalyzer.analyze_with_profile` | `independent` | [docs/registry/implementation-map.yaml:L16439-L16448] |
| 103 | `src/digitalmodel/workflows/workflow_automation/templates.py` | `create_mooring_design_template` | `independent` | [docs/registry/implementation-map.yaml:L16449-L16458] |
| 104 | `src/digitalmodel/workflows/workflow_automation/workflows.py` | `MooringSystemDesignWorkflow.create` | `independent` | [docs/registry/implementation-map.yaml:L16459-L16468] |
| 105 | `tests/contracts/test_force_units_boundary_contract.py` | `<module>` | `test_helper` | [docs/registry/implementation-map.yaml:L16469-L16477] |
| 106 | `tests/legal/test_published_pages_have_no_internal_paths.py` | `_load_producer` | `test_helper` | [docs/registry/implementation-map.yaml:L16478-L16487] |
| 107 | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `sample_database_path` | `test_helper` | [docs/registry/implementation-map.yaml:L16488-L16497] |
| 108 | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `ocimf_db` | `test_helper` | [docs/registry/implementation-map.yaml:L16498-L16507] |
| 109 | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `env_forces` | `test_helper` | [docs/registry/implementation-map.yaml:L16508-L16517] |
| 110 | `tests/marine_ops/marine_engineering/integration/integration_example.py` | `main` | `test_helper` | [docs/registry/implementation-map.yaml:L16518-L16528] |
| 111 | `tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py` | `TestOCIMFMooringIntegration.test_combined_environmental_forces` | `test_helper` | [docs/registry/implementation-map.yaml:L16529-L16538] |
| 112 | `tests/marine_ops/marine_engineering/test_marine_eng_perf.py` | `ocimf_database` | `test_helper` | [docs/registry/implementation-map.yaml:L16539-L16549] |
| 113 | `tests/marine_ops/marine_engineering/test_marine_eng_perf.py` | `pytest_benchmark_update_json` | `test_helper` | [docs/registry/implementation-map.yaml:L16550-L16559] |
| 114 | `tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py` | `test_refactored_polar_signatures_match_fixture` | `test_helper` | [docs/registry/implementation-map.yaml:L16560-L16569] |
| 115 | `tests/solvers/orcaflex/reporting/fixture_helpers.py` | `_build_loads` | `test_helper` | [docs/registry/implementation-map.yaml:L16570-L16579] |
| 116 | `tests/structural/parachute/test_chute_assessment.py` | `TestAssessAllLoadCases.test_governing_force_within_1_percent_of_hand_calc` | `test_helper` | [docs/registry/implementation-map.yaml:L16580-L16589] |
| 117 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_drag_200_mph` | `test_helper` | [docs/registry/implementation-map.yaml:L16590-L16600] |
| 118 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_drag_250_mph` | `test_helper` | [docs/registry/implementation-map.yaml:L16601-L16611] |
| 119 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_force_scales_with_v_squared` | `test_helper` | [docs/registry/implementation-map.yaml:L16612-L16622] |
| 120 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_result_has_newtons` | `test_helper` | [docs/registry/implementation-map.yaml:L16623-L16633] |
| 121 | `tests/structural/parachute/test_parachute_drag.py` | `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_newtons` | `test_helper` | [docs/registry/implementation-map.yaml:L16634-L16644] |
| 122 | `tests/structural/parachute/test_parachute_drag.py` | `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_lbs` | `test_helper` | [docs/registry/implementation-map.yaml:L16645-L16655] |
| 123 | `tests/subsea/catenary_riser/conftest.py` | `catenary_environment` | `test_helper` | [docs/registry/implementation-map.yaml:L16656-L16665] |
| 124 | `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py` | `TestLiftForce.test_known_lift_force` | `test_helper` | [docs/registry/implementation-map.yaml:L16666-L16675] |
| 125 | `tests/subsea/pipeline/test_on_bottom_stability.py` | `TestDragForce.test_scenario_1` | `test_helper` | [docs/registry/implementation-map.yaml:L16676-L16685] |
| 126 | `tests/subsea/pipeline/test_on_bottom_stability.py` | `TestLiftForce.test_scenario_1` | `test_helper` | [docs/registry/implementation-map.yaml:L16686-L16695] |
| 127 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_dnv_circular_mock` | `test_helper` | [docs/registry/implementation-map.yaml:L16696-L16705] |
| 128 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_dnv_rectangular_mock` | `test_helper` | [docs/registry/implementation-map.yaml:L16706-L16715] |
| 129 | `tests/test_on_bottom_stability.py` | `TestHydrodynamicLoads.test_drag_force_per_meter` | `test_helper` | [docs/registry/implementation-map.yaml:L16716-L16725] |
| 130 | `tests/test_on_bottom_stability.py` | `TestHydrodynamicLoads.test_lift_force_per_meter` | `test_helper` | [docs/registry/implementation-map.yaml:L16726-L16735] |
| 131 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | `test_helper` | [docs/registry/implementation-map.yaml:L16736-L16747] |
| 132 | `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py` | `hydrodynamic_force_per_meter` | `independent` | [docs/registry/implementation-map.yaml:L16748-L16758] |
| 133 | `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py` | `lift_force_per_meter` | `independent` | [docs/registry/implementation-map.yaml:L16759-L16769] |
| 134 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `lift_force_per_meter` | `independent` | [docs/registry/implementation-map.yaml:L16770-L16781] |
| 135 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `inertia_force_per_meter` | `independent` | [docs/registry/implementation-map.yaml:L16782-L16791] |



## 10. Coverage and open items

### Files and documentation read

The package has seven Python files plus its manifest; each implementation has been read without importing a licensed interface. [src/digitalmodel/subsea/on_bottom_stability/__init__.py:L1-L91] [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L1-L317] [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L1-L429] [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L1-L346] [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L1-L175] [src/digitalmodel/subsea/on_bottom_stability/e305_workflow.py:L1-L164] [src/digitalmodel/subsea/on_bottom_stability/e305_fixture.py:L1-L178] [src/digitalmodel/subsea/on_bottom_stability/manifest.yaml:L1-L90]
The module tests and synthetic table builder were read; `conftest.py` defines no shared module fixture. [tests/subsea/on_bottom_stability/conftest.py:L1-L2] [tests/subsea/on_bottom_stability/synthetic_e305_tables.py:L1-L27]
The E305 example README and input/run route were read as supporting workflow evidence. [examples/workflows/on-bottom-stability-e305/README.md:L1-L123] [tests/subsea/on_bottom_stability/test_e305_workflow.py:L24-L69]

| Documentation discovered/read | Relevant pinned evidence |
|---|---|
| docs/domains/asset-integrity/ffs-offering-catalog.md | Design-screen classification, not FFS acceptance. [docs/domains/asset-integrity/ffs-offering-catalog.md:L150-L150] |
| docs/domains/pipeline-installation/coverage-2026-10.md | Function and source-standard inventory; native qualification not established. [docs/domains/pipeline-installation/coverage-2026-10.md:L196-L234] |
| docs/domains/pipeline-installation/slay-campaign/analysis.html | On-bottom stability is outside that prepared campaign. [docs/domains/pipeline-installation/slay-campaign/analysis.html:L45-L45] |
| docs/domains/pipelines/_design.md; docs/domains/pipelines/_subsea_pipelines.md | Older recorded edition, not a reconciled basis. [docs/domains/pipelines/_design.md:L10-L10] [docs/domains/pipelines/_subsea_pipelines.md:L41-L41] |
| docs/field-development/subsea-production-systems-mapping.md | Module-to-standard mapping. [docs/field-development/subsea-production-systems-mapping.md:L83-L83] |
| docs/maps/digitalmodel-operator-map.md; docs/registry/module-routing.yaml | Parent-domain scope and maturity. [docs/maps/digitalmodel-operator-map.md:L69-L69] [docs/registry/module-routing.yaml:L351-L363] |
| docs/registry/implementation-map.yaml; docs/registry/implementation-map.md | Code-pin map and full family inventories in section 9; the prose map lists the same family names. [docs/registry/implementation-map.md:L23-L28] |
| docs/registry/workflows.yaml | F109 and E305 workflow registration; no identifying example values reproduced. [docs/registry/workflows.yaml:L476-L482] [docs/registry/workflows.yaml:L1325-L1339] |
| docs/research/subsea-pipelines-academic-references.md | Bibliographic leads only, not independent expected-value evidence read for this pack. [docs/research/subsea-pipelines-academic-references.md:L430-L480] |
| docs/research/subsea-pipelines-industry-practice.md; its .sources.yaml companion | Industry-method leads, not a qualification basis. [docs/research/subsea-pipelines-industry-practice.md:L22-L23] [docs/research/subsea-pipelines-industry-practice.sources.yaml:L51-L54] |
| docs/standards/subsea-pipelines-inventory.yaml | Conflicting recorded current edition/title. [docs/standards/subsea-pipelines-inventory.yaml:L643-L678] |
| docs/vision/CALCULATIONS-VISION.md | Historical documentation claim: 14 tests; that figure is not used as the recounted package total. [docs/vision/CALCULATIONS-VISION.md:L139-L142] |
| docs/archive/archive-blob-map-2026-10-08.csv; docs/archive/archive-candidates-2026-10-08.csv | Report-path discovery only, no report values treated as validation. [docs/archive/archive-blob-map-2026-10-08.csv:L1272-L1275] [docs/archive/archive-candidates-2026-10-08.csv:L1272-L1275] |

UNVERIFIED: U4 — two discovery paths containing identifying or restricted wording are withheld from this public inventory; issue pending (3).
A hash-only E305 substring match was discarded as a documentation-search false positive.
The calculation-citation contract was readable: this pack adds no calculation constant or sidecar; existing F109 sidecar gaps remain #1201, and E305 resolution is exercised by C08. [src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py:L36-L50] [src/digitalmodel/subsea/on_bottom_stability/e305_tables.py:L102-L110] [tests/subsea/on_bottom_stability/test_e305_tables.py:L31-L44]

### Test commands and results

All results below use the header code pin; AFTER uses the uncommitted documentation tree with unchanged calculation/test code.
The common form is `PYTHONPATH=src uv run --no-sources --extra test python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-<run>`; offline reruns use `uv run --no-sync` after installing the declared test extra into a scratch environment.
The registered workflow tests write outputs and are rerun in a disposable copy of the tracked inputs/tests, using the unchanged source at the header code pin; no solver or licence is required.
No `addopts` override, test deselection or test changes were used.

| Selection | BASE | AFTER | Limits / issue |
|---|---|---|---|
| `tests/subsea/on_bottom_stability tests/test_on_bottom_stability.py tests/geotechnical/test_geotechnical.py` | 94 collected; 93 passed; 0 failed; 1 skipped | 94 collected; 93 passed; 0 failed; 1 skipped | Private fixture skipped, recorded NOT_RUN; issue pending (1). [tests/subsea/on_bottom_stability/test_e305_fixture.py:L101-L113] |
| `tests/contracts tests/docs/test_digitalmodel_routing_contract.py` | 494 passed; 0 failed; 0 skipped | 494 passed; 0 failed; 0 skipped | Contract suite plus routing checks, including unchanged top-level domains. [tests/docs/test_digitalmodel_routing_contract.py:L94-L112] |
| `tests/subsea/pipeline/test_on_bottom_stability.py` | 0 run; 1 collection error; 16 statically defined nodes NOT_RUN | 0 run; 1 identical collection error; 16 nodes NOT_RUN | Missing post-move import path; #1923. [tests/subsea/pipeline/test_on_bottom_stability.py:L17-L32] |
| `tests/workflows/test_durable_workflows.py::test_workflow_registry[on-bottom-stability-f109]` and `tests/workflows/test_durable_workflows.py::test_workflow_registry[on-bottom-stability-e305]` | 2 passed; 0 failed; 0 skipped | 2 passed; 0 failed; 0 skipped | Both registered basenames run offline through the shared engine; no solver binary or licence required. [tests/workflows/test_durable_workflows.py:L57-L90] [tests/workflows/test_durable_workflows.py:L793-L822] |

C02 export probe: `PYTHONPATH=src uv run --no-sync python -c 'import digitalmodel.subsea.on_bottom_stability as p; missing = [n for n in p.__all__ if not hasattr(p, n)]; print(len(p.__all__), len(missing)); assert len(p.__all__) == 26 and not missing'`. BASE and AFTER each report 26 exported names, 0 missing; import completes without error. [src/digitalmodel/subsea/on_bottom_stability/__init__.py:L25-L91]

Focused node results below match between BASE and AFTER; the private fixture remains NOT_RUN in both.

| Existing test node | Offline status | Test definition |
|---|---|---|
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestHydrodynamicForce::test_zero_velocity_and_acceleration_gives_zero` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L35-L40] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestHydrodynamicForce::test_pure_drag_force` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L42-L52] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestHydrodynamicForce::test_pure_inertia_force` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L54-L64] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestHydrodynamicForce::test_negative_velocity_reverses_drag` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L66-L74] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestHydrodynamicForce::test_combined_drag_and_inertia` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L76-L84] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestLiftForce::test_zero_velocity_gives_zero_lift` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L96-L101] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestLiftForce::test_known_lift_force` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L103-L110] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestLiftForce::test_lift_always_positive` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L112-L117] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestSubmergedWeight::test_positive_submerged_weight_for_steel_pipe` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L129-L140] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestSubmergedWeight::test_empty_thin_wall_pipe_can_be_buoyant` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L142-L154] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestSubmergedWeight::test_weight_increases_with_heavier_contents` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L156-L168] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestAbsoluteStability::test_stable_pipe` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L180-L192] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestAbsoluteStability::test_unstable_pipe` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L194-L204] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestAbsoluteStability::test_zero_submerged_weight_gives_infinite_utilisation` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L206-L215] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestAbsoluteStability::test_negative_submerged_weight_is_unstable` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L217-L225] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestAbsoluteStability::test_custom_gamma_sc` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L227-L236] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestGeneralizedStability::test_stable_with_soil_resistance` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L248-L260] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestGeneralizedStability::test_unstable_without_soil_resistance` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L262-L273] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestGeneralizedStability::test_zero_capacity_gives_infinite_utilisation` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L275-L286] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestGeneralizedStability::test_lift_exceeding_weight_is_unstable` | PASS | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L288-L297] |
| `tests/subsea/on_bottom_stability/test_e305_fixture.py::test_synthetic_workbook_conversion_is_external_cached_and_reproducible` | PASS | [tests/subsea/on_bottom_stability/test_e305_fixture.py:L52-L89] |
| `tests/subsea/on_bottom_stability/test_e305_fixture.py::test_present_malformed_private_fixture_is_not_a_skip` | PASS | [tests/subsea/on_bottom_stability/test_e305_fixture.py:L92-L98] |
| `tests/subsea/on_bottom_stability/test_e305_inverse.py::test_inverse_concrete_thickness_recomputes_diameter_and_forward_margin` | PASS | [tests/subsea/on_bottom_stability/test_e305_inverse.py:L13-L48] |
| `tests/subsea/on_bottom_stability/test_e305_kinematics.py::test_diameter_average_is_an_integral_not_a_centre_value` | PASS | [tests/subsea/on_bottom_stability/test_e305_kinematics.py:L11-L21] |
| `tests/subsea/on_bottom_stability/test_e305_kinematics.py::test_g_is_not_silently_changed_by_the_spectral_helper` | PASS | [tests/subsea/on_bottom_stability/test_e305_kinematics.py:L24-L32] |
| `tests/subsea/on_bottom_stability/test_e305_parity.py::test_current_counted_once_or_explicitly_duplicated` | PASS | [tests/subsea/on_bottom_stability/test_e305_parity.py:L29-L38] |
| `tests/subsea/on_bottom_stability/test_e305_parity.py::test_single_wave_instant_and_simultaneous_phase_scan` | PASS | [tests/subsea/on_bottom_stability/test_e305_parity.py:L41-L51] |
| `tests/subsea/on_bottom_stability/test_e305_parity.py::test_typed_fw_is_used_and_reported_without_a_table_lookup` | PASS | [tests/subsea/on_bottom_stability/test_e305_parity.py:L54-L59] |
| `tests/subsea/on_bottom_stability/test_e305_parity.py::test_wave_2_period_is_independent` | PASS | [tests/subsea/on_bottom_stability/test_e305_parity.py:L62-L70] |
| `tests/subsea/on_bottom_stability/test_e305_parity.py::test_reduction_convention_and_dimensionless_ratios_are_explicit` | PASS | [tests/subsea/on_bottom_stability/test_e305_parity.py:L73-L82] |
| `tests/subsea/on_bottom_stability/test_e305_parity.py::test_parity_rejects_invalid_inputs[changes0]` | PASS | [tests/subsea/on_bottom_stability/test_e305_parity.py:L88-L90] |
| `tests/subsea/on_bottom_stability/test_e305_parity.py::test_parity_rejects_invalid_inputs[changes1]` | PASS | [tests/subsea/on_bottom_stability/test_e305_parity.py:L88-L90] |
| `tests/subsea/on_bottom_stability/test_e305_parity.py::test_parity_rejects_invalid_inputs[changes2]` | PASS | [tests/subsea/on_bottom_stability/test_e305_parity.py:L88-L90] |
| `tests/subsea/on_bottom_stability/test_e305_parity.py::test_parity_rejects_invalid_inputs[changes3]` | PASS | [tests/subsea/on_bottom_stability/test_e305_parity.py:L88-L90] |
| `tests/subsea/on_bottom_stability/test_e305_parity.py::test_parity_rejects_invalid_inputs[changes4]` | PASS | [tests/subsea/on_bottom_stability/test_e305_parity.py:L88-L90] |
| `tests/subsea/on_bottom_stability/test_e305_parity.py::test_parity_rejects_invalid_inputs[changes5]` | PASS | [tests/subsea/on_bottom_stability/test_e305_parity.py:L88-L90] |
| `tests/subsea/on_bottom_stability/test_e305_parity.py::test_parity_rejects_invalid_inputs[changes6]` | PASS | [tests/subsea/on_bottom_stability/test_e305_parity.py:L88-L90] |
| `tests/subsea/on_bottom_stability/test_e305_tables.py::test_synthetic_tables_present_interpolation_and_citation_sidecar` | PASS | [tests/subsea/on_bottom_stability/test_e305_tables.py:L15-L44] |
| `tests/subsea/on_bottom_stability/test_e305_tables.py::test_tables_absent_fail_closed` | PASS | [tests/subsea/on_bottom_stability/test_e305_tables.py:L47-L54] |
| `tests/subsea/on_bottom_stability/test_e305_tables.py::test_malformed_synthetic_tables_fail_closed[missing_table]` | PASS | [tests/subsea/on_bottom_stability/test_e305_tables.py:L60-L95] |
| `tests/subsea/on_bottom_stability/test_e305_tables.py::test_malformed_synthetic_tables_fail_closed[unsorted_axis]` | PASS | [tests/subsea/on_bottom_stability/test_e305_tables.py:L60-L95] |
| `tests/subsea/on_bottom_stability/test_e305_tables.py::test_malformed_synthetic_tables_fail_closed[nonfinite]` | PASS | [tests/subsea/on_bottom_stability/test_e305_tables.py:L60-L95] |
| `tests/subsea/on_bottom_stability/test_e305_tables.py::test_malformed_synthetic_tables_fail_closed[missing_citation]` | PASS | [tests/subsea/on_bottom_stability/test_e305_tables.py:L60-L95] |
| `tests/subsea/on_bottom_stability/test_e305_tables.py::test_malformed_synthetic_tables_fail_closed[wrong_units]` | PASS | [tests/subsea/on_bottom_stability/test_e305_tables.py:L60-L95] |
| `tests/subsea/on_bottom_stability/test_e305_tables.py::test_malformed_synthetic_tables_fail_closed[ragged]` | PASS | [tests/subsea/on_bottom_stability/test_e305_tables.py:L60-L95] |
| `tests/subsea/on_bottom_stability/test_e305_tables.py::test_malformed_synthetic_tables_fail_closed[duplicate_axis]` | PASS | [tests/subsea/on_bottom_stability/test_e305_tables.py:L60-L95] |
| `tests/subsea/on_bottom_stability/test_e305_tables.py::test_malformed_synthetic_tables_fail_closed[citation_mismatch]` | PASS | [tests/subsea/on_bottom_stability/test_e305_tables.py:L60-L95] |
| `tests/subsea/on_bottom_stability/test_e305_tables.py::test_malformed_synthetic_tables_fail_closed[duplicate_key]` | PASS | [tests/subsea/on_bottom_stability/test_e305_tables.py:L60-L95] |
| `tests/subsea/on_bottom_stability/test_e305_weight.py::test_weight_build_up_synthetic_annuli_and_gravity` | PASS | [tests/subsea/on_bottom_stability/test_e305_weight.py:L11-L39] |
| `tests/subsea/on_bottom_stability/test_e305_workflow.py::test_e305_registration_and_registered_batch_are_reproducible` | PASS | [tests/subsea/on_bottom_stability/test_e305_workflow.py:L31-L70] |
| `tests/subsea/on_bottom_stability/test_e305_workflow.py::test_e305_batch_requires_two_roles_and_never_falls_back_from_tables` | PASS | [tests/subsea/on_bottom_stability/test_e305_workflow.py:L73-L85] |
| `tests/subsea/on_bottom_stability/test_e305_workflow.py::test_e305_table_batch_retains_four_lookup_sidecars` | PASS | [tests/subsea/on_bottom_stability/test_e305_workflow.py:L88-L108] |
| `tests/subsea/on_bottom_stability/test_e305_workflow.py::test_e305_sea_state_workflow_uses_spectral_kinematics` | PASS | [tests/subsea/on_bottom_stability/test_e305_workflow.py:L130-L147] |
| `tests/subsea/on_bottom_stability/test_e305_workflow.py::test_e305_zero_wave_tables_require_defined_M` | PASS | [tests/subsea/on_bottom_stability/test_e305_workflow.py:L150-L156] |
| `tests/subsea/on_bottom_stability/test_e305_workflow.py::test_e305_tables_accept_typed_current_and_cite_only_used_tables` | PASS | [tests/subsea/on_bottom_stability/test_e305_workflow.py:L159-L171] |
| `tests/subsea/on_bottom_stability/test_e305_workflow.py::test_e305_tables_accept_typed_mu_and_cite_only_used_tables` | PASS | [tests/subsea/on_bottom_stability/test_e305_workflow.py:L174-L185] |
| `tests/subsea/on_bottom_stability/test_e305_workflow.py::test_e305_tables_name_missing_R_coordinates` | PASS | [tests/subsea/on_bottom_stability/test_e305_workflow.py:L188-L194] |
| `tests/subsea/on_bottom_stability/test_e305_workflow.py::test_e305_relative_output_requires_config_directory` | PASS | [tests/subsea/on_bottom_stability/test_e305_workflow.py:L197-L208] |
| `tests/subsea/on_bottom_stability/test_e305_workflow.py::test_e305_outputs_state_gamma_is_recorded_only` | PASS | [tests/subsea/on_bottom_stability/test_e305_workflow.py:L211-L227] |
| `tests/test_on_bottom_stability.py::TestSubmergedWeight::test_concrete_coated_pipe` | PASS | [tests/test_on_bottom_stability.py:L9-L22] |
| `tests/test_on_bottom_stability.py::TestSubmergedWeight::test_empty_pipe_may_float` | PASS | [tests/test_on_bottom_stability.py:L24-L35] |
| `tests/test_on_bottom_stability.py::TestHydrodynamicLoads::test_drag_and_inertia_forces` | PASS | [tests/test_on_bottom_stability.py:L39-L52] |
| `tests/test_on_bottom_stability.py::TestHydrodynamicLoads::test_drag_force_per_meter` | PASS | [tests/test_on_bottom_stability.py:L54-L66] |
| `tests/test_on_bottom_stability.py::TestHydrodynamicLoads::test_lift_force_per_meter` | PASS | [tests/test_on_bottom_stability.py:L68-L80] |
| `tests/test_on_bottom_stability.py::TestHydrodynamicLoads::test_inertia_force_per_meter` | PASS | [tests/test_on_bottom_stability.py:L82-L94] |
| `tests/test_on_bottom_stability.py::TestLateralStability::test_stable_pipe` | PASS | [tests/test_on_bottom_stability.py:L98-L112] |
| `tests/test_on_bottom_stability.py::TestLateralStability::test_unstable_pipe` | PASS | [tests/test_on_bottom_stability.py:L114-L127] |
| `tests/test_on_bottom_stability.py::TestLateralStability::test_zero_weight_raises` | PASS | [tests/test_on_bottom_stability.py:L129-L140] |
| `tests/test_on_bottom_stability.py::TestLateralStability::test_check_lateral_stability_passes` | PASS | [tests/test_on_bottom_stability.py:L142-L156] |
| `tests/test_on_bottom_stability.py::TestLateralStability::test_check_lateral_stability_fails` | PASS | [tests/test_on_bottom_stability.py:L158-L171] |
| `tests/test_on_bottom_stability.py::TestVerticalStability::test_vertical_stability_passes` | PASS | [tests/test_on_bottom_stability.py:L175-L185] |
| `tests/test_on_bottom_stability.py::TestVerticalStability::test_vertical_stability_fails` | PASS | [tests/test_on_bottom_stability.py:L187-L196] |
| `tests/test_on_bottom_stability.py::TestVerticalStability::test_zero_weight_raises` | PASS | [tests/test_on_bottom_stability.py:L198-L207] |
| `tests/geotechnical/test_geotechnical.py::TestPackageImport::test_import_package` | PASS | [tests/geotechnical/test_geotechnical.py:L21-L23] |
| `tests/geotechnical/test_geotechnical.py::TestPackageImport::test_import_scour` | PASS | [tests/geotechnical/test_geotechnical.py:L25-L28] |
| `tests/geotechnical/test_geotechnical.py::TestPackageImport::test_import_piles` | PASS | [tests/geotechnical/test_geotechnical.py:L30-L33] |
| `tests/geotechnical/test_geotechnical.py::TestPackageImport::test_import_anchors` | PASS | [tests/geotechnical/test_geotechnical.py:L35-L38] |
| `tests/geotechnical/test_geotechnical.py::TestPackageImport::test_import_on_bottom_stability` | PASS | [tests/geotechnical/test_geotechnical.py:L40-L44] |
| `tests/geotechnical/test_geotechnical.py::TestAnchors::test_drag_anchor_soft_clay` | PASS | [tests/geotechnical/test_geotechnical.py:L50-L57] |
| `tests/geotechnical/test_geotechnical.py::TestAnchors::test_drag_anchor_sand` | PASS | [tests/geotechnical/test_geotechnical.py:L59-L64] |
| `tests/geotechnical/test_geotechnical.py::TestAnchors::test_drag_anchor_invalid_soil_type` | PASS | [tests/geotechnical/test_geotechnical.py:L66-L69] |
| `tests/geotechnical/test_geotechnical.py::TestAnchors::test_drag_anchor_invalid_anchor_type` | PASS | [tests/geotechnical/test_geotechnical.py:L71-L74] |
| `tests/geotechnical/test_geotechnical.py::TestAnchors::test_drag_anchor_zero_weight_raises` | PASS | [tests/geotechnical/test_geotechnical.py:L76-L79] |
| `tests/geotechnical/test_geotechnical.py::TestAnchors::test_suction_anchor_positive_capacity` | PASS | [tests/geotechnical/test_geotechnical.py:L81-L87] |
| `tests/geotechnical/test_geotechnical.py::TestAnchors::test_suction_anchor_components_sum` | PASS | [tests/geotechnical/test_geotechnical.py:L89-L94] |
| `tests/geotechnical/test_geotechnical.py::TestAnchors::test_suction_anchor_zero_diameter_raises` | PASS | [tests/geotechnical/test_geotechnical.py:L96-L99] |
| `tests/geotechnical/test_geotechnical.py::TestOnBottomStability::test_submerged_weight_steel_pipe` | PASS | [tests/geotechnical/test_geotechnical.py:L105-L118] |
| `tests/geotechnical/test_geotechnical.py::TestOnBottomStability::test_hydrodynamic_loads_positive` | PASS | [tests/geotechnical/test_geotechnical.py:L120-L135] |
| `tests/geotechnical/test_geotechnical.py::TestOnBottomStability::test_lateral_stability_stable_pipe` | PASS | [tests/geotechnical/test_geotechnical.py:L137-L147] |
| `tests/geotechnical/test_geotechnical.py::TestOnBottomStability::test_lateral_stability_unstable_pipe` | PASS | [tests/geotechnical/test_geotechnical.py:L149-L159] |
| `tests/geotechnical/test_geotechnical.py::TestOnBottomStability::test_lateral_stability_zero_weight_raises` | PASS | [tests/geotechnical/test_geotechnical.py:L161-L170] |
| `tests/geotechnical/test_geotechnical.py::TestScour::test_pipeline_scour_positive` | PASS | [tests/geotechnical/test_geotechnical.py:L176-L186] |
| `tests/geotechnical/test_geotechnical.py::TestScour::test_monopile_scour_positive` | PASS | [tests/geotechnical/test_geotechnical.py:L188-L194] |
| `tests/subsea/on_bottom_stability/test_e305_fixture.py::test_private_reference_fixture_both_cases_g_9_81_within_0_1_percent` | NOT_RUN: private fixture absent (raw SKIP) | [tests/subsea/on_bottom_stability/test_e305_fixture.py:L101-L113] |

### Open-item register

The maintainer is the proposed steward of each item until the engineering calculations quality review assigns it.
A7 remains conditional on issue creation for the numbered drafts; these references do not assert that issues have been opened.

| Item | Scope / disposition | Tracking |
|---|---|---|
| U1 | Establish licensed E305 edition/profile/demand equivalence and external interpolation suitability | issue pending (1) |
| U2 | Establish the DNV-OS-F101 (*Submarine pipeline systems*; edition not established) comparator basis and independent private-workbook parity; sidecar entry: to be created; no private data read or reproduced | issue pending (1) |
| U3 | Appoint review/deputy assignments and shared-log references | issue pending (4) |
| U4 | Codename the two withheld discovery-document paths before adding public references | issue pending (3) |
| Edition/clauses | Reconcile F109 editions and clause titles before using the pack as a design-standard reference | #1813 |
| Provenance | Version and cite inline F109 coefficients/factors and their output sidecars | #1201 |
| Input/sweep coverage | Resolve section-6 unchecked domains, direction, defaults, sentinel branches and numerical-resolution coverage | issue pending (2) |
| Test migration | Migrate the 16 uncollectable legacy nodes to the current API | #1923 |
| Engine route | F109/E305 registered basenames both PASS offline; no solver or licence prerequisite applies to these two nodes | Resolved by the section-10 BASE/AFTER runs |
| Physical scope | Distinguish scalar stability checks from dynamic lateral displacement and physically derived soil response | #1812, #1835, #1838 |
| Inventory follow-up | Reconcile dependency inventory-row IDs, external duplicates and additional method families named in section 9 | #1817, #1818 |
| Independent standards case | Add a traceable licensed-standard or published benchmark case; the three validated cases here establish synthetic closed-form arithmetic only | #242 |
