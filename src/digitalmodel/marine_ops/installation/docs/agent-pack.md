# Installation module knowledge pack

| Header | Value |
| --- | --- |
| Module | `marine_ops/installation` [src/digitalmodel/marine_ops/installation/manifest.yaml:L1-L7] |
| Repository | `digitalmodel` |
| Path | `src/digitalmodel/marine_ops/installation/docs/agent-pack.md` |
| Pack revision | 0 |
| Code revision | `a1be651234e5541809cf1f33d57fce04d1a2ae5b` |
| Analysis family | Installation; AQG r0, family mapping, section 10 |
| Quality manager role | Installation quality manager; assignment pending in section 10 |
| Status | DRAFT |

## 1. Purpose and scope

The main calculation path transfers six motion RAOs to a crane tip, calculates splash-zone forces and hook loads, searches motion-based sea-state limits, and ranks candidates by operability. [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L45-L107] [src/digitalmodel/marine_ops/installation/splash_zone.py:L81-L156] [src/digitalmodel/marine_ops/installation/operability.py:L78-L187] [src/digitalmodel/marine_ops/installation/vessel_screening.py:L45-L127]

The registry records beta for the separate `digitalmodel/installation` package; that entry does not establish maturity of `marine_ops/installation`. [docs/registry/module-routing.yaml:L351-L356]

`compute_operability` accepts a structure but uses only crane-tip heave and velocity in the boundary calculation; hook-load, tilt, submerged weight and lowering-stage criteria do not enter that calculation. [src/digitalmodel/marine_ops/installation/operability.py:L108-L187]

The splash model uses bounding-box displacement and a half-submerged approximation; it is not a geometry-resolved flooded-frame model. [src/digitalmodel/marine_ops/installation/models.py:L85-L89] [src/digitalmodel/marine_ops/installation/splash_zone.py:L123-L137]

The jumper path calculates properties, decision criteria and line-section outputs; the executed pipeline ends after writing outputs even though its signature exposes solver and report flags. [src/digitalmodel/marine_ops/installation/jumper_installation.py:L333-L399]

Suitability rankings carry confidence and provenance, and the report explicitly limits their use to screening. [src/digitalmodel/marine_ops/installation/vessel_suitability.py:L68-L107] [src/digitalmodel/marine_ops/installation/suitability_report.py:L35-L40]

UNVERIFIED: the registry does not establish maturity for this module at the pin; the inventory beta classification is unverified for this distinct route (issue pending (5)).

## 2. Inputs and outputs

### Inputs

| Name | Unit and convention | Allowed range and default | Source |
| --- | --- | --- | --- |
| Structure dimensions, air mass, CoG | m, kg; CoG in structure local frame | Required dimensions/mass; CoG defaults to origin; dataclass has no physical range checks | [src/digitalmodel/marine_ops/installation/models.py:L36-L89] |
| Projected slamming area; hydrodynamic coefficients | m²; dimensionless | Area defaults to length × width; coefficient defaults are code assumptions whose numerical values are withheld here pending source qualification | [src/digitalmodel/marine_ops/installation/models.py:L54-L78] |
| Crane tip x, y, z | m from vessel CoG; forward, port, up positive | Required offsets; no imposed range | [src/digitalmodel/marine_ops/installation/models.py:L92-L116] |
| Motion RAOs | rad/s grid; headings deg; arrays `(n_frequency, n_heading)`; phases deg; translational m/m and rotational deg/m at the transfer interface | All six DOF keys required; missing keys raise ValueError | [src/digitalmodel/marine_ops/installation/models.py:L142-L181] [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L28-L42] [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L69-L96] |
| Crane curve | radius m, capacity te | Required arrays; interpolation clamps beyond endpoints through `np.interp` | [src/digitalmodel/marine_ops/installation/models.py:L119-L139] |
| Installation criteria | heave m, velocity m/s, hook factor dimensionless, tilt deg, operational multiplier dimensionless | Constructor defaults exist; they are not qualified acceptance limits | [src/digitalmodel/marine_ops/installation/models.py:L184-L206] |
| Installation case | Hs m; Tp s; heading deg; current m/s; depth m; density kg/m³ | Structure, vessel, criteria, Hs and Tp required; heading/current default zero; depth/density have constructor defaults | [src/digitalmodel/marine_ops/installation/models.py:L264-L280] |
| Lowering and tip velocities | m/s; wave surface velocity upward positive | Lowering velocity optional; tip velocity required for splash assessment | [src/digitalmodel/marine_ops/installation/splash_zone.py:L17-L46] [src/digitalmodel/marine_ops/installation/splash_zone.py:L81-L103] |
| Period range, heading and Hs search | s, deg, m | Nearest heading; default search 0.1–5.0 m at 0.1 m increments, stops at first exceedance | [src/digitalmodel/marine_ops/installation/operability.py:L31-L75] [src/digitalmodel/marine_ops/installation/operability.py:L108-L115] |
| Scatter diagram | Hs m, Tp s, occurrence counts | Arrays `(n_hs,)`, `(n_tp,)`, `(n_hs,n_tp)`; empty total returns zero | [src/digitalmodel/marine_ops/installation/operability.py:L190-L235] |
| Lift requirement | te, radius m, DAF dimensionless, optional DP class and deck m² | Weight and radius required; optional gates evaluated when supplied | [src/digitalmodel/marine_ops/installation/vessel_suitability.py:L35-L43] [src/digitalmodel/marine_ops/installation/vessel_suitability.py:L68-L107] |
| Jumper YAML environment and pipe | Water depth m and density te/m³; Hs m, period s, direction deg from-convention; pipe m, material kg/m³ | Positive depth/density/pipe sizes; nonnegative Hs; positive period; extra fields ignored by schema | [src/digitalmodel/marine_ops/installation/jumper_installation_schema.py:L61-L141] [src/digitalmodel/marine_ops/installation/jumper_installation_schema.py:L213-L245] |
| Jumper configuration and crane YAML | Segment lengths in inches; connector/clamp masses kg; crane radius m, SWL te, DDF dimensionless | Schema bounds positive lengths and crane values; nonnegative counts/masses; configuration parser rejects unknown jumper keys | [src/digitalmodel/marine_ops/installation/jumper_installation_schema.py:L150-L205] [src/digitalmodel/marine_ops/installation/jumper_installation.py:L126-L156] |

The transfer represents a complex RAO as amplitude × exp(i × phase); rotational amplitudes are always converted from deg/m, so a rad/m input needs conversion before this interface. [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L28-L42] [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L76-L96]

UNVERIFIED: physical heading zero, rotation sense and time-phase convention are not established by the array contract; do not equate the structure, sensor and RAO frames without a sourced transformation (issue pending (1)). [src/digitalmodel/marine_ops/installation/models.py:L142-L181] [src/digitalmodel/marine_ops/installation/realtime_feedback.py:L19-L47]

### Outputs

| Name | Unit, format and interpretation | Source |
| --- | --- | --- |
| Tip RAOs | surge/sway/heave amplitudes and phases; same grid shape, phase deg | [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L98-L107] |
| Significant motion/velocity | m and m/s; implemented as 2√m0 and 2√m2 | [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L143-L168] |
| Splash result | Slamming, varying buoyancy, combined force, min/max hook loads in N; DAF dimensionless; intermediate values and snap-risk Boolean | [src/digitalmodel/marine_ops/installation/models.py:L233-L261] [src/digitalmodel/marine_ops/installation/splash_zone.py:L139-L156] |
| Operability | `hs_limit_m` m; despite its name `tp_limits_s` contains Hs limits in m, while actual periods are in `details["tp_range_s"]`; percentage initially zero | [src/digitalmodel/marine_ops/installation/operability.py:L158-L187] [src/digitalmodel/marine_ops/installation/operability.py:L220-L235] |
| Screening | Hs matrix m, operability matrix %, governing strings, descending mean-metric rankings | [src/digitalmodel/marine_ops/installation/vessel_screening.py:L78-L127] |
| Suitability and report | Score, confidence, defensible flag, capacity te, static margin dimensionless; Markdown and HTML strings | [src/digitalmodel/marine_ops/installation/vessel_suitability.py:L46-L55] [src/digitalmodel/marine_ops/installation/suitability_report.py:L53-L110] [src/digitalmodel/marine_ops/installation/suitability_report.py:L121-L166] |
| Jumper pipeline files | `line_sections.yml` lengths m; decision TXT/JSON contains value, limit, unit, margin and reference; `analysis_summary.json` mass kg and length m | [src/digitalmodel/marine_ops/installation/jumper_lift.py:L1093-L1127] [src/digitalmodel/marine_ops/installation/jumper_installation.py:L255-L330] |
| Modular input | Dictionary for the modular generator; density te/m³, positions and section lengths m; converter carries environment values and assumed starting geometry | [src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py:L169-L206] [src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py:L209-L353] |
| MRU decisions | Samples in m/deg with timestamps; rolling statistics, decision history, phase and exceedance records | [src/digitalmodel/marine_ops/installation/realtime_feedback.py:L19-L103] [src/digitalmodel/marine_ops/installation/realtime_feedback.py:L105-L180] |

## 3. Method and governing standards

### Manifest clause index

The manifest identifies DNV-RP-H103, *Modelling and Analysis of Marine Operations*, edition 2011 as its primary standard; the following eight rows map its complete function list to implementation, not to an independently checked licensed clause. [src/digitalmodel/marine_ops/installation/manifest.yaml:L4-L56]

For every licensed-standard reference below, clause text not checked against the licensed copy; clause titles not present in the repository are not supplied here. [src/digitalmodel/marine_ops/installation/manifest.yaml:L4-L56] [src/digitalmodel/marine_ops/installation/models.py:L6-L8]

| Manifest function | Clause label as recorded | Implementation and disposition | Evidence |
| --- | --- | --- | --- |
| `crane_tip_raos` | N/A geometric transfer; equation cites DNV-RP-C205 §7.2 | Complex geometric transfer; C205 edition/title provenance remains open in section 10 | [src/digitalmodel/marine_ops/installation/manifest.yaml:L9-L14] [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L69-L107] |
| `crane_tip_significant_motion` | N/A spectral method | Trapezoidal integration and 2√m0; amplitude label inconsistency remains open | [src/digitalmodel/marine_ops/installation/manifest.yaml:L15-L20] [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L118-L150] |
| `slamming_force` | S4.3.4 | Relative velocity is absolute lowering plus wave velocity; force scales with projected area and velocity squared | [src/digitalmodel/marine_ops/installation/manifest.yaml:L21-L26] [src/digitalmodel/marine_ops/installation/splash_zone.py:L17-L46] |
| `varying_buoyancy_force` | S4.4 | Waterplane area is length × width; force linear in wave amplitude | [src/digitalmodel/marine_ops/installation/manifest.yaml:L27-L32] [src/digitalmodel/marine_ops/installation/splash_zone.py:L49-L78] |
| `splash_zone_assessment` | S4 | Half-submerged static baseline, additive maximum and subtractive minimum hook load | [src/digitalmodel/marine_ops/installation/manifest.yaml:L33-L38] [src/digitalmodel/marine_ops/installation/splash_zone.py:L104-L156] |
| `compute_operability` | N/A engineering practice | Per-period minimum of heave and velocity limits; multiplier applied before global minimum | [src/digitalmodel/marine_ops/installation/manifest.yaml:L39-L44] [src/digitalmodel/marine_ops/installation/operability.py:L108-L187] |
| `weather_window_operability` | N/A statistical | Interpolate Hs boundary to Tp bins; count bins at or below boundary, divide by total counts | [src/digitalmodel/marine_ops/installation/manifest.yaml:L45-L50] [src/digitalmodel/marine_ops/installation/operability.py:L216-L235] |
| `screen_vessels` | N/A comparison | Cartesian candidate/structure loop; rank mean operability or mean Hs when no scatter is supplied | [src/digitalmodel/marine_ops/installation/manifest.yaml:L51-L56] [src/digitalmodel/marine_ops/installation/vessel_screening.py:L78-L127] |

The coded heave transfer is H_tip = H_heave − y H_roll + x H_pitch; surge and sway retain their own signed rotation terms, with phase combined before amplitude extraction. [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L76-L105]

The spectral path builds a JONSWAP/Pierson-Moskowitz spectrum, replaces nonfinite spectrum entries by zero, and integrates response variance; the motion docstring uses both single- and double-amplitude descriptions for the same result (issue pending (1)). [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L118-L150] [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L171-L201]

In the splash assessment the surface amplitude is Hs/2, peak frequency is 2π/Tp, and slamming uses lowering + tip + surface particle velocity; the varying buoyancy uses the rectangular waterplane. [src/digitalmodel/marine_ops/installation/splash_zone.py:L104-L121]

The minimum hook load excludes slamming and is the half-submerged static baseline minus varying buoyancy; `snap_risk` is true only when this minimum is negative, so zero tension is not flagged by that Boolean. [src/digitalmodel/marine_ops/installation/splash_zone.py:L123-L155]

The minimum-load comment cites DNV-RP-H103 (2011), *Modelling and Analysis of Marine Operations*, §4.7; clause text not checked against the licensed copy. [src/digitalmodel/marine_ops/installation/splash_zone.py:L129-L137]

DAF in `SplashZoneResult` is maximum hook load divided by air weight, with infinity for nonpositive air weight; this differs from a decision check that uses an embedded design factor rather than deriving dynamic response. [src/digitalmodel/marine_ops/installation/splash_zone.py:L136-L145] [src/digitalmodel/marine_ops/installation/go_no_go.py:L173-L176] [src/digitalmodel/marine_ops/installation/go_no_go.py:L246-L261]

The decision path cites H103 sections 4.2, 5.1, 5.4.1, 5.4.2, 5.5 and 5.5.2 and DNV-ST-N001, *Marine Operations and Marine Warranty*, edition 2021, section 5; these are source-code references, not established clause-title or criterion provenance (issue pending (1)). [src/digitalmodel/marine_ops/installation/go_no_go.py:L11-L13] [src/digitalmodel/marine_ops/installation/go_no_go.py:L178-L315]

The decision path gives NO_GO to any failed criterion, MARGINAL to warnings without failures and GO otherwise; omitted metocean conditions select a zero-wave fallback (issue pending (2)). [src/digitalmodel/marine_ops/installation/go_no_go.py:L292-L303] [src/digitalmodel/marine_ops/installation/go_no_go.py:L338-L346]

The jumper weight build-up sums insulated kits, configured buoyancy-module placements, strakes and clamps; its no-config route instead reproduces stored workbook assumptions and is a regression basis only. [src/digitalmodel/marine_ops/installation/jumper_lift.py:L853-L943]

The jumper converter uses smeared equivalent coatings and an assumed M-profile; its comments explicitly defer geometry verification to issues #480/#481, so no installation load qualification follows from generation. [src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py:L9-L24] [src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py:L69-L88]

UNVERIFIED: standards clause titles, the C205 edition and per-result Citation sidecars have not been established for this path; the manifest provides strings, while result payloads carry numeric details rather than a resolved standards sidecar (issue pending (1)). [src/digitalmodel/marine_ops/installation/manifest.yaml:L4-L56] [src/digitalmodel/marine_ops/installation/splash_zone.py:L139-L156] [src/digitalmodel/marine_ops/installation/operability.py:L175-L187]

## 4. Validated worked examples

Validation here is limited to independent closed-form arithmetic in existing test assertions; it does not qualify a standards coefficient, solver model or operating envelope. [tests/marine_ops/installation/test_models.py:L24-L26] [tests/marine_ops/installation/test_crane_tip_motion.py:L60-L73] [tests/marine_ops/installation/test_operability.py:L39-L47]

| ID / class | Independent source; input | Expected result and tolerance basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| V1 validated | Closed-form weight = mass × gravity; synthetic structure fixture | 25000 kg × 9.80665 m/s² = 245166.25 N; relative error ≤ 1e-6, arithmetic test tolerance | `tests/marine_ops/installation/test_models.py::TestStructure::test_weight_air` | PASS at BASE and AFTER [tests/marine_ops/installation/test_models.py:L24-L26] [tests/marine_ops/installation/conftest.py:L73-L83] |
| V2 validated | Zero-offset rigid-body identity; synthetic RAO arrays, tip at CoG | Output heave amplitude equals input elementwise, absolute error ≤ 1e-10 m/m, identity-test tolerance | `tests/marine_ops/installation/test_crane_tip_motion.py::TestCraneTipRAOs::test_crane_at_cog_returns_cog_raos` | PASS at BASE and AFTER [tests/marine_ops/installation/test_crane_tip_motion.py:L60-L73] |
| V3 validated | Closed-form inverse of response = 2 Hs, allowable 3 m | Limiting Hs = 1.5 m; absolute error ≤ 0.1 m, search-grid tolerance in test | `tests/marine_ops/installation/test_operability.py::TestHsLimitForCriterion::test_linear_criterion` | PASS at BASE and AFTER [tests/marine_ops/installation/test_operability.py:L39-L47] |
| R1 regression | Module's additive force identity; synthetic installation case | Combined force equals sum of computed components, relative error ≤ 1e-10; no independent physical benchmark | `tests/marine_ops/installation/test_splash_zone.py::TestSplashZoneAssessment::test_total_hydro_is_sum` | PASS at BASE and AFTER [tests/marine_ops/installation/test_splash_zone.py:L113-L117] |
| R2 regression | Synthetic scatter with imposed very large Hs boundary | 100.0 % within 0.1 percentage point; implementation consistency test | `tests/marine_ops/installation/test_operability.py::TestWeatherWindowOperability::test_all_operable_gives_100` | PASS at BASE and AFTER [tests/marine_ops/installation/test_operability.py:L82-L96] |
| R3 regression | Synthetic buoyant structure and splash result | One `snap_risk` key present; this asserts neither its truth value nor a physical slack threshold | `tests/marine_ops/installation/test_splash_zone.py::TestSplashZoneAssessment::test_snap_risk_flagged` | PASS at BASE and AFTER [tests/marine_ops/installation/test_splash_zone.py:L106-L111] |

Evidence record for this pack: a1be651234e5541809cf1f33d57fce04d1a2ae5b; BASE and AFTER each ran `PYTHONPATH=src uv run --no-sources python -m pytest tests/marine_ops/installation/ -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-<stage>` with the configured addopts unchanged: 229 run, 229 passed, 0 failed, 0 skipped, 0 NOT_RUN; test-node membership and outcomes were compared from both logs. [tests/marine_ops/installation/test_crane_tip_motion.py:L1-L22] [tests/marine_ops/installation/test_models.py:L1-L18] [tests/marine_ops/installation/test_operability.py:L1-L15]

## 5. QA gates

The following are evidence requirements for installation use, not gate clearance from unit tests; G2 applies per lowering stage with hook and sling hand loads within ±2 %, G3 covers splash zone and deepest stage, and G5 applies to reports (AQG r0, sections 6, 7, 9 and 10).

Each check states an executable comparison or record audit; NOT_RUN requires the missing evidence before the corresponding engineering gate can pass (AQG r0, sections 2 and 12).

| Gate / applicability | Check | Numeric pass criterion and unit | Execution and evidence; present status |
| --- | --- | --- | --- |
| G0 applicable | I0 inputs | 0 missing DOF keys; 0 negative generated section lengths, counts and m | Existing missing-RAO and negative-section tests; PASS for fixtures only [tests/marine_ops/installation/test_crane_tip_motion.py:L116-L125] [tests/marine_ops/installation/test_jumper_config_propagation.py:L158-L172] |
| G0 applicable | I01 manifest | 8 of 8 indexed functions have implementation citations; 0 manifest-only rows, count | Resolve section-3 map against pin; PASS as traceability only [src/digitalmodel/marine_ops/installation/manifest.yaml:L8-L56] |
| G1 applicable downstream | I1 target load | 0 errors and 0 warnings, count; target/version mismatch count = 0 | Audit load receipt for exact target version and input digest; NOT_RUN, no licensed load evidence; offline generation is not G1 [src/digitalmodel/marine_ops/installation/jumper_input.py:L4-L9] (AQG r0, section 5) |
| G2 applicable | I2 in-air weight | Absolute hook-load deviation from independently summed component weights ≤ 2.0 % | Sum weight components then convert kg to N using the declared gravity; synthetic air-weight test PASS, full hook/rigging comparison NOT_RUN [src/digitalmodel/marine_ops/installation/jumper_lift.py:L921-L942] [tests/marine_ops/installation/test_models.py:L24-L26] (AQG r0, section 6) |
| G2 applicable | I21 slack sling | Minimum tension > 0 N for a no-slack disposition; 0 mismatches between coded snap flag and `(F_min < 0 N)` | Recalculate half-submerged baseline minus varying buoyancy; supplied-record audit per stage; synthetic sign audit PASS, stage records NOT_RUN [src/digitalmodel/marine_ops/installation/splash_zone.py:L123-L155] |
| G2 applicable | I22 limiting sea state | Closed-form Hs search error ≤ 0.1 m; no admissible point beyond the governing response limit | Run V3 and retain per-Tp limiting response; PASS for V3 only, structure-dependent qualification open under #2131 [tests/marine_ops/installation/test_operability.py:L39-L47] [src/digitalmodel/marine_ops/installation/operability.py:L158-L185] |
| G2 applicable per stage | I23 hook and slings | Hook and every sling load within ±2.0 % of hand balance; unchecked component count = 0 | Audit each lowering-stage component table and independently balance weight/buoyancy and sling geometry; NOT_RUN, no model-stage load records [src/digitalmodel/marine_ops/installation/jumper_lift.py:L616-L632] (AQG r0, section 6) |
| G3 applicable at splash and deepest stage | I3 short dynamics | Completed duration ≥ 1.0 s; 0 warnings, 0 nonfinite responses, 0 unplanned zero/compressive tensions; tensions within ±25.0 % of statics | Audit supplied short-run histories and images at both stages; NOT_RUN, licensed solver execution excluded (AQG r0, sections 7 and 10) |
| G4 conditional | I4 imported diffraction | For new diffraction data: weight/buoyancy mismatch ≤ 1.0 %, independent volume mismatch ≤ 0.5 % | Audit upstream G4 record; NOT_APPLICABLE for synthetic unit-test RAOs without an imported mesh; required for any newly imported diffraction result [tests/marine_ops/installation/conftest.py:L32-L51] (AQG r0, sections 8 and 10) |
| G5 applicable to issued report | I5 traceability | Missing gate records, fingerprints, component rows and required images = 0, count | Audit report appendix against source records; NOT_RUN, suitability report contains screening provenance but no installation gate appendix; #2427/#1556 [src/digitalmodel/marine_ops/installation/suitability_report.py:L43-L66] [src/digitalmodel/marine_ops/installation/suitability_report.py:L121-L166] (AQG r0, section 9) |

Checks I2, I21 and I23 require a sourced mass/displacement/rigging basis before application to a real structure; the box displacement calculation is not such a basis for an open frame. [src/digitalmodel/marine_ops/installation/models.py:L85-L89] [src/digitalmodel/marine_ops/installation/splash_zone.py:L123-L137]

## 6. Known failure modes and pitfalls

| Symptom | Cause and detection | Incident / open item | Evidence |
| --- | --- | --- | --- |
| Missing waves can receive a passing splash criterion | Omitted conditions become zero waves; I0 requires explicit sourced metocean input; no fail-closed detection check exists in this function | issue pending (2) | [src/digitalmodel/marine_ops/installation/go_no_go.py:L109-L112] [src/digitalmodel/marine_ops/installation/go_no_go.py:L292-L303] |
| Missing or sparse sensor data can become GO | Empty statistics are zero and fewer than ten samples produce zero significant heave; check data availability before evaluating limits; no detection check exists for unavailable data | issue pending (3) | [src/digitalmodel/marine_ops/installation/realtime_feedback.py:L74-L91] [src/digitalmodel/marine_ops/installation/realtime_feedback.py:L134-L173] [tests/marine_ops/installation/test_realtime_feedback.py:L75-L86] |
| One decision can retain capacities from another configuration | Embedded crane, factor, sling and deck assumptions bypass analysed configuration; I2/I23 require independent component comparison; no full override detection check exists | issue pending (2) | [src/digitalmodel/marine_ops/installation/go_no_go.py:L135-L176] [src/digitalmodel/marine_ops/installation/go_no_go.py:L219-L243] [src/digitalmodel/marine_ops/installation/go_no_go.py:L305-L335] |
| Structure changes leave the operability boundary unchanged | Motion-only heave/velocity path; I22 detects search arithmetic, but no structure-demand acceptance check exists | #2131, structure-specific envelopes and two-minute load prediction | [src/digitalmodel/marine_ops/installation/operability.py:L108-L187]; digitalmodel issue #2131, Requested outcome and Discovery findings |
| Bounding box treated as displaced volume | `length × width × height` used without flooding/open-frame geometry; I2/I21 need a qualified displacement basis | #2131 | [src/digitalmodel/marine_ops/installation/models.py:L85-L89] [src/digitalmodel/marine_ops/installation/splash_zone.py:L123-L137]; digitalmodel issue #2131, Discovery findings |
| Motion history interpreted as a future load prediction | MRU checker calculates rolling statistics and present exceedances; no detection check exists for causal 120 s forecast skill | #2131 | [src/digitalmodel/marine_ops/installation/realtime_feedback.py:L65-L91] [src/digitalmodel/marine_ops/installation/realtime_feedback.py:L121-L180]; digitalmodel issue #2131, Requested outcome |
| Analytical splash calculation treated as a qualified lowering workflow | Independent public benchmark, stage definition and recorded-version solver sweep remain separate evidence; I1/I23/I3/I5 detect missing receipts | #2427, splash-zone library workflow | [src/digitalmodel/marine_ops/installation/splash_zone.py:L104-L156]; digitalmodel issue #2427, Method and sources and Acceptance |
| Per-pair scatter fraction interpreted as batch roll-up or window persistence | Calculator sums occurrence bins, with no time ordering or returned-run provenance; I5 requires source-run identities and window evidence | #1556, operability roll-up | [src/digitalmodel/marine_ops/installation/operability.py:L190-L235] [src/digitalmodel/marine_ops/installation/vessel_screening.py:L87-L127]; digitalmodel issue #1556, What and Acceptance |
| Slack test mistaken for criterion verification | Existing test checks key presence only; I21 explicitly checks sign and excludes zero-tension clearance | #2131 | [tests/marine_ops/installation/test_splash_zone.py:L106-L111] [src/digitalmodel/marine_ops/installation/splash_zone.py:L133-L155] |
| Hs boundary array misread as seconds | Result field name and its documented unit differ from assigned data; inspect actual Tp in details and use I22 | #1556 | [src/digitalmodel/marine_ops/installation/models.py:L214-L217] [src/digitalmodel/marine_ops/installation/operability.py:L173-L185] |
| Crane capacity beyond published radius remains endpoint value | `np.interp` clamps; example emits a warning note; independently check curve domain before suitability use | issue pending (2) | [src/digitalmodel/marine_ops/installation/models.py:L137-L139] [examples/demos/installation/go_no_go_crane_curves.py:L100-L105] |

UNVERIFIED: no admitted private criterion IDs were supplied for `structure-installation-envelopes`; no private register was read and no private criterion or value is asserted; sidecar entry: to be created (#2131).

## 7. Lessons log

| Date | Change ID | Lesson | Evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- | --- |
| 2026-10-15 | revision 0 | Treat shape/presence assertions as software regression, not physical slack validation | [tests/marine_ops/installation/test_splash_zone.py:L106-L111] | Sections 4–6 separate these evidence classes | Not assigned; issue pending (4) |
| 2026-10-15 | revision 0 | A structure parameter does not establish a structure-dependent boundary | [src/digitalmodel/marine_ops/installation/operability.py:L108-L187] | Sections 1, 5 and 6 restrict the claim | Not assigned; issue pending (4) |

## 8. Ownership

Proposed review roles: installation family quality manager and installation deputy quality manager; escalation of missing input basis, ambiguous clauses or tolerance changes is to the family quality manager (AQG r0, section 11).

UNVERIFIED: named assignments, deputy availability, producing responsibility and shared-log identifiers are not established by this repository evidence; the maintainer must assign them before clearance (issue pending (4)).

## 9. Dependencies

| Direction | Interface | Evidence / qualification |
| --- | --- | --- |
| Upstream numerical | NumPy arrays and trapezoidal integration | [src/digitalmodel/marine_ops/installation/models.py:L12-L15] [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L143-L168] |
| Upstream specification | Pydantic v2 and PyYAML; positive-dimension schema is distinct from the pipeline's top-level-key check | [src/digitalmodel/marine_ops/installation/jumper_installation_schema.py:L28-L33] [src/digitalmodel/marine_ops/installation/jumper_installation_schema.py:L247-L294] [src/digitalmodel/marine_ops/installation/jumper_installation.py:L159-L168] |
| Upstream candidate data | `marine_ops/vessel_db` loader and confidence scoring | [src/digitalmodel/marine_ops/installation/vessel_suitability.py:L19-L32] [src/digitalmodel/marine_ops/installation/vessel_suitability.py:L68-L115] |
| Downstream generated input | Modular generator schema, include/collision and cross-object validators; no solver load claim | [src/digitalmodel/marine_ops/installation/jumper_input.py:L19-L28] [src/digitalmodel/marine_ops/installation/jumper_input.py:L31-L89] |
| Downstream report | Suitability Markdown/HTML renderers and three installation demonstration scripts | [src/digitalmodel/marine_ops/installation/suitability_report.py:L81-L166] [examples/demos/installation/vessel_capability_score.py:L27-L42] [examples/demos/installation/suitability_report.py:L44-L58] [examples/demos/installation/go_no_go_crane_curves.py:L147-L166] |
| Downstream operational evidence | Batch results, run identities and weather-window persistence belong to roll-up qualification | digitalmodel issue #1556, What and How |

UNVERIFIED: complete inventory rows for dependencies and a qualified target solver version are not supplied here; downstream G1/G3 acceptance remains open under #2427, dependency inventory under issue pending (4).

## 10. Coverage and open items

### Read coverage

The clause and main-path map covers `manifest.yaml`, `models.py`, `splash_zone.py`, `crane_tip_motion.py`, `operability.py`, `go_no_go.py`, `vessel_screening.py`, `vessel_suitability.py`, `suitability_report.py` and `realtime_feedback.py`. [src/digitalmodel/marine_ops/installation/manifest.yaml:L8-L56] [src/digitalmodel/marine_ops/installation/models.py:L36-L89] [src/digitalmodel/marine_ops/installation/go_no_go.py:L87-L130] [src/digitalmodel/marine_ops/installation/vessel_suitability.py:L68-L107] [src/digitalmodel/marine_ops/installation/suitability_report.py:L53-L66] [src/digitalmodel/marine_ops/installation/realtime_feedback.py:L121-L180]

Jumper files read: `jumper_lift.py`, `jumper_installation.py`, `jumper_installation_schema.py`, `jumper_input.py` and `jumper_to_modular_spec.py`; their configuration-derived numeric examples are excluded from this public pack. [src/digitalmodel/marine_ops/installation/jumper_lift.py:L1-L19] [src/digitalmodel/marine_ops/installation/jumper_installation.py:L126-L181] [src/digitalmodel/marine_ops/installation/jumper_installation_schema.py:L213-L245] [src/digitalmodel/marine_ops/installation/jumper_input.py:L69-L89] [src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py:L9-L24]

Guidance read: `structures.md`, `jumpers.md`, `riser.md` and `analysis_vs_operations.md` under `docs/domains/installation/`; these are contextual notes, not independent numerical benchmark evidence. [docs/domains/installation/structures.md:L1-L3] [docs/domains/installation/jumpers.md:L1-L13] [docs/domains/installation/riser.md:L1-L13] [docs/domains/installation/analysis_vs_operations.md:L1-L16]

The twelve test files read and run are `test_crane_tip_motion.py`, `test_go_no_go.py`, `test_jumper_config_propagation.py`, `test_jumper_installation_schema.py`, `test_jumper_lift.py`, `test_models.py`, `test_operability.py`, `test_realtime_feedback.py`, `test_splash_zone.py`, `test_suitability_report.py`, `test_vessel_screening.py` and `test_vessel_suitability.py`, all under `tests/marine_ops/installation/`; reusable fixtures are in `conftest.py`. [tests/marine_ops/installation/test_crane_tip_motion.py:L1-L22] [tests/marine_ops/installation/test_go_no_go.py:L1-L18] [tests/marine_ops/installation/test_jumper_config_propagation.py:L1-L40] [tests/marine_ops/installation/test_jumper_installation_schema.py:L1-L18] [tests/marine_ops/installation/test_jumper_lift.py:L1-L34] [tests/marine_ops/installation/test_models.py:L1-L18] [tests/marine_ops/installation/test_operability.py:L1-L15] [tests/marine_ops/installation/test_realtime_feedback.py:L1-L18] [tests/marine_ops/installation/test_splash_zone.py:L1-L19] [tests/marine_ops/installation/test_suitability_report.py:L1-L16] [tests/marine_ops/installation/test_vessel_screening.py:L1-L16] [tests/marine_ops/installation/test_vessel_suitability.py:L1-L15] [tests/marine_ops/installation/conftest.py:L1-L17]

### Open-item register

All items require maintainer disposition before clearance; draft references are placeholders to replace with issue numbers, not completed issue publication.

| Item | Gap and required evidence | Responsible role | Tracking |
| --- | --- | --- | --- |
| O1 | Resolve the section-2 frame/phase claim, section-3 standards/sidecar claim, clause titles, C205 edition and spectral amplitude contradiction | Installation quality manager | issue pending (1); evidence [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L118-L150] [src/digitalmodel/marine_ops/installation/manifest.yaml:L4-L56] |
| O2 | Qualify missing-input handling and remove configuration-independent capacities from decision use; detect curve-domain overreach | Maintainer | issue pending (2); evidence [src/digitalmodel/marine_ops/installation/go_no_go.py:L135-L176] [src/digitalmodel/marine_ops/installation/go_no_go.py:L292-L335] [src/digitalmodel/marine_ops/installation/models.py:L137-L139] |
| O3 | Add unavailable/stale/sparse MRU disposition and a fail-closed detection test | Maintainer | issue pending (3); evidence [src/digitalmodel/marine_ops/installation/realtime_feedback.py:L74-L91] [src/digitalmodel/marine_ops/installation/realtime_feedback.py:L134-L173] |
| O4 | Resolve section-8 role assignments and section-9 dependency inventory; assign shared-log entry IDs | Maintainer | issue pending (4) |
| O5 | Qualify structure-specific displacement/load boundaries and causal 120 s prediction; identify admitted private criteria by ID only; resolve section-6 private criterion claim | Installation quality manager | #2131; evidence [src/digitalmodel/marine_ops/installation/operability.py:L108-L187] [src/digitalmodel/marine_ops/installation/models.py:L85-L89]; digitalmodel issue #2131, Requested outcome |
| O6 | Supply independent public splash/lowering benchmark and per-stage load/dynamics records, target solver version and G5 appendix; I1/I2/I21/I23/I3/I5 remain NOT_RUN for engineering use | Installation quality manager | #2427; digitalmodel issue #2427, Method and sources and Acceptance |
| O7 | Qualify batch-result provenance, weather-window persistence and result-unit naming before operational roll-up | Maintainer | #1556; evidence [src/digitalmodel/marine_ops/installation/operability.py:L190-L235]; digitalmodel issue #1556, What and Acceptance |
| O8 | Confirm maturity and routing distinctions between the two installation packages | Maintainer | issue pending (5); evidence [docs/registry/module-routing.yaml:L351-L356] |

| O9 | Correct section-total test tolerance: the test multiplies a relative fraction by the expected length, so its comment does not match its asserted relative criterion | Maintainer | issue pending (6); evidence [tests/marine_ops/installation/test_jumper_lift.py:L513-L523] |

UNVERIFIED: second-provider clearance has not occurred; this draft does not establish gate signatures or engineering acceptance (#2427; issue pending (4)).
