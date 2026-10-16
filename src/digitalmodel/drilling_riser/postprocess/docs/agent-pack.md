# Drilling riser postprocess knowledge pack

| Header | Value |
| --- | --- |
| Module | drilling_riser/postprocess |
| Repository | digitalmodel |
| Package | src/digitalmodel/drilling_riser/postprocess |
| Pack revision | 1, 2026-10-15 |
| Code revision | fc6c8f6939182860d11db223067626497caa2713 |
| Analysis family | OrcaFlex riser and mooring; registry family MR |
| Quality manager role | Riser and mooring family quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The package aggregates campaign results, verifies result digests, evaluates criteria and summarises coverage; its channel-reading interface consumes `riser-w5-channels/1` without a solver. [src/digitalmodel/drilling_riser/postprocess/__init__.py:L1-L18]

The main path is `collect` → `load_verified` → `evaluate_case` → `summarise`; failed final attempts and declared cases without ledger entries stay visible, while summary applicability and seed reconciliation remain caller responsibilities. [src/digitalmodel/drilling_riser/postprocess/aggregate.py:L76-L120] [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L266-L284]

Stress extraction is a separate interface accepting supplied model and program objects; it reads range graphs or histories from an already solved result rather than launching analysis. [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L71-L90] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L345-L392]

The parent domain is marked `development`; the package contains no independent maturity field in that entry. [docs/registry/module-routing.yaml:L99-L108]

UNVERIFIED: supported target program versions, physical applicability ranges and complete analysis gate evidence are not established by these offline tests; issue pending (3).

## 2. Inputs and outputs

Input contracts follow the readers and demand functions below; “required” means that the cited path accesses the value without a fallback. [src/digitalmodel/drilling_riser/postprocess/channels.py:L28-L135] [src/digitalmodel/drilling_riser/postprocess/checks.py:L118-L263]

| Input | Unit | Range and default | Source and evidence |
| --- | --- | --- | --- |
| Run manifests, ledger records and result JSON | IDs, UTC strings, SHA-256 | `run.json` required; absent ledger becomes missing; latest timestamp wins, traversal order breaks ties | `collect`; [src/digitalmodel/drilling_riser/postprocess/aggregate.py:L65-L120] |
| `channels.w5` | mixed, as below | supported schema or absent schema; `analysis == dynamics` selects statistics/envelopes, otherwise static access | `w5`, `is_dynamic`; [src/digitalmodel/drilling_riser/postprocess/channels.py:L28-L56] |
| Point loads, angles, arcs, stroke, times | kN, kN m, kPa, deg, m, s | no general magnitude validation; missing required channel raises `MissingChannel` | upstream units and channel access; [src/digitalmodel/drilling_riser/global_model/w5_channels.py:L29-L36] [src/digitalmodel/drilling_riser/postprocess/channels.py:L49-L128] |
| Criterion `check_key`, `limit`, `location` | check-specific | keys selected by `CHECKS`; unknown key becomes `NOT_EVALUATED`; missing location defaults to upper and lower flex joints | [src/digitalmodel/drilling_riser/postprocess/checks.py:L110-L153] [src/digitalmodel/drilling_riser/postprocess/checks.py:L479-L505] [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L90-L115] |
| Context: top tension, minimum tension, stroke datum, connector offsets and conductor section capacities | kips, m, kN, ft-kips | caller supplies required fields; connector level defaults to operational | [src/digitalmodel/drilling_riser/postprocess/checks.py:L172-L263] |
| Context: pipe geometry, minimum wall and strengths | m, MPa | positive OD and wall, twice wall below OD; elastic modulus and Poisson defaults are code settings | [src/digitalmodel/drilling_riser/postprocess/pressure.py:L12-L32] [src/digitalmodel/drilling_riser/postprocess/checks.py:L438-L455] |
| Station geometry and exclusions | m | nonempty ordered positive finite lengths with increasing cumulative arcs; sampling distance enforcement requires `coupling_tol_m` | [src/digitalmodel/drilling_riser/postprocess/station_geometry.py:L8-L30] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L233-L256] |
| Seed documents and expected seed IDs | integer IDs | exact expected set; shorthand count means IDs 1 through count | [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L82-L107] |
| Expected criterion/group cases and final outcomes | IDs and status strings | optional; omitting either yields unknown coverage rather than PASS | [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L266-L284] |

| Output | Unit | Contract and evidence |
| --- | --- | --- |
| `CaseRecord` and unsuccessful/missing case list | metadata | latest final record; verified JSON is loaded separately | [src/digitalmodel/drilling_riser/postprocess/aggregate.py:L27-L51] [src/digitalmodel/drilling_riser/postprocess/aggregate.py:L76-L120] |
| `CaseCheck`, including demand, allowable, utilisation, location, seed, time and reason | check-specific, utilisation dimensionless | PASS, FAIL, NOT_EVALUATED or SCREENING; dictionary serialisation through `as_dict` | [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L36-L59] |
| Criterion and sensitivity-group summaries | case counts, dimensionless utilisation | observed status is separate from coverage-qualified status; screening never governs | [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L227-L263] [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L266-L315] |
| Supplementary stress-range documents and merged channels | kPa, m, deg | schema /1 gives span, /2 also significant range and reconstruction check; separate stress axis retained | [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L29-L42] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L93-L119] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L359-L392] |

Arc length runs from End A; exclusion boundaries distinguish the two sides of coincident samples, and End B derives from section geometry. [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L196-L211] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L258-L282]

Pressure reconstruction uses an external hydrostatic depth and a contents reference above mean sea level; burst demand is internal minus external pressure and collapse reverses that sign, with negative demand clipped only for utilisation. [src/digitalmodel/drilling_riser/postprocess/checks.py:L91-L95] [src/digitalmodel/drilling_riser/postprocess/checks.py:L446-L465]

Coincident rows preserve one location and time; ranges are envelopes rather than coincident load combinations, and stress history extraction excludes the build-up stage. [src/digitalmodel/drilling_riser/postprocess/channels.py:L59-L68] [src/digitalmodel/drilling_riser/postprocess/channels.py:L94-L105] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L345-L352]

UNVERIFIED: full global/local frame, origin, moment sign, angle phase and a general unit-consistency validator are not established here; issue pending (3).

## 3. Method and governing standards

The following describes implemented calculations rather than independent standard compliance. [src/digitalmodel/drilling_riser/postprocess/pressure.py:L1-L32] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L1-L19]

| Method | Implementation and reference basis |
| --- | --- |
| Campaign aggregation | `collect` selects the latest attempt per case and retains missing declarations; `load_verified` compares SHA-256 before JSON decoding. [src/digitalmodel/drilling_riser/postprocess/aggregate.py:L76-L120] |
| Point/range access | `point_rows` joins coincident extremes and optional tension-moment hull; `range_extreme` selects the requested envelope extreme and arc. [src/digitalmodel/drilling_riser/postprocess/channels.py:L59-L68] [src/digitalmodel/drilling_riser/postprocess/channels.py:L94-L112] |
| Flex joints, stress, coupling rating and effective tension | `fj_mean`, `fj_max`, `fj_max_avail`, `von_mises`, `coupling_rating`, `t_eff_min`; unit conversion precedes ratios, and positive effective tension is a sign check. [src/digitalmodel/drilling_riser/postprocess/checks.py:L118-L169] |
| Tension settings and strokes | `t_min` and `tension_setting_max` compare the supplied tension settings; `tj_stroke` measures directional travel from `tj_datum_m`. `tensioner_stroke` compares the largest absolute stroke-channel excursion with half the tensioner stroke, assuming zero at mid-stroke and a travel ratio of one. [src/digitalmodel/drilling_riser/postprocess/checks.py:L172-L202] |
| Connector and conductor checks | `connector_tmp` uses coincident rows, interface tension offset and the lowest interpolated capacity over the pressure interval; out-of-chart values are unevaluated; `conductor_bending` uses per-section capacity. [src/digitalmodel/drilling_riser/postprocess/checks.py:L80-L95] [src/digitalmodel/drilling_riser/postprocess/checks.py:L205-L263] |
| Stress range and station classification | `range_over_theta`, `turning_points`, `significant_range`, `theta_histories`, `significant_over_theta`, `coupling_positions`, `joint_sections`, `classify_stations`; half-cycle ranges come from the upstream counter, highest-third weighting defines significant range, and axial-plus-bending reconstruction supplies angular histories. [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L52-L68] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L125-L193] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L196-L342] |
| Stress acceptance | `stress_range_limit_ksi` names API RP 16Q, *Design, Selection, Operation and Maintenance of Marine Drilling Riser Systems*, first edition 1993 (reaffirmed 2001), Table 3.1 note [5]; `SIGNIFICANT_DEFINITION` names clause 3.3.2; clause text not checked against the licensed copy. Tabulated allowables are not reproduced here. [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L1-L19] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L37-L49] [src/digitalmodel/riser_database/getters.py:L61-L69] Title: API, *2024 Publications Catalog*, RP 16Q entry (PDF page 36), [public catalogue](https://www.api.org/-/media/files/publications/2024-catalog/2024-publication-catalog.pdf), opened 2026-10-15; the entry identifies second edition 2017, not applicability of the implemented 1993 locators. |
| Stress seed combination | `dyn_stress_range` without `wave_kind` preserves the whole-line span path; irregular mode uses significant range with mean per station before governing selection; regular mode reports SCREENING; the special pup-side station may also be the reported hot spot. [src/digitalmodel/drilling_riser/postprocess/checks.py:L314-L429] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L238-L249] |
| Pressure | `burst_pressure_mpa` and `collapse_pressure_mpa` name API STD 2RD, *Dynamic Risers for Floating Production Systems*, second edition 2013, clauses 5.3.2 and 5.3.3; `burst` and `collapse` compare signed differential pressure with caller-factored capacity; clause text not checked against the licensed copy. [src/digitalmodel/drilling_riser/postprocess/pressure.py:L1-L32] [src/digitalmodel/drilling_riser/postprocess/checks.py:L438-L465] [docs/domains/codes-register.md:L44-L51] Title confirmed in API, *2024 Publications Catalog*, Std 2RD entry (PDF page 10), [public catalogue](https://www.api.org/-/media/files/publications/2024-catalog/2024-publication-catalog.pdf), opened 2026-10-15. |
| Clearance | `reach` implements h tan(theta) + d/(2 cos(theta)); `limiting_angle_deg` bisects contact with opening/2, returning zero if the member cannot fit; `moonpool_clearance` compares upper-flex-joint angle with the smallest obstruction limit. [src/digitalmodel/drilling_riser/postprocess/clearance.py:L3-L31] [src/digitalmodel/drilling_riser/postprocess/checks.py:L468-L476] |
| Seed extrema | `_mle`, `_ppcc`, `gumbel_quantile`, `gumbel_fit`: maximum likelihood, probability-plot correlation, asymptotic location interval and leave-one-out sensitivity; minima use negated maxima, at least two finite samples required. [src/digitalmodel/drilling_riser/postprocess/extremes.py:L49-L124] |
| Verdict and summary | `CheckValue` uses a code tolerance for dimensionless utilisation; `evaluate_case` captures missing channels and unsupported demands; `summarise` requires explicit nonempty coverage before PASS. [src/digitalmodel/drilling_riser/postprocess/checks.py:L50-L74] [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L82-L129] [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L244-L284] |

UNVERIFIED: licensed-copy clause titles, formula applicability, edition selection and calculation Citation sidecar coverage need a provenance audit; neither pressure capacity function emits a sidecar, and the stress allowable function returns a bare number; issue pending (1). [src/digitalmodel/drilling_riser/postprocess/pressure.py:L21-L32] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L45-L49]

## 4. Validated worked examples

Inputs below are synthetic Python literals in the cited test files; expected values identified as closed form do not use pressure or stress allowables from a licensed table. [tests/drilling_riser/postprocess/test_w510_significant_range.py:L50-L67] [tests/drilling_riser/postprocess/test_w5_additions.py:L181-L188]

The following offline results were obtained at BASE `da203103cb43722fc6b724c4c73ae74714eed2b6` and its uncommitted documentation revision; package and test code are unchanged from the header pin. They establish the listed mathematical/software checks only.

| Case and classification | Source and input | Expected value, tolerance and basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| V1: validated sine range | closed form: span of A sin(t) is 2A; sampled input in test | 75.0 stress units; relative tolerance 0.1 % for sampling | `tests/drilling_riser/postprocess/test_w510_significant_range.py::test_regular_sine_gives_double_amplitude` | PASS / PASS; [tests/drilling_riser/postprocess/test_w510_significant_range.py:L50-L56] |
| V2: validated geometric contact | closed form: at zero pivot distance, contact gives theta = acos(d/opening); test also substitutes the computed angle in the general contact equation | radial residual <= 1e-9 m; zero-distance angle agrees with acos using the test's default approximate comparison | `tests/drilling_riser/postprocess/test_w5_additions.py::test_limiting_angle_of_a_cylinder_through_an_opening_below_its_pivot` | PASS / PASS; [tests/drilling_riser/postprocess/test_w5_additions.py:L181-L188] |
| R1: pressure regression, burst | formula repeated in test; independent licensed-source agreement not established | implementation equals test expression and stored hand value within 0.01 MPa; values are not reproduced here | `tests/drilling_riser/postprocess/test_w5_additions.py::test_burst_pressure_api_2rd_5_3_2_hand_value` | PASS / PASS; [tests/drilling_riser/postprocess/test_w5_additions.py:L132-L136] |
| R2: pressure regression, collapse | formula repeated in test; independent licensed-source agreement not established | implementation equals test expression and stored hand value within 0.01 MPa; values are not reproduced here | `tests/drilling_riser/postprocess/test_w5_additions.py::test_collapse_pressure_api_2rd_5_3_3_hand_value` | PASS / PASS; [tests/drilling_riser/postprocess/test_w5_additions.py:L139-L144] |
| R3: latest-record regression | constructed manifests and attempts in test | 2 final successful records, 0 issues; latest supplied payload selected | `tests/drilling_riser/postprocess/test_aggregate.py::test_latest_record_per_case_wins_across_runs` | PASS / PASS; [tests/drilling_riser/postprocess/test_aggregate.py:L49-L60] |
| R4: incomplete-coverage regression | synthetic passing check plus unsuccessful final solve | missing_count = 1 case; FAIL count = 0 cases; status NOT_EVALUATED | `tests/drilling_riser/postprocess/test_summary_coverage.py::test_observed_pass_and_failed_solve_are_not_engineering_failure` | PASS / PASS; [tests/drilling_riser/postprocess/test_summary_coverage.py:L29-L42] |
| R5: extraction regression | existing fake model and supplied program-object test fixture | reconstruction flag true for consistent histories; false for inconsistent or untested reconstruction; ranges agree with per-angle calls | `tests/drilling_riser/postprocess/test_w510_significant_range.py::test_extract_significant_against_a_fake_model` | PASS / PASS; [tests/drilling_riser/postprocess/test_w510_significant_range.py:L372-L425] |
| R6: tension-setting regression, PP14 | synthetic context and criterion limits in the test | `t_min` u = 813.5/1149.9 and `tension_setting_max` u = 1149.9/2970.0 (kips/kips); relative error <= 1e-9 (1); near-minimum setting passes | `tests/drilling_riser/postprocess/test_evaluate.py::test_tension_setting_checks_use_the_case_context` | PASS / PASS; [tests/drilling_riser/postprocess/test_evaluate.py:L155-L160] |
| R7: stroke regression, PP15 | synthetic datum, usable range and stroke channels | telescopic extension and collapse u = 0.5; tensioner u = 0.5 (m/m); relative error <= 1e-9 (1) | `tests/drilling_riser/postprocess/test_evaluate.py::test_stroke_checks_from_the_spaced_out_datum` | PASS / PASS; [tests/drilling_riser/postprocess/test_evaluate.py:L163-L169] |
| R8: stress-utilisation regression, PP16 | synthetic legacy stress channel; code-derived allowable retained as a symbol | demand 20.0 MPa; u = demand/returned allowable (MPa/MPa), relative error <= 1e-9 (1); no independent standard validation | `tests/drilling_riser/postprocess/test_w510_significant_range.py::test_legacy_call_without_wave_kind_is_unchanged` | PASS / PASS; [tests/drilling_riser/postprocess/test_w510_significant_range.py:L244-L247] |
| R9: pup-side regression, PP17 | synthetic station ranges; code-derived allowable retained as a symbol | with pup-side station demand 76.0 MPa, FAIL and hot-spot `cr10_station` = coupling; without it demand 62.0 MPa; ratio relative error <= 1e-9 (1) | `tests/drilling_riser/postprocess/test_w510_significant_range.py::test_pup_side_station_governs_with_the_coupling_saf_and_the_hot_spot_is_still_reported` | PASS / PASS; [tests/drilling_riser/postprocess/test_w510_significant_range.py:L622-L631] |

V1 uses arbitrary stress units consistently because the test operates on a bare numerical series; extraction separately labels its stress channels kPa. [tests/drilling_riser/postprocess/test_w510_significant_range.py:L50-L56] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L359-L362]

## 5. QA gates

Gate applicability is the OrcaFlex riser and mooring family set: G0–G3 apply, G4 requires a cited diffraction record when new diffraction data are used, and G5 applies to every report (AQG r0, *Analysis QA gates*, revision 0, sections 3–10).

The table defines executable acceptance checks; passing synthetic software tests does not constitute a signed model gate record (AQG r0, *Analysis QA gates*, revision 0, sections 11–13).

This pack owns checks on `postprocess` functions. The sibling global-model pack IDs cross-reference here: STROKE → PP15, ANGLE-CLEARANCE → PP05 for the postprocess clearance geometry, COVERAGE → PP10, STATIONS → PP01 for the tested geometry subset; the broader topology contract remains #2329. Its next revision needs these cross-references, issue pending (4). [src/digitalmodel/drilling_riser/global_model/docs/agent-pack.md:L189-L193]

| ID / gate | Module-specific check and numeric pass criterion with unit | Execution and evidence |
| --- | --- | --- |
| PP00 / G0 | `load_verified`: 0 mismatching bytes accepted, 1 `DigestMismatch` for a deliberately mismatched digest | existing `test_digest_mismatch_is_refused`, PASS; [tests/drilling_riser/postprocess/test_aggregate.py:L74-L79] |
| PP01 / G0 | `evaluate_case`: 0 missing and 0 unexpected seed IDs before fitting; `section_lengths`: 0 nonpositive/nonfinite lengths accepted (m) | exact-set and invalid-geometry regressions, PASS; [tests/drilling_riser/postprocess/test_evaluate.py:L309-L318] [tests/drilling_riser/postprocess/test_station_geometry.py:L11-L36] |
| PP02 / G0 | `merge_stress_range`: 0 malformed channel/axis lengths accepted (samples), 0 schema-/2 documents accepted without a true theta-check flag | rejection-before-mutation and flag regressions, PASS; [tests/drilling_riser/postprocess/test_w510_significant_range.py:L448-L455] [tests/drilling_riser/postprocess/test_w510_significant_range.py:L518-L525] |
| PP03 / G1 | incoming model that generated `channels.w5`: 0 load errors, 0 load warnings (messages), 1 exact target-version match (record) | target-program route: load original model in the matching OrcaFlex demo where supported, otherwise licensed `LoadData`; no model is emitted by this package; NOT_RUN, solver/program interface forbidden in this offline verification. Input boundary: [src/digitalmodel/drilling_riser/postprocess/__init__.py:L1-L4]; route: AQG r0, *Analysis QA gates*, revision 0, section 5. |
| PP04 / G2 | `t_eff_min`: +5.0 kN passes and -5.0 kN fails in the existing static fixture, 0 sign misclassifications (cases) | existing static/effective-tension regression, PASS; upstream statics convergence and component evidence NOT_RUN without solver records; [tests/drilling_riser/postprocess/test_evaluate.py:L145-L152] |
| PP05 / G2 | `limiting_angle_deg` with `reach`: absolute contact residual <= 1e-9 m; 0 deg clearance when member does not fit | V2, PASS; [tests/drilling_riser/postprocess/test_w5_additions.py:L181-L188] |
| PP06 / G3 | `extract_significant`: checked points > 0 samples and relative theta reconstruction error <= 1e-6 (1); `merge_stress_range` rejects a failed flag | existing fake-model extraction and rejection regressions, PASS; real-history extraction NOT_RUN without a solved result; [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L345-L352] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L377-L391] [tests/drilling_riser/postprocess/test_w510_significant_range.py:L408-L425] |
| PP07 / G3 | `gumbel_fit`: location relative difference <= 1e-6 (1), scale relative difference <= 1e-5 (1) against the separately implemented SciPy fit | `test_mle_matches_an_independent_implementation`, PASS; algorithm comparison, not physical validation; [tests/drilling_riser/postprocess/test_extremes.py:L24-L29] |
| PP08 / G3 | `evaluate_case` stress-range modes: 0 seeded irregular cases accepted as regular-wave screening (cases); 0 differing station grids averaged (grids) | screening and station-grid rejection regressions, PASS; native 1.0 s dynamics, 0 warning messages and output finiteness records remain NOT_RUN (AQG r0, *Analysis QA gates*, revision 0, section 7); [tests/drilling_riser/postprocess/test_w510_significant_range.py:L291-L295] [tests/drilling_riser/postprocess/test_w510_significant_range.py:L353-L356] |
| PP09 / G4 | when new diffraction-derived vessel data feed the incoming channel result, require 1 cited passing diffraction record per body (records), and 0 unmatched input fingerprints (fingerprints) | NOT_APPLICABLE to this synthetic offline evidence: no new diffraction result is an input; incoming production data require the conditional record (AQG r0, *Analysis QA gates*, revision 0, sections 8–10). Package input boundary: [src/digitalmodel/drilling_riser/postprocess/channels.py:L28-L35] |
| PP10 / G5 | `summarise`: 0 missing expected cases and 0 unevaluated cases (cases) before complete PASS; failed final outcomes cannot be replaced by stale PASS | coverage tests, PASS; [tests/drilling_riser/postprocess/test_summary_coverage.py:L111-L141] |
| PP11 / G5 | audit `CaseCheck.as_dict` evidence against verified `CaseRecord` and supplied expected/ledger sets: 0 untraceable reported results (records), 1 disposition for every required gate (records) | manual join of case ID, digest, criterion, unit and gate evidence; NOT_RUN for a production appendix because it needs production solver records; [src/digitalmodel/drilling_riser/postprocess/aggregate.py:L27-L39] [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L39-L59] [docs/drilling-riser-summary-coverage.html:L15-L23]; AQG r0, *Analysis QA gates*, revision 0, section 9. |
| PP12 / G0 | `connector_tmp` and `burst`: 0 out-of-chart states treated as evaluated (cases), 0 missing coincident variables silently accepted (variables) | missing-channel and chart-boundary regressions, PASS; [tests/drilling_riser/postprocess/test_evaluate.py:L183-L187] [tests/drilling_riser/postprocess/test_evaluate.py:L291-L306] [tests/drilling_riser/postprocess/test_w5_additions.py:L201-L206] |
| PP13 / G0 | `collect`: 1 `missing` issue per declared case without a ledger record; 0 dropped cases (cases) | `test_declared_cases_without_a_ledger_record_are_reported_missing`, PASS; [tests/drilling_riser/postprocess/test_aggregate.py:L102-L119] |
| PP14 / G2 | `t_min`: u = 813.5/1149.9; `tension_setting_max`: u = 1149.9/2970.0, demand and allowable in kips; relative error <= 1e-9 (1); engineering pass rule u <= 1 + 1e-9; top tension = minimum tension × (1 + 1e-12) passes | R6 and supplementary strict-ratio assertions, PASS; [tests/drilling_riser/postprocess/test_evaluate.py:L155-L160] [src/digitalmodel/drilling_riser/postprocess/checks.py:L50-L74] |
| PP15 / G2 | `tj_stroke`: extension and collapse each u = 0.5 using each fixture excursion divided by available directional travel; `tensioner_stroke`: u = 0.5, excursion divided by half-stroke; demand and allowable in m; relative error <= 1e-9 (1), engineering pass rule u <= 1 + 1e-9 | R7 and supplementary strict-ratio assertions, PASS; datum/mid-stroke applicability is separate from this software check; [tests/drilling_riser/postprocess/test_evaluate.py:L163-L169] [src/digitalmodel/drilling_riser/postprocess/checks.py:L181-L202] [src/digitalmodel/drilling_riser/postprocess/checks.py:L50-L74] |
| PP16 / G3 | `dyn_stress_range`: legacy demand 20.0 MPa divided by returned allowable in MPa, u agrees within relative 1e-9 (1); `cr10_station_mean`: seeded demand equals the mean at each station before governing selection, relative error <= 1e-9 (1); engineering pass rule u <= 1 + 1e-9 | R8, seed-mean regression and supplementary strict-ratio assertions, PASS; code-derived allowables are not independent standard oracles; [tests/drilling_riser/postprocess/test_w510_significant_range.py:L244-L247] [tests/drilling_riser/postprocess/test_w510_significant_range.py:L259-L270] [src/digitalmodel/drilling_riser/postprocess/checks.py:L50-L74] |
| PP17 / G3 | `evaluate_case`: with the pup-side station, coupling demand 76.0 MPa divided by returned allowable in MPa, FAIL expected; hot-spot `cr10_station` = coupling; without it demand 62.0 MPa divided by returned allowable, hot-spot classification absent; ratio relative error <= 1e-9 (1), 0 station/verdict mismatches (cases) | R9 and supplementary strict-ratio assertions, PASS; no licensed allowable value is reproduced; [tests/drilling_riser/postprocess/test_w510_significant_range.py:L622-L631] |

## 6. Known failure modes and pitfalls

| Symptom | Cause or boundary | Detection | Incident or evidence |
| --- | --- | --- | --- |
| Passing observations mistaken for complete campaign acceptance | caller omitted expected logical cases or final outcomes; current implementation returns unknown coverage and NOT_EVALUATED | PP10; `test_omitted_expected_cases_cannot_establish_complete_coverage` | #2277; [tests/drilling_riser/postprocess/test_summary_coverage.py:L94-L108] [docs/drilling-riser-summary-coverage.html:L15-L23] |
| Off-centre tensioner setting or travel ratio silently misrepresented | `tensioner_stroke` assumes mid-stroke zero and a travel ratio of one; neither is supplied independently | no detection check exists for this physical-basis mismatch; PP15 checks the implemented ratio only; issue pending (3) | [src/digitalmodel/drilling_riser/postprocess/checks.py:L198-L202] [src/digitalmodel/drilling_riser/postprocess/channels.py:L115-L128] |
| Missing ledger case silently absent from an analysis | interrupted campaign; `collect` now retains declarations as missing | PP13; 1 missing case expected | #2277; [tests/drilling_riser/postprocess/test_aggregate.py:L102-L119] |
| Regular-wave stress output used as an irregular-sea verdict | `wave_kind` selects different statistics; omitting it retains the legacy span/Gumbel path | PP08 and PP16; require explicit mode in production context | [src/digitalmodel/drilling_riser/postprocess/checks.py:L410-L429] [tests/drilling_riser/postprocess/test_w510_significant_range.py:L229-L253] |
| Near-End-B interior coupling loses its upper side, or invalid sections reach a verdict | geometry identity must be separated from sample coincidence; current regressions reject those cases | PP01 and `test_interior_coupling_near_end_b_still_requires_its_upper_side` | #2327; [tests/drilling_riser/postprocess/test_station_geometry.py:L11-L50] |
| Unbounded expansion or ambiguous exclusion/sampling topology | joint expansion scales with section length; sampling tolerance is optional, and input/topology limits remain unresolved | no detection check exists for the full supported-input/resource contract; PP01 covers only the existing geometry subset | #2329; [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L196-L211] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L299-L342] [docs/reviews/issue-2327-station-geometry.html:L7-L7] |
| Hot spot inadvertently removed from coupling acceptance | a pup-side coupling can coincide with the reported hot spot; it remains a coupling criterion station | PP17; hot spot and coupling verdict both retained | [tests/drilling_riser/postprocess/test_w510_significant_range.py:L622-L631] |
| False significant-range confidence from an unchecked angular reconstruction | no checked points is not a pass; failed/absent flag prevents merge | PP02, PP06 | [tests/drilling_riser/postprocess/test_w510_significant_range.py:L408-L425] [tests/drilling_riser/postprocess/test_w510_significant_range.py:L448-L455] |
| A pressure regression mistaken for a licensed-standard validation | test repeats the production expression; provenance and licensed applicability remain unchecked | no detection check exists for independent standard agreement; issue pending (1) | [tests/drilling_riser/postprocess/test_w5_additions.py:L132-L144] |
| Map family label mistaken for implemented model writing | the map lists extraction/merge functions under a writer family although the functions read results or merge data | no detection check exists (map classification; issue pending (2)) | [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L71-L119] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L345-L392] |
| Tension setting or directional stroke ratios reported on the wrong basis | minimum tension is the demand, top setting its allowable; strokes compare excursion with available travel | PP14 and PP15 | [src/digitalmodel/drilling_riser/postprocess/checks.py:L172-L202] [tests/drilling_riser/postprocess/test_evaluate.py:L155-L169] |
| Standard edition treated as settled | stress references use 1993 locators while citation metadata records a second edition in 2017 | no detection check exists for edition applicability; issue pending (1) | [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L37-L49] [src/digitalmodel/riser_database/getters.py:L61-L69] |
| Empty pressure-point or conductor range aborts case evaluation | `_pressure_check` unpacks an unset result; `conductor_bending` dereferences an unset governing value; `evaluate_case` catches neither exception | no detection check exists; issue pending (5) | [src/digitalmodel/drilling_riser/postprocess/checks.py:L248-L263] [src/digitalmodel/drilling_riser/postprocess/checks.py:L446-L455] [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L114-L120] |

## 7. Lessons log

| Date | Evidence identifier | Lesson and pack change | Shared-log entry |
| --- | --- | --- | --- |
| 2026-10-15 | #2277 | Keep observed verdict separate from expected coverage; sections 2, 5 and 6 retain caller reconciliation and unknown coverage. [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L266-L284] | issue pending (3) |
| 2026-10-15 | #2327 / #2329 | Geometry regressions do not establish a complete topology/resource contract; sections 5 and 6 preserve this limit. [docs/reviews/issue-2327-station-geometry.html:L7-L7] | issue pending (3) |

## 8. Ownership

Proposed review roles: riser and mooring family quality manager; deputy riser and mooring quality manager; producing discipline: riser analysis; escalation: family quality manager for criterion, tolerance, edition or classification disagreements, then the maintainer for scope decisions (AQG r0, *Analysis QA gates*, revision 0, section 11).

UNVERIFIED: appointments, deputy availability and shared-log entry identifiers have not been established; issue pending (3).

## 9. Dependencies

| Dependency or interface | Inventory relationship and evidence |
| --- | --- |
| `drilling_riser/global_model` | upstream channel producer; the postprocess package identifies its schema and runner relationship. [src/digitalmodel/drilling_riser/postprocess/__init__.py:L1-L4] [src/digitalmodel/drilling_riser/postprocess/channels.py:L1-L7] |
| `global_model.fatigue_channels.half_cycles` → `fatigue.rainflow.rainflow_count` | upstream counter, called on turning points by significant-range calculation. [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L146-L165] [src/digitalmodel/drilling_riser/global_model/fatigue_channels.py:L51-L64] |
| NumPy and standard-library numerical/serialisation functions | arrays for histories and reconstruction; standard-library fitting, JSON and SHA-256. [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L125-L178] [src/digitalmodel/drilling_riser/postprocess/aggregate.py:L114-L120] [src/digitalmodel/drilling_riser/postprocess/extremes.py:L97-L124] |
| SciPy | offline independent implementation comparison, guarded by the existing test; no production dependency in the local Gumbel implementation. [tests/drilling_riser/postprocess/test_extremes.py:L24-L29] [src/digitalmodel/drilling_riser/postprocess/extremes.py:L97-L124] |
| Downstream summary consumers | final-check/coverage API; consumers must supply criterion applicability and logical-case ledger outcomes. [docs/drilling-riser-summary-coverage.html:L12-L23] |

Every literal OrcaFlex occurrence in package Python source is inventoried here: the package introduction at `__init__.py` line 4 identifies the campaign-runner input, and `stress_range.py` lines 3–4 identify the reopened result and outer-fibre stress channel. [src/digitalmodel/drilling_riser/postprocess/__init__.py:L4-L4] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L3-L4]

The associated supplied-object calls are `Period`, `RangeGraph`, `oeLine`, `TimeHistorySpecification` and `GetMultipleTimeHistories`; these interfaces were read as text and existing fake-object tests ran, with no native program interface loaded or solver queried. [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L71-L90] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L345-L392] [tests/drilling_riser/postprocess/test_w510_significant_range.py:L372-L425]

### Implementation-map inventory and counted-family search

Map data pin: `b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407`; embedded searched-code revision: `947fa50ff9a9e210de7e8f90e01b37add85c5954`; citations to `docs/registry/implementation-map.yaml` resolve against the map data pin, while all other repository citations resolve against the header code pin. [docs/registry/implementation-map.yaml:L1-L4]

The package has two map families, tied at three in-package rows each; file order counts effective tension first, then text-model writers, so both are counted. The inventory below enumerates every implementation row of both families, including entries outside the package; an external row identifier resolves its exact path, symbol, kind and relation at the cited map record. Map classifications are retained as inventory data rather than engineering endorsements. [docs/registry/implementation-map.yaml:L1667-L1673] [docs/registry/implementation-map.yaml:L4235-L4241]

#### effective-tension-riser-stack-up

Inventory: 264 rows, 3 in the package. [docs/registry/implementation-map.yaml:L1667-L1673]

| Map row | In-package implementation or external record | Pinned map evidence |
| --- | --- | --- |
| 1 | External implementation record | [docs/registry/implementation-map.yaml:L1674-L1682] |
| 2 | External implementation record | [docs/registry/implementation-map.yaml:L1683-L1691] |
| 3 | External implementation record | [docs/registry/implementation-map.yaml:L1692-L1700] |
| 4 | External implementation record | [docs/registry/implementation-map.yaml:L1701-L1709] |
| 5 | External implementation record | [docs/registry/implementation-map.yaml:L1710-L1718] |
| 6 | External implementation record | [docs/registry/implementation-map.yaml:L1719-L1727] |
| 7 | External implementation record | [docs/registry/implementation-map.yaml:L1728-L1736] |
| 8 | External implementation record | [docs/registry/implementation-map.yaml:L1737-L1745] |
| 9 | External implementation record | [docs/registry/implementation-map.yaml:L1746-L1754] |
| 10 | External implementation record | [docs/registry/implementation-map.yaml:L1755-L1765] |
| 11 | External implementation record | [docs/registry/implementation-map.yaml:L1766-L1776] |
| 12 | External implementation record | [docs/registry/implementation-map.yaml:L1777-L1785] |
| 13 | External implementation record | [docs/registry/implementation-map.yaml:L1786-L1797] |
| 14 | External implementation record | [docs/registry/implementation-map.yaml:L1798-L1806] |
| 15 | External implementation record | [docs/registry/implementation-map.yaml:L1807-L1815] |
| 16 | External implementation record | [docs/registry/implementation-map.yaml:L1816-L1824] |
| 17 | External implementation record | [docs/registry/implementation-map.yaml:L1825-L1836] |
| 18 | External implementation record | [docs/registry/implementation-map.yaml:L1837-L1845] |
| 19 | External implementation record | [docs/registry/implementation-map.yaml:L1846-L1854] |
| 20 | External implementation record | [docs/registry/implementation-map.yaml:L1855-L1865] |
| 21 | External implementation record | [docs/registry/implementation-map.yaml:L1866-L1874] |
| 22 | External implementation record | [docs/registry/implementation-map.yaml:L1875-L1883] |
| 23 | External implementation record | [docs/registry/implementation-map.yaml:L1884-L1894] |
| 24 | External implementation record | [docs/registry/implementation-map.yaml:L1895-L1903] |
| 25 | External implementation record | [docs/registry/implementation-map.yaml:L1904-L1912] |
| 26 | External implementation record | [docs/registry/implementation-map.yaml:L1913-L1921] |
| 27 | External implementation record | [docs/registry/implementation-map.yaml:L1922-L1930] |
| 28 | External implementation record | [docs/registry/implementation-map.yaml:L1931-L1939] |
| 29 | External implementation record | [docs/registry/implementation-map.yaml:L1940-L1948] |
| 30 | External implementation record | [docs/registry/implementation-map.yaml:L1949-L1957] |
| 31 | External implementation record | [docs/registry/implementation-map.yaml:L1958-L1969] |
| 32 | External implementation record | [docs/registry/implementation-map.yaml:L1970-L1982] |
| 33 | External implementation record | [docs/registry/implementation-map.yaml:L1983-L1991] |
| 34 | External implementation record | [docs/registry/implementation-map.yaml:L1992-L2000] |
| 35 | External implementation record | [docs/registry/implementation-map.yaml:L2001-L2011] |
| 36 | External implementation record | [docs/registry/implementation-map.yaml:L2012-L2020] |
| 37 | External implementation record | [docs/registry/implementation-map.yaml:L2021-L2029] |
| 38 | External implementation record | [docs/registry/implementation-map.yaml:L2030-L2038] |
| 39 | External implementation record | [docs/registry/implementation-map.yaml:L2039-L2049] |
| 40 | External implementation record | [docs/registry/implementation-map.yaml:L2050-L2058] |
| 41 | External implementation record | [docs/registry/implementation-map.yaml:L2059-L2067] |
| 42 | External implementation record | [docs/registry/implementation-map.yaml:L2068-L2076] |
| 43 | External implementation record | [docs/registry/implementation-map.yaml:L2077-L2085] |
| 44 | External implementation record | [docs/registry/implementation-map.yaml:L2086-L2097] |
| 45 | External implementation record | [docs/registry/implementation-map.yaml:L2098-L2106] |
| 46 | External implementation record | [docs/registry/implementation-map.yaml:L2107-L2115] |
| 47 | External implementation record | [docs/registry/implementation-map.yaml:L2116-L2124] |
| 48 | External implementation record | [docs/registry/implementation-map.yaml:L2125-L2133] |
| 49 | External implementation record | [docs/registry/implementation-map.yaml:L2134-L2142] |
| 50 | External implementation record | [docs/registry/implementation-map.yaml:L2143-L2153] |
| 51 | External implementation record | [docs/registry/implementation-map.yaml:L2154-L2164] |
| 52 | External implementation record | [docs/registry/implementation-map.yaml:L2165-L2173] |
| 53 | External implementation record | [docs/registry/implementation-map.yaml:L2174-L2184] |
| 54 | External implementation record | [docs/registry/implementation-map.yaml:L2185-L2197] |
| 55 | External implementation record | [docs/registry/implementation-map.yaml:L2198-L2207] |
| 56 | External implementation record | [docs/registry/implementation-map.yaml:L2208-L2220] |
| 57 | External implementation record | [docs/registry/implementation-map.yaml:L2221-L2229] |
| 58 | External implementation record | [docs/registry/implementation-map.yaml:L2230-L2238] |
| 59 | External implementation record | [docs/registry/implementation-map.yaml:L2239-L2251] |
| 60 | External implementation record | [docs/registry/implementation-map.yaml:L2252-L2263] |
| 61 | External implementation record | [docs/registry/implementation-map.yaml:L2264-L2272] |
| 62 | External implementation record | [docs/registry/implementation-map.yaml:L2273-L2281] |
| 63 | External implementation record | [docs/registry/implementation-map.yaml:L2282-L2290] |
| 64 | External implementation record | [docs/registry/implementation-map.yaml:L2291-L2299] |
| 65 | External implementation record | [docs/registry/implementation-map.yaml:L2300-L2310] |
| 66 | External implementation record | [docs/registry/implementation-map.yaml:L2311-L2322] |
| 67 | External implementation record | [docs/registry/implementation-map.yaml:L2323-L2331] |
| 68 | External implementation record | [docs/registry/implementation-map.yaml:L2332-L2342] |
| 69 | External implementation record | [docs/registry/implementation-map.yaml:L2343-L2351] |
| 70 | External implementation record | [docs/registry/implementation-map.yaml:L2352-L2362] |
| 71 | External implementation record | [docs/registry/implementation-map.yaml:L2363-L2372] |
| 72 | External implementation record | [docs/registry/implementation-map.yaml:L2373-L2381] |
| 73 | External implementation record | [docs/registry/implementation-map.yaml:L2382-L2390] |
| 74 | External implementation record | [docs/registry/implementation-map.yaml:L2391-L2400] |
| 75 | External implementation record | [docs/registry/implementation-map.yaml:L2401-L2409] |
| 76 | External implementation record | [docs/registry/implementation-map.yaml:L2410-L2420] |
| 77 | External implementation record | [docs/registry/implementation-map.yaml:L2421-L2429] |
| 78 | External implementation record | [docs/registry/implementation-map.yaml:L2430-L2438] |
| 79 | External implementation record | [docs/registry/implementation-map.yaml:L2439-L2447] |
| 80 | External implementation record | [docs/registry/implementation-map.yaml:L2448-L2456] |
| 81 | External implementation record | [docs/registry/implementation-map.yaml:L2457-L2465] |
| 82 | External implementation record | [docs/registry/implementation-map.yaml:L2466-L2476] |
| 83 | External implementation record | [docs/registry/implementation-map.yaml:L2477-L2485] |
| 84 | External implementation record | [docs/registry/implementation-map.yaml:L2486-L2494] |
| 85 | `src/digitalmodel/drilling_riser/postprocess/checks.py::t_min` | [docs/registry/implementation-map.yaml:L2495-L2503] |
| 86 | `src/digitalmodel/drilling_riser/postprocess/checks.py::tension_setting_max` | [docs/registry/implementation-map.yaml:L2504-L2512] |
| 87 | `src/digitalmodel/drilling_riser/postprocess/checks.py::tensioner_stroke` | [docs/registry/implementation-map.yaml:L2513-L2521] |
| 88 | External implementation record | [docs/registry/implementation-map.yaml:L2522-L2530] |
| 89 | External implementation record | [docs/registry/implementation-map.yaml:L2531-L2539] |
| 90 | External implementation record | [docs/registry/implementation-map.yaml:L2540-L2548] |
| 91 | External implementation record | [docs/registry/implementation-map.yaml:L2549-L2557] |
| 92 | External implementation record | [docs/registry/implementation-map.yaml:L2558-L2566] |
| 93 | External implementation record | [docs/registry/implementation-map.yaml:L2567-L2578] |
| 94 | External implementation record | [docs/registry/implementation-map.yaml:L2579-L2587] |
| 95 | External implementation record | [docs/registry/implementation-map.yaml:L2588-L2596] |
| 96 | External implementation record | [docs/registry/implementation-map.yaml:L2597-L2608] |
| 97 | External implementation record | [docs/registry/implementation-map.yaml:L2609-L2617] |
| 98 | External implementation record | [docs/registry/implementation-map.yaml:L2618-L2626] |
| 99 | External implementation record | [docs/registry/implementation-map.yaml:L2627-L2635] |
| 100 | External implementation record | [docs/registry/implementation-map.yaml:L2636-L2644] |
| 101 | External implementation record | [docs/registry/implementation-map.yaml:L2645-L2653] |
| 102 | External implementation record | [docs/registry/implementation-map.yaml:L2654-L2667] |
| 103 | External implementation record | [docs/registry/implementation-map.yaml:L2668-L2676] |
| 104 | External implementation record | [docs/registry/implementation-map.yaml:L2677-L2685] |
| 105 | External implementation record | [docs/registry/implementation-map.yaml:L2686-L2697] |
| 106 | External implementation record | [docs/registry/implementation-map.yaml:L2698-L2708] |
| 107 | External implementation record | [docs/registry/implementation-map.yaml:L2709-L2722] |
| 108 | External implementation record | [docs/registry/implementation-map.yaml:L2723-L2736] |
| 109 | External implementation record | [docs/registry/implementation-map.yaml:L2737-L2748] |
| 110 | External implementation record | [docs/registry/implementation-map.yaml:L2749-L2759] |
| 111 | External implementation record | [docs/registry/implementation-map.yaml:L2760-L2768] |
| 112 | External implementation record | [docs/registry/implementation-map.yaml:L2769-L2779] |
| 113 | External implementation record | [docs/registry/implementation-map.yaml:L2780-L2790] |
| 114 | External implementation record | [docs/registry/implementation-map.yaml:L2791-L2799] |
| 115 | External implementation record | [docs/registry/implementation-map.yaml:L2800-L2814] |
| 116 | External implementation record | [docs/registry/implementation-map.yaml:L2815-L2823] |
| 117 | External implementation record | [docs/registry/implementation-map.yaml:L2824-L2832] |
| 118 | External implementation record | [docs/registry/implementation-map.yaml:L2833-L2841] |
| 119 | External implementation record | [docs/registry/implementation-map.yaml:L2842-L2850] |
| 120 | External implementation record | [docs/registry/implementation-map.yaml:L2851-L2864] |
| 121 | External implementation record | [docs/registry/implementation-map.yaml:L2865-L2873] |
| 122 | External implementation record | [docs/registry/implementation-map.yaml:L2874-L2882] |
| 123 | External implementation record | [docs/registry/implementation-map.yaml:L2883-L2891] |
| 124 | External implementation record | [docs/registry/implementation-map.yaml:L2892-L2900] |
| 125 | External implementation record | [docs/registry/implementation-map.yaml:L2901-L2909] |
| 126 | External implementation record | [docs/registry/implementation-map.yaml:L2910-L2918] |
| 127 | External implementation record | [docs/registry/implementation-map.yaml:L2919-L2927] |
| 128 | External implementation record | [docs/registry/implementation-map.yaml:L2928-L2936] |
| 129 | External implementation record | [docs/registry/implementation-map.yaml:L2937-L2945] |
| 130 | External implementation record | [docs/registry/implementation-map.yaml:L2946-L2954] |
| 131 | External implementation record | [docs/registry/implementation-map.yaml:L2955-L2963] |
| 132 | External implementation record | [docs/registry/implementation-map.yaml:L2964-L2977] |
| 133 | External implementation record | [docs/registry/implementation-map.yaml:L2978-L2986] |
| 134 | External implementation record | [docs/registry/implementation-map.yaml:L2987-L2995] |
| 135 | External implementation record | [docs/registry/implementation-map.yaml:L2996-L3004] |
| 136 | External implementation record | [docs/registry/implementation-map.yaml:L3005-L3015] |
| 137 | External implementation record | [docs/registry/implementation-map.yaml:L3016-L3024] |
| 138 | External implementation record | [docs/registry/implementation-map.yaml:L3025-L3033] |
| 139 | External implementation record | [docs/registry/implementation-map.yaml:L3034-L3042] |
| 140 | External implementation record | [docs/registry/implementation-map.yaml:L3043-L3051] |
| 141 | External implementation record | [docs/registry/implementation-map.yaml:L3052-L3060] |
| 142 | External implementation record | [docs/registry/implementation-map.yaml:L3061-L3069] |
| 143 | External implementation record | [docs/registry/implementation-map.yaml:L3070-L3078] |
| 144 | External implementation record | [docs/registry/implementation-map.yaml:L3079-L3087] |
| 145 | External implementation record | [docs/registry/implementation-map.yaml:L3088-L3096] |
| 146 | External implementation record | [docs/registry/implementation-map.yaml:L3097-L3105] |
| 147 | External implementation record | [docs/registry/implementation-map.yaml:L3106-L3114] |
| 148 | External implementation record | [docs/registry/implementation-map.yaml:L3115-L3123] |
| 149 | External implementation record | [docs/registry/implementation-map.yaml:L3124-L3132] |
| 150 | External implementation record | [docs/registry/implementation-map.yaml:L3133-L3141] |
| 151 | External implementation record | [docs/registry/implementation-map.yaml:L3142-L3150] |
| 152 | External implementation record | [docs/registry/implementation-map.yaml:L3151-L3159] |
| 153 | External implementation record | [docs/registry/implementation-map.yaml:L3160-L3168] |
| 154 | External implementation record | [docs/registry/implementation-map.yaml:L3169-L3177] |
| 155 | External implementation record | [docs/registry/implementation-map.yaml:L3178-L3186] |
| 156 | External implementation record | [docs/registry/implementation-map.yaml:L3187-L3195] |
| 157 | External implementation record | [docs/registry/implementation-map.yaml:L3196-L3204] |
| 158 | External implementation record | [docs/registry/implementation-map.yaml:L3205-L3215] |
| 159 | External implementation record | [docs/registry/implementation-map.yaml:L3216-L3224] |
| 160 | External implementation record | [docs/registry/implementation-map.yaml:L3225-L3233] |
| 161 | External implementation record | [docs/registry/implementation-map.yaml:L3234-L3242] |
| 162 | External implementation record | [docs/registry/implementation-map.yaml:L3243-L3254] |
| 163 | External implementation record | [docs/registry/implementation-map.yaml:L3255-L3265] |
| 164 | External implementation record | [docs/registry/implementation-map.yaml:L3266-L3279] |
| 165 | External implementation record | [docs/registry/implementation-map.yaml:L3280-L3293] |
| 166 | External implementation record | [docs/registry/implementation-map.yaml:L3294-L3305] |
| 167 | External implementation record | [docs/registry/implementation-map.yaml:L3306-L3316] |
| 168 | External implementation record | [docs/registry/implementation-map.yaml:L3317-L3325] |
| 169 | External implementation record | [docs/registry/implementation-map.yaml:L3326-L3336] |
| 170 | External implementation record | [docs/registry/implementation-map.yaml:L3337-L3347] |
| 171 | External implementation record | [docs/registry/implementation-map.yaml:L3348-L3356] |
| 172 | External implementation record | [docs/registry/implementation-map.yaml:L3357-L3365] |
| 173 | External implementation record | [docs/registry/implementation-map.yaml:L3366-L3374] |
| 174 | External implementation record | [docs/registry/implementation-map.yaml:L3375-L3383] |
| 175 | External implementation record | [docs/registry/implementation-map.yaml:L3384-L3392] |
| 176 | External implementation record | [docs/registry/implementation-map.yaml:L3393-L3401] |
| 177 | External implementation record | [docs/registry/implementation-map.yaml:L3402-L3410] |
| 178 | External implementation record | [docs/registry/implementation-map.yaml:L3411-L3419] |
| 179 | External implementation record | [docs/registry/implementation-map.yaml:L3420-L3428] |
| 180 | External implementation record | [docs/registry/implementation-map.yaml:L3429-L3437] |
| 181 | External implementation record | [docs/registry/implementation-map.yaml:L3438-L3446] |
| 182 | External implementation record | [docs/registry/implementation-map.yaml:L3447-L3455] |
| 183 | External implementation record | [docs/registry/implementation-map.yaml:L3456-L3464] |
| 184 | External implementation record | [docs/registry/implementation-map.yaml:L3465-L3473] |
| 185 | External implementation record | [docs/registry/implementation-map.yaml:L3474-L3482] |
| 186 | External implementation record | [docs/registry/implementation-map.yaml:L3483-L3491] |
| 187 | External implementation record | [docs/registry/implementation-map.yaml:L3492-L3500] |
| 188 | External implementation record | [docs/registry/implementation-map.yaml:L3501-L3509] |
| 189 | External implementation record | [docs/registry/implementation-map.yaml:L3510-L3518] |
| 190 | External implementation record | [docs/registry/implementation-map.yaml:L3519-L3527] |
| 191 | External implementation record | [docs/registry/implementation-map.yaml:L3528-L3539] |
| 192 | External implementation record | [docs/registry/implementation-map.yaml:L3540-L3550] |
| 193 | External implementation record | [docs/registry/implementation-map.yaml:L3551-L3562] |
| 194 | External implementation record | [docs/registry/implementation-map.yaml:L3563-L3571] |
| 195 | External implementation record | [docs/registry/implementation-map.yaml:L3572-L3580] |
| 196 | External implementation record | [docs/registry/implementation-map.yaml:L3581-L3589] |
| 197 | External implementation record | [docs/registry/implementation-map.yaml:L3590-L3601] |
| 198 | External implementation record | [docs/registry/implementation-map.yaml:L3602-L3610] |
| 199 | External implementation record | [docs/registry/implementation-map.yaml:L3611-L3619] |
| 200 | External implementation record | [docs/registry/implementation-map.yaml:L3620-L3628] |
| 201 | External implementation record | [docs/registry/implementation-map.yaml:L3629-L3637] |
| 202 | External implementation record | [docs/registry/implementation-map.yaml:L3638-L3646] |
| 203 | External implementation record | [docs/registry/implementation-map.yaml:L3647-L3658] |
| 204 | External implementation record | [docs/registry/implementation-map.yaml:L3659-L3667] |
| 205 | External implementation record | [docs/registry/implementation-map.yaml:L3668-L3678] |
| 206 | External implementation record | [docs/registry/implementation-map.yaml:L3679-L3687] |
| 207 | External implementation record | [docs/registry/implementation-map.yaml:L3688-L3696] |
| 208 | External implementation record | [docs/registry/implementation-map.yaml:L3697-L3705] |
| 209 | External implementation record | [docs/registry/implementation-map.yaml:L3706-L3714] |
| 210 | External implementation record | [docs/registry/implementation-map.yaml:L3715-L3728] |
| 211 | External implementation record | [docs/registry/implementation-map.yaml:L3729-L3737] |
| 212 | External implementation record | [docs/registry/implementation-map.yaml:L3738-L3746] |
| 213 | External implementation record | [docs/registry/implementation-map.yaml:L3747-L3755] |
| 214 | External implementation record | [docs/registry/implementation-map.yaml:L3756-L3764] |
| 215 | External implementation record | [docs/registry/implementation-map.yaml:L3765-L3773] |
| 216 | External implementation record | [docs/registry/implementation-map.yaml:L3774-L3782] |
| 217 | External implementation record | [docs/registry/implementation-map.yaml:L3783-L3791] |
| 218 | External implementation record | [docs/registry/implementation-map.yaml:L3792-L3802] |
| 219 | External implementation record | [docs/registry/implementation-map.yaml:L3803-L3811] |
| 220 | External implementation record | [docs/registry/implementation-map.yaml:L3812-L3820] |
| 221 | External implementation record | [docs/registry/implementation-map.yaml:L3821-L3831] |
| 222 | External implementation record | [docs/registry/implementation-map.yaml:L3832-L3840] |
| 223 | External implementation record | [docs/registry/implementation-map.yaml:L3841-L3849] |
| 224 | External implementation record | [docs/registry/implementation-map.yaml:L3850-L3858] |
| 225 | External implementation record | [docs/registry/implementation-map.yaml:L3859-L3867] |
| 226 | External implementation record | [docs/registry/implementation-map.yaml:L3868-L3876] |
| 227 | External implementation record | [docs/registry/implementation-map.yaml:L3877-L3885] |
| 228 | External implementation record | [docs/registry/implementation-map.yaml:L3886-L3894] |
| 229 | External implementation record | [docs/registry/implementation-map.yaml:L3895-L3905] |
| 230 | External implementation record | [docs/registry/implementation-map.yaml:L3906-L3914] |
| 231 | External implementation record | [docs/registry/implementation-map.yaml:L3915-L3924] |
| 232 | External implementation record | [docs/registry/implementation-map.yaml:L3925-L3934] |
| 233 | External implementation record | [docs/registry/implementation-map.yaml:L3935-L3943] |
| 234 | External implementation record | [docs/registry/implementation-map.yaml:L3944-L3952] |
| 235 | External implementation record | [docs/registry/implementation-map.yaml:L3953-L3965] |
| 236 | External implementation record | [docs/registry/implementation-map.yaml:L3966-L3975] |
| 237 | External implementation record | [docs/registry/implementation-map.yaml:L3976-L3984] |
| 238 | External implementation record | [docs/registry/implementation-map.yaml:L3985-L3993] |
| 239 | External implementation record | [docs/registry/implementation-map.yaml:L3994-L4004] |
| 240 | External implementation record | [docs/registry/implementation-map.yaml:L4005-L4013] |
| 241 | External implementation record | [docs/registry/implementation-map.yaml:L4014-L4022] |
| 242 | External implementation record | [docs/registry/implementation-map.yaml:L4023-L4031] |
| 243 | External implementation record | [docs/registry/implementation-map.yaml:L4032-L4040] |
| 244 | External implementation record | [docs/registry/implementation-map.yaml:L4041-L4049] |
| 245 | External implementation record | [docs/registry/implementation-map.yaml:L4050-L4058] |
| 246 | External implementation record | [docs/registry/implementation-map.yaml:L4059-L4067] |
| 247 | External implementation record | [docs/registry/implementation-map.yaml:L4068-L4076] |
| 248 | External implementation record | [docs/registry/implementation-map.yaml:L4077-L4085] |
| 249 | External implementation record | [docs/registry/implementation-map.yaml:L4086-L4094] |
| 250 | External implementation record | [docs/registry/implementation-map.yaml:L4095-L4103] |
| 251 | External implementation record | [docs/registry/implementation-map.yaml:L4104-L4112] |
| 252 | External implementation record | [docs/registry/implementation-map.yaml:L4113-L4121] |
| 253 | External implementation record | [docs/registry/implementation-map.yaml:L4122-L4130] |
| 254 | External implementation record | [docs/registry/implementation-map.yaml:L4131-L4139] |
| 255 | External implementation record | [docs/registry/implementation-map.yaml:L4140-L4148] |
| 256 | External implementation record | [docs/registry/implementation-map.yaml:L4149-L4157] |
| 257 | External implementation record | [docs/registry/implementation-map.yaml:L4158-L4166] |
| 258 | External implementation record | [docs/registry/implementation-map.yaml:L4167-L4175] |
| 259 | External implementation record | [docs/registry/implementation-map.yaml:L4176-L4184] |
| 260 | External implementation record | [docs/registry/implementation-map.yaml:L4185-L4193] |
| 261 | External implementation record | [docs/registry/implementation-map.yaml:L4194-L4204] |
| 262 | External implementation record | [docs/registry/implementation-map.yaml:L4205-L4213] |
| 263 | External implementation record | [docs/registry/implementation-map.yaml:L4214-L4222] |
| 264 | External implementation record | [docs/registry/implementation-map.yaml:L4223-L4231] |

#### orcaflex-text-model-writers

Inventory: 440 rows, 3 in the package. [docs/registry/implementation-map.yaml:L4235-L4241]

| Map row | In-package implementation or external record | Pinned map evidence |
| --- | --- | --- |
| 1 | External implementation record | [docs/registry/implementation-map.yaml:L4242-L4250] |
| 2 | External implementation record | [docs/registry/implementation-map.yaml:L4251-L4259] |
| 3 | External implementation record | [docs/registry/implementation-map.yaml:L4260-L4268] |
| 4 | External implementation record | [docs/registry/implementation-map.yaml:L4269-L4277] |
| 5 | External implementation record | [docs/registry/implementation-map.yaml:L4278-L4286] |
| 6 | External implementation record | [docs/registry/implementation-map.yaml:L4287-L4295] |
| 7 | External implementation record | [docs/registry/implementation-map.yaml:L4296-L4304] |
| 8 | External implementation record | [docs/registry/implementation-map.yaml:L4305-L4315] |
| 9 | External implementation record | [docs/registry/implementation-map.yaml:L4316-L4328] |
| 10 | External implementation record | [docs/registry/implementation-map.yaml:L4329-L4341] |
| 11 | External implementation record | [docs/registry/implementation-map.yaml:L4342-L4354] |
| 12 | External implementation record | [docs/registry/implementation-map.yaml:L4355-L4367] |
| 13 | External implementation record | [docs/registry/implementation-map.yaml:L4368-L4376] |
| 14 | External implementation record | [docs/registry/implementation-map.yaml:L4377-L4385] |
| 15 | External implementation record | [docs/registry/implementation-map.yaml:L4386-L4394] |
| 16 | External implementation record | [docs/registry/implementation-map.yaml:L4395-L4403] |
| 17 | External implementation record | [docs/registry/implementation-map.yaml:L4404-L4412] |
| 18 | External implementation record | [docs/registry/implementation-map.yaml:L4413-L4421] |
| 19 | External implementation record | [docs/registry/implementation-map.yaml:L4422-L4432] |
| 20 | External implementation record | [docs/registry/implementation-map.yaml:L4433-L4441] |
| 21 | External implementation record | [docs/registry/implementation-map.yaml:L4442-L4450] |
| 22 | External implementation record | [docs/registry/implementation-map.yaml:L4451-L4459] |
| 23 | External implementation record | [docs/registry/implementation-map.yaml:L4460-L4468] |
| 24 | External implementation record | [docs/registry/implementation-map.yaml:L4469-L4477] |
| 25 | External implementation record | [docs/registry/implementation-map.yaml:L4478-L4486] |
| 26 | External implementation record | [docs/registry/implementation-map.yaml:L4487-L4495] |
| 27 | External implementation record | [docs/registry/implementation-map.yaml:L4496-L4504] |
| 28 | External implementation record | [docs/registry/implementation-map.yaml:L4505-L4515] |
| 29 | External implementation record | [docs/registry/implementation-map.yaml:L4516-L4524] |
| 30 | External implementation record | [docs/registry/implementation-map.yaml:L4525-L4533] |
| 31 | External implementation record | [docs/registry/implementation-map.yaml:L4534-L4542] |
| 32 | External implementation record | [docs/registry/implementation-map.yaml:L4543-L4551] |
| 33 | External implementation record | [docs/registry/implementation-map.yaml:L4552-L4560] |
| 34 | External implementation record | [docs/registry/implementation-map.yaml:L4561-L4569] |
| 35 | External implementation record | [docs/registry/implementation-map.yaml:L4570-L4578] |
| 36 | External implementation record | [docs/registry/implementation-map.yaml:L4579-L4587] |
| 37 | External implementation record | [docs/registry/implementation-map.yaml:L4588-L4596] |
| 38 | External implementation record | [docs/registry/implementation-map.yaml:L4597-L4605] |
| 39 | External implementation record | [docs/registry/implementation-map.yaml:L4606-L4614] |
| 40 | External implementation record | [docs/registry/implementation-map.yaml:L4615-L4623] |
| 41 | External implementation record | [docs/registry/implementation-map.yaml:L4624-L4632] |
| 42 | External implementation record | [docs/registry/implementation-map.yaml:L4633-L4641] |
| 43 | External implementation record | [docs/registry/implementation-map.yaml:L4642-L4650] |
| 44 | External implementation record | [docs/registry/implementation-map.yaml:L4651-L4659] |
| 45 | External implementation record | [docs/registry/implementation-map.yaml:L4660-L4668] |
| 46 | External implementation record | [docs/registry/implementation-map.yaml:L4669-L4677] |
| 47 | External implementation record | [docs/registry/implementation-map.yaml:L4678-L4689] |
| 48 | External implementation record | [docs/registry/implementation-map.yaml:L4690-L4698] |
| 49 | External implementation record | [docs/registry/implementation-map.yaml:L4699-L4709] |
| 50 | External implementation record | [docs/registry/implementation-map.yaml:L4710-L4718] |
| 51 | External implementation record | [docs/registry/implementation-map.yaml:L4719-L4727] |
| 52 | External implementation record | [docs/registry/implementation-map.yaml:L4728-L4736] |
| 53 | External implementation record | [docs/registry/implementation-map.yaml:L4737-L4745] |
| 54 | External implementation record | [docs/registry/implementation-map.yaml:L4746-L4754] |
| 55 | External implementation record | [docs/registry/implementation-map.yaml:L4755-L4763] |
| 56 | External implementation record | [docs/registry/implementation-map.yaml:L4764-L4772] |
| 57 | External implementation record | [docs/registry/implementation-map.yaml:L4773-L4783] |
| 58 | External implementation record | [docs/registry/implementation-map.yaml:L4784-L4794] |
| 59 | External implementation record | [docs/registry/implementation-map.yaml:L4795-L4803] |
| 60 | External implementation record | [docs/registry/implementation-map.yaml:L4804-L4812] |
| 61 | External implementation record | [docs/registry/implementation-map.yaml:L4813-L4821] |
| 62 | External implementation record | [docs/registry/implementation-map.yaml:L4822-L4830] |
| 63 | External implementation record | [docs/registry/implementation-map.yaml:L4831-L4839] |
| 64 | External implementation record | [docs/registry/implementation-map.yaml:L4840-L4851] |
| 65 | External implementation record | [docs/registry/implementation-map.yaml:L4852-L4860] |
| 66 | External implementation record | [docs/registry/implementation-map.yaml:L4861-L4871] |
| 67 | External implementation record | [docs/registry/implementation-map.yaml:L4872-L4883] |
| 68 | External implementation record | [docs/registry/implementation-map.yaml:L4884-L4892] |
| 69 | External implementation record | [docs/registry/implementation-map.yaml:L4893-L4901] |
| 70 | External implementation record | [docs/registry/implementation-map.yaml:L4902-L4910] |
| 71 | External implementation record | [docs/registry/implementation-map.yaml:L4911-L4919] |
| 72 | External implementation record | [docs/registry/implementation-map.yaml:L4920-L4928] |
| 73 | External implementation record | [docs/registry/implementation-map.yaml:L4929-L4937] |
| 74 | External implementation record | [docs/registry/implementation-map.yaml:L4938-L4946] |
| 75 | External implementation record | [docs/registry/implementation-map.yaml:L4947-L4955] |
| 76 | External implementation record | [docs/registry/implementation-map.yaml:L4956-L4964] |
| 77 | External implementation record | [docs/registry/implementation-map.yaml:L4965-L4973] |
| 78 | External implementation record | [docs/registry/implementation-map.yaml:L4974-L4982] |
| 79 | External implementation record | [docs/registry/implementation-map.yaml:L4983-L4993] |
| 80 | External implementation record | [docs/registry/implementation-map.yaml:L4994-L5002] |
| 81 | External implementation record | [docs/registry/implementation-map.yaml:L5003-L5011] |
| 82 | External implementation record | [docs/registry/implementation-map.yaml:L5012-L5022] |
| 83 | External implementation record | [docs/registry/implementation-map.yaml:L5023-L5031] |
| 84 | External implementation record | [docs/registry/implementation-map.yaml:L5032-L5040] |
| 85 | External implementation record | [docs/registry/implementation-map.yaml:L5041-L5049] |
| 86 | External implementation record | [docs/registry/implementation-map.yaml:L5050-L5060] |
| 87 | External implementation record | [docs/registry/implementation-map.yaml:L5061-L5069] |
| 88 | External implementation record | [docs/registry/implementation-map.yaml:L5070-L5078] |
| 89 | External implementation record | [docs/registry/implementation-map.yaml:L5079-L5087] |
| 90 | External implementation record | [docs/registry/implementation-map.yaml:L5088-L5096] |
| 91 | External implementation record | [docs/registry/implementation-map.yaml:L5097-L5105] |
| 92 | External implementation record | [docs/registry/implementation-map.yaml:L5106-L5114] |
| 93 | External implementation record | [docs/registry/implementation-map.yaml:L5115-L5123] |
| 94 | External implementation record | [docs/registry/implementation-map.yaml:L5124-L5132] |
| 95 | External implementation record | [docs/registry/implementation-map.yaml:L5133-L5141] |
| 96 | External implementation record | [docs/registry/implementation-map.yaml:L5142-L5150] |
| 97 | External implementation record | [docs/registry/implementation-map.yaml:L5151-L5161] |
| 98 | External implementation record | [docs/registry/implementation-map.yaml:L5162-L5170] |
| 99 | External implementation record | [docs/registry/implementation-map.yaml:L5171-L5179] |
| 100 | External implementation record | [docs/registry/implementation-map.yaml:L5180-L5190] |
| 101 | External implementation record | [docs/registry/implementation-map.yaml:L5191-L5201] |
| 102 | External implementation record | [docs/registry/implementation-map.yaml:L5202-L5212] |
| 103 | External implementation record | [docs/registry/implementation-map.yaml:L5213-L5221] |
| 104 | External implementation record | [docs/registry/implementation-map.yaml:L5222-L5230] |
| 105 | External implementation record | [docs/registry/implementation-map.yaml:L5231-L5239] |
| 106 | External implementation record | [docs/registry/implementation-map.yaml:L5240-L5248] |
| 107 | External implementation record | [docs/registry/implementation-map.yaml:L5249-L5257] |
| 108 | External implementation record | [docs/registry/implementation-map.yaml:L5258-L5266] |
| 109 | External implementation record | [docs/registry/implementation-map.yaml:L5267-L5275] |
| 110 | External implementation record | [docs/registry/implementation-map.yaml:L5276-L5284] |
| 111 | External implementation record | [docs/registry/implementation-map.yaml:L5285-L5293] |
| 112 | External implementation record | [docs/registry/implementation-map.yaml:L5294-L5302] |
| 113 | External implementation record | [docs/registry/implementation-map.yaml:L5303-L5313] |
| 114 | External implementation record | [docs/registry/implementation-map.yaml:L5314-L5325] |
| 115 | External implementation record | [docs/registry/implementation-map.yaml:L5326-L5334] |
| 116 | External implementation record | [docs/registry/implementation-map.yaml:L5335-L5343] |
| 117 | External implementation record | [docs/registry/implementation-map.yaml:L5344-L5354] |
| 118 | External implementation record | [docs/registry/implementation-map.yaml:L5355-L5365] |
| 119 | External implementation record | [docs/registry/implementation-map.yaml:L5366-L5374] |
| 120 | External implementation record | [docs/registry/implementation-map.yaml:L5375-L5383] |
| 121 | `src/digitalmodel/drilling_riser/postprocess/stress_range.py::extract` | [docs/registry/implementation-map.yaml:L5384-L5392] |
| 122 | `src/digitalmodel/drilling_riser/postprocess/stress_range.py::merge_stress_range` | [docs/registry/implementation-map.yaml:L5393-L5401] |
| 123 | `src/digitalmodel/drilling_riser/postprocess/stress_range.py::extract_significant` | [docs/registry/implementation-map.yaml:L5402-L5410] |
| 124 | External implementation record | [docs/registry/implementation-map.yaml:L5411-L5419] |
| 125 | External implementation record | [docs/registry/implementation-map.yaml:L5420-L5428] |
| 126 | External implementation record | [docs/registry/implementation-map.yaml:L5429-L5437] |
| 127 | External implementation record | [docs/registry/implementation-map.yaml:L5438-L5446] |
| 128 | External implementation record | [docs/registry/implementation-map.yaml:L5447-L5455] |
| 129 | External implementation record | [docs/registry/implementation-map.yaml:L5456-L5464] |
| 130 | External implementation record | [docs/registry/implementation-map.yaml:L5465-L5473] |
| 131 | External implementation record | [docs/registry/implementation-map.yaml:L5474-L5482] |
| 132 | External implementation record | [docs/registry/implementation-map.yaml:L5483-L5491] |
| 133 | External implementation record | [docs/registry/implementation-map.yaml:L5492-L5500] |
| 134 | External implementation record | [docs/registry/implementation-map.yaml:L5501-L5509] |
| 135 | External implementation record | [docs/registry/implementation-map.yaml:L5510-L5518] |
| 136 | External implementation record | [docs/registry/implementation-map.yaml:L5519-L5527] |
| 137 | External implementation record | [docs/registry/implementation-map.yaml:L5528-L5536] |
| 138 | External implementation record | [docs/registry/implementation-map.yaml:L5537-L5547] |
| 139 | External implementation record | [docs/registry/implementation-map.yaml:L5548-L5558] |
| 140 | External implementation record | [docs/registry/implementation-map.yaml:L5559-L5567] |
| 141 | External implementation record | [docs/registry/implementation-map.yaml:L5568-L5576] |
| 142 | External implementation record | [docs/registry/implementation-map.yaml:L5577-L5587] |
| 143 | External implementation record | [docs/registry/implementation-map.yaml:L5588-L5596] |
| 144 | External implementation record | [docs/registry/implementation-map.yaml:L5597-L5607] |
| 145 | External implementation record | [docs/registry/implementation-map.yaml:L5608-L5616] |
| 146 | External implementation record | [docs/registry/implementation-map.yaml:L5617-L5628] |
| 147 | External implementation record | [docs/registry/implementation-map.yaml:L5629-L5641] |
| 148 | External implementation record | [docs/registry/implementation-map.yaml:L5642-L5652] |
| 149 | External implementation record | [docs/registry/implementation-map.yaml:L5653-L5663] |
| 150 | External implementation record | [docs/registry/implementation-map.yaml:L5664-L5672] |
| 151 | External implementation record | [docs/registry/implementation-map.yaml:L5673-L5683] |
| 152 | External implementation record | [docs/registry/implementation-map.yaml:L5684-L5692] |
| 153 | External implementation record | [docs/registry/implementation-map.yaml:L5693-L5701] |
| 154 | External implementation record | [docs/registry/implementation-map.yaml:L5702-L5710] |
| 155 | External implementation record | [docs/registry/implementation-map.yaml:L5711-L5719] |
| 156 | External implementation record | [docs/registry/implementation-map.yaml:L5720-L5728] |
| 157 | External implementation record | [docs/registry/implementation-map.yaml:L5729-L5737] |
| 158 | External implementation record | [docs/registry/implementation-map.yaml:L5738-L5746] |
| 159 | External implementation record | [docs/registry/implementation-map.yaml:L5747-L5755] |
| 160 | External implementation record | [docs/registry/implementation-map.yaml:L5756-L5764] |
| 161 | External implementation record | [docs/registry/implementation-map.yaml:L5765-L5773] |
| 162 | External implementation record | [docs/registry/implementation-map.yaml:L5774-L5784] |
| 163 | External implementation record | [docs/registry/implementation-map.yaml:L5785-L5795] |
| 164 | External implementation record | [docs/registry/implementation-map.yaml:L5796-L5804] |
| 165 | External implementation record | [docs/registry/implementation-map.yaml:L5805-L5813] |
| 166 | External implementation record | [docs/registry/implementation-map.yaml:L5814-L5824] |
| 167 | External implementation record | [docs/registry/implementation-map.yaml:L5825-L5835] |
| 168 | External implementation record | [docs/registry/implementation-map.yaml:L5836-L5844] |
| 169 | External implementation record | [docs/registry/implementation-map.yaml:L5845-L5856] |
| 170 | External implementation record | [docs/registry/implementation-map.yaml:L5857-L5865] |
| 171 | External implementation record | [docs/registry/implementation-map.yaml:L5866-L5874] |
| 172 | External implementation record | [docs/registry/implementation-map.yaml:L5875-L5885] |
| 173 | External implementation record | [docs/registry/implementation-map.yaml:L5886-L5894] |
| 174 | External implementation record | [docs/registry/implementation-map.yaml:L5895-L5903] |
| 175 | External implementation record | [docs/registry/implementation-map.yaml:L5904-L5912] |
| 176 | External implementation record | [docs/registry/implementation-map.yaml:L5913-L5921] |
| 177 | External implementation record | [docs/registry/implementation-map.yaml:L5922-L5930] |
| 178 | External implementation record | [docs/registry/implementation-map.yaml:L5931-L5939] |
| 179 | External implementation record | [docs/registry/implementation-map.yaml:L5940-L5948] |
| 180 | External implementation record | [docs/registry/implementation-map.yaml:L5949-L5957] |
| 181 | External implementation record | [docs/registry/implementation-map.yaml:L5958-L5966] |
| 182 | External implementation record | [docs/registry/implementation-map.yaml:L5967-L5975] |
| 183 | External implementation record | [docs/registry/implementation-map.yaml:L5976-L5984] |
| 184 | External implementation record | [docs/registry/implementation-map.yaml:L5985-L5993] |
| 185 | External implementation record | [docs/registry/implementation-map.yaml:L5994-L6002] |
| 186 | External implementation record | [docs/registry/implementation-map.yaml:L6003-L6011] |
| 187 | External implementation record | [docs/registry/implementation-map.yaml:L6012-L6020] |
| 188 | External implementation record | [docs/registry/implementation-map.yaml:L6021-L6029] |
| 189 | External implementation record | [docs/registry/implementation-map.yaml:L6030-L6038] |
| 190 | External implementation record | [docs/registry/implementation-map.yaml:L6039-L6047] |
| 191 | External implementation record | [docs/registry/implementation-map.yaml:L6048-L6056] |
| 192 | External implementation record | [docs/registry/implementation-map.yaml:L6057-L6065] |
| 193 | External implementation record | [docs/registry/implementation-map.yaml:L6066-L6074] |
| 194 | External implementation record | [docs/registry/implementation-map.yaml:L6075-L6086] |
| 195 | External implementation record | [docs/registry/implementation-map.yaml:L6087-L6095] |
| 196 | External implementation record | [docs/registry/implementation-map.yaml:L6096-L6104] |
| 197 | External implementation record | [docs/registry/implementation-map.yaml:L6105-L6113] |
| 198 | External implementation record | [docs/registry/implementation-map.yaml:L6114-L6122] |
| 199 | External implementation record | [docs/registry/implementation-map.yaml:L6123-L6131] |
| 200 | External implementation record | [docs/registry/implementation-map.yaml:L6132-L6140] |
| 201 | External implementation record | [docs/registry/implementation-map.yaml:L6141-L6149] |
| 202 | External implementation record | [docs/registry/implementation-map.yaml:L6150-L6158] |
| 203 | External implementation record | [docs/registry/implementation-map.yaml:L6159-L6167] |
| 204 | External implementation record | [docs/registry/implementation-map.yaml:L6168-L6180] |
| 205 | External implementation record | [docs/registry/implementation-map.yaml:L6181-L6189] |
| 206 | External implementation record | [docs/registry/implementation-map.yaml:L6190-L6198] |
| 207 | External implementation record | [docs/registry/implementation-map.yaml:L6199-L6207] |
| 208 | External implementation record | [docs/registry/implementation-map.yaml:L6208-L6216] |
| 209 | External implementation record | [docs/registry/implementation-map.yaml:L6217-L6225] |
| 210 | External implementation record | [docs/registry/implementation-map.yaml:L6226-L6234] |
| 211 | External implementation record | [docs/registry/implementation-map.yaml:L6235-L6243] |
| 212 | External implementation record | [docs/registry/implementation-map.yaml:L6244-L6252] |
| 213 | External implementation record | [docs/registry/implementation-map.yaml:L6253-L6261] |
| 214 | External implementation record | [docs/registry/implementation-map.yaml:L6262-L6270] |
| 215 | External implementation record | [docs/registry/implementation-map.yaml:L6271-L6279] |
| 216 | External implementation record | [docs/registry/implementation-map.yaml:L6280-L6290] |
| 217 | External implementation record | [docs/registry/implementation-map.yaml:L6291-L6299] |
| 218 | External implementation record | [docs/registry/implementation-map.yaml:L6300-L6311] |
| 219 | External implementation record | [docs/registry/implementation-map.yaml:L6312-L6320] |
| 220 | External implementation record | [docs/registry/implementation-map.yaml:L6321-L6329] |
| 221 | External implementation record | [docs/registry/implementation-map.yaml:L6330-L6339] |
| 222 | External implementation record | [docs/registry/implementation-map.yaml:L6340-L6348] |
| 223 | External implementation record | [docs/registry/implementation-map.yaml:L6349-L6357] |
| 224 | External implementation record | [docs/registry/implementation-map.yaml:L6358-L6366] |
| 225 | External implementation record | [docs/registry/implementation-map.yaml:L6367-L6375] |
| 226 | External implementation record | [docs/registry/implementation-map.yaml:L6376-L6384] |
| 227 | External implementation record | [docs/registry/implementation-map.yaml:L6385-L6395] |
| 228 | External implementation record | [docs/registry/implementation-map.yaml:L6396-L6404] |
| 229 | External implementation record | [docs/registry/implementation-map.yaml:L6405-L6413] |
| 230 | External implementation record | [docs/registry/implementation-map.yaml:L6414-L6422] |
| 231 | External implementation record | [docs/registry/implementation-map.yaml:L6423-L6431] |
| 232 | External implementation record | [docs/registry/implementation-map.yaml:L6432-L6440] |
| 233 | External implementation record | [docs/registry/implementation-map.yaml:L6441-L6449] |
| 234 | External implementation record | [docs/registry/implementation-map.yaml:L6450-L6458] |
| 235 | External implementation record | [docs/registry/implementation-map.yaml:L6459-L6467] |
| 236 | External implementation record | [docs/registry/implementation-map.yaml:L6468-L6476] |
| 237 | External implementation record | [docs/registry/implementation-map.yaml:L6477-L6485] |
| 238 | External implementation record | [docs/registry/implementation-map.yaml:L6486-L6494] |
| 239 | External implementation record | [docs/registry/implementation-map.yaml:L6495-L6503] |
| 240 | External implementation record | [docs/registry/implementation-map.yaml:L6504-L6512] |
| 241 | External implementation record | [docs/registry/implementation-map.yaml:L6513-L6521] |
| 242 | External implementation record | [docs/registry/implementation-map.yaml:L6522-L6530] |
| 243 | External implementation record | [docs/registry/implementation-map.yaml:L6531-L6539] |
| 244 | External implementation record | [docs/registry/implementation-map.yaml:L6540-L6548] |
| 245 | External implementation record | [docs/registry/implementation-map.yaml:L6549-L6557] |
| 246 | External implementation record | [docs/registry/implementation-map.yaml:L6558-L6566] |
| 247 | External implementation record | [docs/registry/implementation-map.yaml:L6567-L6575] |
| 248 | External implementation record | [docs/registry/implementation-map.yaml:L6576-L6584] |
| 249 | External implementation record | [docs/registry/implementation-map.yaml:L6585-L6593] |
| 250 | External implementation record | [docs/registry/implementation-map.yaml:L6594-L6602] |
| 251 | External implementation record | [docs/registry/implementation-map.yaml:L6603-L6611] |
| 252 | External implementation record | [docs/registry/implementation-map.yaml:L6612-L6620] |
| 253 | External implementation record | [docs/registry/implementation-map.yaml:L6621-L6629] |
| 254 | External implementation record | [docs/registry/implementation-map.yaml:L6630-L6638] |
| 255 | External implementation record | [docs/registry/implementation-map.yaml:L6639-L6649] |
| 256 | External implementation record | [docs/registry/implementation-map.yaml:L6650-L6658] |
| 257 | External implementation record | [docs/registry/implementation-map.yaml:L6659-L6667] |
| 258 | External implementation record | [docs/registry/implementation-map.yaml:L6668-L6676] |
| 259 | External implementation record | [docs/registry/implementation-map.yaml:L6677-L6685] |
| 260 | External implementation record | [docs/registry/implementation-map.yaml:L6686-L6696] |
| 261 | External implementation record | [docs/registry/implementation-map.yaml:L6697-L6705] |
| 262 | External implementation record | [docs/registry/implementation-map.yaml:L6706-L6717] |
| 263 | External implementation record | [docs/registry/implementation-map.yaml:L6718-L6726] |
| 264 | External implementation record | [docs/registry/implementation-map.yaml:L6727-L6735] |
| 265 | External implementation record | [docs/registry/implementation-map.yaml:L6736-L6744] |
| 266 | External implementation record | [docs/registry/implementation-map.yaml:L6745-L6753] |
| 267 | External implementation record | [docs/registry/implementation-map.yaml:L6754-L6762] |
| 268 | External implementation record | [docs/registry/implementation-map.yaml:L6763-L6771] |
| 269 | External implementation record | [docs/registry/implementation-map.yaml:L6772-L6780] |
| 270 | External implementation record | [docs/registry/implementation-map.yaml:L6781-L6789] |
| 271 | External implementation record | [docs/registry/implementation-map.yaml:L6790-L6798] |
| 272 | External implementation record | [docs/registry/implementation-map.yaml:L6799-L6810] |
| 273 | External implementation record | [docs/registry/implementation-map.yaml:L6811-L6819] |
| 274 | External implementation record | [docs/registry/implementation-map.yaml:L6820-L6828] |
| 275 | External implementation record | [docs/registry/implementation-map.yaml:L6829-L6840] |
| 276 | External implementation record | [docs/registry/implementation-map.yaml:L6841-L6853] |
| 277 | External implementation record | [docs/registry/implementation-map.yaml:L6854-L6864] |
| 278 | External implementation record | [docs/registry/implementation-map.yaml:L6865-L6875] |
| 279 | External implementation record | [docs/registry/implementation-map.yaml:L6876-L6884] |
| 280 | External implementation record | [docs/registry/implementation-map.yaml:L6885-L6895] |
| 281 | External implementation record | [docs/registry/implementation-map.yaml:L6896-L6904] |
| 282 | External implementation record | [docs/registry/implementation-map.yaml:L6905-L6913] |
| 283 | External implementation record | [docs/registry/implementation-map.yaml:L6914-L6922] |
| 284 | External implementation record | [docs/registry/implementation-map.yaml:L6923-L6931] |
| 285 | External implementation record | [docs/registry/implementation-map.yaml:L6932-L6940] |
| 286 | External implementation record | [docs/registry/implementation-map.yaml:L6941-L6951] |
| 287 | External implementation record | [docs/registry/implementation-map.yaml:L6952-L6960] |
| 288 | External implementation record | [docs/registry/implementation-map.yaml:L6961-L6969] |
| 289 | External implementation record | [docs/registry/implementation-map.yaml:L6970-L6978] |
| 290 | External implementation record | [docs/registry/implementation-map.yaml:L6979-L6990] |
| 291 | External implementation record | [docs/registry/implementation-map.yaml:L6991-L6999] |
| 292 | External implementation record | [docs/registry/implementation-map.yaml:L7000-L7008] |
| 293 | External implementation record | [docs/registry/implementation-map.yaml:L7009-L7017] |
| 294 | External implementation record | [docs/registry/implementation-map.yaml:L7018-L7026] |
| 295 | External implementation record | [docs/registry/implementation-map.yaml:L7027-L7035] |
| 296 | External implementation record | [docs/registry/implementation-map.yaml:L7036-L7044] |
| 297 | External implementation record | [docs/registry/implementation-map.yaml:L7045-L7053] |
| 298 | External implementation record | [docs/registry/implementation-map.yaml:L7054-L7062] |
| 299 | External implementation record | [docs/registry/implementation-map.yaml:L7063-L7071] |
| 300 | External implementation record | [docs/registry/implementation-map.yaml:L7072-L7080] |
| 301 | External implementation record | [docs/registry/implementation-map.yaml:L7081-L7089] |
| 302 | External implementation record | [docs/registry/implementation-map.yaml:L7090-L7100] |
| 303 | External implementation record | [docs/registry/implementation-map.yaml:L7101-L7109] |
| 304 | External implementation record | [docs/registry/implementation-map.yaml:L7110-L7118] |
| 305 | External implementation record | [docs/registry/implementation-map.yaml:L7119-L7129] |
| 306 | External implementation record | [docs/registry/implementation-map.yaml:L7130-L7138] |
| 307 | External implementation record | [docs/registry/implementation-map.yaml:L7139-L7147] |
| 308 | External implementation record | [docs/registry/implementation-map.yaml:L7148-L7158] |
| 309 | External implementation record | [docs/registry/implementation-map.yaml:L7159-L7167] |
| 310 | External implementation record | [docs/registry/implementation-map.yaml:L7168-L7176] |
| 311 | External implementation record | [docs/registry/implementation-map.yaml:L7177-L7185] |
| 312 | External implementation record | [docs/registry/implementation-map.yaml:L7186-L7194] |
| 313 | External implementation record | [docs/registry/implementation-map.yaml:L7195-L7205] |
| 314 | External implementation record | [docs/registry/implementation-map.yaml:L7206-L7214] |
| 315 | External implementation record | [docs/registry/implementation-map.yaml:L7215-L7223] |
| 316 | External implementation record | [docs/registry/implementation-map.yaml:L7224-L7232] |
| 317 | External implementation record | [docs/registry/implementation-map.yaml:L7233-L7241] |
| 318 | External implementation record | [docs/registry/implementation-map.yaml:L7242-L7250] |
| 319 | External implementation record | [docs/registry/implementation-map.yaml:L7251-L7259] |
| 320 | External implementation record | [docs/registry/implementation-map.yaml:L7260-L7270] |
| 321 | External implementation record | [docs/registry/implementation-map.yaml:L7271-L7279] |
| 322 | External implementation record | [docs/registry/implementation-map.yaml:L7280-L7288] |
| 323 | External implementation record | [docs/registry/implementation-map.yaml:L7289-L7297] |
| 324 | External implementation record | [docs/registry/implementation-map.yaml:L7298-L7306] |
| 325 | External implementation record | [docs/registry/implementation-map.yaml:L7307-L7317] |
| 326 | External implementation record | [docs/registry/implementation-map.yaml:L7318-L7327] |
| 327 | External implementation record | [docs/registry/implementation-map.yaml:L7328-L7336] |
| 328 | External implementation record | [docs/registry/implementation-map.yaml:L7337-L7345] |
| 329 | External implementation record | [docs/registry/implementation-map.yaml:L7346-L7357] |
| 330 | External implementation record | [docs/registry/implementation-map.yaml:L7358-L7366] |
| 331 | External implementation record | [docs/registry/implementation-map.yaml:L7367-L7375] |
| 332 | External implementation record | [docs/registry/implementation-map.yaml:L7376-L7384] |
| 333 | External implementation record | [docs/registry/implementation-map.yaml:L7385-L7393] |
| 334 | External implementation record | [docs/registry/implementation-map.yaml:L7394-L7405] |
| 335 | External implementation record | [docs/registry/implementation-map.yaml:L7406-L7417] |
| 336 | External implementation record | [docs/registry/implementation-map.yaml:L7418-L7429] |
| 337 | External implementation record | [docs/registry/implementation-map.yaml:L7430-L7438] |
| 338 | External implementation record | [docs/registry/implementation-map.yaml:L7439-L7447] |
| 339 | External implementation record | [docs/registry/implementation-map.yaml:L7448-L7456] |
| 340 | External implementation record | [docs/registry/implementation-map.yaml:L7457-L7467] |
| 341 | External implementation record | [docs/registry/implementation-map.yaml:L7468-L7476] |
| 342 | External implementation record | [docs/registry/implementation-map.yaml:L7477-L7487] |
| 343 | External implementation record | [docs/registry/implementation-map.yaml:L7488-L7499] |
| 344 | External implementation record | [docs/registry/implementation-map.yaml:L7500-L7508] |
| 345 | External implementation record | [docs/registry/implementation-map.yaml:L7509-L7519] |
| 346 | External implementation record | [docs/registry/implementation-map.yaml:L7520-L7531] |
| 347 | External implementation record | [docs/registry/implementation-map.yaml:L7532-L7542] |
| 348 | External implementation record | [docs/registry/implementation-map.yaml:L7543-L7551] |
| 349 | External implementation record | [docs/registry/implementation-map.yaml:L7552-L7560] |
| 350 | External implementation record | [docs/registry/implementation-map.yaml:L7561-L7569] |
| 351 | External implementation record | [docs/registry/implementation-map.yaml:L7570-L7581] |
| 352 | External implementation record | [docs/registry/implementation-map.yaml:L7582-L7592] |
| 353 | External implementation record | [docs/registry/implementation-map.yaml:L7593-L7601] |
| 354 | External implementation record | [docs/registry/implementation-map.yaml:L7602-L7614] |
| 355 | External implementation record | [docs/registry/implementation-map.yaml:L7615-L7625] |
| 356 | External implementation record | [docs/registry/implementation-map.yaml:L7626-L7635] |
| 357 | External implementation record | [docs/registry/implementation-map.yaml:L7636-L7647] |
| 358 | External implementation record | [docs/registry/implementation-map.yaml:L7648-L7661] |
| 359 | External implementation record | [docs/registry/implementation-map.yaml:L7662-L7670] |
| 360 | External implementation record | [docs/registry/implementation-map.yaml:L7671-L7679] |
| 361 | External implementation record | [docs/registry/implementation-map.yaml:L7680-L7688] |
| 362 | External implementation record | [docs/registry/implementation-map.yaml:L7689-L7697] |
| 363 | External implementation record | [docs/registry/implementation-map.yaml:L7698-L7710] |
| 364 | External implementation record | [docs/registry/implementation-map.yaml:L7711-L7722] |
| 365 | External implementation record | [docs/registry/implementation-map.yaml:L7723-L7731] |
| 366 | External implementation record | [docs/registry/implementation-map.yaml:L7732-L7745] |
| 367 | External implementation record | [docs/registry/implementation-map.yaml:L7746-L7754] |
| 368 | External implementation record | [docs/registry/implementation-map.yaml:L7755-L7763] |
| 369 | External implementation record | [docs/registry/implementation-map.yaml:L7764-L7776] |
| 370 | External implementation record | [docs/registry/implementation-map.yaml:L7777-L7785] |
| 371 | External implementation record | [docs/registry/implementation-map.yaml:L7786-L7794] |
| 372 | External implementation record | [docs/registry/implementation-map.yaml:L7795-L7803] |
| 373 | External implementation record | [docs/registry/implementation-map.yaml:L7804-L7812] |
| 374 | External implementation record | [docs/registry/implementation-map.yaml:L7813-L7821] |
| 375 | External implementation record | [docs/registry/implementation-map.yaml:L7822-L7830] |
| 376 | External implementation record | [docs/registry/implementation-map.yaml:L7831-L7839] |
| 377 | External implementation record | [docs/registry/implementation-map.yaml:L7840-L7851] |
| 378 | External implementation record | [docs/registry/implementation-map.yaml:L7852-L7860] |
| 379 | External implementation record | [docs/registry/implementation-map.yaml:L7861-L7873] |
| 380 | External implementation record | [docs/registry/implementation-map.yaml:L7874-L7886] |
| 381 | External implementation record | [docs/registry/implementation-map.yaml:L7887-L7895] |
| 382 | External implementation record | [docs/registry/implementation-map.yaml:L7896-L7906] |
| 383 | External implementation record | [docs/registry/implementation-map.yaml:L7907-L7915] |
| 384 | External implementation record | [docs/registry/implementation-map.yaml:L7916-L7924] |
| 385 | External implementation record | [docs/registry/implementation-map.yaml:L7925-L7937] |
| 386 | External implementation record | [docs/registry/implementation-map.yaml:L7938-L7946] |
| 387 | External implementation record | [docs/registry/implementation-map.yaml:L7947-L7959] |
| 388 | External implementation record | [docs/registry/implementation-map.yaml:L7960-L7970] |
| 389 | External implementation record | [docs/registry/implementation-map.yaml:L7971-L7979] |
| 390 | External implementation record | [docs/registry/implementation-map.yaml:L7980-L7988] |
| 391 | External implementation record | [docs/registry/implementation-map.yaml:L7989-L7997] |
| 392 | External implementation record | [docs/registry/implementation-map.yaml:L7998-L8006] |
| 393 | External implementation record | [docs/registry/implementation-map.yaml:L8007-L8015] |
| 394 | External implementation record | [docs/registry/implementation-map.yaml:L8016-L8024] |
| 395 | External implementation record | [docs/registry/implementation-map.yaml:L8025-L8033] |
| 396 | External implementation record | [docs/registry/implementation-map.yaml:L8034-L8042] |
| 397 | External implementation record | [docs/registry/implementation-map.yaml:L8043-L8051] |
| 398 | External implementation record | [docs/registry/implementation-map.yaml:L8052-L8060] |
| 399 | External implementation record | [docs/registry/implementation-map.yaml:L8061-L8069] |
| 400 | External implementation record | [docs/registry/implementation-map.yaml:L8070-L8078] |
| 401 | External implementation record | [docs/registry/implementation-map.yaml:L8079-L8087] |
| 402 | External implementation record | [docs/registry/implementation-map.yaml:L8088-L8096] |
| 403 | External implementation record | [docs/registry/implementation-map.yaml:L8097-L8105] |
| 404 | External implementation record | [docs/registry/implementation-map.yaml:L8106-L8114] |
| 405 | External implementation record | [docs/registry/implementation-map.yaml:L8115-L8123] |
| 406 | External implementation record | [docs/registry/implementation-map.yaml:L8124-L8134] |
| 407 | External implementation record | [docs/registry/implementation-map.yaml:L8135-L8143] |
| 408 | External implementation record | [docs/registry/implementation-map.yaml:L8144-L8152] |
| 409 | External implementation record | [docs/registry/implementation-map.yaml:L8153-L8161] |
| 410 | External implementation record | [docs/registry/implementation-map.yaml:L8162-L8170] |
| 411 | External implementation record | [docs/registry/implementation-map.yaml:L8171-L8179] |
| 412 | External implementation record | [docs/registry/implementation-map.yaml:L8180-L8188] |
| 413 | External implementation record | [docs/registry/implementation-map.yaml:L8189-L8197] |
| 414 | External implementation record | [docs/registry/implementation-map.yaml:L8198-L8206] |
| 415 | External implementation record | [docs/registry/implementation-map.yaml:L8207-L8215] |
| 416 | External implementation record | [docs/registry/implementation-map.yaml:L8216-L8224] |
| 417 | External implementation record | [docs/registry/implementation-map.yaml:L8225-L8233] |
| 418 | External implementation record | [docs/registry/implementation-map.yaml:L8234-L8242] |
| 419 | External implementation record | [docs/registry/implementation-map.yaml:L8243-L8251] |
| 420 | External implementation record | [docs/registry/implementation-map.yaml:L8252-L8260] |
| 421 | External implementation record | [docs/registry/implementation-map.yaml:L8261-L8269] |
| 422 | External implementation record | [docs/registry/implementation-map.yaml:L8270-L8278] |
| 423 | External implementation record | [docs/registry/implementation-map.yaml:L8279-L8287] |
| 424 | External implementation record | [docs/registry/implementation-map.yaml:L8288-L8296] |
| 425 | External implementation record | [docs/registry/implementation-map.yaml:L8297-L8305] |
| 426 | External implementation record | [docs/registry/implementation-map.yaml:L8306-L8314] |
| 427 | External implementation record | [docs/registry/implementation-map.yaml:L8315-L8323] |
| 428 | External implementation record | [docs/registry/implementation-map.yaml:L8324-L8332] |
| 429 | External implementation record | [docs/registry/implementation-map.yaml:L8333-L8341] |
| 430 | External implementation record | [docs/registry/implementation-map.yaml:L8342-L8350] |
| 431 | External implementation record | [docs/registry/implementation-map.yaml:L8351-L8359] |
| 432 | External implementation record | [docs/registry/implementation-map.yaml:L8360-L8368] |
| 433 | External implementation record | [docs/registry/implementation-map.yaml:L8369-L8377] |
| 434 | External implementation record | [docs/registry/implementation-map.yaml:L8378-L8386] |
| 435 | External implementation record | [docs/registry/implementation-map.yaml:L8387-L8395] |
| 436 | External implementation record | [docs/registry/implementation-map.yaml:L8396-L8404] |
| 437 | External implementation record | [docs/registry/implementation-map.yaml:L8405-L8413] |
| 438 | External implementation record | [docs/registry/implementation-map.yaml:L8414-L8422] |
| 439 | External implementation record | [docs/registry/implementation-map.yaml:L8423-L8431] |
| 440 | External implementation record | [docs/registry/implementation-map.yaml:L8432-L8441] |

#### Search result and classification

The counted-family search used the map patterns verbatim as Python `re`, case-sensitive, over every package `.py` file at the header pin; no signal matched the package path case-insensitively, so none was excluded. Definitions were assigned by enclosing AST function/class, with remaining statements grouped once per module.

| Family | Definition hits | Classification and evidence |
| --- | --- | --- |
| Effective tension | `aggregate` module statements; `case_key` | key storage/grouping helpers, not independent calculation of tension. [src/digitalmodel/drilling_riser/postprocess/aggregate.py:L19-L20] [src/digitalmodel/drilling_riser/postprocess/aggregate.py:L132-L134] |
| Effective tension | `t_min`, `tension_setting_max`, `tensioner_stroke` | already inventoried; section 3 lists all three. [src/digitalmodel/drilling_riser/postprocess/checks.py:L172-L202] |
| Effective tension | `checks` module statements | descriptive text and dispatch mapping to listed functions, not a further calculation. [src/digitalmodel/drilling_riser/postprocess/checks.py:L1-L16] [src/digitalmodel/drilling_riser/postprocess/checks.py:L479-L496] |
| Text-model writers | `__init__` and `stress_range` module statements | docstrings describing input/result interfaces. [src/digitalmodel/drilling_riser/postprocess/__init__.py:L1-L4] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L1-L19] |
| Text-model writers | `sha256_file` | binary-file hash helper, no model writing; listed here. [src/digitalmodel/drilling_riser/postprocess/aggregate.py:L54-L59] |

Counted-family result: 0 further second implementations; nine definition hits in total, six for effective tension and three for text-model writers; the pass criterion is at most 1 further implementation across counted families.

#### Whole-repository follow-up

The supplemental repository search included tracked Python files under `scripts/`, source, tests, examples and documentation, looking for Gumbel fitting, stress-range calculation, pressure capacities and clearance; it does not alter the counted-family result.

| Method family needing map follow-up | Additional route or finding |
| --- | --- |
| Extreme-value statistics | `fit_gumbel` uses SciPy maximum likelihood; `process_multi_seed` uses moment-based estimates; neither delegates to this package's fit. [src/digitalmodel/orcaflex/postprocessor.py:L177-L213] [src/digitalmodel/orcaflex/postprocessor.py:L285-L317] |
| Pressure capacity | `ApiStd2rdStrategy._check_burst` and `_check_collapse` independently compute corresponding capacities, with their own factor handling; unit/basis equivalence is not asserted. [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L83-L135] |
| Pressure capacity in scripts | `APISTD2RDCalculation` computes burst and collapse terms itself. [scripts/python/digitalmodel/analysis/APIST2RD_Methods.py:L31-L44] |
| Stress range, geometry and clearance | dedicated families are absent from the current map; the two mapped families are broad signal matches, including extraction/merge labelled as model writers; issue pending (2). [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L52-L119] [src/digitalmodel/drilling_riser/postprocess/clearance.py:L13-L31] |

## 10. Coverage and open items

Coverage inventory: every Python file in the package and every Python test in `tests/drilling_riser/postprocess/` was read, including the package initialisers; no package subdirectories containing other code were present at the pin. The file inventory below records the coverage boundaries.

| Files read | Coverage |
| --- | --- |
| `__init__.py`, `aggregate.py`, `channels.py`, `checks.py`, `clearance.py`, `evaluate.py`, `extremes.py`, `pressure.py`, `station_geometry.py`, `stress_range.py` under the package | all code, docstrings and definitions; algorithms cited in sections 1–3 and 9 |
| `tests/drilling_riser/postprocess/__init__.py`, `test_aggregate.py`, `test_evaluate.py`, `test_extremes.py`, `test_station_geometry.py`, `test_summary_coverage.py`, `test_w510_significant_range.py`, `test_w5_additions.py` | all offline tests, including parameterised cases; 241 collected, 241 passed at BASE and after |
| `docs/drilling-riser-summary-coverage.html` | direct module documentation, full text; [docs/drilling-riser-summary-coverage.html:L8-L52] |
| `docs/reviews/issue-2327-station-geometry.html` | direct module documentation, full text; prior counts treated as historical, current tests recounted; [docs/reviews/issue-2327-station-geometry.html:L4-L12] |
| `docs/plans/issue-2327-station-geometry.html` | related geometry plan, full text; [docs/plans/issue-2327-station-geometry.html:L3-L7] |
| `src/digitalmodel/drilling_riser/global_model/docs/agent-pack.md` | sibling global-model pack references to postprocess, coverage and geometry; sibling code is outside this pack's scope; [src/digitalmodel/drilling_riser/global_model/docs/agent-pack.md:L189-L192] [src/digitalmodel/drilling_riser/global_model/docs/agent-pack.md:L2101-L2102] |
| `docs/registry/module-routing.yaml`; upstream channel/counter helpers; whole-repository method-search candidates | routing and dependencies cited above; no private data inspected |

Offline command form: `PYTHONPATH=src uv run --no-sources python -m pytest tests/drilling_riser/postprocess tests/drilling_riser/global_model/test_campaign.py -q -rA -p no:cacheprovider --basetemp=<scratch>`; no addopts override; both executions use BASE `da203103cb43722fc6b724c4c73ae74714eed2b6`, the second with the uncommitted documentation changes; package/test code equals the header pin. The caller test path was included in the same invocation. The original revision recorded the same module/caller counts at the header pin.

| Test file or selection | BASE collected / pass / fail / skip / NOT_RUN | After collected / pass / fail / skip / NOT_RUN |
| --- | --- | --- |
| `tests/drilling_riser/postprocess/test_aggregate.py` | 7 / 7 / 0 / 0 / 0 | 7 / 7 / 0 / 0 / 0 |
| `tests/drilling_riser/postprocess/test_evaluate.py` | 25 / 25 / 0 / 0 / 0 | 25 / 25 / 0 / 0 / 0 |
| `tests/drilling_riser/postprocess/test_extremes.py` | 8 / 8 / 0 / 0 / 0 | 8 / 8 / 0 / 0 / 0 |
| `tests/drilling_riser/postprocess/test_station_geometry.py` | 97 / 97 / 0 / 0 / 0 | 97 / 97 / 0 / 0 / 0 |
| `tests/drilling_riser/postprocess/test_summary_coverage.py` | 28 / 28 / 0 / 0 / 0 | 28 / 28 / 0 / 0 / 0 |
| `tests/drilling_riser/postprocess/test_w510_significant_range.py` | 60 / 60 / 0 / 0 / 0 | 60 / 60 / 0 / 0 / 0 |
| `tests/drilling_riser/postprocess/test_w5_additions.py` | 16 / 16 / 0 / 0 / 0 | 16 / 16 / 0 / 0 / 0 |
| `tests/drilling_riser/global_model/test_campaign.py` (caller) | 16 / 13 / 0 / 3 / 3 | 16 / 13 / 0 / 3 / 3 |
| `tests/contracts` plus `tests/docs/test_digitalmodel_routing_contract.py` (outside this package) | 465 / 464 / 1 / 0 / 0 | 465 / 464 / 1 / 0 / 0 |

Contracts-plus-routing command: `PYTHONPATH=src uv run --no-sources python -m pytest tests/contracts tests/docs/test_digitalmodel_routing_contract.py -q -rA -p no:cacheprovider --basetemp=<scratch>`; same BASE and uncommitted-after tree, no addopts override. The original record also had 465 collected, 464 passed and this one failure at the header code pin. The routing contract's 10 nodes pass before and after; the registry remains unchanged from BASE. The sole failure is outside the package: `tests/contracts/test_string_addressed_dependencies.py::test_the_three_known_defects_are_still_derived`, assertion `engine='xlsxwriter' (#1906) no longer derived — the scanner regressed`; the assertion and scanner file are unchanged on current main. [tests/contracts/test_string_addressed_dependencies.py:L620-L635]

Supplemental caller coverage: `tests/drilling_riser/global_model/test_campaign.py` was read and run before and after at BASE with unchanged package/test code: 16 collected, 13 passed, 3 solver-dependent cases skipped by their existing guards and recorded here as NOT_RUN with the exact reason `OrcFxAPI not available`; the module-presence guard does not import the native interface. [tests/drilling_riser/global_model/test_campaign.py:L104-L149] [src/digitalmodel/solvers/orcaflex/orcaflex_api.py:L198-L209]

| NOT_RUN test node ID | Reason |
| --- | --- |
| `tests/drilling_riser/global_model/test_campaign.py::test_seeded_statics_matches_direct_statics_on_the_fixed_base_model` | `OrcFxAPI not available`; requires native statics; [tests/drilling_riser/global_model/test_campaign.py:L107-L122] |
| `tests/drilling_riser/global_model/test_campaign.py::test_statics_settings_change_the_path_not_the_static_state` | `OrcFxAPI not available`; requires native statics; [tests/drilling_riser/global_model/test_campaign.py:L126-L144] |
| `tests/drilling_riser/global_model/test_campaign.py::test_runner_with_campaign_adapter_statics_modal_regular_and_proxies` | `OrcFxAPI not available`; requires native solver run; [tests/drilling_riser/global_model/test_campaign.py:L147-L177] |

The module and caller command used the form above with both test paths and separate BASE/after scratch basetemps; no solver-dependent test body ran. Supplementary assertions for PP14–PP17 use the existing synthetic fixtures with explicit relative tolerance 1e-9 (1), rather than relying on the tests' default approximate comparison: 12 numeric comparisons passed, with units, verdicts and station identities checked. The two empty-input exceptions in section 6 were also reproduced without a solver.

| Open item | Responsible role | Issue and evidence |
| --- | --- | --- |
| Standard provenance: the section-3 UNVERIFIED claim; licensed clause titles, formula applicability, 1993 versus 2017 edition selection and Citation sidecars; pressure examples remain regression until an independent expected value is established | calculations maintainer and family quality manager | issue pending (1); [src/digitalmodel/drilling_riser/postprocess/pressure.py:L1-L32] [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L37-L49] |
| Map follow-up: explicit extrema, pressure, stress-range, aggregation, station-geometry and clearance families, correct extraction/merge classification and inventory of additional repository routes | implementation-map maintainer | issue pending (2); section 9 inventories the currently counted families and supplemental candidates |
| Qualification: section-1 version/applicability UNVERIFIED claim; section-2 coordinate/unit-validation UNVERIFIED claim; section-8 appointments/log identifiers UNVERIFIED claim; tensioner mid-stroke/travel-ratio qualification; AQG r1 migration; real input load, statics, dynamics, conditional diffraction and signed report appendix records; production benchmark coverage beyond the two closed-form checks | family quality manager and riser maintainer | issue pending (3); [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L345-L392] [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L39-L59] |
| Check ownership: the sibling global-model pack must cross-reference PP15, PP05, PP10 and PP01 instead of independently owning the postprocess checks; overlapping incident/test records remain historical | riser documentation maintainer | issue pending (4); [src/digitalmodel/drilling_riser/global_model/docs/agent-pack.md:L189-L193] [src/digitalmodel/drilling_riser/global_model/docs/agent-pack.md:L213-L214] |
| Empty pressure-point/conductor input must return a controlled unevaluated result rather than escape as an exception | riser maintainer | issue pending (5); [src/digitalmodel/drilling_riser/postprocess/checks.py:L248-L263] [src/digitalmodel/drilling_riser/postprocess/checks.py:L446-L455] |
| Geometry topology, input parameters and bounded expansion | riser maintainer | #2329; [docs/reviews/issue-2327-station-geometry.html:L7-L7] |
| Caller reconciliation and summary-coverage adoption | downstream maintainer | #2277; [docs/drilling-riser-summary-coverage.html:L15-L23] [docs/drilling-riser-summary-coverage.html:L51-L52] |

The stress references use the 1993 edition, while repository citation metadata records first edition 1993 (reaffirmed 2001) and notes second edition 2017; applicability of these older locators remains an edition-selection defect, issue pending (1). [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L37-L49] [src/digitalmodel/riser_database/getters.py:L61-L69]
