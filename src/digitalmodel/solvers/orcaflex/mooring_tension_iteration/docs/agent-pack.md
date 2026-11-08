# Mooring tension iteration knowledge pack

| Header | Value |
| --- | --- |
| Module | `mooring_tension_iteration` |
| Repository | `digitalmodel` |
| Path | `src/digitalmodel/solvers/orcaflex/mooring_tension_iteration` |
| Pack revision | 0 |
| Code revision | `b47e42fbe7208c69b28dd436aa91fb129bc385f7` |
| Analysis family | OrcaFlex riser and mooring; AQG r0 section 10 |
| Quality role | Riser and mooring family quality manager; appointments unverified in section 8 |
| Status | DRAFT; independent clearance outstanding |
| Implementation-map ref commit | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` |
| Implementation-map data revision | `270c8759c374b67a7f032953e7fcdb14e0224461` |

Code citations resolve at the code revision; citations to `docs/registry/implementation-map.yaml` resolve at the separate map ref commit above.

## 1. Purpose and scope

`MooringTensionIterator` adjusts line section lengths against target tensions using SciPy, Newton iteration or an EA-based fallback. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L102-L141]

`load_model` delegates loading, calls model validation and backs up original line lengths before iteration. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L85-L100]

The real adapter calls `CalculateStatics` and reads effective tension and coordinates at end A; the mock adapter substitutes a length-dependent, iteration-dependent formula. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L165-L210] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L331-L364]

The routing registry classifies the containing `solvers` domain as beta and routes its tests to `tests/solvers/`. [docs/registry/module-routing.yaml:L477-L483]

UNVERIFIED: U1 — supported program versions, geometrical applicability, global frame, origin, rotation convention and model-unit compatibility have no demonstrated qualification; issue pending (1).

## 2. Inputs and outputs

Input constraints below describe implemented validation, rather than engineering acceptance limits. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L25-L32] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L43-L50] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L63-L71]

| Input | Unit | Allowed range and default | Route and evidence |
| --- | --- | --- | --- |
| Configuration | YAML mapping | File or dictionary; section may be nested below `orcaflex_analysis` | `IterationConfig.load_from_file`, `load_from_dict`, `_parse_config`; file uses `yaml.safe_load`. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L137-L154] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L177-L186] |
| `method` | identifier | Default `scipy`; exactly `newton_raphson` selects Newton; all other values select EA | No method enumeration in `_validate`. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L263-L282] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L125-L131] |
| `tolerance`, `max_iterations`, `damping_factor`, `min_tolerance` | %, count, fraction, kN | Defaults 1.0, 10, 0.8, 0.1; positive percentage, count at least 1, damping in (0, 1]; absolute tolerance is not checked | `ConvergenceSettings`. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L17-L32] |
| Jacobian settings | identifier, fraction, m, boolean | `finite_difference`, 0.01, 0.1, true; positive perturbations; method accepts `analytical` although `_compute_jacobian` uses differences | `JacobianSettings`, `_compute_jacobian`. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L35-L50] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L432-L489] |
| Target tensions | kN by code annotation | Positive per line; CSV `ObjectName,target_tension` or YAML `tensions`; only first group parsed | CSV column check then dictionary construction; no duplicate-row check. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L156-L175] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L208-L220] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L234-L261] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L63-L71] |
| `EA`, `sections`, length bounds | kN; m by perturbation/report convention | Positive EA; default EA 850000 and sections `[None]`; bounds optional, checked for order only when both truthy | Configuration defaults require a design basis before engineering use. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L54-L71] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L251-L260] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L126-L143] |
| Vessel restraint | names and degree identifiers | Defaults fixing enabled, empty name list, x/y/z/rx/ry/rz | Only enumerated identifiers are validated; the iterator passes the selected names to `fix_vessels`. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L74-L86] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L118-L123] |
| Model file and mock selection | path; boolean | `use_mock=False`; import failure automatically selects mock | Iterator documents `.sim` or `.dat`; mock loading does not read the supplied file. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L85-L100] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L120-L136] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L382-L407] |
| Output preferences | paths and booleans | Iteration saving true; CSV true; Excel false; PDF and PNG filenames configured | `OutputConfig` only creates an iteration directory; report generation separately writes text. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L89-L103] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L500-L544] |

| Output | Unit / format | Producer and meaning |
| --- | --- | --- |
| `IterationResult` | tensions kN by analyzer convention; lengths m by update convention; error %; elapsed s | Boolean convergence, iteration count, final dictionaries and convergence history; see stale-state limits in section 6. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L22-L39] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L363-L370] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L18-L25] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L126-L143] |
| `StaticResult` | adapter-provided tensions and positions | Real adapter uses end-A result variables without an explicit unit conversion; mock positions use its own depth/span. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L34-L40] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L178-L207] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L349-L364] |
| Tension analysis / snapshots | kN, % | `analyze_tensions`, `record_snapshot`, stored history; snapshot convergence uses default tolerances. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L100-L147] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L205-L238] |
| Export | CSV or Excel | `export_results` writes tension/error rows; unavailable target lines are replaced by zero. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L306-L345] |
| Report | text string, optionally written to path | `generate_report` concatenates summaries and history, regardless of configured PDF filename. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L500-L544] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L89-L97] |

## 3. Method and governing standards

The following are source-defined calculations, with no external engineering oracle asserted; they are unsuitable as independent hand-check references for their own output. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L183-L205] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L149-L194] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L143-L148]

| Definition | Implemented method and qualification boundary |
| --- | --- |
| `MockOrcaFlexAPI.create_model`, `load_model` | Creates dictionaries; loading creates four synthetic lines instead of parsing input. These are model-construction implementations included here for the map inventory. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L114-L136] |
| `MockOrcaFlexAPI.run_static_analysis` | Uses excess length, a sag floor, horizontal/vertical components and an iteration-dependent sine multiplier; reports convergence and a fixed iteration count. It is a test surrogate, not a catenary benchmark. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L165-L210] |
| `OrcaFlexAPI.run_static_analysis` | Calls `CalculateStatics`, then `check_statics`, then extracts end-A responses. Bare `OrcFxAPI` result selectors are inconsistent with the constructor's local import; see section 6. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L230-L239] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L331-L364] |
| `TensionAnalyzer.analyze_tensions`, `check_convergence` | Signed error is current minus target; percentage is 100 times error divided by target. Analysis uses 1.0% alone; convergence accepts either configured percentage or absolute tolerance. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L123-L145] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L149-L185] |
| `LinePropertyManager.calculate_stiffness_matrix` | Diagonal uses half EA/L, or fallback stiffness/length; off-diagonal coupling uses a fixed fraction. These are heuristic constants, not a standards-based stiffness reference. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L149-L194] |
| `LinePropertyManager.calculate_length_updates_ea` | Subtracts `(length / EA) * error`, floors numeric sections, then fills only the first variable section. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L196-L251] |
| `MooringTensionIterator._iterate_scipy` | Sorted line totals feed `fsolve`; objective scales sections, extracts tensions, fills missing tension with zero, stores snapshots; returned convergence combines tolerance with solver status. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L151-L250] |
| `MooringTensionIterator._iterate_newton_raphson` | Solves a regularized Jacobian system, falls back to pseudoinverse, applies damping and clamps each total change, then distributes sections. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L299-L355] |
| `MooringTensionIterator._iterate_ea_based` | Extracts, checks and records tensions, then updates lengths unless already converged. Final extraction is absent after the final update. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L381-L429] |
| `MooringTensionIterator._compute_jacobian` | Forward differences of tensions against length perturbations; restores lengths after each column, caches the matrix, and does not restore analyzer tensions. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L442-L498] |

UNVERIFIED: U2 — the accuracy and provenance of the mock catenary approximation, stiffness factors and EA update as engineering methods are not established by independent expected values; issue pending (2).

No governing engineering standard or edition is identified in these method bodies; AQG r0 sections 4–9 supply acceptance procedures rather than an implementation reference. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L165-L210] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L149-L251] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L143-L498]

## 4. Validated worked examples

Validated examples: 0; completed regression examples: 0. The three existing workflow candidates below carry no independent numerical oracle, and their input-presence guard covers three absent input files. [tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py:L15-L29] [tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py:L52-L111]

| Candidate | Expected result / tolerance basis | Existing test node ID | Last offline result |
| --- | --- | --- | --- |
| Workflow smoke | No numerical assertion; empty expected-result argument | `tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py::test_mooring_tension_iteration` | NOT_RUN (pytest SKIP before and after): reference input absent; #1318. [tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py:L15-L29] [tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py:L52-L71] |
| Single line | Boolean convergence and fewer than 10 iterations; no kN oracle or numerical tolerance | `tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py::test_single_line_iteration` | NOT_RUN (pytest SKIP before and after): reference input absent; #1318. [tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py:L73-L91] |
| Coupled lines | Boolean convergence and `all` over returned line rows; empty rows would satisfy `all` | `tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py::test_multi_line_coupling` | NOT_RUN (pytest SKIP before and after): reference input absent; #1318. [tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py:L93-L111] |

The import route is test → `engine` → `orcaflex` → `orcaflex_analysis` → `orcaflex_iterative_runs`, whose top-level solver import is guarded by `ImportError`; solver-free collection succeeded and the missing-input guard skipped all three cases. [tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py:L10-L29] [src/digitalmodel/engine.py:L29-L35] [src/digitalmodel/solvers/orcaflex/orcaflex.py:L6-L10] [src/digitalmodel/solvers/orcaflex/orcaflex_analysis.py:L1-L6] [src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py:L1-L10]

These candidates are NOT_RUN at the code pin both before and after documentation changes; no solver, licence or interface was queried. The missing inputs and dispatch gap remain tracked by #1318. [tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py:L15-L29]

## 5. QA gates

The family requires G0, G1, G2 and G3; G4 is conditional on imported data from a new diffraction run; G5 applies to reporting. (AQG r0, sections 9–10.)

The following are executable acceptance procedures for a future model, not claims of completed engineering gates. Only source inspection is available for the runtime procedures here; NOT_RUN means no program execution was attempted. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L230-L239] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L331-L364]

| Gate / check | Module-specific procedure | Numeric pass criterion and unit | Evidence / current result |
| --- | --- | --- | --- |
| G0 / C01 | Compare `IterationConfig` parsed targets, EA, section lengths, method and first group against the input manifest; check finite values, duplicates, units and positive lengths before model construction | 0 missing/duplicate/non-finite fields (count); all target tensions and EA > 0 kN; all resolved sections > 0 m; method in the 3 intended choices | Input audit; NOT_RUN, no input basis. Validation lacks several of these checks. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L17-L86] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L156-L175] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L177-L282] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L125-L131] |
| G0 / C02 | Inspect the three `_INPUT_FILES` and follow the `engine` dispatch route for this workflow | 0 absent input files (count); 1 matching dispatch arm (count) | Source/file audit finds 3 absent inputs and no dispatch arm; baseline #1318; this is not a passing workflow. [tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py:L15-L29] [src/digitalmodel/engine.py:L236-L842] |
| G1 / C03 | On a permitted installation, load through `OrcaFlexAPI.load_model` in the recorded target version; compare object names/counts to manifest, forbid mock substitution and inspect the end-A selector binding before extraction | 0 load errors/warnings (count); 0 unexpected mock selections (count); 0 unbound result selectors (count); object-count difference = 0 objects | Load/version/message record; NOT_RUN, licensed route prohibited. `OrcaFlexModelInterface.load_model` delegates to this adapter; demo loading in the exact target version is an alternative G1 route only. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L230-L253] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L349-L357] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L393-L407] (AQG r0, section 5.) |
| G2 / C04 | Compare `analyze_tensions`, `calculate_residuals`, `check_convergence`, snapshots and exported rows for the same complete target/current dictionaries; explicitly try missing lines and zero/non-finite targets on the permitted test route | Residual difference = 0 kN; percentage difference = 0 percentage points; 0 missing line rows (count); 0 false convergence flags (count); runtime percentage OR absolute tolerance must match configuration | Arithmetic and row-completeness table; NOT_RUN, executable unit coverage missing. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L123-L238] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L316-L345] |
| G2 / C05 | Re-extract statics at `IterationResult.final_lengths`; compare with `final_tensions`; perturb one line twice at the same baseline for `_compute_jacobian` and confirm full restoration before the next solve | 0 stale result/length pairs (count); tension mismatch = 0 kN for a deterministic test double; repeated Jacobian difference = 0 kN/m; length restoration error = 0 m | State-pair and Jacobian audit; NOT_RUN, no existing regression node. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L325-L370] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L403-L429] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L442-L498] |
| G2 / C06 | Compare each real adapter end-A tension against an independent low-load hand calculation with recorded geometry, weight and axial stiffness; retain static images and `check_statics` state evidence | Tension difference ≤ 5.0%; angle difference ≤ 1.0 degrees; touchdown-distance difference ≤ 5.0%; 0 unchecked components (count) | Hand calculation, images, component table; NOT_RUN, model and licensed solve absent; mock/stiffness routines are not the reference. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L331-L364] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L149-L194] (AQG r0, section 6.) |
| G2 / C07 | At each imposed offset, call the real model route used by `extract_mooring_tensions`, retain all target-line tensions and inspect convergence and monotonic restoring trends against the independent baseline | At least 6 offset steps (count); 0 nonconverged steps (count); 0 unplanned slack lines (count); first-step restoring-force difference ≤ 20.0% | Offset sweep and static views; NOT_RUN, no input configuration or licensed solve. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L82-L98] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L331-L364] (AQG r0, section 6.) |
| G3 / C08 | After the final lengths pass statics, run short dynamics in the exact target program using the production environment and time step, then compare every target-line tension to the retained static result | Reaches 1.0 s simulated time; 0 warnings/errors/non-finite responses (count); tension changes ≤ 25.0%; 0 new compression/slack cases (count) | Dynamic log, end image and timing; NOT_RUN, licensed route prohibited and this package exposes only statics. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L43-L89] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L331-L372] (AQG r0, section 7.) |
| G4 / C09 | For imported response data from a new diffraction run, attach the upstream signed record to the same loaded model; compare the data fingerprint rather than treating `StaticResult.positions` as diffraction evidence | 0 unmatched data fingerprints (count); 0 missing required upstream G4 records (count) | Conditional gate: NOT_APPLICABLE for the mock-only source review; a new diffraction dataset makes it applicable. No such input dataset was supplied. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L34-L40] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L120-L136] (AQG r0, sections 8 and 10.) |
| G5 / C10 | Compare `generate_report` output with the model's gate manifest: target version, sorted input hashes, hand calculation, sweep, components, images, deviations and independent signature | 0 missing evidence fields/records (count); 0 unmatched fingerprints (count) | Appendix completeness audit; NOT_RUN, no model records; text concatenation does not implement the complete appendix. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L500-L544] (AQG r0, section 9.) |
| G5 / C11 | Compare every `export_results` row and report convergence flag against `check_convergence` using the configured tolerances and complete target set | Row-count difference = 0 lines; tension discrepancy = 0 kN; 0 inconsistent convergence flags (count) | Export/report consistency table; NOT_RUN, no existing regression node. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L255-L304] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L306-L345] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L149-L185] |

## 6. Known failure modes and pitfalls

The sweep below records error handling, skipped records, empty/error returns, fallback values, numeric conversion, units and cache assumptions; benign state initialization is included to distinguish it from missing-data substitution.

sweep: 82 found

| ID | Symptom / cause at code pin | Detection and tracking |
| --- | --- | --- |
| S01 | Mock iteration counter starts at zero; benign state, later changes the response. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L105-L105] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S02 | Mock depth is fixed at 100.0 m. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L108-L108] | C06 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S03 | Mock submerged line weight is fixed at 1.0 kN/m. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L109-L109] | C06 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S04 | Mock horizontal span is fixed at 200.0 m. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L110-L110] | C06 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S05 | Default segmentation/type/connections do not establish a design basis. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L23-L26] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S06 | Total length silently excludes None sections. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L29-L31] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S07 | Mock load ignores file contents and creates four default lines. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L120-L136] | C03 is defined in section 5; no executable regression detection check exists, issue pending (8) |
| S08 | Mock sag floors at 0.1 m for short lines. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L186-L187] | C06 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S09 | Mock horizontal tension has a 1000.0 fallback on the nonpositive-sag branch. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L189-L189] | C06 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S10 | External line-name int conversion rejects names without a numeric suffix; no locale contract. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L194-L194] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S11 | Mock response varies with iteration and always returns converged plus fixed iteration count. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L195-L205] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S12 | ImportError is logged then re-raised; warning text suggests substitution. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L237-L239] | C03 is defined in section 5; no executable regression detection check exists, issue pending (8) |
| S13 | Bare OrcFxAPI is not bound at module scope by the constructor-local import. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L349-L357] | C03 is defined in section 5; no executable regression detection check exists, issue pending (1) |
| S14 | ImportError substitutes mock even when use_mock was false. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L393-L403] | C03 is defined in section 5; no executable regression detection check exists, issue pending (8) |
| S15 | Missing DOFs default to all six restraints. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L430-L434] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S16 | Unconverged StaticResult logs a warning but validation returns true. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L455-L458] | C03 is defined in section 5; no executable regression detection check exists, issue pending (8) |
| S17 | Catch-all converts model errors to false; original exception is retained only in log. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L460-L462] | C03 is defined in section 5; no executable regression detection check exists, issue pending (8) |
| S18 | Default percentage tolerance is 1.0%; repeated at parsing line 192. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L20-L20] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S19 | Default iteration budget is 10; repeated at parsing line 193. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L21-L21] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S20 | Default damping is 0.8; repeated at parsing line 194. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L22-L22] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S21 | Default absolute tolerance is 0.1 kN and is not validated; repeated at parsing line 195. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L23-L32] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S22 | Default difference method, perturbation fraction/length and parallel flag are accepted; parsed again at lines 202–205. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L38-L50] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S23 | EA and targets assume kN; truthiness skips zero bounds and sections lack length validation. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L57-L71] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S24 | Restraint defaults enable fixing with empty names and all DOFs; parsed again at lines 214–216. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L77-L86] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S25 | Output defaults imply saved iterations and PDF/plot/CSV choices; directory creation alone is implemented here. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L92-L103] | C10 is defined in section 5; no executable regression detection check exists, issue pending (7) |
| S26 | Method defaults to scipy; empty config dictionary bypasses loading/validation. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L130-L135] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S27 | Safe YAML loading does not refuse duplicate keys. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L144-L148] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S28 | CSV parsing has implicit locale/type assumptions; duplicate names collapse in dict(zip). [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L170-L175] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S29 | Absent configuration section defaults empty; unknown method is not rejected here. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L180-L186] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S30 | Only the first configuration group is used. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L209-L220] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S31 | Parsed output defaults repeat settings without connecting them to report execution. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L223-L232] | C10 is defined in section 5; no executable regression detection check exists, issue pending (7) |
| S32 | Empty target mapping when pretension type is absent/unsupported eventually fails no-lines validation. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L236-L245] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S33 | Missing line properties supply EA 850000, one variable section and absent bounds. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L248-L260] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S34 | Unknown line lookup returns None rather than raising. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L286-L291] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S35 | Tension fields are annotated kN and percentage without unit conversion. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L18-L25] | C01 is defined in section 5; no executable regression detection check exists, issue pending (1) |
| S36 | Unconverged statics still replace stored tensions. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L94-L98] | C04 is defined in section 5; no executable regression detection check exists, issue pending (8) |
| S37 | Missing target line is skipped from analysis rows. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L123-L126] | C04 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S38 | Zero target becomes infinity; per-row convergence hardcodes 1.0%. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L130-L134] | C04 is defined in section 5; no executable regression detection check exists, issue pending (7) |
| S39 | Empty tension mappings return false/infinity; default tolerances differ from configurable reporting. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L149-L163] | C04 is defined in section 5; no executable regression detection check exists, issue pending (7) |
| S40 | Maximum error starts at zero and all_converged true before line coverage is checked. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L165-L166] | C04 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S41 | Missing current target sets false and continues without adding a missing-line error magnitude. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L168-L171] | C04 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S42 | Zero target becomes infinity; either percentage or absolute criterion suffices. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L175-L182] | C04 is defined in section 5; no executable regression detection check exists, issue pending (7) |
| S43 | Residual vector substitutes zero for absent current tension. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L198-L201] | C04 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S44 | Snapshot omits missing target lines, substitutes zero percentage for zero target and uses default convergence. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L219-L226] | C04 is defined in section 5; no executable regression detection check exists, issue pending (7) |
| S45 | No history returns two empty lists; benign absence can leave a plot empty. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L247-L248] | C10 is defined in section 5; no executable regression detection check exists, issue pending (7) |
| S46 | No data returns an ordinary report string. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L266-L268] | C10 is defined in section 5; no executable regression detection check exists, issue pending (7) |
| S47 | Report overall/default and per-row criteria can disagree with configured tolerances. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L274-L294] | C11 is defined in section 5; no executable regression detection check exists, issue pending (7) |
| S48 | Empty export logs warning then returns without writing evidence. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L316-L318] | C11 is defined in section 5; no executable regression detection check exists, issue pending (7) |
| S49 | Export supplies zero for absent current tension and zero-target percentage; 1.0% criterion hardcoded. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L322-L334] | C11 is defined in section 5; no executable regression detection check exists, issue pending (7) |
| S50 | Tension and EA initialize None; no unit conversion in state holder. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L24-L25] | C01 is defined in section 5; no executable regression detection check exists, issue pending (1) |
| S51 | Backup sums only non-None sections and optionally attaches configured EA. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L76-L87] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S52 | Bounds scale all truthy sections and turn zeros into None; total zero can divide by zero. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L126-L136] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S53 | State updates precede adapter mutation; adapter error can leave inconsistent state. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L141-L146] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S54 | Missing properties substitute diagonal stiffness 1000.0. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L165-L167] | C06 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S55 | EA/L stiffness assumes valid nonzero total and uses a 0.5 catenary factor. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L175-L180] | C06 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S56 | Missing current state substitutes 200.0 m length. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L183-L184] | C06 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S57 | Fixed coupling factor is 0.05, not a derived interaction. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L188-L192] | C06 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S58 | EA update skips lines absent from current state. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L210-L211] | C05 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S59 | EA update skips lines without properties after warning. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L216-L218] | C05 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S60 | Numeric sections subtract EA error adjustment and floor at 0.1 m. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L232-L234] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S61 | Only first None is filled and variable length floors at 0.1 m. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L237-L247] | C05 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S62 | Safety limit defaults to 0.1 fraction and leaves unknown-state updates unchanged. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L253-L279] | C05 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S63 | No original state yields ordinary report text. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L313-L315] | C10 is defined in section 5; no executable regression detection check exists, issue pending (7) |
| S64 | Length report falls back to original state and zero percentage for zero original length. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L320-L324] | C11 is defined in section 5; no executable regression detection check exists, issue pending (7) |
| S65 | Counter zero and empty Jacobian cache initialize iteration state; no input fingerprint. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L78-L81] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S66 | Iteration resets count/history but leaves analyzer history and Jacobian cache intact. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L115-L116] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S67 | Unrecognized iteration method silently selects EA. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L125-L131] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S68 | Missing state yields 200.0 m initial guess before objective indexes that state. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L156-L161] | C01 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S69 | Objective uses scale 1.0 for nonpositive length and floors variable section at 0.1 m. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L174-L188] | C05 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S70 | SciPy objective substitutes zero for absent target-line tension. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L205-L208] | C04 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S71 | fsolve uses fixed xtol and evaluation budget derived from iteration budget. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L225-L230] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S72 | Execution time initializes zero before outer caller overwrites it; returned solution is not reapplied. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L242-L249] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S73 | Catch-all returns failed IterationResult from cached state with default convergence calculation. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L252-L266] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S74 | Fixed regularization then LinAlgError pseudoinverse fallback; rank deficiency is only logged. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L308-L319] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S75 | Newton clamps totals to 0.9/1.1, scale defaults 1.0, variable section floors 0.1 m; final update not re-solved. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L325-L355] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S76 | Newton final convergence/tensions use stored state, elapsed time initializes zero. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L358-L370] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S77 | EA errors omit non-target lines and final updated lengths are returned with older tensions; elapsed time initializes zero. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L403-L429] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S78 | Cached Jacobian reused by iteration index without baseline identity. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L442-L445] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S79 | Jacobian initializes zeros; empty line set creates empty cached matrix. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L448-L449] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S80 | Perturbation uses configured length floor and fills only first None; scale divides by current total. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L458-L478] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |
| S81 | Jacobian uses zero for missing perturbed and baseline tension. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L487-L489] | C05 is defined in section 5; no executable regression detection check exists, issue pending (6) |
| S82 | Restoring lengths does not restore analyzer tensions; matrix cached without input digest or empty-result rejection. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L492-L496] | C05 is defined in section 5; no executable regression detection check exists, issue pending (2) |

Additional confirmed workflow incident: #1318 records absent inputs and missing dispatch; C02 detects the missing files and dispatch arm, while no runnable convergence detection check exists in the guarded tests. [tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py:L15-L29] [tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py:L89-L111]

The historical #531 plan lists three failures in this test file, subsequently represented by the missing-input guard; it supplies no independent expected tensions. [docs/plans/2026-05-05-issue-531-9-pre-existing-test-failures.md:L9-L14] [tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py:L15-L29]

Sweep command, run over all six Python files:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|float\(|int\(|\.get\(|default|kN|meters|metres|percent|factor|cache' src/digitalmodel/solvers/orcaflex/mooring_tension_iteration
```

The numeric conversion of external line-name text is `int(line_name.replace(...))`; the `float('inf')` calls represent sentinels rather than locale parsing, and CSV values are read by pandas without an explicit locale specification. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L194-L199] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L131-L176] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L170-L175]

## 7. Lessons log

| Date | Change reference | Lesson and evidence | Pack change | Shared entry |
| --- | --- | --- | --- | --- |
| 2026-11-08 | Initial documentation | Automatic mock selection can substitute synthetic responses; a log warning is not a programme-level rejection. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L393-L403] | Separate mock evidence from engineering acceptance in sections 3–6 | Not allocated; issue pending (3) |

## 8. Ownership

Required review role: riser and mooring family quality manager; proposed deputy: another riser and mooring quality manager operating independently. The producer must not supply the independent gate signature. (AQG r0, section 11.)

UNVERIFIED: U3 — named appointments, deputy availability and shared-log registration are not evidenced in this package; the maintainer must assign them, issue pending (3).

Escalate engineering-method uncertainty and tolerance changes to the family quality manager; escalate cross-family diffraction evidence to the diffraction quality manager. (AQG r0, sections 8 and 11.)

## 9. Dependencies and other implementations

The package imports NumPy, SciPy optimization, pandas, PyYAML and loguru; the interface additionally delegates solver-state acceptance to `check_statics`. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L8-L14] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L8-L14] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L8-L19] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L8-L15]

The iterator wires `IterationConfig`, `LinePropertyManager`, `TensionAnalyzer` and `OrcaFlexModelInterface`; the analyzer obtains solver results through the interface, while report generation consumes analyzer and line-manager summaries. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L62-L75] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L82-L98] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L516-L534]

### Solver dependency occurrences

Every case-insensitive `OrcaFlex` or `OrcFxAPI` occurrence in package source is indexed below, including docstrings, type annotations, imports, logs, API construction and selectors; each is a text-only dependency observation, not a solver execution.

| Occurrence | Source location | Dependency observation |
| --- | --- | --- |
| 1 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/__init__.py:L2-L2] | Solver-specific source occurrence; read as text only |
| 2 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/__init__.py:L10-L10] | Solver-specific source occurrence; read as text only |
| 3 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L180-L180] | Solver-specific source occurrence; read as text only |
| 4 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L181-L181] | Solver-specific source occurrence; read as text only |
| 5 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L14-L14] | Solver-specific source occurrence; read as text only |
| 6 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L46-L46] | Solver-specific source occurrence; read as text only |
| 7 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L51-L51] | Solver-specific source occurrence; read as text only |
| 8 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L2-L2] | Solver-specific source occurrence; read as text only |
| 9 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L4-L4] | Solver-specific source occurrence; read as text only |
| 10 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L15-L15] | Solver-specific source occurrence; read as text only |
| 11 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L43-L43] | Solver-specific source occurrence; read as text only |
| 12 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L44-L44] | Solver-specific source occurrence; read as text only |
| 13 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L48-L48] | Solver-specific source occurrence; read as text only |
| 14 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L53-L53] | Solver-specific source occurrence; read as text only |
| 15 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L92-L92] | Solver-specific source occurrence; read as text only |
| 16 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L94-L94] | Solver-specific source occurrence; read as text only |
| 17 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L112-L112] | Solver-specific source occurrence; read as text only |
| 18 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L223-L223] | Solver-specific source occurrence; read as text only |
| 19 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L225-L225] | Solver-specific source occurrence; read as text only |
| 20 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L227-L227] | Solver-specific source occurrence; read as text only |
| 21 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L231-L231] | Solver-specific source occurrence; read as text only |
| 22 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L233-L233] | Solver-specific source occurrence; read as text only |
| 23 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L234-L234] | Solver-specific source occurrence; read as text only |
| 24 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L236-L236] | Solver-specific source occurrence; read as text only |
| 25 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L238-L238] | Solver-specific source occurrence; read as text only |
| 26 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L239-L239] | Solver-specific source occurrence; read as text only |
| 27 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L242-L242] | Solver-specific source occurrence; read as text only |
| 28 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L243-L243] | Solver-specific source occurrence; read as text only |
| 29 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L247-L247] | Solver-specific source occurrence; read as text only |
| 30 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L251-L251] | Solver-specific source occurrence; read as text only |
| 31 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L252-L252] | Solver-specific source occurrence; read as text only |
| 32 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L270-L270] | Solver-specific source occurrence; read as text only |
| 33 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L336-L336] | Solver-specific source occurrence; read as text only |
| 34 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L351-L351] | Solver-specific source occurrence; read as text only |
| 35 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L354-L354] | Solver-specific source occurrence; read as text only |
| 36 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L355-L355] | Solver-specific source occurrence; read as text only |
| 37 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L356-L356] | Solver-specific source occurrence; read as text only |
| 38 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L372-L372] | Solver-specific source occurrence; read as text only |
| 39 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L375-L375] | Solver-specific source occurrence; read as text only |
| 40 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L377-L377] | Solver-specific source occurrence; read as text only |
| 41 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L389-L389] | Solver-specific source occurrence; read as text only |
| 42 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L396-L396] | Solver-specific source occurrence; read as text only |
| 43 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L399-L399] | Solver-specific source occurrence; read as text only |
| 44 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L401-L401] | Solver-specific source occurrence; read as text only |
| 45 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L402-L402] | Solver-specific source occurrence; read as text only |
| 46 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L406-L406] | Solver-specific source occurrence; read as text only |
| 47 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L5-L5] | Solver-specific source occurrence; read as text only |
| 48 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L14-L14] | Solver-specific source occurrence; read as text only |
| 49 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L60-L60] | Solver-specific source occurrence; read as text only |
| 50 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L65-L65] | Solver-specific source occurrence; read as text only |
| 51 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L84-L84] | Solver-specific source occurrence; read as text only |
| 52 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L17-L17] | Solver-specific source occurrence; read as text only |
| 53 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L52-L52] | Solver-specific source occurrence; read as text only |
| 54 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L59-L59] | Solver-specific source occurrence; read as text only |
| 55 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L68-L68] | Solver-specific source occurrence; read as text only |
| 56 | [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L87-L87] | Solver-specific source occurrence; read as text only |

No target solver version is selected by the adapter constructor or model loader; supported-version qualification remains U1. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L230-L253]

### Implementation-map inventory

The designated map has 2 in-package rows for `orcaflex-text-model-writers` and 1 for `external-solver-batch-run-stacks`; both families are counted, in that order. The full inventories contain 440 and 137 rows respectively, including out-of-package implementations. Each row below is a stable ordinal within its family's `implementations` list, with an exact map location so the original path and symbol are recoverable without reproducing identifying discovery names.

| Map row | Location relative to package | Exact map record (path and symbol) |
| --- | --- | --- |
| W001 | outside package | [docs/registry/implementation-map.yaml:L4940-L4949] |
| W002 | outside package | [docs/registry/implementation-map.yaml:L4950-L4958] |
| W003 | outside package | [docs/registry/implementation-map.yaml:L4959-L4968] |
| W004 | outside package | [docs/registry/implementation-map.yaml:L4969-L4978] |
| W005 | outside package | [docs/registry/implementation-map.yaml:L4979-L4988] |
| W006 | outside package | [docs/registry/implementation-map.yaml:L4989-L4998] |
| W007 | outside package | [docs/registry/implementation-map.yaml:L4999-L5008] |
| W008 | outside package | [docs/registry/implementation-map.yaml:L5009-L5020] |
| W009 | outside package | [docs/registry/implementation-map.yaml:L5021-L5030] |
| W010 | outside package | [docs/registry/implementation-map.yaml:L5031-L5040] |
| W011 | outside package | [docs/registry/implementation-map.yaml:L5041-L5050] |
| W012 | outside package | [docs/registry/implementation-map.yaml:L5051-L5060] |
| W013 | outside package | [docs/registry/implementation-map.yaml:L5061-L5070] |
| W014 | outside package | [docs/registry/implementation-map.yaml:L5071-L5080] |
| W015 | outside package | [docs/registry/implementation-map.yaml:L5081-L5090] |
| W016 | outside package | [docs/registry/implementation-map.yaml:L5091-L5100] |
| W017 | outside package | [docs/registry/implementation-map.yaml:L5101-L5110] |
| W018 | outside package | [docs/registry/implementation-map.yaml:L5111-L5120] |
| W019 | outside package | [docs/registry/implementation-map.yaml:L5121-L5132] |
| W020 | outside package | [docs/registry/implementation-map.yaml:L5133-L5142] |
| W021 | outside package | [docs/registry/implementation-map.yaml:L5143-L5151] |
| W022 | outside package | [docs/registry/implementation-map.yaml:L5152-L5161] |
| W023 | outside package | [docs/registry/implementation-map.yaml:L5162-L5171] |
| W024 | outside package | [docs/registry/implementation-map.yaml:L5172-L5181] |
| W025 | outside package | [docs/registry/implementation-map.yaml:L5182-L5191] |
| W026 | outside package | [docs/registry/implementation-map.yaml:L5192-L5201] |
| W027 | outside package | [docs/registry/implementation-map.yaml:L5202-L5211] |
| W028 | outside package | [docs/registry/implementation-map.yaml:L5212-L5223] |
| W029 | outside package | [docs/registry/implementation-map.yaml:L5224-L5233] |
| W030 | outside package | [docs/registry/implementation-map.yaml:L5234-L5243] |
| W031 | outside package | [docs/registry/implementation-map.yaml:L5244-L5253] |
| W032 | outside package | [docs/registry/implementation-map.yaml:L5254-L5263] |
| W033 | outside package | [docs/registry/implementation-map.yaml:L5264-L5273] |
| W034 | outside package | [docs/registry/implementation-map.yaml:L5274-L5283] |
| W035 | outside package | [docs/registry/implementation-map.yaml:L5284-L5293] |
| W036 | outside package | [docs/registry/implementation-map.yaml:L5294-L5303] |
| W037 | outside package | [docs/registry/implementation-map.yaml:L5304-L5313] |
| W038 | outside package | [docs/registry/implementation-map.yaml:L5314-L5323] |
| W039 | outside package | [docs/registry/implementation-map.yaml:L5324-L5333] |
| W040 | outside package | [docs/registry/implementation-map.yaml:L5334-L5343] |
| W041 | outside package | [docs/registry/implementation-map.yaml:L5344-L5353] |
| W042 | outside package | [docs/registry/implementation-map.yaml:L5354-L5363] |
| W043 | outside package | [docs/registry/implementation-map.yaml:L5364-L5372] |
| W044 | outside package | [docs/registry/implementation-map.yaml:L5373-L5382] |
| W045 | outside package | [docs/registry/implementation-map.yaml:L5383-L5392] |
| W046 | outside package | [docs/registry/implementation-map.yaml:L5393-L5402] |
| W047 | outside package | [docs/registry/implementation-map.yaml:L5403-L5415] |
| W048 | outside package | [docs/registry/implementation-map.yaml:L5416-L5425] |
| W049 | outside package | [docs/registry/implementation-map.yaml:L5426-L5437] |
| W050 | outside package | [docs/registry/implementation-map.yaml:L5438-L5447] |
| W051 | outside package | [docs/registry/implementation-map.yaml:L5448-L5457] |
| W052 | outside package | [docs/registry/implementation-map.yaml:L5458-L5467] |
| W053 | outside package | [docs/registry/implementation-map.yaml:L5468-L5477] |
| W054 | outside package | [docs/registry/implementation-map.yaml:L5478-L5487] |
| W055 | outside package | [docs/registry/implementation-map.yaml:L5488-L5497] |
| W056 | outside package | [docs/registry/implementation-map.yaml:L5498-L5507] |
| W057 | outside package | [docs/registry/implementation-map.yaml:L5508-L5519] |
| W058 | outside package | [docs/registry/implementation-map.yaml:L5520-L5531] |
| W059 | outside package | [docs/registry/implementation-map.yaml:L5532-L5540] |
| W060 | outside package | [docs/registry/implementation-map.yaml:L5541-L5550] |
| W061 | outside package | [docs/registry/implementation-map.yaml:L5551-L5560] |
| W062 | outside package | [docs/registry/implementation-map.yaml:L5561-L5570] |
| W063 | outside package | [docs/registry/implementation-map.yaml:L5571-L5580] |
| W064 | outside package | [docs/registry/implementation-map.yaml:L5581-L5593] |
| W065 | outside package | [docs/registry/implementation-map.yaml:L5594-L5603] |
| W066 | outside package | [docs/registry/implementation-map.yaml:L5604-L5615] |
| W067 | outside package | [docs/registry/implementation-map.yaml:L5616-L5628] |
| W068 | outside package | [docs/registry/implementation-map.yaml:L5629-L5638] |
| W069 | outside package | [docs/registry/implementation-map.yaml:L5639-L5648] |
| W070 | outside package | [docs/registry/implementation-map.yaml:L5649-L5658] |
| W071 | outside package | [docs/registry/implementation-map.yaml:L5659-L5667] |
| W072 | outside package | [docs/registry/implementation-map.yaml:L5668-L5677] |
| W073 | outside package | [docs/registry/implementation-map.yaml:L5678-L5687] |
| W074 | outside package | [docs/registry/implementation-map.yaml:L5688-L5697] |
| W075 | outside package | [docs/registry/implementation-map.yaml:L5698-L5707] |
| W076 | outside package | [docs/registry/implementation-map.yaml:L5708-L5717] |
| W077 | outside package | [docs/registry/implementation-map.yaml:L5718-L5727] |
| W078 | outside package | [docs/registry/implementation-map.yaml:L5728-L5737] |
| W079 | outside package | [docs/registry/implementation-map.yaml:L5738-L5749] |
| W080 | outside package | [docs/registry/implementation-map.yaml:L5750-L5759] |
| W081 | outside package | [docs/registry/implementation-map.yaml:L5760-L5769] |
| W082 | outside package | [docs/registry/implementation-map.yaml:L5770-L5781] |
| W083 | outside package | [docs/registry/implementation-map.yaml:L5782-L5791] |
| W084 | outside package | [docs/registry/implementation-map.yaml:L5792-L5801] |
| W085 | outside package | [docs/registry/implementation-map.yaml:L5802-L5811] |
| W086 | outside package | [docs/registry/implementation-map.yaml:L5812-L5823] |
| W087 | outside package | [docs/registry/implementation-map.yaml:L5824-L5833] |
| W088 | outside package | [docs/registry/implementation-map.yaml:L5834-L5843] |
| W089 | outside package | [docs/registry/implementation-map.yaml:L5844-L5853] |
| W090 | outside package | [docs/registry/implementation-map.yaml:L5854-L5863] |
| W091 | outside package | [docs/registry/implementation-map.yaml:L5864-L5872] |
| W092 | outside package | [docs/registry/implementation-map.yaml:L5873-L5882] |
| W093 | outside package | [docs/registry/implementation-map.yaml:L5883-L5891] |
| W094 | outside package | [docs/registry/implementation-map.yaml:L5892-L5901] |
| W095 | outside package | [docs/registry/implementation-map.yaml:L5902-L5911] |
| W096 | outside package | [docs/registry/implementation-map.yaml:L5912-L5921] |
| W097 | outside package | [docs/registry/implementation-map.yaml:L5922-L5933] |
| W098 | outside package | [docs/registry/implementation-map.yaml:L5934-L5943] |
| W099 | outside package | [docs/registry/implementation-map.yaml:L5944-L5953] |
| W100 | outside package | [docs/registry/implementation-map.yaml:L5954-L5965] |
| W101 | outside package | [docs/registry/implementation-map.yaml:L5966-L5975] |
| W102 | outside package | [docs/registry/implementation-map.yaml:L5976-L5985] |
| W103 | outside package | [docs/registry/implementation-map.yaml:L5986-L5994] |
| W104 | outside package | [docs/registry/implementation-map.yaml:L5995-L6004] |
| W105 | outside package | [docs/registry/implementation-map.yaml:L6005-L6014] |
| W106 | outside package | [docs/registry/implementation-map.yaml:L6015-L6024] |
| W107 | outside package | [docs/registry/implementation-map.yaml:L6025-L6034] |
| W108 | outside package | [docs/registry/implementation-map.yaml:L6035-L6044] |
| W109 | outside package | [docs/registry/implementation-map.yaml:L6045-L6054] |
| W110 | outside package | [docs/registry/implementation-map.yaml:L6055-L6064] |
| W111 | outside package | [docs/registry/implementation-map.yaml:L6065-L6074] |
| W112 | outside package | [docs/registry/implementation-map.yaml:L6075-L6084] |
| W113 | outside package | [docs/registry/implementation-map.yaml:L6085-L6096] |
| W114 | outside package | [docs/registry/implementation-map.yaml:L6097-L6109] |
| W115 | outside package | [docs/registry/implementation-map.yaml:L6110-L6119] |
| W116 | outside package | [docs/registry/implementation-map.yaml:L6120-L6129] |
| W117 | outside package | [docs/registry/implementation-map.yaml:L6130-L6141] |
| W118 | outside package | [docs/registry/implementation-map.yaml:L6142-L6153] |
| W119 | outside package | [docs/registry/implementation-map.yaml:L6154-L6163] |
| W120 | outside package | [docs/registry/implementation-map.yaml:L6164-L6173] |
| W121 | outside package | [docs/registry/implementation-map.yaml:L6174-L6183] |
| W122 | outside package | [docs/registry/implementation-map.yaml:L6184-L6193] |
| W123 | outside package | [docs/registry/implementation-map.yaml:L6194-L6203] |
| W124 | outside package | [docs/registry/implementation-map.yaml:L6204-L6213] |
| W125 | outside package | [docs/registry/implementation-map.yaml:L6214-L6223] |
| W126 | outside package | [docs/registry/implementation-map.yaml:L6224-L6233] |
| W127 | outside package | [docs/registry/implementation-map.yaml:L6234-L6243] |
| W128 | outside package | [docs/registry/implementation-map.yaml:L6244-L6253] |
| W129 | outside package | [docs/registry/implementation-map.yaml:L6254-L6263] |
| W130 | outside package | [docs/registry/implementation-map.yaml:L6264-L6273] |
| W131 | outside package | [docs/registry/implementation-map.yaml:L6274-L6283] |
| W132 | outside package | [docs/registry/implementation-map.yaml:L6284-L6293] |
| W133 | outside package | [docs/registry/implementation-map.yaml:L6294-L6303] |
| W134 | outside package | [docs/registry/implementation-map.yaml:L6304-L6313] |
| W135 | outside package | [docs/registry/implementation-map.yaml:L6314-L6323] |
| W136 | outside package | [docs/registry/implementation-map.yaml:L6324-L6333] |
| W137 | outside package | [docs/registry/implementation-map.yaml:L6334-L6343] |
| W138 | outside package | [docs/registry/implementation-map.yaml:L6344-L6353] |
| W139 | outside package | [docs/registry/implementation-map.yaml:L6354-L6363] |
| W140 | outside package | [docs/registry/implementation-map.yaml:L6364-L6373] |
| W141 | outside package | [docs/registry/implementation-map.yaml:L6374-L6383] |
| W142 | outside package | [docs/registry/implementation-map.yaml:L6384-L6393] |
| W143 | outside package | [docs/registry/implementation-map.yaml:L6394-L6403] |
| W144 | outside package | [docs/registry/implementation-map.yaml:L6404-L6413] |
| W145 | outside package | [docs/registry/implementation-map.yaml:L6414-L6423] |
| W146 | outside package | [docs/registry/implementation-map.yaml:L6424-L6436] |
| W147 | outside package | [docs/registry/implementation-map.yaml:L6437-L6448] |
| W148 | outside package | [docs/registry/implementation-map.yaml:L6449-L6458] |
| W149 | outside package | [docs/registry/implementation-map.yaml:L6459-L6468] |
| W150 | outside package | [docs/registry/implementation-map.yaml:L6469-L6478] |
| W151 | outside package | [docs/registry/implementation-map.yaml:L6479-L6490] |
| W152 | outside package | [docs/registry/implementation-map.yaml:L6491-L6500] |
| W153 | outside package | [docs/registry/implementation-map.yaml:L6501-L6510] |
| W154 | outside package | [docs/registry/implementation-map.yaml:L6511-L6520] |
| W155 | outside package | [docs/registry/implementation-map.yaml:L6521-L6530] |
| W156 | outside package | [docs/registry/implementation-map.yaml:L6531-L6540] |
| W157 | outside package | [docs/registry/implementation-map.yaml:L6541-L6550] |
| W158 | outside package | [docs/registry/implementation-map.yaml:L6551-L6560] |
| W159 | outside package | [docs/registry/implementation-map.yaml:L6561-L6570] |
| W160 | outside package | [docs/registry/implementation-map.yaml:L6571-L6580] |
| W161 | outside package | [docs/registry/implementation-map.yaml:L6581-L6590] |
| W162 | outside package | [docs/registry/implementation-map.yaml:L6591-L6600] |
| W163 | outside package | [docs/registry/implementation-map.yaml:L6601-L6610] |
| W164 | outside package | [docs/registry/implementation-map.yaml:L6611-L6620] |
| W165 | outside package | [docs/registry/implementation-map.yaml:L6621-L6630] |
| W166 | outside package | [docs/registry/implementation-map.yaml:L6631-L6640] |
| W167 | outside package | [docs/registry/implementation-map.yaml:L6641-L6650] |
| W168 | outside package | [docs/registry/implementation-map.yaml:L6651-L6659] |
| W169 | outside package | [docs/registry/implementation-map.yaml:L6660-L6672] |
| W170 | outside package | [docs/registry/implementation-map.yaml:L6673-L6682] |
| W171 | outside package | [docs/registry/implementation-map.yaml:L6683-L6692] |
| W172 | outside package | [docs/registry/implementation-map.yaml:L6693-L6704] |
| W173 | outside package | [docs/registry/implementation-map.yaml:L6705-L6714] |
| W174 | outside package | [docs/registry/implementation-map.yaml:L6715-L6724] |
| W175 | outside package | [docs/registry/implementation-map.yaml:L6725-L6734] |
| W176 | outside package | [docs/registry/implementation-map.yaml:L6735-L6744] |
| W177 | outside package | [docs/registry/implementation-map.yaml:L6745-L6754] |
| W178 | outside package | [docs/registry/implementation-map.yaml:L6755-L6764] |
| W179 | outside package | [docs/registry/implementation-map.yaml:L6765-L6774] |
| W180 | outside package | [docs/registry/implementation-map.yaml:L6775-L6784] |
| W181 | outside package | [docs/registry/implementation-map.yaml:L6785-L6794] |
| W182 | outside package | [docs/registry/implementation-map.yaml:L6795-L6804] |
| W183 | outside package | [docs/registry/implementation-map.yaml:L6805-L6814] |
| W184 | outside package | [docs/registry/implementation-map.yaml:L6815-L6824] |
| W185 | outside package | [docs/registry/implementation-map.yaml:L6825-L6834] |
| W186 | outside package | [docs/registry/implementation-map.yaml:L6835-L6844] |
| W187 | outside package | [docs/registry/implementation-map.yaml:L6845-L6854] |
| W188 | outside package | [docs/registry/implementation-map.yaml:L6855-L6864] |
| W189 | outside package | [docs/registry/implementation-map.yaml:L6865-L6874] |
| W190 | outside package | [docs/registry/implementation-map.yaml:L6875-L6884] |
| W191 | outside package | [docs/registry/implementation-map.yaml:L6885-L6894] |
| W192 | outside package | [docs/registry/implementation-map.yaml:L6895-L6904] |
| W193 | outside package | [docs/registry/implementation-map.yaml:L6905-L6914] |
| W194 | outside package | [docs/registry/implementation-map.yaml:L6915-L6927] |
| W195 | outside package | [docs/registry/implementation-map.yaml:L6928-L6937] |
| W196 | outside package | [docs/registry/implementation-map.yaml:L6938-L6947] |
| W197 | outside package | [docs/registry/implementation-map.yaml:L6948-L6957] |
| W198 | outside package | [docs/registry/implementation-map.yaml:L6958-L6967] |
| W199 | outside package | [docs/registry/implementation-map.yaml:L6968-L6977] |
| W200 | outside package | [docs/registry/implementation-map.yaml:L6978-L6987] |
| W201 | outside package | [docs/registry/implementation-map.yaml:L6988-L6997] |
| W202 | outside package | [docs/registry/implementation-map.yaml:L6998-L7007] |
| W203 | outside package | [docs/registry/implementation-map.yaml:L7008-L7017] |
| W204 | outside package | [docs/registry/implementation-map.yaml:L7018-L7031] |
| W205 | outside package | [docs/registry/implementation-map.yaml:L7032-L7041] |
| W206 | outside package | [docs/registry/implementation-map.yaml:L7042-L7051] |
| W207 | outside package | [docs/registry/implementation-map.yaml:L7052-L7061] |
| W208 | outside package | [docs/registry/implementation-map.yaml:L7062-L7071] |
| W209 | outside package | [docs/registry/implementation-map.yaml:L7072-L7081] |
| W210 | outside package | [docs/registry/implementation-map.yaml:L7082-L7091] |
| W211 | outside package | [docs/registry/implementation-map.yaml:L7092-L7101] |
| W212 | outside package | [docs/registry/implementation-map.yaml:L7102-L7111] |
| W213 | outside package | [docs/registry/implementation-map.yaml:L7112-L7121] |
| W214 | outside package | [docs/registry/implementation-map.yaml:L7122-L7131] |
| W215 | outside package | [docs/registry/implementation-map.yaml:L7132-L7141] |
| W216 | outside package | [docs/registry/implementation-map.yaml:L7142-L7153] |
| W217 | outside package | [docs/registry/implementation-map.yaml:L7154-L7163] |
| W218 | outside package | [docs/registry/implementation-map.yaml:L7164-L7176] |
| W219 | outside package | [docs/registry/implementation-map.yaml:L7177-L7186] |
| W220 | outside package | [docs/registry/implementation-map.yaml:L7187-L7196] |
| W221 | outside package | [docs/registry/implementation-map.yaml:L7197-L7207] |
| W222 | outside package | [docs/registry/implementation-map.yaml:L7208-L7217] |
| W223 | outside package | [docs/registry/implementation-map.yaml:L7218-L7227] |
| W224 | outside package | [docs/registry/implementation-map.yaml:L7228-L7237] |
| W225 | outside package | [docs/registry/implementation-map.yaml:L7238-L7247] |
| W226 | outside package | [docs/registry/implementation-map.yaml:L7248-L7257] |
| W227 | outside package | [docs/registry/implementation-map.yaml:L7258-L7269] |
| W228 | outside package | [docs/registry/implementation-map.yaml:L7270-L7279] |
| W229 | outside package | [docs/registry/implementation-map.yaml:L7280-L7289] |
| W230 | outside package | [docs/registry/implementation-map.yaml:L7290-L7299] |
| W231 | outside package | [docs/registry/implementation-map.yaml:L7300-L7309] |
| W232 | outside package | [docs/registry/implementation-map.yaml:L7310-L7319] |
| W233 | outside package | [docs/registry/implementation-map.yaml:L7320-L7329] |
| W234 | outside package | [docs/registry/implementation-map.yaml:L7330-L7339] |
| W235 | outside package | [docs/registry/implementation-map.yaml:L7340-L7349] |
| W236 | outside package | [docs/registry/implementation-map.yaml:L7350-L7359] |
| W237 | outside package | [docs/registry/implementation-map.yaml:L7360-L7369] |
| W238 | outside package | [docs/registry/implementation-map.yaml:L7370-L7379] |
| W239 | outside package | [docs/registry/implementation-map.yaml:L7380-L7389] |
| W240 | outside package | [docs/registry/implementation-map.yaml:L7390-L7399] |
| W241 | outside package | [docs/registry/implementation-map.yaml:L7400-L7409] |
| W242 | outside package | [docs/registry/implementation-map.yaml:L7410-L7419] |
| W243 | outside package | [docs/registry/implementation-map.yaml:L7420-L7429] |
| W244 | outside package | [docs/registry/implementation-map.yaml:L7430-L7439] |
| W245 | outside package | [docs/registry/implementation-map.yaml:L7440-L7449] |
| W246 | outside package | [docs/registry/implementation-map.yaml:L7450-L7459] |
| W247 | outside package | [docs/registry/implementation-map.yaml:L7460-L7469] |
| W248 | outside package | [docs/registry/implementation-map.yaml:L7470-L7479] |
| W249 | outside package | [docs/registry/implementation-map.yaml:L7480-L7489] |
| W250 | outside package | [docs/registry/implementation-map.yaml:L7490-L7499] |
| W251 | in package | [docs/registry/implementation-map.yaml:L7500-L7509] |
| W252 | in package | [docs/registry/implementation-map.yaml:L7510-L7519] |
| W253 | outside package | [docs/registry/implementation-map.yaml:L7520-L7529] |
| W254 | outside package | [docs/registry/implementation-map.yaml:L7530-L7539] |
| W255 | outside package | [docs/registry/implementation-map.yaml:L7540-L7551] |
| W256 | outside package | [docs/registry/implementation-map.yaml:L7552-L7561] |
| W257 | outside package | [docs/registry/implementation-map.yaml:L7562-L7571] |
| W258 | outside package | [docs/registry/implementation-map.yaml:L7572-L7581] |
| W259 | outside package | [docs/registry/implementation-map.yaml:L7582-L7591] |
| W260 | outside package | [docs/registry/implementation-map.yaml:L7592-L7603] |
| W261 | outside package | [docs/registry/implementation-map.yaml:L7604-L7613] |
| W262 | outside package | [docs/registry/implementation-map.yaml:L7614-L7626] |
| W263 | outside package | [docs/registry/implementation-map.yaml:L7627-L7636] |
| W264 | outside package | [docs/registry/implementation-map.yaml:L7637-L7646] |
| W265 | outside package | [docs/registry/implementation-map.yaml:L7647-L7656] |
| W266 | outside package | [docs/registry/implementation-map.yaml:L7657-L7666] |
| W267 | outside package | [docs/registry/implementation-map.yaml:L7667-L7676] |
| W268 | outside package | [docs/registry/implementation-map.yaml:L7677-L7686] |
| W269 | outside package | [docs/registry/implementation-map.yaml:L7687-L7696] |
| W270 | outside package | [docs/registry/implementation-map.yaml:L7697-L7706] |
| W271 | outside package | [docs/registry/implementation-map.yaml:L7707-L7716] |
| W272 | outside package | [docs/registry/implementation-map.yaml:L7717-L7729] |
| W273 | outside package | [docs/registry/implementation-map.yaml:L7730-L7739] |
| W274 | outside package | [docs/registry/implementation-map.yaml:L7740-L7749] |
| W275 | outside package | [docs/registry/implementation-map.yaml:L7750-L7762] |
| W276 | outside package | [docs/registry/implementation-map.yaml:L7763-L7774] |
| W277 | outside package | [docs/registry/implementation-map.yaml:L7775-L7784] |
| W278 | outside package | [docs/registry/implementation-map.yaml:L7785-L7794] |
| W279 | outside package | [docs/registry/implementation-map.yaml:L7795-L7804] |
| W280 | outside package | [docs/registry/implementation-map.yaml:L7805-L7816] |
| W281 | outside package | [docs/registry/implementation-map.yaml:L7817-L7826] |
| W282 | outside package | [docs/registry/implementation-map.yaml:L7827-L7836] |
| W283 | outside package | [docs/registry/implementation-map.yaml:L7837-L7846] |
| W284 | outside package | [docs/registry/implementation-map.yaml:L7847-L7856] |
| W285 | outside package | [docs/registry/implementation-map.yaml:L7857-L7866] |
| W286 | outside package | [docs/registry/implementation-map.yaml:L7867-L7878] |
| W287 | outside package | [docs/registry/implementation-map.yaml:L7879-L7888] |
| W288 | outside package | [docs/registry/implementation-map.yaml:L7889-L7898] |
| W289 | outside package | [docs/registry/implementation-map.yaml:L7899-L7908] |
| W290 | outside package | [docs/registry/implementation-map.yaml:L7909-L7921] |
| W291 | outside package | [docs/registry/implementation-map.yaml:L7922-L7931] |
| W292 | outside package | [docs/registry/implementation-map.yaml:L7932-L7941] |
| W293 | outside package | [docs/registry/implementation-map.yaml:L7942-L7951] |
| W294 | outside package | [docs/registry/implementation-map.yaml:L7952-L7961] |
| W295 | outside package | [docs/registry/implementation-map.yaml:L7962-L7971] |
| W296 | outside package | [docs/registry/implementation-map.yaml:L7972-L7981] |
| W297 | outside package | [docs/registry/implementation-map.yaml:L7982-L7991] |
| W298 | outside package | [docs/registry/implementation-map.yaml:L7992-L8001] |
| W299 | outside package | [docs/registry/implementation-map.yaml:L8002-L8011] |
| W300 | outside package | [docs/registry/implementation-map.yaml:L8012-L8021] |
| W301 | outside package | [docs/registry/implementation-map.yaml:L8022-L8031] |
| W302 | outside package | [docs/registry/implementation-map.yaml:L8032-L8043] |
| W303 | outside package | [docs/registry/implementation-map.yaml:L8044-L8053] |
| W304 | outside package | [docs/registry/implementation-map.yaml:L8054-L8063] |
| W305 | outside package | [docs/registry/implementation-map.yaml:L8064-L8073] |
| W306 | outside package | [docs/registry/implementation-map.yaml:L8074-L8083] |
| W307 | outside package | [docs/registry/implementation-map.yaml:L8084-L8093] |
| W308 | outside package | [docs/registry/implementation-map.yaml:L8094-L8103] |
| W309 | outside package | [docs/registry/implementation-map.yaml:L8104-L8113] |
| W310 | outside package | [docs/registry/implementation-map.yaml:L8114-L8123] |
| W311 | outside package | [docs/registry/implementation-map.yaml:L8124-L8133] |
| W312 | outside package | [docs/registry/implementation-map.yaml:L8134-L8143] |
| W313 | outside package | [docs/registry/implementation-map.yaml:L8144-L8155] |
| W314 | outside package | [docs/registry/implementation-map.yaml:L8156-L8165] |
| W315 | outside package | [docs/registry/implementation-map.yaml:L8166-L8175] |
| W316 | outside package | [docs/registry/implementation-map.yaml:L8176-L8185] |
| W317 | outside package | [docs/registry/implementation-map.yaml:L8186-L8195] |
| W318 | outside package | [docs/registry/implementation-map.yaml:L8196-L8205] |
| W319 | outside package | [docs/registry/implementation-map.yaml:L8206-L8215] |
| W320 | outside package | [docs/registry/implementation-map.yaml:L8216-L8227] |
| W321 | outside package | [docs/registry/implementation-map.yaml:L8228-L8237] |
| W322 | outside package | [docs/registry/implementation-map.yaml:L8238-L8247] |
| W323 | outside package | [docs/registry/implementation-map.yaml:L8248-L8257] |
| W324 | outside package | [docs/registry/implementation-map.yaml:L8258-L8267] |
| W325 | outside package | [docs/registry/implementation-map.yaml:L8268-L8279] |
| W326 | outside package | [docs/registry/implementation-map.yaml:L8280-L8290] |
| W327 | outside package | [docs/registry/implementation-map.yaml:L8291-L8300] |
| W328 | outside package | [docs/registry/implementation-map.yaml:L8301-L8310] |
| W329 | outside package | [docs/registry/implementation-map.yaml:L8311-L8323] |
| W330 | outside package | [docs/registry/implementation-map.yaml:L8324-L8333] |
| W331 | outside package | [docs/registry/implementation-map.yaml:L8334-L8343] |
| W332 | outside package | [docs/registry/implementation-map.yaml:L8344-L8353] |
| W333 | outside package | [docs/registry/implementation-map.yaml:L8354-L8363] |
| W334 | outside package | [docs/registry/implementation-map.yaml:L8364-L8373] |
| W335 | outside package | [docs/registry/implementation-map.yaml:L8374-L8383] |
| W336 | outside package | [docs/registry/implementation-map.yaml:L8384-L8393] |
| W337 | outside package | [docs/registry/implementation-map.yaml:L8394-L8403] |
| W338 | outside package | [docs/registry/implementation-map.yaml:L8404-L8413] |
| W339 | outside package | [docs/registry/implementation-map.yaml:L8414-L8423] |
| W340 | outside package | [docs/registry/implementation-map.yaml:L8424-L8435] |
| W341 | outside package | [docs/registry/implementation-map.yaml:L8436-L8445] |
| W342 | outside package | [docs/registry/implementation-map.yaml:L8446-L8457] |
| W343 | outside package | [docs/registry/implementation-map.yaml:L8458-L8470] |
| W344 | outside package | [docs/registry/implementation-map.yaml:L8471-L8480] |
| W345 | outside package | [docs/registry/implementation-map.yaml:L8481-L8492] |
| W346 | outside package | [docs/registry/implementation-map.yaml:L8493-L8505] |
| W347 | outside package | [docs/registry/implementation-map.yaml:L8506-L8517] |
| W348 | outside package | [docs/registry/implementation-map.yaml:L8518-L8527] |
| W349 | outside package | [docs/registry/implementation-map.yaml:L8528-L8537] |
| W350 | outside package | [docs/registry/implementation-map.yaml:L8538-L8547] |
| W351 | outside package | [docs/registry/implementation-map.yaml:L8548-L8560] |
| W352 | outside package | [docs/registry/implementation-map.yaml:L8561-L8572] |
| W353 | outside package | [docs/registry/implementation-map.yaml:L8573-L8582] |
| W354 | outside package | [docs/registry/implementation-map.yaml:L8583-L8596] |
| W355 | outside package | [docs/registry/implementation-map.yaml:L8597-L8608] |
| W356 | outside package | [docs/registry/implementation-map.yaml:L8609-L8619] |
| W357 | outside package | [docs/registry/implementation-map.yaml:L8620-L8632] |
| W358 | outside package | [docs/registry/implementation-map.yaml:L8633-L8647] |
| W359 | outside package | [docs/registry/implementation-map.yaml:L8648-L8657] |
| W360 | outside package | [docs/registry/implementation-map.yaml:L8658-L8667] |
| W361 | outside package | [docs/registry/implementation-map.yaml:L8668-L8677] |
| W362 | outside package | [docs/registry/implementation-map.yaml:L8678-L8687] |
| W363 | outside package | [docs/registry/implementation-map.yaml:L8688-L8701] |
| W364 | outside package | [docs/registry/implementation-map.yaml:L8702-L8714] |
| W365 | outside package | [docs/registry/implementation-map.yaml:L8715-L8724] |
| W366 | outside package | [docs/registry/implementation-map.yaml:L8725-L8739] |
| W367 | outside package | [docs/registry/implementation-map.yaml:L8740-L8749] |
| W368 | outside package | [docs/registry/implementation-map.yaml:L8750-L8759] |
| W369 | outside package | [docs/registry/implementation-map.yaml:L8760-L8773] |
| W370 | outside package | [docs/registry/implementation-map.yaml:L8774-L8783] |
| W371 | outside package | [docs/registry/implementation-map.yaml:L8784-L8793] |
| W372 | outside package | [docs/registry/implementation-map.yaml:L8794-L8803] |
| W373 | outside package | [docs/registry/implementation-map.yaml:L8804-L8813] |
| W374 | outside package | [docs/registry/implementation-map.yaml:L8814-L8823] |
| W375 | outside package | [docs/registry/implementation-map.yaml:L8824-L8833] |
| W376 | outside package | [docs/registry/implementation-map.yaml:L8834-L8843] |
| W377 | outside package | [docs/registry/implementation-map.yaml:L8844-L8856] |
| W378 | outside package | [docs/registry/implementation-map.yaml:L8857-L8866] |
| W379 | outside package | [docs/registry/implementation-map.yaml:L8867-L8876] |
| W380 | outside package | [docs/registry/implementation-map.yaml:L8877-L8886] |
| W381 | outside package | [docs/registry/implementation-map.yaml:L8887-L8896] |
| W382 | outside package | [docs/registry/implementation-map.yaml:L8897-L8908] |
| W383 | outside package | [docs/registry/implementation-map.yaml:L8909-L8918] |
| W384 | outside package | [docs/registry/implementation-map.yaml:L8919-L8928] |
| W385 | outside package | [docs/registry/implementation-map.yaml:L8929-L8938] |
| W386 | outside package | [docs/registry/implementation-map.yaml:L8939-L8948] |
| W387 | outside package | [docs/registry/implementation-map.yaml:L8949-L8958] |
| W388 | outside package | [docs/registry/implementation-map.yaml:L8959-L8970] |
| W389 | outside package | [docs/registry/implementation-map.yaml:L8971-L8980] |
| W390 | outside package | [docs/registry/implementation-map.yaml:L8981-L8990] |
| W391 | outside package | [docs/registry/implementation-map.yaml:L8991-L9000] |
| W392 | outside package | [docs/registry/implementation-map.yaml:L9001-L9010] |
| W393 | outside package | [docs/registry/implementation-map.yaml:L9011-L9020] |
| W394 | outside package | [docs/registry/implementation-map.yaml:L9021-L9030] |
| W395 | outside package | [docs/registry/implementation-map.yaml:L9031-L9040] |
| W396 | outside package | [docs/registry/implementation-map.yaml:L9041-L9050] |
| W397 | outside package | [docs/registry/implementation-map.yaml:L9051-L9060] |
| W398 | outside package | [docs/registry/implementation-map.yaml:L9061-L9070] |
| W399 | outside package | [docs/registry/implementation-map.yaml:L9071-L9080] |
| W400 | outside package | [docs/registry/implementation-map.yaml:L9081-L9090] |
| W401 | outside package | [docs/registry/implementation-map.yaml:L9091-L9100] |
| W402 | outside package | [docs/registry/implementation-map.yaml:L9101-L9110] |
| W403 | outside package | [docs/registry/implementation-map.yaml:L9111-L9120] |
| W404 | outside package | [docs/registry/implementation-map.yaml:L9121-L9130] |
| W405 | outside package | [docs/registry/implementation-map.yaml:L9131-L9140] |
| W406 | outside package | [docs/registry/implementation-map.yaml:L9141-L9152] |
| W407 | outside package | [docs/registry/implementation-map.yaml:L9153-L9162] |
| W408 | outside package | [docs/registry/implementation-map.yaml:L9163-L9172] |
| W409 | outside package | [docs/registry/implementation-map.yaml:L9173-L9182] |
| W410 | outside package | [docs/registry/implementation-map.yaml:L9183-L9192] |
| W411 | outside package | [docs/registry/implementation-map.yaml:L9193-L9202] |
| W412 | outside package | [docs/registry/implementation-map.yaml:L9203-L9212] |
| W413 | outside package | [docs/registry/implementation-map.yaml:L9213-L9222] |
| W414 | outside package | [docs/registry/implementation-map.yaml:L9223-L9232] |
| W415 | outside package | [docs/registry/implementation-map.yaml:L9233-L9242] |
| W416 | outside package | [docs/registry/implementation-map.yaml:L9243-L9252] |
| W417 | outside package | [docs/registry/implementation-map.yaml:L9253-L9262] |
| W418 | outside package | [docs/registry/implementation-map.yaml:L9263-L9272] |
| W419 | outside package | [docs/registry/implementation-map.yaml:L9273-L9282] |
| W420 | outside package | [docs/registry/implementation-map.yaml:L9283-L9292] |
| W421 | outside package | [docs/registry/implementation-map.yaml:L9293-L9302] |
| W422 | outside package | [docs/registry/implementation-map.yaml:L9303-L9312] |
| W423 | outside package | [docs/registry/implementation-map.yaml:L9313-L9322] |
| W424 | outside package | [docs/registry/implementation-map.yaml:L9323-L9332] |
| W425 | outside package | [docs/registry/implementation-map.yaml:L9333-L9342] |
| W426 | outside package | [docs/registry/implementation-map.yaml:L9343-L9352] |
| W427 | outside package | [docs/registry/implementation-map.yaml:L9353-L9362] |
| W428 | outside package | [docs/registry/implementation-map.yaml:L9363-L9372] |
| W429 | outside package | [docs/registry/implementation-map.yaml:L9373-L9382] |
| W430 | outside package | [docs/registry/implementation-map.yaml:L9383-L9392] |
| W431 | outside package | [docs/registry/implementation-map.yaml:L9393-L9402] |
| W432 | outside package | [docs/registry/implementation-map.yaml:L9403-L9412] |
| W433 | outside package | [docs/registry/implementation-map.yaml:L9413-L9422] |
| W434 | outside package | [docs/registry/implementation-map.yaml:L9423-L9432] |
| W435 | outside package | [docs/registry/implementation-map.yaml:L9433-L9442] |
| W436 | outside package | [docs/registry/implementation-map.yaml:L9443-L9452] |
| W437 | outside package | [docs/registry/implementation-map.yaml:L9453-L9462] |
| W438 | outside package | [docs/registry/implementation-map.yaml:L9463-L9472] |
| W439 | outside package | [docs/registry/implementation-map.yaml:L9473-L9482] |
| W440 | outside package | [docs/registry/implementation-map.yaml:L9483-L9493] |
| B001 | outside package | [docs/registry/implementation-map.yaml:L24511-L24520] |
| B002 | outside package | [docs/registry/implementation-map.yaml:L24521-L24530] |
| B003 | outside package | [docs/registry/implementation-map.yaml:L24531-L24540] |
| B004 | outside package | [docs/registry/implementation-map.yaml:L24541-L24550] |
| B005 | outside package | [docs/registry/implementation-map.yaml:L24551-L24560] |
| B006 | outside package | [docs/registry/implementation-map.yaml:L24561-L24570] |
| B007 | outside package | [docs/registry/implementation-map.yaml:L24571-L24580] |
| B008 | outside package | [docs/registry/implementation-map.yaml:L24581-L24590] |
| B009 | outside package | [docs/registry/implementation-map.yaml:L24591-L24600] |
| B010 | outside package | [docs/registry/implementation-map.yaml:L24601-L24610] |
| B011 | outside package | [docs/registry/implementation-map.yaml:L24611-L24620] |
| B012 | outside package | [docs/registry/implementation-map.yaml:L24621-L24630] |
| B013 | outside package | [docs/registry/implementation-map.yaml:L24631-L24640] |
| B014 | outside package | [docs/registry/implementation-map.yaml:L24641-L24650] |
| B015 | outside package | [docs/registry/implementation-map.yaml:L24651-L24660] |
| B016 | outside package | [docs/registry/implementation-map.yaml:L24661-L24670] |
| B017 | outside package | [docs/registry/implementation-map.yaml:L24671-L24680] |
| B018 | outside package | [docs/registry/implementation-map.yaml:L24681-L24690] |
| B019 | outside package | [docs/registry/implementation-map.yaml:L24691-L24700] |
| B020 | outside package | [docs/registry/implementation-map.yaml:L24701-L24712] |
| B021 | outside package | [docs/registry/implementation-map.yaml:L24713-L24722] |
| B022 | outside package | [docs/registry/implementation-map.yaml:L24723-L24732] |
| B023 | outside package | [docs/registry/implementation-map.yaml:L24733-L24742] |
| B024 | outside package | [docs/registry/implementation-map.yaml:L24743-L24751] |
| B025 | outside package | [docs/registry/implementation-map.yaml:L24752-L24760] |
| B026 | outside package | [docs/registry/implementation-map.yaml:L24761-L24769] |
| B027 | outside package | [docs/registry/implementation-map.yaml:L24770-L24778] |
| B028 | outside package | [docs/registry/implementation-map.yaml:L24779-L24787] |
| B029 | outside package | [docs/registry/implementation-map.yaml:L24788-L24796] |
| B030 | outside package | [docs/registry/implementation-map.yaml:L24797-L24805] |
| B031 | outside package | [docs/registry/implementation-map.yaml:L24806-L24814] |
| B032 | outside package | [docs/registry/implementation-map.yaml:L24815-L24823] |
| B033 | outside package | [docs/registry/implementation-map.yaml:L24824-L24832] |
| B034 | outside package | [docs/registry/implementation-map.yaml:L24833-L24841] |
| B035 | outside package | [docs/registry/implementation-map.yaml:L24842-L24850] |
| B036 | outside package | [docs/registry/implementation-map.yaml:L24851-L24859] |
| B037 | outside package | [docs/registry/implementation-map.yaml:L24860-L24868] |
| B038 | outside package | [docs/registry/implementation-map.yaml:L24869-L24877] |
| B039 | outside package | [docs/registry/implementation-map.yaml:L24878-L24886] |
| B040 | outside package | [docs/registry/implementation-map.yaml:L24887-L24895] |
| B041 | outside package | [docs/registry/implementation-map.yaml:L24896-L24904] |
| B042 | outside package | [docs/registry/implementation-map.yaml:L24905-L24913] |
| B043 | outside package | [docs/registry/implementation-map.yaml:L24914-L24922] |
| B044 | outside package | [docs/registry/implementation-map.yaml:L24923-L24932] |
| B045 | outside package | [docs/registry/implementation-map.yaml:L24933-L24942] |
| B046 | outside package | [docs/registry/implementation-map.yaml:L24943-L24952] |
| B047 | outside package | [docs/registry/implementation-map.yaml:L24953-L24962] |
| B048 | outside package | [docs/registry/implementation-map.yaml:L24963-L24974] |
| B049 | outside package | [docs/registry/implementation-map.yaml:L24975-L24984] |
| B050 | outside package | [docs/registry/implementation-map.yaml:L24985-L24997] |
| B051 | outside package | [docs/registry/implementation-map.yaml:L24998-L25007] |
| B052 | outside package | [docs/registry/implementation-map.yaml:L25008-L25017] |
| B053 | outside package | [docs/registry/implementation-map.yaml:L25018-L25027] |
| B054 | outside package | [docs/registry/implementation-map.yaml:L25028-L25037] |
| B055 | outside package | [docs/registry/implementation-map.yaml:L25038-L25047] |
| B056 | outside package | [docs/registry/implementation-map.yaml:L25048-L25057] |
| B057 | outside package | [docs/registry/implementation-map.yaml:L25058-L25067] |
| B058 | outside package | [docs/registry/implementation-map.yaml:L25068-L25079] |
| B059 | outside package | [docs/registry/implementation-map.yaml:L25080-L25089] |
| B060 | outside package | [docs/registry/implementation-map.yaml:L25090-L25099] |
| B061 | outside package | [docs/registry/implementation-map.yaml:L25100-L25109] |
| B062 | outside package | [docs/registry/implementation-map.yaml:L25110-L25119] |
| B063 | outside package | [docs/registry/implementation-map.yaml:L25120-L25129] |
| B064 | outside package | [docs/registry/implementation-map.yaml:L25130-L25139] |
| B065 | in package | [docs/registry/implementation-map.yaml:L25140-L25149] |
| B066 | outside package | [docs/registry/implementation-map.yaml:L25150-L25159] |
| B067 | outside package | [docs/registry/implementation-map.yaml:L25160-L25169] |
| B068 | outside package | [docs/registry/implementation-map.yaml:L25170-L25179] |
| B069 | outside package | [docs/registry/implementation-map.yaml:L25180-L25189] |
| B070 | outside package | [docs/registry/implementation-map.yaml:L25190-L25199] |
| B071 | outside package | [docs/registry/implementation-map.yaml:L25200-L25209] |
| B072 | outside package | [docs/registry/implementation-map.yaml:L25210-L25219] |
| B073 | outside package | [docs/registry/implementation-map.yaml:L25220-L25229] |
| B074 | outside package | [docs/registry/implementation-map.yaml:L25230-L25239] |
| B075 | outside package | [docs/registry/implementation-map.yaml:L25240-L25249] |
| B076 | outside package | [docs/registry/implementation-map.yaml:L25250-L25259] |
| B077 | outside package | [docs/registry/implementation-map.yaml:L25260-L25269] |
| B078 | outside package | [docs/registry/implementation-map.yaml:L25270-L25279] |
| B079 | outside package | [docs/registry/implementation-map.yaml:L25280-L25289] |
| B080 | outside package | [docs/registry/implementation-map.yaml:L25290-L25299] |
| B081 | outside package | [docs/registry/implementation-map.yaml:L25300-L25309] |
| B082 | outside package | [docs/registry/implementation-map.yaml:L25310-L25319] |
| B083 | outside package | [docs/registry/implementation-map.yaml:L25320-L25329] |
| B084 | outside package | [docs/registry/implementation-map.yaml:L25330-L25339] |
| B085 | outside package | [docs/registry/implementation-map.yaml:L25340-L25349] |
| B086 | outside package | [docs/registry/implementation-map.yaml:L25350-L25359] |
| B087 | outside package | [docs/registry/implementation-map.yaml:L25360-L25369] |
| B088 | outside package | [docs/registry/implementation-map.yaml:L25370-L25379] |
| B089 | outside package | [docs/registry/implementation-map.yaml:L25380-L25389] |
| B090 | outside package | [docs/registry/implementation-map.yaml:L25390-L25399] |
| B091 | outside package | [docs/registry/implementation-map.yaml:L25400-L25409] |
| B092 | outside package | [docs/registry/implementation-map.yaml:L25410-L25419] |
| B093 | outside package | [docs/registry/implementation-map.yaml:L25420-L25429] |
| B094 | outside package | [docs/registry/implementation-map.yaml:L25430-L25439] |
| B095 | outside package | [docs/registry/implementation-map.yaml:L25440-L25455] |
| B096 | outside package | [docs/registry/implementation-map.yaml:L25456-L25465] |
| B097 | outside package | [docs/registry/implementation-map.yaml:L25466-L25475] |
| B098 | outside package | [docs/registry/implementation-map.yaml:L25476-L25485] |
| B099 | outside package | [docs/registry/implementation-map.yaml:L25486-L25495] |
| B100 | outside package | [docs/registry/implementation-map.yaml:L25496-L25505] |
| B101 | outside package | [docs/registry/implementation-map.yaml:L25506-L25518] |
| B102 | outside package | [docs/registry/implementation-map.yaml:L25519-L25528] |
| B103 | outside package | [docs/registry/implementation-map.yaml:L25529-L25538] |
| B104 | outside package | [docs/registry/implementation-map.yaml:L25539-L25548] |
| B105 | outside package | [docs/registry/implementation-map.yaml:L25549-L25558] |
| B106 | outside package | [docs/registry/implementation-map.yaml:L25559-L25568] |
| B107 | outside package | [docs/registry/implementation-map.yaml:L25569-L25578] |
| B108 | outside package | [docs/registry/implementation-map.yaml:L25579-L25588] |
| B109 | outside package | [docs/registry/implementation-map.yaml:L25589-L25598] |
| B110 | outside package | [docs/registry/implementation-map.yaml:L25599-L25608] |
| B111 | outside package | [docs/registry/implementation-map.yaml:L25609-L25618] |
| B112 | outside package | [docs/registry/implementation-map.yaml:L25619-L25628] |
| B113 | outside package | [docs/registry/implementation-map.yaml:L25629-L25638] |
| B114 | outside package | [docs/registry/implementation-map.yaml:L25639-L25648] |
| B115 | outside package | [docs/registry/implementation-map.yaml:L25649-L25658] |
| B116 | outside package | [docs/registry/implementation-map.yaml:L25659-L25668] |
| B117 | outside package | [docs/registry/implementation-map.yaml:L25669-L25678] |
| B118 | outside package | [docs/registry/implementation-map.yaml:L25679-L25688] |
| B119 | outside package | [docs/registry/implementation-map.yaml:L25689-L25698] |
| B120 | outside package | [docs/registry/implementation-map.yaml:L25699-L25708] |
| B121 | outside package | [docs/registry/implementation-map.yaml:L25709-L25718] |
| B122 | outside package | [docs/registry/implementation-map.yaml:L25719-L25728] |
| B123 | outside package | [docs/registry/implementation-map.yaml:L25729-L25738] |
| B124 | outside package | [docs/registry/implementation-map.yaml:L25739-L25748] |
| B125 | outside package | [docs/registry/implementation-map.yaml:L25749-L25758] |
| B126 | outside package | [docs/registry/implementation-map.yaml:L25759-L25770] |
| B127 | outside package | [docs/registry/implementation-map.yaml:L25771-L25780] |
| B128 | outside package | [docs/registry/implementation-map.yaml:L25781-L25790] |
| B129 | outside package | [docs/registry/implementation-map.yaml:L25791-L25800] |
| B130 | outside package | [docs/registry/implementation-map.yaml:L25801-L25810] |
| B131 | outside package | [docs/registry/implementation-map.yaml:L25811-L25820] |
| B132 | outside package | [docs/registry/implementation-map.yaml:L25821-L25830] |
| B133 | outside package | [docs/registry/implementation-map.yaml:L25831-L25840] |
| B134 | outside package | [docs/registry/implementation-map.yaml:L25841-L25850] |
| B135 | outside package | [docs/registry/implementation-map.yaml:L25851-L25860] |
| B136 | outside package | [docs/registry/implementation-map.yaml:L25861-L25870] |
| B137 | outside package | [docs/registry/implementation-map.yaml:L25871-L25880] |

The in-package inventory entries are W251 (`MockOrcaFlexAPI.load_model`), W252 (`OrcaFlexAPI.save_model`), B065 (`OrcaFlexAPI.run_static_analysis`); the ordinal mapping comes from the designated map records above.

### Counted-family search and classification

Search all package `.py` files at the code pin using Python `re.search`, case-sensitive, with each map `search_signals[].pattern` verbatim; remove a signal if it matches `solvers/orcaflex/mooring_tension_iteration` under case-insensitive Python `re`. Assign each hit to the smallest enclosing function or class; otherwise to module-level statements. This excludes the vocabulary signal in both families and preserves the other patterns, including their escape sequences.

| Family | Matched enclosing definition | Classification and evidence |
| --- | --- | --- |
| Writers | `VesselConfig`, `IterationConfig.__init__`, `_parse_config` | Data holder and construction/parsing helpers; no model writer algorithm. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L74-L86] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L113-L135] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L177-L232] |
| Writers | `IterationConfig.load_from_file` | Reads configuration, delegates parsing and validation. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L137-L148] |
| Writers | `MooringTensionIterator.generate_report` | Serializes the existing summaries; not another model implementation. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L500-L544] |
| Writers | `OrcaFlexAPI.save_model` | Listed implementation; delegates `SaveData`. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L255-L261] |
| Writers | `OrcaFlexAPI.get_line_names`, `get_line_properties` | Read model metadata and hold properties; no independent model construction. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L263-L294] |
| Batch runs | `OrcaFlexAPI.run_static_analysis` | Listed implementation; solver launch and result extraction. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L331-L364] |

Exact grep outcome: 8 writer definitions and 1 batch-run definition; 0 further second implementations absent from sections 3 and 9. Thus the counted-family inventory/grep criterion passes; this does not qualify the engineering algorithms.

Whole-repository discovery, including `scripts/`, also searched the module identifier, EA-update name, target-pretension vocabulary and reference-script name; the existing mooring workflow independently adds EA-based length changes, whereas this package subtracts them, while the helper scripts supply test-input or baseline tooling. [src/digitalmodel/solvers/orcaflex/mooring.py:L304-L317] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L232-L239] [scripts/python/digitalmodel/tools/add_orcaflex_test_configs.py:L177-L190] [scripts/python/digitalmodel/tools/check_baseline_comparison.py:L90-L101]

Map follow-up: add the mock tension formula, EA length updates, heuristic stiffness, residual/convergence criteria and finite-difference tension Jacobian as explicit calculation families; these have no in-package implementation rows in the designated map, so they are not counted yet; issue pending (4). [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L165-L210] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L149-L251] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L123-L185] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L432-L498]

## 10. Coverage and open items

All six package Python files were read in full; no package-local README, tests or documentation subfolder existed at the code pin. [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/__init__.py:L1-L25] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/config.py:L1-L311] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L1-L462] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_analyzer.py:L1-L345] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/line_manager.py:L1-L334] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/tension_iterator.py:L1-L544]

| Read surface | Scope and boundary |
| --- | --- |
| Package | The six files cited above; no module code was changed |
| Tests | The three test nodes in section 4 and their parent fixture declarations; none imports this package directly. [tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py:L1-L114] [tests/solvers/orcaflex/conftest.py:L1-L67] |
| Local test documentation | `tests/solvers/orcaflex/mooring-tension-iteration/CLEANUP_SUMMARY.md`; historical retention assertions are not validation evidence. [tests/solvers/orcaflex/mooring-tension-iteration/CLEANUP_SUMMARY.md:L43-L64] |
| Related docs | `docs/plans/2026-05-05-issue-531-9-pre-existing-test-failures.md`; `docs/plans/2026-06-11-issue-700-ci-baseline-domain-gates.md`; `docs/plans/2026-08-05-issue-1631-licensed-lane-fail-closed.md`; historical test and mock-fallback leads only. [docs/plans/2026-05-05-issue-531-9-pre-existing-test-failures.md:L9-L29] [docs/plans/2026-06-11-issue-700-ci-baseline-domain-gates.md:L239-L241] [docs/plans/2026-08-05-issue-1631-licensed-lane-fail-closed.md:L116-L122] |
| Guide | `docs/domains/orcaflex/dynamic-analysis-guide.md` names a `batch_runner` route not exported here; documentation repair belongs to issue pending (5). [docs/domains/orcaflex/dynamic-analysis-guide.md:L297-L326] [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/__init__.py:L14-L25] |
| Audit inventories | `docs/registry/agent-pack-audit/production-nde-functions-2.json` and `docs/registry/agent-pack-audit/production-nde-functions-3.json` list package symbols as discovery metadata. [docs/registry/agent-pack-audit/production-nde-functions-2.json:L7269-L7365] [docs/registry/agent-pack-audit/production-nde-functions-3.json:L1220-L1316] |
| Relocation index | `docs/RELOCATED-DATA.md`; historical identifying artifact names are omitted; identifier cleanup is tracked by #2425. [docs/RELOCATED-DATA.md:L44-L48] |
| Collection history | `docs/plans/2026-08-05-issue-1977-norecursedirs-bare-basenames.html`; historical collection counts are not current module-test evidence. [docs/plans/2026-08-05-issue-1977-norecursedirs-bare-basenames.html:L75-L75] [docs/plans/2026-08-05-issue-1977-norecursedirs-bare-basenames.html:L143-L143] |
| Other discovery references | `docs/domains/cathodic_protection/cp_bug_fixes_summary.md`, `docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md` and `docs/domains/project-docs/test_performance_analysis.json`; old source/test references are leads rather than qualification. [docs/domains/cathodic_protection/cp_bug_fixes_summary.md:L476-L482] [docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md:L85-L89] [docs/domains/project-docs/test_performance_analysis.json:L157-L161] |
| Governing documents | AQG r0 and the lifecycle template were read; the calculation-citation contract was readable in the planning repository; no standard-derived constant is introduced by this documentation |

The routing-contract suite is supplementary documentation evidence, not a module regression example; the three module cases remain NOT_RUN as recorded in section 4. [tests/docs/test_digitalmodel_routing_contract.py:L92-L117] [tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py:L15-L29]

| Open item | Responsibility | Tracking |
| --- | --- | --- |
| U1: units, frame, supported versions, geometry and load applicability; bind correct result selectors | Maintainer with family quality manager | issue pending (1) |
| U2: independent numerical examples, mock/stiffness provenance, EA-update sign, final-state and Jacobian regression coverage | Maintainer with family quality manager | issue pending (2) |
| U3: appoint independent reviewer/deputy and allocate shared lesson entry | Maintainer | issue pending (3) |
| Expand calculation-family map and inspect the independent mooring length-update workflow | Maintainer | issue pending (4) |
| Reconcile the guide's missing batch-runner route and guide link | Maintainer | issue pending (5) |
| Clean up historical identifying artifact references | Maintainer | #2425 |
| Reject incomplete/non-finite/duplicate inputs; make target-line omission and fallback settings explicit | Maintainer | issue pending (6) |
| Keep report, snapshot and exported convergence consistent with configured tolerances; attach complete QA records | Maintainer | issue pending (7) |
| Prevent implicit mock substitution and acceptance of unconverged results | Maintainer | issue pending (8) |
| Restore the three absent workflow inputs and engine routing; no independent validated example currently executes | Maintainer | #1318 |

No private data or derived engineering values were copied; no private-sidecar entry is referenced because none is supplied for these source-only observations.
