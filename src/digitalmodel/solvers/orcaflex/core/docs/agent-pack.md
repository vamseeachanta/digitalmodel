# OrcaFlex core knowledge pack

| Header | Value |
| --- | --- |
| Module | solvers/orcaflex/core |
| Repository | digitalmodel |
| Package path | src/digitalmodel/solvers/orcaflex/core |
| Pack revision | 0 |
| Code revision | aebd7e4bc5c3dda5f7433e20e021dd58ac17d0b3 |
| Analysis family | OrcaFlex riser and mooring (MR) |
| Quality manager role | OrcaFlex riser and mooring quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The package exports interfaces, base components, a registry and exceptions; its declared version is `2.0.0`. [src/digitalmodel/solvers/orcaflex/core/__init__.py:L8-L61]

`AnalysisEngine` selects static, dynamic and iterative workflows; fatigue and installation select the dynamic workflow, while modal selects static, rather than implementing separate fatigue or modal calculations. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L391-L434]

The enclosing solver domain is recorded as beta; this is registry metadata, rather than evidence of physical validation. [docs/registry/module-routing.yaml:L458-L463]

This pack covers every Python file inside the header package, including migration and logging; adjacent generators, universal runners and results-analysis packages retain their own registry entries. [docs/registry/module-routing.yaml:L483-L506]

The real wrapper calls `LoadData`, `CalculateStatics`, `RunSimulation`, `SaveSimulation`, `SaveData` and `TimeHistory`; explicit mock execution supplies artificial behavior and refuses numerical extraction. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L284-L305] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L377-L403] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L454-L493] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L528-L547] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L594-L613]

## 2. Inputs and outputs

Input configuration is a Pydantic hierarchy with unknown fields forbidden and assignment validation enabled. [src/digitalmodel/solvers/orcaflex/core/configuration.py:L61-L72]

| Input | Unit and range | Default | Source |
| --- | --- | --- | --- |
| `file_management.input_directory`, `output_directory`, `input_files` | paths; dictionary of file-type lists | `.`, `./results`, empty mapping | [src/digitalmodel/solvers/orcaflex/core/configuration.py:L100-L125] |
| `analysis.analysis_type` | enum list: static, dynamic, modal, fatigue, installation, custom | static | [src/digitalmodel/solvers/orcaflex/core/configuration.py:L21-L28] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L227-L245] |
| `analysis.static.tolerance` | numerical convergence parameter; `0 < value <= 1`; physical normalization unspecified | `1e-6` | [src/digitalmodel/solvers/orcaflex/core/configuration.py:L148-L157] |
| `analysis.static.max_iterations`, `damping` | count > 0; dimensionless damping in [0, 1] | 100; 0.8 | [src/digitalmodel/solvers/orcaflex/core/configuration.py:L158-L165] |
| `analysis.dynamic.duration`, `time_step`, `ramp_time`, `log_interval` | s; first two and interval > 0; ramp >= 0 | 3600.0, 0.1, 0.0, 1.0 s | [src/digitalmodel/solvers/orcaflex/core/configuration.py:L172-L196] |
| `analysis.iteration.target_parameter`, `target_value`, `tolerance`, `step_size` | parameter name; unrestricted optional float; tolerance and step > 0; quantity units unspecified | None, None, 0.01, 0.1 | [src/digitalmodel/solvers/orcaflex/core/configuration.py:L199-L224] |
| `parallel.num_threads`, `timeout` | 1–128 workers; optional positive seconds | 30 workers; 3600.0 s | [src/digitalmodel/solvers/orcaflex/core/configuration.py:L267-L289] |
| Wrapper `file_path` | existing file with `.dat`, `.sim`, `.yml` or `.yaml` suffix | required argument | [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L256-L282] |
| Wrapper `use_mock` and environment opt-ins | boolean argument; opt-in text values `1`, `true`, `yes` | false; unset | [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L144-L186] |

| Output | Unit and content | Producer |
| --- | --- | --- |
| Static result mapping | convergence boolean; iteration count; tolerance; elapsed s; timestamp | [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L393-L416] |
| Dynamic result mapping | duration s; step s; elapsed s; simulated/wall-time ratio; timestamp | [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L484-L507] |
| `ModelResult.to_dict` | state, static/dynamic mappings, warnings, errors, metadata; no physical-unit schema | [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L111-L130] |
| Engine summary | workflow counts; elapsed s; raw workflow attribute dictionaries | [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L549-L563] |
| Real saved model/simulation | delegated native `.sim`, `.dat`, `.yml`, `.yaml` output | [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L540-L549] |
| Artificial saved artifact | sibling `_mock` directory; `.mock` filename infix; `Mock simulation for` body marker | [src/digitalmodel/solvers/orcaflex/core/mock_artifacts.py:L27-L62] |
| Framework configuration YAML | `dict(exclude_unset=True)` serialized with `yaml.dump`; this serializer is distinct from the native model save | [src/digitalmodel/solvers/orcaflex/core/configuration.py:L407-L423] |

UNVERIFIED U1: a package-wide physical unit system, coordinate origin, axis signs and phase convention for native files and `TimeHistory` results have not been established; the wrapper delegates these quantities without conversion; issue pending (7). [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L567-L573] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L608-L613]

## 3. Method and governing standards

| Method | Implementation and limitation | Governing reference |
| --- | --- | --- |
| Configuration merge | Recursively replace supplied nested values, then reconstruct the same model class. [src/digitalmodel/solvers/orcaflex/core/configuration.py:L74-L93] | Software algorithm; no engineering equation claimed. |
| Legacy conversion | Recognize `orcaflex`, `default`, `Analysis`; transfer selected settings with fallback values. [src/digitalmodel/solvers/orcaflex/core/configuration.py:L499-L552] | Software migration; inspect section 6 before use. |
| Static solve route | `run_static_analysis` calls `CalculateStatics` then `check_statics`; tolerance and iteration arguments are read and reported but no assignment to native settings occurs in the body. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L362-L416] | External solver implementation; no independent hand reference here. |
| Dynamic solve route | `run_dynamic_simulation` assigns stage duration and inner step, calls `RunSimulation`, then `check_simulation`. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L442-L507] | External solver implementation. |
| Iteration placeholder | `current_value = target_value * (0.9 + 0.2 * iteration / max_iterations)` replaces result extraction; no model parameter update follows. Do not use it as a physical reference. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L325-L356] | No physical derivation supplied; issue pending (3). |
| Step orchestration | `BaseWorkflow.execute` calls analyzer or processor with `None`; exceptions are recorded and raised. [src/digitalmodel/solvers/orcaflex/core/base_classes.py:L230-L267] | Software control flow. |
| Parallel orchestration | `AnalysisEngine.execute_parallel` schedules workflow calls and collects their results; it delegates numerical work to wrappers, with timeout limitations in section 6. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L463-L519] | Software scheduling; no additional physical solver. |
| Direct mock methods | `MockOrcaFlexModel.CalculateStatics` and `RunSimulation` sleep; `SaveData` and `SaveSimulation` touch empty files. They do not compute a model solution. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L645-L685] | Test placeholders; not physical references. |
| Registry factory | `get(name, component_type, ...)` instantiates through the registered factory and increments its counter. [src/digitalmodel/solvers/orcaflex/core/registry.py:L116-L158] | Software factory algorithm. |
| Mock provenance | `write_mock_artifact` constructs a marked path and writes its body. [src/digitalmodel/solvers/orcaflex/core/mock_artifacts.py:L38-L62] | Artifact contract, without engineering results. |

UNVERIFIED U2: an engineering standard, edition, clause and standards-derived calculation sidecar governing this framework have not been established; the inspected main path delegates calculation to the solver; issue pending (8). [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L377-L403] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L463-L482]

## 4. Validated worked examples

Validated engineering examples: **0**. The cases below assert refusal, explicit mock selection and artifact behavior; their expected values are software contracts, rather than independent physical benchmarks. [tests/solvers/orcaflex/test_mock_is_explicit.py:L137-L212]

All five regression cases ran at the code pin using `PYTHONPATH=src uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`, with Python 3.12.15 and pytest 8.4.2; BASE: 52 passed, 0 failed, 0 skipped; AFTER: 52 passed, 0 failed, 0 skipped. Results describe this documentation change, without a solver run. [tests/solvers/orcaflex/test_mock_is_explicit.py:L137-L212] [tests/solvers/orcaflex/test_optimized_parallel_contract.py:L1-L8]

| Regression case | Synthetic input and expected contract | Tolerance and unit | Existing test node ID | Offline result BASE / AFTER |
| --- | --- | --- | --- | --- |
| Missing binding refusal | Availability patched false; no mock opt-in; exactly one `LicenseError` with `NO_MODULE` | 0 unexpected outcomes, count | `tests/solvers/orcaflex/test_mock_is_explicit.py::test_model_interface_no_module_license_error_is_reachable` | PASS / PASS. [tests/solvers/orcaflex/test_mock_is_explicit.py:L140-L156] |
| Environment opt-in | Availability false; explicit environment switch; `use_mock` true | 0 mismatches, count | `tests/solvers/orcaflex/test_mock_is_explicit.py::test_model_interface_env_opt_in_still_yields_mock` | PASS / PASS. [tests/solvers/orcaflex/test_mock_is_explicit.py:L159-L167] |
| Argument opt-in | Availability false; `use_mock=True` retained | 0 mismatches, count | `tests/solvers/orcaflex/test_mock_is_explicit.py::test_model_interface_explicit_use_mock_still_yields_mock` | PASS / PASS. [tests/solvers/orcaflex/test_mock_is_explicit.py:L170-L176] |
| No artificial engineering results | Mock wrapper in static-complete state refuses tension extraction | 0 returned numerical arrays, count | `tests/solvers/orcaflex/test_mock_is_explicit.py::test_mock_extract_results_does_not_return_random_numbers` | PASS / PASS. [tests/solvers/orcaflex/test_mock_is_explicit.py:L179-L190] |
| Native output separation | Mock save leaves requested native simulation path absent | 0 files at the native target, count | `tests/solvers/orcaflex/test_mock_is_explicit.py::test_mock_save_simulation_does_not_create_a_bare_artifact_at_the_real_path` | PASS / PASS. [tests/solvers/orcaflex/test_mock_is_explicit.py:L193-L212] |

The separate legacy file was also invoked explicitly: 0 tests ran, 1 collection error, `ImportError: cannot import name '__version__' from 'digitalmodel.orcaflex'`; its imports expect the old namespace, and the repository already excludes the file from recursive collection. The missing export is present at BASE; see section 10, issue pending (9). [tests/solvers/orcaflex/test_orcaflex_unit.py:L12-L21] [src/digitalmodel/orcaflex/__init__.py:L1-L27] [tests/conftest.py:L39-L57]

The helper `run_core.py` is NOT_RUN because it constructs a runner and requests native `.dat` analysis; the test launcher `run_tests.py` is NOT_RUN because its environment detection calls the binding and licence helper. These are executable helpers, not collected pytest cases. [tests/solvers/orcaflex/core/scripts/run_core.py:L12-L25] [tests/solvers/orcaflex/run_tests.py:L15-L37]

## 5. QA gates

The following are proposed acceptance checks for this package under AQG r0, sections 3–10; a documentation or mock-test pass does not clear a native analysis gate. Physical checks require a future model, its design basis and native evidence.

| Gate | Applicability | Check IDs and present evidence |
| --- | --- | --- |
| G0 inputs | Applicable | C01–C04; configuration and mock refusal contracts only; complete model preflight remains open. [src/digitalmodel/solvers/orcaflex/core/configuration.py:L61-L93] [tests/solvers/orcaflex/test_mock_is_explicit.py:L140-L176] |
| G1 target-program load | Applicable | C05; real `load_file` uses `OrcFxAPI.Model().LoadData` in the selected target version; NOT_RUN, licensed binding required. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L284-L305] |
| G2 statics | Applicable | C06–C07; NOT_RUN, licensed statics and an independent line reference required. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L346-L416] |
| G3 short dynamics | Applicable | C08; NOT_RUN, licensed simulation required. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L418-L507] |
| G4 diffraction evidence | Conditional | C09; NOT_APPLICABLE to these artificial cases, which contain no diffraction-derived body data; require a cited record when such data are supplied, per AQG r0 section 10. [tests/solvers/orcaflex/test_mock_is_explicit.py:L179-L212] |
| G5 evidence appendix | Applicable | C10–C11; software provenance checks PASS at BASE; a physical QA appendix remains open. [src/digitalmodel/solvers/orcaflex/core/mock_artifacts.py:L9-L20] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L111-L130] |

Each criterion below is a proposed check instruction; PASS/NOT_RUN refers only to the named evidence, rather than a claim of gate clearance.

| ID | Gate | Executable check and numeric pass criterion with unit | Evidence and present result |
| --- | --- | --- | --- |
| C01 | G0 | Run section-4 missing-binding and two opt-in nodes; require 0 assertion failures, count. | BASE PASS; AFTER PASS. [tests/solvers/orcaflex/test_mock_is_explicit.py:L140-L176] |
| C02 | G0 | Inspect `OrcaFlexConfig` before loading: require 0 unknown fields, 0 nonpositive durations/steps, 0 unacknowledged compatibility warnings, count; compare each configured input path with the requested set and require 0 missing paths, count. | Range/schema inspection only; no complete executable preflight exists; issue pending (7). [src/digitalmodel/solvers/orcaflex/core/configuration.py:L61-L72] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L172-L196] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L425-L440] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L236-L249] |
| C03 | G0 | Recompute the manifest SHA-256 for every input consumed by `load_file`; require 0 differing digests, count, and 0 unresolved units/conventions, count, before analysis. | NOT_RUN: no model manifest or declared physical convention; issue pending (7). [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L256-L305] |
| C04 | G0 | Run a strict duplicate-key YAML parser on framework inputs, then `OrcaFlexConfig.from_yaml`; require 0 duplicate keys and 0 parse/schema errors, count; round-trip supplied fields and require 0 changed values, count. | Proposed external preflight; not implemented in `safe_load` route; issue pending (7). [src/digitalmodel/solvers/orcaflex/core/configuration.py:L379-L423] |
| C05 | G1 | On the licensed route call wrapper `load_file`, then `validate_model`; require 0 load exceptions, 0 warnings and 0 differences between native object count and manifest count, count; record exact target version. | NOT_RUN: binding and model absent; mock `LoadData` is not a substitute. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L284-L344] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L551-L577] |
| C06 | G2 | After `run_static_analysis`, require `converged=True`, 0 state-check errors and 0 unchecked native objects, count; compare returned tolerance to the applied native setting with 0 difference, dimensionless. | NOT_RUN: licensed model absent; parameter propagation is a known gap, issue pending (3). [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L362-L416] |
| C07 | G2 | Extract each line's static top tension and angle via `extract_results`; compare against an independent hand calculation: tension difference <= 5.0 %, angle difference <= 1.0 degree; require >= 6 offset steps and 0 nonmonotonic restoring-force increments, count, per AQG r0 section 6. | NOT_RUN: independent line reference and native results absent; issue pending (8). [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L579-L613] |
| C08 | G3 | Call `run_dynamic_simulation(duration=1.0, time_step=<production step>)`; require stop time >= 1.0 s, 0 warnings/errors and 0 nonfinite time-history samples, count; tension change from statics <= 25.0 %, per AQG r0 section 7. | NOT_RUN: licensed model absent; returned duration alone does not prove completion. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L442-L507] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L608-L613] |
| C09 | G4 | If native model data use a new diffraction run, match its fingerprint to a cited G4 PASS record; require 0 missing/mismatched record fields, count, including units and target version. | NOT_APPLICABLE to mock cases; conditional check requires external evidence; issue pending (8), AQG r0 sections 8 and 10. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L551-L577] |
| C10 | G5 | Run the two section-4 extraction/save nodes; require 0 fabricated arrays and 0 mock files at native output targets, count. | BASE PASS; AFTER PASS. [tests/solvers/orcaflex/test_mock_is_explicit.py:L179-L212] |
| C11 | G5 | For a released result, reconcile `ModelResult` and workflow metadata to existing output files and input fingerprint: 0 missing files, 0 untraced quantities and 0 unsigned required records, count. | Proposed evidence reconciliation; no physical QA appendix supplied; issue pending (8). [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L111-L130] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L188-L196] |

## 6. Known failure modes and pitfalls

sweep: 35 found

The count denotes reviewed risk sites, including documented sentinels and explicit mock placeholders; it is not a count of confirmed engineering failures. Each row retains its own detection limitation.

| Site | Symptom and cause at the code pin | Detection | Tracking |
| --- | --- | --- | --- |
| F01 | Cleanup catches every deletion exception and suppresses it; resource-release failure can be hidden. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L140-L149] | no detection check exists | issue pending (5) |
| F02 | Missing input files are logged and omitted from the returned list. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L236-L249] | C02 | issue pending (2) |
| F03 | An empty input list still reaches COMPLETED; success can describe zero work. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L211-L234] | no detection check exists | issue pending (2) |
| F04 | Iteration starts at zero and calculates its own synthetic target trajectory without extracting or updating a model parameter. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L317-L356] | no detection check exists | issue pending (3) |
| F05 | Static workflow metadata records the native requested filename, ignoring the marked path returned by a mock save. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L188-L196] | C11 | issue pending (4) |
| F06 | Dynamic workflow metadata likewise records the native requested filename for a mock save. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L279-L284] | C11 | issue pending (4) |
| F07 | Validation exceptions become warnings; mock validation returns the initial empty warnings list without inspecting data. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L329-L344] | C05 | issue pending (7) |
| F08 | Falsy tolerance/iteration arguments silently fall back to configured values. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L365-L366] | C06 | issue pending (3) |
| F09 | Mock statics report convergence and a fixed iteration count; real iteration metadata falls back to zero when the attribute is absent. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L377-L403] | C06 | issue pending (3) |
| F10 | Falsy dynamic duration and step fall back to defaults; integer division truncates progress steps and a zero interval divides by zero. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L442-L449] | no detection check exists | issue pending (3) |
| F11 | Progress callback is defined but is not attached before the native run; integer conversion discards fractional percent. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L470-L479] | no detection check exists | issue pending (3) |
| F12 | Nonpositive measured elapsed time turns simulation speed into zero. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L488-L494] | no detection check exists | issue pending (5) |
| F13 | A bare catch suppresses metadata access errors and returns a partial mapping. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L567-L577] | C11 | issue pending (5) |
| F14 | Context-exit cleanup suppresses deletion errors and resets state. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L634-L642] | no detection check exists | issue pending (5) |
| F15 | Availability helper suppresses licence exceptions and missing imports into two false flags, losing failure context. Read as text only. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L697-L712] | no detection check exists | issue pending (5) |
| F16 | Mock defaults supply fixed stage duration, step, depth, iteration count and no objects; these are synthetic values. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L648-L660] | C10 | issue pending (4) |
| F17 | Direct mock load is a no-op; direct mock saves touch empty native-named files, bypassing the marked-artifact writer. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L662-L680] | no detection check exists | issue pending (4) |
| F18 | Direct mock object access supplies a zero-filled time-history lambda rather than a physical result. The wrapper extraction path refuses this. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L682-L686] | C10 (wrapper only) | issue pending (4) |
| F19 | Missing files return false without a report row; an unrecognized legacy layout is treated as already converted and succeeds. [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L66-L84] | no detection check exists | issue pending (6) |
| F20 | Backup renames the original and then chooses JSON because the timestamped suffix is no longer YAML; migration can return false after moving the source. Do not run on retained inputs. [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L86-L131] | no detection check exists | issue pending (6) |
| F21 | Missing directory returns zero, indistinguishable from an empty or wholly failed migration by count alone; successes are counted after false returns. [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L152-L176] | no detection check exists | issue pending (6) |
| F22 | Unknown extension catches every YAML error before trying JSON, discarding the first failure context. [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L188-L199] | no detection check exists | issue pending (6) |
| F23 | Compatibility warnings do not fail validation; other exceptions become false. [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L275-L289] | C02 | issue pending (6) |
| F24 | Directory mode reaches exit zero even when every migration failed. [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L367-L393] | no detection check exists | issue pending (6) |
| F25 | Ramp defaults to zero seconds; solver type and logging interval have defaults without application in the dynamic wrapper body. [src/digitalmodel/solvers/orcaflex/core/configuration.py:L184-L196] | no detection check exists | issue pending (3) |
| F26 | An empty configuration dictionary selects the default configuration. [src/digitalmodel/solvers/orcaflex/core/configuration.py:L469-L484] | C02 | issue pending (7) |
| F27 | Legacy conversion supplies default directories, empty files, disabled flags, tolerance and duration and drops unmapped keys. [src/digitalmodel/solvers/orcaflex/core/configuration.py:L505-L552] | C04 | issue pending (7) |
| F28 | Enabled statics, fixed tolerance, iterations and damping are framework defaults, without proof they were applied to the native model. [src/digitalmodel/solvers/orcaflex/core/configuration.py:L151-L169] | C06 | issue pending (3) |
| F29 | Analyzer results initialize empty and cleanup clears results/errors/warnings; empty retrieval is not evidence of completed analysis. [src/digitalmodel/solvers/orcaflex/core/base_classes.py:L75-L115] | no detection check exists | issue pending (5) |
| F30 | Workflow step counts initialize to zero; an empty workflow later returns an empty result after setting completed. [src/digitalmodel/solvers/orcaflex/core/base_classes.py:L196-L207] | no detection check exists | issue pending (2) |
| F31 | Progress fields initialize to zero and elapsed time returns None before start; these are sentinels, not numerical solve evidence. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L64-L103] | no detection check exists | issue pending (5) |
| F32 | Workflow elapsed time returns None before start; COMPLETED with no errors alone determines success. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L60-L83] | no detection check exists | issue pending (2) |
| F33 | Duplicate registration overwrites prior components after a warning; instance count resets to zero. [src/digitalmodel/solvers/orcaflex/core/registry.py:L98-L110] | no detection check exists | issue pending (1) |
| F34 | Repeated setup silently returns; earlier logging handlers/configuration remain in effect. [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L136-L146] | no detection check exists | issue pending (5) |
| F35 | Native depth, duration and TimeHistory are forwarded without a physical unit/frame contract or normalization. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L567-L613] | C03 | issue pending (7) |

Sweep commands (run on all package Python files at the code pin):

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|\b(0|0\.0|1970)\b|\b(int|float)\(' src/digitalmodel/solvers/orcaflex/core --glob '*.py'
rg -n 'default|\.get\(|setdefault|unit|[Cc]ache|0\.05|5%' src/digitalmodel/solvers/orcaflex/core --glob '*.py'
```

No external-text `int()`/`float()` conversion, record-skipping `continue`, year fallback or empty-result cache write was found in the package; integer conversions operate on computed progress, and the registry/loggers cache components and logger objects. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L442-L475] [src/digitalmodel/solvers/orcaflex/core/registry.py:L43-L64] [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L209-L229]

Other exception handlers rethrow contextual errors or return explicit FAILED result objects; they are not silently successful error paths. [src/digitalmodel/solvers/orcaflex/core/base_classes.py:L257-L261] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L396-L423] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L203-L241] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L498-L517]

| Additional confirmed pitfall or historical incident | Detection | Tracking |
| --- | --- | --- |
| `BaseAnalysisWorkflow.__init__` reads `self.workflow_id` although its base initializes no such attribute; engine creation also calls `registry.get` with arguments reversed and treats the returned instance as a class. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L93-L107] [src/digitalmodel/solvers/orcaflex/core/base_classes.py:L196-L208] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L428-L434] [src/digitalmodel/solvers/orcaflex/core/registry.py:L116-L158] | no detection check exists | issue pending (1) |
| The routing contract compares the complete solver-submodule mapping against sixteen enumerated routes; a new core route requires its expected inventory to be extended. [tests/docs/test_digitalmodel_routing_contract.py:L217-L254] | Unchanged routing-contract test | issue pending (11) |
| Dynamic and iterative batch `execute` delegate to a base step workflow that returns a dictionary and accepts no extra keyword arguments; engine aggregation expects objects with `.success`. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L294-L297] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L366-L368] [src/digitalmodel/solvers/orcaflex/core/base_classes.py:L230-L267] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L549-L555] | no detection check exists | issue pending (1) |
| The registry helper documents decorator use with no `component_class`, but that argument is required. [src/digitalmodel/solvers/orcaflex/core/registry.py:L283-L306] | no detection check exists | issue pending (1) |
| Parallel timeout is applied only after `as_completed` yields a finished future; the timeout handler names the package exception rather than the future timeout type. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L34-L39] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L497-L517] | no detection check exists | issue pending (1) |
| Historical implicit mock fallback and random engineering arrays are guarded by the current refusal and artifact tests. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L163-L186] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L598-L607] [tests/solvers/orcaflex/test_mock_is_explicit.py:L1-L10] | C01, C10 | #1631 |
| Optimized-worker artifact reporting shares the marked-artifact helper and has isolated regression coverage. [tests/solvers/orcaflex/test_optimized_parallel_contract.py:L1-L8] [tests/solvers/orcaflex/test_optimized_parallel_contract.py:L393-L433] | C10; worker-specific tests supplement the wrapper checks | #2521 |

Observation date for the sweep and code inspection: 2026-11-07; historical issue dates are not inferred from current issue state.

## 7. Lessons log

| Date | Record | Lesson and evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-11-07 | Initial documentation | A marked artificial artifact and a refused numerical extraction are distinct evidence from a physical solution. [src/digitalmodel/solvers/orcaflex/core/mock_artifacts.py:L9-L20] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L598-L607] | Sections 4–6 separate regression evidence from native gates. | Proposed; issue pending (8). |

## 8. Ownership

Proposed review roles: OrcaFlex riser and mooring quality manager; deputy: riser and mooring verification maintainer. Proposed producing discipline: offshore analysis and software maintenance. Escalate unresolved physical criteria to the family quality manager and cross-family questions to the maintainers. No named appointment or clearance is asserted.

## 9. Dependencies

The implementation map is frozen at **3256bab95f84da3d9fadce8d5dcf8458d66d1eb4**, with data revision **270c8759c374b67a7f032953e7fcdb14e0224461**; implementation-map citations in this section resolve at that map pin, while source and test citations resolve at the header code pin. [docs/registry/implementation-map.yaml:L1-L4]

### Map-family inventory and counted-family audit

Two map families have in-package rows: `orcaflex-text-model-writers` (2) and `external-solver-batch-run-stacks` (2). Both are counted; their tie keeps map order. Every implementation in both families is indexed below, including those outside this package. Inventory IDs are family prefixes plus the original 1-based row ordinal; each citation identifies the complete original map record, preserving paths with identifying discovery names in the map rather than transcribing them here. [docs/registry/implementation-map.yaml:L6875-L6884] [docs/registry/implementation-map.yaml:L6885-L6894] [docs/registry/implementation-map.yaml:L25100-L25109] [docs/registry/implementation-map.yaml:L25110-L25119]

#### `orcaflex-text-model-writers`: 440 implementations

| Inventory ID | Implementation locator | Map relation | Frozen map row |
| --- | --- | --- | --- |
| W001 | External implementation 1 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L4940-L4949] |
| W002 | External implementation 2 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L4950-L4958] |
| W003 | External implementation 3 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L4959-L4968] |
| W004 | External implementation 4 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L4969-L4978] |
| W005 | External implementation 5 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L4979-L4988] |
| W006 | External implementation 6 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L4989-L4998] |
| W007 | External implementation 7 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L4999-L5008] |
| W008 | External implementation 8 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5009-L5020] |
| W009 | External implementation 9 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5021-L5030] |
| W010 | External implementation 10 (original path and symbol in cited row) | copy | [docs/registry/implementation-map.yaml:L5031-L5040] |
| W011 | External implementation 11 (original path and symbol in cited row) | copy | [docs/registry/implementation-map.yaml:L5041-L5050] |
| W012 | External implementation 12 (original path and symbol in cited row) | copy | [docs/registry/implementation-map.yaml:L5051-L5060] |
| W013 | External implementation 13 (original path and symbol in cited row) | example | [docs/registry/implementation-map.yaml:L5061-L5070] |
| W014 | External implementation 14 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5071-L5080] |
| W015 | External implementation 15 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5081-L5090] |
| W016 | External implementation 16 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5091-L5100] |
| W017 | External implementation 17 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5101-L5110] |
| W018 | External implementation 18 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5111-L5120] |
| W019 | External implementation 19 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5121-L5132] |
| W020 | External implementation 20 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5133-L5142] |
| W021 | External implementation 21 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5143-L5151] |
| W022 | External implementation 22 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5152-L5161] |
| W023 | External implementation 23 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5162-L5171] |
| W024 | External implementation 24 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5172-L5181] |
| W025 | External implementation 25 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5182-L5191] |
| W026 | External implementation 26 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5192-L5201] |
| W027 | External implementation 27 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5202-L5211] |
| W028 | External implementation 28 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5212-L5223] |
| W029 | External implementation 29 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5224-L5233] |
| W030 | External implementation 30 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5234-L5243] |
| W031 | External implementation 31 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5244-L5253] |
| W032 | External implementation 32 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5254-L5263] |
| W033 | External implementation 33 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5264-L5273] |
| W034 | External implementation 34 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5274-L5283] |
| W035 | External implementation 35 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5284-L5293] |
| W036 | External implementation 36 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5294-L5303] |
| W037 | External implementation 37 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5304-L5313] |
| W038 | External implementation 38 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5314-L5323] |
| W039 | External implementation 39 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5324-L5333] |
| W040 | External implementation 40 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5334-L5343] |
| W041 | External implementation 41 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5344-L5353] |
| W042 | External implementation 42 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5354-L5363] |
| W043 | External implementation 43 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5364-L5372] |
| W044 | External implementation 44 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5373-L5382] |
| W045 | External implementation 45 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5383-L5392] |
| W046 | External implementation 46 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5393-L5402] |
| W047 | External implementation 47 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5403-L5415] |
| W048 | External implementation 48 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5416-L5425] |
| W049 | External implementation 49 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5426-L5437] |
| W050 | External implementation 50 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5438-L5447] |
| W051 | External implementation 51 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5448-L5457] |
| W052 | External implementation 52 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5458-L5467] |
| W053 | External implementation 53 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5468-L5477] |
| W054 | External implementation 54 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5478-L5487] |
| W055 | External implementation 55 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5488-L5497] |
| W056 | External implementation 56 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5498-L5507] |
| W057 | External implementation 57 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5508-L5519] |
| W058 | External implementation 58 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5520-L5531] |
| W059 | External implementation 59 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5532-L5540] |
| W060 | External implementation 60 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5541-L5550] |
| W061 | External implementation 61 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5551-L5560] |
| W062 | External implementation 62 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5561-L5570] |
| W063 | External implementation 63 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5571-L5580] |
| W064 | External implementation 64 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5581-L5593] |
| W065 | External implementation 65 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5594-L5603] |
| W066 | External implementation 66 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5604-L5615] |
| W067 | External implementation 67 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5616-L5628] |
| W068 | External implementation 68 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5629-L5638] |
| W069 | External implementation 69 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5639-L5648] |
| W070 | External implementation 70 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5649-L5658] |
| W071 | External implementation 71 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5659-L5667] |
| W072 | External implementation 72 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5668-L5677] |
| W073 | External implementation 73 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5678-L5687] |
| W074 | External implementation 74 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5688-L5697] |
| W075 | External implementation 75 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5698-L5707] |
| W076 | External implementation 76 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5708-L5717] |
| W077 | External implementation 77 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5718-L5727] |
| W078 | External implementation 78 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5728-L5737] |
| W079 | External implementation 79 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5738-L5749] |
| W080 | External implementation 80 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5750-L5759] |
| W081 | External implementation 81 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5760-L5769] |
| W082 | External implementation 82 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5770-L5781] |
| W083 | External implementation 83 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5782-L5791] |
| W084 | External implementation 84 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5792-L5801] |
| W085 | External implementation 85 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5802-L5811] |
| W086 | External implementation 86 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5812-L5823] |
| W087 | External implementation 87 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5824-L5833] |
| W088 | External implementation 88 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5834-L5843] |
| W089 | External implementation 89 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5844-L5853] |
| W090 | External implementation 90 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5854-L5863] |
| W091 | External implementation 91 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5864-L5872] |
| W092 | External implementation 92 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5873-L5882] |
| W093 | External implementation 93 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5883-L5891] |
| W094 | External implementation 94 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5892-L5901] |
| W095 | External implementation 95 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5902-L5911] |
| W096 | External implementation 96 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5912-L5921] |
| W097 | External implementation 97 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5922-L5933] |
| W098 | External implementation 98 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5934-L5943] |
| W099 | External implementation 99 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5944-L5953] |
| W100 | External implementation 100 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5954-L5965] |
| W101 | External implementation 101 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5966-L5975] |
| W102 | External implementation 102 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5976-L5985] |
| W103 | External implementation 103 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5986-L5994] |
| W104 | External implementation 104 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L5995-L6004] |
| W105 | External implementation 105 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6005-L6014] |
| W106 | External implementation 106 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6015-L6024] |
| W107 | External implementation 107 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6025-L6034] |
| W108 | External implementation 108 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6035-L6044] |
| W109 | External implementation 109 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6045-L6054] |
| W110 | External implementation 110 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6055-L6064] |
| W111 | External implementation 111 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6065-L6074] |
| W112 | External implementation 112 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6075-L6084] |
| W113 | External implementation 113 (original path and symbol in cited row) | caller | [docs/registry/implementation-map.yaml:L6085-L6096] |
| W114 | External implementation 114 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6097-L6109] |
| W115 | External implementation 115 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6110-L6119] |
| W116 | External implementation 116 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6120-L6129] |
| W117 | External implementation 117 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6130-L6141] |
| W118 | External implementation 118 (original path and symbol in cited row) | caller | [docs/registry/implementation-map.yaml:L6142-L6153] |
| W119 | External implementation 119 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6154-L6163] |
| W120 | External implementation 120 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6164-L6173] |
| W121 | External implementation 121 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6174-L6183] |
| W122 | External implementation 122 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6184-L6193] |
| W123 | External implementation 123 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6194-L6203] |
| W124 | External implementation 124 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6204-L6213] |
| W125 | External implementation 125 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6214-L6223] |
| W126 | External implementation 126 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6224-L6233] |
| W127 | External implementation 127 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6234-L6243] |
| W128 | External implementation 128 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6244-L6253] |
| W129 | External implementation 129 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6254-L6263] |
| W130 | External implementation 130 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6264-L6273] |
| W131 | External implementation 131 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6274-L6283] |
| W132 | External implementation 132 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6284-L6293] |
| W133 | External implementation 133 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6294-L6303] |
| W134 | External implementation 134 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6304-L6313] |
| W135 | External implementation 135 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6314-L6323] |
| W136 | External implementation 136 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6324-L6333] |
| W137 | External implementation 137 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6334-L6343] |
| W138 | External implementation 138 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6344-L6353] |
| W139 | External implementation 139 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6354-L6363] |
| W140 | External implementation 140 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6364-L6373] |
| W141 | External implementation 141 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6374-L6383] |
| W142 | External implementation 142 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6384-L6393] |
| W143 | External implementation 143 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6394-L6403] |
| W144 | External implementation 144 (original path and symbol in cited row) | copy | [docs/registry/implementation-map.yaml:L6404-L6413] |
| W145 | External implementation 145 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6414-L6423] |
| W146 | External implementation 146 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6424-L6436] |
| W147 | External implementation 147 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6437-L6448] |
| W148 | External implementation 148 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6449-L6458] |
| W149 | External implementation 149 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6459-L6468] |
| W150 | External implementation 150 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6469-L6478] |
| W151 | External implementation 151 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6479-L6490] |
| W152 | External implementation 152 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6491-L6500] |
| W153 | External implementation 153 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6501-L6510] |
| W154 | External implementation 154 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6511-L6520] |
| W155 | External implementation 155 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6521-L6530] |
| W156 | External implementation 156 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6531-L6540] |
| W157 | External implementation 157 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6541-L6550] |
| W158 | External implementation 158 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6551-L6560] |
| W159 | External implementation 159 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6561-L6570] |
| W160 | External implementation 160 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6571-L6580] |
| W161 | External implementation 161 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6581-L6590] |
| W162 | External implementation 162 (original path and symbol in cited row) | copy | [docs/registry/implementation-map.yaml:L6591-L6600] |
| W163 | External implementation 163 (original path and symbol in cited row) | copy | [docs/registry/implementation-map.yaml:L6601-L6610] |
| W164 | External implementation 164 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6611-L6620] |
| W165 | External implementation 165 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6621-L6630] |
| W166 | External implementation 166 (original path and symbol in cited row) | copy | [docs/registry/implementation-map.yaml:L6631-L6640] |
| W167 | External implementation 167 (original path and symbol in cited row) | copy | [docs/registry/implementation-map.yaml:L6641-L6650] |
| W168 | External implementation 168 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6651-L6659] |
| W169 | External implementation 169 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6660-L6672] |
| W170 | External implementation 170 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6673-L6682] |
| W171 | External implementation 171 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6683-L6692] |
| W172 | External implementation 172 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6693-L6704] |
| W173 | External implementation 173 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6705-L6714] |
| W174 | External implementation 174 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6715-L6724] |
| W175 | External implementation 175 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6725-L6734] |
| W176 | External implementation 176 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6735-L6744] |
| W177 | External implementation 177 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6745-L6754] |
| W178 | External implementation 178 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6755-L6764] |
| W179 | External implementation 179 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6765-L6774] |
| W180 | External implementation 180 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6775-L6784] |
| W181 | External implementation 181 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6785-L6794] |
| W182 | External implementation 182 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6795-L6804] |
| W183 | External implementation 183 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6805-L6814] |
| W184 | External implementation 184 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6815-L6824] |
| W185 | External implementation 185 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6825-L6834] |
| W186 | External implementation 186 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6835-L6844] |
| W187 | External implementation 187 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6845-L6854] |
| W188 | External implementation 188 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6855-L6864] |
| W189 | External implementation 189 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6865-L6874] |
| W190 | `configuration.py` / `OrcaFlexConfig.to_yaml` | independent | [docs/registry/implementation-map.yaml:L6875-L6884] |
| W191 | `model_interface.py` / `OrcaFlexModelWrapper.save_simulation` | independent | [docs/registry/implementation-map.yaml:L6885-L6894] |
| W192 | External implementation 192 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6895-L6904] |
| W193 | External implementation 193 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6905-L6914] |
| W194 | External implementation 194 (original path and symbol in cited row) | wrapper | [docs/registry/implementation-map.yaml:L6915-L6927] |
| W195 | External implementation 195 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6928-L6937] |
| W196 | External implementation 196 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6938-L6947] |
| W197 | External implementation 197 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6948-L6957] |
| W198 | External implementation 198 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6958-L6967] |
| W199 | External implementation 199 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6968-L6977] |
| W200 | External implementation 200 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6978-L6987] |
| W201 | External implementation 201 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6988-L6997] |
| W202 | External implementation 202 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L6998-L7007] |
| W203 | External implementation 203 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7008-L7017] |
| W204 | External implementation 204 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7018-L7031] |
| W205 | External implementation 205 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7032-L7041] |
| W206 | External implementation 206 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7042-L7051] |
| W207 | External implementation 207 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7052-L7061] |
| W208 | External implementation 208 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7062-L7071] |
| W209 | External implementation 209 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7072-L7081] |
| W210 | External implementation 210 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7082-L7091] |
| W211 | External implementation 211 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7092-L7101] |
| W212 | External implementation 212 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7102-L7111] |
| W213 | External implementation 213 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7112-L7121] |
| W214 | External implementation 214 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7122-L7131] |
| W215 | External implementation 215 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7132-L7141] |
| W216 | External implementation 216 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7142-L7153] |
| W217 | External implementation 217 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7154-L7163] |
| W218 | External implementation 218 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7164-L7176] |
| W219 | External implementation 219 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7177-L7186] |
| W220 | External implementation 220 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7187-L7196] |
| W221 | External implementation 221 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7197-L7207] |
| W222 | External implementation 222 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7208-L7217] |
| W223 | External implementation 223 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7218-L7227] |
| W224 | External implementation 224 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7228-L7237] |
| W225 | External implementation 225 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7238-L7247] |
| W226 | External implementation 226 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7248-L7257] |
| W227 | External implementation 227 (original path and symbol in cited row) | caller | [docs/registry/implementation-map.yaml:L7258-L7269] |
| W228 | External implementation 228 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7270-L7279] |
| W229 | External implementation 229 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7280-L7289] |
| W230 | External implementation 230 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7290-L7299] |
| W231 | External implementation 231 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7300-L7309] |
| W232 | External implementation 232 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7310-L7319] |
| W233 | External implementation 233 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7320-L7329] |
| W234 | External implementation 234 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7330-L7339] |
| W235 | External implementation 235 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7340-L7349] |
| W236 | External implementation 236 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7350-L7359] |
| W237 | External implementation 237 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7360-L7369] |
| W238 | External implementation 238 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7370-L7379] |
| W239 | External implementation 239 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7380-L7389] |
| W240 | External implementation 240 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7390-L7399] |
| W241 | External implementation 241 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7400-L7409] |
| W242 | External implementation 242 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7410-L7419] |
| W243 | External implementation 243 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7420-L7429] |
| W244 | External implementation 244 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7430-L7439] |
| W245 | External implementation 245 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7440-L7449] |
| W246 | External implementation 246 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7450-L7459] |
| W247 | External implementation 247 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7460-L7469] |
| W248 | External implementation 248 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7470-L7479] |
| W249 | External implementation 249 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7480-L7489] |
| W250 | External implementation 250 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7490-L7499] |
| W251 | External implementation 251 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7500-L7509] |
| W252 | External implementation 252 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7510-L7519] |
| W253 | External implementation 253 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7520-L7529] |
| W254 | External implementation 254 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7530-L7539] |
| W255 | External implementation 255 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7540-L7551] |
| W256 | External implementation 256 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7552-L7561] |
| W257 | External implementation 257 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7562-L7571] |
| W258 | External implementation 258 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7572-L7581] |
| W259 | External implementation 259 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7582-L7591] |
| W260 | External implementation 260 (original path and symbol in cited row) | wrapper | [docs/registry/implementation-map.yaml:L7592-L7603] |
| W261 | External implementation 261 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7604-L7613] |
| W262 | External implementation 262 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7614-L7626] |
| W263 | External implementation 263 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7627-L7636] |
| W264 | External implementation 264 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7637-L7646] |
| W265 | External implementation 265 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7647-L7656] |
| W266 | External implementation 266 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7657-L7666] |
| W267 | External implementation 267 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7667-L7676] |
| W268 | External implementation 268 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7677-L7686] |
| W269 | External implementation 269 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7687-L7696] |
| W270 | External implementation 270 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7697-L7706] |
| W271 | External implementation 271 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7707-L7716] |
| W272 | External implementation 272 (original path and symbol in cited row) | wrapper | [docs/registry/implementation-map.yaml:L7717-L7729] |
| W273 | External implementation 273 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7730-L7739] |
| W274 | External implementation 274 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7740-L7749] |
| W275 | External implementation 275 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7750-L7762] |
| W276 | External implementation 276 (original path and symbol in cited row) | copy | [docs/registry/implementation-map.yaml:L7763-L7774] |
| W277 | External implementation 277 (original path and symbol in cited row) | copy | [docs/registry/implementation-map.yaml:L7775-L7784] |
| W278 | External implementation 278 (original path and symbol in cited row) | copy | [docs/registry/implementation-map.yaml:L7785-L7794] |
| W279 | External implementation 279 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7795-L7804] |
| W280 | External implementation 280 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7805-L7816] |
| W281 | External implementation 281 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7817-L7826] |
| W282 | External implementation 282 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7827-L7836] |
| W283 | External implementation 283 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7837-L7846] |
| W284 | External implementation 284 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7847-L7856] |
| W285 | External implementation 285 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7857-L7866] |
| W286 | External implementation 286 (original path and symbol in cited row) | caller | [docs/registry/implementation-map.yaml:L7867-L7878] |
| W287 | External implementation 287 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7879-L7888] |
| W288 | External implementation 288 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7889-L7898] |
| W289 | External implementation 289 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7899-L7908] |
| W290 | External implementation 290 (original path and symbol in cited row) | wrapper | [docs/registry/implementation-map.yaml:L7909-L7921] |
| W291 | External implementation 291 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7922-L7931] |
| W292 | External implementation 292 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7932-L7941] |
| W293 | External implementation 293 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7942-L7951] |
| W294 | External implementation 294 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7952-L7961] |
| W295 | External implementation 295 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7962-L7971] |
| W296 | External implementation 296 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7972-L7981] |
| W297 | External implementation 297 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7982-L7991] |
| W298 | External implementation 298 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L7992-L8001] |
| W299 | External implementation 299 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8002-L8011] |
| W300 | External implementation 300 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8012-L8021] |
| W301 | External implementation 301 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8022-L8031] |
| W302 | External implementation 302 (original path and symbol in cited row) | caller | [docs/registry/implementation-map.yaml:L8032-L8043] |
| W303 | External implementation 303 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8044-L8053] |
| W304 | External implementation 304 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8054-L8063] |
| W305 | External implementation 305 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8064-L8073] |
| W306 | External implementation 306 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8074-L8083] |
| W307 | External implementation 307 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8084-L8093] |
| W308 | External implementation 308 (original path and symbol in cited row) | copy | [docs/registry/implementation-map.yaml:L8094-L8103] |
| W309 | External implementation 309 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8104-L8113] |
| W310 | External implementation 310 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8114-L8123] |
| W311 | External implementation 311 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8124-L8133] |
| W312 | External implementation 312 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8134-L8143] |
| W313 | External implementation 313 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8144-L8155] |
| W314 | External implementation 314 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8156-L8165] |
| W315 | External implementation 315 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8166-L8175] |
| W316 | External implementation 316 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8176-L8185] |
| W317 | External implementation 317 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8186-L8195] |
| W318 | External implementation 318 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8196-L8205] |
| W319 | External implementation 319 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8206-L8215] |
| W320 | External implementation 320 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8216-L8227] |
| W321 | External implementation 321 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8228-L8237] |
| W322 | External implementation 322 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8238-L8247] |
| W323 | External implementation 323 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8248-L8257] |
| W324 | External implementation 324 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8258-L8267] |
| W325 | External implementation 325 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8268-L8279] |
| W326 | External implementation 326 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8280-L8290] |
| W327 | External implementation 327 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8291-L8300] |
| W328 | External implementation 328 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8301-L8310] |
| W329 | External implementation 329 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8311-L8323] |
| W330 | External implementation 330 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8324-L8333] |
| W331 | External implementation 331 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8334-L8343] |
| W332 | External implementation 332 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8344-L8353] |
| W333 | External implementation 333 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8354-L8363] |
| W334 | External implementation 334 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8364-L8373] |
| W335 | External implementation 335 (original path and symbol in cited row) | copy | [docs/registry/implementation-map.yaml:L8374-L8383] |
| W336 | External implementation 336 (original path and symbol in cited row) | copy | [docs/registry/implementation-map.yaml:L8384-L8393] |
| W337 | External implementation 337 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8394-L8403] |
| W338 | External implementation 338 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8404-L8413] |
| W339 | External implementation 339 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8414-L8423] |
| W340 | External implementation 340 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8424-L8435] |
| W341 | External implementation 341 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8436-L8445] |
| W342 | External implementation 342 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8446-L8457] |
| W343 | External implementation 343 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8458-L8470] |
| W344 | External implementation 344 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8471-L8480] |
| W345 | External implementation 345 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8481-L8492] |
| W346 | External implementation 346 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8493-L8505] |
| W347 | External implementation 347 (original path and symbol in cited row) | wrapper | [docs/registry/implementation-map.yaml:L8506-L8517] |
| W348 | External implementation 348 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8518-L8527] |
| W349 | External implementation 349 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8528-L8537] |
| W350 | External implementation 350 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8538-L8547] |
| W351 | External implementation 351 (original path and symbol in cited row) | caller | [docs/registry/implementation-map.yaml:L8548-L8560] |
| W352 | External implementation 352 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8561-L8572] |
| W353 | External implementation 353 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8573-L8582] |
| W354 | External implementation 354 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8583-L8596] |
| W355 | External implementation 355 (original path and symbol in cited row) | wrapper | [docs/registry/implementation-map.yaml:L8597-L8608] |
| W356 | External implementation 356 (original path and symbol in cited row) | wrapper | [docs/registry/implementation-map.yaml:L8609-L8619] |
| W357 | External implementation 357 (original path and symbol in cited row) | caller | [docs/registry/implementation-map.yaml:L8620-L8632] |
| W358 | External implementation 358 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8633-L8647] |
| W359 | External implementation 359 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8648-L8657] |
| W360 | External implementation 360 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8658-L8667] |
| W361 | External implementation 361 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8668-L8677] |
| W362 | External implementation 362 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8678-L8687] |
| W363 | External implementation 363 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8688-L8701] |
| W364 | External implementation 364 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8702-L8714] |
| W365 | External implementation 365 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8715-L8724] |
| W366 | External implementation 366 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8725-L8739] |
| W367 | External implementation 367 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8740-L8749] |
| W368 | External implementation 368 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8750-L8759] |
| W369 | External implementation 369 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8760-L8773] |
| W370 | External implementation 370 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8774-L8783] |
| W371 | External implementation 371 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8784-L8793] |
| W372 | External implementation 372 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8794-L8803] |
| W373 | External implementation 373 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8804-L8813] |
| W374 | External implementation 374 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8814-L8823] |
| W375 | External implementation 375 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8824-L8833] |
| W376 | External implementation 376 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8834-L8843] |
| W377 | External implementation 377 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8844-L8856] |
| W378 | External implementation 378 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L8857-L8866] |
| W379 | External implementation 379 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L8867-L8876] |
| W380 | External implementation 380 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L8877-L8886] |
| W381 | External implementation 381 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L8887-L8896] |
| W382 | External implementation 382 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L8897-L8908] |
| W383 | External implementation 383 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L8909-L8918] |
| W384 | External implementation 384 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L8919-L8928] |
| W385 | External implementation 385 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L8929-L8938] |
| W386 | External implementation 386 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L8939-L8948] |
| W387 | External implementation 387 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L8949-L8958] |
| W388 | External implementation 388 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L8959-L8970] |
| W389 | External implementation 389 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L8971-L8980] |
| W390 | External implementation 390 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L8981-L8990] |
| W391 | External implementation 391 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L8991-L9000] |
| W392 | External implementation 392 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9001-L9010] |
| W393 | External implementation 393 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9011-L9020] |
| W394 | External implementation 394 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9021-L9030] |
| W395 | External implementation 395 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9031-L9040] |
| W396 | External implementation 396 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9041-L9050] |
| W397 | External implementation 397 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9051-L9060] |
| W398 | External implementation 398 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9061-L9070] |
| W399 | External implementation 399 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9071-L9080] |
| W400 | External implementation 400 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9081-L9090] |
| W401 | External implementation 401 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9091-L9100] |
| W402 | External implementation 402 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9101-L9110] |
| W403 | External implementation 403 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9111-L9120] |
| W404 | External implementation 404 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9121-L9130] |
| W405 | External implementation 405 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9131-L9140] |
| W406 | External implementation 406 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9141-L9152] |
| W407 | External implementation 407 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9153-L9162] |
| W408 | External implementation 408 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9163-L9172] |
| W409 | External implementation 409 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9173-L9182] |
| W410 | External implementation 410 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9183-L9192] |
| W411 | External implementation 411 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9193-L9202] |
| W412 | External implementation 412 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9203-L9212] |
| W413 | External implementation 413 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9213-L9222] |
| W414 | External implementation 414 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9223-L9232] |
| W415 | External implementation 415 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9233-L9242] |
| W416 | External implementation 416 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9243-L9252] |
| W417 | External implementation 417 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9253-L9262] |
| W418 | External implementation 418 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9263-L9272] |
| W419 | External implementation 419 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9273-L9282] |
| W420 | External implementation 420 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9283-L9292] |
| W421 | External implementation 421 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9293-L9302] |
| W422 | External implementation 422 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9303-L9312] |
| W423 | External implementation 423 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9313-L9322] |
| W424 | External implementation 424 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9323-L9332] |
| W425 | External implementation 425 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9333-L9342] |
| W426 | External implementation 426 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9343-L9352] |
| W427 | External implementation 427 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9353-L9362] |
| W428 | External implementation 428 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9363-L9372] |
| W429 | External implementation 429 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9373-L9382] |
| W430 | External implementation 430 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9383-L9392] |
| W431 | External implementation 431 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9393-L9402] |
| W432 | External implementation 432 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9403-L9412] |
| W433 | External implementation 433 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9413-L9422] |
| W434 | External implementation 434 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9423-L9432] |
| W435 | External implementation 435 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9433-L9442] |
| W436 | External implementation 436 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9443-L9452] |
| W437 | External implementation 437 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9453-L9462] |
| W438 | External implementation 438 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9463-L9472] |
| W439 | External implementation 439 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9473-L9482] |
| W440 | External implementation 440 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L9483-L9493] |

#### `external-solver-batch-run-stacks`: 137 implementations

| Inventory ID | Implementation locator | Map relation | Frozen map row |
| --- | --- | --- | --- |
| E001 | External implementation 1 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24511-L24520] |
| E002 | External implementation 2 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24521-L24530] |
| E003 | External implementation 3 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24531-L24540] |
| E004 | External implementation 4 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24541-L24550] |
| E005 | External implementation 5 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24551-L24560] |
| E006 | External implementation 6 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24561-L24570] |
| E007 | External implementation 7 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24571-L24580] |
| E008 | External implementation 8 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24581-L24590] |
| E009 | External implementation 9 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24591-L24600] |
| E010 | External implementation 10 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24601-L24610] |
| E011 | External implementation 11 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24611-L24620] |
| E012 | External implementation 12 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24621-L24630] |
| E013 | External implementation 13 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24631-L24640] |
| E014 | External implementation 14 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24641-L24650] |
| E015 | External implementation 15 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24651-L24660] |
| E016 | External implementation 16 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24661-L24670] |
| E017 | External implementation 17 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24671-L24680] |
| E018 | External implementation 18 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24681-L24690] |
| E019 | External implementation 19 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24691-L24700] |
| E020 | External implementation 20 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24701-L24712] |
| E021 | External implementation 21 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24713-L24722] |
| E022 | External implementation 22 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24723-L24732] |
| E023 | External implementation 23 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24733-L24742] |
| E024 | External implementation 24 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24743-L24751] |
| E025 | External implementation 25 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24752-L24760] |
| E026 | External implementation 26 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24761-L24769] |
| E027 | External implementation 27 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24770-L24778] |
| E028 | External implementation 28 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24779-L24787] |
| E029 | External implementation 29 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24788-L24796] |
| E030 | External implementation 30 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24797-L24805] |
| E031 | External implementation 31 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24806-L24814] |
| E032 | External implementation 32 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24815-L24823] |
| E033 | External implementation 33 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24824-L24832] |
| E034 | External implementation 34 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24833-L24841] |
| E035 | External implementation 35 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24842-L24850] |
| E036 | External implementation 36 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24851-L24859] |
| E037 | External implementation 37 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24860-L24868] |
| E038 | External implementation 38 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24869-L24877] |
| E039 | External implementation 39 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24878-L24886] |
| E040 | External implementation 40 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24887-L24895] |
| E041 | External implementation 41 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24896-L24904] |
| E042 | External implementation 42 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24905-L24913] |
| E043 | External implementation 43 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24914-L24922] |
| E044 | External implementation 44 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24923-L24932] |
| E045 | External implementation 45 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24933-L24942] |
| E046 | External implementation 46 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24943-L24952] |
| E047 | External implementation 47 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24953-L24962] |
| E048 | External implementation 48 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24963-L24974] |
| E049 | External implementation 49 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24975-L24984] |
| E050 | External implementation 50 (original path and symbol in cited row) | caller | [docs/registry/implementation-map.yaml:L24985-L24997] |
| E051 | External implementation 51 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L24998-L25007] |
| E052 | External implementation 52 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25008-L25017] |
| E053 | External implementation 53 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25018-L25027] |
| E054 | External implementation 54 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25028-L25037] |
| E055 | External implementation 55 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25038-L25047] |
| E056 | External implementation 56 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25048-L25057] |
| E057 | External implementation 57 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25058-L25067] |
| E058 | External implementation 58 (original path and symbol in cited row) | caller | [docs/registry/implementation-map.yaml:L25068-L25079] |
| E059 | External implementation 59 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25080-L25089] |
| E060 | External implementation 60 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25090-L25099] |
| E061 | `model_interface.py` / `OrcaFlexModelWrapper.run_static_analysis` | independent | [docs/registry/implementation-map.yaml:L25100-L25109] |
| E062 | `model_interface.py` / `OrcaFlexModelWrapper.run_dynamic_simulation` | independent | [docs/registry/implementation-map.yaml:L25110-L25119] |
| E063 | External implementation 63 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25120-L25129] |
| E064 | External implementation 64 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25130-L25139] |
| E065 | External implementation 65 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25140-L25149] |
| E066 | External implementation 66 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25150-L25159] |
| E067 | External implementation 67 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25160-L25169] |
| E068 | External implementation 68 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25170-L25179] |
| E069 | External implementation 69 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25180-L25189] |
| E070 | External implementation 70 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25190-L25199] |
| E071 | External implementation 71 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25200-L25209] |
| E072 | External implementation 72 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25210-L25219] |
| E073 | External implementation 73 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25220-L25229] |
| E074 | External implementation 74 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25230-L25239] |
| E075 | External implementation 75 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25240-L25249] |
| E076 | External implementation 76 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25250-L25259] |
| E077 | External implementation 77 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25260-L25269] |
| E078 | External implementation 78 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25270-L25279] |
| E079 | External implementation 79 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25280-L25289] |
| E080 | External implementation 80 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25290-L25299] |
| E081 | External implementation 81 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25300-L25309] |
| E082 | External implementation 82 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25310-L25319] |
| E083 | External implementation 83 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25320-L25329] |
| E084 | External implementation 84 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25330-L25339] |
| E085 | External implementation 85 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25340-L25349] |
| E086 | External implementation 86 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25350-L25359] |
| E087 | External implementation 87 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25360-L25369] |
| E088 | External implementation 88 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25370-L25379] |
| E089 | External implementation 89 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25380-L25389] |
| E090 | External implementation 90 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25390-L25399] |
| E091 | External implementation 91 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25400-L25409] |
| E092 | External implementation 92 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25410-L25419] |
| E093 | External implementation 93 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25420-L25429] |
| E094 | External implementation 94 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25430-L25439] |
| E095 | External implementation 95 (original path and symbol in cited row) | caller | [docs/registry/implementation-map.yaml:L25440-L25455] |
| E096 | External implementation 96 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25456-L25465] |
| E097 | External implementation 97 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25466-L25475] |
| E098 | External implementation 98 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25476-L25485] |
| E099 | External implementation 99 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25486-L25495] |
| E100 | External implementation 100 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25496-L25505] |
| E101 | External implementation 101 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25506-L25518] |
| E102 | External implementation 102 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25519-L25528] |
| E103 | External implementation 103 (original path and symbol in cited row) | independent | [docs/registry/implementation-map.yaml:L25529-L25538] |
| E104 | External implementation 104 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25539-L25548] |
| E105 | External implementation 105 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25549-L25558] |
| E106 | External implementation 106 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25559-L25568] |
| E107 | External implementation 107 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25569-L25578] |
| E108 | External implementation 108 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25579-L25588] |
| E109 | External implementation 109 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25589-L25598] |
| E110 | External implementation 110 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25599-L25608] |
| E111 | External implementation 111 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25609-L25618] |
| E112 | External implementation 112 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25619-L25628] |
| E113 | External implementation 113 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25629-L25638] |
| E114 | External implementation 114 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25639-L25648] |
| E115 | External implementation 115 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25649-L25658] |
| E116 | External implementation 116 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25659-L25668] |
| E117 | External implementation 117 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25669-L25678] |
| E118 | External implementation 118 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25679-L25688] |
| E119 | External implementation 119 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25689-L25698] |
| E120 | External implementation 120 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25699-L25708] |
| E121 | External implementation 121 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25709-L25718] |
| E122 | External implementation 122 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25719-L25728] |
| E123 | External implementation 123 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25729-L25738] |
| E124 | External implementation 124 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25739-L25748] |
| E125 | External implementation 125 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25749-L25758] |
| E126 | External implementation 126 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25759-L25770] |
| E127 | External implementation 127 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25771-L25780] |
| E128 | External implementation 128 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25781-L25790] |
| E129 | External implementation 129 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25791-L25800] |
| E130 | External implementation 130 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25801-L25810] |
| E131 | External implementation 131 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25811-L25820] |
| E132 | External implementation 132 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25821-L25830] |
| E133 | External implementation 133 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25831-L25840] |
| E134 | External implementation 134 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25841-L25850] |
| E135 | External implementation 135 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25851-L25860] |
| E136 | External implementation 136 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25861-L25870] |
| E137 | External implementation 137 (original path and symbol in cited row) | test_helper | [docs/registry/implementation-map.yaml:L25871-L25880] |

Exact signal execution: Python `re.search`, case-sensitive, on each package `.py` file at the code pin; omit a complete signal when it matches `solvers/orcaflex/core` case-insensitively. The vocabulary signal in each family is omitted for that reason; retained algorithm and constants/key patterns are used unchanged. [docs/registry/implementation-map.yaml:L4933-L4938] [docs/registry/implementation-map.yaml:L24504-L24509]

Retained patterns for `orcaflex-text-model-writers`:

```text
algorithm: (?:yaml\.(?:dump|safe_dump)|write_text|open\(|\.write\(|save\(|SaveData|dump\().*|to_orcaflex_dict|ObjectType|EndAConnection|LineConfig|VesselConfig|WaveTrains|CurrentRamp|WindSpeed|\.write\(|\.ya?ml|yaml\.safe_dump|shutil\.copy|\.write\(|write_model|ModularModelGenerator|ProjectInputSpec|yaml\.safe_dump|\.write_text
constants_or_keys: [\"\'](?:Lines|LineTypes|Vessels|6DBuoys|General|IncludeFile|includefile)[\"\']|%YAML
```

Retained patterns for `external-solver-batch-run-stacks`:

```text
algorithm: subprocess\.(?:run|Popen|call)|ProcessPoolExecutor|ThreadPoolExecutor|CalculateStatics|RunSimulation|run_batch|batch_run|wait_for_license|license_wait|execute_solver|run_solver|collect_results
```

| Family | Raw matching lines | Enclosing definitions | Further second implementations absent from sections 3 and 9 |
| --- | --- | --- | --- |
| `orcaflex-text-model-writers` | 24 | 13 | 0 after classification below. |
| `external-solver-batch-run-stacks` | 9 | 7 | 0 after classification below. |

| Family | Enclosing definition | Classification and evidence |
| --- | --- | --- |
| `orcaflex-text-model-writers` | `configuration.py` / `OrcaFlexConfig.from_yaml` | Configuration reader, not a native model writer. [src/digitalmodel/solvers/orcaflex/core/configuration.py:L380-L405] |
| `orcaflex-text-model-writers` | `configuration.py` / `OrcaFlexConfig.to_yaml` | Listed map writer and section-3 serializer. [src/digitalmodel/solvers/orcaflex/core/configuration.py:L407-L423] |
| `orcaflex-text-model-writers` | `interfaces.py` / `ModelInterface.save` | Protocol declaration; no executable implementation. [src/digitalmodel/solvers/orcaflex/core/interfaces.py:L38-L59] |
| `orcaflex-text-model-writers` | `interfaces.py` / `ResultInterface.save` | Protocol declaration; no executable implementation. [src/digitalmodel/solvers/orcaflex/core/interfaces.py:L203-L224] |
| `orcaflex-text-model-writers` | `migrate_config.py` / `ConfigurationMigrator.migrate_file` | Delegates configuration conversion/serialization; backup handling is not another model algorithm. [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L49-L131] |
| `orcaflex-text-model-writers` | `migrate_config.py` / `ConfigurationMigrator.migrate_directory` | File discovery and delegated migration. [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L133-L176] |
| `orcaflex-text-model-writers` | `migrate_config.py` / `ConfigurationMigrator._load_legacy_file` | Configuration parser. [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L178-L199] |
| `orcaflex-text-model-writers` | `migrate_config.py` / `ConfigurationMigrator.generate_report` | Migration report text, not native model text. [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L201-L263] |
| `orcaflex-text-model-writers` | `migrate_config.py` / `main` | Command-line options and delegated migration. [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L292-L393] |
| `orcaflex-text-model-writers` | `mock_artifacts.py` / `write_mock_artifact` | Listed in section 3; writes artificial provenance, not a physical model. [src/digitalmodel/solvers/orcaflex/core/mock_artifacts.py:L57-L62] |
| `orcaflex-text-model-writers` | `model_interface.py` / `OrcaFlexModelWrapper.load_file` | Delegates native or placeholder load. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L256-L305] |
| `orcaflex-text-model-writers` | `model_interface.py` / `OrcaFlexModelWrapper.save_simulation` | Listed map writer; delegates native save or marked-artifact helper. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L509-L549] |
| `orcaflex-text-model-writers` | `model_interface.py` / `MockOrcaFlexModel.SaveData` | Listed in section 3; empty-file test placeholder. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L678-L680] |
| `external-solver-batch-run-stacks` | `analysis_engine.py` / `<module>` | Executor import only; no solver launch. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L13-L13] |
| `external-solver-batch-run-stacks` | `analysis_engine.py` / `AnalysisEngine.execute_parallel` | Listed in section 3; schedules delegated workflow work. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L463-L519] |
| `external-solver-batch-run-stacks` | `model_interface.py` / `<module>` | Executor import only; no solver launch. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L17-L17] |
| `external-solver-batch-run-stacks` | `model_interface.py` / `OrcaFlexModelWrapper.run_static_analysis` | Listed map implementation; native solver launch. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L346-L416] |
| `external-solver-batch-run-stacks` | `model_interface.py` / `OrcaFlexModelWrapper.run_dynamic_simulation` | Listed map implementation; native solver launch. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L418-L507] |
| `external-solver-batch-run-stacks` | `model_interface.py` / `MockOrcaFlexModel.CalculateStatics` | Listed in section 3; sleep-only test placeholder. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L666-L668] |
| `external-solver-batch-run-stacks` | `model_interface.py` / `MockOrcaFlexModel.RunSimulation` | Listed in section 3; sleep-only test placeholder. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L670-L672] |

Counted-family result: **PASS**, inventory 440 + 137 = 577 rows; 0 further second implementations after the definition-level classification. No independent engineering result is inferred from a docstring, import, protocol or helper.

Map follow-up: registry/factory behavior, configuration migration, marked mock artifacts and telemetry do not have dedicated in-package map families; add them before treating their future searches as counted-family evidence; issue pending (10). The further repository search found 253 matching lines in 118 tracked Python files, including 48 script files, using `git grep -n -E 'CalculateStatics|RunSimulation|write_mock_artifact|OrcaFlexConfig|ConfigurationMigrator|class ComponentRegistry' HEAD -- '*.py'`;  outside-package hits remain discovery candidates, rather than changes to this counted-family result. [src/digitalmodel/solvers/orcaflex/core/registry.py:L66-L158] [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L49-L176] [src/digitalmodel/solvers/orcaflex/core/mock_artifacts.py:L38-L62] [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L232-L283]

### Solver dependency occurrences (text only)

Every source line matching `orcaflex|orcfxapi` case-insensitively inside the package is indexed below, including comments, declarations and opt-in controls; repeated mentions denote dependency surfaces, not additional solver implementations. No interface availability or licence helper was executed for this inventory.

| Source file | All matching source-line citations at the code pin |
| --- | --- |
| `__init__.py` | [src/digitalmodel/solvers/orcaflex/core/__init__.py:L2-L2] [src/digitalmodel/solvers/orcaflex/core/__init__.py:L4-L4] [src/digitalmodel/solvers/orcaflex/core/__init__.py:L26-L26] [src/digitalmodel/solvers/orcaflex/core/__init__.py:L53-L53] |
| `analysis_engine.py` | [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L2-L2] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L4-L4] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L25-L25] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L27-L27] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L35-L35] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L40-L40] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L88-L88] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L93-L93] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L103-L103] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L117-L117] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L373-L373] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L379-L379] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L558-L558] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L574-L574] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L598-L598] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L622-L622] |
| `base_classes.py` | [src/digitalmodel/solvers/orcaflex/core/base_classes.py:L2-L2] [src/digitalmodel/solvers/orcaflex/core/base_classes.py:L27-L27] |
| `configuration.py` | [src/digitalmodel/solvers/orcaflex/core/configuration.py:L2-L2] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L18-L18] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L22-L22] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L326-L327] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L331-L331] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L380-L380] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L452-L452] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L458-L458] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L470-L470] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L479-L479] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L483-L483] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L493-L493] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L502-L502] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L508-L508] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L524-L525] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L544-L545] |
| `exceptions.py` | [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L2-L2] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L5-L5] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L12-L12] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L14-L14] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L16-L16] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L28-L28] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L38-L38] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L67-L67] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L93-L93] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L119-L119] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L145-L146] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L162-L162] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L171-L171] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L191-L191] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L197-L197] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L227-L227] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L253-L253] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L279-L279] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L305-L305] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L335-L335] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L361-L361] |
| `interfaces.py` | [src/digitalmodel/solvers/orcaflex/core/interfaces.py:L2-L2] [src/digitalmodel/solvers/orcaflex/core/interfaces.py:L4-L4] [src/digitalmodel/solvers/orcaflex/core/interfaces.py:L39-L39] |
| `logging_config.py` | [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L2-L2] [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L4-L4] [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L101-L101] [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L103-L103] [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L143-L144] [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L163-L163] [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L169-L169] [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L190-L190] [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L206-L206] [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L223-L225] [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L301-L301] [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L330-L330] |
| `migrate_config.py` | [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L2-L2] [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L16-L16] [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L22-L22] [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L213-L213] [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L276-L276] [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L295-L295] |
| `mock_artifacts.py` | [src/digitalmodel/solvers/orcaflex/core/mock_artifacts.py:L1-L1] [src/digitalmodel/solvers/orcaflex/core/mock_artifacts.py:L3-L3] |
| `model_interface.py` | [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L2-L2] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L4-L4] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L21-L21] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L28-L28] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L30-L30] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L33-L37] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L40-L41] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L133-L133] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L135-L135] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L137-L137] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L141-L141] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L145-L145] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L156-L156] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L159-L160] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L164-L164] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L182-L182] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L184-L184] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L189-L189] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L191-L191] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L193-L193] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L195-L195] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L199-L199] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L201-L201] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L209-L209] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L211-L211] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L258-L258] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L291-L291] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L294-L294] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L411-L411] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L502-L502] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L606-L606] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L645-L646] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L689-L690] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L692-L692] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L700-L700] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L704-L704] |
| `registry.py` | [src/digitalmodel/solvers/orcaflex/core/registry.py:L34-L34] |

Dependency occurrence count: 134 matching source lines.

| Dependency | Route and known version limit |
| --- | --- |
| OrcFxAPI facade | The wrapper uses lazy binding access; the facade locates the package without importing its binding. A separate convenience helper still directly imports and licence-checks when called; do not call it during offline review. The target version remains open as U3 in section 10. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L33-L41] [src/digitalmodel/solvers/orcaflex/orcaflex_api.py:L198-L210] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L689-L712] |
| Pydantic / YAML | Configuration validates typed models, reads with `safe_load`, writes with `dump`. [src/digitalmodel/solvers/orcaflex/core/configuration.py:L61-L93] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L379-L423] |
| pandas | Interface extraction signatures and the base transformation use DataFrames. [src/digitalmodel/solvers/orcaflex/core/interfaces.py:L133-L154] [src/digitalmodel/solvers/orcaflex/core/base_classes.py:L178-L185] |
| Parent execution surfaces | Optimized, universal and batch callers consume the common marked-artifact writer; the wrapper consumes the same helper. [src/digitalmodel/solvers/orcaflex/core/mock_artifacts.py:L1-L20] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L528-L539] |

## 10. Coverage and open items

Coverage inventory: all 11 package Python files were read as text; there are no nested code subpackages at the pin. Package entry and interface exports, workflow, configuration, migration, model wrapper, logging, exceptions, registry and marked-artifact behavior are covered above. [src/digitalmodel/solvers/orcaflex/core/__init__.py:L8-L61] [src/digitalmodel/solvers/orcaflex/core/interfaces.py:L14-L249] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L1-L643] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L1-L571] [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L1-L397] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L1-L712] [src/digitalmodel/solvers/orcaflex/core/logging_config.py:L1-L330] [src/digitalmodel/solvers/orcaflex/core/exceptions.py:L1-L384] [src/digitalmodel/solvers/orcaflex/core/registry.py:L1-L327] [src/digitalmodel/solvers/orcaflex/core/base_classes.py:L1-L297] [src/digitalmodel/solvers/orcaflex/core/mock_artifacts.py:L1-L62]

Documentation discovery used `rg -l 'orcaflex[./]core|OrcaFlex Core|OrcaFlexConfig|OrcaFlexModelWrapper|ConfigurationMigrator' docs --glob '*.md' --glob '*.rst'`; the three results are listed here without treating historical plans or example snippets as current execution evidence. [docs/domains/testing/VALIDATION_FRAMEWORK_BEST_PRACTICES.md:L83-L95] [docs/plans/2026-06-12-issue-703-orcaflex-detector-routing.md:L60-L80] [docs/plans/2026-08-05-issue-1631-licensed-lane-fail-closed.md:L111-L118]

| Documentation found | Treatment |
| --- | --- |
| `docs/domains/testing/VALIDATION_FRAMEWORK_BEST_PRACTICES.md` | Core import example uses the former namespace; inspect current code before reuse; issue pending (9). [docs/domains/testing/VALIDATION_FRAMEWORK_BEST_PRACTICES.md:L83-L95] |
| `docs/plans/2026-06-12-issue-703-orcaflex-detector-routing.md` | Historical detector example names a former core-file path; no current behavior inferred. [docs/plans/2026-06-12-issue-703-orcaflex-detector-routing.md:L60-L80] |
| `docs/plans/2026-08-05-issue-1631-licensed-lane-fail-closed.md` | Historical mock-fallback analysis compared with current wrapper and tests. [docs/plans/2026-08-05-issue-1631-licensed-lane-fail-closed.md:L111-L118] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L163-L186] |

Test discovery used `rg -l 'orcaflex[.]core|orcaflex/core|OrcaFlexConfig|OrcaFlexModelWrapper|AnalysisEngine|ComponentRegistry|BaseAnalyzer|ConfigurationMigrator|OrcaFlexLogger' tests --glob '*.py'`; unrelated fatigue/automation matches were checked for scope and excluded from this package's test count. The package-local test directory contains an execution helper, while the five direct wrapper regression definitions are in `test_mock_is_explicit.py`. [tests/solvers/orcaflex/core/scripts/run_core.py:L1-L28] [tests/solvers/orcaflex/test_mock_is_explicit.py:L137-L212]

| Test inventory | Classification and result |
| --- | --- |
| `tests/solvers/orcaflex/test_mock_is_explicit.py` | 20 tests; all PASS at BASE and AFTER; five direct wrapper cases listed in section 4. [tests/solvers/orcaflex/test_mock_is_explicit.py:L33-L328] |
| `tests/solvers/orcaflex/test_optimized_parallel_contract.py` | 32 expanded tests; all PASS at BASE and AFTER; shared-helper and adjacent-worker regression coverage. [tests/solvers/orcaflex/test_optimized_parallel_contract.py:L1-L549] |
| `tests/solvers/orcaflex/test_orcaflex_unit.py` | Legacy definitions blocked at collection by missing exports; not a physical validation suite; issue pending (9). [tests/solvers/orcaflex/test_orcaflex_unit.py:L12-L21] |
| `tests/solvers/orcaflex/core/scripts/run_core.py`, `tests/solvers/orcaflex/run_tests.py` | NOT_RUN execution helpers: solver/licence query prohibited in offline work. [tests/solvers/orcaflex/core/scripts/run_core.py:L12-L25] [tests/solvers/orcaflex/run_tests.py:L15-L37] |

The former inventory estimate is replaced by the observed counts above; no independent physical worked example is claimed from these software contracts. [tests/solvers/orcaflex/test_mock_is_explicit.py:L137-L212] [tests/solvers/orcaflex/test_optimized_parallel_contract.py:L1-L8]

| Open item | Responsible review role | Tracking and next evidence |
| --- | --- | --- |
| Engine construction, factory use, execution-result type and timeout behavior | Software maintainer | issue pending (1); add solver-free construction/execution regressions. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L93-L107] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L428-L434] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L497-L517] |
| Missing inputs and empty successful workflows | Software maintainer | issue pending (2); require requested/processed input reconciliation. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L211-L249] |
| Placeholder iteration, unapplied settings and progress | Analysis maintainer | issue pending (3); demonstrate native parameter application and real response extraction. [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L317-L356] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L362-L416] |
| Direct mock bypass and incorrect workflow output metadata | Software maintainer | issue pending (4); extend #1631/#2521 contracts to direct mocks and workflows. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L645-L685] [src/digitalmodel/solvers/orcaflex/core/analysis_engine.py:L188-L196] |
| Suppressed errors, cleanup and telemetry sentinels | Software maintainer | issue pending (5); add explicit diagnostic-result tests. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L567-L577] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L634-L642] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L697-L712] |
| Migration backup parsing and unsuccessful directory exit status | Software maintainer | issue pending (6); reproduce only on disposable copies, preserving original evidence. [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L86-L131] [src/digitalmodel/solvers/orcaflex/core/migrate_config.py:L367-L393] |
| U1: physical unit/frame/sign/phase conventions; strict preflight, key-loss and fingerprint coverage | Analysis maintainer | issue pending (7); document native units and implement input reconciliation. [src/digitalmodel/solvers/orcaflex/core/configuration.py:L379-L423] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L505-L552] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L608-L613] |
| U2: engineering standard/edition/clause; independent physical example and complete gates/appendix | Family quality manager | issue pending (8); select a public reference and target version, record G1–G5 evidence and a shared lesson record. No licensed clause text was checked. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L377-L403] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L463-L482] |
| UNVERIFIED U3: exact native solver version for load, statics and dynamics | Family quality manager | issue pending (8); record the actual loaded binding version with native evidence. [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L33-L41] [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L198-L201] |
| Legacy test imports and documentation namespace drift | Software maintainer | issue pending (9); restore solver-free collection before counting the legacy cases. [tests/solvers/orcaflex/test_orcaflex_unit.py:L12-L21] [docs/domains/testing/VALIDATION_FRAMEWORK_BEST_PRACTICES.md:L83-L95] |
| Routing contract expectation for the new core submodule | Software maintainer | issue pending (11); extend the enumerated expected routes together with the new registry entry. [tests/docs/test_digitalmodel_routing_contract.py:L217-L254] |
| Method-family map additions and discovery codenaming | Documentation maintainer | issue pending (10); reconcile configuration migration, registry, artifact and logging methods before adding counted-family scope. [src/digitalmodel/solvers/orcaflex/core/registry.py:L66-L158] [src/digitalmodel/solvers/orcaflex/core/mock_artifacts.py:L38-L62] |

Keep client-derived quantities outside this public pack; select public reference evidence for the open physical qualification.
