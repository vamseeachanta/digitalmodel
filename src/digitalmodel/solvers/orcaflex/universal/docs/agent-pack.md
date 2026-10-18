# Universal runner knowledge pack

| Header | Value |
| --- | --- |
| Module | solvers/orcaflex/universal |
| Repository | digitalmodel |
| Package path | src/digitalmodel/solvers/orcaflex/universal |
| Pack revision | 0, 2026-10-18 |
| Code revision | c099a5e1317e6bed63d876252d87cbb12c774372 |
| Analysis family | OrcaFlex riser and mooring |
| Family quality manager | OrcaFlex riser/mooring quality manager |
| Status | DRAFT |

Repository citations use the code revision above; section 9 explicitly distinguishes the implementation-map data pin.

## 1. Purpose and scope

The runner discovers model files, resolves input/output directories, dispatches sequential or threaded work, and aggregates per-model success, errors and artifact paths. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L274-L334]

Sequential execution supports static, dynamic and combined requests; threaded default processing performs statics only, even when dynamics were requested. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L411-L453] [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L258-L314]

Explicit mock execution writes marked artifacts; its success means completion of an artificial workflow, without a physical solution. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L387-L409] [src/digitalmodel/solvers/orcaflex/core/mock_artifacts.py:L9-L62]

The routing registry classifies the enclosing solvers domain as beta; this is domain metadata, not demonstrated engineering qualification of this runner. [docs/registry/module-routing.yaml:L353-L359]

UNVERIFIED: target solver version and supported geometry, water depth and load envelopes have not been qualified; issue pending (5).

## 2. Inputs and outputs

### Inputs

| Input | Unit | Allowed range / default | Source and behavior |
| --- | --- | --- | --- |
| `base_dir`, model paths, input/output directories | filesystem path | Default base: current directory; default output: input directory plus `sim` | Constructor and `run`; relative paths try current directory, then repository, then current-directory fallback. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L93-L120] [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L274-L288] [src/digitalmodel/solvers/orcaflex/universal/path_resolver.py:L131-L162] |
| `pattern`, `recursive`, `exclude_patterns` | dimensionless | Default `*.yml`, false, built-in exclusions plus caller exclusions | Discovery accepts glob or detected regex; extension candidates are `.yml`, `.yaml`, `.dat`, `.sim`. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L29-L52] [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L76-L124] |
| `analysis_type`, `parallel` | dimensionless | `static`, `dynamic`, `both`; default static and parallel true | Validation precedes discovery; multiple files with parallel true select the batch processor. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L202-L213] [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L249-L260] [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L311-L325] |
| `simulation_time` | s, as documented | Default 100.0 for dynamic or combined requests; no positive/finite bound enforced here | Sequential execution attempts to set `General.StageDuration[0]`; errors retain model defaults. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L227-L227] [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L254-L260] [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L414-L432] |
| `mock_mode`, environment opt-in | Boolean | False; explicit environment values `1`, `true`, `yes` also opt in | `ORCAFLEX_FORCE_MOCK` and `ORCAFLEX_SKIP_REAL`; unavailable binding otherwise raises `LicenseError`. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L24-L32] [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L113-L119] [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L144-L165] |
| `max_workers`, `license_timeout` | worker count; s | Constructor defaults 30 and 300 | Workers are passed into lazy batch construction; timeout is stored but not used by either launch implementation. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L93-L120] [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L183-L192] [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L371-L460] [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L258-L314] |
| `config_file` | YAML mapping | Existing file; `processing` mapping defaults to empty | Only selected directory/pattern arguments are promoted; processing kwargs already contain analysis/duration defaults and override matching configuration keys. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L249-L272] [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L336-L347] |

### Outputs

| Output | Unit | Range / default | Source and behavior |
| --- | --- | --- | --- |
| `RunResults` | counts, %, s, paths | Empty counters start at 0; success rate is 0.0 for no models | Per-model results, errors and successful artifact paths are aggregated. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L51-L73] [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L462-L482] |
| Real simulation artifact | solver-native `.sim` | One filename per input stem | Sequential and batch implementations save after their respective state checks; equal stems share an output name. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L434-L453] [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L297-L308] |
| Artificial artifact | marked text | Separate directory and `.mock.sim` infix | Artifact helper separates artificial output from real output. [src/digitalmodel/solvers/orcaflex/core/mock_artifacts.py:L9-L62] |
| `StatusReporter.generate_report()` | counts, %, s, local ISO timestamps | Missing timing samples become numeric zeros | Summary, performance, output files and failed models; saving JSON does not supply engineering gate evidence. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L279-L326] |

UNVERIFIED: physical units, coordinate origin, axes, signs and phase conventions of loaded solver files are not established by these workflow checks; issue pending (5).

## 3. Method and governing standards

| Method | Implementation and limit |
| --- | --- |
| Path lookup | Existing current-directory paths precede repository paths; unresolved paths are returned rather than rejected. [src/digitalmodel/solvers/orcaflex/universal/path_resolver.py:L131-L162] [src/digitalmodel/solvers/orcaflex/universal/path_resolver.py:L180-L209] |
| Discovery | Filter candidate names using glob/regex, apply exclusions, validate heuristically, and sort by basename; YAML requires any recognized top-level key, DAT checks header strings, SIM checks size only. These are not solver load checks. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L85-L124] [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L189-L263] |
| Sequential solver launch | Construct `OrcFxAPI.Model(path)`; attempt stage-duration assignment; call `CalculateStatics` and/or `RunSimulation`; check state before saving. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L411-L453] |
| Batch solver launch | Construct empty model, dispatch YAML to `LoadData` or DAT to `LoadDataFile`, calculate statics, check state and save; unsupported suffix raises a captured failure. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L283-L314] |
| Worker selection | Prefer file-size optimizer; fallback caps by model count and CPU count, assumes 2 GiB per real instance, and reduces workers above 80 percent CPU load; total memory is measured at construction. This is a heuristic, not a solver memory guarantee. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L63-L65] [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L138-L191] |
| Completion accounting | `success_rate = successful / total * 100` for positive total; zero otherwise. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L70-L73] |

Engineering acceptance uses AQG r0, sections 4–10: workflow success alone cannot establish input, load, statics, dynamics, diffraction or appendix clearance. (AQG r0, sections 4–10.)

UNVERIFIED: no governing engineering standard edition or independently sourced engineering calculation is qualified for this module; issue pending (5).

## 4. Validated worked examples

Validated engineering examples: 0; the dedicated tests use explicit artificial execution and contain no independently sourced physical expected values. [tests/solvers/orcaflex/universal/test_universal_runner.py:L49-L61] [tests/solvers/orcaflex/universal/test_universal_runner.py:L75-L181] [tests/solvers/orcaflex/universal/test_universal_runner.py:L219-L259]

The following regression examples have exact count or string expectations, with zero tolerance in counts or characters; they are workflow assertions rather than engineering references. [tests/solvers/orcaflex/universal/test_universal_runner.py:L57-L181]

| Regression case | Input / expected value and unit | Existing test node ID | Offline result at code pin, BASE and after |
| --- | --- | --- | --- |
| Initialization | Explicit artificial execution; maximum workers equals 30 workers; literals from assertions | `tests/solvers/orcaflex/universal/test_universal_runner.py::TestUniversalRunner::test_initialization` | PASS / PASS. [tests/solvers/orcaflex/universal/test_universal_runner.py:L57-L61] |
| Path resolution | None resolves to current directory; 0 path differences; literals from assertions | `tests/solvers/orcaflex/universal/test_universal_runner.py::TestUniversalRunner::test_path_resolver` | PASS / PASS. [tests/solvers/orcaflex/universal/test_universal_runner.py:L63-L73] |
| Discovery | Two included YAML files; 0 backup files; literals from assertions | `tests/solvers/orcaflex/universal/test_universal_runner.py::TestUniversalRunner::test_model_discovery` | PASS / PASS. [tests/solvers/orcaflex/universal/test_universal_runner.py:L75-L99] |
| Artificial processing | 1 total, 1 successful, 0 failed models; at least 1 artifact; literals from assertions | `tests/solvers/orcaflex/universal/test_universal_runner.py::TestUniversalRunner::test_mock_processing` | PASS / PASS. [tests/solvers/orcaflex/universal/test_universal_runner.py:L101-L116] |
| Pattern filtering | 2 total and 2 successful models; literals from assertions | `tests/solvers/orcaflex/universal/test_universal_runner.py::TestUniversalRunner::test_pattern_matching` | PASS / PASS. [tests/solvers/orcaflex/universal/test_universal_runner.py:L118-L136] |
| Reporter accounting | 2 completed, 1 successful and 1 failed model; 1 failure row; literals from assertions | `tests/solvers/orcaflex/universal/test_universal_runner.py::TestUniversalRunner::test_status_reporter` | PASS / PASS. [tests/solvers/orcaflex/universal/test_universal_runner.py:L138-L157] |
| Batch processing | 5 returned and 5 successful models; literals from assertions | `tests/solvers/orcaflex/universal/test_universal_runner.py::TestUniversalRunner::test_batch_processor` | PASS / PASS. [tests/solvers/orcaflex/universal/test_universal_runner.py:L159-L181] |
| Native-file fixture workflow | No independent expected physical value; artificial outputs only | `tests/solvers/orcaflex/universal/test_universal_runner.py::TestRealOrcaFlexFiles::test_real_dat_files` | NOT_RUN / NOT_RUN: pytest SKIP, `No real .dat files available for testing`; the fixture source directory does not exist at the code pin. [tests/solvers/orcaflex/universal/test_universal_runner.py:L193-L222] |

Run records: dedicated suite, 8 collected / 7 passed / 0 failed / 1 skipped (the same node recorded NOT_RUN above); supplemental package-specific refusal/artifact checks, 7 collected / 7 passed / 0 failed / 0 skipped; contracts plus routing, 497 collected / 497 passed / 0 failed / 0 skipped, at BASE and after. The code pin is unchanged by this documentation change.

Command form: `PYTHONPATH=src uv run --no-sources python -m pytest <paths-or-node-IDs> -q -rA -p no:cacheprovider --basetemp=<scratch>`; the dedicated suite ran from a byte-identical temporary test copy because its fixtures write beside the test file. No addopts override was used. [tests/solvers/orcaflex/universal/test_universal_runner.py:L34-L47] [tests/solvers/orcaflex/universal/test_universal_runner.py:L187-L217]

## 5. QA gates

These are acceptance checks, not a claim of cleared solver results; solver-dependent checks remain NOT_RUN. Gate applicability follows AQG r0, section 10, OrcaFlex riser and mooring, and section 9 for G5. (AQG r0, sections 9–10.)

| Gate | Applicable check IDs | Module-specific check and numeric pass criterion | Evidence / present result |
| --- | --- | --- | --- |
| G0 | U01, U02, U03 | U01: `ModelDiscovery.find_models` returns exactly 2 included files and 0 backup files; U02: `PathResolver.resolve_path(None)` differs from current directory by 0 paths; U03: explicit mock refusal/artifact tests have 0 failed assertions across 7 tests | PASS workflow regressions only; strict YAML, reference resolution, unit and input fingerprint qualification remains absent. [tests/solvers/orcaflex/universal/test_universal_runner.py:L63-L99] [tests/solvers/orcaflex/test_mock_is_explicit.py:L84-L132] [tests/solvers/orcaflex/test_mock_is_explicit.py:L238-L264] [tests/solvers/orcaflex/test_mock_is_explicit.py:L317-L328] |
| G1 | U04 | On the target OrcaFlex version, load each sequential input via `OrcFxAPI.Model(path)` and each batch input via `Model()` plus `LoadData`/`LoadDataFile`; require 0 load errors, 0 warnings and 0 objects differing from manifest; record 1 exact target-version match per input | NOT_RUN: solver and licensed interface excluded; retain native load transcript and object inventory. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L411-L420] [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L283-L295] (AQG r0, section 5.) |
| G2 | U05 | After `CalculateStatics`, require 1 recognized completed state from `check_statics`, 0 unchecked components, at least 6 offset steps, top tension difference ≤5 percent and top-angle difference ≤1.0 degree against an independent reference; preserve images and component rows | NOT_RUN: licensed statics and independent model reference unavailable; `success=True` alone is insufficient. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L434-L440] [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L297-L305] [src/digitalmodel/solvers/orcaflex/run_state.py:L63-L70] (AQG r0, section 6.) |
| G3 | U06 | For a sequential dynamic request, read back every `General.StageDuration` value and stop time; require a 1.0 s requested short run, 0 duration mismatches in s, 1 recognized completed state from `check_simulation`, 0 errors/warnings/nonfinite values and tension difference ≤25 percent from statics | NOT_RUN: solver unavailable; batch execution cannot provide this evidence because it never calls `RunSimulation`; do not use a batch success as dynamics clearance. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L422-L446] [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L283-L308] (AQG r0, section 7.) |
| G4 | U07 | Conditional: when a loaded model uses a new diffraction result, require 1 cited G4 record per imported body, 0 unmatched input fingerprints and weight/buoyancy imbalance ≤1.0 percent of weight | NOT_APPLICABLE to the artificial fixtures; NOT_RUN for a real model until its diffraction provenance is supplied. [tests/solvers/orcaflex/universal/test_universal_runner.py:L79-L87] (AQG r0, sections 8 and 10.) |
| G5 | U08, U09 | U08: `StatusReporter.log_result` gives exactly 2 completed, 1 success, 1 failure and 1 failure row; U09: compare `generate_report()` against `RunResults`, requiring 0 missing model/artifact rows, 0 count differences and 1 signed gate/fingerprint record per input | U08 PASS; U09 NOT_RUN because no complete engineering record exists; the batch reporter receives counters without per-model details. [tests/solvers/orcaflex/universal/test_universal_runner.py:L138-L157] [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L340-L350] [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L279-L309] (AQG r0, section 9.) |

Check totals: G0 3, G1 1, G2 1, G3 1, G4 1 conditional, G5 2: 9 defined checks. Proposed native checks U04–U07 and reconciliation U09 are not automated regression coverage; issue pending (5).

## 6. Known failure modes and pitfalls

Silent-failure sweep method: Python AST inspection of exception handlers, `continue`, empty/None returns, numeric zero/fraction defaults and numeric conversions, followed by line-by-line inspection of all package Python files for unit assumptions, skipped records and empty cache writes.

Exact text-search command:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|\b0(?:\.0)?\b|1970|0\.05|\b(?:int|float)\(|errors=|unit|cache' src/digitalmodel/solvers/orcaflex/universal -g '*.py'
```

sweep: 121 found

The inventory below contains 94 AST candidate locations and 27 additional semantic sites; intentional counters and display branches are retained to expose the full sweep, rather than being treated as engineering defects. No `continue`, epoch-1970 default, fixed 0.05 fraction, float conversion of external text or empty-result cache write occurs in the read package files.

| Site | Symptom / disposition | Detection / tracking |
| --- | --- | --- |
| Default exclusions and reset timing | Included names may be removed; missing-directory returns retain previous statistics. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L77-L83] | no detection check exists; issue pending (1). |
| Record omission | Excluded or invalid candidates never become failed result rows. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L97-L112] | no detection check exists; issue pending (1). |
| Permissive YAML validation | Recognized keys can pass without strict duplicate-key, reference or unit checks. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L225-L242] | no detection check exists; issue pending (1). |
| Decode errors ignored | DAT decoding discards errors; only first 10 lines are searched. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L248-L255] | no detection check exists; issue pending (1). |
| Size-only SIM validation | More than 100 bytes does not establish a loadable simulation. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L260-L263] | no detection check exists; issue pending (1). |
| Related-model duplicates | Overlapping globs can append a model repeatedly. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L289-L295] | no detection check exists; issue pending (1). |
| Identifying discovery text | Grouping vocabulary needs neutral identifiers before reuse in public evidence. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L309-L332] | no detection check exists; issue pending (6). |
| Root heuristic | Only source-directory existence is required, despite additional listed indicator. [src/digitalmodel/solvers/orcaflex/universal/path_resolver.py:L98-L104] | no detection check exists; issue pending (4). |
| Path fallback | Unresolved input is returned without an existence failure. [src/digitalmodel/solvers/orcaflex/universal/path_resolver.py:L155-L162] | no detection check exists; issue pending (1). |
| Model lookup fallback | A name can resolve through search order or return nonexistent path. [src/digitalmodel/solvers/orcaflex/universal/path_resolver.py:L201-L209] | no detection check exists; issue pending (1). |
| Default duration | A dynamic request with no duration receives 100.0 s. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L254-L256] | no detection check exists; issue pending (2). |
| Configuration precedence | Defaults overwrite requested configuration; several named settings stay in unused kwargs. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L262-L272] | no detection check exists; issue pending (2). |
| Empty result | No models becomes zero counters; command can report success. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L300-L302] | no detection check exists; issue pending (1). |
| Different launch route | Multiple inputs with parallel true select static-only default batch processor. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L312-L325] | no detection check exists; issue pending (2). |
| Reporter omitted | Sequential execution does not feed the caller reporter. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L349-L369] | no detection check exists; issue pending (3). |
| Artificial success | Success flag describes artificial artifact creation, not physical completion. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L401-L409] | no detection check exists; issue pending (5). |
| Duration fallback | Only stage zero is set; failure retains model defaults. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L423-L432] | no detection check exists; issue pending (2). |
| Output collision | Input paths with the same stem write the same real output filename. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L449-L450] | no detection check exists; issue pending (1). |
| Resource assumption | Total memory is sampled, not currently available memory. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L63-L65] | no detection check exists; issue pending (4). |
| Resource cap bypass | Optimizer return precedes fallback CPU/model/memory caps; fallback assumes 2 GiB per instance. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L149-L179] | no detection check exists; issue pending (4). |
| Ignored dynamic settings | Default processor ignores analysis/duration kwargs and performs statics only. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L283-L308] | no detection check exists; issue pending (2). |
| Partial report | Counter update omits file list, failed detail list and duration samples. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L340-L350] | no detection check exists; issue pending (3). |
| External numeric text | Only unsigned integer text is converted; decimals, negatives and separators remain strings; tokens without equals vanish. [src/digitalmodel/solvers/orcaflex/universal/__main__.py:L29-L52] | no detection check exists; issue pending (2). |
| Unused test limit | test_mode enters kwargs but execution does not cap the model list. [src/digitalmodel/solvers/orcaflex/universal/__main__.py:L145-L147] | no detection check exists; issue pending (2). |
| Empty command success | Zero failures returns success even if no models were processed. [src/digitalmodel/solvers/orcaflex/universal/__main__.py:L200-L206] | no detection check exists; issue pending (1). |
| Missing samples represented by zeros | Unstarted timing and absent samples are indistinguishable from measured zeros in report fields. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L286-L308] | no detection check exists; issue pending (3). |
| Reporter reuse | Caller must reset reporter; run does not reset lists and samples automatically. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L343-L354] | no detection check exists; issue pending (3). |
| numeric conversion, __main__.py:49 | Unsigned external integer conversion. [src/digitalmodel/solvers/orcaflex/universal/__main__.py:L49-L49] | no detection check exists for this individual branch; issue pending (2). |
| zero or fixed fraction, __main__.py:201 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/__main__.py:L201-L201] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, __main__.py:206 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/__main__.py:L206-L206] | no detection check exists for this individual branch; issue pending (4). |
| exception handler, __main__.py:208 | Interrupt/error becomes nonzero exit status; no solver success inferred. [src/digitalmodel/solvers/orcaflex/universal/__main__.py:L208-L208] | no detection check exists for this individual branch; issue pending (2). |
| exception handler, __main__.py:211 | Interrupt/error becomes nonzero exit status; no solver success inferred. [src/digitalmodel/solvers/orcaflex/universal/__main__.py:L211-L211] | no detection check exists for this individual branch; issue pending (2). |
| zero or fixed fraction, batch_processor.py:55 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L55-L55] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, batch_processor.py:56 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L56-L56] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, batch_processor.py:60 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L60-L60] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, batch_processor.py:61 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L61-L61] | no detection check exists for this individual branch; issue pending (4). |
| empty or None return, batch_processor.py:91 | Missing/empty work, unavailable root or display/performance precondition returns an empty value. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L91-L91] | no detection check exists for this individual branch; issue pending (3). |
| zero or fixed fraction, batch_processor.py:95 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L95-L95] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, batch_processor.py:97 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L97-L97] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, batch_processor.py:98 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L98-L98] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, batch_processor.py:150 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L150-L150] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, batch_processor.py:153 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L153-L153] | no detection check exists for this individual branch; issue pending (4). |
| exception handler, batch_processor.py:164 | Exception becomes fallback or captured diagnostic; inspect caller before treating it as success. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L164-L164] | no detection check exists for this individual branch; issue pending (4). |
| numeric conversion, batch_processor.py:178 | Internal display/resource arithmetic converted to integer; no external decimal parsing. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L178-L178] | no detection check exists for this individual branch; issue pending (4). |
| exception handler, batch_processor.py:188 | Exception becomes fallback or captured diagnostic; inspect caller before treating it as success. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L188-L188] | no detection check exists for this individual branch; issue pending (4). |
| exception handler, batch_processor.py:224 | Worker/processor exception becomes a failed result row. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L224-L224] | no detection check exists for this individual branch; issue pending (3). |
| zero or fixed fraction, batch_processor.py:242 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L242-L242] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, batch_processor.py:266 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L266-L266] | no detection check exists for this individual branch; issue pending (4). |
| exception handler, batch_processor.py:310 | Worker/processor exception becomes a failed result row. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L310-L310] | no detection check exists for this individual branch; issue pending (3). |
| zero or fixed fraction, batch_processor.py:331 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L331-L331] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, batch_processor.py:336 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L336-L336] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, batch_processor.py:360 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L360-L360] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, batch_processor.py:371 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L371-L371] | no detection check exists for this individual branch; issue pending (4). |
| empty or None return, batch_processor.py:384 | Missing/empty work, unavailable root or display/performance precondition returns an empty value. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L384-L384] | no detection check exists for this individual branch; issue pending (3). |
| zero or fixed fraction, batch_processor.py:393 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L393-L393] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, batch_processor.py:395 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L395-L395] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, batch_processor.py:396 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L396-L396] | no detection check exists for this individual branch; issue pending (4). |
| empty or None return, model_discovery.py:70 | Missing/empty work, unavailable root or display/performance precondition returns an empty value. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L70-L70] | no detection check exists for this individual branch; issue pending (1). |
| empty or None return, model_discovery.py:74 | Missing/empty work, unavailable root or display/performance precondition returns an empty value. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L74-L74] | no detection check exists for this individual branch; issue pending (1). |
| zero or fixed fraction, model_discovery.py:204 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L204-L204] | no detection check exists for this individual branch; issue pending (4). |
| exception handler, model_discovery.py:215 | Validation exception becomes false; rejected input is omitted from execution. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L215-L215] | no detection check exists for this individual branch; issue pending (1). |
| exception handler, model_discovery.py:241 | Validation exception becomes false; rejected input is omitted from execution. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L241-L241] | no detection check exists for this individual branch; issue pending (1). |
| exception handler, model_discovery.py:257 | Validation exception becomes false; rejected input is omitted from execution. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L257-L257] | no detection check exists for this individual branch; issue pending (1). |
| zero or fixed fraction, model_discovery.py:350 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L350-L350] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, model_discovery.py:358 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L358-L358] | no detection check exists for this individual branch; issue pending (4). |
| exception handler, path_resolver.py:92 | Exception becomes fallback or captured diagnostic; inspect caller before treating it as success. [src/digitalmodel/solvers/orcaflex/universal/path_resolver.py:L92-L92] | no detection check exists for this individual branch; issue pending (4). |
| empty or None return, path_resolver.py:96 | Missing/empty work, unavailable root or display/performance precondition returns an empty value. [src/digitalmodel/solvers/orcaflex/universal/path_resolver.py:L96-L96] | no detection check exists for this individual branch; issue pending (1). |
| exception handler, path_resolver.py:276 | Exception becomes fallback or captured diagnostic; inspect caller before treating it as success. [src/digitalmodel/solvers/orcaflex/universal/path_resolver.py:L276-L276] | no detection check exists for this individual branch; issue pending (4). |
| exception handler, path_resolver.py:310 | Exception becomes fallback or captured diagnostic; inspect caller before treating it as success. [src/digitalmodel/solvers/orcaflex/universal/path_resolver.py:L310-L310] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:39 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L39-L39] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:40 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L40-L40] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:41 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L41-L41] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:42 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L42-L42] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:62 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L62-L62] | no detection check exists for this individual branch; issue pending (4). |
| exception handler, status_reporter.py:73 | Color or terminal-title failure loses display diagnostics while processing continues. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L73-L73] | no detection check exists for this individual branch; issue pending (4). |
| exception handler, status_reporter.py:78 | Color or terminal-title failure loses display diagnostics while processing continues. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L78-L78] | no detection check exists for this individual branch; issue pending (4). |
| empty or None return, status_reporter.py:87 | Missing/empty work, unavailable root or display/performance precondition returns an empty value. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L87-L87] | no detection check exists for this individual branch; issue pending (3). |
| zero or fixed fraction, status_reporter.py:90 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L90-L90] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:93 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L93-L93] | no detection check exists for this individual branch; issue pending (4). |
| exception handler, status_reporter.py:114 | Color or terminal-title failure loses display diagnostics while processing continues. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L114-L114] | no detection check exists for this individual branch; issue pending (4). |
| empty or None return, status_reporter.py:128 | Missing/empty work, unavailable root or display/performance precondition returns an empty value. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L128-L128] | no detection check exists for this individual branch; issue pending (3). |
| zero or fixed fraction, status_reporter.py:132 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L132-L132] | no detection check exists for this individual branch; issue pending (4). |
| empty or None return, status_reporter.py:133 | Missing/empty work, unavailable root or display/performance precondition returns an empty value. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L133-L133] | no detection check exists for this individual branch; issue pending (3). |
| numeric conversion, status_reporter.py:137 | Internal display/resource arithmetic converted to integer; no external decimal parsing. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L137-L137] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:143 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L143-L143] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:153 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L153-L153] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:160 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L160-L160] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:179 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L179-L179] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:204 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L204-L204] | no detection check exists for this individual branch; issue pending (4). |
| empty or None return, status_reporter.py:214 | Missing/empty work, unavailable root or display/performance precondition returns an empty value. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L214-L214] | no detection check exists for this individual branch; issue pending (3). |
| zero or fixed fraction, status_reporter.py:231 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L231-L231] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:253 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L253-L253] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:286 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L286-L286] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:293 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L293-L293] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:299 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L299-L299] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:300 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L300-L300] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:301 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L301-L301] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:302 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L302-L302] | no detection check exists for this individual branch; issue pending (4). |
| numeric conversion, status_reporter.py:335 | Internal display/resource arithmetic converted to integer; no external decimal parsing. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L335-L335] | no detection check exists for this individual branch; issue pending (4). |
| numeric conversion, status_reporter.py:336 | Internal display/resource arithmetic converted to integer; no external decimal parsing. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L336-L336] | no detection check exists for this individual branch; issue pending (4). |
| numeric conversion, status_reporter.py:339 | Internal display/resource arithmetic converted to integer; no external decimal parsing. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L339-L339] | no detection check exists for this individual branch; issue pending (4). |
| numeric conversion, status_reporter.py:340 | Internal display/resource arithmetic converted to integer; no external decimal parsing. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L340-L340] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:345 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L345-L345] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:346 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L346-L346] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:347 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L347-L347] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:348 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L348-L348] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:354 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L354-L354] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, status_reporter.py:358 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L358-L358] | no detection check exists for this individual branch; issue pending (4). |
| exception handler, status_reporter.py:373 | Color or terminal-title failure loses display diagnostics while processing continues. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L373-L373] | no detection check exists for this individual branch; issue pending (4). |
| exception handler, universal_runner.py:43 | ImportError becomes unavailable; constructor raises without explicit mock opt-in. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L43-L43] | no detection check exists for this individual branch; issue pending (5). |
| zero or fixed fraction, universal_runner.py:54 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L54-L54] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, universal_runner.py:55 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L55-L55] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, universal_runner.py:56 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L56-L56] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, universal_runner.py:58 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L58-L58] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, universal_runner.py:73 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L73-L73] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, universal_runner.py:302 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L302-L302] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, universal_runner.py:382 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L382-L382] | no detection check exists for this individual branch; issue pending (4). |
| zero or fixed fraction, universal_runner.py:429 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L429-L429] | no detection check exists for this individual branch; issue pending (4). |
| exception handler, universal_runner.py:431 | Duration assignment fails and execution continues with model defaults. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L431-L431] | no detection check exists for this individual branch; issue pending (2). |
| exception handler, universal_runner.py:455 | Failure remains success=false with error text; caller must inspect it. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L455-L455] | no detection check exists for this individual branch; issue pending (3). |
| zero or fixed fraction, universal_runner.py:495 | Intentional counter, sentinel, bound or display arithmetic; does not independently establish a physical result. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L495-L495] | no detection check exists for this individual branch; issue pending (4). |

Historical incident #1631: implicit artificial success and misleading real-looking artifacts are now guarded by explicit opt-in and separate artifact naming; U03 detects the package-specific refusal and naming regressions. The historical issue is not evidence that current physical runs are qualified. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L144-L165] [tests/solvers/orcaflex/test_mock_is_explicit.py:L84-L132] [tests/solvers/orcaflex/test_mock_is_explicit.py:L238-L264]

Known state limitation: the shared state helper deliberately permits indeterminate/unreadable states; require a recognized state for U05/U06 instead of interpreting no exception as proof. [src/digitalmodel/solvers/orcaflex/run_state.py:L18-L22]

## 7. Lessons log

| Date | Entry | Lesson / change | Evidence | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-18 | Initial pack | Separate artificial workflow regressions from native solver qualification; record static-only batch execution and stage-duration fallback | [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L283-L314] [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L422-L446] | None; issue pending (5) |

## 8. Ownership

Designated review roles: OrcaFlex riser/mooring quality manager; deputy: OrcaFlex riser/mooring deputy quality manager; producing discipline: solver workflow engineering; unresolved evidence and classification questions go to the family quality manager, then the maintainer for cross-family scope. These are review assignments for this draft, not a recorded clearance.

## 9. Dependencies and implementation inventory

### Runtime and solver dependencies

| Dependency | Usage / citation |
| --- | --- |
| `yaml`, `psutil`, standard-library filesystem/concurrency/terminal support | YAML configuration/discovery parsing and resource sampling; threaded work uses `ThreadPoolExecutor`. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L9-L21] [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L223-L237] [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L8-L18] |
| Shared helpers | `LicenseError`, `write_mock_artifact`, `check_statics`, `check_simulation`, `FileSizeOptimizer`; real state acceptance and artificial artifact provenance are separate concerns. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L17-L21] [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L15-L18] |
| CLI consumers | Package entry point uses the runner and reporter; the outer command also imports these public exports. [src/digitalmodel/solvers/orcaflex/universal/__main__.py:L26-L26] [src/digitalmodel/solvers/orcaflex/universal_cli.py:L21-L25] |
| Solver version | UNVERIFIED: no qualified target version; issue pending (5). |

Every source occurrence of `OrcaFlex` (case-insensitive, including identifiers and comments) or `OrcFxAPI` is indexed below; reading these dependencies involved text inspection only.

| Python source | Occurrence lines | Evidence |
| --- | --- | --- |
| __init__.py | 2, 5, 9, 11, 16, 18, 22, 29 | [src/digitalmodel/solvers/orcaflex/universal/__init__.py:L1-L36] |
| __main__.py | 3, 5, 9, 12, 15, 26, 58, 59, 77, 80, 83, 86, 96, 151, 171 | [src/digitalmodel/solvers/orcaflex/universal/__main__.py:L1-L220] |
| batch_processor.py | 16, 18, 19, 45, 176, 284, 285, 287 | [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L1-L397] |
| model_discovery.py | 5, 19, 29, 30, 61, 191, 197, 200, 203, 228, 229, 236, 237, 247, 251, 252 | [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L1-L359] |
| path_resolver.py | 225 | [src/digitalmodel/solvers/orcaflex/universal/path_resolver.py:L1-L312] |
| status_reporter.py | 96, 103, 222, 360, 365 | [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L1-L374] |
| universal_runner.py | 2, 5, 18, 19, 21, 31, 35, 36, 39, 42, 76, 78, 80, 96, 100, 135, 136, 138, 144, 145, 153, 154, 156, 157, 162, 164, 244, 411, 412, 418, 420, 487 | [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L1-L513] |

### Map revision and counted families

Map reference: `codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; map data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. All map citations in this section resolve at that reference; all source citations resolve at the code pin. The inventory uses stable family-relative row ordinals and exact map locators so that identifying paths in other packages are not reproduced. [docs/registry/implementation-map.yaml:L1-L4]

| Rank | Counted family | In-package rows | Full family rows | Package inventory |
| --- | --- | --- | --- | --- |
| 1 | `external-solver-batch-run-stacks` | 2 | 137 | `BatchProcessor._default_processor`, `UniversalOrcaFlexRunner._process_single_model`. [docs/registry/implementation-map.yaml:L24504-L24506] |
| 2 | `orcaflex-text-model-writers` | 1 | 440 | `UniversalOrcaFlexRunner._process_single_model`. [docs/registry/implementation-map.yaml:L4933-L4935] |

The following source definitions account for the three in-package map rows.

| Family | Definition | Evidence |
| --- | --- | --- |
| `external-solver-batch-run-stacks` | `BatchProcessor._default_processor` | [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L258-L314] |
| `external-solver-batch-run-stacks` | `UniversalOrcaFlexRunner._process_single_model` | [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L371-L460] |
| `orcaflex-text-model-writers` | `UniversalOrcaFlexRunner._process_single_model` | [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L371-L460] |

### Exact signal search and definition classification

For each counted family, apply each `search_signals[].pattern` exactly as stored using case-sensitive Python `re.search`, to every package `.py` line at the code pin. Omit the vocabulary signal when its pattern matches `solvers/orcaflex/universal` case-insensitively. Assign hits to the smallest enclosing function or class, or one module-level definition. No regex normalization or additional case folding is used.

| Family | Retained signal definition | Omitted signal | Hit classification |
| --- | --- | --- | --- |
| `external-solver-batch-run-stacks` | Algorithm and constants/keys, exactly as stored. [docs/registry/implementation-map.yaml:L24507-L24509] | vocabulary: path match | 5 enclosing definitions; see exhaustive table below. |
| `orcaflex-text-model-writers` | Algorithm and constants/keys, exactly as stored. [docs/registry/implementation-map.yaml:L4936-L4938] | vocabulary: path match | 21 enclosing definitions; see exhaustive table below. |

| Family | Definition / hit lines | Classification and source evidence |
| --- | --- | --- |
| `external-solver-batch-run-stacks` | `batch_processor.py:<module>`; 13 | Helper, declaration or documentation signal; delegates to the listed launch definitions or handles paths/discovery. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L1-L397] |
| `external-solver-batch-run-stacks` | `batch_processor.py:BatchProcessor`; 29 | Helper, declaration or documentation signal; delegates to the listed launch definitions or handles paths/discovery. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L24-L397] |
| `external-solver-batch-run-stacks` | `batch_processor.py:_process_parallel`; 203 | Helper: submits the provided processor, without another solver-launch algorithm. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L193-L229] |
| `external-solver-batch-run-stacks` | `batch_processor.py:_default_processor`; 298 | Listed solver launcher. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L258-L314] |
| `external-solver-batch-run-stacks` | `universal_runner.py:_process_single_model`; 437, 445 | Listed solver launcher. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L371-L460] |
| `orcaflex-text-model-writers` | `__init__.py:<module>`; 19 | Helper, declaration or documentation signal; delegates to the listed launch definitions or handles paths/discovery. [src/digitalmodel/solvers/orcaflex/universal/__init__.py:L1-L36] |
| `orcaflex-text-model-writers` | `__main__.py:<module>`; 12, 16 | Helper, declaration or documentation signal; delegates to the listed launch definitions or handles paths/discovery. [src/digitalmodel/solvers/orcaflex/universal/__main__.py:L1-L220] |
| `orcaflex-text-model-writers` | `__main__.py:main`; 65, 77, 80, 96, 141 | Helper, declaration or documentation signal; delegates to the listed launch definitions or handles paths/discovery. [src/digitalmodel/solvers/orcaflex/universal/__main__.py:L55-L216] |
| `orcaflex-text-model-writers` | `batch_processor.py:_calculate_optimal_workers`; 154 | Helper, declaration or documentation signal; delegates to the listed launch definitions or handles paths/discovery. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L138-L191] |
| `orcaflex-text-model-writers` | `batch_processor.py:_default_processor`; 290 | Helper, declaration or documentation signal; delegates to the listed launch definitions or handles paths/discovery. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L258-L314] |
| `orcaflex-text-model-writers` | `model_discovery.py:ModelDiscovery`; 22, 30 | Helper, declaration or documentation signal; delegates to the listed launch definitions or handles paths/discovery. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L17-L359] |
| `orcaflex-text-model-writers` | `model_discovery.py:find_models`; 49 | Helper, declaration or documentation signal; delegates to the listed launch definitions or handles paths/discovery. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L47-L124] |
| `orcaflex-text-model-writers` | `model_discovery.py:_is_regex_pattern`; 129 | Helper, declaration or documentation signal; delegates to the listed launch definitions or handles paths/discovery. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L126-L143] |
| `orcaflex-text-model-writers` | `model_discovery.py:validate_model`; 209 | Helper: model recognition, not model-result generation. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L189-L219] |
| `orcaflex-text-model-writers` | `model_discovery.py:_validate_yaml_model`; 225, 230, 231, 232 | Helper: model recognition, not model-result generation. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L221-L242] |
| `orcaflex-text-model-writers` | `model_discovery.py:_validate_dat_model`; 248 | Helper: model recognition, not model-result generation. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L244-L258] |
| `orcaflex-text-model-writers` | `model_discovery.py:find_related_models`; 283, 285 | Helper, declaration or documentation signal; delegates to the listed launch definitions or handles paths/discovery. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L265-L295] |
| `orcaflex-text-model-writers` | `path_resolver.py:resolve_model_path`; 169, 170, 171, 172 | Helper, declaration or documentation signal; delegates to the listed launch definitions or handles paths/discovery. [src/digitalmodel/solvers/orcaflex/universal/path_resolver.py:L164-L209] |
| `orcaflex-text-model-writers` | `status_reporter.py:update_terminal_title`; 112 | Helper: terminal or JSON serialization, not a solver/model algorithm. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L84-L116] |
| `orcaflex-text-model-writers` | `status_reporter.py:display_progress`; 173 | Helper: terminal or JSON serialization, not a solver/model algorithm. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L118-L174] |
| `orcaflex-text-model-writers` | `status_reporter.py:display_summary`; 219 | Helper: terminal or JSON serialization, not a solver/model algorithm. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L211-L277] |
| `orcaflex-text-model-writers` | `status_reporter.py:save_report`; 325, 326 | Helper: terminal or JSON serialization, not a solver/model algorithm. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L313-L328] |
| `orcaflex-text-model-writers` | `status_reporter.py:set_final_status`; 371 | Helper: terminal or JSON serialization, not a solver/model algorithm. [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L356-L374] |
| `orcaflex-text-model-writers` | `universal_runner.py:run`; 218, 235, 238, 241, 294 | Helper, declaration or documentation signal; delegates to the listed launch definitions or handles paths/discovery. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L202-L334] |
| `orcaflex-text-model-writers` | `universal_runner.py:_load_config`; 343 | Helper: configuration deserialization. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L336-L347] |
| `orcaflex-text-model-writers` | `universal_runner.py:_process_single_model`; 427 | Listed map row; model access/solver launch, not an independent text-model generator. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L371-L460] |

A8 source-search result: 0 further second implementations across both counted families; both retained solver-launch definitions are inventoried above. The writer-family map row is retained as recorded, with classification follow-up rather than being silently removed. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L371-L460] [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L193-L229] [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L258-L314]

Repository-wide comparison search includes tracked Python under `scripts/`, `src/`, `docs/` and `tests/`; it is a discovery inventory, not proof of equivalence. Representative alternate launch implementations are the neighboring model runner and batch script below; these are outside this pack's package. [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L67-L115] [scripts/benchmark_model_library.py:L163-L194]

Map follow-up, issue pending (6): review the text-writer classification of `_process_single_model`; discovery heuristics, path resolution, worker sizing and status serialization have no separate in-package map families and therefore do not add counted families in this revision. [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L371-L460] [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L17-L27] [src/digitalmodel/solvers/orcaflex/universal/path_resolver.py:L17-L26] [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L20-L30]

### Complete map family inventory

Each row below identifies one map implementation by family-relative ordinal and its full YAML-row locator at the map reference, including helpers/test rows as recorded; package membership is stated separately above. This preserves the inventory without copying identifying names from unrelated packages.

| Counted family | Row | Implementation locator at map reference |
| --- | --- | --- |
| `external-solver-batch-run-stacks` | 1 | [docs/registry/implementation-map.yaml:L24511-L24520] |
| `external-solver-batch-run-stacks` | 2 | [docs/registry/implementation-map.yaml:L24521-L24530] |
| `external-solver-batch-run-stacks` | 3 | [docs/registry/implementation-map.yaml:L24531-L24540] |
| `external-solver-batch-run-stacks` | 4 | [docs/registry/implementation-map.yaml:L24541-L24550] |
| `external-solver-batch-run-stacks` | 5 | [docs/registry/implementation-map.yaml:L24551-L24560] |
| `external-solver-batch-run-stacks` | 6 | [docs/registry/implementation-map.yaml:L24561-L24570] |
| `external-solver-batch-run-stacks` | 7 | [docs/registry/implementation-map.yaml:L24571-L24580] |
| `external-solver-batch-run-stacks` | 8 | [docs/registry/implementation-map.yaml:L24581-L24590] |
| `external-solver-batch-run-stacks` | 9 | [docs/registry/implementation-map.yaml:L24591-L24600] |
| `external-solver-batch-run-stacks` | 10 | [docs/registry/implementation-map.yaml:L24601-L24610] |
| `external-solver-batch-run-stacks` | 11 | [docs/registry/implementation-map.yaml:L24611-L24620] |
| `external-solver-batch-run-stacks` | 12 | [docs/registry/implementation-map.yaml:L24621-L24630] |
| `external-solver-batch-run-stacks` | 13 | [docs/registry/implementation-map.yaml:L24631-L24640] |
| `external-solver-batch-run-stacks` | 14 | [docs/registry/implementation-map.yaml:L24641-L24650] |
| `external-solver-batch-run-stacks` | 15 | [docs/registry/implementation-map.yaml:L24651-L24660] |
| `external-solver-batch-run-stacks` | 16 | [docs/registry/implementation-map.yaml:L24661-L24670] |
| `external-solver-batch-run-stacks` | 17 | [docs/registry/implementation-map.yaml:L24671-L24680] |
| `external-solver-batch-run-stacks` | 18 | [docs/registry/implementation-map.yaml:L24681-L24690] |
| `external-solver-batch-run-stacks` | 19 | [docs/registry/implementation-map.yaml:L24691-L24700] |
| `external-solver-batch-run-stacks` | 20 | [docs/registry/implementation-map.yaml:L24701-L24712] |
| `external-solver-batch-run-stacks` | 21 | [docs/registry/implementation-map.yaml:L24713-L24722] |
| `external-solver-batch-run-stacks` | 22 | [docs/registry/implementation-map.yaml:L24723-L24732] |
| `external-solver-batch-run-stacks` | 23 | [docs/registry/implementation-map.yaml:L24733-L24742] |
| `external-solver-batch-run-stacks` | 24 | [docs/registry/implementation-map.yaml:L24743-L24751] |
| `external-solver-batch-run-stacks` | 25 | [docs/registry/implementation-map.yaml:L24752-L24760] |
| `external-solver-batch-run-stacks` | 26 | [docs/registry/implementation-map.yaml:L24761-L24769] |
| `external-solver-batch-run-stacks` | 27 | [docs/registry/implementation-map.yaml:L24770-L24778] |
| `external-solver-batch-run-stacks` | 28 | [docs/registry/implementation-map.yaml:L24779-L24787] |
| `external-solver-batch-run-stacks` | 29 | [docs/registry/implementation-map.yaml:L24788-L24796] |
| `external-solver-batch-run-stacks` | 30 | [docs/registry/implementation-map.yaml:L24797-L24805] |
| `external-solver-batch-run-stacks` | 31 | [docs/registry/implementation-map.yaml:L24806-L24814] |
| `external-solver-batch-run-stacks` | 32 | [docs/registry/implementation-map.yaml:L24815-L24823] |
| `external-solver-batch-run-stacks` | 33 | [docs/registry/implementation-map.yaml:L24824-L24832] |
| `external-solver-batch-run-stacks` | 34 | [docs/registry/implementation-map.yaml:L24833-L24841] |
| `external-solver-batch-run-stacks` | 35 | [docs/registry/implementation-map.yaml:L24842-L24850] |
| `external-solver-batch-run-stacks` | 36 | [docs/registry/implementation-map.yaml:L24851-L24859] |
| `external-solver-batch-run-stacks` | 37 | [docs/registry/implementation-map.yaml:L24860-L24868] |
| `external-solver-batch-run-stacks` | 38 | [docs/registry/implementation-map.yaml:L24869-L24877] |
| `external-solver-batch-run-stacks` | 39 | [docs/registry/implementation-map.yaml:L24878-L24886] |
| `external-solver-batch-run-stacks` | 40 | [docs/registry/implementation-map.yaml:L24887-L24895] |
| `external-solver-batch-run-stacks` | 41 | [docs/registry/implementation-map.yaml:L24896-L24904] |
| `external-solver-batch-run-stacks` | 42 | [docs/registry/implementation-map.yaml:L24905-L24913] |
| `external-solver-batch-run-stacks` | 43 | [docs/registry/implementation-map.yaml:L24914-L24922] |
| `external-solver-batch-run-stacks` | 44 | [docs/registry/implementation-map.yaml:L24923-L24932] |
| `external-solver-batch-run-stacks` | 45 | [docs/registry/implementation-map.yaml:L24933-L24942] |
| `external-solver-batch-run-stacks` | 46 | [docs/registry/implementation-map.yaml:L24943-L24952] |
| `external-solver-batch-run-stacks` | 47 | [docs/registry/implementation-map.yaml:L24953-L24962] |
| `external-solver-batch-run-stacks` | 48 | [docs/registry/implementation-map.yaml:L24963-L24974] |
| `external-solver-batch-run-stacks` | 49 | [docs/registry/implementation-map.yaml:L24975-L24984] |
| `external-solver-batch-run-stacks` | 50 | [docs/registry/implementation-map.yaml:L24985-L24997] |
| `external-solver-batch-run-stacks` | 51 | [docs/registry/implementation-map.yaml:L24998-L25007] |
| `external-solver-batch-run-stacks` | 52 | [docs/registry/implementation-map.yaml:L25008-L25017] |
| `external-solver-batch-run-stacks` | 53 | [docs/registry/implementation-map.yaml:L25018-L25027] |
| `external-solver-batch-run-stacks` | 54 | [docs/registry/implementation-map.yaml:L25028-L25037] |
| `external-solver-batch-run-stacks` | 55 | [docs/registry/implementation-map.yaml:L25038-L25047] |
| `external-solver-batch-run-stacks` | 56 | [docs/registry/implementation-map.yaml:L25048-L25057] |
| `external-solver-batch-run-stacks` | 57 | [docs/registry/implementation-map.yaml:L25058-L25067] |
| `external-solver-batch-run-stacks` | 58 | [docs/registry/implementation-map.yaml:L25068-L25079] |
| `external-solver-batch-run-stacks` | 59 | [docs/registry/implementation-map.yaml:L25080-L25089] |
| `external-solver-batch-run-stacks` | 60 | [docs/registry/implementation-map.yaml:L25090-L25099] |
| `external-solver-batch-run-stacks` | 61 | [docs/registry/implementation-map.yaml:L25100-L25109] |
| `external-solver-batch-run-stacks` | 62 | [docs/registry/implementation-map.yaml:L25110-L25119] |
| `external-solver-batch-run-stacks` | 63 | [docs/registry/implementation-map.yaml:L25120-L25129] |
| `external-solver-batch-run-stacks` | 64 | [docs/registry/implementation-map.yaml:L25130-L25139] |
| `external-solver-batch-run-stacks` | 65 | [docs/registry/implementation-map.yaml:L25140-L25149] |
| `external-solver-batch-run-stacks` | 66 | [docs/registry/implementation-map.yaml:L25150-L25159] |
| `external-solver-batch-run-stacks` | 67 | [docs/registry/implementation-map.yaml:L25160-L25169] |
| `external-solver-batch-run-stacks` | 68 | [docs/registry/implementation-map.yaml:L25170-L25179] |
| `external-solver-batch-run-stacks` | 69 | [docs/registry/implementation-map.yaml:L25180-L25189] |
| `external-solver-batch-run-stacks` | 70 | [docs/registry/implementation-map.yaml:L25190-L25199] |
| `external-solver-batch-run-stacks` | 71 | [docs/registry/implementation-map.yaml:L25200-L25209] |
| `external-solver-batch-run-stacks` | 72 | [docs/registry/implementation-map.yaml:L25210-L25219] |
| `external-solver-batch-run-stacks` | 73 | [docs/registry/implementation-map.yaml:L25220-L25229] |
| `external-solver-batch-run-stacks` | 74 | [docs/registry/implementation-map.yaml:L25230-L25239] |
| `external-solver-batch-run-stacks` | 75 | [docs/registry/implementation-map.yaml:L25240-L25249] |
| `external-solver-batch-run-stacks` | 76 | [docs/registry/implementation-map.yaml:L25250-L25259] |
| `external-solver-batch-run-stacks` | 77 | [docs/registry/implementation-map.yaml:L25260-L25269] |
| `external-solver-batch-run-stacks` | 78 | [docs/registry/implementation-map.yaml:L25270-L25279] |
| `external-solver-batch-run-stacks` | 79 | [docs/registry/implementation-map.yaml:L25280-L25289] |
| `external-solver-batch-run-stacks` | 80 | [docs/registry/implementation-map.yaml:L25290-L25299] |
| `external-solver-batch-run-stacks` | 81 | [docs/registry/implementation-map.yaml:L25300-L25309] |
| `external-solver-batch-run-stacks` | 82 | [docs/registry/implementation-map.yaml:L25310-L25319] |
| `external-solver-batch-run-stacks` | 83 | [docs/registry/implementation-map.yaml:L25320-L25329] |
| `external-solver-batch-run-stacks` | 84 | [docs/registry/implementation-map.yaml:L25330-L25339] |
| `external-solver-batch-run-stacks` | 85 | [docs/registry/implementation-map.yaml:L25340-L25349] |
| `external-solver-batch-run-stacks` | 86 | [docs/registry/implementation-map.yaml:L25350-L25359] |
| `external-solver-batch-run-stacks` | 87 | [docs/registry/implementation-map.yaml:L25360-L25369] |
| `external-solver-batch-run-stacks` | 88 | [docs/registry/implementation-map.yaml:L25370-L25379] |
| `external-solver-batch-run-stacks` | 89 | [docs/registry/implementation-map.yaml:L25380-L25389] |
| `external-solver-batch-run-stacks` | 90 | [docs/registry/implementation-map.yaml:L25390-L25399] |
| `external-solver-batch-run-stacks` | 91 | [docs/registry/implementation-map.yaml:L25400-L25409] |
| `external-solver-batch-run-stacks` | 92 | [docs/registry/implementation-map.yaml:L25410-L25419] |
| `external-solver-batch-run-stacks` | 93 | [docs/registry/implementation-map.yaml:L25420-L25429] |
| `external-solver-batch-run-stacks` | 94 | [docs/registry/implementation-map.yaml:L25430-L25439] |
| `external-solver-batch-run-stacks` | 95 | [docs/registry/implementation-map.yaml:L25440-L25455] |
| `external-solver-batch-run-stacks` | 96 | [docs/registry/implementation-map.yaml:L25456-L25465] |
| `external-solver-batch-run-stacks` | 97 | [docs/registry/implementation-map.yaml:L25466-L25475] |
| `external-solver-batch-run-stacks` | 98 | [docs/registry/implementation-map.yaml:L25476-L25485] |
| `external-solver-batch-run-stacks` | 99 | [docs/registry/implementation-map.yaml:L25486-L25495] |
| `external-solver-batch-run-stacks` | 100 | [docs/registry/implementation-map.yaml:L25496-L25505] |
| `external-solver-batch-run-stacks` | 101 | [docs/registry/implementation-map.yaml:L25506-L25518] |
| `external-solver-batch-run-stacks` | 102 | [docs/registry/implementation-map.yaml:L25519-L25528] |
| `external-solver-batch-run-stacks` | 103 | [docs/registry/implementation-map.yaml:L25529-L25538] |
| `external-solver-batch-run-stacks` | 104 | [docs/registry/implementation-map.yaml:L25539-L25548] |
| `external-solver-batch-run-stacks` | 105 | [docs/registry/implementation-map.yaml:L25549-L25558] |
| `external-solver-batch-run-stacks` | 106 | [docs/registry/implementation-map.yaml:L25559-L25568] |
| `external-solver-batch-run-stacks` | 107 | [docs/registry/implementation-map.yaml:L25569-L25578] |
| `external-solver-batch-run-stacks` | 108 | [docs/registry/implementation-map.yaml:L25579-L25588] |
| `external-solver-batch-run-stacks` | 109 | [docs/registry/implementation-map.yaml:L25589-L25598] |
| `external-solver-batch-run-stacks` | 110 | [docs/registry/implementation-map.yaml:L25599-L25608] |
| `external-solver-batch-run-stacks` | 111 | [docs/registry/implementation-map.yaml:L25609-L25618] |
| `external-solver-batch-run-stacks` | 112 | [docs/registry/implementation-map.yaml:L25619-L25628] |
| `external-solver-batch-run-stacks` | 113 | [docs/registry/implementation-map.yaml:L25629-L25638] |
| `external-solver-batch-run-stacks` | 114 | [docs/registry/implementation-map.yaml:L25639-L25648] |
| `external-solver-batch-run-stacks` | 115 | [docs/registry/implementation-map.yaml:L25649-L25658] |
| `external-solver-batch-run-stacks` | 116 | [docs/registry/implementation-map.yaml:L25659-L25668] |
| `external-solver-batch-run-stacks` | 117 | [docs/registry/implementation-map.yaml:L25669-L25678] |
| `external-solver-batch-run-stacks` | 118 | [docs/registry/implementation-map.yaml:L25679-L25688] |
| `external-solver-batch-run-stacks` | 119 | [docs/registry/implementation-map.yaml:L25689-L25698] |
| `external-solver-batch-run-stacks` | 120 | [docs/registry/implementation-map.yaml:L25699-L25708] |
| `external-solver-batch-run-stacks` | 121 | [docs/registry/implementation-map.yaml:L25709-L25718] |
| `external-solver-batch-run-stacks` | 122 | [docs/registry/implementation-map.yaml:L25719-L25728] |
| `external-solver-batch-run-stacks` | 123 | [docs/registry/implementation-map.yaml:L25729-L25738] |
| `external-solver-batch-run-stacks` | 124 | [docs/registry/implementation-map.yaml:L25739-L25748] |
| `external-solver-batch-run-stacks` | 125 | [docs/registry/implementation-map.yaml:L25749-L25758] |
| `external-solver-batch-run-stacks` | 126 | [docs/registry/implementation-map.yaml:L25759-L25770] |
| `external-solver-batch-run-stacks` | 127 | [docs/registry/implementation-map.yaml:L25771-L25780] |
| `external-solver-batch-run-stacks` | 128 | [docs/registry/implementation-map.yaml:L25781-L25790] |
| `external-solver-batch-run-stacks` | 129 | [docs/registry/implementation-map.yaml:L25791-L25800] |
| `external-solver-batch-run-stacks` | 130 | [docs/registry/implementation-map.yaml:L25801-L25810] |
| `external-solver-batch-run-stacks` | 131 | [docs/registry/implementation-map.yaml:L25811-L25820] |
| `external-solver-batch-run-stacks` | 132 | [docs/registry/implementation-map.yaml:L25821-L25830] |
| `external-solver-batch-run-stacks` | 133 | [docs/registry/implementation-map.yaml:L25831-L25840] |
| `external-solver-batch-run-stacks` | 134 | [docs/registry/implementation-map.yaml:L25841-L25850] |
| `external-solver-batch-run-stacks` | 135 | [docs/registry/implementation-map.yaml:L25851-L25860] |
| `external-solver-batch-run-stacks` | 136 | [docs/registry/implementation-map.yaml:L25861-L25870] |
| `external-solver-batch-run-stacks` | 137 | [docs/registry/implementation-map.yaml:L25871-L25880] |
| `orcaflex-text-model-writers` | 1 | [docs/registry/implementation-map.yaml:L4940-L4949] |
| `orcaflex-text-model-writers` | 2 | [docs/registry/implementation-map.yaml:L4950-L4958] |
| `orcaflex-text-model-writers` | 3 | [docs/registry/implementation-map.yaml:L4959-L4968] |
| `orcaflex-text-model-writers` | 4 | [docs/registry/implementation-map.yaml:L4969-L4978] |
| `orcaflex-text-model-writers` | 5 | [docs/registry/implementation-map.yaml:L4979-L4988] |
| `orcaflex-text-model-writers` | 6 | [docs/registry/implementation-map.yaml:L4989-L4998] |
| `orcaflex-text-model-writers` | 7 | [docs/registry/implementation-map.yaml:L4999-L5008] |
| `orcaflex-text-model-writers` | 8 | [docs/registry/implementation-map.yaml:L5009-L5020] |
| `orcaflex-text-model-writers` | 9 | [docs/registry/implementation-map.yaml:L5021-L5030] |
| `orcaflex-text-model-writers` | 10 | [docs/registry/implementation-map.yaml:L5031-L5040] |
| `orcaflex-text-model-writers` | 11 | [docs/registry/implementation-map.yaml:L5041-L5050] |
| `orcaflex-text-model-writers` | 12 | [docs/registry/implementation-map.yaml:L5051-L5060] |
| `orcaflex-text-model-writers` | 13 | [docs/registry/implementation-map.yaml:L5061-L5070] |
| `orcaflex-text-model-writers` | 14 | [docs/registry/implementation-map.yaml:L5071-L5080] |
| `orcaflex-text-model-writers` | 15 | [docs/registry/implementation-map.yaml:L5081-L5090] |
| `orcaflex-text-model-writers` | 16 | [docs/registry/implementation-map.yaml:L5091-L5100] |
| `orcaflex-text-model-writers` | 17 | [docs/registry/implementation-map.yaml:L5101-L5110] |
| `orcaflex-text-model-writers` | 18 | [docs/registry/implementation-map.yaml:L5111-L5120] |
| `orcaflex-text-model-writers` | 19 | [docs/registry/implementation-map.yaml:L5121-L5132] |
| `orcaflex-text-model-writers` | 20 | [docs/registry/implementation-map.yaml:L5133-L5142] |
| `orcaflex-text-model-writers` | 21 | [docs/registry/implementation-map.yaml:L5143-L5151] |
| `orcaflex-text-model-writers` | 22 | [docs/registry/implementation-map.yaml:L5152-L5161] |
| `orcaflex-text-model-writers` | 23 | [docs/registry/implementation-map.yaml:L5162-L5171] |
| `orcaflex-text-model-writers` | 24 | [docs/registry/implementation-map.yaml:L5172-L5181] |
| `orcaflex-text-model-writers` | 25 | [docs/registry/implementation-map.yaml:L5182-L5191] |
| `orcaflex-text-model-writers` | 26 | [docs/registry/implementation-map.yaml:L5192-L5201] |
| `orcaflex-text-model-writers` | 27 | [docs/registry/implementation-map.yaml:L5202-L5211] |
| `orcaflex-text-model-writers` | 28 | [docs/registry/implementation-map.yaml:L5212-L5223] |
| `orcaflex-text-model-writers` | 29 | [docs/registry/implementation-map.yaml:L5224-L5233] |
| `orcaflex-text-model-writers` | 30 | [docs/registry/implementation-map.yaml:L5234-L5243] |
| `orcaflex-text-model-writers` | 31 | [docs/registry/implementation-map.yaml:L5244-L5253] |
| `orcaflex-text-model-writers` | 32 | [docs/registry/implementation-map.yaml:L5254-L5263] |
| `orcaflex-text-model-writers` | 33 | [docs/registry/implementation-map.yaml:L5264-L5273] |
| `orcaflex-text-model-writers` | 34 | [docs/registry/implementation-map.yaml:L5274-L5283] |
| `orcaflex-text-model-writers` | 35 | [docs/registry/implementation-map.yaml:L5284-L5293] |
| `orcaflex-text-model-writers` | 36 | [docs/registry/implementation-map.yaml:L5294-L5303] |
| `orcaflex-text-model-writers` | 37 | [docs/registry/implementation-map.yaml:L5304-L5313] |
| `orcaflex-text-model-writers` | 38 | [docs/registry/implementation-map.yaml:L5314-L5323] |
| `orcaflex-text-model-writers` | 39 | [docs/registry/implementation-map.yaml:L5324-L5333] |
| `orcaflex-text-model-writers` | 40 | [docs/registry/implementation-map.yaml:L5334-L5343] |
| `orcaflex-text-model-writers` | 41 | [docs/registry/implementation-map.yaml:L5344-L5353] |
| `orcaflex-text-model-writers` | 42 | [docs/registry/implementation-map.yaml:L5354-L5363] |
| `orcaflex-text-model-writers` | 43 | [docs/registry/implementation-map.yaml:L5364-L5372] |
| `orcaflex-text-model-writers` | 44 | [docs/registry/implementation-map.yaml:L5373-L5382] |
| `orcaflex-text-model-writers` | 45 | [docs/registry/implementation-map.yaml:L5383-L5392] |
| `orcaflex-text-model-writers` | 46 | [docs/registry/implementation-map.yaml:L5393-L5402] |
| `orcaflex-text-model-writers` | 47 | [docs/registry/implementation-map.yaml:L5403-L5415] |
| `orcaflex-text-model-writers` | 48 | [docs/registry/implementation-map.yaml:L5416-L5425] |
| `orcaflex-text-model-writers` | 49 | [docs/registry/implementation-map.yaml:L5426-L5437] |
| `orcaflex-text-model-writers` | 50 | [docs/registry/implementation-map.yaml:L5438-L5447] |
| `orcaflex-text-model-writers` | 51 | [docs/registry/implementation-map.yaml:L5448-L5457] |
| `orcaflex-text-model-writers` | 52 | [docs/registry/implementation-map.yaml:L5458-L5467] |
| `orcaflex-text-model-writers` | 53 | [docs/registry/implementation-map.yaml:L5468-L5477] |
| `orcaflex-text-model-writers` | 54 | [docs/registry/implementation-map.yaml:L5478-L5487] |
| `orcaflex-text-model-writers` | 55 | [docs/registry/implementation-map.yaml:L5488-L5497] |
| `orcaflex-text-model-writers` | 56 | [docs/registry/implementation-map.yaml:L5498-L5507] |
| `orcaflex-text-model-writers` | 57 | [docs/registry/implementation-map.yaml:L5508-L5519] |
| `orcaflex-text-model-writers` | 58 | [docs/registry/implementation-map.yaml:L5520-L5531] |
| `orcaflex-text-model-writers` | 59 | [docs/registry/implementation-map.yaml:L5532-L5540] |
| `orcaflex-text-model-writers` | 60 | [docs/registry/implementation-map.yaml:L5541-L5550] |
| `orcaflex-text-model-writers` | 61 | [docs/registry/implementation-map.yaml:L5551-L5560] |
| `orcaflex-text-model-writers` | 62 | [docs/registry/implementation-map.yaml:L5561-L5570] |
| `orcaflex-text-model-writers` | 63 | [docs/registry/implementation-map.yaml:L5571-L5580] |
| `orcaflex-text-model-writers` | 64 | [docs/registry/implementation-map.yaml:L5581-L5593] |
| `orcaflex-text-model-writers` | 65 | [docs/registry/implementation-map.yaml:L5594-L5603] |
| `orcaflex-text-model-writers` | 66 | [docs/registry/implementation-map.yaml:L5604-L5615] |
| `orcaflex-text-model-writers` | 67 | [docs/registry/implementation-map.yaml:L5616-L5628] |
| `orcaflex-text-model-writers` | 68 | [docs/registry/implementation-map.yaml:L5629-L5638] |
| `orcaflex-text-model-writers` | 69 | [docs/registry/implementation-map.yaml:L5639-L5648] |
| `orcaflex-text-model-writers` | 70 | [docs/registry/implementation-map.yaml:L5649-L5658] |
| `orcaflex-text-model-writers` | 71 | [docs/registry/implementation-map.yaml:L5659-L5667] |
| `orcaflex-text-model-writers` | 72 | [docs/registry/implementation-map.yaml:L5668-L5677] |
| `orcaflex-text-model-writers` | 73 | [docs/registry/implementation-map.yaml:L5678-L5687] |
| `orcaflex-text-model-writers` | 74 | [docs/registry/implementation-map.yaml:L5688-L5697] |
| `orcaflex-text-model-writers` | 75 | [docs/registry/implementation-map.yaml:L5698-L5707] |
| `orcaflex-text-model-writers` | 76 | [docs/registry/implementation-map.yaml:L5708-L5717] |
| `orcaflex-text-model-writers` | 77 | [docs/registry/implementation-map.yaml:L5718-L5727] |
| `orcaflex-text-model-writers` | 78 | [docs/registry/implementation-map.yaml:L5728-L5737] |
| `orcaflex-text-model-writers` | 79 | [docs/registry/implementation-map.yaml:L5738-L5749] |
| `orcaflex-text-model-writers` | 80 | [docs/registry/implementation-map.yaml:L5750-L5759] |
| `orcaflex-text-model-writers` | 81 | [docs/registry/implementation-map.yaml:L5760-L5769] |
| `orcaflex-text-model-writers` | 82 | [docs/registry/implementation-map.yaml:L5770-L5781] |
| `orcaflex-text-model-writers` | 83 | [docs/registry/implementation-map.yaml:L5782-L5791] |
| `orcaflex-text-model-writers` | 84 | [docs/registry/implementation-map.yaml:L5792-L5801] |
| `orcaflex-text-model-writers` | 85 | [docs/registry/implementation-map.yaml:L5802-L5811] |
| `orcaflex-text-model-writers` | 86 | [docs/registry/implementation-map.yaml:L5812-L5823] |
| `orcaflex-text-model-writers` | 87 | [docs/registry/implementation-map.yaml:L5824-L5833] |
| `orcaflex-text-model-writers` | 88 | [docs/registry/implementation-map.yaml:L5834-L5843] |
| `orcaflex-text-model-writers` | 89 | [docs/registry/implementation-map.yaml:L5844-L5853] |
| `orcaflex-text-model-writers` | 90 | [docs/registry/implementation-map.yaml:L5854-L5863] |
| `orcaflex-text-model-writers` | 91 | [docs/registry/implementation-map.yaml:L5864-L5872] |
| `orcaflex-text-model-writers` | 92 | [docs/registry/implementation-map.yaml:L5873-L5882] |
| `orcaflex-text-model-writers` | 93 | [docs/registry/implementation-map.yaml:L5883-L5891] |
| `orcaflex-text-model-writers` | 94 | [docs/registry/implementation-map.yaml:L5892-L5901] |
| `orcaflex-text-model-writers` | 95 | [docs/registry/implementation-map.yaml:L5902-L5911] |
| `orcaflex-text-model-writers` | 96 | [docs/registry/implementation-map.yaml:L5912-L5921] |
| `orcaflex-text-model-writers` | 97 | [docs/registry/implementation-map.yaml:L5922-L5933] |
| `orcaflex-text-model-writers` | 98 | [docs/registry/implementation-map.yaml:L5934-L5943] |
| `orcaflex-text-model-writers` | 99 | [docs/registry/implementation-map.yaml:L5944-L5953] |
| `orcaflex-text-model-writers` | 100 | [docs/registry/implementation-map.yaml:L5954-L5965] |
| `orcaflex-text-model-writers` | 101 | [docs/registry/implementation-map.yaml:L5966-L5975] |
| `orcaflex-text-model-writers` | 102 | [docs/registry/implementation-map.yaml:L5976-L5985] |
| `orcaflex-text-model-writers` | 103 | [docs/registry/implementation-map.yaml:L5986-L5994] |
| `orcaflex-text-model-writers` | 104 | [docs/registry/implementation-map.yaml:L5995-L6004] |
| `orcaflex-text-model-writers` | 105 | [docs/registry/implementation-map.yaml:L6005-L6014] |
| `orcaflex-text-model-writers` | 106 | [docs/registry/implementation-map.yaml:L6015-L6024] |
| `orcaflex-text-model-writers` | 107 | [docs/registry/implementation-map.yaml:L6025-L6034] |
| `orcaflex-text-model-writers` | 108 | [docs/registry/implementation-map.yaml:L6035-L6044] |
| `orcaflex-text-model-writers` | 109 | [docs/registry/implementation-map.yaml:L6045-L6054] |
| `orcaflex-text-model-writers` | 110 | [docs/registry/implementation-map.yaml:L6055-L6064] |
| `orcaflex-text-model-writers` | 111 | [docs/registry/implementation-map.yaml:L6065-L6074] |
| `orcaflex-text-model-writers` | 112 | [docs/registry/implementation-map.yaml:L6075-L6084] |
| `orcaflex-text-model-writers` | 113 | [docs/registry/implementation-map.yaml:L6085-L6096] |
| `orcaflex-text-model-writers` | 114 | [docs/registry/implementation-map.yaml:L6097-L6109] |
| `orcaflex-text-model-writers` | 115 | [docs/registry/implementation-map.yaml:L6110-L6119] |
| `orcaflex-text-model-writers` | 116 | [docs/registry/implementation-map.yaml:L6120-L6129] |
| `orcaflex-text-model-writers` | 117 | [docs/registry/implementation-map.yaml:L6130-L6141] |
| `orcaflex-text-model-writers` | 118 | [docs/registry/implementation-map.yaml:L6142-L6153] |
| `orcaflex-text-model-writers` | 119 | [docs/registry/implementation-map.yaml:L6154-L6163] |
| `orcaflex-text-model-writers` | 120 | [docs/registry/implementation-map.yaml:L6164-L6173] |
| `orcaflex-text-model-writers` | 121 | [docs/registry/implementation-map.yaml:L6174-L6183] |
| `orcaflex-text-model-writers` | 122 | [docs/registry/implementation-map.yaml:L6184-L6193] |
| `orcaflex-text-model-writers` | 123 | [docs/registry/implementation-map.yaml:L6194-L6203] |
| `orcaflex-text-model-writers` | 124 | [docs/registry/implementation-map.yaml:L6204-L6213] |
| `orcaflex-text-model-writers` | 125 | [docs/registry/implementation-map.yaml:L6214-L6223] |
| `orcaflex-text-model-writers` | 126 | [docs/registry/implementation-map.yaml:L6224-L6233] |
| `orcaflex-text-model-writers` | 127 | [docs/registry/implementation-map.yaml:L6234-L6243] |
| `orcaflex-text-model-writers` | 128 | [docs/registry/implementation-map.yaml:L6244-L6253] |
| `orcaflex-text-model-writers` | 129 | [docs/registry/implementation-map.yaml:L6254-L6263] |
| `orcaflex-text-model-writers` | 130 | [docs/registry/implementation-map.yaml:L6264-L6273] |
| `orcaflex-text-model-writers` | 131 | [docs/registry/implementation-map.yaml:L6274-L6283] |
| `orcaflex-text-model-writers` | 132 | [docs/registry/implementation-map.yaml:L6284-L6293] |
| `orcaflex-text-model-writers` | 133 | [docs/registry/implementation-map.yaml:L6294-L6303] |
| `orcaflex-text-model-writers` | 134 | [docs/registry/implementation-map.yaml:L6304-L6313] |
| `orcaflex-text-model-writers` | 135 | [docs/registry/implementation-map.yaml:L6314-L6323] |
| `orcaflex-text-model-writers` | 136 | [docs/registry/implementation-map.yaml:L6324-L6333] |
| `orcaflex-text-model-writers` | 137 | [docs/registry/implementation-map.yaml:L6334-L6343] |
| `orcaflex-text-model-writers` | 138 | [docs/registry/implementation-map.yaml:L6344-L6353] |
| `orcaflex-text-model-writers` | 139 | [docs/registry/implementation-map.yaml:L6354-L6363] |
| `orcaflex-text-model-writers` | 140 | [docs/registry/implementation-map.yaml:L6364-L6373] |
| `orcaflex-text-model-writers` | 141 | [docs/registry/implementation-map.yaml:L6374-L6383] |
| `orcaflex-text-model-writers` | 142 | [docs/registry/implementation-map.yaml:L6384-L6393] |
| `orcaflex-text-model-writers` | 143 | [docs/registry/implementation-map.yaml:L6394-L6403] |
| `orcaflex-text-model-writers` | 144 | [docs/registry/implementation-map.yaml:L6404-L6413] |
| `orcaflex-text-model-writers` | 145 | [docs/registry/implementation-map.yaml:L6414-L6423] |
| `orcaflex-text-model-writers` | 146 | [docs/registry/implementation-map.yaml:L6424-L6436] |
| `orcaflex-text-model-writers` | 147 | [docs/registry/implementation-map.yaml:L6437-L6448] |
| `orcaflex-text-model-writers` | 148 | [docs/registry/implementation-map.yaml:L6449-L6458] |
| `orcaflex-text-model-writers` | 149 | [docs/registry/implementation-map.yaml:L6459-L6468] |
| `orcaflex-text-model-writers` | 150 | [docs/registry/implementation-map.yaml:L6469-L6478] |
| `orcaflex-text-model-writers` | 151 | [docs/registry/implementation-map.yaml:L6479-L6490] |
| `orcaflex-text-model-writers` | 152 | [docs/registry/implementation-map.yaml:L6491-L6500] |
| `orcaflex-text-model-writers` | 153 | [docs/registry/implementation-map.yaml:L6501-L6510] |
| `orcaflex-text-model-writers` | 154 | [docs/registry/implementation-map.yaml:L6511-L6520] |
| `orcaflex-text-model-writers` | 155 | [docs/registry/implementation-map.yaml:L6521-L6530] |
| `orcaflex-text-model-writers` | 156 | [docs/registry/implementation-map.yaml:L6531-L6540] |
| `orcaflex-text-model-writers` | 157 | [docs/registry/implementation-map.yaml:L6541-L6550] |
| `orcaflex-text-model-writers` | 158 | [docs/registry/implementation-map.yaml:L6551-L6560] |
| `orcaflex-text-model-writers` | 159 | [docs/registry/implementation-map.yaml:L6561-L6570] |
| `orcaflex-text-model-writers` | 160 | [docs/registry/implementation-map.yaml:L6571-L6580] |
| `orcaflex-text-model-writers` | 161 | [docs/registry/implementation-map.yaml:L6581-L6590] |
| `orcaflex-text-model-writers` | 162 | [docs/registry/implementation-map.yaml:L6591-L6600] |
| `orcaflex-text-model-writers` | 163 | [docs/registry/implementation-map.yaml:L6601-L6610] |
| `orcaflex-text-model-writers` | 164 | [docs/registry/implementation-map.yaml:L6611-L6620] |
| `orcaflex-text-model-writers` | 165 | [docs/registry/implementation-map.yaml:L6621-L6630] |
| `orcaflex-text-model-writers` | 166 | [docs/registry/implementation-map.yaml:L6631-L6640] |
| `orcaflex-text-model-writers` | 167 | [docs/registry/implementation-map.yaml:L6641-L6650] |
| `orcaflex-text-model-writers` | 168 | [docs/registry/implementation-map.yaml:L6651-L6659] |
| `orcaflex-text-model-writers` | 169 | [docs/registry/implementation-map.yaml:L6660-L6672] |
| `orcaflex-text-model-writers` | 170 | [docs/registry/implementation-map.yaml:L6673-L6682] |
| `orcaflex-text-model-writers` | 171 | [docs/registry/implementation-map.yaml:L6683-L6692] |
| `orcaflex-text-model-writers` | 172 | [docs/registry/implementation-map.yaml:L6693-L6704] |
| `orcaflex-text-model-writers` | 173 | [docs/registry/implementation-map.yaml:L6705-L6714] |
| `orcaflex-text-model-writers` | 174 | [docs/registry/implementation-map.yaml:L6715-L6724] |
| `orcaflex-text-model-writers` | 175 | [docs/registry/implementation-map.yaml:L6725-L6734] |
| `orcaflex-text-model-writers` | 176 | [docs/registry/implementation-map.yaml:L6735-L6744] |
| `orcaflex-text-model-writers` | 177 | [docs/registry/implementation-map.yaml:L6745-L6754] |
| `orcaflex-text-model-writers` | 178 | [docs/registry/implementation-map.yaml:L6755-L6764] |
| `orcaflex-text-model-writers` | 179 | [docs/registry/implementation-map.yaml:L6765-L6774] |
| `orcaflex-text-model-writers` | 180 | [docs/registry/implementation-map.yaml:L6775-L6784] |
| `orcaflex-text-model-writers` | 181 | [docs/registry/implementation-map.yaml:L6785-L6794] |
| `orcaflex-text-model-writers` | 182 | [docs/registry/implementation-map.yaml:L6795-L6804] |
| `orcaflex-text-model-writers` | 183 | [docs/registry/implementation-map.yaml:L6805-L6814] |
| `orcaflex-text-model-writers` | 184 | [docs/registry/implementation-map.yaml:L6815-L6824] |
| `orcaflex-text-model-writers` | 185 | [docs/registry/implementation-map.yaml:L6825-L6834] |
| `orcaflex-text-model-writers` | 186 | [docs/registry/implementation-map.yaml:L6835-L6844] |
| `orcaflex-text-model-writers` | 187 | [docs/registry/implementation-map.yaml:L6845-L6854] |
| `orcaflex-text-model-writers` | 188 | [docs/registry/implementation-map.yaml:L6855-L6864] |
| `orcaflex-text-model-writers` | 189 | [docs/registry/implementation-map.yaml:L6865-L6874] |
| `orcaflex-text-model-writers` | 190 | [docs/registry/implementation-map.yaml:L6875-L6884] |
| `orcaflex-text-model-writers` | 191 | [docs/registry/implementation-map.yaml:L6885-L6894] |
| `orcaflex-text-model-writers` | 192 | [docs/registry/implementation-map.yaml:L6895-L6904] |
| `orcaflex-text-model-writers` | 193 | [docs/registry/implementation-map.yaml:L6905-L6914] |
| `orcaflex-text-model-writers` | 194 | [docs/registry/implementation-map.yaml:L6915-L6927] |
| `orcaflex-text-model-writers` | 195 | [docs/registry/implementation-map.yaml:L6928-L6937] |
| `orcaflex-text-model-writers` | 196 | [docs/registry/implementation-map.yaml:L6938-L6947] |
| `orcaflex-text-model-writers` | 197 | [docs/registry/implementation-map.yaml:L6948-L6957] |
| `orcaflex-text-model-writers` | 198 | [docs/registry/implementation-map.yaml:L6958-L6967] |
| `orcaflex-text-model-writers` | 199 | [docs/registry/implementation-map.yaml:L6968-L6977] |
| `orcaflex-text-model-writers` | 200 | [docs/registry/implementation-map.yaml:L6978-L6987] |
| `orcaflex-text-model-writers` | 201 | [docs/registry/implementation-map.yaml:L6988-L6997] |
| `orcaflex-text-model-writers` | 202 | [docs/registry/implementation-map.yaml:L6998-L7007] |
| `orcaflex-text-model-writers` | 203 | [docs/registry/implementation-map.yaml:L7008-L7017] |
| `orcaflex-text-model-writers` | 204 | [docs/registry/implementation-map.yaml:L7018-L7031] |
| `orcaflex-text-model-writers` | 205 | [docs/registry/implementation-map.yaml:L7032-L7041] |
| `orcaflex-text-model-writers` | 206 | [docs/registry/implementation-map.yaml:L7042-L7051] |
| `orcaflex-text-model-writers` | 207 | [docs/registry/implementation-map.yaml:L7052-L7061] |
| `orcaflex-text-model-writers` | 208 | [docs/registry/implementation-map.yaml:L7062-L7071] |
| `orcaflex-text-model-writers` | 209 | [docs/registry/implementation-map.yaml:L7072-L7081] |
| `orcaflex-text-model-writers` | 210 | [docs/registry/implementation-map.yaml:L7082-L7091] |
| `orcaflex-text-model-writers` | 211 | [docs/registry/implementation-map.yaml:L7092-L7101] |
| `orcaflex-text-model-writers` | 212 | [docs/registry/implementation-map.yaml:L7102-L7111] |
| `orcaflex-text-model-writers` | 213 | [docs/registry/implementation-map.yaml:L7112-L7121] |
| `orcaflex-text-model-writers` | 214 | [docs/registry/implementation-map.yaml:L7122-L7131] |
| `orcaflex-text-model-writers` | 215 | [docs/registry/implementation-map.yaml:L7132-L7141] |
| `orcaflex-text-model-writers` | 216 | [docs/registry/implementation-map.yaml:L7142-L7153] |
| `orcaflex-text-model-writers` | 217 | [docs/registry/implementation-map.yaml:L7154-L7163] |
| `orcaflex-text-model-writers` | 218 | [docs/registry/implementation-map.yaml:L7164-L7176] |
| `orcaflex-text-model-writers` | 219 | [docs/registry/implementation-map.yaml:L7177-L7186] |
| `orcaflex-text-model-writers` | 220 | [docs/registry/implementation-map.yaml:L7187-L7196] |
| `orcaflex-text-model-writers` | 221 | [docs/registry/implementation-map.yaml:L7197-L7207] |
| `orcaflex-text-model-writers` | 222 | [docs/registry/implementation-map.yaml:L7208-L7217] |
| `orcaflex-text-model-writers` | 223 | [docs/registry/implementation-map.yaml:L7218-L7227] |
| `orcaflex-text-model-writers` | 224 | [docs/registry/implementation-map.yaml:L7228-L7237] |
| `orcaflex-text-model-writers` | 225 | [docs/registry/implementation-map.yaml:L7238-L7247] |
| `orcaflex-text-model-writers` | 226 | [docs/registry/implementation-map.yaml:L7248-L7257] |
| `orcaflex-text-model-writers` | 227 | [docs/registry/implementation-map.yaml:L7258-L7269] |
| `orcaflex-text-model-writers` | 228 | [docs/registry/implementation-map.yaml:L7270-L7279] |
| `orcaflex-text-model-writers` | 229 | [docs/registry/implementation-map.yaml:L7280-L7289] |
| `orcaflex-text-model-writers` | 230 | [docs/registry/implementation-map.yaml:L7290-L7299] |
| `orcaflex-text-model-writers` | 231 | [docs/registry/implementation-map.yaml:L7300-L7309] |
| `orcaflex-text-model-writers` | 232 | [docs/registry/implementation-map.yaml:L7310-L7319] |
| `orcaflex-text-model-writers` | 233 | [docs/registry/implementation-map.yaml:L7320-L7329] |
| `orcaflex-text-model-writers` | 234 | [docs/registry/implementation-map.yaml:L7330-L7339] |
| `orcaflex-text-model-writers` | 235 | [docs/registry/implementation-map.yaml:L7340-L7349] |
| `orcaflex-text-model-writers` | 236 | [docs/registry/implementation-map.yaml:L7350-L7359] |
| `orcaflex-text-model-writers` | 237 | [docs/registry/implementation-map.yaml:L7360-L7369] |
| `orcaflex-text-model-writers` | 238 | [docs/registry/implementation-map.yaml:L7370-L7379] |
| `orcaflex-text-model-writers` | 239 | [docs/registry/implementation-map.yaml:L7380-L7389] |
| `orcaflex-text-model-writers` | 240 | [docs/registry/implementation-map.yaml:L7390-L7399] |
| `orcaflex-text-model-writers` | 241 | [docs/registry/implementation-map.yaml:L7400-L7409] |
| `orcaflex-text-model-writers` | 242 | [docs/registry/implementation-map.yaml:L7410-L7419] |
| `orcaflex-text-model-writers` | 243 | [docs/registry/implementation-map.yaml:L7420-L7429] |
| `orcaflex-text-model-writers` | 244 | [docs/registry/implementation-map.yaml:L7430-L7439] |
| `orcaflex-text-model-writers` | 245 | [docs/registry/implementation-map.yaml:L7440-L7449] |
| `orcaflex-text-model-writers` | 246 | [docs/registry/implementation-map.yaml:L7450-L7459] |
| `orcaflex-text-model-writers` | 247 | [docs/registry/implementation-map.yaml:L7460-L7469] |
| `orcaflex-text-model-writers` | 248 | [docs/registry/implementation-map.yaml:L7470-L7479] |
| `orcaflex-text-model-writers` | 249 | [docs/registry/implementation-map.yaml:L7480-L7489] |
| `orcaflex-text-model-writers` | 250 | [docs/registry/implementation-map.yaml:L7490-L7499] |
| `orcaflex-text-model-writers` | 251 | [docs/registry/implementation-map.yaml:L7500-L7509] |
| `orcaflex-text-model-writers` | 252 | [docs/registry/implementation-map.yaml:L7510-L7519] |
| `orcaflex-text-model-writers` | 253 | [docs/registry/implementation-map.yaml:L7520-L7529] |
| `orcaflex-text-model-writers` | 254 | [docs/registry/implementation-map.yaml:L7530-L7539] |
| `orcaflex-text-model-writers` | 255 | [docs/registry/implementation-map.yaml:L7540-L7551] |
| `orcaflex-text-model-writers` | 256 | [docs/registry/implementation-map.yaml:L7552-L7561] |
| `orcaflex-text-model-writers` | 257 | [docs/registry/implementation-map.yaml:L7562-L7571] |
| `orcaflex-text-model-writers` | 258 | [docs/registry/implementation-map.yaml:L7572-L7581] |
| `orcaflex-text-model-writers` | 259 | [docs/registry/implementation-map.yaml:L7582-L7591] |
| `orcaflex-text-model-writers` | 260 | [docs/registry/implementation-map.yaml:L7592-L7603] |
| `orcaflex-text-model-writers` | 261 | [docs/registry/implementation-map.yaml:L7604-L7613] |
| `orcaflex-text-model-writers` | 262 | [docs/registry/implementation-map.yaml:L7614-L7626] |
| `orcaflex-text-model-writers` | 263 | [docs/registry/implementation-map.yaml:L7627-L7636] |
| `orcaflex-text-model-writers` | 264 | [docs/registry/implementation-map.yaml:L7637-L7646] |
| `orcaflex-text-model-writers` | 265 | [docs/registry/implementation-map.yaml:L7647-L7656] |
| `orcaflex-text-model-writers` | 266 | [docs/registry/implementation-map.yaml:L7657-L7666] |
| `orcaflex-text-model-writers` | 267 | [docs/registry/implementation-map.yaml:L7667-L7676] |
| `orcaflex-text-model-writers` | 268 | [docs/registry/implementation-map.yaml:L7677-L7686] |
| `orcaflex-text-model-writers` | 269 | [docs/registry/implementation-map.yaml:L7687-L7696] |
| `orcaflex-text-model-writers` | 270 | [docs/registry/implementation-map.yaml:L7697-L7706] |
| `orcaflex-text-model-writers` | 271 | [docs/registry/implementation-map.yaml:L7707-L7716] |
| `orcaflex-text-model-writers` | 272 | [docs/registry/implementation-map.yaml:L7717-L7729] |
| `orcaflex-text-model-writers` | 273 | [docs/registry/implementation-map.yaml:L7730-L7739] |
| `orcaflex-text-model-writers` | 274 | [docs/registry/implementation-map.yaml:L7740-L7749] |
| `orcaflex-text-model-writers` | 275 | [docs/registry/implementation-map.yaml:L7750-L7762] |
| `orcaflex-text-model-writers` | 276 | [docs/registry/implementation-map.yaml:L7763-L7774] |
| `orcaflex-text-model-writers` | 277 | [docs/registry/implementation-map.yaml:L7775-L7784] |
| `orcaflex-text-model-writers` | 278 | [docs/registry/implementation-map.yaml:L7785-L7794] |
| `orcaflex-text-model-writers` | 279 | [docs/registry/implementation-map.yaml:L7795-L7804] |
| `orcaflex-text-model-writers` | 280 | [docs/registry/implementation-map.yaml:L7805-L7816] |
| `orcaflex-text-model-writers` | 281 | [docs/registry/implementation-map.yaml:L7817-L7826] |
| `orcaflex-text-model-writers` | 282 | [docs/registry/implementation-map.yaml:L7827-L7836] |
| `orcaflex-text-model-writers` | 283 | [docs/registry/implementation-map.yaml:L7837-L7846] |
| `orcaflex-text-model-writers` | 284 | [docs/registry/implementation-map.yaml:L7847-L7856] |
| `orcaflex-text-model-writers` | 285 | [docs/registry/implementation-map.yaml:L7857-L7866] |
| `orcaflex-text-model-writers` | 286 | [docs/registry/implementation-map.yaml:L7867-L7878] |
| `orcaflex-text-model-writers` | 287 | [docs/registry/implementation-map.yaml:L7879-L7888] |
| `orcaflex-text-model-writers` | 288 | [docs/registry/implementation-map.yaml:L7889-L7898] |
| `orcaflex-text-model-writers` | 289 | [docs/registry/implementation-map.yaml:L7899-L7908] |
| `orcaflex-text-model-writers` | 290 | [docs/registry/implementation-map.yaml:L7909-L7921] |
| `orcaflex-text-model-writers` | 291 | [docs/registry/implementation-map.yaml:L7922-L7931] |
| `orcaflex-text-model-writers` | 292 | [docs/registry/implementation-map.yaml:L7932-L7941] |
| `orcaflex-text-model-writers` | 293 | [docs/registry/implementation-map.yaml:L7942-L7951] |
| `orcaflex-text-model-writers` | 294 | [docs/registry/implementation-map.yaml:L7952-L7961] |
| `orcaflex-text-model-writers` | 295 | [docs/registry/implementation-map.yaml:L7962-L7971] |
| `orcaflex-text-model-writers` | 296 | [docs/registry/implementation-map.yaml:L7972-L7981] |
| `orcaflex-text-model-writers` | 297 | [docs/registry/implementation-map.yaml:L7982-L7991] |
| `orcaflex-text-model-writers` | 298 | [docs/registry/implementation-map.yaml:L7992-L8001] |
| `orcaflex-text-model-writers` | 299 | [docs/registry/implementation-map.yaml:L8002-L8011] |
| `orcaflex-text-model-writers` | 300 | [docs/registry/implementation-map.yaml:L8012-L8021] |
| `orcaflex-text-model-writers` | 301 | [docs/registry/implementation-map.yaml:L8022-L8031] |
| `orcaflex-text-model-writers` | 302 | [docs/registry/implementation-map.yaml:L8032-L8043] |
| `orcaflex-text-model-writers` | 303 | [docs/registry/implementation-map.yaml:L8044-L8053] |
| `orcaflex-text-model-writers` | 304 | [docs/registry/implementation-map.yaml:L8054-L8063] |
| `orcaflex-text-model-writers` | 305 | [docs/registry/implementation-map.yaml:L8064-L8073] |
| `orcaflex-text-model-writers` | 306 | [docs/registry/implementation-map.yaml:L8074-L8083] |
| `orcaflex-text-model-writers` | 307 | [docs/registry/implementation-map.yaml:L8084-L8093] |
| `orcaflex-text-model-writers` | 308 | [docs/registry/implementation-map.yaml:L8094-L8103] |
| `orcaflex-text-model-writers` | 309 | [docs/registry/implementation-map.yaml:L8104-L8113] |
| `orcaflex-text-model-writers` | 310 | [docs/registry/implementation-map.yaml:L8114-L8123] |
| `orcaflex-text-model-writers` | 311 | [docs/registry/implementation-map.yaml:L8124-L8133] |
| `orcaflex-text-model-writers` | 312 | [docs/registry/implementation-map.yaml:L8134-L8143] |
| `orcaflex-text-model-writers` | 313 | [docs/registry/implementation-map.yaml:L8144-L8155] |
| `orcaflex-text-model-writers` | 314 | [docs/registry/implementation-map.yaml:L8156-L8165] |
| `orcaflex-text-model-writers` | 315 | [docs/registry/implementation-map.yaml:L8166-L8175] |
| `orcaflex-text-model-writers` | 316 | [docs/registry/implementation-map.yaml:L8176-L8185] |
| `orcaflex-text-model-writers` | 317 | [docs/registry/implementation-map.yaml:L8186-L8195] |
| `orcaflex-text-model-writers` | 318 | [docs/registry/implementation-map.yaml:L8196-L8205] |
| `orcaflex-text-model-writers` | 319 | [docs/registry/implementation-map.yaml:L8206-L8215] |
| `orcaflex-text-model-writers` | 320 | [docs/registry/implementation-map.yaml:L8216-L8227] |
| `orcaflex-text-model-writers` | 321 | [docs/registry/implementation-map.yaml:L8228-L8237] |
| `orcaflex-text-model-writers` | 322 | [docs/registry/implementation-map.yaml:L8238-L8247] |
| `orcaflex-text-model-writers` | 323 | [docs/registry/implementation-map.yaml:L8248-L8257] |
| `orcaflex-text-model-writers` | 324 | [docs/registry/implementation-map.yaml:L8258-L8267] |
| `orcaflex-text-model-writers` | 325 | [docs/registry/implementation-map.yaml:L8268-L8279] |
| `orcaflex-text-model-writers` | 326 | [docs/registry/implementation-map.yaml:L8280-L8290] |
| `orcaflex-text-model-writers` | 327 | [docs/registry/implementation-map.yaml:L8291-L8300] |
| `orcaflex-text-model-writers` | 328 | [docs/registry/implementation-map.yaml:L8301-L8310] |
| `orcaflex-text-model-writers` | 329 | [docs/registry/implementation-map.yaml:L8311-L8323] |
| `orcaflex-text-model-writers` | 330 | [docs/registry/implementation-map.yaml:L8324-L8333] |
| `orcaflex-text-model-writers` | 331 | [docs/registry/implementation-map.yaml:L8334-L8343] |
| `orcaflex-text-model-writers` | 332 | [docs/registry/implementation-map.yaml:L8344-L8353] |
| `orcaflex-text-model-writers` | 333 | [docs/registry/implementation-map.yaml:L8354-L8363] |
| `orcaflex-text-model-writers` | 334 | [docs/registry/implementation-map.yaml:L8364-L8373] |
| `orcaflex-text-model-writers` | 335 | [docs/registry/implementation-map.yaml:L8374-L8383] |
| `orcaflex-text-model-writers` | 336 | [docs/registry/implementation-map.yaml:L8384-L8393] |
| `orcaflex-text-model-writers` | 337 | [docs/registry/implementation-map.yaml:L8394-L8403] |
| `orcaflex-text-model-writers` | 338 | [docs/registry/implementation-map.yaml:L8404-L8413] |
| `orcaflex-text-model-writers` | 339 | [docs/registry/implementation-map.yaml:L8414-L8423] |
| `orcaflex-text-model-writers` | 340 | [docs/registry/implementation-map.yaml:L8424-L8435] |
| `orcaflex-text-model-writers` | 341 | [docs/registry/implementation-map.yaml:L8436-L8445] |
| `orcaflex-text-model-writers` | 342 | [docs/registry/implementation-map.yaml:L8446-L8457] |
| `orcaflex-text-model-writers` | 343 | [docs/registry/implementation-map.yaml:L8458-L8470] |
| `orcaflex-text-model-writers` | 344 | [docs/registry/implementation-map.yaml:L8471-L8480] |
| `orcaflex-text-model-writers` | 345 | [docs/registry/implementation-map.yaml:L8481-L8492] |
| `orcaflex-text-model-writers` | 346 | [docs/registry/implementation-map.yaml:L8493-L8505] |
| `orcaflex-text-model-writers` | 347 | [docs/registry/implementation-map.yaml:L8506-L8517] |
| `orcaflex-text-model-writers` | 348 | [docs/registry/implementation-map.yaml:L8518-L8527] |
| `orcaflex-text-model-writers` | 349 | [docs/registry/implementation-map.yaml:L8528-L8537] |
| `orcaflex-text-model-writers` | 350 | [docs/registry/implementation-map.yaml:L8538-L8547] |
| `orcaflex-text-model-writers` | 351 | [docs/registry/implementation-map.yaml:L8548-L8560] |
| `orcaflex-text-model-writers` | 352 | [docs/registry/implementation-map.yaml:L8561-L8572] |
| `orcaflex-text-model-writers` | 353 | [docs/registry/implementation-map.yaml:L8573-L8582] |
| `orcaflex-text-model-writers` | 354 | [docs/registry/implementation-map.yaml:L8583-L8596] |
| `orcaflex-text-model-writers` | 355 | [docs/registry/implementation-map.yaml:L8597-L8608] |
| `orcaflex-text-model-writers` | 356 | [docs/registry/implementation-map.yaml:L8609-L8619] |
| `orcaflex-text-model-writers` | 357 | [docs/registry/implementation-map.yaml:L8620-L8632] |
| `orcaflex-text-model-writers` | 358 | [docs/registry/implementation-map.yaml:L8633-L8647] |
| `orcaflex-text-model-writers` | 359 | [docs/registry/implementation-map.yaml:L8648-L8657] |
| `orcaflex-text-model-writers` | 360 | [docs/registry/implementation-map.yaml:L8658-L8667] |
| `orcaflex-text-model-writers` | 361 | [docs/registry/implementation-map.yaml:L8668-L8677] |
| `orcaflex-text-model-writers` | 362 | [docs/registry/implementation-map.yaml:L8678-L8687] |
| `orcaflex-text-model-writers` | 363 | [docs/registry/implementation-map.yaml:L8688-L8701] |
| `orcaflex-text-model-writers` | 364 | [docs/registry/implementation-map.yaml:L8702-L8714] |
| `orcaflex-text-model-writers` | 365 | [docs/registry/implementation-map.yaml:L8715-L8724] |
| `orcaflex-text-model-writers` | 366 | [docs/registry/implementation-map.yaml:L8725-L8739] |
| `orcaflex-text-model-writers` | 367 | [docs/registry/implementation-map.yaml:L8740-L8749] |
| `orcaflex-text-model-writers` | 368 | [docs/registry/implementation-map.yaml:L8750-L8759] |
| `orcaflex-text-model-writers` | 369 | [docs/registry/implementation-map.yaml:L8760-L8773] |
| `orcaflex-text-model-writers` | 370 | [docs/registry/implementation-map.yaml:L8774-L8783] |
| `orcaflex-text-model-writers` | 371 | [docs/registry/implementation-map.yaml:L8784-L8793] |
| `orcaflex-text-model-writers` | 372 | [docs/registry/implementation-map.yaml:L8794-L8803] |
| `orcaflex-text-model-writers` | 373 | [docs/registry/implementation-map.yaml:L8804-L8813] |
| `orcaflex-text-model-writers` | 374 | [docs/registry/implementation-map.yaml:L8814-L8823] |
| `orcaflex-text-model-writers` | 375 | [docs/registry/implementation-map.yaml:L8824-L8833] |
| `orcaflex-text-model-writers` | 376 | [docs/registry/implementation-map.yaml:L8834-L8843] |
| `orcaflex-text-model-writers` | 377 | [docs/registry/implementation-map.yaml:L8844-L8856] |
| `orcaflex-text-model-writers` | 378 | [docs/registry/implementation-map.yaml:L8857-L8866] |
| `orcaflex-text-model-writers` | 379 | [docs/registry/implementation-map.yaml:L8867-L8876] |
| `orcaflex-text-model-writers` | 380 | [docs/registry/implementation-map.yaml:L8877-L8886] |
| `orcaflex-text-model-writers` | 381 | [docs/registry/implementation-map.yaml:L8887-L8896] |
| `orcaflex-text-model-writers` | 382 | [docs/registry/implementation-map.yaml:L8897-L8908] |
| `orcaflex-text-model-writers` | 383 | [docs/registry/implementation-map.yaml:L8909-L8918] |
| `orcaflex-text-model-writers` | 384 | [docs/registry/implementation-map.yaml:L8919-L8928] |
| `orcaflex-text-model-writers` | 385 | [docs/registry/implementation-map.yaml:L8929-L8938] |
| `orcaflex-text-model-writers` | 386 | [docs/registry/implementation-map.yaml:L8939-L8948] |
| `orcaflex-text-model-writers` | 387 | [docs/registry/implementation-map.yaml:L8949-L8958] |
| `orcaflex-text-model-writers` | 388 | [docs/registry/implementation-map.yaml:L8959-L8970] |
| `orcaflex-text-model-writers` | 389 | [docs/registry/implementation-map.yaml:L8971-L8980] |
| `orcaflex-text-model-writers` | 390 | [docs/registry/implementation-map.yaml:L8981-L8990] |
| `orcaflex-text-model-writers` | 391 | [docs/registry/implementation-map.yaml:L8991-L9000] |
| `orcaflex-text-model-writers` | 392 | [docs/registry/implementation-map.yaml:L9001-L9010] |
| `orcaflex-text-model-writers` | 393 | [docs/registry/implementation-map.yaml:L9011-L9020] |
| `orcaflex-text-model-writers` | 394 | [docs/registry/implementation-map.yaml:L9021-L9030] |
| `orcaflex-text-model-writers` | 395 | [docs/registry/implementation-map.yaml:L9031-L9040] |
| `orcaflex-text-model-writers` | 396 | [docs/registry/implementation-map.yaml:L9041-L9050] |
| `orcaflex-text-model-writers` | 397 | [docs/registry/implementation-map.yaml:L9051-L9060] |
| `orcaflex-text-model-writers` | 398 | [docs/registry/implementation-map.yaml:L9061-L9070] |
| `orcaflex-text-model-writers` | 399 | [docs/registry/implementation-map.yaml:L9071-L9080] |
| `orcaflex-text-model-writers` | 400 | [docs/registry/implementation-map.yaml:L9081-L9090] |
| `orcaflex-text-model-writers` | 401 | [docs/registry/implementation-map.yaml:L9091-L9100] |
| `orcaflex-text-model-writers` | 402 | [docs/registry/implementation-map.yaml:L9101-L9110] |
| `orcaflex-text-model-writers` | 403 | [docs/registry/implementation-map.yaml:L9111-L9120] |
| `orcaflex-text-model-writers` | 404 | [docs/registry/implementation-map.yaml:L9121-L9130] |
| `orcaflex-text-model-writers` | 405 | [docs/registry/implementation-map.yaml:L9131-L9140] |
| `orcaflex-text-model-writers` | 406 | [docs/registry/implementation-map.yaml:L9141-L9152] |
| `orcaflex-text-model-writers` | 407 | [docs/registry/implementation-map.yaml:L9153-L9162] |
| `orcaflex-text-model-writers` | 408 | [docs/registry/implementation-map.yaml:L9163-L9172] |
| `orcaflex-text-model-writers` | 409 | [docs/registry/implementation-map.yaml:L9173-L9182] |
| `orcaflex-text-model-writers` | 410 | [docs/registry/implementation-map.yaml:L9183-L9192] |
| `orcaflex-text-model-writers` | 411 | [docs/registry/implementation-map.yaml:L9193-L9202] |
| `orcaflex-text-model-writers` | 412 | [docs/registry/implementation-map.yaml:L9203-L9212] |
| `orcaflex-text-model-writers` | 413 | [docs/registry/implementation-map.yaml:L9213-L9222] |
| `orcaflex-text-model-writers` | 414 | [docs/registry/implementation-map.yaml:L9223-L9232] |
| `orcaflex-text-model-writers` | 415 | [docs/registry/implementation-map.yaml:L9233-L9242] |
| `orcaflex-text-model-writers` | 416 | [docs/registry/implementation-map.yaml:L9243-L9252] |
| `orcaflex-text-model-writers` | 417 | [docs/registry/implementation-map.yaml:L9253-L9262] |
| `orcaflex-text-model-writers` | 418 | [docs/registry/implementation-map.yaml:L9263-L9272] |
| `orcaflex-text-model-writers` | 419 | [docs/registry/implementation-map.yaml:L9273-L9282] |
| `orcaflex-text-model-writers` | 420 | [docs/registry/implementation-map.yaml:L9283-L9292] |
| `orcaflex-text-model-writers` | 421 | [docs/registry/implementation-map.yaml:L9293-L9302] |
| `orcaflex-text-model-writers` | 422 | [docs/registry/implementation-map.yaml:L9303-L9312] |
| `orcaflex-text-model-writers` | 423 | [docs/registry/implementation-map.yaml:L9313-L9322] |
| `orcaflex-text-model-writers` | 424 | [docs/registry/implementation-map.yaml:L9323-L9332] |
| `orcaflex-text-model-writers` | 425 | [docs/registry/implementation-map.yaml:L9333-L9342] |
| `orcaflex-text-model-writers` | 426 | [docs/registry/implementation-map.yaml:L9343-L9352] |
| `orcaflex-text-model-writers` | 427 | [docs/registry/implementation-map.yaml:L9353-L9362] |
| `orcaflex-text-model-writers` | 428 | [docs/registry/implementation-map.yaml:L9363-L9372] |
| `orcaflex-text-model-writers` | 429 | [docs/registry/implementation-map.yaml:L9373-L9382] |
| `orcaflex-text-model-writers` | 430 | [docs/registry/implementation-map.yaml:L9383-L9392] |
| `orcaflex-text-model-writers` | 431 | [docs/registry/implementation-map.yaml:L9393-L9402] |
| `orcaflex-text-model-writers` | 432 | [docs/registry/implementation-map.yaml:L9403-L9412] |
| `orcaflex-text-model-writers` | 433 | [docs/registry/implementation-map.yaml:L9413-L9422] |
| `orcaflex-text-model-writers` | 434 | [docs/registry/implementation-map.yaml:L9423-L9432] |
| `orcaflex-text-model-writers` | 435 | [docs/registry/implementation-map.yaml:L9433-L9442] |
| `orcaflex-text-model-writers` | 436 | [docs/registry/implementation-map.yaml:L9443-L9452] |
| `orcaflex-text-model-writers` | 437 | [docs/registry/implementation-map.yaml:L9453-L9462] |
| `orcaflex-text-model-writers` | 438 | [docs/registry/implementation-map.yaml:L9463-L9472] |
| `orcaflex-text-model-writers` | 439 | [docs/registry/implementation-map.yaml:L9473-L9482] |
| `orcaflex-text-model-writers` | 440 | [docs/registry/implementation-map.yaml:L9483-L9493] |

## 10. Coverage and open items

### Read coverage

All seven Python files and the README directly inside the package were read; there are no nested implementation subpackages at the code pin. The Python occurrence index in section 9 also provides complete source-file coverage. [src/digitalmodel/solvers/orcaflex/universal/__init__.py:L1-L35] [src/digitalmodel/solvers/orcaflex/universal/README.md:L1-L409]

The dedicated test file and helper script, the seven package-specific nodes from `test_mock_is_explicit.py`, and routing/contract tests were inspected; unrelated solver-dependent tests are not physical verification evidence. [tests/solvers/orcaflex/universal/test_universal_runner.py:L1-L305] [tests/solvers/orcaflex/universal/scripts/run_universal.py:L1-L29] [tests/solvers/orcaflex/test_mock_is_explicit.py:L84-L132] [tests/solvers/orcaflex/test_mock_is_explicit.py:L238-L264] [tests/solvers/orcaflex/test_mock_is_explicit.py:L317-L328] [tests/docs/test_digitalmodel_routing_contract.py:L1-L158]

Docs discovered by package/class-name search and inspected as context are listed below; historical performance, maturity and completion claims are not adopted as validation.

| Context document | Read locator |
| --- | --- |
| `docs/domains/examples/orcaflex/run_universal_example.py` | [docs/domains/examples/orcaflex/run_universal_example.py:L3-L8] |
| `docs/domains/examples/orcaflex/batch_config_example.yml` | [docs/domains/examples/orcaflex/batch_config_example.yml:L1-L6] |
| `docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE3.md` | [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE3.md:L205-L210] |
| `docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md` | [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md:L216-L221] |
| `docs/domains/project-docs/MODULE_PORTFOLIO_2026.md` | [docs/domains/project-docs/MODULE_PORTFOLIO_2026.md:L94-L99] |
| `docs/domains/project-docs/test_performance_analysis.json` | [docs/domains/project-docs/test_performance_analysis.json:L164-L169] |
| `docs/domains/orcaflex/ORCAFLEX_AUTO_OPTIMIZATION_GUIDE.md` | [docs/domains/orcaflex/ORCAFLEX_AUTO_OPTIMIZATION_GUIDE.md:L59-L64] |
| `docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md` | [docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md:L54-L59] |
| `docs/domains/orcaflex/dynamic-analysis-guide.md` | [docs/domains/orcaflex/dynamic-analysis-guide.md:L5-L10] |
| `docs/domains/orcaflex/ORCAFLEX_MODEL_PREPARATION_GUIDE.md` | [docs/domains/orcaflex/ORCAFLEX_MODEL_PREPARATION_GUIDE.md:L302-L307] |
| `docs/domains/orcaflex/DYNAMIC_ANALYSIS_RELEASE_NOTES.md` | [docs/domains/orcaflex/DYNAMIC_ANALYSIS_RELEASE_NOTES.md:L3-L8] |

One additional planning document contains restricted operational identifiers and is intentionally not reproduced; documentation and discovery codenaming follow-up is included in issue pending (6).

### Open-item register

| Item | Evidence / scope | Tracking and responsible discipline |
| --- | --- | --- |
| Qualification envelope | UNVERIFIED claims in sections 1–3: target solver version, geometry/depth/load envelope, file units/axes/sign/phase conventions and engineering reference edition | issue pending (5); solver workflow engineering and family quality manager |
| Independent engineering example | No validated physical benchmark; native-file node lacks its fixture; artificial success is not solver evidence. [tests/solvers/orcaflex/universal/test_universal_runner.py:L193-L259] | issue pending (5); solver workflow engineering |
| Input/discovery completeness | Reject accounting, stale statistics, permissive recognition and output basename collisions, section 6. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L68-L124] [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L449-L450] | issue pending (1); solver workflow engineering |
| Execution parity and configuration | Dynamic batch requests, duration fallback, unused settings and CLI numeric/test-mode handling, section 6. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L283-L308] [src/digitalmodel/solvers/orcaflex/universal/__main__.py:L29-L52] | issue pending (2); solver workflow engineering |
| Complete report and diagnostic record | Counts alone omit result details and provenance; error/display/resource branches lack individual detection tests, section 6. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L316-L350] [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L279-L309] | issue pending (3), issue pending (4); solver workflow engineering |
| Native QA gates | U04–U07 and U09 lack native evidence; diffraction QA is conditional; independent-provider clearance remains outstanding | issue pending (5); family quality manager; AQG r0, sections 5–11 |
| Docs, discovery labels, test and map coverage | Historical examples/imports and the writer-family classification require reconciliation. [docs/domains/examples/orcaflex/run_universal_example.py:L14-L17] [src/digitalmodel/solvers/orcaflex/universal/README.md:L242-L255] [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L309-L332] | issue pending (6); solver workflow engineering |

The three qualification claims marked UNVERIFIED in sections 1–3 are one grouped qualification cause, rather than independent solver-validation results; no client-derived data or private sidecar values are included.
