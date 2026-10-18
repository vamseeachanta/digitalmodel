# Universal runner knowledge pack

| Header | Value |
| --- | --- |
| Module | solvers/orcaflex/universal |
| Repository | digitalmodel |
| Package path | src/digitalmodel/solvers/orcaflex/universal |
| Pack revision | 1, 2026-10-18 |
| Code revision | c099a5e1317e6bed63d876252d87cbb12c774372 |
| Analysis family | OrcaFlex riser and mooring |
| Family quality manager | OrcaFlex riser/mooring quality manager |
| Status | DRAFT |

Repository source citations and the implementation map use the code revision above, which is in main history. Section 9 identifies map entries by family id and symbol.

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

Run records: dedicated suite, 8 collected / 7 passed / 0 failed / 1 skipped (the same node recorded NOT_RUN above); supplemental package-specific refusal/artifact checks, 7 collected / 7 passed / 0 failed / 0 skipped; contracts plus routing, 497 collected / 496 passed / 0 failed / 1 skipped, at BASE and after. The additional skip is the installed-package metadata check in an uninstalled checkout; it is separate from the native-fixture skip and supplies no engineering evidence. The code pin is unchanged by this documentation change.

Command form: `PYTHONPATH=src uv run --no-project --with pytest --with pytest-asyncio --with pyyaml --with psutil --with loguru --with numpy --with scipy --with pandas --with assetutilities --with pydantic python -m pytest <paths-or-node-IDs> -q -rA -p no:cacheprovider --basetemp=<scratch>`; the dedicated suite ran from a byte-identical temporary test copy because its fixtures write beside the test file. No addopts override was used. [tests/solvers/orcaflex/universal/test_universal_runner.py:L34-L47] [tests/solvers/orcaflex/universal/test_universal_runner.py:L187-L217]

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
rg -n 'except|continue|return None|return \[\]|return \{\}|\b0(?:\.0)?\b|1970|0\.[0-9]+|\b(?:int|float)\(|errors=|unit|cache' src/digitalmodel/solvers/orcaflex/universal -g '*.py'
```

sweep: 127 found

The sweep contains 100 AST candidate locations (94 original candidates and six timing constants) and 27 semantic sites. Semantic sites are detailed below; syntax candidates are grouped by file afterwards. Counts overlap where a semantic site explains a syntax candidate. No `continue`, epoch-1970 default, fixed 0.05 fraction, float conversion of external text or empty-result cache write occurs in the read package files.

| Site | Symptom / disposition | Detection / tracking |
| --- | --- | --- |
| Default exclusions and reset timing | Included names may be removed; missing-directory returns retain previous statistics. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L77-L83] | no detection check exists; issue pending (1). |
| Record omission | Excluded or invalid candidates never become failed result rows. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L97-L112] | no detection check exists; issue pending (1). |
| Permissive YAML validation | Recognized keys can pass without strict duplicate-key, reference or unit checks. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L225-L242] | no detection check exists; issue pending (1). |
| Decode errors ignored | DAT decoding discards errors; only first 10 lines are searched. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L248-L255] | no detection check exists; issue pending (1). |
| Size-only SIM validation | More than 100 bytes does not establish a loadable simulation. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L260-L263] | no detection check exists; issue pending (1). |
| Related-model duplicates | Overlapping globs can append a model repeatedly. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L289-L295] | no detection check exists; issue pending (1). |
| Discovery labels | Discovery labels require reconciliation. | no detection check exists; issue pending (6). |
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

### Syntax candidate summary

The 94 original syntax candidates are grouped below by file; counts are candidate locations, not independent defects. Numeric conversion, empty-work, exception and duration semantics are also described in the rows above. Each line list retains its detection disposition. The discovery command above and AST categories below reproduce the inventory.

| Python file | Candidate count and lines at the code pin | Disposition / detection |
| --- | --- | --- |
| `__main__.py` | 5; numeric conversion: 49; zero or fixed fraction: 201, 206; exception handler: 208, 211 | Counters, bounds, display arithmetic or captured fallbacks; no detection check exists for each individual branch; issue pending (4). |
| `batch_processor.py` | 25; zero or fixed fraction: 55, 56, 60, 61, 95, 97, 98, 150, 153, 242, 266, 331, 336, 360, 371, 393, 395, 396; empty or None return: 91, 384; exception handler: 164, 188, 224, 310; numeric conversion: 178 | Counters, bounds, display arithmetic or captured fallbacks; no detection check exists for each individual branch; issue pending (4). |
| `model_discovery.py` | 8; empty or None return: 70, 74; zero or fixed fraction: 204, 350, 358; exception handler: 215, 241, 257 | Counters, bounds, display arithmetic or captured fallbacks; no detection check exists for each individual branch; issue pending (4). |
| `path_resolver.py` | 4; exception handler: 92, 276, 310; empty or None return: 96 | Counters, bounds, display arithmetic or captured fallbacks; no detection check exists for each individual branch; issue pending (4). |
| `status_reporter.py` | 40; zero or fixed fraction: 39, 40, 41, 42, 62, 90, 93, 132, 143, 153, 160, 179, 204, 231, 253, 286, 293, 299, 300, 301, 302, 345, 346, 347, 348, 354, 358; exception handler: 73, 78, 114, 373; empty or None return: 87, 128, 133, 214; numeric conversion: 137, 335, 336, 339, 340 | Counters, bounds, display arithmetic or captured fallbacks; no detection check exists for each individual branch; issue pending (4). |
| `universal_runner.py` | 12; exception handler: 43, 431, 455; zero or fixed fraction: 54, 55, 56, 58, 73, 302, 382, 429, 495 | Counters, bounds, display arithmetic or captured fallbacks; no detection check exists for each individual branch; issue pending (4). |

Six additional timing constants are intentional delays or sampling/display intervals, without a physical-result default: `batch_processor.py:183,275`, `status_reporter.py:63` and `universal_runner.py:393,395,397`. No detection check exists for timing accuracy; issue pending (4).

AST categories: exception handlers; bare or explicit empty/None returns; calls to `int` or `float`; numeric zero or fractions strictly between zero and one (Boolean constants excluded). Candidate locations are deduplicated within each category. Bare returns are retained as display preconditions rather than empty-result errors.

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

Map source: `docs/registry/implementation-map.yaml` at code revision `c099a5e1317e6bed63d876252d87cbb12c774372` in main history; map data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. Family ids, family row counts and in-package symbols below identify the inventory without line-number dependencies. Both families with in-package rows are counted, ranked by their in-package row counts. Full family inventories are the `implementations` sequences under these ids at that pin.

| Rank | Counted family | In-package rows | Full family rows | Package inventory |
| --- | --- | --- | --- | --- |
| 1 | `external-solver-batch-run-stacks` | 2 | 137 | `BatchProcessor._default_processor`, `UniversalOrcaFlexRunner._process_single_model`. |
| 2 | `orcaflex-text-model-writers` | 1 | 440 | `UniversalOrcaFlexRunner._process_single_model`. |

The following source definitions account for the three in-package map rows.

| Family | Definition | Evidence |
| --- | --- | --- |
| `external-solver-batch-run-stacks` | `BatchProcessor._default_processor` | [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L258-L314] |
| `external-solver-batch-run-stacks` | `UniversalOrcaFlexRunner._process_single_model` | [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L371-L460] |
| `orcaflex-text-model-writers` | `UniversalOrcaFlexRunner._process_single_model` | [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L371-L460] |

### Exact signal search and definition classification

For each counted family, apply each `search_signals[].pattern` exactly as stored using case-sensitive Python `re.search`, to every package `.py` line at the code pin. Omit any signal when its pattern matches `solvers/orcaflex/universal` case-insensitively. Assign hits to the smallest enclosing function or class, or one module-level definition. No regex normalization or additional case folding is used.

| Family | Retained signal definition | Omitted signal | Hit classification |
| --- | --- | --- | --- |
| `external-solver-batch-run-stacks` | Algorithm from this family’s `search_signals`, exactly as stored. | vocabulary and constants/keys: path matches (`Solve` matches `solvers` case-insensitively) | 5 enclosing definitions; see exhaustive table below. |
| `orcaflex-text-model-writers` | Algorithm and constants/keys from this family’s `search_signals`, exactly as stored. | vocabulary: path match | 21 enclosing definitions; see exhaustive table below. |

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

### Open-item register

Numbered follow-up drafts are tracked in `ISSUES_DRAFT.md` in the scratch output supplied to the maintainers. They are not repository issues yet; the maintainers need to retain the drafts and replace the markers with issue numbers before merge.

| Item | Evidence / scope | Tracking and responsible discipline |
| --- | --- | --- |
| Qualification envelope | UNVERIFIED claims in sections 1–3: target solver version, geometry/depth/load envelope, file units/axes/sign/phase conventions and engineering reference edition | issue pending (5); solver workflow engineering and family quality manager |
| Independent engineering example | No validated physical benchmark; native-file node lacks its fixture; artificial success is not solver evidence. [tests/solvers/orcaflex/universal/test_universal_runner.py:L193-L259] | issue pending (5); solver workflow engineering |
| Input/discovery completeness | Reject accounting, stale statistics, permissive recognition and output basename collisions, section 6. [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L68-L124] [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L449-L450] | issue pending (1); solver workflow engineering |
| Execution parity and configuration | Dynamic batch requests, duration fallback, unused settings and CLI numeric/test-mode handling, section 6. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L283-L308] [src/digitalmodel/solvers/orcaflex/universal/__main__.py:L29-L52] | issue pending (2); solver workflow engineering |
| Complete report and diagnostic record | Counts alone omit result details and provenance; error/display/resource branches lack individual detection tests, section 6. [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L316-L350] [src/digitalmodel/solvers/orcaflex/universal/status_reporter.py:L279-L309] | issue pending (3), issue pending (4); solver workflow engineering |
| Native QA gates | U04–U07 and U09 lack native evidence; diffraction QA is conditional | issue pending (5); family quality manager; AQG r0, sections 5–11 |
| Docs, discovery labels, test and map coverage | Historical examples/imports and the writer-family classification require reconciliation. [docs/domains/examples/orcaflex/run_universal_example.py:L14-L17] [src/digitalmodel/solvers/orcaflex/universal/README.md:L242-L255] [src/digitalmodel/solvers/orcaflex/universal/model_discovery.py:L309-L332] | issue pending (6); solver workflow engineering |

The three qualification claims marked UNVERIFIED in sections 1–3 are one grouped qualification cause, rather than independent solver-validation results; no project-supplied data or private sidecar values are included.
