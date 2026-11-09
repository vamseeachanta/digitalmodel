# Examples integration knowledge pack

| Field | Value |
| --- | --- |
| Module | solvers/orcaflex/examples_integration |
| Repository | digitalmodel |
| Package path | `src/digitalmodel/solvers/orcaflex/examples_integration` |
| Pack revision | 0 |
| Code revision | `444d36f174ed7b2b97f9da7ef3d6ca44ed6baa55` |
| Analysis family | OrcaFlex riser and mooring |
| Quality manager role | Riser and mooring quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The package exports a downloader, YAML converter, feature analyzer and knowledge integrator. [src/digitalmodel/solvers/orcaflex/examples_integration/__init__.py:L1-L20]

The main path collects files, converts data through `LoadData`/`SaveData`, counts YAML components and builds JSON indexes; filename-based inference is a separate path. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L195-L253] [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L36-L65] [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L76-L134] [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L223-L270] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L85-L106]

The existing solver domain is marked beta; this is domain metadata rather than evidence that a converted model has passed a solver analysis. [docs/registry/module-routing.yaml:L477-L482] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L181-L187]

Mock conversion can be selected automatically after an API import failure; its output is a placeholder and is unsuitable as an engineering reference. [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L27-L34] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L87-L92] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L193-L227]

## 2. Inputs and outputs

### Inputs

| Input | Unit and range | Default | Source and interpretation |
| --- | --- | --- | --- |
| `base_dir` | Directory path; no range validator | `docs/domains/orcaflex/examples` | Creates raw and metadata directories. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L29-L61] |
| Category/example dictionaries | Strings: name, URL, category, file type | No individual example default | Category extraction uses a placeholder HTML class; examples use `.dat`/`.sim` links. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L118-L193] |
| `file_paths`, `use_mock` | Paths; Boolean | `use_mock=False` | `.dat`/`.sim` conversion, with automatic mock fallback. [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L17-L65] |
| Batch input/output directories, `validate`, `max_retries` | Paths, Boolean, count; no bounds check | `validate=True`, `max_retries=2` | Existing nonempty output is reused before validation. [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L23-L65] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L118-L161] |
| YAML component/environment dictionaries | Counts in objects; physical units not interpreted | Missing numeric features start at zero | Reads top-level vessels, lines, buoys, Environment/Waves/Current. [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L23-L74] [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L96-L134] |
| `agent_dir`, analyses | Path and feature dictionaries | `agents/orcaflex` | Creates context directory and merges examples by metadata name. [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L19-L81] |
| ZIP size text and content length | Displayed MB/Gb; header bytes | Unknown size becomes zero | Enhanced parser accepts integer `Mb`; complete parser accepts decimal M/G and multiplies G by 1024. [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L123-L136] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L178-L191] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L131-L138] |

### Outputs

| Output | Unit/range | Destination/default | Meaning |
| --- | --- | --- | --- |
| Raw example and download manifest | Bytes; MD5 checksum string | `raw/`, `metadata/download_manifest.json` under base directory | Existing raw files are accepted without rechecking at download time; `verify_downloads` checks separately. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L205-L253] [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L296-L331] |
| Conversion tuple list | Status strings and paths | Returned; real mode writes `.yml` beside input | Mock mode of `YamlConverter` returns status only and writes no YAML. [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L46-L65] |
| Batch YAML, report and JSON | Bytes; counts; elapsed seconds | Output directory | Mock structure declares SI with fixed placeholder environment; validation checks shape and size rather than physical correctness. [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L193-L253] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L307-L364] |
| Feature dictionary and summary | Object counts, Boolean flags; file size in bytes | JSON to caller-selected path | Analysis flags and environmental numeric fields remain schema defaults in `analyze_file`. [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L23-L74] [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L88-L134] [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L156-L195] |
| Knowledge and indexes | Counts and names, no physical unit | Context JSON and Markdown | Patterns use occurrence counts and truthy analysis values. [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L83-L155] [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L157-L270] |

No coordinate transformation, origin, sign or phase conversion is implemented in the extraction path; the mock writer declares SI while the analyzer stores component lengths and selected strings without unit conversion. [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L96-L134] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L197-L224]

UNVERIFIED: physical unit, frame, origin, sign and phase preservation across real vendor conversion and the supported target version have not been demonstrated; see section 10, issue pending (4).

## 3. Method and governing standards

| Method | Implementation and limit | Basis |
| --- | --- | --- |
| HTTP retrieval | `_download_with_retry`: rate limiting, timeout and doubling retry delay; exhausted request returns `None`. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L80-L116] | Code algorithm, no engineering standard claimed. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L88-L116] |
| File identity | MD5 over downloaded bytes, then reread comparison in `verify_downloads`. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L228-L244] [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L310-L326] | File integrity only; no physical acceptance calculation. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L296-L331] |
| ZIP extraction | Enhanced and complete downloaders flatten archive paths and index by basename. [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L238-L261] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L241-L266] | Collisions and partial extraction need separate checks; see section 6. [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L280-L284] |
| Data conversion | `YamlConverter.convert_batch`, batch `_real_convert`, and `OrcFxAPIConverter.convert_file` delegate to vendor load/save calls. [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L48-L63] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L163-L191] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L118-L165] | No solver execution or response calculation is demonstrated by these calls. [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L48-L63] |
| Feature extraction | `FeatureAnalyzer.analyze_file` reads YAML, counts selected containers and inspects buoy type strings. [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L88-L134] | Schema defaults are not measured model properties. [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L23-L74] |
| Inferred metadata | `DownloadedExamplesIntegrator` derives components, analysis flags and environment categories from filename/category keywords. [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L108-L257] | Classifications only; no catenary, fatigue or VIV calculation. [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L170-L182] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L216-L257] |
| Knowledge aggregation | `KnowledgeIntegrator` counts combinations and builds indexes; the downloaded-example variant also writes summaries. [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L83-L155] [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L223-L270] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L259-L433] | Frequency-derived suggestions are not governing engineering criteria. [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L123-L155] |

No governing engineering standard or standards-derived calculation sidecar is asserted here: these paths retrieve, serialize or classify data; AQG r0 supplies the proposed acceptance gates in section 5. [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L36-L91] [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L76-L195] (AQG r0, sections 3–10.)

## 4. Validated worked examples

Validated engineering examples: 0; the tests below use synthetic data and package behavior assertions, with no independent tension, motion or fatigue reference value. [tests/solvers/orcaflex/test_examples_downloader.py:L104-L141] [tests/solvers/orcaflex/test_examples_downloader.py:L185-L295]

Regression examples: 13; PASS in the baseline run; the documentation-tree repeat is pending, using the offline pytest form below; equality criteria come from the cited assertions rather than physical tolerances.

| Case | Input and expected value/tolerance basis | Existing test node ID | Offline result and source |
| --- | --- | --- | --- |
| R01 | Synthetic fixture; 2 directories present; 0 missing, exact assertions | `tests/solvers/orcaflex/test_examples_downloader.py::TestOrcaflexExampleDownloader::test_directory_creation` | PASS at BASE; AFTER pending. [tests/solvers/orcaflex/test_examples_downloader.py:L37-L45] |
| R02 | Synthetic fixture; 3 required keys and dictionary containers, exact assertions | `tests/solvers/orcaflex/test_examples_downloader.py::TestOrcaflexExampleDownloader::test_manifest_creation` | PASS at BASE; AFTER pending. [tests/solvers/orcaflex/test_examples_downloader.py:L47-L55] |
| R03 | Synthetic fixture; 1 synthetic example checksum retained, exact string equality | `tests/solvers/orcaflex/test_examples_downloader.py::TestOrcaflexExampleDownloader::test_save_and_load_manifest` | PASS at BASE; AFTER pending. [tests/solvers/orcaflex/test_examples_downloader.py:L57-L78] |
| R04 | Synthetic fixture; delay ≥ 0.1 s, elapsed-time lower bound | `tests/solvers/orcaflex/test_examples_downloader.py::TestOrcaflexExampleDownloader::test_rate_limiting` | PASS at BASE; AFTER pending. [tests/solvers/orcaflex/test_examples_downloader.py:L80-L102] |
| R05 | Synthetic fixture; 3 calls after 2 injected errors, exact count | `tests/solvers/orcaflex/test_examples_downloader.py::TestOrcaflexExampleDownloader::test_retry_logic` | PASS at BASE; AFTER pending. [tests/solvers/orcaflex/test_examples_downloader.py:L104-L123] |
| R06 | Synthetic fixture; 3 calls and None after exhaustion, exact count | `tests/solvers/orcaflex/test_examples_downloader.py::TestOrcaflexExampleDownloader::test_max_retries_exceeded` | PASS at BASE; AFTER pending. [tests/solvers/orcaflex/test_examples_downloader.py:L125-L141] |
| R07 | Synthetic fixture; 2 manually inserted categories, exact count; does not call the parser | `tests/solvers/orcaflex/test_examples_downloader.py::TestOrcaflexExampleDownloader::test_parse_categories_mock` | PASS at BASE; AFTER pending. [tests/solvers/orcaflex/test_examples_downloader.py:L143-L183] |
| R08 | Synthetic fixture; 1 saved synthetic byte payload matches exactly | `tests/solvers/orcaflex/test_examples_downloader.py::TestOrcaflexExampleDownloader::test_download_example` | PASS at BASE; AFTER pending. [tests/solvers/orcaflex/test_examples_downloader.py:L185-L217] |
| R09 | Synthetic fixture; 1 entry in each of valid/corrupted/missing, membership assertions | `tests/solvers/orcaflex/test_examples_downloader.py::TestOrcaflexExampleDownloader::test_verify_downloads` | PASS at BASE; AFTER pending. [tests/solvers/orcaflex/test_examples_downloader.py:L219-L258] |
| R10 | Synthetic fixture; 2 example labels, 1 .dat and 1 .sim label, 1 report file; exact assertions | `tests/solvers/orcaflex/test_examples_downloader.py::TestOrcaflexExampleDownloader::test_generate_download_report` | PASS at BASE; AFTER pending. [tests/solvers/orcaflex/test_examples_downloader.py:L260-L295] |
| R11 | Isolated interpreter; 0 guarded side effects, successful subprocess exit | `tests/solvers/orcaflex/examples_integration/test_packaged_download_import.py::test_download_examples_package_import_is_offline` | PASS at BASE; AFTER pending. [tests/solvers/orcaflex/examples_integration/test_packaged_download_import.py:L103-L105] |
| R12 | Isolated interpreter; 3 dictionary fields exactly equal the packaged class identity | `tests/solvers/orcaflex/examples_integration/test_packaged_download_import.py::test_download_examples_resolves_packaged_downloader` | PASS at BASE; AFTER pending. [tests/solvers/orcaflex/examples_integration/test_packaged_download_import.py:L108-L118] |
| R13 | Isolated interpreter; 0 side effects, exit code 0 and 3 expected help tokens | `tests/solvers/orcaflex/examples_integration/test_packaged_download_import.py::test_download_examples_script_help_is_offline` | PASS at BASE; AFTER pending. [tests/solvers/orcaflex/examples_integration/test_packaged_download_import.py:L121-L154] |

Offline command (generic paths): `PYTHONPATH=src uv run --no-sources python -m pytest tests/solvers/orcaflex/examples_integration tests/solvers/orcaflex/test_examples_downloader.py -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-<run>`; the executable was invoked by its configured full path, without changing addopts.

| Run | Code pin | Collected | Passed | Failed | Skipped | NOT_RUN |
| --- | --- | --- | --- | --- | --- | --- |
| BASE | `444d36f174ed7b2b97f9da7ef3d6ca44ed6baa55` | 15 | 13 | 0 | 2 | 3 legacy script functions outside recursive collection |
| AFTER pending | `444d36f174ed7b2b97f9da7ef3d6ca44ed6baa55` (uncommitted documentation tree) | pending | pending | pending | pending | Same 3 legacy script functions |

The two existing skips are `tests/solvers/orcaflex/test_examples_downloader.py::TestDownloaderIntegration::test_real_website_connection` and `tests/solvers/orcaflex/test_examples_downloader.py::TestDownloaderIntegration::test_parse_real_categories`: both carry the repository's unconditional internet requirement skip. [tests/solvers/orcaflex/test_examples_downloader.py:L298-L325]

Legacy functions `test_mock_conversion` and `test_real_conversion` in `tests/solvers/orcaflex/examples_integration/test_converter.py`, and `test_single_download` in `tests/solvers/orcaflex/examples_integration/test_single_download.py`, are NOT_RUN: the existing collection configuration excludes both script files, which import unresolved bare siblings; the real-conversion function also requires the licensed interface and the single-download function performs live network and repository writes. [tests/conftest.py:L46-L53] [tests/solvers/orcaflex/examples_integration/test_converter.py:L6-L7] [tests/solvers/orcaflex/examples_integration/test_converter.py:L15-L99] [tests/solvers/orcaflex/examples_integration/test_single_download.py:L6-L65]

`test_infrastructure.py` defines `check_*` helpers and `run_infrastructure_test`, with no pytest `test_*` function; its standalone routine includes network requests and default-directory construction. [tests/solvers/orcaflex/examples_integration/test_infrastructure.py:L22-L133]

## 5. QA gates

These are acceptance checks, not statements that a gate is cleared; offline regression results do not establish model physics or a target-program load result. (AQG r0, sections 3–10.)

| Gate | Applicability | Check ID and executable route | Numeric pass criterion and unit | Evidence/status |
| --- | --- | --- | --- | --- |
| G0 | Applicable | Q0.1: execute R06 on `_download_with_retry`. | 3 HTTP calls and 1 None result on exhaustion, counts exactly equal. [tests/solvers/orcaflex/test_examples_downloader.py:L125-L141] | PASS offline; transport regression only. |
| G0 | Applicable | Q0.2: execute R09 on `verify_downloads`. | 1 valid, 1 corrupted, 1 missing fixture classified in the expected lists; count 3 entries. [tests/solvers/orcaflex/test_examples_downloader.py:L219-L258] | PASS offline; existing-download acceptance still needs coverage. |
| G0 | Applicable | Q0.3: execute R08 on `download_example` and R11–R13 on package imports/help. | 0 byte mismatches and 0 guarded import/help side effects; exact equality. [tests/solvers/orcaflex/test_examples_downloader.py:L185-L217] [tests/solvers/orcaflex/examples_integration/test_packaged_download_import.py:L25-L68] | PASS offline. |
| G0 | Applicable | Q0.4: inspect every `OrcaFlexBatchConverter` output using a duplicate-key-refusing YAML loader; reject `MockConversion`, reconcile source/output SHA-256 manifests and explicit unit/version declarations before reuse. | 0 duplicate keys, 0 mock outputs in a real set, 0 unresolved references, 0 fingerprint mismatches, 0 unspecified unit systems; counts per file. [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L130-L134] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L193-L253] | Proposed manual acceptance procedure; NOT_RUN on real files, issue pending (4). |
| G1 | Applicable | Q1.1: load each `_real_convert` output in the exact target OrcaFlex version using the documented `Model`/`LoadData` route; a `.sim` input to `OrcFxAPIConverter` uses `LoadSimulation` before `SaveData`. | 0 load errors, 0 warnings, 0 object-count differences; count per model, recorded version equality. [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L168-L179] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L118-L149] | NOT_RUN: licensed program/interface prohibited; issue pending (4). |
| G2 | Applicable | Q2.1: for each converted line, record converged statics, compare end tension and angle to an independent hand reference, then inspect an offset sweep and component table. | 0 unconverged models; tension error ≤ 5 %, angle error ≤ 1.0 degrees; ≥ 6 sweep steps; 0 unchecked components. (AQG r0, section 6.) | NOT_RUN: no independent line basis or licensed results; issue pending (4). These converters only save data. [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L168-L187] |
| G3 | Applicable | Q3.1: run a short target-program simulation from the converted model's statics; compare line tensions and inspect warnings and finite values. | Stop time ≥ 1.0 s, 0 warnings/errors/nonfinite values; tension deviation ≤ 25 % of statics. (AQG r0, section 7.) | NOT_RUN: licensed solve prohibited; issue pending (4). Conversion success alone records no dynamics. [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L124-L155] |
| G4 | Conditional | Q4.1: when a converted vessel type comes from a new diffraction run, require its cited balance/mesh/RAO evidence; otherwise record NOT_APPLICABLE with provenance reason. | 0 missing required records, mass/buoyancy difference ≤ 1.0 %, independent volume difference ≤ 0.5 %. (AQG r0, sections 8 and 10.) | No diffraction provenance demonstrated by the name/count extractor; conditional check NOT_RUN, issue pending (4). [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L96-L101] |
| G5 | Applicable | Q5.1: reconcile batch results, validation failures and recorded gate evidence before accepting the generated conversion report. | 0 unexplained input/output count differences; 0 unreported validation failures; 0 missing required gate records; counts per package. [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L181-L187] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L307-L364] (AQG r0, section 9.) | NOT_RUN for an engineering deliverable; current report contains conversion statistics, issue pending (4). |

Checks defined: G0 = 4; G1 = 1; G2 = 1; G3 = 1; G4 = 1 conditional; G5 = 1 (9 total). Checks Q0.4–Q5.1 are specified acceptance procedures, not shipped automated detectors.

## 6. Known failure modes and pitfalls

The following are text-inspection findings at the code pin; no native conversion, solver import, external download or physics calculation was performed.

| Finding | Symptom and cause | Detection check | Tracking |
| --- | --- | --- | --- |
| F01 | Import/API probe failures cause mock fallback; a real-mode request can return placeholder success. [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L27-L34] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L67-L92] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L163-L166] | no detection check exists | issue pending (1); related #1631 explicitly left these converters for follow-up. [docs/plans/2026-08-05-issue-1631-licensed-lane-fail-closed.md:L120-L120] [docs/plans/2026-08-05-issue-1631-licensed-lane-fail-closed.md:L513-L513] |
| F02 | Real batch conversion returns success after validation failure; lower-level conversion catches errors before the outer retry loop can retry. [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L137-L161] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L181-L191] | no detection check exists | issue pending (1) |
| F03 | Each `.dat`/`.yml` save error is logged but the API converter increments successful conversions after both attempts. [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L133-L165] | no detection check exists | issue pending (1) |
| F04 | Existing raw files, ZIPs and nonempty YAML outputs can be reused without full source/output integrity checks. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L215-L218] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L169-L174] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L174-L184] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L130-L134] | Q0.2 detects missing/corrupted manifest fixtures only; no reuse-path detection check exists | issue pending (1), issue pending (2) |
| F05 | Exhausted requests return None; category fetch errors return empty lists; an empty category set yields a zero/zero summary. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L108-L116] [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L128-L130] [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L173-L175] [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L270-L272] | Q0.1 covers exhaustion only; no category-error detection check exists | issue pending (2) |
| F06 | Integer-only or restricted size text parsing substitutes zero for unknown/unsupported forms; complete parsing uses a binary multiplier for displayed G units and an approximate-size reuse test. [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L123-L125] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L131-L138] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L174-L184] | no detection check exists | issue pending (2) |
| F07 | Archive flattening and basename-indexed manifests permit collisions; extraction exceptions return partial file lists after saving a manifest. [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L238-L284] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L241-L288] [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L50-L85] | no detection check exists | issue pending (2) |
| F08 | Bulk download accepts any successful ZIP as overall success; catalog exceptions only log; the complete downloader prints unconditional completion despite failed counts. [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L83-L109] [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L131-L131] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L522-L545] | no detection check exists | issue pending (2) |
| F09 | Analyzer errors return partially filled/default features; zero environmental fields and false analysis flags remain indistinguishable from measured absence; missing buoy type defaults to six degrees of freedom. [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L23-L74] [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L108-L134] | no detection check exists | issue pending (3) |
| F10 | Complete downloader stores basename keys/category fields while downloaded-example integration expects slash-separated names and absent size/checksum fields; metadata becomes unknown/zero. [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L258-L266] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L50-L78] | no detection check exists | issue pending (3) |
| F11 | Missing manifest returns no analyses; filename heuristics set statics true and infer counts/environment without opening a model. [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L41-L48] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L85-L106] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L108-L214] | no detection check exists | issue pending (3) |
| F12 | Derived practices precede populated common configurations; summary prose hard-codes collection counts; reintegration increments patterns while replacing stored examples. [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L314-L317] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L448-L450] [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L63-L79] [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L83-L121] | no detection check exists | issue pending (3) |
| F13 | Buoy counts are split by degree of freedom in the analyzer but the index looks for a single count; any truthy duration is also indexed as an active analysis. [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L42-L57] [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L243-L253] | no detection check exists | issue pending (3) |
| F14 | Bare sibling imports and a CLI test-mode import rely on script-directory layout; the guarded help tests do not execute test mode. [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L10-L10] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L13-L16] [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L152-L156] | no detection check exists for these paths; R11–R13 protect normal import/help only | #1900 |
| F15 | Mock-conversion script functions return Booleans without assertions and both converter scripts are excluded from recursive collection. [tests/solvers/orcaflex/examples_integration/test_converter.py:L15-L52] [tests/conftest.py:L46-L53] | no detection check exists | issue pending (4) |

### Silent-failure sweep ledger

sweep: 174 found

The count is matched source lines, including benign counters, conditions and comments; each is classified below rather than counted as a confirmed defect. Additional semantic findings F01–F15 cover unreturned failures, omitted data and manifest writes.

Exact command:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|\b1970\b|\bint\(|\bfloat\(|\b0(?:\.0)?\b|\b0\.05\b|[Uu]nit|[Cc]hecksum|[Cc]ache' src/digitalmodel/solvers/orcaflex/examples_integration --glob '*.py'
```

| Site | Classification and symptom | Detection / tracking |
| --- | --- | --- |
| [src/digitalmodel/solvers/orcaflex/examples_integration/__init__.py:L20-L20] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L35-L35] | Zero schema default can survive missing input or extraction failure (F09). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L39-L39] | Zero schema default can survive missing input or extraction failure (F09). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L43-L43] | Zero schema default can survive missing input or extraction failure (F09). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L44-L44] | Zero schema default can survive missing input or extraction failure (F09). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L57-L57] | Zero schema default can survive missing input or extraction failure (F09). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L62-L62] | Zero schema default can survive missing input or extraction failure (F09). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L63-L63] | Zero schema default can survive missing input or extraction failure (F09). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L67-L67] | Zero schema default can survive missing input or extraction failure (F09). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L71-L71] | Zero schema default can survive missing input or extraction failure (F09). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L131-L131] | Error leaves partially populated/default metadata (F09). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L54-L54] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L55-L55] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L56-L56] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L57-L57] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L58-L58] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L59-L59] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L60-L60] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L82-L82] | Error is converted to a flag, tuple, log or fallback; inspect caller acceptance (F01–F03). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L87-L87] | Error is converted to a flag, tuple, log or fallback; inspect caller acceptance (F01–F03). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L131-L131] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L146-L146] | Error is converted to a flag, tuple, log or fallback; inspect caller acceptance (F01–F03). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L189-L189] | Error is converted to a flag, tuple, log or fallback; inspect caller acceptance (F01–F03). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L205-L205] | Fixed mock physical field or SI declaration; no source model property is parsed (F01). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L207-L207] | Fixed mock physical field or SI declaration; no source model property is parsed (F01). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L211-L211] | Fixed mock physical field or SI declaration; no source model property is parsed (F01). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L212-L212] | Fixed mock physical field or SI declaration; no source model property is parsed (F01). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L213-L213] | Fixed mock physical field or SI declaration; no source model property is parsed (F01). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L214-L214] | Fixed mock physical field or SI declaration; no source model property is parsed (F01). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L229-L229] | Error is converted to a flag, tuple, log or fallback; inspect caller acceptance (F01–F03). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L251-L251] | Error is converted to a flag, tuple, log or fallback; inspect caller acceptance (F01–F03). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L280-L280] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L332-L332] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L51-L51] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L55-L55] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L56-L56] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L106-L106] | Error is logged or converted to a partial/empty result (F05–F08). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L108-L108] | Error returns no result rather than raising; callers can treat absence as empty input. | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L134-L134] | External numeric text conversion: restricted separators/units or missing content length becomes zero (F06). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L135-L135] | Fallback or display unit assumption can conceal absent metadata (F06). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L136-L136] | Fallback or display unit assumption can conceal absent metadata (F06). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L138-L138] | Fallback or display unit assumption can conceal absent metadata (F06). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L177-L177] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L194-L194] | External numeric text conversion: restricted separators/units or missing content length becomes zero (F06). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L197-L197] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L204-L204] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L206-L206] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L216-L216] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L225-L225] | Error is logged or converted to a partial/empty result (F05–F08). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L229-L229] | Error returns no result rather than raising; callers can treat absence as empty input. | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L248-L248] | Record is skipped; verify missing-file classification or skipped archive member. | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L284-L284] | Error is logged or converted to a partial/empty result (F05–F08). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L322-L322] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L352-L352] | External numeric text conversion: restricted separators/units or missing content length becomes zero (F06). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L361-L361] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L362-L362] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L363-L363] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L364-L364] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L385-L385] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L409-L409] | Fallback or display unit assumption can conceal absent metadata (F06). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L410-L410] | Fallback or display unit assumption can conceal absent metadata (F06). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L411-L411] | Fallback or display unit assumption can conceal absent metadata (F06). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L412-L412] | Fallback or display unit assumption can conceal absent metadata (F06). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L428-L428] | Fallback or display unit assumption can conceal absent metadata (F06). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L32-L32] | Error is converted to a flag, tuple, log or fallback; inspect caller acceptance (F01–F03). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L61-L61] | Error is converted to a flag, tuple, log or fallback; inspect caller acceptance (F01–F03). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L89-L89] | Error is converted to a flag, tuple, log or fallback; inspect caller acceptance (F01–F03). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L62-L62] | External numeric text conversion: restricted separators/units or missing content length becomes zero (F06). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L72-L72] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L73-L73] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L74-L74] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L99-L99] | Error is logged or converted to a partial/empty result (F05–F08). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L108-L108] | Error is logged or converted to a partial/empty result (F05–F08). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L131-L131] | Summary/exit success depends on incomplete counters rather than artifact validity (F03/F08). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L162-L162] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L48-L48] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L52-L52] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L53-L53] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L57-L57] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L108-L108] | Error is logged or converted to a partial/empty result (F05–F08). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L116-L116] | Error returns no result rather than raising; callers can treat absence as empty input. | Q0.1; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L130-L130] | Error returns no result rather than raising; callers can treat absence as empty input. | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L154-L154] | Category count defaults to zero and is displayed without recomputing examples (F05). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L175-L175] | Error returns no result rather than raising; callers can treat absence as empty input. | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L232-L232] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L233-L233] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L240-L240] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L249-L249] | Error is logged or converted to a partial/empty result (F05–F08). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L272-L272] | Summary/exit success depends on incomplete counters rather than artifact validity (F03/F08). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L278-L278] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L279-L279] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L298-L298] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L316-L316] | Record is skipped; verify missing-file classification or skipped archive member. | Q0.2 (missing-file fixture); issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L318-L318] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L320-L320] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L322-L322] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L326-L326] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L351-L351] | Category count defaults to zero and is displayed without recomputing examples (F05). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L52-L52] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L56-L56] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L57-L57] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L125-L125] | External numeric text conversion: restricted separators/units or missing content length becomes zero (F06). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L184-L184] | External numeric text conversion: restricted separators/units or missing content length becomes zero (F06). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L187-L187] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L193-L193] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L195-L195] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L203-L203] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L212-L212] | Error is logged or converted to a partial/empty result (F05–F08). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L216-L216] | Error returns no result rather than raising; callers can treat absence as empty input. | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L280-L280] | Error is logged or converted to a partial/empty result (F05–F08). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L300-L300] | Summary/exit success depends on incomplete counters rather than artifact validity (F03/F08). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L304-L304] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L305-L305] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L306-L306] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L393-L393] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L27-L27] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L34-L34] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L48-L48] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L56-L56] | Record is skipped; verify missing-file classification or skipped archive member. | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L84-L84] | Error is logged or converted to a partial/empty result (F05–F08). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L70-L70] | Absent source field becomes zero/blank or excludes the component from aggregation (F10–F11). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L71-L71] | Absent source field becomes zero/blank or excludes the component from aggregation (F10–F11). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L97-L97] | Absent source field becomes zero/blank or excludes the component from aggregation (F10–F11). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L111-L111] | Absent source field becomes zero/blank or excludes the component from aggregation (F10–F11). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L112-L112] | Absent source field becomes zero/blank or excludes the component from aggregation (F10–F11). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L113-L113] | Absent source field becomes zero/blank or excludes the component from aggregation (F10–F11). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L114-L114] | Absent source field becomes zero/blank or excludes the component from aggregation (F10–F11). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L115-L115] | Absent source field becomes zero/blank or excludes the component from aggregation (F10–F11). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L296-L296] | Absent source field becomes zero/blank or excludes the component from aggregation (F10–F11). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L333-L333] | Absent source field becomes zero/blank or excludes the component from aggregation (F10–F11). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L339-L339] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L346-L346] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L352-L352] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L371-L371] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L377-L377] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L383-L383] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L395-L395] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L401-L401] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L89-L89] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L92-L92] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L101-L101] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L113-L113] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L120-L120] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L134-L134] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L135-L135] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L136-L136] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L151-L151] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L152-L152] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L153-L153] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L245-L245] | Missing unified buoy count excludes buoy examples from the component index (F13). | no detection check exists; issue pending (3) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L43-L43] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L44-L44] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L45-L45] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L46-L46] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L47-L47] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L49-L49] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L81-L81] | Error is converted to a flag, tuple, log or fallback; inspect caller acceptance (F01–F03). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L87-L87] | Error is converted to a flag, tuple, log or fallback; inspect caller acceptance (F01–F03). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L92-L92] | Error is converted to a flag, tuple, log or fallback; inspect caller acceptance (F01–F03). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L139-L139] | Error is converted to a flag, tuple, log or fallback; inspect caller acceptance (F01–F03). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L148-L148] | Error is converted to a flag, tuple, log or fallback; inspect caller acceptance (F01–F03). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L157-L157] | Error is converted to a flag, tuple, log or fallback; inspect caller acceptance (F01–F03). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L220-L220] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L225-L225] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L296-L296] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L307-L307] | Summary/exit success depends on incomplete counters rather than artifact validity (F03/F08). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L14-L14] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L87-L87] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L128-L128] | Summary/exit success depends on incomplete counters rather than artifact validity (F03/F08). | no detection check exists; issue pending (1) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/scraper_analyzer.py:L23-L23] | Benign counter, condition, documentation or bookkeeping initialization; not an error result. | No defect asserted. |
| [src/digitalmodel/solvers/orcaflex/examples_integration/scraper_analyzer.py:L123-L123] | Error is logged or converted to a partial/empty result (F05–F08). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/scraper_analyzer.py:L125-L125] | Error returns no result rather than raising; callers can treat absence as empty input. | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/scraper_analyzer.py:L126-L126] | Error is logged or converted to a partial/empty result (F05–F08). | no detection check exists; issue pending (2) |
| [src/digitalmodel/solvers/orcaflex/examples_integration/scraper_analyzer.py:L128-L128] | Error returns no result rather than raising; callers can treat absence as empty input. | no detection check exists; issue pending (2) |

## 7. Lessons log

Append future entries with date, change reference, lesson, evidence, pack delta and shared-log entry; this draft records no prior clearance or shared-log entry.

| Date | Change reference | Lesson | Evidence | Pack delta | Shared-log entry |
| --- | --- | --- | --- | --- | --- |
| 2026-11-08 | Initial pack | File conversion and filename inference cannot substitute for physics validation. | [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L193-L227] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L170-L182] | Separate regression examples, proposed gates and unperformed checks. | Not assigned; issue pending (4) |

## 8. Ownership

Proposed review assignments: riser and mooring quality manager; deputy: riser and mooring deputy quality manager; producing discipline: engineering software documentation. The maintainer assigns named reviewers and the independent provider before clearance; unresolved criteria or classification questions go to the family quality manager. No clearance is recorded by this draft. (AQG r0, section 11.)

## 9. Dependencies and implementation inventory

### Runtime and file dependencies

Requests, BeautifulSoup and tqdm support retrieval; PyYAML reads or writes YAML, and JSON/path utilities support manifests and knowledge files. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L7-L19] [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L7-L11] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L8-L15]

The raw examples feed conversion, conversion YAML feeds extraction, and feature dictionaries feed the knowledge integrator; the downloaded-example variant uses the complete manifest and filename inference instead. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L228-L244] [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L48-L65] [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L88-L94] [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L47-L81] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L41-L78]

OrcFxAPI is a call-time dependency of the converter constructors and native load/save paths; no supported vendor version is pinned by those initializers. SHEAR7 appears as classification vocabulary, with no launch in that branch. [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L27-L34] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L67-L92] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L55-L95] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L170-L182] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L238-L239]

The following exhaustive text-occurrence index records every source line matching `(?i)OrcaFlex|OrcFxAPI|SHEAR7` in package Python files, including documentation, strings and import paths; these occurrences are dependencies/vocabulary, not executed checks.

| File | Occurrence citations |
| --- | --- |
| `__init__.py` | [src/digitalmodel/solvers/orcaflex/examples_integration/__init__.py:L2-L2] [src/digitalmodel/solvers/orcaflex/examples_integration/__init__.py:L4-L4] [src/digitalmodel/solvers/orcaflex/examples_integration/__init__.py:L8-L8] [src/digitalmodel/solvers/orcaflex/examples_integration/__init__.py:L14-L14] |
| `analyzer.py` | [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L4-L4] [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L17-L17] [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L29-L29] |
| `batch_converter.py` | [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L2-L2] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L20-L20] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L35-L35] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L45-L45] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L68-L68] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L70-L70] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L71-L71] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L73-L73] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L79-L79] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L83-L83] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L88-L88] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L94-L94] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L120-L120] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L143-L143] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L164-L164] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L198-L198] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L239-L239] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L244-L244] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L269-L269] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L312-L312] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L323-L323] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L368-L368] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L370-L370] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L371-L371] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L379-L379] |
| `complete_downloader.py` | [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L2-L2] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L29-L29] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L30-L30] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L35-L35] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L232-L232] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L241-L241] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L242-L242] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L245-L245] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L282-L282] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L307-L307] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L400-L400] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L411-L411] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L478-L478] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L481-L481] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L502-L502] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L524-L524] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L534-L534] [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L545-L545] |
| `converter.py` | [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L4-L4] [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L15-L15] [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L22-L22] [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L29-L29] [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L30-L30] [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L31-L31] [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L33-L33] [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L55-L55] |
| `download_all_examples.py` | [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L3-L3] [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L13-L13] [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L15-L15] [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L27-L27] [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L33-L33] [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L36-L36] [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L90-L90] [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L92-L92] [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L94-L94] [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L117-L117] [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L120-L120] [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L138-L138] |
| `downloader.py` | [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L2-L2] [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L26-L26] [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L27-L27] [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L29-L29] [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L48-L48] [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L265-L265] [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L342-L342] [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L401-L401] |
| `enhanced_downloader.py` | [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L2-L2] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L28-L28] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L29-L29] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L31-L31] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L220-L220] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L238-L238] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L239-L239] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L242-L242] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L243-L243] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L263-L263] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L293-L293] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L324-L324] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L336-L336] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L387-L387] |
| `fix_extraction.py` | [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L3-L3] [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L10-L10] [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L17-L17] [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L19-L19] [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L22-L22] [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L37-L37] [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L46-L46] [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L47-L47] [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L51-L51] [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L52-L52] [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L68-L68] [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L89-L89] [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L92-L92] |
| `integrate_downloaded_examples.py` | [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L3-L3] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L5-L5] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L24-L24] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L26-L26] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L31-L31] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L179-L179] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L181-L181] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L238-L238] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L239-L239] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L440-L440] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L448-L448] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L544-L544] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L554-L554] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L574-L574] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L579-L579] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L584-L584] |
| `integrator.py` | [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L4-L4] [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L17-L17] [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L19-L19] [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L24-L24] [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L180-L180] |
| `orcfxapi_converter.py` | [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L3-L3] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L5-L5] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L25-L25] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L26-L26] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L30-L30] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L52-L52] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L56-L56] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L58-L58] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L59-L59] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L63-L63] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L78-L78] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L80-L80] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L82-L82] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L88-L88] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L89-L89] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L93-L93] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L116-L116] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L175-L175] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L232-L232] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L254-L254] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L259-L259] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L265-L265] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L270-L270] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L272-L272] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L278-L278] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L280-L280] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L281-L281] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L283-L283] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L284-L284] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L285-L285] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L301-L301] |
| `run_full_conversion.py` | [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L3-L3] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L6-L6] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L16-L16] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L30-L30] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L35-L35] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L36-L36] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L42-L42] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L45-L45] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L47-L47] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L57-L57] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L59-L59] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L121-L121] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L122-L122] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L123-L123] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L124-L124] |
| `scraper_analyzer.py` | [src/digitalmodel/solvers/orcaflex/examples_integration/scraper_analyzer.py:L50-L50] |

### Map revision and counted families

Map reference: `origin/codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, declared data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; **all citations to implementation-map.yaml resolve at that map commit**, while every other repository citation resolves at the header code pin. [docs/registry/implementation-map.yaml:L1-L9]

| Rank | Map family | In-package rows | All inventory rows | Counted |
| --- | --- | --- | --- | --- |
| 1 | `orcaflex-text-model-writers` | 11 | 440 | Yes. [docs/registry/implementation-map.yaml:L4933-L4935] |
| 2 | `wind-and-current-force-routes` | 1 | 135 | Yes. [docs/registry/implementation-map.yaml:L15393-L15395] |

Both families are counted because fewer than three have implementation rows in this package; no nested package is excluded. The inventory below retains the map classification even where its semantic label is too broad.

Each inventory ID is the family identifier plus its one-based implementation ordinal in that map revision; each cited row supplies its path, symbol and relation. External names are not reproduced; the row identifiers provide the complete inventory without republishing identifying discovery paths.

| Family / implementation ordinal | Scope | Map row (path, symbol, relation) |
| --- | --- | --- |
| `orcaflex-text-model-writers` / 1 | Outside package | [docs/registry/implementation-map.yaml:L4940-L4949] |
| `orcaflex-text-model-writers` / 2 | Outside package | [docs/registry/implementation-map.yaml:L4950-L4958] |
| `orcaflex-text-model-writers` / 3 | Outside package | [docs/registry/implementation-map.yaml:L4959-L4968] |
| `orcaflex-text-model-writers` / 4 | Outside package | [docs/registry/implementation-map.yaml:L4969-L4978] |
| `orcaflex-text-model-writers` / 5 | Outside package | [docs/registry/implementation-map.yaml:L4979-L4988] |
| `orcaflex-text-model-writers` / 6 | Outside package | [docs/registry/implementation-map.yaml:L4989-L4998] |
| `orcaflex-text-model-writers` / 7 | Outside package | [docs/registry/implementation-map.yaml:L4999-L5008] |
| `orcaflex-text-model-writers` / 8 | Outside package | [docs/registry/implementation-map.yaml:L5009-L5020] |
| `orcaflex-text-model-writers` / 9 | Outside package | [docs/registry/implementation-map.yaml:L5021-L5030] |
| `orcaflex-text-model-writers` / 10 | Outside package | [docs/registry/implementation-map.yaml:L5031-L5040] |
| `orcaflex-text-model-writers` / 11 | Outside package | [docs/registry/implementation-map.yaml:L5041-L5050] |
| `orcaflex-text-model-writers` / 12 | Outside package | [docs/registry/implementation-map.yaml:L5051-L5060] |
| `orcaflex-text-model-writers` / 13 | Outside package | [docs/registry/implementation-map.yaml:L5061-L5070] |
| `orcaflex-text-model-writers` / 14 | Outside package | [docs/registry/implementation-map.yaml:L5071-L5080] |
| `orcaflex-text-model-writers` / 15 | Outside package | [docs/registry/implementation-map.yaml:L5081-L5090] |
| `orcaflex-text-model-writers` / 16 | Outside package | [docs/registry/implementation-map.yaml:L5091-L5100] |
| `orcaflex-text-model-writers` / 17 | Outside package | [docs/registry/implementation-map.yaml:L5101-L5110] |
| `orcaflex-text-model-writers` / 18 | Outside package | [docs/registry/implementation-map.yaml:L5111-L5120] |
| `orcaflex-text-model-writers` / 19 | Outside package | [docs/registry/implementation-map.yaml:L5121-L5132] |
| `orcaflex-text-model-writers` / 20 | Outside package | [docs/registry/implementation-map.yaml:L5133-L5142] |
| `orcaflex-text-model-writers` / 21 | Outside package | [docs/registry/implementation-map.yaml:L5143-L5151] |
| `orcaflex-text-model-writers` / 22 | Outside package | [docs/registry/implementation-map.yaml:L5152-L5161] |
| `orcaflex-text-model-writers` / 23 | Outside package | [docs/registry/implementation-map.yaml:L5162-L5171] |
| `orcaflex-text-model-writers` / 24 | Outside package | [docs/registry/implementation-map.yaml:L5172-L5181] |
| `orcaflex-text-model-writers` / 25 | Outside package | [docs/registry/implementation-map.yaml:L5182-L5191] |
| `orcaflex-text-model-writers` / 26 | Outside package | [docs/registry/implementation-map.yaml:L5192-L5201] |
| `orcaflex-text-model-writers` / 27 | Outside package | [docs/registry/implementation-map.yaml:L5202-L5211] |
| `orcaflex-text-model-writers` / 28 | Outside package | [docs/registry/implementation-map.yaml:L5212-L5223] |
| `orcaflex-text-model-writers` / 29 | Outside package | [docs/registry/implementation-map.yaml:L5224-L5233] |
| `orcaflex-text-model-writers` / 30 | Outside package | [docs/registry/implementation-map.yaml:L5234-L5243] |
| `orcaflex-text-model-writers` / 31 | Outside package | [docs/registry/implementation-map.yaml:L5244-L5253] |
| `orcaflex-text-model-writers` / 32 | Outside package | [docs/registry/implementation-map.yaml:L5254-L5263] |
| `orcaflex-text-model-writers` / 33 | Outside package | [docs/registry/implementation-map.yaml:L5264-L5273] |
| `orcaflex-text-model-writers` / 34 | Outside package | [docs/registry/implementation-map.yaml:L5274-L5283] |
| `orcaflex-text-model-writers` / 35 | Outside package | [docs/registry/implementation-map.yaml:L5284-L5293] |
| `orcaflex-text-model-writers` / 36 | Outside package | [docs/registry/implementation-map.yaml:L5294-L5303] |
| `orcaflex-text-model-writers` / 37 | Outside package | [docs/registry/implementation-map.yaml:L5304-L5313] |
| `orcaflex-text-model-writers` / 38 | Outside package | [docs/registry/implementation-map.yaml:L5314-L5323] |
| `orcaflex-text-model-writers` / 39 | Outside package | [docs/registry/implementation-map.yaml:L5324-L5333] |
| `orcaflex-text-model-writers` / 40 | Outside package | [docs/registry/implementation-map.yaml:L5334-L5343] |
| `orcaflex-text-model-writers` / 41 | Outside package | [docs/registry/implementation-map.yaml:L5344-L5353] |
| `orcaflex-text-model-writers` / 42 | Outside package | [docs/registry/implementation-map.yaml:L5354-L5363] |
| `orcaflex-text-model-writers` / 43 | Outside package | [docs/registry/implementation-map.yaml:L5364-L5372] |
| `orcaflex-text-model-writers` / 44 | Outside package | [docs/registry/implementation-map.yaml:L5373-L5382] |
| `orcaflex-text-model-writers` / 45 | Outside package | [docs/registry/implementation-map.yaml:L5383-L5392] |
| `orcaflex-text-model-writers` / 46 | Outside package | [docs/registry/implementation-map.yaml:L5393-L5402] |
| `orcaflex-text-model-writers` / 47 | Outside package | [docs/registry/implementation-map.yaml:L5403-L5415] |
| `orcaflex-text-model-writers` / 48 | Outside package | [docs/registry/implementation-map.yaml:L5416-L5425] |
| `orcaflex-text-model-writers` / 49 | Outside package | [docs/registry/implementation-map.yaml:L5426-L5437] |
| `orcaflex-text-model-writers` / 50 | Outside package | [docs/registry/implementation-map.yaml:L5438-L5447] |
| `orcaflex-text-model-writers` / 51 | Outside package | [docs/registry/implementation-map.yaml:L5448-L5457] |
| `orcaflex-text-model-writers` / 52 | Outside package | [docs/registry/implementation-map.yaml:L5458-L5467] |
| `orcaflex-text-model-writers` / 53 | Outside package | [docs/registry/implementation-map.yaml:L5468-L5477] |
| `orcaflex-text-model-writers` / 54 | Outside package | [docs/registry/implementation-map.yaml:L5478-L5487] |
| `orcaflex-text-model-writers` / 55 | Outside package | [docs/registry/implementation-map.yaml:L5488-L5497] |
| `orcaflex-text-model-writers` / 56 | Outside package | [docs/registry/implementation-map.yaml:L5498-L5507] |
| `orcaflex-text-model-writers` / 57 | Outside package | [docs/registry/implementation-map.yaml:L5508-L5519] |
| `orcaflex-text-model-writers` / 58 | Outside package | [docs/registry/implementation-map.yaml:L5520-L5531] |
| `orcaflex-text-model-writers` / 59 | Outside package | [docs/registry/implementation-map.yaml:L5532-L5540] |
| `orcaflex-text-model-writers` / 60 | Outside package | [docs/registry/implementation-map.yaml:L5541-L5550] |
| `orcaflex-text-model-writers` / 61 | Outside package | [docs/registry/implementation-map.yaml:L5551-L5560] |
| `orcaflex-text-model-writers` / 62 | Outside package | [docs/registry/implementation-map.yaml:L5561-L5570] |
| `orcaflex-text-model-writers` / 63 | Outside package | [docs/registry/implementation-map.yaml:L5571-L5580] |
| `orcaflex-text-model-writers` / 64 | Outside package | [docs/registry/implementation-map.yaml:L5581-L5593] |
| `orcaflex-text-model-writers` / 65 | Outside package | [docs/registry/implementation-map.yaml:L5594-L5603] |
| `orcaflex-text-model-writers` / 66 | Outside package | [docs/registry/implementation-map.yaml:L5604-L5615] |
| `orcaflex-text-model-writers` / 67 | Outside package | [docs/registry/implementation-map.yaml:L5616-L5628] |
| `orcaflex-text-model-writers` / 68 | Outside package | [docs/registry/implementation-map.yaml:L5629-L5638] |
| `orcaflex-text-model-writers` / 69 | Outside package | [docs/registry/implementation-map.yaml:L5639-L5648] |
| `orcaflex-text-model-writers` / 70 | Outside package | [docs/registry/implementation-map.yaml:L5649-L5658] |
| `orcaflex-text-model-writers` / 71 | Outside package | [docs/registry/implementation-map.yaml:L5659-L5667] |
| `orcaflex-text-model-writers` / 72 | Outside package | [docs/registry/implementation-map.yaml:L5668-L5677] |
| `orcaflex-text-model-writers` / 73 | Outside package | [docs/registry/implementation-map.yaml:L5678-L5687] |
| `orcaflex-text-model-writers` / 74 | Outside package | [docs/registry/implementation-map.yaml:L5688-L5697] |
| `orcaflex-text-model-writers` / 75 | Outside package | [docs/registry/implementation-map.yaml:L5698-L5707] |
| `orcaflex-text-model-writers` / 76 | Outside package | [docs/registry/implementation-map.yaml:L5708-L5717] |
| `orcaflex-text-model-writers` / 77 | Outside package | [docs/registry/implementation-map.yaml:L5718-L5727] |
| `orcaflex-text-model-writers` / 78 | Outside package | [docs/registry/implementation-map.yaml:L5728-L5737] |
| `orcaflex-text-model-writers` / 79 | Outside package | [docs/registry/implementation-map.yaml:L5738-L5749] |
| `orcaflex-text-model-writers` / 80 | Outside package | [docs/registry/implementation-map.yaml:L5750-L5759] |
| `orcaflex-text-model-writers` / 81 | Outside package | [docs/registry/implementation-map.yaml:L5760-L5769] |
| `orcaflex-text-model-writers` / 82 | Outside package | [docs/registry/implementation-map.yaml:L5770-L5781] |
| `orcaflex-text-model-writers` / 83 | Outside package | [docs/registry/implementation-map.yaml:L5782-L5791] |
| `orcaflex-text-model-writers` / 84 | Outside package | [docs/registry/implementation-map.yaml:L5792-L5801] |
| `orcaflex-text-model-writers` / 85 | Outside package | [docs/registry/implementation-map.yaml:L5802-L5811] |
| `orcaflex-text-model-writers` / 86 | Outside package | [docs/registry/implementation-map.yaml:L5812-L5823] |
| `orcaflex-text-model-writers` / 87 | Outside package | [docs/registry/implementation-map.yaml:L5824-L5833] |
| `orcaflex-text-model-writers` / 88 | Outside package | [docs/registry/implementation-map.yaml:L5834-L5843] |
| `orcaflex-text-model-writers` / 89 | Outside package | [docs/registry/implementation-map.yaml:L5844-L5853] |
| `orcaflex-text-model-writers` / 90 | Outside package | [docs/registry/implementation-map.yaml:L5854-L5863] |
| `orcaflex-text-model-writers` / 91 | Outside package | [docs/registry/implementation-map.yaml:L5864-L5872] |
| `orcaflex-text-model-writers` / 92 | Outside package | [docs/registry/implementation-map.yaml:L5873-L5882] |
| `orcaflex-text-model-writers` / 93 | Outside package | [docs/registry/implementation-map.yaml:L5883-L5891] |
| `orcaflex-text-model-writers` / 94 | Outside package | [docs/registry/implementation-map.yaml:L5892-L5901] |
| `orcaflex-text-model-writers` / 95 | Outside package | [docs/registry/implementation-map.yaml:L5902-L5911] |
| `orcaflex-text-model-writers` / 96 | Outside package | [docs/registry/implementation-map.yaml:L5912-L5921] |
| `orcaflex-text-model-writers` / 97 | Outside package | [docs/registry/implementation-map.yaml:L5922-L5933] |
| `orcaflex-text-model-writers` / 98 | Outside package | [docs/registry/implementation-map.yaml:L5934-L5943] |
| `orcaflex-text-model-writers` / 99 | Outside package | [docs/registry/implementation-map.yaml:L5944-L5953] |
| `orcaflex-text-model-writers` / 100 | Outside package | [docs/registry/implementation-map.yaml:L5954-L5965] |
| `orcaflex-text-model-writers` / 101 | Outside package | [docs/registry/implementation-map.yaml:L5966-L5975] |
| `orcaflex-text-model-writers` / 102 | Outside package | [docs/registry/implementation-map.yaml:L5976-L5985] |
| `orcaflex-text-model-writers` / 103 | Outside package | [docs/registry/implementation-map.yaml:L5986-L5994] |
| `orcaflex-text-model-writers` / 104 | Outside package | [docs/registry/implementation-map.yaml:L5995-L6004] |
| `orcaflex-text-model-writers` / 105 | Outside package | [docs/registry/implementation-map.yaml:L6005-L6014] |
| `orcaflex-text-model-writers` / 106 | Outside package | [docs/registry/implementation-map.yaml:L6015-L6024] |
| `orcaflex-text-model-writers` / 107 | Outside package | [docs/registry/implementation-map.yaml:L6025-L6034] |
| `orcaflex-text-model-writers` / 108 | Outside package | [docs/registry/implementation-map.yaml:L6035-L6044] |
| `orcaflex-text-model-writers` / 109 | Outside package | [docs/registry/implementation-map.yaml:L6045-L6054] |
| `orcaflex-text-model-writers` / 110 | Outside package | [docs/registry/implementation-map.yaml:L6055-L6064] |
| `orcaflex-text-model-writers` / 111 | Outside package | [docs/registry/implementation-map.yaml:L6065-L6074] |
| `orcaflex-text-model-writers` / 112 | Outside package | [docs/registry/implementation-map.yaml:L6075-L6084] |
| `orcaflex-text-model-writers` / 113 | Outside package | [docs/registry/implementation-map.yaml:L6085-L6096] |
| `orcaflex-text-model-writers` / 114 | Outside package | [docs/registry/implementation-map.yaml:L6097-L6109] |
| `orcaflex-text-model-writers` / 115 | Outside package | [docs/registry/implementation-map.yaml:L6110-L6119] |
| `orcaflex-text-model-writers` / 116 | Outside package | [docs/registry/implementation-map.yaml:L6120-L6129] |
| `orcaflex-text-model-writers` / 117 | Outside package | [docs/registry/implementation-map.yaml:L6130-L6141] |
| `orcaflex-text-model-writers` / 118 | Outside package | [docs/registry/implementation-map.yaml:L6142-L6153] |
| `orcaflex-text-model-writers` / 119 | Outside package | [docs/registry/implementation-map.yaml:L6154-L6163] |
| `orcaflex-text-model-writers` / 120 | Outside package | [docs/registry/implementation-map.yaml:L6164-L6173] |
| `orcaflex-text-model-writers` / 121 | Outside package | [docs/registry/implementation-map.yaml:L6174-L6183] |
| `orcaflex-text-model-writers` / 122 | Outside package | [docs/registry/implementation-map.yaml:L6184-L6193] |
| `orcaflex-text-model-writers` / 123 | Outside package | [docs/registry/implementation-map.yaml:L6194-L6203] |
| `orcaflex-text-model-writers` / 124 | Outside package | [docs/registry/implementation-map.yaml:L6204-L6213] |
| `orcaflex-text-model-writers` / 125 | Outside package | [docs/registry/implementation-map.yaml:L6214-L6223] |
| `orcaflex-text-model-writers` / 126 | Outside package | [docs/registry/implementation-map.yaml:L6224-L6233] |
| `orcaflex-text-model-writers` / 127 | Outside package | [docs/registry/implementation-map.yaml:L6234-L6243] |
| `orcaflex-text-model-writers` / 128 | Outside package | [docs/registry/implementation-map.yaml:L6244-L6253] |
| `orcaflex-text-model-writers` / 129 | Outside package | [docs/registry/implementation-map.yaml:L6254-L6263] |
| `orcaflex-text-model-writers` / 130 | Outside package | [docs/registry/implementation-map.yaml:L6264-L6273] |
| `orcaflex-text-model-writers` / 131 | Outside package | [docs/registry/implementation-map.yaml:L6274-L6283] |
| `orcaflex-text-model-writers` / 132 | Outside package | [docs/registry/implementation-map.yaml:L6284-L6293] |
| `orcaflex-text-model-writers` / 133 | Outside package | [docs/registry/implementation-map.yaml:L6294-L6303] |
| `orcaflex-text-model-writers` / 134 | Outside package | [docs/registry/implementation-map.yaml:L6304-L6313] |
| `orcaflex-text-model-writers` / 135 | Outside package | [docs/registry/implementation-map.yaml:L6314-L6323] |
| `orcaflex-text-model-writers` / 136 | Outside package | [docs/registry/implementation-map.yaml:L6324-L6333] |
| `orcaflex-text-model-writers` / 137 | Outside package | [docs/registry/implementation-map.yaml:L6334-L6343] |
| `orcaflex-text-model-writers` / 138 | Outside package | [docs/registry/implementation-map.yaml:L6344-L6353] |
| `orcaflex-text-model-writers` / 139 | Outside package | [docs/registry/implementation-map.yaml:L6354-L6363] |
| `orcaflex-text-model-writers` / 140 | Outside package | [docs/registry/implementation-map.yaml:L6364-L6373] |
| `orcaflex-text-model-writers` / 141 | Outside package | [docs/registry/implementation-map.yaml:L6374-L6383] |
| `orcaflex-text-model-writers` / 142 | Outside package | [docs/registry/implementation-map.yaml:L6384-L6393] |
| `orcaflex-text-model-writers` / 143 | Outside package | [docs/registry/implementation-map.yaml:L6394-L6403] |
| `orcaflex-text-model-writers` / 144 | Outside package | [docs/registry/implementation-map.yaml:L6404-L6413] |
| `orcaflex-text-model-writers` / 145 | Outside package | [docs/registry/implementation-map.yaml:L6414-L6423] |
| `orcaflex-text-model-writers` / 146 | Outside package | [docs/registry/implementation-map.yaml:L6424-L6436] |
| `orcaflex-text-model-writers` / 147 | Outside package | [docs/registry/implementation-map.yaml:L6437-L6448] |
| `orcaflex-text-model-writers` / 148 | Outside package | [docs/registry/implementation-map.yaml:L6449-L6458] |
| `orcaflex-text-model-writers` / 149 | Outside package | [docs/registry/implementation-map.yaml:L6459-L6468] |
| `orcaflex-text-model-writers` / 150 | Outside package | [docs/registry/implementation-map.yaml:L6469-L6478] |
| `orcaflex-text-model-writers` / 151 | Outside package | [docs/registry/implementation-map.yaml:L6479-L6490] |
| `orcaflex-text-model-writers` / 152 | Outside package | [docs/registry/implementation-map.yaml:L6491-L6500] |
| `orcaflex-text-model-writers` / 153 | Outside package | [docs/registry/implementation-map.yaml:L6501-L6510] |
| `orcaflex-text-model-writers` / 154 | Outside package | [docs/registry/implementation-map.yaml:L6511-L6520] |
| `orcaflex-text-model-writers` / 155 | Outside package | [docs/registry/implementation-map.yaml:L6521-L6530] |
| `orcaflex-text-model-writers` / 156 | Outside package | [docs/registry/implementation-map.yaml:L6531-L6540] |
| `orcaflex-text-model-writers` / 157 | Outside package | [docs/registry/implementation-map.yaml:L6541-L6550] |
| `orcaflex-text-model-writers` / 158 | Outside package | [docs/registry/implementation-map.yaml:L6551-L6560] |
| `orcaflex-text-model-writers` / 159 | Outside package | [docs/registry/implementation-map.yaml:L6561-L6570] |
| `orcaflex-text-model-writers` / 160 | Outside package | [docs/registry/implementation-map.yaml:L6571-L6580] |
| `orcaflex-text-model-writers` / 161 | Outside package | [docs/registry/implementation-map.yaml:L6581-L6590] |
| `orcaflex-text-model-writers` / 162 | Outside package | [docs/registry/implementation-map.yaml:L6591-L6600] |
| `orcaflex-text-model-writers` / 163 | Outside package | [docs/registry/implementation-map.yaml:L6601-L6610] |
| `orcaflex-text-model-writers` / 164 | Outside package | [docs/registry/implementation-map.yaml:L6611-L6620] |
| `orcaflex-text-model-writers` / 165 | Outside package | [docs/registry/implementation-map.yaml:L6621-L6630] |
| `orcaflex-text-model-writers` / 166 | Outside package | [docs/registry/implementation-map.yaml:L6631-L6640] |
| `orcaflex-text-model-writers` / 167 | Outside package | [docs/registry/implementation-map.yaml:L6641-L6650] |
| `orcaflex-text-model-writers` / 168 | Outside package | [docs/registry/implementation-map.yaml:L6651-L6659] |
| `orcaflex-text-model-writers` / 169 | Outside package | [docs/registry/implementation-map.yaml:L6660-L6672] |
| `orcaflex-text-model-writers` / 170 | Outside package | [docs/registry/implementation-map.yaml:L6673-L6682] |
| `orcaflex-text-model-writers` / 171 | Outside package | [docs/registry/implementation-map.yaml:L6683-L6692] |
| `orcaflex-text-model-writers` / 172 | Outside package | [docs/registry/implementation-map.yaml:L6693-L6704] |
| `orcaflex-text-model-writers` / 173 | Outside package | [docs/registry/implementation-map.yaml:L6705-L6714] |
| `orcaflex-text-model-writers` / 174 | Outside package | [docs/registry/implementation-map.yaml:L6715-L6724] |
| `orcaflex-text-model-writers` / 175 | Outside package | [docs/registry/implementation-map.yaml:L6725-L6734] |
| `orcaflex-text-model-writers` / 176 | Outside package | [docs/registry/implementation-map.yaml:L6735-L6744] |
| `orcaflex-text-model-writers` / 177 | Outside package | [docs/registry/implementation-map.yaml:L6745-L6754] |
| `orcaflex-text-model-writers` / 178 | Outside package | [docs/registry/implementation-map.yaml:L6755-L6764] |
| `orcaflex-text-model-writers` / 179 | Outside package | [docs/registry/implementation-map.yaml:L6765-L6774] |
| `orcaflex-text-model-writers` / 180 | Outside package | [docs/registry/implementation-map.yaml:L6775-L6784] |
| `orcaflex-text-model-writers` / 181 | Outside package | [docs/registry/implementation-map.yaml:L6785-L6794] |
| `orcaflex-text-model-writers` / 182 | Outside package | [docs/registry/implementation-map.yaml:L6795-L6804] |
| `orcaflex-text-model-writers` / 183 | Outside package | [docs/registry/implementation-map.yaml:L6805-L6814] |
| `orcaflex-text-model-writers` / 184 | Outside package | [docs/registry/implementation-map.yaml:L6815-L6824] |
| `orcaflex-text-model-writers` / 185 | Outside package | [docs/registry/implementation-map.yaml:L6825-L6834] |
| `orcaflex-text-model-writers` / 186 | Outside package | [docs/registry/implementation-map.yaml:L6835-L6844] |
| `orcaflex-text-model-writers` / 187 | Outside package | [docs/registry/implementation-map.yaml:L6845-L6854] |
| `orcaflex-text-model-writers` / 188 | Outside package | [docs/registry/implementation-map.yaml:L6855-L6864] |
| `orcaflex-text-model-writers` / 189 | Outside package | [docs/registry/implementation-map.yaml:L6865-L6874] |
| `orcaflex-text-model-writers` / 190 | Outside package | [docs/registry/implementation-map.yaml:L6875-L6884] |
| `orcaflex-text-model-writers` / 191 | Outside package | [docs/registry/implementation-map.yaml:L6885-L6894] |
| `orcaflex-text-model-writers` / 192 | In package: `analyzer.py`, `FeatureAnalyzer.analyze_file` | [docs/registry/implementation-map.yaml:L6895-L6904] |
| `orcaflex-text-model-writers` / 193 | In package: `analyzer.py`, `FeatureAnalyzer.generate_summary` | [docs/registry/implementation-map.yaml:L6905-L6914] |
| `orcaflex-text-model-writers` / 194 | In package: `batch_converter.py`, `OrcaFlexBatchConverter._real_convert` | [docs/registry/implementation-map.yaml:L6915-L6927] |
| `orcaflex-text-model-writers` / 195 | In package: `batch_converter.py`, `OrcaFlexBatchConverter._mock_convert` | [docs/registry/implementation-map.yaml:L6928-L6937] |
| `orcaflex-text-model-writers` / 196 | In package: `converter.py`, `YamlConverter.convert_batch` | [docs/registry/implementation-map.yaml:L6938-L6947] |
| `orcaflex-text-model-writers` / 197 | In package: `integrate_downloaded_examples.py`, `DownloadedExamplesIntegrator._infer_components` | [docs/registry/implementation-map.yaml:L6948-L6957] |
| `orcaflex-text-model-writers` / 198 | In package: `integrate_downloaded_examples.py`, `DownloadedExamplesIntegrator._extract_common_configs` | [docs/registry/implementation-map.yaml:L6958-L6967] |
| `orcaflex-text-model-writers` / 199 | In package: `integrator.py`, `KnowledgeIntegrator._extract_patterns` | [docs/registry/implementation-map.yaml:L6968-L6977] |
| `orcaflex-text-model-writers` / 200 | In package: `integrator.py`, `KnowledgeIntegrator.build_searchable_index` | [docs/registry/implementation-map.yaml:L6978-L6987] |
| `orcaflex-text-model-writers` / 201 | In package: `orcfxapi_converter.py`, `OrcFxAPIConverter._initialize_api` | [docs/registry/implementation-map.yaml:L6988-L6997] |
| `orcaflex-text-model-writers` / 202 | In package: `orcfxapi_converter.py`, `OrcFxAPIConverter.convert_file` | [docs/registry/implementation-map.yaml:L6998-L7007] |
| `orcaflex-text-model-writers` / 203 | Outside package | [docs/registry/implementation-map.yaml:L7008-L7017] |
| `orcaflex-text-model-writers` / 204 | Outside package | [docs/registry/implementation-map.yaml:L7018-L7031] |
| `orcaflex-text-model-writers` / 205 | Outside package | [docs/registry/implementation-map.yaml:L7032-L7041] |
| `orcaflex-text-model-writers` / 206 | Outside package | [docs/registry/implementation-map.yaml:L7042-L7051] |
| `orcaflex-text-model-writers` / 207 | Outside package | [docs/registry/implementation-map.yaml:L7052-L7061] |
| `orcaflex-text-model-writers` / 208 | Outside package | [docs/registry/implementation-map.yaml:L7062-L7071] |
| `orcaflex-text-model-writers` / 209 | Outside package | [docs/registry/implementation-map.yaml:L7072-L7081] |
| `orcaflex-text-model-writers` / 210 | Outside package | [docs/registry/implementation-map.yaml:L7082-L7091] |
| `orcaflex-text-model-writers` / 211 | Outside package | [docs/registry/implementation-map.yaml:L7092-L7101] |
| `orcaflex-text-model-writers` / 212 | Outside package | [docs/registry/implementation-map.yaml:L7102-L7111] |
| `orcaflex-text-model-writers` / 213 | Outside package | [docs/registry/implementation-map.yaml:L7112-L7121] |
| `orcaflex-text-model-writers` / 214 | Outside package | [docs/registry/implementation-map.yaml:L7122-L7131] |
| `orcaflex-text-model-writers` / 215 | Outside package | [docs/registry/implementation-map.yaml:L7132-L7141] |
| `orcaflex-text-model-writers` / 216 | Outside package | [docs/registry/implementation-map.yaml:L7142-L7153] |
| `orcaflex-text-model-writers` / 217 | Outside package | [docs/registry/implementation-map.yaml:L7154-L7163] |
| `orcaflex-text-model-writers` / 218 | Outside package | [docs/registry/implementation-map.yaml:L7164-L7176] |
| `orcaflex-text-model-writers` / 219 | Outside package | [docs/registry/implementation-map.yaml:L7177-L7186] |
| `orcaflex-text-model-writers` / 220 | Outside package | [docs/registry/implementation-map.yaml:L7187-L7196] |
| `orcaflex-text-model-writers` / 221 | Outside package | [docs/registry/implementation-map.yaml:L7197-L7207] |
| `orcaflex-text-model-writers` / 222 | Outside package | [docs/registry/implementation-map.yaml:L7208-L7217] |
| `orcaflex-text-model-writers` / 223 | Outside package | [docs/registry/implementation-map.yaml:L7218-L7227] |
| `orcaflex-text-model-writers` / 224 | Outside package | [docs/registry/implementation-map.yaml:L7228-L7237] |
| `orcaflex-text-model-writers` / 225 | Outside package | [docs/registry/implementation-map.yaml:L7238-L7247] |
| `orcaflex-text-model-writers` / 226 | Outside package | [docs/registry/implementation-map.yaml:L7248-L7257] |
| `orcaflex-text-model-writers` / 227 | Outside package | [docs/registry/implementation-map.yaml:L7258-L7269] |
| `orcaflex-text-model-writers` / 228 | Outside package | [docs/registry/implementation-map.yaml:L7270-L7279] |
| `orcaflex-text-model-writers` / 229 | Outside package | [docs/registry/implementation-map.yaml:L7280-L7289] |
| `orcaflex-text-model-writers` / 230 | Outside package | [docs/registry/implementation-map.yaml:L7290-L7299] |
| `orcaflex-text-model-writers` / 231 | Outside package | [docs/registry/implementation-map.yaml:L7300-L7309] |
| `orcaflex-text-model-writers` / 232 | Outside package | [docs/registry/implementation-map.yaml:L7310-L7319] |
| `orcaflex-text-model-writers` / 233 | Outside package | [docs/registry/implementation-map.yaml:L7320-L7329] |
| `orcaflex-text-model-writers` / 234 | Outside package | [docs/registry/implementation-map.yaml:L7330-L7339] |
| `orcaflex-text-model-writers` / 235 | Outside package | [docs/registry/implementation-map.yaml:L7340-L7349] |
| `orcaflex-text-model-writers` / 236 | Outside package | [docs/registry/implementation-map.yaml:L7350-L7359] |
| `orcaflex-text-model-writers` / 237 | Outside package | [docs/registry/implementation-map.yaml:L7360-L7369] |
| `orcaflex-text-model-writers` / 238 | Outside package | [docs/registry/implementation-map.yaml:L7370-L7379] |
| `orcaflex-text-model-writers` / 239 | Outside package | [docs/registry/implementation-map.yaml:L7380-L7389] |
| `orcaflex-text-model-writers` / 240 | Outside package | [docs/registry/implementation-map.yaml:L7390-L7399] |
| `orcaflex-text-model-writers` / 241 | Outside package | [docs/registry/implementation-map.yaml:L7400-L7409] |
| `orcaflex-text-model-writers` / 242 | Outside package | [docs/registry/implementation-map.yaml:L7410-L7419] |
| `orcaflex-text-model-writers` / 243 | Outside package | [docs/registry/implementation-map.yaml:L7420-L7429] |
| `orcaflex-text-model-writers` / 244 | Outside package | [docs/registry/implementation-map.yaml:L7430-L7439] |
| `orcaflex-text-model-writers` / 245 | Outside package | [docs/registry/implementation-map.yaml:L7440-L7449] |
| `orcaflex-text-model-writers` / 246 | Outside package | [docs/registry/implementation-map.yaml:L7450-L7459] |
| `orcaflex-text-model-writers` / 247 | Outside package | [docs/registry/implementation-map.yaml:L7460-L7469] |
| `orcaflex-text-model-writers` / 248 | Outside package | [docs/registry/implementation-map.yaml:L7470-L7479] |
| `orcaflex-text-model-writers` / 249 | Outside package | [docs/registry/implementation-map.yaml:L7480-L7489] |
| `orcaflex-text-model-writers` / 250 | Outside package | [docs/registry/implementation-map.yaml:L7490-L7499] |
| `orcaflex-text-model-writers` / 251 | Outside package | [docs/registry/implementation-map.yaml:L7500-L7509] |
| `orcaflex-text-model-writers` / 252 | Outside package | [docs/registry/implementation-map.yaml:L7510-L7519] |
| `orcaflex-text-model-writers` / 253 | Outside package | [docs/registry/implementation-map.yaml:L7520-L7529] |
| `orcaflex-text-model-writers` / 254 | Outside package | [docs/registry/implementation-map.yaml:L7530-L7539] |
| `orcaflex-text-model-writers` / 255 | Outside package | [docs/registry/implementation-map.yaml:L7540-L7551] |
| `orcaflex-text-model-writers` / 256 | Outside package | [docs/registry/implementation-map.yaml:L7552-L7561] |
| `orcaflex-text-model-writers` / 257 | Outside package | [docs/registry/implementation-map.yaml:L7562-L7571] |
| `orcaflex-text-model-writers` / 258 | Outside package | [docs/registry/implementation-map.yaml:L7572-L7581] |
| `orcaflex-text-model-writers` / 259 | Outside package | [docs/registry/implementation-map.yaml:L7582-L7591] |
| `orcaflex-text-model-writers` / 260 | Outside package | [docs/registry/implementation-map.yaml:L7592-L7603] |
| `orcaflex-text-model-writers` / 261 | Outside package | [docs/registry/implementation-map.yaml:L7604-L7613] |
| `orcaflex-text-model-writers` / 262 | Outside package | [docs/registry/implementation-map.yaml:L7614-L7626] |
| `orcaflex-text-model-writers` / 263 | Outside package | [docs/registry/implementation-map.yaml:L7627-L7636] |
| `orcaflex-text-model-writers` / 264 | Outside package | [docs/registry/implementation-map.yaml:L7637-L7646] |
| `orcaflex-text-model-writers` / 265 | Outside package | [docs/registry/implementation-map.yaml:L7647-L7656] |
| `orcaflex-text-model-writers` / 266 | Outside package | [docs/registry/implementation-map.yaml:L7657-L7666] |
| `orcaflex-text-model-writers` / 267 | Outside package | [docs/registry/implementation-map.yaml:L7667-L7676] |
| `orcaflex-text-model-writers` / 268 | Outside package | [docs/registry/implementation-map.yaml:L7677-L7686] |
| `orcaflex-text-model-writers` / 269 | Outside package | [docs/registry/implementation-map.yaml:L7687-L7696] |
| `orcaflex-text-model-writers` / 270 | Outside package | [docs/registry/implementation-map.yaml:L7697-L7706] |
| `orcaflex-text-model-writers` / 271 | Outside package | [docs/registry/implementation-map.yaml:L7707-L7716] |
| `orcaflex-text-model-writers` / 272 | Outside package | [docs/registry/implementation-map.yaml:L7717-L7729] |
| `orcaflex-text-model-writers` / 273 | Outside package | [docs/registry/implementation-map.yaml:L7730-L7739] |
| `orcaflex-text-model-writers` / 274 | Outside package | [docs/registry/implementation-map.yaml:L7740-L7749] |
| `orcaflex-text-model-writers` / 275 | Outside package | [docs/registry/implementation-map.yaml:L7750-L7762] |
| `orcaflex-text-model-writers` / 276 | Outside package | [docs/registry/implementation-map.yaml:L7763-L7774] |
| `orcaflex-text-model-writers` / 277 | Outside package | [docs/registry/implementation-map.yaml:L7775-L7784] |
| `orcaflex-text-model-writers` / 278 | Outside package | [docs/registry/implementation-map.yaml:L7785-L7794] |
| `orcaflex-text-model-writers` / 279 | Outside package | [docs/registry/implementation-map.yaml:L7795-L7804] |
| `orcaflex-text-model-writers` / 280 | Outside package | [docs/registry/implementation-map.yaml:L7805-L7816] |
| `orcaflex-text-model-writers` / 281 | Outside package | [docs/registry/implementation-map.yaml:L7817-L7826] |
| `orcaflex-text-model-writers` / 282 | Outside package | [docs/registry/implementation-map.yaml:L7827-L7836] |
| `orcaflex-text-model-writers` / 283 | Outside package | [docs/registry/implementation-map.yaml:L7837-L7846] |
| `orcaflex-text-model-writers` / 284 | Outside package | [docs/registry/implementation-map.yaml:L7847-L7856] |
| `orcaflex-text-model-writers` / 285 | Outside package | [docs/registry/implementation-map.yaml:L7857-L7866] |
| `orcaflex-text-model-writers` / 286 | Outside package | [docs/registry/implementation-map.yaml:L7867-L7878] |
| `orcaflex-text-model-writers` / 287 | Outside package | [docs/registry/implementation-map.yaml:L7879-L7888] |
| `orcaflex-text-model-writers` / 288 | Outside package | [docs/registry/implementation-map.yaml:L7889-L7898] |
| `orcaflex-text-model-writers` / 289 | Outside package | [docs/registry/implementation-map.yaml:L7899-L7908] |
| `orcaflex-text-model-writers` / 290 | Outside package | [docs/registry/implementation-map.yaml:L7909-L7921] |
| `orcaflex-text-model-writers` / 291 | Outside package | [docs/registry/implementation-map.yaml:L7922-L7931] |
| `orcaflex-text-model-writers` / 292 | Outside package | [docs/registry/implementation-map.yaml:L7932-L7941] |
| `orcaflex-text-model-writers` / 293 | Outside package | [docs/registry/implementation-map.yaml:L7942-L7951] |
| `orcaflex-text-model-writers` / 294 | Outside package | [docs/registry/implementation-map.yaml:L7952-L7961] |
| `orcaflex-text-model-writers` / 295 | Outside package | [docs/registry/implementation-map.yaml:L7962-L7971] |
| `orcaflex-text-model-writers` / 296 | Outside package | [docs/registry/implementation-map.yaml:L7972-L7981] |
| `orcaflex-text-model-writers` / 297 | Outside package | [docs/registry/implementation-map.yaml:L7982-L7991] |
| `orcaflex-text-model-writers` / 298 | Outside package | [docs/registry/implementation-map.yaml:L7992-L8001] |
| `orcaflex-text-model-writers` / 299 | Outside package | [docs/registry/implementation-map.yaml:L8002-L8011] |
| `orcaflex-text-model-writers` / 300 | Outside package | [docs/registry/implementation-map.yaml:L8012-L8021] |
| `orcaflex-text-model-writers` / 301 | Outside package | [docs/registry/implementation-map.yaml:L8022-L8031] |
| `orcaflex-text-model-writers` / 302 | Outside package | [docs/registry/implementation-map.yaml:L8032-L8043] |
| `orcaflex-text-model-writers` / 303 | Outside package | [docs/registry/implementation-map.yaml:L8044-L8053] |
| `orcaflex-text-model-writers` / 304 | Outside package | [docs/registry/implementation-map.yaml:L8054-L8063] |
| `orcaflex-text-model-writers` / 305 | Outside package | [docs/registry/implementation-map.yaml:L8064-L8073] |
| `orcaflex-text-model-writers` / 306 | Outside package | [docs/registry/implementation-map.yaml:L8074-L8083] |
| `orcaflex-text-model-writers` / 307 | Outside package | [docs/registry/implementation-map.yaml:L8084-L8093] |
| `orcaflex-text-model-writers` / 308 | Outside package | [docs/registry/implementation-map.yaml:L8094-L8103] |
| `orcaflex-text-model-writers` / 309 | Outside package | [docs/registry/implementation-map.yaml:L8104-L8113] |
| `orcaflex-text-model-writers` / 310 | Outside package | [docs/registry/implementation-map.yaml:L8114-L8123] |
| `orcaflex-text-model-writers` / 311 | Outside package | [docs/registry/implementation-map.yaml:L8124-L8133] |
| `orcaflex-text-model-writers` / 312 | Outside package | [docs/registry/implementation-map.yaml:L8134-L8143] |
| `orcaflex-text-model-writers` / 313 | Outside package | [docs/registry/implementation-map.yaml:L8144-L8155] |
| `orcaflex-text-model-writers` / 314 | Outside package | [docs/registry/implementation-map.yaml:L8156-L8165] |
| `orcaflex-text-model-writers` / 315 | Outside package | [docs/registry/implementation-map.yaml:L8166-L8175] |
| `orcaflex-text-model-writers` / 316 | Outside package | [docs/registry/implementation-map.yaml:L8176-L8185] |
| `orcaflex-text-model-writers` / 317 | Outside package | [docs/registry/implementation-map.yaml:L8186-L8195] |
| `orcaflex-text-model-writers` / 318 | Outside package | [docs/registry/implementation-map.yaml:L8196-L8205] |
| `orcaflex-text-model-writers` / 319 | Outside package | [docs/registry/implementation-map.yaml:L8206-L8215] |
| `orcaflex-text-model-writers` / 320 | Outside package | [docs/registry/implementation-map.yaml:L8216-L8227] |
| `orcaflex-text-model-writers` / 321 | Outside package | [docs/registry/implementation-map.yaml:L8228-L8237] |
| `orcaflex-text-model-writers` / 322 | Outside package | [docs/registry/implementation-map.yaml:L8238-L8247] |
| `orcaflex-text-model-writers` / 323 | Outside package | [docs/registry/implementation-map.yaml:L8248-L8257] |
| `orcaflex-text-model-writers` / 324 | Outside package | [docs/registry/implementation-map.yaml:L8258-L8267] |
| `orcaflex-text-model-writers` / 325 | Outside package | [docs/registry/implementation-map.yaml:L8268-L8279] |
| `orcaflex-text-model-writers` / 326 | Outside package | [docs/registry/implementation-map.yaml:L8280-L8290] |
| `orcaflex-text-model-writers` / 327 | Outside package | [docs/registry/implementation-map.yaml:L8291-L8300] |
| `orcaflex-text-model-writers` / 328 | Outside package | [docs/registry/implementation-map.yaml:L8301-L8310] |
| `orcaflex-text-model-writers` / 329 | Outside package | [docs/registry/implementation-map.yaml:L8311-L8323] |
| `orcaflex-text-model-writers` / 330 | Outside package | [docs/registry/implementation-map.yaml:L8324-L8333] |
| `orcaflex-text-model-writers` / 331 | Outside package | [docs/registry/implementation-map.yaml:L8334-L8343] |
| `orcaflex-text-model-writers` / 332 | Outside package | [docs/registry/implementation-map.yaml:L8344-L8353] |
| `orcaflex-text-model-writers` / 333 | Outside package | [docs/registry/implementation-map.yaml:L8354-L8363] |
| `orcaflex-text-model-writers` / 334 | Outside package | [docs/registry/implementation-map.yaml:L8364-L8373] |
| `orcaflex-text-model-writers` / 335 | Outside package | [docs/registry/implementation-map.yaml:L8374-L8383] |
| `orcaflex-text-model-writers` / 336 | Outside package | [docs/registry/implementation-map.yaml:L8384-L8393] |
| `orcaflex-text-model-writers` / 337 | Outside package | [docs/registry/implementation-map.yaml:L8394-L8403] |
| `orcaflex-text-model-writers` / 338 | Outside package | [docs/registry/implementation-map.yaml:L8404-L8413] |
| `orcaflex-text-model-writers` / 339 | Outside package | [docs/registry/implementation-map.yaml:L8414-L8423] |
| `orcaflex-text-model-writers` / 340 | Outside package | [docs/registry/implementation-map.yaml:L8424-L8435] |
| `orcaflex-text-model-writers` / 341 | Outside package | [docs/registry/implementation-map.yaml:L8436-L8445] |
| `orcaflex-text-model-writers` / 342 | Outside package | [docs/registry/implementation-map.yaml:L8446-L8457] |
| `orcaflex-text-model-writers` / 343 | Outside package | [docs/registry/implementation-map.yaml:L8458-L8470] |
| `orcaflex-text-model-writers` / 344 | Outside package | [docs/registry/implementation-map.yaml:L8471-L8480] |
| `orcaflex-text-model-writers` / 345 | Outside package | [docs/registry/implementation-map.yaml:L8481-L8492] |
| `orcaflex-text-model-writers` / 346 | Outside package | [docs/registry/implementation-map.yaml:L8493-L8505] |
| `orcaflex-text-model-writers` / 347 | Outside package | [docs/registry/implementation-map.yaml:L8506-L8517] |
| `orcaflex-text-model-writers` / 348 | Outside package | [docs/registry/implementation-map.yaml:L8518-L8527] |
| `orcaflex-text-model-writers` / 349 | Outside package | [docs/registry/implementation-map.yaml:L8528-L8537] |
| `orcaflex-text-model-writers` / 350 | Outside package | [docs/registry/implementation-map.yaml:L8538-L8547] |
| `orcaflex-text-model-writers` / 351 | Outside package | [docs/registry/implementation-map.yaml:L8548-L8560] |
| `orcaflex-text-model-writers` / 352 | Outside package | [docs/registry/implementation-map.yaml:L8561-L8572] |
| `orcaflex-text-model-writers` / 353 | Outside package | [docs/registry/implementation-map.yaml:L8573-L8582] |
| `orcaflex-text-model-writers` / 354 | Outside package | [docs/registry/implementation-map.yaml:L8583-L8596] |
| `orcaflex-text-model-writers` / 355 | Outside package | [docs/registry/implementation-map.yaml:L8597-L8608] |
| `orcaflex-text-model-writers` / 356 | Outside package | [docs/registry/implementation-map.yaml:L8609-L8619] |
| `orcaflex-text-model-writers` / 357 | Outside package | [docs/registry/implementation-map.yaml:L8620-L8632] |
| `orcaflex-text-model-writers` / 358 | Outside package | [docs/registry/implementation-map.yaml:L8633-L8647] |
| `orcaflex-text-model-writers` / 359 | Outside package | [docs/registry/implementation-map.yaml:L8648-L8657] |
| `orcaflex-text-model-writers` / 360 | Outside package | [docs/registry/implementation-map.yaml:L8658-L8667] |
| `orcaflex-text-model-writers` / 361 | Outside package | [docs/registry/implementation-map.yaml:L8668-L8677] |
| `orcaflex-text-model-writers` / 362 | Outside package | [docs/registry/implementation-map.yaml:L8678-L8687] |
| `orcaflex-text-model-writers` / 363 | Outside package | [docs/registry/implementation-map.yaml:L8688-L8701] |
| `orcaflex-text-model-writers` / 364 | Outside package | [docs/registry/implementation-map.yaml:L8702-L8714] |
| `orcaflex-text-model-writers` / 365 | Outside package | [docs/registry/implementation-map.yaml:L8715-L8724] |
| `orcaflex-text-model-writers` / 366 | Outside package | [docs/registry/implementation-map.yaml:L8725-L8739] |
| `orcaflex-text-model-writers` / 367 | Outside package | [docs/registry/implementation-map.yaml:L8740-L8749] |
| `orcaflex-text-model-writers` / 368 | Outside package | [docs/registry/implementation-map.yaml:L8750-L8759] |
| `orcaflex-text-model-writers` / 369 | Outside package | [docs/registry/implementation-map.yaml:L8760-L8773] |
| `orcaflex-text-model-writers` / 370 | Outside package | [docs/registry/implementation-map.yaml:L8774-L8783] |
| `orcaflex-text-model-writers` / 371 | Outside package | [docs/registry/implementation-map.yaml:L8784-L8793] |
| `orcaflex-text-model-writers` / 372 | Outside package | [docs/registry/implementation-map.yaml:L8794-L8803] |
| `orcaflex-text-model-writers` / 373 | Outside package | [docs/registry/implementation-map.yaml:L8804-L8813] |
| `orcaflex-text-model-writers` / 374 | Outside package | [docs/registry/implementation-map.yaml:L8814-L8823] |
| `orcaflex-text-model-writers` / 375 | Outside package | [docs/registry/implementation-map.yaml:L8824-L8833] |
| `orcaflex-text-model-writers` / 376 | Outside package | [docs/registry/implementation-map.yaml:L8834-L8843] |
| `orcaflex-text-model-writers` / 377 | Outside package | [docs/registry/implementation-map.yaml:L8844-L8856] |
| `orcaflex-text-model-writers` / 378 | Outside package | [docs/registry/implementation-map.yaml:L8857-L8866] |
| `orcaflex-text-model-writers` / 379 | Outside package | [docs/registry/implementation-map.yaml:L8867-L8876] |
| `orcaflex-text-model-writers` / 380 | Outside package | [docs/registry/implementation-map.yaml:L8877-L8886] |
| `orcaflex-text-model-writers` / 381 | Outside package | [docs/registry/implementation-map.yaml:L8887-L8896] |
| `orcaflex-text-model-writers` / 382 | Outside package | [docs/registry/implementation-map.yaml:L8897-L8908] |
| `orcaflex-text-model-writers` / 383 | Outside package | [docs/registry/implementation-map.yaml:L8909-L8918] |
| `orcaflex-text-model-writers` / 384 | Outside package | [docs/registry/implementation-map.yaml:L8919-L8928] |
| `orcaflex-text-model-writers` / 385 | Outside package | [docs/registry/implementation-map.yaml:L8929-L8938] |
| `orcaflex-text-model-writers` / 386 | Outside package | [docs/registry/implementation-map.yaml:L8939-L8948] |
| `orcaflex-text-model-writers` / 387 | Outside package | [docs/registry/implementation-map.yaml:L8949-L8958] |
| `orcaflex-text-model-writers` / 388 | Outside package | [docs/registry/implementation-map.yaml:L8959-L8970] |
| `orcaflex-text-model-writers` / 389 | Outside package | [docs/registry/implementation-map.yaml:L8971-L8980] |
| `orcaflex-text-model-writers` / 390 | Outside package | [docs/registry/implementation-map.yaml:L8981-L8990] |
| `orcaflex-text-model-writers` / 391 | Outside package | [docs/registry/implementation-map.yaml:L8991-L9000] |
| `orcaflex-text-model-writers` / 392 | Outside package | [docs/registry/implementation-map.yaml:L9001-L9010] |
| `orcaflex-text-model-writers` / 393 | Outside package | [docs/registry/implementation-map.yaml:L9011-L9020] |
| `orcaflex-text-model-writers` / 394 | Outside package | [docs/registry/implementation-map.yaml:L9021-L9030] |
| `orcaflex-text-model-writers` / 395 | Outside package | [docs/registry/implementation-map.yaml:L9031-L9040] |
| `orcaflex-text-model-writers` / 396 | Outside package | [docs/registry/implementation-map.yaml:L9041-L9050] |
| `orcaflex-text-model-writers` / 397 | Outside package | [docs/registry/implementation-map.yaml:L9051-L9060] |
| `orcaflex-text-model-writers` / 398 | Outside package | [docs/registry/implementation-map.yaml:L9061-L9070] |
| `orcaflex-text-model-writers` / 399 | Outside package | [docs/registry/implementation-map.yaml:L9071-L9080] |
| `orcaflex-text-model-writers` / 400 | Outside package | [docs/registry/implementation-map.yaml:L9081-L9090] |
| `orcaflex-text-model-writers` / 401 | Outside package | [docs/registry/implementation-map.yaml:L9091-L9100] |
| `orcaflex-text-model-writers` / 402 | Outside package | [docs/registry/implementation-map.yaml:L9101-L9110] |
| `orcaflex-text-model-writers` / 403 | Outside package | [docs/registry/implementation-map.yaml:L9111-L9120] |
| `orcaflex-text-model-writers` / 404 | Outside package | [docs/registry/implementation-map.yaml:L9121-L9130] |
| `orcaflex-text-model-writers` / 405 | Outside package | [docs/registry/implementation-map.yaml:L9131-L9140] |
| `orcaflex-text-model-writers` / 406 | Outside package | [docs/registry/implementation-map.yaml:L9141-L9152] |
| `orcaflex-text-model-writers` / 407 | Outside package | [docs/registry/implementation-map.yaml:L9153-L9162] |
| `orcaflex-text-model-writers` / 408 | Outside package | [docs/registry/implementation-map.yaml:L9163-L9172] |
| `orcaflex-text-model-writers` / 409 | Outside package | [docs/registry/implementation-map.yaml:L9173-L9182] |
| `orcaflex-text-model-writers` / 410 | Outside package | [docs/registry/implementation-map.yaml:L9183-L9192] |
| `orcaflex-text-model-writers` / 411 | Outside package | [docs/registry/implementation-map.yaml:L9193-L9202] |
| `orcaflex-text-model-writers` / 412 | Outside package | [docs/registry/implementation-map.yaml:L9203-L9212] |
| `orcaflex-text-model-writers` / 413 | Outside package | [docs/registry/implementation-map.yaml:L9213-L9222] |
| `orcaflex-text-model-writers` / 414 | Outside package | [docs/registry/implementation-map.yaml:L9223-L9232] |
| `orcaflex-text-model-writers` / 415 | Outside package | [docs/registry/implementation-map.yaml:L9233-L9242] |
| `orcaflex-text-model-writers` / 416 | Outside package | [docs/registry/implementation-map.yaml:L9243-L9252] |
| `orcaflex-text-model-writers` / 417 | Outside package | [docs/registry/implementation-map.yaml:L9253-L9262] |
| `orcaflex-text-model-writers` / 418 | Outside package | [docs/registry/implementation-map.yaml:L9263-L9272] |
| `orcaflex-text-model-writers` / 419 | Outside package | [docs/registry/implementation-map.yaml:L9273-L9282] |
| `orcaflex-text-model-writers` / 420 | Outside package | [docs/registry/implementation-map.yaml:L9283-L9292] |
| `orcaflex-text-model-writers` / 421 | Outside package | [docs/registry/implementation-map.yaml:L9293-L9302] |
| `orcaflex-text-model-writers` / 422 | Outside package | [docs/registry/implementation-map.yaml:L9303-L9312] |
| `orcaflex-text-model-writers` / 423 | Outside package | [docs/registry/implementation-map.yaml:L9313-L9322] |
| `orcaflex-text-model-writers` / 424 | Outside package | [docs/registry/implementation-map.yaml:L9323-L9332] |
| `orcaflex-text-model-writers` / 425 | Outside package | [docs/registry/implementation-map.yaml:L9333-L9342] |
| `orcaflex-text-model-writers` / 426 | Outside package | [docs/registry/implementation-map.yaml:L9343-L9352] |
| `orcaflex-text-model-writers` / 427 | Outside package | [docs/registry/implementation-map.yaml:L9353-L9362] |
| `orcaflex-text-model-writers` / 428 | Outside package | [docs/registry/implementation-map.yaml:L9363-L9372] |
| `orcaflex-text-model-writers` / 429 | Outside package | [docs/registry/implementation-map.yaml:L9373-L9382] |
| `orcaflex-text-model-writers` / 430 | Outside package | [docs/registry/implementation-map.yaml:L9383-L9392] |
| `orcaflex-text-model-writers` / 431 | Outside package | [docs/registry/implementation-map.yaml:L9393-L9402] |
| `orcaflex-text-model-writers` / 432 | Outside package | [docs/registry/implementation-map.yaml:L9403-L9412] |
| `orcaflex-text-model-writers` / 433 | Outside package | [docs/registry/implementation-map.yaml:L9413-L9422] |
| `orcaflex-text-model-writers` / 434 | Outside package | [docs/registry/implementation-map.yaml:L9423-L9432] |
| `orcaflex-text-model-writers` / 435 | Outside package | [docs/registry/implementation-map.yaml:L9433-L9442] |
| `orcaflex-text-model-writers` / 436 | Outside package | [docs/registry/implementation-map.yaml:L9443-L9452] |
| `orcaflex-text-model-writers` / 437 | Outside package | [docs/registry/implementation-map.yaml:L9453-L9462] |
| `orcaflex-text-model-writers` / 438 | Outside package | [docs/registry/implementation-map.yaml:L9463-L9472] |
| `orcaflex-text-model-writers` / 439 | Outside package | [docs/registry/implementation-map.yaml:L9473-L9482] |
| `orcaflex-text-model-writers` / 440 | Outside package | [docs/registry/implementation-map.yaml:L9483-L9493] |
| `wind-and-current-force-routes` / 1 | Outside package | [docs/registry/implementation-map.yaml:L15400-L15409] |
| `wind-and-current-force-routes` / 2 | Outside package | [docs/registry/implementation-map.yaml:L15410-L15419] |
| `wind-and-current-force-routes` / 3 | Outside package | [docs/registry/implementation-map.yaml:L15420-L15428] |
| `wind-and-current-force-routes` / 4 | Outside package | [docs/registry/implementation-map.yaml:L15429-L15438] |
| `wind-and-current-force-routes` / 5 | Outside package | [docs/registry/implementation-map.yaml:L15439-L15448] |
| `wind-and-current-force-routes` / 6 | Outside package | [docs/registry/implementation-map.yaml:L15449-L15460] |
| `wind-and-current-force-routes` / 7 | Outside package | [docs/registry/implementation-map.yaml:L15461-L15470] |
| `wind-and-current-force-routes` / 8 | Outside package | [docs/registry/implementation-map.yaml:L15471-L15480] |
| `wind-and-current-force-routes` / 9 | Outside package | [docs/registry/implementation-map.yaml:L15481-L15490] |
| `wind-and-current-force-routes` / 10 | Outside package | [docs/registry/implementation-map.yaml:L15491-L15500] |
| `wind-and-current-force-routes` / 11 | Outside package | [docs/registry/implementation-map.yaml:L15501-L15510] |
| `wind-and-current-force-routes` / 12 | Outside package | [docs/registry/implementation-map.yaml:L15511-L15520] |
| `wind-and-current-force-routes` / 13 | Outside package | [docs/registry/implementation-map.yaml:L15521-L15530] |
| `wind-and-current-force-routes` / 14 | Outside package | [docs/registry/implementation-map.yaml:L15531-L15540] |
| `wind-and-current-force-routes` / 15 | Outside package | [docs/registry/implementation-map.yaml:L15541-L15550] |
| `wind-and-current-force-routes` / 16 | Outside package | [docs/registry/implementation-map.yaml:L15551-L15559] |
| `wind-and-current-force-routes` / 17 | Outside package | [docs/registry/implementation-map.yaml:L15560-L15568] |
| `wind-and-current-force-routes` / 18 | Outside package | [docs/registry/implementation-map.yaml:L15569-L15578] |
| `wind-and-current-force-routes` / 19 | Outside package | [docs/registry/implementation-map.yaml:L15579-L15587] |
| `wind-and-current-force-routes` / 20 | Outside package | [docs/registry/implementation-map.yaml:L15588-L15597] |
| `wind-and-current-force-routes` / 21 | Outside package | [docs/registry/implementation-map.yaml:L15598-L15607] |
| `wind-and-current-force-routes` / 22 | Outside package | [docs/registry/implementation-map.yaml:L15608-L15617] |
| `wind-and-current-force-routes` / 23 | Outside package | [docs/registry/implementation-map.yaml:L15618-L15629] |
| `wind-and-current-force-routes` / 24 | Outside package | [docs/registry/implementation-map.yaml:L15630-L15639] |
| `wind-and-current-force-routes` / 25 | Outside package | [docs/registry/implementation-map.yaml:L15640-L15649] |
| `wind-and-current-force-routes` / 26 | Outside package | [docs/registry/implementation-map.yaml:L15650-L15659] |
| `wind-and-current-force-routes` / 27 | Outside package | [docs/registry/implementation-map.yaml:L15660-L15669] |
| `wind-and-current-force-routes` / 28 | Outside package | [docs/registry/implementation-map.yaml:L15670-L15684] |
| `wind-and-current-force-routes` / 29 | Outside package | [docs/registry/implementation-map.yaml:L15685-L15693] |
| `wind-and-current-force-routes` / 30 | Outside package | [docs/registry/implementation-map.yaml:L15694-L15703] |
| `wind-and-current-force-routes` / 31 | Outside package | [docs/registry/implementation-map.yaml:L15704-L15713] |
| `wind-and-current-force-routes` / 32 | Outside package | [docs/registry/implementation-map.yaml:L15714-L15723] |
| `wind-and-current-force-routes` / 33 | Outside package | [docs/registry/implementation-map.yaml:L15724-L15733] |
| `wind-and-current-force-routes` / 34 | Outside package | [docs/registry/implementation-map.yaml:L15734-L15743] |
| `wind-and-current-force-routes` / 35 | Outside package | [docs/registry/implementation-map.yaml:L15744-L15753] |
| `wind-and-current-force-routes` / 36 | Outside package | [docs/registry/implementation-map.yaml:L15754-L15763] |
| `wind-and-current-force-routes` / 37 | Outside package | [docs/registry/implementation-map.yaml:L15764-L15773] |
| `wind-and-current-force-routes` / 38 | Outside package | [docs/registry/implementation-map.yaml:L15774-L15783] |
| `wind-and-current-force-routes` / 39 | Outside package | [docs/registry/implementation-map.yaml:L15784-L15793] |
| `wind-and-current-force-routes` / 40 | Outside package | [docs/registry/implementation-map.yaml:L15794-L15806] |
| `wind-and-current-force-routes` / 41 | Outside package | [docs/registry/implementation-map.yaml:L15807-L15816] |
| `wind-and-current-force-routes` / 42 | Outside package | [docs/registry/implementation-map.yaml:L15817-L15826] |
| `wind-and-current-force-routes` / 43 | Outside package | [docs/registry/implementation-map.yaml:L15827-L15836] |
| `wind-and-current-force-routes` / 44 | Outside package | [docs/registry/implementation-map.yaml:L15837-L15846] |
| `wind-and-current-force-routes` / 45 | Outside package | [docs/registry/implementation-map.yaml:L15847-L15856] |
| `wind-and-current-force-routes` / 46 | Outside package | [docs/registry/implementation-map.yaml:L15857-L15866] |
| `wind-and-current-force-routes` / 47 | Outside package | [docs/registry/implementation-map.yaml:L15867-L15876] |
| `wind-and-current-force-routes` / 48 | Outside package | [docs/registry/implementation-map.yaml:L15877-L15886] |
| `wind-and-current-force-routes` / 49 | Outside package | [docs/registry/implementation-map.yaml:L15887-L15896] |
| `wind-and-current-force-routes` / 50 | Outside package | [docs/registry/implementation-map.yaml:L15897-L15906] |
| `wind-and-current-force-routes` / 51 | Outside package | [docs/registry/implementation-map.yaml:L15907-L15916] |
| `wind-and-current-force-routes` / 52 | Outside package | [docs/registry/implementation-map.yaml:L15917-L15926] |
| `wind-and-current-force-routes` / 53 | Outside package | [docs/registry/implementation-map.yaml:L15927-L15936] |
| `wind-and-current-force-routes` / 54 | Outside package | [docs/registry/implementation-map.yaml:L15937-L15946] |
| `wind-and-current-force-routes` / 55 | Outside package | [docs/registry/implementation-map.yaml:L15947-L15956] |
| `wind-and-current-force-routes` / 56 | Outside package | [docs/registry/implementation-map.yaml:L15957-L15965] |
| `wind-and-current-force-routes` / 57 | Outside package | [docs/registry/implementation-map.yaml:L15966-L15975] |
| `wind-and-current-force-routes` / 58 | Outside package | [docs/registry/implementation-map.yaml:L15976-L15987] |
| `wind-and-current-force-routes` / 59 | Outside package | [docs/registry/implementation-map.yaml:L15988-L15997] |
| `wind-and-current-force-routes` / 60 | Outside package | [docs/registry/implementation-map.yaml:L15998-L16009] |
| `wind-and-current-force-routes` / 61 | Outside package | [docs/registry/implementation-map.yaml:L16010-L16019] |
| `wind-and-current-force-routes` / 62 | Outside package | [docs/registry/implementation-map.yaml:L16020-L16029] |
| `wind-and-current-force-routes` / 63 | Outside package | [docs/registry/implementation-map.yaml:L16030-L16039] |
| `wind-and-current-force-routes` / 64 | Outside package | [docs/registry/implementation-map.yaml:L16040-L16049] |
| `wind-and-current-force-routes` / 65 | Outside package | [docs/registry/implementation-map.yaml:L16050-L16059] |
| `wind-and-current-force-routes` / 66 | Outside package | [docs/registry/implementation-map.yaml:L16060-L16074] |
| `wind-and-current-force-routes` / 67 | Outside package | [docs/registry/implementation-map.yaml:L16075-L16083] |
| `wind-and-current-force-routes` / 68 | Outside package | [docs/registry/implementation-map.yaml:L16084-L16093] |
| `wind-and-current-force-routes` / 69 | Outside package | [docs/registry/implementation-map.yaml:L16094-L16103] |
| `wind-and-current-force-routes` / 70 | Outside package | [docs/registry/implementation-map.yaml:L16104-L16113] |
| `wind-and-current-force-routes` / 71 | Outside package | [docs/registry/implementation-map.yaml:L16114-L16126] |
| `wind-and-current-force-routes` / 72 | Outside package | [docs/registry/implementation-map.yaml:L16127-L16136] |
| `wind-and-current-force-routes` / 73 | Outside package | [docs/registry/implementation-map.yaml:L16137-L16146] |
| `wind-and-current-force-routes` / 74 | Outside package | [docs/registry/implementation-map.yaml:L16147-L16156] |
| `wind-and-current-force-routes` / 75 | Outside package | [docs/registry/implementation-map.yaml:L16157-L16168] |
| `wind-and-current-force-routes` / 76 | Outside package | [docs/registry/implementation-map.yaml:L16169-L16178] |
| `wind-and-current-force-routes` / 77 | Outside package | [docs/registry/implementation-map.yaml:L16179-L16188] |
| `wind-and-current-force-routes` / 78 | Outside package | [docs/registry/implementation-map.yaml:L16189-L16198] |
| `wind-and-current-force-routes` / 79 | Outside package | [docs/registry/implementation-map.yaml:L16199-L16208] |
| `wind-and-current-force-routes` / 80 | Outside package | [docs/registry/implementation-map.yaml:L16209-L16218] |
| `wind-and-current-force-routes` / 81 | Outside package | [docs/registry/implementation-map.yaml:L16219-L16228] |
| `wind-and-current-force-routes` / 82 | Outside package | [docs/registry/implementation-map.yaml:L16229-L16238] |
| `wind-and-current-force-routes` / 83 | In package: `integrator.py`, `KnowledgeIntegrator._extract_configurations` | [docs/registry/implementation-map.yaml:L16239-L16248] |
| `wind-and-current-force-routes` / 84 | Outside package | [docs/registry/implementation-map.yaml:L16249-L16258] |
| `wind-and-current-force-routes` / 85 | Outside package | [docs/registry/implementation-map.yaml:L16259-L16268] |
| `wind-and-current-force-routes` / 86 | Outside package | [docs/registry/implementation-map.yaml:L16269-L16280] |
| `wind-and-current-force-routes` / 87 | Outside package | [docs/registry/implementation-map.yaml:L16281-L16293] |
| `wind-and-current-force-routes` / 88 | Outside package | [docs/registry/implementation-map.yaml:L16294-L16304] |
| `wind-and-current-force-routes` / 89 | Outside package | [docs/registry/implementation-map.yaml:L16305-L16314] |
| `wind-and-current-force-routes` / 90 | Outside package | [docs/registry/implementation-map.yaml:L16315-L16324] |
| `wind-and-current-force-routes` / 91 | Outside package | [docs/registry/implementation-map.yaml:L16325-L16334] |
| `wind-and-current-force-routes` / 92 | Outside package | [docs/registry/implementation-map.yaml:L16335-L16344] |
| `wind-and-current-force-routes` / 93 | Outside package | [docs/registry/implementation-map.yaml:L16345-L16354] |
| `wind-and-current-force-routes` / 94 | Outside package | [docs/registry/implementation-map.yaml:L16355-L16364] |
| `wind-and-current-force-routes` / 95 | Outside package | [docs/registry/implementation-map.yaml:L16365-L16374] |
| `wind-and-current-force-routes` / 96 | Outside package | [docs/registry/implementation-map.yaml:L16375-L16384] |
| `wind-and-current-force-routes` / 97 | Outside package | [docs/registry/implementation-map.yaml:L16385-L16396] |
| `wind-and-current-force-routes` / 98 | Outside package | [docs/registry/implementation-map.yaml:L16397-L16408] |
| `wind-and-current-force-routes` / 99 | Outside package | [docs/registry/implementation-map.yaml:L16409-L16418] |
| `wind-and-current-force-routes` / 100 | Outside package | [docs/registry/implementation-map.yaml:L16419-L16428] |
| `wind-and-current-force-routes` / 101 | Outside package | [docs/registry/implementation-map.yaml:L16429-L16438] |
| `wind-and-current-force-routes` / 102 | Outside package | [docs/registry/implementation-map.yaml:L16439-L16448] |
| `wind-and-current-force-routes` / 103 | Outside package | [docs/registry/implementation-map.yaml:L16449-L16458] |
| `wind-and-current-force-routes` / 104 | Outside package | [docs/registry/implementation-map.yaml:L16459-L16468] |
| `wind-and-current-force-routes` / 105 | Outside package | [docs/registry/implementation-map.yaml:L16469-L16477] |
| `wind-and-current-force-routes` / 106 | Outside package | [docs/registry/implementation-map.yaml:L16478-L16487] |
| `wind-and-current-force-routes` / 107 | Outside package | [docs/registry/implementation-map.yaml:L16488-L16497] |
| `wind-and-current-force-routes` / 108 | Outside package | [docs/registry/implementation-map.yaml:L16498-L16507] |
| `wind-and-current-force-routes` / 109 | Outside package | [docs/registry/implementation-map.yaml:L16508-L16517] |
| `wind-and-current-force-routes` / 110 | Outside package | [docs/registry/implementation-map.yaml:L16518-L16528] |
| `wind-and-current-force-routes` / 111 | Outside package | [docs/registry/implementation-map.yaml:L16529-L16538] |
| `wind-and-current-force-routes` / 112 | Outside package | [docs/registry/implementation-map.yaml:L16539-L16549] |
| `wind-and-current-force-routes` / 113 | Outside package | [docs/registry/implementation-map.yaml:L16550-L16559] |
| `wind-and-current-force-routes` / 114 | Outside package | [docs/registry/implementation-map.yaml:L16560-L16569] |
| `wind-and-current-force-routes` / 115 | Outside package | [docs/registry/implementation-map.yaml:L16570-L16579] |
| `wind-and-current-force-routes` / 116 | Outside package | [docs/registry/implementation-map.yaml:L16580-L16589] |
| `wind-and-current-force-routes` / 117 | Outside package | [docs/registry/implementation-map.yaml:L16590-L16600] |
| `wind-and-current-force-routes` / 118 | Outside package | [docs/registry/implementation-map.yaml:L16601-L16611] |
| `wind-and-current-force-routes` / 119 | Outside package | [docs/registry/implementation-map.yaml:L16612-L16622] |
| `wind-and-current-force-routes` / 120 | Outside package | [docs/registry/implementation-map.yaml:L16623-L16633] |
| `wind-and-current-force-routes` / 121 | Outside package | [docs/registry/implementation-map.yaml:L16634-L16644] |
| `wind-and-current-force-routes` / 122 | Outside package | [docs/registry/implementation-map.yaml:L16645-L16655] |
| `wind-and-current-force-routes` / 123 | Outside package | [docs/registry/implementation-map.yaml:L16656-L16665] |
| `wind-and-current-force-routes` / 124 | Outside package | [docs/registry/implementation-map.yaml:L16666-L16675] |
| `wind-and-current-force-routes` / 125 | Outside package | [docs/registry/implementation-map.yaml:L16676-L16685] |
| `wind-and-current-force-routes` / 126 | Outside package | [docs/registry/implementation-map.yaml:L16686-L16695] |
| `wind-and-current-force-routes` / 127 | Outside package | [docs/registry/implementation-map.yaml:L16696-L16705] |
| `wind-and-current-force-routes` / 128 | Outside package | [docs/registry/implementation-map.yaml:L16706-L16715] |
| `wind-and-current-force-routes` / 129 | Outside package | [docs/registry/implementation-map.yaml:L16716-L16725] |
| `wind-and-current-force-routes` / 130 | Outside package | [docs/registry/implementation-map.yaml:L16726-L16735] |
| `wind-and-current-force-routes` / 131 | Outside package | [docs/registry/implementation-map.yaml:L16736-L16747] |
| `wind-and-current-force-routes` / 132 | Outside package | [docs/registry/implementation-map.yaml:L16748-L16758] |
| `wind-and-current-force-routes` / 133 | Outside package | [docs/registry/implementation-map.yaml:L16759-L16769] |
| `wind-and-current-force-routes` / 134 | Outside package | [docs/registry/implementation-map.yaml:L16770-L16781] |
| `wind-and-current-force-routes` / 135 | Outside package | [docs/registry/implementation-map.yaml:L16782-L16791] |

### Exact-signal audit

Search uses Python `re`, case-sensitive, on `.py` files at the code pin, assigning a hit to the innermost enclosing function/class or one module-level definition. Signals matching `solvers/orcaflex/examples_integration` case-insensitively are omitted; only the writer family vocabulary expression matches that path. No expression is corrected or unescaped.

Exact map expressions (JSON escaping only):

```json
[
  {
    "family": "orcaflex-text-model-writers",
    "search_signals": [
      {
        "search": "algorithm",
        "pattern": "(?:yaml\\.(?:dump|safe_dump)|write_text|open\\(|\\.write\\(|save\\(|SaveData|dump\\().*|to_orcaflex_dict|ObjectType|EndAConnection|LineConfig|VesselConfig|WaveTrains|CurrentRamp|WindSpeed|\\.write\\(|\\.ya?ml|yaml\\.safe_dump|shutil\\.copy|\\.write\\(|write_model|ModularModelGenerator|ProjectInputSpec|yaml\\.safe_dump|\\.write_text"
      },
      {
        "search": "vocabulary",
        "pattern": "orcaflex|OrcaFlex|includefile|modular_generator"
      },
      {
        "search": "constants_or_keys",
        "pattern": "[\\\"\\'](?:Lines|LineTypes|Vessels|6DBuoys|General|IncludeFile|includefile)[\\\"\\']|%YAML"
      }
    ]
  },
  {
    "family": "wind-and-current-force-routes",
    "search_signals": [
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
    ]
  }
]
```

The code-side audit below enumerates every matched definition; listed entries are already in the map inventory, and the remaining entries read, delegate, copy bytes, serialize, report, hold data or match documentation rather than computing a second model-writing/force result.

| Family | Definition | Hit lines | Classification and code citation |
| --- | --- | --- | --- |
| `orcaflex-text-model-writers` | `analyzer.py` / `FeatureAnalyzer.analyze_directory` | 147 | Reader, validator or caller helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L147-L147] |
| `orcaflex-text-model-writers` | `analyzer.py` / `FeatureAnalyzer.analyze_file` | 89, 99, 100, 104, 105 | Listed map implementation; no added miss. [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L89-L105] |
| `orcaflex-text-model-writers` | `analyzer.py` / `FeatureAnalyzer.save_analysis` | 164, 165 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L164-L165] |
| `orcaflex-text-model-writers` | `batch_converter.py` / `OrcaFlexBatchConverter.__init__` | 34 | Documentation/data declaration; no result algorithm. [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L34-L34] |
| `orcaflex-text-model-writers` | `batch_converter.py` / `OrcaFlexBatchConverter._generate_report` | 311, 312, 313, 316, 317, 318, 319, 320, 321, 322, 323, 326, 327, 328, 334, 338, 340, 341, 342, 347, 349, 352, 356, 357 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L311-L357] |
| `orcaflex-text-model-writers` | `batch_converter.py` / `OrcaFlexBatchConverter._mock_convert` | 204, 223, 224 | Listed map implementation; no added miss. [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L204-L224] |
| `orcaflex-text-model-writers` | `batch_converter.py` / `OrcaFlexBatchConverter._real_convert` | 176 | Listed map implementation; no added miss. [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L176-L176] |
| `orcaflex-text-model-writers` | `batch_converter.py` / `OrcaFlexBatchConverter._validate_yaml` | 236, 244 | Reader, validator or caller helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L236-L244] |
| `orcaflex-text-model-writers` | `batch_converter.py` / `OrcaFlexBatchConverter.convert_file` | 127 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L127-L127] |
| `orcaflex-text-model-writers` | `complete_downloader.py` / `CompleteOrcaflexDownloader.download_zip_file` | 196, 201, 205 | Archive/byte transfer helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L196-L205] |
| `orcaflex-text-model-writers` | `complete_downloader.py` / `CompleteOrcaflexDownloader.extract_zip_file` | 242, 252, 253, 254, 278, 279, 280 | Archive/byte transfer helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L242-L280] |
| `orcaflex-text-model-writers` | `complete_downloader.py` / `CompleteOrcaflexDownloader.generate_complete_catalog` | 470, 471 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L470-L471] |
| `orcaflex-text-model-writers` | `complete_downloader.py` / `CompleteOrcaflexDownloader.load_manifest` | 65 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L65-L65] |
| `orcaflex-text-model-writers` | `complete_downloader.py` / `CompleteOrcaflexDownloader.save_manifest` | 78, 79 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L78-L79] |
| `orcaflex-text-model-writers` | `complete_downloader.py` / `main` | 538, 543 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L538-L543] |
| `orcaflex-text-model-writers` | `converter.py` / `YamlConverter.convert_batch` | 52, 57, 58 | Listed map implementation; no added miss. [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L52-L58] |
| `orcaflex-text-model-writers` | `converter.py` / `YamlConverter.validate_yaml` | 80 | Reader, validator or caller helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L80-L80] |
| `orcaflex-text-model-writers` | `download_all_examples.py` / `download_all_examples` | 124, 129 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L124-L129] |
| `orcaflex-text-model-writers` | `downloader.py` / `OrcaflexExampleDownloader.download_example` | 229, 230 | Archive/byte transfer helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L229-L230] |
| `orcaflex-text-model-writers` | `downloader.py` / `OrcaflexExampleDownloader.generate_download_report` | 392, 393 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L392-L393] |
| `orcaflex-text-model-writers` | `downloader.py` / `OrcaflexExampleDownloader.load_manifest` | 66 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L66-L66] |
| `orcaflex-text-model-writers` | `downloader.py` / `OrcaflexExampleDownloader.save_manifest` | 77, 78 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L77-L78] |
| `orcaflex-text-model-writers` | `downloader.py` / `OrcaflexExampleDownloader.verify_downloads` | 319 | Reader, validator or caller helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L319-L319] |
| `orcaflex-text-model-writers` | `enhanced_downloader.py` / `EnhancedOrcaflexDownloader.download_zip_file` | 186, 190, 194 | Archive/byte transfer helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L186-L194] |
| `orcaflex-text-model-writers` | `enhanced_downloader.py` / `EnhancedOrcaflexDownloader.extract_zip_file` | 239, 248, 249, 250, 274, 275, 276 | Archive/byte transfer helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L239-L276] |
| `orcaflex-text-model-writers` | `enhanced_downloader.py` / `EnhancedOrcaflexDownloader.generate_catalog` | 378, 379 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L378-L379] |
| `orcaflex-text-model-writers` | `enhanced_downloader.py` / `EnhancedOrcaflexDownloader.load_manifest` | 66 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L66-L66] |
| `orcaflex-text-model-writers` | `enhanced_downloader.py` / `EnhancedOrcaflexDownloader.save_manifest` | 78, 79 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L78-L79] |
| `orcaflex-text-model-writers` | `fix_extraction.py` / `re_extract_all` | 47, 61, 62, 63, 79, 80, 81 | Archive/byte transfer helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L47-L81] |
| `orcaflex-text-model-writers` | `integrate_downloaded_examples.py` / `DownloadedExamplesIntegrator._create_markdown_summary` | 547, 548 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L547-L548] |
| `orcaflex-text-model-writers` | `integrate_downloaded_examples.py` / `DownloadedExamplesIntegrator.analyze_downloaded_examples` | 44 | Reader, validator or caller helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L44-L44] |
| `orcaflex-text-model-writers` | `integrate_downloaded_examples.py` / `DownloadedExamplesIntegrator.save_knowledge_base` | 422, 423, 431, 432 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L422-L432] |
| `orcaflex-text-model-writers` | `integrator.py` / `KnowledgeIntegrator.load_existing_knowledge` | 36 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L36-L36] |
| `orcaflex-text-model-writers` | `integrator.py` / `KnowledgeIntegrator.save_index` | 267, 268 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L267-L268] |
| `orcaflex-text-model-writers` | `integrator.py` / `KnowledgeIntegrator.save_knowledge` | 164, 165 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L164-L165] |
| `orcaflex-text-model-writers` | `integrator.py` / `KnowledgeIntegrator.update_agent_context` | 218, 219 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L218-L219] |
| `orcaflex-text-model-writers` | `orcfxapi_converter.py` / `<module>` | 3, 6 | Documentation/data declaration; no result algorithm. [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L3-L6] |
| `orcaflex-text-model-writers` | `orcfxapi_converter.py` / `OrcFxAPIConverter` | 26 | Documentation/data declaration; no result algorithm. [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L26-L26] |
| `orcaflex-text-model-writers` | `orcfxapi_converter.py` / `OrcFxAPIConverter._generate_report` | 216, 233, 234 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L216-L234] |
| `orcaflex-text-model-writers` | `orcfxapi_converter.py` / `OrcFxAPIConverter.convert_file` | 107, 131, 136, 142, 144, 145, 149 | Listed map implementation; no added miss. [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L107-L149] |
| `orcaflex-text-model-writers` | `orcfxapi_converter.py` / `main` | 254, 289, 302 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L254-L302] |
| `orcaflex-text-model-writers` | `scraper_analyzer.py` / `analyze_orcina_site` | 51, 95, 96 | Serialization/reporting/path/delegation helper; no model-text construction. [src/digitalmodel/solvers/orcaflex/examples_integration/scraper_analyzer.py:L51-L96] |
| `wind-and-current-force-routes` | `integrator.py` / `KnowledgeIntegrator._extract_configurations` | 118, 120, 121 | Listed map implementation; no added miss. [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L118-L121] |

Counted-family audit: writer signals hit 126 source lines in 42 definitions; force-route signals hit 3 lines in 1 definition, already inventoried. Further second implementations found: 0 summed across both families. The force-family row aggregates profile frequencies rather than computing force, and several writer rows count/extract metadata; reconciliation remains a map follow-up, issue pending (5). [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L104-L121] [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L96-L134]

Repository-wide discovery also scans tracked Python files outside the package, including `scripts/`, with the same retained expressions. Those external hits are discovery candidates/map gaps rather than additional counted-package misses; independent implementation claims require semantic review, issue pending (5).

Map follow-up: HTTP example discovery/retry, ZIP extraction/integrity, YAML feature extraction, filename classification and knowledge aggregation lack correctly scoped result families; add or reconcile in-package rows before counting them. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L88-L158] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L218-L284] [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L76-L134] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L108-L257] [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L83-L155]

Repository discovery observation at the code pin: 6747 tracked Python files read; writer expressions matched 2725 files (458 under scripts/), and force-route expressions matched 342 files (27 under scripts/). These broad string hits have not been classified as independent engineering implementations; issue pending (5).

## 10. Coverage and open items

### Read coverage

All package Python files were read as text, including the empty `feature_analyzer.py`; the following list is a coverage record, not a claim that every path has a runtime check.

- `__init__.py`: [src/digitalmodel/solvers/orcaflex/examples_integration/__init__.py:L1-L20]
- `analyzer.py`: [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L1-L195]
- `batch_converter.py`: [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L1-L408]
- `complete_downloader.py`: [src/digitalmodel/solvers/orcaflex/examples_integration/complete_downloader.py:L1-L549]
- `converter.py`: [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L1-L91]
- `download_all_examples.py`: [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L1-L169]
- `downloader.py`: [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L1-L409]
- `enhanced_downloader.py`: [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L1-L403]
- `feature_analyzer.py`: no executable-contract claim; see missing coverage, issue pending (4).
- `fix_extraction.py`: [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L1-L105]
- `integrate_downloaded_examples.py`: [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L1-L592]
- `integrator.py`: [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L1-L270]
- `orcfxapi_converter.py`: [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L1-L311]
- `run_full_conversion.py`: [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L1-L132]
- `scraper_analyzer.py`: [src/digitalmodel/solvers/orcaflex/examples_integration/scraper_analyzer.py:L1-L132]

Tests read: the package test directory's four script/test files and `test_examples_downloader.py`; the latter's 12 nodes and the packaged-import file's 3 nodes make 15 recursively collected tests, while three legacy script functions remain outside that collection. [tests/solvers/orcaflex/test_examples_downloader.py:L21-L325] [tests/solvers/orcaflex/examples_integration/test_packaged_download_import.py:L25-L154] [tests/conftest.py:L46-L53]

Documentation found by `rg -n 'examples_integration|Examples Integration|examples integration' docs --glob '*.md' --glob '*.rst'`: one historical plan; its package-specific fallback and exclusion passages were checked. The collection README was also read as context; its status figures are not adopted as validation evidence. [docs/plans/2026-08-05-issue-1631-licensed-lane-fail-closed.md:L120-L120] [docs/plans/2026-08-05-issue-1631-licensed-lane-fail-closed.md:L513-L513] [docs/domains/orcaflex/examples/README.md:L1-L22]

Read surfaces also include the routing registry, test collection settings, applicable hook entries and the implementation map at its separately declared pin; the public pack cites AQG r0 by title and section rather than copying private planning locators. [docs/registry/module-routing.yaml:L477-L525] [tests/conftest.py:L46-L53] [.pre-commit-config.yaml:L1-L258] [docs/registry/implementation-map.yaml:L1-L9]

### Open-item register

| Item | Remaining work / evidence limit | Responsible role | Tracking |
| --- | --- | --- | --- |
| O1 | Refuse implicit mock success, ignored validation errors, partial native-save success and unchecked reuse; implement real error-path detectors for F01–F04. [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L163-L191] [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L133-L155] | Maintainer | issue pending (1) |
| O2 | Detect missing/empty discovery, unknown display-size units, archive collisions/partial extraction and incomplete-success summaries; cover F04–F08. [src/digitalmodel/solvers/orcaflex/examples_integration/downloader.py:L128-L158] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L123-L125] [src/digitalmodel/solvers/orcaflex/examples_integration/enhanced_downloader.py:L238-L284] [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L83-L109] | Maintainer | issue pending (2) |
| O3 | Reconcile manifest schema, distinguish unknown/default metadata, verify inferred tags against actual YAML and make knowledge integration idempotent; cover F09–F13. [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L23-L74] [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L50-L78] [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L63-L102] | Maintainer | issue pending (3) |
| O4 | Add independent worked examples, real version-matched load/statics/dynamics evidence, conditional diffraction records and generated gate appendix; Q0.4–Q5.1 remain unperformed, with no engineering validation claimed. [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L168-L187] [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L307-L364] (AQG r0, sections 4–10.) | Family quality manager | issue pending (4) |
| O5 | UNVERIFIED: unit/frame/origin/sign/phase preservation and supported real-conversion versions; one unverified claim, repeated here from section 2. | Family quality manager | issue pending (4) |
| O6 | Repair legacy script imports and CLI test-mode resolution; normal packaged import/help regression does not exercise those paths. [src/digitalmodel/solvers/orcaflex/examples_integration/download_all_examples.py:L152-L156] [src/digitalmodel/solvers/orcaflex/examples_integration/fix_extraction.py:L10-L10] [src/digitalmodel/solvers/orcaflex/examples_integration/run_full_conversion.py:L13-L16] | Maintainer | #1900 |
| O7 | Add assertions/offline coverage for legacy conversion routines, analyzers, integrators and extraction; remove stale fixed collection prose only in a separate code change. [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L448-L450] [tests/solvers/orcaflex/examples_integration/test_converter.py:L15-L52] [tests/conftest.py:L46-L53] | Maintainer | issue pending (4) |
| O8 | Reconcile broad map false positives and add the uncounted retrieval/extraction/classification/aggregation families before counting them; inspect external discovery candidates without changing this package's audit result. [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L104-L121] [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L96-L134] | Family quality manager | issue pending (5) |
| O9 | Assign independent reviewers, shared-log entry and actual gate signatures before clearance. (AQG r0, section 11.) | Maintainer | issue pending (4) |
| O10 | Align the fixed solver-route contract with this required registry addition; its current equality assertion enumerates the previous set and cannot accept an additional submodule without an intentional test update. [tests/docs/test_digitalmodel_routing_contract.py:L217-L254] | Maintainer | issue pending (6) |

No private sidecar values are published; no sidecar entry identifier is asserted by this pack.
