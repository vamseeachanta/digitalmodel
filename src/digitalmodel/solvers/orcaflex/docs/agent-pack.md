# OrcaFlex top-level module knowledge pack

| Header               | Value                                                |
| -------------------- | ---------------------------------------------------- |
| Module               | solvers/orcaflex, directly contained Python files    |
| Repository           | digitalmodel                                         |
| Path                 | src/digitalmodel/solvers/orcaflex/docs/agent-pack.md |
| Pack revision        | 2                                                    |
| Code revision        | fb8a214bdb622ed04f0b6473a9311695c9a5a314             |
| Analysis family      | OrcaFlex riser and mooring                           |
| Quality manager role | Riser and mooring quality manager                    |
| Status               | DRAFT                                                |

## 1. Purpose and scope

This pack covers the text preparation, validation, execution and post-processing
routes implemented directly in the module; optional execution exports are resolved
lazily by the package initializer. `[src/digitalmodel/solvers/orcaflex/__init__.py:L24-L65]`

The run-to-simulation route produces a static-state simulation, whereas the native
parallel route optionally runs dynamics, reopens the saved simulation and extracts
adapter-defined channels. [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L119-L143]
[src/digitalmodel/solvers/orcaflex/parallel_runner.py:L245-L291]

The routing registry assigns beta maturity to the parent solvers group; the
separate production entry refers to the compatibility path, not this module.
[docs/registry/module-routing.yaml:L195-L200]
[docs/registry/module-routing.yaml:L255-L260]

"Production Ready" is a README claim, unverified; the thread guideline's timing
and optimality claims are not independent benchmark evidence.
[src/digitalmodel/solvers/orcaflex/MODULE_README.md:L1-L5]
[src/digitalmodel/solvers/orcaflex/THREAD_OPTIMIZATION_GUIDELINES.md:L106-L136]

Applicability is limited by the route: the native parallel runner accepts only
statics or dynamics case records and checks the actual DLL against its requested
version; this does not qualify arbitrary geometry, loading or solver versions.
[src/digitalmodel/solvers/orcaflex/parallel_runner.py:L113-L126]
[src/digitalmodel/solvers/orcaflex/parallel_runner.py:L207-L225]

## 2. Inputs and outputs

### Inputs

| Name                                  | Unit                                         | Range and default                                                                                                                           | Source and interpretation                                                                                                                                                                                                                                                                       |
| ------------------------------------- | -------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| YAML model path                       | file path                                    | Loader reads UTF-8 with optional BOM; no geometric range is imposed by this loader                                                          | Text input; returns data plus header lines. [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L88-L115]                                                                                                                                                                                          |
| Model search                          | file path                                    | Default pattern `*.yml`; matching files must end in `.yml`; default exclusion substrings are `includefile`, `_output`, `_backup` and `_old` | `find_model_files`; default search does not accept `.yaml` or `.dat`. [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L236-L268]                                                                                                                                                               |
| Run-to-simulation workers             | worker count                                 | Default 30; passed to a thread-pool executor                                                                                                | `run_batch(max_workers)`; a worker count is distinct from solver thread count. [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L145-L155] [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L175-L180]                                                                                          |
| Parallel cases                        | dimensionless identifiers; analysis selector | Unique identifier matched by `CASE_ID`; analysis is statics or dynamics                                                                     | `check_cases` checks syntax and JSON serialization. [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L48-L50] [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L113-L126]                                                                                                             |
| Native parallel controls              | worker and thread counts                     | Default maximum workers 57, maximum attempts 3, one solver thread; worker check bounds count by local CPU count                             | `run_cases`, `check_workers`; these are implementation defaults, not resource recommendations. [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L47-L51] [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L106-L110] [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L387-L405] |
| Solver version                        | version string                               | Native defaults request 11.6 and expect DLL 11.6c                                                                                           | `run_one` compares returned DLL version before building a case. [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L207-L225]                                                                                                                                                                |
| Retention policy                      | h                                            | Default window 72.0; required channel key defaults to `w5`                                                                                  | `DEFAULT_WINDOW_H`, `DEFAULT_REQUIRED`; caller must select the channels appropriate to its adapter. [src/digitalmodel/solvers/orcaflex/sim_retention.py:L36-L40] [src/digitalmodel/solvers/orcaflex/sim_retention.py:L130-L143]                                                                 |
| Authoring inputs                      | structured intent and request text           | Optional author; absent author selects the CLI-backed implementation                                                                        | `author_spec` maps intent to resolver inputs and returns spec plus assumption ledger. [src/digitalmodel/solvers/orcaflex/spec_author.py:L480-L505]                                                                                                                                              |
| OPP configuration and simulation path | file path and variable selectors             | Strength workflow selects effective tension, wall tension, bend moment, curvature and bend radius                                           | High-level template and expansion into per-object summaries/range graphs. [examples/workflows/orcaflex-strength-post/input.yml:L20-L51] [src/digitalmodel/solvers/orcaflex/opp_config_expansion.py:L61-L106]                                                                                    |

### Outputs

| Name                 | Unit                                                             | Destination and condition                                                                                                                                                                                                                                                                                                                                                       |
| -------------------- | ---------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Static simulation    | binary simulation file                                           | `<model stem>.sim` in supplied output folder or model folder, after static-state check. [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L80-L98] [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L119-L143]                                                                                                                                                                   |
| Native run record    | s for timings; bytes for simulation size; SHA-256 digest strings | Case, attempt, analysis, threads, input and output digests, DLL, message, step timings, CPU time and UTC completion are recorded; parent appends attempts to `ledger.jsonl`. [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L207-L240] [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L260-L304] [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L435-L449] |
| Extracted results    | adapter-defined channels                                         | `results/<case_id>.json` contains case/input/simulation identities, static information and channels; result bytes are hashed. [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L275-L285]                                                                                                                                                                                  |
| Campaign disposition | categorical status, case identifiers                             | Optional stop record carries disposition, failed/untouched identifiers, clearing condition and automatic-resume flag. [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L358-L384]                                                                                                                                                                                          |
| Retention records    | h; bytes; UTC timestamps                                         | `retention.jsonl` marks eligible files and records changed, absent or deleted files; sweep is destructive and is not a documentation check. [src/digitalmodel/solvers/orcaflex/sim_retention.py:L130-L169]                                                                                                                                                                      |
| Authoring artifacts  | YAML, JSON and Markdown                                          | Spec, assumptions, assumption ledger and intent files; optional generation delegates to the modular generator. [src/digitalmodel/solvers/orcaflex/spec_author.py:L449-L472]                                                                                                                                                                                                     |
| Catalogue            | HTML and SVG coordinates in input coordinate units               | Deterministically sorted cards; project-derived groups are excluded before rendering. [src/digitalmodel/solvers/orcaflex/model_catalogue.py:L78-L112]                                                                                                                                                                                                                           |
| Strength summary     | CSV; channel units UNVERIFIED                                    | The durable workflow declares `strength_loadcase_summary.csv` and requires a licence. [docs/registry/workflows.yaml:L380-L387]                                                                                                                                                                                                                                                  |

The catalogue renders only explicitly resolved global coordinates: free/fixed line
end connections qualify, other frames remain unresolved; plan projects X/Y,
elevation X/Z, and the SVG screen-Y sign is inverted.
[src/digitalmodel/solvers/orcaflex/model_catalogue_render.py:L161-L215]
[src/digitalmodel/solvers/orcaflex/model_catalogue_render.py:L218-L242]

UNVERIFIED: the top-level OPP output contract does not establish a checked unit
mapping for the five strength channels, the source model's coordinate origin,
angular phase convention or unit conversion; no numerical strength result is
qualified here (open item U1).
[src/digitalmodel/solvers/orcaflex/opp_range_graph.py:L48-L90]
[examples/workflows/orcaflex-strength-post/input.yml:L39-L51]

## 3. Method and governing standards

1. **Text preparation — offline.** `orcaflex_load` separates header lines and
   parses remaining text; blank scalars become empty text, explicit null tokens
   become `None`; dumping emits `~` for `None`, Yes/No for booleans, and
   preserves dictionary insertion order.
   [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L17-L60]
   [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L63-L115]
2. **Structural preflight — offline, separate from execution.**
   `OrcaFlexYAMLValidator.validate_file` composes source diagnostics before
   `safe_load`, detects flat/include/hybrid/fragment forms, and dispatches
   structural checks; it is not invoked by the static execution block.
   [src/digitalmodel/solvers/orcaflex/yaml_validator.py:L158-L231]
   [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L119-L132]
3. **Assignment-time check — offline.** `inspect_source` bounds YAML graph
   traversal, preserves diagnostics hidden by duplicate relevant properties and
   tracks preceding solution method/variable-step mode; includes reset known
   context, and unresolved inheritance yields a warning rather than native
   acceptance.
   [src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py:L22-L53]
   [src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py:L83-L141]
4. **Static route — licence required.** `run_single_model` constructs a model,
   calls `LoadData`, `CalculateStatics`, `check_statics` and
   `SaveSimulation`; it catches exceptions into an error field and records
   elapsed wall time. It does not call `RunSimulation`.
   [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L119-L143]
5. **Native parallel route — licence required.** `run_one` configures the
   binding/version, builds and hashes model inputs plus canonical case JSON,
   loads the model, then assigns the requested solver thread count, optionally prepares the model,
   solves statics, and runs dynamics only when the case requests it.
   [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L140-L151]
   [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L171-L179]
   [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L207-L258]
6. **Saved-result acceptance — licence required.** Native execution saves and
   reopens the simulation; statics requires `InStaticState`, dynamics requires
   `SimulationStopped` and current time no earlier than stop time minus
   0.000001 s. Extraction is from the reopened model, not the original model.
   [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L194-L204]
   [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L260-L291]
7. **State-name gate — offline logic, native callers require a licence.**
   `state_name` resolves enum names before string conversion;
   `check_state` rejects known disallowed and vendor-undefined states but
   permits indeterminate/unrecognized states. A returned value alone is not a
   complete run-evidence record.
   [src/digitalmodel/solvers/orcaflex/run_state.py:L101-L158]
8. **Post-processing — licence required for native results.** OPP loads model
   metadata and dispatches range graphs, summary, linked statistics and time
   series; high-level expansion is a separate pure configuration operation.
   [src/digitalmodel/solvers/orcaflex/opp.py:L59-L112]
   [src/digitalmodel/solvers/orcaflex/opp_config_expansion.py:L61-L106]
9. **Retention — offline file policy; production sweep is not executed.**
   Eligibility requires recorded input digest and DLL version, verifies result-file
   and simulation digests, and requires channel keys; marking adds a window, and sweep rechecks
   the simulation digest before deletion. That is reproducibility evidence, not
   an engineering validity check of channel values.
   [src/digitalmodel/solvers/orcaflex/sim_retention.py:L87-L107]
   [src/digitalmodel/solvers/orcaflex/sim_retention.py:L130-L169]

The OPP range-graph implementation applies a square root when the variable is
labelled "API STD 2RD Method 1"; UNVERIFIED: edition, clause title and a Citation sidecar are
not established by that branch (U1). No standard-derived threshold is reproduced.
Clause text not checked against the licensed copy (U1).
[src/digitalmodel/solvers/orcaflex/opp_range_graph.py:L76-L91]

Legacy installation generation emits elevation variants and orientation
overrides and declares replacement by `CampaignSpec`; the two umbilical
installation helpers alter line-length/reference-distance targets and write
step YAML.
[src/digitalmodel/solvers/orcaflex/orcaflex_installation.py:L11-L27]
[src/digitalmodel/solvers/orcaflex/orcaflex_installation.py:L29-L91]
[src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py:L21-L88]
[src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py:L21-L88]

## 4. Validated worked examples

No case below is classified as an independently validated engineering result;
these are synthetic or repository regression expectations, not solver
benchmarks. [tests/solvers/orcaflex/test_model_catalogue.py:L4-L7]
[tests/solvers/orcaflex/test_values_equal_bool_normalization.py:L34-L45]
[tests/solvers/orcaflex/test_yaml_validator_timestep.py:L40-L53]

| Class / case                    | Input and expectation                                                                                                                                                                           | Tolerance basis                                                        | Existing test node ID                                                                                                  | Last offline result at execution revision                                           |
| ------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------ |
| Regression / global chord       | Synthetic `fixtures/model_catalogue/coordinates.yml`; rendered endpoints equal the input endpoints, and object count is one Line and one Vessel                                                 | Exact equality to fixture, coordinate unit not independently qualified | `tests/solvers/orcaflex/test_model_catalogue.py::TestModelCatalogue::test_counts_and_global_chord`                     | PASS. [tests/solvers/orcaflex/test_model_catalogue.py:L37-L61]                 |
| Regression / bool normalization | True and Yes compare equal; difference is 0.0 %, significance COSMETIC                                                                                                                          | Exact comparator contract, not physical tolerance                      | `tests/solvers/orcaflex/test_values_equal_bool_normalization.py::TestBoolStringEquivalence::test_true_equals_yes_left` | PASS. [tests/solvers/orcaflex/test_values_equal_bool_normalization.py:L34-L45] |
| Regression / assignment context | Synthetic General block with preceding implicit method, enabled variable mode and maximum 0.125 as a raw input scalar (unit not established by this parser test); zero errors and zero warnings | Exact diagnostic counts; no solver acceptance claim                    | `tests/solvers/orcaflex/test_yaml_validator_timestep.py::test_known_enabled_context_accepts_positive_maximum[true]`    | PASS. [tests/solvers/orcaflex/test_yaml_validator_timestep.py:L40-L45]         |
| Regression / disabled mode      | Same maximum after disabled mode; at least one error, with no false unknown-property diagnosis                                                                                                  | Diagnostic presence, count at least 1                                  | `tests/solvers/orcaflex/test_yaml_validator_timestep.py::test_explicit_disabled_prefix_is_error[false]`                | PASS. [tests/solvers/orcaflex/test_yaml_validator_timestep.py:L48-L53]         |
| Regression / header boundary    | Synthetic directive and header; scanning stops at first data line                                                                                                                               | Exact empty finding list, 0 findings                                   | `tests/solvers/orcaflex/test_model_header_guard.py::TestModelHeaderGuard::test_header_boundary_and_directives`         | PASS. [tests/solvers/orcaflex/test_model_header_guard.py:L61-L64]              |

The native strength workflow is NOT_RUN: simulation loading and extraction
require a licensed interface; its offline registry test is skipped by its runtime
guard and is included in the execution totals below. Its test node is
`tests/workflows/test_durable_workflows.py::test_workflow_registry[orcaflex-strength-post]`.
It is not a validated case. [docs/registry/workflows.yaml:L380-L387]

### Offline execution record

The baseline used execution revision `591612b9aeba650a1f7e963e19292fea40c0c113`
(BASE), later than the retained code/citation pin. The initial batch recorded
511 passes and 71 runtime skips, plus two collection skips and the compatibility
collection error. Pytest 8.4.2 then raised an internal traceback-rendering error
after the last completed constructor/source-findings case and left one unnamed incomplete result;
that placeholder is excluded from completed test counts.
[tests/solvers/orcaflex/test_orcaflex_unit.py:L12-L24]
[tests/solvers/orcaflex/test_yaml_validator_timestep.py:L125-L128]

The 47 nodes without completed outcomes were selected in a continuation using
`--tb=native`: 46 passed and one retained its workflow guard. Combined baseline
outcomes are **557 passed, 0 failed, 72 runtime skipped across 629 unique nodes**;
no completed node was repeated. The five section-4 regression nodes each passed.
UNVERIFIED: the cause of the original test exception remains unresolved (U4);
the deep-YAML node passed in the continuation. The observed failing getter is
[pytest 8.4.2, TracebackEntry.lineno](https://github.com/pytest-dev/pytest/blob/8.4.2/src/_pytest/_code/code.py#L210-L212).

The initial command used `PYTHONPATH=src uv run --no-sources --with
pytest-timeout==2.4.0 python -m pytest <top-level test paths>
tests/reporting/test_engine_golden.py <strength-workflow registry node> -q -rA
-p no:cacheprovider --continue-on-collection-errors --basetemp=<scratch>
--junitxml=<scratch>`. The continuation used the same environment and pytest
options with the 17 remaining selectors and `--tb=native`. The assertions and
existing timeout marks were retained. [uv.lock:L5571-L5581]
[tests/solvers/orcaflex/test_yaml_validator_timestep.py:L125-L128]

The initial pytest exit status was 3; the continuation exit status was 0.
The collection error and internal reporting event remain recorded separately
from completed node outcomes. The AFTER command used the same form on the
uncommitted documentation tree at the same HEAD and completed all 629 nodes:
**557 passed, 0 failed and 72 runtime skipped**, with identical per-file outcomes
and skip messages. It retained the same two collection skips and one collection
error, giving pytest exit status 1. The internal reporting error did not recur;
no AFTER continuation was needed. All five section-4 cases passed in both phases.

At the same BASE revision, the configured tests-contracts roots recorded
**823 passed, 0 failed and 60 runtime skipped across 883 runtime nodes**;
one migration file was skipped at collection. The command also selected
`tests/orcaflex/test_generic_scr_slwr.py::test_analytical_reference_reproduces_from_basis`,
which passed: combined command totals were **824 passed and 60 runtime skipped**,
with pytest exit status 0. All **10** cases in
`tests/docs/test_digitalmodel_routing_contract.py` passed. The migration collector
reported "API STD 2RD migration incomplete - modules not yet implemented at
expected paths"; no executed case is claimed for that file.

The contract command used `PYTHONPATH=src uv run --no-sources --with
<configured dependency> --with-editable '.[test]' python -m pytest
<tests-contracts roots> <analytical-reference node> -rfE -p no:randomly
-p no:sugar --no-header -q --tb=line -rA -p no:cacheprovider
--basetemp=<scratch> --junitxml=<scratch>`. This is a separate command from the
37-file module matrix. AFTER used the same command form on the uncommitted
documentation tree at the same HEAD: **823 domain passes and 60 runtime skips**,
plus the same one migration collection skip; the supplementary node passed,
giving **824 combined passes**, pytest exit status 0 and **10 routing-contract
passes**. All 884 runtime outcomes and all non-passing messages match BASE.
The native strength workflow is NOT_RUN because it requires a licensed interface;
its registry-only test was skipped by its existing guard, included in the runtime
skip total. [docs/registry/workflows.yaml:L380-L387]
[tests/workflows/test_durable_workflows.py:L51-L54]


## 5. QA gates

Criteria below are acceptance requirements for future evidence, not claims that
the runner enforces AQG r0; NOT_RUN is not PASS. AQG r0, §§3–13.

| Gate / check | Applicable check and numeric pass criterion                                                                                                                                     | Implementation evidence and current limit                                                                                                                                                                                                                                                                                                            |
| ------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| G0.1 / C01   | Applicable: audit `validate_file` parse diagnostics against strict input fixtures; require 0 duplicate keys and 0 invalid UTF-8 files accepted (count). | `validate_file` reads UTF-8-sig and uses `safe_load`; `inspect_source` recognizes ambiguity for relevant timestep keys, not a universal duplicate-key refusal. UNVERIFIED: full strict-parser coverage (U1). [src/digitalmodel/solvers/orcaflex/yaml_validator.py:L158-L203] [src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py:L46-L53] |
| G0.2 / C02   | Applicable: round-trip blank/default fixtures through `orcaflex_load` and `orcaflex_dump`; require 0 unintended null/default changes (count). | Loader/dumper distinguish blank text from explicit null and emit tilde defaults; blanket acceptance of arbitrary null positions is not established. [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L17-L60] [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L63-L115]                                                                                                                                        |
| G0.3 / C03   | Applicable: exercise `_maximum_issue` and `_mapping_issues` on contradictory timestep assignments; require 0 accepted contradictions (count). | `_maximum_issue` and `_mapping_issues` check supported preceding method/mode; general selector-table completeness is UNVERIFIED (U1). [src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py:L83-L112]                                                                                                                                       |
| G0.4 / C04   | Applicable: compare `validate_file` diagnostics with target-version data-name and object-reference fixtures; require 0 unresolved references and 0 unsupported data names accepted (count). | Structural validation dispatches checks but no version-exported name table is supplied in this dispatch; full target-version coverage is UNVERIFIED (U1). [src/digitalmodel/solvers/orcaflex/yaml_validator.py:L296-L327]                                                                                                                            |
| G0.5 / C05   | Applicable: check `validate_file` input-basis diagnostics and OPP channel unit records; require 0 unit/design-basis mismatches and 0 unitless extracted channels (count). | WaterDepth check only checks numeric type; OPP copies range arrays without a unit record. Full range and channel-unit qualification is UNVERIFIED (U1). [src/digitalmodel/solvers/orcaflex/yaml_validator.py:L450-L458] [src/digitalmodel/solvers/orcaflex/opp_range_graph.py:L76-L90]                                                               |
| G0.6 / C06   | Applicable: challenge `validate_file` with fixtures whose independent arc-length error exceeds 0.1 %, endpoint error exceeds 0.01 m, or sections mismatch; require 0 `valid=True` outcomes for these violations (count). | `validate_file` returns `ValidationResult(valid=...)` and dispatches structural checks; full starting-shape acceptance remains UNVERIFIED (U1). [src/digitalmodel/solvers/orcaflex/yaml_validator.py:L158-L173] [src/digitalmodel/solvers/orcaflex/yaml_validator.py:L296-L327] |
| G0.7 / C07   | Applicable: compare `input_digest` with a closed model/include/case fingerprint; require 0 mismatches (count). | `input_digest` hashes files below the master folder plus case JSON; external referenced files are not automatically resolved by this loop. AQG input-closure equivalence is UNVERIFIED (U1). [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L140-L151]                                                                                        |
| G0.8 / C08   | Applicable: audit `run_one` records for generator, template and basis revisions; require 0 missing revision fields (count). | Initial native record lists input digest and DLL but not those three provenance revisions. UNVERIFIED: complete provenance record (U2). [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L207-L240]                                                                                                                                                     |
| G1 / C09 | Applicable: for each model loaded by `run_single_model` or `run_one`, require exact target version, load duration in s, 0 errors and 0 warnings (count), and 0 missing route/message fields (count). | AQG r0 G1.2 qualification route: the OrcaFlex demo under Wine only when its version equals the target version; otherwise `LoadData` through the licensed Python interface. The checked load objects are `run_one` and `run_single_model`. Implemented licensed OrcaFlex routes: `run_single_model` uses `Model()` then `LoadData`; `run_one` uses `Model(master)` and checks DLL version. Neither establishes a complete AQG route/message record. NOT_RUN: the matching-version demo binary or licensed interface is required; gap U2. [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L119-L143] [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L222-L240] |
| G2 / C10 | Applicable: every `run_one` record with `status=ok` and `analysis=statics` must follow `_verify` on the reopened `.sim` in `InStaticState`; require 0 false-ok records and 0 state-verification exceptions among accepted records (count). | `_verify` rejects every other static state before extraction and status assignment. Native readback is NOT_RUN because it requires a licence. Independent hand checks, offset sweeps, component completeness and static images remain UNVERIFIED (U2). [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L194-L199] [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L268-L291] |
| G3 / C11 | Applicable: every `run_one` dynamics record with `status=ok` must follow `_verify` on the reopened `.sim` in `SimulationStopped`, with `CurrentTime >= StopTime - 0.000001 s`; require 0 falsely accepted records (count). | `_verify` checks state and time before extraction; complete warnings, finite-channel screening and physical comparison remain UNVERIFIED (U2). NOT_RUN: native dynamics readback requires a licence. [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L194-L204] [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L268-L291] |
| G3.3 / C12 | Applicable: for each accepted `run_one` dynamics record, divide `timings_s[total]` (s) by positive simulated duration (s); record wall time per simulated second in s/s, with 0 missing ratios and 0 missing end-image digests (count). | The final record writes total/per-step `timings_s` and `cpu_s`, but does not write simulated duration, the ratio or an end-image digest. UNVERIFIED: complete G3.3 evidence (U2). [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L300-L304] |
| G4 / C13     | Conditional: for a `check_cases` request using new diffraction-derived data, require 1 cited passing G4 record per imported data set and 0 missing records (count). | NOT_APPLICABLE to these text-only regression examples, which import no new diffraction results; native case admission does not check a G4 record (U2). [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L113-L126]                                                                                                                              |
| G5 / C14 | Applicable to issued analysis: every `results/<case_id>.json` from `run_one` must contain `input_sha256` and `sim_sha256` equal to its passing QA evidence record, with recomputed result-file SHA-256 equal to `results_sha256` in its run record; require 0 missing or mismatched digests and 0 untraced result files (count). | Extraction writes both input/simulation identities and records the result-file digest. Native `status=ok` is not AQG PASS; linkage to signed evidence and the generated appendix remains a proposed acceptance check (U2). UNVERIFIED: linkage to signed report evidence is not established (U2). [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L279-L291] |

The G0 and G3.3 acceptance requirements above refer to AQG r0 §§4 and 7, not to a
licensed standard; C06 comes from AQG r0 §4 G0.6.

### Queue refusals

Each proposed refusal check will submit one malformed evidence record to a
future AQG admission check around `check_cases` and require **1 refusal / 1 request**, with **0 accepted invalid
requests**; a valid control will require **1 admission / 1 request**.
These are defined checks, not implemented-test results. AQG r0 §12.

| Refusal / check | Malformed evidence to detect                              | Current support or gap                                                                                                                                           |
| --------------- | --------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| B1 / C15        | Missing model evidence record                             | UNVERIFIED: `check_cases` does not require an AQG evidence record (U2). [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L113-L126]                         |
| B2 / C16        | Fingerprint differs at admission                          | UNVERIFIED: worker digest is computed after admission, not compared to an approved record (U2). [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L227-L240] [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L113-L126] [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L387-L405] |
| B3 / C17        | Required gate FAIL/NOT_RUN or unexplained NOT_APPLICABLE  | UNVERIFIED: case admission validates identifiers/analysis/serialization, not gate status (U2). [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L113-L126]  |
| B4 / C18        | Record version differs from requested program             | Worker DLL mismatch returns `version_drift`; AQG record-version admission is UNVERIFIED (U2). [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L222-L225] [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L113-L126]   |
| B5 / C19        | Missing or changed required image                         | UNVERIFIED: no image field checked by case admission (U2). [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L113-L126]                                      |
| B6 / C20        | Missing/unapproved signature or producer signs own record | UNVERIFIED: no signature/provider-independence check in case admission (U2). [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L113-L126]                    |
| B7 / C21        | Open deviation without disposition                        | UNVERIFIED: no deviation check in case admission (U2). [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L113-L126]                                          |
| B8 / C22        | Superseded AQG revision                                   | UNVERIFIED: no standard-revision check in case admission (U2). [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L113-L126]                                  |
| B9 / C23        | No passing representative for configuration/sample        | UNVERIFIED: no representative lookup in case admission (U2). [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L113-L126]                                    |

### Supplementary module checks

| Check | Gate   | Execution and numeric pass criterion                                                                                                                                                       | Evidence                                                                                                                                                                                                                                  |
| ----- | ------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| S01   | G0     | Statically enumerate conditional skip mechanisms and resolve fixture roots; 0 unaccounted mechanisms and 0 unexplained missing-fixture skips                                               | Source reason inventory and offline test outcomes; missing-fixture skip behavior is explicit. [tests/solvers/orcaflex/format_converter/conftest.py:L26-L55]                                                                               |
| S02 | G0 | Collect and execute each top-level offline test file explicitly; require 0 collection errors (count), including compatibility exports, and 0 whole-file collection skips hiding pure-Python cases and 0 native-binding-dependent skips among filesystem/YAML-only checks (count) and 0 files hidden from the recorded matrix (count); require 1 completed outcome for `test_deep_yaml_returns_diagnostic_before_recursive_parser_failure_escapes`, 0 unnamed outcomes and 0 internal reporting errors (counts). | Direct collection exposes missing imports even when recursive collection excludes the file. [tests/solvers/orcaflex/test_orcaflex_unit.py:L12-L24] [tests/conftest.py:L14-L29] [tests/solvers/orcaflex/test_include_library.py:L272-L275] [tests/solvers/orcaflex/test_yaml_validator_timestep.py:L125-L128] [tests/solvers/orcaflex/test_hybrid_templates.py:L224-L318] |
| C24   | G3     | Proposed monitor regression: inject 1 event after an awaited initial scan and 1 callback from a non-loop thread; require 1 processed event, 0 terminated consumers and 0 scheduling errors | Gap tracked by the monitor incident; no executed detection test is claimed. [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L220-L243] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L280-L320] |
| C25   | G3     | Compare completed, failed and unrun native records with campaign disposition; 0 false-complete classifications and 0 missing clearing conditions                                           | Proposed disposition/health contract check; mapping alone is not output-advancement evidence. [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L358-L384]                                                                            |
| C26 | G1 | With no native binding and no explicit mock opt-in, exercise `process_single_file_worker`; require 0 `mock_success` records and 0 claimed `output_files` entries (count). | The unavailable branch assigns both fields; UNVERIFIED: an offline branch-specific regression (U5). The separate `run_to_sim` explicit-mock file-existence test is included in the offline matrix. [src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel_v2.py:L63-L80] [tests/solvers/orcaflex/test_mock_is_explicit.py:L52-L65] |
| C27   | G2, G3 | Audit every claimed benchmark for an independent expected value, input revision and native result; 0 unsupported validation claims                                                         | No native engineering benchmark is established by the synthetic text tests. [tests/solvers/orcaflex/test_model_catalogue.py:L4-L7]                                                                                                        |
| C28   | G0     | Run catalogue determinism/freshness tests; 0 byte differences between repeated builds and the committed page                                                                               | Existing regression test. [tests/solvers/orcaflex/test_model_catalogue.py:L356-L364]                                                                                                                                                      |
| C29   | G0     | Run header and duplicate-digest tests; 0 forbidden header findings and 0 failed digest-equality assertions                                                                                 | Existing text-only guards. [tests/solvers/orcaflex/test_model_header_guard.py:L48-L103]                                                                                                                                                   |
| C30 | G2 | Proposed legacy statics regression: drive `run_static_analysis` with a mock whose second `CalculateStatics` raises; require `run_success_flag=False` and 0 false-success records (count). | The helper catches the second-solve exception and returns before its caller sets the flag true; this proposed detection has not been executed (U13). [src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py:L217-L242] |

## 6. Known failure modes and pitfalls
sweep: not run (legacy pack)

| Symptom / cause                                                                       | Detection and limit                                                                                                                                                                                                                                                                                                                                                                                                    | Incident                                                                                        |
| ------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------- |
| Failed second statics solve is reported as success | C30: proposed detection: drive `run_static_analysis` with a mock model whose second `CalculateStatics` raises; require `run_success_flag=False` and 0 false-success records (count). `analysis_with_calculated_positions` catches the failure and returns, after which its caller sets the flag true. This detection has not been executed. [src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py:L217-L242] | False-success defect (#2548), U13 |
| Pytest traceback rendering interrupts offline completion | S02: compare completed node IDs with the collected set; require 0 missing/unnamed outcomes and 0 internal reporting errors (counts). The initial batch reporting interruption and successful native-traceback continuation are recorded in section 4; the original test exception cause is UNVERIFIED (U4). [tests/solvers/orcaflex/test_yaml_validator_timestep.py:L125-L128] | issue pending (5), U4 |
| Fixture miss becomes skip instead of failure                                          | S01 below: resolve every fixture root and count skipped tests; require 0 unexplained missing-fixture skips. Historical issue count 34 is not asserted as a current run result. [tests/solvers/orcaflex/format_converter/conftest.py:L8-L55]                                                                                                                                                                   `test_load_orcaflex_files.py` adds 8 fixture-root skips: its data root is `docs/modules/orcaflex`. [tests/solvers/orcaflex/test_load_orcaflex_files.py:L26-L31] | Deleted fixture roots (#2008)              |
| Compatibility-unit tests fail at collection | S02: explicit collection reports an ImportError for `__version__` from `digitalmodel.orcaflex`; no executed test case is claimed for this file. [tests/solvers/orcaflex/test_orcaflex_unit.py:L12-L24] | Collection visibility (#1923) |
| Frequency-domain strength false fail                                                  | Existing `sample_line` accesses Min/Max; no detection check exists in this pack for the frequency-domain false-fail case. A future regression must pass a frequency-domain `.sim` through the strength-variable check with 0 false failures (count). [scripts/validate_orcaflex_sim.py:L58-L76]                                                                                                                                                                                                                               | Frequency-domain strength check (#2325)    |
| Native-interface guards hide pure checks | S02: require 0 whole-file collection skips hiding pure cases and 0 native-binding-dependent skips among filesystem/YAML-only checks (count). The include-library decorator stops module collection; library-component and directory-structure classes are also native-gated. Their pure outcomes remain unestablished. [tests/solvers/orcaflex/test_include_library.py:L272-L275] [tests/solvers/orcaflex/test_hybrid_templates.py:L224-L318] | issue pending (3), U11 |
| File monitor consumer exits before start completes; event callback uses task creation | No detection check exists in the executed subset; proposed C24: start with an awaited initial scan, enqueue 1 event and require 1 processed event plus 0 terminated consumers; invoke 1 callback from a non-loop thread and require 0 scheduling errors. [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L220-L243] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L280-L320] | File-monitor race (#1925)                  |
| Liveness mistaken for qualified progress; continuation lacks disposition              | C25: require 0 false completed statuses and 0 missing clearing conditions; optional stop-record code maps native statuses and records disposition. Full output-advancement health and criteria agreement are not established by that record. [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L358-L384]                                                                                                          | Continuation and progress contract (#2298) |
| Optimized fallback reports a simulation that was not written | C26 will exercise `process_single_file_worker` with no native binding and require 0 implicit mock-success records and 0 claimed simulation paths (count). The pinned branch sets `status=mock_success` and `output_files=["mock_output.sim"]`; UNVERIFIED: a branch-specific offline detection regression (U5). This check does not itself require a licence. [src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel_v2.py:L63-L80] | Implicit mock output (#2521) |
| YAML default stability and batch failure exit status | C02: require 0 unintended default changes (count). `run_to_sim_cli.main` exits with code 1 when `results["failed"] > 0`; UNVERIFIED: an executed regression for that failed-batch-to-exit mapping (U4). [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L17-L60] [src/digitalmodel/solvers/orcaflex/run_to_sim_cli.py:L123-L148] | YAML defaults and batch failure propagation (#2051) |
| Catenary reference geometry and elastic-stretch qualification | C27: require 0 accepted hand-check references without independently reproduced geometry and full tension-dependent stretch (count). The pinned fixture records reconciliation and the distinction between elongation and horizontal elastic displacement; this pack does not qualify a catenary implementation as an independent hand-check reference (U3). [tests/fixtures/test_vectors/mooring_risers/mooring_catenary_solver.yaml:L1-L20] [tests/fixtures/test_vectors/mooring_risers/mooring_catenary_solver.yaml:L29-L40] | Catenary reference basis (#2050) |
| Unknown state falls through the generic state gate | C10/C11 require native state/time readback, not merely absence of an exception; unrecognized states pass through. [src/digitalmodel/solvers/orcaflex/run_state.py:L150-L158] | QA evidence (#2520) |

### Model-library work references

These references define follow-up checks; they do not record engineering acceptance.

| Issue                                                                                  | Detection check or explicit gap                                                                                                                                                                                                                  |
| -------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Library programme (#2421)         | C27: require 0 benchmark claims without an independent target and native result; no library-wide qualification check exists in this pack.                                                                                                        |
| Catalogue (#2422)                 | Existing catalogue count/chord, deterministic-build and freshness tests; C28 requires 0 byte differences on repeated builds. [tests/solvers/orcaflex/test_model_catalogue.py:L37-L92] [tests/solvers/orcaflex/test_model_catalogue.py:L356-L364] |
| Floating wind example (#2423)     | C27; no end-to-end detection check exists in the executed subset.                                                                                                                                                                                |
| Jumper modal benchmark (#2424)    | C27; no independently validated modal run is recorded here.                                                                                                                                                                                      |
| Provenance and duplicates (#2425) | Existing header, digest and provenance tests; C29 requires 0 forbidden header findings and exact digest comparisons. [tests/solvers/orcaflex/test_model_header_guard.py:L48-L80] [tests/solvers/orcaflex/test_model_header_guard.py:L90-L103]    |
| Generic mooring workflow (#2426)  | C10/C11/C27; no end-to-end detection check exists in the executed subset.                                                                                                                                                                        |
| Splash-zone installation (#2427)  | C10/C11/C27; no end-to-end detection check exists in the executed subset.                                                                                                                                                                        |
| Load/statics attestation (#2428)  | C09/C10; no native attestation check was run.                                                                                                                                                                                                    |
| Riser comparison (#2429)          | C10/C11/C27; no end-to-end detection check exists in the executed subset.                                                                                                                                                                        |
| Fixed-bottom turbine (#2430)      | C27; no end-to-end detection check exists in the executed subset.                                                                                                                                                                                |
| Pipelay workflow (#2431)          | C10/C11/C27; no end-to-end detection check exists in the executed subset. Existing campaign-document tests check evidence terms and neutral roles, not physical pipe response. [tests/solvers/orcaflex/test_slay_campaign_plans.py:L34-L51]                                                                                |
| Buoy mooring (#2432)              | C10/C11/C27; no end-to-end detection check exists in the executed subset.                                                                                                                                                                        |
| Drilling riser (#2433)            | C10/C11/C27; no end-to-end detection check exists in the executed subset.                                                                                                                                                                        |
| Berth mooring (#2434)             | C10/C11/C27; no end-to-end detection check exists in the executed subset.                                                                                                                                                                        |
| Wind sizing chain (#2435)         | C27; no end-to-end detection check exists in the executed subset.                                                                                                                                                                                |

### Skip and expected-failure inventory

S01 statically enumerates every skip, skipif, importorskip and xfail
mechanism recursively under the test folder; the reason expressions below are
quoted source expressions, not expanded local paths. Conditional mechanisms
are not counted as observed skips unless execution reports them.
Static enumeration at the pinned revision found 134 conditional skip/xfail/import-skip call sites; the table covers mechanism source locations, fixture scopes and statically associated test definitions.

#### `tests/solvers/orcaflex/format_converter/conftest.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `a01_single_file`; `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIDetect::test_detect_single_file`, `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISingle2Modular::test_convert`, `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISingle2Modular::test_no_output_uses_default`, `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISingle2Spec::test_convert`, `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIAuto::test_auto_single_to_modular`, `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_single_file`, `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_real_a01`, `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_convert_real_a01_file`, `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSingleToSpecConverter::test_convert_real_a01`, `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSingleToSpecConverter::test_extracted_spec_has_environment`, `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSingleToSpecConverter::test_report_metadata`, `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexLoad::test_load_real_a01_file` | `f'Test data not found: {p}'` | `pytest.skip` [tests/solvers/orcaflex/format_converter/conftest.py:L31-L31] |
| `a01_modular_dir`; `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIDetect::test_detect_modular_dir`, `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_modular_directory`, `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_from_directory` | `f'Test data not found: {p}'` | `pytest.skip` [tests/solvers/orcaflex/format_converter/conftest.py:L40-L40] |
| `spec_file`; `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIDetect::test_detect_spec_file`, `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISpec2Modular::test_convert`, `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_spec_file`, `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_convert_creates_modular_structure`, `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_convert_generates_includes`, `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_report_metadata`, `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_init_with_spec_path`, `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_convert_creates_single_file`, `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_output_has_orcaflex_sections`, `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_report_metadata` | `f'Test data not found: {p}'` | `pytest.skip` [tests/solvers/orcaflex/format_converter/conftest.py:L55-L55] |

#### `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py::TestBackwardCompatSingleRunFloating::test_backward_compat_single_run_floating` | `f'Floating spec not found: {FLOATING_SPEC}'` | `pytest.mark.skipif` [tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py:L451-L454] |
| `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py::TestBackwardCompatSingleRunSlay::test_backward_compat_single_run_slay` | `f'S-lay spec not found: {SLAY_SPEC}'` | `pytest.mark.skipif` [tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py:L493-L496] |

#### `tests/solvers/orcaflex/modular_generator/schema/test_equipment_compat.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `tests/solvers/orcaflex/modular_generator/schema/test_equipment_compat.py::TestEquipmentRollerCompat::test_slay_spec_loads_without_error` | `'S-lay spec not available'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/schema/test_equipment_compat.py:L117-L117] |

#### `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `generated_includes`; `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py::TestJumperLineTypes::test_three_jumper_variants_emitted`, `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py::TestJumperLineTypes::test_wcoat_coating_properties_preserved`, `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py::TestJumperLineTypes::test_wbuoy_and_wstrake_reference_named_variable_data`, `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py::TestJumperLineTypes::test_ocs_200v_connector_mass_per_length`, `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py::TestJumperLineReferences::test_jumper_line_present`, `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py::TestJumperLineReferences::test_jumper_line_uses_all_three_variants`, `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py::TestJumperLineReferences::test_every_line_linetype_reference_resolves`, `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py::TestCraneConstraint::test_crane_boom_base_constraint_emitted`, `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py::TestEnvironmentAndSimulation::test_water_depth_and_density_carried_through`, `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py::TestEnvironmentAndSimulation::test_stage_durations_preserved_in_parameters`, `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py::TestMasterIncludesWired::test_master_includes_generic_objects_and_environment` | `f'Canonical jumper spec not present: {SPEC_PATH}'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py:L65-L65] |

#### `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `requires_orcaflex`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_loads_in_orcaflex`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_pipeline_exists`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_results_are_finite`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_pipeline_length`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestInstantRiser::test_a01_riser_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestInstantRiser::test_a01_riser_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestInstantRiser::test_a01_riser_has_line`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestInstantRiser::test_a01_riser_tension_is_finite`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestLazyWaveRiser::test_lazy_wave_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestLazyWaveRiser::test_lazy_wave_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestLazyWaveRiser::test_lazy_wave_has_line`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestLazyWaveRiser::test_lazy_wave_tension_is_finite`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestFenderModel::test_fender_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestFenderModel::test_fender_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestFenderModel::test_fender_has_constraints`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_has_line`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_tension_is_finite`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestSmokeGeneration::test_monolithic_yml_loads_in_orcaflex`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestModularStatics::test_modular_loads_in_orcaflex`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestModularStatics::test_modular_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestStaticResultExtraction::test_modular_results_are_finite`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestStaticResultExtraction::test_monolithic_results_are_finite`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestStaticResultExtraction::test_modular_pipeline_length`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestStaticResultExtraction::test_monolithic_pipeline_length`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestModularVsMonolithicComparison::test_end_a_effective_tension`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestModularVsMonolithicComparison::test_end_b_effective_tension`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestModularVsMonolithicComparison::test_end_a_bending_moment`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestModularVsMonolithicComparison::test_end_b_bending_moment`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestModularVsMonolithicComparison::test_object_count_difference_explained`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestCrossValidation30in::test_30in_monolithic_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestCrossValidation30in::test_30in_modular_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestCrossValidation30in::test_30in_tension_comparison`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestAlongLengthNoLoadComparison::test_noload_modular_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestAlongLengthNoLoadComparison::test_noload_monolithic_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestAlongLengthNoLoadComparison::test_noload_range_results_are_finite`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestAlongLengthNoLoadComparison::test_noload_tension_profile_comparison`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestAlongLengthNoLoadComparison::test_noload_bending_moment_profile_comparison` | `'OrcFxAPI not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L35-L37] |
| `_skip_if_no_sim`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestCrossValidation30in::test_30in_monolithic_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestCrossValidation30in::test_30in_modular_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestCrossValidation30in::test_30in_tension_comparison` | `f'{label} .sim not found: {sim_path}. Generate offline with: model.CalculateStatics(); model.SaveSimulation(path)'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L236-L240] |
| `qa_output_dir`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_generates_master_yml`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_generates_includes` | `f'QA spec not found: {SPEC_24IN_QA}'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L271-L271] |
| `qa_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_loads_in_orcaflex`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_pipeline_exists`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_results_are_finite`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_pipeline_length` | `'OrcFxAPI not available'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L286-L286] |
| `a01_riser_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestInstantRiser::test_a01_riser_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestInstantRiser::test_a01_riser_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestInstantRiser::test_a01_riser_has_line`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestInstantRiser::test_a01_riser_tension_is_finite` | `'OrcFxAPI not available'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L397-L397] |
| `a01_riser_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestInstantRiser::test_a01_riser_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestInstantRiser::test_a01_riser_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestInstantRiser::test_a01_riser_has_line`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestInstantRiser::test_a01_riser_tension_is_finite` | `f'A01 riser not found: {A01_RISER}'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L399-L399] |
| `lazy_wave_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestLazyWaveRiser::test_lazy_wave_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestLazyWaveRiser::test_lazy_wave_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestLazyWaveRiser::test_lazy_wave_has_line`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestLazyWaveRiser::test_lazy_wave_tension_is_finite` | `'OrcFxAPI not available'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L467-L467] |
| `lazy_wave_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestLazyWaveRiser::test_lazy_wave_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestLazyWaveRiser::test_lazy_wave_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestLazyWaveRiser::test_lazy_wave_has_line`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestLazyWaveRiser::test_lazy_wave_tension_is_finite` | `f'A01 Lazy Wave not found: {A01_LAZY_WAVE}'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L469-L469] |
| `fender_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestFenderModel::test_fender_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestFenderModel::test_fender_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestFenderModel::test_fender_has_constraints` | `'OrcFxAPI not available'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L480-L480] |
| `fender_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestFenderModel::test_fender_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestFenderModel::test_fender_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestFenderModel::test_fender_has_constraints` | `f'C09 Fenders not found: {C09_FENDERS}'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L482-L482] |
| `pull_in_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_has_line`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_tension_is_finite` | `'OrcFxAPI not available'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L493-L493] |
| `pull_in_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_has_line`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_tension_is_finite` | `f'D02 Pull-in not found: {D02_PULL_IN}'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L495-L495] |
| `modular_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestModularStatics::test_modular_loads_in_orcaflex`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestModularStatics::test_modular_statics_converge` | `'OrcFxAPI not available'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L663-L663] |
| `monolithic_model` | `'OrcFxAPI not available'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L688-L688] |
| `modular_noload_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestAlongLengthNoLoadComparison::test_noload_modular_statics_converge` | `'OrcFxAPI not available'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L710-L710] |
| `monolithic_noload_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestAlongLengthNoLoadComparison::test_noload_monolithic_statics_converge` | `'OrcFxAPI not available'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L732-L732] |
| `monolithic_noload_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestAlongLengthNoLoadComparison::test_noload_monolithic_statics_converge` | `f'Monolithic YAML not found: {MONOLITHIC_24IN_YML}'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L740-L740] |
| `qa_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_loads_in_orcaflex`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_pipeline_exists`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_results_are_finite`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_pipeline_length` | `f'QA master.yml failed to load: {exc}'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L293-L293] |
| `modular_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestModularStatics::test_modular_loads_in_orcaflex`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestModularStatics::test_modular_statics_converge` | `f'Modular master.yml not loadable by OrcFxAPI — generator output needs fixing: {exc}'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L671-L674] |
| `modular_noload_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestAlongLengthNoLoadComparison::test_noload_modular_statics_converge` | `f'Modular master.yml not loadable: {exc}'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L717-L717] |
| `qa_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_loads_in_orcaflex`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_pipeline_exists`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_results_are_finite`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestQuickQA::test_qa_pipeline_length` | `f'Modular generator statics non-convergence (Task #9). Error: {exc}'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L299-L302] |
| `pull_in_model`; `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_loads`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_statics_converge`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_has_line`, `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py::TestPullInAnalysis::test_pull_in_tension_is_finite` | `f'D02 Pull-in YAML incompatible with installed OrcFxAPI: {exc}'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L502-L504] |

#### `tests/solvers/orcaflex/modular_generator/test_riser_generation.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `TestRiserOrcaFlexLoad`; `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserOrcaFlexLoad::test_generated_riser_loads_in_orcaflex` | `'OrcFxAPI not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/modular_generator/test_riser_generation.py:L428-L428] |
| `TestPliantWaveOrcaFlexConvergence`; `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestPliantWaveOrcaFlexConvergence::test_pliant_with_tether_loads_in_orcaflex`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestPliantWaveOrcaFlexConvergence::test_pliant_with_tether_statics_converge` | `'OrcFxAPI not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/modular_generator/test_riser_generation.py:L462-L462] |
| `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserSpecLoading::test_load_a01_catenary_spec` | `'A01 catenary riser spec not found'` | `pytest.mark.skipif` [tests/solvers/orcaflex/modular_generator/test_riser_generation.py:L156-L159] |
| `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserSpecLoading::test_spec_model_type_methods` | `'A01 catenary riser spec not found'` | `pytest.mark.skipif` [tests/solvers/orcaflex/modular_generator/test_riser_generation.py:L174-L177] |
| `TestRiserModularGeneration::catenary_spec`; `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserModularGeneration::test_generate_riser_modular_output`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserModularGeneration::test_generated_line_types_content`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserModularGeneration::test_generated_vessel_content`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserModularGeneration::test_generated_lines_content`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserOrcaFlexLoad::test_generated_riser_loads_in_orcaflex` | `'A01 catenary riser spec not found'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_riser_generation.py:L197-L197] |
| `TestRiserLinkBuilderGeneration::pliant_spec`; `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserLinkBuilderGeneration::test_pliant_spec_has_links`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserLinkBuilderGeneration::test_pliant_generates_link_file`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserLinkBuilderGeneration::test_pliant_link_connections_format`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestPliantWaveOrcaFlexConvergence::test_pliant_with_tether_loads_in_orcaflex`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestPliantWaveOrcaFlexConvergence::test_pliant_with_tether_statics_converge` | `'A01 pliant wave riser spec not found'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_riser_generation.py:L350-L350] |
| `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserLinkBuilderGeneration::test_catenary_has_no_link_file` | `'A01 catenary riser spec not found'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_riser_generation.py:L407-L407] |
| `TestRiserOrcaFlexLoad::catenary_spec`; `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserModularGeneration::test_generate_riser_modular_output`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserModularGeneration::test_generated_line_types_content`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserModularGeneration::test_generated_vessel_content`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserModularGeneration::test_generated_lines_content`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserOrcaFlexLoad::test_generated_riser_loads_in_orcaflex` | `'A01 catenary riser spec not found'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_riser_generation.py:L436-L436] |
| `TestPliantWaveOrcaFlexConvergence::pliant_spec`; `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserLinkBuilderGeneration::test_pliant_spec_has_links`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserLinkBuilderGeneration::test_pliant_generates_link_file`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserLinkBuilderGeneration::test_pliant_link_connections_format`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestPliantWaveOrcaFlexConvergence::test_pliant_with_tether_loads_in_orcaflex`, `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestPliantWaveOrcaFlexConvergence::test_pliant_with_tether_statics_converge` | `'A01 pliant wave riser spec not found'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_riser_generation.py:L470-L470] |

#### `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `TestRoundtripPreservesLineTypes`; `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesLineTypes::test_line_type_names_preserved`, `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesLineTypes::test_line_type_od_values_match`, `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesLineTypes::test_line_type_ea_values_match`, `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesLineTypes::test_line_type_mass_values_match`, `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesLineTypes::test_properties_bag_values_emitted` | `'a01_catenary_riser spec.yml not found'` | `pytest.mark.skipif` [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L175-L178] |
| `TestRoundtripPreservesEnvironment`; `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesEnvironment::test_water_depth_matches`, `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesEnvironment::test_wave_type_matches`, `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesEnvironment::test_wave_height_matches` | `'a01_catenary_riser spec.yml not found'` | `pytest.mark.skipif` [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L267-L270] |
| `TestRoundtripPreservesVessels`; `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesVessels::test_vessel_names_preserved`, `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesVessels::test_vessel_type_names_preserved` | `'a01_catenary_riser spec.yml not found'` | `pytest.mark.skipif` [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L321-L324] |
| `TestRoundtripExtraction`; `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripExtraction::test_extracted_spec_water_depth_matches`, `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripExtraction::test_extracted_spec_wave_height_matches` | `'a01_catenary_riser spec.yml not found'` | `pytest.mark.skipif` [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L359-L362] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesVessels::test_vessel_names_preserved` | `'No vessels in catenary riser spec'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L332-L332] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesVessels::test_vessel_type_names_preserved` | `'No vessel types in catenary riser spec'` | `pytest.skip` [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L349-L349] |

#### `tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `module`; `test_mooring_tension_iteration`, `test_single_line_iteration`, `test_multi_line_coupling` | `'input ymls relocated off-repo at repo slimming; restore tracked in #1318'` | `pytest.mark.skipif` [tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py:L26-L29] |

#### `tests/solvers/orcaflex/test_environment_defaults_vs_orcfxapi.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `module`; `tests/solvers/orcaflex/test_environment_defaults_vs_orcfxapi.py::TestEnvironmentDefaultsAgainstOrcFxAPI::test_default_matches_or_is_documented_override`, `tests/solvers/orcaflex/test_environment_defaults_vs_orcfxapi.py::TestNoUndocumentedDefaultDrift::test_expected_overrides_have_rationales`, `tests/solvers/orcaflex/test_environment_defaults_vs_orcfxapi.py::TestNoUndocumentedDefaultDrift::test_expected_overrides_keys_are_in_defaults` | `"OrcFxAPI not installed (Windows + licensed OrcaFlex required)" [remaining restricted wording omitted]` | `pytest.importorskip` [tests/solvers/orcaflex/test_environment_defaults_vs_orcfxapi.py:L93-L97] |
| `tests/solvers/orcaflex/test_environment_defaults_vs_orcfxapi.py::TestEnvironmentDefaultsAgainstOrcFxAPI::test_default_matches_or_is_documented_override` | `f'{prop_name!r} not accessible on blank Environment (likely mode-gated): {exc}'` | `pytest.skip` [tests/solvers/orcaflex/test_environment_defaults_vs_orcfxapi.py:L154-L157] |

#### `tests/solvers/orcaflex/test_examples_downloader.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `tests/solvers/orcaflex/test_examples_downloader.py::TestDownloaderIntegration::test_real_website_connection` | `'Integration test - requires internet connection'` | `unittest.skip` [tests/solvers/orcaflex/test_examples_downloader.py:L301-L301] |
| `tests/solvers/orcaflex/test_examples_downloader.py::TestDownloaderIntegration::test_parse_real_categories` | `'Integration test - requires internet connection'` | `unittest.skip` [tests/solvers/orcaflex/test_examples_downloader.py:L312-L312] |

#### `tests/solvers/orcaflex/test_hybrid_templates.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `TestHybridTemplates`; `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_load`, `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_converge`, `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_pipeline_base_models_load`, `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_pipeline_base_models_converge`, `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_umbilical_base_models_load`, `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_umbilical_base_models_converge`, `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_subsea_base_models_load`, `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_subsea_base_models_converge`, `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_mooring_base_models_load`, `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_mooring_base_models_converge`, `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge` | `'OrcFxAPI not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `TestLibraryComponents`; `tests/solvers/orcaflex/test_hybrid_templates.py::TestLibraryComponents::test_line_types_exist`, `tests/solvers/orcaflex/test_hybrid_templates.py::TestLibraryComponents::test_buoy_types_exist`, `tests/solvers/orcaflex/test_hybrid_templates.py::TestLibraryComponents::test_line_type_files_are_valid_yaml` | `'OrcFxAPI not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_hybrid_templates.py:L224-L224] |
| `TestTemplateStructure`; `tests/solvers/orcaflex/test_hybrid_templates.py::TestTemplateStructure::test_all_templates_have_readme`, `tests/solvers/orcaflex/test_hybrid_templates.py::TestTemplateStructure::test_all_templates_have_base_dir`, `tests/solvers/orcaflex/test_hybrid_templates.py::TestTemplateStructure::test_all_templates_have_variations_dir`, `tests/solvers/orcaflex/test_hybrid_templates.py::TestTemplateStructure::test_all_templates_have_cases_dir` | `'OrcFxAPI not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_hybrid_templates.py:L259-L259] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_load` | `f'Template not found: {template_path}'` | `pytest.skip` [tests/solvers/orcaflex/test_hybrid_templates.py:L59-L59] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_converge` | `f'Template not found: {template_path}'` | `pytest.skip` [tests/solvers/orcaflex/test_hybrid_templates.py:L76-L76] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_pipeline_base_models_load` | `f'Template not found: {template_path}'` | `pytest.skip` [tests/solvers/orcaflex/test_hybrid_templates.py:L89-L89] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_pipeline_base_models_converge` | `f'Template not found: {template_path}'` | `pytest.skip` [tests/solvers/orcaflex/test_hybrid_templates.py:L101-L101] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_umbilical_base_models_load` | `f'Template not found: {template_path}'` | `pytest.skip` [tests/solvers/orcaflex/test_hybrid_templates.py:L113-L113] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_umbilical_base_models_converge` | `f'Template not found: {template_path}'` | `pytest.skip` [tests/solvers/orcaflex/test_hybrid_templates.py:L125-L125] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_subsea_base_models_load` | `f'Template not found: {template_path}'` | `pytest.skip` [tests/solvers/orcaflex/test_hybrid_templates.py:L137-L137] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_subsea_base_models_converge` | `f'Template not found: {template_path}'` | `pytest.skip` [tests/solvers/orcaflex/test_hybrid_templates.py:L149-L149] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_mooring_base_models_load` | `f'Template not found: {template_path}'` | `pytest.skip` [tests/solvers/orcaflex/test_hybrid_templates.py:L164-L164] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_mooring_base_models_converge` | `f'Template not found: {template_path}'` | `pytest.skip` [tests/solvers/orcaflex/test_hybrid_templates.py:L179-L179] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge` | `f'Case file not found: {template_path}'` | `pytest.skip` [tests/solvers/orcaflex/test_hybrid_templates.py:L218-L218] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestLibraryComponents::test_line_types_exist` | `'Library line_types directory not found'` | `pytest.skip` [tests/solvers/orcaflex/test_hybrid_templates.py:L234-L234] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestLibraryComponents::test_buoy_types_exist` | `'Library buoy_types directory not found'` | `pytest.skip` [tests/solvers/orcaflex/test_hybrid_templates.py:L243-L243] |

#### `tests/solvers/orcaflex/test_include_library.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `tests/solvers/orcaflex/test_include_library.py::TestOrcaFlexIntegration::test_load_model_with_object_includes` | `'OrcFxAPI not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_include_library.py:L272-L275] |
| `tests/solvers/orcaflex/test_include_library.py::TestOrcaFlexIntegration::test_basefile_variation_loading` | `'OrcFxAPI not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_include_library.py:L314-L317] |
| `tests/solvers/orcaflex/test_include_library.py::TestOrcaFlexIntegration::test_load_model_with_object_includes` | `'OrcFxAPI not available'` | `pytest.importorskip` [tests/solvers/orcaflex/test_include_library.py:L273-L273] |
| `tests/solvers/orcaflex/test_include_library.py::TestOrcaFlexIntegration::test_basefile_variation_loading` | `'OrcFxAPI not available'` | `pytest.importorskip` [tests/solvers/orcaflex/test_include_library.py:L315-L315] |

#### `tests/solvers/orcaflex/test_load_orcaflex_files.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `test_yaml_structure` | `'No OrcaFlex files found'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_load_orcaflex_files.py:L81-L81] |
| `test_yaml_model_has_general` | `'No OrcaFlex files found'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_load_orcaflex_files.py:L104-L104] |
| `test_orcaflex_api_load` | `'OrcFxAPI not available on Linux'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_load_orcaflex_files.py:L127-L127] |
| `test_orcaflex_api_load` | `'No OrcaFlex files found'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_load_orcaflex_files.py:L128-L128] |
| `test_input_file_yaml_valid` | `'No input_file good files found'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_load_orcaflex_files.py:L155-L155] |
| `test_input_file_error_yaml_parses` | `'No input_file error files found'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_load_orcaflex_files.py:L172-L172] |
| `test_input_file_coating_references_valid` | `'No input_file good files found'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_load_orcaflex_files.py:L195-L195] |
| `test_input_file_error_has_bad_coating_ref` | `'No input_file error files found'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_load_orcaflex_files.py:L225-L225] |
| `test_input_file_error_has_known_diff` | `'No input_file error files found'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_load_orcaflex_files.py:L260-L260] |

#### `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `sample_dat_files`; `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_single_dat_to_yml`, `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_single_yml_to_dat`, `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_conversion_skip_existing`, `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_batch_conversion_dat_to_yml`, `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_batch_parallel_conversion`, `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_mock_conversion`, `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_mock_batch_conversion`, `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_yaml_validation`, `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_round_trip_validation`, `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_statistics_tracking`, `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_report_generation`, `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestCLI::test_cli_single_file`, `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestCLI::test_cli_batch` | `'Example files not found'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L50-L50] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_single_dat_to_yml` | `'No .dat files available'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L83-L83] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_single_yml_to_dat` | `'No .dat files available'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L98-L98] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_conversion_skip_existing` | `'No .dat files available'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L128-L128] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_batch_conversion_dat_to_yml` | `'No .dat files available'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L149-L149] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_batch_conversion_with_pattern` | `'Example files not found'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L161-L161] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_batch_parallel_conversion` | `'Need at least 2 .dat files'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L181-L181] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_mock_conversion` | `'No .dat files available'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L205-L205] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_mock_batch_conversion` | `'No .dat files available'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L227-L227] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_yaml_validation` | `'No .dat files available'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L243-L243] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_round_trip_validation` | `'No .dat files available'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L259-L259] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_statistics_tracking` | `'No .dat files available'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L336-L336] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_report_generation` | `'No .dat files available'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L356-L356] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestOrcaFlexConverterEnhanced::test_find_files` | `'Example files not found'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L385-L385] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestPerformance::test_large_batch_performance` | `'Example files not found'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L425-L425] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestCLI::test_cli_single_file` | `'No .dat files available'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L463-L463] |
| `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py::TestCLI::test_cli_batch` | `'No .dat files available'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L483-L483] |

#### `tests/solvers/orcaflex/test_orcaflex_unit.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `TestUniversalRunner`; `tests/solvers/orcaflex/test_orcaflex_unit.py::TestUniversalRunner::test_universal_runner_import`, `tests/solvers/orcaflex/test_orcaflex_unit.py::TestUniversalRunner::test_status_reporter_import` | `'Universal runner not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_orcaflex_unit.py:L122-L122] |
| `TestPostProcessing`; `tests/solvers/orcaflex/test_orcaflex_unit.py::TestPostProcessing::test_post_processor_import` | `'Post-processing not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_orcaflex_unit.py:L137-L137] |

#### `tests/solvers/orcaflex/test_orcaflex_yml_converter_import.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `test_convert_returns_false_without_orcfxapi` | `'OrcFxAPI installed on this machine'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_yml_converter_import.py:L47-L47] |
| `test_main_returns_error_without_orcfxapi` | `'OrcFxAPI installed on this machine'` | `pytest.skip` [tests/solvers/orcaflex/test_orcaflex_yml_converter_import.py:L58-L58] |

#### `tests/solvers/orcaflex/test_pipeline_schematic.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `tests/solvers/orcaflex/test_pipeline_schematic.py::TestPipelineSchematicGenerator::test_parse_model` | `'Sample YAML not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_pipeline_schematic.py:L180-L186] |
| `tests/solvers/orcaflex/test_pipeline_schematic.py::TestPipelineSchematicGenerator::test_parse_model_extracts_end_a` | `'Sample YAML not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_pipeline_schematic.py:L199-L205] |
| `tests/solvers/orcaflex/test_pipeline_schematic.py::TestPipelineSchematicGenerator::test_create_plan_view` | `'Sample YAML not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_pipeline_schematic.py:L222-L228] |
| `tests/solvers/orcaflex/test_pipeline_schematic.py::TestPipelineSchematicGenerator::test_create_elevation_view` | `'Sample YAML not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_pipeline_schematic.py:L242-L248] |
| `tests/solvers/orcaflex/test_pipeline_schematic.py::TestPipelineSchematicGenerator::test_generate_html_report` | `'Sample YAML not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_pipeline_schematic.py:L262-L268] |
| `tests/solvers/orcaflex/test_pipeline_schematic.py::TestPipelineSchematicGenerator::test_save_individual_views` | `'Sample YAML not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_pipeline_schematic.py:L286-L292] |
| `TestIntegration::test_full_workflow` | `'Sample YAML not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_pipeline_schematic.py:L375-L381] |
| `tests/solvers/orcaflex/test_pipeline_schematic.py::TestIntegration::test_model_coordinates_accuracy` | `'Sample YAML not available'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_pipeline_schematic.py:L405-L411] |
| `tests/solvers/orcaflex/test_pipeline_schematic.py::TestPipelineSchematicGenerator::test_init_with_valid_path` | `'Sample YAML file not found'` | `pytest.skip` [tests/solvers/orcaflex/test_pipeline_schematic.py:L162-L162] |
| `tests/solvers/orcaflex/test_pipeline_schematic.py::TestPipelineSchematicGenerator::test_init_with_custom_output_dir` | `'Sample YAML file not found'` | `pytest.skip` [tests/solvers/orcaflex/test_pipeline_schematic.py:L172-L172] |
| `tests/solvers/orcaflex/test_pipeline_schematic.py::TestPipelineSchematicGenerator::test_get_marker_symbol` | `'Sample YAML file not found'` | `pytest.skip` [tests/solvers/orcaflex/test_pipeline_schematic.py:L308-L308] |
| `tests/solvers/orcaflex/test_pipeline_schematic.py::TestPipelineSchematicGenerator::test_get_marker_size` | `'Sample YAML file not found'` | `pytest.skip` [tests/solvers/orcaflex/test_pipeline_schematic.py:L320-L320] |
| `tests/solvers/orcaflex/test_pipeline_schematic.py::TestPipelineSchematicGenerator::test_create_boundary_condition_legend` | `'Sample YAML file not found'` | `pytest.skip` [tests/solvers/orcaflex/test_pipeline_schematic.py:L331-L331] |
| `tests/solvers/orcaflex/test_pipeline_schematic.py::TestPipelineLengthCalculation::test_simple_straight_line` | `'Sample YAML file not found'` | `pytest.skip` [tests/solvers/orcaflex/test_pipeline_schematic.py:L346-L346] |
| `tests/solvers/orcaflex/test_pipeline_schematic.py::TestPipelineLengthCalculation::test_diagonal_line` | `'Sample YAML file not found'` | `pytest.skip` [tests/solvers/orcaflex/test_pipeline_schematic.py:L360-L360] |

#### `tests/solvers/orcaflex/test_run_state_gating.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `test_vendor_enum_members_resolve_by_name` | `'OrcFxAPI is not installed'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_run_state_gating.py:L156-L156] |
| `test_the_gate_covers_every_state_the_vendor_enum_defines` | `'OrcFxAPI is not installed'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_run_state_gating.py:L179-L179] |

#### `tests/solvers/orcaflex/test_skip_list_reconciliation.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `tests/solvers/orcaflex/test_skip_list_reconciliation.py::TestSkipListReconciliation::test_allowed_diff_props_classified` | `f'scripts/semantic_validate.py not importable: {exc}'` | `pytest.skip` [tests/solvers/orcaflex/test_skip_list_reconciliation.py:L144-L144] |

#### `tests/solvers/orcaflex/test_solver_version_record.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `requires_orcfxapi`; `test_requested_version_is_selected_and_recorded`, `test_record_is_complete_when_no_version_is_requested`, `test_selected_lib_path_is_in_force_for_a_later_module_scope_importer` | `'OrcFxAPI is not installed on this host'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_solver_version_record.py:L33-L35] |
| `test_requested_version_is_selected_and_recorded` | `"no versioned OrcaFlex install"` | `pytest.skip` [tests/solvers/orcaflex/test_solver_version_record.py:L87-L87] [tests/solvers/orcaflex/test_solver_version_record.py:L73-L73] |
| `test_selected_lib_path_is_in_force_for_a_later_module_scope_importer` | `"no versioned OrcaFlex install"` | `pytest.skip` [tests/solvers/orcaflex/test_solver_version_record.py:L214-L214] [tests/solvers/orcaflex/test_solver_version_record.py:L192-L192] |

#### `tests/solvers/orcaflex/test_spec_upgrader.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `riser_spec`; `tests/solvers/orcaflex/test_spec_upgrader.py::TestRiserDetection::test_detects_riser_type`, `tests/solvers/orcaflex/test_spec_upgrader.py::TestRiserDetection::test_riser_confidence_above_threshold`, `tests/solvers/orcaflex/test_spec_upgrader.py::TestRiserDetection::test_riser_has_evidence` | `f'Spec file not found: {path}'` | `pytest.skip` [tests/solvers/orcaflex/test_spec_upgrader.py:L29-L29] |
| `mooring_spec`; `tests/solvers/orcaflex/test_spec_upgrader.py::TestMooringDetection::test_detects_mooring_type`, `tests/solvers/orcaflex/test_spec_upgrader.py::TestMooringDetection::test_mooring_confidence_above_threshold`, `tests/solvers/orcaflex/test_spec_upgrader.py::TestMooringDetection::test_mooring_has_evidence` | `f'Spec file not found: {path}'` | `pytest.skip` [tests/solvers/orcaflex/test_spec_upgrader.py:L38-L38] |
| `pipeline_spec`; `tests/solvers/orcaflex/test_spec_upgrader.py::TestPipelineDetection::test_detects_pipeline_type`, `tests/solvers/orcaflex/test_spec_upgrader.py::TestPipelineDetection::test_pipeline_confidence_above_threshold`, `tests/solvers/orcaflex/test_spec_upgrader.py::TestPipelineDetection::test_pipeline_has_evidence` | `f'Spec file not found: {path}'` | `pytest.skip` [tests/solvers/orcaflex/test_spec_upgrader.py:L47-L47] |

#### `tests/solvers/orcaflex/test_template_generator.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `tests/solvers/orcaflex/test_template_generator.py::TestIntegration::test_full_workflow_calm_buoy` | `'Templates directory not found'` | `pytest.mark.skipif` [tests/solvers/orcaflex/test_template_generator.py:L467-L467] |
| `tests/solvers/orcaflex/test_template_generator.py::TestTemplateManager::test_discover_templates_finds_hybrid_dirs` | `'Templates directory not found'` | `pytest.skip` [tests/solvers/orcaflex/test_template_generator.py:L157-L157] |
| `tests/solvers/orcaflex/test_template_generator.py::TestTemplateManager::test_discover_templates_returns_correct_structure` | `'Templates directory not found'` | `pytest.skip` [tests/solvers/orcaflex/test_template_generator.py:L174-L174] |
| `tests/solvers/orcaflex/test_template_generator.py::TestTemplateManager::test_get_template_by_name` | `'Templates directory not found'` | `pytest.skip` [tests/solvers/orcaflex/test_template_generator.py:L191-L191] |
| `tests/solvers/orcaflex/test_template_generator.py::TestTemplateManager::test_get_template_by_name` | `'calm_buoy_hybrid template not found'` | `pytest.skip` [tests/solvers/orcaflex/test_template_generator.py:L199-L199] |
| `tests/solvers/orcaflex/test_template_generator.py::TestTemplateManager::test_list_variations_for_template` | `'Templates directory not found'` | `pytest.skip` [tests/solvers/orcaflex/test_template_generator.py:L210-L210] |
| `tests/solvers/orcaflex/test_template_generator.py::TestTemplateManager::test_list_variations_for_template` | `'calm_buoy_hybrid template not found'` | `pytest.skip` [tests/solvers/orcaflex/test_template_generator.py:L216-L216] |
| `tests/solvers/orcaflex/test_template_generator.py::TestCLI::test_list_templates_command` | `'Templates directory not found'` | `pytest.skip` [tests/solvers/orcaflex/test_template_generator.py:L403-L403] |
| `tests/solvers/orcaflex/test_template_generator.py::TestIntegration::test_full_workflow_calm_buoy` | `'calm_buoy_hybrid template not found'` | `pytest.skip` [tests/solvers/orcaflex/test_template_generator.py:L479-L479] |
| `tests/solvers/orcaflex/test_template_generator.py::TestIntegration::test_full_workflow_calm_buoy` | `'Template missing base or variations'` | `pytest.skip` [tests/solvers/orcaflex/test_template_generator.py:L483-L483] |


#### `tests/solvers/orcaflex/universal/test_universal_runner.py`

| Source scope / affected definitions | Quoted reason expression | Mechanism and source |
| --- | --- | --- |
| `TestRealOrcaFlexFiles.test_real_dat_files`; `tests/solvers/orcaflex/universal/test_universal_runner.py::TestRealOrcaFlexFiles::test_real_dat_files` | `"No real .dat files available for testing"` | `self.skipTest` [tests/solvers/orcaflex/universal/test_universal_runner.py:L219-L222] |

### Offline test-file dispositions

Results below are identical at BASE and AFTER, with combined baseline outcomes
and one complete AFTER batch at the execution revision in section 4. Source locators identify the test code at the retained citation pin;
the numerical results are execution records, not assertions from source text.
A collection skip is kept separate from a collected test case.

| Test file | Result / boundary | Source |
| --- | --- | --- |
| `test_c13_removed_dat_references.py` | 16 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_c13_removed_dat_references.py:L36-L36] |
| `test_dynamics_progress.py` | 12 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_dynamics_progress.py:L9-L9] |
| `test_environment_defaults_vs_orcfxapi.py` | 0 passed; 0 failed; 0 skipped; 1 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_environment_defaults_vs_orcfxapi.py:L93-L97] |
| `test_examples_downloader.py` | 10 passed; 0 failed; 2 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_examples_downloader.py:L37-L37] |
| `test_hybrid_templates.py` | 0 passed; 0 failed; 56 skipped (49 native-route skips; 7 licence-free-but-gated skips); 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_hybrid_templates.py:L56-L56] |
| `test_include_library.py` | 0 passed; 0 failed; 0 skipped; 1 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_include_library.py:L272-L275] |
| `test_inverse_resolver.py` | 25 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_inverse_resolver.py:L28-L28] |
| `test_library_generator.py` | 23 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_library_generator.py:L24-L24] |
| `test_load_orcaflex_files.py` | 0 passed; 0 failed; 8 skipped (8 fixture-root skips, not a licence boundary); 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_load_orcaflex_files.py:L87-L87] |
| `test_mock_is_explicit.py` | 20 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_mock_is_explicit.py:L33-L33] |
| `test_modal_benchmark.py` | 20 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_modal_benchmark.py:L47-L47] |
| `test_model_catalogue.py` | 29 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_model_catalogue.py:L40-L40] |
| `test_model_catalogue_references.py` | 6 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_model_catalogue_references.py:L24-L24] |
| `test_model_generator.py` | 10 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_model_generator.py:L32-L32] |
| `test_model_header_guard.py` | 8 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_model_header_guard.py:L49-L49] |
| `test_modular_input_validation_fixes.py` | 15 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L58-L58] |
| `test_opp_config_expansion.py` | 7 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_opp_config_expansion.py:L68-L68] |
| `test_opp_summary_fix.py` | 2 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_opp_summary_fix.py:L12-L12] |
| `test_oq1_oq2_classifications.py` | 9 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_oq1_oq2_classifications.py:L39-L39] |
| `test_orcaflex_cli.py` | 24 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_orcaflex_cli.py:L16-L16] |
| `test_orcaflex_converter_enhanced.py` | 21 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py:L80-L80] |
| `test_orcaflex_unit.py` | 0 passed; 0 failed; 0 skipped; 0 collection skipped; 1 collection errors | [tests/solvers/orcaflex/test_orcaflex_unit.py:L12-L24] |
| `test_orcaflex_yml_converter_import.py` | 4 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_orcaflex_yml_converter_import.py:L20-L20] |
| `test_pipeline_schematic.py` | 23 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_pipeline_schematic.py:L44-L44] |
| `test_production_optimization.py` | 1 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_production_optimization.py:L27-L27] |
| `test_run_state_gating.py` | 16 passed; 0 failed; 2 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_run_state_gating.py:L64-L64] |
| `test_sim_retention.py` | 13 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_sim_retention.py:L45-L45] |
| `test_skip_list_reconciliation.py` | 14 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_skip_list_reconciliation.py:L59-L59] |
| `test_slay_campaign_plans.py` | 5 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_slay_campaign_plans.py:L38-L38] |
| `test_slay_campaign_preparation.py` | 13 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_slay_campaign_preparation.py:L28-L28] |
| `test_solver_version_record.py` | 5 passed; 0 failed; 3 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_solver_version_record.py:L65-L65] |
| `test_spec_author.py` | 23 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_spec_author.py:L65-L65] |
| `test_spec_upgrader.py` | 19 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_spec_upgrader.py:L53-L53] |
| `test_template_generator.py` | 21 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_template_generator.py:L16-L26] |
| `test_values_equal_bool_normalization.py` | 24 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_values_equal_bool_normalization.py:L34-L34] |
| `test_yaml_validator.py` | 38 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_yaml_validator.py:L44-L44] |
| `test_yaml_validator_timestep.py` | 65 passed; 0 failed; 0 skipped; 0 collection skipped; 0 collection errors | [tests/solvers/orcaflex/test_yaml_validator_timestep.py:L41-L41] |

The reporting regression file and the guarded workflow node are included in the
section-4 totals; they are outside the top-level test-file inventory.

### Non-passing offline nodes

The BASE and AFTER records contain the same following 72 runtime-skipped node IDs.
Quoted reasons are identical recorded messages; source locators identify their
pinned guards. Collection and reporting events are listed separately below.

| Runtime-skipped node ID | Quoted reason | Pinned guard |
| --- | --- | --- |
| `tests/solvers/orcaflex/test_examples_downloader.py::TestDownloaderIntegration::test_parse_real_categories` | "Integration test - requires internet connection" | [tests/solvers/orcaflex/test_examples_downloader.py:L312-L312] |
| `tests/solvers/orcaflex/test_examples_downloader.py::TestDownloaderIntegration::test_real_website_connection` | "Integration test - requires internet connection" | [tests/solvers/orcaflex/test_examples_downloader.py:L301-L301] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path0]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path10]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path11]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path12]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path13]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path14]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path15]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path16]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path17]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path18]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path19]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path1]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path20]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path21]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path22]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path2]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path3]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path4]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path5]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path6]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path7]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path8]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_case_files_converge[template_path9]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_mooring_base_models_converge[template_path0]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_mooring_base_models_converge[template_path1]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_mooring_base_models_converge[template_path2]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_mooring_base_models_converge[template_path3]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_mooring_base_models_load[template_path0]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_mooring_base_models_load[template_path1]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_mooring_base_models_load[template_path2]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_mooring_base_models_load[template_path3]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_pipeline_base_models_converge[template_path0]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_pipeline_base_models_load[template_path0]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_converge[template_path0]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_converge[template_path1]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_converge[template_path2]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_converge[template_path3]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_converge[template_path4]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_converge[template_path5]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_load[template_path0]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_load[template_path1]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_load[template_path2]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_load[template_path3]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_load[template_path4]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_riser_base_models_load[template_path5]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_subsea_base_models_converge[template_path0]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_subsea_base_models_load[template_path0]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_umbilical_base_models_converge[template_path0]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestHybridTemplates::test_umbilical_base_models_load[template_path0]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L43-L43] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestLibraryComponents::test_buoy_types_exist` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L224-L224] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestLibraryComponents::test_line_type_files_are_valid_yaml[NOTSET]` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L224-L224] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestLibraryComponents::test_line_types_exist` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L224-L224] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestTemplateStructure::test_all_templates_have_base_dir` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L259-L259] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestTemplateStructure::test_all_templates_have_cases_dir` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L259-L259] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestTemplateStructure::test_all_templates_have_readme` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L259-L259] |
| `tests/solvers/orcaflex/test_hybrid_templates.py::TestTemplateStructure::test_all_templates_have_variations_dir` | "OrcFxAPI not available" | [tests/solvers/orcaflex/test_hybrid_templates.py:L259-L259] |
| `tests/solvers/orcaflex/test_load_orcaflex_files.py::test_input_file_coating_references_valid[NOTSET]` | "No input_file good files found" | [tests/solvers/orcaflex/test_load_orcaflex_files.py:L195-L195] |
| `tests/solvers/orcaflex/test_load_orcaflex_files.py::test_input_file_error_has_bad_coating_ref[NOTSET]` | "No input_file error files found" | [tests/solvers/orcaflex/test_load_orcaflex_files.py:L225-L225] |
| `tests/solvers/orcaflex/test_load_orcaflex_files.py::test_input_file_error_has_known_diff[NOTSET]` | "No input_file error files found" | [tests/solvers/orcaflex/test_load_orcaflex_files.py:L260-L260] |
| `tests/solvers/orcaflex/test_load_orcaflex_files.py::test_input_file_error_yaml_parses[NOTSET]` | "No input_file error files found" | [tests/solvers/orcaflex/test_load_orcaflex_files.py:L172-L172] |
| `tests/solvers/orcaflex/test_load_orcaflex_files.py::test_input_file_yaml_valid[NOTSET]` | "No input_file good files found" | [tests/solvers/orcaflex/test_load_orcaflex_files.py:L155-L155] |
| `tests/solvers/orcaflex/test_load_orcaflex_files.py::test_orcaflex_api_load[NOTSET]` | "No OrcaFlex files found" | [tests/solvers/orcaflex/test_load_orcaflex_files.py:L128-L128] |
| `tests/solvers/orcaflex/test_load_orcaflex_files.py::test_yaml_model_has_general[NOTSET]` | "No OrcaFlex files found" | [tests/solvers/orcaflex/test_load_orcaflex_files.py:L104-L104] |
| `tests/solvers/orcaflex/test_load_orcaflex_files.py::test_yaml_structure[NOTSET]` | "No OrcaFlex files found" | [tests/solvers/orcaflex/test_load_orcaflex_files.py:L81-L81] |
| `tests/solvers/orcaflex/test_run_state_gating.py::test_the_gate_covers_every_state_the_vendor_enum_defines` | "OrcFxAPI is not installed" | [tests/solvers/orcaflex/test_run_state_gating.py:L179-L179] |
| `tests/solvers/orcaflex/test_run_state_gating.py::test_vendor_enum_members_resolve_by_name` | "OrcFxAPI is not installed" | [tests/solvers/orcaflex/test_run_state_gating.py:L156-L156] |
| `tests/solvers/orcaflex/test_solver_version_record.py::test_record_is_complete_when_no_version_is_requested` | "OrcFxAPI is not installed on this host" | [tests/solvers/orcaflex/test_solver_version_record.py:L33-L35] [tests/solvers/orcaflex/test_solver_version_record.py:L101-L101] |
| `tests/solvers/orcaflex/test_solver_version_record.py::test_requested_version_is_selected_and_recorded` | "OrcFxAPI is not installed on this host" | [tests/solvers/orcaflex/test_solver_version_record.py:L33-L35] [tests/solvers/orcaflex/test_solver_version_record.py:L64-L64] |
| `tests/solvers/orcaflex/test_solver_version_record.py::test_selected_lib_path_is_in_force_for_a_later_module_scope_importer` | "OrcFxAPI is not installed on this host" | [tests/solvers/orcaflex/test_solver_version_record.py:L33-L35] [tests/solvers/orcaflex/test_solver_version_record.py:L183-L183] |
| `tests/workflows/test_durable_workflows.py::test_workflow_registry[orcaflex-strength-post]` | "orcaflex-strength-post requires runtime=requires-license" | [tests/workflows/test_durable_workflows.py:L51-L54] |

Collection outcomes, outside the 629 completed runtime nodes:

- `tests.solvers.orcaflex.test_environment_defaults_vs_orcfxapi`: SKIP at collection; diagnostic prefix "OrcFxAPI not installed (Windows + licensed OrcaFlex required)". The identifying suffix is omitted. [tests/solvers/orcaflex/test_environment_defaults_vs_orcfxapi.py:L93-L97]
- `tests.solvers.orcaflex.test_include_library`: SKIP at collection; "OrcFxAPI not available". This guard hides the pure cases as well as the native cases (U11). [tests/solvers/orcaflex/test_include_library.py:L272-L275]
- `tests.solvers.orcaflex.test_orcaflex_unit`: ERROR at collection; `ImportError: cannot import name '__version__' from 'digitalmodel.orcaflex'`. No executed case is claimed for that file. [tests/solvers/orcaflex/test_orcaflex_unit.py:L12-L24]

The initial batch also produced one internal reporting error and one unnamed
incomplete attempt; neither counts as a completed node or pass. The continuation
established all 47 missing node outcomes, including a PASS for the deep-YAML
test. The full AFTER batch completed those nodes without a reporting error.
The original exception remains unresolved under issue pending (5), U4.
[tests/solvers/orcaflex/test_yaml_validator_timestep.py:L125-L128]

## 7. Lessons log

| Date       | Lesson                                                                                                                  | Evidence and pack change                                                                                                                                                                               | Shared-log entry                                 |
| ---------- | ----------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------ |
| 2026-10-15 | A saved simulation path does not establish completed dynamics; select the execution route and verify native state/time. | Sections 2–3 distinguish static-only save from dynamic reopen-and-verify. [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L119-L143] [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L194-L204] | UNVERIFIED: no shared-log entry is assigned; U4. |
| 2026-10-15 | A guarded solver import alone does not make a pure-Python or mocked test licence-dependent. | Offline dispositions distinguish execution results from native qualification. [tests/solvers/orcaflex/test_template_generator.py:L16-L26] [tests/solvers/orcaflex/test_opp_config_expansion.py:L1-L7] | UNVERIFIED: shared-log reference remains U4. |
| 2026-10-15 | None serialization requires a check for each writer before model-default substitution. | Section 9 distinguishes tilde and literal-null representers and retains their boundaries. [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L49-L60] [src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py:L78-L82] | UNVERIFIED: shared-log reference remains U4. |
| 2026-10-15 | Skip inventories require qualified test scope and both pytest/unittest mechanisms; equal reason strings do not identify a guard. | Node-specific guards and an instance skip are recorded in section 6. [tests/solvers/orcaflex/test_examples_downloader.py:L301-L313] [tests/solvers/orcaflex/test_hybrid_templates.py:L224-L260] [tests/solvers/orcaflex/universal/test_universal_runner.py:L219-L222] | issue pending (4), U12 |

## 8. Ownership

Family quality manager: riser and mooring quality manager.
Deputy: UNVERIFIED, designation requires the maintainer; U4.
Producing role: module documentation contributor.
Escalation: family quality manager for missing/contradictory input,
standard editions, tolerances or failed checks; maintainer for scope or rights.
These are pack responsibilities, not an assertion of technical clearance.

## 9. Dependencies

The top-level package delegates optional exports to universal, core and template
components; authoring delegates YAML generation to the modular generator.
`[src/digitalmodel/solvers/orcaflex/__init__.py:L29-L40]`
[src/digitalmodel/solvers/orcaflex/spec_author.py:L465-L472]

Subpackages are dependencies, not expanded scope: their own implementation and
validation need separate packs. The modular-generator schema and routing,
format conversion, modular input validation, result analysis, reporting and
universal execution are listed here only as boundaries.

- `analysis/` boundary. `[src/digitalmodel/solvers/orcaflex/analysis/__init__.py:L1-L4]`
- `browser/` boundary. `[src/digitalmodel/solvers/orcaflex/browser/__init__.py:L1-L1]`
- `core/` boundary. `[src/digitalmodel/solvers/orcaflex/core/__init__.py:L1-L4]`
- `examples_integration/` boundary. `[src/digitalmodel/solvers/orcaflex/examples_integration/__init__.py:L1-L4]`
- `format_converter/` boundary. `[src/digitalmodel/solvers/orcaflex/format_converter/__init__.py:L1-L4]`
- `model_generator/` boundary. `[src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L1-L4]`
- `modular_generator/` boundary. `[src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L4-L4]`
- `modular_input_validation/` boundary. `[src/digitalmodel/solvers/orcaflex/modular_input_validation/__init__.py:L1-L4]`
- `mooring_analysis/` boundary. `[src/digitalmodel/solvers/orcaflex/mooring_analysis/__init__.py:L1-L4]`
- `mooring_tension_iteration/` boundary. `[src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/__init__.py:L1-L4]`
- `post_process/` boundary. `[src/digitalmodel/solvers/orcaflex/post_process/__init__.py:L1-L1]`
- `post_results/` boundary. `[src/digitalmodel/solvers/orcaflex/post_results/__init__.py:L1-L1]`
- `reporting/` boundary. `[src/digitalmodel/solvers/orcaflex/reporting/__init__.py:L1-L1]`
- `results_analysis/` boundary. `[src/digitalmodel/solvers/orcaflex/results_analysis/__init__.py:L3-L6]`
- `universal/` boundary. `[src/digitalmodel/solvers/orcaflex/universal/__init__.py:L1-L4]`
- `utils/` boundary. `[src/digitalmodel/solvers/orcaflex/utils/__init__.py:L1-L4]`

### Repository-wide method alternatives

A whole-repository text search, including `scripts/`, identifies the following
routes; their presence does not qualify their engineering results.

- `scripts/conversion/yaml_to_include.py` delegates dumping and single-to-modular
  conversion to package implementations; it is a compatibility wrapper rather
  than an independent YAML method. [scripts/conversion/yaml_to_include.py:L3-L10]
  [scripts/conversion/yaml_to_include.py:L33-L43]
- `scripts/semantic_validate.py::load_monolithic` separately merges YAML documents,
  while `values_equal` implements the bool/string comparator exercised by section 4.
  [scripts/semantic_validate.py:L291-L301] [scripts/semantic_validate.py:L382-L406]
- `scripts/run_riser_analysis.py::load_model` uses `Model()` plus `LoadData`, and
  `run_statics` separately calls `CalculateStatics`; these native routes require
  a licence and were read as text only. [scripts/run_riser_analysis.py:L129-L150]
- `scripts/orcaflex/prepare_slay_campaign.py::catenary` computes an inextensible
  weight-only touchdown screen; the separate script implementation
  `scripts/python/digitalmodel/analysis/catenary_solver.py::solve_catenary` uses
  numerical minimization of endpoint, slope and length penalties. Neither is
  qualified here as a G2 hand-check reference (U3).
  [scripts/orcaflex/prepare_slay_campaign.py:L24-L48]
  [scripts/python/digitalmodel/analysis/catenary_solver.py:L23-L50]
  [scripts/python/digitalmodel/analysis/catenary_solver.py:L52-L90]
  Source reading confirms an extra arc-length factor `a`: the integral already
  uses the curve derivative, and `arc_length` multiplies that integral by `a`
  again. Numerical reproduction is not executed and independent regression
  acceptance remains open (U10).
  [scripts/python/digitalmodel/analysis/catenary_solver.py:L12-L21]

### Repository-wide implementations

The [repository implementation map](https://github.com/vamseeachanta/digitalmodel/blob/b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407/docs/registry/implementation-map.yaml) and its [index](https://github.com/vamseeachanta/digitalmodel/blob/b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407/docs/registry/implementation-map.md) are pinned to document commit `b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407`. The map-data code revision is `947fa50ff9a9e210de7e8f90e01b37add85c5954`; its family classifications and defect flags are inventory metadata, separate from the code revision in the pack header. Each source citation below is reopened at the pack header's code revision, `fb8a214bdb622ed04f0b6473a9311695c9a5a314`. `Mapped:` introduces the map classification rather than a qualified numerical conclusion. `not checked` means agreement with the section-5 controls is not established; these rows are not G2 hand-check references.

#### `catenary-mooring-line-statics`

| Pinned implementation | Map classification | Relation | Controls | Covering pack |
| --- | --- | --- | --- | --- |
| `solve_static` [docs/domains/mooring/screening/run_screening.py:L35-L48] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `compute_record` [docs/domains/mooring/screening/run_screening.py:L98-L136] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `calc_catenary` [examples/demos/gtm/demo_04_shallow_water_pipelay.py:L349-L418] | Mapped: Demonstrates the family method | caller | not checked | none |
| `solve_catenary_equations` [examples/domains/fpso/fpso_mooring_analysis.py:L285-L344] | Mapped: Demonstrates the family method | caller | not checked | none |
| `main` [examples/domains/fpso/fpso_mooring_analysis.py:L497-L562] | Mapped: Demonstrates the family method | caller | not checked | none |
| `main` [examples/domains/mooring/lazy_wave_example.py:L20-L124] | Mapped: Demonstrates the family method | caller | not checked | none |
| `analyze_mooring_line` [examples/domains/workflows/01_spread_mooring_8point.py:L180-L248] | Mapped: Demonstrates the family method | caller | not checked | none |
| `analyze_turret_at_heading` [examples/domains/workflows/02_turret_mooring.py:L76-L162] | Mapped: Demonstrates the family method | caller | not checked | none |
| `analyze_calm_buoy` [examples/domains/workflows/03_calm_buoy.py:L53-L113] | Mapped: Demonstrates the family method | caller | not checked | none |
| `demo_spline_pipe` [scripts/examples/pyvista_3d_evaluation.py:L111-L147] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `general_arc` [scripts/orcaflex/check_generic_scr_slwr.py:L57-L74] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `scr_reference` [scripts/orcaflex/check_generic_scr_slwr.py:L77-L100] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `slwr_reference` [scripts/orcaflex/check_generic_scr_slwr.py:L103-L145] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `catenary` [scripts/orcaflex/prepare_slay_campaign.py:L24-L48] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `catenaryEquation` [scripts/python/digitalmodel/analysis/catenary_equation.py:L5-L78] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `catenary_curve` [scripts/python/digitalmodel/analysis/catenary_solver.py:L8-L10] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `catenary_derivative` [scripts/python/digitalmodel/analysis/catenary_solver.py:L12-L14] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `arc_length.integrand` [scripts/python/digitalmodel/analysis/catenary_solver.py:L18-L19] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/analysis/catenary_solver.py:L102-L142] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `catenary` [scripts/python/digitalmodel/analysis/simple_catenary_equation.py:L6-L17] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `catenary` [scripts/python/digitalmodel/analysis/simple_catenary_x1y1_x2y2.py:L7-L20] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `solve_a` [scripts/python/digitalmodel/analysis/simple_catenary_x1y1_x2y2.py:L22-L34] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/generate_integration_charts.py:L40-L47] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `PerformanceProfiler.profile_catenary_solver` [scripts/python/digitalmodel/legacy/profile_marine_modules.py:L263-L332] | Mapped: Evaluates catenary geometry, stretch or restoring response; AST-equivalent definition after docstring removal | copy | not checked | none |
| `demonstrate_usage` [scripts/python/digitalmodel/legacy/validate_catenary_solver.py:L118-L162] | Mapped: Evaluates catenary geometry, stretch or restoring response; AST-equivalent definition after docstring removal | copy | not checked | none |
| `calculate_catenary_anchor_position` [scripts/update_baltic_moorings_10m_catenary.py:L60-L134] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `_catenary_riser_polyline` [src/digitalmodel/field_development/layout_model.py:L443-L490] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `OrcaflexModelComponents.spring_properties` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L619-L675] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `module scope` [src/digitalmodel/marine_ops/marine_analysis/catenary/adapter.py:L35-L47] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `catenaryEquation` [src/digitalmodel/marine_ops/marine_analysis/catenary/adapter.py:L50-L115] | Mapped: Evaluates catenary geometry, stretch or restoring response | caller | not checked | none |
| `_angle_based_method` [src/digitalmodel/marine_ops/marine_analysis/catenary/adapter.py:L197-L259] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `catenaryForces` [src/digitalmodel/marine_ops/marine_analysis/catenary/adapter.py:L262-L337] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `LazyWaveSolver._solve_hangoff_section` [src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py:L454-L503] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `LazyWaveSolver._solve_sag_hog_sections` [src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py:L505-L618] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `SimplifiedCatenarySolver.solve_from_angle` [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L71-L147] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `SimplifiedCatenarySolver.solve_from_force` [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L149-L229] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `solve_catenary_dict` [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L299-L363] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `calculate_forces_dict` [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L366-L393] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `CatenarySolver.solve` [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L77-L118] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `CatenarySolver._geometry_for_a` [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L203-L219] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `CatenarySolver._solve_simplified.length_error` [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L234-L256] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `CatenarySolver._compute_results` [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L275-L332] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `safe_sinh` [src/digitalmodel/marine_ops/marine_analysis/catenary/utils.py:L12-L48] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `safe_cosh` [src/digitalmodel/marine_ops/marine_analysis/catenary/utils.py:L51-L83] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `calculate_elongation` [src/digitalmodel/marine_ops/marine_analysis/catenary/utils.py:L192-L217] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `touchdown_point` [src/digitalmodel/marine_ops/marine_analysis/catenary/utils.py:L220-L249] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `PerformanceProfiler.profile_catenary_solver` [src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py:L256-L326] | Mapped: Evaluates catenary geometry, stretch or restoring response; AST-equivalent definition after docstring removal | copy | not checked | none |
| `module scope` [src/digitalmodel/marine_ops/marine_analysis/validation/validate_catenary.py:L12-L12] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `demonstrate_usage` [src/digitalmodel/marine_ops/marine_analysis/validation/validate_catenary.py:L123-L167] | Mapped: Evaluates catenary geometry, stretch or restoring response; AST-equivalent definition after docstring removal | copy | not checked | none |
| `module scope` [src/digitalmodel/marine_ops/marine_analysis/visualization/integration_charts.py:L40-L47] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `FPSOMooringFullWorkflow._baseline_span` [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L198-L226] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `FPSOMooringFullWorkflow._line_tensions` [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L282-L289] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `solve_catenary` [src/digitalmodel/orcaflex/mooring_design.py:L251-L335] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `MooringLineDesign.estimate_catenary` [src/digitalmodel/orcaflex/mooring_design.py:L385-L403] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `JLayConfig.calculate_sagbend` [src/digitalmodel/orcaflex/pipelay_analysis.py:L222-L260] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `SCRDesignInput.calculate_geometry` [src/digitalmodel/orcaflex/riser_config.py:L170-L229] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `_lazy` [src/digitalmodel/riser_configuration_screen/static.py:L48-L63] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `_lazy_shape` [src/digitalmodel/riser_configuration_screen/static.py:L72-L89] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `scr_offsets` [src/digitalmodel/riser_configuration_screen/static.py:L166-L220] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `scr_offsets.residual` [src/digitalmodel/riser_configuration_screen/static.py:L193-L195] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `OrcaflexModelComponents.spring_properties` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L917-L1046] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | this pack |
| `LazyWaveAnalyzer.analyze` [src/digitalmodel/subsea/catenary_riser/lazy_wave.py:L34-L171] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `catenaryEquation` [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L12-L68] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `catenaryForces` [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L71-L95] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `sagHogEquation` [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L98-L158] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `lazyWaveCatenaryEquation` [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L161-L199] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `gethorizontalDistanceHangofftoTDPrange` [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L434-L508] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `CatenaryCalculator._calculate_from_angle` [src/digitalmodel/subsea/catenary_riser/legacy/catenary_equation.py:L63-L81] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `catenary_riser` [src/digitalmodel/subsea/catenary_riser/legacy/catenary_riser.py:L32-L167] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `orcaflexModel` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L13-L357] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `lineTypes` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L555-L629] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `SimpleCatenaryAnalyzer.analyze_riser` [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L42-L101] | Mapped: Evaluates catenary geometry, stretch or restoring response | caller | not checked | none |
| `SimpleCatenaryAnalyzer._solve_catenary_from_H` [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L103-L172] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `SimpleCatenaryAnalyzer._solve_H_from_top_tension.calc_top_tension` [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L200-L219] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `SimpleCatenaryAnalyzer._solve_H_from_geometry` [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L279-L342] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `CatenaryAnalyzer.solve_catenary` [src/digitalmodel/subsea/mooring_analysis/catenary.py:L38-L119] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `CatenaryAnalyzer.solve_for_horizontal_tension` [src/digitalmodel/subsea/mooring_analysis/catenary.py:L121-L176] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `CatenaryAnalyzer.calculate_stiffness` [src/digitalmodel/subsea/mooring_analysis/catenary.py:L178-L238] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `CatenaryAnalyzer.check_touchdown` [src/digitalmodel/subsea/mooring_analysis/catenary.py:L240-L298] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `CatenaryAnalyzer.check_touchdown.s_from_a` [src/digitalmodel/subsea/mooring_analysis/catenary.py:L264-L268] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `catenary_analysis` [src/digitalmodel/subsea/mooring_analysis/cli.py:L50-L142] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `MooringDesigner.analyze_line_tension` [src/digitalmodel/subsea/mooring_analysis/designer.py:L126-L207] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `_catinb` [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L339-L414] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `_stretc` [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L417-L434] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `_fulcat` [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L437-L522] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `_catnry` [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L525-L657] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `_sub` [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L660-L686] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `build_line_table` [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L689-L700] | Mapped: Evaluates catenary geometry, stretch or restoring response | caller | not checked | none |
| `interp_table` [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L708-L744] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `anchor_pull` [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L747-L784] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `solve_pretension` [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L833-L884] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `restoring_force` [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L913-L961] | Mapped: Evaluates catenary geometry, stretch or restoring response | independent | not checked | none |
| `_geometry` [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L41-L56] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_derive.primitive` [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L84-L88] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_elastic_fixture_geometry_and_force_identities` [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L144-L172] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_inextensible_chord_arc_and_end_forces` [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L175-L193] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_superseded_answers_fail_geometry_and_touchdown_diagnosis` [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L196-L215] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_solver_against_reconciled_fixture` [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L226-L247] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `module scope` [tests/marine_ops/marine_engineering/catenary/test_run_quick.py:L8-L8] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `expected_from_angle` [tests/marine_ops/marine_engineering/catenary/test_simplified.py:L22-L37] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `expected_from_force` [tests/marine_ops/marine_engineering/catenary/test_simplified.py:L40-L52] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `CatenarySolver.solve.length_error` [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L97-L108] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `CatenarySolver.solve.length_error_derivative` [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L111-L124] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `CatenarySolver._compute_results` [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L165-L218] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `CatenarySolver._calculate_touchdown` [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L220-L238] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `_signed_anchor_coordinate` [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L248-L255] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `TestCatenarySolver.test_shape_coordinates` [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L378-L403] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `TestCatenaryPerformance.test_batch_processing` [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L526-L552] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `test_angle_method` [tests/marine_ops/marine_engineering/test_adapter_manual.py:L69-L112] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_forces` [tests/marine_ops/marine_engineering/test_adapter_manual.py:L115-L152] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestCatenaryEquationAngleMethod.test_angle_method_exact_legacy_match` [tests/marine_ops/marine_engineering/test_catenary_adapter.py:L182-L211] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestCatenaryForces.test_basic_forces_calculation` [tests/marine_ops/marine_engineering/test_catenary_adapter.py:L300-L328] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestCatenaryForces.test_forces_exact_legacy_match` [tests/marine_ops/marine_engineering/test_catenary_adapter.py:L330-L355] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestCatenaryForces.test_forces_various_scenarios` [tests/marine_ops/marine_engineering/test_catenary_adapter.py:L357-L378] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `CatenarySolver.solve.length_error` [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L97-L108] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `CatenarySolver.solve.length_error_derivative` [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L111-L124] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `CatenarySolver._compute_results` [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L165-L218] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `CatenarySolver._calculate_touchdown` [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L220-L238] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `_signed_anchor_coordinate` [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L248-L255] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `TestCatenarySolver.test_shape_coordinates` [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L378-L403] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `TestCatenaryPerformance.test_batch_processing` [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L526-L552] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `test_scr_matches_independent_inextensible_catenary` [tests/riser_configuration_screen/test_static.py:L58-L68] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `catenary_system` [tests/solve_catenary_correct.py:L12-L30] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `catenary_system` [tests/solve_general_catenary.py:L39-L77] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `test_marine_engineering_imports` [tests/structural/analysis/validate_imports.py:L139-L166] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `lazy_results` [tests/subsea/catenary_riser/test_static_reconciliation.py:L71-L91] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_scr_design_and_general_solver_in_inextensible_limit` [tests/subsea/catenary_riser/test_static_reconciliation.py:L119-L137] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestLibraryVsOracle.test_catinb_sloped_bottom_matches_oracle` [tests/subsea/mooring_analysis/test_dynmoor.py:L552-L572] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_excel_validation_case` [tests/test_catenary_module.py:L26-L69] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestLazyWaveSolver.test_hangoff_section_calculation` [tests/test_lazy_wave_catenary.py:L43-L68] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestLazyWaveSolver.test_sag_to_buoyancy_segment` [tests/test_lazy_wave_catenary.py:L70-L94] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestLazyWaveSolver.test_buoyancy_to_hog_segment` [tests/test_lazy_wave_catenary.py:L96-L121] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestLazyWaveSolver.test_hog_to_buoyancy_segment` [tests/test_lazy_wave_catenary.py:L123-L145] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestLazyWaveSolver.test_buoyancy_to_touchdown_segment` [tests/test_lazy_wave_catenary.py:L147-L167] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_hangoff_vertical_force_balances_suspended_weight` [tests/test_lazy_wave_force_equilibrium.py:L67-L72] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_hangoff_total_tension_matches_catenary_and_force_triangle` [tests/test_lazy_wave_force_equilibrium.py:L76-L87] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `solver` [tests/test_lazy_wave_kernel_defects.py:L64-L65] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |

#### `effective-tension-riser-stack-up`

| Pinned implementation | Map classification | Relation | Controls | Covering pack |
| --- | --- | --- | --- | --- |
| `main` [docs/domains/orcaflex/examples/modular/A01/A01 Catenary riser/run_orcaflex.py:L45-L94] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `main` [docs/domains/orcaflex/examples/modular/A01/A01 Lazy wave riser/run_orcaflex.py:L44-L92] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `main` [docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py:L47-L113] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `main` [docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py:L48-L116] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `check_r01_catenary` [docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py:L140-L180] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `check_r02_lazy_wave` [docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py:L187-L223] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `check_r05_lay_table` [docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py:L317-L346] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `check_r06_multi_static` [docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py:L353-L388] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `extract_vessel_params` [examples/demos/gtm/demo_04_shallow_water_pipelay.py:L273-L290] | Mapped: Demonstrates the family method | caller | not checked | none |
| `vessel_band_specs` [examples/demos/gtm/demo_04_shallow_water_pipelay.py:L293-L310] | Mapped: Demonstrates the family method | caller | not checked | none |
| `find_required_tension` [examples/demos/gtm/demo_04_shallow_water_pipelay.py:L563-L774] | Mapped: Demonstrates the family method | caller | not checked | none |
| `_build_infeasible_result` [examples/demos/gtm/demo_04_shallow_water_pipelay.py:L777-L819] | Mapped: Demonstrates the family method | caller | not checked | none |
| `run_single_case` [examples/demos/gtm/demo_04_shallow_water_pipelay.py:L826-L891] | Mapped: Demonstrates the family method | caller | not checked | none |
| `module scope` [examples/demos/gtm/results_store_demo04.py:L52-L55] | Mapped: Demonstrates the family method | caller | not checked | none |
| `_good_case` [examples/demos/gtm/tests/test_demo_04_results_store.py:L79-L124] | Mapped: Demonstrates the family method | caller | not checked | none |
| `_infeasible_case` [examples/demos/gtm/tests/test_demo_04_results_store.py:L127-L173] | Mapped: Demonstrates the family method | caller | not checked | none |
| `test_999_sentinel_and_none_null_round_trip` [examples/demos/gtm/tests/test_demo_04_results_store.py:L342-L403] | Mapped: Demonstrates the family method | caller | not checked | none |
| `analyze_installation_catenary` [examples/domains/workflows/04_pipeline_installation.py:L51-L123] | Mapped: Demonstrates the family method | caller | not checked | none |
| `_schedule_row` [examples/workflows/riser-stackup-registry-batch/run.py:L115-L154] | Mapped: Demonstrates the family method | caller | not checked | none |
| `_assemble_row` [examples/workflows/riser-stackup-registry-batch/run.py:L219-L256] | Mapped: Demonstrates the family method | caller | not checked | none |
| `module scope` [scripts/capabilities/build_wall_thickness_3d.py:L36-L36] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_analyze_point` [scripts/capabilities/build_wall_thickness_3d.py:L171-L219] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `build_study` [scripts/capabilities/build_wall_thickness_3d.py:L222-L244] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | wrapper | not checked | none |
| `_axes` [scripts/capabilities/build_wall_thickness_3d_page.py:L34-L45] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_pack_rows` [scripts/capabilities/build_wall_thickness_3d_page.py:L75-L114] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_run` [scripts/capabilities/build_wall_thickness_explorer.py:L48-L70] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `metrics` [scripts/orcaflex/check_generic_scr_slwr.py:L37-L54] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `calculate` [scripts/orcaflex/check_generic_scr_slwr.py:L148-L186] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `catenary` [scripts/orcaflex/prepare_slay_campaign.py:L24-L48] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `case_spec` [scripts/orcaflex/prepare_slay_campaign.py:L66-L76] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `prepare_case` [scripts/orcaflex/prepare_slay_campaign.py:L87-L110] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `prepare` [scripts/orcaflex/prepare_slay_campaign.py:L113-L134] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `main` [scripts/parametric/build_orcaflex_library.py:L39-L72] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `APISTD2RDCalculation` [scripts/python/digitalmodel/analysis/APIST2RD_Methods.py:L6-L149] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` [scripts/python/digitalmodel/analysis/RigidPipe.py:L205-L274] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | none |
| `StackUpDiagram` [scripts/python/digitalmodel/analysis/RiserStackup.py:L26-L180] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `main` [scripts/python/digitalmodel/analysis/StackUp_3000Ft.py:L10-L151] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `main` [scripts/python/digitalmodel/analysis/StackUp_9500Ft.py:L10-L181] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `evaluate` [scripts/reporting/build_riser_pipe_comparison.py:L50-L81] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | caller | not checked | none |
| `_validate` [scripts/reporting/build_riser_pipe_comparison.py:L84-L110] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `numeric_values` [scripts/reporting/build_riser_pipe_comparison.py:L151-L178] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_input_table` [scripts/reporting/build_riser_pipe_comparison.py:L210-L237] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `module scope` [scripts/riser_database/build_tables.py:L24-L24] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `VonMises_Pipe.init_calculate_required_properties` [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L696-L721] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | none |
| `local_buckling_lcc` [src/digitalmodel/code_checks/dnv_os_f101.py:L91-L147] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_run_api_rp_2rd` [src/digitalmodel/code_checks/workflow.py:L58-L79] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_run_dnv_os_f201` [src/digitalmodel/code_checks/workflow.py:L82-L110] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `minimum_top_tension_16q` [src/digitalmodel/drilling_riser/assembly.py:L208-L236] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_unit_count` [src/digitalmodel/drilling_riser/assembly.py:L254-L263] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `check_rig_capability` [src/digitalmodel/drilling_riser/assembly.py:L266-L306] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `case_spec` [src/digitalmodel/drilling_riser/campaign.py:L134-L239] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_ring_tensioners` [src/digitalmodel/drilling_riser/campaign.py:L358-L359] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `set_line_tension` [src/digitalmodel/drilling_riser/campaign.py:L362-L368] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `calibrate_line_tension` [src/digitalmodel/drilling_riser/campaign.py:L371-L393] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_statics_path` [src/digitalmodel/drilling_riser/campaign.py:L400-L565] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `robust_statics` [src/digitalmodel/drilling_riser/campaign.py:L568-L634] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_tensioner_vertical_n` [src/digitalmodel/drilling_riser/campaign.py:L734-L737] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `tensioner_vertical_target_n` [src/digitalmodel/drilling_riser/campaign.py:L752-L755] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `physical_state_checks` [src/digitalmodel/drilling_riser/campaign.py:L758-L810] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `RiserCampaignAdapter.prepare` [src/digitalmodel/drilling_riser/campaign.py:L855-L886] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `RiserCampaignAdapter.extract` [src/digitalmodel/drilling_riser/campaign.py:L912-L952] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `compute_operating_envelope` [src/digitalmodel/drilling_riser/envelope.py:L278-L446] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `equivalent_string_top_tension_n` [src/digitalmodel/drilling_riser/global_model/build.py:L35-L41] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_tensioner_geometry` [src/digitalmodel/drilling_riser/global_model/build.py:L51-L55] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `winch_tension_n` [src/digitalmodel/drilling_riser/global_model/build.py:L58-L61] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `build_generic_spec` [src/digitalmodel/drilling_riser/global_model/build.py:L180-L369] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `effective_tension_chain` [src/digitalmodel/drilling_riser/global_model/hand_checks.py:L45-L55] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `tension_references` [src/digitalmodel/drilling_riser/global_model/hand_checks.py:L64-L83] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `tensioner_lateral_stiffness_n_m` [src/digitalmodel/drilling_riser/global_model/hand_checks.py:L164-L176] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `reference_periods` [src/digitalmodel/drilling_riser/global_model/hand_checks.py:L216-L232] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | caller | not checked | none |
| `hung_weight_n` [src/digitalmodel/drilling_riser/global_model/hang_off.py:L48-L53] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `OpenWaterRiserSpec._closes` [src/digitalmodel/drilling_riser/global_model/open_water.py:L114-L139] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `build_open_water_generic_spec` [src/digitalmodel/drilling_riser/global_model/open_water.py:L143-L210] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `tension_references` [src/digitalmodel/drilling_riser/global_model/open_water.py:L218-L240] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `reference_periods` [src/digitalmodel/drilling_riser/global_model/open_water.py:L272-L281] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `release_reference` [src/digitalmodel/drilling_riser/global_model/open_water.py:L293-L317] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `ring_vertical_residual_n` [src/digitalmodel/drilling_riser/global_model/orcaflex_run.py:L181-L190] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `open_water_physical_checks` [src/digitalmodel/drilling_riser/global_model/orcaflex_run.py:L309-L327] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `Tensioners._failed` [src/digitalmodel/drilling_riser/global_model/spec.py:L146-L149] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `RiserGlobalModelSpec._geometry_closes` [src/digitalmodel/drilling_riser/global_model/spec.py:L405-L436] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_missing_open_water` [src/digitalmodel/drilling_riser/global_model/w5_channels.py:L140-L159] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `missing_channels` [src/digitalmodel/drilling_riser/global_model/w5_channels.py:L188-L218] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `extract` [src/digitalmodel/drilling_riser/global_model/w5_channels.py:L317-L366] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `extract_open_water` [src/digitalmodel/drilling_riser/global_model/w5_channels.py:L409-L455] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `t_min` [src/digitalmodel/drilling_riser/postprocess/checks.py:L172-L174] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `tension_setting_max` [src/digitalmodel/drilling_riser/postprocess/checks.py:L177-L178] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `tensioner_stroke` [src/digitalmodel/drilling_riser/postprocess/checks.py:L198-L202] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `ScheduleAssembly.minimum_top_tension_16q_kn` [src/digitalmodel/drilling_riser/schedule_assembly.py:L196-L212] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_load_oc_assembly` [src/digitalmodel/drilling_riser/schedule_assembly.py:L532-L724] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `effective_tension` [src/digitalmodel/drilling_riser/stackup.py:L85-L108] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `parse_snapshots` [src/digitalmodel/drilling_riser/telemetry_inputs.py:L135-L158] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `snapshot_to_tension_n` [src/digitalmodel/drilling_riser/telemetry_inputs.py:L178-L184] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `router` [src/digitalmodel/drilling_riser/tsj_workflow.py:L19-L71] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_section_result` [src/digitalmodel/drilling_riser/tsj_workflow.py:L74-L111] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_effective_tension_n` [src/digitalmodel/drilling_riser/tsj_workflow.py:L146-L160] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `router` [src/digitalmodel/drilling_riser/workflow.py:L32-L76] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | caller | not checked | none |
| `_write_profile_csv` [src/digitalmodel/drilling_riser/workflow.py:L154-L159] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `FatigueAnalysisComponents.add_riser_length_to_shear7_result` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L251-L268] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `FatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L308-L314] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `FatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L374-L410] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `FatigueAnalysisComponents.save_fatigue_summary_result` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L493-L519] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `FatigueAnalysisComponents.add_fatigue_damage_results_to_df` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L521-L552] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | none |
| `FatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L554-L562] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `FatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L564-L572] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `OrcaflexModelComponents.orcaflex_model` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L11-L59] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `OrcaflexModelComponents.customize_fj_types` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L494-L508] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaflexModelComponents.links` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L530-L533] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaflexModelComponents.add_support_spring` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L535-L555] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaflexModelComponents.add_tensioners` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L557-L585] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaflexModelComponents.tensioner_vessel_locations` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L587-L617] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaflexModelComponents.spring_properties` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L619-L675] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `OrcaflexModelComponents.get_connection_definitions` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L677-L683] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaflexModelComponents.get_fj_line_definitions` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L691-L702] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaflexModelComponents.get_line_items_vertical` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L704-L717] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `TypicalRiserStackUpCalculations.prepare_stack_up_properties_df` [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L8-L54] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | caller | not checked | none |
| `TypicalRiserStackUpCalculations.populate_elevation_from_endA_in_df` [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L56-L93] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `TypicalRiserStackUpCalculations.populate_shear7_x_by_L_in_df` [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L95-L105] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `TypicalRiserStackUpCalculations.populate_elevation_relative_to_mudline_in_df` [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L107-L162] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `TypicalRiserStackUpCalculations.populate_elevation_relative_to_MSL_in_df` [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L164-L171] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `TypicalRiserStackUpCalculations.evaluate_inservice_weights_and_tensions` [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L173-L177] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | caller | not checked | none |
| `TypicalRiserStackUpCalculations.assign_key_properties_to_df` [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L179-L342] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `TypicalRiserStackUpCalculations.evaluate_stack_in_service_weights` [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L344-L413] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `TypicalRiserStackUpCalculations.evaluate_effective_tension` [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L415-L527] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `TypicalRiserStackUpCalculations.evaluate_stretch_in_riser` [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L529-L552] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `Shear7ModelComponents.title` [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L21-L52] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `Shear7ModelComponents.riser_joints_to_shear7_zones` [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L58-L132] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `Shear7ModelComponents.add_block_3` [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L152-L165] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `VIVFatigueAnalysisComponents.add_riser_length_to_shear7_result` [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L454-L474] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `VIVFatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L533-L542] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `VIVFatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L631-L690] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `VIVFatigueAnalysisComponents.save_fatigue_summary_result` [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L799-L864] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `VIVFatigueAnalysisComponents.add_fatigue_damage_results_to_df` [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L866-L940] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | none |
| `VIVFatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L942-L951] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `VIVFatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L953-L964] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_write_tensions_csv` [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L90-L103] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `FPSOMooringFullWorkflow._line_state` [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L291-L321] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `FPSOMooringFullWorkflow._solve_line` [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L323-L349] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `FPSOMooringFullWorkflow._result` [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L369-L400] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `solve_catenary` [src/digitalmodel/orcaflex/mooring_design.py:L251-L335] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `JLayConfig.calculate_sagbend` [src/digitalmodel/orcaflex/pipelay_analysis.py:L222-L260] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `SCRDesignInput.calculate_geometry` [src/digitalmodel/orcaflex/riser_config.py:L170-L229] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `TTRDesignInput.calculate_stroke` [src/digitalmodel/orcaflex/riser_config.py:L339-L375] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `TTRDesignInput.calculate_top_tension` [src/digitalmodel/orcaflex/riser_config.py:L377-L395] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_TopTensionedInput.to_legacy` [src/digitalmodel/orcaflex/riser_input_schema.py:L244-L253] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `BeamProperties.natural_frequency` [src/digitalmodel/orcaflex/viv_screening.py:L185-L218] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_code_check_utilisation` [src/digitalmodel/parametric/generate.py:L65-L97] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `run_jlay` [src/digitalmodel/pipelay/workflow.py:L62-L93] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_lazy_row` [src/digitalmodel/riser_configuration_screen/static.py:L92-L125] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `scr_offsets` [src/digitalmodel/riser_configuration_screen/static.py:L166-L220] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_validate` [src/digitalmodel/riser_configuration_screen/workflow.py:L28-L60] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_utilisation` [src/digitalmodel/riser_configuration_screen/workflow.py:L63-L89] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_checks` [src/digitalmodel/riser_configuration_screen/workflow.py:L97-L121] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `GroupsBuilder._build_from_spec` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/groups_builder.py:L93-L144] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `LineTypeBuilder.build` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py:L85-L158] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `WinchBuilder.should_generate` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py:L29-L34] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `WinchBuilder.build` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py:L36-L81] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_apply_overrides` [src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py:L411-L478] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `Mooring.mooring_pre_tension_analysis` [src/digitalmodel/solvers/orcaflex/mooring.py:L127-L287] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | this pack |
| `Mooring.evaluate_line_length_next_iteration` [src/digitalmodel/solvers/orcaflex/mooring.py:L289-L348] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | this pack |
| `OPPRangeGraph.postProcessRange` [src/digitalmodel/solvers/orcaflex/opp_range_graph.py:L34-L102] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | this pack |
| `OrcaFlexAnalysis.postProcessRange` [src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py:L401-L456] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | this pack |
| `OrcaflexModelComponents.orcaflex_model` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L14-L81] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | this pack |
| `OrcaflexModelComponents.customize_fj_types` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L673-L708] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `OrcaflexModelComponents.links` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L743-L746] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `OrcaflexModelComponents.add_support_spring` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L748-L775] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `OrcaflexModelComponents.add_tensioners` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L777-L835] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `OrcaflexModelComponents.tensioner_vessel_locations` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L837-L915] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `OrcaflexModelComponents.spring_properties` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L917-L1046] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | this pack |
| `OrcaflexModelComponents.get_connection_definitions` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L1048-L1063] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `OrcaflexModelComponents.get_fj_line_definitions` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L1071-L1108] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `OrcaflexModelComponents.get_line_items_vertical` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L1110-L1148] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | this pack |
| `check` [src/digitalmodel/structural/analysis/cli.py:L83-L161] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `WallThicknessAnalyzer.check_combined_loading` [src/digitalmodel/structural/analysis/wall_thickness.py:L435-L497] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `ApiStd2rdStrategy._check_combined` [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L161-L226] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `DnvStF101Strategy._check_combined_loading` [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L268-L315] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `run_wall_thickness_compare` [src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py:L41-L109] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `MTLookupGenerator.generate` [src/digitalmodel/structural/analysis/wall_thickness_lookup.py:L60-L130] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `ParametricSweep.run` [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L66-L147] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `PipelinePhase.to_design_loads` [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L141-L148] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `create_standard_phases` [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L156-L220] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `LoadScalingProcessor.process_all_conditions` [src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py:L186-L252] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `IntegratedFatigueProcessor.process_fatigue_condition` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L290-L364] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `IntegratedFatigueProcessorWithNaming._process_condition_base` [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L81-L173] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `LoadScalingProcessor.load_reference_data` [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L101-L154] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `ReferenceSeaStateProcessor.process_fatigue_condition` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L355-L429] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `LoadScaler.process_fatigue_condition` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L458-L504] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `LoadScaler.process_sea_state` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L506-L564] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `main` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L566-L660] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `ProductionDataHandlerWithProgress.process_single_analysis` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor_with_progress.py:L83-L120] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `ParametricCoordinator._run_wall_thickness` [src/digitalmodel/structural/parametric_coordinator.py:L255-L275] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `VonMises_Pipe.init_calculate_required_properties` [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L985-L1008] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | none |
| `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L342-L493] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | none |
| `VonMises_Pipe.init_calculate_required_properties` [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1272-L1295] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms; AST-equivalent definition after docstring removal | copy | not checked | none |
| `simple_catenary` [src/digitalmodel/subsea/catenary_riser/cli.py:L43-L110] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `lazy_wave_analysis` [src/digitalmodel/subsea/catenary_riser/cli.py:L182-L253] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `LazyWaveAnalyzer.analyze` [src/digitalmodel/subsea/catenary_riser/lazy_wave.py:L34-L171] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `SimpleCatenaryAnalyzer.analyze_riser` [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L42-L101] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | caller | not checked | none |
| `SimpleCatenaryAnalyzer._solve_catenary_from_H` [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L103-L172] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `SimpleCatenaryAnalyzer._solve_H_from_top_tension` [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L174-L277] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `SimpleCatenaryAnalyzer.check_design_limits` [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L396-L428] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `developed_min_radius` [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L64-L88] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `required_effective_ei` [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L91-L106] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `screen_bend_stiffener` [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L109-L131] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `armour_tension_stress` [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L160-L165] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `verify_flexible_pipe` [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L178-L243] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `effective_driving_force` [src/digitalmodel/subsea/pipeline/global_buckling/hobbs_lateral.py:L294-L316] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `RiserLoadCase.to_loads` [src/digitalmodel/subsea/riser/riser_case_screening.py:L44-L50] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `run_riser_screening` [src/digitalmodel/subsea/riser/riser_case_screening.py:L125-L201] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `StackUpDiagram` [src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py:L27-L187] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `VerticalRiser.prepare_riser_model` [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L28-L39] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | caller | not checked | none |
| `VerticalRiser.read_riser_input_data` [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L41-L48] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `VerticalRiser.assign_key_properties_to_self` [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L50-L105] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `VerticalRiser.get_fea_properties` [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L120-L261] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `VerticalRiser.prepare_tensioner_data` [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L297-L304] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `VerticalRiser.evaluate_stack_up_properties` [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L325-L328] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `VerticalRiser.save_data` [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L374-L382] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `VerticalRiser.alter_properties_for_Shear7_software` [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L403-L407] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `FrequencyCalculator.calculate_natural_frequency` [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L59-L92] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | caller | not checked | none |
| `FrequencyCalculator._frequency_tension_controlled` [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L164-L218] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `FrequencyCalculator.calculate_effective_tension` [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L248-L286] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `FrequencyCalculator.check_stability` [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L288-L316] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | caller | not checked | none |
| `_beam` [src/digitalmodel/viv_screening/inputs.py:L63-L85] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `riser_analysis_cmd` [src/digitalmodel/workflows/workflow_automation/cli.py:L84-L141] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `create_riser_analysis_template` [src/digitalmodel/workflows/workflow_automation/templates.py:L257-L303] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `CompleteRiserAnalysisWorkflow.create` [src/digitalmodel/workflows/workflow_automation/workflows.py:L35-L145] | Mapped: Evaluates pressure-area, submerged-weight or stack-up tension terms | independent | not checked | none |
| `_make_pipe_cfg` [tests/asset_integrity/test_calculations.py:L29-L126] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_study` [tests/capabilities/test_wall_thickness_3d_page.py:L32-L84] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `module scope` [tests/contracts/test_force_units_boundary_contract.py:L52-L52] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_envelope_von_mises_n_to_kn_seam` [tests/contracts/test_force_units_boundary_contract.py:L454-L504] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `synthetic_spec` [tests/drilling_riser/global_model/conftest.py:L42-L77] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_physical_checks_use_the_remaining_tensioners` [tests/drilling_riser/global_model/test_batch_params.py:L78-L92] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_fake` [tests/drilling_riser/global_model/test_campaign.py:L65-L76] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `open_water_dict` [tests/drilling_riser/global_model/test_open_water.py:L43-L68] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_doc` [tests/drilling_riser/global_model/test_open_water_campaign.py:L17-L25] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_statics_tension_and_modes_agree_with_hand_checks` [tests/drilling_riser/global_model/test_orcaflex_solver.py:L17-L44] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_branch_tolerance_and_ring_buoyancy` [tests/drilling_riser/global_model/test_statics_path.py:L287-L303] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_vertical` [tests/drilling_riser/global_model/test_tensioner_representation.py:L24-L28] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_mc` [tests/drilling_riser/postprocess/test_aggregate.py:L20-L25] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_case` [tests/drilling_riser/postprocess/test_aggregate.py:L44-L46] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_recorded_arguments` [tests/drilling_riser/test_drive_off_worked_example.py:L48-L56] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_deltas` [tests/drilling_riser/test_schedule_assembly.py:L57-L66] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_base_cfg` [tests/infrastructure/unit/test_pipe_capacity_codes.py:L6-L41] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_analytical_reference_reproduces_from_basis` [tests/orcaflex/test_generic_scr_slwr.py:L150-L159] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_code_check_atlas` [tests/parametric/test_atlas.py:L84-L99] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `reference_input` [tests/reporting/test_riser_pipe_comparison.py:L24-L35] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `settings` [tests/riser_configuration_screen/test_workflow.py:L17-L29] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `verify_configuration` [tests/solvers/orcaflex/analysis/scripts/verify_configs.py:L13-L112] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_make_slay_spec` [tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py:L39-L65] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_make_pipeline_spec` [tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py:L19-L64] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_make_pipeline_spec` [tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py:L21-L69] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_write_slay_campaign` [tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py:L126-L219] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_make_base_spec_data` [tests/solvers/orcaflex/modular_generator/schema/test_campaign.py:L8-L66] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `extract_static_range_results` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L165-L190] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_make_valid_model` [tests/solvers/orcaflex/modular_generator/test_post_validator.py:L26-L70] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_support_types_are_defined_before_vessel_references` (`tests/solvers/orcaflex/test_slay_layout.py`) | Absent at code revision; map row retained as a missing-path record | not checked | not checked | none |
| `installation_parameters` [tests/specialized/umbilical_analysis/conftest.py:L24-L32] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `run_catenary_riser` [tests/subsea/catenary_riser/test_catenary.py:L13-L38] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `get_catenary_analysis_mock` [tests/test_automation/fixtures/engineering_fixtures.py:L97-L117] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `module scope` [tests/test_automation/scripts/batch_fix_tests.py:L12-L94] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `process_effective_tension_file` [tests/test_rainflow_with_effective_tension.py:L25-L114] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `main` [tests/test_rainflow_with_effective_tension.py:L117-L184] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_cfg` [tests/test_wall_thickness_compare_workflow.py:L22-L42] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_workflow_registry` [tests/workflows/test_durable_workflows.py:L52-L1680] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `screening_input` [tests/workflows/test_viv_screening_workflow.py:L18-L46] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |

#### `orcaflex-text-model-writers`

| Pinned implementation | Map classification | Relation | Controls | Covering pack |
| --- | --- | --- | --- | --- |
| `main` [docs/domains/examples/orcaflex/run_universal_example.py:L218-L264] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `module scope` [docs/domains/orcaflex/examples/gom_scr_design_study/generate_configs.py:L10-L10] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `main` [docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py:L47-L113] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `main` [docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py:L48-L116] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `strengthInputFilePreperation` [docs/domains/orcaflex/risers/drilling/wd9500ft/Preprocessing code.py:L2-L20] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `run_orcawave_diffraction` [docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction.py:L29-L187] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `run_diffraction_with_monitoring` [docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction_improved.py:L166-L273] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `run_batch_execution` [docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction_improved.py:L276-L372] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_load_data_compat` [docs/domains/orcawave/examples/L02 OC4 Semi-sub/run_orcawave.py:L37-L52] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `_load_data_compat` [docs/domains/orcawave/examples/L03 Semi-sub multibody analysis/run_orcawave.py:L39-L54] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `_load_data_compat` [docs/domains/orcawave/examples/L04 Sectional bodies/run_orcawave.py:L40-L55] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `_load_data_compat` [docs/domains/orcawave/examples/L05 Panel pressures/run_orcawave.py:L40-L55] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaFlexExporter.create_mooring_yml` [examples/domains/workflows/utils.py:L256-L295] | Mapped: Demonstrates the family method | caller | not checked | none |
| `write_audit_yaml` [scripts/audit_spec_library.py:L241-L246] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `generate_yml` [scripts/benchmark/generate_owd_ymls.py:L48-L66] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `main` [scripts/benchmark/run_spar_benchmark.py:L155-L341] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `solve_and_extract` [scripts/benchmark/solve_owd_extract.py:L116-L302] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `solve_owd` [scripts/benchmark/validate_owd_vs_spec.py:L426-L543] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `solve_spec` [scripts/benchmark/validate_owd_vs_spec.py:L546-L657] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_solve_spec_via_orcfxapi` [scripts/benchmark/validate_owd_vs_spec.py:L660-L761] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `module scope` [scripts/benchmark_model_library.py:L26-L29] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `load_and_run` [scripts/benchmark_model_library.py:L330-L341] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `run_spec_driven` [scripts/benchmark_model_library.py:L346-L386] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `load_json` [scripts/benchmark_model_library.py:L551-L574] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_trace` [scripts/benchmark_model_library.py:L645-L648] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `apply_mesh_preset` [scripts/benchmark_riser_library.py:L358-L377] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `run_mesh_sensitivity` [scripts/benchmark_riser_library.py:L430-L469] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `benchmark_model` [scripts/benchmark_riser_library.py:L524-L605] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `make_chart_js` [scripts/build_sme_report.py:L70-L102] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `make_profile_js` [scripts/build_sme_report.py:L105-L153] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `extract_references` [scripts/ci/check_include_targets.py:L36-L65] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_build_manual_entries` [scripts/consolidate_panels.py:L182-L224] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaFlexConverter.convert_file` [scripts/convert_orcaflex_to_yml.py:L122-L191] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_flattened_model_dir` [scripts/create_flattened_models.py:L19-L161] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `process_model_dir` [scripts/extract_library_validation.py:L64-L134] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_field_to_section_name` [scripts/extract_property_inventory.py:L145-L176] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `write_yaml` [scripts/extract_property_inventory.py:L273-L282] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `main` [scripts/extract_riser_validation.py:L56-L107] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `save_spec` [scripts/extract_s7_specs.py:L164-L178] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_env_section_wrappers` [scripts/fix_orcaflex_structure.py:L20-L37] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_fixed_analysis_model` [scripts/fix_orcaflex_structure.py:L39-L75] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `fix_sections_for_discretised` [scripts/fix_orcaflex_structure.py:L90-L112] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `module scope` [scripts/generate_all_specs.py:L39-L44] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `extract_spec` [scripts/generate_all_specs.py:L230-L262] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `save_spec_yml` [scripts/generate_all_specs.py:L265-L281] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_analysis_file` [scripts/generate_analysis_files.py:L21-L120] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `CALMBuoyProjectGenerator.generate_orcaflex_modules` [scripts/generate_calm_buoy_project.py:L310-L386] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `CALMBuoyProjectGenerator._populate_environment_module` [scripts/generate_calm_buoy_project.py:L388-L462] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `CALMBuoyProjectGenerator._apply_module_customizations` [scripts/generate_calm_buoy_project.py:L464-L511] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `CALMBuoyProjectGenerator._update_base_file_paths` [scripts/generate_calm_buoy_project.py:L691-L705] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `CALMBuoyProjectGenerator.save_updated_config` [scripts/generate_calm_buoy_project.py:L766-L775] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_wave_file` [scripts/generate_env_files.py:L38-L77] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_current_file` [scripts/generate_env_files.py:L79-L106] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_wind_file` [scripts/generate_env_files.py:L108-L140] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_version_metadata_block` [scripts/initialize_skill_metadata.py:L78-L103] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_report_type` [scripts/inventory/orcaflex_legacy_inventory.py:L28-L42] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `classify` [scripts/inventory/orcaflex_legacy_inventory.py:L118-L143] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `main` [scripts/inventory/orcaflex_legacy_inventory.py:L170-L319] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `module scope` [scripts/maintenance/archive_candidates.py:L32-L32] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `main` [scripts/maintenance/archive_candidates.py:L663-L999] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `apply_mesh_preset` [scripts/mesh_sensitivity_riser.py:L101-L126] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `scr_reference` [scripts/orcaflex/check_generic_scr_slwr.py:L77-L100] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `slwr_reference` [scripts/orcaflex/check_generic_scr_slwr.py:L103-L145] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `calculate` [scripts/orcaflex/check_generic_scr_slwr.py:L148-L186] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `case_spec` [scripts/orcaflex/prepare_slay_campaign.py:L66-L76] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `prepare_case` [scripts/orcaflex/prepare_slay_campaign.py:L87-L110] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `prepare` [scripts/orcaflex/prepare_slay_campaign.py:L113-L134] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_generated_sections` [scripts/prepare_mooring_qualification.py:L37-L51] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `ContentAnalyzer._categorize_folder` [scripts/python/digitalmodel/doc_tools/check_structure_creation.py:L309-L323] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `FrontmatterGenerator.frontmatter_to_yaml` [scripts/python/digitalmodel/doc_tools/frontmatter_generator.py:L380-L387] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/51_-_SerializationYAMLPart1.py:L4-L4] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `make_comparison_loaded_vs_ballast_cxc` [scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py:L315-L349] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `load_config` [scripts/python/digitalmodel/orcawave/analyze_tug_30m_1.py:L12-L19] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `generate_optimized_config` [scripts/python/digitalmodel/orcawave/analyze_tug_30m_1.py:L199-L231] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_config_with_gdf` [scripts/python/digitalmodel/orcawave/create_gdf_consistent.py:L345-L402] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_simple_vessel_config` [scripts/python/digitalmodel/orcawave/create_working_examples.py:L15-L142] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_oc4_semisub_config` [scripts/python/digitalmodel/orcawave/create_working_examples.py:L144-L270] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_minimal_test_config` [scripts/python/digitalmodel/orcawave/create_working_examples.py:L272-L336] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_final_working_config` [scripts/python/digitalmodel/orcawave/final_working_config.py:L18-L165] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_extended_version` [scripts/python/digitalmodel/orcawave/final_working_config.py:L167-L184] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_tug_30m_1_working` [scripts/python/digitalmodel/orcawave/fix_orcawave_yaml.py:L60-L128] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `fix_configuration` [scripts/python/digitalmodel/orcawave/fix_tug_30m_1_errors.py:L14-L135] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_minimal_test_config` [scripts/python/digitalmodel/orcawave/fix_tug_30m_1_errors.py:L137-L178] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `fix_working_model` [scripts/python/digitalmodel/orcawave/fix_working_model.py:L11-L93] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_minimal_with_test_box` [scripts/python/digitalmodel/orcawave/fix_working_model.py:L143-L189] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaWaveValidator.run_validation` [scripts/python/digitalmodel/orcawave/validate_orcawave_configs.py:L189-L285] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_batch_config` [scripts/python/digitalmodel/tools/add_orcaflex_test_configs.py:L14-L53] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaFlexTestStandardizer.create_standard_config` [scripts/python/digitalmodel/tools/standardize_orcaflex_tests.py:L203-L239] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `run_library` [scripts/run_riser_analysis.py:L366-L410] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `main` [scripts/run_semisub_quality_check.py:L14-L97] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `module scope` [scripts/sanitize_s7_models.py:L89-L94] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `process_dat_file` [scripts/sanitize_s7_models.py:L550-L606] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `module scope` [scripts/semantic_validate.py:L53-L53] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `convert_dat_to_yml` [scripts/sync_monolithic_yml.py:L72-L85] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `update_mooring_lines` [scripts/update_baltic_moorings_10m.py:L128-L393] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `update_mooring_lines` [scripts/update_baltic_moorings_10m_catenary.py:L207-L483] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `update_skill_file` [scripts/update_skill_versions.py:L147-L236] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `ModelValidator.extract_model_info` [scripts/validate_orcaflex_model.py:L181-L212] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_save_body` [scripts/verification/verify_orcaflex_campaign.py:L131-L143] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `main` [scripts/verification/verify_orcaflex_campaign.py:L203-L336] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `SaveData.saveDataYaml` [src/digitalmodel/asset_integrity/common/data.py:L417-L433] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `SaveData.WriteOrcaFlexYMLFile` [src/digitalmodel/asset_integrity/common/data.py:L614-L624] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `module scope` [src/digitalmodel/asset_integrity/common/data.py:L910-L941] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `StreamHandler._format_orcaflex_yml` [src/digitalmodel/data_systems/data_procurement/common/stream_handler.py:L367-L371] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `MooringClient.to_orcaflex_mooring` [src/digitalmodel/data_systems/data_procurement/mooring/client.py:L515-L585] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `ChainDatabaseClient.to_orcaflex_line_type` [src/digitalmodel/data_systems/data_procurement/mooring/database_clients/chain_db_client.py:L354-L405] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `SyntheticRopeDatabaseClient.to_orcaflex_line_type` [src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py:L416-L465] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `WireRopeDatabaseClient.to_orcaflex_line_type` [src/digitalmodel/data_systems/data_procurement/mooring/database_clients/wire_rope_db_client.py:L339-L377] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `PipeSpecificationClient.to_orcaflex_line_type` [src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py:L440-L500] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `VesselClient.to_orcaflex_vessel` [src/digitalmodel/data_systems/data_procurement/vessel/client.py:L351-L426] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `case_spec` [src/digitalmodel/drilling_riser/campaign.py:L134-L239] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_probe` [src/digitalmodel/drilling_riser/campaign.py:L313-L325] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_capped_probe` [src/digitalmodel/drilling_riser/campaign.py:L328-L336] | Mapped: Builds OrcaFlex model text or model sections | caller | not checked | none |
| `_statics_path` [src/digitalmodel/drilling_riser/campaign.py:L400-L565] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `physical_state_checks` [src/digitalmodel/drilling_riser/campaign.py:L758-L810] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_line_type` [src/digitalmodel/drilling_riser/global_model/build.py:L112-L143] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `build_generic_spec` [src/digitalmodel/drilling_riser/global_model/build.py:L180-L369] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `write_model` [src/digitalmodel/drilling_riser/global_model/build.py:L475-L492] | Mapped: Writes a modular OrcaFlex model through the modular generator | caller | not checked | none |
| `reference_periods` [src/digitalmodel/drilling_riser/global_model/hand_checks.py:L216-L232] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `build_open_water_generic_spec` [src/digitalmodel/drilling_riser/global_model/open_water.py:L143-L210] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `extract` [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L71-L90] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `merge_stress_range` [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L93-L119] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `extract_significant` [src/digitalmodel/drilling_riser/postprocess/stress_range.py:L345-L392] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `prepare_variant` [src/digitalmodel/floating_wind/solver_tier.py:L292-L325] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaFlexConverter.convert_to_vessel_yaml` [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L146-L170] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_add_fdf_surface_traces` [src/digitalmodel/hydrodynamics/diffraction/benchmark_mesh_schematic.py:L274-L317] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `run_aqwa` [src/digitalmodel/hydrodynamics/diffraction/diffraction_cli.py:L170-L230] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `DiffractionSpec.to_yaml` [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L1046-L1054] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaFlexExporter.export_vessel_type` [src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py:L71-L103] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_write_yml` [src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py:L701-L718] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaWaveRunner._execute_via_api` [src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py:L500-L567] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `PanelCatalog.to_yaml` [src/digitalmodel/hydrodynamics/hull_library/panel_catalog.py:L114-L127] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `RAOAnalysis.write_filtered_orcaflex_seastate_rao_file` [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L41-L63] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `RAOAnalysis.get_shape_data` [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L544-L579] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `FatigueAnalysisComponents.get_viv_modes_excited` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L27-L76] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `FatigueAnalysisComponents.tabulate_viv_results` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L114-L167] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `FatigueAnalysisComponents.tabulate_overall_viv_results` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L169-L204] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `FatigueAnalysisComponents.save_viv_histograms` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L364-L371] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `FatigueAnalysisComponents.save_combined_histograms` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L467-L475] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `FatigueAnalysisComponents.save_fatigue_summary_result` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L493-L519] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L586-L701] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `DNVRPH103_hydrodynamics_circular.save_model` [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L56-L69] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `DNVRPH103_hydrodynamics_circular.update_6d_buoy` [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L71-L94] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `DNVRPH103_hydrodynamics_rectangular.save_model` [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L54-L67] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `DNVRPH103_hydrodynamics_rectangular.update_6d_buoy` [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L69-L91] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaflexModelComponents.orcaflex_model` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L11-L59] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaflexModelComponents.riser_joints_to_line_types` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L64-L136] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaflexModelComponents.GetGeneral` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L138-L167] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaflexModelComponents.lineTypes` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L379-L441] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaflexModelComponents.WriteOrcaflexModel` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L791-L800] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaflexModelComponents.build_model` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L802-L857] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `FEAComponents.AssignGeneral` [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L51-L54] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `FEAComponents.AssignBuoyProperties` [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L81-L85] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `FEAComponents.AssignLineProperties` [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L87-L91] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `FEAComponents.AssignLineTypeProperties` [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L93-L97] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `FEAComponents.AssignVesselProperties` [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L99-L103] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `FEAComponents.save_output` [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L205-L211] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `FEAComponents.load_variations` [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L217-L251] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `VIVFatigueAnalysisComponents.get_viv_modes_excited` [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L40-L142] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `VIVFatigueAnalysisComponents.tabulate_viv_results` [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L200-L317] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `VIVFatigueAnalysisComponents.tabulate_overall_viv_results` [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L319-L366] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `VIVFatigueAnalysisComponents.save_viv_histograms` [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L618-L629] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `VIVFatigueAnalysisComponents.save_combined_histograms` [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L766-L782] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `VIVFatigueAnalysisComponents.save_fatigue_summary_result` [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L799-L864] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L982-L1164] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `SaveData.saveDataYaml` [src/digitalmodel/infrastructure/utils/data.py:L416-L432] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `SaveData.WriteOrcaFlexYMLFile` [src/digitalmodel/infrastructure/utils/data.py:L613-L623] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `module scope` [src/digitalmodel/infrastructure/utils/data.py:L926-L957] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `generate_input` [src/digitalmodel/marine_ops/installation/jumper_input.py:L69-L89] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `JumperInstallationSpec.to_yaml` [src/digitalmodel/marine_ops/installation/jumper_installation_schema.py:L279-L294] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `build_line_types` [src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py:L91-L166] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `build_modular_spec` [src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py:L209-L353] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `UnifiedRAOReader._detect_format` [src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py:L252-L282] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `VesselConfig.to_orcaflex_dict` [src/digitalmodel/orcaflex/model_builder.py:L113-L125] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `Buoy6DConfig.to_orcaflex_dict` [src/digitalmodel/orcaflex/model_builder.py:L147-L158] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `LineConfig.to_orcaflex_dict` [src/digitalmodel/orcaflex/model_builder.py:L183-L207] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `build_scr_model` [src/digitalmodel/orcaflex/model_builder.py:L259-L312] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `build_lazy_wave_model` [src/digitalmodel/orcaflex/model_builder.py:L315-L367] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `build_mooring_line` [src/digitalmodel/orcaflex/model_builder.py:L370-L422] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `build_model_summary` [src/digitalmodel/orcaflex/reporting/sections/model_summary.py:L20-L48] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `build_range_graphs` [src/digitalmodel/orcaflex/reporting/sections/range_graphs.py:L6-L61] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `build_rao_plots` [src/digitalmodel/orcawave/reporting/sections/rao_plots.py:L23-L129] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_write_result` [src/digitalmodel/parametric/query.py:L339-L356] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `orcawave_depth_sweep.runner` [src/digitalmodel/parametrics/pilots.py:L100-L120] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_write_diffraction_inputs` [src/digitalmodel/solvers/benchmark/solvers.py:L118-L129] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `LineType.get_general` [src/digitalmodel/solvers/fea_model/LineType_components.py:L43-L56] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `General.getGeneralOrcaFlexSetUp` [src/digitalmodel/solvers/fea_model/general_components.py:L7-L38] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `load_csv` [src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L184-L288] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `get_data` [src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L291-L555] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaFlexConfig.to_yaml` [src/digitalmodel/solvers/orcaflex/core/configuration.py:L407-L423] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaFlexModelWrapper.save_simulation` [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L509-L549] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `FeatureAnalyzer.analyze_file` [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L76-L134] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `FeatureAnalyzer.generate_summary` [src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py:L169-L195] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaFlexBatchConverter._real_convert` [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L163-L191] | Mapped: Builds OrcaFlex model text or model sections | wrapper | not checked | none |
| `OrcaFlexBatchConverter._mock_convert` [src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py:L193-L231] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `YamlConverter.convert_batch` [src/digitalmodel/solvers/orcaflex/examples_integration/converter.py:L36-L65] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `DownloadedExamplesIntegrator._infer_components` [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L108-L168] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `DownloadedExamplesIntegrator._extract_common_configs` [src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py:L356-L385] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `KnowledgeIntegrator._extract_patterns` [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L83-L102] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `KnowledgeIntegrator.build_searchable_index` [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L223-L261] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcFxAPIConverter._initialize_api` [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L55-L95] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcFxAPIConverter.convert_file` [src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py:L105-L165] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `detect_format` [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L30-L91] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `ModularToSingleConverter.convert` [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L29-L90] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `ModularToSingleConverter._merge_master` [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L107-L165] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `ModularToSingleConverter._extract_includefiles.traverse` [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L191-L200] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `ModularToSpecConverter.convert` [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L96-L222] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `SingleToModularConverter._write_master_file` [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L230-L260] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `SingleToModularConverter._write_parameters_file` [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L262-L277] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_resolve_mooring` [src/digitalmodel/solvers/orcaflex/inverse_resolver.py:L342-L472] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `_resolve_riser` [src/digitalmodel/solvers/orcaflex/inverse_resolver.py:L475-L653] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `LibraryGenerator.generate_component` [src/digitalmodel/solvers/orcaflex/library_generator.py:L234-L283] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `LibraryGenerator.write_index` [src/digitalmodel/solvers/orcaflex/library_generator.py:L379-L401] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `object_counts` [src/digitalmodel/solvers/orcaflex/model_catalogue_render.py:L120-L138] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `geometry` [src/digitalmodel/solvers/orcaflex/model_catalogue_render.py:L190-L215] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexModelGenerator.generate_from_template` [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L206-L266] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaFlexModelGenerator._assemble_model` [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L268-L335] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `ModularModelGenerator.generate` [src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L154-L201] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `ModularModelGenerator._write_parameters` [src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L228-L246] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `ModularModelGenerator._write_master` [src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L248-L262] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `ModularModelGenerator.generate_with_overrides` [src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L264-L373] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `GeneralBuilder.build` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/general_builder.py:L23-L81] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `GenericModelBuilder.build` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py:L201-L281] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `LinesBuilder.build` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/lines_builder.py:L42-L79] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `LineTypeBuilder.build` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py:L85-L158] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `RiserLinesBuilder.build` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_lines_builder.py:L40-L59] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `RiserLineTypeBuilder.build` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_linetype_builder.py:L34-L54] | Mapped: Builds OrcaFlex model text or model sections | caller | not checked | none |
| `RiserLineTypeBuilder._build_riser_line_type` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_linetype_builder.py:L56-L137] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `RiserVesselBuilder.build` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_vessel_builder.py:L32-L61] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `VesselBuilder.build` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_builder.py:L34-L72] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `WinchBuilder.build` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py:L36-L81] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `cmd_generate` [src/digitalmodel/solvers/orcaflex/modular_generator/cli.py:L170-L307] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `MonolithicExtractor._extract_simulation` [src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py:L368-L385] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `MonolithicExtractor._extract_general_properties` [src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py:L568-L584] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `PostGenerationValidator._check_line_type_references` [src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py:L281-L338] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `PostGenerationValidator._check_vessel_references` [src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py:L340-L412] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `PostGenerationValidator._check_winch_references` [src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py:L414-L480] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_iter_includefile_refs.traverse` [src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py:L747-L756] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `MooringRouter.route` [src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py:L105-L143] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `MooringRouter._chain_linetype` [src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py:L161-L199] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `MooringRouter._wire_linetype` [src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py:L201-L234] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `MooringRouter._polyester_linetype` [src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py:L236-L268] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `VesselRouter.route` [src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py:L28-L54] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `VesselRouter.from_hull_catalog` [src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py:L57-L112] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `CampaignGenerator.generate` [src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py:L539-L662] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `write_variation_model` [src/digitalmodel/solvers/orcaflex/modular_generator/writers/basefile.py:L194-L222] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `extract_includefiles.traverse` [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L45-L54] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `ModularInputValidator._collect_issues_from_level_1` [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L219-L247] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `Mooring.prepare_includefile_for_all_lines` [src/digitalmodel/solvers/orcaflex/mooring.py:L509-L571] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `Mooring.prepare_includefile_for_fender_compression` [src/digitalmodel/solvers/orcaflex/mooring.py:L573-L622] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `MockOrcaFlexAPI.load_model` [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L120-L136] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaFlexAPI.save_model` [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L255-L261] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OperabilityAnalyzer.generate_operability_envelope` [src/digitalmodel/solvers/orcaflex/operability_analysis.py:L83-L198] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OPPVisualization.combine_models` [src/digitalmodel/solvers/orcaflex/opp_visualization.py:L128-L137] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexAnalysis.save_data` [src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py:L107-L125] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexAnalysis.saveSummaryToExcel` [src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py:L285-L294] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexAnalysis.save_histograms` [src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py:L819-L827] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexAnalysis.save_cfg_files_from_multiple_files` [src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py:L853-L860] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexAnalysis.save_RAOs` [src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py:L963-L977] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexConverterEnhanced._real_convert` [src/digitalmodel/solvers/orcaflex/orcaflex_converter_enhanced.py:L234-L263] | Mapped: Builds OrcaFlex model text or model sections | wrapper | not checked | this pack |
| `OrcaFlexConverterEnhanced._mock_convert` [src/digitalmodel/solvers/orcaflex/orcaflex_converter_enhanced.py:L265-L299] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexCustomAnalysis.process_fea` [src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py:L83-L196] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexCustomAnalysis.clean_model` [src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py:L198-L215] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexCustomAnalysis.save_model_with_calculated_positions` [src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py:L244-L262] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexCustomAnalysis.iterate_to_target_value` [src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py:L264-L325] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexCustomAnalysis.save_data` [src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py:L342-L379] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcInstallation.create_model_for_water_depth` [src/digitalmodel/solvers/orcaflex/orcaflex_installation.py:L29-L103] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcInstallation.create_installation_depth_model` [src/digitalmodel/solvers/orcaflex/orcaflex_installation.py:L159-L197] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcInstallation.create_installation_depth_model2` [src/digitalmodel/solvers/orcaflex/orcaflex_installation.py:L199-L240] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaflexIterativeRuns.save_iterative_file` [src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py:L64-L72] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaflexIterativeRuns.run_iterative_simulations` [src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py:L152-L173] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaflexLineTypes.get_umbilical_properties` [src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py:L25-L33] | Mapped: Builds OrcaFlex model text or model sections | wrapper | not checked | this pack |
| `OrcaflexLineTypes.assign_key_umbilical_properties` [src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py:L35-L60] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaflexLineTypes.save_model` [src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py:L62-L72] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaflexModelComponents.orcaflex_model` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L14-L81] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaflexModelComponents.riser_joints_to_line_types` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L86-L177] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `OrcaflexModelComponents.GetGeneral` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L179-L208] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `OrcaflexModelComponents.lineTypes` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L501-L591] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `OrcaflexModelComponents.WriteOrcaflexModel` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L1233-L1245] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaflexModelComponents.build_model` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L1247-L1366] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexOptimizedParallelAnalysis.process_single_file` [src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel.py:L89-L187] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `process_single_file_worker` [src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel_v2.py:L39-L137] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexParallelAnalysis.process_single_file` [src/digitalmodel/solvers/orcaflex/orcaflex_parallel_analysis.py:L59-L159] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaflexUtilities.update_model` [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L136-L149] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaflexUtilities.update_input_file` [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L164-L222] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaflexUtilities.get_full_yaml_file_and_save` [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L444-L469] | Mapped: Builds OrcaFlex model text or model sections | caller | not checked | this pack |
| `OrcaflexUtilities.get_general_yml` [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L471-L479] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaflexUtilities.save_cfg_files_from_multiple_files` [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L725-L734] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `convert_to_yml` [src/digitalmodel/solvers/orcaflex/orcaflex_yml_converter.py:L15-L48] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `PipelineSchematicGenerator._merge_yaml` [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L180-L197] | Mapped: Builds OrcaFlex model text or model sections | wrapper | not checked | this pack |
| `PipelineSchematicGenerator._merge_lines` [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L199-L226] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `PipelineSchematicGenerator._merge_6dbuoys` [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L228-L267] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `PipelineSchematicGenerator._parse_pipeline_path` [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L269-L291] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `PipelineSchematicGenerator._parse_boundary_conditions` [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L293-L461] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `PipelineSchematicGenerator.create_plan_view` [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L513-L602] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `PipelineSchematicGenerator.create_elevation_view` [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L604-L723] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `LoadVessel.aqwa_group_item` [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L24-L100] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_extract_dynamic_results_all` [src/digitalmodel/solvers/orcaflex/reporting/extractors/aggregator.py:L259-L284] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `RiserRenderer._build_riser_dynamic_results` [src/digitalmodel/solvers/orcaflex/reporting/renderers/riser.py:L34-L62] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `find_model_files` [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L236-L268] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `AuthoredSpec.to_spec_yaml` [src/digitalmodel/solvers/orcaflex/spec_author.py:L403-L410] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `TemplateGenerator.generate` [src/digitalmodel/solvers/orcaflex/template_generator.py:L327-L380] | Mapped: Builds OrcaFlex model text or model sections | caller | not checked | this pack |
| `TemplateGenerator._generate_reference` [src/digitalmodel/solvers/orcaflex/template_generator.py:L382-L420] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `InstallationVtoHost.installation_step` [src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py:L27-L44] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `InstallationVtoHost.get_target_line_length` [src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py:L54-L88] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `InstallationVtoHost.get_target_reference_distance` [src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py:L90-L152] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `Lower2ndEnd.installation_step` [src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py:L27-L44] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `Lower2ndEnd.get_target_line_length` [src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py:L54-L88] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `Lower2ndEnd.get_target_reference_distance` [src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py:L90-L189] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `UniversalOrcaFlexRunner._process_single_model` [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L371-L460] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `create_config` [src/digitalmodel/solvers/orcaflex/universal_cli.py:L337-L374] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `_mapping_issues` [src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py:L97-L112] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `inspect_source` [src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py:L115-L141] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `orcaflex_dump` [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L63-L85] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `orcaflex_load` [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L88-L115] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexYAMLValidator._detect_format` [src/digitalmodel/solvers/orcaflex/yaml_validator.py:L266-L290] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaFlexYAMLValidator._check_singular_section_keys` [src/digitalmodel/solvers/orcaflex/yaml_validator.py:L707-L721] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | this pack |
| `OrcaWaveOrchestrator._create_vessel_config` [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L71-L116] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaWaveOrchestrator.list_available_vessels` [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L652-L665] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaFlexConverter.save_orcaflex_file` [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L402-L420] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaFlexConverter._save_compact_format` [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L422-L465] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_tree` [src/digitalmodel/solvers/smoke/model_manifest.py:L96-L115] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `check_settings` [src/digitalmodel/solvers/smoke/model_probe.py:L36-L62] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_saved_proof` [src/digitalmodel/solvers/smoke/probes.py:L137-L165] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `prepareFatiguePostProcessing` [src/digitalmodel/subsea/catenary_riser/legacy/getFatigueLoading.py:L69-L89] | Mapped: Builds fatigue post-processing model sections from S-N configuration | independent | not checked | none |
| `orcaflexModel` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L13-L357] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `lineTypes` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L555-L629] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `WriteOrcaflexModel` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L1211-L1219] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `build_model` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L1222-L1288] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaFlexModelGenerator.generate_model_yml` [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L165-L216] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaFlexModelGenerator.generate_model_dict` [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L218-L247] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `VerticalRiser.get_fea_properties` [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L120-L261] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `save_echo` [src/digitalmodel/viv_screening/workflow.py:L254-L273] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaFlex.get` [src/digitalmodel/web/digitaltwinfeed/BuoySALM/BuoySALM.py:L83-L106] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaFlex.get` [src/digitalmodel/web/digitaltwinfeed/FFS/FFS.py:L68-L86] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaFlex.get` [src/digitalmodel/web/digitaltwinfeed/OffshoreRiser/OffshoreRiser.py:L75-L98] | Mapped: Builds OrcaFlex model text or model sections; AST-equivalent definition after docstring removal | copy | not checked | none |
| `AnalysisMetadataGenerator._detect_workflow_type` [src/digitalmodel/workflows/automation/go_by_folder/metadata/analysis.py:L340-L360] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `integration_basis` [src/digitalmodel/workflows/installation_added_mass_bracket.py:L21-L28] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `named_body` [src/digitalmodel/workflows/installation_added_mass_bracket.py:L45-L49] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `expected_change` [src/digitalmodel/workflows/installation_added_mass_bracket.py:L94-L98] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `read_cases` [src/digitalmodel/workflows/installation_added_mass_evidence.py:L141-L162] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_write_request` [src/digitalmodel/workflows/installation_added_mass_materialize.py:L107-L117] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `materialize_bracket_case` [src/digitalmodel/workflows/installation_added_mass_materialize.py:L130-L170] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_source` [src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py:L24-L47] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_case_files` [src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py:L88-L118] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_apply_change` [src/digitalmodel/workflows/installation_numerical_compare.py:L126-L146] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_canonical_tables` [src/digitalmodel/workflows/installation_numerical_compare.py:L166-L178] | Mapped: Builds OrcaFlex model text or model sections | wrapper | not checked | none |
| `_runtime` [src/digitalmodel/workflows/installation_numerical_compare.py:L214-L233] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_field` [src/digitalmodel/workflows/installation_numerical_prepare.py:L21-L25] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_line` [src/digitalmodel/workflows/installation_numerical_prepare.py:L28-L35] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_value` [src/digitalmodel/workflows/installation_numerical_prepare.py:L38-L49] | Mapped: Builds OrcaFlex model text or model sections | caller | not checked | none |
| `_anchors` [src/digitalmodel/workflows/installation_numerical_prepare.py:L61-L86] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_timing` [src/digitalmodel/workflows/installation_numerical_prepare.py:L89-L97] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_ordered` [src/digitalmodel/workflows/installation_numerical_prepare.py:L108-L138] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_apply_native` [src/digitalmodel/workflows/installation_numerical_prepare.py:L161-L165] | Mapped: Builds OrcaFlex model text or model sections | wrapper | not checked | none |
| `_counts` [src/digitalmodel/workflows/installation_numerical_prepare.py:L168-L176] | Mapped: Builds OrcaFlex model text or model sections | wrapper | not checked | none |
| `_variation` [src/digitalmodel/workflows/installation_numerical_prepare.py:L179-L187] | Mapped: Builds OrcaFlex model text or model sections | caller | not checked | none |
| `_native_case` [src/digitalmodel/workflows/installation_numerical_prepare.py:L190-L211] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_request` [src/digitalmodel/workflows/installation_numerical_prepare.py:L214-L223] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_snapshot` [src/digitalmodel/workflows/installation_seastates.py:L41-L44] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_structure` [src/digitalmodel/workflows/installation_seastates.py:L47-L57] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_request` [src/digitalmodel/workflows/installation_seastates.py:L154-L166] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `generate_case` [src/digitalmodel/workflows/installation_seastates.py:L169-L200] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `prepare_matrix` [src/digitalmodel/workflows/installation_seastates.py:L203-L241] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_change_payload` [src/digitalmodel/workflows/installation_seastates.py:L244-L258] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `materialize_case` [src/digitalmodel/workflows/installation_seastates.py:L261-L316] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_verify_master` [src/digitalmodel/workflows/installation_variant_study.py:L72-L79] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_verify_settings` [src/digitalmodel/workflows/installation_variant_study.py:L82-L93] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `prepare_variant_study` [src/digitalmodel/workflows/installation_variant_study.py:L117-L165] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `OrcaWaveMCPServer._register_tools.export_to_orcaflex` [src/digitalmodel/workflows/mcp_server/orcawave/core/mcp_server.py:L486-L542] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_prepare` [src/digitalmodel/workflows/orcaflex_reproduce.py:L230-L237] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `extract_results` [src/digitalmodel/workflows/orcaflex_reproduce_results.py:L153-L184] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_write_yaml` [src/digitalmodel/workflows/parametric_run.py:L210-L212] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `SkillResolver._parse_skill_file` [src/digitalmodel/workflows/skills/skill_resolver.py:L98-L134] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_model` [src/digitalmodel/workflows/structure_hydrodynamic_candidate.py:L152-L167] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_apply` [src/digitalmodel/workflows/structure_hydrodynamic_candidate.py:L170-L185] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `build_candidate` [src/digitalmodel/workflows/structure_hydrodynamic_candidate.py:L208-L257] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `_dependencies` [src/digitalmodel/workflows/structure_installation_prepare.py:L30-L42] | Mapped: Builds OrcaFlex model text or model sections | independent | not checked | none |
| `base_spec` [tests/drilling_riser/global_model/test_batch_params.py:L23-L28] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `base_spec` [tests/drilling_riser/global_model/test_campaign.py:L21-L26] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `_case` [tests/drilling_riser/global_model/test_event_models.py:L20-L23] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_generic` [tests/drilling_riser/global_model/test_hang_off.py:L75-L77] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_case` [tests/drilling_riser/global_model/test_hang_off.py:L110-L115] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_ow_case` [tests/drilling_riser/global_model/test_open_water.py:L354-L362] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `base_spec` [tests/drilling_riser/global_model/test_statics_path.py:L28-L33] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `_line` [tests/drilling_riser/global_model/test_tensioner_representation.py:L31-L32] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `base_spec` [tests/drilling_riser/global_model/test_w5_channels.py:L73-L78] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `_doc` [tests/drilling_riser/postprocess/test_w510_significant_range.py:L174-L185] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `setup` [tests/legal/test_sanitize_s7_audit_private.py:L47-L64] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_deep_reference_and_registry_paths_are_excluded` [tests/maintenance/test_archive_safety.py:L59-L70] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_generator_partitions_move_keep_hold_and_excluded` [tests/maintenance/test_archive_safety.py:L256-L303] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_specs_match_basis_and_static_geometry` [tests/orcaflex/test_generic_scr_slwr.py:L56-L103] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_analytical_reference_reproduces_from_basis` [tests/orcaflex/test_generic_scr_slwr.py:L150-L159] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestModularToSpecBoundaryReporting.test_expected_gaps_classified` [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L393-L409] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestModularToSpecBoundaryReporting.test_actionable_gaps_classified` [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L411-L425] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestModularToSpecBoundaryReporting.test_all_unmapped_are_categorized` [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L436-L450] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_build` [tests/solvers/orcaflex/modular_generator/builders/test_general_builder.py:L54-L58] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_build` [tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py:L67-L74] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_build` [tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py:L72-L79] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `main` [tests/solvers/orcaflex/modular_generator/goldens/basefile/capture.py:L79-L135] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_write_floating_campaign` [tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py:L30-L120] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_write_slay_campaign` [tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py:L126-L219] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_write_campaign_yaml` [tests/solvers/orcaflex/modular_generator/test_campaign_generator.py:L98-L102] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_write_model` [tests/solvers/orcaflex/modular_generator/test_collection_collision.py:L52-L74] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_sections` [tests/solvers/orcaflex/modular_generator/test_gom_tieback_jumper_pipeline.py:L45-L48] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_generation_writes_master_and_includes` [tests/solvers/orcaflex/modular_generator/test_gom_tieback_jumper_pipeline.py:L87-L118] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestJumperLineTypes.test_three_jumper_variants_emitted` [tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py:L108-L120] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestJumperLineReferences.test_jumper_line_uses_all_three_variants` [tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py:L187-L209] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_assert_line_type_references_resolve` [tests/solvers/orcaflex/modular_generator/test_riser_variant_semantic_proof.py:L74-L87] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_write_mono_yaml` [tests/solvers/orcaflex/modular_generator/test_semantic_roundtrip.py:L31-L36] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `find_model_files` [tests/solvers/orcaflex/mooring-tension-iteration/fsts-l015-test-cases/scripts/run_models_to_sim.py:L198-L228] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_build_boundary_conditions` [tests/solvers/orcaflex/reporting/fixture_helpers.py:L126-L158] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `SingleFileParallelStrategy._get_line_results` [tests/solvers/orcaflex/single_file_parallel_test.py:L135-L147] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `SingleFileParallelStrategy._get_vessel_results` [tests/solvers/orcaflex/single_file_parallel_test.py:L149-L159] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_structural_owners_and_path_references` [tests/solvers/orcaflex/test_model_catalogue.py:L203-L227] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_same_named_unrelated_folders_are_not_references` [tests/solvers/orcaflex/test_model_catalogue_references.py:L24-L45] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `header_repository_findings` [tests/solvers/orcaflex/test_model_header_guard.py:L25-L45] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `master_with_missing_include` [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L51-L54] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_support_types_are_defined_before_vessel_references` (`tests/solvers/orcaflex/test_slay_layout.py`) | Absent at code revision; map row retained as a missing-path record | not checked | not checked | none |
| `_resolve_case.read` (`tests/solvers/orcaflex/test_slay_load_form.py`) | Absent at code revision; map row retained as a missing-path record | not checked | not checked | none |
| `_pipe_seed_points` (`tests/solvers/orcaflex/test_slay_load_form.py`) | Absent at code revision; map row retained as a missing-path record | not checked | not checked | none |
| `sample_base_template` [tests/solvers/orcaflex/test_template_generator.py:L42-L70] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `sample_variation` [tests/solvers/orcaflex/test_template_generator.py:L74-L97] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `sample_case_template` [tests/solvers/orcaflex/test_template_generator.py:L101-L120] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestTemplateGenerator.test_generate_with_basefile_reference` [tests/solvers/orcaflex/test_template_generator.py:L261-L283] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestTemplateGenerator.test_reference_paths_resolve_from_arbitrary_output_dir` [tests/solvers/orcaflex/test_template_generator.py:L285-L317] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestTemplateGenerator.test_reference_paths_in_cases_dir_stay_relative` [tests/solvers/orcaflex/test_template_generator.py:L319-L343] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `manifest` [tests/solvers/smoke/test_model_manifest.py:L16-L31] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `temp_config_file` [tests/test_engine.py:L814-L829] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_solve_input` [tests/test_main_exit_status.py:L110-L132] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_write_skill` [tests/test_skill_resolver.py:L22-L65] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_workflow_registry` [tests/workflows/test_durable_workflows.py:L52-L1680] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_synthetic_rope_tn_curve_matches_dnv_os_e301_table_f3` [tests/workflows/test_durable_workflows.py:L1760-L1822] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` [tests/workflows/test_durable_workflows.py:L1825-L1881] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` [tests/workflows/test_durable_workflows.py:L1884-L1951] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_vessel_seakeeping_significant_amplitude_matches_library` [tests/workflows/test_durable_workflows.py:L1954-L2028] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `fake` [tests/workflows/test_installation_instability_diagnostic.py:L47-L58] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_duration_render_preserves_native_default_spelling_and_source` [tests/workflows/test_orcaflex_batch_contract.py:L97-L121] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `resume_case` [tests/workflows/test_orcaflex_reproduce.py:L107-L127] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `case` [tests/workflows/test_structure_hydrodynamic_candidate.py:L16-L52] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |

#### `rao-readers-and-orcaflex-converters`

| Pinned implementation | Map classification | Relation | Controls | Covering pack |
| --- | --- | --- | --- | --- |
| `main` [docs/domains/diffraction/examples/orcawave_conversion_example.py:L55-L357] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [docs/domains/orcawave/L01_aqwa_benchmark/compare_model_inputs.py:L61-L119] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_aqwa_raos` [docs/domains/orcawave/L01_aqwa_benchmark/create_interactive_rao_comparison.py:L22-L59] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_orcawave_raos` [docs/domains/orcawave/L01_aqwa_benchmark/create_interactive_rao_comparison.py:L62-L119] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [docs/domains/orcawave/L01_aqwa_benchmark/create_interactive_rao_comparison.py:L787-L814] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `module scope` [docs/domains/orcawave/L01_aqwa_benchmark/extract_orcawave_raos.py:L14-L14] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_orcawave_raos` [docs/domains/orcawave/L01_aqwa_benchmark/extract_orcawave_raos.py:L25-L131] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `monitor_progress` [docs/domains/orcawave/L01_aqwa_benchmark/monitor_orcawave_run.py:L11-L53] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_aqwa_data` [docs/domains/orcawave/L01_aqwa_benchmark/run_comparison.py:L34-L62] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_orcawave_data` [docs/domains/orcawave/L01_aqwa_benchmark/run_comparison.py:L65-L104] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [docs/domains/orcawave/L01_aqwa_benchmark/run_comparison.py:L382-L462] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_orcawave_raos` [docs/domains/orcawave/L01_aqwa_benchmark/run_comparison_with_interpolation.py:L460-L490] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [docs/domains/orcawave/L01_aqwa_benchmark/run_comparison_with_interpolation.py:L493-L561] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_orcawave_raos` [docs/domains/orcawave/L01_aqwa_benchmark/run_heading_specific_comparison.py:L495-L521] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [docs/domains/orcawave/L01_aqwa_benchmark/run_heading_specific_comparison.py:L524-L581] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `module scope` [docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_api.py:L21-L21] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_matched_api.py:L131-L160] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_orcawave_motion_raos` [docs/domains/orcawave/L01_aqwa_benchmark/run_proper_comparison.py:L434-L480] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [docs/domains/orcawave/L01_aqwa_benchmark/run_proper_comparison.py:L483-L548] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [docs/domains/orcawave/L01_aqwa_benchmark/validate_matched_model.py:L84-L104] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `run_orcawave_diffraction` [docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction.py:L29-L187] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `module scope` [docs/domains/orcawave/examples/qa/orcawave_example_qa.py:L28-L28] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_check_raos_finite` [docs/domains/orcawave/examples/qa/orcawave_example_qa.py:L122-L137] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_check_heave_rao_long_period` [docs/domains/orcawave/examples/qa/orcawave_example_qa.py:L140-L173] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `example_realistic_usage.load_hydrodynamic_database` [examples/domains/core/cache_example.py:L238-L261] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_rao_polar_data_validation` [examples/domains/validation/validation_pipeline_example.py:L58-L92] | Mapped: Demonstrates the family method | caller | not checked | none |
| `module scope` [scripts/benchmark/compare_hydro_properties.py:L38-L38] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_aqwa_damping` [scripts/benchmark/compare_hydro_properties.py:L220-L229] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_aqwa_added_mass` [scripts/benchmark/compare_hydro_properties.py:L232-L241] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `parse_args` [scripts/benchmark/compare_hydro_properties.py:L1228-L1250] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `parse_args` [scripts/benchmark/digitize_validation_plots.py:L438-L469] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_extract_core_data` [scripts/benchmark/probe_owd_content.py:L228-L299] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `probe_body_details` [scripts/benchmark/probe_owd_content.py:L307-L396] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `module scope` [scripts/benchmark/qtf_postprocessing.py:L47-L47] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `module scope` [scripts/benchmark/regenerate_barge_benchmark.py:L20-L20] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `module scope` [scripts/benchmark/regenerate_ship_benchmark.py:L18-L18] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `module scope` [scripts/benchmark/rerun_benchmarks_with_fidp.py:L33-L33] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_extract_correlations_from_json` [scripts/benchmark/rerun_benchmarks_with_fidp.py:L301-L320] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_extract_correlations_from_runner` [scripts/benchmark/rerun_benchmarks_with_fidp.py:L330-L348] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_run_ship_benchmark` [scripts/benchmark/rerun_benchmarks_with_fidp.py:L356-L501] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_run_spar_benchmark` [scripts/benchmark/rerun_benchmarks_with_fidp.py:L614-L694] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [scripts/benchmark/rerun_benchmarks_with_fidp.py:L835-L915] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `run_orcawave` [scripts/benchmark/run_3way_benchmark.py:L95-L207] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `run_aqwa` [scripts/benchmark/run_3way_benchmark.py:L210-L291] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_extract_from_owr` [scripts/benchmark/run_3way_benchmark.py:L399-L553] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_extract_from_aqwa_lis` [scripts/benchmark/run_3way_benchmark.py:L556-L769] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_parse_aqwa_raos` [scripts/benchmark/run_3way_benchmark.py:L772-L893] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_parse_aqwa_matrix_section` [scripts/benchmark/run_3way_benchmark.py:L896-L1012] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `module scope` [scripts/benchmark/run_spar_benchmark.py:L24-L24] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [scripts/benchmark/run_spar_benchmark.py:L155-L341] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_run_orcawave_for_case` [scripts/benchmark/run_validation_benchmark.py:L145-L258] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_load_wamit_reference` [scripts/benchmark/run_validation_benchmark.py:L266-L289] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `module scope` [scripts/benchmark/solve_owd_extract.py:L24-L26] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `solve_and_extract` [scripts/benchmark/solve_owd_extract.py:L116-L302] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_extract_from_diffraction` [scripts/benchmark/validate_owd_vs_spec.py:L201-L389] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `solve_owd` [scripts/benchmark/validate_owd_vs_spec.py:L426-L543] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `solve_spec` [scripts/benchmark/validate_owd_vs_spec.py:L546-L657] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_solve_spec_via_orcfxapi` [scripts/benchmark/validate_owd_vs_spec.py:L660-L761] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `module scope` [scripts/build_results_workbook.py:L33-L34] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [scripts/populate_rao_database.py:L15-L27] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [scripts/python/digitalmodel/legacy/aqwa_data_helper.py:L151-L219] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `test_actual_file_counts` [scripts/python/digitalmodel/legacy/check_aqwa_heading_fix.py:L134-L210] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [scripts/python/digitalmodel/legacy/check_aqwa_parser_fix.py:L103-L116] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/diagnose_rao_parsing.py:L3-L55] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `parse_rao_section_simple` [scripts/python/digitalmodel/legacy/direct_rao_plot.py:L15-L87] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [scripts/python/digitalmodel/legacy/direct_rao_plot.py:L137-L199] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [scripts/python/digitalmodel/legacy/fix_rao_plots.py:L19-L249] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/fix_rao_plots_standalone.py:L3-L195] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [scripts/python/digitalmodel/legacy/generate_rao_qa_report.py:L694-L717] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [scripts/python/digitalmodel/legacy/regenerate_rao_plots.py:L186-L195] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/simple_rao_plot.py:L5-L5] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `test_rao_section` [scripts/python/digitalmodel/legacy/verify_parser_extraction.py:L57-L173] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [scripts/python/digitalmodel/legacy/verify_parser_extraction.py:L175-L187] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `parse_args` [scripts/rao_quality_check.py:L41-L121] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `process_file` [scripts/rao_quality_check.py:L182-L226] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `create_diffraction_results` [scripts/run_benchmark_ship_raos.py:L82-L226] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [scripts/run_benchmark_ship_raos.py:L229-L380] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [scripts/run_semisub_quality_check.py:L14-L97] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `GenericRAOClient._generate_generic_raos` [src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py:L221-L273] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `GenericRAOClient.interpolate_raos` [src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py:L344-L397] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `GenericRAOClient.stream_raos_orcaflex` [src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py:L399-L429] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `GenericRAOClient.query_by_date` [src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py:L431-L460] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `VesselClient._initialize_clients` [src/digitalmodel/data_systems/data_procurement/vessel/client.py:L103-L136] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_vessel_type` [src/digitalmodel/drilling_riser/global_model/build.py:L82-L109] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `parse_direction_grouped_rao_text` [src/digitalmodel/drilling_riser/global_model/raos.py:L29-L56] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AqwaRAOs.get_natural_period_from_lis_file` [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L254-L285] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AqwaRAOs.get_percent_critical_damping` [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L301-L334] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AqwaRAOs.get_absolute_damping` [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L336-L356] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AqwaRAOs.get_raos_by_cfg` [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L675-L722] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `create_parser` [src/digitalmodel/hydrodynamics/aqwa/aqwa_cli.py:L16-L81] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `list_methods` [src/digitalmodel/hydrodynamics/aqwa/aqwa_cli.py:L84-L119] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AqwaReader.get_change_of_data_result_group` [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L166-L223] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AqwaReader.get_property_args_array` [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L272-L288] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `ViscousDampingDetermination._process_case` [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L176-L294] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `get_module_info` [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L149-L162] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `convert` [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L51-L120] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_export_raos_csv` [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L181-L210] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `status` [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L547-L575] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaFlexConverter._call_orcaflex_exporter` [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L123-L144] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaFlexConverter._build_vessel_yaml` [src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py:L172-L206] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `DATHandler._parse_aqwa_deck` [src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py:L221-L306] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWAParser.supported_extensions` [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L58-L60] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWAParser.parse` [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L67-L142] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWAParser._extract_raos` [src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py:L389-L515] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWAConverter._extract_rao_data` [src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py:L115-L176] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `AQWAConverter._extract_added_mass_data` [src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py:L178-L213] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `AQWAConverter._extract_damping_data` [src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py:L215-L246] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `AQWAConverter._build_rao_set` [src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py:L248-L300] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWAConverter._build_added_mass_set` [src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py:L302-L343] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWAConverter._build_damping_set` [src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py:L345-L382] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWALISParser.extract_frequencies_and_periods` [src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py:L64-L123] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWALISParser.extract_headings` [src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py:L125-L163] | Mapped: Reads or converts hydrodynamic response data; affected by #2107 | independent | not checked | none |
| `AQWALISParser._table_span` [src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py:L176-L192] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWALISParser.parse_added_mass_table` [src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py:L194-L254] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `AQWALISParser.parse_damping_table` [src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py:L256-L314] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `AQWALISParser.parse_rao_table` [src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py:L316-L510] | Mapped: Reads or converts hydrodynamic response data; affected by #2107 | independent | not checked | none |
| `AQWALISParser.parse_all` [src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py:L512-L550] | Mapped: Reads or converts hydrodynamic response data | wrapper | not checked | none |
| `parse_aqwa_lis_file` [src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py:L555-L566] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_aqwa_frequencies` [src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py:L569-L572] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_aqwa_headings` [src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py:L575-L578] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_aqwa_added_mass` [src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py:L581-L584] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_aqwa_damping` [src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py:L587-L590] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_aqwa_raos` [src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py:L593-L596] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWAResultExtractor.extract` [src/digitalmodel/hydrodynamics/diffraction/aqwa_result_extractor.py:L42-L87] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWARunner._capture_lis_file` [src/digitalmodel/hydrodynamics/diffraction/aqwa_runner.py:L717-L728] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `convert_aqwa` [src/digitalmodel/hydrodynamics/diffraction/cli.py:L108-L197] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `create_parser` [src/digitalmodel/hydrodynamics/diffraction/diffraction_cli.py:L15-L111] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaFlexExporter.export_raos_csv` [src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py:L105-L162] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaFlexExporter._write_rao_sheet` [src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py:L363-L387] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaFlexExporter._write_discretization_sheet` [src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py:L424-L442] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaWaveRunner._execute_via_api` [src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py:L500-L567] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaWaveRunner._read_live_attrs` [src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py:L666-L678] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaWaveRunner._build_results_from_object` [src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py:L680-L820] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `MockDataGenerator.create_mock_vessel` [src/digitalmodel/hydrodynamics/diffraction/orcawave_test_utilities.py:L301-L417] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `convert_orcawave_xlsx_to_orcaflex` [src/digitalmodel/hydrodynamics/diffraction/orcawave_to_orcaflex.py:L252-L303] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `module scope` [src/digitalmodel/hydrodynamics/diffraction/orcawave_to_orcaflex.py:L372-L373] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `PolarsExporter.export_all_csv` [src/digitalmodel/hydrodynamics/diffraction/polars_exporter.py:L111-L116] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `PolarsExporter._build_rao_records` [src/digitalmodel/hydrodynamics/diffraction/polars_exporter.py:L122-L143] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `module scope` [src/digitalmodel/hydrodynamics/diffraction/report_generator.py:L56-L65] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaWaveInputParser.parse` [src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py:L426-L482] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaWaveConverter._extract_rao_data` [src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py:L137-L161] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `OrcaWaveConverter._build_rao_set` [src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py:L199-L247] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaWaveConverter._build_added_mass_set` [src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py:L249-L281] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaWaveConverter._build_damping_set` [src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py:L283-L315] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaWaveDataExtractor.extract_rao_frequencies` [src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_data_extraction.py:L38-L69] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaWaveDataExtractor.extract_rao_headings` [src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_data_extraction.py:L71-L97] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `extract_all_rao_data` [src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_data_extraction.py:L365-L393] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `WamitReferenceLoader.from_owd` [src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py:L83-L183] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `WamitReferenceLoader.from_yaml` [src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py:L186-L311] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `WamitReferenceLoader._build_results` [src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py:L416-L558] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_attempt_rao_lookup` [src/digitalmodel/hydrodynamics/hull_library/analysis_setup.py:L214-L261] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `setup_hull_analysis` [src/digitalmodel/hydrodynamics/hull_library/analysis_setup.py:L269-L371] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `HullCatalog.compute_motions` [src/digitalmodel/hydrodynamics/hull_library/catalog.py:L260-L315] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `HullCatalog.compute_accelerations` [src/digitalmodel/hydrodynamics/hull_library/catalog.py:L321-L409] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `RAODatabase.save_to_disk` [src/digitalmodel/hydrodynamics/hull_library/rao_database.py:L163-L207] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `RAODatabase.load_from_disk` [src/digitalmodel/hydrodynamics/hull_library/rao_database.py:L209-L250] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `PipelineXlsxReader.read_rao_data` [src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py:L125-L192] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `NativeXlsxReader.read_rao_data` [src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py:L275-L364] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `xlsx_to_rao_data` [src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py:L469-L493] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `xlsx_to_hydro_coefficients` [src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py:L496-L515] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `populate_database_from_xlsx` [src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py:L518-L561] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `compare_box_exports` [src/digitalmodel/hydrodynamics/hull_library/rao_fixture_comparison.py:L48-L85] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_read_units` [src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py:L54-L94] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_same_content` [src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py:L189-L193] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `store_fixture_entry` [src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py:L207-L225] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `populate_fixture_database` [src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py:L258-L292] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_verify_readback` [src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py:L295-L304] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `regenerate_fixture_database` [src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py:L307-L350] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `RaoRegistry.register_rao` [src/digitalmodel/hydrodynamics/hull_library/rao_registry.py:L62-L105] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `RaoRegistry._store_content` [src/digitalmodel/hydrodynamics/hull_library/rao_registry.py:L107-L134] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `RaoRegistry.load_rao_data` [src/digitalmodel/hydrodynamics/hull_library/rao_registry.py:L164-L185] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `_read_payload` [src/digitalmodel/hydrodynamics/hull_library/rao_registry.py:L231-L240] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `CoefficientsInterpolator.load_raos` [src/digitalmodel/hydrodynamics/interpolator.py:L59-L66] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `CoefficientsInterpolator.interpolate_rao_1d` [src/digitalmodel/hydrodynamics/interpolator.py:L68-L110] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `CoefficientsInterpolator.interpolate_rao_2d` [src/digitalmodel/hydrodynamics/interpolator.py:L112-L163] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `CoefficientsInterpolator.interpolate_all_dofs` [src/digitalmodel/hydrodynamics/interpolator.py:L165-L207] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `CoefficientsInterpolator.extract_rao_at_frequency` [src/digitalmodel/hydrodynamics/interpolator.py:L230-L267] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `CoefficientsInterpolator.extract_rao_at_direction` [src/digitalmodel/hydrodynamics/interpolator.py:L269-L306] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `run_parametric_sweep` [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L27-L178] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_to_rao_db_entry` [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L287-L310] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `RAOAnalysis.read_orcaflex_displacement_raos` [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L16-L33] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `RAOAnalysis.get_rao_data_for_vessel` [src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py:L335-L346] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `router` [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L25-L46] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `_database` [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L69-L73] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_rao_array` [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L106-L124] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_interpolated_rows` [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L203-L219] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `_check_query_bounds` [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L222-L234] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_extract_rao_dict` [src/digitalmodel/installation/installation_pamphlet/calculations.py:L156-L189] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `load_rao_set_from_artifact` [src/digitalmodel/installation/installation_pamphlet/calculations.py:L192-L223] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `crane_tip_raos` [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L45-L107] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `Vessel.has_complete_raos` [src/digitalmodel/marine_ops/installation/models.py:L173-L181] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWAEnhancedParser.parse_lis_file` [src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py:L46-L93] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `AQWAEnhancedParser._parse_rao_section_enhanced` [src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py:L145-L227] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWAEnhancedParser._extract_section_lines` [src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py:L319-L360] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWAEnhancedParser.extract_drag_table` [src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py:L612-L629] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `AQWAEnhancedParser.export_to_csv` [src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py:L757-L803] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWAReader.parse_lis_file` [src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py:L58-L109] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `AQWAReader._parse_rao_section` [src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py:L138-L199] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWAReader._extract_last_rao_section` [src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py:L231-L262] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWAReader._parse_frequencies` [src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py:L264-L283] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWAReader._parse_headings` [src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py:L285-L296] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWAReaderFixed.parse_lis_file` [src/digitalmodel/marine_ops/marine_analysis/aqwa_reader_fixed.py:L52-L103] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `AQWAReaderFixed._parse_rao_section_fixed` [src/digitalmodel/marine_ops/marine_analysis/aqwa_reader_fixed.py:L143-L240] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaFlexReader.parse_yml_file` [src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py:L32-L97] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `OrcaFlexReader._parse_orcaflex_rao_data` [src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py:L153-L187] | Mapped: Reads or converts hydrodynamic response data | wrapper | not checked | none |
| `OrcaFlexReader._extract_headings` [src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py:L189-L217] | Mapped: Reads or converts hydrodynamic response data; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaFlexReader._extract_frequencies` [src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py:L219-L248] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaFlexReader._fill_rao_values` [src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py:L250-L301] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaFlexReader._parse_table_format` [src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py:L304-L322] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `AQWALISParser.parse_lis_file` [src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py:L111-L164] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `AQWALISParser._extract_displacement_raos` [src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py:L182-L207] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `AQWALISParser._extract_velocity_raos` [src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py:L209-L232] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `AQWALISParser._extract_acceleration_raos` [src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py:L234-L257] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `AQWALISParser._parse_rao_section` [src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py:L284-L323] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaFlexYMLParser.parse_yml_file` [src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py:L52-L110] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `OrcaFlexYMLParser._navigate_to_draught` [src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py:L160-L171] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaFlexYMLParser._parse_orcaflex_rao_structure` [src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py:L203-L224] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `OrcaFlexYMLParser._extract_headings` [src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py:L226-L251] | Mapped: Reads or converts hydrodynamic response data; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaFlexYMLParser._extract_frequencies` [src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py:L253-L276] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaFlexYMLParser._fill_rao_values` [src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py:L278-L335] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `RAOInterpolator.interpolate_2d` [src/digitalmodel/marine_ops/marine_analysis/rao_interpolator.py:L35-L94] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `RAOInterpolator.interpolate_1d_frequency` [src/digitalmodel/marine_ops/marine_analysis/rao_interpolator.py:L255-L292] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `RAODataProcessor.import_aqwa_lis_file` [src/digitalmodel/marine_ops/marine_analysis/rao_processor.py:L98-L149] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `RAODataProcessor.import_orcaflex_yml_file` [src/digitalmodel/marine_ops/marine_analysis/rao_processor.py:L151-L197] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `RAODataProcessor.get_rao_dataframes` [src/digitalmodel/marine_ops/marine_analysis/rao_processor.py:L199-L255] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `RAODataProcessor.interpolate_rao_data` [src/digitalmodel/marine_ops/marine_analysis/rao_processor.py:L353-L368] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `RAODataValidators._detect_phase_convention` [src/digitalmodel/marine_ops/marine_analysis/rao_validators.py:L702-L759] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `RAODataValidators._find_phase_convention_in_data` [src/digitalmodel/marine_ops/marine_analysis/rao_validators.py:L761-L796] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `RAODataValidators._extract_orcaflex_raos` [src/digitalmodel/marine_ops/marine_analysis/rao_validators.py:L798-L918] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `RAOReaderError.get_user_message` [src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py:L43-L50] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `UnifiedRAOReader.read` [src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py:L127-L200] | Mapped: Reads or converts hydrodynamic response data | caller | not checked | none |
| `UnifiedRAOReader.read_aqwa_lis` [src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py:L202-L233] | Mapped: Reads or converts hydrodynamic response data | wrapper | not checked | none |
| `UnifiedRAOReader._detect_format` [src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py:L252-L282] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `UnifiedRAOReader._read_aqwa_lis` [src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py:L284-L295] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `main` [src/digitalmodel/marine_ops/marine_analysis/visualization/rao_plotter.py:L311-L330] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `read_rao_workbook` [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L155-L165] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `real_rao_datasets` [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L168-L182] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `promote` [src/digitalmodel/marine_ops/vessel_db/promote.py:L179-L205] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `GridRAO.from_file` [src/digitalmodel/motion_forecast/rao_adapter.py:L92-L97] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_run_checks` [src/digitalmodel/orcawave/reporting/sections/qa_summary.py:L23-L71] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_rao_interpolator` [src/digitalmodel/parametric/generate.py:L101-L123] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `run_orcawave` [src/digitalmodel/solvers/benchmark/solvers.py:L136-L176] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `run_aqwa` [src/digitalmodel/solvers/benchmark/solvers.py:L202-L249] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `csv_row_to_vessel_type` [src/digitalmodel/solvers/orcaflex/library_generator.py:L183-L209] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | this pack |
| `VesselTypeBuilder.build` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_type_builder.py:L33-L71] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `VesselRouter.from_hull_catalog` [src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py:L57-L112] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `LoadVessel.aqwa_group_item` [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L24-L100] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaFlexConverter.convert_to_orcaflex` [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L286-L343] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaFlexConverter._format_load_raos` [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L370-L400] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaFlexConverter._save_compact_format` [src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py:L422-L465] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `check_aqwa` [src/digitalmodel/solvers/smoke/probes.py:L168-L236] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `OrcaWaveMCPServer._register_tools.export_to_orcaflex` [src/digitalmodel/workflows/mcp_server/orcawave/core/mcp_server.py:L486-L542] | Mapped: Reads or converts hydrodynamic response data | independent | not checked | none |
| `_vessel_motion` [tests/drilling_riser/global_model/test_nonlinear_vessel_foundation.py:L116-L119] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `module scope` [tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L18-L18] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_FakeLISParser.parse_lis_file` [tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L49-L63] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_run_case` [tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L99-L129] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `sample_aqwa_file` [tests/hydrodynamics/bemrosetta/conftest.py:L21-L28] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `temp_lis_file` [tests/hydrodynamics/bemrosetta/test_cli.py:L35-L73] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `temp_lis_file` [tests/hydrodynamics/bemrosetta/test_parsers.py:L29-L67] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `_load_reference_data` [tests/hydrodynamics/capytaine/test_cylinder_benchmark.py:L52-L81] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `l00_owr_path` [tests/hydrodynamics/diffraction/conftest.py:L144-L149] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `l01_owr_path` [tests/hydrodynamics/diffraction/conftest.py:L162-L167] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_synthetic` [tests/hydrodynamics/diffraction/test_aqwa_lis_coefficient_grid.py:L37-L63] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `module scope` [tests/hydrodynamics/diffraction/test_aqwa_parser.py:L19-L19] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_patch_extract` [tests/hydrodynamics/diffraction/test_aqwa_runner.py:L672-L678] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_fake_diffraction` [tests/hydrodynamics/diffraction/test_matrix_unit_completeness.py:L48-L61] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_make_fake_diffraction` [tests/hydrodynamics/diffraction/test_orcawave_runner.py:L41-L112] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_make_fake_diffraction._install` [tests/hydrodynamics/diffraction/test_orcawave_runner.py:L88-L93] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_fake_diffraction` [tests/hydrodynamics/diffraction/test_orcawave_runner_units_boundary.py:L55-L79] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_full_basic` [tests/hydrodynamics/diffraction/test_report_generator_golden.py:L74-L102] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_benchmark` [tests/hydrodynamics/diffraction/test_report_generator_golden.py:L111-L134] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestValidateDataConsistency.test_mismatched_frequencies_logged` [tests/hydrodynamics/diffraction/test_solver_orcawave_data_extraction.py:L326-L343] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_mock_diffraction_object` [tests/hydrodynamics/diffraction/test_solver_report_extractors.py:L53-L107] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `repository_pair` [tests/hydrodynamics/hull_library/test_rao_fixture_metadata.py:L136-L148] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_ship_periods_convert_and_responses_stay_aligned` [tests/hydrodynamics/hull_library/test_rao_fixture_population.py:L127-L148] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_make_entry` [tests/hydrodynamics/hull_library/test_rao_lookup_plots.py:L40-L49] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestPanelCatalogRaoRoundTrip.test_csv_export_with_raos` [tests/hydrodynamics/hull_library/test_rao_registry.py:L334-L352] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `run_rao_analysis` [tests/hydrodynamics/rao_analysis/test_rao_analysis.py:L13-L56] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_base_cfg` [tests/hydrodynamics/test_rao_tabulation_workflow.py:L29-L63] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `vessel` [tests/marine_ops/installation/conftest.py:L61-L70] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestCraneTipRAOs.test_heave_transfer_formula` [tests/marine_ops/installation/test_crane_tip_motion.py:L83-L114] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `create_synthetic_orcaflex_rao_data` [tests/marine_ops/marine_analysis/test_rao_quality_checks.py:L28-L146] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `RAOVerification._find_test_files` [tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py:L46-L67] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `RAOVerification.verify_file` [tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py:L69-L125] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `RAOVerification._extract_unified_data` [tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py:L127-L197] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `RAOVerification._extract_legacy_data` [tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py:L199-L234] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `mock_diff_single_body` [tests/orcawave/conftest.py:L15-L52] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `mock_diff_two_body` [tests/orcawave/conftest.py:L56-L84] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `owr_path` [tests/orcawave/test_builder_orchestration.py:L36-L39] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `builder` [tests/orcawave/test_report_builder.py:L47-L50] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_orcawave_dir` [tests/postprocess/test_build_results_workbook.py:L25-L37] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `module scope` [tests/solver/smoke_test.py:L23-L23] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `promote_committed_artifacts` [tests/solver/smoke_test.py:L107-L138] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `run_l00_smoke_test` [tests/solver/smoke_test.py:L141-L179] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `run_l01_smoke_test` [tests/solver/smoke_test.py:L182-L216] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |

#### `wave-spectra`

| Pinned implementation | Map classification | Relation | Controls | Covering pack |
| --- | --- | --- | --- | --- |
| `Actuator.output` [docs/domains/orcaflex/examples/raw/K01/PythonController.py:L56-L78] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `Actuator.output` [docs/domains/orcaflex/examples/raw/K02/BladedControllerWrapper.py:L61-L83] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `define_environmental_conditions` [examples/domains/fpso/fpso_mooring_analysis.py:L137-L191] | Mapped: Demonstrates the family method | caller | not checked | none |
| `generate_visualizations` [examples/domains/fpso/fpso_mooring_analysis.py:L347-L494] | Mapped: Demonstrates the family method | caller | not checked | none |
| `main` [examples/domains/fpso/fpso_mooring_analysis.py:L497-L562] | Mapped: Demonstrates the family method | caller | not checked | none |
| `main` [examples/prototypes/orcaflex-agent/generate_test.py:L35-L73] | Mapped: Demonstrates the family method | caller | not checked | none |
| `mock_time_series` [scripts/_seed_eq_mock.py:L33-L83] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `module scope` [scripts/benchmark/extract_pdf_plots.py:L38-L45] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `module scope` [scripts/benchmark/validate_owd_vs_spec.py:L61-L61] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `build_dormancy_rules` [scripts/extract_property_inventory.py:L179-L205] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `create_wave_file` [scripts/generate_env_files.py:L38-L77] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `main` [scripts/generate_env_files.py:L142-L194] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `module scope` [scripts/ingest_orcina_help.py:L21-L28] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/19_-_DistanceBetweenTwoVessels.py:L4-L4] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/26_-_LinkedStatistics.py:L3-L3] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py:L3-L3] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/29_-_ExtremeStatisticsRayleigh.py:L3-L3] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/30_-_ExtremeStatisticsLikelihood.py:L3-L3] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/31_-_ExtremeStatisticsAggregation.py:L5-L5] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/32_-_FrequencyDomainResults.py:L3-L3] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/33_-_FrequencyDomainMPM.py:L3-L3] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/34_-_FrequencyDomainRangeGraph.py:L3-L3] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/35_-_FrequencyDomainSpectralDensitySpectralResponse.py:L4-L4] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/36_-_FrequencyDomainTimeHistorySynthesis.py:L4-L4] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `analyze_excel_file` [scripts/python/digitalmodel/legacy/analyze_marine_excel.py:L13-L345] | Mapped: Evaluates a wave spectrum or its normalization; byte-identical file copy | copy | not checked | none |
| `PerformanceProfiler.profile_wave_spectrum` [scripts/python/digitalmodel/legacy/profile_marine_modules.py:L121-L194] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `PerformanceProfiler.profile_wave_spectrum.run_spectrum` [scripts/python/digitalmodel/legacy/profile_marine_modules.py:L146-L150] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `freq_analyze` [scripts/python/digitalmodel/orcawave/mcp_orcawave.py:L36-L57] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `_environment_and_simulation` [src/digitalmodel/drilling_riser/global_model/build.py:L430-L471] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `short_chord_factor_f1` [src/digitalmodel/fatigue/scf_library.py:L108-L136] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `spectrum_cmd` [src/digitalmodel/hydrodynamics/cli.py:L46-L102] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `operability_analysis` [src/digitalmodel/hydrodynamics/seakeeping.py:L129-L231] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `WaveSpectra._normalize_to_significant_height` [src/digitalmodel/hydrodynamics/wave_spectra.py:L29-L44] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `WaveSpectra.jonswap` [src/digitalmodel/hydrodynamics/wave_spectra.py:L46-L96] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `WaveSpectra.pierson_moskowitz` [src/digitalmodel/hydrodynamics/wave_spectra.py:L98-L137] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `WaveSpectra.bretschneider` [src/digitalmodel/hydrodynamics/wave_spectra.py:L139-L174] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `WaveSpectra.issc` [src/digitalmodel/hydrodynamics/wave_spectra.py:L176-L213] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `WaveSpectra.generate_spectrum` [src/digitalmodel/hydrodynamics/wave_spectra.py:L215-L262] | Mapped: Evaluates a wave spectrum or its normalization | wrapper | not checked | none |
| `WaveSpectra.significant_height_from_spectrum` [src/digitalmodel/hydrodynamics/wave_spectra.py:L288-L306] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `WaveSpectra.zero_crossing_period_from_spectrum` [src/digitalmodel/hydrodynamics/wave_spectra.py:L308-L328] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `WaveSpectra.spectrum_statistics` [src/digitalmodel/hydrodynamics/wave_spectra.py:L348-L386] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `router` [src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L16-L69] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `_spectrum_name` [src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L78-L96] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `OrcaflexModelComponents.wave` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L207-L232] | Mapped: Evaluates a wave spectrum or its normalization; AST-equivalent definition after docstring removal | copy | not checked | none |
| `OrcaflexModelComponents.RayleighDampingCoefficients` [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L307-L377] | Mapped: Evaluates a wave spectrum or its normalization; AST-equivalent definition after docstring removal | copy | not checked | none |
| `_complex_rao` [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L28-L30] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `crane_tip_raos` [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L45-L107] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `crane_tip_significant_motion` [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L110-L150] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `crane_tip_significant_velocity` [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L153-L168] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `_jonswap_spectrum` [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L171-L201] | Mapped: Evaluates a wave spectrum or its normalization; affected by #2543 | independent | not checked | none |
| `analyze_excel_file` [src/digitalmodel/marine_ops/marine_analysis/analysis/excel_analyzer.py:L13-L345] | Mapped: Evaluates a wave spectrum or its normalization; byte-identical file copy | copy | not checked | none |
| `PerformanceProfiler.profile_wave_spectrum` [src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py:L119-L186] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `PerformanceProfiler.profile_wave_spectrum.run_spectrum` [src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py:L139-L143] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `FPSOMooringFullWorkflow._wave_spectrum` [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L54-L75] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `operation_envelope` [src/digitalmodel/marine_ops/operation_envelope.py:L168-L217] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `_significant_response` [src/digitalmodel/marine_ops/operation_envelope.py:L283-L288] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `synthesize_directional_forecast` [src/digitalmodel/motion_forecast/wave_forecast.py:L111-L156] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `jonswap_spectrum` [src/digitalmodel/motion_forecast/wave_source.py:L21-L46] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `synthesize_forecast` [src/digitalmodel/motion_forecast/wave_source.py:L49-L102] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `WaveSpectrumParams.tz` [src/digitalmodel/orcaflex/environment.py:L139-L144] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `WaveSpectrumParams.spectral_density` [src/digitalmodel/orcaflex/environment.py:L146-L174] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `WaveSpectrumParams.to_orcaflex_dict` [src/digitalmodel/orcaflex/environment.py:L176-L184] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `compute_response_spectrum` [src/digitalmodel/orcawave/motion_statistics.py:L97-L124] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `pierson_moskowitz` [src/digitalmodel/orcawave/wave_spectrum.py:L109-L137] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `jonswap` [src/digitalmodel/orcawave/wave_spectrum.py:L140-L184] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `bretschneider` [src/digitalmodel/orcawave/wave_spectrum.py:L187-L204] | Mapped: Evaluates a wave spectrum or its normalization | wrapper | not checked | none |
| `issc_spectrum` [src/digitalmodel/orcawave/wave_spectrum.py:L207-L230] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `ochi_hubble._component` [src/digitalmodel/orcawave/wave_spectrum.py:L266-L277] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `torsethaugen` [src/digitalmodel/orcawave/wave_spectrum.py:L282-L328] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `generate_spectrum` [src/digitalmodel/orcawave/wave_spectrum.py:L335-L390] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `_spectral_fatigue_annual_damage` [src/digitalmodel/parametric/generate.py:L208-L238] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `_esp_pump_utilisation` [src/digitalmodel/parametric/generate.py:L405-L426] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `_stress_psd_from_rao` [src/digitalmodel/rao_spectral_fatigue/workflow.py:L84-L121] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `run_orcaflex` [src/digitalmodel/solvers/benchmark/solvers.py:L49-L112] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `Environment.get_wave_waveTrains` [src/digitalmodel/solvers/fea_model/environment_components.py:L89-L98] | Mapped: Evaluates a wave spectrum or its normalization | wrapper | not checked | none |
| `Environment.getJONSWAPGamma` [src/digitalmodel/solvers/fea_model/environment_components.py:L100-L110] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `Environment.RayleighDampingCoefficients` [src/digitalmodel/solvers/fea_model/environment_components.py:L193-L298] | Mapped: Evaluates a wave spectrum or its normalization; AST-equivalent definition after docstring removal | copy | not checked | none |
| `EnvironmentBuilder._build_wave_trains` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L248-L362] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `EnvironmentBuilder._order_wave_train` [src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L1-L417] | Mapped symbol absent at code revision; behaviour not established | independent | not checked | none |
| `CampaignSpec.from_legacy_config` [src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py:L234-L343] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `OrcaflexModelComponents.wave` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L248-L297] | Mapped: Evaluates a wave spectrum or its normalization; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `OrcaflexModelComponents.RayleighDampingCoefficients` [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L394-L499] | Mapped: Evaluates a wave spectrum or its normalization; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `_evaluate_location` [src/digitalmodel/spectral_fatigue/workflow.py:L79-L127] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `FatigueAnalysisEngine.analyze_psd` [src/digitalmodel/structural/fatigue/analysis.py:L421-L526] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `FatigueAnalysisEngine._perform_frequency_validation` [src/digitalmodel/structural/fatigue/analysis.py:L786-L809] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `NarrowBandMethod.calculate_damage_rate` [src/digitalmodel/structural/fatigue/frequency_domain.py:L191-L226] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `DirlikMethod.calculate_damage_rate` [src/digitalmodel/structural/fatigue/frequency_domain.py:L240-L369] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `TovoBenasciuttiMethod.calculate_damage_rate` [src/digitalmodel/structural/fatigue/frequency_domain.py:L382-L468] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `SingleMomentMethod.calculate_damage_rate` [src/digitalmodel/structural/fatigue/frequency_domain.py:L481-L530] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `_resolve_wave_spectrum_func` [src/digitalmodel/structural/fatigue/scatter_fatigue.py:L281-L321] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `_resolve_wave_spectrum_func._jonswap_wrapper` [src/digitalmodel/structural/fatigue/scatter_fatigue.py:L305-L319] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `orcaflexModel` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L13-L357] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `wave` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L410-L452] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `jonswap_gamma` [src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py:L69-L90] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `jonswap_spectrum` [src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py:L93-L140] | Mapped: Evaluates a wave spectrum or its normalization; affected by #2543 | independent | not checked | none |
| `compute_wave_velocity` [src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py:L269-L359] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `_spectrum_type` [src/digitalmodel/vessel_seakeeping/workflow.py:L106-L112] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `_design` [src/digitalmodel/workflows/installation_full_report.py:L116-L136] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `design_data` [src/digitalmodel/workflows/installation_report_sections.py:L19-L37] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `_configure` [src/digitalmodel/workflows/installation_seastates.py:L82-L106] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `_verify` [src/digitalmodel/workflows/installation_seastates.py:L109-L138] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `_change_payload` [src/digitalmodel/workflows/installation_seastates.py:L244-L258] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `VesselResponseWorkflow.create` [src/digitalmodel/workflows/workflow_automation/workflows.py:L385-L445] | Mapped: Evaluates a wave spectrum or its normalization | independent | not checked | none |
| `wave_spectrum` [tests/hydrodynamics/rao_analysis/conftest.py:L31-L39] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `jonswap_spectrum` [tests/hydrodynamics/test_seakeeping.py:L44-L47] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `main` [tests/marine_ops/marine_engineering/integration/integration_example.py:L32-L399] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestWaveDynamicsIntegration.test_jonswap_generates_valid_spectrum` [tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py:L50-L65] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestWaveDynamicsIntegration.test_zero_crossing_period_relationship` [tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py:L95-L103] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestWaveDynamicsIntegration.test_wave_elevation_time_series_generation` [tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py:L134-L164] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestWaveDynamicsIntegration.test_wave_energy_distribution` [tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py:L240-L278] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_jonswap_hs_recovery` [tests/marine_ops/marine_engineering/legacy/test_validation.py:L28-L38] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_pierson_moskowitz_hs_recovery` [tests/marine_ops/marine_engineering/legacy/test_validation.py:L67-L77] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestJONSWAPSpectrum.test_spectral_moments_accuracy` [tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py:L105-L119] | Mapped: Checks or supplies inputs to the family method; byte-identical file copy | copy | not checked | none |
| `TestJONSWAPSpectrum.test_hs_from_m0_relationship` [tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py:L121-L132] | Mapped: Checks or supplies inputs to the family method; byte-identical file copy | copy | not checked | none |
| `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` [tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py:L309-L329] | Mapped: Checks or supplies inputs to the family method; byte-identical file copy | copy | not checked | none |
| `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` [tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py:L349-L371] | Mapped: Checks or supplies inputs to the family method; byte-identical file copy | copy | not checked | none |
| `TestPublishedBenchmarks.test_jonswap_vs_published_curves` [tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py:L438-L461] | Mapped: Checks or supplies inputs to the family method; byte-identical file copy | copy | not checked | none |
| `TestJONSWAPSpectrum.test_spectral_moments_accuracy` [tests/marine_ops/marine_engineering/test_wave_spectra.py:L105-L119] | Mapped: Checks or supplies inputs to the family method; byte-identical file copy | copy | not checked | none |
| `TestJONSWAPSpectrum.test_hs_from_m0_relationship` [tests/marine_ops/marine_engineering/test_wave_spectra.py:L121-L132] | Mapped: Checks or supplies inputs to the family method; byte-identical file copy | copy | not checked | none |
| `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` [tests/marine_ops/marine_engineering/test_wave_spectra.py:L309-L329] | Mapped: Checks or supplies inputs to the family method; byte-identical file copy | copy | not checked | none |
| `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` [tests/marine_ops/marine_engineering/test_wave_spectra.py:L349-L371] | Mapped: Checks or supplies inputs to the family method; byte-identical file copy | copy | not checked | none |
| `TestPublishedBenchmarks.test_jonswap_vs_published_curves` [tests/marine_ops/marine_engineering/test_wave_spectra.py:L438-L461] | Mapped: Checks or supplies inputs to the family method; byte-identical file copy | copy | not checked | none |
| `wave_order_findings` [tests/orcaflex/test_model_default_tokens.py:L1-L331] | Mapped symbol absent at code revision; behaviour not established | caller | not checked | none |
| `_write_floating_campaign` [tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py:L30-L120] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_make_campaign_data` [tests/solvers/orcaflex/modular_generator/test_campaign_generator.py:L13-L95] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestSpectralMomentsCalculation.test_white_noise_m0_equals_level_times_bandwidth` [tests/structural/fatigue/test_frequency_domain.py:L215-L224] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_jonswap_hz` [tests/structural/fatigue/test_scatter_fatigue.py:L42-L54] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `simple_wave_params` [tests/unit/hydrodynamics/test_rao_analysis.py:L38-L44] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestWaveSpectra.test_jonswap_peak_near_tp` [tests/unit/hydrodynamics/test_rao_analysis.py:L191-L199] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestPiersonMoskowitzExtended.test_pm_peak_near_tp` [tests/unit/hydrodynamics/test_wave_spectra_extended.py:L155-L162] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestBretschneiderExtended.test_bretschneider_peak_near_tp` [tests/unit/hydrodynamics/test_wave_spectra_extended.py:L202-L210] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestZeroCrossingPeriodFromSpectrum.test_tz_pm_approximately_0711_tp` [tests/unit/hydrodynamics/test_wave_spectra_extended.py:L393-L400] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestPeakFrequencyFromSpectrum.test_peak_frequency_near_2pi_over_tp` [tests/unit/hydrodynamics/test_wave_spectra_extended.py:L421-L427] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_assert_wave_spectra` [tests/workflows/hydrodynamics_assertions.py:L51-L64] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_workflow_registry` [tests/workflows/test_durable_workflows.py:L52-L1680] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` [tests/workflows/test_durable_workflows.py:L1825-L1881] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` [tests/workflows/test_durable_workflows.py:L1884-L1951] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_vessel_seakeeping_significant_amplitude_matches_library` [tests/workflows/test_durable_workflows.py:L1954-L2028] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_esp_pump_hydraulics_tdh_and_sizing_closed_form` [tests/workflows/test_durable_workflows.py:L2284-L2322] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |

#### `sn-curves-and-miner-damage`

| Pinned implementation | Map classification | Relation | Controls | Covering pack |
| --- | --- | --- | --- | --- |
| `example_6_custom_workflow` [docs/domains/examples/orcaflex_signal_analysis_example.py:L295-L363] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `example_1_basic_rainflow_counting` [docs/domains/examples/signal_analysis_usage_example.py:L29-L65] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `example_4_fatigue_analysis` [docs/domains/examples/signal_analysis_usage_example.py:L175-L253] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `example_6_backward_compatibility` [docs/domains/examples/signal_analysis_usage_example.py:L315-L377] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `main` [docs/domains/examples/signal_analysis_usage_example.py:L380-L410] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `main` [examples/demos/demo_sn_library.py:L43-L207] | Mapped: Demonstrates the family method | caller | not checked | none |
| `demonstrate_sn_curves` [examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py:L120-L160] | Mapped: Demonstrates the family method | caller | not checked | none |
| `demonstrate_rainflow_counting` [examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py:L163-L243] | Mapped: Demonstrates the family method | caller | not checked | none |
| `demonstrate_damage_accumulation` [examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py:L246-L326] | Mapped: Demonstrates the family method | caller | not checked | none |
| `demonstrate_frequency_domain_analysis` [examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py:L329-L408] | Mapped: Demonstrates the family method | caller | not checked | none |
| `demonstrate_quick_analysis` [examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py:L411-L458] | Mapped: Demonstrates the family method | caller | not checked | none |
| `main` [examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py:L461-L502] | Mapped: Demonstrates the family method | caller | not checked | none |
| `list_curves` [examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py:L24-L37] | Mapped: Demonstrates the family method | caller | not checked | none |
| `compare_curves` [examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py:L66-L88] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_2_linear_log` [examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py:L46-L64] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_3_with_scf` [examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py:L67-L85] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_4_without_fatigue_limit` [examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py:L88-L106] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_5_dnv_comparison` [examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py:L109-L133] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_6_abs_comparison` [examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py:L136-L174] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_7_reference_comparison` [examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py:L177-L197] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_8_joint_type_comparison` [examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py:L200-L228] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_1_basic_sn_curves` [examples/domains/fatigue/fatigue_analysis_examples.py:L55-L95] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_2_multislope_curves` [examples/domains/fatigue/fatigue_analysis_examples.py:L98-L139] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_3_time_domain_analysis` [examples/domains/fatigue/fatigue_analysis_examples.py:L142-L212] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_4_frequency_domain_analysis` [examples/domains/fatigue/fatigue_analysis_examples.py:L215-L279] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_5_high_stress_range_analysis` [examples/domains/fatigue/fatigue_analysis_examples.py:L282-L324] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_6_shear_data_analysis` [examples/domains/fatigue/fatigue_analysis_examples.py:L327-L383] | Mapped: Demonstrates the family method | caller | not checked | none |
| `run_all_examples` [examples/domains/fatigue/fatigue_analysis_examples.py:L454-L477] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_well_log_analysis` [examples/domains/reservoir/reservoir_analysis_examples.py:L138-L219] | Mapped: Demonstrates the family method | caller | not checked | none |
| `example_stratigraphic_correlation` [examples/domains/reservoir/reservoir_analysis_examples.py:L376-L457] | Mapped: Demonstrates the family method | caller | not checked | none |
| `main` [examples/domains/reservoir/reservoir_analysis_examples.py:L578-L603] | Mapped: Demonstrates the family method | caller | not checked | none |
| `module scope` [scripts/capabilities/build_onepagers.py:L36-L36] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py:L3-L3] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `process_single_test_file` [scripts/python/digitalmodel/tools/check_process_fatigue_single.py:L25-L225] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `YAMLConfigMigrator._migrate_analysis` [scripts/python/digitalmodel/tools/migrate_yaml_configs.py:L134-L168] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `YAMLConfigMigrator._migrate_time_series_components` [scripts/python/digitalmodel/tools/migrate_yaml_configs.py:L225-L242] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `process_fatigue_files` [scripts/python/digitalmodel/tools/process_fatigue_data.py:L24-L116] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `process_single_test_file` [scripts/python/digitalmodel/tools/process_fatigue_final.py:L25-L439] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage; AST-equivalent definition after docstring removal | copy | not checked | none |
| `process_fatigue_file` [scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py:L23-L273] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `process_sample_file` [scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py:L275-L300] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `batch_process_all_files` [scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py:L302-L343] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `rainflow_counting` [scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py:L19-L59] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `process_fatigue_file` [scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py:L61-L238] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `batch_process` [scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py:L275-L316] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `process_single_test_file` [scripts/python/digitalmodel/tools/process_fatigue_single_test_v2.py:L25-L309] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `process_single_test_file` [scripts/python/digitalmodel/tools/process_fatigue_single_test_v3.py:L26-L337] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `process_single_test_file` [scripts/python/digitalmodel/tools/process_fatigue_single_test_v4.py:L25-L352] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `process_single_test_file` [scripts/python/digitalmodel/tools/process_fatigue_single_test_v5.py:L25-L382] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `process_single_test_file` [scripts/python/digitalmodel/tools/process_fatigue_single_test_v6.py:L25-L405] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `module scope` [scripts/riser_database/build_tables.py:L24-L24] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `SNcurve.from_dnv` [src/digitalmodel/ansys/fatigue_postprocessor.py:L101-L122] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatiguePostprocessor.calculate_cycles_to_failure` [src/digitalmodel/ansys/fatigue_postprocessor.py:L158-L192] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatiguePostprocessor.calculate_fatigue_damage` [src/digitalmodel/ansys/fatigue_postprocessor.py:L222-L284] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatiguePostprocessor.generate_sn_curve_apdl` [src/digitalmodel/ansys/fatigue_postprocessor.py:L347-L369] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatiguePostprocessor.create_damage_summary_df` [src/digitalmodel/ansys/fatigue_postprocessor.py:L371-L398] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `build_model_inventory` [src/digitalmodel/benchmarks/inventory.py:L77-L327] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `half_cycles` [src/digitalmodel/drilling_riser/global_model/fatigue_channels.py:L51-L64] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `extract` [src/digitalmodel/drilling_riser/global_model/fatigue_channels.py:L104-L137] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `engine` [src/digitalmodel/engine.py:L87-L885] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `router` [src/digitalmodel/fatg_spectral_fatigue/workflow.py:L52-L117] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_sn_curve` [src/digitalmodel/fatg_spectral_fatigue/workflow.py:L120-L134] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_case_histogram` [src/digitalmodel/fatigue/assessment.py:L116-L121] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `assess_fatigue` [src/digitalmodel/fatigue/assessment.py:L136-L160] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `module scope` [src/digitalmodel/fatigue/counting_contract.py:L47-L68] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_adapt_pylife_4pt` [src/digitalmodel/fatigue/counting_contract.py:L432-L440] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_adapt_pypi_rainflow` [src/digitalmodel/fatigue/counting_contract.py:L443-L454] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_adapt_sigproc_astm` [src/digitalmodel/fatigue/counting_contract.py:L457-L469] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_adapt_calm_buoy` [src/digitalmodel/fatigue/counting_contract.py:L472-L478] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_adapt_fapps_counting` [src/digitalmodel/fatigue/counting_contract.py:L481-L487] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_adapt_fapps_counter` [src/digitalmodel/fatigue/counting_contract.py:L490-L494] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_adapt_struct_fatigue` [src/digitalmodel/fatigue/counting_contract.py:L497-L501] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_is_counting_name` [src/digitalmodel/fatigue/counting_contract.py:L866-L869] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `miner_damage` [src/digitalmodel/fatigue/damage.py:L13-L39] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `design_life_check` [src/digitalmodel/fatigue/damage.py:L42-L88] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `environment_correction` [src/digitalmodel/fatigue/environmental_correction.py:L165-L244] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `apply_environment_to_sn` [src/digitalmodel/fatigue/environmental_correction.py:L251-L292] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `fatg_sea_state_damage` [src/digitalmodel/fatigue/fatg.py:L211-L238] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `fatg_annual_damage` [src/digitalmodel/fatigue/fatg.py:L241-L311] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_sec_fatigue_locations` [src/digitalmodel/fatigue/fatigue_reporting.py:L376-L399] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `fatigue_life` [src/digitalmodel/fatigue/rainflow.py:L130-L201] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_compute_sd` [src/digitalmodel/fatigue/sn_curves.py:L56-L58] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_curve_dict` [src/digitalmodel/fatigue/sn_curves.py:L61-L74] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `get_sn_curve` [src/digitalmodel/fatigue/sn_curves.py:L80-L134] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `SNCurveRecord.knee_stress` [src/digitalmodel/fatigue/sn_library.py:L141-L146] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | wrapper | not checked | none |
| `SNCurveRecord.switch_stress` [src/digitalmodel/fatigue/sn_library.py:L149-L160] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | wrapper | not checked | none |
| `SNCurveRecord.cycles` [src/digitalmodel/fatigue/sn_library.py:L162-L177] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | wrapper | not checked | none |
| `_knee_stress` [src/digitalmodel/fatigue/sn_library.py:L180-L190] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_switch_stress` [src/digitalmodel/fatigue/sn_library.py:L200-L222] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `_bilinear_cycles` [src/digitalmodel/fatigue/sn_library.py:L225-L250] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_sd` [src/digitalmodel/fatigue/sn_library.py:L288-L290] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_dnv_curves` [src/digitalmodel/fatigue/sn_library.py:L293-L381] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_api_curves` [src/digitalmodel/fatigue/sn_library.py:L384-L413] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_bs7608_curves` [src/digitalmodel/fatigue/sn_library.py:L416-L463] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_iiw_curves` [src/digitalmodel/fatigue/sn_library.py:L466-L497] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_eurocode3_curves` [src/digitalmodel/fatigue/sn_library.py:L500-L526] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_norsok_curves` [src/digitalmodel/fatigue/sn_library.py:L529-L566] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `_build_aws_curves` [src/digitalmodel/fatigue/sn_library.py:L569-L597] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_asme_curves` [src/digitalmodel/fatigue/sn_library.py:L600-L627] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_en13445_curves` [src/digitalmodel/fatigue/sn_library.py:L630-L654] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_pd5500_curves` [src/digitalmodel/fatigue/sn_library.py:L657-L687] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `_build_abs_curves` [src/digitalmodel/fatigue/sn_library.py:L690-L730] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_iso19902_curves` [src/digitalmodel/fatigue/sn_library.py:L733-L802] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_csa_curves` [src/digitalmodel/fatigue/sn_library.py:L805-L832] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_hse_curves` [src/digitalmodel/fatigue/sn_library.py:L835-L877] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_doe_curves` [src/digitalmodel/fatigue/sn_library.py:L880-L911] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `_build_gl_curves` [src/digitalmodel/fatigue/sn_library.py:L914-L941] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_jis_curves` [src/digitalmodel/fatigue/sn_library.py:L944-L970] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_ensure_loaded` [src/digitalmodel/fatigue/sn_library.py:L1002-L1011] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `summary_table` [src/digitalmodel/fatigue/sn_library.py:L1129-L1148] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `_record_to_curve_info` [src/digitalmodel/fatigue/sn_library_api.py:L146-L155] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_record_to_sncurve` [src/digitalmodel/fatigue/sn_library_api.py:L158-L174] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `get_curve` [src/digitalmodel/fatigue/sn_library_api.py:L210-L235] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | wrapper | not checked | none |
| `calculate_endurance` [src/digitalmodel/fatigue/sn_library_api.py:L238-L281] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | wrapper | not checked | none |
| `wirsching_light_damage` [src/digitalmodel/fatigue/spectral_fatigue.py:L207-L254] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `benasciutti_tovo_damage` [src/digitalmodel/fatigue/spectral_fatigue.py:L339-L395] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `weld_fatigue_quickcheck` [src/digitalmodel/fatigue/weld_fatigue_quickcheck.py:L105-L240] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `main` [src/digitalmodel/fatigue/weld_fatigue_quickcheck.py:L340-L373] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `generate_test_data` [src/digitalmodel/fatigue/woehler_fitting.py:L18-L89] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_fallback_log_regression` [src/digitalmodel/fatigue/woehler_fitting.py:L137-L181] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `router` [src/digitalmodel/fatigue/workflow.py:L16-L40] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `_curve_ids` [src/digitalmodel/fatigue/workflow.py:L43-L55] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_stress_ranges` [src/digitalmodel/fatigue/workflow.py:L58-L85] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_curve_summary` [src/digitalmodel/fatigue/workflow.py:L99-L111] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_curve_csv_path` [src/digitalmodel/fatigue/workflow.py:L114-L120] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_input_stem` [src/digitalmodel/fatigue/workflow.py:L131-L134] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueAnalysis.router` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L23-L50] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `FatigueAnalysis.damage_from_sn_data` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L52-L72] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueAnalysis.damage_from_rainflow_cycles` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L74-L93] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueAnalysis.damage_from_timetrace` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L95-L139] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueAnalysis.get_rainflow_from_timetrace` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L249-L261] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L586-L701] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `ShipFatigueAnalysis.router` [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L22-L36] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | wrapper | not checked | none |
| `ShipFatigueAnalysis.get_lcf_timetrace_rainflow_and_fatigue_damage` [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L113-L166] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `ShipFatigueAnalysis.get_fatigue_damage_output_df_for_timetrace` [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L298-L391] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | wrapper | not checked | none |
| `ShipFatigueAnalysis.get_fatigue_df_definition_for_timetrace` [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L476-L489] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L982-L1164] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `SNcurve.cycles_to_failure` [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L53-L66] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `RainflowFatigue.count_cycles` [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L203-L225] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `MinersRuleDamage.compute` [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L323-L344] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `ScatterDiagramFatigue.compute` [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L373-L433] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `compute_fatigue_life` [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L522-L600] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `StratigraphicAnalysis.add_well_analysis` [src/digitalmodel/marine_ops/reservoir/analysis.py:L414-L423] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `StratigraphicAnalysis.set_well_order` [src/digitalmodel/marine_ops/reservoir/analysis.py:L425-L436] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `StratigraphicAnalysis.create_flattened_section` [src/digitalmodel/marine_ops/reservoir/analysis.py:L451-L525] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `WellCorrelation.add_well_data` [src/digitalmodel/marine_ops/reservoir/analysis.py:L803-L811] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `CoreAnalysis.correlate_core_to_logs` [src/digitalmodel/marine_ops/reservoir/analysis.py:L1046-L1091] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `ReservoirAnalysis.add_well_analysis` [src/digitalmodel/marine_ops/reservoir/analysis.py:L1119-L1142] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `ReservoirAnalysis.run_comprehensive_analysis` [src/digitalmodel/marine_ops/reservoir/analysis.py:L1144-L1175] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `router` [src/digitalmodel/mooring_fatigue/workflow.py:L23-L58] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `_evaluate_line` [src/digitalmodel/mooring_fatigue/workflow.py:L61-L85] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_history_bins` [src/digitalmodel/mooring_fatigue/workflow.py:L137-L149] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_curve_settings` [src/digitalmodel/mooring_fatigue/workflow.py:L184-L186] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_mooring_fatigue_damage` [src/digitalmodel/parametric/generate.py:L28-L45] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_free_span_utilisation` [src/digitalmodel/parametric/generate.py:L141-L175] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_span_rectification_utilisation` [src/digitalmodel/parametric/generate.py:L327-L365] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `module scope` [src/digitalmodel/parametric/refresh.py:L27-L27] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `router` [src/digitalmodel/rao_spectral_fatigue/workflow.py:L34-L81] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `assess_touchdown_fatigue` [src/digitalmodel/riser_fatigue/touchdown.py:L157-L247] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `router` [src/digitalmodel/riser_fatigue/workflow.py:L65-L103] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `_viv_damage` [src/digitalmodel/riser_fatigue/workflow.py:L205-L245] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_curve_settings` [src/digitalmodel/riser_fatigue/workflow.py:L255-L266] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `TimeSeriesComponentsAdapter.get_rainflow_count_from_time_series` [src/digitalmodel/signal_processing/signal_analysis/adapters.py:L50-L72] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueAnalysisAdapter.get_rainflow_from_timetrace` [src/digitalmodel/signal_processing/signal_analysis/adapters.py:L293-L315] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueAnalysisAdapter.damage_from_rainflow_cycles` [src/digitalmodel/signal_processing/signal_analysis/adapters.py:L317-L355] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `OrcaFlexAdapter.RainflowHalfCycles` [src/digitalmodel/signal_processing/signal_analysis/adapters.py:L376-L405] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `rainflow_cmd` [src/digitalmodel/signal_processing/signal_analysis/cli.py:L30-L86] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `RainflowCounter.count_cycles` [src/digitalmodel/signal_processing/signal_analysis/core/rainflow.py:L39-L83] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `SNCurve._bilinear` [src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py:L189-L234] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `SNCurve.from_test_data` [src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py:L376-L417] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueDamageCalculator.calculate_damage` [src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py:L34-L84] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueDamageCalculator._miners_rule` [src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py:L86-L116] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueDamageCalculator._modified_miners_rule` [src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py:L118-L158] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueDamageCalculator._nonlinear_damage` [src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py:L160-L199] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueDamageCalculator._calculate_allowable_cycles` [src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py:L201-L255] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueDamageCalculator._apply_mean_stress_correction` [src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py:L257-L314] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueDamageCalculator._estimate_cycles_to_failure` [src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py:L316-L351] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueDamageCalculator.calculate_equivalent_stress` [src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py:L353-L388] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `create_parser` [src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py:L55-L248] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `build_config_from_args` [src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py:L251-L324] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `main` [src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py:L327-L479] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `TimeSeriesAnalyzer._analyze_column` [src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py:L147-L222] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `TimeSeriesAnalyzer._generate_outputs` [src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py:L327-L360] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `TimeSeriesAnalyzer._generate_summary` [src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py:L438-L474] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `SafeAnalyzer.safe_rainflow_analysis` [src/digitalmodel/signal_processing/signal_analysis/orcaflex/error_handling.py:L162-L222] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `TimeSeriesAnalysis.router` [src/digitalmodel/signal_processing/time_series/time_series_analysis.py:L20-L41] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `TimeSeriesComponents.count_cycles` [src/digitalmodel/signal_processing/time_series/time_series_components.py:L644-L657] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `OPPTimeSeries.get_histogram_object_from_orcaflex_run` [src/digitalmodel/solvers/orcaflex/opp_time_series.py:L441-L471] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | this pack |
| `OPPTimeSeries.get_detailed_histogram_df_columns` [src/digitalmodel/solvers/orcaflex/opp_time_series.py:L473-L494] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `OPPTimeSeriesV2.process_histograms` [src/digitalmodel/solvers/orcaflex/opp_time_series_v2.py:L241-L282] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | this pack |
| `OrcaFlexAnalysis.get_histogram_object_from_orcaflex_run` [src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py:L470-L492] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | this pack |
| `OrcaFlexAnalysis.get_detailed_histogram_df_columns` [src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py:L755-L767] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage; AST-equivalent definition after docstring removal | copy | not checked | this pack |
| `OrcaFlexTimeTraceProcessor._perform_fatigue_analysis` [src/digitalmodel/solvers/orcaflex/time_trace_processor.py:L248-L338] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | this pack |
| `OrcaFlexTimeTraceProcessor._parallel_fatigue_analysis` [src/digitalmodel/solvers/orcaflex/time_trace_processor.py:L420-L448] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | this pack |
| `OrcaFlexTimeTraceProcessor._analyze_single_trace` [src/digitalmodel/solvers/orcaflex/time_trace_processor.py:L451-L496] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | this pack |
| `router` [src/digitalmodel/spectral_fatigue/workflow.py:L37-L76] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `_sn_curve` [src/digitalmodel/spectral_fatigue/workflow.py:L139-L143] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `quick_fatigue_analysis` [src/digitalmodel/structural/fatigue/__init__.py:L226-L305] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `get_module_info` [src/digitalmodel/structural/fatigue/__init__.py:L431-L447] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `module scope` [src/digitalmodel/structural/fatigue/analysis.py:L39-L39] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueAnalysisEngine._setup_sn_curve` [src/digitalmodel/structural/fatigue/analysis.py:L266-L285] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | wrapper | not checked | none |
| `FatigueAnalysisEngine._setup_damage_calculator` [src/digitalmodel/structural/fatigue/analysis.py:L287-L300] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueAnalysisEngine._setup_rainflow_counter` [src/digitalmodel/structural/fatigue/analysis.py:L302-L310] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueAnalysisEngine.analyze_time_series` [src/digitalmodel/structural/fatigue/analysis.py:L312-L419] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueAnalysisEngine.analyze_psd` [src/digitalmodel/structural/fatigue/analysis.py:L421-L526] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueAnalysisEngine.calculate_high_stress_range_life` [src/digitalmodel/structural/fatigue/analysis.py:L528-L569] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueAnalysisEngine.analyze_shear_data` [src/digitalmodel/structural/fatigue/analysis.py:L571-L620] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueAnalysisEngine._perform_engineering_validation` [src/digitalmodel/structural/fatigue/analysis.py:L733-L784] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `quick_time_domain_analysis` [src/digitalmodel/structural/fatigue/analysis.py:L979-L1022] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `quick_frequency_domain_analysis` [src/digitalmodel/structural/fatigue/analysis.py:L1025-L1062] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `module scope` [src/digitalmodel/structural/fatigue/damage_accumulation.py:L32-L32] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `LinearDamageAccumulation.calculate_damage` [src/digitalmodel/structural/fatigue/damage_accumulation.py:L109-L217] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `LinearDamageAccumulation._apply_mean_stress_correction` [src/digitalmodel/structural/fatigue/damage_accumulation.py:L219-L256] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `ModifiedMinersRule.calculate_damage` [src/digitalmodel/structural/fatigue/damage_accumulation.py:L292-L355] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `NonlinearDamageAccumulation.calculate_damage` [src/digitalmodel/structural/fatigue/damage_accumulation.py:L383-L400] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | wrapper | not checked | none |
| `NonlinearDamageAccumulation._marco_starkey_damage` [src/digitalmodel/structural/fatigue/damage_accumulation.py:L402-L449] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `NonlinearDamageAccumulation._corten_dolan_damage` [src/digitalmodel/structural/fatigue/damage_accumulation.py:L451-L495] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `NonlinearDamageAccumulation._double_linear_damage` [src/digitalmodel/structural/fatigue/damage_accumulation.py:L497-L544] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `compare_damage_methods` [src/digitalmodel/structural/fatigue/damage_accumulation.py:L652-L699] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_curve` [src/digitalmodel/structural/fatigue/design_code_report.py:L101-L114] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `failure_probability` [src/digitalmodel/structural/fatigue/fatigue_reliability.py:L84-L168] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `sensitivity_analysis` [src/digitalmodel/structural/fatigue/fatigue_reliability.py:L171-L273] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `monte_carlo_fatigue` [src/digitalmodel/structural/fatigue/fatigue_reliability.py:L276-L366] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `module scope` [src/digitalmodel/structural/fatigue/frequency_domain.py:L30-L45] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `NarrowBandMethod.calculate_damage_rate` [src/digitalmodel/structural/fatigue/frequency_domain.py:L191-L226] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `DirlikMethod.calculate_damage_rate` [src/digitalmodel/structural/fatigue/frequency_domain.py:L240-L369] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `TovoBenasciuttiMethod.calculate_damage_rate` [src/digitalmodel/structural/fatigue/frequency_domain.py:L382-L468] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `SingleMomentMethod.calculate_damage_rate` [src/digitalmodel/structural/fatigue/frequency_domain.py:L481-L530] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `compare_frequency_methods` [src/digitalmodel/structural/fatigue/frequency_domain.py:L683-L725] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `run_sweep` [src/digitalmodel/structural/fatigue/parametric_sweep.py:L74-L163] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `compute_sensitivity._single_damage` [src/digitalmodel/structural/fatigue/parametric_sweep.py:L206-L221] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_build_scatter_json` [src/digitalmodel/structural/fatigue/parametric_sweep.py:L448-L512] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `module scope` [src/digitalmodel/structural/fatigue/rainflow.py:L32-L32] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `RainflowCounter.count_cycles` [src/digitalmodel/structural/fatigue/rainflow.py:L230-L310] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `RainflowCounter._rainflow_count` [src/digitalmodel/structural/fatigue/rainflow.py:L316-L318] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `RainflowCounter.extract_cycles_with_means` [src/digitalmodel/structural/fatigue/rainflow.py:L502-L536] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `rainflow_count` [src/digitalmodel/structural/fatigue/rainflow.py:L729-L752] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `rainflow_with_means` [src/digitalmodel/structural/fatigue/rainflow.py:L755-L770] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `scatter_fatigue_damage` [src/digitalmodel/structural/fatigue/scatter_fatigue.py:L102-L255] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `fatigue_assessment` [src/digitalmodel/structural/fatigue/skill.py:L188-L273] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `main` [src/digitalmodel/structural/fatigue/sn_curve_plotter.py:L397-L415] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `module scope` [src/digitalmodel/structural/fatigue/sn_curves.py:L32-L36] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `StandardSNCurves._find_sn_data_file` [src/digitalmodel/structural/fatigue/sn_curves.py:L442-L453] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `StandardSNCurves._ensure_loaded` [src/digitalmodel/structural/fatigue/sn_curves.py:L456-L505] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `StandardSNCurves.register_custom_curve` [src/digitalmodel/structural/fatigue/sn_curves.py:L522-L563] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `StandardSNCurves.get_dnv_c203_curve` [src/digitalmodel/structural/fatigue/sn_curves.py:L566-L616] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `StandardSNCurves.get_curve` [src/digitalmodel/structural/fatigue/sn_curves.py:L619-L668] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `StandardSNCurves.get_multislope_curve` [src/digitalmodel/structural/fatigue/sn_curves.py:L671-L713] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `ThicknessCorrection.apply_thickness_effect` [src/digitalmodel/structural/fatigue/sn_curves.py:L863-L916] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `SNDataFitting.fit_power_law` [src/digitalmodel/structural/fatigue/sn_curves.py:L927-L1005] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `get_dnv_curve` [src/digitalmodel/structural/fatigue/sn_curves.py:L1047-L1066] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `get_api_curve` [src/digitalmodel/structural/fatigue/sn_curves.py:L1069-L1071] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `get_bs_curve` [src/digitalmodel/structural/fatigue/sn_curves.py:L1074-L1076] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_dnv_curves_from_c203` [src/digitalmodel/structural/fatigue/sn_curves.py:L1085-L1099] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_dnv_seawater_cp` [src/digitalmodel/structural/fatigue/worked_examples.py:L58-L77] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_dnv_seawater_free` [src/digitalmodel/structural/fatigue/worked_examples.py:L80-L97] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_chain_seawater_cp` [src/digitalmodel/structural/fatigue/worked_examples.py:L100-L119] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `pipeline_girth_weld` [src/digitalmodel/structural/fatigue/worked_examples.py:L199-L260] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `scr_touchdown` [src/digitalmodel/structural/fatigue/worked_examples.py:L263-L323] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `mooring_chain` [src/digitalmodel/structural/fatigue/worked_examples.py:L326-L388] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `_compute_example` [src/digitalmodel/structural/fatigue/worked_examples.py:L395-L431] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `run_analysis` [src/digitalmodel/structural/fatigue_apps/__main__.py:L244-L501] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `SNCurve._load_sn_parameters` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L55-L118] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `SNCurve.calculate_cycles_to_failure` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L120-L149] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueDamageCalculator.calculate_damage` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L182-L209] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `main` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L489-L549] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `module scope` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L21-L21] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `SNCurveParameters.a1` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L36-L38] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `SNCurveParameters.a2` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L41-L43] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueDamageCalculator.cycles_to_failure` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L106-L132] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueDamageCalculator.calculate_damage` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L134-L184] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueDamageCalculator.export_results` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L297-L320] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `FatigueFileNamer.get_rainflow_filename` [src/digitalmodel/structural/fatigue_apps/file_namer.py:L87-L105] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `module scope` [src/digitalmodel/structural/fatigue_apps/file_namer.py:L201-L230] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `IntegratedFatigueProcessor.process_fatigue_condition` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L290-L364] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `main` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L366-L484] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `IntegratedFatigueProcessorWithNaming._process_condition_base` [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L81-L173] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `module scope` [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L20-L20] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `RainflowCounter.count_cycles` [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L48-L79] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `RainflowCounter._rainflow_algorithm` [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L164-L220] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `RainflowCounter.export_results` [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L292-L322] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `rainflow_count` [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L325-L338] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `RainflowCounter.process_time_series` [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L198-L254] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `RainflowCounter.process_batch` [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L332-L403] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `RainflowCounter.save_cycle_data` [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L405-L432] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `main` [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L463-L502] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `ReferenceSeaStateProcessor.process_fatigue_condition` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L355-L429] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `TensionStressConverter.process_batch` [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L271-L334] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `main` [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L428-L474] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `generate_all_visualizations` [src/digitalmodel/structural/fatigue_apps/visualizer.py:L507-L570] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `narrow_band_damage` [src/digitalmodel/structural/spectral_fatigue.py:L122-L178] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `assess_jumper_fatigue` [src/digitalmodel/subsea/connectors/fatigue.py:L62-L106] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `_dnv_params` [src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py:L133-L150] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `get_sn_curve` [src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py:L170-L237] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `SpanFatigueDamage._load_sn_curve` [src/digitalmodel/subsea/pipeline/free_span/span_fatigue_damage.py:L79-L91] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `VIVFatigueCalculator.calculate_damage` [src/digitalmodel/subsea/viv_analysis/fatigue.py:L108-L165] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `VIVFatigueCalculator.calculate_fatigue_life` [src/digitalmodel/subsea/viv_analysis/fatigue.py:L167-L207] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `VIVFatigueCalculator.analyze_viv_fatigue` [src/digitalmodel/subsea/viv_analysis/fatigue.py:L209-L268] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `VIVFatigueCalculator.cumulative_damage` [src/digitalmodel/subsea/viv_analysis/fatigue.py:L270-L300] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | caller | not checked | none |
| `parse_inputs` [src/digitalmodel/viv_screening/inputs.py:L88-L125] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `router` [src/digitalmodel/viv_screening/workflow.py:L25-L57] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_damage_rate` [src/digitalmodel/viv_screening/workflow.py:L128-L147] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_provenance` [src/digitalmodel/viv_screening/workflow.py:L173-L191] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `page` [src/digitalmodel/web/digitaltwinfeed/app.py:L71-L86] | Mapped: Evaluates stress-life curves, cycle counts or cumulative damage | independent | not checked | none |
| `_check_receipt` [tests/ansys/padeye_pressure_bundle.py:L23-L38] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `verify_pressure_bundle` [tests/ansys/padeye_pressure_bundle.py:L41-L65] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestThicknessCorrection.test_known_correction` [tests/ansys/test_fatigue_postprocessor.py:L92-L96] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `bundle` [tests/ansys/test_padeye_pressure_bundle.py:L20-L29] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `histogram_input` [tests/fatigue/test_assessment.py:L22-L34] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_span_input` [tests/fatigue/test_c203_consumers.py:L303-L331] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `_non_dnv_bilinear` [tests/fatigue/test_c203_sn_values.py:L298-L303] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_non_dnv_bilinear_switch_stays_at_endurance_limit` [tests/fatigue/test_c203_sn_values.py:L326-L343] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_signal_e` [tests/fatigue/test_c203_variable_amplitude.py:L179-L182] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_user_curve` [tests/fatigue/test_c203_variable_amplitude.py:L230-L233] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_span_input` [tests/fatigue/test_c203_variable_amplitude.py:L304-L332] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `_fresh_standard_curves` [tests/fatigue/test_c203_variable_amplitude.py:L378-L391] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_data_dir` [tests/fatigue/test_c203_variable_amplitude.py:L394-L398] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `f_curve` [tests/fatigue/test_damage.py:L16-L17] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestThicknessCorrection.test_correction_above_ref` [tests/fatigue/test_damage.py:L90-L95] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestThicknessCorrection.test_correction_formula` [tests/fatigue/test_damage.py:L97-L103] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestAgainstTransliterationSweep.test_all_presets_and_curves` [tests/fatigue/test_fatg.py:L267-L284] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_cfg` [tests/fatigue/test_fatg_workflow.py:L23-L49] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_library_reference` [tests/fatigue/test_fatg_workflow.py:L52-L64] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_sample_locations` [tests/fatigue/test_fatigue_reporting.py:L15-L55] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_damage_calls` [tests/fatigue/test_rainflow_invariants.py:L408-L488] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_damage_calls.calm_scatter` [tests/fatigue/test_rainflow_invariants.py:L443-L449] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_damage_calls.fapps_batch` [tests/fatigue/test_rainflow_invariants.py:L456-L461] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestGetSNCurve.test_f_curve_at_100mpa` [tests/fatigue/test_sn_curves.py:L40-L45] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestGetSNCurve.test_d_curve_at_100mpa` [tests/fatigue/test_sn_curves.py:L52-L57] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_hand_N` [tests/fatigue/test_weld_fatigue_quickcheck.py:L29-L31] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestSNcurve.test_cycles_to_failure_chain_known_value` [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L117-L123] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestSNcurve.test_cycles_to_failure_wire_rope_known_value` [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L125-L131] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestSNcurve.test_custom_sn_curve` [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L138-L143] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestMinersRuleDamage.test_single_range_known_damage` [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L294-L307] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestMinersRuleDamage.test_damage_additivity` [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L319-L329] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_small_atlas` [tests/parametric/test_atlas.py:L19-L34] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `process_single_test_file` [tests/signal_processing/signal_analysis/fatigue_analysis_reference/process_fatigue_final.py:L25-L439] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `process_single_test_file` [tests/signal_processing/signal_analysis/reference_fatigue_processor.py:L25-L439] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `module scope` [tests/signal_processing/signal_analysis/test_generate_outputs.py:L20-L20] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `analyze_signal` [tests/signal_processing/signal_analysis/test_generate_outputs.py:L97-L374] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `main` [tests/signal_processing/signal_analysis/test_generate_outputs.py:L377-L455] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `OrcaFlexTensionAnalyzer.analyze_tension` [tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py:L108-L177] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `OrcaFlexTensionAnalyzer.export_results` [tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py:L301-L352] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `run_rainflow_for_timeseries` [tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py:L19-L71] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_rainflow_for_timeseries` [tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py:L74-L271] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `main` [tests/signal_processing/signal_analysis/validate_module.py:L183-L219] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `mock_solver_factory.MockSolverFactory.create_fatigue_solver` [tests/solvers/test_integration_solvers.py:L154-L165] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `simple_sn_curve` [tests/structural/fatigue/conftest.py:L14-L16] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `dnv_d_curve` [tests/structural/fatigue/test_damage_accumulation.py:L40-L47] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `simple_curve` [tests/structural/fatigue/test_damage_accumulation.py:L51-L58] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `curve_with_material` [tests/structural/fatigue/test_damage_accumulation.py:L62-L76] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestSNCurveMigration.test_multislope_curve_implementation` [tests/structural/fatigue/test_fatigue_migration.py:L90-L128] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestSNCurveMigration.test_curve_continuity` [tests/structural/fatigue/test_fatigue_migration.py:L130-L161] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestFrequencyDomainMigration.test_basic_frequency_analysis` [tests/structural/fatigue/test_fatigue_migration.py:L231-L252] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `simple_sn` [tests/structural/fatigue/test_fatigue_reliability.py:L32-L34] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `mild_sn` [tests/structural/fatigue/test_fatigue_reliability.py:L38-L40] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_simple_input` [tests/structural/fatigue/test_fatigue_skill.py:L39-L49] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestOffshoreScenario.test_high_cycle_low_stress_result` [tests/structural/fatigue/test_fatigue_skill.py:L494-L512] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `simple_sn_curve` [tests/structural/fatigue/test_frequency_domain.py:L84-L86] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `default_counter` [tests/structural/fatigue/test_rainflow.py:L41-L43] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `simple_sn` [tests/structural/fatigue/test_scatter_fatigue.py:L25-L27] | Mapped: Checks or supplies inputs to the family method; AST-equivalent definition after docstring removal | copy | not checked | none |
| `TestFatigueLife.test_fatigue_life_inverse_of_damage` [tests/structural/fatigue/test_scatter_fatigue.py:L233-L251] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `simple_power_law` [tests/structural/fatigue/test_sn_curves.py:L57-L59] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `power_law_with_limit` [tests/structural/fatigue/test_sn_curves.py:L63-L67] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `bilinear_curve` [tests/structural/fatigue/test_sn_curves.py:L71-L86] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `dnv_d_curve` [tests/structural/fatigue/test_sn_curves.py:L90-L91] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestBS7608RevisedCurveParameters.test_bs7608_revised_d_known_n_at_50mpa` [tests/structural/fatigue/test_sn_curves.py:L886-L892] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestISO19902CurveParameters.test_iso19902_f_air_known_n_at_100mpa` [tests/structural/fatigue/test_sn_curves.py:L964-L970] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestISO19902CurveParameters.test_iso19902_f2_sw_cp_known_n_at_50mpa` [tests/structural/fatigue/test_sn_curves.py:L994-L1000] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_all_single_slope_cases` [tests/structural/fatigue/test_sn_curves_yaml.py:L55-L60] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_all_multislope_cases` [tests/structural/fatigue/test_sn_curves_yaml.py:L63-L66] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `sn_curve_data` [tests/structural/fatigue_analysis/conftest.py:L6-L13] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `run_fatigue_analysis` [tests/structural/fatigue_analysis/test_fatigue_analysis_sn.py:L13-L20] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `run_fatigue_analysis` [tests/structural/fatigue_analysis/test_fatigue_analysis_timetrace.py:L15-L40] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `ref_input` [tests/subsea/pipeline/test_free_span_f105.py:L43-L65] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestVIVFatigueCalculator.test_cumulative_damage` [tests/subsea/viv_analysis/test_viv_analysis_unit.py:L502-L519] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `get_orcaflex_post_process_mock` [tests/test_automation/fixtures/engineering_fixtures.py:L72-L94] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `ComprehensiveTestFixCampaign.generate_mock_data` [tests/test_automation/scripts/comprehensive_fix_campaign.py:L103-L153] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `process_configuration` [tests/test_complete_fatigue_pipeline.py:L65-L175] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `main` [tests/test_complete_fatigue_pipeline.py:L178-L270] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `run_all_tests` [tests/test_fatigue_basic.py:L223-L269] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `process_effective_tension_file` [tests/test_rainflow_with_effective_tension.py:L25-L114] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `main` [tests/test_rainflow_with_effective_tension.py:L117-L184] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `run_performance_tests` [tests/test_reservoir_framework.py:L800-L846] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `TestSNCurveDNVReference.test_dnv_d_curve_parameters` [tests/test_spectral_fatigue_validation.py:L50-L58] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_workflow_registry` [tests/workflows/test_durable_workflows.py:L52-L1680] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` [tests/workflows/test_durable_workflows.py:L1825-L1881] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` [tests/workflows/test_durable_workflows.py:L1884-L1951] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |

#### `external-solver-batch-run-stacks`

| Pinned implementation | Map classification | Relation | Controls | Covering pack |
| --- | --- | --- | --- | --- |
| `main` [docs/domains/orcaflex/examples/modular/A01/A01 Catenary riser/run_orcaflex.py:L45-L94] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `main` [docs/domains/orcaflex/examples/modular/A01/A01 Lazy wave riser/run_orcaflex.py:L44-L92] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `main` [docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py:L47-L113] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `main` [docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py:L48-L116] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `main` [docs/domains/orcaflex/examples/modular/K01/K01 5MW spar FOWT/run_orcaflex.py:L49-L114] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `main` [docs/domains/orcaflex/examples/modular/L01/L01 Default vessel/run_orcaflex.py:L44-L95] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `run_comparison` [docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_benchmark.py:L314-L344] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `run_orcawave_analysis` [docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_matched_api.py:L24-L128] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `run_orcawave_diffraction` [docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_api.py:L22-L100] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `reproduce` [docs/plans/evidence/issue-2082-reproduce.py:L133-L151] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `run_bemrosetta` [scripts/benchmark/run_3way_benchmark.py:L294-L385] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `_calc_statics` [scripts/benchmark_model_library.py:L163-L172] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `run_model` [scripts/benchmark_riser_library.py:L254-L279] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `run_mesh_variant` [scripts/benchmark_riser_library.py:L380-L427] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `_launch` (`scripts/cfd/run_sloshing_3d_check.py`) | Absent at code revision; map row retained as a missing-path record | not checked | not checked | none |
| `main` [scripts/cfd/run_sloshing_convergence.py:L123-L173] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `process_model_dir` [scripts/extract_library_validation.py:L64-L134] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `main` [scripts/extract_riser_validation.py:L56-L107] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `main` [scripts/generate_24in_statics_sim.py:L23-L43] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `CALMBuoyProjectGenerator.generate_orcaflex_modules` [scripts/generate_calm_buoy_project.py:L310-L386] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `CALMBuoyProjectGenerator._generate_batch_script` [scripts/generate_calm_buoy_project.py:L540-L689] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `run_mesh_variant` [scripts/mesh_sensitivity_riser.py:L162-L209] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `calcClearance` [scripts/python/digitalmodel/analysis/FinalRollerClearance.py:L12-L26] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/15_-_Analyses.py:L3-L3] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/16_-_TimeHistoryResultsPeriod.py:L3-L3] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/17_-_SampleTimes.py:L3-L3] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/18_-_TimeHistoryMultipleVariables.py:L3-L3] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/19_-_DistanceBetweenTwoVessels.py:L4-L4] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/1_-_FirstProgram.py:L3-L3] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/20_-_ObjectExtra.py:L3-L3] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/21_-_StaticResult.py:L3-L3] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/22_-_RangeGraphDynamics.py:L3-L3] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/26_-_LinkedStatistics.py:L3-L3] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py:L3-L3] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/29_-_ExtremeStatisticsRayleigh.py:L3-L3] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/30_-_ExtremeStatisticsLikelihood.py:L3-L3] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/31_-_ExtremeStatisticsAggregation.py:L5-L5] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/32_-_FrequencyDomainResults.py:L3-L3] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/33_-_FrequencyDomainMPM.py:L3-L3] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/34_-_FrequencyDomainRangeGraph.py:L3-L3] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/35_-_FrequencyDomainSpectralDensitySpectralResponse.py:L4-L4] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/36_-_FrequencyDomainTimeHistorySynthesis.py:L4-L4] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/51_-_SerializationYAMLPart1.py:L4-L4] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `main` [scripts/run_benchmarks.py:L229-L257] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `run_statics` [scripts/run_riser_analysis.py:L136-L149] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `run_dynamics` [scripts/run_riser_analysis.py:L209-L249] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `solve` [scripts/studies/surface_crack_post.py:L91-L117] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `ANSYSRunner.execute` [src/digitalmodel/ansys/runner.py:L128-L154] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `ANSYSRunner._execute_solver` [src/digitalmodel/ansys/runner.py:L156-L175] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `engine` [src/digitalmodel/engine.py:L87-L885] | Mapped: Runs or coordinates external solver work and its results | caller | not checked | none |
| `AqwaReader.get_result_group` [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L114-L164] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `AqwaUtilities.run_aqwa_analysis_as_subprocess` [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L74-L97] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `AQWABatchRunner._run_parallel` [src/digitalmodel/hydrodynamics/diffraction/aqwa_batch_runner.py:L158-L167] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `AQWABatchRunner._process_single_job` [src/digitalmodel/hydrodynamics/diffraction/aqwa_batch_runner.py:L171-L230] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `OrcaWaveBatchRunner._process_single_job` [src/digitalmodel/hydrodynamics/diffraction/orcawave_batch_runner.py:L171-L247] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `DiffractionWorkflow.router` [src/digitalmodel/hydrodynamics/diffraction/workflow.py:L44-L54] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `run_orcaflex` [src/digitalmodel/solvers/benchmark/solvers.py:L49-L112] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `OpenFOAMRunner._stage_plan` [src/digitalmodel/solvers/openfoam/runner.py:L346-L392] | Mapped: Runs or coordinates external solver work and its results | caller | not checked | none |
| `OpenFOAMRunner._dtchull_stage_plan` [src/digitalmodel/solvers/openfoam/runner.py:L394-L443] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `OpenFOAMRunner._required_executable` [src/digitalmodel/solvers/openfoam/runner.py:L586-L591] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `OrcaFlexModelWrapper.run_static_analysis` [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L346-L416] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `OrcaFlexModelWrapper.run_dynamic_simulation` [src/digitalmodel/solvers/orcaflex/core/model_interface.py:L418-L507] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `write_queue` (`src/digitalmodel/solvers/orcaflex/modular_generator/slay_execution.py`) | Absent at code revision; map row retained as a missing-path record | not checked | not checked | none |
| `Level2OrcaFlexValidator._get_model_warnings` [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L230-L267] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `OrcaFlexAPI.run_static_analysis` [src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py:L331-L364] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `OPPVisualization.save_views_for_files` [src/digitalmodel/solvers/orcaflex/opp_visualization.py:L61-L88] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `OPPVisualization._process_single_file` [src/digitalmodel/solvers/orcaflex/opp_visualization.py:L207-L229] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `OrcaFlexAnalysis.process_fea` [src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py:L61-L92] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `OrcaFlexCustomAnalysis.process_fea` [src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py:L83-L196] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `OrcaflexIterativeRuns.run_iterative_simulations` [src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py:L152-L173] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `OrcModalAnalysis.run_by_file` [src/digitalmodel/solvers/orcaflex/orcaflex_modal_analysis.py:L78-L96] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `OrcaFlexOptimizedParallelAnalysis.process_single_file` [src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel.py:L89-L187] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `process_single_file_worker` [src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel_v2.py:L39-L137] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `OrcaFlexParallelAnalysis.process_single_file` [src/digitalmodel/solvers/orcaflex/orcaflex_parallel_analysis.py:L59-L159] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `OrcaflexUtilities.file_run_and_save` [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L120-L134] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `run_one` [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L207-L312] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `OrcaFlexViewCapture.load_model` [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L1556-L1566] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `OrcaFlexModelRunner.run_single_model` [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L67-L143] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `run_models` [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L271-L341] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `save_orcaflex_views` [src/digitalmodel/solvers/orcaflex/schematic_capture.py:L79-L148] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `OrcaFlexTimeTraceProcessor._load_from_orcaflex` [src/digitalmodel/solvers/orcaflex/time_trace_processor.py:L128-L171] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | this pack |
| `BatchProcessor._default_processor` [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L258-L314] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `UniversalOrcaFlexRunner._process_single_model` [src/digitalmodel/solvers/orcaflex/universal/universal_runner.py:L371-L460] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `OrcaWaveOrchestrator._convert_to_orcaflex` [src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py:L433-L469] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `_run_inputs` [src/digitalmodel/workflows/installation_added_mass_evidence.py:L94-L117] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `_verify_run` [src/digitalmodel/workflows/installation_campaign.py:L50-L67] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `run_case` [src/digitalmodel/workflows/installation_instability_diagnostic.py:L113-L163] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `extract` [src/digitalmodel/workflows/installation_trace_extract.py:L202-L252] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `resolve_case_matrix` [src/digitalmodel/workflows/openfoam_batch_config.py:L79-L85] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `_base_name` [src/digitalmodel/workflows/openfoam_batch_config.py:L114-L127] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `run_cases` [src/digitalmodel/workflows/openfoam_batch_execution.py:L85-L106] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `mpi_command_plan` [src/digitalmodel/workflows/openfoam_batch_execution.py:L328-L352] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `run_command` [src/digitalmodel/workflows/openfoam_batch_execution.py:L355-L368] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `write_results` [src/digitalmodel/workflows/openfoam_batch_results.py:L48-L72] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `router` [src/digitalmodel/workflows/openfoam_run_batch.py:L83-L111] | Mapped: Runs or coordinates external solver work and its results | caller | not checked | none |
| `_settings` [src/digitalmodel/workflows/openfoam_run_batch.py:L114-L123] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `batch_config` [src/digitalmodel/workflows/orcaflex_reproduce.py:L85-L92] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `_code_identity` [src/digitalmodel/workflows/orcaflex_reproduce.py:L111-L121] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `_solve` [src/digitalmodel/workflows/orcaflex_reproduce.py:L166-L183] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `_readback` [src/digitalmodel/workflows/orcaflex_reproduce.py:L186-L211] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `router` [src/digitalmodel/workflows/orcaflex_run_batch.py:L65-L154] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `_resolve_models` [src/digitalmodel/workflows/orcaflex_run_batch.py:L204-L221] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `_write_summary` [src/digitalmodel/workflows/orcaflex_run_batch.py:L302-L334] | Mapped: Runs or coordinates external solver work and its results | independent | not checked | none |
| `_setup` [tests/ansys/test_runner_evidence_failures.py:L11-L26] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_run_solver_success` [tests/hydrodynamics/diffraction/test_aqwa_runner.py:L661-L669] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_models_validate_and_reproduce` [tests/orcaflex/test_generic_scr_slwr.py:L107-L147] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_patch_execution` [tests/solvers/openfoam/test_runner_prebuilt.py:L162-L185] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_change_dictionary_v2312_applies_literals_and_preserves_warm_internal` [tests/solvers/openfoam/test_warm_start.py:L103-L185] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_canonical_batch_cfg` [tests/solvers/openfoam/test_workflow_case_definition.py:L348-L361] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_sweep_point_solves_within_band` [tests/solvers/openfoam/validation/test_wave_excited_body_rao.py:L190-L225] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `main` [tests/solvers/orcaflex/batch_processing/scripts/run_analysis.py:L8-L22] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `main` [tests/solvers/orcaflex/modular_generator/goldens/basefile/capture.py:L79-L135] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `qa_model` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L280-L305] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `a01_riser_model` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L394-L403] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `lazy_wave_model` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L464-L473] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `fender_model` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L477-L486] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `pull_in_model` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L490-L507] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `modular_model` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L655-L676] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `modular_noload_model` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L707-L721] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `monolithic_noload_model` [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L725-L745] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `run_single_model` [tests/solvers/orcaflex/mooring-tension-iteration/fsts-l015-test-cases/scripts/run_models_to_sim.py:L38-L105] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `main` [tests/solvers/orcaflex/mooring-tension-iteration/fsts-l015-test-cases/scripts/run_models_to_sim.py:L231-L326] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `run_tests` [tests/solvers/orcaflex/run_tests.py:L38-L131] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `SingleFileParallelStrategy.process_file_sequential_load_parallel_analysis` [tests/solvers/orcaflex/single_file_parallel_test.py:L39-L124] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `SingleFileParallelStrategy._run_static_analysis` [tests/solvers/orcaflex/single_file_parallel_test.py:L126-L133] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `SingleFileParallelStrategy.compare_strategies` [tests/solvers/orcaflex/single_file_parallel_test.py:L168-L296] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `prepared_queue_inputs.DenyNative.find_spec` (`tests/solvers/orcaflex/test_slay_execution.py`) | Absent at code revision; map row retained as a missing-path record | not checked | not checked | none |
| `fake_repo` [tests/solvers/smoke/test_native_harness.py:L18-L30] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `fake_repo` [tests/solvers/smoke/test_native_model_harness.py:L16-L24] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `synthetic_study` [tests/workflows/test_installation_added_mass_comparison.py:L136-L185] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `study.reproduce` [tests/workflows/test_installation_campaign.py:L34-L48] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_identity_is_stable_and_changes_with_input_tool_and_capacity` [tests/workflows/test_openfoam_batch_identity.py:L44-L74] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_single_model` [tests/workflows/test_orcaflex_batch_contract.py:L34-L42] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_read_summary` [tests/workflows/test_orcaflex_batch_contract.py:L45-L47] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `_assert_verdict` [tests/workflows/test_orcaflex_batch_contract.py:L50-L58] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `resume_case` [tests/workflows/test_orcaflex_reproduce.py:L107-L127] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |
| `test_batch_child_configures_api_before_importing_router.intercept` [tests/workflows/test_orcaflex_reproduce.py:L208-L213] | Mapped: Checks or supplies inputs to the family method | caller | not checked | none |

### Review-named implementations not represented by the family tables

| Pinned implementation | Implemented operation | Relation | Controls | Covering pack |
| --- | --- | --- | --- | --- |
| `ModelValidator.validate_with_orcaflex` [src/digitalmodel/solvers/orcaflex/template_generator.py:L518-L565] | Loads a model and optionally checks statics; records `statics_converged` | independent | not checked | this pack |
| `OrcaflexUtilities.save_sim_file` [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L116-L118] | Saves `.sim` without a state check; differs from `file_run_and_save` | wrapper | no: no state check before save | this pack |
| `run_modular_direct` [scripts/benchmark_model_library.py:L388-L405] | Calls `Model` | caller | not checked | none |
| `run_mesh_sensitivity` [scripts/benchmark_model_library.py:L423-L451] | Calls `Model` | caller | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/10_-_IndexedDataAccess.py:L3-L3] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/11_-_LineTypeDataIsNonIndexed.py:L3-L3] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/12_-_WaveTrainSelection.py:L3-L3] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/13_-_CurrentSelection.py:L3-L3] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/14_-_WaveTrainSelectionByIndex.py:L3-L3] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/23_-_RangeGraphStaticState.py:L3-L5] | Calls `CalculateStatics`, `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/24_-_RangeGraphObjectExtra.py:L3-L5] | Calls `CalculateStatics`, `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/25_-_RangeGraphArclengthRange.py:L3-L5] | Calls `CalculateStatics`, `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/2_-_ShowObjects.py:L3-L3] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/37_-_SingleLineModePeriods.py:L3-L5] | Calls `CalculateStatics`, `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/38_-_WholeSystemModePeriods.py:L3-L10] | Calls `CalculateStatics`, `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/39_-_ModalAnalysisSpecification.py:L3-L5] | Calls `CalculateStatics`, `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/3_-_DeleteObjects.py:L3-L3] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/40_-_ModeDetails.py:L3-L5] | Calls `CalculateStatics`, `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/41_-_ModeShapeComponentIdentification.py:L3-L5] | Calls `CalculateStatics`, `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/46_-_ExcelInteropRead.py:L6-L16] | Calls `Model`, `SaveData` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/47_-_ExcelInteropChartOpenPyXL.py:L4-L7] | Calls `CalculateStatics`, `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/48_-_ExcelInteropChartXlsxWriter.py:L4-L7] | Calls `CalculateStatics`, `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/49_-_SerializationJSONPart1.py:L4-L18] | Calls `CalculateStatics`, `Model`, `dump` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/4_-_NamedObjects.py:L3-L3] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/53_-_SerializationHDF5Part1.py:L5-L8] | Calls `CalculateStatics`, `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/55_-_SerializationHDF5AdvancedPart1.py:L5-L8] | Calls `CalculateStatics`, `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/57_-_ScipyInterpolation.py:L5-L5] | Calls `Model` | independent | not checked | none |
| `calcHogBendZ` [scripts/python/digitalmodel/legacy/58_-_ScipyRootFinding.py:L31-L38] | Calls `CalculateStatics` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/58_-_ScipyRootFinding.py:L4-L4] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/5_-_GeneralEnvironmentDirectAccess.py:L3-L3] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/6_-_ModelMapping.py:L3-L3] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/7_-_ObjectType.py:L3-L3] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/8_-_GetDataSetData.py:L3-L3] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/9_-_DynamicDataAccess.py:L3-L3] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/Ellipsoid_convergence_study.py:L4-L4] | Calls `LoadData` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/Hemisphere_convergence_study.py:L7-L7] | Calls `LoadData` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/L05_Panel_pressures_1.py:L5-L5] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/L05_Panel_pressures_2.py:L10-L10] | Calls `Model` | independent | not checked | none |
| `module scope` [scripts/python/digitalmodel/legacy/Pyramid_convergence_study.py:L4-L4] | Calls `LoadData` | independent | not checked | none |
| `load_model` [scripts/run_riser_analysis.py:L129-L133] | Calls `LoadData`, `Model` | independent | not checked | none |
| `analyze_spec` [scripts/run_riser_analysis.py:L256-L363] | Calls `dump`, `load_model`, `run_dynamics`, `run_statics` | caller | not checked | none |
| `load_model` [src/digitalmodel/drilling_riser/global_model/orcaflex_run.py:L34-L35] | Calls `Model` | caller | not checked | none |
| `load_and_solve_statics` [src/digitalmodel/drilling_riser/global_model/orcaflex_run.py:L38-L41] | Calls `CalculateStatics`, `load_model` | caller | not checked | none |
| `run_dynamics` [src/digitalmodel/drilling_riser/global_model/orcaflex_run.py:L80-L81] | Calls `RunSimulation` | caller | not checked | none |
| `BenchmarkRunner._generate_hydro_data_yaml` [src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py:L335-L505] | Calls `dump` | independent | not checked | none |
| `OrcaFlexReportBuilder._load_simulation` [src/digitalmodel/orcaflex/reporting/builder.py:L77-L90] | Calls `Model` | caller | not checked | none |
| `run_model` [src/digitalmodel/solvers/smoke/model_probe.py:L80-L121] | Calls `CalculateStatics`, `LoadData`, `Model`, `RunSimulation`, `SaveSimulation` | independent | not checked | none |
| `extract_histories` [src/digitalmodel/workflows/installation_instability_diagnostic.py:L166-L193] | Calls `Model` | caller | not checked | none |
| `ParametricStudy.generate_case_configs` [src/digitalmodel/orcaflex/batch_parametric.py:L93-L111] | Produces per-case dictionaries by injecting parameter values into base configuration | independent | not checked | none |
| `ParametricStudy.create_directory_structure` [src/digitalmodel/orcaflex/batch_parametric.py:L113-L133] | Creates one output directory per generated case | independent | not checked | none |
| `run_orcaflex_qa` [src/digitalmodel/orcaflex/qa.py:L29-L39] | Delegates example QA checks to the documentation QA implementation | wrapper | not checked | none |
| `_represent_none_as_null` [src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py:L78-L82] | Writes Python None as the literal YAML scalar `null`; differs from section-5 tilde control | independent | not checked | none |

The separate `src/digitalmodel/orcaflex/` package contains 32 tracked files at the code revision (31 Python files); the following boundary inventory does not claim that every file implements load, statics, run or save.

- Boundary only. [src/digitalmodel/orcaflex/__init__.py:L1-L7]
- Boundary only. [src/digitalmodel/orcaflex/batch_parametric.py:L28-L34]
- Boundary only. [src/digitalmodel/orcaflex/code_check_engine.py:L33-L37]
- Boundary only. [src/digitalmodel/orcaflex/data/synthetic_rope_profiles.yml:L1-L6]
- Boundary only. [src/digitalmodel/orcaflex/environment.py:L29-L34]
- Boundary only. [src/digitalmodel/orcaflex/fowt_mooring_workflow.py:L25-L31]
- Boundary only. [src/digitalmodel/orcaflex/installation_analysis.py:L28-L34]
- Boundary only. [src/digitalmodel/orcaflex/model_builder.py:L28-L34]
- Boundary only. [src/digitalmodel/orcaflex/mooring_design.py:L38-L44]
- Boundary only. [src/digitalmodel/orcaflex/mooring_design_fowt.py:L88-L93]
- Boundary only. [src/digitalmodel/orcaflex/mooring_workflow.py:L27-L33]
- Boundary only. [src/digitalmodel/orcaflex/pipelay_analysis.py:L27-L31]
- Boundary only. [src/digitalmodel/orcaflex/postprocessor.py:L28-L34]
- Boundary only. [src/digitalmodel/orcaflex/qa.py:L9-L15]
- Boundary only. [src/digitalmodel/orcaflex/reporting/__init__.py:L22-L28]
- Boundary only. [src/digitalmodel/orcaflex/reporting/builder.py:L60-L66]
- Boundary only. [src/digitalmodel/orcaflex/reporting/config.py:L15-L16]
- Boundary only. [src/digitalmodel/orcaflex/reporting/sections/__init__.py:L1-L1]
- Boundary only. [src/digitalmodel/orcaflex/reporting/sections/code_check.py:L5-L11]
- Boundary only. [src/digitalmodel/orcaflex/reporting/sections/modal_analysis.py:L5-L11]
- Boundary only. [src/digitalmodel/orcaflex/reporting/sections/model_summary.py:L5-L10]
- Boundary only. [src/digitalmodel/orcaflex/reporting/sections/mooring_loads.py:L6-L12]
- Boundary only. [src/digitalmodel/orcaflex/reporting/sections/qa_summary.py:L9-L15]
- Boundary only. [src/digitalmodel/orcaflex/reporting/sections/range_graphs.py:L6-L12]
- Boundary only. [src/digitalmodel/orcaflex/reporting/sections/static_config.py:L5-L10]
- Boundary only. [src/digitalmodel/orcaflex/reporting/sections/time_series.py:L6-L12]
- Boundary only. [src/digitalmodel/orcaflex/riser_config.py:L32-L38]
- Boundary only. [src/digitalmodel/orcaflex/riser_input_schema.py:L56-L62]
- Boundary only. [src/digitalmodel/orcaflex/synthetic_rope_design.py:L32-L33]
- Boundary only. [src/digitalmodel/orcaflex/viv_screening.py:L27-L30]
- Boundary only. [src/digitalmodel/orcaflex/viv_screening_workflow.py:L59-L65]
- Boundary only. [src/digitalmodel/orcaflex/weather_window.py:L28-L34]

### Map discrepancies at the code revision

| Map-only record | Disposition |
| --- | --- |
| `effective-tension-riser-stack-up`: `tests/solvers/orcaflex/test_slay_layout.py::test_support_types_are_defined_before_vessel_references` | Path absent; no code citation is substituted |
| `orcaflex-text-model-writers`: `tests/solvers/orcaflex/test_slay_layout.py::test_support_types_are_defined_before_vessel_references` | Path absent; no code citation is substituted |
| `orcaflex-text-model-writers`: `tests/solvers/orcaflex/test_slay_load_form.py::_resolve_case.read` | Path absent; no code citation is substituted |
| `orcaflex-text-model-writers`: `tests/solvers/orcaflex/test_slay_load_form.py::_pipe_seed_points` | Path absent; no code citation is substituted |
| `external-solver-batch-run-stacks`: `scripts/cfd/run_sloshing_3d_check.py::_launch` | Path absent; no code citation is substituted |
| `external-solver-batch-run-stacks`: `src/digitalmodel/solvers/orcaflex/modular_generator/slay_execution.py::write_queue` | Path absent; no code citation is substituted |
| `external-solver-batch-run-stacks`: `tests/solvers/orcaflex/test_slay_execution.py::prepared_queue_inputs.DenyNative.find_spec` | Path absent; no code citation is substituted |
| `wave-spectra`: `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py::EnvironmentBuilder._order_wave_train` | Symbol absent; behaviour not established |
| `wave-spectra`: `tests/orcaflex/test_model_default_tokens.py::wave_order_findings` | Symbol absent; behaviour not established |


### Downstream import dependencies and concrete uses

Import declarations identify dependencies; only the direct-call and argument-use rows below establish executable use at the code revision. A loader or exception type passed as an argument is identified as argument use, rather than a call to that type. Import-only entries establish no executable use in this inventory.

| Dependency source | Imported target | Disposition and source use |
| --- | --- | --- |
| [src/digitalmodel/drilling_riser/campaign.py:L772-L772] | `parallel_runner.CaseFailed` | direct call: `CaseFailed` [src/digitalmodel/drilling_riser/campaign.py:L684-L685] |
| [src/digitalmodel/drilling_riser/global_model/build.py:L478-L478] | `modular_generator.ModularModelGenerator` | direct call: `ModularModelGenerator.from_spec` [src/digitalmodel/drilling_riser/global_model/build.py:L488-L488] |
| [src/digitalmodel/drilling_riser/global_model/orcaflex_run.py:L16-L16] | `orcaflex_api` | direct call: `orcaflex_api.api` [src/digitalmodel/drilling_riser/global_model/orcaflex_run.py:L23-L23] |
| [src/digitalmodel/engine.py:L25-L25] | `orcaflex.OrcaFlex` | direct call: `OrcaFlex` [src/digitalmodel/engine.py:L237-L237] |
| [src/digitalmodel/engine.py:L26-L28] | `orcaflex_file_management.OrcaflexFileManagement` | direct call: `OrcaflexFileManagement` [src/digitalmodel/engine.py:L257-L257] |
| [src/digitalmodel/engine.py:L29-L29] | `orcaflex_installation.OrcInstallation` | direct call: `OrcInstallation` [src/digitalmodel/engine.py:L286-L286] |
| [src/digitalmodel/engine.py:L30-L30] | `orcaflex_modal_analysis.OrcModalAnalysis` | direct call: `OrcModalAnalysis` [src/digitalmodel/engine.py:L248-L248] |
| [src/digitalmodel/engine.py:L31-L33] | `umbilical_analysis_components.UmbilicalAnalysis` | direct call: `UmbilicalAnalysis` [src/digitalmodel/engine.py:L254-L254] |
| [src/digitalmodel/engine.py:L49-L52] | `output_control.OutputController` | import only; executable use not established |
| [src/digitalmodel/engine.py:L49-L52] | `output_control.get_output_level_from_argv` | direct call: `get_output_level_from_argv` [src/digitalmodel/engine.py:L219-L219] |
| [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L8-L10] | `orcaflex_model_utilities.OrcaflexModelUtilities` | direct call: `OrcaflexModelUtilities` [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L13-L13] |
| [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L6-L8] | `orcaflex_model_utilities.OrcaflexModelUtilities` | direct call: `OrcaflexModelUtilities` [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L11-L11] |
| [src/digitalmodel/marine_ops/installation/jumper_input.py:L22-L22] | `modular_generator.ModularModelGenerator` | direct call: `ModularModelGenerator.from_spec` [src/digitalmodel/marine_ops/installation/jumper_input.py:L88-L88] |
| [src/digitalmodel/marine_ops/installation/jumper_input.py:L28-L28] | `yaml_validator.validate_orcaflex_yaml` | direct call: `validate_orcaflex_yaml` [src/digitalmodel/marine_ops/installation/jumper_input.py:L56-L56] |
| [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L13-L15] | `orcaflex_model_components.OrcaflexModelComponents` | direct call: `OrcaflexModelComponents` [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L354-L354] |
| [src/digitalmodel/workflows/installation_added_mass_bracket.py:L11-L11] | `yaml_utils.OrcaFlexLoader` | argument use: `yaml.load(..., OrcaFlexLoader)` [src/digitalmodel/workflows/installation_added_mass_bracket.py:L139-L139] |
| [src/digitalmodel/workflows/installation_added_mass_bracket.py:L11-L11] | `yaml_utils.orcaflex_dump` | direct call: `orcaflex_dump` [src/digitalmodel/workflows/installation_added_mass_bracket.py:L120-L120] |
| [src/digitalmodel/workflows/installation_added_mass_evidence.py:L12-L12] | `yaml_utils.OrcaFlexLoader` | argument use: `yaml.load(..., OrcaFlexLoader)` [src/digitalmodel/workflows/installation_added_mass_evidence.py:L115-L115] |
| [src/digitalmodel/workflows/installation_added_mass_materialize.py:L12-L12] | `yaml_utils.OrcaFlexLoader` | argument use: `yaml.load(..., OrcaFlexLoader)` [src/digitalmodel/workflows/installation_added_mass_materialize.py:L61-L61] |
| [src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py:L14-L14] | `yaml_utils.OrcaFlexLoader` | argument use: `yaml.load(..., OrcaFlexLoader)` [src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py:L37-L37] |
| [src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py:L14-L14] | `yaml_utils.orcaflex_dump` | direct call: `orcaflex_dump` [src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py:L107-L107] |
| [src/digitalmodel/workflows/installation_instability_diagnostic.py:L29-L29] | `run_state.state_name` | direct call: `state_name` [src/digitalmodel/workflows/installation_instability_diagnostic.py:L30-L30] |
| [src/digitalmodel/workflows/installation_numerical_compare.py:L16-L16] | `yaml_utils.OrcaFlexLoader` | argument use: `yaml.load(..., OrcaFlexLoader)` [src/digitalmodel/workflows/installation_numerical_compare.py:L195-L195] |
| [src/digitalmodel/workflows/installation_numerical_prepare.py:L13-L13] | `yaml_utils.OrcaFlexLoader` | argument use: `yaml.load(..., OrcaFlexLoader)` [src/digitalmodel/workflows/installation_numerical_prepare.py:L75-L75] |
| [src/digitalmodel/workflows/installation_numerical_prepare.py:L13-L13] | `yaml_utils.orcaflex_dump` | direct call: `orcaflex_dump` [src/digitalmodel/workflows/installation_numerical_prepare.py:L185-L185] |
| [src/digitalmodel/workflows/installation_seastates.py:L208-L208] | `yaml_utils.OrcaFlexLoader` | argument use: `yaml.load(..., OrcaFlexLoader)` [src/digitalmodel/workflows/installation_seastates.py:L44-L44] |
| [src/digitalmodel/workflows/installation_seastates.py:L208-L208] | `yaml_utils.orcaflex_dump` | direct call: `orcaflex_dump` [src/digitalmodel/workflows/installation_seastates.py:L234-L234] |
| [src/digitalmodel/workflows/installation_variant_study.py:L25-L25] | `yaml_utils.OrcaFlexLoader` | argument use: `yaml.load(..., OrcaFlexLoader)` [src/digitalmodel/workflows/installation_variant_study.py:L26-L26] |
| [src/digitalmodel/workflows/orcaflex_reproduce.py:L125-L125] | `orcaflex_api` | direct call: `orcaflex_api.is_configured` [src/digitalmodel/workflows/orcaflex_reproduce.py:L126-L126] |
| [src/digitalmodel/workflows/orcaflex_reproduce.py:L187-L187] | `orcaflex_utilities.OrcaflexUtilities` | direct call: `OrcaflexUtilities` [src/digitalmodel/workflows/orcaflex_reproduce.py:L190-L190] |
| [src/digitalmodel/workflows/orcaflex_reproduce_results.py:L13-L13] | `orcaflex_api.lazy_api` | direct call: `lazy_api` [src/digitalmodel/workflows/orcaflex_reproduce_results.py:L17-L17] |
| [src/digitalmodel/workflows/orcaflex_reproduce_results.py:L14-L14] | `orcaflex_objects.OrcaFlexObjects` | direct call: `OrcaFlexObjects` [src/digitalmodel/workflows/orcaflex_reproduce_results.py:L17-L17] |
| [src/digitalmodel/workflows/orcaflex_reproduce_results.py:L15-L15] | `opp_time_series.OPPTimeSeries` | direct call: `OPPTimeSeries` [src/digitalmodel/workflows/orcaflex_reproduce_results.py:L18-L18] |
| [src/digitalmodel/workflows/orcaflex_reproduce_results.py:L16-L16] | `opp_range_graph.OPPRangeGraph` | direct call: `OPPRangeGraph` [src/digitalmodel/workflows/orcaflex_reproduce_results.py:L18-L18] |
| [src/digitalmodel/workflows/orcaflex_reproduce_results.py:L126-L126] | `reporting.OrcaFlexAnalysisReport` | direct call: `OrcaFlexAnalysisReport` [src/digitalmodel/workflows/orcaflex_reproduce_results.py:L141-L144] |
| [src/digitalmodel/workflows/orcaflex_reproduce_results.py:L126-L126] | `reporting.generate_orcaflex_report` | direct call: `generate_orcaflex_report` [src/digitalmodel/workflows/orcaflex_reproduce_results.py:L145-L145] |
| [src/digitalmodel/workflows/orcaflex_run_batch.py:L32-L34] | `orcaflex_parallel_analysis.OrcaFlexParallelAnalysis` | import only; executable use not established |
| [src/digitalmodel/workflows/orcaflex_run_batch.py:L35-L35] | `yaml_utils.OrcaFlexLoader` | argument use: `yaml.load(..., OrcaFlexLoader)` [src/digitalmodel/workflows/orcaflex_run_batch.py:L339-L339] |
| [src/digitalmodel/workflows/orcaflex_run_batch.py:L35-L35] | `yaml_utils.orcaflex_dump` | direct call: `orcaflex_dump` [src/digitalmodel/workflows/orcaflex_run_batch.py:L252-L252] |
| [src/digitalmodel/workflows/orcaflex_run_batch.py:L193-L195] | `orcaflex_utilities.OrcaflexUtilities` | direct call: `OrcaflexUtilities` [src/digitalmodel/workflows/orcaflex_run_batch.py:L199-L199] |
| [src/digitalmodel/workflows/structure_hydrodynamic_candidate.py:L15-L15] | `yaml_utils.OrcaFlexLoader` | argument use: `yaml.load(..., OrcaFlexLoader)` [src/digitalmodel/workflows/structure_hydrodynamic_candidate.py:L157-L157] |
| [src/digitalmodel/workflows/structure_hydrodynamic_candidate.py:L15-L15] | `yaml_utils.orcaflex_dump` | direct call: `orcaflex_dump` [src/digitalmodel/workflows/structure_hydrodynamic_candidate.py:L191-L191] |
| [src/digitalmodel/workflows/structure_installation_prepare.py:L12-L12] | `yaml_utils.OrcaFlexLoader` | argument use: `yaml.load(..., OrcaFlexLoader)` [src/digitalmodel/workflows/structure_installation_prepare.py:L109-L109] |
| [src/digitalmodel/workflows/structure_installation_prepare.py:L12-L12] | `yaml_utils.orcaflex_dump` | direct call: `orcaflex_dump` [src/digitalmodel/workflows/structure_installation_prepare.py:L118-L118] |

The map records #2050 against the catenary family: 1 row is marked unaffected, 136 rows are not checked, and 0 rows are marked affected. This metadata does not qualify any catenary route as an independent G2 reference.

The two map-listed byte-identical file groups are verified at the code revision. The additional pair of legacy CSV templates is also byte-identical at that revision; this is a third file group found during the review-directory inventory. The 46 definition groups described as AST-equivalent are equal after docstring removal and exclusion of source locations; this does not establish byte-identical files or copying provenance.

### Follow-up: implementation-map omissions

The following review-named definitions are absent from the entire implementation map and are included above as additional source-read rows; the implementation map requires these entries to be considered for inclusion.

- `scripts/benchmark_model_library.py::run_modular_direct`. [scripts/benchmark_model_library.py:L388-L405]
- `scripts/benchmark_model_library.py::run_mesh_sensitivity`. [scripts/benchmark_model_library.py:L423-L451]
- `scripts/python/digitalmodel/legacy/10_-_IndexedDataAccess.py::module`. [scripts/python/digitalmodel/legacy/10_-_IndexedDataAccess.py:L3-L3]
- `scripts/python/digitalmodel/legacy/11_-_LineTypeDataIsNonIndexed.py::module`. [scripts/python/digitalmodel/legacy/11_-_LineTypeDataIsNonIndexed.py:L3-L3]
- `scripts/python/digitalmodel/legacy/12_-_WaveTrainSelection.py::module`. [scripts/python/digitalmodel/legacy/12_-_WaveTrainSelection.py:L3-L3]
- `scripts/python/digitalmodel/legacy/13_-_CurrentSelection.py::module`. [scripts/python/digitalmodel/legacy/13_-_CurrentSelection.py:L3-L3]
- `scripts/python/digitalmodel/legacy/14_-_WaveTrainSelectionByIndex.py::module`. [scripts/python/digitalmodel/legacy/14_-_WaveTrainSelectionByIndex.py:L3-L3]
- `scripts/python/digitalmodel/legacy/23_-_RangeGraphStaticState.py::module`. [scripts/python/digitalmodel/legacy/23_-_RangeGraphStaticState.py:L3-L5]
- `scripts/python/digitalmodel/legacy/24_-_RangeGraphObjectExtra.py::module`. [scripts/python/digitalmodel/legacy/24_-_RangeGraphObjectExtra.py:L3-L5]
- `scripts/python/digitalmodel/legacy/25_-_RangeGraphArclengthRange.py::module`. [scripts/python/digitalmodel/legacy/25_-_RangeGraphArclengthRange.py:L3-L5]
- `scripts/python/digitalmodel/legacy/2_-_ShowObjects.py::module`. [scripts/python/digitalmodel/legacy/2_-_ShowObjects.py:L3-L3]
- `scripts/python/digitalmodel/legacy/37_-_SingleLineModePeriods.py::module`. [scripts/python/digitalmodel/legacy/37_-_SingleLineModePeriods.py:L3-L5]
- `scripts/python/digitalmodel/legacy/38_-_WholeSystemModePeriods.py::module`. [scripts/python/digitalmodel/legacy/38_-_WholeSystemModePeriods.py:L3-L10]
- `scripts/python/digitalmodel/legacy/39_-_ModalAnalysisSpecification.py::module`. [scripts/python/digitalmodel/legacy/39_-_ModalAnalysisSpecification.py:L3-L5]
- `scripts/python/digitalmodel/legacy/3_-_DeleteObjects.py::module`. [scripts/python/digitalmodel/legacy/3_-_DeleteObjects.py:L3-L3]
- `scripts/python/digitalmodel/legacy/40_-_ModeDetails.py::module`. [scripts/python/digitalmodel/legacy/40_-_ModeDetails.py:L3-L5]
- `scripts/python/digitalmodel/legacy/41_-_ModeShapeComponentIdentification.py::module`. [scripts/python/digitalmodel/legacy/41_-_ModeShapeComponentIdentification.py:L3-L5]
- `scripts/python/digitalmodel/legacy/46_-_ExcelInteropRead.py::module`. [scripts/python/digitalmodel/legacy/46_-_ExcelInteropRead.py:L6-L16]
- `scripts/python/digitalmodel/legacy/47_-_ExcelInteropChartOpenPyXL.py::module`. [scripts/python/digitalmodel/legacy/47_-_ExcelInteropChartOpenPyXL.py:L4-L7]
- `scripts/python/digitalmodel/legacy/48_-_ExcelInteropChartXlsxWriter.py::module`. [scripts/python/digitalmodel/legacy/48_-_ExcelInteropChartXlsxWriter.py:L4-L7]
- `scripts/python/digitalmodel/legacy/49_-_SerializationJSONPart1.py::module`. [scripts/python/digitalmodel/legacy/49_-_SerializationJSONPart1.py:L4-L18]
- `scripts/python/digitalmodel/legacy/4_-_NamedObjects.py::module`. [scripts/python/digitalmodel/legacy/4_-_NamedObjects.py:L3-L3]
- `scripts/python/digitalmodel/legacy/53_-_SerializationHDF5Part1.py::module`. [scripts/python/digitalmodel/legacy/53_-_SerializationHDF5Part1.py:L5-L8]
- `scripts/python/digitalmodel/legacy/55_-_SerializationHDF5AdvancedPart1.py::module`. [scripts/python/digitalmodel/legacy/55_-_SerializationHDF5AdvancedPart1.py:L5-L8]
- `scripts/python/digitalmodel/legacy/57_-_ScipyInterpolation.py::module`. [scripts/python/digitalmodel/legacy/57_-_ScipyInterpolation.py:L5-L5]
- `scripts/python/digitalmodel/legacy/58_-_ScipyRootFinding.py::calcHogBendZ`. [scripts/python/digitalmodel/legacy/58_-_ScipyRootFinding.py:L31-L38]
- `scripts/python/digitalmodel/legacy/58_-_ScipyRootFinding.py::module`. [scripts/python/digitalmodel/legacy/58_-_ScipyRootFinding.py:L4-L4]
- `scripts/python/digitalmodel/legacy/5_-_GeneralEnvironmentDirectAccess.py::module`. [scripts/python/digitalmodel/legacy/5_-_GeneralEnvironmentDirectAccess.py:L3-L3]
- `scripts/python/digitalmodel/legacy/6_-_ModelMapping.py::module`. [scripts/python/digitalmodel/legacy/6_-_ModelMapping.py:L3-L3]
- `scripts/python/digitalmodel/legacy/7_-_ObjectType.py::module`. [scripts/python/digitalmodel/legacy/7_-_ObjectType.py:L3-L3]
- `scripts/python/digitalmodel/legacy/8_-_GetDataSetData.py::module`. [scripts/python/digitalmodel/legacy/8_-_GetDataSetData.py:L3-L3]
- `scripts/python/digitalmodel/legacy/9_-_DynamicDataAccess.py::module`. [scripts/python/digitalmodel/legacy/9_-_DynamicDataAccess.py:L3-L3]
- `scripts/python/digitalmodel/legacy/Ellipsoid_convergence_study.py::module`. [scripts/python/digitalmodel/legacy/Ellipsoid_convergence_study.py:L4-L4]
- `scripts/python/digitalmodel/legacy/Hemisphere_convergence_study.py::module`. [scripts/python/digitalmodel/legacy/Hemisphere_convergence_study.py:L7-L7]
- `scripts/python/digitalmodel/legacy/L05_Panel_pressures_1.py::module`. [scripts/python/digitalmodel/legacy/L05_Panel_pressures_1.py:L5-L5]
- `scripts/python/digitalmodel/legacy/L05_Panel_pressures_2.py::module`. [scripts/python/digitalmodel/legacy/L05_Panel_pressures_2.py:L10-L10]
- `scripts/python/digitalmodel/legacy/Pyramid_convergence_study.py::module`. [scripts/python/digitalmodel/legacy/Pyramid_convergence_study.py:L4-L4]
- `scripts/run_riser_analysis.py::load_model`. [scripts/run_riser_analysis.py:L129-L133]
- `scripts/run_riser_analysis.py::analyze_spec`. [scripts/run_riser_analysis.py:L256-L363]
- `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py::load_model`. [src/digitalmodel/drilling_riser/global_model/orcaflex_run.py:L34-L35]
- `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py::load_and_solve_statics`. [src/digitalmodel/drilling_riser/global_model/orcaflex_run.py:L38-L41]
- `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py::run_dynamics`. [src/digitalmodel/drilling_riser/global_model/orcaflex_run.py:L80-L81]
- `src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py::BenchmarkRunner._generate_hydro_data_yaml`. [src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py:L335-L505]
- `src/digitalmodel/orcaflex/reporting/builder.py::OrcaFlexReportBuilder._load_simulation`. [src/digitalmodel/orcaflex/reporting/builder.py:L77-L90]
- `src/digitalmodel/solvers/smoke/model_probe.py::run_model`. [src/digitalmodel/solvers/smoke/model_probe.py:L80-L121]
- `src/digitalmodel/workflows/installation_instability_diagnostic.py::extract_histories`. [src/digitalmodel/workflows/installation_instability_diagnostic.py:L166-L193]
- `src/digitalmodel/orcaflex/batch_parametric.py::ParametricStudy.generate_case_configs`. [src/digitalmodel/orcaflex/batch_parametric.py:L93-L111]
- `src/digitalmodel/orcaflex/batch_parametric.py::ParametricStudy.create_directory_structure`. [src/digitalmodel/orcaflex/batch_parametric.py:L113-L133]
- `src/digitalmodel/orcaflex/qa.py::run_orcaflex_qa`. [src/digitalmodel/orcaflex/qa.py:L29-L39]
- `src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py::_represent_none_as_null`. [src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py:L78-L82]

- `src/digitalmodel/solvers/orcaflex/template_generator.py::ModelValidator.validate_with_orcaflex`. The native-route family rejects this candidate as an input-validation/presentation helper, although the pinned function loads and can solve statics. [src/digitalmodel/solvers/orcaflex/template_generator.py:L518-L565]
- `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py::OrcaflexUtilities.save_sim_file`. [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L116-L118]

UNVERIFIED: control agreement for the indexed alternatives and complete map coverage require the follow-up in U12.

UNVERIFIED: inventory-row revisions and supported solver-version matrix for all
dependencies are not qualified by this pack; U4.

## 10. Coverage and open items

The main execution/validation path is documented above; the explicit limits
below remain open and prevent a CLEARED status.

| Item | Missing evidence / required next checkpoint                                                                                                                                                        | Responsible role                  | Issue                                                                                                                                                              |
| ---- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| U1   | Complete G0.1–G0.7 coverage, unit/frame/phase declarations for native and OPP data, and edition/clause/Citation evidence for standards-labelled transforms                                         | Riser and mooring quality manager | Input and unit evidence (#2519)                                                                               |
| U2   | AQG run records including load messages, independent statics checks, images, s/s ratio, signatures, report appendix and B1–B9 admission tests                                                      | Riser and mooring quality manager | QA evidence and admission (#2520)                                                                             |
| U3   | Independently sourced worked examples and native load/statics/dynamics attestations; source and result units must be checked before qualification                                                  | Riser and mooring quality manager | Library qualification (#2421), Attestation (#2428)       |
| U4   | Investigate the recorded traceback-reporting interruption and collection error; complete the native runtime matrix and dependency inventory; confirm deputy and shared-log reference; independently substantiate README maturity/performance claims | Riser and mooring quality manager | Coverage and documentation (#2522), Collection visibility (#1923); issue pending (5)                                                                            |
| U5   | Detect unavailable-interface fallback without claiming a native simulation or nonexistent output                                                                                                   | Riser and mooring quality manager | Implicit mock output (#2521)                                                                                  |
| U6   | Resolve missing fixture roots, including `test_load_orcaflex_files.py` and its 8 fixture-root skips, and retain explicit skip/xfail coverage                                                                                                                              | Test maintainer                   | Fixture coverage (#2008)                                                                                      |
| U7   | Frequency-domain strength detection and event-monitor concurrency regression                                                                                                                       | Test maintainer                   | Strength result type (#2325), Monitor scheduling (#1925) |
| U8   | Progress/continuation and versioned criteria agreement beyond the optional stop record                                                                                                             | Workflow maintainer               | Campaign contract (#2298)                                                                                     |
| U9 | Replace environment-specific labels in guarded defaults-test prose without changing solver guards | Test maintainer | issue pending (1) |
| U10 | Check legacy catenary arc-length scaling against an independent integral before reference use | Test maintainer | issue pending (2) |
| U11 | Separate pure filesystem/YAML checks from module/class native-interface guards | Test maintainer | issue pending (3) |
| U12 | Extend the map with 52 source-read omissions; retain five paths and two symbols absent at the code pin; establish source attribution and control agreement before substituting implementations | Module maintainer | issue pending (4) |
| U13 | Propagate failure of the second statics solve and execute the mock-based false-success detection in section 6 | Module maintainer | False-success defect (#2548) |

### Reading and execution boundary

Top-level Python source inventory: these bullets establish file presence only. Import or header locators establish no algorithm behaviour; detailed coverage is limited to the implementation paths cited above.

- `OrcaFlexAnalysis.py`. [src/digitalmodel/solvers/orcaflex/OrcaFlexAnalysis.py:L1-L1]
- `__init__.py`. `[src/digitalmodel/solvers/orcaflex/__init__.py:L4-L9]`
- `all_vars.py`. [src/digitalmodel/solvers/orcaflex/all_vars.py:L1-L1]
- `comprehensive_benchmark.py`. [src/digitalmodel/solvers/orcaflex/comprehensive_benchmark.py:L1-L4]
- `convert_cli.py`. [src/digitalmodel/solvers/orcaflex/convert_cli.py:L2-L7]
- `deploy_production_optimization.py`. [src/digitalmodel/solvers/orcaflex/deploy_production_optimization.py:L1-L6]
- `dynamics_progress.py`. [src/digitalmodel/solvers/orcaflex/dynamics_progress.py:L1-L1]
- `file_size_optimizer.py`. [src/digitalmodel/solvers/orcaflex/file_size_optimizer.py:L1-L6]
- `file_size_profiler.py`. [src/digitalmodel/solvers/orcaflex/file_size_profiler.py:L1-L5]
- `hybrid_processor.py`. [src/digitalmodel/solvers/orcaflex/hybrid_processor.py:L1-L5]
- `inverse_resolver.py`. [src/digitalmodel/solvers/orcaflex/inverse_resolver.py:L1-L6]
- `io_optimizer.py`. [src/digitalmodel/solvers/orcaflex/io_optimizer.py:L1-L5]
- `library_generator.py`. [src/digitalmodel/solvers/orcaflex/library_generator.py:L1-L6]
- `memory_optimizer_advanced.py`. [src/digitalmodel/solvers/orcaflex/memory_optimizer_advanced.py:L1-L5]
- `modal_benchmark.py`. [src/digitalmodel/solvers/orcaflex/modal_benchmark.py:L4-L9]
- `model_catalogue.py`. [src/digitalmodel/solvers/orcaflex/model_catalogue.py:L4-L4]
- `model_catalogue_render.py`. [src/digitalmodel/solvers/orcaflex/model_catalogue_render.py:L4-L4]
- `mooring.py`. [src/digitalmodel/solvers/orcaflex/mooring.py:L1-L1]
- `operability_analysis.py`. [src/digitalmodel/solvers/orcaflex/operability_analysis.py:L2-L5]
- `opp.py`. [src/digitalmodel/solvers/orcaflex/opp.py:L2-L2]
- `opp_config_expansion.py`. [src/digitalmodel/solvers/orcaflex/opp_config_expansion.py:L1-L6]
- `opp_linkedstatistics.py`. [src/digitalmodel/solvers/orcaflex/opp_linkedstatistics.py:L2-L2]
- `opp_range_graph.py`. [src/digitalmodel/solvers/orcaflex/opp_range_graph.py:L1-L1]
- `opp_summary.py`. [src/digitalmodel/solvers/orcaflex/opp_summary.py:L2-L2]
- `opp_summary_superseded.py`. [src/digitalmodel/solvers/orcaflex/opp_summary_superseded.py:L2-L2]
- `opp_time_series.py`. [src/digitalmodel/solvers/orcaflex/opp_time_series.py:L2-L2]
- `opp_time_series_v2.py`. [src/digitalmodel/solvers/orcaflex/opp_time_series_v2.py:L1-L6]
- `opp_visualization.py`. [src/digitalmodel/solvers/orcaflex/opp_visualization.py:L1-L1]
- `orcaflex.py`. [src/digitalmodel/solvers/orcaflex/orcaflex.py:L1-L1]
- `orcaflex_analysis.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_analysis.py:L2-L2]
- `orcaflex_analysis_components.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py:L1-L6]
- `orcaflex_api.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_api.py:L1-L6]
- `orcaflex_converter_enhanced.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_converter_enhanced.py:L2-L7]
- `orcaflex_custom_analysis.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py:L1-L1]
- `orcaflex_fatigue_analysis.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_fatigue_analysis.py:L1-L3]
- `orcaflex_file_management.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_file_management.py:L1-L1]
- `orcaflex_installation.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_installation.py:L1-L1]
- `orcaflex_iterative_runs.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py:L1-L1]
- `orcaflex_linetypes.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py:L1-L1]
- `orcaflex_modal_analysis.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_modal_analysis.py:L1-L1]
- `orcaflex_model_components.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L1-L1]
- `orcaflex_model_linesetup_wizard.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_model_linesetup_wizard.py:L2-L2]
- `orcaflex_model_utilities.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_model_utilities.py:L1-L1]
- `orcaflex_objects.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_objects.py:L2-L2]
- `orcaflex_optimized_parallel.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel.py:L1-L6]
- `orcaflex_optimized_parallel_v2.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel_v2.py:L1-L6]
- `orcaflex_parallel_analysis.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_parallel_analysis.py:L1-L5]
- `orcaflex_post_process.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_post_process.py:L1-L1]
- `orcaflex_preprocess.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_preprocess.py:L1-L1]
- `orcaflex_utilities.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py:L9-L9]
- `orcaflex_yml_converter.py`. [src/digitalmodel/solvers/orcaflex/orcaflex_yml_converter.py:L1-L1]
- `output_control.py`. [src/digitalmodel/solvers/orcaflex/output_control.py:L1-L5]
- `parallel_runner.py`. [src/digitalmodel/solvers/orcaflex/parallel_runner.py:L1-L6]
- `performance_monitor.py`. [src/digitalmodel/solvers/orcaflex/performance_monitor.py:L1-L4]
- `pipeline_schematic.py`. [src/digitalmodel/solvers/orcaflex/pipeline_schematic.py:L1-L6]
- `run_state.py`. [src/digitalmodel/solvers/orcaflex/run_state.py:L1-L6]
- `run_to_sim.py`. [src/digitalmodel/solvers/orcaflex/run_to_sim.py:L2-L7]
- `run_to_sim_cli.py`. [src/digitalmodel/solvers/orcaflex/run_to_sim_cli.py:L2-L5]
- `schematic_capture.py`. [src/digitalmodel/solvers/orcaflex/schematic_capture.py:L1-L6]
- `sim_retention.py`. [src/digitalmodel/solvers/orcaflex/sim_retention.py:L1-L6]
- `spec_author.py`. [src/digitalmodel/solvers/orcaflex/spec_author.py:L1-L6]
- `spec_upgrader.py`. [src/digitalmodel/solvers/orcaflex/spec_upgrader.py:L1-L6]
- `template_generator.py`. [src/digitalmodel/solvers/orcaflex/template_generator.py:L1-L6]
- `time_trace_processor.py`. [src/digitalmodel/solvers/orcaflex/time_trace_processor.py:L1-L6]
- `umbilical_analysis_components.py`. [src/digitalmodel/solvers/orcaflex/umbilical_analysis_components.py:L2-L2]
- `umbilical_installation_IV_to_host.py`. [src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py:L1-L1]
- `umbilical_installation_lower_2nd_end.py`. [src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py:L1-L1]
- `universal_cli.py`. [src/digitalmodel/solvers/orcaflex/universal_cli.py:L2-L7]
- `verify_optimization_deployment.py`. [src/digitalmodel/solvers/orcaflex/verify_optimization_deployment.py:L1-L4]
- `visualization.py`. [src/digitalmodel/solvers/orcaflex/visualization.py:L1-L1]
- `yaml_timestep_validation.py`. [src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py:L1-L4]
- `yaml_utils.py`. [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L1-L6]
- `yaml_validator.py`. [src/digitalmodel/solvers/orcaflex/yaml_validator.py:L1-L6]

No client-derived numerical data or private sidecar entry is supplied in this
pack; no sidecar identifier is claimed.
