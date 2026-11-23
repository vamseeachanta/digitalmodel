# OrcaFlex batch reliability — first execution slice

Issues: [2051](https://github.com/vamseeachanta/digitalmodel/issues/2051), [1564](https://github.com/vamseeachanta/digitalmodel/issues/1564)
Client: N/A | Lane: lane:codex | Complexity: T2
Status: plan review; implementation will follow review under the user's 2026-09-09 instruction to continue the proposed sequence. Owner-controlled approval labels will not be self-applied.

## Deliverable

The supported engine CLI will configure the OrcaFlex batch, render native-compatible defaults, and return failure when any case fails while retaining diagnostic artifacts. Existing agent entry points will link to the documented methodology and staged way forward.

## Resource intelligence and reproduction evidence

The work will build on origin/main 61e0c9c2, the existing workflow, canonical yaml_utils.py, run_contract.py, and the genuine engine-configuration regression in test_openfoam_run_batch.py. The deployed checkout and its unrelated edits will remain untouched.

Reproduction on 2026-09-09: calling engine(inputfile=...) on a copied mock example with the genuine ApplicationManager.configure raised `FileNotFoundError: Application input file base_configs/modules/orcaflex_run_batch/orcaflex_run_batch.yml not found`. The temporary reproduction also encountered Windows cleanup of its current directory; that harness-only residue will be removed after returning to the worktree.

The licensed reproduction already recorded in issue 2051 will anchor the YAML defect: native blank LogStartTime became `null`, rejected by OrcaFlex 11.6c; the workflow still exited zero with completed=0/failed=1. New tests will reproduce the serialization and verdict errors before code edits. Existing mixed-case tests will supply deterministic failure fixtures without requiring a licence.

Documents consulted: workspace-hub strategy and execution plan for issue 3831; digitalmodel issue bodies 1564/2051; batch source/tests/example; adjacent base configs; canonical OrcaFlex YAML utilities; central run verdict contract. The prior drive-index search returned no matches with five indexes inaccessible; no claim of complete drive coverage will be made. No standards constants will change.

## Design

1. A packaged base config will declare top-level basename: orcaflex_run_batch and mirror current code defaults: models/variants empty; analysis both; workers null (existing resolver semantics); mock false; save_sim true; results and batch_runs directories; standard default block. Tests will verify explicit example overrides survive the actual engine merge.
2. Variant/duration rendering will call the existing orcaflex_dump helper, preserving OrcaFlex default spelling (`~`) and boolean spelling. The proposed batch loader will preserve a native blank value as an empty string, including `RestartStateRecordingTest:`, and will keep that value distinct from an explicit `~` default marker. Rendering will retain this distinction instead of converting both forms to the same null/default value. The batch reader will explicitly accept UTF-8 input with or without a BOM and the shared writer will explicitly emit UTF-8, tested with non-ASCII names. Legacy cp1252 input will remain uncertified; the reader will not silently fall back to that encoding after a UTF-8 decoding failure. The source file will remain unchanged. This will not claim lexical round-trip fidelity for arbitrary duplicate-key/includes/script models.
3. The batch will populate the existing validation_verdict/output_directory/solver_available/validation_issues fields after writing current manifests. It will import the verified VERDICT_FAIL, VERDICT_PASS and VERDICT_SKIPPED constants from hydrodynamics.diffraction.validation_runner. Tests will assert membership in ALL_VERDICTS. Any failed case or empty completed case set will produce FAIL; successful explicitly mocked runs will produce SKIPPED with solver_available=false; successful real runs will produce PASS. The existing central CLI contract will emit run_verdict.json and map the verdict to process exit. No workflow-local sys.exit or parallel exit vocabulary will be introduced.
4. A bounded native proof will use one case and one worker, no production queue, generic in-memory/exported inputs, a subprocess timeout, statics/dynamics and saved-simulation reload. Thread use will be recorded honestly; changes to broader executor resource policy will remain in the execution workstream unless separately covered by this slice's test/review.
5. The digitalmodel operator map will point to the canonical workspace-hub methodology/runbook. Existing skills and the legacy queue README will gain short routing/status pointers in the documentation lane.

## Pseudocode

```text
configure: load packaged defaults -> merge user input -> call existing batch
render: parse source -> apply requested variant/duration -> native OrcaFlex dump
complete:
  write cases.csv and batch_summary.json
  if no cases or any case failed: validation_verdict = FAIL
  else if explicit mock: validation_verdict = SKIPPED
  else: validation_verdict = PASS
  attach current output paths, solver availability and failure diagnostics
  return cfg -> existing CLI writes sidecar and returns central exit status
```

## Artifact map and files to change

| File | Action |
|---|---|
| src/digitalmodel/base_configs/modules/orcaflex_run_batch/orcaflex_run_batch.yml | Add packaged defaults |
| src/digitalmodel/workflows/orcaflex_run_batch.py | Reuse dumper and expose central verdict |
| src/digitalmodel/solvers/orcaflex/yaml_utils.py | Explicit UTF-8 output encoding |
| tests/workflows/test_orcaflex_batch_contract.py | New focused RED-first engine/render/CLI regressions |
| tests/workflows/test_orcaflex_run_batch.py | Preserve existing regression coverage |
| docs/maps/digitalmodel-operator-map.md | Add methodology/runbook routing |
| docs/plans/README.md | Index this bounded plan |
| docs/reports/orcaflex-batch-reliability.html | Record actual test/native outcomes and limits |
| workspace-hub docs/solver and existing OrcaFlex skills | Parallel documentation lane under issue 3831 |

## TDD test list

- Actual engine configure on copied three-case mock example: reports produced, three completed, user workers preserved.
- Packaged defaults match existing router defaults; no ambient project or ApplicationManager mock hides loading.
- Variant/duration rendering: native default and boolean spelling, source bytes unchanged, requested numeric override retained.
- A native blank `RestartStateRecordingTest:` and an explicit `RestartStateRecordingTest: ~` will load as distinct values; a render/reload regression will verify that the blank remains an empty string while the explicit default retains its marker semantics.
- UTF-8 inputs with and without a BOM will preserve non-ASCII object names through rendering. An invalid UTF-8 fixture containing cp1252-only bytes will produce a decoding failure rather than a silent fallback or corrupted name.
- Mixed successful/missing case: durable summary and error details, FAIL verdict, central refusal.
- Entirely successful explicit mock: SKIPPED verdict, solver_available=false, CLI zero, clearly marked mock artifacts.
- Real-mode all-success with both _license_available and the executor stubbed: PASS with solver availability, central zero; no actual checkout in this offline test.
- Missing/empty executor results: FAIL, never stale-success substitution.
- Actual `sys.executable -m digitalmodel` success/failure subprocesses: expected exit plus fresh summary and sidecar; bounded timeout.
- Native manual opt-in acceptance outside the CI test suite: exported generic model with default fields will render, run and reload; nonfinite results or incomplete simulation will fail proof. Ordinary pytest runs will never trigger this acceptance or acquire a licence.

## Acceptance and scope limits

Focused tests will fail before repair and pass after it; existing batch/run-contract tests will pass. Legal scan and code-stage adversarial review will precede delivery. The native canary will demonstrate only the measured generic case, not arbitrary corpus conversion or engineering adequacy.

This slice will not deploy/restart the production agent, change host aliases, create trust envelopes, expand licence limits, release the complete library, or close issues automatically. Include relocation, same-stem collisions and corpus-level lossless conversion will remain explicitly tracked under the broader plan. Any native failure requiring a wider fix will be reported with evidence before widening scope.

## Risks and rollback

The canonical dumper is not a lossless native parser; licence-backed acceptance will verify the observed blank/default failure and the distinct empty-string/default treatment of `RestartStateRecordingTest`. UTF-8/BOM support will not certify legacy cp1252 models or arbitrary historical file encodings. Mock test success will not represent a real solve. Existing package dependencies will be reused without synchronizing the live solver environment. Changes will remain in an isolated worktree and draft PR; rollback will be branch abandonment, not alteration of deployed code.
