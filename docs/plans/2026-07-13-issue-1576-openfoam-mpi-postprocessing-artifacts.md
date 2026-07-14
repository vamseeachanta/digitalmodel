# Plan for #1576: OpenFOAM MPI post-processing and host-local artifact index

> **Status:** draft
> **Complexity:** T2
> **Date:** 2026-07-13
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1576
> **Client:** N/A
> **Lane:** lane:codex
> **Execution mode:** single-lane — command planning, artifact identity, and queue publication form one fail-closed transaction
> **Review artifacts:** `scripts/review/results/2026-07-13-plan-1576-claude.md` (pending) | `scripts/review/results/2026-07-13-plan-1576-codex.md` (pending) | `scripts/review/results/2026-07-13-plan-1576-gemini.md` (pending)

---

## Resource Intelligence Summary

### Existing repo code

- `src/digitalmodel/workflows/openfoam_run_batch.py` currently gives serial/pool cases `to_vtk`, while its MPI plan stops after optional `reconstructPar`. Its readiness check also models a hand-written subset instead of the selected argv plan.
- The same workflow currently prunes `processor*` immediately after reconstruction, writes a completed checkpoint afterward, and returns rows containing `case_dir`; it does not create a heavy-artifact index.
- `src/digitalmodel/solvers/openfoam/runner.py` already defines the serial stage order and fail-closed fatal-log behavior. The batch MPI executor currently checks only process return codes, so parity will require fatal-marker inspection.
- `src/digitalmodel/solvers/openfoam/smoke.py` supplies the executable 8-rank precedent: `setFields`, `decomposePar -force`, `mpirun -np 8 ... -parallel`, mesh reconstruction when selected, then field reconstruction.
- `src/digitalmodel/solvers/openfoam/smoke_evidence.py` supplies reusable privacy and integrity conventions: safe relative POSIX paths, forbidden infrastructure fields, exact schemas, byte counts, file counts, and SHA-256 digests.
- `tests/workflows/test_openfoam_run_batch.py` already fixes the public return boundary at CSV/JSON files, fewer than 100 files, and less than 2 MiB per file; it also tests MPI order, retry, resume, and processor retention.
- `src/digitalmodel/solvers/openfoam/artifact_index.py` and `tests/solvers/openfoam/test_artifact_index.py` do not exist and will be created.

### Standards and registries

| Source | Finding |
|---|---|
| `data/document-index/standards-transfer-ledger.yaml` | No standard-specific constant or calculation applies; this issue is an execution/integrity contract. |
| `data/document-index/code-registry.yaml` | No separate registered OpenFOAM artifact-index implementation is available for reuse. |
| `data/document-index/online-resource-registry.yaml` | The registry contains OpenFOAM and VTK resources, but the repository's tested command contracts will remain authoritative for this bounded change. |
| Domain wiki search | No relevant OpenFOAM/VTK page supplies an artifact contract; no wiki update will be required. |

### Documents and issues consulted

- Issue [#1576](https://github.com/vamseeachanta/digitalmodel/issues/1576) requires MPI `foamToVTK`, exact selected-utility preflight, complete host-resident artifact metadata, bounded queue derivatives, and synthetic parity tests.
- Issue [#1560](https://github.com/vamseeachanta/digitalmodel/issues/1560) is the closed origin of the current batch/MPI execution surface; this plan will extend that surface rather than introduce a second workflow.
- Issue [#662](https://github.com/vamseeachanta/digitalmodel/issues/662) and `docs/plans/2026-07-10-issue-662-gmsh-openfoam-bridge.md` establish generic OpenFOAM evidence and 8-rank validation precedents.
- Issue [#1540](https://github.com/vamseeachanta/digitalmodel/issues/1540) owns the future unified public run-manifest schema. This issue will define only a versioned host-local execution index and will not claim that public schema.
- Issue [#1495](https://github.com/vamseeachanta/digitalmodel/issues/1495) confirms that solver-capable execution is host-routed; returned metadata must therefore remain portable and infrastructure-neutral.
- Issue [#640](https://github.com/vamseeachanta/digitalmodel/issues/640) reinforces the public/private geometry boundary; all new fixtures and examples will remain synthetic.
- `docs/plans/2026-06-29-cfd-environment-spec.md` places heavy solver outputs outside git and calls for small run-index metadata.
- `docs/plans/2026-05-15-issue-611-orcawave-result-contract.md` provides a prior explicit artifact-manifest and atomic-publication pattern.
- The required drive-index query `OpenFOAM MPI reconstruction foamToVTK artifact manifest` returned no relevant files. Stale-index warnings were not treated as evidence.
- `docs/README.md` is the canonical documentation entry point. Because this repository has no `docs/plans/README.md`, this plan will add a narrow current-plan row to `docs/README.md` instead of inventing a second plan index.

### Gaps identified

- No shared function derives readiness from the exact selected serial or MPI argv plan.
- MPI cannot schedule `foamToVTK`, and a zero return code with a fatal OpenFOAM marker can pass as completed.
- No versioned heavy-artifact record supplies stable identity, kind, run/input/tool hashes, size, content hash, and a privacy-safe locator.
- No root-confined resolver validates an opaque host-local locator against the indexed content before retrieval.
- Queue publication does not validate a closed derivative set as a transaction, and completed checkpoints are not bound to input/tool identity.

### Evidence (embedded verification)

**Issue statuses** (verified 2026-07-14T04:41:31Z):

- `#1576` — OPEN — OpenFOAM MPI runner: emit post-processing and VTK artifacts
- `#1560` — CLOSED — Land CFD execution surface on main + openfoam-run-batch saturating workflow
- `#662` — CLOSED — feat(openfoam): gmsh -> OpenFOAM polyMesh bridge (Phase B / 3D)
- `#1540` — OPEN — Unify the Hugging Face run-manifest schema
- `#1495` — OPEN — Onboard dedicated CFD compute box and offload CFD
- `#640` — OPEN — Build a generic 3D ballast-tank geometry and mesh pipeline

**Repository checks** (against `origin/main` at `2ff0f72c9c5ce9022bfca763a6bb24ae4fb768d4`):

```text
EXISTS  src/digitalmodel/workflows/openfoam_run_batch.py
EXISTS  src/digitalmodel/solvers/openfoam/runner.py
EXISTS  src/digitalmodel/solvers/openfoam/doctor.py
EXISTS  src/digitalmodel/solvers/openfoam/smoke.py
EXISTS  src/digitalmodel/solvers/openfoam/smoke_evidence.py
EXISTS  tests/workflows/test_openfoam_run_batch.py
MISSING src/digitalmodel/solvers/openfoam/artifact_index.py
MISSING tests/solvers/openfoam/test_artifact_index.py
```

**Current MPI seam:**

```python
plan.append(["decomposePar", "-force"])
plan.append(["mpirun", "-np", str(workers), "--oversubscribe", solver, "-parallel"])
if reconstruct:
    plan.append(["reconstructPar"])
```

The selected MPI plan has no `foamToVTK` stage. `_solver_ready()` currently adds the mesh utility, solver, `decomposePar`, `mpirun`, and optional `reconstructPar`; it cannot add selected `setFields`, `reconstructParMesh`, or `foamToVTK`.

**Reproduction proof:** N/A — this is a missing-capability issue. Static command-plan inspection above directly establishes the missing stage and readiness coverage; no solver installation or private case is required.

Distinct sources consulted: issue body, six source/test files, three prior plans, five related issues, three registries, wiki search, and required drive-index search.

---

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-07-13-issue-1576-openfoam-mpi-postprocessing-artifacts.md` |
| Artifact index implementation | `src/digitalmodel/solvers/openfoam/artifact_index.py` |
| Batch integration | `src/digitalmodel/workflows/openfoam_run_batch.py` |
| Readiness integration | `src/digitalmodel/solvers/openfoam/doctor.py` |
| Artifact tests | `tests/solvers/openfoam/test_artifact_index.py` |
| Batch/MPI tests | `tests/workflows/test_openfoam_run_batch.py` |
| Readiness tests | `tests/solvers/openfoam/test_doctor.py` |
| Default configuration | `src/digitalmodel/base_configs/modules/openfoam_run_batch/openfoam_run_batch.yml` |
| Synthetic example | `examples/workflows/openfoam-run-batch/input.yml` |
| Documentation row | `docs/README.md` |
| Plan reviews | `scripts/review/results/2026-07-13-plan-1576-{claude,codex,gemini}.md` in the workspace-hub review repository |

---

## Deliverable

The OpenFOAM batch workflow will schedule selected post-processing after serial or MPI solves, publish a versioned and privacy-safe host-local index for heavy outputs, and return only validated bounded CSV/JSON derivatives.

## Frozen Execution Contract

### Preflight

1. The workflow will validate configuration before cleaning, building, resuming, writing decomposition dictionaries, or invoking a utility.
2. `to_vtk: true` in MPI mode will require `reconstruct: true`; the unsupported `to_vtk: true, reconstruct: false` combination will fail before mutation.
3. The workflow will build the exact selected argv plan first and will preflight each distinct executable in that plan: mesh utility, optional `setFields`, `decomposePar`, `mpirun`, solver, optional `reconstructParMesh`, `reconstructPar`, and optional `foamToVTK`.
4. Resume mode will omit mesh, `setFields`, and decomposition only after validated `processor*` state is present. Otherwise the workflow will use the fresh plan.
5. Mock mode will validate and record plan shape but will emit no completed-solve or heavy-artifact claims.

### Stage order

The fresh MPI plan will be:

```text
mesh utility
optional setFields
decomposePar -force
mpirun -np <workers> --oversubscribe <solver> -parallel
optional reconstructParMesh -latestTime
reconstructPar
optional foamToVTK
artifact scan and index staging
bounded derivative staging and validation
optional processor-directory pruning
atomic index/summary/checkpoint publication
```

The resume plan will start at `mpirun` and will retain the same selected reconstruction, VTK, indexing, derivative, pruning, and publication suffix. Serial/pool execution will retain its current solver-stage order and will enter the same artifact/index/publication suffix after success.

`reconstruct_mesh` will be an explicit MPI option defaulting to false. It will select `reconstructParMesh -latestTime` immediately before `reconstructPar`; no heuristic will infer dynamic-mesh behavior from private case contents.

### Failure and cleanup behavior

- A nonzero exit, timeout, launch error, or OpenFOAM fatal marker at any utility will fail the case and will prevent every later stage.
- `foamToVTK`, hashing, index validation, derivative validation, and atomic publication failures will fail the case even when the solve succeeds.
- Failed and interrupted MPI runs will retain `processor*`, logs, and host-local heavy outputs for diagnosis/resume; they will not receive a completed checkpoint.
- `processor*` will be pruned only after reconstruction, optional VTK, artifact hashing, and derivative validation succeed. A prune failure will fail closed before completed publication.
- The completed checkpoint will be the final commit point. A retry will skip work only when checkpoint schema, run/input/tool hashes, and indexed content all validate; a mismatch will trigger a fresh or explicitly valid resume path.
- Each case will use an exclusive host-local lock while stages and hashing run. Hashing will reject files whose identity, size, or modification metadata changes during the scan.

## Frozen Artifact Contract

`artifact_index.json` will use schema version 1 and will remain distinct from the public dataset manifest owned by issue #1540. Its top-level fields will be:

```text
schema_version, run_id, input_sha256, tool_sha256, created_utc,
artifacts, derivatives
```

The workflow will compute `input_sha256` from canonical UTF-8 JSON for the effective public configuration after removing runtime-only/internal path keys. It will compute `tool_sha256` from canonical tool identity containing the source commit and SHA-256 of each selected executable; it will emit executable basenames, not resolved paths. `run_id` will be a lowercase SHA-256 derived from the schema tag, input hash, tool hash, and synthetic/public case name, so a retry of identical inputs and tools will retain identity.

Each heavy artifact record will contain exactly:

```text
artifact_id, kind, run_id, input_sha256, tool_sha256,
relative_path, size_bytes, file_count, content_sha256, locator
```

- `kind` will be one of `mesh_tree`, `field_tree`, `processor_field_tree`, `vtk_tree`, or `postprocessing_tree`.
- Files will use their byte SHA-256. Trees will use a deterministic SHA-256 over sorted safe relative POSIX paths, sizes, and file hashes. Symlinks, special files, absolute paths, traversal, NULs, and non-portable path encodings will fail validation.
- `artifact_id` will be SHA-256 over the schema tag, run ID, kind, relative path, size, file count, and content hash. Identical indexed bytes will therefore remain stable; changed bytes will change identity.
- `locator` will use `host-local:///<run_id>/<artifact_id>` with no authority component. It will never contain a hostname, IP address, username, mount root, or absolute case path.
- A host-side resolver will map only locally registered run/artifact IDs beneath the configured work root, will reject traversal/symlink escapes, and will revalidate size/content hash before retrieval.
- Returned rows will replace raw `case_dir` values with `run_id` plus opaque artifact locators. Absolute host paths may remain process-internal but will not appear in queue-returned JSON/CSV.

## Frozen Queue Derivative Policy

- The only queue-returned files will be `cases.csv`, `batch_summary.json`, and `artifact_index.json`, plus explicitly registered derivative producers that emit `.csv` or `.json`.
- Arbitrary path/glob copying will not be supported. VTK, mesh, time/field, `processor*`, raw log, and arbitrary `postProcessing` trees will never be queue derivatives.
- Every returned path will be a regular file beneath a staging root, will use a safe relative path, and will have a `.csv` or `.json` suffix.
- The transaction will permit fewer than 100 files, less than 2 MiB per file, and no more than 16 MiB total. Any violation will reject the entire derivative publication.
- Publication will use a temporary sibling directory plus atomic replacement. A partial index, summary, or checkpoint will not be visible as completed.

## Pseudocode

```text
function selected_command_plan(settings, run_settings, workers, resuming):
    validate cross-field options before filesystem mutation
    append fresh-only mesh, optional setFields, and decompose stages
    append mpirun solver stage
    append selected mesh reconstruction, field reconstruction, and foamToVTK
    return immutable ordered argv plan

function preflight_plan(plan):
    derive unique executable names from every selected argv
    resolve and hash every executable before case mutation
    fail with the complete missing-executable list
    return privacy-safe tool identity and tool hash

function index_case(case_root, run_identity):
    discover only closed heavy-artifact kinds under the root
    reject unsafe, special, escaped, or mutating entries
    hash regular files and deterministic trees
    create stable artifact IDs and opaque host-local locators
    validate exact schema and return index without absolute paths

function publish_result(case_result, artifact_index):
    generate only registered CSV/JSON derivatives into staging
    validate extension, path, regular-file type, count, per-file, and total limits
    prune processor directories only after all prior stages succeed
    atomically publish derivatives, index, summary, and completed checkpoint last
```

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/solvers/openfoam/artifact_index.py` | Isolate schema, hashing, privacy validation, resolver, and bounded derivative checks from the already-large batch module. |
| Create | `tests/solvers/openfoam/test_artifact_index.py` | Drive artifact identity, integrity, locator, root confinement, mutation, and bounds behavior first. |
| Modify | `src/digitalmodel/workflows/openfoam_run_batch.py` | Build exact plans, add MPI post-processing, share publication across modes, and move the commit point after validation. |
| Modify | `src/digitalmodel/solvers/openfoam/doctor.py` | Expose selected executable readiness without weakening existing doctor behavior. |
| Modify | `tests/workflows/test_openfoam_run_batch.py` | Add stage-order, failure, resume, parity, publication, and checkpoint tests before implementation. |
| Modify | `tests/solvers/openfoam/test_doctor.py` | Add exact selected-utility readiness tests before implementation. |
| Modify | `src/digitalmodel/base_configs/modules/openfoam_run_batch/openfoam_run_batch.yml` | Document `reconstruct_mesh` and the frozen artifact/derivative defaults. |
| Modify | `examples/workflows/openfoam-run-batch/input.yml` | Keep a generic mock example and explain host-local artifacts without claiming real output. |
| Modify | `docs/README.md` | Add the requested current issue-plan row. |

## TDD Test List

Tests will be written and observed failing before implementation. They will use only temporary directories, fake executables, injected command runners, and generic synthetic OpenFOAM trees.

| Test | Verification |
|---|---|
| `test_mpi_eight_rank_vtk_stage_order` | The selected fresh plan orders mesh, `setFields`, decomposition, 8-rank solve, reconstruction, then `foamToVTK`. |
| `test_mpi_resume_vtk_stage_order` | A validated resume starts at `mpirun` and retains reconstruction/VTK suffix order. |
| `test_mpi_vtk_requires_reconstruction_before_mutation` | Invalid cross-field options fail before clean/build/write/command calls. |
| `test_selected_plan_preflights_every_executable` | Parametrically removing each selected utility reports the full missing set and performs no mutation. |
| `test_unselected_utilities_are_not_required` | Optional utilities are absent from readiness when their stages are not selected. |
| `test_each_mpi_stage_failure_stops_later_stages` | Nonzero/timeout/launch failures at every stage stop the suffix, retain processors, and omit completion. |
| `test_mpi_fatal_marker_with_zero_rc_fails` | MPI matches serial fail-closed fatal-log behavior. |
| `test_vtk_hash_or_publication_failure_is_terminal` | Post-solve failures cannot produce a completed checkpoint. |
| `test_processor_prune_occurs_only_before_final_commit` | Pruning follows validated artifacts and precedes the completed checkpoint; prune failure fails closed. |
| `test_serial_and_mpi_emit_same_artifact_schema` | Equivalent synthetic outputs use the same schema, kinds, hashes, and locator rules. |
| `test_artifact_identity_is_stable_and_content_bound` | Repeated scans are stable; one-byte changes alter tree/content/artifact hashes and preserve exact sizes/counts. |
| `test_artifact_scan_rejects_unsafe_entries_and_mutation` | Symlinks, special files, escapes, invalid paths, and hash-time mutation fail. |
| `test_host_local_locator_is_private_and_root_confined` | Metadata contains no infrastructure strings/absolute roots and the resolver cannot escape its configured root. |
| `test_derivative_allowlist_and_bounds_are_transactional` | Unknown suffixes, raw-tree patterns, symlinks, count, per-file, and total limits reject the whole staging set. |
| `test_checkpoint_requires_matching_identity_and_content` | Only matching schema/input/tool/content can skip; drift reruns or resumes explicitly. |
| `test_mock_mode_emits_no_heavy_artifact_claims` | Mock records the plan without claiming solved artifacts. |
| `test_atomic_publication_exposes_no_partial_completion` | An injected write/replace failure leaves no completed checkpoint or mixed generation. |

## Acceptance Criteria

- [ ] Fresh MPI with `workers: 8`, `to_vtk: true`, and synthetic injected commands will execute the frozen order and will place `foamToVTK` after reconstruction.
- [ ] Resume and optional `reconstruct_mesh` paths will match the frozen stage contract exactly.
- [ ] Every selected executable will be preflighted before mutation; every missing-utility test will fail closed with no invoked stage.
- [ ] MPI will treat OpenFOAM fatal markers, nonzero exits, launch errors, and timeouts as failures and will stop later stages.
- [ ] Serial/pool and MPI will emit schema-version-1 artifact records with exact stable identity, kind, run/input/tool hashes, byte size, file count, content hash, and opaque host-local locator fields.
- [ ] No returned CSV/JSON value will expose an absolute path, hostname, IP address, username, or mount root.
- [ ] Resolver tests will prove root confinement and content revalidation; unsafe paths, symlinks, special files, and concurrent mutation will fail.
- [ ] Queue publication will return only regular `.csv`/`.json` derivatives, fewer than 100 files, less than 2 MiB each, and no more than 16 MiB total; raw VTK/mesh/field/processor/log trees will remain host-local.
- [ ] A completed checkpoint will appear only after all selected stages, artifact hashing, derivative validation, optional pruning, and atomic publication succeed.
- [ ] Generic fixtures and example text will pass the repository legal/privacy scan and will contain no client or project identifiers.
- [ ] Focused tests will pass: `PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 PYTHONPATH=src uv run pytest tests/solvers/openfoam/test_artifact_index.py tests/solvers/openfoam/test_doctor.py tests/workflows/test_openfoam_run_batch.py -q`.
- [ ] Existing OpenFOAM tests will pass: `PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 PYTHONPATH=src uv run pytest tests/solvers/openfoam tests/workflows/test_openfoam_run_batch.py -q`.
- [ ] Repository validation will pass: `git diff --check` and the workspace legal sanity scan in diff-only mode.
- [ ] T2 adversarial plan review will be recorded before `status:plan-review`; the user will be the only actor who can authorize `status:plan-approved`.
- [ ] Code/artifact cross-review and an issue implementation summary will occur only after a separately approved implementation.

## Non-Goals

- This issue will not upload heavy artifacts, define remote object-store retention, or implement the public Hugging Face manifest owned by #1540.
- This issue will not return raw OpenFOAM trees through the queue, infer private case semantics, or add client-derived fixtures/results.
- This issue will not broaden the queue's extension contract beyond CSV/JSON.
- This issue will not change solver physics, meshing algorithms, or numerical defaults.

## Adversarial Review Summary

Adversarial plan review has not started. Reviewers will be instructed to hunt for stage-order defects, preflight gaps, false completion, identity ambiguity, TOCTOU/symlink escapes, locator privacy leaks, oversized queue returns, and overlap with #1540. Default disposition will be non-approval until every major finding is resolved.

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | PENDING | Not yet dispatched. |
| Codex | PENDING | Not yet dispatched. |
| Gemini | PENDING | Not yet dispatched. |

**Overall result:** PENDING

## Risks and Open Questions

- **Risk — OpenFOAM variants:** utility flags differ across distributions. Tests will freeze only argv already supported by this repository, and real-host verification will capture tool identity rather than silently changing flags.
- **Risk — live-tree integrity:** hashing a changing solver tree can create a false identity. The implementation will combine exclusive case locking with before/after file metadata checks and fail on drift.
- **Risk — large-tree cost:** full SHA-256 is intentionally required for integrity. Hashing will stream files and avoid copies; performance measurement will be reported without weakening the content contract.
- **Risk — legacy rows:** removing returned absolute `case_dir` can affect consumers. Tests will migrate the public row contract to `run_id` and locator fields while keeping local paths process-internal.
- **Open question for plan review:** whether the 16 MiB aggregate derivative ceiling must be stricter to match the dispatch transport. The implementation will not begin until that bound is accepted or revised during review/user approval.

## Complexity: T2

The change will touch a bounded workflow, one new integrity module, configuration, documentation, and focused tests. It is multi-file and security-sensitive but does not require a cross-repository schema or migration.
