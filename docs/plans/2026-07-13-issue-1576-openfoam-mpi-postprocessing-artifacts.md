# Plan for #1576: OpenFOAM MPI post-processing and host-local artifact index

> **Status:** draft — r1 MAJOR findings patched; r2 adversarial verification required
> **Complexity:** T3
> **Date:** 2026-07-13
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1576
> **Client:** N/A
> **Lane:** lane:codex
> **Execution mode:** single-lane after the exact merged #1565 and #1575 interfaces are pinned
> **Review artifacts:** `scripts/review/results/2026-07-13-plan-1576-r1-consolidated.md`; r2 Claude/Codex/Gemini artifacts pending

---

## Resource Intelligence Summary

### Existing repo code

- `src/digitalmodel/workflows/openfoam_run_batch.py` is 680 lines. Its serial path forwards `to_vtk`, but its MPI plan stops after optional `reconstructPar`, preflights only a hand-written executable subset, accepts unconstrained rank counts, uses `--oversubscribe`, and prunes `processor*` before durable artifact metadata exists.
- `tests/workflows/test_openfoam_run_batch.py` is 445 lines. Both touched files already exceed the universal 400-line limit and will be split before behavior changes.
- `src/digitalmodel/solvers/openfoam/runner.py` supplies serial stage ordering and fatal-marker handling. MPI currently checks only process return codes.
- `src/digitalmodel/solvers/openfoam/smoke.py` supplies a synthetic eight-rank precedent with `setFields`, decomposition, `mpirun -np 8`, and reconstruction.
- `src/digitalmodel/solvers/openfoam/smoke_evidence.py` supplies privacy-safe relative-path and SHA-256 evidence patterns, but not the immutable descriptor snapshot or generation commit required here.
- No `artifact_index.py`, attempt-generation module, host artifact ledger, or cross-host resolver exists.

### Required dependency order

Implementation order is strictly:

```text
#1565 merged RunIdentity + external WorkLayout
    -> #1575 merged ParsedCaseDefinition + selected ExecutionPlan
        -> #1576 MPI/post-processing + retained host-local artifact generation
```

1. #1565 is a hard dependency. #1576 will record its exact merged commit and will consume its `RunIdentity` and `WorkLayout`; it will not redefine run/input/source/tool identity, work-root precedence, or checkpoint namespace.
2. #1575 is a hard dependency. #1576 will record its exact merged commit and will consume its closed `ParsedCaseDefinition.execution_plan`; it will not infer required utilities from partial batch settings.
3. Deckhand [#564](https://github.com/vamseeachanta/deckhand/issues/564) owns transactional queue return, the exact maximum of 100 files and 2,000,000 bytes per file, terminal return error states, host-routing leases, authorization, retention, and cross-host locator retrieval. It is a downstream integration dependency, not permission for #1576 to modify Deckhand or host state.
4. Until #564 is merged and separately integrated, #1576 will create only host-local generations and metadata. It will not claim queue retrieval, queue-transactional publication, lease-backed retention, or remote locator resolution.

The draft commits for #1565 and #1575 are planning evidence only and will never satisfy these dependency gates. Implementation will stop unless both live issues are merged and their exact implementation SHAs/interfaces are recorded in this plan or an approved implementation handoff.

### Prior plans, registries, and searches

- Issue #1560 and the current workflow establish pool/MPI execution and small-result compatibility.
- `docs/plans/2026-06-29-cfd-environment-spec.md` places heavy solver output outside git.
- `docs/plans/2026-07-10-issue-662-gmsh-openfoam-bridge.md` establishes generic OpenFOAM evidence and eight-rank validation.
- `docs/plans/2026-05-15-issue-611-orcawave-result-contract.md` supplies a prior manifest/commit pattern, not an OpenFOAM artifact schema.
- Issue #1540 owns a future public dataset manifest. The host-local index here will not impersonate it.
- Standards/code registries and domain-wiki search contain no reusable host-local artifact-generation contract. No standards-derived constant applies.
- The required Drive query `OpenFOAM MPI reconstruction foamToVTK artifact manifest` returned no relevant file. Stale-index warnings were not used as evidence.

### Literal reproduction

Reproduced against `origin/main` `2ff0f72c9c5ce9022bfca763a6bb24ae4fb768d4` at `2026-07-14T04:59:03Z`:

```text
$ git show 2ff0f72c9c5ce9022bfca763a6bb24ae4fb768d4:src/digitalmodel/workflows/openfoam_run_batch.py | sed -n '465,476p'
    plan: list[list[str]] = []
    if not resume:
        plan.append([mesh_utility])
        if run_set_fields:
            plan.append(["setFields"])
        plan.append(["decomposePar", "-force"])
    plan.append(
        ["mpirun", "-np", str(workers), "--oversubscribe", solver, "-parallel"]
    )
    if reconstruct:
        plan.append(["reconstructPar"])
    return plan
```

The literal selected plan has no `foamToVTK`, uses `--oversubscribe`, and cannot include case-selected mesh reconstruction. The issue claim is reproduced without a solver or private case.

### Gaps

- No exact selected-plan readiness or rank-ceiling contract exists.
- No MPI fatal-marker parity or MPI `foamToVTK` stage exists.
- No immutable, non-overlapping retained-artifact snapshot exists.
- No attempt-generation commit/pointer prevents mixed or stale metadata.
- No queue transport or authorized cross-host resolver exists; #564 will own those capabilities.

---

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-07-13-issue-1576-openfoam-mpi-postprocessing-artifacts.md` |
| R1 review | `scripts/review/results/2026-07-13-plan-1576-r1-consolidated.md` |
| Artifact codec/snapshot | `src/digitalmodel/solvers/openfoam/artifact_index.py` |
| Attempt generation/commit | `src/digitalmodel/solvers/openfoam/artifact_generation.py` |
| Extracted batch execution | `src/digitalmodel/workflows/openfoam_batch_execution.py` |
| Batch router/integration | `src/digitalmodel/workflows/openfoam_run_batch.py` |
| Readiness | `src/digitalmodel/solvers/openfoam/doctor.py` |
| Codec/snapshot tests | `tests/solvers/openfoam/test_artifact_index.py` |
| Generation tests | `tests/solvers/openfoam/test_artifact_generation.py` |
| Extracted execution tests | `tests/workflows/test_openfoam_batch_execution.py` |
| Remaining router tests | `tests/workflows/test_openfoam_run_batch.py` |
| Config/example | `src/digitalmodel/base_configs/modules/openfoam_run_batch/openfoam_run_batch.yml`; `examples/workflows/openfoam-run-batch/input.yml` |

No queue adapter, host service, retention daemon, or Deckhand file is part of this issue.

## Deliverable

After #1565 and #1575 merge, OpenFOAM batch attempts will execute an exact bounded-rank MPI/post-processing plan inside one host-local generation and will commit a privacy-safe index of retained heavy artifacts only after immutable snapshot verification.

## Frozen Execution Contract

### Dependency and identity preflight

Before cleaning, creating a generation, or invoking a utility, the workflow will:

1. require the recorded exact merged #1565 and #1575 interface versions;
2. validate #1565 `RunIdentity` against the canonical effective input, clean pinned source commit, source-tree digest, and selected tool identity;
3. consume #1565 `WorkLayout` and reject any generation root outside it or on a different device;
4. consume #1575 `ParsedCaseDefinition.execution_plan` and reject unknown/dropped stages;
5. derive every required executable from the exact argv plan and resolve/hash every selected executable before mutation; and
6. validate MPI ranks before constructing argv.

For standalone execution, `dispatcher_rank_limit` will equal the process-visible CPU/rank count. For dispatcher execution, it will be mandatory authenticated execution-envelope data. `visible_rank_count` will use the process CPU-affinity count where supported and `os.cpu_count()` only as the explicit fallback. `workers` will be an integer from 1 through `min(visible_rank_count, dispatcher_rank_limit)`; booleans, zero, negatives, and over-limit requests will fail before filesystem mutation. No code path will add `--oversubscribe`.

### Exact stage order

Fresh MPI execution will use:

```text
selected mesh utility
optional setFields
decomposePar -force
mpirun -np <workers> <solver> -parallel
optional reconstructParMesh -latestTime
reconstructPar
optional foamToVTK
processor prune
final retained-artifact snapshot
generation commit manifest
current-generation pointer replacement
```

Resume will begin at the MPI solver only after #1565 identity and #1575 execution-plan validation prove the generation resumable. It will retain the same selected reconstruction, VTK, prune, snapshot, and commit suffix. `to_vtk: true` with reconstruction disabled will fail before mutation. Serial/pool execution will keep its #1575 stage plan and will enter the same generation/snapshot suffix.

The real-host canary will use exactly eight ranks, will prove the dispatcher ceiling and visible affinity are both at least eight, and will assert argv contains `mpirun -np 8 <solver> -parallel` with no oversubscription flag.

### Failure contract

- Nonzero exit, timeout, launch error, OpenFOAM fatal marker, identity drift, tool drift, prune failure, snapshot mutation, or commit failure will fail the generation and prevent `current.json` replacement.
- Failed/interrupted MPI generations will retain `processor*` and logs. They may receive a generation-specific `diagnostic_index.json` after all processes terminate and a best-effort immutable scan succeeds.
- A diagnostic index will carry `state: failed`, generation ID, stage/error class, and locally observed diagnostic roots. It will never be merged into a completed artifact index, selected by `current.json`, or described as queue-returned.
- Successful reconstructed generations will prune `processor*` before the final scan. A completed index will never contain `processor_field_tree`.
- Mock mode will validate schema/rank/plan shape but will not resolve solver executables, execute stages, or emit solved/heavy-artifact claims.

## Generation and Commit Contract

Every attempt will operate entirely below the #1565 case root:

```text
<case-root>/.generations/.<generation-id>.staging/
  case/
  metadata/
  candidate_returns/
<case-root>/.generations/<generation-id>/
<case-root>/current.json
```

`generation_id` will be an opaque lowercase SHA-256 over the #1565 run ID and an injected unique attempt nonce using the framed codec below. Tests will inject deterministic nonces. The staging directory, final generation directory, and pointer will be required to share `st_dev`; cross-device copy/replace fallback is forbidden.

Mesh, solve, reconstruction, `foamToVTK`, processor pruning, retained snapshot, and candidate derivative generation will all occur inside the same staging generation. On success the workflow will fsync files/directories, write `metadata/generation.commit.json`, atomically rename the staging directory to its immutable final generation name, then atomically replace `current.json` as the sole completion point. The pointer will contain only schema version, run ID, generation ID, commit-manifest SHA-256, and state `completed`.

A reader will resolve only the generation named by `current.json`, verify the commit-manifest hash, and verify the referenced artifact-index hash. Orphan staging, failed, or unpointed generations are never current. Cleanup/retention of those generations and cross-host access are not authorized by this issue.

## Immutable Snapshot and Hash Contract

### Non-overlapping completed artifact roots

The completed index will use only these disjoint selections beneath `generation/case`:

| Kind | Included root/members | Explicit exclusions |
|---|---|---|
| `mesh_tree` | `constant/polyMesh/**` | every path outside that root |
| `field_tree` | root-level nonnegative finite OpenFOAM numeric time directories and their descendants | `constant`, `system`, `VTK`, `postProcessing`, `processor*`, logs, metadata, and non-time roots |
| `vtk_tree` | `VTK/**` | every path outside `VTK` |
| `postprocessing_tree` | `postProcessing/**` | every path outside `postProcessing` |

The numeric-time parser will accept canonical decimal/scientific names representing finite values greater than or equal to zero and will reject aliases, traversal, separators, and duplicate numeric values with different spellings. Empty or absent selections will be omitted. Logs and processors are diagnostic-only. No path can belong to two completed kinds.

### Descriptor snapshot

Snapshot traversal will open the generation root and descendants through directory descriptors using `O_DIRECTORY`, `O_NOFOLLOW`, and `dir_fd` operations. It will reject symlinks and non-regular/non-directory entries. Each file will be opened with `O_NOFOLLOW`, hashed from its descriptor, and compared before/after on `st_dev`, `st_ino`, mode, size, and `st_ctime_ns`. A second descriptor walk will compare the exact sorted path set and directory/file identities, detecting additions, removals, replacements, and renames. Root identity and source/tool identity will be revalidated immediately before the commit manifest is written.

The selected executable descriptors will be re-opened and rechecked for basename, device, inode, size, ctime, and content SHA-256 after post-processing. The source checkout will be required to remain at the pinned #1565 commit with no tracked changes before execution and before commit. Any drift fails the generation.

### Length-framed codec

All tree, artifact, generation, and commit identity hashes will use versioned length framing, never delimiter concatenation or JSON stringification:

```text
frame(bytes) = uint64-big-endian(len(bytes)) || bytes
tree stream = frame(UTF8("dm-artifact-tree-v1")) || record...
file record = frame(UTF8("file"))
              || frame(UTF8(relative POSIX path))
              || frame(uint64-big-endian(size_bytes))
              || frame(raw 32-byte file SHA-256)
```

Records will be sorted by UTF-8 path bytes. Paths must be canonical safe relative POSIX UTF-8. The file digest is over exact bytes. The tree digest is SHA-256 of the complete framed stream. Artifact/generation/commit IDs will use separate domain tags and the same framing rule. `artifact_id` will frame, in order, domain tag `dm-artifact-id-v1`, run ID, generation ID, kind, relative selection, unsigned size, unsigned file count, and raw content digest. No timestamp, host path, or mutable locator participates.

Golden vectors will be literal tests:

```text
SHA256(frame("dm-artifact-tree-v1"))
  = a4474ba5694b4356dc78a265d304584b2de64afcf6136c6cd968025dbd1d6708
SHA256(file bytes "x")
  = 2d711642b726b04401627ca9fbac32f5c8530fb1903cc4db02258717921a4881
tree stream with one record path="0/U", size=1, digest=SHA256("x")
  = 15749b5cd2b684b587d2d7da29accbe736aeb6736d6afa2a0500fe5fd01e6c73
```

Each completed artifact record will contain exact schema version, artifact ID, kind, #1565 run/input/source/tool hashes, generation ID, safe relative selection, size bytes, file count, content SHA-256, and a host-local opaque locator.

## Host-Local and Queue Boundary

`candidate_returns/` may contain only regular `.csv` and `.json` files generated from parsed data, at most 100 files, each no larger than 2,000,000 bytes. Raw VTK, mesh, field, processor, post-processing, and log trees cannot enter it. These are host-local candidates only.

Before Deckhand #564 is merged and separately integrated:

- no candidate file or artifact index will be claimed as queue-returned or transactionally delivered;
- `host-local:///<run-id>/<generation-id>/<artifact-id>` means only that the execution host can identify the retained generation while it exists;
- no remote consumer, retention period, authorization decision, routing lease, or cross-host resolver is promised; and
- #1576 will not create a public host ledger or accept a locator over a network boundary.

Deckhand #564 will define the authorized ledger/resolver/lease/retention and full-set queue state machine. A later approved integration will consume its exact merged contract; #1576 will not pre-implement it.

## Enforcement Self-Coverage

The literal reproduction and r1 forensic artifact necessarily quote the retired
oversubscription token. Any enforcement added by this issue will inspect the
runtime argv produced from `openfoam_batch_execution.py`, not raw documentation,
review artifacts, or a repository-wide spelling grep. Tests will assert the
constructed argv token set without creating a blanket file/path exemption.
Likewise, heavy-artifact checks will validate enumerated candidate files and
generated manifests rather than banning words such as `VTK` from their own
plans/tests. The existing legal scanner will scan the plan, review, tests, and
implementation normally; no whole-file exemption will be introduced.

## File Decomposition and Implementation Order

The 680-line implementation and 445-line test module will be split before behavior changes:

1. RED characterization tests will freeze current router, serial, MPI, checkpoint, and result behavior.
2. Pure execution-plan/preflight/executor code will move to `openfoam_batch_execution.py` without behavior change.
3. Execution tests will move to `test_openfoam_batch_execution.py`; remaining router tests will stay in `test_openfoam_run_batch.py`.
4. Both original files and every new/modified Python file will be at most 400 physical lines; every function/method will be at most 50 physical lines before feature work begins.
5. Artifact codec/snapshot and generation commit behavior will then be added in focused modules.
6. MPI VTK integration will be the final production slice.

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/workflows/openfoam_batch_execution.py` | extracted selected-plan, rank, preflight, and executor seam |
| Create | `tests/workflows/test_openfoam_batch_execution.py` | extracted characterization plus new MPI tests |
| Create | `src/digitalmodel/solvers/openfoam/artifact_index.py` | framed codec, disjoint selections, descriptor snapshot, schema |
| Create | `src/digitalmodel/solvers/openfoam/artifact_generation.py` | staging, diagnostic generation, commit manifest, current pointer |
| Create | `tests/solvers/openfoam/test_artifact_index.py` | golden codec, mutation, selection, privacy tests |
| Create | `tests/solvers/openfoam/test_artifact_generation.py` | crash, same-device, commit/pointer tests |
| Modify | `src/digitalmodel/workflows/openfoam_run_batch.py` | thin integration consuming #1565/#1575 and extracted modules |
| Modify | `tests/workflows/test_openfoam_run_batch.py` | retain bounded router/compatibility tests below 400 lines |
| Modify | `src/digitalmodel/solvers/openfoam/doctor.py` | expose exact selected-executable readiness |
| Modify | `tests/solvers/openfoam/test_doctor.py` | selected/unselected utility tests |
| Modify | packaged config and synthetic example | document rank, VTK, host-local-only behavior without private defaults |

## TDD Test List

- `test_split_preserves_current_router_serial_mpi_checkpoint_results`
- `test_all_touched_files_and_functions_meet_400_50`
- `test_dependency_versions_and_interfaces_fail_closed`
- `test_workers_reject_bool_zero_negative_visible_and_dispatcher_overflow`
- `test_eight_rank_canary_argv_has_no_oversubscribe`
- `test_selected_plan_preflights_every_and_only_selected_executable`
- `test_fresh_and_resume_vtk_stage_order`
- `test_vtk_requires_reconstruction_before_mutation`
- `test_nonzero_timeout_launch_and_fatal_marker_stop_suffix`
- `test_failed_generation_retains_processors_and_is_never_current`
- `test_completed_generation_prunes_processors_before_snapshot`
- `test_completed_index_never_contains_processor_or_diagnostic_roots`
- `test_artifact_root_selections_are_disjoint_and_alias_time_names_reject`
- `test_framed_hash_golden_vectors_and_field_order_independence`
- `test_symlink_special_path_escape_and_invalid_utf8_reject`
- `test_descriptor_snapshot_detects_replace_add_remove_rename_and_ctime_drift`
- `test_source_and_tool_revalidation_fail_on_dirty_commit_or_binary_drift`
- `test_generation_rejects_cross_device_and_mixed_attempt_paths`
- `test_crash_before_pointer_never_changes_current_generation`
- `test_pointer_commit_and_index_hash_chain`
- `test_host_local_candidates_enforce_csv_json_100_and_2000000_boundaries`
- `test_no_queue_retrieval_lease_or_retention_claim_without_deckhand_contract`
- `test_mock_emits_no_solved_artifact_claim`

Every test will use generic temporary trees, fake executable descriptors, injected clocks/nonces/affinity, and synthetic logs. RED evidence will precede each implementation slice.

## Acceptance Criteria

- [ ] Exact merged #1565 and #1575 SHAs/interfaces are recorded, and implementation order #1565 -> #1575 -> #1576 is proven.
- [ ] Generated runtime argv and non-forensic execution source contain no oversubscription flag; the literal reproduction/r1 evidence remains the only quoted historical evidence. Workers never exceed visible or authenticated dispatcher ranks.
- [ ] The synthetic/host canary uses exactly eight ranks and completes reconstruction then `foamToVTK` before pruning and snapshot.
- [ ] Completed indexes contain only retained, disjoint mesh/time-field/VTK/post-processing selections; processor/log diagnostics are separate and generation-specific.
- [ ] Framed-codec goldens match exactly, and descriptor tests detect symlinks, special files, mutation, replacement, addition, removal, and rename.
- [ ] Clean pinned #1565 source/run/tool identity is checked before mutation and immediately before commit.
- [ ] All attempt work and post-processing occur in one same-device generation; only a verified commit manifest plus atomic pointer creates completion.
- [ ] No queue transaction, remote resolver, lease, retention, or authorization claim exists before Deckhand #564 is merged and separately integrated.
- [ ] Every touched Python file is at most 400 physical lines and every function/method at most 50 physical lines.
- [ ] Focused tests pass from the digitalmodel root:

  ```bash
  PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 PYTHONPATH=src uv run python -m pytest -q \
    tests/solvers/openfoam/test_artifact_index.py \
    tests/solvers/openfoam/test_artifact_generation.py \
    tests/solvers/openfoam/test_doctor.py \
    tests/workflows/test_openfoam_batch_execution.py \
    tests/workflows/test_openfoam_run_batch.py
  ```

- [ ] OpenFOAM regressions pass:

  ```bash
  PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 PYTHONPATH=src uv run python -m pytest -q \
    tests/solvers/openfoam tests/workflows/test_openfoam_batch_execution.py \
    tests/workflows/test_openfoam_run_batch.py
  ```

- [ ] Static checks pass:

  ```bash
  uv run ruff check src/digitalmodel/solvers/openfoam \
    src/digitalmodel/workflows/openfoam_batch_execution.py \
    src/digitalmodel/workflows/openfoam_run_batch.py \
    tests/solvers/openfoam tests/workflows/test_openfoam_batch_execution.py \
    tests/workflows/test_openfoam_run_batch.py
  uv run python -m compileall -q src/digitalmodel/solvers/openfoam \
    src/digitalmodel/workflows
  git diff --check
  ```

- [ ] Literal file-size gate passes; function-size coverage is enforced by `test_all_touched_files_and_functions_meet_400_50` using Python AST end-line metadata:

  ```bash
  wc -l src/digitalmodel/workflows/openfoam_run_batch.py \
    src/digitalmodel/workflows/openfoam_batch_execution.py \
    src/digitalmodel/solvers/openfoam/artifact_index.py \
    src/digitalmodel/solvers/openfoam/artifact_generation.py \
    tests/workflows/test_openfoam_run_batch.py \
    tests/workflows/test_openfoam_batch_execution.py \
    tests/solvers/openfoam/test_artifact_index.py \
    tests/solvers/openfoam/test_artifact_generation.py \
    | awk '$1 > 400 { bad=1; print } END { exit bad }'
  ```

- [ ] From the canonical workspace-hub checkout, the legal scan passes the implementation checkout recorded in the execution handoff:

  ```bash
  bash scripts/legal/legal-sanity-scan.sh --repo=../digitalmodel --diff-only
  ```

- [ ] T3 r2 plan review has no MAJOR before user approval is requested; T3 code/artifact review has no MAJOR before close.

## Non-Goals

- No Deckhand implementation, host service installation, queue upload, network resolver, retention activation, or locator authorization.
- No public/Hugging Face run-manifest schema, raw heavy-tree return, client fixture, or private result.
- No processor artifact in a completed retained index.
- No blanket enforcement exemption for this plan, its review artifact, tests, or implementation.
- No physics, meshing, or solver-default change beyond exact selected stage execution.

## Adversarial Review Summary

R1 reviewed exact SHA `b97aa4f42546c75781687dad67c0d420527accd5`. The consolidated verdict was **MAJOR**. This revision incorporates the consensus patches for dependency order, rank/argv correctness, prune/snapshot order, disjoint artifact roots, framed hashes, descriptor snapshots, source/tool revalidation, generation commit semantics, queue/Deckhand boundaries, and 400/50 decomposition.

| Round | Verdict | State |
|---|---|---|
| R1 Claude/Codex/Gemini consensus | MAJOR | findings consolidated in the r1 artifact |
| R2 Claude/Codex/Gemini | PENDING | must verify this pushed revision; no resolution is self-asserted |

**Overall:** revised draft for r2, not approval-ready. No label, approval marker, implementation, or issue comment is authorized by this revision.

## Risks and Open Questions

- **Dependency drift:** #1565/#1575 draft APIs may change. #1576 will bind only their exact merged interfaces and will stop on mismatch.
- **Filesystem semantics:** descriptor traversal and same-device rename require POSIX support on the CFD execution host. Unsupported platforms will fail preflight rather than weaken guarantees.
- **Hashing cost:** complete SHA-256 is required for integrity; implementation will stream from descriptors and report timing without sampling.
- **Retention:** host-local locators can expire when owner policy removes generations. #564 must define retention/lease semantics before any remote claim.
- **Real-host authority:** the eight-rank canary and dispatcher envelope are external execution actions and require separately authorized dispatch after code review.

## Complexity: T3

The work spans two hard upstream contracts, MPI execution, filesystem race resistance, content identity, crash-consistent generation publication, privacy boundaries, and a separately governed Deckhand transport dependency.
