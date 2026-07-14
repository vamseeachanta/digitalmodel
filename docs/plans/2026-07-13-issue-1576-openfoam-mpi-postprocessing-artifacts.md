# Plan for #1576: OpenFOAM MPI post-processing and host-local artifact index

> **Status:** draft — r2 MAJOR findings resolved inline in r3; user approval required
> **Complexity:** T3
> **Date:** 2026-07-13
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1576
> **Client:** N/A
> **Lane:** lane:codex
> **Execution mode:** single-lane after the exact merged #1565 and #1575 interfaces are pinned
> **Review artifacts:** `scripts/review/results/2026-07-13-plan-1576-r{1,2}-consolidated.md`

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
    -> #1575 merged ParsedCaseDefinition + SelectedExecutionPlan
        -> #1576 MPI/post-processing + retained host-local artifact generation
```

1. #1565 is a hard dependency. #1576 will record its exact merged commit and will consume its `RunIdentity` and `WorkLayout`; it will not redefine run/input/source/tool identity, work-root precedence, or checkpoint namespace.
2. #1575 is a hard dependency. #1576 will record its exact merged commit and will consume `ParsedCaseDefinition.selected_execution_plan`; it will not infer utilities from partial settings. PrebuiltCaseV1 remains serial/pool only and prebuilt MPI rejects before mutation.
3. Deckhand [#564](https://github.com/vamseeachanta/deckhand/issues/564) owns transactional queue return, the exact maximum of 100 files and 2,000,000 bytes per file, terminal return error states, host-routing leases, authorization, retention, and cross-host locator retrieval. It is a downstream integration dependency, not permission for #1576 to modify Deckhand or host state.
4. Until #564 is merged and separately integrated, #1576 will create only host-local generations and metadata. It will not claim queue retrieval, queue-transactional publication, lease-backed retention, or remote locator resolution.

The draft commits for #1565 and #1575 are planning evidence only and will never satisfy these dependency gates. Implementation will stop unless both live issues are merged and their exact implementation SHAs/interfaces are recorded in this plan or an approved implementation handoff.

### Prior evidence

Issues #1560/#662 and the CFD environment spec establish pool/MPI, eight-rank,
and external-heavy-output precedents. #1540 retains public dataset-manifest
ownership. Registries, wiki, standards, and the required Drive query supplied no
reusable artifact-generation contract; no standards-derived constant applies.

### Literal reproduction

At base `2ff0f72c9c5ce9022bfca763a6bb24ae4fb768d4` on
`2026-07-14T04:59:03Z`, lines 465--476 of `openfoam_run_batch.py` generated MPI
argv with an oversubscription flag, no `foamToVTK`, and no case-selected mesh
reconstruction. This license-free static probe reproduces the issue.

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
2. validate #1565 `RunIdentity` against effective input, the clean current
   candidate source commit/tree/package digest, and selected tool identity; the
   recorded #1565/#1575 interface SHAs must be ancestors of that candidate;
3. consume #1565 `WorkLayout` and reject any generation root outside it or on a different device;
4. consume #1575 `ParsedCaseDefinition.selected_execution_plan` and reject unknown/dropped stages;
5. derive every required executable from the exact argv plan and resolve/hash every selected executable before mutation; and
6. validate MPI ranks before constructing argv.

`run_batch.workers` from #1575 is the sole requested rank authority. In
standalone mode its ceiling is process-visible affinity. In dispatcher mode an
authenticated `dispatcher_rank_limit` is a host capability ceiling, not a
second requested rank; it and `visible_rank_count` enter #1565 RunIdentity host-
capability fields. Workers must be 1..min(ceilings); invalid/over-limit values
reject before mutation. No code path adds `--oversubscribe`.

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

MPI `resume: true` rejects before mutation in this version. A retry creates a
fresh generation and reruns the complete selected plan; it never reopens a
failed generation or assumes decomposition state. Existing serial/pool resume
remains owned by #1565/#1575. `to_vtk: true` without reconstruction rejects.

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
  working-case/
  sealed/
  metadata/
  candidate_returns/
<case-root>/.generations/<generation-id>/
<case-root>/current.json
```

`generation_id` will be an opaque lowercase SHA-256 over the #1565 run ID and an injected unique attempt nonce using the framed codec below. Tests will inject deterministic nonces. The staging directory, final generation directory, and pointer will be required to share `st_dev`; cross-device copy/replace fallback is forbidden.

Mesh, solve, reconstruction, `foamToVTK`, and prune occur in `working-case/`.
With every producer terminated and #1565 run/case locks held, retained roots are
descriptor-copied into a new `sealed/` tree with no-follow/exclusive creation,
fsynced, made non-writable descriptor-relatively, then hashed from sealed file
descriptors. A complete second scan runs after sealing and immediately before
commit. The workflow then writes/fsyncs the commit manifest, atomically renames
staging to its final generation, and replaces `current.json` as the sole
completion point.

A reader will resolve only `current.json`, verify manifest/index hashes, reopen
every selected sealed file no-follow, recompute its byte/tree digest, and compare
the sealed generation device/inode identity. Byte drift rejects the read. Orphan,
failed, or unpointed generations are never current.

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

Snapshot traversal will open the sealed generation root and descendants through directory descriptors using `O_DIRECTORY`, `O_NOFOLLOW`, and `dir_fd` operations. It will reject symlinks and non-regular/non-directory entries. Each file will be opened with `O_NOFOLLOW`, hashed from its descriptor, and compared before/after on `st_dev`, `st_ino`, mode, size, and `st_ctime_ns`. A second descriptor walk will compare the exact sorted path set and identities, detecting additions, removals, replacements, and renames. Root and candidate source/tool identity will be revalidated before the commit manifest and again before pointer replacement.

The selected executable descriptors will be re-opened and rechecked for basename, device, inode, size, ctime, and content SHA-256 after post-processing. The source checkout will remain clean at the candidate RunIdentity commit; recorded #1565/#1575 SHAs remain verified ancestors. Any drift fails.

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

Argv enforcement will inspect constructed runtime tokens, and artifact checks
will inspect enumerated outputs/manifests. Plans, reviews, and tests remain under
normal legal scanning; no spelling ban or whole-file exemption is allowed.

## File Decomposition and Implementation Order

The 680-line implementation and 445-line test module will be split before behavior changes:

1. RED characterization tests will freeze current router, serial, MPI, checkpoint, and result behavior.
2. The merged #1565 `openfoam_batch_execution.py` will be modified to consume
   #1575 `SelectedExecutionPlan`; it will not be recreated or re-extracted.
3. Its existing focused tests will be extended; router tests stay bounded.
4. Both original files and every new/modified Python file will be at most 400 physical lines; every function/method will be at most 50 physical lines before feature work begins.
5. Artifact codec/snapshot and generation commit behavior will then be added in focused modules.
6. MPI VTK integration will be the final production slice.

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/workflows/openfoam_batch_execution.py` | selected plan, rank ceiling, preflight, MPI/VTK suffix |
| Modify | `tests/workflows/test_openfoam_batch_execution.py` | characterization plus new MPI tests |
| Create | `src/digitalmodel/solvers/openfoam/artifact_index.py` | framed codec, disjoint selections, descriptor snapshot, schema |
| Create | `src/digitalmodel/solvers/openfoam/artifact_generation.py` | staging, diagnostic generation, commit manifest, current pointer |
| Create | `tests/solvers/openfoam/test_artifact_index.py` | golden codec, mutation, selection, privacy tests |
| Create | `tests/solvers/openfoam/test_artifact_generation.py` | crash, same-device, commit/pointer tests |
| Modify | #1565 facade/config/execution/results modules | typed integration plus inactive result-policy registration |
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
- `test_fresh_vtk_stage_order_and_mpi_resume_rejects_before_mutation`
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
- `test_artifact_index_result_extension_is_reserved_inactive_until_564`
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
  test -n "$DIGITALMODEL_REL_FROM_HUB"
  test -n "$EXPECTED_DIGITALMODEL_SHA"
  test "$EXPECTED_DIGITALMODEL_SHA" = "$(git -C "$DIGITALMODEL_REL_FROM_HUB" rev-parse HEAD)"
  bash scripts/legal/legal-sanity-scan.sh --repo="$DIGITALMODEL_REL_FROM_HUB" --diff-only
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
| R2 Claude/Codex/Gemini | MAJOR | distinct findings resolved inline in r3 |

**Overall:** r1/r2 findings are resolved inline in r3; per the loop-break rule r3
is not redispatched. Explicit user approval remains required; no approval marker
or implementation is authorized.

## Risks and Open Questions

- Exact merged dependency interfaces are mandatory; drift stops implementation.
- Unsupported POSIX descriptor/same-device semantics fail rather than weaken.
- Full hashing remains mandatory; #564 owns retention, while the real-host canary
  and dispatcher envelope require separate post-code-review authorization.

## Complexity: T3

The work spans two hard upstream contracts, MPI execution, filesystem race resistance, content identity, crash-consistent generation publication, privacy boundaries, and a separately governed Deckhand transport dependency.
