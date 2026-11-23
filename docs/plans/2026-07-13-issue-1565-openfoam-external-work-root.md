# Plan for [#1565](https://github.com/vamseeachanta/digitalmodel/issues/1565): External OpenFOAM batch work root

> **Status:** draft — r2 MAJOR findings resolved inline in r3; user approval required
> **Complexity:** T3
> **Date:** 2026-07-13
> **Client/Project:** N/A
> **Lane:** lane:claude
> **Review artifacts:** `scripts/review/results/2026-07-13-plan-1565-r{1,2}-consolidated.md`

## Resource Intelligence and Reproduction

- `openfoam_run_batch.py` currently resolves heavy case work below the input
  checkout, returns absolute `case_dir`, and performs clean/prune operations
  without an operator-root ownership contract. It is 680 lines; `router` and
  `_run_case_mpi` exceed 50 lines. Its test is 445 lines.
- Existing `oracaflex_run_batch.py` is behavior precedent only; it will not be
  changed. CFD host/environment docs require heavy work outside git and small
  results inside the dispatch scope.
- Issue [#1575](https://github.com/vamseeachanta/digitalmodel/issues/1575) will
  consume this layout/identity; [#1576](https://github.com/vamseeachanta/digitalmodel/issues/1576)
  will consume both. Order is **#1565 → #1575 → #1576**.
- A synthetic config resolved `_work_dir` below its config directory and emitted
  an absolute case path at base `2ff0f72c9c5ce9022bfca763a6bb24ae4fb768d4`.
  No private input or solver license is needed to reproduce the placement leak.
- Drive/standards/wiki searches found no reusable root/return contract. No
  standards-derived calculation applies.

## Artifacts

| Artifact | Path |
|---|---|
| Plan | `docs/plans/2026-07-13-issue-1565-openfoam-external-work-root.md` |
| Config/identity | `src/digitalmodel/workflows/openfoam_batch_config.py` |
| Layout/locks | `src/digitalmodel/workflows/openfoam_batch_layout.py` |
| Execution | `src/digitalmodel/workflows/openfoam_batch_execution.py` |
| Results | `src/digitalmodel/workflows/openfoam_batch_results.py` |
| Facade | `src/digitalmodel/workflows/openfoam_run_batch.py` |
| Tests | `tests/workflows/test_openfoam_batch_{identity,layout,execution,results,structure}.py` |

## Deliverable and Landing Boundary

The workflow will place heavy cases beneath an operator-authorized external root,
keep bounded results below input-local `results/`, bind reuse to source/input/tool
identity, and perform destructive work only through owned descriptor-confined
layouts. No host mutation, dispatch, approval label, or queue publication is
authorized by this plan.

## Execution Context and Root Authority

Hosted classification comes only from operator environment:

```text
DIGITALMODEL_EXECUTION_CONTEXT=hosted-deckhand
DIGITALMODEL_WORK_ROOT=<operator-private absolute root>
```

Hosted YAML may supply only `run_batch.work_root_namespace`, a portable relative
path with no empty/dot/dot-dot/control/NUL/absolute/symlink component. Missing or
invalid operator root fails before side effects. YAML cannot downgrade hosted
mode or choose an absolute root.

Trusted-local mode requires explicit `run_batch.execution_context: trusted-local`
with no hosted marker. Only there may `run_batch.work_root` name a precreated
writable absolute directory outside every Git checkout; symlinks reject. With no
external context, legacy placement remains byte-compatible. `output_dir` stays
input-local in all modes.

## RunIdentity v1

`openfoam_batch_config.py` will canonicalize strict JSON into:

```text
schema_version, identity_kind
source: git_commit_sha, tracked_tree_clean, package_name/version/content_sha256
effective_config_sha256
referenced_inputs[]: role, safe_relative_path, size_bytes, content_sha256
selected_executables[]: role, basename, content_sha256
host_capabilities: visible_rank_count, dispatcher_rank_limit
result_policy_version, work_layout_version, identity_sha256
```

The candidate checkout must be clean for package/config/input paths. Wheel mode
will parse every RECORD entry, reject missing/unrecorded package files, verify
actual size/hash against RECORD, and hash canonical actual-byte records; RECORD-
only hashing is forbidden. Source mode hashes exact tracked package bytes.
Referenced input identity includes the top-level request and every YAML/CSV/case
byte, including #1575 prebuilt evidence.

Every selected executable will be content-hashed before mutation, reopened and
revalidated immediately before and after every launch, and represented by
basename only. The full lowercase identity SHA names the namespace. Any source,
package, config, input, tool, host-ceiling, policy, or layout drift selects a new
identity and cannot reuse a checkpoint.

## Owned Layout, Locks, and Destructive Safety

```text
<operator-root>/<namespace>/openfoam-run-<identity>/
  .digitalmodel-run-owner.json
  .locks/
  <work-dir>/<case>/
```

The process will atomically create the run directory and an owner marker binding
schema, uid, identity, canonical root device/inode, and random owner token. A
preexisting unowned/mismatched namespace rejects. Run and case locks are acquired
before checkpoint read or mutation; they record boot ID/PID/start/heartbeat.
Stale reclaim requires expired heartbeat, proven-dead process/prior boot, matching
owner, and atomic tombstone rename. Unknown liveness never reclaims.

Destructive helpers accept `WorkLayout`, open the owned root once with directory/
no-follow flags, walk using `dir_fd|O_DIRECTORY|O_NOFOLLOW`, compare device/inode/
owner, and mutate descriptor-relatively. Path-only check-then-delete is forbidden.
Targets must be strict case descendants and never equal operator/namespace/run/
config/Git roots. Race tests substitute renames/symlinks at every clean/prune
seam and must leave replacements untouched.

## Checkpoint and Result Policy

External checkpoint v2 carries exact RunIdentity, owner token, case, status, and
bounded result row. Only completed exact matches skip. Corrupt/legacy/foreign
records rerun only while both locks and marker remain valid. Legacy mode retains
its current schema until a separate migration.

`result-policy-v1` has mandatory `cases.csv` and `batch_summary.json`. Extensions
are code-owned records with ID, exact basename, schema, media type, byte bound,
and policy version; YAML/glob discovery cannot register one. #1576 reserves but
does not activate `openfoam-artifact-index-v1` while Deckhand
[#564](https://github.com/vamseeachanta/deckhand/issues/564) is open. Activation
requires #564 merged plus separately approved integration/readback. Heavy trees,
checkpoints, logs, mesh/field/VTK/processor output remain external.

## Deckhand External-State Gate

Code merge will not authorize host configuration. The owner must separately
approve a preview naming environment/drop-in files, ownership/mode, restart,
rollback, exact agent/digitalmodel SHAs, and non-sensitive readback. After that,
a synthetic Deckhand canary must prove:

- systemd/agent inherits hosted context/root without logging it;
- heavy work is outside the scope checkout;
- bounded results still return input-locally;
- CSV/JSON/stdout/stderr/errors contain no host path; and
- rollback restores prior service/root behavior.

## File Decomposition

Before behavior changes, characterization tests will freeze argv, rows,
checkpoints, pool/MPI order, and legacy placement. The 680-line facade will split
into config/layout/execution/results modules; facade ≤200 lines/router ≤50. The
445-line test will split by identity/layout/execution/results. Every touched file
must be ≤400 lines and function ≤50 before feature edits. #1575/#1576 will modify,
not recreate, these exact modules.

## TDD Matrix

- Environment root wins; hosted mode rejects missing root or YAML absolute root.
- Trusted-local requires explicit opt-in and rejects Git/symlink roots.
- Namespace traversal/control/collision/foreign marker reject before side effect.
- Wheel/source/input/tool/host-capability mutation changes or rejects identity.
- Concurrent identical runs serialize; stale-lock reclaim follows every predicate.
- Descriptor clean/prune resists rename/symlink/inode substitution.
- Checkpoint exact-match skips; every identity/owner/schema drift reruns.
- Result base is stable; unknown/active-before-#564 extensions reject.
- External outputs/log tails redact canonical root; legacy mode stays identical.
- Real engine deep merge and OrcaFlex regression remain unchanged.
- Structure tests enforce 400/50 on every touched Python path.

## Implementation Sequence

1. Commit RED characterization, authority, identity, lock, path-race, checkpoint,
   result-policy, and redaction tests.
2. Land the behavior-preserving file split with characterization green.
3. Implement strict config plus RunIdentity actual-byte/tool binding.
4. Implement owned layout, descriptor operations, locks, checkpoint v2.
5. Implement result policy/redaction and inactive #1576 extension seam.
6. Run exact acceptance and T3 code/artifact review.
7. After merge only, request separate owner approval for host preview/canary.

## Exact Verification and Acceptance

```bash
PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 PYTHONPATH=src uv run python -m pytest -q tests/workflows/test_openfoam_run_batch.py tests/workflows/test_openfoam_batch_identity.py tests/workflows/test_openfoam_batch_layout.py tests/workflows/test_openfoam_batch_execution.py tests/workflows/test_openfoam_batch_results.py tests/workflows/test_openfoam_batch_structure.py tests/workflows/test_orcaflex_run_batch.py
PYTHONPATH=src uv run python -m pytest -q
uv run ruff check src/digitalmodel/workflows/openfoam_*.py tests/workflows/test_openfoam*.py
uv run python -m compileall -q src/digitalmodel/workflows
PYTHONPATH=src uv run python -m pytest -q tests/workflows/test_openfoam_batch_structure.py
git diff --check
```

For the legal gate, set `WORKSPACE_HUB_ROOT` and a candidate-relative
`DIGITALMODEL_REL_FROM_HUB`, then run:

```bash
EXPECTED_SHA="$(git rev-parse HEAD)"
test "$(git -C "$WORKSPACE_HUB_ROOT/$DIGITALMODEL_REL_FROM_HUB" rev-parse HEAD)" = "$EXPECTED_SHA"
(cd "$WORKSPACE_HUB_ROOT" && bash scripts/legal/legal-sanity-scan.sh --repo="$DIGITALMODEL_REL_FROM_HUB" --diff-only)
```

Acceptance additionally requires RED-before-GREEN evidence, exact merged
predecessor SHAs, zero new full-suite failures against base, real canary only
after separate approval, issue implementation comment, and no self-merge/close.

## Adversarial Review

R1 and R2 each reached three-provider MAJOR consensus. R1 required hosted root
authority, owned locking, source/tool identity, landing order, module split, and
Deckhand readback. R2 required actual installed-byte verification, descriptor-
anchored deletion, per-launch tool revalidation, dormant #564 return extension,
and portable legal invocation. Consolidated artifacts record exact reviewed SHAs.
All distinct findings are resolved inline in r3; under the loop-break rule r3 is
not redispatched. The plan remains draft until the user explicitly approves it;
no agent may create approval state.

## Risks and Non-Goals

- Same-account malicious host processes are outside the cooperative lock model;
  descriptor confinement still protects against path substitution.
- External roots may exhaust disk; owner provisioning/monitoring is separate.
- No host mutation, queue dispatch, public heavy result, OrcaFlex change, approval,
  merge, or close is included.
