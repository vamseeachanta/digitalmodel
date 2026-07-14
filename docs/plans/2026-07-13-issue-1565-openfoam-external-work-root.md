# Plan for [#1565](https://github.com/vamseeachanta/digitalmodel/issues/1565): External OpenFOAM batch work root

> **Status:** draft — r1 MAJOR findings incorporated; r2 adversarial review required
> **Complexity:** T3
> **Date:** 2026-07-13
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1565
> **Client:** N/A
> **Project:** N/A
> **Lane:** lane:claude
> **Execution mode:** single-lane implementation after explicit user approval
> **Review artifact:** scripts/review/results/2026-07-13-plan-1565-r1-consolidated.md

---

## Resource Intelligence Summary

### Existing repository behavior

- src/digitalmodel/workflows/openfoam_run_batch.py resolves relative output_dir
  and work_dir against _config_dir_path, so the defaults place results/ and
  batch_runs/ beside the dispatch input.
- The 680-line workflow contains an 89-line router plus configuration, matrix
  expansion, pool/MPI execution, cleanup, checkpoint, and publication logic.
- tests/workflows/test_openfoam_run_batch.py is 445 lines. Both files already
  exceed the universal 400-line limit before this change.
- Case names reach filesystem joins without a closed single-component contract.
  Cleanup and processor pruning do not have a shared root-confinement object.
- Completed checkpoints carry result rows but are not bound to the selecting
  source/package/config/input/tool identity.
- Deckhand runs the workflow from the scope checkout, inherits operator-provided
  systemd environment, and returns allowlisted small files from the input-local
  results/ directory. Host configuration and queue transport remain separate
  authority surfaces.

### Reproduction evidence

At base SHA 2ff0f72c9c5ce9022bfca763a6bb24ae4fb768d4, a synthetic input under
scope-repo/cases/openfoam-run-batch reproduces the placement:

~~~text
work_parent_is_config=True
output_parent_is_config=True
inside_config=True
rc=0
~~~

The reproduction executes the exact path-resolution helpers extracted from the
source AST. The isolated planning worktree cannot resolve its sibling editable
dependency, so the implementation verification will run from a correctly linked
checkout and will not treat the AST harness as a regression suite.

### Related issues and landing order

The shared contracts will land in this order:

1. [#1565](https://github.com/vamseeachanta/digitalmodel/issues/1565) will own
   RunIdentity v1, work-layout v1, namespace ownership/locking, and result-policy
   v1.
2. [#1575](https://github.com/vamseeachanta/digitalmodel/issues/1575) will pin the
   merged #1565 SHA and will add normalized case-definition bytes to the shared
   RunIdentity referenced-input/config surfaces.
3. [#1576](https://github.com/vamseeachanta/digitalmodel/issues/1576) will pin the
   merged #1565 and #1575 SHAs, consume RunIdentity v1 unchanged, and register
   artifact_index.json through result-policy v1.

No downstream plan will duplicate fingerprint, checkpoint, work-root, or result
allowlist logic. Any incompatible shared-schema change will require a version
increment and an upstream issue rather than an inline downstream fork.

Related [deckhand#557](https://github.com/vamseeachanta/deckhand/issues/557)
owns the OpenFOAM execution-host agent. #1565 will not mutate its systemd
environment, create host directories, restart the agent, or dispatch a run
without a separate owner-approved transaction.

### Resource and privacy intelligence

- The OrcaFlex batch workflow has the same config-directory default but no
  reviewed external-root contract and will remain unchanged.
- No engineering standard or calculation constant applies.
- Fixtures and reports will remain synthetic. Public output will contain no host
  path, username, mount root, client/project identifier, or private case data.
- The plan will not move or delete any existing legacy batch_runs/ tree.

---

## Artifact Map

| Artifact | Path |
|---|---|
| Plan | docs/plans/2026-07-13-issue-1565-openfoam-external-work-root.md |
| Consolidated r1 review | scripts/review/results/2026-07-13-plan-1565-r1-consolidated.md |
| Public workflow facade | src/digitalmodel/workflows/openfoam_run_batch.py |
| Configuration and identity | src/digitalmodel/workflows/openfoam_batch_config.py |
| Work layout and locks | src/digitalmodel/workflows/openfoam_batch_layout.py |
| Pool/MPI orchestration | src/digitalmodel/workflows/openfoam_batch_execution.py |
| Checkpoint/result policy | src/digitalmodel/workflows/openfoam_batch_results.py |
| Base configuration | src/digitalmodel/base_configs/modules/openfoam_run_batch/openfoam_run_batch.yml |
| Legacy/facade tests | tests/workflows/test_openfoam_run_batch.py |
| Identity tests | tests/workflows/test_openfoam_batch_identity.py |
| Layout/lock tests | tests/workflows/test_openfoam_batch_layout.py |
| Result-policy tests | tests/workflows/test_openfoam_batch_results.py |
| Structural limits | tests/workflows/test_openfoam_batch_structure.py |
| Baseline comparator | scripts/testing/compare_pytest_json.py; tests/scripts/test_compare_pytest_json.py |

## Deliverable

openfoam_run_batch will place heavy case work in an operator-authorized external
root without moving input-local bounded results. A shared RunIdentity will bind
checkpoint and namespace reuse to clean source/package/config/input/tool
identity. Atomic ownership markers and locks will prevent concurrent or foreign
mutation. Result-policy v1 will retain the two-file base contract while exposing
a closed extension registry for #1576.

---

## Normative Design Contract

### 1. Execution context and root authority

The parser will classify the invocation before resolving a work root. Hosted
classification will come only from operator environment, never request YAML:

~~~text
DIGITALMODEL_EXECUTION_CONTEXT=hosted-deckhand
DIGITALMODEL_WORK_ROOT=<operator-private absolute root>
~~~

Hosted Deckhand mode will require a non-empty DIGITALMODEL_WORK_ROOT inherited
from the operator-controlled agent environment. Request YAML will never select
an absolute host root in hosted mode. run_batch.work_root_namespace will be the
only hosted YAML placement field; its packaged default will be
openfoam-run-batch. It will be a non-empty portable relative path
with no absolute form, empty/dot/dot-dot component, separator ambiguity,
control/NUL byte, or symlink traversal. A missing/invalid operator root will fail
before directory creation, cleanup, checkpoint reads, threads, or commands.
An input-supplied execution_context that conflicts with the operator environment
will reject and cannot downgrade hosted execution.

Trusted-local mode will be opt-in through run_batch.execution_context:
trusted-local, and will be available only when the hosted environment marker is
absent. Only that explicit context may use run_batch.work_root as an absolute
precreated directory. The root will be canonicalized, required to be a writable
directory outside the input Git checkout, and rejected when it or any descendant
component is a symlink. Relative or implicit trusted-local roots will not be
accepted.

When neither external mode is selected nor a hosted context is asserted, legacy
work_dir resolution will remain byte-compatible. output_dir will remain
independently based at _config_dir_path so Deckhand can return small results.

In either external mode, work_dir will be a non-empty safe relative path. Case
names will be unique non-empty portable single components. Absolute paths,
separator variants, dot components, controls/NULs, duplicate names, or a
resolved escape will reject for the complete batch before any filesystem or
worker side effect.

The operator root value and canonical host path will remain process-internal.
CSV, JSON, checkpoints, exceptions, log messages, stdout, and stderr will expose
only stable relative work references and non-sensitive source labels.

### 2. RunIdentity v1

src/digitalmodel/workflows/openfoam_batch_config.py will own canonical
digitalmodel-run-identity-v1. Canonical JSON will use UTF-8, sorted object keys,
compact separators, integers without Boolean coercion, finite JSON numbers, and
exact schema validation. Non-JSON input will reject rather than use repr().

The identity payload will contain:

~~~text
schema_version = 1
identity_kind = openfoam-run-batch
source:
  git_commit_sha
  tracked_tree_clean = true
  package_name
  package_version
  package_content_sha256
effective_config_sha256
referenced_inputs[]:
  role
  safe_repo_relative_path
  size_bytes
  content_sha256
selected_executables[]:
  role
  basename
  content_sha256
result_policy_version
work_layout_version
identity_sha256
~~~

The source checkout will be pinned to HEAD with no staged or tracked working-tree
changes and no untracked files under src/, pyproject.toml, uv.lock, packaged base
config, or referenced-input paths. Unrelated legacy runtime residue will not
alter source identity, but it will never be read as an input. package_content_sha256
will cover the installed package RECORD when wheel-installed or the exact
tracked source/package file set when source-installed.

Referenced inputs will include the top-level input bytes and every YAML/CSV/file
used to form the case matrix. #1575 will add its canonical case definition and
prebuilt-manifest evidence through this same typed collection. Paths will be
repository-relative public locators; absolute host paths will not enter the
payload.

Selected executable identity will cover every executable in the validated argv
plan, including nested MPI solver execution. It will record basenames and byte
hashes, not resolved paths. Hashing and preflight will finish before work-tree
mutation. #1576 will consume this list and revalidate executable/content identity
around real execution; it will not define another tool hash.

identity_sha256 will hash the canonical payload excluding identity_sha256 itself.
The full lowercase SHA-256 will name the run namespace and will bind every
external-mode checkpoint. Any source, package, effective config, referenced
input, executable, result policy, or layout change will select a new identity.

### 3. Work layout, ownership marker, and locks

External mode will use:

~~~text
<operator-root>/<validated-namespace>/openfoam-run-<identity-sha256>/
  .digitalmodel-run-owner.json
  .locks/
  <relative-work-dir>/<case-name>/
~~~

The run directory will be created as a strict descendant of the canonical
operator root. The ownership marker will be created with exclusive no-follow
semantics, restrictive permissions, file fsync, and parent-directory fsync. It
will contain only schema version, RunIdentity, layout version, and a random
ownership token. An existing directory without a valid marker, with a symlinked
marker, or with a mismatched identity will reject without cleanup or adoption.

Before checkpoint inspection or mutation, the process will acquire an exclusive
run lock. Each worker will also acquire an exclusive case lock before reading,
cleaning, building, solving, or checkpointing its case. Lock records will contain
schema version, identity, case name when applicable, random token, PID, local
boot ID, process-start token, creation time, and heartbeat time. These records
will remain private host-local state. Release will unlink only a token-matching
record.

Contention will wait only for the configured bounded lock timeout, then fail
without mutation. Automatic stale reclamation will require all of:

1. heartbeat age above the configured stale threshold;
2. the boot ID/process-start/PID tuple proves the recorded process is not live
   or belongs to a prior boot;
3. identity and owner marker still match;
4. the claimant atomically renames the stale record to a tokenized tombstone;
5. containment and no-symlink checks pass again immediately before replacement.

Unknown liveness, a live PID, marker drift, or rename loss will never reclaim.
Tombstones will remain bounded diagnostic records until a later successful
owner-matched cleanup; #1565 will not delete unrelated lock files.

Every destructive helper will accept a validated WorkLayout rather than a raw
Path. It will revalidate owner token, canonical containment, file type, and
no-symlink ancestry immediately before mutation. The target must be a strict
case descendant and must not equal the operator root, namespace, run root,
config directory, or Git root.

### 4. Checkpoint reuse

External checkpoints will use schema version 2 and will carry the exact
RunIdentity plus owner token, case name, status, and bounded result row. Reuse
will require a completed status and exact schema/identity/owner/case match.
Missing, corrupt, legacy, mismatched, or foreign checkpoints will never skip
execution.

A rejected checkpoint will trigger a clean rerun only after the current process
holds both valid locks and revalidates the ownership marker. Legacy mode will
retain the existing checkpoint shape and semantics until a separately reviewed
migration.

### 5. Versioned result policy

result-policy-v1 will define two mandatory base files:

~~~text
cases.csv
batch_summary.json
~~~

Only registered code-owned producers may extend the set. YAML paths, globs,
suffix-only discovery, and arbitrary copying will not register a producer. Each
extension will declare an immutable extension ID, exact relative basename,
schema validator, media type, maximum bytes, and policy version.

#1576 will register openfoam-artifact-index-v1 for artifact_index.json. It will
consume result-policy-v1 and RunIdentity v1; it will not modify the two-file base
or add a second allowlist. Unknown IDs, duplicate basenames, unsupported policy
versions, producer failures, or validation failures will reject publication.

All result files will remain beneath input-local results/. Heavy case trees,
checkpoints, solver logs, VTK, mesh/field trees, and processor directories will
remain beneath the external namespace.

### 6. Deckhand external-state gate and canary

Code merge will not authorize host mutation. Before hosted activation, the
owner will receive a transaction preview containing:

- the execution-context and root environment keys to add, service/drop-in files
  affected, directory ownership/mode requirements, restart command, rollback,
  and non-sensitive verification commands;
- confirmation that no root value will be committed or echoed;
- the exact agent/source SHA and digitalmodel SHA to activate.

Only after explicit owner approval may an operator create the directory, update
the environment, and restart the agent. Readback will prove, without printing
the value, that the running process inherited a non-empty root, the canonical
root is outside the scope checkout, owner/mode/writability are valid, and the
agent reports the intended code SHA.

The owner-approved synthetic dispatch canary will then prove:

1. two mock cases create case trees, ownership marker, locks/checkpoints, and no
   heavy data beneath the scope checkout;
2. cases.csv and batch_summary.json return through the existing Deckhand result
   policy and no other file returns;
3. stdout/stderr tails, result JSON, returned files, and logs contain neither the
   canonical root bytes nor an absolute heavy-work path;
4. retry reuses the exact identity while a one-byte referenced-input change
   selects a new namespace;
5. the canary performs no real solver run or client-data access.

The canary result will be evidence for activation, not authorization to run a
production CFD batch.

---

## File Decomposition and Limits

The refactor will precede behavior changes and will preserve characterization
tests:

| File | Responsibility | Target |
|---|---|---|
| openfoam_run_batch.py | public constants, compatibility imports, router facade | <=200 lines; router <=50 |
| openfoam_batch_config.py | exact config parsing, RunIdentity, selected-input/tool identity | <=350 lines |
| openfoam_batch_layout.py | WorkLayout, ownership marker, locks, containment | <=350 lines |
| openfoam_batch_execution.py | pool/MPI orchestration using validated objects | <=350 lines |
| openfoam_batch_results.py | checkpoint v2, rows, result-policy registry/publication | <=350 lines |
| test_openfoam_run_batch.py | facade and legacy compatibility only | <=300 lines |
| test_openfoam_batch_identity.py | identity codecs/change matrix | <=350 lines |
| test_openfoam_batch_layout.py | root/marker/lock/concurrency/destruction | <=350 lines |
| test_openfoam_batch_results.py | checkpoint and result-policy contracts | <=350 lines |
| test_openfoam_batch_structure.py | file/function-size enforcement | <=200 lines |

Every touched Python file will be at most 400 lines and every function/method at
most 50 physical lines. The structural test will parse the named touched-file
manifest and AST; it will fail when a new touched Python path is omitted.

---

## TDD Test List

Tests will be committed RED before each behavior slice. The decomposition slice
will begin with GREEN characterization tests and will make no behavior change.

| Test | Contract |
|---|---|
| test_hosted_mode_requires_operator_environment_root | Hosted input cannot fall back to YAML or checkout-local heavy work |
| test_yaml_cannot_downgrade_operator_hosted_context | Request config cannot claim trusted-local under hosted policy |
| test_hosted_yaml_accepts_only_relative_namespace | Absolute/traversal/control/symlink forms reject pre-side-effect |
| test_trusted_local_absolute_root_requires_explicit_context | Absolute config authority is never implicit |
| test_external_work_dir_and_case_names_are_safe_unique_components | Whole matrix rejects escape/duplicates before side effects |
| test_operator_root_never_appears_in_outputs_or_captured_streams | CSV/JSON/checkpoints/errors/log/stdout/stderr redact host paths |
| test_run_identity_canonical_golden_vector | Exact codec and SHA are stable |
| test_identity_changes_for_each_bound_surface | Source/package/config/input/executable/policy/layout changes isolate runs |
| test_identity_rejects_dirty_or_unbound_source | Modified governed source/input cannot claim clean identity |
| test_referenced_input_bytes_are_complete | Every selected YAML/CSV/file contributes size/hash/role |
| test_mpi_nested_solver_is_in_executable_identity | Selected-plan hashing cannot omit the solver under mpirun |
| test_owner_marker_create_and_exact_reopen | Atomic marker permits exact retry only |
| test_foreign_missing_symlinked_marker_rejects | Existing unowned/unsafe namespaces are never adopted |
| test_concurrent_identical_runs_have_one_owner | Two processes cannot clean/build the same namespace |
| test_case_lock_serializes_duplicate_external_access | Case mutation requires token-matched ownership |
| test_live_unknown_and_recent_locks_never_reclaim | Stale policy fails closed |
| test_dead_expired_lock_reclaim_is_atomic | Only one claimant wins tombstone/replacement |
| test_cleanup_revalidates_owner_and_containment | Marker/path drift blocks deletion |
| test_checkpoint_v2_requires_exact_identity_owner_case | Foreign/stale checkpoint cannot skip |
| test_legacy_no_override_is_byte_compatible | Existing default paths/rows/checkpoints remain unchanged |
| test_result_policy_v1_requires_two_base_files | Base return contract is exact |
| test_result_extension_registry_is_closed_and_versioned | YAML/glob/unknown/duplicate producers reject |
| test_artifact_index_extension_contract_is_reserved_for_1576 | Shared seam accepts the exact future registration without publishing it now |
| test_deckhand_canary_external_work_and_bounded_return | Owner-approved dispatch proves the end-to-end boundary |
| test_touched_python_files_are_within_400_lines | Named manifest and physical line cap hold |
| test_touched_functions_are_within_50_lines | AST function/method cap holds |
| test_compare_pytest_json_rejects_new_failure | Baseline comparator is fail-closed and deterministic |

---

## Implementation Sequence

1. Verify issue/approval state, parallel work, exact base SHA, and merged dependency
   state. Stop unless the user has approved this revised plan.
2. Capture focused and full same-base test reports before edits.
3. Add/retain GREEN characterization tests, then split the workflow/router/tests
   into the named modules. Run focused tests after each file move.
4. Add RED RunIdentity codec, completeness, dirty-source, and change-isolation
   tests; implement the shared v1 identity.
5. Add RED hosted/trusted-local authority, owner-marker, locking, stale-reclaim,
   concurrency, containment, and redaction tests; implement WorkLayout.
6. Add RED checkpoint-v2 and result-policy tests; implement the closed base and
   extension registry.
7. Integrate the validated objects into pool/MPI execution without changing
   legacy no-override behavior.
8. Run focused/full/baseline, lint, compile, size, legal, and privacy commands.
9. Run T3 adversarial code/artifact review. Resolve every MAJOR.
10. Prepare the Deckhand host-config transaction preview. Stop for separate owner
    approval before any directory, environment, service, or dispatch change.
11. After approval, perform readback and the synthetic canary, then post only
    non-sensitive evidence. Production execution remains out of scope.

---

## Exact Verification Commands

Focused:

~~~bash
PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 PYTHONPATH=src uv run python -m pytest -q tests/workflows/test_openfoam_run_batch.py tests/workflows/test_openfoam_batch_identity.py tests/workflows/test_openfoam_batch_layout.py tests/workflows/test_openfoam_batch_results.py tests/workflows/test_openfoam_batch_structure.py tests/workflows/test_orcaflex_run_batch.py tests/scripts/test_compare_pytest_json.py
~~~

Canonical full suite:

~~~bash
PYTHONPATH=src uv run python -m pytest -q
~~~

Paired same-base no-new-failure oracle, with VERIFY_PARENT set to a directory
whose sibling assetutilities checkout is the same for both worktrees:

~~~bash
test -n "$VERIFY_PARENT"
test -d "$VERIFY_PARENT/assetutilities"
git worktree add --detach "$VERIFY_PARENT/dm-1565-base" 2ff0f72c9c5ce9022bfca763a6bb24ae4fb768d4
git worktree add --detach "$VERIFY_PARENT/dm-1565-candidate" HEAD
(cd "$VERIFY_PARENT/dm-1565-base" && uv sync --locked && PYTHONPATH=src uv run python -m pytest -p no:randomly --json-report --json-report-file="$VERIFY_PARENT/1565-base.json")
(cd "$VERIFY_PARENT/dm-1565-candidate" && uv sync --locked && PYTHONPATH=src uv run python -m pytest -p no:randomly --json-report --json-report-file="$VERIFY_PARENT/1565-candidate.json")
PYTHONPATH=src uv run python scripts/testing/compare_pytest_json.py --baseline "$VERIFY_PARENT/1565-base.json" --candidate "$VERIFY_PARENT/1565-candidate.json" --require-candidate-failures-subset
~~~

The comparator will normalize collection phase, node ID, exception type, and
final exception message while stripping only the two verification-root prefixes.
Candidate failures must be a subset of base failures; every touched-scope failure,
new collection error, missing report, or malformed record will block.

Size, lint, and compile:

~~~bash
PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 PYTHONPATH=src uv run python -m pytest -q tests/workflows/test_openfoam_batch_structure.py::test_touched_python_files_are_within_400_lines tests/workflows/test_openfoam_batch_structure.py::test_touched_functions_are_within_50_lines
uv run ruff check src/digitalmodel/workflows/openfoam_run_batch.py src/digitalmodel/workflows/openfoam_batch_config.py src/digitalmodel/workflows/openfoam_batch_layout.py src/digitalmodel/workflows/openfoam_batch_execution.py src/digitalmodel/workflows/openfoam_batch_results.py tests/workflows/test_openfoam_run_batch.py tests/workflows/test_openfoam_batch_identity.py tests/workflows/test_openfoam_batch_layout.py tests/workflows/test_openfoam_batch_results.py tests/workflows/test_openfoam_batch_structure.py scripts/testing/compare_pytest_json.py tests/scripts/test_compare_pytest_json.py
uv run python -m compileall -q src/digitalmodel/workflows/openfoam_run_batch.py src/digitalmodel/workflows/openfoam_batch_config.py src/digitalmodel/workflows/openfoam_batch_layout.py src/digitalmodel/workflows/openfoam_batch_execution.py src/digitalmodel/workflows/openfoam_batch_results.py scripts/testing/compare_pytest_json.py
git diff --check
~~~

Cross-repository legal scan, after the reviewed candidate SHA is checked out at
the workspace-hub canonical digitalmodel sibling:

~~~bash
test -n "$WORKSPACE_HUB_ROOT"
test "$(git -C "$WORKSPACE_HUB_ROOT/digitalmodel" rev-parse HEAD)" = "$(git rev-parse HEAD)"
(cd "$WORKSPACE_HUB_ROOT" && bash scripts/legal/legal-sanity-scan.sh --repo=digitalmodel --diff-only)
~~~

Any command failure blocks closeout. Test reports, hashes, and the normalized
base/candidate failure delta will be attached as non-sensitive review evidence.

---

## Acceptance Criteria

- [ ] Hosted mode accepts an operator environment root only; YAML supplies only
      a validated relative namespace and cannot select an absolute host path.
- [ ] Trusted-local absolute config is available only through explicit opt-in.
- [ ] RunIdentity v1 binds clean source SHA/package, effective config, every
      referenced input byte set, selected executable hashes, and schema versions.
- [ ] #1575 and #1576 consume the shared identity/layout/result schemas in the
      declared landing order and pin merged predecessor SHAs.
- [ ] Ownership marker plus exclusive run/case locks prevent concurrent/foreign
      mutation; stale reclamation satisfies the full fail-closed policy.
- [ ] Checkpoint v2 reuses only exact completed identity/owner/case matches.
- [ ] result-policy-v1 preserves cases.csv and batch_summary.json and exposes only
      the closed artifact-index registration seam for #1576.
- [ ] Legacy no-override behavior remains compatible.
- [ ] The workflow facade is <=200 lines, router <=50, and every touched Python
      file/function satisfies 400/50.
- [ ] Focused, canonical full, paired same-base, lint, compile, diff, and legal
      commands pass; candidate failures are a subset of the exact base failures.
- [ ] A separate owner-approved Deckhand preview/readback precedes activation.
- [ ] The synthetic dispatch canary proves external heavy residency, bounded
      result return, identity isolation/retry, and stdout/stderr/path redaction.
- [ ] No private data, real solver run, host path publication, legacy-tree
      deletion, self-approval, self-merge, self-close, or production promotion
      occurs.
- [ ] T3 code/artifact review reaches no-MAJOR before implementation closeout and
      the issue receives the required implementation summary comment.

---

## Adversarial Review Summary

R1 returned MAJOR across the supplied Claude/Codex/Gemini review wave. The
consolidated artifact records the provider themes and exact r2 dispositions.

This revision incorporates the r1 blockers: hosted root authority, owner-approved
Deckhand preview/readback and canary, atomic ownership/locking, shared complete
RunIdentity, landing order, versioned result extensions, explicit module/test
decomposition, literal verification commands, and conservative status wording.

**Overall result:** revised draft for r2 adversarial review. It is not approved,
not approval-ready until r2 returns no MAJOR, and does not authorize
implementation or external-state changes.

---

## Risks and Open Decisions

- A host root is an external-state capability. Code merge and operator activation
  will remain separate approvals.
- Source/tool identity hashing adds startup cost but prevents false checkpoint
  reuse; #1576 may optimize caching only while preserving exact revalidation.
- Lock reclamation can destroy active work if liveness is guessed. Unknown
  liveness therefore blocks automatic reclaim.
- Full-suite baseline debt may remain on the exact base SHA. The machine-readable
  subset oracle will distinguish inherited failures from candidate regressions.
- Result extensions can become a covert queue bypass if callers register arbitrary
  paths. Registration will remain code-owned, exact-name, schema-validated, and
  versioned.

## Complexity: T3

The revision spans filesystem authority, concurrent destructive operations,
checkpoint identity, cross-issue shared schemas, Deckhand external state, queue
publication, a mandatory large-file decomposition, and paired regression
evidence.
