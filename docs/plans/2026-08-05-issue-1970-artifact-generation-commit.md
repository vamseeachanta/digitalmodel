# Plan for #1970: give the artifact layer consumers, or remove it

> **Status:** plan-review (awaiting owner approval — never self-approved)
> **Complexity:** T3
> **Date:** 2026-08-05
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1970
> **Client:** N/A
> **Lane:** domain:cfd
> **Branch:** `plan/1968-1970-openfoam-followons` (worktree off `origin/main` @ `7b4119cc`)
> **Review artifacts:** r1 Claude — inline, main session (this document, see Adversarial Review Summary)

---

## Premise verification (2026-08-05, against `origin/main` @ `7b4119cc`)

Every claim in the issue was re-checked. All confirmed — and the problem is
**substantially wider** than filed, in a way that changes which options are worth
considering.

| # | Claim as filed | Verdict | Evidence |
|---|---|---|---|
| 1 | `artifact_index.py:16-17` defines `GENERATION_DOMAIN` and `COMMIT_DOMAIN` | **CONFIRMED** | verbatim: `GENERATION_DOMAIN = b"dm-generation-id-v1"`, `COMMIT_DOMAIN = b"dm-commit-v1"` |
| 2 | `git grep` across `origin/main` returns only those two definition lines | **CONFIRMED** | reproduced exactly; no consumers, no tests, no callers |
| 3 | The #1576 plan `:261` names `artifact_generation.py`, `:263` names its test | **CONFIRMED** | at `ba9366da8425ea6d0508eb3643c2ac985065e2dd`, verbatim |
| 4 | Neither file exists on `main` | **CONFIRMED** | `git ls-tree -r origin/main` → no match for `artifact_gener` |
| 5 | Out of scope: "`artifact_index.py` itself, which shipped **and is exercised**" | **CONFIRMED but materially incomplete** | It is exercised **only by its own unit tests**. It has **zero production consumers** |

### The unbacked promise is the whole module, not two constants

```text
$ git grep -n "artifact_index" origin/main | grep -v scripts/review/results
tests/solvers/openfoam/test_artifact_index.py:10:from digitalmodel.solvers.openfoam.artifact_index import (
tests/solvers/openfoam/test_module_size_limits.py:36:    "src/digitalmodel/solvers/openfoam/artifact_index.py",
```

One test file imports it. One test file lists its path for a size check. **No
module under `src/` imports `artifact_index` at all.** `build_index` — the
module's top-level entry point — has no caller anywhere in the repository.

PR [#1967](https://github.com/vamseeachanta/digitalmodel/pull/1967) created
`artifact_index.py` and modified `openfoam_batch_execution.py` in the same
change, but never connected them:

```text
$ git show origin/main:src/digitalmodel/workflows/openfoam_batch_execution.py \
    | grep -n "generation\|commit\|current.json\|retain\|artifact"
# no output
```

The merge commit is titled *"schedule VTK on the MPI path and index retained
artifacts"*. The VTK half shipped. **The indexing half shipped as a library that
nothing calls.** So the two unconsumed constants are the visible edge of a
~350-line unwired layer, and a decision scoped to the constants would leave the
larger promise standing.

### A second, sharper finding: `generation_id` is a required parameter with no producer

`artifact_id` (`:126-149`) and `build_index` (`:330-352`) both take
`generation_id: str` as a required keyword. The value is folded into a hash. So
the design does not merely *anticipate* generation identifiers — it **requires**
them, and there is no canonical way to construct one. Any two call sites would
invent their own format, and `GENERATION_DOMAIN` — the tag that exists precisely
to make that construction canonical — is the thing nobody can reach.

### Domain-tag coverage is uneven

`test_artifact_index.py:10-23` imports `TREE_DOMAIN` only, and pins one golden
vector at `:59` (`sha256(frame(TREE_DOMAIN))`). `ARTIFACT_ID_DOMAIN` is used by
production code but **has no golden vector**. `GENERATION_DOMAIN` and
`COMMIT_DOMAIN` have neither. Nothing anywhere asserts the four tags are
distinct — which is the entire point of a domain-separation tag.

### One refinement of wording

There is no `__all__` in `artifact_index.py`. The constants are module-level
public names, not declared exports. The issue's "exported" is fair in substance;
noted so nobody searches for an export list that does not exist.

---

## Deliverable

The artifact layer will stop being an unbacked promise. Depending on the owner's
decision at D2, `artifact_generation.py` will be implemented **and wired into the
batch execution path** so that retained artifacts are actually indexed under a
committed generation, or the unwired layer will be removed in full.

---

## Resource Intelligence Summary

### Existing repo code

- `src/digitalmodel/solvers/openfoam/artifact_index.py`: `frame:44`,
  `tree_digest:112`, `artifact_id:126`, `host_local_locator:152`,
  `is_numeric_time_name:164`, `snapshot_tree:224`, `verify_unchanged:237`,
  `select_roots:273`, `build_index:330`. Domain tags at `:14-17`.
  `ARTIFACT_KINDS` at `:19-24`.
- `src/digitalmodel/workflows/openfoam_batch_execution.py` (**379 lines**):
  the MPI/pool/serial execution path. Prunes `processor*` on success at
  `:203-204`. No artifact awareness of any kind.
- `src/digitalmodel/workflows/openfoam_batch_identity.py:41-64`:
  `build_run_identity`, producing the `run_identity_sha256` that `build_index`
  requires.
- `src/digitalmodel/workflows/openfoam_batch_layout.py:46`: run dir as
  `openfoam-run-{identity_sha256}`; `prune_processors:99-111`.
- `tests/solvers/openfoam/test_module_size_limits.py:35+`: `GOVERNED_MODULES`,
  explicitly enumerated so that "adding a module to the package is a deliberate
  decision to bring it under this limit".

### Gaps identified

- No `artifact_generation.py` exists; no staging, diagnostic generation, commit
  manifest, or current pointer exists.
- No production call site for `build_index` exists.
- No canonical construction for `generation_id` exists, though two functions require one.
- No golden vector pins `ARTIFACT_ID_DOMAIN`, `GENERATION_DOMAIN`, or `COMMIT_DOMAIN`.
- No test asserts the four domain tags are pairwise distinct.
- No identity revalidation before commit exists.

### Evidence

**`build_index` has no caller** (2026-08-05):

```text
$ git grep -n "build_index" origin/main -- src
src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py:95
src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py:260
# both are _build_indexes on an unrelated pipe database client
```

**Standards and reusable-contract retrieval returned nothing.** Consistent with
the #1576 plan's own finding — "Registries, wiki, standards, and the required
Drive query supplied no reusable artifact-generation contract; no
standards-derived constant applies." Re-checked and recorded again as a null
result.

---

## Design decisions

**D1 — The decision is about the layer, not the constants.**
The issue frames a binary on two constants. Premise 5 shows that removing just
the constants would leave a ~350-line module with no caller — a smaller version
of the same defect, wearing a closed issue as cover. Every option below is stated
at layer scope.

**D2 — Ranked options.**

| | Option | Cost | Assessment |
|---|---|---|---|
| A | **Implement `artifact_generation.py` and wire the layer into `openfoam_batch_execution.py`** | new module, module split, workflow integration, ~12 tests | Delivers what #1576 promised. Retained artifacts become addressable under a committed generation. T3. |
| B | **Remove the unwired layer entirely** — the two constants, `artifact_index.py`, and `test_artifact_index.py` | one deletion commit | Honest and cheap. The repo returns to a true state, and `git` retains the work for the day it is wanted. **A real option, not a rhetorical one.** |
| C | Remove only `GENERATION_DOMAIN` and `COMMIT_DOMAIN` | two lines | **Should not be chosen.** It resolves the filed symptom while leaving the larger unbacked promise in place, and closes the issue that would otherwise surface it. Named explicitly so it is rejected deliberately rather than drifted into. |

**The choice between A and B is a roadmap question this plan cannot answer from
the repository.** The honest test is: *is host-local retention and indexing of
heavy CFD artifacts wanted within a release or two?* If yes, A. If it is
speculative, B — and B is genuinely better than leaving the layer sitting there,
because an unwired module accumulates readers who assume it works.

The one recorded signal is that identity revalidation before commit was flagged
at #1576's close as belonging to this slice, which suggests intent. Intent
recorded at close is not demand, and this plan does not treat it as approval.

**D3 — If A: the generation lifecycle, stated as a contract.**
A generation is a directory under the run root holding one attempt's retained
artifacts. The lifecycle is:

1. **Stage** — artifacts are written into `generations/<generation_id>/` while the
   run proceeds. Nothing outside the generation is touched.
2. **Revalidate identity** — immediately before commit, the run identity is
   recomputed and compared to the one the generation was opened with. A mismatch
   fails the generation. This is the item #1576's close assigned to this slice.
3. **Snapshot and verify** — `snapshot_tree` then `verify_unchanged`
   (`artifact_index.py:224,237`) prove the tree did not move under the commit.
4. **Commit** — a commit manifest is written into the generation, then
   `current.json` is replaced by atomic same-directory rename.
5. **Diagnostic generations** — a failed generation retains its contents, carries
   `state: failed`, and is never selected by `current.json`.

**D4 — Identifier construction, and how it is pinned.**
`generation_id = sha256(frame(GENERATION_DOMAIN) || framed fields…)` and the
commit identifier uses `COMMIT_DOMAIN` in the same shape, reusing the existing
`frame` codec (`:44-49`) rather than introducing a second one. The exact field
list will be fixed in implementation and **the resulting digests will be pinned
as literal hex golden vectors in the test file**, following the existing pattern
at `test_artifact_index.py:59`.

**No hash literal appears in this plan.** Writing one here would mean inventing
it, and a fabricated golden vector that a test is then written to match is worse
than none — it looks like verification and is not. The vectors are computed once
at implementation and pinned thereafter.

**D5 — Crash-consistency is testable without crashing anything.**
"Crash tests" cannot mean killing a real process at a chosen instruction. The
testable invariant is: **`current.json` names either the previous generation or
the new one, never a partial state**, under a failure injected at each step of
D3. Each step gets a fault-injection test asserting that invariant plus the
retention of the failed generation.

**D6 — The same-device check is asserted on `st_dev`, not on a real mount.**
Atomic replacement by rename requires source and destination on one filesystem.
The check compares `os.stat().st_dev` of the staging directory and the run root
and fails closed when they differ. **The test fakes `st_dev`** rather than
requiring a second mounted filesystem — no CI or dev host here can be assumed to
provide one, and a criterion needing a real cross-device mount would be
unrunnable rather than merely awkward.

**D7 — Domain-tag divergence is closed by pinning, and this is the item that
survives every option.**
The hazard the issue names — a second implementation defining its own tags and
diverging silently — is closed by a test that pins all four tags as literal byte
strings and asserts they are pairwise distinct. That test costs almost nothing
and is worth having under A. Under B it disappears with the module.

**D8 — `openfoam_batch_execution.py` cannot absorb this, and cannot absorb
[#1968](https://github.com/vamseeachanta/digitalmodel/issues/1968) either.**
The file is 379 of its permitted 400 lines. Option A's integration and #1968's
decomposition gate both need call sites in it, and **the two plans collide there**.
Artifact staging and commit move to a new
`src/digitalmodel/workflows/openfoam_batch_artifacts.py`, leaving
`openfoam_batch_execution.py` with a thin call. Whichever of the two issues is
approved second inherits a smaller file, not a conflict — but if both are
approved, they should not be implemented concurrently in the same file.

---

## Files to Change

**Option A:**

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/solvers/openfoam/artifact_generation.py` | D3 lifecycle: staging, diagnostic generation, commit manifest, current pointer; D4 identifiers |
| Create | `src/digitalmodel/workflows/openfoam_batch_artifacts.py` | D8 split: the workflow-side integration, keeping `openfoam_batch_execution.py` under 400 |
| Modify | `src/digitalmodel/workflows/openfoam_batch_execution.py` | open a generation around the run; commit on success; retain as diagnostic on failure; ordered before the `processor*` prune at `:203-204` |
| Create | `tests/solvers/openfoam/test_artifact_generation.py` | D5 fault injection, D6 same-device, commit/pointer, D4 golden vectors |
| Modify | `tests/solvers/openfoam/test_artifact_index.py` | D7 domain-tag pinning and pairwise distinctness |
| Modify | `tests/workflows/test_openfoam_batch_execution.py` | the run path produces a committed generation |
| Modify | `tests/solvers/openfoam/test_module_size_limits.py` | add the two new modules to `GOVERNED_MODULES:35+`, per its own "deliberate decision" comment |
| Update | `docs/plans/README.md` | index row |

**Option B:**

| Action | Path | Reason |
|---|---|---|
| Delete | `src/digitalmodel/solvers/openfoam/artifact_index.py`; `tests/solvers/openfoam/test_artifact_index.py` | remove the unwired layer in full |
| Modify | `tests/solvers/openfoam/test_module_size_limits.py` | drop the `:36` entry |
| Update | `docs/plans/README.md` | index row |

**Explicitly untouched**, to stay clear of live lanes:
`src/digitalmodel/solvers/openfoam/solver_contracts.py`, `templates/`,
`case_builder.py`, `models.py` (owned by
[#1959](https://github.com/vamseeachanta/digitalmodel/issues/1959));
`src/digitalmodel/hydrodynamics/diffraction/`; capability-page producers (owned
by [#1965](https://github.com/vamseeachanta/digitalmodel/issues/1965));
`case_definition.py` and `prebuilt_mesh.py` (owned by
[#1969](https://github.com/vamseeachanta/digitalmodel/issues/1969));
`_prepare_mpi_case` and `mpi_command_plan` (owned by
[#1968](https://github.com/vamseeachanta/digitalmodel/issues/1968)); every file
on `chore/1576-mpi-artifact-plan`.

---

## TDD Test List

Every row states the expected value and why it is red on `origin/main` @ `7b4119cc`.

**Option A:**

| Test | Input | Expected | Red today because |
|---|---|---|---|
| `test_generation_id_golden_vector` | fixed field values | equals a pinned hex literal computed at implementation | `GENERATION_DOMAIN` has no consumer |
| `test_commit_id_golden_vector` | fixed field values | equals a pinned hex literal | `COMMIT_DOMAIN` has no consumer |
| `test_domain_tags_are_pinned_and_pairwise_distinct` | the four tags | each equals its literal byte string; all four differ | D7; nothing asserts distinctness |
| `test_generation_id_changes_with_every_field` | one field varied at a time | digest differs for each variation | guards a field silently dropped from the hash |
| `test_successful_commit_advances_current_pointer` | staged generation, clean commit | `current.json` names the new generation; manifest present | **anti-vacuity guard** — an implementation that never writes `current.json` passes every failure test below and fails this one |
| `test_failure_before_commit_leaves_pointer_at_previous` | fault injected at stage | `current.json` unchanged; failed generation retained with `state: failed` | D5 |
| `test_failure_during_snapshot_leaves_pointer_at_previous` | fault injected at snapshot | same invariant | D5 |
| `test_failure_during_manifest_write_leaves_pointer_at_previous` | fault injected at manifest write | same invariant | D5 |
| `test_identity_drift_before_commit_fails_the_generation` | run identity changed between open and commit | commit refused; generation retained as failed; pointer unchanged | the item #1576's close assigned to this slice |
| `test_tree_mutation_between_snapshot_and_commit_fails` | file altered after `snapshot_tree` | `verify_unchanged` raises; no commit | reuses `artifact_index.py:237` rather than a second checker |
| `test_cross_device_staging_is_refused` | `st_dev` faked to differ | refused before any rename | D6; no real mount required |
| `test_failed_generation_is_never_selected_by_current` | one failed and one committed generation | `current.json` names only the committed one | #1576's failure contract |
| `test_no_processor_root_enters_a_committed_index` | case with `processor0` present | committed index contains no processor root | `select_roots:273` behaviour, now asserted end-to-end |
| `test_batch_run_produces_a_committed_generation` | full run path, mocked runner | `build_index` is called and `current.json` exists | **the wiring test** — today `build_index` has no caller |

**Option B:**

| Test | Input | Expected | Red today because |
|---|---|---|---|
| `test_artifact_index_module_is_absent` | import attempt | `ModuleNotFoundError` | the module exists |
| `test_no_domain_tag_remains_in_the_tree` | `git grep` over `src/` | no `GENERATION_DOMAIN` or `COMMIT_DOMAIN` match | both are defined |

**Not included, deliberately:** no test that kills a real process mid-write (D5 —
not reproducible; fault injection asserts the same invariant); no test requiring
a second mounted filesystem (D6 — unrunnable on these hosts); no test requiring
an OpenFOAM binary (none exists here); no golden vector recomputed by calling the
function under test (circular — every vector is a hex literal in the test body);
no test asserting a manifest matches a schema the writer renders from the same
structure (circular — manifests are asserted field-by-field against literals).

---

## Execution environment

Implementation runs in a dedicated worktree off `origin/main`
(`/mnt/ace/ws/agent-worktrees/dm-1968-1970-plans`). The shared checkout
`/mnt/ace/ws/digitalmodel` is on `fix/3787-startup-tax` and is **not** used.
Root `/` is at 100% (1.2 G free), so worktrees live on `/mnt/ace`.

Every test is a pure-filesystem fixture under `tmp_path` with injected faults and
a faked `st_dev`. No OpenFOAM binary and no second filesystem is required, and
neither is available here — `which interFoam decomposePar` returns nothing
(2026-08-05).

---

## Acceptance Criteria

- [ ] **Every test above fails on `origin/main` @ `7b4119cc` and passes after.** The red list is captured by running the new files against a clean `origin/main` worktree and recorded in the PR body.
- [ ] **`test_successful_commit_advances_current_pointer` passes** (option A). This criterion exists to make the failure-path criteria non-vacuous: an implementation that never writes `current.json` satisfies every "pointer unchanged" test and fails this one.
- [ ] **`test_batch_run_produces_a_committed_generation` passes** (option A). This is the criterion the whole plan turns on — it is the one that would have failed on [#1967](https://github.com/vamseeachanta/digitalmodel/pull/1967), and its absence is why an unwired module merged under a commit message claiming it indexed things.
- [ ] **`git grep -n "build_index" -- src` returns at least one call site outside `artifact_index.py`** (option A), quoted in the PR body. A library with tests but no caller is the exact state this issue exists to end.
- [ ] **Every golden vector is a hex literal in the test file, computed once at implementation and never recomputed by the function under test.** The PR body states how each was produced.
- [ ] **All four domain tags are pinned to literal byte strings and asserted pairwise distinct.**
- [ ] **`pytest tests/solvers/openfoam/ tests/workflows/ -q` is compared node-ID by node-ID against a baseline captured in the same worktree at the branch point, with no file excluded from either side.** No new failure node IDs. Symmetric exclusion is forbidden: #1575 excluded `test_workflow_router.py` from both baseline and after — textbook-honest — and still hid two regressions only CI caught. **`test_workflow_router.py` is named here and must be run.**
- [ ] Every touched Python file is at most 400 physical lines and every function at most 50. `openfoam_batch_execution.py` is at 379 today; the PR body states its line count after the change, and both new modules are added to `GOVERNED_MODULES`.
- [ ] **No numeric threshold is introduced.** The design has no tuned value: identifiers are hashes, the device check is an equality on `st_dev`, and retention is a state flag. The PR body confirms this explicitly.
- [ ] **No acceptance criterion requires an OpenFOAM installation or a second mounted filesystem**, and this is deliberate — neither exists on any host here, so such a criterion would be unrunnable.
- [ ] **No legal-scan criterion is stated, and its absence is deliberate.** `scripts/legal/legal-sanity-scan.sh` does not exist in this repository (verified 2026-08-05), and workspace-hub's `--repo=` form is **fail-open** under OPEN workspace-hub [#3804](https://github.com/vamseeachanta/workspace-hub/issues/3804) — it resolves an empty scan path, scans nothing, and exits 0, so a green result proves nothing. Relevant here because generation manifests could carry paths: instead of citing that scan, `test_no_processor_root_enters_a_committed_index` and the `host_local_locator` contract (`artifact_index.py:152-162`, which rejects any component containing a separator) are the repo-local guards, and they run.
- [ ] If option A is approved alongside [#1968](https://github.com/vamseeachanta/digitalmodel/issues/1968), the two are **not** implemented concurrently in `openfoam_batch_execution.py` (D8). The PR body names which landed first.
- [ ] T3 plan review has no MAJOR before user approval is requested; T3 code review has no MAJOR before close.
- [ ] r1 review artifact recorded.

---

## Out of scope

- **The MPI resume contradiction** — [#1968](https://github.com/vamseeachanta/digitalmodel/issues/1968).
- **The prebuilt case schema arm** — [#1969](https://github.com/vamseeachanta/digitalmodel/issues/1969).
- **Queue transport, cross-host resolvers, retention daemons, and Deckhand.** The #1576 plan assigns those to #564. `host_local_locator` stays host-local and this plan adds no network surface.
- **Public or Hugging Face run-manifest schemas.** Nothing here publishes.
- **Retention policy** — how long generations live, and what prunes them. This plan commits and points; it does not expire.
- **Changing `artifact_index.py`'s codec, selection rules, or schema** under option A. It is called as-is; only its tests gain domain-tag coverage.
- **Physics, meshing, or solver behaviour.** Nothing here changes what is computed.

---

## Adversarial Review Summary

| Round | Provider | Verdict |
|---|---|---|
| r1 | Claude — inline, main session | **MAJOR** — 6 findings against this plan's own earlier draft, all folded in |

r1 findings:

1. **MAJOR — the first draft scoped the decision to the two constants**, accepting
   the issue's Out-of-scope line that `artifact_index.py` "shipped and is
   exercised". It is exercised only by its own unit tests and has no production
   caller. A plan that gave the constants consumers while leaving `build_index`
   uncalled would have reproduced the defect one level up. Rewritten as D1/D2 at
   layer scope, and option C is named explicitly as the trap.
2. **MAJOR — the draft contained an invented SHA256 golden vector.** It was
   plausible-looking and wrong, and a test written to match it would have
   certified nothing while appearing rigorous. All hash literals removed from the
   plan; D4 now requires vectors be computed at implementation and pinned.
3. **MAJOR — "crash tests" and a cross-device test were unrunnable as written.**
   The draft required killing a process mid-rename and a real second filesystem.
   Neither is reproducible on these hosts. Replaced with fault injection (D5) and
   a faked `st_dev` (D6) asserting the same invariants.
4. **MAJOR — the failure criteria were vacuously satisfiable.** Five tests
   asserting "`current.json` unchanged" are all passed by an implementation that
   never writes `current.json`. Added
   `test_successful_commit_advances_current_pointer` and
   `test_batch_run_produces_a_committed_generation` as named anti-vacuity guards,
   each with its own acceptance criterion.
5. **MINOR — a circular manifest test.** The draft validated the commit manifest
   against a schema the writer rendered from. Changed to field-by-field literals.
6. **MINOR — the collision with [#1968](https://github.com/vamseeachanta/digitalmodel/issues/1968) was unnoticed.**
   Both plans add call sites to `openfoam_batch_execution.py`, which is at 379 of
   400 lines. Surfaced as D8 with a split and a sequencing criterion.

**Overall:** r1 findings are resolved inline. Per the loop-break rule r3 is not
redispatched. Explicit user approval remains required; no approval marker has
moved and no implementation is authorized.

---

## Risks and Open Questions

- **The A-versus-B choice is a roadmap question the repository cannot answer.**
  Approving A commits to host-local artifact retention as a direction. If that is
  speculative, B is the better outcome and this plan says so plainly rather than
  steering toward the larger build.
- **Option B deletes working, well-tested code.** `artifact_index.py` has a
  careful framed codec and golden vectors. `git` retains it and it can return,
  but approving B accepts throwing away real work to keep the tree honest.
- **Option A is the largest of the three follow-ons by a wide margin** and is the
  only T3 among them. Its integration touches the batch execution path that
  [#1968](https://github.com/vamseeachanta/digitalmodel/issues/1968) also touches
  (D8).
- **This plan does not establish that anyone has asked for artifact retention.**
  The only recorded signal is intent noted at #1576's close. Treating that as
  demand would be exactly the reasoning that produced the unwired module.
- **`docs/plans/README.md` will conflict** with the
  [#1959](https://github.com/vamseeachanta/digitalmodel/issues/1959),
  [#1965](https://github.com/vamseeachanta/digitalmodel/issues/1965),
  [#1968](https://github.com/vamseeachanta/digitalmodel/issues/1968), and
  [#1969](https://github.com/vamseeachanta/digitalmodel/issues/1969) plan
  branches. Trivial and additive, flagged so it is expected.
- **Review diversity is degraded.** Only r1 Claude inline is recorded. If Codex
  and Agy remain unavailable, the approval preview will name that residual review
  risk rather than claim consensus.

## Complexity: T3

Option A crosses content identity, crash-consistent publication, filesystem
device boundaries, run-identity revalidation, workflow integration, and a module
split, and it collides with another approved-pending plan in one file. Option B
alone would be T1; the tier reflects option A. Implementation will remain
single-lane.
