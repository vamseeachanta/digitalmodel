# Plan for #1968: settle MPI resume by validating the decomposition it reuses

> **Status:** plan-approved — the owner applied `status:plan-approved` to
> [#1968](https://github.com/vamseeachanta/digitalmodel/issues/1968) and chose
> **D2 option 1 (validated resume)**. Options 2 and 3 were not approved. No
> approval marker was ever self-applied.
> **Complexity:** T2
> **Date:** 2026-08-05
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1968
> **Client:** N/A
> **Lane:** domain:cfd
> **Branch:** `plan/1968-1970-openfoam-followons` (worktree off `origin/main` @ `7b4119cc`)
> **Review artifacts:** r1 Claude — inline, main session (this document, see Adversarial Review Summary)

---

## Premise verification (2026-08-05, against `origin/main` @ `7b4119cc`)

The issue was filed by another lane. Every claim was re-checked against the tree
before this plan was written. Four confirmed, one **refuted**, and the refuted one
inverts the recommendation.

| # | Claim as filed | Verdict | Evidence |
|---|---|---|---|
| 1 | The #1576 plan mandates MPI `resume: true` rejects before mutation | **CONFIRMED** | `docs/plans/2026-07-13-issue-1576-openfoam-mpi-postprocessing-artifacts.md:129` at `ba9366da8425ea6d0508eb3643c2ac985065e2dd`, verbatim as quoted |
| 2 | Its TDD list names `test_fresh_vtk_stage_order_and_mpi_resume_rejects_before_mutation` | **CONFIRMED** | same file `:278` |
| 3 | A merged green test asserts MPI resume succeeds | **CONFIRMED** | `tests/workflows/test_openfoam_run_batch.py:339` `test_mpi_resume_restarts_from_latest_time`; `:369` asserts `mpi_command_plan("interFoam", 4, resume=True)[0][0] == "mpirun"` |
| 4 | `:359` pins a deliberate "must not rebuild" guard | **CONFIRMED** | `patch.object(ofb, "_build_case", side_effect=AssertionError("resume must not rebuild the case"))` at `:357-360` |
| 5 | "Serial and pool resume remain owned by #1565/#1575" — the basis for scoping those out | **REFUTED** | **There is no serial or pool resume to own.** `solve_serial` (`openfoam_batch_execution.py:128-153`) has no resume concept. `run_case_pool` (`:106-125`) calls `_clean(item, layout)` then `build(item)` on *every* non-checkpointed run (`:117-118`) — it always destroys and rebuilds. The only "resume-like" pool behaviour is a completed-checkpoint short-circuit (`:112-114`), which is skip-if-done, not resume. The shipped config is explicit: `src/digitalmodel/base_configs/modules/openfoam_run_batch/openfoam_run_batch.yml:35` reads `resume: false          # mpi only: restart solver from latestTime if` / `# a previous attempt's processor* dirs exist` |

**Why premise 5 changes the answer.** `resume` is an **MPI-only** feature by
definition in this codebase. The #1576 plan scoped serial/pool resume out on the
belief that they existed elsewhere; they do not. So "MPI `resume: true` rejects"
is not a narrowing of a broader capability — it is **deletion of the product's
only resume capability**, and the plan's own sentence shows its authors did not
realise that. A T2 decision to delete a shipped, documented, configured feature
cannot rest on a premise that is false.

### Three findings not in the issue

**A — the green test itself encodes the defect.** At `:353` the test creates
exactly one directory, `processor0`, and at `:361-364` runs
`_run_case_mpi(..., workers=4, ...)`, then asserts the run **completes**. So the
merged test does not merely assert "resume works"; it asserts that a **4-rank
run resumes from a 1-rank decomposition**. That is not a defensible behaviour
under any reading of OpenFOAM, and it means the test cannot simply be kept.
Both artifacts are wrong. Neither side of the filed contradiction is correct as
written.

**B — nothing validates decomposition state anywhere.** The entire resume
precondition is `_has_processors` (`openfoam_batch_execution.py:308-309`):

```python
def _has_processors(case_dir: Path) -> bool:
    return case_dir.is_dir() and any(case_dir.glob("processor*"))
```

A boolean on the existence of any one glob match. It does not count directories,
does not check that `processor0..processorN-1` form a contiguous set, and does
not compare anything against `workers`. Downstream, `mpi_command_plan:334` emits
`["mpirun", "-np", str(workers), solver, "-parallel"]` from the *current*
request unconditionally, and `write_decompose_par_dict` is skipped on resume
(`:195-196`), so `system/decomposeParDict` keeps the previous run's
`numberOfSubdomains`. Across `src/`, `decomposeParDict` and `numberOfSubdomains`
have **three write sites and zero read sites**. `validate_workers`, the one
function that reasons about rank counts, is imported at
`openfoam_batch_execution.py:18` and **never called** in production code.

**C — mutation precedes any check.** `_prepare_mpi_case:263-270` calls
`set_start_from_latest_time(item["work_dir"])` and returns the work dir before
any stage runs. That function (`:367-374`) rewrites `system/controlDict` in
place. So on the legacy layout path a rank-mismatched resume has already mutated
the case before `mpirun` is launched, and the failure — if OpenFOAM catches it
at all — surfaces only through the `FOAM_FATAL_MARKERS` log scan (`:29-32`,
`_fatal_marker:239-249`) after launch.

**Locational note for the implementer.** `openfoam_run_batch.py` is a 259-line
re-export facade; `_run_case_mpi`, `mpi_command_plan`, and `_build_case` are
imported from `openfoam_batch_execution.py` at `:20-36` and re-exported at
`:48-69`. The behaviour under discussion lives in `openfoam_batch_execution.py`,
not in the file the issue's test citation points at.

---

## Deliverable

MPI `resume: true` will reuse an existing `processor*` decomposition only after
that decomposition is proven consistent with the run being requested, will refuse
before touching `system/controlDict` when it is not, and will say which check
failed. The #1576 plan's resume paragraph will be corrected in the record, and
`test_mpi_resume_restarts_from_latest_time` will be replaced by a pair of tests
that pin both the accept and the refuse path, carrying its "must not rebuild"
guard forward.

---

## Resource Intelligence Summary

### Existing repo code

- `src/digitalmodel/workflows/openfoam_batch_execution.py` (379 lines) owns the
  MPI path: `run_case_mpi:156-167`, `_run_case_mpi_unlocked:170-208`,
  `_prepare_mpi_case:263-270`, `_has_processors:308-309`,
  `mpi_command_plan:317-341`, `write_decompose_par_dict:360-364`,
  `set_start_from_latest_time:367-374`.
- `src/digitalmodel/workflows/openfoam_batch_config.py:56+` holds
  `validate_workers`, which compares a request against `visible_rank_count` and
  `dispatcher_rank_limit`. It never consults on-disk state and is never called
  from production.
- `src/digitalmodel/workflows/openfoam_batch_layout.py:46` derives the run dir as
  `openfoam-run-{identity_sha256}`; `build_run_identity`
  (`openfoam_batch_identity.py:41-64`) folds `dispatcher_rank_limit` into
  `host_capabilities` at `:58-59`.
- `tests/workflows/test_openfoam_run_batch.py` (445 lines) holds the four MPI
  tests: `:246` fresh-run ordering, `:283` `reconstruct: False` retains
  `processor*`, `:308` a failed stage retains `processor0` "for diagnosis /
  potential resume", `:339` the resume test under discussion.
- `tests/workflows/test_openfoam_mpi_stages.py` (182 lines) asserts
  `mpi_command_plan` shape at `resume=False` only — **zero** resume coverage.

### Gaps identified

- No decomposition-consistency contract exists; resume is gated on a glob.
- No read path for `system/decomposeParDict` exists anywhere in `src/`.
- No refusal path exists that fires before `system/controlDict` is mutated.
- No test pins a resume **refusal**; the suite has an accept path only.
- No record exists reconciling the #1576 plan's resume paragraph with `main`.

### Evidence

**Rank mismatch is accepted today** (2026-08-05, `origin/main` @ `7b4119cc`) —
`tests/workflows/test_openfoam_run_batch.py:353,361-364`:

```text
    (case_dir / "processor0").mkdir()
...
        row = ofb._run_case_mpi(
            item, {"resume": True, "reconstruct": True}, workers=4, mock=False,
            command_runner=fake_runner,
        )
...
    assert row["status"] == "completed"
```

**Decomposition state is never read** (2026-08-05):

```text
$ git grep -n "numberOfSubdomains" origin/main -- src
src/digitalmodel/workflows/openfoam_batch_execution.py:362
src/digitalmodel/solvers/openfoam/case_builder.py:272
src/digitalmodel/solvers/openfoam/validation/sloshing_3d.py:188
# three writers, no readers
```

**Standards and reusable-contract retrieval returned nothing.** No repo
registry, standard, or wiki artifact supplies a decomposition-reuse contract, and
no standards-derived constant applies to this work. Recorded as a null result.

---

## Design decisions

**D1 — The filed question is the wrong question; neither artifact is right.**
The issue asks which of two artifacts to amend. Finding A shows the answer is
"both". The plan mandates a blanket refusal justified by a false premise
(premise 5); the test asserts an acceptance that is unsafe on its own terms
(4 ranks against 1 processor dir). Amending either one to match the other would
ship a defect. The deliverable is therefore a third behaviour, and **both**
artifacts change.

**D2 — Resume will be validated, not rejected.** The three options, ranked:

| | Option | Cost | Why not / why |
|---|---|---|---|
| 1 | **Validated resume** (recommended) | one new module, one gate, two tests | Keeps the capability, removes the hazard, and refuses loudly and early instead of silently. |
| 2 | Reject MPI resume entirely (the #1576 plan's position) | delete `resume` from the config, the plan, and the code | Cheapest to implement and unimpeachably safe — but per premise 5 it **removes the product's only resume capability**, and a CFD run that dies at hour 9 of 12 then has no path but a full rerun. The plan chose this believing serial/pool resume survived. It does not. |
| 3 | Keep current behaviour, document the hazard | zero code | Not defensible: the behaviour accepts a 4-rank run against a 1-rank decomposition, and finding C shows it mutates `controlDict` before anything checks. |

Option 1 is recommended. Option 2 remains a legitimate owner choice — it is
strictly safer and strictly cheaper — and if the owner prefers it, D6's
replacement test becomes a refusal-only test and the config key is removed in the
same change. **This plan does not assume option 1 is approved.**

**D3 — The gate is derived entirely from named inputs. No fitted constants.**
A resume will be accepted only if all of the following hold. Every value comes
from a named input or from on-disk state; none is a tuned threshold:

1. The set of `processor*` directory names is exactly
   `{processor0 … processor{workers-1}}` — contiguous, no gaps, no extras.
   Derived from `run_batch.workers`, the same value `mpi_command_plan:334`
   passes to `-np`.
2. `system/decomposeParDict` exists and its `numberOfSubdomains` equals
   `workers`. This is the first read site for that file in the repo.
3. Every `processorN/` exposes at least one numeric time directory, and the
   **maximum** numeric time directory is identical across all of them.
   Derived from the directory listings; the comparison is equality between
   ranks, not a tolerance.
4. That common maximum time is a valid OpenFOAM time name, reusing
   `artifact_index.is_numeric_time_name` (`artifact_index.py:164`) rather than
   a second parser.

Any failure refuses the resume with a message naming the failed check and the
observed-versus-required values.

**D4 — Refusal happens before mutation.** The gate runs inside
`_prepare_mpi_case` *before* `set_start_from_latest_time` is called, so a
refused resume leaves `system/controlDict` byte-identical. This is directly
testable by hashing the file before and after, and it is what makes "rejects
before mutation" — the one phrase from the #1576 plan that survives — true.

**D5 — What is settled locally, and what is not.**
Check 1 and check 2 need no knowledge of OpenFOAM internals: a run requesting
N ranks against an M-rank decomposition is incoherent on its face, and the repo
demonstrably permits it.

Check 3 is different. It exists because a parallel run killed mid-write can
leave ranks with divergent latest times, and `startFrom latestTime` resolves per
rank. **This plan does not assert that as an established fact.** No OpenFOAM
installation exists on `ace-linux-1` (`which interFoam decomposePar` → no
output, 2026-08-05), so it could not be tested here, and it is not being taken
from memory.

The design is deliberately arranged so the answer does not change the code:
requiring all ranks to agree on a maximum time is correct if divergence is
dangerous, and merely redundant if OpenFOAM already synchronises. It fails
closed either way, and it costs one directory listing per rank. An implementer
who wants to *relax* check 3 must first cite OpenFOAM v2312 source for the
claim that per-rank time selection is synchronised — relaxation needs evidence;
keeping it does not. The pinned toolchain is `PINNED_TOOLCHAIN` at
`gmsh_bridge.py:51`, OpenFOAM `2312.260127-2`.

**D6 — The test is replaced in the open, and its guard is carried forward.**
`test_mpi_resume_restarts_from_latest_time` will be **rewritten, not deleted**,
as two tests in a new file, and the commit body will state that its `workers=4`
against one `processor0` was the reason. The `_build_case` /
`side_effect=AssertionError("resume must not rebuild the case")` guard moves
into the **accept-path** test, which is where it belongs: it is an assertion
about what a *successful* resume must not do. The refuse-path test asserts the
complement — that a refused resume does not rebuild *and* does not mutate.

**D7 — The size limit forces new files, and that is fine.**
`openfoam_batch_execution.py` is already 379 of its permitted 400 lines and
`tests/workflows/test_openfoam_run_batch.py` is already 445. Neither can absorb
this work. The gate goes in a new `openfoam_batch_decomposition.py` and the
tests in a new `tests/workflows/test_openfoam_mpi_resume.py`.

**D8 — The #1576 plan correction is recorded here, not edited on its branch.**
`chore/1576-mpi-artifact-plan` is an unmerged plan branch whose plan describes
work that has partly shipped. Editing a stale unmerged branch buries the
correction where nobody reads it. The correction is recorded in this plan (the
premise-verification table above) and in a comment on
[#1576](https://github.com/vamseeachanta/digitalmodel/issues/1576), and this
plan is indexed in `docs/plans/README.md`. No file on that branch is touched.

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/workflows/openfoam_batch_decomposition.py` | D3 gate: rank-set contiguity, `numberOfSubdomains` read, cross-rank latest-time agreement, typed refusal |
| Modify | `src/digitalmodel/workflows/openfoam_batch_execution.py` | call the gate in `_prepare_mpi_case:263-270` before `set_start_from_latest_time`; widen `_has_processors:308-309`'s role from precondition to input |
| Create | `tests/workflows/test_openfoam_mpi_resume.py` | D6 accept-path and refuse-path tests, carrying the "must not rebuild" guard |
| Modify | `tests/workflows/test_openfoam_run_batch.py` | remove `test_mpi_resume_restarts_from_latest_time:339` with its replacement named in the commit body; file drops below 400 lines |
| Modify | `src/digitalmodel/base_configs/modules/openfoam_run_batch/openfoam_run_batch.yml` | `:35` comment states that resume requires a decomposition matching `workers` |
| Update | `docs/plans/README.md` | index row |

**Explicitly untouched**, to stay clear of live lanes:
`src/digitalmodel/solvers/openfoam/solver_contracts.py`, `templates/`,
`case_builder.py`, `models.py` (owned by
[#1959](https://github.com/vamseeachanta/digitalmodel/issues/1959));
`src/digitalmodel/hydrodynamics/diffraction/`; all capability-page producers
(owned by [#1965](https://github.com/vamseeachanta/digitalmodel/issues/1965));
`artifact_index.py` and `artifact_generation.py` (owned by
[#1970](https://github.com/vamseeachanta/digitalmodel/issues/1970)); every file
on `chore/1576-mpi-artifact-plan`.

---

## TDD Test List

Every row states the expected value and why it is red on `origin/main` @ `7b4119cc`.

| Test | Input | Expected | Red today because |
|---|---|---|---|
| `test_resume_accepts_matching_decomposition` | `processor0..processor3` each holding `0.5/`, `decomposeParDict` with `numberOfSubdomains 4`, `workers=4` | run completes; plan is exactly `["mpirun", "reconstructPar"]`; `controlDict` contains `latestTime`; `_build_case` never called | passes today by accident — this is the **anti-vacuity guard**: a gate that refuses everything fails here |
| `test_resume_refuses_rank_count_mismatch` | one `processor0`, `workers=4` | refusal naming rank count, observed 1, required 4 | today this **completes** (`test_openfoam_run_batch.py:353,361-364`) |
| `test_resume_refuses_non_contiguous_ranks` | `processor0`, `processor1`, `processor3`, `workers=4` | refusal naming the missing `processor2` | nothing counts or orders processor dirs |
| `test_resume_refuses_subdomain_dict_mismatch` | `processor0..3` present, `decomposeParDict` says `numberOfSubdomains 8`, `workers=4` | refusal naming the dict value | `decomposeParDict` has no read site in `src/` |
| `test_resume_refuses_divergent_latest_times` | `processor0..3`; ranks 0-2 hold `0.5/`, rank 3's newest is `0.4/` | refusal naming rank 3 and both times | D5 check 3; no cross-rank comparison exists |
| `test_resume_refuses_when_a_rank_has_no_time_directory` | `processor0..3`; `processor3` empty | refusal naming rank 3 | nothing inspects processor dir contents |
| `test_refused_resume_does_not_mutate_control_dict` | any refusal fixture | sha256 of `system/controlDict` identical before and after; no stage runs | D4; today `set_start_from_latest_time` runs first (`:263-270`) |
| `test_refused_resume_does_not_rebuild_the_case` | any refusal fixture, `_build_case` patched to raise | refusal is raised, not the `AssertionError` | the rehomed `:359` guard, complement side |
| `test_resume_time_name_validation_reuses_artifact_index` | `processor0..3` newest dir named `0.5.0` | refusal; `is_numeric_time_name` is the only parser | no second time-name parser may be introduced |
| `test_fresh_run_plan_is_unchanged` | `resume=False`, `workers=8` | `decomposePar -force` still precedes `mpirun -np 8`; `decomposeParDict` written | regression fence — the gate must not touch the fresh path |

**Not included, deliberately:** no test that runs a real `mpirun`, `decomposePar`,
or solver (D5 — no OpenFOAM on any CI or dev host here, and requiring one would
make the criterion unrunnable); no test that reads its expected rank count from
the gate's own parser (circular — every fixture writes `decomposeParDict` as
literal text and every expected count is a literal in the test body); no test
asserting OpenFOAM's internal time-selection semantics (D5 — unverified, and the
design does not depend on it).

---

## Execution environment

Implementation runs in a dedicated worktree off `origin/main`
(`/mnt/ace/ws/agent-worktrees/dm-1968-1970-plans`). The shared checkout
`/mnt/ace/ws/digitalmodel` is on `fix/3787-startup-tax` and is **not** used.
Root `/` is at 100% (1.2 G free), so worktrees live on `/mnt/ace`; the `/tmp`
scratchpad cannot hold another checkout — an attempt on 2026-08-05 failed with
`No space left on device` mid-checkout.

Every test in this plan is a pure-filesystem fixture under `tmp_path` with an
injected `command_runner`. No OpenFOAM binary is required, and none is available:

```text
$ which mpirun interFoam decomposePar reconstructPar
/home/vamsee/miniforge3/bin/mpirun
$ ls -d /opt/openfoam* /usr/lib/openfoam* 2>/dev/null
# no output
```

`mpirun` alone resolves, from an unrelated conda environment. That is not an
OpenFOAM installation and will not be treated as one.

---

## Acceptance Criteria

- [ ] **Every test above fails on `origin/main` @ `7b4119cc` and passes after**, except `test_resume_accepts_matching_decomposition` and `test_fresh_run_plan_is_unchanged`, which are green-before-and-after by design and are named as such in the PR body. The red list is captured by running the new file against a clean `origin/main` worktree.
- [ ] **`test_resume_accepts_matching_decomposition` passes.** This criterion exists to make the refusal criteria non-vacuous: a gate that refuses every resume satisfies six of the rows above and fails this one.
- [ ] **A refused resume leaves `system/controlDict` byte-identical**, asserted by sha256 taken before the call and after the refusal — not by asserting the absence of the string `latestTime`, which an empty file also satisfies.
- [ ] `PYTHONPATH=src uv run python -m pytest -q tests/workflows/test_openfoam_mpi_resume.py tests/workflows/test_openfoam_run_batch.py tests/workflows/test_openfoam_mpi_stages.py tests/workflows/test_openfoam_batch_execution.py` passes.
- [ ] **`pytest tests/workflows/ tests/solvers/openfoam/ -q` is compared node-ID by node-ID against a baseline captured in the same worktree at the branch point, with no file excluded from either side.** No new failure node IDs. Symmetric exclusion is forbidden here: #1575 excluded `test_workflow_router.py` from both baseline and after — textbook-honest — and still hid two regressions that only CI caught.
- [ ] Every touched Python file is at most 400 physical lines and every function at most 50. `tests/workflows/test_openfoam_run_batch.py` drops from 445 to below 400 as a result of the D6 removal.
- [ ] **`test_mpi_resume_restarts_from_latest_time` is replaced, not deleted.** The commit body names the two tests that succeed it and states that its `workers=4` against a single `processor0` was the reason it could not be kept. A reviewer can find the successor from the commit message alone.
- [ ] **No numeric threshold introduced by this change is a fitted constant.** Every value in the gate is `workers`, a directory count, a `numberOfSubdomains` read from disk, or an equality between ranks. The PR body lists them.
- [ ] **No acceptance criterion requires an OpenFOAM installation**, and this is deliberate — no host in this environment has one, so such a criterion would be unrunnable rather than merely inconvenient. Real-host confirmation on `gpu-claw` is available and is recorded below as optional evidence, not as a gate.
- [ ] **No legal-scan criterion is stated, and its absence is deliberate.** `scripts/legal/legal-sanity-scan.sh` does not exist in this repository (verified 2026-08-05). workspace-hub's `--repo=` form is **fail-open** under OPEN workspace-hub [#3804](https://github.com/vamseeachanta/workspace-hub/issues/3804) — it resolves an empty scan path, scans nothing, and exits 0 — so a green result from it is evidence of nothing. This change touches rank arithmetic and directory listings and introduces no client-identifier surface.
- [ ] A comment is posted on [#1576](https://github.com/vamseeachanta/digitalmodel/issues/1576) recording that its `:129` resume paragraph rests on a refuted premise, with this plan linked. No file on `chore/1576-mpi-artifact-plan` is edited.
- [ ] T2 plan review has no MAJOR before user approval is requested; T2 code review has no MAJOR before close.
- [ ] r1 review artifact recorded.

---

## Out of scope

- **Serial and pool resume** — and the reason is now different from the one the issue gives. They are out of scope because they **do not exist** (premise 5), not because another issue owns them. Building either is separate work and is not proposed here.
- **The completed-checkpoint short-circuit** (`openfoam_batch_execution.py:112-114`, `163-165`) and its identity gating, including the legacy `identity_sha256 is None` path at `:62-63`. That is checkpoint semantics, not decomposition reuse.
- **`validate_workers` being imported and never called** (`:18`). A real finding, filed separately rather than fixed opportunistically here; wiring it in changes fresh-run behaviour, which this plan fences off.
- **The dead duplicate helpers** `_clean_case_dir` / `_prune_processor_dirs` / `_has_processor_dirs` at `openfoam_run_batch.py:248-259`, unreferenced anywhere. Same reason.
- **Whether `reconstruct: False` runs should be resumable at all** (`test_openfoam_run_batch.py:283` pins that `processor*` is then the only copy of the result). A genuine question this plan does not answer.
- **Physical validity of any resumed run.** This plan proves a resume is *coherent* with its decomposition. Whether the resulting solution is correct is a validation question and nothing here should be read as evidence of it.

---

## Adversarial Review Summary

| Round | Provider | Verdict |
|---|---|---|
| r1 | Claude — inline, main session | **MAJOR** — 5 findings against this plan's own earlier draft, all folded in |

r1 findings:

1. **MAJOR — the first draft proposed amending the test to match the plan.** That
   is exactly the failure mode the issue was filed to prevent, and it would have
   shipped the plan's blanket refusal on the strength of premise 5, which is
   false. Rewritten as D1: both artifacts are wrong.
2. **MAJOR — an acceptance criterion required a real interrupted parallel run.**
   No OpenFOAM exists on any host in this environment. That criterion could never
   have been executed, and it is precisely the class of defect flagged in the
   brief (a Sphinx build in a repo with no `conf.py`). Replaced with pure
   filesystem fixtures; real-host confirmation demoted to optional evidence.
3. **MAJOR — the refusal criteria were vacuously satisfiable.** Six refusal tests
   with no accept test are all passed by a gate that refuses every resume,
   deleting the feature while appearing to validate it. Added
   `test_resume_accepts_matching_decomposition` as a named anti-vacuity guard and
   made it its own acceptance criterion.
4. **MINOR — a circular fixture.** The draft had tests write `decomposeParDict`
   via the production `write_decompose_par_dict` and expected counts read back
   through the gate's own parser. Fixtures now write literal text and expected
   counts are literals in the test body.
5. **MINOR — "controlDict unchanged" asserted by absence of `latestTime`.** An
   empty or truncated file satisfies that. Changed to a sha256 equality.

**Overall:** r1 findings are resolved inline. Per the loop-break rule r3 is not
redispatched. Explicit user approval remains required; no approval marker has
moved and no implementation is authorized.

---

## Risks and Open Questions

- **The owner may prefer option 2 (reject).** It is safer and cheaper, and this
  plan says so plainly rather than arguing it away. Approving option 1 accepts
  that resume remains a real capability with a real, now-gated hazard.
- **D5's check 3 is not evidence-backed and is retained as a conservative
  default.** Approving this plan accepts a check whose necessity is unproven but
  whose cost is one directory listing per rank. If OpenFOAM v2312 turns out to
  synchronise per-rank time selection, the check is redundant, not wrong.
- **The external-layout path already degrades silently.** Because
  `dispatcher_rank_limit` is folded into the run identity
  (`openfoam_batch_identity.py:58-59`) and the run dir is
  `openfoam-run-{identity_sha256}`, changing `workers` lands in a *different*
  directory, so the old `processor*` set is invisible and resume quietly becomes
  a fresh build rather than a refusal. This plan makes the legacy path loud; it
  does not make the external path loud. Named here rather than fixed, because
  fixing it means changing identity derivation, which #1565 owns.
- **`docs/plans/README.md` will conflict** with the
  [#1959](https://github.com/vamseeachanta/digitalmodel/issues/1959) and
  [#1965](https://github.com/vamseeachanta/digitalmodel/issues/1965) plan
  branches, which add their own index rows. A trivial additive conflict, flagged
  so it is expected rather than surprising.
- **Review diversity is degraded.** Only r1 Claude inline is recorded. If Codex
  and Agy remain unavailable, the approval preview will name that residual review
  risk rather than claim consensus.

## Complexity: T2

The change crosses MPI stage planning, on-disk decomposition state, a merged-test
replacement, and a correction to another issue's plan record — but it is a single
new module with a single call site, and implementation will remain single-lane.
