# Plan for #1969: end the prebuilt-case middle state between the schema and the runner

> **Status:** plan-review (awaiting owner approval — never self-approved)
> **Complexity:** T2
> **Date:** 2026-08-05
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1969
> **Client:** N/A
> **Lane:** domain:cfd
> **Branch:** `plan/1968-1970-openfoam-followons` (worktree off `origin/main` @ `7b4119cc`)
> **Review artifacts:** r1 Claude — inline, main session (this document, see Adversarial Review Summary)

---

## Premise verification (2026-08-05, against `origin/main` @ `7b4119cc`)

The issue was filed by another lane. Every claim was re-checked. Five confirmed,
one **refuted** — and the refuted one is the issue's central characterisation.

| # | Claim as filed | Verdict | Evidence |
|---|---|---|---|
| 1 | Plan `:104` defines `CaseSource = AuthoredCaseV1 \| PrebuiltCaseV1` | **CONFIRMED** | `docs/plans/2026-07-13-issue-1575-openfoam-case-definition-contract.md:104` at `454ce30c889f6f1306c1015fef12fad176117e1f` |
| 2 | Plan `:24` says `runner.py` and `prebuilt_mesh.py` own a closed case-local prebuilt-manifest | **CONFIRMED** | same file `:24-26`, which also gives the reason: that contract "hashes the whole case and cannot safely consume an arbitrary external path or a newly authored semantic definition" |
| 3 | The Risks section says prebuilt is intentionally unavailable in v1 — the plan contradicts itself | **CONFIRMED** | `:388-389`. Against `:104-122`, which specifies the prebuilt arm in full detail, this is a genuine self-contradiction |
| 4 | `case_definition.py:332` allows `"prebuilt"`, then `:342-343` raises | **CONFIRMED** | verbatim as filed |
| 5 | `prebuilt_mesh.py` exists on `main` with that docstring | **CONFIRMED** | verbatim |
| 6 | The capability is "**partially present and unreachable**" — "attestation handling with no path from the case-definition schema to it" | **REFUTED** | Prebuilt case execution is **fully present, fully reachable, and covered by fourteen tests.** It is unreachable from the *schema*, and only from the schema |

### Why premise 6 is wrong, and what the real middle state is

`prebuilt_mesh.py` has four consumers, not zero:

```text
$ git grep -ln "prebuilt_mesh" origin/main
.github/workflows/gmsh-meshing-tests.yml
scripts/cfd/run_synthetic_tank_3d_smoke.py
src/digitalmodel/solvers/openfoam/runner.py
tests/solvers/openfoam/test_runner_prebuilt.py
```

`OpenFOAMRunner.run(case_dir, prebuilt_manifest=...)` (`runner.py:144-190`) is a
complete, working, attested prebuilt path. It ships, it runs in CI, and
`tests/solvers/openfoam/test_runner_prebuilt.py` pins fourteen behaviours of it,
including tamper rejection, stale-residue rejection, symlink rejection, lock
contention, and post-run mutation detection.

**The two questions the issue poses as open are already answered by merged code.**

- *"how attestation is verified before any copy"* — `prebuilt_mesh.py:91` calls
  `_validate_bound_case(case, payload)`; the `shutil.copytree` is at `:93`.
  Validation precedes the copy by two lines. The snapshot is then independently
  re-validated at `:97`, the source re-validated again at `:98`, `checkMesh` runs
  in the snapshot at `:100`, and the mesh digest is checked at `:101`.
- *"what happens on attestation failure"* — `PrebuiltMeshError` propagates to
  `runner.py:183-185`, which fails the result; `execution.release()` runs in a
  `finally` at `:186-187`; and `execution.verify_unchanged()` at `:182` catches
  post-run mutation.

So the reader the issue worries about — one who finds `prebuilt_mesh.py` and
concludes prebuilt cases are supported — **is correct**. They are supported. What
misleads is narrower and, in one respect, worse: `case_definition.py:342-343`
raises the message

```python
raise ValidationError("prebuilt cases are not available in schema v1")
```

which reads as a statement about the product. The true statement is about the
schema only. A user driving digitalmodel through YAML is told a shipped,
tested, CI-exercised capability does not exist.

**The real middle state is that two entry points disagree.** The Python API
supports attested prebuilt cases; the declarative schema refuses them.

### The gap is one argument wide

`workflow.py:154` is the join point, and it never passes the manifest:

```text
$ sed -n '154p' src/digitalmodel/solvers/openfoam/workflow.py
        result = OpenFOAMRunner(run_cfg).run(case_dir)
```

`runner.run` accepts `prebuilt_manifest` as a keyword; `workflow.py` does not
supply it. `workflow.py:22` documents the consequence in a comment:
`kind: authored            # "prebuilt" is reserved and refused in v1`.

The manifest location the #1575 plan specified — `constant/polyMesh.manifest.json`
(`:114-115`) — already matches the shipped constant exactly:
`MANIFEST_NAME = "polyMesh.manifest.json"` (`gmsh_bridge.py:28`), resolved as
`case / "constant" / MANIFEST_NAME` (`:178`).

**This makes the build option substantially cheaper than the issue implies, and
it makes the decline option correspondingly weaker.** The attestation,
snapshotting, locking, failure handling, and post-run verification the issue
lists as work to be planned are already merged and tested.

---

## Deliverable

The schema will stop making a false statement about the product. Depending on
the owner's decision at D2, `case_definition.py` will either carry a typed
`prebuilt` arm that reaches the shipped attested runner path, or an explicit
reserved-but-unreachable refusal that names the entry point which does work. The
#1575 plan's self-contradiction will be resolved in the record either way.

---

## Resource Intelligence Summary

### Existing repo code

- `src/digitalmodel/solvers/openfoam/case_definition.py` (**399 lines**, one under
  the 400 limit): `_parse_canonical:324-354` allows `"prebuilt"` in the key set at
  `:332` and refuses it at `:342-343`; `parse_case_request:378`; `SCHEMA_VERSION = 1`
  at `:25`.
- `src/digitalmodel/solvers/openfoam/workflow.py` (196 lines): calls
  `parse_case_request` at `:100-106` and `OpenFOAMRunner(run_cfg).run(case_dir)`
  at `:154`.
- `src/digitalmodel/solvers/openfoam/prebuilt_mesh.py` (345 lines):
  `prepare_prebuilt_execution:76-101+`, `PrebuiltExecution.verify_unchanged:64`,
  `_acquire_lock:85`, `_load_manifest`, `_reject_links`, `_reject_residue`,
  `_validate_bound_case`, `_hash_protected_inputs`, `_run_check_mesh`,
  `_validate_mesh_digest`.
- `src/digitalmodel/solvers/openfoam/runner.py` (397 lines): prebuilt constraints
  at `:210-211` (interFoam only) and `:234-243` (no mesh-modifying stages);
  `_stage_plan:245` omits the meshing stage for prebuilt.
- `tests/solvers/openfoam/test_runner_prebuilt.py`: fourteen tests at
  `:194,216,231,256,270,284,296,313,326,342,364,387,399,416`.

### Gaps identified

- No typed `PrebuiltCaseV1` parse arm exists; `kind: prebuilt` is refused, not parsed.
- No `prebuilt_cases/` directory convention exists anywhere in the repo
  (`grep -rn "prebuilt_cases" src/ tests/ docs/ scripts/` → no matches), so the
  #1575 plan's `case_id` resolution scheme is entirely unbuilt.
- No `prebuilt_manifest` argument is passed at the one call site that could pass it.
- No record reconciles the #1575 plan's `:104-122` specification with its `:388-389` Risks.
- The refusal message at `:342-343` states something untrue about the product.

### Evidence

**Attestation precedes copy** (2026-08-05, `prebuilt_mesh.py:88-101`):

```text
 88:        payload = _load_manifest(case, manifest)
 89:        _reject_links(case)
 90:        _reject_residue(case)
 91:        _validate_bound_case(case, payload)
 92:        snapshot = Path(tempfile.mkdtemp(prefix=f".{case.name}.run-", dir=case.parent))
 93:        shutil.copytree(case, snapshot, dirs_exist_ok=True, symlinks=False)
```

**The convention the #1575 plan assumed does not exist** (2026-08-05):

```text
$ grep -rn "prebuilt_cases" src/ tests/ docs/ scripts/
# no matches
```

**Standards and reusable-contract retrieval returned nothing.** No registry,
standard, or wiki artifact supplies a prebuilt-case locator contract, and no
standards-derived constant applies. Recorded as a null result.

---

## Design decisions

**D1 — The issue's question is answerable, and the answer is smaller than it looks.**
The issue asks the owner to decide "is prebuilt case staging wanted?" as though
the capability must be built. It is built. The decision is narrower: **should the
YAML schema be able to reach it?**

**D2 — Ranked options. Option B is a floor that applies regardless.**

| | Option | Cost | Assessment |
|---|---|---|---|
| A | **Wire the schema arm to the shipped runner path** | typed parse arm, `case_id` resolution, one argument at `workflow.py:154`, a module split | Now much cheaper than the issue implies — attestation, snapshot, locking, and failure handling all ship. The only genuinely new surface is `case_id` resolution (D3). |
| B | **Reserved-but-unreachable, stated honestly** (the floor) | corrected refusal message, corrected plan record, a docstring | **Required whether or not A is approved**, because the current message is false as written. Cheap, and removes the active misdirection. |
| C | Remove `"prebuilt"` from the allowed key set entirely | one line | Makes `kind: prebuilt` an unknown-key error instead of a specific refusal. Loses the reserved-name signal and the chance to point at the working path. Not recommended. |

**Recommendation: approve B unconditionally, and decide A separately.** B is not a
consolation prize for declining A — it is correction of a false statement, and it
should ship even if A ships too. This plan is written so B can be approved alone
and A deferred without rework, because A's changes are additive to B's.

**D3 — If A is approved, `case_id` resolution is the only new security surface,
and it is the whole risk.**
Everything else in A is plumbing over shipped code. `case_id` resolution is not:
it turns a user-supplied string into a filesystem path. The #1575 plan specified
it (`:112-115`) as a portable component resolving beneath a fixed
`prebuilt_cases/` directory by descriptor-relative, no-follow traversal — and
that directory does not exist yet. The contract will be: exactly one path
component; no separators, no `.`, no `..`, no absolute form, no URI, no
hostname; resolved with `O_NOFOLLOW` descriptor traversal beneath the input
bundle's `prebuilt_cases/`; and the manifest location fixed at
`constant/polyMesh.manifest.json`, never user-supplied. `prebuilt_mesh.py`
already rejects symlinks inside the case (`_reject_links`, pinned by
`test_manifest_symlink_is_rejected:270`); this contract governs reaching the
case at all.

**D4 — No attestation logic will be written, and none may be.**
The issue lists "how attestation is verified before any copy" and "what happens
on attestation failure" as things to plan. They are merged and tested. Writing a
second implementation would create exactly the silent-divergence hazard that
[#1970](https://github.com/vamseeachanta/digitalmodel/issues/1970) exists to
prevent for domain tags. Option A calls `prepare_prebuilt_execution` and adds
nothing to it. A code review that finds new hashing, copying, or locking logic in
this change should reject it.

**D5 — The 399-line ceiling forces a split, and the split is chosen on
responsibility, not on line count.**
`case_definition.py` is 399 of 400 permitted lines. Adding a parse arm is
impossible without a split. The prebuilt arm and its locator contract move to a
new `case_definition_prebuilt.py`; `case_definition.py` retains the authored
schema and the discriminator. Under option B alone the split is not needed — B
changes a message and adds no branch.

**D6 — Constraints inherited from the shipped runner are carried, not re-litigated.**
Prebuilt execution requires `interFoam` (`runner.py:210-211`) and forbids
mesh-modifying stages (`:234-243`). The #1575 plan adds that v1 prebuilt is
serial/pool only and that prebuilt MPI needs its own issue (`:120-122`). Option A
inherits all of these and states them in the schema documentation. It does not
relax any of them, and MPI prebuilt stays out of scope.

**D7 — The #1575 plan correction is recorded here, not edited on its branch.**
`chore/1575-case-definition-plan` is an unmerged plan branch whose plan describes
work that has shipped. Editing it buries the correction where nobody reads it.
The self-contradiction is recorded in this plan's premise table and in a comment
on [#1575](https://github.com/vamseeachanta/digitalmodel/issues/1575), and this
plan is indexed in `docs/plans/README.md`. No file on that branch is touched.

---

## Files to Change

**Option B (the floor):**

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/solvers/openfoam/case_definition.py` | `:342-343` refusal message states the schema limit and names the working entry point, instead of denying the capability |
| Modify | `src/digitalmodel/solvers/openfoam/workflow.py` | `:20-22` docstring records that prebuilt runs via `OpenFOAMRunner.run(prebuilt_manifest=...)` and is reserved in schema v1 |
| Modify | `tests/solvers/openfoam/test_case_definition.py` | pin the new refusal message literally |
| Update | `docs/plans/README.md` | index row |

**Option A, additive on top of B:**

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/solvers/openfoam/case_definition_prebuilt.py` | D5 split: `ParsedPrebuiltCaseV1`, the D3 `case_id` locator contract |
| Modify | `src/digitalmodel/solvers/openfoam/case_definition.py` | `_parse_canonical:324-354` dispatches `kind: prebuilt` to the new arm; file stays under 400 lines |
| Modify | `src/digitalmodel/solvers/openfoam/workflow.py` | `:154` passes `prebuilt_manifest=` when the parsed source is prebuilt |
| Create | `tests/solvers/openfoam/test_case_definition_prebuilt.py` | locator traversal, refusal, and dispatch tests |
| Modify | `tests/solvers/openfoam/test_workflow_case_definition.py` | end-to-end: a prebuilt request reaches `prepare_prebuilt_execution` |

**Explicitly untouched**, to stay clear of live lanes:
`src/digitalmodel/solvers/openfoam/prebuilt_mesh.py` and `runner.py` — the
attested path is shipped and this change calls it rather than altering it (D4);
`solver_contracts.py`, `templates/`, `case_builder.py`, `models.py` (owned by
[#1959](https://github.com/vamseeachanta/digitalmodel/issues/1959));
`src/digitalmodel/hydrodynamics/diffraction/`; capability-page producers (owned
by [#1965](https://github.com/vamseeachanta/digitalmodel/issues/1965));
`artifact_index.py` (owned by
[#1970](https://github.com/vamseeachanta/digitalmodel/issues/1970)); every file
on `chore/1575-case-definition-plan`.

---

## TDD Test List

Every row states the expected value and why it is red on `origin/main` @ `7b4119cc`.

**Option B:**

| Test | Input | Expected | Red today because |
|---|---|---|---|
| `test_prebuilt_refusal_names_the_supported_entry_point` | `kind: prebuilt` | raises, and the message contains the literal `OpenFOAMRunner` and the literal `prebuilt_manifest` | today the message is `"prebuilt cases are not available in schema v1"`, which names no alternative |
| `test_prebuilt_refusal_does_not_claim_the_capability_is_absent` | `kind: prebuilt` | message does not contain the literal substring `not available` | that exact phrase is what makes the current message false |
| `test_authored_case_is_unaffected_by_the_message_change` | `kind: authored` | parses unchanged | regression fence |

**Option A, additive:**

| Test | Input | Expected | Red today because |
|---|---|---|---|
| `test_prebuilt_case_id_rejects_path_separators` | `case_id: "a/b"` | raises; no filesystem access occurs | D3; no locator exists |
| `test_prebuilt_case_id_rejects_dot_and_dotdot` | `case_id: ".."` and `case_id: "."` | raises | D3 |
| `test_prebuilt_case_id_rejects_absolute_and_uri_forms` | `/etc`, `file:///etc`, `host:/x` | raises | D3 |
| `test_prebuilt_case_id_rejects_symlinked_component` | `prebuilt_cases/evil` → symlink outside the bundle | raises before any read | D3 no-follow traversal |
| `test_prebuilt_case_id_resolves_a_valid_component` | `case_id: "tank_a"` with a real directory | resolves to `<bundle>/prebuilt_cases/tank_a` | **anti-vacuity guard** — a locator that rejects everything fails here |
| `test_manifest_location_is_fixed_not_user_supplied` | request carrying `manifest: <path>` | raises unknown-key; the resolved manifest is always `constant/polyMesh.manifest.json` | the plan's fixed-location rule is unenforced |
| `test_prebuilt_request_reaches_prepare_prebuilt_execution` | valid prebuilt request, `prepare_prebuilt_execution` patched to record its arguments | called exactly once with the resolved case dir and the fixed manifest path | `workflow.py:154` passes no manifest |
| `test_prebuilt_rejects_non_interfoam_solver` | `solver: simpleFoam` | refused | inherited from `runner.py:210-211`; must be refused at parse time, not after staging |
| `test_prebuilt_rejects_mpi_mode` | `run_batch.mode: mpi` | refused | D6; v1 prebuilt is serial/pool only |
| `test_prebuilt_arm_adds_no_hashing_or_copy_logic` | `case_definition_prebuilt.py` source | contains no `sha256`, `copytree`, `mkdtemp`, or `chmod` | D4 anti-divergence guard |

**Not included, deliberately:** no test re-asserting attestation, snapshot, lock,
or post-run-mutation behaviour (D4 — fourteen such tests already exist at
`test_runner_prebuilt.py`, and duplicating them creates the divergence risk this
plan is trying to avoid); no test requiring a real `checkMesh` or OpenFOAM
binary (none exists on any host here); no test reading its expected refusal
message from the production constant (circular — the literal is written out in
the test body).

---

## Execution environment

Implementation runs in a dedicated worktree off `origin/main`
(`/mnt/ace/ws/agent-worktrees/dm-1968-1970-plans`). The shared checkout
`/mnt/ace/ws/digitalmodel` is on `fix/3787-startup-tax` and is **not** used.
Root `/` is at 100% (1.2 G free), so worktrees live on `/mnt/ace`.

No OpenFOAM installation exists on `ace-linux-1` (`which interFoam checkMesh`
returns nothing, 2026-08-05). Every test above is a pure-filesystem fixture with
`prepare_prebuilt_execution` patched at the boundary. The shipped attested path
is exercised by CI (`.github/workflows/gmsh-meshing-tests.yml`), not by this
change.

---

## Acceptance Criteria

- [ ] **Every test above fails on `origin/main` @ `7b4119cc` and passes after**, except `test_authored_case_is_unaffected_by_the_message_change`, which is green-before-and-after by design and named as such in the PR body.
- [ ] **`test_prebuilt_case_id_resolves_a_valid_component` passes** (option A only). This criterion exists to make the six locator-refusal criteria non-vacuous: a locator that rejects every input satisfies all of them and fails this one.
- [ ] **The refusal message no longer contains the substring `not available`**, asserted literally. The current message is false as a statement about the product, and this is the criterion that fixes it.
- [ ] **`case_definition_prebuilt.py` contains no `sha256`, `copytree`, `mkdtemp`, or `chmod`** (option A only), asserted by reading the source. This is the D4 guard against a second attestation implementation diverging from `prebuilt_mesh.py`.
- [ ] **No file under `src/digitalmodel/solvers/openfoam/prebuilt_mesh.py` or `runner.py` is modified**, verified by `git diff --stat` in the PR body. The shipped attested path is called, not altered.
- [ ] `PYTHONPATH=src uv run python -m pytest -q tests/solvers/openfoam/` passes, **including `test_runner_prebuilt.py`, which is run and not excluded** — its fourteen tests are the evidence that this change did not disturb the path it calls.
- [ ] **`pytest tests/solvers/openfoam/ tests/workflows/ -q` is compared node-ID by node-ID against a baseline captured in the same worktree at the branch point, with no file excluded from either side.** No new failure node IDs. Symmetric exclusion is forbidden here: #1575 excluded `test_workflow_router.py` from both baseline and after and still hid two regressions that only CI caught — and that was this very issue's parent.
- [ ] Every touched Python file is at most 400 physical lines and every function at most 50. `case_definition.py` is at 399 today; the PR body states its line count after the change.
- [ ] **No numeric threshold is introduced.** The locator contract is structural — component count, character classes, and `O_NOFOLLOW` — with no tuned value anywhere. The PR body confirms this explicitly.
- [ ] A comment is posted on [#1575](https://github.com/vamseeachanta/digitalmodel/issues/1575) recording its plan's `:104-122` versus `:388-389` self-contradiction and the resolution, with this plan linked. No file on `chore/1575-case-definition-plan` is edited.
- [ ] **No legal-scan criterion is stated, and its absence is deliberate.** `scripts/legal/legal-sanity-scan.sh` does not exist in this repository (verified 2026-08-05), and workspace-hub's `--repo=` form is **fail-open** under OPEN workspace-hub [#3804](https://github.com/vamseeachanta/workspace-hub/issues/3804) — it resolves an empty scan path, scans nothing, and exits 0. This change introduces no client-identifier surface; the locator contract explicitly forbids hostnames and URIs.
- [ ] T2 plan review has no MAJOR before user approval is requested; T2 code review has no MAJOR before close.
- [ ] r1 review artifact recorded.

---

## Out of scope

- **The authored-case contract**, which shipped and is closed.
- **Any change to `prebuilt_mesh.py` or `runner.py`** (D4). The attested path works and is tested; this plan calls it.
- **Prebuilt MPI execution.** The #1575 plan restricts v1 prebuilt to serial/pool (`:120-122`) and #1576 rejects prebuilt MPI. Carried, not revisited. [#1968](https://github.com/vamseeachanta/digitalmodel/issues/1968) owns MPI resume separately.
- **Creating or populating a `prebuilt_cases/` directory with real meshes.** Option A defines how a `case_id` resolves; supplying cases is a data question, not a contract one.
- **Relaxing the interFoam-only constraint** (`runner.py:210-211`). Inherited as-is.
- **The `_parse_legacy` path** (`case_definition.py:355`). Legacy inputs retain documented behaviour and gain no prebuilt arm.

---

## Adversarial Review Summary

| Round | Provider | Verdict |
|---|---|---|
| r1 | Claude — inline, main session | **MAJOR** — 5 findings against this plan's own earlier draft, all folded in |

r1 findings:

1. **MAJOR — the first draft accepted the issue's "partially present and
   unreachable" framing** and planned a full staging path: attestation
   verification, copy semantics, failure handling. All of that is merged and
   covered by fourteen tests. The draft would have specified a **second**
   attestation implementation — precisely the silent-divergence hazard that
   [#1970](https://github.com/vamseeachanta/digitalmodel/issues/1970) is filed
   about. Rewritten as D4, with a source-inspection test to enforce it.
2. **MAJOR — the decline option was presented as the cheap fallback.** It is not
   optional: the current refusal message is false as written, so correcting it is
   required whether or not the arm is wired. Restructured as D2 option B, a floor
   that applies regardless, with A additive on top.
3. **MAJOR — the locator refusal criteria were vacuously satisfiable.** Six
   rejection tests with no acceptance test are all passed by a locator that
   rejects everything. Added `test_prebuilt_case_id_resolves_a_valid_component`
   as a named anti-vacuity guard and its own acceptance criterion.
4. **MINOR — the draft assumed `prebuilt_cases/` existed** because the #1575 plan
   describes it in the present tense. It does not exist anywhere in the repo.
   Corrected, and called out as the one genuinely new surface (D3).
5. **MINOR — a criterion asserted the refusal message by comparing against the
   production constant**, which cannot fail. Changed to literal substring
   assertions in the test body, including a negative one on `not available`.

**Overall:** r1 findings are resolved inline. Per the loop-break rule r3 is not
redispatched. Explicit user approval remains required; no approval marker has
moved and no implementation is authorized.

---

## Risks and Open Questions

- **Option A may not be wanted, and this plan does not assume it is.** Approving B
  alone is a complete, coherent outcome that leaves the schema honest and the
  capability reachable through the API. A can be approved later with no rework,
  because A is additive to B.
- **Approving A accepts one new security surface.** `case_id` resolution converts
  a user string into a path. Everything else in A is plumbing over shipped code,
  but this part is not, and the D3 contract is the whole of its defence.
- **Nobody may actually want prebuilt via YAML.** The capability has shipped for
  some time with an API-only entry point and no request for a schema arm is
  recorded. If the demand is hypothetical, B is the right stopping point and A is
  speculative work. This is a genuine open question the plan cannot settle from
  the repository, and the owner is better placed to answer it than this plan is.
- **`docs/plans/README.md` will conflict** with the
  [#1959](https://github.com/vamseeachanta/digitalmodel/issues/1959),
  [#1965](https://github.com/vamseeachanta/digitalmodel/issues/1965), and
  [#1968](https://github.com/vamseeachanta/digitalmodel/issues/1968) plan
  branches. A trivial additive conflict, flagged so it is expected.
- **Review diversity is degraded.** Only r1 Claude inline is recorded. If Codex
  and Agy remain unavailable, the approval preview will name that residual review
  risk rather than claim consensus.

## Complexity: T2

The change crosses schema versioning, a filesystem locator contract, and a
correction to another issue's plan record, but it calls a shipped attested path
rather than building one, and implementation will remain single-lane. Option B
alone would be T1; the tier reflects option A's locator surface.
