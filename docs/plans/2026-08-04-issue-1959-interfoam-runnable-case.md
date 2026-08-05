# Plan for #1959: make the emitted OpenFOAM case actually runnable by its selected solver

> **Provenance.** Reproduced from the owner-approved revision
> `9b7810523f4493d45ea7649b9010d2e0e2f8ea9c` on branch
> `plan/1959-interfoam-runnable`. One divergence from that revision, confined to
> the Execution environment section: two machine-specific absolute paths were
> generalized so the file passes this repository's diff-scoped absolute-path
> gate (`scripts/enforcement/check-no-abs-paths.sh`). No scope, design decision,
> TDD row, or acceptance criterion is altered. The approved branch holds the
> byte-exact original.

> **Status:** plan-review (awaiting owner approval — never self-approved)
> **Complexity:** T2
> **Date:** 2026-08-04
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1959
> **Client:** N/A
> **Lane:** domain:cfd
> **Branch:** `plan/1959-interfoam-runnable` (worktree off `origin/main` @ `85c3c4af`)
> **Review artifacts:** r1 Claude — inline, main session (this document, see Adversarial Review Summary)

---

## Premise verification (2026-08-04, against `origin/main` @ `85c3c4af`)

The issue was filed by another lane. Every claim in it was re-checked against the
tree and against the CFD node before this plan was written. Two claims changed.

| # | Claim as filed | Verdict | Evidence |
|---|---|---|---|
| 1 | `alpha.water` is lumped into a turbulence regex group | **CONFIRMED** | `src/digitalmodel/solvers/openfoam/templates/__init__.py:37,44` — `"(k|omega|epsilon|alpha.water)"` and its `Final` twin. No `nAlphaCorr`, `nAlphaSubCycles`, `cAlpha`, or MULES controls anywhere in the file |
| 2 | `p_rgh` and `p_rghFinal` are absent | **CONFIRMED** | `templates/__init__.py:9-22` provides `p` / `pFinal` only. `interFoam` solves `p_rgh`; `p` does not match it |
| 3 | "There is no `PIMPLE` block" | **REFUTED** | `PIMPLE_BLOCK` exists at `templates/__init__.py:54-61` and **is** emitted for `interFoam` at `case_builder.py:252-257`. It carries `nOuterCorrectors 3`, `nCorrectors 2`, `nNonOrthogonalCorrectors 1`. Only `momentumPredictor` is absent — and that key is **optional** in `interFoam` (it defaults to `yes`), so its absence is not a start-up error. Carrying this claim forward unexamined would have put a non-defect in the TDD list |
| 4 | A node-local script overwrites nine generated files | **CONFIRMED, with a correction** | `/home/undi/ws/cfd_work/dm1528/patch_case.sh` on `gpu-claw`, 6137 bytes, mtime 2026-07-11. Exactly nine `cat >` overwrites, enumerated in D5 below. It also performs a **tenth** action the issue does not mention: `rm -f 0/k 0/nut 0/omega 0/epsilon`. The issue cites the script at `~/ws/cfd_work/...` but the script's own internal target is `C=~/cfd_work/dm1528/coupled_run/...`, a path that **does not exist** on the node — so the script as committed cannot run unmodified |
| 5 | Earlier slice-7 evidence came from a patched, not emitted, tree | **CONFIRMED, and stronger than filed** | See "The builder has never been validated" below |

**New finding, not in the issue — the defect is wider than `fvSolution`.**
`system/fvSchemes` is *also* not `interFoam`-runnable. `case_builder.py:218-228`
emits `divSchemes { default none; }` together with the **single-phase** forms
`div(phi,U)` and `div((nuEff*dev(T(grad(U)))))`. `interFoam` requires
`div(rhoPhi,U)` and `div(((rho*nuEff)*dev2(T(grad(U)))))`. Under `default none`
a missing `div` key is a fatal IO error of exactly the same class as the one in
the issue title. **Fixing `fvSolution` alone therefore moves the failure, it does
not remove it** — the case would die a second time, at `fvSchemes`. This is the
single most important correction in this plan and it reshapes the Deliverable
(D1).

`system/controlDict` (`case_builder.py:172`) is a third, softer gap: it emits
`adjustTimeStep no` and `maxCo 0.9` with no `maxAlphaCo` and no `maxDeltaT`. That
is not a start-up error, but a VOF run at fixed `deltaT` with no interface Courant
bound is not a case anyone can defend as runnable.

### The builder has never been validated — measured, not asserted

The issue says earlier evidence came from a patched tree. Sweeping the node
turns that into a number:

| measurement on `gpu-claw`, `~/ws/cfd_work` | value |
|---|---|
| `system/fvSolution` files across all case dirs | **139** |
| distinct content hashes among them | **18** |
| containing `cAlpha` (i.e. VOF-runnable) | **133** |
| **not** containing `cAlpha` (i.e. builder-shaped) | **6** |

The failing `vent_study` case carries the builder's exact three keys — `p`,
`"(k|omega|epsilon|alpha.water)"`, `"(k|omega|epsilon|alpha.water)Final"` — and
dies at start-up. A *successful* run,
`single_tank_amplitude/deg2/resp_fr_hl50_r100`, reached `End` with
`ExecutionTime = 181.25 s` using `"alpha.water.*"` with `nAlphaSubCycles 1;
cAlpha 1;` plus `p_rgh` / `p_rghFinal`.

Note carefully: those values are **not** `patch_case.sh`'s values, which are
`nAlphaSubCycles 3; cAlpha 1.5;` on a literal `alpha.water` key. So the script
named in the issue is **not** the source of the successful slice-7 evidence — it
is one of eighteen hand-authored variants in circulation, and not the winning one.

Two consequences, both load-bearing for this plan:

1. "The builder emits a runnable case" is **unproven**, exactly as the issue
   frames it. It is not a regression from a working state; there is no working
   state to regress from. Every green test today is consistent with a case that
   has never started.
2. **"Fold `patch_case.sh` back into the builder" is the wrong instruction**, and
   this plan does not follow it (D4). Promoting one of eighteen undocumented
   variants — and specifically one that did not produce the evidence — would be
   adopting a fitted constant with extra steps.

---

## Deliverable

A case builder whose emitted tree **starts and advances under the solver it
declares in `controlDict`**, proven by a real solver start rather than by a
rendering assertion — and a regression gate that fails when that stops being
true.

---

## Resource Intelligence Summary

### Existing repo code

- `src/digitalmodel/solvers/openfoam/templates/__init__.py` (103 lines) — all dict
  block constants. `FV_SOLUTION_SOLVERS:6-52` is **solver-agnostic**: the same
  `solvers` block is emitted for `simpleFoam` and `interFoam` alike. This is the
  root cause. `PIMPLE_BLOCK:54-61`, `SIMPLE_BLOCK:63-75`.
- `src/digitalmodel/solvers/openfoam/case_builder.py` (349 lines) —
  `_write_fv_solution:249-261` branches on `is_transient` for the *algorithm*
  block only and appends `FV_SOLUTION_SOLVERS` unconditionally at `:255`.
  `_write_fv_schemes:199-247` is not solver-aware at all beyond `ddtSchemes`.
  `_write_control_dict:172`.
- `src/digitalmodel/solvers/openfoam/models.py:283-289` — `_CASE_SOLVER_MAP`, five
  case types over **three** solvers (`interFoam`, `simpleFoam`, `pimpleFoam`).
  This is the set the contract must cover exhaustively.
- `src/digitalmodel/solvers/openfoam/smoke.py` — **already has the oracle
  machinery.** `_FATAL_MARKERS:22` is `("foam fatal error", "foam fatal io error")`
  and `_TIME:21` parses `Time = ` lines. `:46` already builds
  `("mpirun", "-np", N, "interFoam", "-parallel")`. This plan **reuses** it and
  builds no new runner.
- `src/digitalmodel/solvers/openfoam/runner.py` — fail-closed subprocess runner,
  `mesh_utility: str = "blockMesh"` at `:83`.
- `tests/solvers/openfoam/test_case_builder.py` — 0/`p_rgh` is already asserted
  at `:149-153`, so the **field** side is correct; only the **dictionary** side is
  wrong. `:156-162` pins "no `$p` shorthand", which constrains how `p_rghFinal`
  may be written.
- `tests/solvers/openfoam/validation/fixtures/sloshing_3d_case_sha256.json:20`
  pins a sha256 of `system/fvSolution`. It **will** break and must be regenerated
  deliberately, not silently.

### Gaps identified

No per-solver dictionary requirement exists anywhere in the repo. Nothing asserts
that a rendered case is consistent with the application named in its own
`controlDict`. No test in the suite has ever started a solver.

### Evidence

**Issue states** (2026-08-04): `#1959` OPEN (no labels) · `#1528` OPEN · `#1575`
**CLOSED — landed on `main` as `85c3c4af`** · `#1576` OPEN.

**Execution host verified 2026-08-04**: `gpu-claw` reachable over SSH
`BatchMode=yes`; OpenFOAM **v2312** at `/usr/lib/openfoam/openfoam2312`;
`interFoam`, `blockMesh`, `setFields` all resolve after sourcing
`etc/bashrc`; `nproc` = **8**.

---

## Design decisions

**D1 — The defect is "the emitted case is not runnable by its declared solver",
not "`fvSolution` is wrong".**
Premise verification found a second fatal error waiting in `fvSchemes`. A plan
scoped to the issue title would ship, close the issue, and leave the 144-case
matrix just as blocked — with the added harm that the issue would read as fixed.
Scope is therefore `fvSolution` **and** `fvSchemes` **and** the VOF entries of
`controlDict`. The acceptance oracle (D3) is what makes this scoping check
itself: if a fourth gap exists that this plan has not found, the solver start
will surface it before the PR merges.

**D2 — Requirements live in one declared per-solver contract.**
A `SolverDictContract` table keyed by solver name declares, per solver: the
`fvSolution` `solvers` keys that must be present, the `divSchemes` keys that must
be present, and the algorithm block. `case_builder` renders *from* it. It is a
static table of literals, not derived from the emitter.

**D3 — The contract cannot be its own oracle; a real solver start is.**
This is the load-bearing decision. If the regression test asserts "the emitted
dict contains the keys the contract lists" and the emitter renders from that same
contract, the test is **circular** — it cannot fail, and it would have passed on
`origin/main` had the contract been wrong in the same way. It proves consistency,
never correctness.

The gate is therefore two-layer, and the layers do different jobs:

- **Layer 1 (CI, fast, offline)** — unit tests pinning *literal* keys per solver
  (`cAlpha`, `p_rgh`, `div(rhoPhi,U)`, …). These are hand-written assertions about
  what `interFoam` needs, red on `origin/main`, green after. They are
  regression-locks, and they are honestly *not* independent of the author.
- **Layer 2 (`gpu-claw`, the oracle)** — start `interFoam` on a freshly emitted,
  unpatched case and require it to **advance at least one timestep**. This is the
  only layer whose correctness does not depend on the author having guessed the
  requirement list right, and it is the layer that would have caught the original
  defect. Nothing about it is CI-portable, and the plan says so rather than
  pretending otherwise (see Risks).

**D4 — Values are derived from the OpenFOAM v2312 upstream tutorial set, never
from the node's patch scripts.**
There are eighteen hand-authored `fvSolution` variants on `gpu-claw`;
`patch_case.sh` is one of them and is **not** the one that produced the
successful slice-7 runs. Adopting any node variant means adopting numbers whose
provenance is "someone tuned this once", which is a fitted constant. The
reference is instead `$FOAM_TUTORIALS/multiphase/interFoam/laminar/damBreak`
from the pinned v2312 installation — a named, versioned, externally-owned input
that can be cited and re-derived. Where the tutorial and a node variant disagree,
the tutorial wins and the divergence is recorded.

`cAlpha` in particular is **not** free tuning: `cAlpha = 1` is the standard
interface-compression coefficient for MULES and is what the successful run used.
The plan adopts `1`, cited to the tutorial, and does **not** adopt
`patch_case.sh`'s `1.5`.

**D5 — `patch_case.sh`'s nine overwrites classify into three buckets, and only
one folds back.**

*Bucket A — builder defects. Fold into the builder.* These are wrong for
**every** `interFoam` case, not just this one:
- `system/fvSolution` — the headline defect.
- `system/fvSchemes` — the second fatal error found by premise verification.

*Bucket B — case-definition expressiveness gaps. Fold in as declarable inputs,
never as hardcoded values.* The script had to hand-write these only because the
case definition cannot express the choice. Hardcoding the script's answer would
break every case that wants the other answer:
- `constant/turbulenceProperties` — the script forces `laminar` and deletes
  `0/k 0/nut 0/omega 0/epsilon`. The builder can only emit RAS. **Turbulence type
  must become selectable**, and the `0/` field set must follow the selection.
- `0/U`, `0/p_rgh`, `0/alpha.water` — these are rewritten only because the script
  also collapses the mesh to a single closed `walls` patch. They are downstream of
  Bucket C's geometry choice, not independent defects.
- `system/controlDict`'s `adjustTimeStep` / `maxAlphaCo` / `maxDeltaT` — VOF
  stability controls the builder cannot currently emit at all.

*Bucket C — genuinely node-local. Leave on the node; do not fold in.*
- `system/blockMeshDict` — a closed single-patch tank is a **modelling decision**
  specific to the roll-only fallback, not a builder defect. The builder's
  multi-patch topology is correct for the general case.
- `system/decomposeParDict` — `numberOfSubdomains 8` is `gpu-claw`'s core count
  (`nproc` = 8, verified). Rank count is a property of the host, not of the case.
- `system/controlDict`'s `endTime` / `writeInterval` — driven by env `ENDT`/`WI`;
  a run-window choice.
- The `functions {}` block — evidence-gathering for one study.

**Disposition of the script itself:** once Bucket A and B land,
`patch_case.sh` is retired rather than maintained. It is not in this repository,
so this plan cannot delete it; the deliverable is that **it is no longer needed**,
demonstrated by the D3 solver start running against an unpatched tree. Retiring
the file on the node is a follow-on task for whoever owns [#1528](https://github.com/vamseeachanta/digitalmodel/issues/1528)'s run
lane, and is named in Out of Scope rather than silently assumed.

**D6 — Every future solver must declare a contract or the suite fails.**
A coverage test asserts that the contract's key set equals `_CASE_SOLVER_MAP`'s
value set exactly. Adding a fourth solver without declaring its requirements is
then a red test, not a silent hole. The test asserts the set is **non-empty and
equal to the known three** — an equality against a pinned literal set, so it
cannot pass vacuously if the map is emptied or the contract is emptied.

**D7 — No fitted constants.** Every number this plan introduces traces to a named
input:
- `cAlpha 1`, `nAlphaCorr 1`, `nAlphaSubCycles 1`, MULES entries, `p_rgh` /
  `p_rghFinal` tolerances — the v2312 `damBreak` tutorial (D4).
- `maxAlphaCo` — the interface Courant bound. Set **equal to `maxCo`**, which the
  builder already carries as a declared case input at `0.9`. Deriving one from
  the other means this plan introduces no new number at all; it does not invent a
  second, differently-tuned bound.
- The solver-start acceptance threshold is **one advanced timestep**, which is the
  minimum non-vacuous value and is not tunable.

---

## Pseudocode

```
# --- the contract: a static declaration, not derived from the emitter ---
SOLVER_DICTS = {
  "interFoam": SolverDictContract(
      fv_solution_keys = ("alpha.water", "p_rgh", "p_rghFinal", "U"),
      div_keys         = ("div(rhoPhi,U)", "div(phi,alpha)", "div(phirb,alpha)",
                          "div(((rho*nuEff)*dev2(T(grad(U))))"),
      algorithm        = "PIMPLE",
      needs_alpha_courant = True),
  "pimpleFoam": SolverDictContract(... algorithm="PIMPLE", needs_alpha_courant=False),
  "simpleFoam": SolverDictContract(... algorithm="SIMPLE", needs_alpha_courant=False),
}

# --- emission renders FROM the contract ---
function write_fv_solution(system_dir, solver):
    contract = SOLVER_DICTS[solver]          # KeyError = unsupported solver, fail closed
    emit(header + render_solvers(contract) + render_algorithm(contract) + footer)

function write_control_dict(system_dir, case, solver):
    ...
    if SOLVER_DICTS[solver].needs_alpha_courant:
        emit("adjustTimeStep yes")
        emit(f"maxAlphaCo {case.max_co}")     # D7: derived from an existing input
        emit(f"maxDeltaT  {case.max_delta_t}")

# --- Layer 2 oracle: reuse smoke.py, add no new runner ---
function assert_case_starts(case_dir, solver, timeout):
    run(blockMesh); run(setFields if multiphase)
    log = run(solver)                                    # serial; ranks are host-local
    if any(m in log.lower() for m in smoke._FATAL_MARKERS):   # smoke.py:22
        raise NotRunnable(first_fatal_block(log))
    times = smoke._TIME.findall(log)                          # smoke.py:21
    if len(times) < 2 or float(times[-1]) <= float(times[0]):
        raise NotRunnable("solver started but advanced no timestep")
    return times[-1]
```

The `len(times) < 2` guard is deliberate: a solver that prints `Time = 0` and
exits cleanly produces **no fatal marker**, and a naive "no FATAL in log" check
would call that a pass. That is the vacuous-result trap for this plan, and this
is where it is closed.

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/solvers/openfoam/solver_contracts.py` | D2: `SolverDictContract` + `SOLVER_DICTS` for the three solvers in `_CASE_SOLVER_MAP` |
| Modify | `src/digitalmodel/solvers/openfoam/templates/__init__.py` | split `FV_SOLUTION_SOLVERS:6-52` into per-solver blocks; add VOF `alpha.water` (MULES), `p_rgh`, `p_rghFinal`, `pcorr.*`; add VOF `divSchemes` |
| Modify | `src/digitalmodel/solvers/openfoam/case_builder.py` | `_write_fv_solution:249-261` and `_write_fv_schemes:199-247` render from the contract; `_write_control_dict:172` emits `adjustTimeStep`/`maxAlphaCo`/`maxDeltaT` when the contract asks |
| Modify | `src/digitalmodel/solvers/openfoam/models.py` | D5 Bucket B: turbulence type becomes selectable (`laminar` \| RAS) and drives the `0/` field set; `max_delta_t` input |
| Create | `tests/solvers/openfoam/test_solver_contracts.py` | D6 coverage guard + per-solver literal-key assertions |
| Create | `tests/solvers/openfoam/test_case_runnable.py` | D3 Layer 2, behind `@pytest.mark.requires_openfoam`, skipped where no solver exists |
| Modify | `tests/solvers/openfoam/test_case_builder.py` | extend; `:156-162` no-shorthand constraint must still hold for `p_rghFinal` |
| Regenerate | `tests/solvers/openfoam/validation/fixtures/sloshing_3d_case_sha256.json` | `:20` pins the old `fvSolution` hash; regenerate **deliberately**, with the new hashes justified in the commit body |
| Create | `docs/reports/2026-08-04-issue-1959-solver-start-evidence.html` | the `gpu-claw` run log and its provenance, per the repo's HTML-default rule |
| Update | `docs/plans/README.md` | index row |

**Explicitly untouched**, to stay clear of live lanes:
`src/digitalmodel/hydrodynamics/diffraction/`, `tests/hydrodynamics/diffraction/`,
`docs/benchmarks/unit_box/`, and `post_processing.py` / VTK artifact emission
(owned by [#1576](https://github.com/vamseeachanta/digitalmodel/issues/1576)).

---

## TDD Test List

Every row states the expected value and why it is red on `origin/main` @ `85c3c4af`.

| Test | Input | Expected | Red today because |
|---|---|---|---|
| `test_interfoam_fvsolution_has_dedicated_alpha_water` | `CaseType.SLOSHING` | emitted `fvSolution` has an `alpha.water` block containing `cAlpha` | today `alpha.water` appears only inside `"(k|omega|epsilon|alpha.water)"`; `cAlpha` appears nowhere |
| `test_interfoam_fvsolution_has_p_rgh_and_final` | `CaseType.SLOSHING` | `p_rgh` **and** `p_rghFinal` blocks present | only `p` / `pFinal` exist (`templates:9-22`) |
| `test_interfoam_fvsolution_drops_p_solver` | `CaseType.SLOSHING` | no bare `p` solver block — `interFoam` never solves `p` | `p` is emitted unconditionally at `templates:9` |
| `test_interfoam_fvschemes_has_rhophi_div` | `CaseType.SLOSHING` | `div(rhoPhi,U)` present | **the second fatal error**; today `div(phi,U)` only (`case_builder:221`) |
| `test_interfoam_fvschemes_has_two_phase_stress_div` | `CaseType.SLOSHING` | `div(((rho*nuEff)*dev2(T(grad(U))))` present | today the single-phase `div((nuEff*dev(T(grad(U)))))` at `:225` |
| `test_simplefoam_fvsolution_keeps_p_and_has_no_alpha` | `CaseType.CURRENT_LOADING` | `p` present, `cAlpha` **absent**, `SIMPLE` block | guards over-correction — must not leak VOF entries into single-phase cases |
| `test_interfoam_controldict_bounds_alpha_courant` | `CaseType.SLOSHING` | `adjustTimeStep yes` and `maxAlphaCo` equal to the case's `maxCo` | today `adjustTimeStep no`, no `maxAlphaCo` |
| `test_contract_covers_every_mapped_solver` | `_CASE_SOLVER_MAP` | `set(SOLVER_DICTS) == {"interFoam","simpleFoam","pimpleFoam"}` **and** equals the map's value set | contract does not exist. Pinned literal set — cannot pass on an emptied map |
| `test_unknown_solver_fails_closed` | solver `"fooFoam"` | raises, does not emit a default dict | today any solver silently gets the single-phase block |
| `test_laminar_selection_omits_rans_fields` | turbulence `laminar` | no `0/k`, `0/nut`, `0/omega`; `turbulenceProperties` says `laminar` | D5 Bucket B; not selectable today |
| `test_emitted_case_starts_and_advances` *(requires_openfoam)* | freshly emitted `SLOSHING` case, **unpatched** | no fatal marker **and** ≥2 distinct `Time =` values with the last strictly greater than the first | **the whole point** — dies at `Entry 'cAlpha' not found` today |
| `test_started_but_stalled_case_is_not_a_pass` | a synthetic log with one `Time = 0` and no fatal | helper raises `NotRunnable` | the anti-vacuity guard; no such helper exists |

**Not included, deliberately:** no test asserting `momentumPredictor` is present
(premise 3 — it is optional and its absence is not a defect); no test asserting
the emitted dict equals the contract (D3 — circular by construction); no test
pinning `patch_case.sh`'s values (D4).

---

## Execution environment

Implementation runs in a dedicated worktree off `origin/main`. The shared
checkout is on an unrelated branch and is **not** used. The root filesystem is
full, so worktrees live on the large data volume; the scratchpad cannot hold
another 2.8 G checkout.

Layer 2 runs on `gpu-claw`, verified 2026-08-04:

| check | result |
|---|---|
| SSH `BatchMode=yes` | reachable |
| OpenFOAM | v2312 at `/usr/lib/openfoam/openfoam2312` |
| `interFoam` / `blockMesh` / `setFields` | resolve after `source etc/bashrc` |
| cores | `nproc` = 8 |

Layer 2 runs **serially**, not under `mpirun`. The original failure was an
`IOerror` during dictionary read, which is rank-independent; serial execution
removes the MPI variable from the oracle and keeps the run inside a short
timeout. Parallel decomposition remains [#1576](https://github.com/vamseeachanta/digitalmodel/issues/1576)'s concern.

---

## Acceptance Criteria

- [ ] **Every test above fails on `origin/main` @ `85c3c4af` and passes after.** The failure list is captured by running the new test files against a clean `origin/main` worktree and recorded in the PR body. Any row green before the change is removed or rewritten.
- [ ] `pytest tests/solvers/openfoam/ -q` compared **node-ID by node-ID** against a baseline captured in the same worktree at the branch point. No new failure node IDs. (Counts alone are not evidence.)
- [ ] **A freshly emitted, unpatched `interFoam` case starts on `gpu-claw` and advances at least one timestep.** Evidence is the solver log, showing ≥2 distinct `Time =` values with the last strictly greater than the first, and no `FOAM FATAL` marker. The log is committed under `docs/reports/`. A `blockMesh`/`setFields` `rc=0` **does not** satisfy this criterion, and neither does a clean exit at `Time = 0`.
- [ ] The case used for that run is emitted by `OpenFOAMCaseBuilder` and **no file in it is modified between emission and solver start** — asserted by recording a sha256 manifest of the emitted tree immediately after `build()` and re-verifying it immediately before the solver is invoked. This is what makes the run evidence about *the builder* rather than about a tree someone touched.
- [ ] `set(SOLVER_DICTS)` equals the pinned literal `{"interFoam","simpleFoam","pimpleFoam"}` **and** equals `set(_CASE_SOLVER_MAP.values())`.
- [ ] A `simpleFoam` case still emits `p` and contains no `cAlpha` — the fix does not leak VOF entries into single-phase cases.
- [ ] `sloshing_3d_case_sha256.json` is regenerated, and the commit body states which files' hashes changed and why.
- [ ] **No numeric value introduced by this PR is taken from `patch_case.sh` or from any case directory on `gpu-claw`.** Each traces to the v2312 tutorial or to an existing declared case input (D7).
- [ ] **No legal-scan criterion is stated, and its absence is deliberate.** `scripts/legal/legal-sanity-scan.sh` does not exist in this repository (verified 2026-08-04; it lives in `workspace-hub`, `worldenergydata`, `llm-wiki` and others, but not here). Both invocations of workspace-hub's copy are unusable for a digitalmodel worktree: the `--repo=<name>` form is **fail-open** under OPEN workspace-hub issue [#3804](https://github.com/vamseeachanta/workspace-hub/issues/3804) — it resolves an empty scan path, scans nothing and exits 0 — and the root form scans workspace-hub rather than this repo. An earlier revision of this plan cited the `--repo=` form as returning `PASS`; **that PASS was the fail-open**, and citing it would have been evidence of nothing. This work touches solver dictionaries and carries no client-identifier surface, so no substitute gate is proposed here; [#1965](https://github.com/vamseeachanta/digitalmodel/issues/1965)'s plan carries the repo-local detector for work that does.
- [ ] r1 review artifact recorded.

---

## Out of scope

- **The 144-case matrix run itself** — [#1528](https://github.com/vamseeachanta/digitalmodel/issues/1528). This plan unblocks it; it does not run it.
- **Deleting `patch_case.sh` from `gpu-claw`** — the file is not in this repository. This plan makes it unnecessary and says so; retiring it belongs to [#1528](https://github.com/vamseeachanta/digitalmodel/issues/1528)'s run lane.
- **Re-validating slice-7 evidence already posted on [#1528](https://github.com/vamseeachanta/digitalmodel/issues/1528)** — that evidence came from hand-authored trees and its status is a question for [#1528](https://github.com/vamseeachanta/digitalmodel/issues/1528), not a code change here. This plan's finding that eighteen variants exist should be carried there.
- **MPI/parallel post-processing and VTK artifacts** — [#1576](https://github.com/vamseeachanta/digitalmodel/issues/1576).
- **Mesh topology and `blockMeshDict`** — D5 Bucket C. The closed single-patch tank is a modelling choice.
- **Physical validation of results.** This plan proves a case *runs*. Whether its answers are right is a validation question, and nothing here should be read as evidence of solution accuracy.

---

## Adversarial Review Summary

| Round | Provider | Verdict |
|---|---|---|
| r1 | Claude — inline, main session | **MAJOR** — 6 findings, all folded in below (finding 6 amended after a later measurement) |

r1 findings against this plan's own earlier draft:

1. **Scoping to the issue title would have shipped a still-broken builder.** The
   draft followed the title and touched only `fvSolution`. Empirical check of the
   emitted `fvSchemes` found a second fatal error of the same class. → D1; two new
   TDD rows. **This is the finding that mattered most**; without it the plan's
   central claim would have been false.
2. **The proposed regression test was circular.** "Emitted dict contains the
   contract's keys" cannot fail when the emitter renders from the contract. → D3
   two-layer split; the solver start became the oracle and the dict test was
   demoted to a regression-lock with that limitation stated.
3. **"No FATAL in the log" is satisfiable by a vacuous result.** A solver printing
   `Time = 0` and exiting produces no fatal marker. → the `len(times) < 2` guard,
   the "≥2 distinct `Time =`" acceptance criterion, and
   `test_started_but_stalled_case_is_not_a_pass`.
4. **"Fold `patch_case.sh` back in" would have imported a fitted constant.**
   Measurement found eighteen variants on the node, and the script is not the one
   that produced the successful evidence (`cAlpha 1.5` vs the winning `1`). → D4;
   the tutorial became the reference and an acceptance criterion forbids node-derived
   numbers.
5. **The solver-start criterion could be satisfied by a patched tree** — the exact
   failure mode this whole issue is about, reproduced inside its own fix. → the
   sha256-manifest-before-and-after acceptance criterion.

6. **The legal-scan criterion named a path that does not exist in this repo, and
   its replacement was worse.** The draft wrote
   `scripts/legal/legal-sanity-scan.sh --diff-only`, copied from habit; `ls` says
   it is absent from digitalmodel. The first correction substituted workspace-hub's
   copy via `--repo=` and recorded that it returned `PASS`. **That PASS was a
   fail-open** — workspace-hub issue [#3804](https://github.com/vamseeachanta/workspace-hub/issues/3804) (OPEN) documents that the
   per-repo form resolves an empty scan path, scans nothing and exits 0; it was
   independently observed returning `PASS` over a worktree containing a known
   live leak while planning [#1965](https://github.com/vamseeachanta/digitalmodel/issues/1965). The criterion is now **withdrawn entirely**,
   with the reason recorded so it is not restored.

   Two distinct defect classes, one after the other: first a criterion that
   **cannot be run** (the same shape as the Sphinx-build criterion filed in
   another plan today), then a criterion that **runs and proves nothing**. The
   second is more dangerous, because it produces a green tick.

One earlier draft criterion was **withdrawn** as not executable: "the case runs to
completion under `mpirun -np 8`". A full VOF run is minutes-to-hours, is not a
gate, and imports the MPI variable into an oracle for a rank-independent IO error.
Replaced by the serial one-timestep criterion.

**Verdict: ready for owner review.** No blockers outstanding.

---

## Risks and Open Questions

- **Layer 2 is not CI-portable, and this plan does not pretend otherwise.** The
  only non-circular test requires an OpenFOAM installation, so it runs on
  `gpu-claw` and is `skipif`-guarded elsewhere. A skipped test proves nothing —
  so the acceptance criterion demands a **committed log artifact**, not a green
  tick. Residual risk: a future regression is caught by Layer 1 only, which is
  weaker. Making Layer 2 a scheduled job on `gpu-claw` is the durable answer and
  is a candidate follow-on issue, not scope here.
- **Risk — the tutorial reference is a version-pinned input.** The v2312
  `damBreak` values are correct for v2312. If the node upgrades, the reference
  moves. The contract records the version it was derived from so the coupling is
  visible rather than silent.
- **Risk — turbulence selectability (D5 Bucket B) is the largest single change**
  and touches `models.py`, the `0/` field writers, and `turbulenceProperties`. If
  its blast radius proves wider than expected during implementation, it is the
  natural split point for a follow-on issue; the `fvSolution`/`fvSchemes` fix
  stands alone without it.
- **Open question for the owner — what happens to the slice-7 evidence on
  [#1528](https://github.com/vamseeachanta/digitalmodel/issues/1528)?** This plan
  establishes that it came from hand-authored trees and that eighteen variants
  exist. Whether that invalidates the posted conclusions is a call this plan does
  not make and cannot make for [#1528](https://github.com/vamseeachanta/digitalmodel/issues/1528)'s owner.

---

## Complexity: T2

One new module, three modified modules, two new test files, one regenerated
fixture. No licensed seat. One remote host, already verified reachable, for a
single short run. Not T3: no cross-repo coordination and no systemic change.
