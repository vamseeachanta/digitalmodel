# Plan for #1574: Source-Neutral Sloshing Privacy Cleanup

> **Status:** revised draft — scope split into neutralization-only; user approval required
> **Complexity:** T3
> **Date:** 2026-07-13 (revised 2026-08-04)
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1574
> **Client:** N/A
> **Lane:** lane:codex
> **Review artifacts:** `scripts/review/results/2026-07-13-plan-1574-r{1,2}-consolidated.md`
> **Deferred half:** https://github.com/vamseeachanta/digitalmodel/issues/1961

---

## Revision Note — 2026-08-04

The plan approved at `a5852b35` will be superseded by this revision. Approval of
the earlier revision does not carry forward; this revision requires its own
explicit user approval.

**Why the split will happen.** The approved plan is blocked at its own first
acceptance criterion, which requires workspace-hub
[#3522](https://github.com/vamseeachanta/workspace-hub/issues/3522) Phase B to be
approved and provisioned. Live readback on 2026-08-04 shows Phase A merged
(PR [#3535](https://github.com/vamseeachanta/workspace-hub/pull/3535), merge commit
`966401108`) but never activated: the `legal-rule-authority` environment carries
`protection_rules: []`, `deployment_branch_policy: null`, zero secrets, and an
`updated_at` still equal to its `created_at` of `2026-07-14T12:54:18Z`. The
corrective issue [#3544](https://github.com/vamseeachanta/workspace-hub/issues/3544)
remains OPEN; its PR [#3590](https://github.com/vamseeachanta/workspace-hub/pull/3590)
merged activation *tooling* only. Phase B is neither approved nor started.

Only the enforcement half of this issue needs that authority. Neutralizing the
reusable modules needs none.

**What will move out.** The authenticated fail-closed scanner, its versioned
scope manifest, its hostile-case tests, the protected maintainer workflow and
fork-CI separation, and every acceptance criterion that depends on a provisioned
authority will move to
[#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961), which will
carry `status:needs-plan` and remain hard-gated on Phase B.

**What will stay.** Removing the protected identifier classes from the reusable
sloshing/OpenFOAM modules, their tests, their generator scripts, and the tracked
artifacts those generators emit — plus the module-size dispositions for the files
this work will touch.

**The [#1575](https://github.com/vamseeachanta/digitalmodel/issues/1575) boundary
reasoning.** The approval comment on
[#1575](https://github.com/vamseeachanta/digitalmodel/issues/1575), recorded
2026-07-14, states its scope verbatim as *"dependency-blocked on merged issues
1565 and 1574."*
[#1565](https://github.com/vamseeachanta/digitalmodel/issues/1565) closed on
2026-07-14, so a merged [#1574](https://github.com/vamseeachanta/digitalmodel/issues/1574)
is the sole remaining gate on that chain. A neutralization-only
[#1574](https://github.com/vamseeachanta/digitalmodel/issues/1574) that merges
will satisfy that boundary literally, unblocking
[#1575](https://github.com/vamseeachanta/digitalmodel/issues/1575) and, behind it,
[#1576](https://github.com/vamseeachanta/digitalmodel/issues/1576) — both
plan-approved, both waiting on an idle dedicated CFD node. The split will not
weaken that boundary: the reachable public export that
[#1575](https://github.com/vamseeachanta/digitalmodel/issues/1575) would otherwise
inherit is removed by the neutralization half, not the deferred half.

**Residual risk the owner is accepting.** Until
[#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961) lands there
will be no durable fail-closed CI gate on this identifier class. Verification of
the neutralization will be point-in-time rather than continuous, so a later
commit could reintroduce the class undetected. This risk is stated explicitly in
Risks and Open Questions and is the principal cost of proceeding in halves.

---

## Resource Intelligence Summary

### Existing code and exposure

- `src/digitalmodel/solvers/openfoam/__init__.py` publicly imports and exports a
  project-coded pressure-tap helper from the reusable package surface.
- `src/digitalmodel/solvers/openfoam/pressure_taps.py` and its tests contain the
  corresponding helper, project-shaped defaults, labels, and examples.
- `src/digitalmodel/solvers/openfoam/sloshing_coupling.py`, validation modules,
  scripts, tests, generated documentation, and tracked plans are additional
  reachable surfaces requiring an inventory rather than a one-symbol rename.
- A repository search confirms the identifier class is already reachable from
  public imports. This plan deliberately does not repeat the sensitive literal
  spellings or private job context.
- The coded factory has **no caller anywhere in the repository** outside its own
  module, the package `__init__`, and its own test module. The intentional public
  break will therefore require no in-repo adaptation, which strengthens the
  decision to ship no compatibility alias.
- Existing legal-scanning documentation allows narrowly justified forensic
  fixtures. The deferred cleanup scanner will therefore use line-level sentinels
  and will not exempt whole files or silently skip its own test artifacts.
- The public deny-list carries neither identifier class, and the cross-repository
  legal scan passes against the unmodified tree. That scan cannot substitute for
  this work; it passes precisely because the protected values cannot be placed in
  a public list.

### Two identifier classes will be in scope

The read-only inventory will treat two distinct classes. Counts are reported
without values, and no value will appear in any artifact of this issue.

| Class | Genuine files | Nature |
|---|---|---|
| Project code | 10 | a short coded token in symbols, defaults, docstrings, labels and one generated artifact |
| Organization / partner name | 6 | prose in module docstrings, a generator, and one generated artifact |

The union will be **11 tracked files**, of which **2 will be tracked generated
artifacts** and **9 will be hand-authored source, tests or generator scripts**.

Two false-positive findings will shape the work rather than the file list. A
lockfile hit for the project-code class sits inside a content hash, and a hit in
an unrelated provisioning script for the organization class is an operating-system
release codename. Neither will be edited. The full matching-boundary evidence,
including the discovery that a strict word-boundary matcher fails open on the
public export itself, will be carried by
[#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961).

### Governing dependencies and boundaries

- Workspace-hub [#3522](https://github.com/vamseeachanta/workspace-hub/issues/3522)
  will own the authenticated private rule authority needed to compare sensitive
  values without committing those values to this public repository. That
  dependency will belong entirely to
  [#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961); this issue
  will not depend on it, will not invent a second secret registry, and will not
  expose reversible encodings.
- Public source, fixtures, review artifacts, commit messages, issue comments,
  generated docs, and reports will contain neutral synthetic terminology only.
- Compatibility will preserve generic behavior, not a sensitive public symbol.
  Removing a project-coded import is an intentional privacy break; a deprecated
  alias would perpetuate the disclosure and is therefore out of scope.
- No private geometry, values, paths, queue records, or result excerpts will be
  copied into this repository or its GitHub metadata.
- Two branches for [#1888](https://github.com/vamseeachanta/digitalmodel/issues/1888)
  will already be modifying `scripts/capabilities/build_sloshing_explorer.py` and
  its emitted page. The coordination rule for that overlap is stated under Files
  to Change.

### Gap and reproduction

The following value-withholding probe reproduces the public-namespace defect at
`origin/main` without printing the coded symbol or using private data:

```bash
PYTHONPATH=src uv run python -c \
  "import digitalmodel.solvers.openfoam as m; print(sum(n.endswith('_default_taps') for n in m.__all__))"
# exact output: 1
```

Current tests do not enforce a neutral public namespace or generated-artifact
closure.

Distinct sources: issue #1574; package exports; pressure-tap module and tests;
coupling and validation modules; CFD scripts; capability-page generator; legal
deny-list and scan; workspace-hub #3522 environment readback; documentation
generators and their tracked outputs (9+).

## Artifact Map

| Artifact | Path |
|---|---|
| Plan | `docs/plans/2026-07-13-issue-1574-sloshing-privacy-cleanup.md` |
| Neutral tap API | `src/digitalmodel/solvers/openfoam/pressure_taps.py` |
| Public exports | `src/digitalmodel/solvers/openfoam/__init__.py` |
| Reusable sloshing code | `src/digitalmodel/solvers/openfoam/validation/`; `sloshing_coupling*.py` |
| Generator scripts | `scripts/cfd/run_sloshing_3d_benchmark.py`; `scripts/capabilities/build_sloshing_explorer.py` |
| Tracked generated artifacts | `docs/api/cfd/sloshing-3d-benchmark.json`; `docs/api/structural/sloshing-explorer.html` |
| API/regression tests | `tests/solvers/openfoam/` |
| Review evidence | `scripts/review/results/2026-07-13-plan-1574-*.md` |
| Deferred enforcement half | https://github.com/vamseeachanta/digitalmodel/issues/1961 |

## Deliverable

The reusable sloshing/OpenFOAM surface will use source-neutral names, synthetic
defaults and neutral prose across source, tests, generator scripts and every
tracked artifact those generators emit, verified by value-free shape assertions
in the repository and by an in-session point-in-time confirmation that records
counts only.

## Privacy and Compatibility Contract

The cleanup will classify every finding before editing:

```text
public_api_symbol | default_value | docstring | fixture | output_label |
generated_artifact | forensic_reference
```

The `git_metadata` class from the earlier revision will move to
[#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961) together with
the scanner entry points that were its only mechanism. Commit-message and branch
name hygiene will remain a standing discipline of this issue, enforced by review
rather than by an automated gate.

The public model will remain `PressureTap(name, location=None, patch=None,
fields=("p",), operation="areaAverage")` in `pressure_tap_models.py`, with the
same point/patch/surface validation and export from `openfoam.__init__`. The
coded factory will be replaced by
`rectangular_tank_wall_taps(*, tank_length_m, tank_width_m,
tap_elevations_m, fields=("p","p_rgh")) -> tuple[PressureTap,...]` in
`pressure_taps.py`; it validates finite positive dimensions, strictly interior
finite elevations, and emits deterministic neutral `wall_<n>` names. No defaults
encode a real geometry. #1575 will consume only this exact `PressureTap` model.
Sensitive aliases, re-exports, deprecation warnings, migration maps, and
changelog spellings will not be committed.

### Generated artifacts will be fixed at their generator

Two tracked artifacts will carry a protected identifier. The generator will be
corrected first in both cases, because applying a migration to a page instead of
its template is the exact defect recorded in
[#1888](https://github.com/vamseeachanta/digitalmodel/issues/1888) and
[#1903](https://github.com/vamseeachanta/digitalmodel/issues/1903), where every
regeneration silently reverted the migration. This plan will not repeat it.

Regeneration itself, however, will **not** be the verification mechanism, for two
separately verified reasons:

- `scripts/cfd/run_sloshing_3d_benchmark.py` is a measurement harness. Its
  manifest embeds the executing hostname, core count and wall-clock scaling
  timings from a live OpenFOAM MPI run. Re-running it requires the solver and the
  dedicated CFD node, and cannot be byte-reproducible even then. Requiring a
  re-run would re-block this issue on the very compute node the split exists to
  release.
- `scripts/capabilities/build_sloshing_explorer.py` **already** fails a
  regeneration round-trip at `origin/main`, before any change from this issue: a
  clean re-run rewrites the page's `data-theme` attribute, drops the shared brand
  stylesheet link and restores the pre-migration inline token block. That is the
  drift catalogued in
  [#1903](https://github.com/vamseeachanta/digitalmodel/issues/1903), it is
  unrelated to identifiers, and adopting it as an acceptance criterion here would
  import that remediation wholesale into this issue's scope.

The mechanism will instead be **generator-literal coupling**. In both generators
the identifier lives in a static module-level string, not in computed output. The
work will neutralize that literal, apply the identical substitution to the
corresponding field of the tracked artifact, and add a test asserting that the
artifact's field equals the generator's literal. That test gives the anti-drift
property [#1888](https://github.com/vamseeachanta/digitalmodel/issues/1888) is
about — output cannot silently diverge from its generator — without executing a
solver and without depending on
[#1903](https://github.com/vamseeachanta/digitalmodel/issues/1903). The artifact
edit will be shown in the pull request as a mechanical substitution derived from
the generator change, never as free-hand editing.

### Verification without the deferred scanner

Because the fail-closed scanner will move to
[#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961), this issue
will verify in two tiers, and neither tier will require a protected value to
exist in the repository.

**Tier 1 — durable, value-free, committed.** Shape and identity assertions that
never name the protected values: the reproduction probe inverted to expect `0`;
an exact-tuple assertion on `openfoam.__all__` so that any reintroduced export
fails the test by construction; positive assertions on the neutral factory's
signature, validation and emitted names; and a byte-identical round-trip
assertion for each regenerated artifact. These will be permanent regression
tests.

**Tier 2 — point-in-time, in-session, not committed.** The implementation will
capture both class patterns in step 1, before any edit, from the tree itself —
the values are present at `origin/main`, which is the defect. The same captured
patterns will then be replayed against the finished tree to confirm zero
remaining occurrences. This closes the loop inside one session and introduces no
dependency on the owner, on a provisioned authority, or on any out-of-band value
handoff. Evidence will be recorded as counts and file totals only, in the issue
comment, with no value and no context line.

Tier 1 will not detect a *newly invented* leak of a value it does not know.
That gap is real, is the reason
[#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961) exists, and
will not be papered over by claiming this issue closes the enforcement question.

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Split/modify | `src/digitalmodel/solvers/openfoam/pressure_taps.py`; new `pressure_tap_models.py`; new `pressure_tap_analysis.py` | neutral API and reduce current 629-line module |
| Split/modify | `src/digitalmodel/solvers/openfoam/sloshing_coupling.py`; new `sloshing_coupling_models.py` | neutralize reusable models and reduce current 682-line module before #1578 |
| Split/modify | `src/digitalmodel/solvers/openfoam/validation/sloshing_2d.py`; new sibling module(s) by responsibility | neutralize coded default and prose, and reduce current 1007-line module |
| Split/modify | `src/digitalmodel/solvers/openfoam/validation/sloshing_sweep.py`; new sibling module(s) by responsibility | neutralize coded defaults and prose, and reduce current 485-line module |
| Modify | `src/digitalmodel/solvers/openfoam/__init__.py` | remove the coded export from imports and `__all__` |
| Modify | `scripts/cfd/run_sloshing_3d_benchmark.py` | neutralize the coded comment and the emitted geometry label |
| Modify (bounded) | `scripts/capabilities/build_sloshing_explorer.py` | neutralize identifier-bearing prose only; see coordination rule below |
| Derive | `docs/api/cfd/sloshing-3d-benchmark.json` | one field, substituted mechanically from the corrected generator literal; not regenerated, because the harness requires a live solver run |
| Derive | `docs/api/structural/sloshing-explorer.html` | one prose region, substituted mechanically from the corrected generator literal; not regenerated, because the page already drifts from its generator under #1903 |
| Split/modify | `tests/solvers/openfoam/test_pressure_taps.py`; `test_sloshing_coupling.py`; new focused test modules | synthetic fixtures; reduce current 410-line coupling test |
| Regenerate | outputs named by `uv run python -m sphinx -W --keep-going docs docs/_build/html` | clean public API documentation |

`scripts/setup/provision-cfd-box.sh` appeared in the earlier revision and will be
**removed from scope**. Its four apparent hits are the Ubuntu 24.04 release
codename, not the organization name. Editing it would be a false-positive-driven
change to an unrelated provisioner.

### Module-size dispositions

Every modified implementation file will remain at or below 400 lines and every
function at or below 50 lines, with one named exemption.

- `validation/sloshing_2d.py` at 1007 lines and `validation/sloshing_sweep.py` at
  485 lines will be **split by responsibility**, not grandfathered. Both are
  reusable library modules and are directly within the decoupling intent of this
  issue.
- `scripts/capabilities/build_sloshing_explorer.py` at 865 lines will be
  **explicitly exempted** from the size limit for this issue. Its bulk is a single
  embedded HTML template string rather than logic; the change here will be three
  prose strings; and two in-flight branches for
  [#1888](https://github.com/vamseeachanta/digitalmodel/issues/1888) are
  concurrently rewriting that same template, so splitting the file now would
  create a large avoidable conflict for no privacy benefit. The size question for
  the capability generators will be carried by
  [#1903](https://github.com/vamseeachanta/digitalmodel/issues/1903). This
  exemption will be named in the implementation summary, not left implicit.

### Coordination rule for the explorer generator

Both `fix/1888-generator-brand-drift` and `codex/1888-generator-drift` will
already touch `scripts/capabilities/build_sloshing_explorer.py`,
`docs/api/structural/sloshing-explorer.html` and its sidecar JSON. Their hunks
sit in the template head and CSS block; the identifiers sit in the module
docstring and body prose. Both branches will still carry the identifiers, so
[#1888](https://github.com/vamseeachanta/digitalmodel/issues/1888) will not
resolve them. The rule for this issue will be: touch only the identifier-bearing
lines in that generator, take no structural change to it, and re-run the
generator-literal coupling test after any rebase onto a landed
[#1888](https://github.com/vamseeachanta/digitalmodel/issues/1888) — which is the
check that will catch a rebase silently reintroducing the old prose.

## TDD Test List

| Test | Verification |
|---|---|
| `test_coded_export_absent` | removed public symbol cannot be imported and the shape probe over `__all__` returns zero |
| `test_public_all_matches_expected_tuple` | `__all__` equals an exact expected tuple, so any reintroduced export fails by construction |
| `test_neutral_tap_api_preserves_geometry` | independent synthetic coordinates match expected engineering layout |
| `test_neutral_tap_api_validates_inputs` | non-finite, non-positive and non-interior inputs raise |
| `test_no_sensitive_compatibility_alias` | no alias, warning, mapping or docs entry survives the removal, asserted by exact-namespace comparison rather than by naming the removed spelling |
| `test_benchmark_manifest_field_tracks_generator_literal` | the tracked manifest's geometry field equals the generator's module-level literal, so the two cannot silently diverge |
| `test_explorer_page_prose_tracks_generator_literal` | the tracked page's prose region equals the generator's template literal, so the two cannot silently diverge |
| `test_module_and_function_size_limits` | all touched code satisfies the universal limits, with the single named exemption declared in the test itself |

The authority, scanner, Git-metadata, filesystem-hostility and self-blocking
tests from the earlier revision will move to
[#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961) with the
scanner they exercise.

## Implementation Sequence

1. Inventory all reachable public surfaces for both identifier classes, capturing
   the patterns in-session for the later Tier 2 replay, and store only neutral
   rule IDs, classifications, file locations and counts in the implementation
   evidence. No value will be written to any tracked file.
2. Add RED package-export and neutral pressure-tap behavior tests; replace the
   coded API and synthetic defaults without a sensitive compatibility alias.
3. Neutralize the remaining inventoried reusable modules, fixtures, scripts and
   prose one surface at a time, running focused tests after every file, splitting
   the two oversized validation modules as their content is touched.
4. Correct the static literal in each of the two generators, then apply the
   identical substitution to the corresponding field of each tracked artifact and
   add the coupling test. Free-hand editing of a generated artifact, or any edit
   not derivable from the generator change, will invalidate the slice.
5. Regenerate API docs from a clean tree and confirm the build is warning-free.
6. Replay the captured patterns from step 1 against the finished tree and record
   counts only.
7. Run legal/security, packaging and full OpenFOAM regressions, then T3
   adversarial code/artifact review; resolve every MAJOR before requesting merge
   review.

No step in this sequence will wait on
[#3522](https://github.com/vamseeachanta/workspace-hub/issues/3522).

## Acceptance Criteria

- [ ] RED evidence precedes every implementation slice.
- [ ] Inventory covers source, tests, scripts, exports, fixtures, docs and tracked
      generated artifacts for both identifier classes, recorded as counts and
      classifications without values.
- [ ] Project-coded APIs, defaults and prose are absent from the reusable surface;
      generic replacement behavior uses only synthetic fixtures and has no
      sensitive compatibility alias.
- [ ] Both tracked generated artifacts are corrected at their generator first, the
      artifact edit is a mechanical substitution derivable from that generator
      change, and a coupling test asserts each artifact field equals its
      generator literal. No generator re-run is required or claimed.
- [ ] `scripts/setup/provision-cfd-box.sh` is untouched, and the two documented
      false positives are untouched.
- [ ] Tier 2 confirmation reports zero remaining occurrences of either class
      across the tracked tree, recorded as counts only.
- [ ] `PYTHONPATH=src uv run python -m pytest tests/solvers/openfoam/test_pressure_taps.py tests/solvers/openfoam/test_sloshing_coupling.py tests/solvers/openfoam/validation -q` passes.
- [ ] `PYTHONPATH=src uv run python -m pytest tests/solvers/openfoam -q` passes with no new failures relative to the recorded pre-existing baseline.
- [ ] `uv run python -m sphinx -W --keep-going docs docs/_build/html` succeeds from
      a clean tree.
- [ ] `uv run ruff check src/digitalmodel/solvers/openfoam tests/solvers/openfoam scripts/cfd/run_sloshing_3d_benchmark.py scripts/capabilities/build_sloshing_explorer.py` passes.
- [ ] `PYTHONPATH=src uv run python -m compileall -q src/digitalmodel/solvers/openfoam scripts/cfd/run_sloshing_3d_benchmark.py scripts/capabilities/build_sloshing_explorer.py` passes.
- [ ] The SHA-verified cross-repository legal scan passes:
      `test -n "$WORKSPACE_HUB_ROOT" && test -n "$DIGITALMODEL_REL_FROM_HUB" && EXPECTED_SHA="$(git rev-parse HEAD)" && test "$(git -C "$WORKSPACE_HUB_ROOT/$DIGITALMODEL_REL_FROM_HUB" rev-parse HEAD)" = "$EXPECTED_SHA" && (cd "$WORKSPACE_HUB_ROOT" && bash scripts/legal/legal-sanity-scan.sh --repo="$DIGITALMODEL_REL_FROM_HUB" --diff-only)`; `git diff --check` passes.
- [ ] Modified files and functions satisfy the 400/50-line limits, except the
      single named exemption for the explorer generator, which is declared in the
      implementation summary.
- [ ] No protected value appears in any commit message, branch name, PR title,
      issue title or issue comment produced by this work.
- [ ] T3 code/artifact review reaches no-MAJOR consensus and the issue receives
      an implementation summary comment.
- [ ] No client data, queue execution, self-merge, self-close or public result
      promotion occurs.

Criteria concerning Phase B provisioning, authenticated scanning, scanner
self-coverage, forensic sentinels, fork-CI separation and Git-metadata coverage
are not dropped; they move intact to
[#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961).

## Adversarial Review Summary

| Provider | Verdict | Findings |
|---|---|---|
| Claude | MAJOR | Phase-B gate, separate Git entry points, exhaustive scope, exact commands |
| Codex | MAJOR | authority phase, opaque diagnostics, reproduction, executable inventory |
| Gemini | MAJOR | authority contract, path redaction, oversized-module split |

**Overall:** r1/r2 MAJOR findings were resolved inline in r3 against the previous
scope. This revision changes scope rather than resolving new findings, so it will
require its own review at the depth the owner directs before implementation.
Explicit user approval remains required. No agent may apply
`status:plan-approved` or create its marker.

## Risks and Open Questions

- **No durable enforcement until the deferred half lands.** Tier 1 tests will
  catch reintroduction of the *known* export shape but cannot catch a newly
  invented leak. The window between this issue merging and
  [#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961) landing is
  unguarded, and its length depends on
  [#3522](https://github.com/vamseeachanta/workspace-hub/issues/3522) Phase B,
  which has no owner-committed date.
- Removing an already-public sensitive symbol is intentionally breaking. Keeping
  an alias would preserve the defect; downstream callers must adopt the neutral
  API in the same approved change.
- Git history is not rewritten by this issue. Historical exposure and provider-
  side cache remediation require separate owner-authorized incident scope, and
  the split does not change that.
- Neither tracked generated artifact will be reproduced by re-running its
  generator, so this issue will not establish that the artifacts are otherwise
  faithful to their generators. The explorer page is already known to diverge on
  brand tokens at `origin/main`, and the benchmark manifest embeds live
  measurement. The coupling tests will bound this to the identifier fields only;
  full generator fidelity remains
  [#1903](https://github.com/vamseeachanta/digitalmodel/issues/1903)'s scope.
- Sphinx output is acceptance evidence only when built from a clean tree; stale
  local output cannot establish closure.
- The explorer generator overlap with
  [#1888](https://github.com/vamseeachanta/digitalmodel/issues/1888) is a
  scheduling risk rather than a correctness risk, but a rebase after that work
  lands will require re-running the round-trip check.
- `feature/662-gmsh-openfoam-bridge` has a live worktree carrying large unmerged
  changes to `scripts/cfd/run_sloshing_3d_benchmark.py` while its issue
  [#662](https://github.com/vamseeachanta/digitalmodel/issues/662) is CLOSED. If
  that branch is revived rather than retired, it will conflict.
- Open question for the owner: whether the module-size exemption for the explorer
  generator is acceptable, or whether the owner would rather this issue leave
  that file untouched entirely and hand its identifier prose to
  [#1903](https://github.com/vamseeachanta/digitalmodel/issues/1903) — which
  would leave an organization name live on a public page for longer.

## Complexity: T3

The split reduces the file count but not the review depth. The remaining scope
still breaks a public API in a public repository, rewrites tracked public
artifacts through their generators, and turns on privacy correctness where a
missed surface is not recoverable by a later commit.
