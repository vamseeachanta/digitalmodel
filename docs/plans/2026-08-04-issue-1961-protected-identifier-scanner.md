# Plan for #1961: a fail-closed scanner that cannot be fooled by the surface it guards

> **Status:** plan-review (awaiting owner approval — never self-approved)
> **Complexity:** T3
> **Date:** 2026-08-04
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1961
> **Client:** N/A
> **Lane:** domain:security / lane:codex
> **Branch:** `plan/1961-public-surface-scanner` (worktree off `origin/main` @ `7b4119cc`)
> **Review artifacts:** r1 Claude — inline, main session (see Adversarial Review Summary)

> **Handling note.** No identifier value, organization name, project code, share
> name or internal path appears in this plan, in the branch name, in any commit
> message, or in any issue comment produced by this work. Findings are stated as
> counts, file paths, commit SHAs and structural shapes. The Client-PII gate
> scans commit messages as well as file contents; this plan is written to that
> constraint throughout.

---

## The property this plan is judged on

The scanner exists to detect a **newly invented** leak on a surface that, by
construction, no internal test exercises. #1574's own closeout recorded the two
halves of the same fact:

> **No internal callers** means both *safe to change* and *nothing will notice if
> you break it.*

#1574 justified removing a coded factory partly on blast radius — no callers
outside its module, its `__init__`, and its own tests. Sound. During the module
splits, moving the CLI silently took the `__main__` dispatch with it, turning the
documented module-execution path into a **no-op**, and no test failed, because
nothing in-repo invokes that path either.

#1961's scanner guards the **public** surface: exported API, emitted defaults,
generated artifacts. That is precisely the part internal tests do not exercise,
because anything internal depending on it would have surfaced the identifiers
long ago. **A scanner verified only by its own unit tests would be validated
against exactly the wrong population** — it would prove it detects what a test
author thought to plant, on a surface whose defining property is that nothing
internal touches it.

Two verifications in this plan therefore do **not** route through the test suite,
and neither is authored by the scanner's author (D4, D5).

---

## Premise verification (2026-08-04, against `origin/main` @ `7b4119cc`)

Every premise carried into this lane was re-measured. Three are corrected.

| Claim | Verdict | Evidence |
|---|---|---|
| The matcher must be **word-bounded** | **REFUTED as a prescription; mechanism CONFIRMED** | `\b` requires a `\w`/`\W` transition and `_` is a word character, so `\bTOKEN\b` never matches a token immediately followed by `_`. The removed public package export had exactly that shape. Bounding is the wrong contract; the mechanism it is based on is real |
| Strict bounding produced **13 false negatives** including the public export | **CONFIRMED EXACTLY — the refutation carried in is itself wrong** | Measured at `5f437bfb^` (the tree the #1574 lane measured, i.e. the then-`origin/main`): **11 / 9 / 10 files** and **31 / 17 / 29 occurrences** for unbounded / word-bounded / non-alphanumeric-bounded — reproducing the issue's table to the digit. Genuine surface excluding the lockfile = **30 occurrences in 10 files**. Word bounding finds 17 of 30, so **13 genuine occurrences are missed**. The figure reproduces |
| The `10/0/0/372/211` counter-measurement disproves the above | **REFUTED — different population** | That measurement was taken by the #1965 lane over **five tokens derived from a leaked capability page**, not over the #1574 coded token, and its own plan says *"should not be restated without re-measurement"*. Both measurements are correct about different tokens; fusing them produced a false refutation |
| At today's `origin/main` the mechanism can be demonstrated live | **REFUTED** | #1574 landed in `5f437bfb`. At `7b4119cc` the coded token has **1 occurrence in 1 tracked file** (`uv.lock`) and **0 genuine occurrences**. The demonstration must cite the historical tree; the current tree cannot show it |
| Lockfile hits are a content-addressed false positive to be excluded **by classification** | **CONFIRMED; magnitude corrected** | In `uv.lock` the coded token matches **1× unbounded / 0× word-bounded**, on a single line, inside a `sha256:` value and a `files.pythonhosted.org` URL path segment. The carried-in figure of *4 unbounded* belongs to the #1965 lane's different token. The conclusion is unchanged and is the important half: bounding would drop it for the wrong reason |
| One boundary rule chosen to silence the lockfile would certify the real export as clean | **CONFIRMED** | The export is the token followed by `_`; both-sides bounding never matches it. 1 false positive would be traded for 13 real misses |
| The organization/partner class collides with an operating-system codename | **CONFIRMED** | `scripts/setup/provision-cfd-box.sh:83-84`, an Ubuntu 24.04 release codename compared against `VERSION_CODENAME`. **2 occurrences in that 1 file** |
| The two identifier classes need separate rules | **CONFIRMED, and stronger than filed** | Repo-wide the organization name occurs in **38 tracked files / 90 occurrences** — public vessel-registry data, a public stock-ticker dataset, and the OS codename among them. A bare name match on this class is mostly non-violations. The partner/project name occurs in **18 files**; **12 files carry both**, all under `config/visualization/` |
| `scripts/legal/legal-sanity-scan.sh` can be invoked for this repo | **NOT EXECUTABLE — the path does not exist here** | `git ls-files` returns no `scripts/legal/` in digitalmodel; only `docs/legal-scanning.md`, which documents the script and a `.legal-deny-list.yaml` that **is also absent from the tracked tree**. `.pre-commit-config.yaml:140` points at `../scripts/legal/legal-sanity-scan.sh`, i.e. outside the repository |
| The workspace-hub `--repo=` form would verify this work | **REFUTED — fail-open** | OPEN workspace-hub [#3804](https://github.com/vamseeachanta/workspace-hub/issues/3804): *"scans NOTHING and exits 0"*. The #1965 lane observed `RESULT: PASS` rc=0 over a worktree containing a live leak. #1574's closeout records the same form returning PASS both **before and after** its own change, while the leak was present before |
| The gate census is patchy | **CONFIRMED on both counts** | The leaking page is registered in `PAGE_EXCLUSIONS` at `scripts/generated_html_ownership.py:126-128`, under a dict documented at `:91` as *"Generated-looking pages which cannot yet be reproduced deterministically"*; a second, near-duplicate page is outside the `docs/api/**` census (**370 tracked files**) entirely |
| #1961 is hard-gated on workspace-hub #3522 Phase B, which is not provisioned | **CONFIRMED by live readback** | Environment `legal-rule-authority`: `protection_rules: []`, `deployment_branch_policy: null`, `created_at == updated_at == 2026-07-14T12:54:18Z`, `secrets.total_count = 0`. Phase A merged (PR #3535, `966401108fa45eae95927918bae34044d8ba20fa`) but its activation preflight **stopped with no external mutation**. #3522 OPEN, #3544 OPEN. **Phase B: not approved, not started, no committed date** |
| The identifier value is private | **REFUTED, and it changes what the scanner can claim** | The coded token is recoverable from this public repository's own history at `5f437bfb^`. A scanner keyed to it cannot assert the value was never published. Only #3522's history-remediation scope can address that; the scanner cannot un-publish history |
| The inherited Sphinx acceptance criterion is executable | **REFUTED** | No `conf.py` exists at any tracked path (`git ls-files` → empty for `conf.py`), though `sphinx>=6.0` is declared at `pyproject.toml:172`. #1574 closed recording this criterion as *"not executable"* and *"closed WITHOUT being satisfied"*. #1961 inherits it verbatim and **must not** |

### The three corrections that change the design

1. **The evidence table in the issue is sound.** It was nearly discarded on a
   refutation measured over a different token set. It reproduces exactly at the
   tree it was measured over. The matcher contract stands unchanged.
2. **The demonstration is historical, not live.** Nothing in today's tree shows
   the defect, because #1574 fixed it. This is not a weakness — it is the
   scanner's best available oracle (D4).
3. **Two inherited acceptance criteria cannot execute** — the Sphinx build and
   the cross-repository legal scan. Both are removed, and their removal is made
   an explicit criterion so neither is quietly restored.

---

## Deliverable

A fail-closed, byte-oriented scanner over the complete tracked Git tree, staged
index blobs, changed commit messages and generated artifacts, driven by a
versioned scope manifest with **class-specific** rules; plus a public-surface
structural snapshot whose diff is verified **outside** the test suite; plus a
retrospective-corpus oracle that judges the scanner against a leak population
its author did not construct.

---

## Resource Intelligence Summary

### Existing content-scanning gates (prior art, and the failure modes to avoid)

- `tests/riser_database/test_leak_gate.py` — the repo's existing two-layer
  forbidden-content merge gate. Layer B loads the **private** hub deny list,
  walks the YAML generically so new deny groups are picked up, normalizes case /
  whitespace / `_` / `-`, and **never echoes the matched pattern** (reports
  `group[index]` only). Anti-vacuity guard at `:120`:
  `assert patterns, "deny list parsed to zero patterns — parser drift?"`. It
  **loud-skips when the hub is absent**.
- `tests/riser_database/test_provenance_tripwire.py` (414 lines) — the scoped
  complement. Carries a planted-token test (`:331`), an anti-vacuity assert
  (`:283`), and a stale-artifact-list guard (`:346-352`). It also **skips when
  the private clone is absent** (`:252`, `:360`, `:396` — *"In-context gate
  (skips standalone)"*), i.e. green on skip.
- `scripts/enforcement/check-no-abs-paths.sh` (117 lines) — the nearest existing
  repo-wide *content* scanner. Three modes (`--added <base>` diff-scoped for CI,
  `--all`, explicit files), per-line `# abs-path-allowed` sentinels, an
  `ALLOW_ABS_PATHS=1` logged bypass, exit `0/1/2`. Its enumeration is a
  **hardcoded `GLOBS` array** (`:38`) covering `src/**`, `tests/**`, `config/**`
  and `docs/**/*.md` — **not the whole tree**. The per-line sentinel is the
  precedent D-series follows; the partial census is the defect it must not.
- `scripts/check_generated_html.py` (428 lines) — the strongest census
  precedent. `validate_page_census()` (`:209`) partitions every discovered page
  into exactly one of {registered output, `PAGE_EXCLUSIONS`, `MANUAL_PAGES`} and
  errors on unclassified, stale **and** overlapping classifications;
  `validate_registry()` (`:236`) requires **every exclusion to carry a non-empty
  reason** (`:245-255`). Its census is `docs/api/**` only — 370 tracked files —
  and excluded pages are not regenerated, so it fails no PR that hand-edits one.
- `scripts/generated_html_ownership.py` — the reasons-as-data companion:
  `PAGE_EXCLUSIONS` at `:92` (documented `:91`), `MANUAL_PAGES` at `:161`.
  `PAGE_EXCLUSIONS` means *a producer exists but is not deterministically
  reproducible*; `MANUAL_PAGES` means *no producer exists*.
- `scripts/brand_guard.py` — has a `--selftest` mode (`:42`) asserting its regex
  both matches known-bad and does not match known-good. Directly reusable idiom.

### Existing public-API-surface tooling — the gap D5 fills

- `tests/solvers/openfoam/test_pressure_tap_api.py:20` pins one package's
  exports as a hand-maintained `EXPECTED_ALL` tuple, documented at `:17-19` as
  *"an identity assertion, not a containment check"*. One package, hand-written,
  **no signatures and no defaults** — which is exactly the surface where #1574's
  two transcription errors lived.
- `tests/contracts/test_string_addressed_dependencies.py` (~780 lines) already
  walks every module in `src/` with `SRC.rglob("*.py")` + `ast.parse` (`:519`,
  `:527`), fails on unrecognised values rather than passing silently (`:578`),
  has a stale-allowlist test (`:597`), and carries planted-violation self-tests
  (`:717`, `:731`). Its docstring argues that a hand-curated list is *"a sample,
  not a set"* — the same argument this plan makes about test-only verification.
  D5 reuses this walker shape wholesale.
- **Nothing in the repository captures parameter names, defaults or annotations,
  and no `inspect.signature` walker exists.** That gap is the one #1574's
  regressions escaped through.

### Gaps identified

1. No repo-local gate covers the public surface. The last two leaks — #1965's
   capability page, and #1574's own residue — were **both found by manual
   sweep**, not by any check.
2. The only verification route named in the inherited criteria is fail-open.
3. The generated-artifact census stops at `docs/api/**` and further excludes
   pages inside it, so it inherits two holes at once.
4. Nothing structurally compares the public surface against its previous state.
   #1574's inline AST pass caught a silent `__main__` no-op plus two
   transcription errors in hand-written delegating methods — a wrong default and
   a dropped optional — that **723 passing tests** did not.
5. **Both existing legal hooks reach outside the repository.**
   `.pre-commit-config.yaml:137` runs `../scripts/legal/legal-sanity-scan.sh`
   and `:78` runs gitleaks with `--config ../.gitleaks.toml`. Neither relative
   path resolves from a worktree, and the first is the fail-open form.
6. **The repo has no required status checks.** `docs/plans/2026-06-11-issue-700-ci-baseline-domain-gates.md:115` records that `main` has no classic branch protection and the active ruleset defines no required checks; `quality-gates-by-domain.yml:201` provides the intended stable aggregate context (`domain-tests-required`).
7. **`docs.yml` is path-filtered** (`:9-31`). A scanner wired only there would
   not fire on changes outside `src/`, `scripts/`, `docs/api/`, `data/` and a
   short list of siblings.

### Evidence

Distinct sources: issue #1961; issue #1574 body, plan and closeout comments;
issue #1965 body, plan and approval comments; issue #1575 closeout; workspace-hub
#3522, #3544, #3804 and the live `legal-rule-authority` environment readback;
`git grep` measurements at `7b4119cc` and `5f437bfb^`; the seven scripts and six
test modules above; `pytest.ini`; `pyproject.toml`; `.pre-commit-config.yaml`;
`.claude/quality-gates.yaml`; `tests/DOMAINS.md`; the workspace-hub deny list (20+).

---

## Design decisions

**D1 — Matcher contract: unbounded substring, plus manifest-classified exclusion
of content-addressed fields.** No boundary rule is used for class A. The
lockfile hit is excluded because of *what the field is* — a `sha256:` value and
a package-index URL path segment, declared in the manifest as a content-addressed
field — never by a boundary rule, and never by a whole-file exemption. Measured
trade at `5f437bfb^`: bounding suppresses 1 false positive at the cost of 13
real misses including the public export.

**D2 — Class-specific rules; one global expression will not serve.** Class A
(coded token) is unbounded, case-insensitive substring. Class B (organization +
partner name) is defined as **same-file co-occurrence of the two names** — 38
files carry name-1, 18 carry name-2, 12 carry both. No character or line window
is used, because **any window width would be a constant fitted to the observed
data**. Same-file co-occurrence is a structural rule with no fitted constant.

**D3 — Exhaustive classification, not a census glob.** Every path returned by
`git ls-files -z` must be classified by the manifest; an unclassified path fails
rather than disappearing. This is what makes an empty-result pass impossible:
the enumeration itself is checked (D6a).

**D4 — Retrospective-corpus oracle (non-test verification #1).** The scanner is
run against a leak population **its author did not construct and did not
choose**: the tree at `5f437bfb^`, the last commit before #1574's neutralization.
Required result, two-sided (any extra finding fails; any missing finding fails):

| Path at `5f437bfb^` | Required class-A findings |
|---|---|
| `docs/api/cfd/sloshing-3d-benchmark.json` | 1 |
| `scripts/capabilities/build_sloshing_explorer.py` | 1 |
| `scripts/cfd/run_sloshing_3d_benchmark.py` | 2 |
| `src/digitalmodel/solvers/openfoam/__init__.py` | 2 |
| `src/digitalmodel/solvers/openfoam/pressure_taps.py` | 6 |
| `src/digitalmodel/solvers/openfoam/sloshing_coupling.py` | 2 |
| `src/digitalmodel/solvers/openfoam/validation/sloshing_2d.py` | 1 |
| `src/digitalmodel/solvers/openfoam/validation/sloshing_sweep.py` | 4 |
| `tests/solvers/openfoam/test_pressure_taps.py` | 10 |
| `tests/solvers/openfoam/test_sloshing_coupling.py` | 1 |
| **total, 10 files** | **30** |
| `uv.lock` | **0** — the content-addressed exclusion must suppress its 1 raw hit |

**These are not fitted constants.** They are measured properties of an immutable
historical tree used as an oracle. The tree being judged is a different tree.
The numbers are exact equalities, never thresholds, and the SHA is pinned in the
manifest so the oracle cannot drift. No threshold anywhere in this plan is
derived from the data it judges: every count is either an exact equality against
a fixed external tree, or a structural assertion (non-empty, contains-by-name,
exactly-one-classification) with no number in it at all. The repo already uses
the exact-equality ratchet idiom — `tests/capabilities/test_generated_html_freshness.py:92`
pins registry sizes exactly rather than with a bound.

Class B carries **no numeric window at all**, for the same reason (D2).

This is the criterion that answers *"a scanner test that plants a token and finds
it proves only that the author knew what to plant."* Nobody planted these; the
#1574 lane removed them and Git preserved them.

**D5 — Public-surface structural snapshot (non-test verification #2).** A
snapshot tool emits deterministic JSON over `src/digitalmodel/`: per module, the
`__all__` list; every public class and function name; each signature with its
default **literals** rendered from the AST; the presence and target of any
`if __name__ == "__main__"` dispatch; and a sha256 per generated artifact in the
manifest's generated census. Acceptance is a shell `diff` of the snapshot built
from `git show <base>:` blobs against the snapshot at the PR head — **empty, or
every differing line enumerated in the PR body**. It runs as one command in CI
and one command locally; **it is not a pytest assertion**, because the point is
to verify a surface the test suite does not reach.

It must be built from **Git blobs, not the working tree**, or it becomes a
symmetric comparison of the same bytes through the same reader.

Why this shape: it is population-complete over the public surface — it enumerates
the surface rather than asserting the absence of something a test author
imagined. It is exactly the pass that caught #1574's `__main__` no-op and two
transcription errors that 723 passing tests did not.

**D6 — Anti-symmetric-exclusion rule.** #1575's closeout recorded: *"Symmetric
exclusion proves nothing about the excluded file; it only removes it from the
comparison."* Two real backward-compatibility regressions hid behind exactly
that. Applied here:

- **(a)** the scanner's internal enumeration is cross-checked against an
  independent `git ls-files -z` shell run and must match **exactly**, so an
  enumerator defect cannot cancel out on both sides;
- **(b)** no path may be dropped from both baseline and head snapshots. Any
  manifest exclusion must name the other entry point that does scan it;
- **(c)** undecodable, oversized, symlink-escaping or unenumerated artifacts
  fail closed rather than being dropped from both sides.

**D7 — Rename the scanner.** The issue names
`scripts/legal/check_sloshing_public_surface.py`, but its own specification
enumerates the complete tracked tree, and #1965 proved both identifier classes
live outside the sloshing surface. A sloshing-scoped filename invites a future
reader to narrow the census back to the sloshing modules — which is how the
`docs/api/**` census acquired its two holes. **Recommend
`scripts/legal/check_protected_identifiers.py`.** Owner decision; the plan is
executable either way.

**D8 — The Sphinx criterion is removed, not carried.** There is no Sphinx
project in this repository. The generated-artifact census is instead the tracked
generated-HTML surface already modelled by `scripts/generated_html_ownership.py`,
**extended past its `docs/api/**` limit**, since #1965 found a leaking
near-duplicate outside it. Removal is itself an acceptance criterion so it is not
restored by a future reader copying #1574's list.

**D9 — Fail closed, never skip.** The existing tripwire's green-on-skip posture
is the failure mode to avoid. Missing or invalid authority returns a **distinct
nonzero code**; the CI job asserts the check actually executed and prints its
enumeration counts, so a skipped or vacuous run is visibly distinguishable from a
clean one.

**D10 — Two stages against the Phase B gate.** Only the *rule values* are
private; #3522's own design makes rule **IDs** public. So the work splits:

- **Stage 1 — authority-independent.** Manifest schema, exhaustive enumeration
  and classification, class rules keyed to public rule IDs, the retrospective
  oracle (D4), the public-surface snapshot (D5), anti-vacuity guards, self
  coverage, and the fork-safe test suite. Commits no protected value.
- **Stage 2 — authority-dependent `[PHASE-B]`.** Authenticated CURRENT-snapshot
  schema/generation/authenticity and anti-rollback validation, the protected
  maintainer workflow, fork-CI separation, and any production-clean assertion.

The issue's governance says implementation may not proceed. Whether Stage 1 may
proceed under that gate is an **owner decision**, presented in Risks. A Stage 1
green must never read as "the surface is guarded" — see D11.

**D11 — Stage 1 must not be mistakable for a production gate.** Without the
authority, Stage 1 exercises synthetic rules and the retrospective corpus only.
Its CI job prints an explicit `UNAUTHENTICATED` banner, is not a required check,
and is named so that it cannot satisfy any production-clean requirement.

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `scripts/legal/check_protected_identifiers.py` (D7; issue's name is `check_sloshing_public_surface.py`) | D1/D2/D3/D6/D9 scanner; byte-oriented entry points for tree, staged index blobs, commit-message file, metadata JSON, generated root + manifest |
| Create | `scripts/legal/protected-surface-v1.json` | versioned scope manifest: class definitions by public rule ID, exhaustive path classification, content-addressed field declarations, generated census, pinned oracle SHA |
| Create | `scripts/legal/public_surface_snapshot.py` | D5 structural snapshot over `src/digitalmodel/`, built from Git blobs |
| Create | `scripts/legal/verify_public_surface.sh` | D4 + D5 + D6a runner — the three non-test verifications as one reproducible command set |
| Create | `tests/scripts/test_check_protected_identifiers.py` | hostile Git/filesystem/authority/self-block cases |
| Create | `tests/scripts/test_public_surface_snapshot.py` | snapshot determinism, census identity, mutation sensitivity |
| Create | `scripts/legal/protected_surface_ownership.py` | reasons-as-data companion, following the `generated_html_ownership.py` split; every entry carries a non-empty reason, enforced as `check_generated_html.py:245-255` already does |
| Modify | `scripts/generated_html_ownership.py` | D8: expose the generated census to the manifest and record that it is no longer the only census |
| Modify | `.github/workflows/quality-gates.yml` | Stage 1 job — **not** `docs.yml`, which is path-filtered at `:9-31` and would not fire on most changes (Gap 7). Stage 2 protected job `[PHASE-B]` |
| Modify | `tests/DOMAINS.md` + `.claude/quality-gates.yaml` + the aggregate `depends_on` (`.claude/quality-gates.yaml:264`) | the three coordinated edits the domain-gate indirection requires; `scripts/ci/detect_touched_domains.py:21-22` already treats both files as full-matrix triggers |
| Modify | `.pre-commit-config.yaml` | replace the hub-relative hook at `:137` with a repo-local Stage 1 tree scan, mirroring the `no-windows-path-dirs` local-hook block at `:216`. Record that the gitleaks hook at `:78` has the same unresolvable relative path |
| Modify | `docs/legal-scanning.md` | record that the documented script and the `.legal-deny-list.yaml` it names do not exist in this repo, and what replaced them |
| Update | `docs/plans/README.md` | index row |

Tests live under `tests/`, never under `scripts/` — `pytest.ini`'s
`norecursedirs` excludes `scripts`, so a test placed there would never run and
would read as green forever (the exact failure `tests/capabilities/test_capabilities_inventory.py:356-360` records: *"this exact test reported PASS for 15 days while verifying nothing"*).

Every new implementation file will remain at or below 400 lines and every
function at or below 50 lines; the scanner will be split by responsibility
(enumeration / classification / matching / reporting) rather than grandfathered.

---

## TDD Test List

| Test | Input | Expected | Red today because |
|---|---|---|---|
| `test_unbounded_matcher_finds_token_followed_by_underscore` | synthetic export-shaped name | found | word bounding misses it; the contract is not implemented |
| `test_word_bounded_rule_is_rejected_by_the_manifest_schema` | a manifest declaring a boundary rule | schema error | nothing prevents a future edit from reintroducing it |
| `test_content_addressed_field_excluded_by_classification` | synthetic lockfile-shaped hash field | no finding | no classification exists |
| `test_whole_file_exemption_is_rejected` | manifest with a file-level exempt | schema error | the blanket-exempt backdoor is unguarded |
| `test_class_b_requires_co_occurrence` | file with one name only / file with both | no finding / finding | no class-B rule exists |
| `test_os_codename_shape_is_not_a_class_b_finding` | synthetic provisioning-script shape | no finding | would fire today |
| `test_unclassified_tracked_path_fails` | a new tracked path/extension | nonzero exit | omission would read as clean |
| `test_enumeration_matches_independent_git_ls_files` | both enumerations | exact equality | D6a has no implementation |
| `test_staged_blob_differs_from_working_tree` | index blob ≠ worktree bytes | index bytes scanned | no staged entry point |
| `test_commit_message_surface_is_scanned` | synthetic message file | finding | not covered |
| `test_rename_and_delete_paths_are_scanned` | staged rename + delete | findings | not covered |
| `test_symlink_escape_oversize_undecodable_fail_closed` | each hostile artifact | nonzero exit | ambiguity would pass clean |
| `test_missing_authority_returns_distinct_code` `[PHASE-B]` | no snapshot | distinct nonzero | fails closed, never skips |
| `test_forged_or_rolled_back_snapshot_rejected` `[PHASE-B]` | stale / forged CURRENT | rejected | — |
| `test_diagnostics_disclose_only_rule_ids_and_offsets` | any finding | no value in output | redaction unimplemented |
| `test_scanner_scans_itself` | its own implementation, tests, plan, review artifacts | covered | self-block hole |
| `test_only_per_line_synthetic_sentinels_are_honoured` | line sentinel vs file sentinel | honoured / rejected | — |
| `test_scan_catches_a_planted_token` | temp file with a synthetic token | finding | **necessary but not sufficient — see D4** |
| `test_snapshot_is_deterministic_across_two_runs` | same blob set twice | byte-identical | — |
| `test_snapshot_census_is_non_empty_and_names_known_modules` | the snapshot's module list | non-empty **and** contains the OpenFOAM solver package and the module that carried the `__main__` regression | **anti-vacuity** — two empty snapshots diff clean |
| `test_snapshot_detects_a_changed_default_and_a_dropped_optional` | perturbed scratch copy | non-empty diff | proves the exact defect class 723 tests missed |
| `test_snapshot_detects_a_removed_main_dispatch` | scratch copy with `__main__` removed | non-empty diff | the #1574 regression, reproduced as an oracle |

`test_scan_catches_a_planted_token` is retained deliberately and marked in the
table as insufficient on its own, so no future reader mistakes it for the
scanner's proof of capability.

---

## The three verifications that do not route through the test suite

Run by `scripts/legal/verify_public_surface.sh`, reported in the PR body:

1. **Retrospective corpus (D4).** Scan the tree at the pinned pre-#1574 SHA.
   Require the exact 10-file / 30-occurrence table above, and exactly 0 findings
   in the lockfile. Two-sided.
2. **Public-surface snapshot diff (D5).** Build the snapshot from base blobs and
   from head blobs; `diff` them; require empty, or every differing line
   enumerated in the PR body. Includes a mutation step that perturbs one default
   in a scratch copy and requires the diff to become non-empty — so an
   always-empty diff is distinguishable from a correct one.
3. **Enumeration cross-check (D6a).** Compare the scanner's internal path
   enumeration against an independent `git ls-files -z` run; require exact
   equality; require the count to be non-zero and to contain named paths.

---

## Phase B dependency ledger

| Capability | Stage | Blocked today |
|---|---|---|
| Manifest schema, exhaustive enumeration, classification | 1 | no |
| Class A / class B matching by public rule ID | 1 | no |
| Retrospective corpus oracle | 1 | no — the corpus is in this repo's history |
| Public-surface snapshot and diff | 1 | no |
| Anti-vacuity, self-coverage, sentinel policy | 1 | no |
| Authenticated CURRENT-snapshot validation, anti-rollback | 2 | **yes** |
| Real (non-synthetic) rule values | 2 | **yes** |
| Protected maintainer workflow, fork-CI separation | 2 | **yes** |
| Any "production-clean" assertion | 2 | **yes** |

Nothing in Stage 1 asserts production-clean state. Every Stage 2 item is marked
`[PHASE-B]` in the acceptance criteria below.

---

## Implementation Sequence

1. Land the manifest schema and the exhaustive enumerator with D6a's cross-check
   RED first; no matching logic yet.
2. Add class A and class B rules against synthetic rules and public rule IDs.
3. Stand up the retrospective corpus oracle (D4) — the first verification that
   can fail for a reason nobody chose.
4. Build the public-surface snapshot (D5) from Git blobs; prove mutation
   sensitivity before wiring the diff into CI.
5. Add hostile Git/filesystem/self-block/sentinel cases.
6. Wire the Stage 1 CI job with the `UNAUTHENTICATED` banner and the pre-commit
   replacement; retire the hook that points outside the repository.
7. `[PHASE-B]` On Phase B provisioning: pin the exact workflow, CURRENT
   generation, environment and ruleset readbacks; add authenticated validation,
   anti-rollback, protected workflow and fork separation; run the authenticated
   production scan.
8. T3 adversarial review at the code stage; implementation summary comment.

---

## Acceptance Criteria

- [ ] **Retrospective corpus:** the scanner run against the pinned pre-#1574 tree
      reports **exactly 30 class-A findings across exactly the 10 named files**,
      with the per-file counts in D4, and **exactly 0** findings in the lockfile.
      Two-sided: any extra or missing finding fails. Not routed through pytest.
- [ ] **Public-surface snapshot diff** between base blobs and head blobs is empty
      or fully enumerated in the PR body, and the mutation step makes it
      non-empty. Not routed through pytest.
- [ ] **Enumeration cross-check** against an independent `git ls-files -z` run
      matches exactly, is non-zero, and contains named paths. Not routed through
      pytest.
- [ ] **The snapshot census is asserted non-empty and to contain named modules.**
      A diff satisfiable by two empty snapshots is not a criterion.
- [ ] **Every tracked path is classified**; an unclassified path fails the run.
      A clean result caused by an omission is impossible by construction.
- [ ] **No boundary rule and no whole-file exemption is representable** in the
      manifest schema; both are schema errors with tests.
- [ ] **Class B fires only on same-file co-occurrence**, and the provisioning
      script's OS-codename shape produces no finding.
- [ ] **The class-B population on today's tree is reported, not remediated.**
      #1961 builds the guard; the scanner's class-B report on `HEAD` is attached
      to the issue and remediation is a named follow-on. A zero-violation
      criterion for class B here would silently convert this issue into a
      remediation issue and would be satisfiable by deleting files.
- [ ] **The scanner scans its own implementation, tests, plan and review
      artifacts**, with only exact-line synthetic sentinels.
- [ ] **Diagnostics disclose only rule IDs, opaque path IDs, byte offsets and
      match classes** — no value, in any output stream or CI log.
- [ ] The scanner tests pass under the **repo's real CI invocation**, not a
      convenience variant: `uv run --no-sources --with 'assetutilities @ git+https://github.com/vamseeachanta/assetutilities.git@main' --with-editable '.[test]' python -m pytest tests/scripts/test_check_protected_identifiers.py tests/scripts/test_public_surface_snapshot.py -rfE -p no:randomly -p no:sugar --no-header -q --tb=line`
      (the shape every gate in `.claude/quality-gates.yaml` uses; `pytest.ini` is
      the live config, the `pyproject.toml` table is inert per its own `:264-268`).
- [ ] The full suite is compared **node-ID by node-ID** against a baseline
      captured in this worktree at the branch point, with no new failure node IDs
      and **no module excluded from both sides** (D6b). Any module that cannot
      collect must have its dependency installed and be run, not excluded
      symmetrically.
- [ ] The new gate is reachable: a `tests/DOMAINS.md` row, a
      `.claude/quality-gates.yaml` entry with `failure_action: block`, and
      inclusion in the aggregate `depends_on` — all three, verified by a touched
      file actually routing to the gate. A gate wired in only two of the three
      places never runs.
- [ ] `ruff check` and `python -m compileall -q` pass over every new and modified file.
- [ ] Modified files ≤ 400 lines, functions ≤ 50 lines.
- [ ] **The Sphinx criterion is deliberately absent.** Its omission is a finding,
      not an oversight: no `conf.py` exists at any tracked path, and #1574 closed
      recording this criterion as not executable and not satisfied.
- [ ] **The cross-repository legal-scan criterion is deliberately absent.** Its
      omission is a finding: `scripts/legal/legal-sanity-scan.sh` does not exist
      in this repository, and the workspace-hub `--repo=` form is fail-open under
      OPEN [#3804](https://github.com/vamseeachanta/workspace-hub/issues/3804),
      observed returning PASS over a worktree containing a live leak.
- [ ] **No identifier value, organization name, project code, share name or
      internal path appears in any commit message, branch name, PR title or issue
      comment** produced by this work. Verified by reading the branch's full log
      before opening the PR.
- [ ] The Stage 1 CI job prints its enumeration counts and an `UNAUTHENTICATED`
      banner, and is not a required check.
- [ ] `[PHASE-B]` #3522 Phase B is separately approved, merged, provisioned,
      CAS-promoted to CURRENT, and its exact workflow / generation / environment /
      ruleset readbacks are pinned in the manifest.
- [ ] `[PHASE-B]` Authenticated production scan passes without printing protected
      values; missing or invalid authority fails closed with a distinct code.
- [ ] `[PHASE-B]` `[ALSO-BLOCKED]` Fork CI cannot satisfy the protected
      production-clean merge requirement. **This criterion is blocked twice
      over:** it needs Phase B *and* it needs a required status check to exist,
      and this repository has none (Gap 6). Satisfying it requires a ruleset
      change on `main` — owner-controlled external state, outside this issue.
- [ ] `[PHASE-B]` Staged index blobs, renames, deletions and changed commit
      messages are covered by the authenticated run.
- [ ] T3 adversarial review at the code stage reaches no-MAJOR consensus and the
      issue receives an implementation summary comment.
- [ ] No client data, queue execution, self-merge, self-close or public result
      promotion occurs.

---

## Out of scope

- **Remediating class B.** 12 tracked files carry the co-occurrence today. This
  issue reports them; a named follow-on fixes them.
- **Fixing workspace-hub [#3804](https://github.com/vamseeachanta/workspace-hub/issues/3804)** — cross-repo. Named as a required follow-on; without it the documented gate stays fail-open.
- **Adding the organization identifier class to any deny list** — cross-repo, and
  the reason #1965's leak matched zero patterns.
- **History remediation.** The coded token remains recoverable from this repo's
  own history. Only #3522's separately approved transaction addresses that.
- **Restoring a `.legal-deny-list.yaml` or a repo-local
  `scripts/legal/legal-sanity-scan.sh`** to match `docs/legal-scanning.md`.
- **Making excluded pages deterministically reproducible** so they can leave
  `PAGE_EXCLUSIONS`.

---

## Adversarial Review Summary

| Round | Provider | Verdict |
|---|---|---|
| r1 | Claude — inline, main session | **MAJOR** — 10 findings, all folded in |

Findings that changed the plan:

1. **MAJOR — I nearly discarded a correct evidence table.** The carried-in
   refutation of the "13 false negatives" figure was measured over a different
   token population. Re-measurement at the correct tree reproduced the issue's
   table to the digit. Acting on the refutation would have weakened D1 on false
   evidence.
2. **MAJOR — two inherited criteria cannot execute.** The Sphinx build (no
   `conf.py` anywhere) and the cross-repository legal scan (script absent here,
   fail-open there). Both removed, and their absence made criteria.
3. **MAJOR — a zero-violation class-B criterion would have been scope creep and
   satisfiable by deletion.** Changed to a reporting criterion with remediation
   named as a follow-on.
4. **MAJOR — the snapshot diff is satisfiable by two empty snapshots.** Added
   census identity assertions plus a mutation step.
5. **MAJOR — symmetric exclusion.** #1575 lost two real regressions to it. Added
   D6a/b/c: independent enumeration cross-check, no both-sides exclusions, and
   fail-closed on ambiguity.
6. **MEDIUM — the planted-token test proves only that the author knew what to
   plant.** Retained but explicitly labelled insufficient; D4 carries the burden.
7. **MEDIUM — Stage 1 green could be misread as "guarded".** Added D11: an
   `UNAUTHENTICATED` banner and a non-required job.
8. **MAJOR — a `[PHASE-B]` criterion that is blocked twice over.** "Fork CI
   cannot satisfy the protected production-clean merge requirement" presumes a
   required status check. This repo has **none**. Marked `[ALSO-BLOCKED]` rather
   than left to read as Phase-B-only. This is exactly the failure mode the plan
   was told to hunt for — a criterion that can only be met once something else
   lands, not marked as such — and I had written one myself.
9. **MEDIUM — I first wired the gate into `docs.yml`.** That workflow is
   path-filtered, so the scanner would not have fired on most changes: a gate
   that silently does not run is worse than no gate, because it reads green.
   Moved to `quality-gates.yml` plus the three-place domain-gate registration.
10. **MEDIUM — I first placed tests under `scripts/`.** `pytest.ini`'s
   `norecursedirs` excludes `scripts`, so they would never have been collected.
   The same repo already recorded a test that *"reported PASS for 15 days while
   verifying nothing"* (`tests/capabilities/test_capabilities_inventory.py:356-360`).
   Two of my own three CI-shaped criteria were unexecutable before this pass —
   which is the plan's own thesis applied to itself.

Per the loop-break rule, r1 findings are resolved inline; explicit user approval
remains required. No agent may apply `status:plan-approved`.

---

## Risks and Open Questions

1. **Open question for the owner — may Stage 1 proceed?** The issue hard-gates
   all implementation on Phase B. Stage 1 needs no secret and commits none, and
   Phase B has no committed date; the surface is meanwhile guarded by nothing.
   Recommendation: allow Stage 1, keep Stage 2 gated. This is the owner's call
   and the plan is executable either way.
2. **The oracle can rot for the right reason.** If normalization legitimately
   changes, D4's counts fail. The tree SHA is pinned in the manifest and any
   change to the expected counts must carry its re-measurement command in the
   same commit.
3. **This plan discloses the coded token's *shape*** — length and character
   class. That narrows a search space. It is accepted because the value itself is
   already recoverable from this repository's public history (premise table), so
   the shape adds nothing an adversary could not obtain.
4. **Until this lands, the surface is guarded by nothing**, and the last two
   leaks — #1965's capability page and #1574's own residue — were **both found by
   manual sweep**, not by any gate.
5. **Phase B is not merely late, it is not started**, and Phase A's activation
   preflight stopped with no external mutation. Any schedule that assumes
   provisioning is unfounded.
6. **The scanner cannot prove a value was never published.** It can prove a tree
   is clean today. Historical exposure is #3522's scope.

---

## Complexity: T3

Cross-repository authority dependency, protected-workflow evidence, a public API
surface contract, generated-artifact census, Git metadata coverage, and two
verification routes outside the test suite.
