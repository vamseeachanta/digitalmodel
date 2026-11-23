# Plan for #1965: stop publishing internal filesystem paths on capability pages

> **Status:** plan-review (awaiting owner approval — never self-approved)
> **Complexity:** T2
> **Date:** 2026-08-04
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1965
> **Client:** N/A
> **Lane:** domain:security
> **Branch:** `plan/1965-published-path-leak` (worktree off `origin/main` @ `7b4119cc`)
> **Review artifacts:** r1 Claude — inline, main session (see Adversarial Review Summary)

> **Handling note.** No identifier, internal path, share name or project code
> appears in this plan, in the branch name, in any commit message, or in any
> issue comment. Findings are stated as counts, file paths and structural
> shapes. The Client-PII gate scans commit messages as well as file contents;
> this plan is written to that constraint throughout.

---

## Premise verification (2026-08-04, against `origin/main` @ `7b4119cc`)

| Claim | Verdict | Evidence |
|---|---|---|
| 8 occurrences of internal absolute paths on `docs/api/hydro/ocimf-coefficient-explorer.html`, rendered as a source-provenance table | **CONFIRMED exactly** | 8 matches; they sit in the `5. Data provenance and citation` table and in the page footer |
| The page is a **generated** page — fix the generator and regenerate | **REFUTED as stated** | The page is registered in **`PAGE_EXCLUSIONS`** (`scripts/generated_html_ownership.py:126-128`), a dict documented at `:91` as *"Generated-looking pages which cannot yet be reproduced deterministically."* The drift gate therefore **does not regenerate it**, so "regenerate from source" is not an available remedy and a PR hand-editing this page will **not** be failed by the gate |
| The page's build script lived **outside** the repository (issue text) | **REFUTED** | The producer is in-repo: `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py`, 779 lines. What is external is the **input workbook**. The page's own footer credits a `/tmp/` copy of the script, which is the likely source of the confusion |
| The page is classified as a **manual page** (issue text) | **REFUTED — near miss that changes the remedy** | `MANUAL_PAGES` begins at `:161`; this entry is at `:126`, inside `PAGE_EXCLUSIONS`. `MANUAL_PAGES` means *no producer exists*; `PAGE_EXCLUSIONS` means *a producer exists but is not reproducible*. Since a producer exists **and is in-repo**, the fix-at-the-generator rule **does** apply here — contrary to the issue's conclusion that it has no generator to apply to |
| `main` carries a generated-HTML drift gate that fails a PR editing a generated page without regenerating | **CONFIRMED, but does not bind here** | `scripts/check_generated_html.py`; its census is `docs/api/**` only (module docstring `:1`), and excluded pages are not regenerated |
| The matcher must be **unbounded substring**, not word-bounded | **MECHANISM CONFIRMED — but not load-bearing for this fix** | `\b` requires a `\w`/`\W` transition, and `_` is a word character, so `\bTOKEN\b` fails wherever the token is followed by `_`. Measured: derived tokens occur 9, 26 and 11 times immediately followed by `_`. **However**, the tokens actually leaked on this page are hyphen-delimited, and `-` *is* a non-word character — so word-bounding finds **all** of them (missed = 0). The rule is correct in general and irrelevant to #1965's own remediation |
| Strict bounding produces **13** false negatives incl. a public package export | **NOT REPRODUCIBLE at this scope — do not restate as fact** | Repo-wide, the five tokens derivable from the leaked paths miss 10, 0, 0, 372 and 211 occurrences under bounding. None is 13. The figure was measured by the [#1574](https://github.com/vamseeachanta/digitalmodel/issues/1574) lane over its own scope and term. The *mechanism* holds and a public package path (`src/digitalmodel/installation/installation_pamphlet/`) does embed a token followed by `_`; the *count* does not transfer |
| Exclude content-addressed fields (a lockfile wheel hash) by **classification**, not by boundary rule | **CONFIRMED** | In `uv.lock` a derived token matches **4× unbounded / 0× word-bounded** — it occurs only inside hash strings. Bounding would drop those for the wrong reason; unbounded matching surfaces them and they must be excluded because of *what the field is*, not where its characters sit |
| `scripts/legal/legal-sanity-scan.sh --diff-only` can be run and reported | **NOT EXECUTABLE for this repo** | See below |
| The existing deny list would catch this identifier | **REFUTED** | Zero of workspace-hub's deny-list patterns match the page. Even a correctly functioning scanner would not flag this leak today |

### The legal scan cannot verify this fix, and saying otherwise would be a vacuous criterion

`scripts/legal/legal-sanity-scan.sh` **does not exist in digitalmodel**. Using
workspace-hub's copy, both available invocations fail for this purpose:

- **`--repo=<name>` is fail-open.** workspace-hub issue
  [#3804](https://github.com/vamseeachanta/workspace-hub/issues/3804) is **OPEN**
  and documents that `resolve_repo_path()` is defined twice, the second
  definition returning via a global while both call sites capture stdout — so the
  scan path is the empty string. Reproduced here: running it against this
  worktree, **which contains the leak**, printed `Scanning: dm-1965-plan ()` and
  `RESULT: PASS`, rc=0. The empty parentheses are the empty path.
- **The root form scans the wrong tree.** Invoked without `--repo` from inside
  this worktree it reports `Scanning: workspace-hub (root)` — it scans
  workspace-hub, not digitalmodel.
- digitalmodel has no `.legal-deny-list.yaml` of its own, and the global list
  does not contain this identifier class anyway.

**Any acceptance criterion of the form "the legal scan passes" would therefore be
satisfied by a scan of nothing.** That is precisely the vacuous-result failure
this plan must avoid, so the criterion is withdrawn and replaced by a repo-local,
executable detector (D4).

### The leak is 3.25× larger than filed

| file | occurrences | in the issue? | in the ownership registry? |
|---|---|---|---|
| `docs/api/hydro/ocimf-coefficient-explorer.html` | **8** | yes | yes — `PAGE_EXCLUSIONS:126` |
| `docs/domains/charts/phase2/ocimf/ocimf_coefficient_explorer.html` | **8** | **no** | **no** — outside the gate's `docs/api/**` census entirely |
| `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | **10** | no (the producer) | n/a |
| **total** | **26 across 3 files** | | |

The second published page is a near-duplicate of the first under `docs/domains/`,
carrying the same eight paths and covered by **no** gate. Fixing only the page
named in the issue would leave an identical leak live. Across all of `docs/`,
24 files contain the internal root; the two above are the only **HTML** ones.

---

## Deliverable

Published pages that state *which* documents produced their data without stating
*where those documents live*, with the producer fixed so the paths cannot return
on the next regeneration, and a repo-local test that fails if they do.

---

## Resource Intelligence Summary

### Existing repo code

- `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` — the
  producer. Module-level constants `XLSX` and `OUT` at `:16-17` are hardcoded
  absolute paths; the provenance table is a hardcoded HTML block at `:567-575`
  (six rows, each with a `<td class="path">`); the footer at `:599` embeds the
  source path and credits a `/tmp/` build script.
- `scripts/generated_html_ownership.py` — `EXCLUDED_GENERATORS:64`,
  `PAGE_EXCLUSIONS:92`, `MANUAL_PAGES:161`. Every page must carry a
  classification **with a reason**, enforced at `check_generated_html.py:230-232`;
  stale classifications are errors at `:219-224`.
- `scripts/check_generated_html.py` — the drift gate. Census is `docs/api/**`.
- `tests/capabilities/test_generated_html_freshness.py` — the closest precedent
  for a published-surface test; imports `scripts.check_generated_html` and
  `scripts.brand_guard` directly. The new detector follows its shape.

### Gaps identified

Nothing in the repo asserts anything about the *content* of published pages
beyond brand tokens and generator agreement. There is no check that a public page
does not disclose internal infrastructure, and the one external check that might
have caught it is fail-open and lacks the pattern.

### Evidence

**Issue states** (2026-08-04): `#1965` OPEN (no labels) · `#1961` OPEN
(`status:needs-plan`, hard-gated on workspace-hub #3522 Phase B) · `#1574` CLOSED ·
`#1888` CLOSED · workspace-hub `#3804` **OPEN**.

---

## Design decisions

**D1 — Remove internal absolute paths entirely; do not scrub identifiers out of
them.** This is the issue's own first suggestion and it is the stronger fix.
Scrubbing an identifier leaves the share layout, the directory names and the
project code — and it requires deciding, per string, which substring is
sensitive. Removing the path removes the whole class in one move, needs no deny
list, and survives the identifier changing. It also makes the matcher-bounding
question moot for this remediation (D5).

**D2 — Fix the producer *and* both committed pages. Either alone is incomplete.**
- Producer only → the committed pages still leak, because the drift gate does
  **not** regenerate an excluded page (premise table). Nothing would republish
  them.
- Pages only → the next regeneration reintroduces all eight paths verbatim,
  because they are hardcoded in the producer's source at `:567-575`.

Since the producer is in-repo (premise refutation), the repo's fix-at-the-
generator rule applies here after all — the issue concluded it did not.

**D3 — Provenance keeps its evidentiary value through content, not location.**
The table exists for a real reason: a reader must be able to tell which document
produced a coefficient. Each row keeps the human-readable document title and
edition it already carries, and the absolute path is replaced by a **sha256 of
the source file**. A content hash identifies the exact artifact more precisely
than a path does — a path can be moved or overwritten — while disclosing nothing
about where it lives. Rows whose source cannot be hashed at build time state that
plainly rather than falling back to a path.

**D4 — The regression gate is a repo-local pytest, because every external gate is
unavailable.** It cannot depend on:
- [#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961)'s scanner — deferred and hard-gated (D6);
- the deny list — does not contain this identifier class;
- `legal-sanity-scan.sh` — fail-open and wrong-tree (premise table);
- `check_generated_html.py` — does not cover the second page and does not
  regenerate the first.

The test scans **published HTML across `docs/`** — not just `docs/api/**`, which
is the gap that hid the second page — for absolute-filesystem-path shapes, and
fails with the file and line. It matches on **path shape**, not on identifier
strings.

**D5 — Path-shape matching sidesteps the bounding question; the unbounded rule is
recorded for [#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961), not applied here.**
A detector looking for `/<root>/<segment>/…` or a drive-letter path has no token
boundaries to get wrong. That is a positive reason to prefer it over identifier
matching, on top of D1. The verified matcher findings — unbounded substring, and
exclusion of content-addressed fields **by classification** — are real and belong
to the identifier-matching problem, which is [#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961)'s. They are carried into this
plan's Out of Scope section as an input to that issue rather than half-applied
here.

**D6 — This plan does not wait for [#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961), and says so explicitly.**
[#1965](https://github.com/vamseeachanta/digitalmodel/issues/1965) is the *immediate leak*; [#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961) is the *durable guard*. [#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961) is `status:needs-plan` and
hard-gated on workspace-hub #3522 Phase B, so waiting would leave a live
disclosure on a public page for the length of that gate. The two are
complementary: this plan removes the content and adds a narrow local detector;
[#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961) adds authenticated fail-closed scanning across the whole surface. Neither
substitutes for the other, and nothing here should be read as reducing [#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961)'s
scope.

**D7 — Narrow the registry exclusion reason rather than leaving it stale.**
The `PAGE_EXCLUSIONS` reason at `:127` cites **two** causes: an external
absolute-path workbook and random Plotly div ids. D2 removes the first. The
second remains and still justifies exclusion, so the page stays excluded — but
the reason is edited to cite only the surviving cause. Leaving a justification
that names a fixed problem is how a registry rots, and the checker validates that
reasons exist but not that they are true.

**D8 — No fitted constants.** This plan introduces no threshold. The detector's
patterns are structural (a leading `/` followed by two or more path segments; a
drive letter followed by a backslash), and its allowlist is enumerated by
classification, not by count.

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | D2/D3: `XLSX`/`OUT` at `:16-17` become CLI arguments with no absolute defaults; the provenance table at `:567-575` renders title + sha256 instead of `<td class="path">`; the footer at `:599` drops the source path and the `/tmp/` script credit |
| Modify | `docs/api/hydro/ocimf-coefficient-explorer.html` | the 8 occurrences; page is excluded from regeneration, so it is corrected in place |
| Modify | `docs/domains/charts/phase2/ocimf/ocimf_coefficient_explorer.html` | **not in the issue** — the same 8 occurrences, covered by no gate |
| Modify | `scripts/generated_html_ownership.py:127` | D7: narrow the exclusion reason to the surviving cause |
| Create | `tests/legal/test_published_pages_have_no_internal_paths.py` | D4 detector |
| Update | `docs/plans/README.md` | index row |

**Explicitly untouched:** `src/digitalmodel/hydrodynamics/diffraction/`,
`tests/hydrodynamics/diffraction/`, `docs/benchmarks/unit_box/`,
`src/digitalmodel/solvers/openfoam/`, and workspace-hub's deny list (cross-repo).

---

## TDD Test List

| Test | Input | Expected | Red today because |
|---|---|---|---|
| `test_published_html_has_no_absolute_filesystem_paths` | every `.html` under `docs/` | empty violation list | **8 + 8 = 16 violations across two pages today** |
| `test_the_page_census_is_not_empty_and_includes_the_known_pages` | the detector's own glob | census is non-empty **and** contains both known page paths by name | **anti-vacuity guard** — without it, a glob that matches nothing makes the detector above pass forever. No such guard exists |
| `test_producer_emits_no_absolute_path` | run the producer's table renderer against a fixture source | rendered HTML contains no absolute-path shape | producer hardcodes them at `:567-575` |
| `test_producer_has_no_absolute_path_defaults` | import the producer module | `XLSX`/`OUT` are not absolute-path literals | `:16-17` are absolute literals |
| `test_provenance_rows_retain_document_identity` | rendered table | each row still carries its document title **and** a 64-hex sha256 | guards against "fixing" the leak by deleting the table's usefulness |
| `test_detector_catches_a_planted_path` | a temp HTML file containing a synthetic absolute path | detector reports it | proves the detector can fail — a detector never seen to fail is not evidence |
| `test_detector_ignores_a_url_path` | HTML containing an `https://` URL with path segments | no violation | URLs share the `/a/b/c` shape; without this the detector is unusable and would be weakened until it passed |

**Not included, deliberately:** no test asserting the legal scan passes (not
executable — premise table); no test asserting a specific identifier string is
absent (that would place the identifier in the repository, which is the thing
being removed); no test depending on [#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961).

---

## Acceptance Criteria

- [ ] **Every test above fails on `origin/main` @ `7b4119cc` and passes after**, with the failure list captured against a clean `origin/main` worktree and recorded in the PR body.
- [ ] **Zero absolute-filesystem-path occurrences remain in any `.html` under `docs/`.** Baseline measured 2026-08-04: **16 occurrences across exactly 2 files** (8 + 8). The criterion is zero, and the two files are named so the count cannot be met by deleting a page.
- [ ] **The producer emits none.** Re-running the table renderer against a fixture produces no absolute-path shape — so the paths cannot return on the next regeneration.
- [ ] **The detector is proven capable of failing**, by the planted-path test. A green detector that has never been observed red is not evidence.
- [ ] **The detector's census is asserted non-empty and to contain both known pages by name.** A criterion satisfiable by an empty glob is not a criterion.
- [ ] Each provenance row still identifies its source document by title/edition **and** a sha256 — the leak is removed without removing the table's evidentiary purpose.
- [ ] `pytest tests/ -q` compared **node-ID by node-ID** against a baseline captured in this worktree at the branch point. No new failure node IDs.
- [ ] `python scripts/check_generated_html.py --check` passes, and `scripts/generated_html_ownership.py`'s reason for this page names only causes that are still true.
- [ ] **No identifier, internal path, share name or project code appears in any commit message, branch name, PR title, or issue comment** produced by this work. Verified by reading the branch's full log before opening the PR.
- [ ] **The legal-scan criterion is deliberately absent.** Its omission is a finding, not an oversight: workspace-hub [#3804](https://github.com/vamseeachanta/workspace-hub/issues/3804) makes the per-repo form fail-open, and it was observed returning PASS on this very worktree while the leak was present.
- [ ] r1 review artifact recorded.

---

## Out of scope

- **[#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961), the authenticated fail-closed scanner.** Deferred and hard-gated; this plan neither waits for it nor narrows it (D6). **Two verified inputs are handed to it:** (1) identifier matching must be **unbounded substring** — `\b` fails wherever a token is followed by `_`, measured at 9/26/11 occurrences for the tokens derivable here; (2) content-addressed fields such as lockfile hashes must be excluded **by classification**, not by boundary rule — measured 4× unbounded / 0× word-bounded in `uv.lock`. The widely-quoted "13 false negatives" figure did **not** reproduce at repo scope and should not be restated without re-measurement.
- **Adding this identifier class to workspace-hub's deny list.** Cross-repo, and it belongs with [#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961)'s scope manifest. **Named as a required follow-on**, because without it even a repaired scanner would not flag this leak — verified: zero deny-list patterns match the page today.
- **Fixing workspace-hub [#3804](https://github.com/vamseeachanta/workspace-hub/issues/3804)**, the fail-open legal gate. Different repo. Reported here because it silently invalidates any legal-scan evidence cited by digitalmodel work.
- **The other 22 files under `docs/` containing the internal root.** They are not HTML and not published capability pages. They deserve their own sweep; widening here would repeat the unreviewed-scope-widening that created this issue.
- **Making the producer deterministic** so the page could leave `PAGE_EXCLUSIONS` — the random Plotly div ids are a separate concern (D7).

---

## Adversarial Review Summary

| Round | Provider | Verdict |
|---|---|---|
| r1 | Claude — inline, main session | **MAJOR** — 6 findings, all folded in |

1. **The draft required running the legal scan, as the briefing asked.** The
   script does not exist in this repo; workspace-hub's `--repo=` form is
   **fail-open** under OPEN issue [#3804](https://github.com/vamseeachanta/workspace-hub/issues/3804), and was **observed returning PASS on this
   worktree while the leak was present**; the root form scans workspace-hub. The
   criterion was **satisfiable by a scan of nothing** — the exact vacuity trap.
   → withdrawn, replaced by D4, and its absence made an explicit criterion so a
   reviewer does not "helpfully" restore it.
2. **The draft inherited "it's a generated page, fix the generator and
   regenerate".** The page is in `PAGE_EXCLUSIONS`, which the gate does **not**
   regenerate, so "regenerate" was not an executable step. But the issue's own
   opposite conclusion — that there is no in-repo generator — is **also** wrong.
   Both errors folded into D2, which fixes producer *and* pages and explains why
   neither alone suffices.
3. **The issue's scope was one page; there are two.** A near-duplicate under
   `docs/domains/` carries the same eight paths and is covered by **no** gate,
   because the drift gate's census is `docs/api/**`. Fixing only the filed page
   would have left an identical live leak. → Files to Change; the detector spans
   `docs/`, not `docs/api/`.
4. **The draft restated "13 false negatives" as fact.** It does not reproduce at
   repo scope (measured 10/0/0/372/211). The *mechanism* is real and verified;
   the *number* is not transferable. → premise table records mechanism-confirmed,
   count-not-reproducible. This is the briefing's own warning about carrying
   forward a measured claim into a scope where it was not measured.
5. **The unbounded-matcher rule is not load-bearing here, and applying it anyway
   would have looked like diligence.** This page's tokens are hyphen-delimited,
   so bounding misses none of them. → D5 records the rule for [#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961) and chooses
   path-shape matching, which has no boundaries to get wrong.
6. **The detector could have passed vacuously two ways** — an empty glob, and a
   pattern so narrow it never fires. → `test_the_page_census_is_not_empty…`,
   `test_detector_catches_a_planted_path`, and `test_detector_ignores_a_url_path`
   (without the last, the detector would be weakened until it stopped firing on
   URLs, which is how these checks die).

**Verdict: ready for owner review.** No blockers outstanding.

---

## Risks and Open Questions

- **Risk — the page stays in `PAGE_EXCLUSIONS`, so the drift gate still will not
  regenerate it.** After this fix the committed page and the producer agree, but
  nothing enforces that they stay agreed; only the new detector protects the
  *specific* leak class. Making the producer deterministic is the durable answer
  and is out of scope (D7). Recorded rather than hidden.
- **Risk — the sha256 provenance (D3) is only as useful as the reader's access
  to the source.** For published standards the title and edition already identify
  the document; the hash disambiguates the exact digitisation. For the
  project-bundled row there may be no public counterpart, in which case that row
  should say so rather than imply retrievability.
- **Open question for the owner — should the project-bundled provenance row
  survive at all?** It refers to a project-specific artifact. Removing the path
  removes the disclosure, but the row's continued presence still discloses that
  such a bundle exists. This plan removes the path and keeps the row; deleting
  the row entirely is a defensible stricter choice and is the owner's call.
- **Unverified — whether these two pages are actually reachable on a public
  surface.** They are committed under `docs/` and the issue describes them as
  published. This plan treats them as public, which is the safe assumption, but
  it does not verify the hosting path.

---

## Complexity: T2

One producer script, two committed pages, one registry reason, one new test file.
No cross-repo change, no new dependency. Not T1: the blast radius was
mis-scoped in the issue, the remediation had to be re-derived after two refuted
premises, and the verification route had to be rebuilt after the intended gate
turned out to be fail-open.
