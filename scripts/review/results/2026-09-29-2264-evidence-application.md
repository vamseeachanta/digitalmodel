# Evidence application review and validation

Issue: [2264](https://github.com/vamseeachanta/digitalmodel/issues/2264).
Authorization: user application-step request; local commit only. No push, PR, issue,
comment, label or other remote mutation.

## Adversarial review

Native read-only Codex reviewer assessed plan and code independently of the writing lane.
Plan verdict MINOR: require both numeric/provenance boundary paths, retain geometry-only
ICCP behavior, avoid supplier confirmation claims, expose ICCP evidence through its
raw-source bypass, and correct approximate coefficient wording. All addressed.
Code verdict MINOR, documentation only: galvanic checklist needed per-row classes/gaps;
past-tense reproduction claims needed separation from future-tense plan. Both addressed
in the main session. No calculation or gate regression identified.
Claude and Gemini executables were unavailable on PATH; this is a single-provider review,
not cross-provider consensus. No extra provider authentication or remote action attempted.

## Verification

- TDD first run: 13 failed, 1 passed in new evidence tests before implementation.
- Focused CP regression: 112 passed.
- Final full authorized scope: tests/cathodic_protection and tests/reporting,
  1575 passed, 1 skipped, 1 xfailed, zero failures/errors.
- Runner: Python UTF-8 mode, local src first, editable finder removed, pytest addopts
  cleared. An isolated temporary beautifulsoup4 dependency resolved the pre-existing
  engine-import failure; the shared environment was not modified.
- Ruff check: clean on all six touched Python files.
- Mypy --follow-imports=silent --ignore-missing-imports: clean on the same six files.
- Workspace scripts/legal/legal-sanity-scan.sh --repo resolver returned an empty
  path because duplicate resolver definitions disagree on their return contract.
  Its printed PASS was rejected as evidence. The unchanged scanner function body
  was then invoked directly against an enumerated isolated copy of all 13 task
  files, with canonical global and repository deny lists: 0 block violations,
  0 warnings. Temporary driver/copies were removed; the shared scanner was not
  modified. Its resolver repair is outside this user-directed application scope.
- git diff --check: clean.

## Evidence coverage and retained gaps

12 stray-current records and 41 ICCP material/environment/utilisation rows are enumerated
in the regenerated inventory. Graphite nominal density is removed; its Table 1 maximum
is documented separately with required measured mass. Galvanic kinetics remain required
inputs, not fabricated default records. All provisional flags and experimental gates
remain in force. Standards qualification, extraction rights, normative AC equality
operators and supplier/service-specific qualification are deferred per the crosswalk.

Generalizable review lesson: where adjacent formulas agree numerically at an endpoint,
a boundary regression must also assert the selected record/provenance. The exact-200
regressions preserve this requirement for future table changes. Promoted to
`.claude/rules/cp-boundary-provenance.md` for future discovery.

## Final repository and cleanup state

Git add failed creating the worktree index.lock: Permission denied. No commit was
created; index verified empty and all 13 task files remain unstaged/untracked.
No push or remote mutation occurred.

CLEAN: no session-created scratch dependency/cache or scan directory remains; no
stash, cleanup lock/trash stage or session handoff residue was introduced.
EXPECTED: the 13 application files await staging/commit in a writable Git session;
ignored supplied evidence and standard pytest/ruff caches remain.
UNEXPECTED: none detected in the task worktree. Sibling worktrees were inspected
by git worktree list and were not modified.
