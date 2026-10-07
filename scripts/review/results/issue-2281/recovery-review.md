# CP 2281 recovery review

Date: 2026-10-05. Base: `8609ee8d76a48710dd3b839142e81e5669a293f4`.
Scope: post-merge source fixes for mutable archived decisions and false source
attribution, plus truthful public guide status. Published packs and numerical
results remain untouched; coverage remains 11/16.

## Plan and code review

- Codex plan review: proceed with mandatory nested snapshot copies, explicit
  observed revision, root/import checks and rejection before output/calculation.
- Codex code review found unchecked lazy offshore imports and reporting helpers.
  The known routes are now preloaded without solver execution and checked; helper
  origins are checked. Final re-review reported no blocking findings.
- Claude first review requested changes for revision changes between cases,
  hidden index flags, guide overclaims and lost live-reload coverage.
- Claude second review requested post-render/final-index guards and a real fresh
  process preload check. Each blocking finding was corrected and tested.
- Claude final review: PLAN APPROVE; CODE APPROVE, no blockers. Actual outputs
  are retained beside this receipt as `recovery-claude-r1.md`,
  `recovery-claude-r2.md` and `recovery-claude-r3.md`.
- Gemini: UNAVAILABLE. Current headless invocation returned an authentication
  configuration error, with no review verdict. No credential or settings change
  was made. No full three-provider consensus is claimed.

## Verification by the implementing session

61 focused tests passed across CP portfolio provenance, review document, portfolio
contract, coverage and storage tests. Eighteen provenance tests use disposable Git
fixtures/synthetic module registries; the fresh subprocess imports actual routes
without running them. Node executes actual UI binding/save/load/copy and snapshot
immutability checks. Numerical engines were not invoked for a report build.

Meaningful red probes reproduced archived pending becoming accepted, edits hidden
by assume-unchanged/skip-worktree, commit changes across preload/cases and a final
foreign rendering import receiving a success summary. Final checks pass.

Touched Python Ruff checks and builder mypy passed. Staged identifier scan had no
findings or uninspectable files; added-path and legal scans passed. Original
release-register comparison checked all 130 artifact SHA-256 values: zero
mismatches. This is source/reporting verification, not engineering acceptance.

## Limits and next publication gate

The explicit revision pin is a trusted internal CLI interface; direct calls without
it reject preloaded implementation code. Repository source checks do not qualify
Python environment injection, ignored resources/bytecode, citation-wiki revision,
engineering inputs or source rights.

Existing released HTML retains the original embedded UI. Before another candidate
publication, resolve Claude's non-blocking successful-build-receipt and consistent
revision validation advisory. No new candidate or publication was performed.
Private A correction still requires its authorized exact artifact/receipt and
owning private repository. The handoff records the precise missing-input question.
