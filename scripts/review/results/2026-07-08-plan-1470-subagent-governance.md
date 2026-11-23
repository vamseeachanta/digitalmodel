## Verdict
MAJOR — not approval-ready.

## Retrieval
Verified locally:
- Plan: `docs/plans/2026-07-08-issue-1470-registry-batch-coverage.md`
- Review artifacts: `scripts/review/results/*1470*`
- Live issue: `gh issue view 1470` showed OPEN with `cat:engineering`, `lane:codex`, no `status:*`, no comments
- Leakage controls: `docs/riser_database.md`, `tests/riser_database/test_leak_gate.py`, `test_provenance_tripwire.py`
- Checked commands: legal scan path and `scripts/enforcement/check-no-abs-paths.sh` against the plan/review files

Incomplete checks:
- Did not rerun the batch reproduction command.
- Did not verify drive-index search output.
- Did not verify live llm-wiki claim state.

## Findings
- MAJOR: Review-state evidence is false/incomplete. Plan lines 10 and 99-100 cite `2026-07-08-plan-1470-codex.md`, but that file does not exist. Existing `*1470*` provider artifacts are all `UNAVAILABLE`, not adversarial review evidence.

- MAJOR: Governance index requirement is waived without replacement. Plan lines 53 and 68 say `docs/plans/README.md` is missing and will not be created, but the issue-planning workflow requires a plan index row or an explicit equivalent state surface.

- MAJOR: Public/private leakage controls are under-scoped. Plan lines 75-77 include raw local paths, and failed review artifacts include local runtime paths. Plan line 178 only bans private source identifiers and wiki-side numeric values in commits/PRs/comments; it omits plan files and review artifacts.

- MAJOR: Legal scan command is non-executable as written from this topology. `workspace-hub/scripts/legal/legal-sanity-scan.sh --repo=digitalmodel --diff-only` returned `ERROR: Repository not found`.

- MAJOR: `--diff-only` can miss the relevant files. Current plan/review files are untracked, while `git diff --name-only HEAD` is empty for untracked files. The plan does not require staging or an all-file scan before the leakage gate.

- MAJOR: Approval gate is not locked in the plan. It does not require `status:plan-approved` plus `.planning/plan-approved/1470.md` bound to the reviewed plan revision before implementation.

- MINOR: TDD list calls `test_registry_batch_current_baseline_before_wave6` a RED baseline, but its expected output is the current 8/21 behavior, so it is not a failing pre-implementation test.

## Blockers
- Produce real no-MAJOR review artifacts or keep the plan in draft.
- Fix artifact map/review summary to match actual files.
- Add an executable leakage/legal gate that scans this worktree's plan, review artifacts, and future PR text before push.
- Add explicit user approval marker/label preflight before implementation.
- Resolve the missing plan-index governance surface.
