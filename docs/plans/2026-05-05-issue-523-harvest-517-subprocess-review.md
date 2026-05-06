# Plan: digitalmodel #523 — Harvest #517 subprocess review into actionable tasks for #515 program

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/523
**Status:** plan-review
**Tier:** T3

## Context

Follow-up from #515 and the one successful subprocess review output for #517 (captured at `/tmp/digitalmodel-gh-515-prompt-pack/runs/20260411-162709/issue-517.out.md`). The captured review produced three concrete recommendations:
1. create a shared taxonomy/classification Python module
2. normalize `Bool ↔ Yes/No` in semantic comparison
3. refactor comparison logic to use a structured registry instead of ad hoc sets

**This is a meta / harvest issue — it routes insights into existing work, it does not implement code itself.** Sibling: #522 codifies the subprocess-review pattern; this issue uses one such review's output. **Recommended order: #522 first** (canonical template) → **#523 second** (harvest using the template's vocabulary).

**Boundary respect:** #515 is `status:plan-approved`. This plan does NOT modify #515's plan. It only:
- maps the #517 review's recommendations into the trio's child issues (#517/#518/#519, all `status:plan-review` from this batch)
- files new sub-issues if a recommendation has no existing home
- comments on #515 with a summary so the parent program reflects the harvest

## Plan

1. **Recover the captured review.** `ls /tmp/digitalmodel-gh-515-prompt-pack/runs/20260411-162709/issue-517.out.md`. If still present, copy verbatim to `docs/sessions/2026-05-05-issue-523-harvest-source.md` (the `/tmp` source is volatile). If missing, recover from git history of the #515 program OR document the recovery failure and proceed with the three recommendations enumerated in the issue body.

2. **Map each recommendation to an existing or new issue.** Produce a table in the harvest doc:
   - **Rec 1 (shared taxonomy module)** → already covered by **#517** plan task 3 (`semantic_diff/taxonomy.py`). No new issue.
   - **Rec 2 (normalize Bool ↔ Yes/No)** → covered by **#519** plan task 2 (Environment normalization classification). No new issue.
   - **Rec 3 (structured registry vs ad hoc sets)** → covered by **#517** plan task 3 ("structured registry, not ad hoc sets") AND **#519** plan task 3 (`_GENERAL_VIEW_SKIP_KEYS` named constant). No new issue.
   If any recommendation is NOT covered by existing trio plans, file a new GitHub issue with `discovered-via:523` and link back.

3. **Add comments to the destination issues.** On each of #517 / #518 / #519 add a comment:
   ```
   Subprocess review from #517 program (harvested via #523):
   - Rec X: [recommendation text]
   - Mapped to: [this issue, plan task N]
   ```
   This makes the routing visible in each child issue's discussion thread.

4. **Comment on #515 with harvest summary.** Single comment on #515 listing the three recommendations, the destination issue + plan-task for each, and a link to the harvest doc. Explicitly state "no #515 plan changes; routing is into already-`plan-review` children".

5. **Smoke check.** `gh issue view 523 --comments` to confirm the harvest mapping is preserved on the issue itself. Confirm new comments on #515/#517/#518/#519 are visible via `gh issue view <N> --comments | tail -30`.

## Acceptance Criteria

- [ ] `docs/sessions/2026-05-05-issue-523-harvest-source.md` contains the recovered (or reconstructed) review output
- [ ] Each of the three captured recommendations is mapped to an existing trio-plan task or a new sub-issue
- [ ] #517, #518, #519 each receive a routing comment naming the recommendation and plan task
- [ ] #515 receives one summary comment with the harvest mapping
- [ ] `git diff src/` is empty (this is a routing/documentation issue, not a code change)
- [ ] No modifications to the #515 plan document (boundary preserved)
