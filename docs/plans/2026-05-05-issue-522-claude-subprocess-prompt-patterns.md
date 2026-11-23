# Plan: digitalmodel #522 — Codify ultra-constrained Claude subprocess prompt patterns for issue-scope reviews

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/522
**Status:** plan-review
**Tier:** T3

## Context

Follow-up from #515 and the subprocess prompt-pack experiments executed for #517-#520. Empirical findings:
- `claude -p` works in subprocess mode; `--bare` causes auth invisibility
- broader prompts hit `Reached max turns`
- one-issue-per-run helps, but #518/#519/#520 still need more aggressive prompt compression

We have one proven-working prompt format for #517 review. This issue is to **standardize** it — capture the pattern, the invocation contract, and a minimal harness — so future issue-program automation in this repo is reliable.

Sibling issue: #523 harvests the #517 subprocess output into actionable tasks. This issue is the meta-pattern; #523 is the harvest. **Recommended order: do #522 first** (codify the pattern) so #523's harvest can cite the canonical template.

## Plan

1. **Recover the working artifact set.** Run `ls /tmp/digitalmodel-gh-515-prompt-pack/runs/20260411-162709/` to confirm the #517 successful run still exists. If yes, copy the input prompt + output to `docs/sessions/2026-05-05-issue-522-canonical-run/` for permanence (the `/tmp` source is volatile). If gone, reconstruct from git history of the #515 program or skip step 4's "successful example" requirement.

2. **Author canonical prompt template.** New file `docs/domains/automation/CLAUDE_SUBPROCESS_REVIEW_TEMPLATE.md` with:
   - **Hard constraints** (must be in every prompt):
     - one question only
     - fixed output schema (e.g., `## Findings\n## Open questions\n## Recommended steps`)
     - max bullet count per section (e.g., 5)
     - "no implementation, no code" directive
     - explicit file allowlist (limit reads)
     - max-turn budget rationale
   - **Recommended Claude flags**: `claude -p` (file-based prompt), `--print`, `--max-turns N`, no `--bare`
   - **Failure-mode appendix**: document `--bare` auth invisibility and broader-prompt max-turn exhaustion with the symptom strings to grep for
   - **Worked example**: include the #517 successful prompt verbatim with annotations

3. **Add minimal runner harness.** New script `scripts/automation/claude_subprocess_review.sh`:
   - takes a prompt file + output dir as arguments
   - applies the recommended flags from step 2
   - logs invocation + exit code to a manifest
   - exits non-zero if `Reached max turns` appears in output
   Keep under ~60 lines. No retry logic in v1 (manual rerun is fine).

4. **Demonstrate by re-running #517.** With the harness from step 3, re-run the canonical #517 prompt and confirm output matches the original successful artifact (modulo wall-clock differences). Save the output under `docs/sessions/2026-05-05-issue-522-canonical-run/issue-517-replay.md`.

5. **Smoke check.** `bash scripts/automation/claude_subprocess_review.sh --help` (or its equivalent dry-run mode) should print usage and exit 0. Linked from `docs/domains/automation/CLAUDE_SUBPROCESS_REVIEW_TEMPLATE.md`.

## Acceptance Criteria

- [ ] `docs/domains/automation/CLAUDE_SUBPROCESS_REVIEW_TEMPLATE.md` documents the canonical prompt template, hard constraints, and recommended flags
- [ ] Working invocation contract (`claude -p` without `--bare`) explicitly named with rationale
- [ ] `scripts/automation/claude_subprocess_review.sh` runner exists and exits non-zero on max-turn exhaustion
- [ ] Replay of #517 prompt produces semantically equivalent output (artifact stored in `docs/sessions/`)
- [ ] Failure-mode appendix lists at least the two known symptoms (`--bare` auth invisibility, max-turns exhaustion)
- [ ] Doc cross-references #523 as the first consumer of the harvest pattern
