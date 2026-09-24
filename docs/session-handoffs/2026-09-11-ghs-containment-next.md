# GHS containment exit handoff
Verified: 2026-09-13.
Issue: [2089](https://github.com/vamseeachanta/digitalmodel/issues/2089), OPEN.
PR: [2102](https://github.com/vamseeachanta/digitalmodel/pull/2102), OPEN DRAFT.
Branch: chore/2089-ghs-containment-plan.
Worktree: agent-worktrees/dm-2089-containment-plan on ace-linux-1.

## Completed and verified
M1 merged at 6c89b4e30c9c224cccaef6cba4cc57885ba11153.
Containment implementation pushed at a1e170fc; main sync pushed at 2eb1d96fea5e06de8698bba8e82f731f89449d8c.
The post-sync Linux GHS suite passed 145 tests with one explicit native acceptance skip.
CI at sync commit: 30 checks, 30 successful.
Fetched origin/main at exit: 7e71d6b26eb863e95ae3f0ec9957b081d85dea28; it advanced one commit after the last sync. This exit checkpoint will not start another integration cycle.

Actual Claude/Codex MAJOR reviews, corrective main dispositions and source hashes are preserved under scripts/review/results/issue-2089/. The parent plan and approved containment addendum remain unchanged. The implementation uses the actual interpreter image and bounded process observations; these changes are not native qualification evidence.

## Blocker and exact next checkpoint
The first Windows attempt failed because a virtual-environment interpreter redirected to a different process identity. Historical process census is incomplete. Original private observations, readiness records, temporary checkout and the active interlock remain preserved. Their presence was checked at exit; no new process-liveness claim is made.

Explicit one-time recovery approval is still pending. See docs/plans/2026-09-11-issue-2089-first-attempt-recovery.md and its bound source manifest. A general request to continue, sync or exit does not waive this exception.

On resumption:
1. Check live sessions, issue comments, branch state and claim availability; confirm no later recovery disposition or implementation supersedes this handoff.
2. Obtain explicit approval for the documented one-time exception before modifying the marker.
3. After approval, recheck live processes; preserve original evidence and hashes; write the private operator-disposition record linking explicit authorization and implementation identity; archive only the matching marker without overwrite as unresolved-operator-waived.
4. Run one fresh bounded harmless Windows qualification with the approved source identity. Any new incomplete cleanup remains unresolved.
5. Review actual native evidence before merging PR2102. Keep issue2089 open for the later licensed capture and reviewed native-result comparison.

No GHS, Part Maker or other vendor execution occurred. No native retry, marker clearance, merge or issue closure occurred during sync/exit. External actions were limited to authorized GitHub publication and repository synchronization. Licensed capture and native parser/format work remain separate approval checkpoints.

## Cleanup and preserved state
CLEAN: task worktree before this handoff edit; no task stash; prior cooperative claim released; subagents completed.
EXPECTED: this handoff will be committed and pushed; task branch/worktree retained for continuation. Private failed-attempt evidence, marker and temporary checkout remain required recovery evidence.
EXPECTED: Windows digitalmodel main remains at 87d56cac637f971ca3ed57d8ca98c16a845ff0f7. Its seven pre-existing docs/benchmarks/unit_box edits blocked fast-forward and remain untouched. No stash/reset was used.
Windows llm-wiki and llm-wiki-acma working trees were clean at exit. Canonical Linux checkout and sibling worktrees will remain outside this exit operation.
Cleanup scope is the task worktree and named preserved locations, not an ecosystem-wide deletion or process audit.

Exit handoff review: independent Codex read-only review approved the documentation; its minor request to make the private operator-disposition record explicit was incorporated. GitHub state and CI totals were verified by the main session. Legal scan passed against the resolved task worktree.
