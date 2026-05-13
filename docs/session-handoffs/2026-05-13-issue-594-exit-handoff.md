# Exit handoff — Issue #594 vessel operability reference synthesis

Date: 2026-05-13T12:09:41-05:00
Repository: `digitalmodel`
Issue: [#594 Vessel operability reference synthesis for CTV/SOV/W2W suitability](https://github.com/vamseeachanta/digitalmodel/issues/594)

## Current state

Issue #594 is closed. The final implementation is on `main` and pushed to `origin/main`.

Primary final commit:

- `bc528dc0780d182c8f57ac76bda73ce4471b224a` — `docs: reclassify CTV operability reference`

Plan/review/proof commits immediately below it:

- `eeba8bdf` — `docs: record approval for issue 594 plan`
- `1f3c414a` — `docs: add issue 594 plan rereview approvals`
- `7de25758` — `docs: add issue 594 vessel operability restart plan`
- `846b17e4` — `docs: update CTV operability handoff proof`

## What was changed

The CTV/Kincardine operability material was reclassified from GTM/demo output to internal reference material.

Key artifact movements and edits:

- Removed GTM fixture path: `examples/demos/gtm/data/ctv_operability_kincardine.json`
- Added internal reference fixture: `references/vessel-suitability/data/ctv_operability_kincardine.json`
- Added synthesis: `references/vessel-suitability/ctv-sov-w2w-operability-reference-synthesis.md`
- Updated reference README: `references/vessel-suitability/README.md`
- Removed CTV/Kincardine from GTM output surfaces and indexes.
- Added/updated tests: `examples/demos/gtm/tests/test_ctv_operability_reference.py`

Boundary now enforced:

- CTV operability fixture is internal reference only.
- GTM/client-facing reuse requires explicit approval and source-rights check.
- GTM pack output no longer references CTV/Kincardine material.

## Verification evidence

Live issue state checked during exit:

```json
{"closedAt":"2026-05-13T14:06:49Z","labels":["enhancement","cat:engineering","priority:medium","status:closed"],"number":594,"state":"CLOSED","title":"Vessel operability reference synthesis for CTV/SOV/W2W suitability","url":"https://github.com/vamseeachanta/digitalmodel/issues/594"}
```

Live targeted test rerun during exit:

```text
cd /mnt/local-analysis/workspace-hub/digitalmodel
uv run pytest examples/demos/gtm/tests/test_ctv_operability_reference.py -q
# 5 passed in 1.26s
```

Live GTM output leakage check during exit:

```text
cd /mnt/local-analysis/workspace-hub/digitalmodel
grep -RIlE 'ctv_operability|CTV access-operability|Kincardine' examples/demos/gtm/outputs 2>/dev/null | wc -l
# 0
```

Live whitespace diff check during exit:

```text
cd /mnt/local-analysis/workspace-hub/digitalmodel
git diff --check
# no output
```

## Repo state proof before this handoff commit

`digitalmodel` before writing this handoff:

```text
branch: main
HEAD:   bc528dc0780d182c8f57ac76bda73ce4471b224a
origin/main: bc528dc0780d182c8f57ac76bda73ce4471b224a
ahead/behind: 0 / 0
status: clean
```

`workspace-hub` control repo was inspected but not modified by this handoff. It was not clean and should not be mixed into the `digitalmodel` closeout commit:

```text
branch: main
HEAD:   760d3b5a2f2cf93c5b8b7bb65ea753d00b344b0c
origin/main: 5a3d4246361894a30c20729ee64b517c58640fcd
ahead/behind: 5 / 1
status: dirty, with session/generated/review/report churn including .claude/state, logs/orchestrator, scripts/review/results, docs/reports, and logs/quality paths
```

## Branch/worktree disposition

- Active worktree: `/mnt/local-analysis/workspace-hub/digitalmodel` on `main`
- Issue branch `issue-594-vessel-operability-plan`: removed locally and remotely before exit.
- Preserved unrelated local branch: `preserve/catenary-canonicalize-cec18733`
  - Reason: it contains a pre-existing unrelated local commit that was preserved before resetting `main` to clean `origin/main`.
- `git worktree list` shows only the main digitalmodel worktree.

## External-action status

No external send/action was performed during exit closeout beyond GitHub issue/status reads and the repository commit/push workflow.

## Restart notes

If future work resumes here:

1. Start from `digitalmodel/main` after `git fetch origin main`.
2. Treat CTV/Kincardine assets as internal reference only unless the user explicitly approves GTM/client-facing reuse.
3. If using the preserved `preserve/catenary-canonicalize-cec18733` branch, first verify whether that unrelated commit is still needed before merging or deleting it.
4. Do not use the current dirty `workspace-hub` control repo state as evidence of this issue's implementation state; re-check it separately if doing workspace-hub closeout.
