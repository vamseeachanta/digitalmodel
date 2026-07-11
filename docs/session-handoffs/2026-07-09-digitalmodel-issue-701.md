# Handoff — digitalmodel: issue 701 Linux-first dependency repair

**Session:** codex-acma-hou-rds02-701b  •  **UTC:** 2026-07-09T23:36:06Z  •  **Tracking issue:** #701

## Goal

On a Linux ecosystem host, remove the unused root `cx-Oracle==6.3.1` and
`hiredis==2.2.3` dependencies, make the CI/clean-install dependency graph
reproducible, restore Windows workflow coverage, and raise a reviewed PR; use
ACMA-HOU-RDS02 only for post-push Windows/licensed verification.

## Current state

- Owner direction: software issue resolution, dependency decisions, lock
  generation, implementation, commits, and PR work happen on Linux before any
  Windows verification.
- `digitalmodel/main` and `origin/main` are both
  `4ab92d9063e698147bc8e63bbdef139bf868bc32`.
- No tracked file was changed, no branch/commit was created, and no PR was
  opened from ACMA-HOU-RDS02.
- Windows reproduction with uv 0.11.25 and Python 3.12.13 reaches
  `cx-Oracle==6.3.1`, which requires MSVC; an independent probe shows the next
  obsolete root pin, `hiredis==2.2.3`, also requires compilation on Python 3.12.
- Tracked source has no active `cx_Oracle`, `oracledb`, or hiredis import. The
  dashboard Docker requirements retain their separate hiredis pin and are out
  of root-project scope.
- A local, uncommitted plan and review scratch set exists only on
  ACMA-HOU-RDS02. Round-10 Claude returned MINOR; Codex returned MAJOR. These
  files were deliberately not published because the owner redirected all issue
  resolution to Linux.

## Next steps (ordered)

1. On a Linux ecosystem host, synchronize `digitalmodel`, `assetutilities`,
   `workspace-hub`, and `llm-wiki`; confirm a clean `digitalmodel/main` at or
   after the base SHA above.
2. Read issue #701 and acquire `issue/digitalmodel/701` through the canonical
   ASCP helper. After posting the claim, wait briefly, re-read the thread, and
   verify that the new claim is the unique latest unexpired owner.
3. Rebuild the issue plan on Linux and resolve the round-10 Codex blockers:
   choose one reproducible registry graph for CI, clean-host proof, and paired
   regression; pin uv; explicitly handle the local editable
   `../assetutilities` source; isolate baseline/final environments and allow
   only enumerated new test nodeids; require push-before-release and exact
   remote-head pickup; bind approval to the reviewed plan/review blob SHAs.
4. Run two usable no-MAJOR plan reviews, publish the plan evidence, move #701 to
   `status:plan-review`, and obtain explicit user approval plus a SHA-bound
   `.planning/plan-approved/701.md` marker before implementation.
5. Implement with TDD on Linux, generate the dependency artifacts there, run
   Linux validation, commit/push every resumable file and this handoff, and open
   the draft PR. Verify remote HEAD before releasing the claim.
6. Only after the Linux branch is pushed, use ACMA-HOU-RDS02 to fetch the exact
   remote commit and run Windows Python 3.11/3.12 clean-install/workflow tests,
   locked solver sync, `digitalmodel --help`, and OrcFxAPI import. Do not start
   or enable Deckhand; its scheduled task must stay Disabled.

## Key files / commands

- Issue: `https://github.com/vamseeachanta/digitalmodel/issues/701`
- Root metadata: `pyproject.toml`
- Local-source lock: `uv.lock` (`assetutilities` currently resolves as editable
  `../assetutilities`)
- CI: `.github/workflows/workflow-automation-tests.yml`
- Windows scratch plan (not published):
  `docs/plans/2026-07-09-issue-701-oracle-dependency-isolation.md`
- ASCP helper: `../llm-wiki/scripts/coordination/claim.py`
- Approval precedent: `.planning/plan-approved/596.md`

## Gotchas

- Existing CI uses `uv pip install --no-sources -e ".[dev]"`; that is an
  unlocked registry resolution and does not consume `uv.lock`.
- `uv.lock` embeds the sibling path `../assetutilities`; detached worktrees or
  GitHub runners without that sibling cannot use it as written.
- Registry `assetutilities` 0.0.8 is available, while the local editable checkout
  reports 0.1.1. Do not silently treat these as the same graph.
- A base-SHA baseline becomes installable only after removing the two blockers;
  document that masking boundary and keep focused no-consumer/cache tests.
- ASCP's helper is check-then-post. Re-read after acquire/renew and immediately
  before push/release; stop on ambiguous ownership.
- Windows is verification-only for this issue. Do not resolve or regenerate
  project dependency state here.

## Open claims to release

- `issue/digitalmodel/701`, session `codex-acma-hou-rds02-701b`: released in the
  issue thread when this handoff was posted.

## How to resume

On a Linux ecosystem host, read issue #701, acquire and verify the ASCP claim,
then execute the ordered steps above. Do not rely on the untracked Windows
scratch files; the GitHub handoff comment is the cross-machine source of truth.
