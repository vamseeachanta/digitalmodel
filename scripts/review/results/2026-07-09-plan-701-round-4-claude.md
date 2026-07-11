## Verdict

MINOR

## Retrieval

- Read `docs/plans/2026-07-09-issue-701-oracle-dependency-isolation.md` in full (352 lines).
- Read `pyproject.toml` in full — verified line 10 (`requires-python = ">=3.11"`), line 42 (`cx-Oracle==6.3.1`), line 55 (`hiredis==2.2.3`), line 48 (`fastapi-limiter`), `[project.scripts]` entry `workflow-automation` (line 218).
- Read `.github/workflows/workflow-automation-tests.yml` in full — verified Ubuntu-only matrix (line 19), Python 3.11/3.12 (line 20), push paths include the workflow file but omit pyproject/lock (lines 4–8), PR paths omit all three (lines 9–12), coverage-upload guard (line 49), and the four exact commands (lines 41, 45, 57, 61).
- Read `README.md` lines 80–109 — verified line 90 says "Requires Python 3.10+".
- Grep'd `uv.lock` — confirmed `cx-oracle 6.3.1` package (line 1263), `hiredis 2.2.3` package (line 2570), and both as direct dependency + requires-dist edges of the editable `digitalmodel` package (lines 1398, 1414, 1596, 1619).
- Extracted the hiredis 2.2.3 wheel list from `uv.lock` — cp311 win32/win_amd64 wheels present, **no cp312 Windows wheel**, corroborating the plan's wheel claim from repo evidence.
- Extracted wheel lists for the other pinned C extensions (`lxml 4.9.3`, `statsmodels 0.14.0`, `brotli 1.1.0`, `h5py 3.10.0`, `pyarrow 14.0.1`, `scikit-learn 1.3.2`, `pymssql 2.3.8`, `pillow 10.1.0`, `psutil`, `pykalman`) — all have cp312 win_amd64 wheels or are pure/abi3, so hiredis is plausibly the last cp312 Windows blocker at the current lock.
- Repo-wide case-insensitive grep for `cx[-_]oracle|oracledb` and `hiredis` — confirmed zero hits in `src/` (imports), `tests/`, `.github/`, `config/`; found hits the plan does NOT mention (see Findings 1–2).
- Read `src/digitalmodel/visualization/orcaflex_dashboard/backend/requirements.txt` lines 15–19 and `.../backend/app/core/cache.py` lines 1–25.
- Glob'd `tests/contracts/` (exists, six files, no `test_dependency_isolation.py` yet), `tests/workflows/workflow_automation/` (nine test files), `tests/test_cache.py`, `tests/infrastructure/core/test_cache.py`, `tests/data_systems/test_cache_manager.py`, `src/digitalmodel/infrastructure/{core,persistence}/cache.py`, `src/digitalmodel/__main__.py`, `docs/domains/structural/plate_capacity_migration.md` — all exist.
- Grep'd `plate_capacity_migration.md` — line 17 confirms "StiffnerBuckling_Cal/ - Stiffener calculations (not migrated)".
- `git log 41289638` — confirmed it is the PR #699 squash commit; `git rev-parse HEAD` = `4ab92d90…`, matching the plan's reproduction header.
- Listed `scripts/review/results/2026-07-09-plan-701-*` with sizes; Glob'd `.planning/plan-approved/` (convention exists, ~85 files) and `docs/plans/README.md` (absent, as claimed).
- NOT verified (tooling absent in this environment): `gh` CLI is not installed, so issue #701's state/labels/decision comment, the Windows host reproduction transcripts, and the 93-ref ecosystem audit remain assertions. The commit and lockfile evidence corroborate them indirectly.

## Findings

1. **Undisclosed in-tree hiredis pin.** `src/digitalmodel/visualization/orcaflex_dashboard/backend/requirements.txt:17-18` pins `redis[hiredis]==5.0.1` and `hiredis==2.2.3` — the exact version being removed — and `src/digitalmodel/visualization/orcaflex_dashboard/backend/app/core/cache.py:12-18` imports redis for that sub-service. The plan's hiredis narrative (§Resource Intelligence: "no root source imports hiredis"; §Gaps: "another unused unconditional root C extension") never mentions this file, even though the plan carefully classifies the archived Oracle docs for exactly this reason. It doesn't affect root metadata (requirements.txt is not packaged), but an implementer or reviewer running the same grep will find it and have no scope ruling. Classify it in the plan the way `StiffnerBuckling_Cal` was classified.

2. **Incomplete historical-Oracle inventory.** §Resource Intelligence states historical cx-Oracle text lives "under `docs/domains/platecapacity/StiffnerBuckling_Cal/Python_Environment/`". Grep also finds `docs/domains/articles/PY_Virtualization.md:82` (`cx_oracle=6.3.1` in a conda env listing). Same disposition (archived doc, out of scope), but the plan's claim as written is an incomplete inventory presented as complete.

3. **Review-artifact naming collision destroys round provenance.** Plan line 12 names the "current review targets" as the unsuffixed files `2026-07-09-plan-701-{claude,codex,gemini,disagreement}.md`, and the Files-to-Change table (line 205) says to "Update" them. Those names already exist on disk as round-1 leftovers: `claude.md` and `codex.md` are 0 bytes with `.err` companions (the codex `.err` is 725 KB), `gemini.md` is a 338-byte stub, and no unsuffixed `disagreement.md` exists. Writing round 4 to round-1 filenames overwrites failed-run evidence and makes rounds indistinguishable; every other round used explicit `round-N` suffixes. Relatedly, line 13 says only "round-2 artifacts … are superseded" while round-3 files also exist and are declared superseded in §Adversarial Review Summary — the two statements about what is superseded don't agree.

4. **CI base-isolation step omits hiredis.** §Workflow contract (line 186) asserts the post-install step proves `find_spec('cx_Oracle')` and `find_spec('oracledb')` are None, and `test_workflow_proves_base_oracle_isolation` (line 223) covers only Oracle modules. Yet the Deliverable (line 148) promises "neither unused root C-extension pin", and the disposable-install proof (line 271) does assert hiredis absence. hiredis regression is thus guarded only by the lock/metadata assertions, not by the runtime CI proof — one `find_spec('hiredis') is None` clause would make the CI contract match the deliverable.

5. **Type mismatch latent in the workflow-contract pseudocode.** Line 183: `assert matrix.python-version == [3.11, 3.12]` — under `yaml.BaseLoader` (which the plan itself mandates at line 189) every scalar loads as a string, so the comparison must be against `['3.11', '3.12']`. Implemented literally as written, the test fails against a correct workflow. Pseudocode, but the one place a literal transcription produces a wrong test.

6. **Externally sourced claims remain unverified in this environment.** The governing decision ("owner decided on 2026-07-09 to remove cx-Oracle entirely", §Resource Intelligence), issue labels, and `status:needs-plan` could not be checked (`gh` unavailable here); the Windows host reproduction and the 93-ref audit are session transcripts reproducible only on `ACMA-HOU-RDS02`. HEAD hash and commit `41289638` do check out, and the lockfile wheel data independently confirms the hiredis 3.11-vs-3.12 wheel claim, so nothing contradicts these assertions — but a subsequent reviewer with `gh` access should confirm the issue-decision claim before approval is recorded.

Checks performed that produced no finding: pyproject line citations (10/42/55) all exact; lock direct-edge and requires-dist claims exact; workflow trigger-gap description exact; coverage-guard claim exact; all referenced source/test/doc paths exist; `tests/contracts/` convention exists and the new test file does not collide; `.planning/plan-approved/<issue>.md` gate convention is real; `docs/plans/README.md` correctly absent; no other pinned C extension in the lock lacks a cp312 Windows wheel, so the "hiredis is the next and last blocker" inference survives the strongest available repo-side test; `workflow-automation` and `digital_model` entry points exist for the verification commands.

## Blockers

None. All findings are MINOR: the correctness-critical claims (dependency lines, lock edges, workflow contents, wheel availability, README drift, file existence) were affirmatively verified against the repository. Findings 1–5 should be folded into the next plan revision before the fresh review round it already requires; finding 6 should be closed by whichever reviewer has `gh` access.
