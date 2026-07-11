## Verdict

MAJOR

## Retrieval

- Read `docs/plans/2026-07-09-issue-701-oracle-dependency-isolation.md` in full.
- Read `AGENTS.md` and `../workspace-hub/AGENTS.md`; compared the canonical test command.
- Read `../workspace-hub/docs/plans/_template-issue-plan.md` evidence requirements and `../workspace-hub/.claude/skills/coordination/issue-planning-mode/SKILL.md`.
- Retrieved GitHub issue #701, both comments, and PR #699 metadata/workflow diff through the GitHub connector.
- Read `pyproject.toml` dependency, optional-dependency, package-discovery, pytest, and script sections.
- Grep’d `uv.lock` for `digitalmodel`, `cx-oracle`, `hiredis`, `redis`, and `fastapi-limiter` packages and edges.
- Read `.github/workflows/workflow-automation-tests.yml` in full.
- Read `README.md` installation guidance and Python-version statement.
- Grep’d `src/`, `tests/`, `.github/`, and `config/` for Oracle, hiredis, and Redis consumers.
- Read `src/digitalmodel/infrastructure/{core,persistence}/cache.py` lines 145–215.
- Read the relevant portions of `tests/test_cache.py`, `tests/infrastructure/core/test_cache.py`, and `tests/data_systems/test_cache_manager.py`.
- Read `src/digitalmodel/visualization/orcaflex_dashboard/backend/requirements.txt`, its `Dockerfile`, and `backend/app/core/cache.py`.
- Read `docs/domains/structural/plate_capacity_migration.md`.
- Grep’d the round-3 Claude and Codex review artifacts for prior findings.
- Checked `uv 0.11.25` help for `uv venv`, `uv pip install --python`, `--no-cache`, and `--no-sources`.
- Ran the proposed focused cache command; `uv run --no-sync ...pytest` failed with `.venv\Scripts\python.exe: No module named pytest`.
- Checked HEAD `4ab92d9063e698147bc8e63bbdef139bf868bc32`, worktree status, missing `tests/contracts/test_dependency_isolation.py`, and missing local `docs/plans/README.md`.

## Findings

1. **MAJOR — The TDD and regression commands cannot run from the stated baseline.** Plan §Implementation Steps 1 and §Verification Commands require RED evidence and then invoke every test through `uv run --no-sync`. The current `.venv` was left incomplete by the reproduced failed sync, and the proposed focused command exits with `No module named pytest`. The plan also calls `$env:PYTHONPATH='src'; uv run --no-sync python -m pytest` canonical, but `AGENTS.md` defines `PYTHONPATH=src uv run python -m pytest` without `--no-sync`. An isolated no-project test runner is needed for pre-fix RED evidence, followed by an explicit post-change locked sync before any `--no-sync` verification.

2. **MAJOR — `Get-Command cl.exe` does not prove that no C++ compiler is installed.** Plan §Implementation Step 4 and §Verification Commands treat `Get-Command cl.exe` returning nothing as satisfying Acceptance Criterion “without a C++ compiler.” That only proves `cl.exe` is absent from the current `PATH`; Visual Studio Build Tools can be installed and discoverable to build backends without that shell command resolving. Use `vswhere`/installed-component checks or a known compiler-free clean image.

3. **MINOR — The hiredis consumer inventory is incomplete.** Plan §Resource Intelligence calls hiredis unused, and §Proposed Design says “no active consumer exists,” but `src/digitalmodel/visualization/orcaflex_dashboard/backend/requirements.txt:17-18` explicitly installs `redis[hiredis]` and `hiredis==2.2.3`; its `Dockerfile:25-28` and `:49-52` install that file. Absence of an application-level `import hiredis` is not evidence of non-use because redis-py loads the accelerator indirectly. The plan must explicitly classify this separate dashboard environment and explain why its retained pin and behavior are outside root-package scope.

4. **MAJOR — The plan does not satisfy the mandatory embedded-evidence contract.** `../workspace-hub/docs/plans/_template-issue-plan.md` requires timestamped issue status, file existence results, line excerpts, and exact command output for every “no existing consumer” claim. Plan §Sources consulted supplies narrative assertions instead: the drive search has no command/output, the 93-ref audit has no generating command or ref list, and the Oracle/hiredis searches have no recorded commands. No attested-evidence block was supplied with this review. These omissions prevent the approval gate from independently reproducing the plan’s load-bearing claims.

5. **MINOR — The workflow contract omits hiredis from its installed-environment assertion.** Plan §Workflow contract and `test_workflow_proves_base_oracle_isolation` check only `cx_Oracle` and `oracledb`, although §Implementation Step 4 and Acceptance Criteria require hiredis to be absent too. The workflow uses unlocked `uv pip install`, so the lockfile assertion is not a substitute for checking the actual matrix environment. The post-install step and its contract test should assert all three modules are absent.

## Blockers

- Finding 1 — provide executable RED/GREEN environment setup and run the actual canonical regression command.
- Finding 2 — replace the PATH-only compiler check with a valid compiler-free-host proof.
- Finding 4 — embed the mandatory reproducible evidence before the plan advances to approval.
