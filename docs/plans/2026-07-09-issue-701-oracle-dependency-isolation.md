# Plan: digitalmodel #701 — remove unused cx-Oracle and restore Windows CI

> **Status:** draft
> **Complexity:** T2
> **Date:** 2026-07-09
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/701
> **Client:** N/A
> **Project:** N/A
> **Lane:** lane:claude
> **Execution mode:** single-lane
> **PR workflow:** multi-session pickup requested by the owner; a PR branch is the documented exception to main-only execution
> **Current review targets:** `scripts/review/results/issue-701-round-10/2026-07-09-plan-701-{claude,codex,gemini,disagreement}.md`
> **Review history:** parent-directory `round-2-*` through `round-4-*` files and `issue-701-round-{5,6,7,8,9}/` are superseded evidence only
> **Artifact timing:** current-round provider files are outputs, never inputs; validate/promote them only after fanout exits, so zero/missing files observed by a provider during its own run are not evidence

---

## Resource Intelligence Summary

Issue #701 is open with `cat:engineering`, `lane:claude`, and
`status:needs-plan`. The owner decided on 2026-07-09 to remove `cx-Oracle`
entirely because it is unused; no replacement or optional Oracle extra is
wanted. The issue body and pickup comment now record that decision.

### Existing code and metadata

- `pyproject.toml:10` requires Python 3.11+, but `README.md:90` still says
  Python 3.10+.
- `pyproject.toml:42` declares unconditional `cx-Oracle==6.3.1`.
- `pyproject.toml:55` declares unconditional `hiredis==2.2.3`, but no root
  source imports hiredis. It is an optional C accelerator for redis-py.
- Redis is reached transitively through `fastapi-limiter`, and cache
  configuration defaults `enable_redis=True`. The locked graph pairs redis
  7.2.0 with hiredis 2.2.3; redis 7.2 rejects that old accelerator, so
  `redis.utils.HIREDIS_AVAILABLE` is false. The pin is installed but inactive.
  Removing it preserves current correctness and performance behavior. A future
  compatible accelerator requires a separately justified optional
  dependency/benchmark issue.
- `src/digitalmodel/visualization/orcaflex_dashboard/backend/requirements.txt`
  separately pins `redis[hiredis]` and hiredis 2.2.3 for a Docker-installed
  dashboard sub-service. That file is not root project metadata or installed by
  the root workflow. Its pin and runtime remain unchanged and out of scope.
- `uv.lock` records both packages as direct dependencies of the editable
  `digitalmodel` package and resolves those exact old versions.
- A tracked-file search of `src/`, `tests/`, `.github/`, and `config/` finds no
  `cx_Oracle`, `cx-Oracle`, `oracledb`, or Oracle-database consumer. Ignored
  generated `src/digitalmodel.egg-info/` may contain stale metadata until the
  successful editable reinstall regenerates it; it is not source evidence.
- Historical `cx-Oracle` text under
  `docs/domains/platecapacity/StiffnerBuckling_Cal/Python_Environment/` belongs
  to an archived, internally old example environment. The migration document
  explicitly says that tree is not migrated. It is outside root packaging,
  runtime, and CI scope and remains unchanged.
- `docs/domains/articles/PY_Virtualization.md` also contains an archived
  `cx_oracle=6.3.1` environment listing; it has the same docs-only disposition.
- `.github/workflows/workflow-automation-tests.yml` currently covers only
  Ubuntu with Python 3.11/3.12. Pull-request paths omit the workflow file,
  `pyproject.toml`, and `uv.lock`; push paths omit the two metadata files.
- The revised workflow will have no `paths`/`paths-ignore` filters, so it cannot
  skip a push or pull request solely because of the changed paths. This costs a
  roughly 150-dependency, four-leg matrix run even for docs-only changes. The
  narrower alternative is to retain filters and add the workflow,
  `pyproject.toml`, and `uv.lock`; the owner-requested plan deliberately chooses
  unconditional trigger coverage. This is not complete self-disable protection:
  branch protection/required-check policy must prevent a PR from deleting or
  gutting the workflow itself.
- The workflow's install, pytest, CLI, and package-import commands are existing
  contracts and must not be weakened.
- The repository has no local `docs/plans/README.md`; the standalone plan is
  indexed in the governing `../workspace-hub/docs/plans/README.md` instead.

### Sources consulted

1. `digitalmodel#701` and PR #699 / squash commit `41289638`: governing issue,
   original Windows/Python 3.12 linker failure, and temporary Ubuntu-only CI.
2. `digitalmodel#700`: open main-branch CI baseline debt; supplies the explicit
   no-new-failures comparison boundary for the canonical regression suite.
3. `pyproject.toml` and `uv.lock`: prove the unconditional/direct dependency
   edges and current Python contract.
4. `.github/workflows/workflow-automation-tests.yml`: proves the current matrix,
   trigger gaps, and exact commands to preserve.
5. `README.md`: proves Python-version and installation-guidance drift.
6. `docs/domains/structural/plate_capacity_migration.md` plus the archived
   `StiffnerBuckling_Cal` requirements/batch files: classify historical Oracle
   text without falsely claiming repo-wide removal.
7. `src/digitalmodel/infrastructure/{core,persistence}/cache.py`, the focused
   cache tests, and official hiredis metadata: prove hiredis is an optional
   accelerator rather than an application import. Version 2.2.3 has Windows
   Python 3.11 wheels but no Python 3.12 wheel.
8. `../workspace-hub/docs/document-intelligence/README.md` and
   `data-intelligence-map.md`: route indexed-drive retrieval to the unified
   search used below; no dependency-specific durable page was identified.
9. `../workspace-hub/data/design-codes/code-registry.yaml` and
   `../workspace-hub/data/document-index/{standards-transfer-ledger.yaml,online-resource-registry.yaml,resource-intelligence-maturity.yaml}`:
   no applicable calculation standard or Oracle/hiredis dependency contract.
10. Drive-file search for `digitalmodel cx-Oracle hiredis Windows Python
   dependency` returned no relevant document, with coverage limits embedded
   below.

Engineering calculation standards are not applicable: this is packaging and
CI work with no formula, solver input, or engineering result change.

### Reproduction proofs

Synchronized Windows host `ACMA-HOU-RDS02`, 2026-07-09:

```text
$ git rev-parse HEAD
4ab92d9063e698147bc8e63bbdef139bf868bc32

$ uv sync --python 3.12 --locked --extra solvers
Using CPython 3.12.13
Resolved 464 packages
× Failed to build `cx-oracle==6.3.1`
error: Microsoft Visual C++ 14.0 or greater is required
hint: `cx-oracle` was included because `digitalmodel` depends on `cx-oracle`
exit=1
```

Independent Python 3.12 next-blocker audit (no project install, no cache):

```text
$ Get-Command cl.exe -ErrorAction SilentlyContinue
CL_EXE_PRESENT=False

$ uv run --no-project --isolated --no-cache --python 3.12 --with hiredis==2.2.3 python -c "import hiredis"
Failed to build hiredis==2.2.3: Microsoft Visual C++ 14.0 or greater is required
exit=1

$ uv run --no-project --isolated --no-cache --python 3.12 --with hiredis==3.4.0 python -c "import hiredis"
HIREDIS_340_IMPORT_OK
exit=0
```

Toolchain discovery on this host:

```text
$ Test-Path 'C:\Program Files (x86)\Microsoft Visual Studio\Installer\vswhere.exe'
VSWHERE_EXISTS=False
$ Test-Path 'C:\Program Files\Microsoft Visual Studio'
VS_ROOT_64_EXISTS=False
$ Test-Path 'C:\Program Files (x86)\Microsoft Visual Studio'
VS_ROOT_32_EXISTS=False
$ query uninstall registry for Visual Studio / Build Tools / Windows SDK products
VC_BUILD_TOOL_PRODUCT_COUNT=0
$ Get-Command cl.exe -ErrorAction SilentlyContinue
CL_ON_PATH=False
```

Redistributable runtimes are present but are not compiler toolchains. The
combined component/product/root/PATH evidence establishes this host as the
compiler-free execution environment; a future machine with ambiguous signals
must use a known compiler-free clean image instead.

All eight ecosystem repositories were fast-forwarded first. A fetched-ref
audit recorded:

```text
REMOTE_PYPROJECT_REFS_CHECKED=93
REFS_WITHOUT_CX_ORACLE_PIN=0
REFS_WITH_ORACLEDB=0
```

Repository drift is not the cause.

### Gaps

- No contract test prevents unused Oracle drivers from returning to root
  metadata or the lock.
- Hiredis is another unused unconditional root C extension. It blocks only the
  Python 3.12 Windows leg (2.2.3 has 3.11 Windows wheels) and should be removed
  rather than upgraded and frozen as a new contract.
- No contract test freezes the intended OS/Python matrix, trigger paths, and
  exact workflow commands.
- Windows workflow-automation CI coverage is absent. A compiler-free clean
  install can be proven on the audited host/clean image, but GitHub's hosted
  `windows-latest` runner includes build tools and cannot continuously enforce
  that generic property.

### Embedded verification evidence

Verified 2026-07-09T21:33:13Z unless a command records its own timestamp.

**Live GitHub state (connected GitHub app):**

```text
digitalmodel#701 — OPEN — Remove unused cx-Oracle and restore Windows workflow automation CI
labels=cat:engineering,lane:claude,status:needs-plan; comments=2 at evidence capture
digitalmodel#700 — OPEN — CI baseline: Quality Gates and broad domain shards fail on main
labels=priority:high,cat:ci,status:pending,lane:codex
digitalmodel#699 — MERGED — feat: add DNV RP F106 coating helpers
merge_commit=41289638e85572cb9397bd3c052418888ac8d492
```

**File existence:**

```text
EXISTS pyproject.toml
EXISTS uv.lock
EXISTS .github/workflows/workflow-automation-tests.yml
EXISTS README.md
EXISTS tests/contracts/
MISSING tests/contracts/test_dependency_isolation.py  # created by this issue
EXISTS tests/test_cache.py
EXISTS tests/infrastructure/core/test_cache.py
EXISTS tests/data_systems/test_cache_manager.py
EXISTS src/digitalmodel/visualization/orcaflex_dashboard/backend/requirements.txt
EXISTS docs/domains/articles/PY_Virtualization.md
```

**Line excerpts:**

```text
$ Select-String pyproject.toml 'requires-python|cx-Oracle|hiredis=='
10: requires-python = ">=3.11"
42: "cx-Oracle==6.3.1",
55: "hiredis==2.2.3",

$ Select-String README.md 'Requires Python 3.10'
90: Requires Python 3.10+.
```

**Active-consumer gap proofs:**

```text
$ git grep -n -i -E 'cx[_-]?oracle|oracledb' -- src tests .github config
<no matches>; exit=1

$ rg -n -i '(^|\s)(import|from)\s+hiredis\b|importlib[^\r\n]*hiredis' src/digitalmodel tests .github config
<no matches>; exit=1

$ rg -n -i 'hiredis' src/digitalmodel tests .github config
src/digitalmodel/visualization/orcaflex_dashboard/backend/requirements.txt:17:redis[hiredis]==5.0.1
src/digitalmodel/visualization/orcaflex_dashboard/backend/requirements.txt:18:hiredis==2.2.3
```

The last result proves the separate Docker sub-service pin exists; the first
two commands bound the no-active-root-consumer claim.

**Fetched-ref audit command/result:**

```powershell
$refs = git for-each-ref --format='%(refname:short)' refs/remotes/origin
$checked = 0
$withoutCx = 0
$withOracledb = 0
foreach ($ref in $refs) {
  $lines = git show "$ref`:pyproject.toml" 2>$null
  if ($LASTEXITCODE -eq 0) {
    $checked++
    $content = $lines -join "`n"
    if ($content -notmatch 'cx-Oracle==6\.3\.1') { $withoutCx++ }
    if ($content -match 'oracledb') { $withOracledb++ }
  }
}
REMOTE_PYPROJECT_REFS_CHECKED=93
REFS_WITHOUT_CX_ORACLE_PIN=0
REFS_WITH_ORACLEDB=0
```

**Drive-file search:**

```text
$ cd ../workspace-hub
$ uv run --isolated --no-project --python 3.12 --with pyyaml python \
    scripts/data/drive-index-search/search.py \
    "digitalmodel cx-Oracle hiredis Windows Python dependency" \
    --json --caller plan-resource-intel
generated_at=2026-07-09T21:32:56.900609+00:00
results=[]
indexes_queried=6; coverage_gaps=5
```

No identifying drive paths are persisted. Two index snapshots were stale and
five configured indexes were unreachable, so this is negative evidence with
documented coverage limits, not a claim of exhaustive drive coverage.

---

## Artifact Map

| Artifact | Path |
|---|---|
| Plan | `docs/plans/2026-07-09-issue-701-oracle-dependency-isolation.md` |
| Dependency/workflow contract tests | `tests/contracts/test_dependency_isolation.py` |
| Root dependency metadata | `pyproject.toml` |
| Resolved graph | `uv.lock` |
| Windows CI matrix | `.github/workflows/workflow-automation-tests.yml` |
| Installation guidance | `README.md` |
| Final plan reviews | `scripts/review/results/issue-701-round-10/2026-07-09-plan-701-{claude,codex,gemini,disagreement}.md` |
| Cross-machine handoff | `docs/session-handoffs/2026-07-09-digitalmodel-issue-701.md` |
| Central plan index | `../workspace-hub/docs/plans/README.md` |

---

## Deliverable

`digitalmodel` installs without an MSVC/VC toolchain on supported Windows Python
3.11/3.12 environments, contains neither unused root C-extension pin, and
again exercises workflow automation on Windows CI.

---

## Proposed Design / Pseudocode

### Dependency contract

```text
load pyproject.toml with tomllib
recursively enumerate dependency-bearing surfaces:
    build-system.requires
    project.dependencies
    every project.optional-dependencies list
    every dependency-groups list/include
normalize requirement names with packaging.Requirement
assert none contains cx-oracle, oracledb, or hiredis
assert project.optional-dependencies contains no oracle extra

load uv.lock with tomllib
assert no package named cx-oracle, oracledb, or hiredis exists
find editable digitalmodel package
assert its dependencies and metadata.requires-dist contain neither Oracle driver
assert its dependencies and metadata.requires-dist contain no hiredis edge
```

No application import is migrated because no active consumer exists. Archived
docs are excluded from the root-metadata assertion rather than rewritten.

### Workflow contract

```text
load YAML with yaml.load(..., Loader=yaml.BaseLoader)
assert matrix.os == [ubuntu-latest, windows-latest]
assert matrix.python-version == ['3.11', '3.12']
assert push and pull_request exist with no paths or paths-ignore filter
assert normalized Install dependencies command equals current command exactly
assert normalized Create virtual environment command equals current command exactly
assert normalized Run workflow automation tests command equals current command exactly
assert normalized Test CLI commands command equals current command exactly
assert normalized Verify package installation command equals current command exactly
assert a post-install step uses `uv run --no-sync python -c ...`
assert that project-environment command proves find_spec for cx_Oracle, oracledb, and hiredis is None
assert a workflow step runs tests/contracts/test_dependency_isolation.py
assert a workflow step runs `uv lock --check`
assert coverage upload remains Ubuntu/Python-3.11-only
```

`yaml.BaseLoader` avoids YAML 1.1 coercion of the `on` key. The implementation
adds the Windows matrix, removes both event path filters, and adds dependency
contract, lock-freshness, and base-isolation steps. Existing test, CLI, import,
venv-creation, and coverage behavior is preserved.

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `tests/contracts/test_dependency_isolation.py` | TDD guards for metadata, lock, and CI contracts |
| Modify | `pyproject.toml` | remove unused cx-Oracle and hiredis root pins |
| Modify | `uv.lock` | remove cx-oracle and hiredis packages/direct edges |
| Modify | `.github/workflows/workflow-automation-tests.yml` | restore Windows matrix/triggers; run dependency contract and base-isolation proofs |
| Modify | `README.md` | align Python/base/solver guidance and reconcile the Windows build-dependency/lxml-wheel caveat |
| Create | `scripts/review/results/issue-701-round-10/2026-07-09-plan-701-{claude,codex,gemini,disagreement}.md` | revision-isolated final evidence; directory must be absent/empty before review, and fanout completion, mtime-after-plan, non-empty, and no-`.err` validation precede promotion |
| Create | `docs/session-handoffs/2026-07-09-digitalmodel-issue-701.md` | ASCP pickup state for the owner-requested cross-machine PR workflow |
| Update | `../workspace-hub/docs/plans/README.md` | mandatory central index row for sibling digitalmodel plan |

No `src/digitalmodel/` code, historical example environment, solver algorithm,
or Deckhand repository file changes are in scope.

---

## TDD Test List

| Test | Current RED | Final GREEN |
|---|---|---|
| `test_root_metadata_has_no_oracle_driver` | cx-Oracle is unconditional | no cx-Oracle/oracledb root dependency or Oracle extra |
| `test_root_metadata_has_no_unused_hiredis_pin` | hiredis is unconditional | no root hiredis dependency |
| `test_lock_has_no_oracle_driver` | cx-oracle package/direct edge exists | neither Oracle driver occurs in package/editable metadata |
| `test_lock_has_no_unused_hiredis_package` | hiredis package/direct edge exists | no hiredis package or editable edge |
| `test_workflow_matrix_restores_windows` | Ubuntu only | Ubuntu/Windows × Python 3.11/3.12 |
| `test_workflow_has_unconditional_event_triggers` | self-selecting path filters exist | both events are unfiltered; branch protection owns anti-tampering |
| `test_workflow_commands_are_exact` | passes before change | install, pytest, CLI, and import commands remain exact |
| `test_workflow_proves_unused_dependency_isolation` | isolation step absent | `uv run --no-sync python` checks both Oracle modules and hiredis in `.venv` |
| `test_workflow_runs_dependency_contract` | contract is not a PR/push gate | triggered workflow explicitly runs the new contract file |
| `test_workflow_checks_lock_freshness` | no lock check in job | job explicitly runs `uv lock --check` |
| `test_coverage_upload_remains_single_job` | passes before change | existing Ubuntu/3.11 guard unchanged |

Tests are written first. Every table row classified as currently RED must be
captured failing against the current checkout before metadata/workflow changes.

---

## Implementation Steps

1. Create the owner-requested multi-session PR branch and add the contract tests.
   Capture RED with an isolated no-project pytest/PyYAML/packaging runner because
   the reproduced failed sync intentionally left `.venv` incomplete. Preflight
   the required sibling `../assetutilities/src`, then import it with the same
   isolated runner before pytest. A failure before test collection is a
   prerequisite failure, not RED evidence.
2. Remove `cx-Oracle==6.3.1` and `hiredis==2.2.3`; add no Oracle replacement,
   optional extra, or hiredis replacement.
3. Regenerate the lock with uv 0.11.25 (the audited host version). Accept only
   the two package/direct-edge removals. If that pinned version still produces
   unrelated churn, stop and amend the plan rather than normalize the churn.
4. Prove the host has no MSVC/VC toolchain using Visual Studio component/product,
   installation-root, and PATH checks; if any signal is ambiguous, use a known
   MSVC/VC-toolchain-free clean Windows image. Then run disposable Windows Python 3.11
   and 3.12 base+dev installs with `--no-cache` and a fresh empty
   `UV_CACHE_DIR`. Prove
   both Oracle modules and hiredis are absent. Run the workflow-automation
   pytest suite plus the existing CLI and package-import workflow commands in
   both environments.
5. Only after clean installs pass, restore the Windows matrix, remove both
   event path filters, and add explicit dependency-contract, lock-freshness,
   and base-isolation steps while preserving existing commands and venv
   creation exactly. The unfiltered workflow cannot skip a workflow-only,
   lock-only, metadata-only, or docs-only change solely through path filtering;
   repository branch protection remains responsible for anti-tampering.
6. Align README Python/base/solver guidance and reconcile the adjacent Windows
   build-dependency/lxml-wheel caveat with the verified wheel-based install.
7. Complete the locked solver sync first, then run focused contracts,
   workflow-automation and cache suites. Run the canonical
   `PYTHONPATH=src uv run python -m pytest` regression command (without
   `--no-sync`), `uv lock --check`, and licensed-host imports.
8. Cross-review the implementation against this approved plan, resolve all
   MAJOR findings, push the digitalmodel branch, and open the requested PR
   referencing #701.

### Multi-session coordination contract

- Claim key: `issue/digitalmodel/701`; session IDs use
  `<provider>-<host>-<4hex>`.
- Before any write-capable pickup, use `../llm-wiki/scripts/coordination/claim.py`
  with authenticated `gh`, or reproduce its exact parser/post behavior through
  a connected GitHub app. The canonical tokens are
  `🔒 ASCP CLAIM key=issue/digitalmodel/701 session=<id> until=<ISO8601>` and
  `🔓 ASCP RELEASE key=issue/digitalmodel/701 session=<id>`. If neither `gh`
  nor the connected app can fetch/post and verify comments, stop; a local
  filesystem lock is insufficient for cross-machine work. If another
  unexpired claim lacks a later matching ASCP release, do not start. Otherwise
  acquire with a 30-minute TTL and renew at least every 20 minutes while active.
- One claimed session owns the PR branch plus shared `pyproject.toml`,
  `uv.lock`, workflow, tests, README, and plan/approval surfaces. Other sessions
  are read-only unless the owner explicitly reassigns paths.
- Before any stop, write/update
  `docs/session-handoffs/2026-07-09-digitalmodel-issue-701.md` from
  `../llm-wiki/coordination/HANDOFF_TEMPLATE.md` (goal, state, next steps, key
  files, gotchas, resume commands, open claim), post the same branch/commit/test
  checkpoint to #701, then post
  `🔓 ASCP RELEASE key=issue/digitalmodel/701 session=<id>`.
- Push and PR creation remain serialized to the claimed orchestrator. GitHub
  reads/writes use the connected app on hosts without `gh`.

---

## Verification Commands

Pre-change RED command, independent of the broken project environment:

```powershell
if (-not (Test-Path ../assetutilities/src)) { throw 'required sibling assetutilities checkout missing' }
uv run --no-project --isolated --no-cache --python 3.12 `
  python -c "import pathlib,sys; sys.path.insert(0, str(pathlib.Path('../assetutilities/src').resolve())); import assetutilities; print(assetutilities.__file__)"
uv run --no-project --isolated --no-cache --python 3.12 `
  --with pytest --with pyyaml --with packaging `
  python -m pytest tests/contracts/test_dependency_isolation.py -v
```

Post-change commands run only after `uv sync --python 3.12 --locked --extra
solvers` succeeds:

```powershell
uv run --no-sync python -m pytest tests/contracts/test_dependency_isolation.py -v
uv lock --check
uv run --no-sync pytest tests/workflows/workflow_automation/ -v --cov=digitalmodel.workflows.workflow_automation --cov-report=xml --cov-report=term
uv run --no-sync workflow-automation list
uv run --no-sync python -c "from digitalmodel.workflows.workflow_automation import WorkflowOrchestrator, CompleteRiserAnalysisWorkflow; print('Workflow automation module installed')"
uv run --no-sync python -m pytest tests/test_cache.py tests/infrastructure/core/test_cache.py tests/data_systems/test_cache_manager.py -v
$env:PYTHONPATH='src'; uv run python -m pytest
```

The canonical full-suite baseline/comparison is reproducible:

1. Exact base SHA: `4ab92d9063e698147bc8e63bbdef139bf868bc32`.
2. Create a detached temporary worktree under a parent that has no sibling
   `assetutilities/`. Apply only the two metadata removals and regenerated lock
   needed to make the base install runnable; do not add tests/workflow/docs.
3. On Windows/Python 3.12.13 with uv 0.11.25, run the locked sync, then use
   PowerShell: `$env:PYTHONPATH='src'; uv run python -m pytest -p no:randomly
   --json-report --json-report-file=<baseline.json>`. Disabling pytest-randomly
   gives the comparison a stable collected order when the final tree adds tests.
4. In a second detached worktree of the final branch under the same parent,
   use the same interpreter, environment, and command for `<final.json>`.
5. Normalize failures by collection phase, nodeid, exception type, and final
   exception message (strip temp-root prefixes only). The final failure set
   must be a subset of the baseline set; new tests may only add PASS rows. Any
   new/unmapped signature, touched-scope failure, collection difference, or
   missing report blocks closeout. Store counts, report hashes, and the delta
   table in the #701/PR validation comment. Issue #700 retains baseline debt.
6. Separately run the repository's canonical full-suite command with its normal
   pytest plugins enabled. Report that result, but adjudicate the baseline/final
   no-new-failure subset only from the `-p no:randomly` paired runs. Any failure
   in touched scope or any repeatable final-only failure still blocks closeout.

Disposable Python 3.11 and 3.12 environments run from a detached temporary
worktree whose parent has no `assetutilities/` sibling. After install, assert
`assetutilities.__file__` is inside the temporary environment. Each runs:

```powershell
$env:UV_CACHE_DIR=<fresh-empty-cache>
$vswhere='C:\Program Files (x86)\Microsoft Visual Studio\Installer\vswhere.exe'
if (Test-Path $vswhere) {
  $vc = & $vswhere -all -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath
  if ($vc) { throw "VC Tools installed: $vc" }
}
if (Test-Path 'C:\Program Files\Microsoft Visual Studio') { throw 'Visual Studio root present; inspect or use clean image' }
if (Test-Path 'C:\Program Files (x86)\Microsoft Visual Studio') { throw 'Visual Studio root present; inspect or use clean image' }
$uninstall='HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\*','HKLM:\SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\*'
$products=Get-ItemProperty $uninstall -ErrorAction SilentlyContinue | Where-Object { $_.DisplayName -match 'Visual Studio|Build Tools|Windows Software Development Kit|Windows SDK' -and $_.DisplayName -notmatch 'Redistributable|Runtime' }
if ($products -or (Get-Command cl.exe -ErrorAction SilentlyContinue)) { throw 'MSVC/VC toolchain signal present' }
# If any signal is ambiguous, use a fresh Windows Sandbox/VM with no Visual Studio or Build Tools installed.
uv venv --python <version> <temp-venv>
uv pip install --no-cache --python <temp-venv> --no-sources -e ".[dev]"
<temp-venv>\Scripts\python -c "import importlib.util; assert importlib.util.find_spec('cx_Oracle') is None; assert importlib.util.find_spec('oracledb') is None; assert importlib.util.find_spec('hiredis') is None"
<temp-venv>\Scripts\python -c "import assetutilities, pathlib; assert pathlib.Path(assetutilities.__file__).is_relative_to(pathlib.Path(r'<temp-venv>'))"
<temp-venv>\Scripts\python -m pytest tests/workflows/workflow_automation/ -v --cov=digitalmodel.workflows.workflow_automation --cov-report=xml --cov-report=term
<temp-venv>\Scripts\workflow-automation list
<temp-venv>\Scripts\python -c "from digitalmodel.workflows.workflow_automation import WorkflowOrchestrator, CompleteRiserAnalysisWorkflow"
```

Licensed-host consumer check, without starting Deckhand:

```powershell
uv sync --python 3.12 --locked --extra solvers
uv run --no-sync python -m digitalmodel --help
uv run --no-sync python -c "import OrcFxAPI; print(OrcFxAPI.DLLVersion())"
```

The Deckhand verifier/report remains owned by deckhand#529 and is not a closure
gate for this packaging PR. Its task must remain Disabled and never run.

---

## Acceptance Criteria

- [ ] TDD RED evidence precedes dependency/workflow implementation.
- [ ] Root `pyproject.toml` and `uv.lock` contain no cx-Oracle package or edge.
- [ ] Root metadata, optional extras, and lock contain no oracledb dependency.
- [ ] Root metadata and lock contain no hiredis package or direct edge.
- [ ] uv 0.11.25 lock churn is limited to removal of cx-Oracle and hiredis; any
  unavoidable unrelated churn stops the work for a plan amendment.
- [ ] Visual Studio component/product/root/PATH evidence (or a known clean
  image) establishes an MSVC/VC-toolchain-free environment; disposable Windows Python
  3.11 and 3.12 base+dev installs then succeed without a reusable cache and
  contain no Oracle driver or hiredis.
- [ ] Workflow matrix covers Ubuntu/Windows × Python 3.11/3.12.
- [ ] Push/PR events have no path filters, so changes cannot be skipped solely by
  path selection; branch protection/required checks own workflow anti-tampering.
- [ ] Existing install, pytest, CLI, import, and coverage-job contracts remain
  exact; focused tests pass.
- [ ] The dependency contract runs in the triggered workflow for workflow-only,
  lock-only, and metadata changes.
- [ ] The job runs `uv lock --check`; the contract scans build, project,
  optional, and recursively included dependency-group surfaces.
- [ ] Fixed-SHA/OS/Python/uv and deterministic-order JSON baseline/final reports satisfy the
  normalized subset rule; every new/unmapped delta is resolved.
- [ ] README matches Python 3.11+ and base/solver install behavior.
- [ ] Locked Python 3.12 solver sync, digitalmodel help, and OrcFxAPI import pass
  on the licensed host without starting Deckhand.
- [ ] Final implementation reviews contain no unresolved MAJOR finding.
- [ ] PR references #701 and reports exact validation results.

---

## Adversarial Review Summary

Earlier rounds reviewed broader replacement/bump designs and are superseded.
Round 9 returned Claude MINOR; Gemini was unavailable and Codex was intentionally
dispatched separately because the harness's inline Windows argument exceeds the
OS limit. This revision states that compiler-free installation is a clean-host
proof rather than a property hosted Windows CI can continuously enforce, and it
defines exactly which review artifacts count as usable. A fresh two-provider
round is required.

| Provider | Current verdict | Notes |
|---|---|---|
| Claude | MINOR (round 9, superseded) | compiler-free CI enforcement boundary and concrete usable-review definition |
| Codex | PENDING (round 10) | run directly from the repository with a concise local-plan path prompt to avoid the fanout argv limit |
| Gemini | UNAVAILABLE unless non-interactive auth is configured | availability is not approval |

**Overall:** PENDING; implementation remains blocked.

---

## Risks and Open Questions

- **Further old pins:** an MSVC/VC-toolchain-free, cache-clean disposable install is authoritative. If another
  deterministic Windows blocker appears, stop and amend #701 rather than hide
  it or weaken CI.
- **Lock churn:** regenerate with uv 0.11.25. If it updates more than the two
  intended removals/direct edges, stop and amend the plan rather than accepting
  or hiding unrelated churn.
- **CI cost and enforcement boundary:** unfiltered push/PR triggers run the full
  four-leg matrix even for docs-only changes. They prevent path-only skips, not
  workflow tampering; required-check/branch-protection policy is the independent
  enforcement boundary.
- **Compiler-free proof boundary:** hosted `windows-latest` includes MSVC/Build
  Tools, so restored CI cannot prove that every future dependency has a Windows
  wheel. The clean-host/clean-image install proves this revision, while the
  dependency contract durably denies the three known unused C-extension names.
  A future generic wheels-only policy requires a separate scoped issue.
- **Archived docs:** historical `StiffnerBuckling_Cal` pins remain by design and
  must not be mistaken for active root dependencies.
- **No Oracle compatibility promise:** removing the unused driver deliberately
  removes root-level Oracle support. A future real consumer requires a new
  issue, explicit API choice, tests, and credentials/network boundaries.
- **Licensed host:** OrcaFlex 11.6c and a seat were previously verified, but the
  Deckhand standby remains disabled and outside PR activation scope.

---

## Complexity

**T2** — the implementation is mechanically small but couples root dependency
metadata, the resolved lock, CI matrix/triggers, contract tests, cache/full
regression coverage, installation docs, and a cross-machine PR handoff.

---

## Approval Gate

After two usable no-MAJOR plan reviews, validate each final artifact is
non-empty, newer than this reviewed plan revision, and has no retained `.err`
sibling. A usable review must have a Verdict of `APPROVE`, `MINOR`, or `MAJOR`,
a non-empty Retrieval section, and a Blockers section; an `UNAVAILABLE` stub
never counts. The final Codex review is invoked directly from the repository
with a concise local-plan path prompt, avoiding the fanout's Windows inline-argv
limit. If fewer than two usable providers return, pause without promotion.
Commit/push the digitalmodel plan evidence. Separately commit/push the
single central-index row from clean `workspace-hub/main`; do not mix sibling
history into the digitalmodel PR. Then post the evidence to #701, move the issue
to `status:plan-review`, and stop. Implementation may
start only after the user approves this reviewed revision, the issue has
`status:plan-approved`, and `.planning/plan-approved/701.md` records that
approval. The implementing agent must not self-approve.
