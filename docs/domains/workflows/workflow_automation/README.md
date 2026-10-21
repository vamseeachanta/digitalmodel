# Workflow automation CI

> Part of `digitalmodel.workflows`

## Summary

The workflow automation job installs the package with `.[dev,test]` and runs the
workflow automation suite with coverage. The `test` extra in `pyproject.toml`
declares `pytest`, `pytest-cov` and `pytest-benchmark`; installing only `.[dev]`
does not supply the plugins required by the coverage flags and benchmark fixture.
This repairs the dependency mismatch reported in issue #1958.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| A | 2026-10-19 | Document the corrected test installation and heartbeat so an old passing run is not treated as current evidence. |
| B | 2026-10-19 | Select the coverage threshold explicitly, align shared coverage settings and explain test-extra side effects so a passing heartbeat has a defined meaning. |
| C | 2026-10-21 | Separate test outcomes from coverage enforcement and retain both Python versions and diagnostic checks when an earlier step fails. |

## Execution and coverage

`.github/workflows/workflow-automation-tests.yml` retains the Python matrix and
coverage collection/upload. Matrix fail-fast is disabled so a failure on one
Python version does not cancel the other. Push and pull-request path filters include the
module, its tests, the workflow, `pyproject.toml`, `.coveragerc`, `uv.lock` and `pytest.ini`.
Dependency and collection changes therefore request fresh execution.

The test command explicitly selects `--cov-config=pyproject.toml` and uses
`--cov-fail-under=0` during collection, so its exit status reports test failures
independently of coverage. After coverage upload and the CLI/import diagnostics,
the final **Enforce coverage threshold** step runs
`coverage report --rcfile=pyproject.toml` against the same collected data and
enforces the repository's unchanged 80.0% threshold. Missing coverage data also
fails that step. No coverage threshold is removed.

The CLI, import and final coverage steps use `if: ${{ !cancelled() }}`. They run
after earlier failures, including an installation failure, unless the run is
cancelled. Earlier failed steps retain their failure conclusions; subsequent
errors can therefore reflect that earlier failure. Coverage upload retains its
existing matrix condition and requires the preceding test step to succeed.

The final coverage step is expected to fail while measured coverage remains below
80.0%, even when the tests and diagnostics pass. The overall job remains red;
inspect individual step conclusions for fresh installation, test, CLI and import
evidence rather than treating that overall conclusion as the heartbeat signal.

Each TOML-declared coverage setting agrees with `.coveragerc` and is protected
by regression contracts. INI-only settings and sections are outside that comparison.
Before this pull request, automatic discovery selected `.coveragerc` without a threshold;
a passing run therefore did not establish 80.0% coverage. The current
`.coveragerc` also declares 80.0%, so ad hoc local `pytest --cov` commands using
default discovery enforce that threshold too.

The daily schedule is `43 6 * * *` in UTC, independently of those path filters.
The time is an operational choice. `workflow_dispatch` permits an immediate
manual run. According to the [GitHub Actions workflow syntax](https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#onschedule)
(accessed 2026-10-19), schedules run the default branch. The [schedule event reference](https://docs.github.com/en/actions/reference/workflows-and-actions/events-that-trigger-workflows#schedule)
also documents delays and automatic disabling in inactive public repositories
(accessed 2026-10-19).
The heartbeat requests daily evidence, rather than guaranteeing a deadline.
Check the most recent run's timestamp, event and conclusion when assessing CI.

## Worked example: dependency selection

Inputs are the workflow's editable-install argument and the optional dependencies
in `pyproject.toml`. The method is to form the union of runtime dependencies and
the dependencies of each selected extra, then look for the plugins consumed by
the test command and suite. No engineering equations or acceptance limits apply.

| Install argument | Required plugins present in the declared set | Expected outcome |
| --- | --- | --- |
| `.[dev]` | None of `pytest`, `pytest-cov`, `pytest-benchmark` | Installation does not ensure the test runner, coverage flags or benchmark fixture work. |
| `.[dev,test]` | All of the required plugins | The declared installation supplies the test runner, coverage flags and benchmark fixture. |

These answers follow directly from the repository declarations, independently
of packages left in a developer environment. See the [uv package installation guide](https://docs.astral.sh/uv/pip/packages/)
for editable installs and extras (accessed 2026-10-19).

The `test` extra also installs `pytest-randomly`, which randomizes test order on
each run. The printed seed permits reproduction with `--randomly-seed=<seed>`.
It also installs `hullprod`, which bundles OpenCascade; this CAD dependency is
installed even though this suite does not exercise it.

From the repository root, reproduce the hosted job in an isolated environment:

```sh
uv venv --python 3.11
uv pip install --no-sources -e ".[dev,test]"
uv run --no-sync pytest tests/workflows/workflow_automation/ -v \
  --cov=digitalmodel.workflows.workflow_automation --cov-config=pyproject.toml \
  --cov-fail-under=0 --cov-report=xml --cov-report=term
uv run --no-sync workflow-automation list
uv run --no-sync python -c "from digitalmodel.workflows.workflow_automation import WorkflowOrchestrator, CompleteRiserAnalysisWorkflow"
uv run --no-sync coverage report --rcfile=pyproject.toml
```

The outputs are a pytest summary, coverage XML and terminal table, and a CLI
listing and an import check. A successful test command establishes passing tests;
the final coverage command fails unless the repository's coverage gate is met.
This example checks dependency
installation and workflow behavior; it does not execute engineering solvers.
The regression contracts in `test_ci_contract.py`
also check the daily schedule, manual trigger, dependency paths, preservation
of coverage upload, explicit coverage selection, independent matrix completion,
diagnostic guards, final coverage enforcement and agreement of TOML-declared settings.

## Limits

Local execution cannot establish that GitHub has accepted or executed a schedule,
or that an external coverage upload succeeded. Inspect the first hosted runs
after merging. The existing TOML coverage gate is now selected explicitly; dependency repair
alone does not establish that this suite meets it. No plugins or tests are
disabled here.
