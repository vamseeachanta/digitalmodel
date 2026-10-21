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
| C | 2026-10-21 | Separate test and coverage outcomes and retain smoke checks and both matrix legs after failures. |

## Execution and coverage

`.github/workflows/workflow-automation-tests.yml` retains the Python matrix and
coverage collection/upload. Push and pull-request path filters include the
module, its tests, the workflow, `pyproject.toml`, `.coveragerc`, `uv.lock` and `pytest.ini`.
Dependency and collection changes therefore request fresh execution.

The test command selects `--cov-config=pyproject.toml` and overrides the
threshold only during collection with `--cov-fail-under=0`. Its conclusion
therefore reports test failures independently of the coverage shortfall. A final
**Enforce coverage threshold** step runs `python -m coverage report
--rcfile=pyproject.toml`, enforcing the repository's existing 80.0% threshold.
The job is expected to remain red until coverage meets that criterion; inspect
individual step conclusions to distinguish coverage, test, install and smoke
check failures. At the reviewed revision, hosted Python 3.11 passed 131 tests
with 50.57% coverage; Python 3.12 was cancelled. Those results precede this fix.

Matrix `fail-fast: false` preserves evidence from both Python versions. CLI and
import checks, and the final coverage check, run after earlier failures unless
the workflow is cancelled. Coverage upload precedes the threshold check; a
coverage shortfall alone therefore no longer suppresses upload or smoke checks.
An earlier test failure can still suppress upload under its default condition.

TOML settings in the run, report, HTML, XML and JSON sections are also checked
against `.coveragerc`. INI-only settings (`context`, `sort`, `ignore_errors`,
`[lcov]` and `[paths]`) are outside that one-way contract. `.coveragerc` also
sets 80.0%, so ad hoc `pytest --cov` commands using default discovery enforce
that threshold unless explicitly overridden. Previously it had no threshold.

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
  --cov=digitalmodel.workflows.workflow_automation --cov-config=pyproject.toml --cov-fail-under=0 \
  --cov-report=xml --cov-report=term
uv run --no-sync workflow-automation list
uv run --no-sync python -m coverage report --rcfile=pyproject.toml
```

The outputs are a pytest summary, coverage XML and terminal table, and a CLI
listing. The final coverage command requires meeting the repository's
coverage gate, even if every test passes. This example checks dependency
installation and workflow behavior; it does not execute engineering solvers.
The regression contracts in `test_ci_contract.py`
also check the daily schedule, manual trigger, dependency paths, preservation
of coverage upload, explicit coverage selection and agreement of shared settings.

## Limits

Local execution cannot establish that GitHub has accepted or executed a schedule,
or that an external coverage upload succeeded. Inspect the first hosted runs
after merging. The existing TOML coverage gate is now selected explicitly; dependency repair
alone does not establish that this suite meets it. The linked startup-cost issue
is separate from this dependency and heartbeat repair; no plugins or tests are
disabled here.
