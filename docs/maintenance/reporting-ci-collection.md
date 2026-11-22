# Reporting test collection

The reporting tests belong to `infrastructure-other`. Registration includes
the domain table, gate root metadata and the executable pytest command.
The independent CI harness also checks actual reporting directory collection,
including the acceptance modules named in issue #2450.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-22 | Document the collection regression and omission controls so metadata membership is distinguished from pytest collection. |

## Inputs and method

- [Domain table](../../tests/DOMAINS.md): assigns `tests/reporting/` to
  `infrastructure-other` for change detection.
- [Gate configuration](../../.claude/quality-gates.yaml): supplies both
  `test_roots` and the executable command for `tests-infrastructure-other`.
- [Hosted workflow](../../.github/workflows/quality-gates-by-domain.yml):
  runs the configured domain command and the independent CI harness.
- [Collection regression](../../tests/scripts/test_reporting_ci_registration.py):
  checks registration, source and fixture routing, harness inclusion and actual
  directory collection with the current interpreter and repository configuration.

Issue #2450 defines the required modules as `test_calc_report.py` and
`test_engine_golden.py`. The regression runs pytest in a child process with
`--collect-only`, requires successful collection and reads the module part of
each collected node ID. Both required modules must occur. It does not pin a
test count, since adding a reporting test must not require updating a count.
No engineering equations, coefficients or acceptance limits are introduced.

The omission controls repeat collection while explicitly ignoring either
required module. Pytest can still succeed because other modules remain.
The expected missing set is exactly the ignored module. These controls exercise
the distinction between a successful pytest exit and complete collection of the
required modules. They do not alter the hosted domain command or its exclusions.

## Worked example

Use an environment containing the repository's runtime and test dependencies.
Run from the repository root, with temporary output directed outside the checkout:

```bash
python -m pytest tests/reporting/ --collect-only --verbosity=-1 \
  -p no:cacheprovider --basetemp="$TMPDIR/reporting-collection"
```

Input: the configured reporting directory and the current repository pytest
configuration. Method: import matching test modules and enumerate node IDs
without executing test bodies. Expected answer: successful collection, with IDs
beginning with both of these prefixes:

```text
tests/reporting/test_calc_report.py::
tests/reporting/test_engine_golden.py::
```

Repeat with `--ignore=tests/reporting/test_calc_report.py`. The calculation-report
prefix must be absent and the golden-engine prefix must remain, even if the exit
code is successful. Ignoring `test_engine_golden.py` gives the reverse result.
The expected answers follow directly from the explicit exclusion and the module
names required by #2450, independently of gate metadata.

Run the regression and then the full reporting suite to obtain collection and
execution evidence:

```bash
python -m pytest tests/scripts/test_reporting_ci_registration.py \
  -q -rA -p no:cacheprovider --basetemp="$TMPDIR/reporting-registration"
python -m pytest tests/reporting/ \
  -q -rA -p no:cacheprovider --basetemp="$TMPDIR/reporting-execution"
```

## Evidence and limits

Collection imports modules; it does not execute their test functions or prove
their assertions pass. Missing dependencies and other collection errors fail the
regression instead of being treated as evidence of collection. Local reporting
results must be supplemented with hosted logs showing the required modules and
an explicit disposition of other failures, as required by #2450.

The child process collects the reporting directory rather than executing the
entire infrastructure domain command. The static registration checks separately
bind that directory to the command consumed by the hosted workflow. This is a
bounded reporting regression; repository-wide reconciliation of metadata,
configured arguments and intentional exclusions remains in #2083.
