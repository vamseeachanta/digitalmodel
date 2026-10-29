# Example test CI policy

The demonstrations under `examples/` are explicitly non-gating in the domain
matrix. Issue #1992 offers this as option 2: record the exclusion instead of
leaving CI ownership undecided. The tests remain available to explicit and
recursive pytest runs. A green domain matrix makes no claim about these tests.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-19 | Record the non-gating policy and repair prerequisites for the example suite. |
| Validation recipe | 2026-10-25 | Add isolated policy validation with its required dependency and clarify the limits of local results. |

## Scope and evidence

`scripts/ci/detect_touched_domains.py` lists `examples/` in `NO_DOMAIN_PATHS`.
This covers the demo tests, demo implementation and input files, workflow
examples, prototypes, and future example paths. It does not add an exclusion to
pytest collection or change `pytest.ini` defaults. A bare pytest invocation
continues to target `tests/`; use an explicit example path to inspect examples.

The initial report in #1992 counted 315 tests and five collection errors. A
baseline run on 2026-10-19 collected 393 tests, with 372 passing and 21 failing.
These are measured populations from different revisions, not fixed acceptance
limits. The later run supplied the demo and source import paths and the runtime
dependencies explicitly:

```sh
PYTHONPATH=examples/demos/gtm:src uv run --no-project \
  --with pytest --with pytest-asyncio --with pyyaml --with numpy \
  --with scipy --with plotly --with pandas --with jsonschema \
  --with assetutilities --with pydantic \
  python -m pytest examples/demos/gtm/tests -q -rA -p no:cacheprovider \
  --basetemp="$TMPDIR/example-tests"
```

The remaining failures have these causes:

- Intake and delivery tests require a schema and canonical input library from a
  sibling repository. They do not supply those resources in an isolated checkout.
- Wall-thickness golden comparisons disagree with current collapse and combined
  loading results. Repair requires checking the engineering basis before changing
  numerical expected values.
- Cached-report subprocess tests inherit a relative source import path, then
  change their working directory and lose access to the source package.

The suite also rewrites tracked reports and result files during execution. The
command above documents the measurement; it is not a hermetic test recipe. Run
it in a disposable checkout, with temporary output outside the repository, and
retain the evidence before restoring generated outputs. No solver is needed for
the measured demo suite.

## Policy validation

The routing and collection guards can be checked without executing the
demonstrations. Set `TMPDIR` to an existing temporary directory outside the
repository, then run:

```sh
uv run --no-project --with pytest --with pytest-asyncio \
  --with pyyaml --with assetutilities \
  python -m pytest tests/scripts/test_example_ci_policy.py \
  tests/scripts/test_detect_touched_domains.py \
  tests/contracts/test_collection_visibility.py \
  -q -rA -p no:cacheprovider --basetemp="$TMPDIR/example-policy-tests"
```

`assetutilities` is required by `tests/contracts/conftest.py`, including when
only the collection-visibility contract is selected. `--no-project` requires
that dependency to be supplied explicitly. The command passed 93 tests,
including 12 policy cases, on 2026-10-25. These counts describe the targeted
suite; the historical demonstration counts above describe a different suite.

A targeted pass establishes the asserted routing and collection properties.
Completed required hosted checks remain necessary before merging. Both
`Run Quality Gates` and `Domain test aggregate` reject cancelled prerequisites
under the existing workflow definitions. Such an aggregate failure does not
establish a failing application test when the prerequisite records no test
steps. The prerequisite results and annotations shall be inspected before a
test regression or a passing hosted gate is reported.

## Worked routing example

Run this from the repository root. The inputs are changed paths; the method is
`NO_DOMAIN_PATHS` filtering followed by the union of matching domain roots,
implemented by `touched_domains` in `scripts/ci/detect_touched_domains.py`.

```sh
python - <<'PY'
from pathlib import Path
from scripts.ci.detect_touched_domains import parse_domains, touched_domains

domains = parse_domains(Path("tests/DOMAINS.md"))
example = "examples/demos/gtm/tests/test_demo_01_cli.py"
guard = "tests/scripts/test_example_ci_policy.py"
for changed in ([example], [example, guard], [example, "pytest.ini"]):
    selected = touched_domains(changed, domains)
    print([domain.name for domain in selected])
PY
```

The first answer is `[]`: examples intentionally dispatch no shard. The second
is `['workflows']`: an example edit does not suppress another path's domain.
The third contains every domain: an example edit does not suppress a full matrix
trigger. These answers follow directly from empty-set union with the other
path's domain set; no engineering equation or acceptance coefficient is involved.

`tests/scripts/test_example_ci_policy.py` checks these cases, all tracked example
filenames matching the live `python_files` patterns, a future example path, and
collection visibility. Its own path belongs to `workflows` and is already covered
by that domain's recursive test command. Editing this policy also selects
`workflows`, so the routing and visibility guards run.

## Conditions for a gating domain

A later change can select option 1 of #1992 after it:

- Supplies the schema and canonical inputs within the repository or through an
  explicit input contract tested without a sibling checkout.
- Resolves the wall-thickness golden differences against an independent,
  documented engineering basis.
- Makes subprocess imports and report generation hermetic, with output under a
  temporary directory and no tracked-file changes.
- Extends the domain parser to accept `examples/` roots, registers a domain, and
  adds its command to both touched and full-matrix execution.
- Removes the explicit non-gating entry and replaces its guards with assertions
  that every intended example test is owned and actually executed.

## Sources

- Issue #1992: the initial census, later failure triage, and the two resolution
  options. Related collection fixes are described in #1977, #1983 and #1923.
- `scripts/ci/detect_touched_domains.py`: `is_no_domain_path`,
  `is_full_matrix_trigger`, `path_matches_root` and `touched_domains` define the
  dispatch method used above.
- `pytest.ini` and repository-root `conftest.py`: collection defaults and
  root-anchored exclusions.
- The unmodified example tests and the measured baseline run provide the current
  test counts and failure categories; these values are observations, not design
  criteria.
