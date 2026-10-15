# Repository structure contract

`config/repo_structure.yml` defines the path classification checked by
`scripts/maintenance/verify_repo_structure.py`. The checker reads tracked files
and unignored additions; it does not move, delete or rewrite files.

Run from the repository root:

```bash
python scripts/maintenance/verify_repo_structure.py --repo . --config config/repo_structure.yml
```

## Durable content

Source belongs under `src/digitalmodel/`, tests under `tests/`, maintenance
utilities under `scripts/maintenance/`, and structure rules under `config/` and
`docs/standards/`. The root file and directory allowlists enumerate the current
control surfaces. An additional root path requires explicit classification in
the contract. Root Python utilities are rejected unless individually listed;
`conftest.py` and `vulture_whitelist.py` are existing control files.

The nine existing notebooks, including the GIS notebook, are retained by exact
path. This records the current inventory without moving any notebook. Further
notebooks need an explicit policy decision. Tests nested inside `src/` are rejected.

## Runtime and generated content

Outputs, caches, build directories, distributions, logs, coverage files and
quality-gate reports are runtime artifacts. Ignore rules alone do not authorize
tracking: any candidate path matching a generated or CI-artifact pattern fails
unless a validated durable exception covers it. CI upload destinations are
runtime destinations, not durable source.

An exception contains `pattern`, `maintainer`, `reason`, `category`, `reviewed_on`,
and `follow_up_issue_or_permanent_justification`. All fields are substantive;
placeholder values fail even when no current file matches the exception.
`reviewed_on` is a valid ISO calendar date.

A `temporary_durable_generated_evidence` exception requires a positive issue
reference such as `#597` or a full issue URL. A
`permanent_durable_generated_evidence` exception requires a justification
beginning with `permanent: `, at least 40 characters including that prefix,
and no placeholder wording. Metadata validation does not establish that an
issue is open or that retention has been approved; those remain review checks.

## Current inventory and remaining enforcement

The inventory at `624c72096bab8acd31e889b46b39bb8f87580e99` contains no tracked
`outputs/` evidence and none of the three former root utilities. Their removal
is already present in `9737ccbb`. There is no live generated-evidence exception
to grandfather. `durable_exceptions` therefore starts empty; no additional
output path is authorized. The separate evidence-retention decision stays
with #597.

Local hook and mandatory CI wiring remain open under #2012. The checker and
its regression tests are usable directly. They do not claim that enforcement
has been installed or that branch protection has been verified.
