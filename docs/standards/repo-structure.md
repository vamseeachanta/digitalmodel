# Repository structure contract

`config/repo_structure.yml` defines the path classification checked by
`scripts/maintenance/verify_repo_structure.py`. The checker reads tracked files
and unignored additions; it does not move, delete or rewrite files.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 1 | 2026-10-20 | Record the existing contract and reject ambiguous YAML and placeholder metadata so invalid rules cannot authorize paths. Add a worked classification example and docs link enforcement. |

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
Missing-value wording such as `none`, `N/A`, `not applicable`, `pending`,
`unknown` and `unassigned`, and punctuation-only values, are placeholders.

A `temporary_durable_generated_evidence` exception requires a positive issue
reference such as `#597` or a full issue URL. A
`permanent_durable_generated_evidence` exception requires a justification
beginning with `permanent: `, at least 40 characters including that prefix,
excluding surrounding whitespace, and no placeholder wording. The text after
the prefix must also be substantive. Metadata validation does not establish that an
issue is open or that retention has been approved; those remain review checks.

Contract YAML rejects duplicate keys at every mapping level, including merged
keys, rather than allowing a later value to replace an earlier rule. Schema
version and required-field names are type-checked before path evaluation.
Paths and patterns use normalized relative POSIX form; drive prefixes, empty
segments, traversal, backslashes, NULs, tabs and line breaks fail validation.
Invalid paths are reported as `<invalid-path>` without echoing their contents.

## Worked classification example

The method is rule membership from the approved
[normalization plan](../plans/2026-05-07-issue-596-repo-structure-normalization.md),
Classification Rules (2026-05-07). No physical equations, numerical coefficients
or engineering acceptance limits apply to this structural check. The live
[machine-readable contract](../../config/repo_structure.yml) supplies the sets.

Evaluate these candidate repository-relative inputs with no durable exceptions:

| Input | Method | Answer |
|---|---|---|
| `README.md` | Member of the root-file allowlist. | Pass. |
| `scripts/maintenance/example.py` | Root directory is `scripts`, which is allowlisted. | Pass. |
| `coverage.xml` | Generated artifact and absent from the root-file allowlist. | `generated-tracked-path` and `root-allowlist`. |
| `src/tests/example.py` | Matches `src/**/tests/**` with no intermediate directory. | `forbidden-path`. |

Independent enumeration gives three violations for four candidate paths.
`test_worked_path_classification_example` in the
[contract tests](../../tests/repo_structure/test_repo_structure_contract.py)
asserts that exact list. CLI fixtures also show a clean repository passing and
an added root Python utility failing with its path and rule name. Malformed
contracts fail before any exception can authorize a path.

## Current inventory and remaining enforcement

The inventory at `624c72096bab8acd31e889b46b39bb8f87580e99` contains no tracked
`outputs/` evidence and none of the three former root utilities. Their removal
is already present in `9737ccbb`. There is no live generated-evidence exception
to grandfather. `durable_exceptions` therefore starts empty; no additional
output path is authorized. The separate evidence-retention decision stays
with #597.

Rechecking the available base inventory for this change confirms those paths
remain absent. The scoped utility-reference scan finds planning/review records
and rejection tests, with no surviving utility invocation to migrate. The
existing notebook allowlist matches the tracked notebooks. No utility or
generated evidence is restored, moved or reclassified by this change.

The `verify-repo-structure` pre-commit hook checks the complete repository on
every commit, including commits with no matching filenames. Its isolated Python
environment includes PyYAML. Install the repository's configured hooks before
using local enforcement.

The Repository Structure workflow runs the checker and its regression tests on
every pull request and every push to `main`, without path filters or optional
failure handling. Docs CI also covers `docs/standards/**` and `docs/domains/**`
and runs the docs routing contract tests plus `markdown-link-check` on this
standard, using the local hook's configured version and link configuration.
These checks do not change source,
move evidence, or regenerate skill pointers.

Repository administrators must require the `Repository structure contract`
status check in branch protection to prevent merging a failing check. Workflow
wiring alone does not establish that this repository setting is enabled.

## Generated-file retirement

`config/generated_retirements.yml` records one entry per retired generated file.
The schema and duplicate-key validation are ported from the reviewed retirement
rule. Each entry requires exactly `path`, `sha256`, `date`, `reason`,
`disposition` and `follow_up`. Paths are normalized repository-relative paths;
SHA-256 identifies the last committed bytes. Reasons require at least 30
characters and the follow-up identifies an issue or pull request in this repository.

Cold-store dispositions contain `kind: cold-store`, a relative `dataset_path`
and a relative `digest_list`. The storage root is documented in the register.
The original basename is retained beneath the dataset path. Before removal,
copy every file, read each copy back and compare its SHA-256 to the committed
source, then retain a digest list. LFS-tracked files require a separate disposition
and are left in the tree by this retirement.

The structure check validates the register on every default run, including clean
checkouts. Current generated deletions and rename sources require matching HEAD
blob digests. Entries for still-present files, duplicate paths, malformed entries
and remaining durable-exception classifications fail. Historical absent paths
receive schema validation; store availability and stored digests require separate
readback and are not established by a structure-check pass. A rename destination
still undergoes the ordinary path rules.

`generated_retirement_patterns` extends deletion coverage to nested generated
surfaces without granting permission to add generated output. The existing
`generated_patterns`, artifact patterns and addition rules remain in effect.
Reproducible dispositions from the source rule are also recognized, but these
retirements use cold-store copies exclusively.
