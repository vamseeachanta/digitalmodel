# Advisory CI module families

## Summary

The family inventory assigns every tracked Python module and test file a home.
It records absolute imports from application families into the proposed shared
ENGINE family. It does not select CI jobs. Snapshot freshness is report-only in
required checks; generator correctness and registry agreement remain tested.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-22 | Replace frozen assignments with reproducible rules to prevent unrelated additions from blocking required checks. |

## Generation

`config/ci/module-family-rules.json` is the classification input.
`config/ci/module-families.json` is its generated snapshot. Run from the repository
root:

```sh
python scripts/ci/generate_module_families.py --write
python scripts/ci/generate_module_families.py --check
python scripts/ci/generate_module_families.py --report
```

`--write` updates the snapshot. `--check` exits with status 1 for drift and is an
explicit maintenance check, outside required CI. `--report` reports drift with
status 0. Input or parsing errors still fail. No new required gate is added.
The existing family tests check the rules, live assignments, measured dependency
equality and snapshot internal consistency without requiring snapshot freshness.
Existing registry annotations must agree with the rules, including submodule
annotations. New registry records can inherit the rules without adding a field.

Only regular files listed by `git ls-files --stage -z` are inventoried. Untracked
files, environments, nested checkouts, symlinks and submodules are excluded.
Tracked files removed from the worktree are omitted. Stage new files before
regenerating. `--ref` reads a Git tree and source blobs without a checkout; an
optional `--output` keeps a comparison snapshot outside the repository. Tree
reads use the current classification rules so new paths can be evaluated before
integrating changes.

The longest dotted module prefix or test directory prefix wins. Exact test-file
exceptions preserve existing homes. An unmatched module or test uses `platform`;
this deterministic fallback needs review before any family selector is enabled.
The root `digitalmodel` facade is an exact ENGINE assignment, so it does not
absorb all descendants. `_output_paths` is explicitly proposed as ENGINE because
it centralizes output-path handling; that decision remains subject to maintainer
confirmation. Other ENGINE prefixes retain the existing proposed boundaries.

All previous source and test homes are preserved. Provenance records the
tracked generator and rule paths and a digest of the rules, source text and test
paths. No external input map or untracked measurement report is required.

## Dependency measurement

The generator parses source ASTs without importing application or solver code.
It includes `import digitalmodel`, qualified imports and absolute `from` imports,
including conditional and function-local imports. Imported symbols resolve to
the longest inventoried module. Edges are deduplicated and sorted per family.
An independent test scan asserts measured edges equal generated declarations;
it rejects both missing and surplus edges.

Relative imports, dynamic imports, strings and runtime path dependencies are
outside this measurement. The inventory alone does not establish collection
parity or permit selective execution. Advisory selection is tracked in #3345;
the inventory correction follows #2845.

## Inventory measurements

Both inventories were generated and checked using the same rules. The tracked
snapshot matches the branch source tree. The available main reference was read
directly through Git; no merge was performed.

| Source tree | Modules and namespaces | Test files | ENGINE modules |
| --- | ---: | ---: | ---: |
| Branch | 2613 | 1930 | 29 |
| Available main | 2625 | 1968 | 30 |

## Required-check behavior

Workflow configuration and branch protection are unchanged. The existing domain
detector remains responsible for test selection. Regression tests exercise its
routing and execute the aggregate's actual shell script:

| Example diff | Domain matrix | Checks still required |
| --- | --- | --- |
| This development document only | Empty | Detection, CI harness, Domain test aggregate and existing quality gates |
| A new Python module under the source package | Full existing matrix | Detection, CI harness, selected domain tests, Domain test aggregate and existing quality gates |

Docs-only runs fail when detection or the harness fails. Code runs also fail when
a selected domain result fails or is skipped. No required check is removed or
replaced with a snapshot-only result.
