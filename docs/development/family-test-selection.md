# Advisory family test selection

`scripts/ci/select_family_tests.py` prints a read-only JSON explanation of
which recorded families and test files a change would select. It also reports
the existing domain detector's selection for comparison. It does not run tests,
write an inventory, emit a CI matrix or change the active domain gates.

The family inventory referenced by #2845 and #3181 is absent from the current
default branch. Supply an existing version-one inventory with
`--families-file`; the command fails explicitly if that file is unavailable.
This change does not restore or generate the inventory.

Run from the repository root. To explain two revision endpoints:

```sh
python scripts/ci/select_family_tests.py \
  --families-file path/to/module-families.json \
  --base origin/main --head HEAD
```

The comparison uses `git diff --name-only --no-renames` between those endpoints,
matching the current detector. It does not compute a merge base. Renames expose
both paths, deletions retain their old paths, and an empty diff selects nothing.
For changes that are not committed, supply repository-relative paths directly:

```sh
python scripts/ci/select_family_tests.py \
  --families-file path/to/module-families.json \
  --changed-path src/digitalmodel/engine.py \
  --changed-path tests/test_engine.py
```

The command accepts the recorded `schema_version`, `families`,
`module_to_family`, `test_file_to_family` and `family_to_engine_deps` fields.
Other inventory metadata is not used. Duplicate JSON keys, unknown families,
invalid paths and missing or invalid ENGINE dependency lists are rejected.

For each changed path, `reasons` names the rule and selected families:

- `module-home`: an exact Python module or package initializer selects its home.
- `test-home`: a recorded test file selects its home.
- `engine-dependents`: an ENGINE module selects ENGINE and application families
  whose recorded dependency lists contain that exact module. The root package
  initializer matches `digitalmodel` exactly; it is not a descendant wildcard.
- `unmapped`: an unrecorded source, test, configuration or other path selects
  every recorded family and appears in `unmapped_paths`. Assignments are not
  inferred for new files, including files below a recorded package.

`test_roots` contains sorted, unique test file paths. Exact files are pytest
roots and preserve homes in directories containing several families. No files
are imported or collected. Results therefore depend on the supplied inventory;
stale entries may name deleted files, and newly added tests may be absent.

`domain_comparison` reports the current detector's domains and roots, plus
`family_only_test_files` and `domain_only_test_files` within the supplied
inventory. Revision comparisons include record pins from both endpoints and
revision-aware configuration routing. Explicit paths use the detector without
revision metadata. These differences are not a collection-parity check and do
not enumerate active-domain test files missing from the inventory.

Selection remains advisory. Static ENGINE dependency lists do not establish
dynamic dependencies, transitive dependencies inside ENGINE or application
cross-family isolation. Inventory freshness, collection parity, runtime savings
and enabling family-based CI dispatch remain open work. The existing domain
detector continues to determine which domain gates run.
