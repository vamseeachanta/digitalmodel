# Include-target checks

The include-target check prevents new broken YAML input chains from entering
version control. It scans tracked YAML outside the excluded `projects/` tree,
using the shared include-reference extractor with a lexical adapter for
`BaseFile`, repeated keys, and scalar aliases. It does not load a solver.

Run from the repository root:

```sh
PYTHONPATH=src python scripts/ci/check_include_targets.py
```

A referenced target must appear in the Git index and exist as a file in the
working tree. An index entry alone does not establish that its bytes are
available: `git ls-files` lists cached entries even after a local file removal.
See the [Git ls-files options](https://git-scm.com/docs/git-ls-files#_options),
particularly `--cached` and `--deleted`.

The command fails on extraction errors, new broken references, and stale
baseline entries. Existing defects are recorded as `[source, reference]` JSON
rows in `scripts/ci/include_target_baseline.txt`. Repair a chain and remove its
baseline row together. Do not add baseline entries to suppress a new defect.
`--write-baseline` is for initial inventories and refuses to overwrite a file.
The existing regression ceiling also exposes baseline growth in review.

## Worked example: an unavailable target

Given a tracked `models/run.yml` containing:

```yaml
BaseFile: ../inputs/case.dat
```

The target is calculated by joining the source directory `models/` with
`../inputs/case.dat`, then normalizing the result to `inputs/case.dat`.
The expected answers follow directly from file membership and existence:

| Target condition | Result |
| --- | --- |
| Tracked, with file present | Pass |
| File present, but untracked | Fail; unresolved include target |
| Tracked, but file absent | Fail; target missing from working tree |
| File ignored and untracked | Fail; responsible ignore rule is also printed |
| Broken reference matches baseline | Pass pending repair |
| Repaired reference still in baseline | Fail; stale baseline entry |

The same rules apply to `includefile`. Executable examples are in
`tests/scripts/test_check_include_targets.py`; fixtures create disposable Git
repositories without loading engineering inputs or licensed software.

## Ignore rules and limits

The root virtual environment is excluded by `/env/`. A leading slash anchors
this pattern; unanchored `env/` would also exclude nested engineering inputs.
See [Git ignore pattern format](https://git-scm.com/docs/gitignore#_pattern_format).
Specific nested virtual-environment paths remain ignored, while YAML environment
inputs under `docs/` have explicit re-inclusions. Re-inclusion cannot override
an excluded parent directory.

The checker establishes input-file availability, not physical correctness,
solver compatibility, or a qualified result. It reads working-tree contents
against the current index; it does not validate an isolated staged snapshot.
The broader audit of other directory ignore patterns, project-tree publication,
and repair of existing baseline defects remain separate work.
