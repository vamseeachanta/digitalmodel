# Module-family inventory

The family contract is a proposed inventory. It does not select tests or
activate family gates. Frozen inventory drift is report-only in the existing
pytest check path; adding a source module or test does not require editing the
generated JSON to keep that check green.

## Generate and inspect

Run from the repository root with Python 3.11 or later:

```sh
python3 scripts/ci/generate_module_families.py
python3 scripts/ci/generate_module_families.py --write
python3 scripts/ci/generate_module_families.py --check
```

The default command reports missing, stale and changed assignments and whether
ENGINE dependencies differ, then exits successfully. Input or parsing errors
still fail. `--write` replaces the snapshot. `--check` requires identical bytes
and returns a failure for drift; it is an explicit maintenance command, not a
new required CI gate.

To inventory main without merging it, use the same revision for both commands:

```sh
python3 scripts/ci/generate_module_families.py --write --ref origin/main
python3 scripts/ci/generate_module_families.py --check --ref origin/main
```

The committed snapshot records the resolved source revision and hashes of the
rules, tracked paths and source bytes. A revision snapshot describes that
revision, so a report against another checkout can legitimately show drift.
Revision mode reads Git tree entries and blobs; default mode reads indexed
paths and their working contents. Neither walks untracked environments or
nested checkouts. Tests excluded from collection remain inventoried if tracked.

## Assignment rules

`config/ci/module-family-rules.json` is the editable input. Module assignment
uses explicit ENGINE boundaries, then the longest dotted module prefix. The
root `digitalmodel` facade is an exact boundary and does not capture domains.
The shared output-path module is proposed as ENGINE because it supplies common
output placement. ENGINE boundaries remain subject to maintainer confirmation.

Test assignment uses the longest directory prefix or exact filename rule.
Tests under the source tree inherit their module family. New paths without a
matching rule use the declared `platform` default. Existing file homes are
preserved by directory rules and exact exceptions. Node IDs inherit their file
home. These are inventory defaults; test selection requires separate work.

## Dependency measurement

The generator parses source ASTs without executing application modules. It
records absolute `import` and `from` imports, including bare `import digitalmodel`
and imports inside functions, conditions and type-checking blocks. For a
`from` import it records the base module and any importable child; symbols
resolve to the longest known module. Sets are sorted and deduplicated.

Generated dependencies must equal the measured edges. Tests reject both missing
and extra edges, exercise the reported import forms, and verify deterministic
generation and report-only drift. The snapshot can be reproduced with the
recorded revision and the tracked rules. Relative imports, dynamic imports,
runtime call graphs, collection parity and runtime savings are not measured.
