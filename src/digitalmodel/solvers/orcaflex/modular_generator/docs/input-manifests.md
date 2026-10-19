# Generated input fingerprints

Both `ModularModelGenerator.generate()` and `generate_with_overrides()` write
`input-manifest.json` before publishing the generated inputs. A missing dependency
or an invalid composition graph prevents publication and preserves any existing
output. `GenerationResult.manifest_file` identifies the published manifest for
the override entry point.

This implements input integrity evidence for issue #2515. It does not establish
engineering acceptance or native solver compatibility.

## Scope and format

The manifest seeds include `master.yml`, `inputs/parameters.yml` and every
include generated in the current invocation. The collector follows `BaseFile`
and `includefile` directives recursively, including directives nested inside
mappings or sequences. Repeated mapping keys are retained during discovery.
References resolve relative to the containing file. Shared dependencies are
hashed once; composition cycles fail.

Paths use forward slashes relative to the bundle root. Absolute references,
references outside the bundle (including symlink escapes), missing files and
malformed YAML fail. Dependencies outside the bundle must be packaged inside it
with updated references before manifest generation. No path is copied or rewritten
by the collector.

The format identifier is `orcaflex-input-manifest-v1`. `files` maps each
relative path to the lowercase SHA-256 digest of its exact bytes. The aggregate
`fingerprint` is defined by:

```text
record(path) = UTF8(path + NUL + files[path] + LF)
fingerprint = SHA256(concatenate(record(path) for path in sorted(files)))
```

The manifest also records sorted `roots`, `opaque_dependencies`, the hash
algorithm and revision evidence. `manifest_sha256` covers the complete manifest
object excluding that digest field. This object is encoded as UTF-8 JSON with
sorted keys, compact separators, unescaped Unicode and no non-finite numbers.
The final newline in the manifest file is outside this object digest.

The record encoding and format identifier are definitions of this format.
SHA-256 is specified by [RFC 6234 (2011), sections 4 and 6.2](https://www.rfc-editor.org/rfc/rfc6234).
The existing [agent pack](agent-pack.md) defines G0.7/C07 and G0.8/C08 under
AQG r0, revision 0. No physical constants or numerical engineering tolerances
are introduced here.

## Independent checking

```python
from pathlib import Path
from digitalmodel.solvers.orcaflex.modular_generator.input_manifest import (
    verify_input_manifest,
    write_input_manifest,
)

bundle = Path("generated-model")
# For an existing bundle, explicitly seed every generated input, including
# files such as parameters that are not reached from the master.
write_input_manifest(bundle, ["master.yml", "inputs/parameters.yml"])
check = verify_input_manifest(bundle)
print(check.status, check.findings)
```

The checker re-reads the recorded seed files, rediscovers their dependency graph
and recomputes the per-file and aggregate digests. A changed include, missing
file, omitted digest, unexpected record or changed manifest metadata fails.
The unit tests use the published `abc` SHA256 TEST1 digest in RFC 6234 section
8.5, manually enumerated file graphs and independently assembled hash records.

`FingerprintCheck.status` describes the declared YAML composition closure:

| Status | Meaning |
| --- | --- |
| `PASS` | The declared composition inputs match all recorded fingerprints. |
| `FAIL` | Evidence is unreadable, incomplete, invalid or inconsistent. |
| `NOT_EVALUATED` | Opaque inputs are hashed, but their dependency graphs cannot be inspected. |

## Known limits and later slices

Generator, template and design-basis revisions each explicitly record
`NOT_EVALUATED`. No immutable revision is inferred from a filename or package
version. Resolving and binding those revisions is a later slice; a fingerprint
`PASS` does not satisfy G0.8/C08.

Only explicit `BaseFile` and `includefile` composition references are followed.
Other solver data-file fields and external resources require separate discovery
before claiming complete native input coverage. Non-YAML dependencies are
fingerprinted as opaque bytes; their transitive dependencies are not evaluated.

The manifest is unsigned. It detects stale or inconsistent evidence, and does
not authenticate a manifest rewritten together with its digests. Existing
unreferenced files from an earlier generation are not current invocation inputs.
Publication retains the generator's existing file-copy behaviour and is not
atomic against copy failures or concurrent filesystem changes. No licensed
solver is started or imported by manifest collection or checking.
