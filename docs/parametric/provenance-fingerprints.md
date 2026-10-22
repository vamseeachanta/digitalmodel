# Provenance fingerprints and manifest ancestry

Text fingerprints identify a declared software basis. UTF-8 inputs with LF,
CRLF or lone CR line endings now share a canonical LF representation. The
manifest inventory separately reports whether recorded revisions belong to
retained history. Neither operation qualifies engineering results.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-22 | Define canonical text, reproduce a hash example and record manifest ancestry gaps without replacing atlas data. |

## Fingerprint coverage and method

The standalone operability fingerprint covers the config YAML text, its declared
standards editions, a per-file digest for every path in
`operability_atlas.SOURCE_FILES`, and the basename, response and physics strings.
It covers the declared response files, including `operability_atlas.py` itself;
it does not promise complete transitive dependency coverage.

Let `N(b)` strictly decode UTF-8, reject NUL, replace CRLF with LF followed by
lone CR with LF, and encode UTF-8 again. The per-file digest is
`SHA256(N(file_bytes))`. The config basis stores `N(config_bytes).decode("utf-8")`.
The outer digest is `SHA256(json.dumps(basis, sort_keys=True).encode("utf-8"))`,
using the existing JSON defaults. The standards list is parsed from the same
canonical config text. Sorted JSON keys preserve the original contract on LF
inputs. No Unicode normalization, BOM stripping or whitespace folding occurs.
Non-UTF-8 and NUL-containing files raise `ValueError` naming the text contract;
replacement decoding and binary fallback are not supported.

The shared implementation is `digitalmodel.parametric.text_basis`, used by both
standalone operability and parametric refresh. The CRLF replacement previously
shown in the parametric refresh change is consolidated there and extended to
lone CR and strict text validation. The checked-out base contained raw-byte
refresh hashing rather than a standalone normalization helper.

Sources for the software identity operations are the existing
`src/digitalmodel/drilling_riser/operability_atlas.py::content_fingerprint`
contract and the [Python 3.11 hashlib API](https://docs.python.org/3.11/library/hashlib.html).
This change introduces no engineering coefficient, acceptance threshold or
numerical tolerance.

## Worked example: inputs, method and answer

Inputs are the literal byte strings `b"alpha\nbeta\n"` and
`b"alpha\r\nbeta\r\n"`. Raw SHA-256 hashes differ because the bytes differ.
After `N`, both streams are exactly `b"alpha\nbeta\n"`.

| Calculation | SHA-256 |
| --- | --- |
| LF before normalization | `e49c81e2d2f84e259d40e2fb8192f3bcd198b355184845d76d8f58807d0d78ee` |
| CRLF before normalization | `98ab4d3aeab1e120560e942e2df6a0db1147bf94bafcf1590000ffb3c2b6fc80` |
| Either after normalization | `e49c81e2d2f84e259d40e2fb8192f3bcd198b355184845d76d8f58807d0d78ee` |

The values above were produced by the committed script:

```console
PYTHONPATH=src python scripts/provenance/fingerprint_example.py --work-dir "$TMPDIR/hash-example"
```

Use an environment with the package dependencies installed and an existing
scratch-directory setting. The script writes only its supplied example folder
and emits JSON. `tests/provenance/test_fingerprint_example.py` independently
calculates each expected digest with `hashlib` over the literal bytes and checks
these documented values. The example demonstrates text identity, rather than a
full atlas fingerprint or a collision-resistance experiment.

## Source availability and installed packages

`content_fingerprint` checks the config and every declared source file before
hashing. Missing files raise `SourceBasisUnavailable`, a `FileNotFoundError`
subclass with a `missing_files` tuple of logical input names. Installed packages
without the repository source layout receive this explicit failure. There is no
substitution of installed code, omission of missing files or successful fallback.
A complete matching source snapshot is required to calculate the fingerprint.

The standalone screening consumer on the inspected base does not verify the
fingerprint. Its integration with a fail-closed `ESCALATE` verdict, absent
utilisation and distinct unavailable-source versus mismatch reasons remains
`NOT_EVALUATED`. The text-hashing change alone does not establish that gate.

## Current cache identity

Before edits, the LF checkout fingerprint matched the committed default. The
normalizer preserves the legacy hash of unchanged LF fixtures. Editing the
self-hashed implementation necessarily changes the current source identity;
therefore the existing stored fingerprint is no longer the current basis.
Generated atlas files are preserved. The rebuild regression retains the exact
numerical grid checks and compares surrogate specifications apart from the
source-dependent identity. Requalification and replacement of the default cache
must accompany the separate consumer-gate integration; a historical fingerprint
must not be rewritten to imply a computation used the new source.

## Manifest inventory

The committed [JSON report](atlas-manifest-inventory.json) was produced by:

```console
python scripts/provenance/inventory_atlas_manifests.py
```

Its exact search pattern is `atlases/*/*/manifest.yaml` and its inventory and retained
revision is `3554b14fde67f44b1c52b81b6c72106282eaff29`. Counts come from that script's report:

| Status | Manifests |
| --- | ---: |
| REACHABLE | 18 |
| UNAVAILABLE | 9 |
| UNREACHABLE | 13 |

`UNAVAILABLE` includes absent recorded revisions and unknown objects. The JSON
report gives each original `code_version` and reason. `UNREACHABLE` means the
object exists but `git merge-base --is-ancestor` returned its documented
non-ancestor status. Other Git errors never count as ancestry success. See the
[Git merge-base manual](https://git-scm.com/docs/git-merge-base),
`--is-ancestor`, for the status contract. The report is a snapshot of recorded
locators, separate from historical fingerprint reconstruction.

`tests/provenance/test_atlas_manifest_revisions.py` audits the current checkout.
Known historical exceptions are enumerated by exact manifest path, original
revision, status, inventory reason and explanatory reason in
`tests/provenance/atlas_manifest_revision_exceptions.yaml`. They acknowledge
debt and never convert unavailable evidence into verified ancestry. New gaps,
changed locators, unused exceptions and unavailable retained refs fail the guard.
The synthetic Git tests cover an ancestor, an orphan object, an unknown object,
a shallow clone, a missing retained ref and a source archive.

## Source archives and Git history requirements

The ancestry guard requires Git, the manifest tree, the recorded commit objects,
complete ancestry and `origin/main`. Only a shallow checkout causes an explicit
skip, with a message requesting complete history. A missing ref, missing object,
missing Git installation, empty manifest tree or source archive cannot silently
pass. The inventory reports unavailable evidence and never fetches automatically.
CI that needs an ancestry verdict must provide complete retained history.

An exported source archive cannot prove Git ancestry. It can calculate the
portable standalone fingerprint only when every declared source and config
file is supplied. For historical raw-byte evidence, preserve the original bytes
and use the separate [historical source evidence contract](../plans/parametric-refresh/historical-provenance.md).
Current text normalization does not migrate or reinterpret historical digest
maps, manifests, numerical artifacts or validation criteria.

## Limits and NOT_EVALUATED

- Consumer fingerprint-gate integration and default-cache requalification are
  `NOT_EVALUATED` here; overlapping cache work is handled separately.
- The unresolved direct-consumer qualification recorded in audit issue #2746,
  section 14, is `NOT_EVALUATED` by these identity checks.
- Durable source reconstruction for unavailable or unreachable recorded
  revisions is `NOT_EVALUATED` by ancestry alone. The existing historical evidence
  index records separate per-file publication-basis evidence and unresolved
  reconstruction gaps; this inventory does not replace it.
- Hash agreement is neither a signature nor proof of execution, numerical
  correctness, engineering suitability or dependency closure.
