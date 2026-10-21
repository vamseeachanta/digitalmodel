# Historical atlas source evidence

The historical evidence index inventories every committed computed atlas with a
recorded code revision and content fingerprint. It preserves the existing
manifests, grids, surrogate specifications, validation evidence and default
pointers. Of the 34 inventoried bases, 30 reconstruct from retained history;
four remain `NOT_EVALUATED`. These are fingerprint reconstruction results, not
qualification of the engineering methods or proof of a historical execution.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-21 | Record historical source evidence and expose reconstruction gaps without changing numerical artifacts. |

## Inventory and interpretation

The machine-readable inventory is
[`atlases/historical-provenance.json`](../../../atlases/historical-provenance.json).
Each entry records:

- The manifest path, workflow identifier, original recorded code revision and
  stored content fingerprint.
- A full retained source revision and raw SHA-256 digests for the input template,
  registry, historical refresh module, complete response wrapper module and the
  response source files declared by that historical refresh module.
- The historical registry specification, response arguments, ordered response
  source paths and standards editions extracted from that snapshot.
- Raw digests of the unchanged manifest, grid, surrogate and validation files.
- Either `VERIFIED` for a matching historical fingerprint or `NOT_EVALUATED`
  with the reconstruction gap stated explicitly.

The preferred source snapshot is the earliest retained publication of the
fingerprint that reconstructs it. A matching original recorded revision is a
fallback only when it is also retained in the default branch ancestry. These
snapshots describe publication evidence. They do not retroactively assert that
the recorded computation ran from a clean checkout at that revision.

The issue described seventeen manifests carrying a revision from a temporary
branch. That state is absent from the current default branch. The index covers
the actual current historical manifests, including later versions, instead of
applying a blanket revision substitution. Existing `code_version` values remain
unchanged even where they are unavailable or do not reconstruct the fingerprint.

## Method and source contract

The implemented `legacy-raw-v1` contract is the algorithm in
`src/digitalmodel/parametric/refresh.py::content_fingerprint` and `_sha_files`
at each indexed source revision. It uses the historical registry and literal
`SOURCE_FILES` and `STANDARDS` tables, not the current module's defaults.
The response sources are read as bytes; historical Python is parsed with AST
literal extraction and is never executed or imported.

With `H` denoting SHA-256, the legacy source digest is
`H(path_1 || bytes_1 || path_2 || bytes_2 || ...)`, in the historical list order.
The input digest is `H(raw input bytes)`. The final fingerprint is `H(JSON(basis))`,
where JSON uses the historical Python `json.dumps(..., sort_keys=True)` defaults.
The basis fields are `spec`, `response_kwargs`, `standards`, `input_sha256` and
`source_sha256`. These are software identity operations; no engineering
coefficient, acceptance limit or standard value is introduced.

Legacy fingerprints excluded the response wrapper. The additional per-file map
now binds the entire historical `generate.py`, including wrapper helpers and
module constants. This preserves the spectral and mudmat wrapper snapshots
without claiming that the older hash originally authenticated them. The older
VIV basis retains its single recorded standards edition rather than acquiring
the newer VIV standards list. Downstream dependency coverage is the historical
declared source list, not a claim of transitive runtime dependency closure.

## Worked example: reconstruct a retained basis

Input: the `rao_tabulation/6b27320669f6` manifest and its inventory entry. Its
expected answer comes directly from that unchanged manifest:
`6b27320669f6c8ed6652bf3d28dc2c705ee9744a3a25bc71cf0704caadfc1110`.
Run the following from the repository root in an environment with the package
dependencies installed:

```python
import json
from pathlib import Path
from digitalmodel.parametric.historical_provenance import verify_git

index = json.loads(Path("atlases/historical-provenance.json").read_text())
record = next(row for row in index["atlases"]
              if row["manifest"] == "atlases/rao_tabulation/6b27320669f6/manifest.yaml")
answer = verify_git(record, Path.cwd(), retained_ref="origin/main")
assert answer["status"] == "VERIFIED"
assert answer["fingerprint"] == record["content_fingerprint"]
```

Method: require the full source revision to be an ancestor of the retained ref;
check every source and artifact digest; extract the historical basis; reconstruct
the legacy fingerprint. The answer is an exact identity comparison with the
stored fingerprint. It does not recalculate RAOs or test interpolation accuracy.
The independent toy hash calculation in
`tests/parametric/test_historical_provenance.py` spells out the source byte stream
separately from the implementation.

To audit all entries:

```console
python -m digitalmodel.parametric.historical_provenance
```

Each output line gives the manifest and status. The command returns nonzero if
any entry is rejected or remains `NOT_EVALUATED`; the known gaps are not silently
counted as successful reconstruction.

## Source archives and Git history requirements

A Git checkout needs the indexed commits and blobs, their complete ancestry to
the retained ref, and the unchanged atlas files. A shallow clone, missing object,
missing ref or detached temporary branch alone is insufficient. The verifier
does not fetch, modify Git metadata or silently use current source files.
Keep the index in retained history alongside the atlas artifacts. Its digests
are trusted reviewable metadata, not signatures or an independent trust service.

For a source archive, retain every file named in an entry's `sources` map at its
repository-relative path, preserving exact bytes and line endings. The archive
must be accompanied by the trusted index and unchanged atlas artifacts.
After extracting it, call `verify_sources(record, archive_reader(source_root))`
and `verify_artifacts(record, artifact_root)`. No Git installation is required
for those operations. Archive verification establishes byte agreement and
fingerprint reconstruction; it cannot establish Git ancestry. A digest map
alone cannot recover missing bytes. Symbolic links, missing files, changed bytes,
omitted wrapper digests and unsupported algorithms are rejected.

## Follow-up

These retained publication snapshots do not reconstruct their stored fingerprint:

| Atlas | Status | Evidence needed |
| --- | --- | --- |
| `fowt_mooring/b4046a89d219` | `NOT_EVALUATED` | A retained source archive or revision matching the stored basis; the matching recorded object is outside retained ancestry. |
| `free_span/914ec0881a84` | `NOT_EVALUATED` | Source bytes matching the stored basis; the recorded revision is unavailable and inspected retained candidates do not reconstruct it. |
| `riser_fatigue/f272ab682259` | `NOT_EVALUATED` | A retained historical refresh table matching the stored basis; the matching recorded object is outside retained ancestry. |
| `riser_fatigue/f2ca1d87007d` | `NOT_EVALUATED` | Retained touchdown source bytes matching the stored basis; the matching recorded object is outside retained ancestry. |

Their index entries bind the available publication evidence and preserve the
disagreement. `NOT_EVALUATED` does not permit use of these snapshots as matching
source evidence. Resolving the gaps requires authentic retained source evidence,
not replacement of the stored fingerprint, grid or acceptance criteria.
