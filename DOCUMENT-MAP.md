# digitalmodel — Document Map

Bulky engineering artifacts that left git, where they are kept, and how to get
one back. The machine-readable form is `data/inputs.yaml`.

## 2026-10-08 move (owner decision D01)

**1,394 files (1,816,070,378 git blob bytes)** were removed from git. They are
kept on the archive host (`ace-linux-1`) in a content-addressed store with one
copy per unique content: **1,249 blobs, 1,715,309,069 bytes**. The 145 files
whose content duplicates another moved file share that file's blob.

```
/mnt/ace/digitalmodel/blobs/sha256/<first two hex digits>/<sha256>.<ext>
```

The selection rule, the candidate list and the review record are in PR #2293
(`docs/archive/ARCHIVE-MOVE-PLAN-2026-10-08.md` on that branch). In short: the
extension/size rule of PR #2146, minus every file that `src/` or `tests/`
explicitly reads (repo path, unique filename, or a resolvable glob outside a
hygiene sweep), minus 11 model YAML files kept because they carry model
features no file staying in the repository has.

| Class | Files | Blob bytes |
|---|---|---|
| solver-inputs | 321 | 846,160,176 |
| html-report-renders | 470 | 676,308,437 |
| documentation-images | 553 | 203,416,172 |
| office-documents | 50 | 90,185,593 |
| **Total** | **1,394** | **1,816,070,378** |

Records in this repository (`data/archive/2026-10-08/`):

| File | Contents |
|---|---|
| `MANIFEST.blob-map-2026-10-08.tsv` | every moved repo path → `sha256_blob`, bytes, `blob_path` |
| `MANIFEST.blobs-2026-10-08.sha256.tsv` | one row per stored blob: `sha256`, bytes, `blob_path` |
| `VERIFY-blobs-2026-10-08.tsv` | SHA-256 recomputed from the stored bytes on the archive host: 1,249 of 1,249 OK |

The same two manifests and the verification record are kept at the archive
root, `/mnt/ace/digitalmodel/`. The stored bytes are the git blob bytes (the
canonical repository content), not a Windows working-tree copy with CRLF line
endings.

The moved paths are listed in the archive section of `.gitignore` so they are
not re-added; `tests/maintenance/test_archive_2026_10_08_pointers.py` checks
that none is tracked, that every one is ignored, and that the records agree.

### Retrieving a file

```bash
# repo path -> blob path
grep -P '^docs/some/path/file.html\t' data/archive/2026-10-08/MANIFEST.blob-map-2026-10-08.tsv
# copy over Tailscale and check the digest against the blob name
scp '<user>@ace-linux-1:/mnt/ace/digitalmodel/<blob_path>' docs/some/path/file.html
sha256sum docs/some/path/file.html
```

## Earlier relocations

The archive root also holds earlier relocations that mirror repo-relative
paths (2026-03-24 and the 2026-09-22 copy made for PR #2146). Their manifest
is `/mnt/ace/digitalmodel/MANIFEST.sha256.tsv` and their log
`/mnt/ace/digitalmodel/RELOCATION-LOG.md`; the 2026-10-08 move adds to the
archive root and changes none of their files.

## Policy

- Solver program binaries never belong in git or the archive.
- Regenerable bulky outputs stay local and out of git (see `.gitignore`).
- The archive is the home of irreplaceable bulky inputs and reference
  outputs; git keeps code, manifests, pointers and small key results.
