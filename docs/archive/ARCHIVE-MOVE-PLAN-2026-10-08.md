# Archive move plan — regenerated candidate manifest (2026-10-08)

Status: **review only.** This change adds a candidate list. No file is deleted
and nothing is copied to the archive by this change.

## Why this replaces PR #2146

PR #2146 (`chore/slim-20260922`) selected its files against the 2026-09-22
tree. By 2026-10-07, 1,538 of its 2,299 deleted paths still existed on `main`
and 124 of those had been modified since; merging it would have deleted live
inputs and code. The selection is therefore regenerated here against current
`main`, and the move is split into a review step (this change) and a later
move step.

## Files in this change

| File | Contents |
|---|---|
| `archive-candidates-2026-10-08.csv` | One row per candidate (columns below) |
| `archive-candidates-2026-10-08.summary.json` | Rule, base commit, totals by top-level folder and class, reference counts, comparison with PR #2146 |

CSV columns:

- `path` — repo-relative path at the base commit.
- `top_level`, `ext`, `class` — grouping keys. Classes follow PR #2146:
  `solver-inputs`, `html-report-renders`, `documentation-images`,
  `office-documents`.
- `size_bytes`, `sha256` — on-disk working-tree bytes of the checkout that
  generated the list (text files carry CRLF line endings because the checkout
  used `core.autocrlf=true`).
- `blob_size_bytes`, `sha256_blob` — the git blob (canonical repository bytes).
  The archive copy and its verification shall use the blob values.
- `last_commit_date` — date of the last commit touching the path.
- `referenced`, `ref_count` — whether the file basename appears in any other
  tracked text file (`git grep -l -I -F <basename>`), and in how many.
- `ref_count_noncandidate` — references from files that are not themselves
  candidates (a reference from a report that moves with the image is not a
  blocker; a reference from code, config or a kept page is).
- `first_ref_path` — first referencing path, preferring a non-candidate.
- `in_pr2146` — whether PR #2146 also deleted this path.

## Selection rule (recovered from PR #2146)

Applied to `git ls-files` (tracked files only; no Git LFS in this repository):

- Any size, case-insensitive extension: `dat lis qtf sim owr igs stl dwg dxf
  engd scdoc gz` (solver inputs/results, CAD and mesh), `html` (report
  renders), `png jpg jpeg jfif gif svg bmp tif tiff webp` (images),
  `pptx ppt docx doc pdf xlsx xls` (office documents).
- Size-gated: `yml yaml csv` only when the git blob exceeds 1,000,000 bytes.
- Excluded prefixes: `src/` and `tests/` (excluded entirely in this
  regeneration; PR #2146 removed 164 files there), `docs/api/` (generated API
  site checked by CI) and `assets/logo/` (docs generator inputs) — the last two
  were restored to git within PR #2146 itself.

## Totals at base commit `7665afc3`

| Top-level folder | Files | Blob bytes |
|---|---|---|
| `docs/` | 1,314 | 1,887,708,314 |
| `examples/` | 91 | 53,004,729 |
| `scripts/` | 2 | 1,165,388 |
| `config/` | 3 | 657,140 |
| `references/` | 4 | 236,723 |
| **Total** | **1,414** | **1,942,772,294** (on-disk 1,952,163,371) |

707 candidates have a basename reference elsewhere (697 from a non-candidate).
The basename match is a coarse signal: generic names (`spec.yml`,
`report.html`, `index.html`) match many unrelated files, so `ref_count` over
about 50 needs a manual look. Large `spec.yml` / `includes/*.yml` files under
`docs/domains/orcaflex/` (model library and modular examples) may be live
inputs to library code and shall be checked before any move.

1,374 candidates were also in PR #2146 (every PR #2146 path still on `main`
outside `src/` and `tests/`); 40 are new since 2026-09-22.

## How the move will run (later, after owner review)

1. **Owner review of the list.** The owner marks the rows to move. Rows with
   `ref_count_noncandidate > 0` are excluded by default unless the owner
   decides otherwise for that row.
2. **Refresh against `main`.** Re-run the generator
   (`scripts/maintenance/archive_candidates.py`, read-only) on the then-current `main`
   and drop any approved row whose `sha256_blob` changed or whose path no
   longer exists.
3. **Copy.** Copy each approved blob (`git cat-file blob <oid>`, not the
   working-tree file) to `/mnt/ace/digitalmodel/<repo-relative path>`.
4. **Verify.** Recompute SHA-256 of every archive copy and compare it with
   `sha256_blob`. Any mismatch stops the move.
5. **Record.** Append the verified rows to the archive manifest
   `/mnt/ace/digitalmodel/MANIFEST.sha256.tsv` (tab-separated: `sha256`,
   `bytes`, repo-relative path), the mechanism PR #2146 used.
6. **Registry in the repository** (PR #2146 mechanism; there are no
   per-file stub files):
   - `data/inputs.yaml` — machine-readable pointer: archive root
     `/mnt/ace/digitalmodel`, base commit, per-class file count, bytes and
     extensions.
   - `DOCUMENT-MAP.md` — human-readable map: what moved, what stayed and why,
     how to retrieve and verify a file.
   - `.gitignore` — archive-policy section so moved files are not re-added.
7. **Delete in git** only the verified, approved rows, in a separate PR that
   references this manifest. Tests that consumed a moved file shall skip with a
   message naming the archive path rather than fail.

No history rewrite is part of this plan.
