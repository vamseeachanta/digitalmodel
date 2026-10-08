# Archive move plan — regenerated candidate manifest (2026-10-08)

Status: **review only.** This change adds a candidate list, a deduplicated blob
map and a model-YAML feature inventory. No file is deleted and nothing is
copied to the archive by this change.

Revision 2 (2026-10-08) applies the owner's round-4 decision on the first list
(1,414 files, 1.94 GB): reduce the list, deduplicate, use `/mnt/ace` as the
home of the required data with one copy per unique content, and keep every
unique feature of the model YAML files reachable by the modules.

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
| `archive-candidates-2026-10-08.summary.json` | Rule, base commit, totals, exclusions with evidence, dedup figures, model-YAML summary, comparison with PR #2146 |
| `archive-blob-map-2026-10-08.csv` | Every candidate repo path mapped to its single content-addressed blob in the archive store |
| `model-yaml-feature-inventory-2026-10-08.json` | Feature definition, feature-to-file counts, files with unique features, feature cover, candidate-only features and the candidates kept for them |

All files are produced by `scripts/maintenance/archive_candidates.py`
(read-only; tests in `tests/maintenance/test_archive_candidates.py`):

```
python scripts/maintenance/archive_candidates.py <checkout> <pr2146_deleted_list.txt> \
    docs/archive/archive-candidates-2026-10-08.csv \
    docs/archive/archive-candidates-2026-10-08.summary.json \
    --blob-map docs/archive/archive-blob-map-2026-10-08.csv \
    --features docs/archive/model-yaml-feature-inventory-2026-10-08.json
```

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
  candidates.
- `first_ref_path` — first referencing path, preferring a non-candidate.
- `in_pr2146` — whether PR #2146 also deleted this path.
- `model_yaml_kind` — `orcaflex-native`, `orcawave` or `spec` when the file is a
  model YAML; empty otherwise.
- `unique_features` — number of model features no other tracked file carries.
- `dup_group` — duplicate-content group number (empty when the content occurs
  once); groups are numbered by bytes saved, largest first.
- `blob_store_path` — the content-addressed archive path of this file's bytes.
- `is_canonical_copy` — `True` for the one path per group that names the blob
  (lexicographically first path).
- `keep_for_feature` — `True` when the file stays in the repository because it
  carries a model feature that would otherwise leave with the candidates.

## Selection rule

Applied to `git ls-files` (tracked files only; no Git LFS in this repository):

- Any size, case-insensitive extension: `dat lis qtf sim owr igs stl dwg dxf
  engd scdoc gz` (solver inputs/results, CAD and mesh), `html` (report
  renders), `png jpg jpeg jfif gif svg bmp tif tiff webp` (images),
  `pptx ppt docx doc pdf xlsx xls` (office documents).
- Size-gated: `yml yaml csv` only when the git blob exceeds 1,000,000 bytes.
- Excluded prefixes: `src/`, `tests/`, `docs/api/` (generated API site checked
  by CI) and `assets/logo/` (docs generator inputs).
- **New in revision 2 — excluded when referenced from `src/` or `tests/`.** A
  rule match is dropped when any file under `src/` or `tests/` contains one of:
  its full path, its last two path components, its basename when that basename
  is unique among tracked files, or any ancestor directory at depth 4 or more
  (for example `docs/domains/orcaflex/pipeline`). The directory rule is
  deliberately conservative: code that builds a path from a directory constant
  reads files the basename search cannot see.

## Totals at base commit `c4563b11` (`main` at `e05ba133` plus this branch)

1,424 tracked files match the extension/size rule; 476 of them
(1,021,816,411 blob bytes) are referenced from `src/` or `tests/` and are
removed from the list. 948 candidates remain.

Exclusions by evidence:

| Evidence (ancestor directory named in `src/` or `tests/`) | Files | Blob bytes |
|---|---|---|
| `docs/domains/orcawave/L00_validation_wamit` | 231 | 415,911,870 |
| `docs/domains/orcaflex/pipeline` | 55 | 262,777,363 |
| `docs/domains/orcaflex/risers` | 44 | 92,721,125 |
| `docs/domains/freecad/src/ref` | 1 | 64,528,245 |
| `docs/domains/orcaflex/examples` | 22 | 59,372,435 |
| `docs/domains/orcaflex/library` | 18 | 55,674,920 |
| `docs/domains/orcawave/L01_aqwa_benchmark` | 48 | 7,885,393 |
| `docs/domains/charts/phase2/ocimf` | 7 | 3,275,040 |
| `examples/domains/fatigue/advanced_examples` | 4 | 1,677,592 |
| `docs/domains/orcawave/examples` | 1 | 6,481 |
| File-level match (full path, parent/basename or unique basename) | 45 | 57,985,947 |
| **Total excluded** | **476** | **1,021,816,411** |

Remaining candidates:

| Top-level folder | Files | Blob bytes |
|---|---|---|
| `docs/` | 854 | 870,137,605 |
| `examples/` | 85 | 48,801,144 |
| `scripts/` | 2 | 1,165,388 |
| `config/` | 3 | 657,140 |
| `references/` | 4 | 236,723 |
| **Total** | **948** | **920,998,000** (on-disk 925,815,905) |

| Class | Files | Blob bytes |
|---|---|---|
| solver-inputs | 210 | 575,786,326 |
| documentation-images | 531 | 198,141,769 |
| office-documents | 49 | 90,168,961 |
| html-report-renders | 158 | 56,900,944 |

## Deduplication by blob SHA-256

| Measure | Value |
|---|---|
| Candidate files | 948 |
| Unique blobs (distinct content) | 810 |
| Duplicate groups (content held by 2 or more paths) | 87 |
| Duplicate files (copies beyond the first in each group) | 138 |
| Blob bytes, all paths | 920,998,000 |
| Blob bytes, one copy per unique blob | 835,664,070 |
| **Bytes saved by keeping one copy per unique content** | **85,333,930** (9.3 %) |

Duplicate copies by class: documentation images 101 files (50,663,132 bytes),
office documents 14 (14,170,097), solver inputs 13 (20,493,381), HTML renders
10 (7,320). The largest groups are training CAD/mesh files held twice or three
times under the AQWA examples, and draft office documents and result plots
duplicated across the legacy API RP 2RD guide folders; the 20 largest groups
are listed in the summary JSON (`dedup_top_groups`).

## `/mnt/ace` as the home of the required data

The archive store keeps **one copy per unique blob**, content-addressed:

```
/mnt/ace/digitalmodel/blobs/sha256/<first two hex digits>/<sha256>.<ext>
```

`archive-blob-map-2026-10-08.csv` maps every original repository path to its
blob (`path`, `sha256_blob`, `blob_size_bytes`, `blob_path`,
`is_canonical_copy`). Several paths may map to one blob; the map is the
authority for "where did this file go", and the blob name is its own integrity
check. The extension of the canonical path is kept so the stored file opens in
its application.

## Model YAML unique-feature inventory

Owner note (round 4): every unique feature in a model `.yml` file contains
information the modules need for enhanced analysis. Model YAML is therefore
inventoried, not excluded wholesale.

**Scope.** Every tracked `*.yml` / `*.yaml` outside `src/` and `tests/` (4,459
files) is parsed. A file is a model YAML when its top level holds OrcaFlex
model sections (`General`, `Environment`, `Lines`, `LineTypes`, `Vessels`,
`VesselTypes`, `6DBuoys`, `Shapes`, `Winches`, `Groups`, `VariableData`,
`BaseFile`, …), OrcaWave model keys (`Bodies`, `SolveType`, …) or a modular
spec (`metadata` with `environment`, `geometry`, `lines`, …). This covers
`docs/domains/orcaflex/**` `spec.yml` and `includes/*.yml`, monolithic models,
templates, OrcaWave inputs and model YAML under `examples/`.

**Feature definition (normalised).**

- `section:<name>` — a top-level section, i.e. an object-type collection such as
  `Lines`, `VesselTypes`, `Winches` or `environment`.
- `key:<path>` — a key path with list indices collapsed (all line types share
  `LineTypes/OD`) and object-name maps collapsed to `*`.
- `value:<path>=<option>` — the option chosen for an enumerated solver setting
  (wave type, seabed model, dynamics solution method, contents method, …),
  case-normalised.

Object names, cross-references to named objects (vessel type, line type, clump
type, coordinate system), free text and numbers are never features. Numbers are
data; the inventory records which capabilities a file exercises.

**Results.**

| Measure | Value |
|---|---|
| YAML files scanned | 4,459 |
| Parse failures (not YAML-safe; listed in the JSON) | 51 |
| Model YAML files | 3,112 (OrcaFlex native 2,852; OrcaWave 144; spec 116) |
| Distinct features | 4,045 (key 3,548; value 306; section 191) |
| Features held by exactly one file | 665 (key 565; value 57; section 43) |
| Files that contribute a feature no other file has | 72 |
| Greedy feature cover (files that together carry every feature) | 167 |
| Model YAML among the 948 candidates | 39 |
| Candidates holding a unique feature | 4 |
| Features found only in candidate files | 68 |
| **Candidates kept in the repository for those features** | **7** (27,547,468 blob bytes) |

Files with unique features, by area:

| Area | Files | Unique features | Of which archive candidates |
|---|---|---|---|
| `docs/domains/orcaflex/installation/` | 13 | 155 | 0 |
| `docs/domains/orcaflex/library/` | 25 | 152 | 0 |
| `docs/domains/orcaflex/templates/` | 2 | 75 | 0 |
| `docs/domains/orcaflex/risers/` | 7 | 62 | 0 |
| `examples/prototypes/` | 1 | 40 | 0 |
| `docs/domains/orcaflex/jumper/` | 3 | 34 | 0 |
| `docs/domains/orcaflex/mooring/` | 1 | 24 | 1 |
| `examples/workflows/` | 3 | 19 | 0 |
| `docs/domains/orcaflex/examples/` | 2 | 18 | 0 |
| `docs/domains/orcawave/L00_validation_wamit/` | 3 | 18 | 0 |
| `docs/domains/orcaflex/subsea/` | 1 | 17 | 0 |
| `docs/domains/orcaflex/regional/` | 1 | 12 | 1 |
| `docs/domains/orcaflex/aqwa/` | 1 | 10 | 0 |
| `docs/domains/orcawave/examples/` | 2 | 7 | 0 |
| `docs/domains/orcaflex/reference/` | 1 | 5 | 1 |
| `docs/domains/orcaflex/support/` | 1 | 5 | 0 |
| `examples/domains/` | 1 | 5 | 0 |
| `docs/domains/orcaflex/training/` | 2 | 3 | 1 |
| `docs/domains/orcawave/L03_ship_benchmark/` | 1 | 3 | 0 |
| `docs/domains/orcaflex/structures/` | 1 | 1 | 0 |
| **Total** | **72** | **665** | **4** |

The 68 candidate-only features are mostly line contents settings
(`ContentsMethod`, `ContentsPressure`, `ContentsFlowRate`, axial contents
inertia), API RP 1111 line-type code-check factors, line VIV and pre-bend
settings, 3-D seabed interpolation, the ESDU wind spectrum, sea-state RAOs on
a vessel type and two spec structure classes (`reference`, `regional`). The
seven candidates that carry them are listed in the inventory JSON
(`candidates_kept_for_feature`) and flagged `keep_for_feature` in the CSV.

**How every unique feature stays reachable.**

1. The 7 `keep_for_feature` candidates stay in the repository as library
   inputs; they are not moved. 68 features would otherwise exist only in the
   archive.
2. 3,073 of the 3,112 model YAML files are not candidates (below the 1 MB
   gate, or referenced from `src/` or `tests/`) and stay where they are. The 32
   model YAML candidates proposed to move carry no feature that is absent from
   the files staying in the repository, and none of them holds a unique
   feature (all 4 unique-feature candidates are among the 7 kept).
3. `model-yaml-feature-inventory-2026-10-08.json` is the query index: for every
   feature it records how many files carry it, and for each file with a unique
   feature, which ones. A module that needs an example of a capability (for
   example a line with a uniform contents method) can look the feature up and load the
   file. When a later move takes model YAML out of the repository, the
   inventory shall be regenerated with blob paths so the same lookup resolves
   to `/mnt/ace/digitalmodel/blobs/...`.
4. The move step (below) shall re-run the inventory and refuse to move any file
   whose removal would leave a feature with zero in-repository holders unless
   the owner has approved that file with an archive loader in place.

## Move set after this revision

| Measure | Value |
|---|---|
| Candidates | 948 |
| Kept for model features | 7 |
| Proposed to move | 941 (893,450,532 blob bytes) |
| Unique blobs to store on `/mnt/ace` | 803 (808,116,602 bytes) |
| Duplicate copies not stored again | 138 (85,333,930 bytes) |

## How the move will run (later, after owner review)

1. **Owner review of the list.** The owner marks the rows to move. Rows with
   `keep_for_feature = True` stay. Rows with `ref_count_noncandidate > 0` are
   excluded by default unless the owner decides otherwise for that row.
2. **Refresh against `main`.** Re-run the generator (read-only) on the
   then-current `main` and drop any approved row whose `sha256_blob` changed,
   whose path no longer exists, that is now referenced from `src/` or `tests/`,
   or that is now flagged `keep_for_feature`.
3. **Copy, one blob per content.** For each unique `sha256_blob` among the
   approved rows, copy the git blob (`git cat-file blob <oid>`, not the
   working-tree file) to `/mnt/ace/digitalmodel/<blob_path>`. A blob already
   present with the right digest is not copied again.
4. **Verify.** Recompute SHA-256 of every stored blob and compare it with its
   name. Any mismatch stops the move.
5. **Record.** Write the blob map rows of the approved paths to
   `/mnt/ace/digitalmodel/MANIFEST.blob-map.tsv` (tab-separated: repo-relative
   path, `sha256_blob`, bytes, `blob_path`) and keep the same file in the
   repository, so every original path resolves to its blob from either side.
6. **Registry in the repository** (PR #2146 mechanism; there are no
   per-file stub files):
   - `data/inputs.yaml` — machine-readable pointer: archive root
     `/mnt/ace/digitalmodel`, store layout, base commit, per-class file count,
     bytes and extensions.
   - `DOCUMENT-MAP.md` — human-readable map: what moved, what stayed and why,
     how to retrieve and verify a file from the blob map.
   - `.gitignore` — archive-policy section so moved files are not re-added.
7. **Delete in git** only the verified, approved rows, in a separate PR that
   references this manifest. Tests that consumed a moved file shall skip with a
   message naming the archive path rather than fail.

No history rewrite is part of this plan.
