# Archive move plan — regenerated candidate manifest (2026-10-08)

Status: **review only.** This change adds a candidate list, a deduplicated blob
map and a model-YAML feature inventory. No file is deleted and nothing is
copied to the archive by this change.

Revision 2 (2026-10-08) applies the owner's round-4 decision on the first list
(1,414 files, 1.94 GB): reduce the list, deduplicate, use `/mnt/ace` as the
home of the required data with one copy per unique content, and keep every
unique feature of the model YAML files reachable by the modules.

Revision 3 (2026-10-08) applies the owner's round-5 decision C02: the
`src/`/`tests/` reference exclusion is narrowed to explicit file references. A
directory named in code no longer excludes everything under it. The branch was
also brought up to date with `main` (base `9aa78711`), so the rule-match count
moves from 1,424 to 1,471 independently of the rule change.

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
- **Excluded when `src/` or `tests/` explicitly reads the file (revision 3,
  owner decision C02).** All tracked text under `src/` and `tests/` (code,
  config and fixtures) is searched. A rule match is dropped on one of three
  kinds of evidence, recorded per file in the summary JSON
  (`evidence`, `matched`, `source`):
  - `path` — the file's repo path appears in the text, or a Python path
    expression resolves to it (string literal, `Path(__file__).resolve().parents[n] / "docs" / …`,
    `os.path.join`, `joinpath`, including constants imported from another
    module under `src/` or `tests/`);
  - `filename` — the exact filename appears as a whole token (not as part of a
    longer name);
  - `glob` — a `glob`, `rglob`, `iterdir`, `os.listdir`, `os.scandir`,
    `os.walk` or `glob.glob` call, or a glob literal in a config or fixture,
    is anchored on a directory that resolves statically to a repo path, and the
    pattern matches the file. Only the matching files are excluded.

  A bare directory mention (a directory constant that is never read by
  pattern, a comment, or prose naming a folder) excludes nothing. Patterns
  without a resolvable anchor (for example `glob("*.dat")` relative to an
  unknown directory) are not evidence. Revision 2 instead excluded every file
  under any directory of depth 4 or more named in code; that rule removed
  476 files (1,021,816,411 bytes) and is retired.

## Totals at base commit `9aa78711` (`main` at `1226c7b1` plus this branch)

1,471 tracked files match the extension/size rule; 521 of them
(837,553,641 blob bytes) are explicitly read by `src/` or `tests/` and are
removed from the list. 950 candidates remain.

Exclusions by evidence:

| Evidence | Files | Blob bytes |
|---|---|---|
| `path` (repo path in text, or a resolved path expression) | 13 | 29,615,363 |
| `filename` (exact filename token) | 183 | 180,571,891 |
| `glob` (resolved pattern) | 325 | 627,366,387 |
| **Total excluded** | **521** | **837,553,641** |

Two evidence groups are flagged for the owner (see *Sensitivity* below):

| Flagged group | Files | Blob bytes |
|---|---|---|
| `glob` from `docs/**/*.html` — `tests/legal/test_published_pages_have_no_internal_paths.py` scans every HTML page under `docs/` for internal paths (a publication-hygiene sweep, not a data input) | 314 | 608,060,258 |
| `filename` on a name shared by several tracked files (for example `benchmark_report.html` ×19, `spec.yml` ×12, `report.html` ×11, `index.html` ×4) — mostly output names that code writes | 142 | 146,665,185 |

Effect on the folders the revision-2 rule excluded wholesale:

| Folder | Moves (files, MiB) | Still excluded (files, MiB) | Remaining exclusion evidence |
|---|---|---|---|
| `docs/domains/orcawave/L00_validation_wamit/` | 18, 0.6 | 213, 396.0 | HTML sweep and generic `benchmark_*.html` names |
| `docs/domains/orcaflex/pipeline/` | 38, 64.1 | 18, 187.7 | HTML sweep, `index.html`, one explicit path (`…/24in_pipeline/monolithic/basefile/vessel_end_winch.yml`) |
| `docs/domains/orcaflex/risers/` | 43, 86.4 | 0, 0.0 | — |
| `docs/domains/orcawave/L01_aqwa_benchmark/` | 14, 0.5 | 34, 7.1 | explicit filenames and paths |

Remaining candidates:

| Top-level folder | Files | Blob bytes |
|---|---|---|
| `docs/` | 854 | 1,053,793,999 |
| `examples/` | 87 | 50,167,742 |
| `scripts/` | 2 | 1,165,388 |
| `config/` | 3 | 657,140 |
| `references/` | 4 | 236,723 |
| **Total** | **950** | **1,106,020,992** (on-disk 1,111,319,856) |

| Class | Files | Blob bytes |
|---|---|---|
| solver-inputs | 301 | 795,901,071 |
| documentation-images | 553 | 203,416,172 |
| office-documents | 50 | 90,185,593 |
| html-report-renders | 46 | 16,518,156 |

## Deduplication by blob SHA-256

| Measure | Value |
|---|---|
| Candidate files | 950 |
| Unique blobs (distinct content) | 821 |
| Duplicate groups (content held by 2 or more paths) | 87 |
| Duplicate files (copies beyond the first in each group) | 129 |
| Blob bytes, all paths | 1,106,020,992 |
| Blob bytes, one copy per unique blob | 1,016,494,231 |
| **Bytes saved by keeping one copy per unique content** | **89,526,761** (8.1 %) |

Duplicate copies by class: documentation images 101 files (50,663,132 bytes),
solver inputs 14 (24,693,532), office documents 14 (14,170,097). The largest groups are training CAD/mesh files held twice or three
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
| Model YAML among the 950 candidates | 84 |
| Candidates holding a unique feature | 0 |
| Features found only in candidate files | 23 |
| **Candidates kept in the repository for those features** | **4** (13,450,956 blob bytes) |

Files with unique features, by area:

| Area | Files | Unique features | Of which archive candidates |
|---|---|---|---|
| `docs/domains/orcaflex/installation/` | 13 | 155 | 0 |
| `docs/domains/orcaflex/library/` | 25 | 152 | 0 |
| `docs/domains/orcaflex/templates/` | 2 | 75 | 0 |
| `docs/domains/orcaflex/risers/` | 7 | 62 | 0 |
| `examples/prototypes/` | 1 | 40 | 0 |
| `docs/domains/orcaflex/jumper/` | 3 | 34 | 0 |
| `docs/domains/orcaflex/mooring/` | 1 | 24 | 0 |
| `examples/workflows/` | 3 | 19 | 0 |
| `docs/domains/orcaflex/examples/` | 2 | 18 | 0 |
| `docs/domains/orcawave/L00_validation_wamit/` | 3 | 18 | 0 |
| `docs/domains/orcaflex/subsea/` | 1 | 17 | 0 |
| `docs/domains/orcaflex/regional/` | 1 | 12 | 0 |
| `docs/domains/orcaflex/aqwa/` | 1 | 10 | 0 |
| `docs/domains/orcawave/examples/` | 2 | 7 | 0 |
| `docs/domains/orcaflex/reference/` | 1 | 5 | 0 |
| `docs/domains/orcaflex/support/` | 1 | 5 | 0 |
| `examples/domains/` | 1 | 5 | 0 |
| `docs/domains/orcaflex/training/` | 2 | 3 | 0 |
| `docs/domains/orcawave/L03_ship_benchmark/` | 1 | 3 | 0 |
| `docs/domains/orcaflex/structures/` | 1 | 1 | 0 |
| **Total** | **72** | **665** | **0** |

The 23 candidate-only features are line contents settings
(`ContentsMethod` with the `uniform` option, `ContentsDensity`,
`ContentsPressure`, `ContentsFlowRate`, `ContentsTemperature`, axial contents
inertia), a variable-data bending connection stiffness table, line connection
bending stiffness, decoupled lateral/axial seabed friction, the ESDU wind
spectrum with its latitude, sea-state RAOs on a vessel type, and drawing
settings (pens, node discs). No candidate holds a feature unique to one file;
these 23 are shared among candidates and absent from every file staying in the
repository. The four candidates that together carry them are listed in the
inventory JSON (`candidates_kept_for_feature`) and flagged `keep_for_feature`
in the CSV. Compared with revision 2, the four unique-feature model files
(spec structure classes `reference`, `regional`, and the training and mooring
specs) are no longer candidates: each is named `spec.yml`, a filename that
appears in `src/` (see *Sensitivity*).

**How every unique feature stays reachable.**

1. The 4 `keep_for_feature` candidates stay in the repository as library
   inputs; they are not moved. 23 features would otherwise exist only in the
   archive.
2. 3,028 of the 3,112 model YAML files are not candidates (below the 1 MB
   gate, or explicitly read by `src/` or `tests/`) and stay where they are. The
   80 model YAML candidates proposed to move carry no feature that is absent
   from the files staying in the repository, and none of them holds a unique
   feature.
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
| Candidates | 950 |
| Kept for model features | 4 (13,450,956 blob bytes) |
| Proposed to move | 946 (1,092,570,036 blob bytes) |
| Unique blobs to store on `/mnt/ace` | 817 (1,003,043,275 bytes) |
| Duplicate copies not stored again | 129 (89,526,761 bytes) |

Comparison with revision 2:

| Measure | Rev 2 (dir rule, base `c4563b11`) | Rev 2 rule on base `9aa78711` | Rev 3 (explicit refs, base `9aa78711`) |
|---|---|---|---|
| Rule matches | 1,424 | 1,471 | 1,471 |
| Excluded by reference | 476 files, 1,021,816,411 B | 477 files, 1,021,839,065 B | 521 files, 837,553,641 B |
| Candidates | 948 files, 920,998,000 B | 994 files, 921,735,568 B | 950 files, 1,106,020,992 B |
| Unique blobs / dedup saving | 810 / 85,333,930 B | 856 / 85,333,930 B | 821 / 89,526,761 B |
| Kept for model features | 7 files, 27,547,468 B | 7 files, 27,547,468 B | 4 files, 13,450,956 B |
| Move set | 941 files, 893,450,532 B | 987 files, 894,188,100 B | 946 files, 1,092,570,036 B |
| Unique blobs to store | 803, 808,116,602 B | 849, 808,854,170 B | 817, 1,003,043,275 B |

The file count excluded rises (477 → 521) while the bytes fall (1.02 GB →
0.84 GB): the exact-filename rule now counts any filename, not only names
unique among tracked files, so many small generic outputs are excluded, while
the large directory trees are no longer excluded wholesale.

## Sensitivity (owner decision required)

Two evidence groups follow the C02 rule literally but do not show that code
needs the file as an input:

1. **HTML sweep.** `tests/legal/test_published_pages_have_no_internal_paths.py`
   reads `docs/**/*.html` to check published pages for internal paths. Moving
   a page removes it from the check; it does not break the check. These 314
   files (608,060,258 B) include 571 MiB of the 584 MiB still excluded from the
   OrcaWave validation and OrcaFlex pipeline folders.
2. **Shared filenames.** 142 files (146,665,185 B) are excluded only because
   their filename, shared by several tracked files, appears in `src/` or
   `tests/` — mostly names that code writes (`benchmark_report.html`,
   `benchmark_*.html`, `report.html`, `index.html`) and `spec.yml`.

If the owner treats both groups as not referenced, the move set grows by up to
456 files and 754,725,443 B before deduplication and feature keep (the four
unique-feature `spec.yml` files would then return as candidates and be kept
for their features). The generator records the evidence kind, matched pattern
and source file for every exclusion, so either group can be released without
re-deriving the rule.

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
