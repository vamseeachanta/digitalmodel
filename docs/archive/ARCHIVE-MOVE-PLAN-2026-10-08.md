# Archive move plan — regenerated candidate manifest (2026-10-08)

**R01 fix-up 2 supersedes the revision-4 selection below.** The figures and decisions in
this document describe historical revisions. The current selection and review
record are [R01-archive-manifest-review.html](R01-archive-manifest-review.html)
and the regenerated summary JSON. Candidate rows with either `keep_for_feature`
or `needs_human_check` stay in the repository. The blob map is an inventory,
not authorization to remove every mapped path. Removal [PR 2304](https://github.com/vamseeachanta/digitalmodel/pull/2304) is held until
regeneration, Claude review and owner confirmation of the revised selection.

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

Revision 4 (2026-10-08) applies the owner's round-6 decision D01: neither of
the two groups flagged in revision 3 counts as a reference. A pattern in a
hygiene sweep (the published-page path scan) is not consumer evidence, and a
filename shared by more than one tracked path counts only when path-qualified.
The branch was brought up to date with `main` (base `74d26949`). The move set
is copied to the archive store and removed from git in a separate change; this
change remains the review record.

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
| `archive-candidates-2026-10-08.holds.json` | Full consumer-to-held-candidate mapping; distinct patterns/reasons and counts are stored once per consumer in the summary |
| `archive-blob-map-2026-10-08.csv` | Every candidate repo path mapped to its single content-addressed blob in the archive store |
| `model-yaml-feature-inventory-2026-10-08.json` | Feature definition, feature-to-file counts, files with unique features, feature cover, candidate-only features and the candidates kept for them |

All files are produced by `scripts/maintenance/archive_candidates.py`
(read-only; tests in `tests/maintenance/test_archive_candidates.py`):

```
python scripts/maintenance/archive_candidates.py <checkout> <pr2146_deleted_list.txt> \
    docs/archive/archive-candidates-2026-10-08.csv \
    docs/archive/archive-candidates-2026-10-08.summary.json \
    --blob-map docs/archive/archive-blob-map-2026-10-08.csv \
    --features docs/archive/model-yaml-feature-inventory-2026-10-08.json \
    --hold-detail docs/archive/archive-candidates-2026-10-08.holds.json
```

CSV columns:

- `path` — repo-relative path at the scanned revision.
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
  candidates. Since revision 3 the committed manifest files in `docs/archive/`
  name every candidate, so this count is at least 1 for every row and is no
  longer a selection signal; R01 selection uses proven tracked Python/YAML consumer evidence.
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

- `needs_human_check` — `True` when the candidate is held for consumer uncertainty or parse failure.
- `hold_consumers` — JSON array of consumer IDs, resolved in `human_holds_by_consumer` in the summary and the separate hold detail.

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
    longer name) and no other tracked file has that filename (revision 4). A
    filename shared by several tracked files counts only when path-qualified:
    a trailing path of two or more segments that ends exactly one tracked file
    appears in the text (recorded as `path` evidence with the qualifier as
    `matched`);
  - `glob` — a `glob`, `rglob`, `iterdir`, `os.listdir`, `os.scandir`,
    `os.walk` or `glob.glob` call, or a glob literal in a config or fixture,
    is anchored on a directory that resolves statically to a repo path, and the
    pattern matches the file. Only the matching files are excluded. Patterns in
    a hygiene sweep listed in `HYGIENE_SCANS` (revision 4:
    `tests/legal/test_published_pages_have_no_internal_paths.py`, which scans
    every published page for absolute paths) are not evidence; pages that sweep
    names explicitly still count.

  A bare directory mention (a directory constant that is never read by
  pattern, a comment, or prose naming a folder) excludes nothing. Patterns
  without a resolvable anchor (for example `glob("*.dat")` relative to an
  unknown directory) are not evidence. Revision 2 instead excluded every file
  under any directory of depth 4 or more named in code; that rule removed
  476 files (1,021,816,411 bytes) and is retired.

## Totals at base commit `74d26949` (`main` at `a35bb887` plus this branch)

1,471 tracked files match the extension/size rule; 66 of them
(85,467,617 blob bytes) are explicitly read by `src/` or `tests/` and are
removed from the list. 1,405 candidates remain.

Exclusions by evidence:

| Evidence | Files | Blob bytes |
|---|---|---|
| `path` (repo path in text, a resolved path expression, or a path-qualified shared filename) | 13 | 29,615,363 |
| `filename` (exact filename token, filename unique among tracked files) | 41 | 33,906,706 |
| `glob` (resolved pattern outside a hygiene sweep) | 12 | 21,945,548 |
| **Total excluded** | **66** | **85,467,617** |

Revision 3 excluded 521 files (837,553,641 B). The two groups released by D01
were the published-page sweep (314 files, 608,060,258 B) and filenames shared
by several tracked files (142 files, 146,665,185 B).

Effect on the folders the revision-2 rule excluded wholesale:

| Folder | Moves (files, MiB) | Still excluded (files, MiB) | Remaining exclusion evidence |
|---|---|---|---|
| `docs/domains/orcawave/L00_validation_wamit/` | 231, 396.6 | 0, 0.0 | — |
| `docs/domains/orcaflex/pipeline/` | 55, 250.6 | 1, 1.2 | one explicit path (`…/24in_pipeline/monolithic/basefile/vessel_end_winch.yml`) |
| `docs/domains/orcaflex/risers/` | 43, 86.4 | 0, 0.0 | — |
| `docs/domains/orcawave/L01_aqwa_benchmark/` | 47, 5.3 | 1, 2.2 | one explicit path (`orcawave_001_ship_raos_rev2.xlsx`) |

Remaining candidates:

| Top-level folder | Files | Blob bytes |
|---|---|---|
| `docs/` | 1,309 | 1,805,880,023 |
| `examples/` | 87 | 50,167,742 |
| `scripts/` | 2 | 1,165,388 |
| `config/` | 3 | 657,140 |
| `references/` | 4 | 236,723 |
| **Total** | **1,405** | **1,858,107,016** (on-disk 1,866,839,555) |

| Class | Files | Blob bytes |
|---|---|---|
| solver-inputs | 332 | 888,196,814 |
| html-report-renders | 470 | 676,308,437 |
| documentation-images | 553 | 203,416,172 |
| office-documents | 50 | 90,185,593 |

## Deduplication by blob SHA-256

| Measure | Value |
|---|---|
| Candidate files | 1,405 |
| Unique blobs (distinct content) | 1,259 |
| Duplicate groups (content held by 2 or more paths) | 95 |
| Duplicate files (copies beyond the first in each group) | 146 |
| Blob bytes, all paths | 1,858,107,016 |
| Blob bytes, one copy per unique blob | 1,749,943,555 |
| **Bytes saved by keeping one copy per unique content** | **108,163,461** (5.8 %) |

The largest groups are training CAD/mesh files held twice or three
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
| Model YAML among the 1,405 candidates | 109 |
| Candidates holding a unique feature | 6 |
| Features found only in candidate files | 124 |
| **Candidates kept in the repository for those features** | **11** (42,036,638 blob bytes) |

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

The 124 candidate-only features include line contents settings
(`ContentsMethod` with the `uniform` option, `ContentsDensity`,
`ContentsPressure`, `ContentsFlowRate`, `ContentsTemperature`, axial contents
inertia), bending connection stiffness, decoupled lateral/axial seabed
friction, the ESDU and full-field wind options, sea-state RAOs on a vessel
type, 3D seabed data, multibody added mass and damping, Rayleigh damping,
turbine controller settings, API RP 1111 line-type checks, the `reference` and
`regional` spec structure classes, and drawing settings. They are absent from
every file staying in the repository. The 11 candidates that together carry
them are listed in the inventory JSON (`candidates_kept_for_feature`) and
flagged `keep_for_feature` in the CSV: the four model files kept in revision 3
plus seven `spec.yml` files that revision 3 excluded on the shared filename
`spec.yml` and that D01 returns to the candidate list (six of them hold a
feature unique to one file).

**How every unique feature stays reachable.**

1. The 11 `keep_for_feature` candidates stay in the repository as library
   inputs; they are not moved. 124 features would otherwise exist only in the
   archive.
2. 3,003 of the 3,112 model YAML files are not candidates (below the 1 MB
   gate, or explicitly read by `src/` or `tests/`) and stay where they are. The
   98 model YAML candidates proposed to move carry no feature that is absent
   from the files staying in the repository.
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
| Candidates | 1,405 |
| Kept for model features | 11 (42,036,638 blob bytes) |
| Proposed to move | 1,394 (1,816,070,378 blob bytes) |
| Unique blobs to store on `/mnt/ace` | 1,249 (1,715,309,069 bytes) |
| Duplicate copies not stored again | 145 (100,761,309 bytes) |

Comparison with earlier revisions:

| Measure | Rev 2 (dir rule, base `c4563b11`) | Rev 3 (explicit refs, base `9aa78711`) | Rev 4 (D01, base `74d26949`) |
|---|---|---|---|
| Rule matches | 1,424 | 1,471 | 1,471 |
| Excluded by reference | 476 files, 1,021,816,411 B | 521 files, 837,553,641 B | 66 files, 85,467,617 B |
| Candidates | 948 files, 920,998,000 B | 950 files, 1,106,020,992 B | 1,405 files, 1,858,107,016 B |
| Unique blobs / dedup saving | 810 / 85,333,930 B | 821 / 89,526,761 B | 1,259 / 108,163,461 B |
| Kept for model features | 7 files, 27,547,468 B | 4 files, 13,450,956 B | 11 files, 42,036,638 B |
| Move set | 941 files, 893,450,532 B | 946 files, 1,092,570,036 B | 1,394 files, 1,816,070,378 B |
| Unique blobs to store | 803, 808,116,602 B | 817, 1,003,043,275 B | 1,249, 1,715,309,069 B |

## Sensitivity (resolved by owner decision D01)

Revision 3 flagged two evidence groups that followed the C02 rule literally
but did not show that code needs the file as an input: the published-page
sweep in `tests/legal/test_published_pages_have_no_internal_paths.py`
(314 files) and filenames shared by several tracked files, mostly names that
code writes (`benchmark_*.html`, `report.html`, `index.html`, `spec.yml`;
142 files). D01 (2026-10-08) decided that neither counts as a reference;
revision 4 implements that decision as described under *Selection rule*.

## How the move runs (copy and git removal in a separate change)

1. **Owner review of the list.** R01 requires review of the regenerated move
   set. Both `keep_for_feature = True` and `needs_human_check = True` rows stay.
   The revision-4 D01 selection is superseded; PR #2304 remains held.
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
   path, `sha256_blob`, bytes, `blob_path`) and one line per stored blob to
   `/mnt/ace/digitalmodel/MANIFEST.sha256.tsv` (`sha256`, bytes, `blob_path`), and
   keep the same files in the repository, so every original path resolves to
   its blob from either side.
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
