# CAD/CAM File Inventory — ace share (`/mnt/ace`)

**Epic:** [#1004](https://github.com/vamseeachanta/digitalmodel/issues/1004) · **Issue:** [#1005](https://github.com/vamseeachanta/digitalmodel/issues/1005) (1/5)
**Host:** ace-linux-1 · **Scanned path:** native local disk `/mnt/ace` (no network share) · **Scan date:** 2026-06-24

This folder holds the discovery output for the CAD/CAM automation initiative. This file (`README.md`) is the
aggregate inventory; the per-file manifest is `cad-file-manifest-deidentified.csv.gz`.

> **Methodology.** This work is an instance of the [raw-to-knowledge-playbook](https://github.com/vamseeachanta/raw-to-knowledge-playbook)
> — *extract deterministically, verify, trust nothing by default*. The binary-CAD/B-rep lane it embodies is written up
> as that playbook's [doc 21 (CAD geometry & B-rep)](https://github.com/vamseeachanta/raw-to-knowledge-playbook/blob/main/docs/21-cad-and-brep-geometry.md):
> license-locked native read (the [#1006](https://github.com/vamseeachanta/digitalmodel/issues/1006) extraction/run split), header/version detection,
> the round-trip invariant oracle (the [pilot](./pilot-tier0-results.md)), and the de-identify-before-first-commit rule (GP-53).

> **Privacy — this repo is PUBLIC.** The committed manifest is **de-identified**: the raw `path` column (which
> embeds a personal name and client/field linkage) is replaced by `path_sha1`, and the two external-company top
> folders are relabelled (`epc-partner`, `eng-partner`). All aggregate columns are intact. The **full raw-path
> manifest is retained only in the private ace-linux-1 context**, not committed. `build-manifest.py` produces the
> raw manifest; `deidentify-manifest.py` produces the committed one.

---

## TL;DR — what's on the share

- **525,152 CAD/CAM-related files, ~5.4 TB**, found by an unbounded `find` over `/mnt/ace`.
- **One folder dominates everything.** A single raw flush-drive dump —
  `docs/disciplines/knowledge_skills/projects/ri/00_inbox/` — holds **416,283 files (79%)**: almost the entire
  Autodesk-Inventor population (133k `.ipt` + 51k `.iam`) and the bulk of AutoCAD (227k `.dwg`). It is an
  *un-curated inbox*, not an active model tree — treat its counts as a backlog, not as 416k hand-built models.
- **OrcaFlex is the heaviest real engineering asset by volume:** 10,204 `.sim` files ≈ **5.2 TB** — the largest
  single data mass on the share by size.
- **SolidWorks is the dominant *curated* native-CAD ecosystem** outside the inbox dump: ~59k `.sldprt`/`.sldasm`
  in `docs/disciplines/{misc,drilling}` and the subsea project repos.
- **CAM / toolpath data is essentially absent.** `.mcam`=0, `.cnc`=0, `.tap`=0, `.gcode`=0, and all 10 `.nc`
  files are **NetCDF hydrodynamics output** (WEC-Sim/scipy), *not* CNC G-code. The share is **CAD-rich, CAM-empty.**
- **Neutral exchange formats exist but are thin:** STEP 1,759 + IGES 564 + Parasolid (`.x_t`/`.x_b`) 815 ≈ **3,138 files**.
  These are the license-free automation entry points (see [#1006](https://github.com/vamseeachanta/digitalmodel/issues/1006)).

---

## Method & caveats

- Command: a single `find /mnt/ace -type f` filtered (case-insensitive) to the 26 target extensions, printing
  `size · mtime · path`. The exact extension set and classification logic live in `build-manifest.py` (committed
  alongside, reproducible).
- **Extensions:** `.step .stp .iges .igs .sldprt .sldasm .dwg .dxf .x_t .x_b .ipt .iam .catpart .prt .f3d .f3z
  .mcam .nc .cnc .tap .gcode .stl .3mf` plus engineering models `.sim .dat`.
- **`.dat` and `.nc` are ambiguous** and flagged as such in the manifest (`category` = `*-ambiguous`): `.dat` is
  OrcaFlex input *or* generic data; `.nc` is NetCDF *or* (in principle) G-code — here it is 100% NetCDF.
- **Dependency noise excluded from "signal" counts:** 1,115 files inside `.venv`/`site-packages` (scipy test
  `.dat`/`.nc`) are tagged `is_dependency=True` and dropped from the deduped headline.
- **SolidWorks lock/temp files (found in #1008):** 8,839 of the 60,036 `.sldprt`/`.sldasm` matches are `~$…`
  SolidWorks lock/temp files (8,684 parts + 155 assemblies), **not models** — real SW model count ≈ **51,200**.
  They remain in the manifest (they were on disk); filter `path` for a leading `~$` to exclude them.
- **Manifest is gzipped** (`cad-file-manifest-deidentified.csv.gz`, ~9 MB) to keep the repo lean. Decompress with
  `zcat cad-file-manifest-deidentified.csv.gz`. Columns:
  `format, ecosystem, category, size_bytes, mtime, top_folder, project_group, is_preexisting, is_dependency, is_inbox_dump, path_sha1`
  (`path_sha1` = first 16 hex of `sha1(full_path)` — preserves per-file uniqueness/dedup without exposing the path).

### `.preexisting-before-repo-move-*` deduplication

Six top-level folders have an archived `*.preexisting-before-repo-move-<timestamp>` snapshot taken during a
repo move. Where an **active twin still exists**, the snapshot is treated as a duplicate archive and removed from
the deduped counts. Dedup is by **folder identity**, not content hash (a per-file hash dedup is out of scope for
the inventory; flagged for #1007).

| Archived folder | Active twin present? | Files in archive | Dedup action |
|---|---|---|---|
| `client_projects.preexisting-…-064502` | yes (`client_projects`) | 4,522 | **drop** as duplicate |
| `acma-projects.preexisting-…-075928` | yes (`acma-projects`) | 3,230 | **drop** as duplicate |
| `eng-partner.preexisting-…-064502` | yes (`eng-partner`) | 502 | **drop** as duplicate |
| `seanation.preexisting-…-064502` | yes (`seanation`) | 481 | **drop** as duplicate |
| `epc-partner.preexisting-…-064502` | **no live twin** | 317 | **keep** — only copy |
| `rock-oil-field.preexisting-…-064502` | **no live twin** | 424 | **keep** — only copy |

Removed by dedup: **8,735** archived duplicates + **1,115** dependency files = **9,850**.
**Signal total = 515,302 files.**

---

## Aggregate 1 — by format (all 525,152 rows)

| Format | Ecosystem | Files | Size |
|---|---|--:|--:|
| `.dwg`  | AutoCAD | 234,701 | 124.2 GB |
| `.ipt`  | Inventor (part) | 133,405 | 40.3 GB |
| `.sldprt` | SolidWorks (part) | 55,318 | 20.2 GB |
| `.iam`  | Inventor (assembly) | 51,065 | 9.2 GB |
| `.dat`  | OrcaFlex / generic *(ambiguous)* | 29,644 | 26.9 GB |
| `.sim`  | OrcaFlex | 10,204 | **5.2 TB** |
| `.sldasm` | SolidWorks (assembly) | 4,718 | 8.4 GB |
| `.dxf`  | AutoCAD exchange | 2,624 | 1.5 GB |
| `.stp`  | STEP (neutral) | 1,500 | 2.3 GB |
| `.x_t`  | Parasolid (neutral) | 815 | 2.1 GB |
| `.igs`  | IGES (neutral) | 389 | 3.5 GB |
| `.step` | STEP (neutral) | 259 | 4.3 GB |
| `.prt`  | NX / generic | 256 | 27.9 MB |
| `.iges` | IGES (neutral) | 175 | 3.1 GB |
| `.stl`  | Mesh / 3D-print | 69 | 528 MB |
| `.nc`   | NetCDF *(not G-code)* | 10 | 12.2 MB |
| `.catpart` `.f3d` `.f3z` `.mcam` `.cnc` `.tap` `.gcode` `.x_b` `.3mf` | — | **0** | — |

## Aggregate 2 — by software ecosystem (all rows)

| Ecosystem | Files | Size |
|---|--:|--:|
| AutoCAD (`.dwg`+`.dxf`) | 237,325 | 125.7 GB |
| Autodesk Inventor (`.ipt`+`.iam`) | 184,470 | 49.5 GB |
| SolidWorks (`.sldprt`+`.sldasm`) | 60,036 | 28.7 GB |
| OrcaFlex (`.sim`) | 10,204 | 5.2 TB |
| OrcaFlex / generic data (`.dat`, ambiguous) | 29,644 | 26.9 GB |
| STEP (neutral) | 1,759 | 6.6 GB |
| Parasolid (neutral) | 815 | 2.1 GB |
| IGES (neutral) | 564 | 6.6 GB |
| NX / generic (`.prt`) | 256 | 27.9 MB |
| Mesh / 3D-print (`.stl`) | 69 | 528 MB |
| Mastercam / CNC G-code | **0** | — |

## Aggregate 3 — by top-level project folder (all rows)

| Top folder under `/mnt/ace` | Files | Size | Note |
|---|--:|--:|---|
| `docs` | 473,552 | 2.0 TB | **79% is the `ri/00_inbox` flush-drive dump** (416,283 files) |
| `digitalmodel` | 37,422 | 19.7 GB | the repo itself — mostly `.dat`/test fixtures + benchmark models |
| `client_projects.preexisting-…` | 4,522 | 59.1 GB | archived twin of `client_projects` → deduped |
| `acma-projects.preexisting-…` | 3,230 | 3.4 TB | archived twin of `acma-projects` (OrcaFlex-heavy) → deduped |
| `OGManufacturing` | 1,115 | 30.7 MB | **all `.venv`/site-packages noise** (not real CAD) |
| `data` | 944 | 706 MB | |
| `client_projects` | 815 | 170 MB | **active** client engineering CAD |
| `eng-partner.preexisting-…` | 502 | 750 MB | deduped |
| `seanation.preexisting-…` | 481 | 87 MB | deduped |
| `acma-projects` | 474 | 3.3 GB | **active** |
| `rock-oil-field.preexisting-…` | 424 | 1.1 GB | no live twin → kept |
| `gdrive` | 407 | 719 MB | mirrored Google-Drive corpus |
| `epc-partner.preexisting-…` | 317 | 1.5 GB | no live twin → kept |
| `aceengineer-admin` | 264 | 6.8 MB | |
| `WEC-Sim` | 258 | 88 MB | open-source tool (incl. the 10 NetCDF `.nc`) |
| `seanation` | 225 | 269 MB | **active** subsea project |
| `O&G-Standards` | 31 | 5.7 GB | |
| `eng-partner` `acma-codes` `MoorDyn` `MoorPy` `openfast` `capytaine` `gmsh` … | <50 each | — | sim tools / minor |

## Aggregate 4 — "signal" set, **excluding** the `ri/00_inbox` dump

This is the curated engineering CAD that actually warrants automation analysis (deps + archived dups dropped,
416k-file inbox excluded): **99,019 files.**

| Ecosystem | Files | Size |
|---|--:|--:|
| SolidWorks | 59,119 | 23.8 GB |
| OrcaFlex / generic data (`.dat`) | 26,017 | 16.3 GB |
| AutoCAD | 6,600 | 11.2 GB |
| OrcaFlex (`.sim`) | 5,958 | 1.8 TB |
| Parasolid (neutral) | 815 | 2.1 GB |
| STEP (neutral) | 233 | 2.8 GB |
| AutoCAD exchange (`.dxf`) | 103 | 157 MB |
| IGES (neutral) | 77 | 1.3 GB |
| Mesh / 3D-print (`.stl`) | 69 | 528 MB |
| NX / generic (`.prt`) | 20 | 436 KB |
| NetCDF (`.nc`) | 7 | 12.2 MB |

> Most curated SolidWorks lives under `docs/disciplines/{misc,drilling}`; the subsea project repos
> (`client_projects`, `acma-projects`, `seanation`, `epc-partner`, `rock-oil-field`, `eng-partner`) are OrcaFlex-dominant.
> Folder→domain→client mapping is in [`project-domain-map.md`](./project-domain-map.md) (#1007).

---

## What this means for the CAD/CAM automation initiative

1. **The automatable native-CAD surface is SolidWorks + AutoCAD + Inventor**, not CAM. There is no existing
   toolpath/NC corpus to mine — Phase-3 (shop-floor/CAM) automation would start from zero data here.
2. **The `ri/00_inbox` dump is itself an automation target**: 416k unsorted Inventor/AutoCAD files = a sorting,
   de-duplication, BOM-extraction and naming-normalization problem before it is a modeling problem.
3. **OrcaFlex (Python-scriptable) is the lowest-friction automation win** — large file base + a documented public
   API + already-present in-house `digitalmodel` tooling.
4. **Neutral formats (STEP/IGES/Parasolid, ~3,138 files)** are the license-free path to headless geometry
   automation; quantified for feasibility in [`ecosystem-and-automation-surface.md`](./ecosystem-and-automation-surface.md) (#1006).

See [`automation-opportunity-report.md`](./automation-opportunity-report.md) (#1009) for the ranked, phase-mapped
recommendations.
