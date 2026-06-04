# Relocated Data — Heavy-Binary Index & Canonical Share Paths

## What & Why

On **2026-03-24** this repository was slimmed from **~17 GB to ~4.7 GB** working
tree. Heavy binaries were **relocated out of git** onto the team's *ace* data
share, recorded authoritatively in the share-side `RELOCATION-LOG.md`. YAML
configs, input decks, and code **stayed in-repo as the source of truth** — only
large, regenerable, or archival binaries moved.

| What moved | Size | Count | New home (share-relative) |
|---|---|---|---|
| PDFs (literature) | 2.3 GB | 880 files | `digitalmodel/docs/<domain>/literature/` |
| Excel workbooks | 57 MB | 117 files | `docs/<domain>/data/` + `_project-data/` |
| OrcaFlex `.sim` | 864 MB | 77 files | mirrors repo structure under the share |
| Heavy binaries (`.dxf`, `.gz`, QTF-class) | — | — | `docs/<domain>/data/` |
| `htmlcov/` coverage | — | — | **deleted** (regenerable, not relocated) |

## Where The Data Lives — Canonical Paths

All relocated-data paths below are **share-relative**. Prepend your machine's
path prefix to resolve them. This table is the key to the local-vs-remote-mount
confusion.

| Machine | Path prefix |
|---|---|
| ace-linux-1 (file server) | `/mnt/ace/` |
| ace-linux-2 | `/mnt/remote/ace-linux-1/ace/` (NFS) |

> **ace-linux-2 has no `/mnt/ace` symlink** — this is by design. Any hardcoded
> `/mnt/ace/...` path will fail on ace-linux-2; use the NFS prefix above.

**Example:** share-relative `digitalmodel/docs/openfoam/data/...` resolves to
`/mnt/ace/digitalmodel/docs/openfoam/data/...` on ace-linux-1 and
`/mnt/remote/ace-linux-1/ace/digitalmodel/docs/openfoam/data/...` on ace-linux-2.

**Full machine-readable index:** `digitalmodel/INDEX.md` on the share, plus the
share-root `INDEX.md`.

## Known Key Datasets (share-relative)

| Dataset | Path | Notes |
|---|---|---|
| OpenFOAM Wigley hull | `digitalmodel/docs/openfoam/data/case_studies/wigleHull_LTS/wigleyHull_LTS.tar.gz` | ~70 MB |
| QGIS bathymetry | `digitalmodel/docs/qgis/data/project1/inputs/combined_bathymetry.dxf` | ~38 MB |
| Mooring-tension `.sim` | `digitalmodel/tests/solvers/orcaflex/mooring-tension-iteration/fsts-l015-test-cases/run_files/sim/*.sim` | OrcaFlex run files |
| AQWA / OrcaFlex / OrcaWave data trees | `digitalmodel/docs/{aqwa,orcaflex,orcawave}/...` | per-domain `data/` |
| Reference snapshots | `digitalmodel/tests/{baseline_results,optimized_results}` | **NOT regenerable** |

> The AQWA multibody **QTF set remains tracked IN this repo** at
> `docs/domains/orcaflex/aqwa/to_orcaflex/multibody_test1/input/` — it was *not*
> relocated.

## History & Provenance

- **Pre-slim history** was preserved on an `archive/pre-slim` branch pushed to
  GitHub on 2026-03-24. That branch is **currently MISSING from origin** —
  flagged for investigation.
- **Stale local branches** — four July-2025 branches
  (`docs-organization-ai-friendly`, `feature-36_raos`,
  `feature/82_parallel_process`, `git-repository-optimization`) were assessed on
  2026-06-04 and **deleted without archive** per owner decision. Every theme was
  superseded on `main`: git-tooling scripts byte-identical; RAO and parallel-OPP
  work evolved in `marine_ops` / signal-analysis; docs migration landed. Their
  only unique content was July-2025 revisions of `fsts_lngc` pretension `.sim`
  files, judged expendable (current sets live on the share).
- **Guard-rail rule:** never re-commit relocated binaries to this repo.
  `.gitattributes` guard-rails are in place to prevent this.
