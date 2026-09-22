# digitalmodel — Document Map (2026-09-22 slim)

On 2026-09-22 the repository was slimmed from ~2.87 GB to ~0.30 GB at HEAD.
**2,299 files (~2.49 GB)** of bulky engineering artifacts were moved out of git
to canonical archive storage. Nothing was deleted: every file is byte-verified
in the archive, and a complete pre-slim backup bundle exists.

> Note: the archive manifest lists 2,472 files. 173 of those were restored to
> git the same day: 153 `docs/api/**` files (the generated API documentation
> site, 33 MB) required by CI's "Generated HTML freshness" check
> (`scripts/check_generated_html.py`), 5 `assets/logo/**` files read by the
> docs generators as build inputs, and 15 `tests/fixtures/**` solver fixtures
> (33.7 MB) required by the domain test suite. 2,299 files remain moved out of
> git.

## Archive location

```
ace-linux-1:/mnt/ace/digitalmodel/
```

Paths under the archive root **mirror the repo-relative paths** from the slim
commit's parent (`7e71d6b26eb863e95ae3f0ec9957b081d85dea28`). To restore a file,
copy it back to the same relative path in a checkout.

- Full per-file SHA-256 manifest: `ace-linux-1:/mnt/ace/digitalmodel/MANIFEST.sha256.tsv`
  (tab-separated: `sha256`, `bytes`, `repo-relative path`; 2,472 entries —
  173 entries were restored to git, 2,299 remain archived)
- Machine-readable pointers: `data/inputs.yaml` (in this repo)
- Pre-slim backup (verified complete history): `ace-linux-1:/mnt/ace/_transfer/digitalmodel-pre-slim-20260922.bundle`

## What moved

| Class | Files | Bytes | Contents |
|---|---|---|---|
| solver-inputs | 479 | ~1.35 GB | `.dat` / `.lis` / `.qtf` AQWA inputs, `.owr` OrcaWave results, `.sim` OrcaFlex models, `.igs`/`.stl`/`.dwg`/`.dxf` CAD & mesh, large vessel-RAO `.yml`/`.yaml` (>1 MB), `.csv`, `.engd`/`.scdoc`, `.gz` (15 `tests/fixtures/**` solver fixtures restored to git for CI) |
| html-report-renders | 474 | ~693 MB | Rendered HTML analysis reports (regenerable from code + inputs) |
| documentation-images | 1,282 | ~344 MB | `.png`/`.jpg`/`.jfif`/`.gif`/`.svg` illustrations under `docs/` and `examples/` (5 `assets/logo/**` build inputs were restored to git) |
| office-documents | 64 | ~94 MB | `.pptx`/`.ppt`/`.docx`/`.pdf` engineering docs archived for reference (6 `tests/fixtures/solver/*.xlsx` restored to git for CI) |
| **Total moved** | **2,299** | **~2.49 GB** | Mutually exclusive classes; verified against the archive manifest |

The 153 `docs/api/**` files (88 HTML + 58 PDF + 7 images, 33 MB) in the manifest
were restored to git because they are the generated API documentation site,
not analysis report renders — CI's "Generated HTML freshness" check requires
them committed. The 5 `assets/logo/**` files were also restored: the docs
generators read them as build inputs.

The extension rules that produced this set are recorded in the slim plan
(`workspace-hub` issue #3880); the 399 files over 1 MB are individually
inventoried there.

## What stayed (and why)

- **Runtime test fixtures** — small files tests load directly, kept in git:
  `tests/fixtures/reporting/*.report.snapshot.html`,
  `tests/hydrodynamics/diffraction/fixtures/golden/*.html`,
  `tests/hydrodynamics/bemrosetta/fixtures/*`,
  `tests/solvers/orcaflex/modular_generator/goldens/basefile/fixtures/base_model.dat`.
- **Solver tests skip cleanly** when archived fixtures are absent:
  - `tests/hydrodynamics/diffraction/conftest.py` — `l00/l01_owr/xlsx_path`
    fixtures `pytest.skip` with the archive location in the message.
  - `tests/hydrodynamics/diffraction/test_solver_fixtures.py` — asserts the
    `.gitignore` archive policy instead of the old committable-fixture rule.
  - `tests/solvers/orcaflex/reporting/test_fixture_snapshot.py` —
    `test_mooring_with_raos_snapshot...` is an explicit skip naming the archive path.
  - `tests/.../fsts-l015-test-cases/scripts/run_test.py` (step 2) and
    `dm_iterator.sh` — skip `.sim` generation with a message when no `.dat`
    inputs are present.
- `tests/fixtures/solver/` retains `.gitkeep`, so the directory still exists.

## Retrieving files

```bash
# from a machine with Tailscale access to ace-linux-1
scp 'vamsee@ace-linux-1:/mnt/ace/digitalmodel/<repo-relative path>' <path>

# verify after copying
sha256sum <path>   # compare against MANIFEST.sha256.tsv
```

## Policy

- Solver program binaries never belong in git or the archive (reproducible
  from vendor installers; see workspace-hub ecosystem notes).
- Regenerable bulky outputs stay local and out of git (see `.gitignore`).
- The archive is the canonical home for irreplaceable bulky inputs and
  reference outputs; git keeps code, manifests, pointers, and small key results.
