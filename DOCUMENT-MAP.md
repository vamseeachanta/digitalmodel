# digitalmodel — Document Map (2026-09-22 slim)

On 2026-09-22 the repository was slimmed from ~2.87 GB to ~0.30 GB at HEAD.
**2,472 files (~2.56 GB)** of bulky engineering artifacts were moved out of git
to canonical archive storage. Nothing was deleted: every file is byte-verified
in the archive, and a complete pre-slim backup bundle exists.

## Archive location

```
ace-linux-1:/mnt/ace/digitalmodel/
```

Paths under the archive root **mirror the repo-relative paths** from the slim
commit's parent (`7e71d6b26eb863e95ae3f0ec9957b081d85dea28`). To restore a file,
copy it back to the same relative path in a checkout.

- Full per-file SHA-256 manifest: `ace-linux-1:/mnt/ace/digitalmodel/MANIFEST.sha256.tsv`
  (tab-separated: `sha256`, `bytes`, `repo-relative path`; 2,472 entries)
- Machine-readable pointers: `data/inputs.yaml` (in this repo)
- Pre-slim backup (verified complete history): `ace-linux-1:/mnt/ace/_transfer/digitalmodel-pre-slim-20260922.bundle`

## What moved

| Class | Files | Bytes | Contents |
|---|---|---|---|
| solver-inputs | 1,180 | ~2.22 GB | `.dat` (550 MB) / `.lis` / `.qtf` AQWA inputs, `.owr` OrcaWave results, `.sim` OrcaFlex models, `.igs`/`.stl`/`.dwg`/`.dxf` CAD & mesh, large vessel-RAO `.yml`/`.yaml` (>1 MB, 395 MB) and `.csv`, `.engd`/`.scdoc`, `.gz`, `.tif` |
| html-report-renders | 562 | ~720 MB | Rendered HTML analysis reports (regenerable from code + inputs) |
| documentation-images | 1,292 | ~335 MB | `.png`/`.jpg`/`.gif`/`.svg` illustrations under `docs/` and `examples/` (wave 2) |
| office-documents | 117 | ~97 MB | `.pptx`/`.docx`/`.pdf` engineering docs archived for reference |

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
