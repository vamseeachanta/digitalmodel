# Licensed E2E Acceptance Test (issue #610)

The arbitrary-mesh OrcaWave workflow has a licensed end-to-end acceptance test:

- **Test:** `tests/solver/test_licensed_e2e_arbitrary_mesh.py`
- **Fixture pair (repo-managed):** `tests/hydrodynamics/diffraction/fixtures/acceptance_610/`
  - `unit_box.gdf` — a 5-panel WAMIT GDF unit-box mesh (copied from the
    BEMRosetta `sample_box.gdf`).
  - `spec.yml` — a minimal DigitalModel `DiffractionSpec` referencing that mesh
    (3 frequencies × 3 headings, kept small for a fast licensed run).

## What it exercises

`spec.yml` + `unit_box.gdf` → `DiffractionSpec.from_yaml` →
`OrcaWaveRunner(use_api=True).run()` → `OrcaWaveBackend.generate_single()` (the
`.yml` input package) → `OrcFxAPI.Diffraction(.yml).Calculate()` →
`SaveResults()` (`.owr`) + `SaveData()` (`_data.dat`) → `_diffraction_to_results`
adapter (#625) → `validation_runner` verdict. The test then exports the `.xlsx`
from the saved `.owr` (`SaveResultsSpreadsheet`) and asserts:

1. The run completes (status `COMPLETED`, return code 0).
2. The `.yml`, copied mesh, `.owr`, `_data.dat`, and `.xlsx` paths are recorded.
3. Frequencies and headings are non-empty.
4. At least one RAO/result array is populated (plus added-mass/damping matrices).
5. Hydrostatic data is present when the solver produced it.
6. The run produced a non-`SKIPPED` auto-validation verdict (#625 wiring).

## Skip behaviour (Linux / CI)

`OrcFxAPI` is Windows-only and licensed. On hosts without it (e.g. dev-primary
`ace-linux-1`, CI), a module-level `@pytest.mark.skipif` SKIPS every test
cleanly. `OrcFxAPI` is imported lazily inside the fixture, never at module
scope, so importing/collecting the module never raises on a license-free host.
Verified on dev-primary: `pytest` reports **6 skipped**, not failed/errored.

## How licensed-win-1 runs it

`licensed-win-1` pulls this repo through the shared git remote (its scheduler
checks out the merged branch) and runs the solver suite:

```
set PYTHONPATH=src
uv run python -m pytest tests/solver/test_licensed_e2e_arbitrary_mesh.py -v
```

Because `OrcFxAPI` is importable there, the `skipif` is inactive and the real
`Diffraction(...).Calculate()` chain runs and must exit 0. That run:

- **discharges the #610 acceptance criteria** (full licensed E2E proof), and
- **discharges the #628 live-attrs verification debt** — it is the first
  licensed exercise of the assumption that result arrays are populated after
  `Calculate()` without an explicit `LoadResults()`. If the live attributes are
  empty, the adapter's mandatory `.owr LoadResults` fallback keeps the test
  green while flagging the divergence for #628 follow-up.

## Known runtime gap (xlsx_path)

`OrcaWaveRunner` saves `.owr` + `_data.dat` and populates
`RunResult.owr_path` / `data_file`, but does **not** yet populate
`RunResult.xlsx_path` (reserved for a later exporter phase, locked decision D2).
The test therefore exports the spreadsheet itself from the `.owr` — the same
pattern `tests/solver/smoke_test.py` uses — rather than monkeypatching the
runner. When a runtime exporter lands and sets `RunResult.xlsx_path`, the test
should switch to asserting that field directly.
