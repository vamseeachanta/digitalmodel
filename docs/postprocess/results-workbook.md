# Multi-tab results workbook

`scripts/build_results_workbook.py` turns a workflow's results directory into a
single multi-tab `.xlsx`, in the style of the OrcaFlex / OrcaWave post-processing
spreadsheets. It is **data-driven** — it reports whatever the workflow actually
wrote, so it works for any run and never silently drops an output.

## Tabs produced
| Tab | Source |
|---|---|
| `Index` | every tab + a one-line description |
| `Run Metadata` | the licensed-run result JSON (`--result-json`), if given — run_id / scope / state / returncode / sha / timestamps |
| one per CSV | OrcaFlex `Summary`, per-variable `RangeGraph`s; OrcaWave `RAOs`, `AddedMass`, `Damping`, `Excitation` … |
| `Validation` | a `*validation*.json` flattened to `section / item / detail` |
| one per other JSON | flattened `key / value` |
| `Artifacts (local)` | `.owr` / `.owd` / `.sim` / `.gdf` / images — **path + size only, never embedded** (heavy outputs stay on the licensed host) |

## Usage
```bash
# OrcaFlex strength post-process results
uv run python scripts/build_results_workbook.py \
    <case>/results --out strength_report.xlsx

# OrcaWave diffraction solve results, with the licensed-run result metadata tab
uv run python scripts/build_results_workbook.py \
    <case>/results \
    --result-json <queue>/results/<run_id>.json \
    --out orcawave_report.xlsx

# multiple dirs in one workbook
uv run python scripts/build_results_workbook.py dirA dirB --out combined.xlsx
```

## Licensed-run note
Run it **on the licensed host** (ace-win-2) against the local results dir: the
detailed numbers and heavy solver files never leave the host, and the workbook is
the deliverable. Heavy files are referenced by path/size, never embedded — so the
workbook itself stays metadata-plus-numbers, not raw solver payload.
