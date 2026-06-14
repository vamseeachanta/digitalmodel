# Legacy Engineering-Flow Catalog & Coverage Reconciliation

> Status: Draft (2026-06-13)
> Companion to [`legacy-batch-workflows.md`](./legacy-batch-workflows.md).
> That doc covers the **batch/parametric** engine; this one catalogs the
> **standalone engineering calculators** in the legacy `aceengineercode` repo
> (esp. `ExistingCodes/`, 414 MB) and reconciles them against what digitalmodel
> *already has*.

Heavy read-only legacy mapping delegated to a codex subagent; coverage
reconciliation done against the real digitalmodel tree.

---

## The key finding: "module exists, workflow doesn't"

The codex inventory inferred digitalmodel coverage *without reading digitalmodel*.
Reconciling against the actual tree, **most legacy "gaps" already have code
modules in digitalmodel** — they simply aren't exposed as registered, runnable
`uv run python -m digitalmodel <input.yml>` workflows. So the highest-value,
lowest-risk work is **exposing existing modules via the workflow contract**, not
re-porting legacy code.

| Legacy flow | digitalmodel module (already present) | Registered workflow? | Action |
|---|---|---|---|
| Ramberg-Osgood material curve | `structural/stress/stress_strain.py` (`RambergOsgoodModel`, `StressStrainCurve`) | **No** | Expose as `stress_strain` workflow ← next port |
| DNV-OS-F101 rigid pipe | `structural/analysis/wall_thickness_codes/dnv_st_f101.py`; `orcaflex/pipelay_analysis.py` | Partial (under `wall_thickness`) | Verify/extend coverage |
| DNV-OS-F201 / API 2RD code check | `orcaflex/code_check_engine.py`, `orcaflex/riser_config.py` | Partial | Verify; may need OrcaFlex |
| Riser stack-up | `drilling_riser/stackup.py` (`top_tension_required`, `wall_thickness_required`, `effective_tension`) | **No** (functions, no router) | Expose as `riser_stackup` workflow |
| SN fatigue curve generator | `ansys/fatigue_postprocessor.py`; `fatigue/` | Partial | Distinct from `fatigue_analysis` (damage) — assess |
| Plate / stiffener buckling | `plate_buckling` (registered) | **Yes** | Already covered |
| RAO analysis | `rao_analysis` (registered, routed) | **Yes** | Already covered |
| Beam / taper FE | — none — | **No** | Genuine new-module gap |

> Lesson for every future port: **grep digitalmodel first.** Codex inferences
> about coverage are starting points, not ground truth.

---

## Condensed legacy flow inventory (offline-capable highlighted)

Pure-Python / offline flows are the priority (no OrcaFlex/SHEAR7/ANSYS/network).

**Pipe / pipeline:** ASME B31 sizing (`ExistingCodes/ASMEB31/ASMEB31Sizing.py`),
DNV-OS-F101 (`DNV-OS-F101/DNVOSF101Analysis.py`), DNV-OS-F201 burst/collapse
(`Burst_collapse_code/`), API STD 2RD (`API-STD-2RD/APISTD2RDMethods.py`), pipe
ovality (`pipeOvality/`), pipe section properties (`scripts/design_calculations.py`).
*All pure-Python.*

**Risers:** API RP 2RD utilization (`API-RP-2RD/APIRP2RD.py`), TSJ sizing
(`API-RP-2RD/TSJSizing/`), riser stack-up drawing (`stack_up_drawing/`). OrcaFlex
strength/fatigue/modal postprocess (`OrcaFlex-Post/`) — needs OrcFxAPI.

**Structural:** plate buckling (`015_Plate_Buckling/`), stiffener/girder buckling,
beam/taper FE (`taperDesign/beamFE.py`), Ramberg-Osgood curve
(`Non-LinearStressStrainRatio/`). *Pure-Python.*

**Fatigue:** SN fatigue curve generator (`017_Fatigue_Curves/FatigueBasiccurve.py`).

**Hydro/metocean/naval:** RAO plotter (`RAOs/RAOAnalysis.py`), fatigue-RAO,
NOAA/NDBC metocean fetch (`metocean/` — network), time-series signal analysis
(`common/time_series_components.py`).

**Geotech/drilling/field-dev:** Wellpath3D minimum-curvature
(`common/wellpath3D.py`), ONG/BSEE field development, ODA depth QA.

**Utilities:** compare tool, statistical describe, PDF table extraction, project
timeline, Sastry reference DB seeds (API-SPEC-5L, metocean).

---

## Revised port priority (offline + verified gap)

1. **`stress_strain`** (Ramberg-Osgood) — module exists, unregistered. ← in progress
2. **`riser_stackup`** — `drilling_riser/stackup.py` functions, needs a router.
3. **DNV-OS-F101 / F201 code-check workflow** — verify current `wall_thickness`
   coverage, extend if partial.
4. **Beam FE** (`beam_analysis`) — genuine new module.
5. **`compare_tool`** — generic CSV/XLSX/YAML overlay; broadly useful utility.
6. **SN fatigue curves** (`fatigue_curves`) — generator distinct from damage calc.

Each port = expose existing module (or port legacy calc) → `base_configs/modules/<basename>/<basename>.yml`
+ offline `examples/workflows/<id>/input.yml` + `docs/registry/workflows.yaml`
row + green durable-workflows test.

---

## Reusable data assets to preserve (committed XLSX/CSV)

Worth importing as fixtures (paths in legacy repo):
- `ExistingCodes/017_Fatigue_Curves/Ref/SN Curve Definitions.xlsx`
- `ExistingCodes/Non-LinearStressStrainRatio/...120 ksi Non Linear Stress Strain Curve (Released).xlsx`
- `ExistingCodes/API-RP-2RD/APIRP2RD Calculations.xlsx`
- `ExistingCodes/Burst_collapse_code/TestData/...Burst & Collapse Calculation.xlsx`
- `ExistingCodes/DNV-OS-F201/...DNV-OS-F201 Derating Calculations.xlsx`
- `ExistingCodes/015_Plate_Buckling/COD/TestData/ParametricInputs.xlsx`
- `ExistingCodes/stack_up_drawing/riserStackup.xlsx`
- `Sastry/API-SPEC-5L_Metric.xlsx`, `data_manager/data/pipe/API-SPEC-5L.csv`, `API-SPEC-5CT.csv`
- `data_manager/data/metocean/*.csv`, `*.xlsx`

> ⚠️ Do **not** carry forward secrets: `data_manager/finance.yml` (API keys),
> `data_manager/time_series.yml` (DB credentials).
