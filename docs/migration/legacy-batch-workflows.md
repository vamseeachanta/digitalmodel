# Legacy Batch-Run Workflows → digitalmodel config-driven workflows

> Status: Draft (2026-06-13)
> Scope: the "one base input file → many runs" workflows from the legacy
> `aceengineercode` repo, and how to re-establish them as YAML-config-driven
> workflows in digitalmodel.
> Companion to [`aceengineercode-migration.md`](./aceengineercode-migration.md)
> (which covers the high-level module consolidation). This doc covers the
> **batch / parametric run engine** specifically.

Source repo mapped: `/mnt/remote/ace-linux-1/ace/aceengineercode`
Target repo: `digitalmodel` (this repo)

---

## 1. Why this exists

The legacy repo's dominant value is a large body of **batch/parametric
engineering workflows**: take one *base input file*, expand it into *N runs*
(load cases, sensitivities, sweeps), drive an external solver or pure-Python
calc, then collect the results. The user's tier-1 repos (`digitalmodel`,
`assethold`, …) have since standardized on a cleaner **UV-workflow contract**.
The goal is to re-establish the legacy workflows on top of that contract so they
become a durable basis for "a lot of analysis", and to reuse the legacy data
after a light sanity cleanup.

---

## 2. The legacy mechanism (what we're porting)

Confirmed by reading the source (not the README). The legacy engine is:

```
data_manager/<basename>.yml         # base config (template)
        +  custom YAML from CLI      # recursive deep-merge overlay
        +  a "matrix" source         # csv | embedded-yaml-list | xlsx | numeric-range
   ──► render N per-case input files (named by pattern)
   ──► run solver (OrcaFlex / SHEAR7 / pure-Python) — usually serial
   ──► collect manifests + summaries (csv/yml/xlsx/plots)
```

Key code:
- `common/ApplicationManager.py` — loads base YAML, overlays custom YAML
  (`update_deep_dictionary`), derives result/log names, optional DB run-queue.
- `common/FEAComponents.py` — the canonical "1 base OrcaFlex config → N YAMLs"
  loop driven by a CSV metocean matrix.
- `common/ETL_components.py` — ASCII-template replacement → many SHEAR7 `.dat`
  files + generated `.bat` batch scripts.
- `custom/catenary/orcaflexModel.py` — catenary SLWR/SCR generation from an
  embedded `EnvironmentLoad.Extreme/Fatigue` YAML matrix.
- `common/BS7910_critical_flaw_limits.py` — fracture-mechanics sweep over YAML
  arrays (`a_array`, `c_array`, `location_array`, …), optional `multiprocessing`.
- `common/API579_components.py` — API 579 corrosion-grid sweep (`gml_simulated_grid`
  + `FCA` list).

**Four matrix-source patterns** seen in the wild:
| Pattern | Example config | How variants are produced |
|---|---|---|
| CSV matrix | `data_manager/data/metocean/0198_SALM_*.csv` | one run per CSV row; columns named `Wave*`/`Current*`/`Wind*` map into the model |
| Embedded YAML list | `dataManager/catenary/16OD_SE1200_HE1500.yml` | loop `EnvironmentLoad[type]` list |
| XLSX sheet / numeric range | `config/etl/ETL_py_shear7_input_files_1.yml` (`from_range: [1, 24]`) | expand range / XLSX rows into per-case files |
| YAML arrays | `config/fracture_mechanics/*sens*.yml` | cartesian over `*_array` keys |

> ⚠️ Caveat from the map: the `cfg_variations.pre_analysis/post_analysis`
> hook referenced in several `data_manager/*_cfg_variations.yml` files is
> **stale/incomplete** in this checkout (no working `update_cfg_with_variation`).
> Treat it as design intent, not a runnable feature — do not port it verbatim.

The legacy VIV workflow illustrates the scale: one base `set_up/viv_analysis.yaml`
plus **~17** run-variant configs under `config/viv_analysis/` (currents/waves,
TTRs A5/A6/A10, extreme vs long-term, water-depth, wall-thickness, shielding).

---

## 3. The digitalmodel target contract (port *into* this)

digitalmodel already implements the tier-1 UV-workflow contract — do **not**
reinvent it:

- **Invocation:** `uv run python -m digitalmodel <input.yml>`
- **Dispatch:** `src/digitalmodel/engine.py` reads the YAML, takes
  `cfg["basename"]`, routes to that module's router.
- **Defaults:** `src/digitalmodel/base_configs/modules/<basename>/<basename>.yml`
- **Runnable example per workflow:** `examples/workflows/<id>/input.yml`
- **Registry:** `docs/registry/workflows.yaml` (schema_version 1; one row per
  workflow: `id, basename, input, outputs, test, runtime`). Invariant:
  *registry row == runnable example == green run*.

**Batch/parametric infrastructure already present** (this is gap-filling, not
greenfield):
- `src/digitalmodel/orcaflex/batch_parametric.py` — `ParameterSweep`,
  `ParametricStudy` (full-factorial case matrix, output-dir org, summary tables).
- `src/digitalmodel/orcaflex/model_builder.py`
- `src/digitalmodel/ansys/batch_runner.py`
- `src/digitalmodel/hydrodynamics/diffraction/{batch_processor,parametric_spec_generator,*_batch_runner}.py`

Already-registered relatives of the legacy workflows: `viv-analysis`,
`wall-thickness-quickcheck`, `free-span-f105`, `pipe-capacity`,
`pipeline-lateral-buckling`, `api579-pipe-ffs-*`, `fatigue-analysis`.

---

## 4. Proposed unified "parametric run" config

The clean abstraction (matches all four legacy matrix patterns) — a single
`basename: parametric_run` that wraps any existing workflow basename:

```yaml
basename: parametric_run
parametric_run:
  base_input: examples/workflows/<wf>/input.yml   # the template to expand
  target_basename: viv_analysis                    # workflow each case runs
  variants:
    source: csv            # csv | yaml_matrix | xlsx | range | factorial
    # csv:        file: cases.csv          (one run per row; columns → cfg paths)
    # yaml_matrix: list: [ {...}, {...} ]
    # xlsx:       file: x.xlsx, sheet: Sheet1
    # range:      param: cp, from: 1, to: 24
    # factorial:  parameters: [ {name: wave_hs, values: [...]}, ... ]
    mapping:               # column/array name → dotted cfg path it overrides
      WaveHs: Environment.wave.Hs
      RefCurrentSpeed: Environment.current.speed
  render:
    output_pattern: "LC_{index}"        # or "{riser}_{WaveTrainName}"
    output_dir: results/parametric
  run:
    engine: python          # python | orcaflex | shear7
    mode: serial            # serial | parallel
  collect:
    manifest: results/parametric/cases.csv
    summary: results/parametric/summary.csv
```

This reuses `batch_parametric.ParametricStudy` for the `factorial` source and
adds thin readers for `csv`/`xlsx`/`range`/`yaml_matrix`. Each generated case is
just a normal digitalmodel `input.yml` for `target_basename`, so every existing
workflow gets batch capability for free.

---

## 5. Port plan (priority order)

Ranked by completeness of the legacy lifecycle and reuse of existing
digitalmodel modules.

| # | Workflow | Legacy entry | Matrix driver | digitalmodel home | Notes |
|---|---|---|---|---|---|
| 1 | Parametric-run framework | `common/FEAComponents.py` | csv/range/yaml/factorial | `orcaflex/batch_parametric.py` + new `parametric_run` basename | Foundational — unlocks the rest |
| 2 | VIV model preparation | `scripts/viv_analysis.py`, `config/viv_analysis/*` | ~17 variant ymls | `subsea/viv_analysis/` (exists) | Wire variants as `parametric_run` cases; reuse `.mds` modal data |
| 3 | Vertical/drilling riser stack-up | `scripts/vertical_riser.py`, `common/typical_riser_stack_up_calculations.py` | depth × WT × pcf | `drilling_riser/stackup.py` (exists) | Reuse `data_manager/data/risers/drilling/*.csv` |
| 4 | Wall-thickness / pipe capacity | `scripts/pipe.py`, `config/pipe/*Drilling_Riser*.yml` | per-case ymls | `structural/pipe_capacity/`, `wall-thickness-quickcheck` (exists) | Mostly registry + example gap-fill |
| 5 | SHEAR7 VIV `.dat` generation | `scripts/ETL.py` | range 1..24 + XLSX | new `shear7` adapter | ASCII-template replacement engine |
| 6 | Catenary SLWR/SCR generation | `custom/catenary/orcaflexModel.py` | embedded YAML lists | `subsea/` catenary modules | OrcaFlex-license gated |
| 7 | Fracture mechanics sensitivity | `scripts/fracture_mechanics.py` | YAML `*_array` | `fatigue/` | Pure-Python; parallelizable |
| 8 | API 579 corrosion grid | `scripts/API579.py` | `gml_simulated_grid` + FCA | `asset_integrity/`, `api579-*` (exists) | Mostly gap-fill |

Each port = add/confirm `base_configs/modules/<basename>/<basename>.yml` + a
small **offline** `examples/workflows/<id>/input.yml` + a `docs/registry/workflows.yaml`
row + a `tests/workflows/` green run. Defer anything that can't run green offline
(OrcaFlex/SHEAR7 license) and say so honestly in the registry (`runtime:` field).

---

## 6. Reusable legacy data + sanity cleanup

Legacy data worth reusing as committed fixtures (under `data_manager/data/`):

| Data | Path | Use |
|---|---|---|
| Vertical-riser stack-up properties | `data_manager/data/risers/drilling/app_vertical_riser_{2500,5000,8000,10000}ft_WT{0750,1000}_{064,120}pcf_stack_up_properties.csv` | riser stack-up + wall-thickness examples |
| TTR modal data (14 `.mds`) | `data_manager/data/risers/ttr/{modes,shear7}/` | VIV model-prep fixtures |
| Metocean template | `data_manager/data/metocean_data_template.xlsx`, `data/metocean/*.csv` | parametric matrix examples |
| Drilling-riser templates | `data_manager/data/drilling_riser_template{,_SQL}.xlsx` | riser config examples |
| Fatigue / GI-WI repaired | `data_manager/data/fatigue/`, `GI_repaired.xlsx`, `WI_repaired.xlsx` | fatigue/FFS examples |

Totals under `data_manager/data`: ~29 csv, 27 xlsx, 14 mds, 4 dat.

**Sanity cleanup required before reuse (mechanical, scriptable):**
- **62** config YAMLs hardcode Windows absolute paths
  (`C:\Users\achantv\…\Desktop\…`, `K:\0198\…`) — strip / repoint at portable
  fixture locations.
- **94** configs use backslash separators (`data_manager\data\…`) — convert to
  POSIX `/`.
- Legacy `set_up/*.yaml` pin `python=3.7` conda envs — ignore; digitalmodel uses
  `uv`.
- De-duplicate the `data_manager/` vs `dataManager/` twin trees.

Recommended: a one-shot `scripts/migration/clean_legacy_config.py` that does
backslash→POSIX + absolute-path stripping into a `mapping:` block, so cleanup is
reproducible rather than hand-edited.

---

## 7. Status / next steps

- [x] Legacy batch-run mechanism mapped (this doc)
- [x] digitalmodel target contract + existing batch infra surveyed
- [x] Reusable data + cleanup signals inventoried
- [ ] Implement `parametric_run` basename (port #1)
- [ ] Port VIV model-prep variants (port #2)
- [ ] Migrate cleaned riser/VIV data as `examples/workflows/*` fixtures
- [ ] Register each green workflow in `docs/registry/workflows.yaml`

Heavy legacy-repo reading was delegated to a codex subagent (read-only); this
doc is the curated synthesis. No legacy files were modified.
