# GTM Demo Suite -- Parametric Engineering Reports

> 5 demos | 1,292 cases | Overnight engineering screening for marine installation contractors

Each demo sweeps a realistic parameter space, runs engineering calculations, and produces a branded HTML report with interactive Plotly charts. Reports are self-contained single-file HTML -- open in any browser, print to PDF.

## Quick Start

All demos run from the `digitalmodel` repo root:

```bash
cd digitalmodel

# Run any demo (e.g. Demo 1):
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_01_dnv_freespan_viv.py

# Regenerate report from cached results (no recalculation):
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_01_dnv_freespan_viv.py --from-cache
```

## Demos

### Demo 1: DNV Freespan / VIV Screening

**Script:** `demo_01_dnv_freespan_viv.py` (1,257 lines) | **Cases:** 680

Parametric VIV screening per DNV-RP-F105 simplified methodology:

- 3 pipeline sizes (8", 12", 16") x 8 spans x 5 currents x 4 gap ratios = 480 pipeline cases
- 1 rigid jumper (8") x 8 spans x 5 currents x 5 gap ratios = 200 jumper cases
- 5 interactive charts: span screening heatmaps, gap ratio sensitivity, current profiles

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_01_dnv_freespan_viv.py
```

---

### Demo 2: Pipeline Wall Thickness -- Multi-Code Comparison

**Script:** `demo_02_wall_thickness_multicode.py` (1,343 lines) | **Cases:** 72

Compares wall thickness requirements across three design codes:

- 6 pipe sizes (6" to 20")
- 3 design codes: DNV-ST-F101, API RP 1111, PD 8010-2
- 4 internal pressures (10, 15, 20, 25 MPa)
- 5 lifecycle phases per pipe
- 5 interactive charts: code comparison matrices, utilisation envelopes

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_02_wall_thickness_multicode.py
```

> **Note:** Full-calc mode uses the `digitalmodel` library (via `PYTHONPATH=...src`). Cached mode has no library dependency.

---

### Demo 3: Deepwater Mudmat Installation

**Script:** `demo_03_deepwater_mudmat_installation.py` (1,223 lines) | **Cases:** 180

5-phase installation screening for subsea mudmat foundations:

- 2 vessels (Large CSV 5,000te, Medium CSV 2,500te)
- 6 water depths (500--3,000 m)
- 3 mudmat sizes (50te, 100te, 200te)
- 5 significant wave heights (1.0--3.0 m)
- 5 phases per case: lift-off, in-air, splash, lowering, landing

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_03_deepwater_mudmat_installation.py
```

---

### Demo 4: Shallow Water Pipeline Installation (S-Lay)

**Script:** `demo_04_shallow_water_pipelay.py` (1,642 lines) | **Cases:** 60

Self-contained S-lay catenary mechanics -- no external pipeline modules:

- 2 vessels (Large PLV 600te, Shallow Water Barge 250te)
- 5 pipe sizes (8", 12", 16", 20", 24") -- all X65
- 6 water depths (7--30 m)
- Evaluates overbend strain, sagbend stress, top tension, and stinger departure angle

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_04_shallow_water_pipelay.py
```

---

### Demo 5: Deepwater Rigid Jumper Installation

**Script:** `demo_05_deepwater_rigid_jumper_installation.py` (1,229 lines) | **Cases:** 300

5-phase jumper installation plus tie-in alignment assessment:

- 2 vessels (Large CSV 5,000te, Medium CSV 2,500te)
- 6 water depths (500--3,000 m)
- 5 rigid jumper lengths (20--100 m, 8" OD X65)
- 5 significant wave heights (1.0--3.0 m)
- Phases: lift-off, in-air bending, splash zone slamming, lowering, tie-in alignment (50 mm tolerance)

```bash
PYTHONPATH=examples/demos/gtm:src uv run python \
    examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py
```

## Common Flags

| Flag | Behaviour |
|------|-----------|
| *(none)* | Calculate if no cache exists, else reuse cache |
| `--from-cache` | Skip engineering calculations, regenerate report from cached JSON |
| `--force` | Recalculate everything, overwrite existing cache |

Cached results live in `results/` as JSON. Use `--from-cache` for fast chart iteration.

## Data Files

All input data lives in `data/`:

| File | Description |
|------|-------------|
| `csv_hlv_vessels.json` | Construction / heavy-lift vessel specs (crane capacity, RAOs) |
| `design_codes.json` | Wall thickness code parameters (DNV, API, PD 8010) |
| `freespan_scenarios.json` | Freespan geometry and current profiles for Demo 1 |
| `mudmat_structures.json` | Mudmat dimensions, weights, and lift points |
| `pipelay_vessels.json` | Pipelay vessel specs (tensioner, stinger geometry) |
| `pipelines.json` | Pipeline material and coating properties |
| `rigid_jumpers.json` | Rigid jumper geometry and material data |

## Supporting Modules

### `report_template.py` (620 lines)

Shared HTML report generator used by all 5 demos. Key API:

```python
from report_template import GTMReportBuilder

report = GTMReportBuilder(
    title="Wall Thickness Multi-Code Comparison",
    subtitle="72 parametric cases across 6 pipe sizes and 3 design codes",
    demo_id="demo_02",
    case_count=72,
)
report.add_section("Methodology", methodology_html)
report.add_chart("chart_id", plotly_fig)
report.add_table("table_id", pandas_df)
report.add_live_mode_teaser()
report.build("output/demo_02_wall_thickness_report.html")
```

### `calc_values.py` (203 lines)

Pipe property calculator -- steel/coating/concrete weights, submerged weight, moment of inertia, natural frequency inputs. Uses API 5L pipe dimensions.

## Output

- `output/` -- Generated HTML reports (one per demo)
- `results/` -- Cached JSON results plus cross-demo comparison matrices (`structure_comparison_matrix.json`, `vessel_comparison_matrix.json`)

## Dependencies

```
numpy
pandas
plotly
```

Demo 2 additionally imports from `digitalmodel` (via `PYTHONPATH=...src`) when running in full-calc mode. All other demos are fully self-contained.

## Related Issues

- [#2091](https://github.com/vamseeachanta/workspace-hub/issues/2091) -- This README
- [#1800](https://github.com/vamseeachanta/workspace-hub/issues/1800) -- GTM demo umbrella issue
