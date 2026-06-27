# Structural demo outputs

Generated artifacts for the ship plate/panel buckling demos. Small curated
deliverables are committed; large regenerable files are gitignored (see
`.gitignore` in this directory).

Regenerate commands are run **from the repo root** using the project venv
interpreter (not `uv run`):

```
/mnt/local-analysis/digitalmodel/.venv/bin/python <script>
```

| File | What it is | Status | Regenerate command |
|------|------------|--------|--------------------|
| `ship_plate_buckling_report.html` | Plate buckling parametric report (curated lookup deliverable) | committed | `.venv/bin/python examples/demos/structural/ship_plate_buckling_parametric.py` |
| `results.json` | Plate buckling result records | committed | `.venv/bin/python examples/demos/structural/ship_plate_buckling_parametric.py` |
| `cases.csv` | Plate buckling case matrix | committed | `.venv/bin/python examples/demos/structural/ship_plate_buckling_parametric.py` |
| `ship_panel_buckling_report.html` | Panel buckling parametric report (curated lookup deliverable) | committed | `.venv/bin/python examples/demos/structural/ship_panel_buckling_parametric.py` |
| `panel_results.json` | Panel buckling result records | committed | `.venv/bin/python examples/demos/structural/ship_panel_buckling_parametric.py` |
| `panel_cases.csv` | Panel buckling case matrix | committed | `.venv/bin/python examples/demos/structural/ship_panel_buckling_parametric.py` |
| `ship_panel_design_explorer.html` | Interactive design-explorer dashboard (~8 MB) | generated / gitignored | `.venv/bin/python examples/demos/structural/ship_panel_design_explorer.py` |
| `panel_design_grids.json` | Embedded grid data for the explorer (~5 MB) | generated / gitignored | `.venv/bin/python examples/demos/structural/ship_panel_design_explorer.py` |
| `*_standalone.html` | Self-contained (Plotly-inlined) report variants (~3.5 MB each) | generated / gitignored | `.venv/bin/python examples/demos/structural/make_standalone_report.py` |

## Viewing the interactive explorer

`ship_panel_design_explorer.html` is ~8 MB and is best viewed locally rather
than in git previews:

```
DISPLAY=:1 xdg-open examples/demos/structural/output/ship_panel_design_explorer.html
```
