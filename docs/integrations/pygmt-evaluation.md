# PyGMT Bathymetric Mapping — Evaluation

**Issue:** vamseeachanta/workspace-hub#1455
**Date:** 2026-03-31
**Status:** BLOCKED — GMT system library not installed

## Overview

[PyGMT](https://github.com/GenericMappingTools/pygmt) is a BSD-3-Clause Python interface to the Generic Mapping Tools (GMT) for earth/ocean science mapping. It provides publication-quality maps with built-in datasets (earth relief, coastlines, plate boundaries) and is particularly relevant for offshore/subsea engineering visualization: bathymetric charts, platform location maps, pipeline route overlays.

## Prerequisites

PyGMT requires the GMT C library as a system dependency. This is a hard requirement — PyGMT cannot function without it.

```bash
# Ubuntu/Debian
sudo apt-get install gmt gmt-gshhg-low gmt-gshhg-high

# macOS
brew install gmt

# conda (recommended — bundles GMT automatically)
conda install -c conda-forge pygmt
```

## Installation (Python package)

```bash
uv add "pygmt>=0.13.0"
```

## Key API Patterns

```python
import pygmt

# Load built-in earth relief (bathymetry + topography)
grid = pygmt.datasets.load_earth_relief(resolution="02m", region=[-98, -82, 18, 31])

# Create publication-quality map
fig = pygmt.Figure()
fig.grdimage(grid=grid, projection="M20c", cmap="geo", frame=True)
fig.coast(shorelines=True, resolution="h")

# Plot point data (e.g., platform locations)
fig.plot(x=[-89.0], y=[28.2], style="t0.3c", fill="red", pen="0.5p,black")

fig.savefig("gulf_bathymetry.png", dpi=300)
```

## Data Caching

PyGMT downloads earth science datasets on first use from the GMT data server. Data is cached at `~/.gmt/server/`. Expected cache sizes:

| Resolution | Description | Approximate Size |
|------------|-------------|-----------------|
| 30m | 30 arc-minute (~55 km) | ~1 MB |
| 10m | 10 arc-minute (~18 km) | ~5 MB |
| 02m | 2 arc-minute (~3.7 km) | ~50 MB |
| 01m | 1 arc-minute (~1.8 km) | ~150 MB |

## Evaluation Blocker

GMT system library (`gmt`) is not installed on this machine and `sudo` access is unavailable. The Python package (`pygmt`) can be pip-installed but will fail at runtime without GMT.

**Blocked: needs GMT.** To unblock:
1. Install GMT: `sudo apt-get install gmt gmt-gshhg-low gmt-gshhg-high`
2. Run tests: `uv run pytest tests/test_pygmt_integration.py -v`
3. Run evaluation: `uv run python scripts/integrations/pygmt_evaluation.py`

## Engineering Workflow Fit

- **Bathymetric charts**: visualize seabed topography for pipeline routing, platform siting
- **Subsea infrastructure maps**: overlay field layouts, mooring spreads, cable routes on relief maps
- **Geohazard assessment**: combine bathymetry with fault lines, slope gradients
- **Report-quality figures**: GMT is the standard for peer-reviewed earth science publications
- **Built-in datasets**: no need to source/license separate bathymetry data for initial assessments

## Test Coverage

4 integration tests at `tests/test_pygmt_integration.py` (all skipped when GMT unavailable):
- Import verification and version check
- Figure object creation with expected methods
- Earth relief dataset loading for Gulf of Mexico region
- PNG figure export with coastline rendering

## Recommendation

**Integrate when GMT is available.** PyGMT fills a unique niche for offshore/subsea mapping that matplotlib and plotly cannot match — built-in bathymetric data, proper map projections, coastline rendering, and publication-quality cartographic output. The conda installation path (`conda install -c conda-forge pygmt`) bundles GMT automatically and may be the simplest route.

## Artifacts

- Evaluation script: `scripts/integrations/pygmt_evaluation.py`
- Tests: `tests/test_pygmt_integration.py`
- Dependency: `pyproject.toml` — `pygmt>=0.13.0`
- Catalog: `data/oss-engineering-catalog.yaml` — already present in GIS section
