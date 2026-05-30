# GIS historical-imagery timelapse — worked example (workspace-hub #2538)

This folder is the **as-run evidence** for [workspace-hub issue #2538](https://github.com/vamseeachanta/workspace-hub/issues/2538):
generating a property/neighborhood historical-imagery timelapse for
`11511 Piping Rock Dr., Houston, TX 77077`.

It is kept here as a **traceable, reusable example**. The reusable pipeline code
that produced these artifacts has been generalized and moved into the
`digitalmodel` package so it can be used across the repo ecosystem.

## Where the code lives now

| Original script (in `scripts/`, kept here as-run) | Generalized module in `digitalmodel` |
|---|---|
| `prepare_aoi.py` | `digitalmodel.gis.imagery.aoi` |
| `probe_imagery_access.py` | `digitalmodel.gis.imagery.stac_client` |
| `render_naip_preview_artifacts.py` | `digitalmodel.gis.imagery.renderer` |
| (hard-coded `addresses.json`/`addresses.yaml`) | `digitalmodel.gis.imagery.manifest.ImageryManifest` |
| — | CLI: `imagery-timelapse` (`digitalmodel.gis.imagery.cli`) |

The key change in the port: the hard-coded `/mnt/local-analysis/ace2-gis-timelapse`
working directory was replaced by manifest-driven `output_root` / `cache_root` /
`log_root`, so the same code runs for any address set in any workspace. The
ported AOI math reproduces this example's buffer bounds exactly.

## Reproducing with the generalized code

```bash
# from a digitalmodel environment (extras: pip install 'digitalmodel[gis-imagery]' for Earth Engine)
imagery-timelapse run --manifest addresses.yaml
# or stage by stage:
imagery-timelapse prepare-aoi --manifest addresses.yaml
imagery-timelapse probe       --manifest addresses.yaml
imagery-timelapse render      --manifest addresses.yaml
```

> Note: `addresses.yaml`/`addresses.json` here still contain the original
> absolute `output_root` paths from the live run. For a fresh run, point those
> roots at a local output directory (relative roots resolve against the manifest
> file's location).

## What's in this folder

- `addresses.yaml` / `addresses.json` — the address manifest (one address).
- `outputs/11511-piping-rock/` — AOI GeoJSON, NAIP preview frames, GIF/MP4,
  contact sheet, and per-address review report (~17 MB of media).
- `reports/` — source feasibility matrix, execution evidence, readiness checks,
  and the prepared GitHub comment draft for #2538.
- `scripts/` — the original as-run scripts (superseded by `digitalmodel.gis.imagery`).
- `cache/`, `logs/` — empty run scaffolding.

## Limitations of this example run

Earth Engine was not authenticated on the execution host, so the renderer used
public NAIP STAC preview/thumbnail assets. The frames are review-grade artifacts
that prove the multi-address pipeline; the documented next step is to upgrade the
renderer to NAIP COG/tile crop/mosaic for consistent AOI extent. See
`reports/source-feasibility-matrix.md` and the per-address review report for detail.
