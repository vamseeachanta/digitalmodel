---
title: PointCloud Studio (external reference)
type: external-reference
url: https://github.com/skyflying/pointcloud-studio
recorded: 2026-10-20
recorded_by: maintainers
licence: none stated (no LICENSE file on 2026-10-20)
---

# PointCloud Studio (external reference)

**Repository:** https://github.com/skyflying/pointcloud-studio
**Author:** skyflying (GitHub)

**What it is:** a Python desktop application for viewing, cleaning and analysing point clouds and surfaces from hydrographic (MBES) and land (LiDAR, survey) sources. Version 0.1.23, last updated 2026-10-19. Its GitHub topics are offshore wind, cable, RPL, bathymetry, dredge, DTM, lidar and geophysics.

## Why we keep it

It covers survey-data work around subsea cables, pipelines and foundations. That is the input side of several digitalmodel modules:

| PointCloud Studio feature | Related digitalmodel area |
|---|---|
| Free spans along a cable or pipe profile, from a cable-top profile or from the point cloud alone | `src/digitalmodel/subsea/pipeline/free_span` |
| Scour monitoring per structure (far-field reference, maximum depth, extent, volume, rate between surveys) | `base_configs/modules/scour` |
| Depth of burial against a target, with KP-bin statistics; multi-year cable assessment with jetting re-burial | subsea cable and pipeline on-bottom work |
| Rock berm as-built against design (thickness, under- and over-fill, cover over the cable) | subsea cable and pipeline protection |
| Sand wave migration, with a burial forecast along the route | geotechnical and seabed mobility |
| Volumes, earthworks and trench design, DEM of difference, M3C2 change detection | `src/digitalmodel/geotechnical` |

Other capabilities it has:
- Readers for LAS/LAZ, XYZ, GeoTIFF and RPL route files.
- Pure-Python Shapefile, GeoPackage, GeoJSON, KML, GPX and DXF input and output (no GDAL).
- TIN and DEM gridding with shoalest-depth cells.
- Photogrammetry (structure from motion plus dense matching) to a point cloud.
- Mesh and primitive fitting (planes, cylinders for piles, buildings).

## How to use it

- **Reference only.** The repository states no licence, so all rights are reserved by default. Do not copy its code, files or screenshots into our repositories. Study its features and published methods, and write our own implementations from the standards and literature.
- **Useful for:**
  - checking which survey-derived inputs a free-span, scour or burial assessment needs;
  - cross-checking our results against an independent tool on a shared public dataset;
  - ideas for how cable route (KP) data and multi-epoch surveys are handled.
- **Before any reuse:** if the author adds a licence, record it here with the date and check it before vendoring or depending on the package.
- **Not reviewed:** we have not run it or verified its numbers. Treat it as an unverified tool until one of its outputs is checked against a hand calculation or a published benchmark.
