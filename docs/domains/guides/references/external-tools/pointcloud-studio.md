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

**Scope:** a Python desktop application for viewing, cleaning and analysing point clouds and surfaces from hydrographic (MBES) and land (LiDAR, survey) sources. Version 0.1.23, last updated 2026-10-19. Its GitHub topics are offshore wind, cable, RPL, bathymetry, dredge, DTM, lidar and geophysics.

## Relevance to this repository

The application covers survey-data work around subsea cables, pipelines and foundations. That is the input side of several digitalmodel modules:

| PointCloud Studio feature | Related digitalmodel area |
|---|---|
| Free spans along a cable or pipe profile, from a cable-top profile or from the point cloud alone | `src/digitalmodel/subsea/pipeline/free_span` |
| Scour monitoring per structure (far-field reference, maximum depth, extent, volume, rate between surveys) | `src/digitalmodel/base_configs/modules/scour` |
| Depth of burial against a target, with KP-bin statistics; multi-year cable assessment with jetting re-burial | subsea cable and pipeline on-bottom assessment |
| Rock berm as-built against design (thickness, under- and over-fill, cover over the cable) | subsea cable and pipeline protection |
| Sand wave migration, with a burial forecast along the route | geotechnical and seabed mobility |
| Volumes, earthworks and trench design, DEM of difference, M3C2 change detection | `src/digitalmodel/geotechnical` |

Other capabilities include:
- Readers for LAS/LAZ, XYZ, GeoTIFF and RPL route files.
- Pure-Python Shapefile, GeoPackage, GeoJSON, KML, GPX and DXF input and output (no GDAL).
- TIN and DEM gridding with shoalest-depth cells.
- Photogrammetry (structure from motion plus dense matching) to a point cloud.
- Mesh and primitive fitting (planes, cylinders for piles, buildings).

## Conditions of use

- **Reference only.** The repository states no licence, so all rights are reserved by default. Code, files and screenshots from it are not copied into this repository. Features and published methods may be studied; any implementation here is written from the standards and literature.
- **Uses:**
  - identifying the survey-derived inputs that a free-span, scour or burial assessment needs;
  - cross-checking digitalmodel results against an independent tool on a shared public dataset;
  - ideas for handling cable route (KP) data and multi-epoch surveys.
- **Licence recheck.** If the author adds a licence, this page records it with the date. The licence is checked again before any vendoring or dependency.
- **Verification status:** not run or verified. The tool stays unverified until one of its outputs is checked against a hand calculation or a published benchmark.
