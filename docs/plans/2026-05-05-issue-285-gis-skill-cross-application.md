# Plan: digitalmodel #285 — WRK-020 GIS skill (close-as-stale recommended)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/285
**Status:** plan-review
**Tier:** T1 (close-out / verification only — no source changes proposed)

## Context

Issue #285 (WRK-020) requested a cross-application GIS skill for digitalmodel covering CRS handling, GeoJSON/Shapefile/KML I/O, spatial queries, and Blender + QGIS exporters. The acceptance-criteria list inside the issue body shows **all 12 of the explicit deliverables already checked**, with the implementation grounded in concrete files: `src/digitalmodel/gis/coordinates.py`, `gis/io/{geojson,kml,shapefile,geotiff}_handler.py`, `gis/core/spatial_query.py`, `gis/integrations/{blender_export,qgis_export,folium_maps,google_earth_export,plotly_maps}.py`, `gis/layers/{feature,well}_layer.py`, plus 7 test files under `tests/gis/`. The issue is in Stage 17 (Reclaim) with the `archived` label already applied — only the `close` and `archive` ledger steps remain.

**Stale-flag:** Yes, partial. The implementation has landed; only one acceptance bullet (Blender bathymetry/terrain mesh from GeoTIFF) is explicitly deferred. The WRK lifecycle expects a close-as-done with a follow-on issue for the deferred bathymetry mesh, **not** a fresh code plan. Recommend treating this as a verification-and-close task rather than re-opening implementation.

## Plan

### Task 1 — Verify completion claims at HEAD
Run `git ls-files src/digitalmodel/gis/ tests/gis/` and confirm every path enumerated in the issue's acceptance list exists. Run `uv run pytest tests/gis/ -v` and capture the count vs. the per-file totals quoted in the issue (27 + 8 + 14 + 19 + 13 + 18 + 5 = 104 tests claimed). Record any divergence between claim and tree in the close comment.

### Task 2 — Confirm SKILL.md and integration docs
Verify `.claude/skills/gis/SKILL.md` exists and lists the supported CRS, file formats, and Blender/QGIS integration steps as the acceptance list states. If missing or incomplete, scope the gap (one paragraph) and move it into a follow-on issue rather than this one.

### Task 3 — File the deferred-bathymetry follow-on
Open a new digitalmodel issue titled "GIS: Blender bathymetry/terrain mesh from GeoTIFF (deferred from #285)" referencing `gis/io/geotiff_handler.py` (which is done) and the missing Blender mesh-generation step. Link from the close comment on #285.

### Task 4 — Close the issue
Post a close comment on #285 enumerating: (a) which acceptance bullets verified live, (b) the test count from Task 1, (c) the follow-on issue number from Task 3. Apply `status:done` if the project uses it; otherwise close as completed.

## Acceptance Criteria

- [ ] All paths in the issue's acceptance list exist in HEAD (verified via `git ls-files`).
- [ ] `uv run pytest tests/gis/ -v` collected and passing count recorded; deviations from the claimed 104 tests are explained.
- [ ] `.claude/skills/gis/SKILL.md` confirmed present and aligned with the acceptance list (or gap is filed as a follow-on, not silently dropped).
- [ ] One follow-on issue filed for the deferred Blender bathymetry-mesh deliverable.
- [ ] #285 closed with the verification comment; no source changes pushed under this plan.

## Open questions

- The issue lists `dev-secondary` as the working machine but the gis module clearly landed via the dev-primary branch — was there a session that cross-machine-merged the work? If a CI run is required for close-out, identify the machine that should host the verification before this plan executes.
- Should this plan recommend `archived` → `closed` directly, or is the WRK ledger expecting a `Reclaim` artifact step we should not skip?
