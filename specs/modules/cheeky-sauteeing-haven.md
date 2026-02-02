---
title: "GIS Module for Cross-Application Geospatial Tools"
description: "Build a GIS module providing CRS handling, coordinate transforms, spatial queries, and integrations with Blender, QGIS, Google Earth, and Folium for offshore engineering workflows."
version: "1.0"
module: "gis"

session:
  id: "cheeky-sauteeing-haven"
  agent: "claude-opus-4.5"

review:
  required_iterations: 3
  current_iteration: 0
  status: "pending"
  reviewers:
    openai_codex:
      status: "pending"
      iteration: 0
      feedback: ""
    google_gemini:
      status: "pending"
      iteration: 0
      feedback: ""
  ready_for_next_step: false

status: "draft"
progress: 0

created: "2026-02-01"
updated: "2026-02-01"
target_completion: ""

priority: "high"
tags: [gis, qgis, blender, google-earth, folium, coordinate-transform, spatial-query, offshore]

links:
  spec: ""
  branch: "feature/gis-module"
  work_item: "WRK-020"
  related: ["WRK-015"]
---

# GIS Module for Cross-Application Geospatial Tools

> **Module**: gis | **Status**: draft | **WRK**: 020 | **Priority**: high

## Summary

Replace the legacy `gis/gis.py` (59-line hardcoded KML script) with a production GIS module providing: CRS handling (WGS84/UTM/local), coordinate transforms via pyproj, GeoJSON/KML/Shapefile I/O, spatial queries, and integrations for Blender 3D visualization, QGIS processing scripts, Google Earth KML export, and Folium interactive web maps. Temporal layer tracks offshore infrastructure (platforms, wells, pipelines) over time with condition/value progression.

---

## Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Asset types | Offshore infrastructure | Platforms, wells, pipelines with installation dates, condition, depreciation |
| Google Maps | GeoJSON + Folium | No API key needed; export GeoJSON for Google Maps import, Folium for HTML maps |
| Blender output | 3D visualization | Georeferenced scenes with assets positioned on terrain/bathymetry |
| Primary data container | pandas DataFrame | GeoDataFrame when geopandas available; keeps core deps minimal |
| CRS authority | pyproj (existing dep) | Replaces legacy `utm` package and scattered conversion code |
| Blender/QGIS pattern | Script generation | Generate Python scripts executed externally; no bpy/qgis imports in digitalmodel |
| Optional deps | `[gis]` extra group | shapely, geopandas, fiona, folium, rasterio installed on demand |

---

## Module Structure

```
src/digitalmodel/modules/gis/
    __init__.py                         # Public API, lazy imports
    core/
        crs.py                          # CRSType enum, CRSDefinition dataclass, UTM zone detection
        coordinate_transformer.py       # pyproj-based transforms (WGS84 <-> UTM <-> local)
        geometry.py                     # GeoPoint, GeoBoundingBox (shapely optional)
        spatial_query.py                # within_radius, bounding_box, within_polygon
    io/
        geojson_handler.py              # Read/write GeoJSON, FeatureCollection
        kml_handler.py                  # Read/write KML/KMZ (replaces legacy gis.py)
        shapefile_handler.py            # Read/write Shapefiles via fiona (optional)
        geotiff_handler.py              # Read GeoTIFF rasters via rasterio (optional)
    layers/
        feature_layer.py                # DataFrame-backed vector layer with CRS
        raster_layer.py                 # Raster data layer (bathymetry/terrain)
        temporal_layer.py               # Time-indexed layer for asset tracking
        well_layer.py                   # Domain-specific well locations
    integrations/
        blender_gis.py                  # Generate Blender scripts for georeferenced scenes
        qgis_processing.py             # Generate standalone QGIS processing scripts
        google_earth_export.py          # KML/KMZ with TimeSpan, viewpoints
        folium_maps.py                  # Interactive HTML maps via Folium
        plotly_maps.py                  # Plotly scatter/density maps
    scripts/
        blender/
            georeferenced_scene.py      # bpy script: place objects at geo coords
            bathymetry_mesh.py          # bpy script: terrain mesh from raster
            well_markers.py             # bpy script: well marker objects
        qgis/
            well_layer_generator.py     # QGIS script: well point layers
            asset_tracking.py           # QGIS script: temporal visualization

tests/modules/gis/
    conftest.py
    test_data/
        sample_wells.geojson            # 5-10 GoM well locations
        sample_polygons.kml             # Simple placemarks
    test_crs.py
    test_coordinate_transformer.py
    test_geometry.py
    test_spatial_query.py
    test_geojson_handler.py
    test_kml_handler.py
    test_feature_layer.py
    test_temporal_layer.py
    test_well_layer.py
    test_blender_gis.py
    test_folium_maps.py
```

---

## Phases

### Phase 1: Core CRS + Transforms + GeoJSON/KML I/O

Foundation that all other phases build on.

- [ ] Add `geojson>=3.1.0,<4.0.0` to pyproject.toml dependencies
- [ ] Add optional `[gis]` dependency group to pyproject.toml
- [ ] Implement `core/crs.py` — CRSDefinition dataclass, CRSType enum, UTM zone auto-detect from lon/lat
- [ ] Implement `core/coordinate_transformer.py` — pyproj Transformer wrapper: transform_point, transform_points, transform_dataframe, wgs84_to_utm, utm_to_wgs84
- [ ] Implement `core/geometry.py` — GeoPoint, GeoBoundingBox with distance_to, contains (pure Python; shapely optional enhancement)
- [ ] Implement `io/geojson_handler.py` — read/write GeoJSON files, FeatureCollection construction
- [ ] Implement `io/kml_handler.py` — parse KML/KMZ to features, write KML from features (replaces legacy gis.py)
- [ ] Implement `layers/feature_layer.py` — DataFrame-backed layer with CRS, from_geojson, from_kml, to_geojson, to_kml, reproject
- [ ] Implement `__init__.py` — public API exports with lazy imports
- [ ] Fix engine.py routing: `basename == "gis"` currently routes to TimeSeriesAnalysis (line 166-168)
- [ ] Write tests: test_crs, test_coordinate_transformer, test_geometry, test_geojson_handler, test_kml_handler, test_feature_layer
- [ ] Create test_data/sample_wells.geojson and sample_polygons.kml fixtures

### Phase 2: Spatial Queries + Shapefile + Well Layer

- [ ] Implement `core/spatial_query.py` — within_radius (haversine for WGS84, euclidean for projected), bounding_box, within_polygon
- [ ] Implement `io/shapefile_handler.py` — fiona-based read/write (optional dep)
- [ ] Implement `layers/well_layer.py` — WellLayer extending FeatureLayer with target points (SHL, PP, FTP, LTP, EOT), from_excel
- [ ] Deprecate legacy code in `common/data.py` (gis_deg_to_distance, get_gis_converted_df_superseded) with import redirect
- [ ] Write tests: test_spatial_query, test_shapefile_handler, test_well_layer

### Phase 3: Visualization + Google Earth + Folium Web Maps

- [ ] Implement `integrations/google_earth_export.py` — KML/KMZ export with styling, viewpoints, folder organization
- [ ] Implement `integrations/folium_maps.py` — FoliumMapBuilder: create_map, add_feature_layer, add_well_markers, save HTML
- [ ] Implement `integrations/plotly_maps.py` — scatter_map, density_map using Plotly scattermapbox
- [ ] Write tests: test_folium_maps, test_plotly_maps, test_google_earth_export

### Phase 4: Blender 3D + QGIS + Bathymetry/Terrain

- [ ] Implement `integrations/blender_gis.py` — generate bpy scripts for georeferenced scenes; uses CoordinateTransformer to convert WGS84 to local coords relative to scene origin; interfaces with BlenderWrapper.run_script()
- [ ] Implement `scripts/blender/georeferenced_scene.py` — bpy script placing objects at transformed coordinates
- [ ] Implement `scripts/blender/bathymetry_mesh.py` — bpy script creating terrain mesh from raster grid
- [ ] Implement `scripts/blender/well_markers.py` — bpy script creating well marker cylinders/cones with metadata labels
- [ ] Implement `integrations/qgis_processing.py` — generate standalone PyQGIS scripts for well layers, styling, attribute tables
- [ ] Implement `scripts/qgis/well_layer_generator.py` — QGIS-executable script
- [ ] Implement `layers/raster_layer.py` — rasterio-based raster data (bathymetry from GEBCO)
- [ ] Implement `io/geotiff_handler.py` — GeoTIFF read via rasterio (optional dep)
- [ ] Write tests: test_blender_gis (script generation + syntax validation)

### Phase 5: Temporal GIS + Asset Value Tracking

- [ ] Implement `layers/temporal_layer.py` — TemporalFeatureLayer: at_time, time_range, value_progression, animate_to_kml
- [ ] Add temporal support to google_earth_export (KML TimeSpan elements)
- [ ] Add temporal support to folium_maps (time slider widget)
- [ ] Implement `scripts/qgis/asset_tracking.py` — QGIS temporal controller integration
- [ ] Asset properties: installation_date, condition_score, estimated_value, inspection_dates, status (active/decommissioned)
- [ ] Write tests: test_temporal_layer

---

## Key Files to Modify (Existing)

| File | Change |
|------|--------|
| `pyproject.toml` | Add `geojson` dep + `[gis]` optional group |
| `src/digitalmodel/engine.py:166-168` | Fix broken GIS routing (currently routes to TimeSeriesAnalysis) |
| `src/digitalmodel/modules/gis/__init__.py` | Replace empty init with public API |
| `src/digitalmodel/common/data.py` | Deprecate `gis_deg_to_distance`, redirect to gis.core |

## Data Flow

```
Input Sources              Core Processing            Output Targets
─────────────              ───────────────            ──────────────
GeoJSON/KML/SHP ─┐
Excel (wells)    ─┤  io/ ──► FeatureLayer ──► CoordinateTransformer
GEBCO stream     ─┘  handlers  (DataFrame      (pyproj: WGS84↔UTM↔local)
                                + CRS)                │
                                  │        ┌──────────┼──────────┐
                            SpatialQuery   │          │          │
                            (radius/bbox/  │          │          │
                             polygon)      │          │          │
                                     Blender 3D   Folium HTML  KML/KMZ
                                     (subprocess  (web maps)   (Google
                                      scripts)                  Earth)
                                           │          │
                                     QGIS scripts  Plotly maps
                                     (standalone)  (dashboards)
```

---

## Dependencies

**Add to pyproject.toml (required):**
```
geojson>=3.1.0,<4.0.0
```

**Add to pyproject.toml (optional `[gis]` group):**
```
shapely>=2.0.0,<3.0.0
geopandas>=0.14.0,<1.0.0
fiona>=1.9.0,<2.0.0
folium>=0.15.0,<1.0.0
rasterio>=1.3.0,<2.0.0
```

**Already available:** pyproj 3.6.1, lxml 4.9.3, pandas, numpy, plotly 5.17.0

---

## Verification

### Phase 1 Verification
```bash
uv run pytest tests/modules/gis/ -v
# Expect: ~25 passing tests for CRS, transforms, geometry, GeoJSON/KML I/O, FeatureLayer

# Round-trip test:
uv run python -c "
from digitalmodel.gis import FeatureLayer, CRSDefinition
layer = FeatureLayer.from_geojson('tests/modules/gis/test_data/sample_wells.geojson')
layer_utm = layer.reproject(CRSDefinition.utm_from_longitude(-90.5, 28.1))
print(layer_utm.data.head())
layer_utm.to_geojson('/tmp/wells_utm.geojson')
"
```

### Phase 3 Verification
```bash
# Generate interactive map:
uv run python -c "
from digitalmodel.gis import FeatureLayer, FoliumMapBuilder
layer = FeatureLayer.from_geojson('tests/modules/gis/test_data/sample_wells.geojson')
builder = FoliumMapBuilder()
builder.add_feature_layer(layer)
builder.save('/tmp/wells_map.html')
"
# Open /tmp/wells_map.html in browser — should show well markers on map
```

### Phase 4 Verification
```bash
# Generate Blender script (doesn't require Blender installed):
uv run python -c "
from digitalmodel.gis import WellLayer, BlenderGISIntegration
wells = WellLayer.from_geojson('tests/modules/gis/test_data/sample_wells.geojson')
blender_gis = BlenderGISIntegration()
result = blender_gis.create_georeferenced_scene(wells, origin=wells.centroid)
print(result['script'][:500])  # Verify valid bpy Python script
"
```

---

## Progress

| Phase | Status | Notes |
|-------|--------|-------|
| Review Iteration 1 | Pending | |
| Review Iteration 2 | Pending | |
| Review Iteration 3 | Pending | |
| Plan Approved | Pending | |
| Phase 1: Core + I/O | Pending | |
| Phase 2: Queries + Wells | Pending | |
| Phase 3: Viz + Maps | Pending | |
| Phase 4: Blender + QGIS | Pending | |
| Phase 5: Temporal | Pending | |

---

## Session Log

| Date | Session ID | Agent | Notes |
|------|------------|-------|-------|
| 2026-02-01 | cheeky-sauteeing-haven | claude-opus-4.5 | Plan created, WRK-020 |
