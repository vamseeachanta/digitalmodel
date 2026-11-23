#!/usr/bin/env python3
"""GeoPandas evaluation script — offshore engineering use cases.

Demonstrates:
1. Platform/facility location GeoDataFrame (lat/lon with attributes)
2. Pipeline route as LineString with kilometer posts
3. Exclusion zone polygons (environmental, shipping lanes)
4. Spatial join: which platforms are within an exclusion zone?
5. Distance matrix between platforms
6. CRS transformation (WGS84 → UTM for accurate distances)
7. Export to GeoJSON

Run: uv run python scripts/integrations/geopandas_evaluation.py
"""

from __future__ import annotations

import json
import sys
import tempfile
from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
from shapely.geometry import LineString, MultiPoint, Point, Polygon

SEPARATOR = "=" * 72


def section(title: str) -> None:
    print(f"\n{SEPARATOR}")
    print(f"  {title}")
    print(SEPARATOR)


def main() -> None:
    print("GeoPandas Offshore Engineering Evaluation")
    print(f"GeoPandas version: {gpd.__version__}")
    print(f"Default engine: {gpd.options.io_engine}")

    # ------------------------------------------------------------------
    # 1. Platform / facility locations
    # ------------------------------------------------------------------
    section("1. Platform & Facility Locations (WGS84)")

    platforms = gpd.GeoDataFrame(
        {
            "name": [
                "Thunder Horse",
                "Atlantis",
                "Mad Dog",
                "Na Kika",
                "Holstein",
                "Marlin",
            ],
            "operator": ["BP", "BP", "BP", "BP/Shell", "BP", "BP"],
            "water_depth_m": [1829, 2134, 1311, 1920, 1325, 986],
            "platform_type": ["Semi", "Semi", "Spar", "Semi", "Spar", "TLP"],
            "production_bopd": [250_000, 200_000, 140_000, 130_000, 100_000, 60_000],
        },
        geometry=[
            Point(-88.50, 28.19),  # Thunder Horse
            Point(-90.03, 27.19),  # Atlantis
            Point(-90.35, 27.28),  # Mad Dog
            Point(-89.70, 26.95),  # Na Kika
            Point(-90.52, 27.23),  # Holstein
            Point(-87.85, 29.27),  # Marlin
        ],
        crs="EPSG:4326",
    )

    print(platforms.to_string())
    print(f"\nCRS: {platforms.crs}")
    print(f"Bounds:\n{platforms.total_bounds}")

    # ------------------------------------------------------------------
    # 2. Pipeline route as LineString with KP markers
    # ------------------------------------------------------------------
    section("2. Pipeline Route (LineString) with Kilometer Posts")

    route_coords = [
        (-88.50, 28.19),
        (-88.80, 27.90),
        (-89.20, 27.60),
        (-89.50, 27.35),
        (-89.70, 26.95),
    ]
    pipeline_route = LineString(route_coords)

    pipeline_gdf = gpd.GeoDataFrame(
        {
            "name": ["Thunder Horse to Na Kika Export"],
            "diameter_in": [18],
            "product": ["Oil"],
        },
        geometry=[pipeline_route],
        crs="EPSG:4326",
    )

    # KP markers along the route (every ~25% of length)
    fractions = np.linspace(0, 1, 5)
    kp_points = [pipeline_route.interpolate(f, normalized=True) for f in fractions]
    kp_gdf = gpd.GeoDataFrame(
        {"kp_fraction": fractions, "label": [f"KP-{i}" for i in range(len(fractions))]},
        geometry=kp_points,
        crs="EPSG:4326",
    )

    print(f"Pipeline length (degrees): {pipeline_route.length:.4f}")
    print(f"Pipeline:\n{pipeline_gdf.to_string()}")
    print(f"\nKilometer Posts:\n{kp_gdf.to_string()}")

    # ------------------------------------------------------------------
    # 3. Exclusion zone polygons
    # ------------------------------------------------------------------
    section("3. Exclusion Zones (Environmental & Shipping)")

    env_zone = Polygon(
        [
            (-90.60, 27.40),
            (-90.60, 27.10),
            (-90.10, 27.10),
            (-90.10, 27.40),
            (-90.60, 27.40),
        ]
    )

    shipping_lane = Polygon(
        [
            (-89.00, 28.30),
            (-89.00, 26.80),
            (-88.60, 26.80),
            (-88.60, 28.30),
            (-89.00, 28.30),
        ]
    )

    zones_gdf = gpd.GeoDataFrame(
        {
            "zone_name": ["Chemosynthetic Community", "Fairway Shipping Lane"],
            "zone_type": ["environmental", "shipping"],
            "restriction": ["No anchoring", "No permanent structures"],
        },
        geometry=[env_zone, shipping_lane],
        crs="EPSG:4326",
    )

    print(zones_gdf[["zone_name", "zone_type", "restriction"]].to_string())
    for _, row in zones_gdf.iterrows():
        print(f"  {row['zone_name']} area (deg²): {row.geometry.area:.4f}")

    # ------------------------------------------------------------------
    # 4. Spatial join — platforms within exclusion zones
    # ------------------------------------------------------------------
    section("4. Spatial Join: Platforms in Exclusion Zones")

    joined = gpd.sjoin(platforms, zones_gdf, how="inner", predicate="within")
    if joined.empty:
        print("No platforms fall within any exclusion zone.")
    else:
        print("Platforms within exclusion zones:")
        print(joined[["name", "zone_name", "zone_type"]].to_string())

    # Also check intersections (platforms whose 0.1° buffer touches a zone)
    platforms_buffered = platforms.copy()
    platforms_buffered["geometry"] = platforms.geometry.buffer(0.15)
    near_zones = gpd.sjoin(platforms_buffered, zones_gdf, how="inner", predicate="intersects")
    print("\nPlatforms whose 0.15° buffer intersects a zone:")
    if near_zones.empty:
        print("  (none)")
    else:
        print(near_zones[["name", "zone_name"]].to_string())

    # ------------------------------------------------------------------
    # 5. Distance matrix between platforms (geographic degrees)
    # ------------------------------------------------------------------
    section("5. Distance Matrix (WGS84 — degrees, approximate)")

    n = len(platforms)
    names = platforms["name"].values
    dist_deg = np.zeros((n, n))
    for i in range(n):
        for j in range(n):
            dist_deg[i, j] = platforms.geometry.iloc[i].distance(platforms.geometry.iloc[j])

    dist_df = pd.DataFrame(dist_deg, index=names, columns=names).round(4)
    print(dist_df.to_string())

    # ------------------------------------------------------------------
    # 6. CRS transformation — WGS84 → UTM Zone 16N for accurate distances
    # ------------------------------------------------------------------
    section("6. CRS Transformation: WGS84 → UTM 16N (EPSG:32616)")

    platforms_utm = platforms.to_crs(epsg=32616)
    print(f"CRS after transform: {platforms_utm.crs}")
    print(f"Easting range: {platforms_utm.geometry.x.min():.0f} – {platforms_utm.geometry.x.max():.0f} m")
    print(f"Northing range: {platforms_utm.geometry.y.min():.0f} – {platforms_utm.geometry.y.max():.0f} m")

    # Accurate distance matrix in kilometers
    dist_m = np.zeros((n, n))
    for i in range(n):
        for j in range(n):
            dist_m[i, j] = platforms_utm.geometry.iloc[i].distance(platforms_utm.geometry.iloc[j])

    dist_km_df = pd.DataFrame(dist_m / 1000.0, index=names, columns=names).round(1)
    print("\nDistance matrix (km, UTM-projected):")
    print(dist_km_df.to_string())

    # ------------------------------------------------------------------
    # 7. Export to GeoJSON
    # ------------------------------------------------------------------
    section("7. Export to GeoJSON")

    with tempfile.TemporaryDirectory() as tmpdir:
        out_dir = Path(tmpdir)

        platforms_path = out_dir / "platforms.geojson"
        pipeline_path = out_dir / "pipeline.geojson"
        zones_path = out_dir / "exclusion_zones.geojson"

        platforms.to_file(platforms_path, driver="GeoJSON")
        pipeline_gdf.to_file(pipeline_path, driver="GeoJSON")
        zones_gdf.to_file(zones_path, driver="GeoJSON")

        for p in [platforms_path, pipeline_path, zones_path]:
            size = p.stat().st_size
            with open(p) as f:
                data = json.load(f)
            n_features = len(data["features"])
            print(f"  {p.name}: {size:,} bytes, {n_features} features")

        # Round-trip verification
        reloaded = gpd.read_file(platforms_path)
        assert len(reloaded) == len(platforms), "Round-trip feature count mismatch"
        assert reloaded.crs == platforms.crs, "Round-trip CRS mismatch"
        print("\n  Round-trip GeoJSON verification: PASSED")

    # ------------------------------------------------------------------
    # Bonus: union_all() for merged exclusion zone
    # ------------------------------------------------------------------
    section("Bonus: union_all() — Merged Exclusion Zone")

    merged = zones_gdf.union_all()
    print(f"Geometry type: {merged.geom_type}")
    print(f"Area (deg²): {merged.area:.4f}")
    print(f"Bounds: {merged.bounds}")

    # Check which pipeline segments cross the merged zone
    if pipeline_route.intersects(merged):
        clipped = pipeline_route.intersection(merged)
        print(f"\nPipeline crosses merged exclusion zone!")
        print(f"  Intersection type: {clipped.geom_type}")
        print(f"  Intersection length (deg): {clipped.length:.4f}")
    else:
        print("\nPipeline does not cross any exclusion zone.")

    print(f"\n{SEPARATOR}")
    print("  EVALUATION COMPLETE — All demonstrations successful")
    print(SEPARATOR)


if __name__ == "__main__":
    main()
