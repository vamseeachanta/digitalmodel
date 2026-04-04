"""GeoPandas integration tests for digitalmodel.

Tests cover:
- Import / version checks
- GeoDataFrame creation (Points, LineStrings, Polygons)
- CRS assignment and transformation
- Spatial predicates (contains, intersects, within)
- Buffer operations
- Distance calculations
- GeoJSON I/O round-trip (uses tmp_path)
- Spatial join
- union_all() (NOT deprecated unary_union)
- Attribute filtering + spatial operations
- Edge cases: empty GeoDataFrame, single point, antimeridian

Run: uv run pytest tests/test_geopandas_integration.py -v
"""

from __future__ import annotations

import json

import geopandas as gpd
import numpy as np
import pandas as pd
import pytest
from shapely.geometry import LineString, MultiPolygon, Point, Polygon, box


# ---------------------------------------------------------------------------
# 1. Import & version
# ---------------------------------------------------------------------------
class TestImportAndVersion:
    def test_geopandas_importable(self):
        """GeoPandas can be imported."""
        import geopandas  # noqa: F811

        assert geopandas is not None

    def test_geopandas_version(self):
        """Installed version is >= 1.1."""
        major, minor, *_ = gpd.__version__.split(".")
        assert int(major) >= 1
        assert int(minor) >= 1

    def test_shapely_available(self):
        """Shapely (geometry engine) is available."""
        from shapely import __version__ as shp_ver

        assert shp_ver is not None


# ---------------------------------------------------------------------------
# 2. GeoDataFrame creation
# ---------------------------------------------------------------------------
class TestGeoDataFrameCreation:
    def test_create_from_points(self):
        """Create GeoDataFrame from Point geometries."""
        gdf = gpd.GeoDataFrame(
            {"name": ["A", "B"], "val": [1, 2]},
            geometry=[Point(0, 0), Point(1, 1)],
            crs="EPSG:4326",
        )
        assert len(gdf) == 2
        assert gdf.geometry.name == "geometry"
        assert gdf.crs.to_epsg() == 4326

    def test_create_from_linestrings(self):
        """Create GeoDataFrame with LineString geometries."""
        line = LineString([(0, 0), (1, 1), (2, 0)])
        gdf = gpd.GeoDataFrame({"id": [1]}, geometry=[line], crs="EPSG:4326")
        assert gdf.geometry.iloc[0].geom_type == "LineString"
        assert gdf.geometry.iloc[0].length == pytest.approx(line.length)

    def test_create_from_polygons(self):
        """Create GeoDataFrame with Polygon geometries."""
        poly = Polygon([(0, 0), (1, 0), (1, 1), (0, 1)])
        gdf = gpd.GeoDataFrame({"zone": ["test"]}, geometry=[poly], crs="EPSG:4326")
        assert gdf.geometry.iloc[0].geom_type == "Polygon"
        assert gdf.geometry.iloc[0].area == pytest.approx(1.0)

    def test_create_from_xy_columns(self):
        """Create GeoDataFrame from lon/lat columns via points_from_xy."""
        df = pd.DataFrame({"lon": [-90.0, -89.0], "lat": [28.0, 27.0], "tag": ["X", "Y"]})
        gdf = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df.lon, df.lat), crs="EPSG:4326")
        assert len(gdf) == 2
        assert gdf.geometry.iloc[0].x == pytest.approx(-90.0)


# ---------------------------------------------------------------------------
# 3. CRS assignment & transformation
# ---------------------------------------------------------------------------
class TestCRS:
    @pytest.fixture()
    def platforms_wgs84(self) -> gpd.GeoDataFrame:
        return gpd.GeoDataFrame(
            {"name": ["P1", "P2"]},
            geometry=[Point(-88.5, 28.2), Point(-90.0, 27.2)],
            crs="EPSG:4326",
        )

    def test_crs_assignment(self, platforms_wgs84):
        """CRS is correctly assigned on creation."""
        assert platforms_wgs84.crs is not None
        assert platforms_wgs84.crs.to_epsg() == 4326

    def test_crs_transform_to_utm(self, platforms_wgs84):
        """Transform WGS84 → UTM Zone 16N preserves feature count."""
        utm = platforms_wgs84.to_crs(epsg=32616)
        assert utm.crs.to_epsg() == 32616
        assert len(utm) == len(platforms_wgs84)
        # UTM coordinates should be in meters (large positive values)
        assert utm.geometry.iloc[0].x > 100_000
        assert utm.geometry.iloc[0].y > 3_000_000

    def test_crs_roundtrip(self, platforms_wgs84):
        """WGS84 → UTM → WGS84 round-trip preserves coordinates."""
        original_x = platforms_wgs84.geometry.iloc[0].x
        roundtrip = platforms_wgs84.to_crs(epsg=32616).to_crs(epsg=4326)
        assert roundtrip.geometry.iloc[0].x == pytest.approx(original_x, abs=1e-6)


# ---------------------------------------------------------------------------
# 4. Spatial predicates
# ---------------------------------------------------------------------------
class TestSpatialPredicates:
    @pytest.fixture()
    def zone_and_points(self):
        zone = Polygon([(-1, -1), (1, -1), (1, 1), (-1, 1)])
        inside = Point(0, 0)
        outside = Point(5, 5)
        on_edge = Point(1, 0)
        return zone, inside, outside, on_edge

    def test_contains(self, zone_and_points):
        zone, inside, outside, _ = zone_and_points
        assert zone.contains(inside)
        assert not zone.contains(outside)

    def test_intersects(self, zone_and_points):
        zone, inside, _, on_edge = zone_and_points
        assert zone.intersects(inside)
        assert zone.intersects(on_edge)

    def test_within(self, zone_and_points):
        zone, inside, outside, _ = zone_and_points
        assert inside.within(zone)
        assert not outside.within(zone)

    def test_gdf_spatial_predicate(self):
        """GeoDataFrame-level spatial query with .sindex."""
        polys = gpd.GeoDataFrame(
            {"id": [1]}, geometry=[box(0, 0, 2, 2)], crs="EPSG:4326"
        )
        pts = gpd.GeoDataFrame(
            {"name": ["in", "out"]},
            geometry=[Point(1, 1), Point(5, 5)],
            crs="EPSG:4326",
        )
        within = pts[pts.geometry.within(polys.union_all())]
        assert len(within) == 1
        assert within.iloc[0]["name"] == "in"


# ---------------------------------------------------------------------------
# 5. Buffer operations
# ---------------------------------------------------------------------------
class TestBuffer:
    def test_point_buffer_creates_polygon(self):
        gdf = gpd.GeoDataFrame({"id": [1]}, geometry=[Point(0, 0)], crs="EPSG:4326")
        buffered = gdf.buffer(1.0)
        assert buffered.iloc[0].geom_type == "Polygon"
        assert buffered.iloc[0].area > 0

    def test_buffer_distance(self):
        """Buffer in UTM meters gives expected area (π r²)."""
        gdf = gpd.GeoDataFrame({"id": [1]}, geometry=[Point(500000, 3100000)], crs="EPSG:32616")
        radius = 1000  # 1 km
        buffered = gdf.buffer(radius)
        expected_area = np.pi * radius**2
        assert buffered.iloc[0].area == pytest.approx(expected_area, rel=0.01)


# ---------------------------------------------------------------------------
# 6. Distance calculations
# ---------------------------------------------------------------------------
class TestDistance:
    def test_distance_between_points(self):
        """Distance in projected CRS returns meters."""
        gdf = gpd.GeoDataFrame(
            {"name": ["A", "B"]},
            geometry=[Point(500000, 3100000), Point(501000, 3100000)],
            crs="EPSG:32616",
        )
        d = gdf.geometry.iloc[0].distance(gdf.geometry.iloc[1])
        assert d == pytest.approx(1000.0)

    def test_distance_matrix(self):
        """Build pairwise distance matrix."""
        pts = [Point(0, 0), Point(3, 0), Point(0, 4)]
        gdf = gpd.GeoDataFrame({"id": range(3)}, geometry=pts)
        n = len(gdf)
        mat = np.zeros((n, n))
        for i in range(n):
            for j in range(n):
                mat[i, j] = gdf.geometry.iloc[i].distance(gdf.geometry.iloc[j])
        # 3-4-5 triangle
        assert mat[0, 1] == pytest.approx(3.0)
        assert mat[0, 2] == pytest.approx(4.0)
        assert mat[1, 2] == pytest.approx(5.0)


# ---------------------------------------------------------------------------
# 7. GeoJSON I/O
# ---------------------------------------------------------------------------
class TestGeoJSONIO:
    def test_write_and_read_geojson(self, tmp_path):
        """Round-trip GeoDataFrame through GeoJSON file."""
        gdf = gpd.GeoDataFrame(
            {"name": ["Alpha", "Bravo"], "value": [10, 20]},
            geometry=[Point(1, 2), Point(3, 4)],
            crs="EPSG:4326",
        )
        path = tmp_path / "test.geojson"
        gdf.to_file(path, driver="GeoJSON")
        reloaded = gpd.read_file(path)

        assert len(reloaded) == 2
        assert reloaded.crs.to_epsg() == 4326
        assert set(reloaded["name"]) == {"Alpha", "Bravo"}

    def test_geojson_content_structure(self, tmp_path):
        """Written GeoJSON has correct FeatureCollection structure."""
        gdf = gpd.GeoDataFrame(
            {"id": [1]}, geometry=[Point(0, 0)], crs="EPSG:4326"
        )
        path = tmp_path / "struct.geojson"
        gdf.to_file(path, driver="GeoJSON")

        with open(path) as f:
            data = json.load(f)
        assert data["type"] == "FeatureCollection"
        assert len(data["features"]) == 1
        assert data["features"][0]["geometry"]["type"] == "Point"

    def test_linestring_geojson_roundtrip(self, tmp_path):
        """LineString survives GeoJSON round-trip."""
        line = LineString([(0, 0), (1, 1), (2, 0)])
        gdf = gpd.GeoDataFrame({"id": [1]}, geometry=[line], crs="EPSG:4326")
        path = tmp_path / "line.geojson"
        gdf.to_file(path, driver="GeoJSON")
        reloaded = gpd.read_file(path)
        assert reloaded.geometry.iloc[0].geom_type == "LineString"
        assert reloaded.geometry.iloc[0].length == pytest.approx(line.length, rel=1e-6)


# ---------------------------------------------------------------------------
# 8. Spatial join
# ---------------------------------------------------------------------------
class TestSpatialJoin:
    def test_sjoin_within(self):
        """Spatial join identifies points within polygons."""
        zones = gpd.GeoDataFrame(
            {"zone": ["A", "B"]},
            geometry=[box(0, 0, 2, 2), box(5, 5, 7, 7)],
            crs="EPSG:4326",
        )
        pts = gpd.GeoDataFrame(
            {"name": ["in_A", "in_B", "nowhere"]},
            geometry=[Point(1, 1), Point(6, 6), Point(10, 10)],
            crs="EPSG:4326",
        )
        result = gpd.sjoin(pts, zones, how="inner", predicate="within")
        assert len(result) == 2
        assert set(result["zone"]) == {"A", "B"}

    def test_sjoin_intersects(self):
        """Spatial join with 'intersects' predicate."""
        line_gdf = gpd.GeoDataFrame(
            {"pipe": ["L1"]},
            geometry=[LineString([(0, 0), (3, 3)])],
            crs="EPSG:4326",
        )
        zone_gdf = gpd.GeoDataFrame(
            {"zone": ["Z1"]},
            geometry=[box(1, 1, 2, 2)],
            crs="EPSG:4326",
        )
        result = gpd.sjoin(line_gdf, zone_gdf, how="inner", predicate="intersects")
        assert len(result) == 1


# ---------------------------------------------------------------------------
# 9. union_all() — NOT deprecated unary_union
# ---------------------------------------------------------------------------
class TestUnionAll:
    def test_union_all_polygons(self):
        """union_all() merges multiple polygons into single geometry."""
        gdf = gpd.GeoDataFrame(
            {"id": [1, 2]},
            geometry=[box(0, 0, 2, 2), box(1, 1, 3, 3)],
            crs="EPSG:4326",
        )
        merged = gdf.union_all()
        assert merged is not None
        assert merged.geom_type == "Polygon"
        # Merged area < sum of parts (overlap)
        assert merged.area < gdf.geometry.area.sum()

    def test_union_all_disjoint(self):
        """union_all() of disjoint polygons → MultiPolygon."""
        gdf = gpd.GeoDataFrame(
            {"id": [1, 2]},
            geometry=[box(0, 0, 1, 1), box(5, 5, 6, 6)],
            crs="EPSG:4326",
        )
        merged = gdf.union_all()
        assert merged.geom_type == "MultiPolygon"
        assert merged.area == pytest.approx(2.0)


# ---------------------------------------------------------------------------
# 10. Attribute filtering + spatial
# ---------------------------------------------------------------------------
class TestFilterAndSpatial:
    def test_attribute_filter_then_spatial(self):
        """Filter by attribute, then do spatial operation."""
        gdf = gpd.GeoDataFrame(
            {
                "name": ["P1", "P2", "P3"],
                "depth_m": [500, 1500, 2500],
            },
            geometry=[Point(0, 0), Point(1, 1), Point(2, 2)],
            crs="EPSG:4326",
        )
        deepwater = gdf[gdf["depth_m"] > 1000]
        assert len(deepwater) == 2
        merged = deepwater.union_all()
        assert merged.geom_type == "MultiPoint"

    def test_spatial_filter_then_attribute(self):
        """Spatial query first, then attribute filter."""
        zone = box(0, 0, 1.5, 1.5)
        gdf = gpd.GeoDataFrame(
            {
                "name": ["A", "B", "C"],
                "production": [100, 200, 300],
            },
            geometry=[Point(1, 1), Point(2, 2), Point(0.5, 0.5)],
            crs="EPSG:4326",
        )
        in_zone = gdf[gdf.geometry.within(zone)]
        high_prod = in_zone[in_zone["production"] > 150]
        assert len(in_zone) == 2  # A(1,1) and C(0.5,0.5)
        # A production=100, C production=300 → high_prod has C only
        assert len(high_prod) == 1
        assert high_prod.iloc[0]["name"] == "C"


# ---------------------------------------------------------------------------
# 11. Edge cases
# ---------------------------------------------------------------------------
class TestEdgeCases:
    def test_empty_geodataframe(self):
        """Empty GeoDataFrame operations don't error."""
        gdf = gpd.GeoDataFrame({"id": pd.Series([], dtype="int64")}, geometry=[], crs="EPSG:4326")
        assert len(gdf) == 0
        assert gdf.crs.to_epsg() == 4326
        # Operations on empty should return empty
        buffered = gdf.buffer(1.0)
        assert len(buffered) == 0

    def test_single_point_geodataframe(self):
        """Single-point GeoDataFrame works for all basic operations."""
        gdf = gpd.GeoDataFrame({"id": [1]}, geometry=[Point(0, 0)], crs="EPSG:4326")
        assert len(gdf) == 1
        centroid = gdf.geometry.centroid
        assert centroid.iloc[0].equals(Point(0, 0))
        merged = gdf.union_all()
        assert merged.geom_type == "Point"

    def test_antimeridian_crossing(self):
        """Points near antimeridian (+/- 180°) are handled."""
        gdf = gpd.GeoDataFrame(
            {"name": ["east", "west"]},
            geometry=[Point(179.5, 0), Point(-179.5, 0)],
            crs="EPSG:4326",
        )
        # In WGS84, these are valid coordinates
        assert len(gdf) == 2
        # Distance in degrees wraps around (Shapely doesn't handle antimeridian)
        # so distance will be ~359° not ~1° — this is a known limitation
        d = gdf.geometry.iloc[0].distance(gdf.geometry.iloc[1])
        assert d > 300  # ~359 degrees, confirming the limitation

    def test_duplicate_geometries(self):
        """Duplicate geometries are preserved (no dedup)."""
        gdf = gpd.GeoDataFrame(
            {"id": [1, 2, 3]},
            geometry=[Point(0, 0), Point(0, 0), Point(1, 1)],
            crs="EPSG:4326",
        )
        assert len(gdf) == 3
        unique_pts = gdf.union_all()
        # Two of three points are identical → MultiPoint of 2
        assert unique_pts.geom_type == "MultiPoint"
