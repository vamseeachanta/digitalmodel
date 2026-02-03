"""Tests for SpatialQuery — spatial query operations on GIS data."""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.specialized.gis.core.geometry import GeoBoundingBox, GeoPoint
from digitalmodel.specialized.gis.core.spatial_query import SpatialQuery
from digitalmodel.specialized.gis.layers.feature_layer import FeatureLayer


# ------------------------------------------------------------------
# Fixtures
# ------------------------------------------------------------------


@pytest.fixture
def gom_origin() -> GeoPoint:
    """A central Gulf of Mexico origin point (MC-252-A coords)."""
    return GeoPoint(x=-90.5, y=28.1)


@pytest.fixture
def gom_points() -> list[GeoPoint]:
    """Well locations from the sample_wells.geojson fixture."""
    return [
        GeoPoint(x=-90.5, y=28.1),   # MC-252-A
        GeoPoint(x=-90.3, y=28.2),   # MC-253-B
        GeoPoint(x=-89.8, y=28.5),   # VK-915-C
        GeoPoint(x=-91.2, y=27.8),   # GC-640-D
        GeoPoint(x=-90.0, y=28.3),   # MC-311-E
        GeoPoint(x=-89.5, y=28.7),   # MP-306-F
        GeoPoint(x=-91.5, y=27.6),   # GC-338-G
    ]


# ------------------------------------------------------------------
# nearest_neighbors
# ------------------------------------------------------------------


class TestNearestNeighbors:
    """Find k nearest points to an origin."""

    def test_k_equals_one_returns_closest(
        self, gom_origin: GeoPoint, gom_points: list[GeoPoint]
    ) -> None:
        result = SpatialQuery.nearest_neighbors(gom_origin, gom_points, k=1)
        assert len(result) == 1
        point, dist = result[0]
        # Origin itself is in the list so distance should be 0
        assert dist == pytest.approx(0.0)
        assert point.x == pytest.approx(-90.5)
        assert point.y == pytest.approx(28.1)

    def test_k_equals_three_sorted_ascending(
        self, gom_origin: GeoPoint, gom_points: list[GeoPoint]
    ) -> None:
        result = SpatialQuery.nearest_neighbors(gom_origin, gom_points, k=3)
        assert len(result) == 3
        distances = [d for _, d in result]
        assert distances == sorted(distances)

    def test_k_greater_than_len_returns_all(
        self, gom_origin: GeoPoint, gom_points: list[GeoPoint]
    ) -> None:
        result = SpatialQuery.nearest_neighbors(gom_origin, gom_points, k=100)
        assert len(result) == len(gom_points)

    def test_empty_points_returns_empty(self, gom_origin: GeoPoint) -> None:
        result = SpatialQuery.nearest_neighbors(gom_origin, [], k=5)
        assert result == []

    def test_k_equals_all(
        self, gom_origin: GeoPoint, gom_points: list[GeoPoint]
    ) -> None:
        result = SpatialQuery.nearest_neighbors(
            gom_origin, gom_points, k=len(gom_points)
        )
        assert len(result) == len(gom_points)
        distances = [d for _, d in result]
        assert distances == sorted(distances)


# ------------------------------------------------------------------
# points_within_radius
# ------------------------------------------------------------------


class TestPointsWithinRadius:
    """Find all points within a given radius."""

    def test_zero_radius_finds_only_coincident(
        self, gom_origin: GeoPoint, gom_points: list[GeoPoint]
    ) -> None:
        result = SpatialQuery.points_within_radius(
            gom_origin, gom_points, radius_m=0.0
        )
        # Only the origin itself (distance 0)
        assert len(result) == 1
        assert result[0][1] == pytest.approx(0.0)

    def test_small_radius_finds_nearby(
        self, gom_origin: GeoPoint, gom_points: list[GeoPoint]
    ) -> None:
        # MC-253-B is about 22 km away; use 25 km radius
        result = SpatialQuery.points_within_radius(
            gom_origin, gom_points, radius_m=25_000.0
        )
        # Should include at least origin and MC-253-B
        assert len(result) >= 2
        names_x = [p.x for p, _ in result]
        assert -90.5 in [pytest.approx(x) for x in names_x]
        assert -90.3 in [pytest.approx(x) for x in names_x]

    def test_large_radius_finds_all(
        self, gom_origin: GeoPoint, gom_points: list[GeoPoint]
    ) -> None:
        # All wells are within 200 km of the origin
        result = SpatialQuery.points_within_radius(
            gom_origin, gom_points, radius_m=200_000.0
        )
        assert len(result) == len(gom_points)

    def test_results_sorted_by_distance(
        self, gom_origin: GeoPoint, gom_points: list[GeoPoint]
    ) -> None:
        result = SpatialQuery.points_within_radius(
            gom_origin, gom_points, radius_m=200_000.0
        )
        distances = [d for _, d in result]
        assert distances == sorted(distances)

    def test_empty_points(self, gom_origin: GeoPoint) -> None:
        result = SpatialQuery.points_within_radius(
            gom_origin, [], radius_m=100_000.0
        )
        assert result == []


# ------------------------------------------------------------------
# points_in_bounding_box
# ------------------------------------------------------------------


class TestPointsInBoundingBox:
    """Filter points by bounding box."""

    def test_all_inside_large_box(self, gom_points: list[GeoPoint]) -> None:
        bbox = GeoBoundingBox(min_x=-92.0, min_y=27.0, max_x=-89.0, max_y=29.0)
        result = SpatialQuery.points_in_bounding_box(gom_points, bbox)
        assert len(result) == len(gom_points)

    def test_none_inside_small_box(self, gom_points: list[GeoPoint]) -> None:
        bbox = GeoBoundingBox(min_x=-80.0, min_y=40.0, max_x=-79.0, max_y=41.0)
        result = SpatialQuery.points_in_bounding_box(gom_points, bbox)
        assert len(result) == 0

    def test_partial_filter(self, gom_points: list[GeoPoint]) -> None:
        # Box covering only the eastern wells (lon > -90.1)
        bbox = GeoBoundingBox(min_x=-90.1, min_y=27.0, max_x=-89.0, max_y=29.0)
        result = SpatialQuery.points_in_bounding_box(gom_points, bbox)
        assert 0 < len(result) < len(gom_points)
        for p in result:
            assert p.x >= -90.1

    def test_point_on_edge_included(self) -> None:
        bbox = GeoBoundingBox(min_x=0.0, min_y=0.0, max_x=10.0, max_y=10.0)
        edge_point = GeoPoint(x=0.0, y=5.0)
        result = SpatialQuery.points_in_bounding_box([edge_point], bbox)
        assert len(result) == 1


# ------------------------------------------------------------------
# point_in_polygon
# ------------------------------------------------------------------


class TestPointInPolygon:
    """Ray-casting point-in-polygon test."""

    @pytest.fixture
    def square_polygon(self) -> list[tuple[float, float]]:
        """A unit square from (0,0) to (10,10)."""
        return [(0, 0), (10, 0), (10, 10), (0, 10), (0, 0)]

    def test_inside_square(self, square_polygon: list[tuple[float, float]]) -> None:
        point = GeoPoint(x=5.0, y=5.0)
        assert SpatialQuery.point_in_polygon(point, square_polygon) is True

    def test_outside_square(self, square_polygon: list[tuple[float, float]]) -> None:
        point = GeoPoint(x=15.0, y=5.0)
        assert SpatialQuery.point_in_polygon(point, square_polygon) is False

    def test_far_outside(self, square_polygon: list[tuple[float, float]]) -> None:
        point = GeoPoint(x=-100.0, y=-100.0)
        assert SpatialQuery.point_in_polygon(point, square_polygon) is False

    def test_concave_polygon_inside(self) -> None:
        # L-shaped polygon (concave)
        polygon = [
            (0, 0), (10, 0), (10, 5), (5, 5), (5, 10), (0, 10), (0, 0)
        ]
        # Inside the lower-left part of the L
        point = GeoPoint(x=2.0, y=2.0)
        assert SpatialQuery.point_in_polygon(point, polygon) is True

    def test_concave_polygon_in_notch(self) -> None:
        # L-shaped polygon — point in the notch (outside)
        polygon = [
            (0, 0), (10, 0), (10, 5), (5, 5), (5, 10), (0, 10), (0, 0)
        ]
        point = GeoPoint(x=7.0, y=7.0)
        assert SpatialQuery.point_in_polygon(point, polygon) is False

    def test_triangle(self) -> None:
        polygon = [(0, 0), (10, 0), (5, 10), (0, 0)]
        inside = GeoPoint(x=5.0, y=3.0)
        outside = GeoPoint(x=1.0, y=9.0)
        assert SpatialQuery.point_in_polygon(inside, polygon) is True
        assert SpatialQuery.point_in_polygon(outside, polygon) is False

    def test_gom_polygon(self) -> None:
        """Test with a polygon around the Mississippi Canyon area."""
        polygon = [
            (-91.0, 27.5), (-89.0, 27.5), (-89.0, 28.5),
            (-91.0, 28.5), (-91.0, 27.5),
        ]
        inside = GeoPoint(x=-90.0, y=28.0)
        outside = GeoPoint(x=-92.0, y=28.0)
        assert SpatialQuery.point_in_polygon(inside, polygon) is True
        assert SpatialQuery.point_in_polygon(outside, polygon) is False


# ------------------------------------------------------------------
# bounding_box_overlap
# ------------------------------------------------------------------


class TestBoundingBoxOverlap:
    """Test overlap detection between two bounding boxes."""

    def test_overlapping_boxes(self) -> None:
        bbox1 = GeoBoundingBox(min_x=0, min_y=0, max_x=10, max_y=10)
        bbox2 = GeoBoundingBox(min_x=5, min_y=5, max_x=15, max_y=15)
        assert SpatialQuery.bounding_box_overlap(bbox1, bbox2) is True

    def test_no_overlap_horizontal(self) -> None:
        bbox1 = GeoBoundingBox(min_x=0, min_y=0, max_x=5, max_y=5)
        bbox2 = GeoBoundingBox(min_x=10, min_y=0, max_x=15, max_y=5)
        assert SpatialQuery.bounding_box_overlap(bbox1, bbox2) is False

    def test_no_overlap_vertical(self) -> None:
        bbox1 = GeoBoundingBox(min_x=0, min_y=0, max_x=5, max_y=5)
        bbox2 = GeoBoundingBox(min_x=0, min_y=10, max_x=5, max_y=15)
        assert SpatialQuery.bounding_box_overlap(bbox1, bbox2) is False

    def test_contained_box(self) -> None:
        outer = GeoBoundingBox(min_x=0, min_y=0, max_x=20, max_y=20)
        inner = GeoBoundingBox(min_x=5, min_y=5, max_x=10, max_y=10)
        assert SpatialQuery.bounding_box_overlap(outer, inner) is True
        assert SpatialQuery.bounding_box_overlap(inner, outer) is True

    def test_touching_edges(self) -> None:
        bbox1 = GeoBoundingBox(min_x=0, min_y=0, max_x=5, max_y=5)
        bbox2 = GeoBoundingBox(min_x=5, min_y=0, max_x=10, max_y=5)
        assert SpatialQuery.bounding_box_overlap(bbox1, bbox2) is True

    def test_touching_corners(self) -> None:
        bbox1 = GeoBoundingBox(min_x=0, min_y=0, max_x=5, max_y=5)
        bbox2 = GeoBoundingBox(min_x=5, min_y=5, max_x=10, max_y=10)
        assert SpatialQuery.bounding_box_overlap(bbox1, bbox2) is True

    def test_identical_boxes(self) -> None:
        bbox = GeoBoundingBox(min_x=0, min_y=0, max_x=10, max_y=10)
        assert SpatialQuery.bounding_box_overlap(bbox, bbox) is True

    def test_gom_region_overlap(self) -> None:
        """Two overlapping GoM regions."""
        mc_bbox = GeoBoundingBox(
            min_x=-91.0, min_y=27.5, max_x=-89.5, max_y=28.5
        )
        gc_bbox = GeoBoundingBox(
            min_x=-92.0, min_y=27.0, max_x=-90.0, max_y=28.0
        )
        assert SpatialQuery.bounding_box_overlap(mc_bbox, gc_bbox) is True


# ------------------------------------------------------------------
# filter_layer_by_proximity
# ------------------------------------------------------------------


class TestFilterLayerByProximity:
    """Filter a FeatureLayer to features near an origin."""

    def test_small_radius_filters(self, sample_wells_geojson: Path) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        origin = GeoPoint(x=-90.5, y=28.1)
        # 25 km should capture MC-252-A and MC-253-B at minimum
        filtered = SpatialQuery.filter_layer_by_proximity(
            layer, origin, radius_m=25_000.0
        )
        assert len(filtered) >= 1
        assert len(filtered) < len(layer)

    def test_large_radius_keeps_all(self, sample_wells_geojson: Path) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        origin = GeoPoint(x=-90.5, y=28.1)
        filtered = SpatialQuery.filter_layer_by_proximity(
            layer, origin, radius_m=500_000.0
        )
        assert len(filtered) == len(layer)

    def test_zero_radius_minimal(self, sample_wells_geojson: Path) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        # Origin exactly at MC-252-A
        origin = GeoPoint(x=-90.5, y=28.1)
        filtered = SpatialQuery.filter_layer_by_proximity(
            layer, origin, radius_m=0.0
        )
        # Should include exactly the coincident point
        assert len(filtered) == 1

    def test_returns_feature_layer_type(
        self, sample_wells_geojson: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        origin = GeoPoint(x=-90.5, y=28.1)
        filtered = SpatialQuery.filter_layer_by_proximity(
            layer, origin, radius_m=100_000.0
        )
        assert isinstance(filtered, FeatureLayer)

    def test_preserves_crs(self, sample_wells_geojson: Path) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        origin = GeoPoint(x=-90.5, y=28.1)
        filtered = SpatialQuery.filter_layer_by_proximity(
            layer, origin, radius_m=100_000.0
        )
        assert filtered.crs.epsg_code == layer.crs.epsg_code
