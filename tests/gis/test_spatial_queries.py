"""Tests for spatial query operations.

TDD: These tests are written before the implementation.
"""
from __future__ import annotations

import pytest

from digitalmodel.gis.core.geometry import GeoBoundingBox, GeoPoint
from digitalmodel.gis.core.spatial_query import SpatialQuery


# ---------------------------------------------------------------------------
# points_within_radius
# ---------------------------------------------------------------------------


class TestPointsWithinRadius:
    def test_find_single_nearby_point(self):
        origin = GeoPoint(x=-90.0, y=29.0)
        nearby = GeoPoint(x=-90.001, y=29.001)
        far_away = GeoPoint(x=-85.0, y=25.0)
        results = SpatialQuery.points_within_radius(origin, [nearby, far_away], 200.0)
        points = [p for p, _ in results]
        assert nearby in points
        assert far_away not in points

    def test_returns_sorted_by_distance(self):
        origin = GeoPoint(x=0.0, y=0.0)
        p1 = GeoPoint(x=0.01, y=0.0)
        p2 = GeoPoint(x=0.005, y=0.0)
        results = SpatialQuery.points_within_radius(origin, [p1, p2], 5000.0)
        assert results[0][0] == p2
        assert results[1][0] == p1

    def test_empty_input_returns_empty(self):
        origin = GeoPoint(x=0.0, y=0.0)
        results = SpatialQuery.points_within_radius(origin, [], 1000.0)
        assert results == []

    def test_distance_included_in_result(self):
        origin = GeoPoint(x=0.0, y=0.0)
        p = GeoPoint(x=0.01, y=0.0)
        results = SpatialQuery.points_within_radius(origin, [p], 5000.0)
        assert len(results) == 1
        point, distance = results[0]
        assert isinstance(distance, float)
        assert distance > 0.0


# ---------------------------------------------------------------------------
# points_in_bounding_box
# ---------------------------------------------------------------------------


class TestPointsInBoundingBox:
    def test_filters_inside_outside(self):
        bbox = GeoBoundingBox(min_x=-91.0, min_y=28.0, max_x=-89.0, max_y=30.0)
        inside = GeoPoint(x=-90.0, y=29.0)
        outside = GeoPoint(x=-95.0, y=25.0)
        results = SpatialQuery.points_in_bounding_box([inside, outside], bbox)
        assert inside in results
        assert outside not in results

    def test_edge_points_included(self):
        bbox = GeoBoundingBox(min_x=-91.0, min_y=28.0, max_x=-89.0, max_y=30.0)
        edge = GeoPoint(x=-91.0, y=28.0)
        results = SpatialQuery.points_in_bounding_box([edge], bbox)
        assert edge in results

    def test_empty_returns_empty(self):
        bbox = GeoBoundingBox(min_x=0.0, min_y=0.0, max_x=1.0, max_y=1.0)
        assert SpatialQuery.points_in_bounding_box([], bbox) == []


# ---------------------------------------------------------------------------
# point_in_polygon
# ---------------------------------------------------------------------------


class TestPointInPolygon:
    def _square(self):
        return [(-1.0, -1.0), (1.0, -1.0), (1.0, 1.0), (-1.0, 1.0)]

    def test_center_is_inside(self):
        pt = GeoPoint(x=0.0, y=0.0)
        assert SpatialQuery.point_in_polygon(pt, self._square()) is True

    def test_outside_point_is_outside(self):
        pt = GeoPoint(x=5.0, y=5.0)
        assert SpatialQuery.point_in_polygon(pt, self._square()) is False

    def test_convex_polygon(self):
        triangle = [(0.0, 0.0), (2.0, 0.0), (1.0, 2.0)]
        inside = GeoPoint(x=1.0, y=0.5)
        outside = GeoPoint(x=0.0, y=2.0)
        assert SpatialQuery.point_in_polygon(inside, triangle) is True
        assert SpatialQuery.point_in_polygon(outside, triangle) is False


# ---------------------------------------------------------------------------
# nearest_neighbors
# ---------------------------------------------------------------------------


class TestNearestNeighbors:
    def test_single_nearest(self):
        origin = GeoPoint(x=0.0, y=0.0)
        p1 = GeoPoint(x=0.01, y=0.0)
        p2 = GeoPoint(x=1.0, y=1.0)
        results = SpatialQuery.nearest_neighbors(origin, [p1, p2], k=1)
        assert len(results) == 1
        assert results[0][0] == p1

    def test_k_exceeds_available_returns_all(self):
        origin = GeoPoint(x=0.0, y=0.0)
        points = [GeoPoint(x=float(i), y=0.0) for i in range(3)]
        results = SpatialQuery.nearest_neighbors(origin, points, k=10)
        assert len(results) == 3

    def test_empty_returns_empty(self):
        origin = GeoPoint(x=0.0, y=0.0)
        assert SpatialQuery.nearest_neighbors(origin, [], k=3) == []

    def test_sorted_ascending(self):
        origin = GeoPoint(x=0.0, y=0.0)
        p1 = GeoPoint(x=0.1, y=0.0)
        p2 = GeoPoint(x=0.05, y=0.0)
        p3 = GeoPoint(x=0.2, y=0.0)
        results = SpatialQuery.nearest_neighbors(origin, [p1, p2, p3], k=3)
        distances = [d for _, d in results]
        assert distances == sorted(distances)


# ---------------------------------------------------------------------------
# bounding_box_overlap
# ---------------------------------------------------------------------------


class TestBoundingBoxOverlap:
    def test_overlapping_boxes(self):
        b1 = GeoBoundingBox(0.0, 0.0, 2.0, 2.0)
        b2 = GeoBoundingBox(1.0, 1.0, 3.0, 3.0)
        assert SpatialQuery.bounding_box_overlap(b1, b2) is True

    def test_non_overlapping_boxes(self):
        b1 = GeoBoundingBox(0.0, 0.0, 1.0, 1.0)
        b2 = GeoBoundingBox(2.0, 2.0, 3.0, 3.0)
        assert SpatialQuery.bounding_box_overlap(b1, b2) is False

    def test_touching_edge_counts_as_overlap(self):
        b1 = GeoBoundingBox(0.0, 0.0, 1.0, 1.0)
        b2 = GeoBoundingBox(1.0, 0.0, 2.0, 1.0)
        assert SpatialQuery.bounding_box_overlap(b1, b2) is True
