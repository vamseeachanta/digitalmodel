"""Tests for digitalmodel.modules.gis.core.geometry."""

from __future__ import annotations

import math

import pytest

from digitalmodel.modules.gis.core.geometry import (
    EARTH_RADIUS_METERS,
    GeoBoundingBox,
    GeoPoint,
)


# ===================================================================
# GeoPoint creation
# ===================================================================


class TestGeoPointCreation:
    def test_geopoint_creation_with_xy_only_stores_coordinates(self) -> None:
        pt = GeoPoint(x=1.0, y=2.0)

        assert pt.x == 1.0
        assert pt.y == 2.0
        assert pt.z is None
        assert pt.properties is None

    def test_geopoint_creation_with_xyz_stores_all_coordinates(self) -> None:
        pt = GeoPoint(x=1.0, y=2.0, z=3.0)

        assert pt.x == 1.0
        assert pt.y == 2.0
        assert pt.z == 3.0

    def test_geopoint_creation_with_properties_stores_properties(self) -> None:
        props = {"name": "wellhead", "depth": 100}
        pt = GeoPoint(x=1.0, y=2.0, properties=props)

        assert pt.properties == props


# ===================================================================
# GeoPoint convenience properties
# ===================================================================


class TestGeoPointProperties:
    def test_geopoint_longitude_returns_x(self) -> None:
        pt = GeoPoint(x=-90.5, y=28.1)

        assert pt.longitude == -90.5

    def test_geopoint_latitude_returns_y(self) -> None:
        pt = GeoPoint(x=-90.5, y=28.1)

        assert pt.latitude == 28.1

    def test_geopoint_elevation_returns_z_when_set(self) -> None:
        pt = GeoPoint(x=0.0, y=0.0, z=150.0)

        assert pt.elevation == 150.0

    def test_geopoint_elevation_returns_none_when_not_set(self) -> None:
        pt = GeoPoint(x=0.0, y=0.0)

        assert pt.elevation is None


# ===================================================================
# GeoPoint.distance_to (haversine)
# ===================================================================


class TestGeoPointDistanceHaversine:
    def test_geopoint_distance_to_haversine_known_pair_returns_approximately_23km(
        self,
    ) -> None:
        """(-90.5, 28.1) to (-90.3, 28.2) should be ~23 km."""
        pt_a = GeoPoint(x=-90.5, y=28.1)
        pt_b = GeoPoint(x=-90.3, y=28.2)

        distance_m = pt_a.distance_to(pt_b, method="haversine")

        assert abs(distance_m - 23_000) < 1_000  # tolerance 1 km

    def test_geopoint_distance_to_haversine_same_point_returns_zero(self) -> None:
        pt = GeoPoint(x=-90.5, y=28.1)

        distance_m = pt.distance_to(pt, method="haversine")

        assert distance_m == pytest.approx(0.0, abs=1e-9)

    def test_geopoint_distance_to_haversine_is_default_method(self) -> None:
        pt_a = GeoPoint(x=-90.5, y=28.1)
        pt_b = GeoPoint(x=-90.3, y=28.2)

        distance_default = pt_a.distance_to(pt_b)
        distance_haversine = pt_a.distance_to(pt_b, method="haversine")

        assert distance_default == distance_haversine

    def test_geopoint_distance_to_haversine_antipodal_returns_half_circumference(
        self,
    ) -> None:
        pt_a = GeoPoint(x=0.0, y=0.0)
        pt_b = GeoPoint(x=180.0, y=0.0)

        distance_m = pt_a.distance_to(pt_b, method="haversine")
        expected = math.pi * EARTH_RADIUS_METERS

        assert distance_m == pytest.approx(expected, rel=1e-9)


# ===================================================================
# GeoPoint.distance_to (euclidean)
# ===================================================================


class TestGeoPointDistanceEuclidean:
    def test_geopoint_distance_to_euclidean_2d_returns_correct_distance(self) -> None:
        pt_a = GeoPoint(x=0.0, y=0.0)
        pt_b = GeoPoint(x=3.0, y=4.0)

        distance = pt_a.distance_to(pt_b, method="euclidean")

        assert distance == pytest.approx(5.0)

    def test_geopoint_distance_to_euclidean_3d_returns_correct_distance(self) -> None:
        pt_a = GeoPoint(x=0.0, y=0.0, z=0.0)
        pt_b = GeoPoint(x=1.0, y=2.0, z=2.0)

        distance = pt_a.distance_to(pt_b, method="euclidean")

        assert distance == pytest.approx(3.0)

    def test_geopoint_distance_to_euclidean_mixed_z_falls_back_to_2d(self) -> None:
        pt_a = GeoPoint(x=0.0, y=0.0, z=10.0)
        pt_b = GeoPoint(x=3.0, y=4.0)  # z is None

        distance = pt_a.distance_to(pt_b, method="euclidean")

        assert distance == pytest.approx(5.0)

    def test_geopoint_distance_to_unknown_method_raises_value_error(self) -> None:
        pt_a = GeoPoint(x=0.0, y=0.0)
        pt_b = GeoPoint(x=1.0, y=1.0)

        with pytest.raises(ValueError, match="Unknown distance method"):
            pt_a.distance_to(pt_b, method="manhattan")


# ===================================================================
# GeoPoint serialization
# ===================================================================


class TestGeoPointSerialization:
    def test_geopoint_to_dict_2d_returns_geojson_point(self) -> None:
        pt = GeoPoint(x=-90.5, y=28.1)

        result = pt.to_dict()

        assert result == {
            "type": "Point",
            "coordinates": [-90.5, 28.1],
        }

    def test_geopoint_to_dict_3d_includes_z_in_coordinates(self) -> None:
        pt = GeoPoint(x=-90.5, y=28.1, z=100.0)

        result = pt.to_dict()

        assert result["coordinates"] == [-90.5, 28.1, 100.0]

    def test_geopoint_to_dict_with_properties_includes_properties(self) -> None:
        props = {"name": "platform"}
        pt = GeoPoint(x=1.0, y=2.0, properties=props)

        result = pt.to_dict()

        assert result["properties"] == props

    def test_geopoint_to_dict_without_properties_omits_properties_key(self) -> None:
        pt = GeoPoint(x=1.0, y=2.0)

        result = pt.to_dict()

        assert "properties" not in result

    def test_geopoint_from_dict_round_trip_2d_preserves_data(self) -> None:
        original = GeoPoint(x=-90.5, y=28.1)

        reconstructed = GeoPoint.from_dict(original.to_dict())

        assert reconstructed.x == original.x
        assert reconstructed.y == original.y
        assert reconstructed.z is None

    def test_geopoint_from_dict_round_trip_3d_preserves_data(self) -> None:
        original = GeoPoint(x=-90.5, y=28.1, z=50.0)

        reconstructed = GeoPoint.from_dict(original.to_dict())

        assert reconstructed.x == original.x
        assert reconstructed.y == original.y
        assert reconstructed.z == original.z

    def test_geopoint_from_dict_round_trip_with_properties_preserves_data(self) -> None:
        original = GeoPoint(x=1.0, y=2.0, properties={"id": 42})

        reconstructed = GeoPoint.from_dict(original.to_dict())

        assert reconstructed.properties == {"id": 42}


# ===================================================================
# GeoBoundingBox creation and contains
# ===================================================================


class TestGeoBoundingBoxContains:
    def test_geoboundingbox_creation_stores_bounds(self) -> None:
        bbox = GeoBoundingBox(min_x=-1.0, min_y=-2.0, max_x=3.0, max_y=4.0)

        assert bbox.min_x == -1.0
        assert bbox.min_y == -2.0
        assert bbox.max_x == 3.0
        assert bbox.max_y == 4.0

    def test_geoboundingbox_contains_interior_point_returns_true(self) -> None:
        bbox = GeoBoundingBox(min_x=0.0, min_y=0.0, max_x=10.0, max_y=10.0)
        pt = GeoPoint(x=5.0, y=5.0)

        assert bbox.contains(pt) is True

    def test_geoboundingbox_contains_edge_point_returns_true(self) -> None:
        bbox = GeoBoundingBox(min_x=0.0, min_y=0.0, max_x=10.0, max_y=10.0)
        pt = GeoPoint(x=0.0, y=5.0)

        assert bbox.contains(pt) is True

    def test_geoboundingbox_contains_corner_point_returns_true(self) -> None:
        bbox = GeoBoundingBox(min_x=0.0, min_y=0.0, max_x=10.0, max_y=10.0)
        pt = GeoPoint(x=0.0, y=0.0)

        assert bbox.contains(pt) is True

    def test_geoboundingbox_contains_outside_point_returns_false(self) -> None:
        bbox = GeoBoundingBox(min_x=0.0, min_y=0.0, max_x=10.0, max_y=10.0)
        pt = GeoPoint(x=15.0, y=5.0)

        assert bbox.contains(pt) is False


# ===================================================================
# GeoBoundingBox.expand
# ===================================================================


class TestGeoBoundingBoxExpand:
    def test_geoboundingbox_expand_increases_bounds_by_margin(self) -> None:
        bbox = GeoBoundingBox(min_x=0.0, min_y=0.0, max_x=10.0, max_y=10.0)

        expanded = bbox.expand(margin=2.0)

        assert expanded.min_x == -2.0
        assert expanded.min_y == -2.0
        assert expanded.max_x == 12.0
        assert expanded.max_y == 12.0

    def test_geoboundingbox_expand_returns_new_instance(self) -> None:
        bbox = GeoBoundingBox(min_x=0.0, min_y=0.0, max_x=10.0, max_y=10.0)

        expanded = bbox.expand(margin=1.0)

        assert expanded is not bbox

    def test_geoboundingbox_expand_does_not_mutate_original(self) -> None:
        bbox = GeoBoundingBox(min_x=0.0, min_y=0.0, max_x=10.0, max_y=10.0)

        bbox.expand(margin=5.0)

        assert bbox.min_x == 0.0
        assert bbox.max_x == 10.0


# ===================================================================
# GeoBoundingBox.center
# ===================================================================


class TestGeoBoundingBoxCenter:
    def test_geoboundingbox_center_returns_midpoint(self) -> None:
        bbox = GeoBoundingBox(min_x=0.0, min_y=0.0, max_x=10.0, max_y=20.0)

        center = bbox.center()

        assert center.x == pytest.approx(5.0)
        assert center.y == pytest.approx(10.0)

    def test_geoboundingbox_center_with_negative_coords_returns_midpoint(self) -> None:
        bbox = GeoBoundingBox(min_x=-10.0, min_y=-4.0, max_x=10.0, max_y=4.0)

        center = bbox.center()

        assert center.x == pytest.approx(0.0)
        assert center.y == pytest.approx(0.0)

    def test_geoboundingbox_center_returns_geopoint(self) -> None:
        bbox = GeoBoundingBox(min_x=0.0, min_y=0.0, max_x=10.0, max_y=10.0)

        center = bbox.center()

        assert isinstance(center, GeoPoint)


# ===================================================================
# GeoBoundingBox.to_polygon_coords
# ===================================================================


class TestGeoBoundingBoxToPolygonCoords:
    def test_geoboundingbox_to_polygon_coords_returns_five_closed_ring_coords(
        self,
    ) -> None:
        bbox = GeoBoundingBox(min_x=0.0, min_y=0.0, max_x=10.0, max_y=20.0)

        coords = bbox.to_polygon_coords()

        assert len(coords) == 5

    def test_geoboundingbox_to_polygon_coords_first_equals_last(self) -> None:
        bbox = GeoBoundingBox(min_x=0.0, min_y=0.0, max_x=10.0, max_y=20.0)

        coords = bbox.to_polygon_coords()

        assert coords[0] == coords[-1]

    def test_geoboundingbox_to_polygon_coords_returns_correct_vertices(self) -> None:
        bbox = GeoBoundingBox(min_x=1.0, min_y=2.0, max_x=3.0, max_y=4.0)

        coords = bbox.to_polygon_coords()

        assert coords == [
            (1.0, 2.0),  # SW
            (3.0, 2.0),  # SE
            (3.0, 4.0),  # NE
            (1.0, 4.0),  # NW
            (1.0, 2.0),  # SW (closed)
        ]


# ===================================================================
# GeoBoundingBox.from_points
# ===================================================================


class TestGeoBoundingBoxFromPoints:
    def test_geoboundingbox_from_points_computes_correct_bounds(self) -> None:
        points = [
            GeoPoint(x=-5.0, y=2.0),
            GeoPoint(x=10.0, y=-3.0),
            GeoPoint(x=3.0, y=7.0),
        ]

        bbox = GeoBoundingBox.from_points(points)

        assert bbox.min_x == -5.0
        assert bbox.min_y == -3.0
        assert bbox.max_x == 10.0
        assert bbox.max_y == 7.0

    def test_geoboundingbox_from_points_single_point_creates_zero_area_box(
        self,
    ) -> None:
        points = [GeoPoint(x=5.0, y=10.0)]

        bbox = GeoBoundingBox.from_points(points)

        assert bbox.min_x == 5.0
        assert bbox.max_x == 5.0
        assert bbox.min_y == 10.0
        assert bbox.max_y == 10.0

    def test_geoboundingbox_from_points_empty_list_raises_value_error(self) -> None:
        with pytest.raises(ValueError, match="empty"):
            GeoBoundingBox.from_points([])
