"""Tests for CRS handling, coordinate transformation, and geometry operations.

TDD: Validates Phase 1 + Phase 3 core capabilities with known control points.
"""
from __future__ import annotations

import math

import pytest

from digitalmodel.gis.core.crs import CRSDefinition, CRSType, detect_utm_zone
from digitalmodel.gis.core.geometry import GeoBoundingBox, GeoPoint


# ---------------------------------------------------------------------------
# UTM zone detection
# ---------------------------------------------------------------------------


class TestUTMZoneDetection:
    def test_london_is_zone_30(self):
        # London: ~0.1W, 51.5N
        zone, hemi = detect_utm_zone(-0.1, 51.5)
        assert zone == 30
        assert hemi == "N"

    def test_houston_is_zone_15(self):
        # Houston: ~95.4W, 29.8N
        zone, hemi = detect_utm_zone(-95.4, 29.8)
        assert zone == 15
        assert hemi == "N"

    def test_new_orleans_is_zone_16(self):
        zone, hemi = detect_utm_zone(-90.0, 29.9)
        assert zone == 16
        assert hemi == "N"

    def test_southern_hemisphere(self):
        # Sydney: ~151.2E, 33.9S
        zone, hemi = detect_utm_zone(151.2, -33.9)
        assert hemi == "S"

    def test_zone_boundary_180(self):
        zone, hemi = detect_utm_zone(180.0, 0.0)
        assert 1 <= zone <= 60

    def test_invalid_longitude_raises(self):
        with pytest.raises(ValueError):
            detect_utm_zone(181.0, 0.0)

    def test_invalid_latitude_raises(self):
        with pytest.raises(ValueError):
            detect_utm_zone(0.0, 91.0)


# ---------------------------------------------------------------------------
# CRSDefinition factory methods
# ---------------------------------------------------------------------------


class TestCRSDefinition:
    def test_wgs84_factory(self):
        crs = CRSDefinition.wgs84()
        assert crs.crs_type == CRSType.WGS84
        assert crs.epsg_code == 4326

    def test_utm_from_zone_north(self):
        crs = CRSDefinition.utm_from_zone(15, "N")
        assert crs.crs_type == CRSType.UTM
        assert crs.epsg_code == 32615
        assert crs.zone == 15
        assert crs.hemisphere == "N"

    def test_utm_from_zone_south(self):
        crs = CRSDefinition.utm_from_zone(23, "S")
        assert crs.epsg_code == 32723
        assert crs.hemisphere == "S"

    def test_utm_from_longitude_auto(self):
        # GoM: zone 16N
        crs = CRSDefinition.utm_from_longitude(-90.0, 29.0)
        assert crs.zone == 16
        assert crs.hemisphere == "N"

    def test_from_epsg_4326(self):
        crs = CRSDefinition.from_epsg(4326)
        assert crs.epsg_code == 4326
        assert crs.crs_type in (CRSType.WGS84, CRSType.GEOGRAPHIC)

    def test_from_epsg_utm_north(self):
        crs = CRSDefinition.from_epsg(32616)
        assert crs.zone == 16
        assert crs.hemisphere == "N"

    def test_utm_invalid_hemisphere_raises(self):
        with pytest.raises(ValueError):
            CRSDefinition.utm_from_zone(15, "X")

    def test_utm_invalid_zone_raises(self):
        with pytest.raises(ValueError):
            CRSDefinition.utm_from_zone(0, "N")


# ---------------------------------------------------------------------------
# GeoPoint distance calculations
# ---------------------------------------------------------------------------


class TestGeoPointDistance:
    def test_haversine_same_point_is_zero(self):
        p = GeoPoint(x=0.0, y=0.0)
        assert p.distance_to(p) == pytest.approx(0.0, abs=1e-6)

    def test_haversine_known_distance(self):
        # Approximately 1 degree of latitude ≈ 111_195 m at equator
        p1 = GeoPoint(x=0.0, y=0.0)
        p2 = GeoPoint(x=0.0, y=1.0)
        distance = p1.distance_to(p2)
        assert 110_000 < distance < 112_000

    def test_euclidean_2d(self):
        p1 = GeoPoint(x=0.0, y=0.0)
        p2 = GeoPoint(x=3.0, y=4.0)
        assert p1.distance_to(p2, method="euclidean") == pytest.approx(5.0)

    def test_euclidean_3d(self):
        p1 = GeoPoint(x=0.0, y=0.0, z=0.0)
        p2 = GeoPoint(x=1.0, y=2.0, z=2.0)
        expected = math.sqrt(1 + 4 + 4)
        assert p1.distance_to(p2, method="euclidean") == pytest.approx(expected)

    def test_unknown_method_raises(self):
        p = GeoPoint(x=0.0, y=0.0)
        with pytest.raises(ValueError):
            p.distance_to(p, method="unknown")


# ---------------------------------------------------------------------------
# GeoBoundingBox
# ---------------------------------------------------------------------------


class TestGeoBoundingBox:
    def test_contains_inside(self):
        bbox = GeoBoundingBox(-91.0, 28.0, -89.0, 30.0)
        assert bbox.contains(GeoPoint(x=-90.0, y=29.0)) is True

    def test_contains_outside(self):
        bbox = GeoBoundingBox(-91.0, 28.0, -89.0, 30.0)
        assert bbox.contains(GeoPoint(x=-95.0, y=25.0)) is False

    def test_center(self):
        bbox = GeoBoundingBox(0.0, 0.0, 4.0, 2.0)
        center = bbox.center()
        assert center.x == pytest.approx(2.0)
        assert center.y == pytest.approx(1.0)

    def test_from_points(self):
        pts = [GeoPoint(x=-91.0, y=28.0), GeoPoint(x=-89.0, y=30.0)]
        bbox = GeoBoundingBox.from_points(pts)
        assert bbox.min_x == pytest.approx(-91.0)
        assert bbox.max_x == pytest.approx(-89.0)

    def test_from_points_empty_raises(self):
        with pytest.raises(ValueError):
            GeoBoundingBox.from_points([])

    def test_expand(self):
        bbox = GeoBoundingBox(1.0, 1.0, 3.0, 3.0)
        expanded = bbox.expand(0.5)
        assert expanded.min_x == pytest.approx(0.5)
        assert expanded.max_x == pytest.approx(3.5)

    def test_to_polygon_coords_length(self):
        bbox = GeoBoundingBox(0.0, 0.0, 1.0, 1.0)
        coords = bbox.to_polygon_coords()
        assert len(coords) == 5  # closed ring
