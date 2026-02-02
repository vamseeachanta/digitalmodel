"""Tests for coordinate_transformer module.

Covers CoordinateTransformer class, wgs84_to_utm, and utm_to_wgs84
convenience functions using real pyproj transforms (no mocks).
"""

from __future__ import annotations

import pandas as pd
import pytest

from digitalmodel.gis.core.coordinate_transformer import (
    CoordinateTransformer,
    utm_to_wgs84,
    wgs84_to_utm,
)


# ------------------------------------------------------------------
# CoordinateTransformer construction
# ------------------------------------------------------------------


class TestCoordinateTransformerConstruction:
    """CoordinateTransformer initialisation and properties."""

    def test_constructor_with_wgs84_source_and_utm_target_creates_transformer(
        self,
    ) -> None:
        source = "EPSG:4326"
        target = "EPSG:32616"
        transformer = CoordinateTransformer(source, target)

        assert transformer is not None

    def test_source_crs_property_returns_source(self) -> None:
        source = "EPSG:4326"
        target = "EPSG:32616"
        transformer = CoordinateTransformer(source, target)

        assert transformer.source_crs == source

    def test_target_crs_property_returns_target(self) -> None:
        source = "EPSG:4326"
        target = "EPSG:32616"
        transformer = CoordinateTransformer(source, target)

        assert transformer.target_crs == target


# ------------------------------------------------------------------
# transform_point
# ------------------------------------------------------------------


class TestTransformPoint:
    """Single-point coordinate transformation.

    Note: longitude -90.5 falls in UTM zone 15 (EPSG:32615).
    Expected values in zone 15: easting ~745623, northing ~3110805.
    When forced into zone 16 (EPSG:32616) the easting is ~156070 because
    the point is far west of the zone 16 central meridian.
    """

    def test_transform_point_wgs84_to_utm16n_easting_within_tolerance(
        self,
    ) -> None:
        """Transform (-90.5, 28.1) into UTM zone 16N and verify easting."""
        transformer = CoordinateTransformer("EPSG:4326", "EPSG:32616")

        easting, northing = transformer.transform_point(-90.5, 28.1)

        # -90.5 is in zone 15; projected into zone 16 easting is ~156070
        assert abs(easting - 156_070) < 1_000

    def test_transform_point_wgs84_to_utm16n_northing_within_tolerance(
        self,
    ) -> None:
        """Transform (-90.5, 28.1) into UTM zone 16N and verify northing."""
        transformer = CoordinateTransformer("EPSG:4326", "EPSG:32616")

        easting, northing = transformer.transform_point(-90.5, 28.1)

        assert abs(northing - 3_113_000) < 1_000

    def test_transform_point_wgs84_to_utm15n_easting_within_tolerance(
        self,
    ) -> None:
        """Transform (-90.5, 28.1) into native UTM zone 15N."""
        transformer = CoordinateTransformer("EPSG:4326", "EPSG:32615")

        easting, northing = transformer.transform_point(-90.5, 28.1)

        assert abs(easting - 745_623) < 1_000

    def test_transform_point_wgs84_to_utm15n_northing_within_tolerance(
        self,
    ) -> None:
        """Transform (-90.5, 28.1) into native UTM zone 15N."""
        transformer = CoordinateTransformer("EPSG:4326", "EPSG:32615")

        easting, northing = transformer.transform_point(-90.5, 28.1)

        assert abs(northing - 3_110_805) < 1_000

    def test_transform_point_with_z_returns_three_values(self) -> None:
        transformer = CoordinateTransformer("EPSG:4326", "EPSG:32616")

        result = transformer.transform_point(-90.5, 28.1, z=100.0)

        assert len(result) == 3

    def test_transform_point_without_z_returns_two_values(self) -> None:
        transformer = CoordinateTransformer("EPSG:4326", "EPSG:32616")

        result = transformer.transform_point(-90.5, 28.1)

        assert len(result) == 2


# ------------------------------------------------------------------
# transform_points
# ------------------------------------------------------------------


class TestTransformPoints:
    """Batch coordinate transformation for lists of points."""

    def test_transform_points_with_list_returns_correct_length(self) -> None:
        transformer = CoordinateTransformer("EPSG:4326", "EPSG:32616")
        lons = [-90.5, -90.0, -89.5]
        lats = [28.1, 28.5, 29.0]

        eastings, northings = transformer.transform_points(lons, lats)

        assert len(eastings) == 3
        assert len(northings) == 3

    def test_transform_points_first_element_matches_single_transform(
        self,
    ) -> None:
        transformer = CoordinateTransformer("EPSG:4326", "EPSG:32616")
        lons = [-90.5, -90.0]
        lats = [28.1, 28.5]

        eastings, northings = transformer.transform_points(lons, lats)
        single_e, single_n = transformer.transform_point(-90.5, 28.1)

        assert abs(eastings[0] - single_e) < 0.01
        assert abs(northings[0] - single_n) < 0.01

    def test_transform_points_with_z_returns_three_arrays(self) -> None:
        transformer = CoordinateTransformer("EPSG:4326", "EPSG:32616")
        lons = [-90.5, -90.0]
        lats = [28.1, 28.5]
        zs = [0.0, 100.0]

        result = transformer.transform_points(lons, lats, zs=zs)

        assert len(result) == 3


# ------------------------------------------------------------------
# transform_dataframe
# ------------------------------------------------------------------


class TestTransformDataframe:
    """DataFrame-based coordinate transformation."""

    def test_transform_dataframe_adds_prefixed_columns(self) -> None:
        transformer = CoordinateTransformer("EPSG:4326", "EPSG:32616")
        df = pd.DataFrame({"lon": [-90.5, -90.0], "lat": [28.1, 28.5]})

        result = transformer.transform_dataframe(df, x_col="lon", y_col="lat")

        assert "transformed_lon" in result.columns
        assert "transformed_lat" in result.columns

    def test_transform_dataframe_preserves_original_columns(self) -> None:
        transformer = CoordinateTransformer("EPSG:4326", "EPSG:32616")
        df = pd.DataFrame({"lon": [-90.5], "lat": [28.1]})

        result = transformer.transform_dataframe(df, x_col="lon", y_col="lat")

        assert "lon" in result.columns
        assert "lat" in result.columns
        assert result["lon"].iloc[0] == pytest.approx(-90.5)
        assert result["lat"].iloc[0] == pytest.approx(28.1)

    def test_transform_dataframe_values_match_transform_point(self) -> None:
        transformer = CoordinateTransformer("EPSG:4326", "EPSG:32616")
        df = pd.DataFrame({"lon": [-90.5], "lat": [28.1]})

        result = transformer.transform_dataframe(df, x_col="lon", y_col="lat")
        expected_e, expected_n = transformer.transform_point(-90.5, 28.1)

        assert result["transformed_lon"].iloc[0] == pytest.approx(expected_e)
        assert result["transformed_lat"].iloc[0] == pytest.approx(expected_n)

    def test_transform_dataframe_with_custom_prefix(self) -> None:
        transformer = CoordinateTransformer("EPSG:4326", "EPSG:32616")
        df = pd.DataFrame({"lon": [-90.5], "lat": [28.1]})

        result = transformer.transform_dataframe(
            df, x_col="lon", y_col="lat", prefix="utm_"
        )

        assert "utm_lon" in result.columns
        assert "utm_lat" in result.columns

    def test_transform_dataframe_with_z_column(self) -> None:
        transformer = CoordinateTransformer("EPSG:4326", "EPSG:32616")
        df = pd.DataFrame(
            {"lon": [-90.5], "lat": [28.1], "elev": [50.0]}
        )

        result = transformer.transform_dataframe(
            df, x_col="lon", y_col="lat", z_col="elev"
        )

        assert "transformed_elev" in result.columns

    def test_transform_dataframe_row_count_unchanged(self) -> None:
        transformer = CoordinateTransformer("EPSG:4326", "EPSG:32616")
        df = pd.DataFrame(
            {"lon": [-90.5, -90.0, -89.5], "lat": [28.1, 28.5, 29.0]}
        )

        result = transformer.transform_dataframe(df, x_col="lon", y_col="lat")

        assert len(result) == 3


# ------------------------------------------------------------------
# wgs84_to_utm convenience function
# ------------------------------------------------------------------


class TestWgs84ToUtm:
    """wgs84_to_utm convenience function.

    Note: longitude -90.5 auto-detects to UTM zone 15, not 16.
    """

    def test_wgs84_to_utm_with_explicit_zone_returns_correct_zone(
        self,
    ) -> None:
        easting, northing, zone, hemisphere = wgs84_to_utm(
            -90.5, 28.1, zone=16
        )

        assert zone == 16

    def test_wgs84_to_utm_with_explicit_zone_returns_correct_hemisphere(
        self,
    ) -> None:
        easting, northing, zone, hemisphere = wgs84_to_utm(
            -90.5, 28.1, zone=16
        )

        assert hemisphere == "N"

    def test_wgs84_to_utm_with_explicit_zone_easting_is_reasonable(
        self,
    ) -> None:
        easting, northing, zone, hemisphere = wgs84_to_utm(
            -90.5, 28.1, zone=15
        )

        assert 100_000 < easting < 900_000

    def test_wgs84_to_utm_with_explicit_zone_northing_is_reasonable(
        self,
    ) -> None:
        easting, northing, zone, hemisphere = wgs84_to_utm(
            -90.5, 28.1, zone=15
        )

        assert 0 < northing < 10_000_000

    def test_wgs84_to_utm_with_explicit_zone_easting_within_tolerance(
        self,
    ) -> None:
        easting, northing, zone, hemisphere = wgs84_to_utm(
            -90.5, 28.1, zone=15
        )

        assert abs(easting - 745_623) < 1_000

    def test_wgs84_to_utm_with_auto_zone_detection_returns_correct_zone(
        self,
    ) -> None:
        easting, northing, zone, hemisphere = wgs84_to_utm(
            -90.5, 28.1, zone=None
        )

        assert zone == 15
        assert hemisphere == "N"

    def test_wgs84_to_utm_with_auto_zone_detection_southern_hemisphere(
        self,
    ) -> None:
        easting, northing, zone, hemisphere = wgs84_to_utm(151.2, -33.9)

        assert hemisphere == "S"
        assert zone == 56

    def test_wgs84_to_utm_southern_hemisphere_with_explicit_zone(self) -> None:
        easting, northing, zone, hemisphere = wgs84_to_utm(
            151.2, -33.9, zone=56
        )

        assert hemisphere == "S"
        assert zone == 56


# ------------------------------------------------------------------
# utm_to_wgs84 round-trip
# ------------------------------------------------------------------


class TestUtmToWgs84:
    """utm_to_wgs84 function and round-trip accuracy."""

    def test_utm_to_wgs84_round_trip_longitude_within_tolerance(self) -> None:
        original_lon, original_lat = -90.5, 28.1
        easting, northing, zone, hemisphere = wgs84_to_utm(
            original_lon, original_lat, zone=15
        )

        recovered_lon, recovered_lat = utm_to_wgs84(
            easting, northing, zone, hemisphere
        )

        assert abs(recovered_lon - original_lon) < 0.001

    def test_utm_to_wgs84_round_trip_latitude_within_tolerance(self) -> None:
        original_lon, original_lat = -90.5, 28.1
        easting, northing, zone, hemisphere = wgs84_to_utm(
            original_lon, original_lat, zone=15
        )

        recovered_lon, recovered_lat = utm_to_wgs84(
            easting, northing, zone, hemisphere
        )

        assert abs(recovered_lat - original_lat) < 0.001

    def test_utm_to_wgs84_round_trip_southern_hemisphere(self) -> None:
        original_lon, original_lat = 151.2, -33.9
        easting, northing, zone, hemisphere = wgs84_to_utm(
            original_lon, original_lat, zone=56
        )

        recovered_lon, recovered_lat = utm_to_wgs84(
            easting, northing, zone, hemisphere
        )

        assert abs(recovered_lon - original_lon) < 0.001
        assert abs(recovered_lat - original_lat) < 0.001

    def test_utm_to_wgs84_returns_two_values(self) -> None:
        result = utm_to_wgs84(782000.0, 3112000.0, 16, "N")

        assert len(result) == 2

    def test_utm_to_wgs84_default_hemisphere_is_north(self) -> None:
        lon_default, lat_default = utm_to_wgs84(782000.0, 3112000.0, 16)
        lon_explicit, lat_explicit = utm_to_wgs84(
            782000.0, 3112000.0, 16, "N"
        )

        assert lon_default == pytest.approx(lon_explicit)
        assert lat_default == pytest.approx(lat_explicit)
