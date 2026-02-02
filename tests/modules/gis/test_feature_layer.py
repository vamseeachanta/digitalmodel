"""Tests for FeatureLayer — DataFrame-backed vector layer with CRS support."""

from __future__ import annotations

from pathlib import Path

import pandas as pd
import pytest

from digitalmodel.gis.core.crs import CRSDefinition, CRSType
from digitalmodel.gis.core.geometry import GeoPoint
from digitalmodel.gis.layers.feature_layer import FeatureLayer


# ------------------------------------------------------------------
# from_geojson
# ------------------------------------------------------------------


class TestFromGeoJSON:
    """Load a FeatureLayer from a GeoJSON file."""

    def test_loads_correct_number_of_features(
        self, sample_wells_geojson: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        assert len(layer) == 7

    def test_crs_is_wgs84(self, sample_wells_geojson: Path) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        assert layer.crs.crs_type == CRSType.WGS84
        assert layer.crs.epsg_code == 4326


# ------------------------------------------------------------------
# from_kml
# ------------------------------------------------------------------


class TestFromKML:
    """Load a FeatureLayer from a KML file."""

    def test_loads_features(self, sample_polygons_kml: Path) -> None:
        layer = FeatureLayer.from_kml(sample_polygons_kml)
        assert len(layer) > 0

    def test_crs_is_wgs84(self, sample_polygons_kml: Path) -> None:
        layer = FeatureLayer.from_kml(sample_polygons_kml)
        assert layer.crs.crs_type == CRSType.WGS84


# ------------------------------------------------------------------
# from_dataframe
# ------------------------------------------------------------------


class TestFromDataFrame:
    """Create a FeatureLayer from a plain pandas DataFrame."""

    @pytest.fixture
    def simple_df(self) -> pd.DataFrame:
        return pd.DataFrame(
            {
                "longitude": [-90.0, -91.0, -89.5],
                "latitude": [28.0, 27.5, 29.0],
                "name": ["A", "B", "C"],
            }
        )

    def test_creates_layer_with_correct_length(
        self, simple_df: pd.DataFrame
    ) -> None:
        layer = FeatureLayer.from_dataframe(simple_df)
        assert len(layer) == 3

    def test_defaults_to_wgs84(self, simple_df: pd.DataFrame) -> None:
        layer = FeatureLayer.from_dataframe(simple_df)
        assert layer.crs.crs_type == CRSType.WGS84

    def test_custom_crs(self, simple_df: pd.DataFrame) -> None:
        utm_crs = CRSDefinition.utm_from_zone(16, "N")
        layer = FeatureLayer.from_dataframe(simple_df, crs=utm_crs)
        assert layer.crs.crs_type == CRSType.UTM
        assert layer.crs.zone == 16

    def test_custom_column_names(self) -> None:
        df = pd.DataFrame({"lon": [1.0, 2.0], "lat": [3.0, 4.0]})
        layer = FeatureLayer.from_dataframe(df, lon_col="lon", lat_col="lat")
        assert layer.lon_col == "lon"
        assert layer.lat_col == "lat"
        assert len(layer) == 2


# ------------------------------------------------------------------
# bounds
# ------------------------------------------------------------------


class TestBounds:
    """Verify bounding box values for the sample wells data."""

    def test_bounds_values(self, sample_wells_geojson: Path) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        bounds = layer.bounds

        assert bounds.min_x == pytest.approx(-91.5)
        assert bounds.min_y == pytest.approx(27.6)
        assert bounds.max_x == pytest.approx(-89.5)
        assert bounds.max_y == pytest.approx(28.7)


# ------------------------------------------------------------------
# centroid
# ------------------------------------------------------------------


class TestCentroid:
    """Verify centroid is within expected bounds."""

    def test_centroid_within_bounds(self, sample_wells_geojson: Path) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        centroid = layer.centroid
        bounds = layer.bounds

        assert bounds.min_x <= centroid.x <= bounds.max_x
        assert bounds.min_y <= centroid.y <= bounds.max_y

    def test_centroid_approximate_values(
        self, sample_wells_geojson: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        centroid = layer.centroid

        assert centroid.x == pytest.approx(-90.4, abs=0.1)
        assert centroid.y == pytest.approx(28.17, abs=0.1)


# ------------------------------------------------------------------
# __len__
# ------------------------------------------------------------------


class TestLen:
    """Verify feature count via __len__."""

    def test_len_matches_dataframe_rows(
        self, sample_wells_geojson: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        assert len(layer) == len(layer.data)

    def test_len_value(self, sample_wells_geojson: Path) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        assert len(layer) == 7


# ------------------------------------------------------------------
# __repr__
# ------------------------------------------------------------------


class TestRepr:
    """Verify string representation."""

    def test_repr_contains_key_info(
        self, sample_wells_geojson: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        text = repr(layer)

        assert "FeatureLayer" in text
        assert "features=7" in text
        assert "wgs84" in text

    def test_repr_contains_name(self, sample_wells_geojson: Path) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        text = repr(layer)

        assert "sample_wells" in text


# ------------------------------------------------------------------
# to_geojson
# ------------------------------------------------------------------


class TestToGeoJSON:
    """Export to a GeoJSON file and verify it exists."""

    def test_export_creates_file(
        self, sample_wells_geojson: Path, tmp_path: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        output = tmp_path / "export.geojson"
        result = layer.to_geojson(output)

        assert result.exists()
        assert result.stat().st_size > 0


# ------------------------------------------------------------------
# to_kml
# ------------------------------------------------------------------


class TestToKML:
    """Export to a KML file and verify it exists."""

    def test_export_creates_file(
        self, sample_wells_geojson: Path, tmp_path: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        output = tmp_path / "export.kml"
        result = layer.to_kml(output)

        assert result.exists()
        assert result.stat().st_size > 0


# ------------------------------------------------------------------
# reproject
# ------------------------------------------------------------------


class TestReproject:
    """Reproject WGS84 to UTM zone 16N and verify changes."""

    def test_crs_updated(self, sample_wells_geojson: Path) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        utm_crs = CRSDefinition.utm_from_zone(16, "N")
        reprojected = layer.reproject(utm_crs)

        assert reprojected.crs.crs_type == CRSType.UTM
        assert reprojected.crs.epsg_code == 32616

    def test_coordinates_changed(self, sample_wells_geojson: Path) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        utm_crs = CRSDefinition.utm_from_zone(16, "N")
        reprojected = layer.reproject(utm_crs)

        original_lons = layer.data["longitude"].tolist()
        reprojected_lons = reprojected.data["longitude"].tolist()

        # UTM easting values are positive and large (hundreds of thousands)
        # while WGS84 longitudes in Gulf of Mexico are around -90
        assert original_lons != reprojected_lons
        for val in reprojected_lons:
            assert val > 0, "UTM easting should be positive"

    def test_feature_count_preserved(
        self, sample_wells_geojson: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        utm_crs = CRSDefinition.utm_from_zone(16, "N")
        reprojected = layer.reproject(utm_crs)

        assert len(reprojected) == len(layer)


# ------------------------------------------------------------------
# filter
# ------------------------------------------------------------------


class TestFilter:
    """Filter with a boolean mask and verify reduced count."""

    def test_filter_reduces_count(
        self, sample_wells_geojson: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        mask = layer.data["longitude"] > -90.5
        filtered = layer.filter(mask)

        assert len(filtered) < len(layer)
        assert len(filtered) > 0

    def test_filter_preserves_crs(
        self, sample_wells_geojson: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        mask = layer.data["longitude"] > -90.5
        filtered = layer.filter(mask)

        assert filtered.crs.crs_type == layer.crs.crs_type
        assert filtered.crs.epsg_code == layer.crs.epsg_code

    def test_filter_all_false_returns_empty(
        self, sample_wells_geojson: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        mask = pd.Series([False] * len(layer))
        filtered = layer.filter(mask)

        assert len(filtered) == 0


# ------------------------------------------------------------------
# to_geo_points
# ------------------------------------------------------------------


class TestToGeoPoints:
    """Convert features to a list of GeoPoint objects."""

    def test_returns_list_of_geopoints(
        self, sample_wells_geojson: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        points = layer.to_geo_points()

        assert isinstance(points, list)
        assert all(isinstance(p, GeoPoint) for p in points)

    def test_correct_count(self, sample_wells_geojson: Path) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        points = layer.to_geo_points()

        assert len(points) == len(layer)
        assert len(points) == 7

    def test_point_coordinates_match_data(
        self, sample_wells_geojson: Path
    ) -> None:
        layer = FeatureLayer.from_geojson(sample_wells_geojson)
        points = layer.to_geo_points()

        for i, point in enumerate(points):
            assert point.x == pytest.approx(layer.data["longitude"].iloc[i])
            assert point.y == pytest.approx(layer.data["latitude"].iloc[i])
