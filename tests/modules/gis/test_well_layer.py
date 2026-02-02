"""Tests for WellLayer -- specialized FeatureLayer for offshore well data."""

from __future__ import annotations

from pathlib import Path

import pandas as pd
import pytest

from digitalmodel.gis.core.crs import CRSDefinition, CRSType
from digitalmodel.gis.core.geometry import GeoBoundingBox, GeoPoint
from digitalmodel.gis.layers.feature_layer import FeatureLayer
from digitalmodel.gis.layers.well_layer import WellLayer


# ------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------


@pytest.fixture
def well_df() -> pd.DataFrame:
    """Minimal DataFrame with well columns for unit tests."""
    return pd.DataFrame(
        {
            "longitude": [-90.5, -90.3, -89.8],
            "latitude": [28.1, 28.2, 28.5],
            "name": ["W-1", "W-2", "W-3"],
            "field": ["Alpha", "Alpha", "Beta"],
            "status": ["active", "active", "decommissioned"],
            "water_depth_m": [1500.0, 1200.0, 800.0],
        }
    )


# ------------------------------------------------------------------
# Construction from GeoJSON
# ------------------------------------------------------------------


class TestFromGeoJSON:
    """Load a WellLayer from a GeoJSON file."""

    def test_returns_well_layer_type(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        assert isinstance(layer, WellLayer)

    def test_is_also_feature_layer(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        assert isinstance(layer, FeatureLayer)

    def test_loads_correct_number_of_wells(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        assert len(layer) == 7

    def test_crs_is_wgs84(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        assert layer.crs.crs_type == CRSType.WGS84
        assert layer.crs.epsg_code == 4326

    def test_default_name_is_wells(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        assert layer.name == "wells"


# ------------------------------------------------------------------
# Construction from DataFrame
# ------------------------------------------------------------------


class TestFromDataFrame:
    """Create a WellLayer from a plain pandas DataFrame."""

    def test_creates_layer_with_correct_count(self, well_df: pd.DataFrame) -> None:
        layer = WellLayer(data=well_df)
        assert len(layer) == 3

    def test_defaults_to_wgs84_when_no_crs(self, well_df: pd.DataFrame) -> None:
        layer = WellLayer(data=well_df)
        assert layer.crs.crs_type == CRSType.WGS84

    def test_accepts_explicit_crs(self, well_df: pd.DataFrame) -> None:
        utm_crs = CRSDefinition.utm_from_zone(16, "N")
        layer = WellLayer(data=well_df, crs=utm_crs)
        assert layer.crs.crs_type == CRSType.UTM


# ------------------------------------------------------------------
# well_count property
# ------------------------------------------------------------------


class TestWellCount:
    """Verify the well_count property."""

    def test_from_geojson(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        assert layer.well_count == 7

    def test_from_dataframe(self, well_df: pd.DataFrame) -> None:
        layer = WellLayer(data=well_df)
        assert layer.well_count == 3

    def test_empty_layer(self) -> None:
        df = pd.DataFrame({"longitude": [], "latitude": []})
        layer = WellLayer(data=df)
        assert layer.well_count == 0


# ------------------------------------------------------------------
# fields property
# ------------------------------------------------------------------


class TestFields:
    """Verify the fields property."""

    def test_returns_unique_fields_sorted(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        fields = layer.fields
        assert isinstance(fields, list)
        assert "Mississippi Canyon" in fields
        assert "Green Canyon" in fields
        assert "Main Pass" in fields
        assert "Viosca Knoll" in fields
        assert len(fields) == 4

    def test_returns_empty_list_when_no_field_column(self) -> None:
        df = pd.DataFrame({"longitude": [-90.0], "latitude": [28.0]})
        layer = WellLayer(data=df)
        assert layer.fields == []


# ------------------------------------------------------------------
# statuses property
# ------------------------------------------------------------------


class TestStatuses:
    """Verify the statuses property."""

    def test_returns_status_counts(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        statuses = layer.statuses
        assert isinstance(statuses, dict)
        assert statuses["active"] == 6
        assert statuses["decommissioned"] == 1

    def test_returns_empty_dict_when_no_status_column(self) -> None:
        df = pd.DataFrame({"longitude": [-90.0], "latitude": [28.0]})
        layer = WellLayer(data=df)
        assert layer.statuses == {}


# ------------------------------------------------------------------
# filter_by_field
# ------------------------------------------------------------------


class TestFilterByField:
    """Filter wells by field name."""

    def test_filter_existing_field(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        mc = layer.filter_by_field("Mississippi Canyon")
        assert isinstance(mc, WellLayer)
        assert mc.well_count == 3
        assert all(mc.data["field"] == "Mississippi Canyon")

    def test_filter_non_existing_field_returns_empty(
        self, sample_wells_geojson: Path
    ) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        result = layer.filter_by_field("Nonexistent Field")
        assert isinstance(result, WellLayer)
        assert result.well_count == 0

    def test_raises_when_field_column_missing(self) -> None:
        df = pd.DataFrame({"longitude": [-90.0], "latitude": [28.0]})
        layer = WellLayer(data=df)
        with pytest.raises(ValueError, match="field"):
            layer.filter_by_field("Alpha")


# ------------------------------------------------------------------
# filter_by_status
# ------------------------------------------------------------------


class TestFilterByStatus:
    """Filter wells by status value."""

    def test_filter_active(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        active = layer.filter_by_status("active")
        assert isinstance(active, WellLayer)
        assert active.well_count == 6

    def test_filter_decommissioned(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        decom = layer.filter_by_status("decommissioned")
        assert decom.well_count == 1

    def test_raises_when_status_column_missing(self) -> None:
        df = pd.DataFrame({"longitude": [-90.0], "latitude": [28.0]})
        layer = WellLayer(data=df)
        with pytest.raises(ValueError, match="status"):
            layer.filter_by_status("active")


# ------------------------------------------------------------------
# filter_by_water_depth
# ------------------------------------------------------------------


class TestFilterByWaterDepth:
    """Filter wells by water depth range."""

    def test_depth_range(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        # water_depth_m values: 1500, 1200, 800, 2000, 1000, 600, 2500
        result = layer.filter_by_water_depth(min_depth=1000, max_depth=2000)
        assert isinstance(result, WellLayer)
        assert result.well_count == 4  # 1500, 1200, 2000, 1000
        for depth in result.data["water_depth_m"]:
            assert 1000 <= depth <= 2000

    def test_no_matches(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        result = layer.filter_by_water_depth(min_depth=5000, max_depth=6000)
        assert result.well_count == 0

    def test_raises_when_column_missing(self) -> None:
        df = pd.DataFrame({"longitude": [-90.0], "latitude": [28.0]})
        layer = WellLayer(data=df)
        with pytest.raises(ValueError, match="water_depth_m"):
            layer.filter_by_water_depth(min_depth=100)


# ------------------------------------------------------------------
# wells_within_radius
# ------------------------------------------------------------------


class TestWellsWithinRadius:
    """Find wells within a geographic radius."""

    def test_finds_nearby_wells(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        # Search near MC-252-A at (-90.5, 28.1) with a 50km radius
        result = layer.wells_within_radius(
            origin_lon=-90.5, origin_lat=28.1, radius_m=50_000
        )
        assert isinstance(result, WellLayer)
        assert result.well_count > 0
        # The origin well itself should always be included
        assert result.well_count >= 1

    def test_large_radius_includes_all(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        result = layer.wells_within_radius(
            origin_lon=-90.5, origin_lat=28.1, radius_m=500_000
        )
        assert result.well_count == 7

    def test_tiny_radius_excludes_distant(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        result = layer.wells_within_radius(
            origin_lon=-90.5, origin_lat=28.1, radius_m=1
        )
        # Only the exact origin point (or none if floating point prevents match)
        assert result.well_count <= 1


# ------------------------------------------------------------------
# summary
# ------------------------------------------------------------------


class TestSummary:
    """Verify the summary dict."""

    def test_summary_keys(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        s = layer.summary()
        assert set(s.keys()) == {"well_count", "fields", "statuses", "bounds", "centroid"}

    def test_summary_well_count(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        s = layer.summary()
        assert s["well_count"] == 7

    def test_summary_bounds_type(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        s = layer.summary()
        assert isinstance(s["bounds"], GeoBoundingBox)

    def test_summary_centroid_type(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        s = layer.summary()
        assert isinstance(s["centroid"], GeoPoint)

    def test_summary_fields(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        s = layer.summary()
        assert len(s["fields"]) == 4

    def test_summary_statuses(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        s = layer.summary()
        assert s["statuses"]["active"] == 6
        assert s["statuses"]["decommissioned"] == 1
