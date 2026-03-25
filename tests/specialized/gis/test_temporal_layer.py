"""Tests for TemporalLayer -- time-aware geospatial feature layer."""

from __future__ import annotations

from datetime import datetime, timedelta
from pathlib import Path

import pandas as pd
import pytest

from digitalmodel.specialized.gis.core.crs import CRSDefinition, CRSType
from digitalmodel.specialized.gis.layers.feature_layer import FeatureLayer
from digitalmodel.specialized.gis.layers.temporal_layer import TemporalLayer


# ------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------


@pytest.fixture
def temporal_df() -> pd.DataFrame:
    """Minimal DataFrame with temporal columns for unit tests."""
    return pd.DataFrame(
        {
            "longitude": [-90.5, -90.5, -90.3, -90.3, -89.8],
            "latitude": [28.1, 28.1, 28.2, 28.2, 28.5],
            "name": ["W-A", "W-A", "W-B", "W-B", "W-C"],
            "timestamp": [
                "2010-01-01",
                "2015-06-15",
                "2012-03-10",
                "2018-09-20",
                "2020-12-01",
            ],
            "event_type": [
                "installed",
                "inspected",
                "installed",
                "repaired",
                "installed",
            ],
            "status": [
                "operational",
                "operational",
                "operational",
                "under_maintenance",
                "operational",
            ],
        }
    )


# ------------------------------------------------------------------
# Construction from DataFrame
# ------------------------------------------------------------------


class TestFromDataFrame:
    """Create a TemporalLayer from a plain pandas DataFrame."""

    def test_creates_layer_with_correct_count(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        assert len(layer) == 5

    def test_defaults_to_wgs84_when_no_crs(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        assert layer.crs.crs_type == CRSType.WGS84

    def test_is_feature_layer_subclass(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        assert isinstance(layer, FeatureLayer)

    def test_timestamp_column_converted_to_datetime(
        self, temporal_df: pd.DataFrame
    ) -> None:
        layer = TemporalLayer(data=temporal_df)
        assert pd.api.types.is_datetime64_any_dtype(layer.data["timestamp"])

    def test_default_name_is_temporal(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        assert layer.name == "temporal"

    def test_time_column_property(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df, time_col="timestamp")
        assert layer.time_column == "timestamp"


# ------------------------------------------------------------------
# Construction from GeoJSON
# ------------------------------------------------------------------


class TestFromGeoJSON:
    """Load a TemporalLayer from a GeoJSON file."""

    def test_returns_temporal_layer_type(
        self, temporal_wells_geojson: Path
    ) -> None:
        layer = TemporalLayer.from_geojson(temporal_wells_geojson)
        assert isinstance(layer, TemporalLayer)

    def test_is_also_feature_layer(self, temporal_wells_geojson: Path) -> None:
        layer = TemporalLayer.from_geojson(temporal_wells_geojson)
        assert isinstance(layer, FeatureLayer)

    def test_loads_correct_number_of_features(
        self, temporal_wells_geojson: Path
    ) -> None:
        layer = TemporalLayer.from_geojson(temporal_wells_geojson)
        assert len(layer) == 11

    def test_crs_is_wgs84(self, temporal_wells_geojson: Path) -> None:
        layer = TemporalLayer.from_geojson(temporal_wells_geojson)
        assert layer.crs.crs_type == CRSType.WGS84
        assert layer.crs.epsg_code == 4326


# ------------------------------------------------------------------
# time_range and duration properties
# ------------------------------------------------------------------


class TestTimeProperties:
    """Verify time_range and duration properties."""

    def test_time_range_returns_tuple(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        tr = layer.time_range
        assert isinstance(tr, tuple)
        assert len(tr) == 2

    def test_time_range_min_max(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        start, end = layer.time_range
        assert start == datetime(2010, 1, 1)
        assert end == datetime(2020, 12, 1)

    def test_duration_returns_timedelta(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        d = layer.duration
        assert isinstance(d, timedelta)

    def test_duration_value(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        expected = datetime(2020, 12, 1) - datetime(2010, 1, 1)
        assert layer.duration == expected

    def test_time_range_from_geojson(self, temporal_wells_geojson: Path) -> None:
        layer = TemporalLayer.from_geojson(temporal_wells_geojson)
        start, end = layer.time_range
        assert start.year == 2010
        assert end.year == 2024


# ------------------------------------------------------------------
# filter_by_time
# ------------------------------------------------------------------


class TestFilterByTime:
    """Filter features to a time window."""

    def test_filter_both_bounds(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        result = layer.filter_by_time(start="2012-01-01", end="2018-12-31")
        assert isinstance(result, TemporalLayer)
        assert len(result) == 3  # 2015-06-15, 2012-03-10, 2018-09-20

    def test_filter_start_only(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        result = layer.filter_by_time(start="2018-01-01")
        assert len(result) == 2  # 2018-09-20, 2020-12-01

    def test_filter_end_only(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        result = layer.filter_by_time(end="2012-12-31")
        assert len(result) == 2  # 2010-01-01, 2012-03-10

    def test_filter_no_matches(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        result = layer.filter_by_time(start="2025-01-01", end="2026-01-01")
        assert len(result) == 0

    def test_filter_accepts_datetime_objects(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        result = layer.filter_by_time(
            start=datetime(2015, 1, 1), end=datetime(2019, 1, 1)
        )
        assert len(result) == 2  # 2015-06-15, 2018-09-20


# ------------------------------------------------------------------
# snapshot_at
# ------------------------------------------------------------------


class TestSnapshotAt:
    """Get the latest state per feature at a given timestamp."""

    def test_snapshot_returns_temporal_layer(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        result = layer.snapshot_at("2016-01-01")
        assert isinstance(result, TemporalLayer)

    def test_snapshot_one_row_per_feature(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        # At 2016-01-01: W-A has 2010 and 2015 events (take 2015), W-B has 2012 (take it)
        # W-C has 2020 only, excluded
        result = layer.snapshot_at("2016-01-01")
        assert len(result) == 2
        names = sorted(result.data["name"].tolist())
        assert names == ["W-A", "W-B"]

    def test_snapshot_takes_latest_before_timestamp(
        self, temporal_df: pd.DataFrame
    ) -> None:
        layer = TemporalLayer(data=temporal_df)
        result = layer.snapshot_at("2016-01-01")
        wa_row = result.data[result.data["name"] == "W-A"].iloc[0]
        assert wa_row["event_type"] == "inspected"

    def test_snapshot_before_any_data(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        result = layer.snapshot_at("2005-01-01")
        assert len(result) == 0

    def test_snapshot_all_features(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        result = layer.snapshot_at("2025-01-01")
        assert len(result) == 3  # All three wells have at least one event


# ------------------------------------------------------------------
# time_series
# ------------------------------------------------------------------


class TestTimeSeries:
    """Extract time series for a specific feature."""

    def test_returns_dataframe(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        ts = layer.time_series("W-A", "status")
        assert isinstance(ts, pd.DataFrame)

    def test_correct_columns(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        ts = layer.time_series("W-A", "status")
        assert list(ts.columns) == ["timestamp", "status"]

    def test_sorted_by_time(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        ts = layer.time_series("W-A", "event_type")
        assert ts.iloc[0]["event_type"] == "installed"
        assert ts.iloc[1]["event_type"] == "inspected"

    def test_correct_row_count(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        ts = layer.time_series("W-B", "status")
        assert len(ts) == 2


# ------------------------------------------------------------------
# animate_frames
# ------------------------------------------------------------------


class TestAnimateFrames:
    """Split into time-binned frames."""

    def test_yearly_frames(self, temporal_wells_geojson: Path) -> None:
        layer = TemporalLayer.from_geojson(temporal_wells_geojson)
        frames = layer.animate_frames(freq="Y")
        assert isinstance(frames, list)
        assert all(isinstance(f, TemporalLayer) for f in frames)
        # Events span multiple years so we should have multiple frames
        assert len(frames) > 1

    def test_total_features_preserved(self, temporal_wells_geojson: Path) -> None:
        layer = TemporalLayer.from_geojson(temporal_wells_geojson)
        frames = layer.animate_frames(freq="Y")
        total = sum(len(f) for f in frames)
        assert total == len(layer)

    def test_quarterly_gives_more_frames_than_yearly(
        self, temporal_wells_geojson: Path
    ) -> None:
        layer = TemporalLayer.from_geojson(temporal_wells_geojson)
        yearly = layer.animate_frames(freq="Y")
        quarterly = layer.animate_frames(freq="Q")
        assert len(quarterly) >= len(yearly)


# ------------------------------------------------------------------
# summary
# ------------------------------------------------------------------


class TestSummary:
    """Verify the summary dict."""

    def test_summary_keys(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        s = layer.summary()
        assert set(s.keys()) == {
            "feature_count",
            "time_range",
            "duration_days",
            "unique_features",
        }

    def test_summary_feature_count(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        s = layer.summary()
        assert s["feature_count"] == 5

    def test_summary_unique_features(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        s = layer.summary()
        assert s["unique_features"] == ["W-A", "W-B", "W-C"]

    def test_summary_duration_days(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        s = layer.summary()
        expected_days = (datetime(2020, 12, 1) - datetime(2010, 1, 1)).days
        assert s["duration_days"] == expected_days

    def test_summary_time_range(self, temporal_df: pd.DataFrame) -> None:
        layer = TemporalLayer(data=temporal_df)
        s = layer.summary()
        start, end = s["time_range"]
        assert start.year == 2010
        assert end.year == 2020
