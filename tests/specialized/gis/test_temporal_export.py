"""Tests for TemporalExporter — temporal KML, GeoJSON, and CSV export."""

from __future__ import annotations

import csv
import json
from pathlib import Path

import pandas as pd
import pytest
from lxml import etree

from digitalmodel.specialized.gis.core.crs import CRSDefinition
from digitalmodel.specialized.gis.integrations.temporal_export import TemporalExporter
from digitalmodel.specialized.gis.layers.feature_layer import FeatureLayer

KML_NS = "http://www.opengis.net/kml/2.2"


def _tag(name: str) -> str:
    return f"{{{KML_NS}}}{name}"


# ------------------------------------------------------------------
# Fixtures
# ------------------------------------------------------------------


@pytest.fixture
def temporal_layer() -> FeatureLayer:
    """FeatureLayer with temporal event data for five records."""
    data = pd.DataFrame(
        {
            "name": ["Well-A", "Well-A", "Well-B", "Well-B", "Well-C"],
            "longitude": [-90.5, -90.5, -89.8, -89.8, -91.0],
            "latitude": [28.1, 28.1, 28.5, 28.5, 27.8],
            "timestamp": pd.to_datetime(
                [
                    "2015-01-15",
                    "2020-06-20",
                    "2016-03-10",
                    "2022-11-05",
                    "2018-08-22",
                ]
            ),
            "event_type": [
                "installed",
                "inspected",
                "installed",
                "repaired",
                "installed",
            ],
            "status": ["active", "active", "active", "active", "active"],
        }
    )
    return FeatureLayer(data=data, crs=CRSDefinition.wgs84(), name="temporal_test")


@pytest.fixture
def timeline_layer() -> FeatureLayer:
    """FeatureLayer with start/end date columns for TimeSpan tests."""
    data = pd.DataFrame(
        {
            "name": ["Asset-1", "Asset-2", "Asset-3"],
            "longitude": [-90.0, -89.5, -91.2],
            "latitude": [28.0, 28.3, 27.9],
            "start_date": pd.to_datetime(
                ["2010-01-01", "2015-06-15", "2018-03-20"]
            ),
            "end_date": pd.to_datetime(
                ["2020-12-31", pd.NaT, "2023-07-01"]
            ),
        }
    )
    return FeatureLayer(data=data, crs=CRSDefinition.wgs84(), name="timeline_test")


# ---------------------------------------------------------------------------
# 1. export_temporal_kml — KML with TimeStamp
# ---------------------------------------------------------------------------


class TestExportTemporalKml:
    """Verify export_temporal_kml creates KML files with TimeStamp elements."""

    def test_creates_kml_file(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "temporal.kml"
        result = TemporalExporter.export_temporal_kml(temporal_layer, out)
        assert result.exists()
        assert result.suffix == ".kml"

    def test_correct_number_of_placemarks(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "temporal.kml"
        TemporalExporter.export_temporal_kml(temporal_layer, out)
        tree = etree.parse(str(out))
        placemarks = list(tree.getroot().iter(_tag("Placemark")))
        assert len(placemarks) == len(temporal_layer)

    def test_each_placemark_has_timestamp(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "temporal.kml"
        TemporalExporter.export_temporal_kml(temporal_layer, out)
        tree = etree.parse(str(out))
        for pm in tree.getroot().iter(_tag("Placemark")):
            ts = pm.find(_tag("TimeStamp"))
            assert ts is not None
            when = ts.find(_tag("when"))
            assert when is not None
            assert when.text  # non-empty ISO date string

    def test_placemark_names_present(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "temporal.kml"
        TemporalExporter.export_temporal_kml(temporal_layer, out)
        tree = etree.parse(str(out))
        names = [
            pm.find(_tag("name")).text
            for pm in tree.getroot().iter(_tag("Placemark"))
        ]
        assert "Well-A" in names
        assert "Well-B" in names
        assert "Well-C" in names

    def test_placemarks_have_point_geometry(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "temporal.kml"
        TemporalExporter.export_temporal_kml(temporal_layer, out)
        tree = etree.parse(str(out))
        for pm in tree.getroot().iter(_tag("Placemark")):
            point = pm.find(_tag("Point"))
            assert point is not None
            coords = point.find(_tag("coordinates"))
            assert coords is not None
            assert "," in coords.text

    def test_returns_resolved_path(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "temporal.kml"
        result = TemporalExporter.export_temporal_kml(temporal_layer, out)
        assert result.is_absolute()


# ---------------------------------------------------------------------------
# 2. export_timeline_kml — KML with TimeSpan
# ---------------------------------------------------------------------------


class TestExportTimelineKml:
    """Verify export_timeline_kml creates KML files with TimeSpan elements."""

    def test_creates_kml_file(
        self, timeline_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "timeline.kml"
        result = TemporalExporter.export_timeline_kml(timeline_layer, out)
        assert result.exists()
        assert result.suffix == ".kml"

    def test_each_placemark_has_timespan(
        self, timeline_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "timeline.kml"
        TemporalExporter.export_timeline_kml(timeline_layer, out)
        tree = etree.parse(str(out))
        for pm in tree.getroot().iter(_tag("Placemark")):
            ts = pm.find(_tag("TimeSpan"))
            assert ts is not None
            begin = ts.find(_tag("begin"))
            assert begin is not None
            assert begin.text

    def test_missing_end_date_omits_end_element(
        self, timeline_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "timeline.kml"
        TemporalExporter.export_timeline_kml(timeline_layer, out)
        tree = etree.parse(str(out))
        placemarks = list(tree.getroot().iter(_tag("Placemark")))
        # Asset-2 (index 1) has NaT end_date
        asset2 = placemarks[1]
        ts = asset2.find(_tag("TimeSpan"))
        assert ts.find(_tag("end")) is None

    def test_present_end_date_has_end_element(
        self, timeline_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "timeline.kml"
        TemporalExporter.export_timeline_kml(timeline_layer, out)
        tree = etree.parse(str(out))
        placemarks = list(tree.getroot().iter(_tag("Placemark")))
        # Asset-1 (index 0) has a valid end_date
        asset1 = placemarks[0]
        ts = asset1.find(_tag("TimeSpan"))
        end = ts.find(_tag("end"))
        assert end is not None
        assert end.text


# ---------------------------------------------------------------------------
# 3. export_animated_geojson — frame-based GeoJSON
# ---------------------------------------------------------------------------


class TestExportAnimatedGeojson:
    """Verify export_animated_geojson creates JSON with a frames array."""

    def test_creates_json_file(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "animated.json"
        result = TemporalExporter.export_animated_geojson(temporal_layer, out)
        assert result.exists()

    def test_json_has_frames_key(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "animated.json"
        TemporalExporter.export_animated_geojson(temporal_layer, out)
        payload = json.loads(out.read_text())
        assert "frames" in payload
        assert isinstance(payload["frames"], list)

    def test_frames_contain_feature_collections(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "animated.json"
        TemporalExporter.export_animated_geojson(temporal_layer, out)
        payload = json.loads(out.read_text())
        for frame in payload["frames"]:
            assert "timestamp" in frame
            assert "features" in frame
            fc = frame["features"]
            assert fc["type"] == "FeatureCollection"
            assert "features" in fc

    def test_total_features_match_layer(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "animated.json"
        TemporalExporter.export_animated_geojson(temporal_layer, out)
        payload = json.loads(out.read_text())
        total = sum(
            len(frame["features"]["features"]) for frame in payload["frames"]
        )
        assert total == len(temporal_layer)

    def test_returns_resolved_path(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "animated.json"
        result = TemporalExporter.export_animated_geojson(temporal_layer, out)
        assert result.is_absolute()


# ---------------------------------------------------------------------------
# 4. export_temporal_csv — time-sorted CSV
# ---------------------------------------------------------------------------


class TestExportTemporalCsv:
    """Verify export_temporal_csv creates a sorted CSV with all columns."""

    def test_creates_csv_file(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "temporal.csv"
        result = TemporalExporter.export_temporal_csv(temporal_layer, out)
        assert result.exists()
        assert result.suffix == ".csv"

    def test_csv_has_correct_columns(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "temporal.csv"
        TemporalExporter.export_temporal_csv(temporal_layer, out)
        df = pd.read_csv(out)
        expected_cols = {"name", "longitude", "latitude", "timestamp", "event_type", "status"}
        assert expected_cols == set(df.columns)

    def test_csv_sorted_by_time(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "temporal.csv"
        TemporalExporter.export_temporal_csv(temporal_layer, out)
        df = pd.read_csv(out, parse_dates=["timestamp"])
        timestamps = df["timestamp"].tolist()
        assert timestamps == sorted(timestamps)

    def test_csv_row_count_matches(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "temporal.csv"
        TemporalExporter.export_temporal_csv(temporal_layer, out)
        df = pd.read_csv(out)
        assert len(df) == len(temporal_layer)

    def test_returns_resolved_path(
        self, temporal_layer: FeatureLayer, tmp_path: Path
    ) -> None:
        out = tmp_path / "temporal.csv"
        result = TemporalExporter.export_temporal_csv(temporal_layer, out)
        assert result.is_absolute()
