"""Tests for GeoJSON format round-trip: write -> read -> compare.

TDD: Validates Phase 2a core format support.
"""
from __future__ import annotations

import json
import tempfile
from pathlib import Path

import pandas as pd
import pytest

from digitalmodel.gis.io.geojson_handler import GeoJSONHandler


class TestGeoJSONRoundTrip:
    def _sample_feature_collection(self):
        return {
            "type": "FeatureCollection",
            "features": [
                {
                    "type": "Feature",
                    "geometry": {"type": "Point", "coordinates": [-90.5, 28.5]},
                    "properties": {"name": "Well-A", "depth": 100.0},
                },
                {
                    "type": "Feature",
                    "geometry": {"type": "Point", "coordinates": [-89.8, 29.1]},
                    "properties": {"name": "Well-B", "depth": 200.0},
                },
            ],
        }

    def test_write_read_roundtrip(self, tmp_path):
        fc = self._sample_feature_collection()
        out_path = tmp_path / "wells.geojson"
        GeoJSONHandler.write(fc, out_path)
        loaded = GeoJSONHandler.read(out_path)

        assert loaded["type"] == "FeatureCollection"
        assert len(loaded["features"]) == 2
        assert loaded["features"][0]["properties"]["name"] == "Well-A"

    def test_write_read_preserves_coordinates(self, tmp_path):
        fc = self._sample_feature_collection()
        out_path = tmp_path / "wells.geojson"
        GeoJSONHandler.write(fc, out_path)
        loaded = GeoJSONHandler.read(out_path)

        coords_0 = loaded["features"][0]["geometry"]["coordinates"]
        assert coords_0 == [-90.5, 28.5]

    def test_read_nonexistent_file_raises(self, tmp_path):
        with pytest.raises(FileNotFoundError):
            GeoJSONHandler.read(tmp_path / "missing.geojson")

    def test_features_to_dataframe_columns(self):
        fc = self._sample_feature_collection()
        df = GeoJSONHandler.features_to_dataframe(fc["features"])
        assert "longitude" in df.columns
        assert "latitude" in df.columns
        assert "name" in df.columns

    def test_features_to_dataframe_row_count(self):
        fc = self._sample_feature_collection()
        df = GeoJSONHandler.features_to_dataframe(fc["features"])
        assert len(df) == 2

    def test_dataframe_to_features_roundtrip(self):
        df = pd.DataFrame(
            {
                "longitude": [-90.5, -89.8],
                "latitude": [28.5, 29.1],
                "name": ["A", "B"],
            }
        )
        features = GeoJSONHandler.dataframe_to_features(df)
        assert len(features) == 2
        assert features[0]["geometry"]["type"] == "Point"
        assert features[0]["geometry"]["coordinates"] == [-90.5, 28.5]
        assert features[0]["properties"]["name"] == "A"

    def test_create_point_feature(self):
        feature = GeoJSONHandler.create_point_feature(-90.5, 28.5, {"well": "A1"})
        assert feature["type"] == "Feature"
        assert feature["geometry"]["type"] == "Point"
        assert feature["geometry"]["coordinates"] == [-90.5, 28.5]
        assert feature["properties"]["well"] == "A1"

    def test_create_feature_collection_wraps_correctly(self):
        features = [
            GeoJSONHandler.create_point_feature(-90.5, 28.5),
            GeoJSONHandler.create_point_feature(-89.8, 29.1),
        ]
        fc = GeoJSONHandler.create_feature_collection(features)
        assert fc["type"] == "FeatureCollection"
        assert len(fc["features"]) == 2
