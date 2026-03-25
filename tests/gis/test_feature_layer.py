"""Tests for FeatureLayer and WellLayer data model classes.

TDD: These tests are written before the implementation.
"""
from __future__ import annotations

import pytest
import pandas as pd

from digitalmodel.gis.layers.feature_layer import FeatureLayer
from digitalmodel.gis.layers.well_layer import WellLayer
from digitalmodel.gis.core.geometry import GeoPoint, GeoBoundingBox


# ---------------------------------------------------------------------------
# FeatureLayer
# ---------------------------------------------------------------------------


class TestFeatureLayerConstruction:
    def test_construct_from_dataframe(self):
        df = pd.DataFrame(
            {
                "longitude": [-90.0, -89.5],
                "latitude": [29.0, 29.5],
                "name": ["A", "B"],
            }
        )
        layer = FeatureLayer(data=df, name="test_layer")
        assert layer.name == "test_layer"
        assert len(layer.data) == 2

    def test_default_coordinate_column_names(self):
        df = pd.DataFrame({"longitude": [-90.0], "latitude": [29.0]})
        layer = FeatureLayer(data=df, name="pts")
        assert layer.lon_col == "longitude"
        assert layer.lat_col == "latitude"

    def test_custom_coordinate_column_names(self):
        df = pd.DataFrame({"lon": [-90.0], "lat": [29.0]})
        layer = FeatureLayer(data=df, name="pts", lon_col="lon", lat_col="lat")
        assert layer.lon_col == "lon"
        assert layer.lat_col == "lat"

    def test_missing_lon_column_raises(self):
        df = pd.DataFrame({"latitude": [29.0]})
        with pytest.raises(KeyError):
            FeatureLayer(data=df, name="bad")

    def test_missing_lat_column_raises(self):
        df = pd.DataFrame({"longitude": [-90.0]})
        with pytest.raises(KeyError):
            FeatureLayer(data=df, name="bad")


class TestFeatureLayerProperties:
    def setup_method(self):
        self.df = pd.DataFrame(
            {
                "longitude": [-90.0, -89.0, -88.0],
                "latitude": [28.0, 29.0, 30.0],
            }
        )
        self.layer = FeatureLayer(data=self.df, name="layer")

    def test_centroid_is_geopoint(self):
        centroid = self.layer.centroid
        assert isinstance(centroid, GeoPoint)

    def test_centroid_is_mean_of_coordinates(self):
        centroid = self.layer.centroid
        assert abs(centroid.x - (-89.0)) < 1e-9
        assert abs(centroid.y - 29.0) < 1e-9

    def test_bounding_box_type(self):
        bbox = self.layer.bounding_box
        assert isinstance(bbox, GeoBoundingBox)

    def test_bounding_box_extents(self):
        bbox = self.layer.bounding_box
        assert bbox.min_x == pytest.approx(-90.0)
        assert bbox.max_x == pytest.approx(-88.0)
        assert bbox.min_y == pytest.approx(28.0)
        assert bbox.max_y == pytest.approx(30.0)

    def test_len_returns_row_count(self):
        assert len(self.layer) == 3


class TestFeatureLayerFilter:
    def setup_method(self):
        self.df = pd.DataFrame(
            {
                "longitude": [-90.0, -89.0, -88.0],
                "latitude": [28.0, 29.0, 30.0],
                "type": ["A", "B", "A"],
            }
        )
        self.layer = FeatureLayer(data=self.df, name="layer")

    def test_filter_returns_new_layer(self):
        mask = self.layer.data["type"] == "A"
        filtered = self.layer.filter(mask)
        assert isinstance(filtered, FeatureLayer)
        assert filtered is not self.layer

    def test_filter_reduces_rows(self):
        mask = self.layer.data["type"] == "A"
        filtered = self.layer.filter(mask)
        assert len(filtered) == 2

    def test_filter_preserves_name(self):
        mask = self.layer.data["type"] == "A"
        filtered = self.layer.filter(mask)
        assert filtered.name == self.layer.name

    def test_from_geojson_feature_collection(self):
        fc = {
            "type": "FeatureCollection",
            "features": [
                {
                    "type": "Feature",
                    "geometry": {"type": "Point", "coordinates": [-90.0, 29.0]},
                    "properties": {"name": "W1"},
                },
                {
                    "type": "Feature",
                    "geometry": {"type": "Point", "coordinates": [-89.0, 28.0]},
                    "properties": {"name": "W2"},
                },
            ],
        }
        layer = FeatureLayer.from_geojson(fc, name="wells")
        assert len(layer) == 2
        assert "name" in layer.data.columns

    def test_to_geojson_roundtrip(self):
        layer = FeatureLayer.from_geojson(
            {
                "type": "FeatureCollection",
                "features": [
                    {
                        "type": "Feature",
                        "geometry": {"type": "Point", "coordinates": [-90.0, 29.0]},
                        "properties": {"well_name": "A1"},
                    }
                ],
            },
            name="roundtrip",
        )
        fc = layer.to_geojson()
        assert fc["type"] == "FeatureCollection"
        assert len(fc["features"]) == 1
        feature = fc["features"][0]
        assert feature["geometry"]["coordinates"] == [-90.0, 29.0]
        assert feature["properties"]["well_name"] == "A1"


# ---------------------------------------------------------------------------
# WellLayer
# ---------------------------------------------------------------------------


class TestWellLayerConstruction:
    def _make_df(self, **extra):
        data = {
            "longitude": [-90.0, -89.5],
            "latitude": [29.0, 28.5],
            "well_name": ["W1", "W2"],
            "water_depth_m": [100.0, 200.0],
            "status": ["producing", "abandoned"],
        }
        data.update(extra)
        return pd.DataFrame(data)

    def test_construct_basic(self):
        layer = WellLayer(data=self._make_df(), name="wells")
        assert layer.name == "wells"
        assert len(layer) == 2

    def test_is_feature_layer_subclass(self):
        layer = WellLayer(data=self._make_df(), name="wells")
        assert isinstance(layer, FeatureLayer)

    def test_well_name_column_defaults(self):
        layer = WellLayer(data=self._make_df(), name="wells")
        assert layer.well_name_col == "well_name"

    def test_filter_by_status(self):
        layer = WellLayer(data=self._make_df(), name="wells")
        producing = layer.filter_by_status("producing")
        assert len(producing) == 1
        assert producing.data["status"].iloc[0] == "producing"

    def test_filter_by_status_returns_well_layer(self):
        layer = WellLayer(data=self._make_df(), name="wells")
        result = layer.filter_by_status("producing")
        assert isinstance(result, WellLayer)

    def test_filter_by_status_no_status_col_raises(self):
        df = self._make_df()
        df = df.drop(columns=["status"])
        layer = WellLayer(data=df, name="wells")
        with pytest.raises(KeyError):
            layer.filter_by_status("producing")

    def test_depth_range_filter(self):
        df = self._make_df()
        layer = WellLayer(data=df, name="wells")
        deep = layer.filter_by_depth_range(min_depth_m=150.0)
        assert len(deep) == 1
        assert deep.data["water_depth_m"].iloc[0] == 200.0

    def test_depth_range_filter_max(self):
        df = self._make_df()
        layer = WellLayer(data=df, name="wells")
        shallow = layer.filter_by_depth_range(max_depth_m=150.0)
        assert len(shallow) == 1
        assert shallow.data["water_depth_m"].iloc[0] == 100.0

    def test_to_well_list(self):
        layer = WellLayer(data=self._make_df(), name="wells")
        wells = layer.to_well_list()
        assert isinstance(wells, list)
        assert len(wells) == 2
        assert "well_name" in wells[0]
        assert "longitude" in wells[0]
        assert "latitude" in wells[0]
