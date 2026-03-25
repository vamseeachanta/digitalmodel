"""Tests for GeoJSON handler read, write, and conversion methods."""

from __future__ import annotations

from pathlib import Path

import pandas as pd
import pytest

from digitalmodel.specialized.gis.io.geojson_handler import GeoJSONHandler


class TestRead:
    """Tests for GeoJSONHandler.read()."""

    def test_read_returns_feature_collection(
        self, sample_wells_geojson: Path
    ) -> None:
        data = GeoJSONHandler.read(sample_wells_geojson)

        assert isinstance(data, dict)
        assert data["type"] == "FeatureCollection"

    def test_read_returns_correct_number_of_features(
        self, sample_wells_geojson: Path
    ) -> None:
        data = GeoJSONHandler.read(sample_wells_geojson)

        assert len(data["features"]) == 7

    def test_read_raises_file_not_found_for_missing_path(
        self, tmp_path: Path
    ) -> None:
        missing = tmp_path / "nonexistent.geojson"

        with pytest.raises(FileNotFoundError):
            GeoJSONHandler.read(missing)


class TestWrite:
    """Tests for GeoJSONHandler.write()."""

    def test_write_creates_file(self, tmp_path: Path) -> None:
        data = GeoJSONHandler.create_feature_collection([])
        output_path = tmp_path / "output.geojson"

        result = GeoJSONHandler.write(data, output_path)

        assert result == output_path
        assert output_path.exists()

    def test_write_produces_readable_geojson(self, tmp_path: Path) -> None:
        feature = GeoJSONHandler.create_point_feature(
            longitude=-90.5, latitude=28.1, properties={"name": "test"}
        )
        data = GeoJSONHandler.create_feature_collection([feature])
        output_path = tmp_path / "output.geojson"

        GeoJSONHandler.write(data, output_path)
        read_back = GeoJSONHandler.read(output_path)

        assert read_back["type"] == "FeatureCollection"
        assert len(read_back["features"]) == 1
        assert read_back["features"][0]["properties"]["name"] == "test"

    def test_write_creates_parent_directories(self, tmp_path: Path) -> None:
        data = GeoJSONHandler.create_feature_collection([])
        nested_path = tmp_path / "sub" / "dir" / "output.geojson"

        GeoJSONHandler.write(data, nested_path)

        assert nested_path.exists()


class TestFeaturesToDataframe:
    """Tests for GeoJSONHandler.features_to_dataframe()."""

    def test_dataframe_has_coordinate_columns(
        self, sample_wells_geojson: Path
    ) -> None:
        data = GeoJSONHandler.read(sample_wells_geojson)
        df = GeoJSONHandler.features_to_dataframe(data["features"])

        assert "longitude" in df.columns
        assert "latitude" in df.columns

    def test_dataframe_has_correct_row_count(
        self, sample_wells_geojson: Path
    ) -> None:
        data = GeoJSONHandler.read(sample_wells_geojson)
        df = GeoJSONHandler.features_to_dataframe(data["features"])

        assert len(df) == 7

    def test_properties_are_flattened_into_columns(
        self, sample_wells_geojson: Path
    ) -> None:
        data = GeoJSONHandler.read(sample_wells_geojson)
        df = GeoJSONHandler.features_to_dataframe(data["features"])

        expected_property_columns = {
            "name",
            "field",
            "block",
            "water_depth_m",
            "status",
            "installation_date",
        }
        assert expected_property_columns.issubset(set(df.columns))

    def test_coordinate_values_are_extracted(
        self, sample_wells_geojson: Path
    ) -> None:
        data = GeoJSONHandler.read(sample_wells_geojson)
        df = GeoJSONHandler.features_to_dataframe(data["features"])

        first_row = df.iloc[0]
        assert first_row["longitude"] == pytest.approx(-90.5)
        assert first_row["latitude"] == pytest.approx(28.1)

    def test_empty_features_returns_empty_dataframe(self) -> None:
        df = GeoJSONHandler.features_to_dataframe([])

        assert len(df) == 0


class TestDataframeToFeatures:
    """Tests for GeoJSONHandler.dataframe_to_features()."""

    def test_converts_dataframe_rows_to_features(self) -> None:
        df = pd.DataFrame(
            {
                "longitude": [-90.5, -89.8],
                "latitude": [28.1, 28.5],
                "name": ["Well-A", "Well-B"],
            }
        )

        features = GeoJSONHandler.dataframe_to_features(df)

        assert len(features) == 2

    def test_features_have_correct_geojson_structure(self) -> None:
        df = pd.DataFrame(
            {
                "longitude": [-90.5],
                "latitude": [28.1],
                "name": ["Well-A"],
            }
        )

        features = GeoJSONHandler.dataframe_to_features(df)
        feature = features[0]

        assert feature["type"] == "Feature"
        assert feature["geometry"]["type"] == "Point"
        assert feature["geometry"]["coordinates"] == [-90.5, 28.1]
        assert feature["properties"]["name"] == "Well-A"

    def test_excludes_coordinate_columns_from_properties(self) -> None:
        df = pd.DataFrame(
            {
                "longitude": [-90.5],
                "latitude": [28.1],
                "name": ["Well-A"],
            }
        )

        features = GeoJSONHandler.dataframe_to_features(df)

        assert "longitude" not in features[0]["properties"]
        assert "latitude" not in features[0]["properties"]

    def test_respects_explicit_properties_cols(self) -> None:
        df = pd.DataFrame(
            {
                "longitude": [-90.5],
                "latitude": [28.1],
                "name": ["Well-A"],
                "status": ["active"],
            }
        )

        features = GeoJSONHandler.dataframe_to_features(
            df, properties_cols=["name"]
        )

        assert "name" in features[0]["properties"]
        assert "status" not in features[0]["properties"]


class TestCreateFeatureCollection:
    """Tests for GeoJSONHandler.create_feature_collection()."""

    def test_wraps_features_in_collection(self) -> None:
        features = [
            GeoJSONHandler.create_point_feature(-90.5, 28.1),
            GeoJSONHandler.create_point_feature(-89.8, 28.5),
        ]

        collection = GeoJSONHandler.create_feature_collection(features)

        assert collection["type"] == "FeatureCollection"
        assert len(collection["features"]) == 2

    def test_empty_features_list(self) -> None:
        collection = GeoJSONHandler.create_feature_collection([])

        assert collection["type"] == "FeatureCollection"
        assert collection["features"] == []


class TestCreatePointFeature:
    """Tests for GeoJSONHandler.create_point_feature()."""

    def test_creates_point_feature_with_coordinates(self) -> None:
        feature = GeoJSONHandler.create_point_feature(
            longitude=-90.5, latitude=28.1
        )

        assert feature["type"] == "Feature"
        assert feature["geometry"]["type"] == "Point"
        assert feature["geometry"]["coordinates"] == [-90.5, 28.1]

    def test_includes_properties_when_provided(self) -> None:
        props = {"name": "Test Well", "depth": 1500.0}

        feature = GeoJSONHandler.create_point_feature(
            longitude=-90.5, latitude=28.1, properties=props
        )

        assert feature["properties"]["name"] == "Test Well"
        assert feature["properties"]["depth"] == 1500.0

    def test_defaults_to_empty_properties(self) -> None:
        feature = GeoJSONHandler.create_point_feature(
            longitude=-90.5, latitude=28.1
        )

        assert feature["properties"] == {}


class TestRoundTrip:
    """Tests for read -> dataframe -> features -> write -> read back."""

    def test_round_trip_preserves_data(
        self, sample_wells_geojson: Path, tmp_path: Path
    ) -> None:
        original = GeoJSONHandler.read(sample_wells_geojson)
        original_features = original["features"]

        df = GeoJSONHandler.features_to_dataframe(original_features)
        reconstructed_features = GeoJSONHandler.dataframe_to_features(df)
        collection = GeoJSONHandler.create_feature_collection(
            reconstructed_features
        )

        output_path = tmp_path / "round_trip.geojson"
        GeoJSONHandler.write(collection, output_path)
        read_back = GeoJSONHandler.read(output_path)

        assert read_back["type"] == "FeatureCollection"
        assert len(read_back["features"]) == len(original_features)

    def test_round_trip_preserves_coordinates(
        self, sample_wells_geojson: Path, tmp_path: Path
    ) -> None:
        original = GeoJSONHandler.read(sample_wells_geojson)

        df = GeoJSONHandler.features_to_dataframe(original["features"])
        features = GeoJSONHandler.dataframe_to_features(df)
        collection = GeoJSONHandler.create_feature_collection(features)

        output_path = tmp_path / "round_trip_coords.geojson"
        GeoJSONHandler.write(collection, output_path)
        read_back = GeoJSONHandler.read(output_path)

        for orig, rebuilt in zip(
            original["features"], read_back["features"]
        ):
            orig_coords = orig["geometry"]["coordinates"]
            rebuilt_coords = rebuilt["geometry"]["coordinates"]
            assert rebuilt_coords[0] == pytest.approx(orig_coords[0])
            assert rebuilt_coords[1] == pytest.approx(orig_coords[1])

    def test_round_trip_preserves_properties(
        self, sample_wells_geojson: Path, tmp_path: Path
    ) -> None:
        original = GeoJSONHandler.read(sample_wells_geojson)

        df = GeoJSONHandler.features_to_dataframe(original["features"])
        features = GeoJSONHandler.dataframe_to_features(df)
        collection = GeoJSONHandler.create_feature_collection(features)

        output_path = tmp_path / "round_trip_props.geojson"
        GeoJSONHandler.write(collection, output_path)
        read_back = GeoJSONHandler.read(output_path)

        for orig, rebuilt in zip(
            original["features"], read_back["features"]
        ):
            orig_props = orig["properties"]
            rebuilt_props = rebuilt["properties"]
            for key in orig_props:
                assert rebuilt_props[key] == orig_props[key], (
                    f"Property '{key}' mismatch"
                )
