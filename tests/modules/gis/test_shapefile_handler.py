"""Tests for Shapefile handler read, write, and conversion methods."""

from __future__ import annotations

from pathlib import Path
from unittest.mock import patch

import pandas as pd
import pytest

from digitalmodel.modules.gis.io.shapefile_handler import (
    HAS_GEOPANDAS,
    ShapefileHandler,
)

requires_geopandas = pytest.mark.skipif(
    not HAS_GEOPANDAS, reason="geopandas not installed"
)


def _sample_features() -> list[dict]:
    """Return a small set of Point features for testing."""
    return [
        {
            "geometry": {
                "type": "Point",
                "coordinates": [-90.5, 28.1],
            },
            "properties": {"name": "Well-A", "depth_m": 1500.0},
        },
        {
            "geometry": {
                "type": "Point",
                "coordinates": [-89.8, 28.5],
            },
            "properties": {"name": "Well-B", "depth_m": 2000.0},
        },
    ]


@requires_geopandas
class TestReadWrite:
    """Tests for ShapefileHandler.read() and write() roundtrip."""

    def test_write_creates_shapefile(self, tmp_path: Path) -> None:
        output = tmp_path / "test.shp"

        result = ShapefileHandler.write(_sample_features(), output)

        assert result == output
        assert output.exists()

    def test_roundtrip_preserves_feature_count(self, tmp_path: Path) -> None:
        shp_path = tmp_path / "roundtrip.shp"
        original = _sample_features()

        ShapefileHandler.write(original, shp_path)
        read_back = ShapefileHandler.read(shp_path)

        assert len(read_back) == len(original)

    def test_roundtrip_preserves_coordinates(self, tmp_path: Path) -> None:
        shp_path = tmp_path / "coords.shp"
        original = _sample_features()

        ShapefileHandler.write(original, shp_path)
        read_back = ShapefileHandler.read(shp_path)

        for orig, rebuilt in zip(original, read_back):
            orig_coords = orig["geometry"]["coordinates"]
            rebuilt_coords = rebuilt["geometry"]["coordinates"]
            assert rebuilt_coords[0] == pytest.approx(orig_coords[0])
            assert rebuilt_coords[1] == pytest.approx(orig_coords[1])

    def test_roundtrip_preserves_properties(self, tmp_path: Path) -> None:
        shp_path = tmp_path / "props.shp"
        original = _sample_features()

        ShapefileHandler.write(original, shp_path)
        read_back = ShapefileHandler.read(shp_path)

        for orig, rebuilt in zip(original, read_back):
            for key in orig["properties"]:
                assert rebuilt["properties"][key] == pytest.approx(
                    orig["properties"][key]
                ), f"Property '{key}' mismatch"

    def test_read_raises_file_not_found(self, tmp_path: Path) -> None:
        missing = tmp_path / "nonexistent.shp"

        with pytest.raises(FileNotFoundError):
            ShapefileHandler.read(missing)

    def test_write_creates_parent_directories(self, tmp_path: Path) -> None:
        nested = tmp_path / "sub" / "dir" / "output.shp"

        ShapefileHandler.write(_sample_features(), nested)

        assert nested.exists()

    def test_feature_geometry_has_type_and_coordinates(
        self, tmp_path: Path
    ) -> None:
        shp_path = tmp_path / "geom.shp"
        ShapefileHandler.write(_sample_features(), shp_path)

        read_back = ShapefileHandler.read(shp_path)

        feature = read_back[0]
        assert "type" in feature["geometry"]
        assert "coordinates" in feature["geometry"]


class TestFeaturesToDataframe:
    """Tests for ShapefileHandler.features_to_dataframe()."""

    def test_dataframe_has_coordinate_columns(self) -> None:
        df = ShapefileHandler.features_to_dataframe(_sample_features())

        assert "longitude" in df.columns
        assert "latitude" in df.columns

    def test_dataframe_has_correct_row_count(self) -> None:
        df = ShapefileHandler.features_to_dataframe(_sample_features())

        assert len(df) == 2

    def test_properties_are_flattened_into_columns(self) -> None:
        df = ShapefileHandler.features_to_dataframe(_sample_features())

        assert "name" in df.columns
        assert "depth_m" in df.columns

    def test_coordinate_values_are_extracted(self) -> None:
        df = ShapefileHandler.features_to_dataframe(_sample_features())

        first = df.iloc[0]
        assert first["longitude"] == pytest.approx(-90.5)
        assert first["latitude"] == pytest.approx(28.1)

    def test_empty_features_returns_empty_dataframe(self) -> None:
        df = ShapefileHandler.features_to_dataframe([])

        assert len(df) == 0


class TestDataframeToFeatures:
    """Tests for ShapefileHandler.dataframe_to_features()."""

    def test_converts_dataframe_rows_to_features(self) -> None:
        df = pd.DataFrame(
            {
                "longitude": [-90.5, -89.8],
                "latitude": [28.1, 28.5],
                "name": ["Well-A", "Well-B"],
            }
        )

        features = ShapefileHandler.dataframe_to_features(df)

        assert len(features) == 2

    def test_features_have_correct_structure(self) -> None:
        df = pd.DataFrame(
            {
                "longitude": [-90.5],
                "latitude": [28.1],
                "name": ["Well-A"],
            }
        )

        features = ShapefileHandler.dataframe_to_features(df)
        feature = features[0]

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

        features = ShapefileHandler.dataframe_to_features(df)

        assert "longitude" not in features[0]["properties"]
        assert "latitude" not in features[0]["properties"]


class TestMissingGeopandas:
    """Tests for graceful handling when geopandas is not available."""

    def test_read_raises_import_error_when_geopandas_missing(
        self, tmp_path: Path
    ) -> None:
        with patch(
            "digitalmodel.modules.gis.io.shapefile_handler.HAS_GEOPANDAS",
            False,
        ):
            with pytest.raises(ImportError, match="geopandas is required"):
                ShapefileHandler.read(tmp_path / "test.shp")

    def test_write_raises_import_error_when_geopandas_missing(
        self, tmp_path: Path
    ) -> None:
        with patch(
            "digitalmodel.modules.gis.io.shapefile_handler.HAS_GEOPANDAS",
            False,
        ):
            with pytest.raises(ImportError, match="geopandas is required"):
                ShapefileHandler.write([], tmp_path / "test.shp")
