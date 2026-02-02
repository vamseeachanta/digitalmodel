"""Tests for KMLHandler — KML/KMZ reading, writing, and conversion."""

from __future__ import annotations

import zipfile
from pathlib import Path

import pandas as pd
import pytest
from lxml import etree

from digitalmodel.gis.io.kml_handler import KMLHandler


# ---------------------------------------------------------------------------
# 1. read_kml — basic parsing
# ---------------------------------------------------------------------------


class TestReadKml:
    """Verify read_kml parses sample_polygons.kml correctly."""

    def test_returns_five_features(self, sample_polygons_kml: Path) -> None:
        features = KMLHandler.read_kml(sample_polygons_kml)
        assert len(features) == 5

    def test_geometry_type_counts(self, sample_polygons_kml: Path) -> None:
        features = KMLHandler.read_kml(sample_polygons_kml)
        type_counts = {}
        for f in features:
            gt = f["geometry_type"]
            type_counts[gt] = type_counts.get(gt, 0) + 1

        assert type_counts["Point"] == 3
        assert type_counts["LineString"] == 1
        assert type_counts["Polygon"] == 1


# ---------------------------------------------------------------------------
# 2. read — auto-detect extension
# ---------------------------------------------------------------------------


class TestRead:
    """Verify read() dispatches correctly based on file extension."""

    def test_read_autodetects_kml(self, sample_polygons_kml: Path) -> None:
        features = KMLHandler.read(sample_polygons_kml)
        assert len(features) == 5

    def test_read_matches_read_kml(self, sample_polygons_kml: Path) -> None:
        from_read = KMLHandler.read(sample_polygons_kml)
        from_read_kml = KMLHandler.read_kml(sample_polygons_kml)
        assert from_read == from_read_kml

    def test_read_unsupported_extension_raises(self, tmp_path: Path) -> None:
        bad_file = tmp_path / "data.shp"
        bad_file.write_text("")
        with pytest.raises(ValueError, match="Unsupported file extension"):
            KMLHandler.read(bad_file)


# ---------------------------------------------------------------------------
# 3. write_kml — output a valid KML file
# ---------------------------------------------------------------------------


class TestWriteKml:
    """Verify write_kml produces a valid KML XML file."""

    def test_write_creates_file(
        self, sample_polygons_kml: Path, tmp_path: Path
    ) -> None:
        features = KMLHandler.read_kml(sample_polygons_kml)
        out_path = tmp_path / "output.kml"
        result = KMLHandler.write_kml(features, out_path)

        assert result.exists()
        assert result.suffix == ".kml"

    def test_written_file_is_valid_xml(
        self, sample_polygons_kml: Path, tmp_path: Path
    ) -> None:
        features = KMLHandler.read_kml(sample_polygons_kml)
        out_path = tmp_path / "output.kml"
        KMLHandler.write_kml(features, out_path)

        tree = etree.parse(str(out_path))
        root = tree.getroot()
        assert root.tag == "{http://www.opengis.net/kml/2.2}kml"

    def test_write_returns_resolved_path(
        self, sample_polygons_kml: Path, tmp_path: Path
    ) -> None:
        features = KMLHandler.read_kml(sample_polygons_kml)
        out_path = tmp_path / "output.kml"
        result = KMLHandler.write_kml(features, out_path)

        assert result.is_absolute()


# ---------------------------------------------------------------------------
# 4. write_kmz — ZIP containing a KML
# ---------------------------------------------------------------------------


class TestWriteKmz:
    """Verify write_kmz produces a valid ZIP archive containing a KML."""

    def test_write_kmz_creates_file(
        self, sample_polygons_kml: Path, tmp_path: Path
    ) -> None:
        features = KMLHandler.read_kml(sample_polygons_kml)
        out_path = tmp_path / "output.kmz"
        result = KMLHandler.write_kmz(features, out_path)

        assert result.exists()
        assert result.suffix == ".kmz"

    def test_kmz_is_valid_zip(
        self, sample_polygons_kml: Path, tmp_path: Path
    ) -> None:
        features = KMLHandler.read_kml(sample_polygons_kml)
        out_path = tmp_path / "output.kmz"
        KMLHandler.write_kmz(features, out_path)

        assert zipfile.is_zipfile(out_path)

    def test_kmz_contains_kml(
        self, sample_polygons_kml: Path, tmp_path: Path
    ) -> None:
        features = KMLHandler.read_kml(sample_polygons_kml)
        out_path = tmp_path / "output.kmz"
        KMLHandler.write_kmz(features, out_path)

        with zipfile.ZipFile(out_path, "r") as zf:
            names = zf.namelist()
            assert any(name.endswith(".kml") for name in names)


# ---------------------------------------------------------------------------
# 5. features_to_dataframe — conversion to DataFrame
# ---------------------------------------------------------------------------


class TestFeaturesToDataframe:
    """Verify features_to_dataframe returns the expected structure."""

    def test_row_count_matches_features(
        self, sample_polygons_kml: Path
    ) -> None:
        features = KMLHandler.read_kml(sample_polygons_kml)
        df = KMLHandler.features_to_dataframe(features)
        assert len(df) == len(features)

    def test_expected_columns_present(
        self, sample_polygons_kml: Path
    ) -> None:
        features = KMLHandler.read_kml(sample_polygons_kml)
        df = KMLHandler.features_to_dataframe(features)

        for col in ("name", "description", "geometry_type", "longitude", "latitude", "coordinates"):
            assert col in df.columns, f"Missing column: {col}"


# ---------------------------------------------------------------------------
# 6. dataframe_to_features — conversion from DataFrame
# ---------------------------------------------------------------------------


class TestDataframeToFeatures:
    """Verify dataframe_to_features creates valid feature dicts."""

    def test_converts_dataframe_rows_to_features(self) -> None:
        df = pd.DataFrame(
            {
                "longitude": [-90.5, -89.8],
                "latitude": [28.1, 28.5],
                "label": ["A", "B"],
            }
        )
        features = KMLHandler.dataframe_to_features(df)

        assert len(features) == 2
        assert all(f["geometry_type"] == "Point" for f in features)

    def test_name_col_used_when_provided(self) -> None:
        df = pd.DataFrame(
            {
                "longitude": [-90.5],
                "latitude": [28.1],
                "site": ["Alpha"],
            }
        )
        features = KMLHandler.dataframe_to_features(df, name_col="site")

        assert features[0]["name"] == "Alpha"

    def test_coordinates_populated(self) -> None:
        df = pd.DataFrame(
            {
                "longitude": [-90.5],
                "latitude": [28.1],
            }
        )
        features = KMLHandler.dataframe_to_features(df)

        assert features[0]["coordinates"] == [(-90.5, 28.1)]


# ---------------------------------------------------------------------------
# 7. Round-trip: read -> write -> read
# ---------------------------------------------------------------------------


class TestRoundTrip:
    """Verify data survives a read_kml -> write_kml -> read_kml cycle."""

    def test_feature_names_preserved(
        self, sample_polygons_kml: Path, tmp_path: Path
    ) -> None:
        original = KMLHandler.read_kml(sample_polygons_kml)
        out_path = tmp_path / "roundtrip.kml"
        KMLHandler.write_kml(original, out_path)
        reloaded = KMLHandler.read_kml(out_path)

        original_names = [f["name"] for f in original]
        reloaded_names = [f["name"] for f in reloaded]
        assert original_names == reloaded_names

    def test_feature_count_preserved(
        self, sample_polygons_kml: Path, tmp_path: Path
    ) -> None:
        original = KMLHandler.read_kml(sample_polygons_kml)
        out_path = tmp_path / "roundtrip.kml"
        KMLHandler.write_kml(original, out_path)
        reloaded = KMLHandler.read_kml(out_path)

        assert len(reloaded) == len(original)


# ---------------------------------------------------------------------------
# 8. Feature structure — required keys
# ---------------------------------------------------------------------------


class TestFeatureStructure:
    """Verify each parsed feature contains the expected dictionary keys."""

    REQUIRED_KEYS = {"name", "coordinates", "geometry_type"}

    def test_all_features_have_required_keys(
        self, sample_polygons_kml: Path
    ) -> None:
        features = KMLHandler.read_kml(sample_polygons_kml)

        for feature in features:
            for key in self.REQUIRED_KEYS:
                assert key in feature, (
                    f"Feature '{feature.get('name')}' missing key '{key}'"
                )

    def test_coordinates_are_list_of_tuples(
        self, sample_polygons_kml: Path
    ) -> None:
        features = KMLHandler.read_kml(sample_polygons_kml)

        for feature in features:
            assert isinstance(feature["coordinates"], list)
            for coord in feature["coordinates"]:
                assert isinstance(coord, tuple)

    def test_geometry_type_is_valid(
        self, sample_polygons_kml: Path
    ) -> None:
        features = KMLHandler.read_kml(sample_polygons_kml)
        valid_types = {"Point", "LineString", "Polygon"}

        for feature in features:
            assert feature["geometry_type"] in valid_types
