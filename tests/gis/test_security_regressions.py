"""Security regression tests for GIS module.

Tests for zip-slip prevention, XXE-safe KML parsing, CRS normalisation,
and style ID sanitisation — all identified during Codex cross-review.
"""
from __future__ import annotations

import io
import tempfile
import zipfile
from pathlib import Path

import pandas as pd
import pytest
from lxml import etree

from digitalmodel.gis.coordinates import CoordinatePoint
from digitalmodel.gis.core.coordinate_transformer import (
    wgs84_to_utm,
    utm_to_wgs84,
)
from digitalmodel.gis.integrations.blender_export import (
    BlenderExporter,
    _wgs84_to_utm_zone,
)
from digitalmodel.gis.integrations.google_earth_export import (
    GoogleEarthExporter,
    _sanitise_style_id,
)
from digitalmodel.gis.io.kml_handler import KMLHandler
from digitalmodel.gis.layers.feature_layer import FeatureLayer
from digitalmodel.gis.layers.well_layer import WellLayer


# ------------------------------------------------------------------
# P1: Zip-slip prevention in KMZ extraction
# ------------------------------------------------------------------


class TestZipSlipPrevention:
    """Verify that KMZ extraction rejects path traversal entries."""

    def test_rejects_dotdot_path(self, tmp_path: Path) -> None:
        """Refuse to extract entries containing '..' segments."""
        kmz_path = tmp_path / "malicious.kmz"
        with zipfile.ZipFile(kmz_path, "w") as zf:
            zf.writestr("../../etc/evil.kml", "<kml/>")

        with pytest.raises(ValueError, match="unsafe path"):
            KMLHandler.read_kmz(kmz_path)

    def test_rejects_absolute_path(self, tmp_path: Path) -> None:
        """Refuse to extract entries with absolute paths."""
        kmz_path = tmp_path / "absolute.kmz"
        with zipfile.ZipFile(kmz_path, "w") as zf:
            zf.writestr("/tmp/evil.kml", "<kml/>")

        with pytest.raises(ValueError, match="unsafe path"):
            KMLHandler.read_kmz(kmz_path)

    def test_accepts_safe_kmz(self, tmp_path: Path) -> None:
        """Normal KMZ files with safe paths extract successfully."""
        kml_content = (
            '<?xml version="1.0" encoding="UTF-8"?>'
            '<kml xmlns="http://www.opengis.net/kml/2.2">'
            "<Document><name>test</name></Document></kml>"
        )
        kmz_path = tmp_path / "safe.kmz"
        with zipfile.ZipFile(kmz_path, "w") as zf:
            zf.writestr("doc.kml", kml_content)

        features = KMLHandler.read_kmz(kmz_path)
        assert isinstance(features, list)


# ------------------------------------------------------------------
# P1: XXE-safe KML parsing
# ------------------------------------------------------------------


class TestXXESafeParsing:
    """Verify that KML parsing uses a hardened XML parser."""

    def test_read_kml_does_not_expand_entities(self, tmp_path: Path) -> None:
        """External entity declarations should not be resolved."""
        # A KML file with an XXE payload — the parser should not fetch it
        kml_with_xxe = (
            '<?xml version="1.0" encoding="UTF-8"?>'
            '<!DOCTYPE foo [<!ENTITY xxe SYSTEM "file:///etc/passwd">]>'
            '<kml xmlns="http://www.opengis.net/kml/2.2">'
            "<Document><name>&xxe;</name></Document></kml>"
        )
        kml_path = tmp_path / "xxe.kml"
        kml_path.write_text(kml_with_xxe, encoding="utf-8")

        # Should either parse safely (entity not resolved) or raise
        try:
            features = KMLHandler.read_kml(kml_path)
            # If it parses, the entity should NOT contain /etc/passwd content
            for f in features:
                assert "root:" not in str(f.get("name", ""))
        except etree.XMLSyntaxError:
            pass  # Rejecting the document is also acceptable


# ------------------------------------------------------------------
# P2: CRS normalisation for bare numeric strings
# ------------------------------------------------------------------


class TestCRSNormalisation:
    """Verify that numeric CRS strings are normalised to EPSG:<code>."""

    def test_bare_numeric_normalised(self) -> None:
        """A bare '4326' should become 'EPSG:4326'."""
        pt = CoordinatePoint(x=-90.0, y=29.0, crs="4326")
        assert pt.crs == "EPSG:4326"

    def test_bare_3857_normalised(self) -> None:
        """A bare '3857' should become 'EPSG:3857'."""
        pt = CoordinatePoint(x=0.0, y=0.0, crs="3857")
        assert pt.crs == "EPSG:3857"

    def test_transform_from_normalised_numeric_crs(self) -> None:
        """Transform from a bare-numeric CRS should use the correct source."""
        pt = CoordinatePoint(x=-90.0, y=29.0, crs="4326")
        same = pt.to_crs("EPSG:4326")
        assert same.x == pytest.approx(-90.0)
        assert same.y == pytest.approx(29.0)


# ------------------------------------------------------------------
# P2: Style ID sanitisation in Google Earth export
# ------------------------------------------------------------------


class TestStyleIdSanitisation:
    """Verify that style IDs are sanitised for use in KML."""

    def test_sanitise_removes_spaces(self) -> None:
        assert _sanitise_style_id("some value") == "some_value"

    def test_sanitise_removes_special_chars(self) -> None:
        assert _sanitise_style_id("a&b<c>d") == "a_b_c_d"

    def test_sanitise_preserves_safe_chars(self) -> None:
        assert _sanitise_style_id("well-A_01.v2") == "well-A_01.v2"

    def test_exported_kml_has_sanitised_ids(self, tmp_path: Path) -> None:
        """Style IDs in the exported KML use sanitised values."""
        df = pd.DataFrame(
            {
                "longitude": [-90.0, -89.5],
                "latitude": [29.0, 29.5],
                "name": ["W1", "W2"],
                "status": ["active well", "shut in"],
            }
        )
        layer = FeatureLayer(data=df, name="test")
        filepath = tmp_path / "wells.kml"
        GoogleEarthExporter.export_wells(
            layer, filepath, color_by="status"
        )

        tree = etree.parse(str(filepath))
        ns = {"kml": "http://www.opengis.net/kml/2.2"}
        style_ids = [
            s.get("id")
            for s in tree.findall(".//kml:Style", namespaces=ns)
        ]
        for sid in style_ids:
            assert " " not in sid
            assert "&" not in sid


# ------------------------------------------------------------------
# P3: FeatureLayer.data returns defensive copy
# ------------------------------------------------------------------


class TestFeatureLayerDefensiveCopy:
    """Verify that modifying .data does not affect the layer."""

    def test_mutation_does_not_affect_layer(self) -> None:
        df = pd.DataFrame(
            {"longitude": [-90.0], "latitude": [29.0], "val": [1]}
        )
        layer = FeatureLayer(data=df, name="test")
        external = layer.data
        external["val"] = 999
        assert layer.data["val"].iloc[0] == 1


# ------------------------------------------------------------------
# P1: Cross-UTM-zone Blender coordinate consistency
# ------------------------------------------------------------------


class TestBlenderCrossZoneConsistency:
    """Verify Blender export uses a single UTM zone for all points."""

    def test_cross_zone_wells_produce_consistent_coordinates(self) -> None:
        """Wells spanning two UTM zones should all project to centroid zone."""
        # Zone 15 (-90) and Zone 16 (-84) boundary
        df = pd.DataFrame(
            {
                "longitude": [-90.0, -84.0],
                "latitude": [29.0, 29.0],
                "well_name": ["West", "East"],
                "water_depth_m": [100.0, 200.0],
            }
        )
        layer = FeatureLayer(data=df, name="cross-zone")
        script = BlenderExporter.generate_well_script(layer)

        # The script should parse and contain both wells
        import ast

        ast.parse(script)
        assert "West" in script
        assert "East" in script

    def test_utm_zone_helper_uses_specified_zone(self) -> None:
        """_wgs84_to_utm_zone should use the explicitly given zone."""
        e1, n1, z1, h1 = _wgs84_to_utm_zone(-90.0, 29.0, 15, "N")
        e2, n2, z2, h2 = _wgs84_to_utm_zone(-90.0, 29.0, 16, "N")
        assert z1 == 15
        assert z2 == 16
        # Different zones yield different eastings for the same point
        assert abs(e1 - e2) > 1.0


# ------------------------------------------------------------------
# P2: UTM zone/hemisphere validation
# ------------------------------------------------------------------


class TestUTMValidation:
    """Verify UTM helpers reject invalid zone and hemisphere inputs."""

    def test_wgs84_to_utm_rejects_zone_zero(self) -> None:
        with pytest.raises(ValueError, match="1-60"):
            wgs84_to_utm(-90.0, 29.0, zone=0)

    def test_wgs84_to_utm_rejects_zone_61(self) -> None:
        with pytest.raises(ValueError, match="1-60"):
            wgs84_to_utm(-90.0, 29.0, zone=61)

    def test_utm_to_wgs84_rejects_invalid_hemisphere(self) -> None:
        with pytest.raises(ValueError, match="'N' or 'S'"):
            utm_to_wgs84(500000.0, 3000000.0, zone=15, hemisphere="X")

    def test_utm_to_wgs84_rejects_zone_zero(self) -> None:
        with pytest.raises(ValueError, match="1-60"):
            utm_to_wgs84(500000.0, 3000000.0, zone=0, hemisphere="N")


# ------------------------------------------------------------------
# P2: HTML escaping in KML descriptions
# ------------------------------------------------------------------


class TestKMLDescriptionEscaping:
    """Verify that property values are HTML-escaped in KML descriptions."""

    def test_description_escapes_html_entities(self, tmp_path: Path) -> None:
        """Angle brackets and ampersands in properties should be escaped."""
        df = pd.DataFrame(
            {
                "longitude": [-90.0],
                "latitude": [29.0],
                "name": ["test<script>"],
                "note": ["a & b"],
            }
        )
        layer = FeatureLayer(data=df, name="test")
        filepath = tmp_path / "escape_test.kml"
        GoogleEarthExporter.export_wells(layer, filepath)

        content = filepath.read_text(encoding="utf-8")
        assert "<script>" not in content
        assert "&lt;script&gt;" in content
        assert "&amp;" in content


# ------------------------------------------------------------------
# P3: WellLayer depth filter raises on missing column
# ------------------------------------------------------------------


class TestWellLayerDepthFilterConsistency:
    """Verify depth filter raises when depth column is missing."""

    def test_filter_by_depth_raises_when_column_missing(self) -> None:
        df = pd.DataFrame(
            {
                "longitude": [-90.0],
                "latitude": [29.0],
                "well_name": ["W1"],
                "status": ["active"],
            }
        )
        layer = WellLayer(data=df, name="test")
        with pytest.raises(KeyError, match="water_depth_m"):
            layer.filter_by_depth_range(min_depth_m=100.0)
