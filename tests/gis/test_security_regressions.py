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
from digitalmodel.gis.integrations.google_earth_export import (
    GoogleEarthExporter,
    _sanitise_style_id,
)
from digitalmodel.gis.io.kml_handler import KMLHandler
from digitalmodel.gis.layers.feature_layer import FeatureLayer


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
