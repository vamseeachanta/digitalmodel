"""Tests for QGIS export integration.

TDD: Tests validate QGIS project file generation and QML style definition
without requiring a QGIS runtime. XML structure is validated using the
standard library xml.etree.ElementTree.
"""
from __future__ import annotations

import tempfile
import xml.etree.ElementTree as ET
from pathlib import Path

import pandas as pd
import pytest

from digitalmodel.gis.integrations.qgis_export import QGISExporter
from digitalmodel.gis.layers.well_layer import WellLayer


@pytest.fixture()
def well_layer():
    df = pd.DataFrame(
        {
            "longitude": [-90.5, -89.8],
            "latitude": [28.5, 29.1],
            "well_name": ["A-1", "B-2"],
            "water_depth_m": [150.0, 300.0],
            "status": ["producing", "abandoned"],
        }
    )
    return WellLayer(data=df, name="offshore_wells")


# ---------------------------------------------------------------------------
# QGS project file
# ---------------------------------------------------------------------------


class TestQGSProjectGeneration:
    def test_generate_project_returns_string(self, well_layer):
        qgs = QGISExporter.generate_project(well_layer)
        assert isinstance(qgs, str)
        assert len(qgs) > 0

    def test_generated_project_is_valid_xml(self, well_layer):
        qgs = QGISExporter.generate_project(well_layer)
        root = ET.fromstring(qgs)
        assert root is not None

    def test_project_root_element_is_qgis(self, well_layer):
        qgs = QGISExporter.generate_project(well_layer)
        root = ET.fromstring(qgs)
        assert root.tag == "qgis"

    def test_project_contains_layer_name(self, well_layer):
        qgs = QGISExporter.generate_project(well_layer)
        assert "offshore_wells" in qgs

    def test_project_contains_well_coordinates(self, well_layer):
        qgs = QGISExporter.generate_project(well_layer)
        assert "-90.5" in qgs or "-90" in qgs

    def test_write_project_creates_file(self, well_layer, tmp_path):
        out_path = tmp_path / "wells.qgs"
        result = QGISExporter.write_project(well_layer, out_path)
        assert result.exists()

    def test_write_project_returns_path(self, well_layer, tmp_path):
        out_path = tmp_path / "wells.qgs"
        result = QGISExporter.write_project(well_layer, out_path)
        assert isinstance(result, Path)

    def test_written_project_is_valid_xml(self, well_layer, tmp_path):
        out_path = tmp_path / "wells.qgs"
        QGISExporter.write_project(well_layer, out_path)
        content = out_path.read_text(encoding="utf-8")
        root = ET.fromstring(content)
        assert root.tag == "qgis"

    def test_creates_parent_directories(self, well_layer, tmp_path):
        out_path = tmp_path / "subdir" / "nested" / "wells.qgs"
        QGISExporter.write_project(well_layer, out_path)
        assert out_path.exists()

    def test_project_has_map_layers_element(self, well_layer):
        qgs = QGISExporter.generate_project(well_layer)
        root = ET.fromstring(qgs)
        # Either projectlayers or maplayers element should exist
        found = (
            root.find(".//projectlayers") is not None
            or root.find(".//maplayers") is not None
        )
        assert found


# ---------------------------------------------------------------------------
# QML style file
# ---------------------------------------------------------------------------


class TestQMLStyleGeneration:
    def test_generate_qml_returns_string(self, well_layer):
        qml = QGISExporter.generate_well_qml(well_layer)
        assert isinstance(qml, str)
        assert len(qml) > 0

    def test_generated_qml_is_valid_xml(self, well_layer):
        qml = QGISExporter.generate_well_qml(well_layer)
        root = ET.fromstring(qml)
        assert root is not None

    def test_qml_root_element_is_qgis(self, well_layer):
        qml = QGISExporter.generate_well_qml(well_layer)
        root = ET.fromstring(qml)
        assert root.tag == "qgis"

    def test_write_qml_creates_file(self, well_layer, tmp_path):
        out_path = tmp_path / "wells.qml"
        result = QGISExporter.write_well_qml(well_layer, out_path)
        assert result.exists()

    def test_write_qml_returns_path(self, well_layer, tmp_path):
        out_path = tmp_path / "wells.qml"
        result = QGISExporter.write_well_qml(well_layer, out_path)
        assert isinstance(result, Path)


# ---------------------------------------------------------------------------
# GeoJSON layer generation
# ---------------------------------------------------------------------------


class TestQGISGeoJSONLayer:
    def test_generate_geojson_layer_returns_dict(self, well_layer):
        fc = QGISExporter.to_geojson_layer(well_layer)
        assert fc["type"] == "FeatureCollection"
        assert isinstance(fc["features"], list)

    def test_geojson_layer_feature_count(self, well_layer):
        fc = QGISExporter.to_geojson_layer(well_layer)
        assert len(fc["features"]) == 2

    def test_geojson_layer_has_well_properties(self, well_layer):
        fc = QGISExporter.to_geojson_layer(well_layer)
        props = fc["features"][0]["properties"]
        assert "well_name" in props or "status" in props
