"""Tests for QGISScriptGenerator."""
from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.modules.gis.scripts.qgis.qgis_processing import QGISScriptGenerator


# ---------------------------------------------------------------------------
# Load layer tests
# ---------------------------------------------------------------------------

class TestGenerateLoadLayerScript:
    def test_returns_string(self):
        script = QGISScriptGenerator.generate_load_layer_script("/data/wells.geojson")
        assert isinstance(script, str)

    def test_imports_qgs_vector_layer(self):
        script = QGISScriptGenerator.generate_load_layer_script("/data/wells.geojson")
        assert "QgsVectorLayer" in script

    def test_imports_qgs_project(self):
        script = QGISScriptGenerator.generate_load_layer_script("/data/wells.geojson")
        assert "QgsProject" in script

    def test_contains_filepath(self):
        script = QGISScriptGenerator.generate_load_layer_script("/data/wells.geojson")
        assert "/data/wells.geojson" in script

    def test_custom_layer_name(self):
        script = QGISScriptGenerator.generate_load_layer_script(
            "/data/wells.shp", layer_name="offshore_wells"
        )
        assert "offshore_wells" in script

    def test_adds_map_layer(self):
        script = QGISScriptGenerator.generate_load_layer_script("/data/wells.kml")
        assert "addMapLayer" in script


# ---------------------------------------------------------------------------
# Buffer analysis tests
# ---------------------------------------------------------------------------

class TestGenerateBufferAnalysisScript:
    def test_returns_string(self):
        script = QGISScriptGenerator.generate_buffer_analysis_script(
            "input.shp", 500.0, "output.shp"
        )
        assert isinstance(script, str)

    def test_imports_processing(self):
        script = QGISScriptGenerator.generate_buffer_analysis_script(
            "input.shp", 500.0, "output.shp"
        )
        assert "import processing" in script

    def test_uses_native_buffer(self):
        script = QGISScriptGenerator.generate_buffer_analysis_script(
            "input.shp", 500.0, "output.shp"
        )
        assert "native:buffer" in script

    def test_contains_distance(self):
        script = QGISScriptGenerator.generate_buffer_analysis_script(
            "input.shp", 750.0, "output.shp"
        )
        assert "750.0" in script


# ---------------------------------------------------------------------------
# Heatmap tests
# ---------------------------------------------------------------------------

class TestGenerateHeatmapScript:
    def test_returns_string(self):
        script = QGISScriptGenerator.generate_heatmap_script(
            "points.shp", 1000.0, "heatmap.tif"
        )
        assert isinstance(script, str)

    def test_imports_processing(self):
        script = QGISScriptGenerator.generate_heatmap_script(
            "points.shp", 1000.0, "heatmap.tif"
        )
        assert "import processing" in script

    def test_uses_heatmap_algorithm(self):
        script = QGISScriptGenerator.generate_heatmap_script(
            "points.shp", 1000.0, "heatmap.tif"
        )
        assert "qgis:heatmapkerneldensityestimation" in script

    def test_contains_radius(self):
        script = QGISScriptGenerator.generate_heatmap_script(
            "points.shp", 2000.0, "heatmap.tif"
        )
        assert "2000.0" in script


# ---------------------------------------------------------------------------
# save_script tests
# ---------------------------------------------------------------------------

class TestSaveScript:
    def test_creates_file(self, tmp_path):
        dest = tmp_path / "output" / "qgis_script.py"
        result = QGISScriptGenerator.save_script("print('qgis')", dest)
        assert result.exists()
        assert result.read_text() == "print('qgis')"

    def test_returns_path(self, tmp_path):
        dest = tmp_path / "qgis_script.py"
        result = QGISScriptGenerator.save_script("# empty", dest)
        assert isinstance(result, Path)
        assert result == dest
