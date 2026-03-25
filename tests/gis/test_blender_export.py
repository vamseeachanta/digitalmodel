"""Tests for Blender export integration.

TDD: These tests validate the Blender Python script generator.
All tests run without a Blender runtime — the generated script is
validated using ast.parse to verify syntactic correctness.
"""
from __future__ import annotations

import ast
import json
import tempfile
from pathlib import Path

import pandas as pd
import pytest

from digitalmodel.gis.integrations.blender_export import BlenderExporter
from digitalmodel.gis.layers.well_layer import WellLayer


@pytest.fixture()
def well_layer():
    df = pd.DataFrame(
        {
            "longitude": [-90.5, -89.8, -91.0],
            "latitude": [28.5, 29.1, 27.9],
            "well_name": ["A-1", "B-2", "C-3"],
            "water_depth_m": [150.0, 300.0, 80.0],
            "status": ["producing", "abandoned", "drilling"],
        }
    )
    return WellLayer(data=df, name="test_wells")


class TestBlenderExporterScriptGeneration:
    def test_generate_script_returns_string(self, well_layer):
        script = BlenderExporter.generate_well_script(well_layer)
        assert isinstance(script, str)
        assert len(script) > 0

    def test_generated_script_is_valid_python(self, well_layer):
        script = BlenderExporter.generate_well_script(well_layer)
        # ast.parse raises SyntaxError if the script is not valid Python
        tree = ast.parse(script)
        assert tree is not None

    def test_script_contains_well_names(self, well_layer):
        script = BlenderExporter.generate_well_script(well_layer)
        assert "A-1" in script
        assert "B-2" in script
        assert "C-3" in script

    def test_script_contains_bpy_import(self, well_layer):
        script = BlenderExporter.generate_well_script(well_layer)
        assert "bpy" in script

    def test_script_contains_coordinates(self, well_layer):
        script = BlenderExporter.generate_well_script(well_layer)
        assert "-90.5" in script or "-90" in script

    def test_custom_scale_factor_applied(self, well_layer):
        script_default = BlenderExporter.generate_well_script(well_layer)
        script_scaled = BlenderExporter.generate_well_script(
            well_layer, scale_factor=0.001
        )
        assert "0.001" in script_scaled

    def test_script_contains_origin_anchor(self, well_layer):
        # Origin anchor centers the scene at the centroid of the well layer
        script = BlenderExporter.generate_well_script(well_layer)
        assert "origin" in script.lower() or "anchor" in script.lower() or "center" in script.lower()

    def test_well_metadata_as_custom_properties(self, well_layer):
        script = BlenderExporter.generate_well_script(well_layer)
        # Script should include custom property assignments
        assert "custom_properties" in script or '["water_depth_m"]' in script or "water_depth_m" in script


class TestBlenderExporterFileWrite:
    def test_write_script_creates_file(self, well_layer, tmp_path):
        out_path = tmp_path / "wells.py"
        result = BlenderExporter.write_well_script(well_layer, out_path)
        assert result.exists()

    def test_write_script_returns_path(self, well_layer, tmp_path):
        out_path = tmp_path / "wells.py"
        result = BlenderExporter.write_well_script(well_layer, out_path)
        assert isinstance(result, Path)

    def test_written_script_is_valid_python(self, well_layer, tmp_path):
        out_path = tmp_path / "wells.py"
        BlenderExporter.write_well_script(well_layer, out_path)
        source = out_path.read_text(encoding="utf-8")
        ast.parse(source)  # raises SyntaxError if invalid

    def test_creates_parent_directories(self, well_layer, tmp_path):
        out_path = tmp_path / "subdir" / "nested" / "wells.py"
        BlenderExporter.write_well_script(well_layer, out_path)
        assert out_path.exists()


class TestBlenderExporterCoordinateMapping:
    def test_coordinate_to_blender_units(self):
        # Test the static helper for geo coord -> Blender scene units
        easting, northing = 500000.0, 3200000.0
        origin_e, origin_n = 500000.0, 3200000.0
        scale = 0.001  # 1 meter = 0.001 Blender units

        bx, by = BlenderExporter.geo_to_blender(
            easting, northing, origin_e, origin_n, scale
        )
        assert bx == pytest.approx(0.0)
        assert by == pytest.approx(0.0)

    def test_offset_coordinate(self):
        easting, northing = 500100.0, 3200200.0
        origin_e, origin_n = 500000.0, 3200000.0
        scale = 0.001

        bx, by = BlenderExporter.geo_to_blender(
            easting, northing, origin_e, origin_n, scale
        )
        assert bx == pytest.approx(0.1)
        assert by == pytest.approx(0.2)
