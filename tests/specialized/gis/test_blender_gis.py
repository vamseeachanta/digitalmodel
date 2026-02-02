"""Tests for BlenderScriptGenerator."""
from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.gis.scripts.blender.blender_gis import BlenderScriptGenerator


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture()
def sample_points_xyz() -> list[dict]:
    return [
        {"name": "Well-A", "x": 1.0, "y": 2.0, "z": -50.0},
        {"name": "Well-B", "x": 3.0, "y": 4.0, "z": -80.0},
    ]


@pytest.fixture()
def sample_points_geo() -> list[dict]:
    return [
        {"name": "Platform-1", "longitude": 55.0, "latitude": 25.0, "elevation": -30.0},
    ]


@pytest.fixture()
def sample_routes() -> list[dict]:
    return [
        {
            "name": "Pipeline-A",
            "coordinates": [(0, 0, -10), (10, 5, -12), (20, 10, -15)],
        },
        {
            "name": "Pipeline-B",
            "coordinates": [(1, 1, -5), (11, 6, -7)],
        },
    ]


@pytest.fixture()
def sample_grid() -> dict:
    return {
        "vertices": [(0, 0, -10), (1, 0, -12), (0, 1, -11), (1, 1, -13)],
        "faces": [(0, 1, 3, 2)],
    }


# ---------------------------------------------------------------------------
# Point cloud tests
# ---------------------------------------------------------------------------

class TestGeneratePointCloudScript:
    def test_returns_string(self, sample_points_xyz):
        script = BlenderScriptGenerator.generate_point_cloud_script(sample_points_xyz)
        assert isinstance(script, str)

    def test_imports_bpy(self, sample_points_xyz):
        script = BlenderScriptGenerator.generate_point_cloud_script(sample_points_xyz)
        assert "import bpy" in script

    def test_creates_collection(self, sample_points_xyz):
        script = BlenderScriptGenerator.generate_point_cloud_script(sample_points_xyz)
        assert "bpy.data.collections.new" in script

    def test_creates_icospheres(self, sample_points_xyz):
        script = BlenderScriptGenerator.generate_point_cloud_script(sample_points_xyz)
        assert "bpy.ops.mesh.primitive_ico_sphere_add" in script

    def test_contains_point_names(self, sample_points_xyz):
        script = BlenderScriptGenerator.generate_point_cloud_script(sample_points_xyz)
        assert "Well-A" in script
        assert "Well-B" in script

    def test_custom_scale(self, sample_points_xyz):
        script = BlenderScriptGenerator.generate_point_cloud_script(
            sample_points_xyz, scale=2.5
        )
        assert "2.5" in script

    def test_geo_coordinate_keys(self, sample_points_geo):
        script = BlenderScriptGenerator.generate_point_cloud_script(sample_points_geo)
        assert "55.0" in script
        assert "25.0" in script
        assert "-30.0" in script


# ---------------------------------------------------------------------------
# Pipeline tests
# ---------------------------------------------------------------------------

class TestGeneratePipelineScript:
    def test_returns_string(self, sample_routes):
        script = BlenderScriptGenerator.generate_pipeline_script(sample_routes)
        assert isinstance(script, str)

    def test_imports_bpy(self, sample_routes):
        script = BlenderScriptGenerator.generate_pipeline_script(sample_routes)
        assert "import bpy" in script

    def test_creates_curve(self, sample_routes):
        script = BlenderScriptGenerator.generate_pipeline_script(sample_routes)
        assert "bpy.data.curves.new" in script

    def test_creates_objects(self, sample_routes):
        script = BlenderScriptGenerator.generate_pipeline_script(sample_routes)
        assert "bpy.data.objects.new" in script

    def test_contains_route_names(self, sample_routes):
        script = BlenderScriptGenerator.generate_pipeline_script(sample_routes)
        assert "Pipeline-A" in script
        assert "Pipeline-B" in script


# ---------------------------------------------------------------------------
# Bathymetry tests
# ---------------------------------------------------------------------------

class TestGenerateBathymetryScript:
    def test_returns_string(self, sample_grid):
        script = BlenderScriptGenerator.generate_bathymetry_script(sample_grid)
        assert isinstance(script, str)

    def test_imports_bpy(self, sample_grid):
        script = BlenderScriptGenerator.generate_bathymetry_script(sample_grid)
        assert "import bpy" in script

    def test_creates_mesh(self, sample_grid):
        script = BlenderScriptGenerator.generate_bathymetry_script(sample_grid)
        assert "bpy.data.meshes.new" in script

    def test_uses_from_pydata(self, sample_grid):
        script = BlenderScriptGenerator.generate_bathymetry_script(sample_grid)
        assert "from_pydata" in script


# ---------------------------------------------------------------------------
# save_script tests
# ---------------------------------------------------------------------------

class TestSaveScript:
    def test_creates_file(self, tmp_path):
        dest = tmp_path / "output" / "test_script.py"
        result = BlenderScriptGenerator.save_script("print('hello')", dest)
        assert result.exists()
        assert result.read_text() == "print('hello')"

    def test_returns_path(self, tmp_path):
        dest = tmp_path / "test_script.py"
        result = BlenderScriptGenerator.save_script("# empty", dest)
        assert isinstance(result, Path)
        assert result == dest
