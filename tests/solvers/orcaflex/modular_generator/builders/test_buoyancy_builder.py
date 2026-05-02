"""Tests for BuoyancyBuilder SRP sub-builder."""
from __future__ import annotations

import pytest


def _make_spec_with_bm(mass=100, volume=5, height=3.5):
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    data = {
        "metadata": {
            "name": "test",
            "description": "test",
            "structure": "pipeline",
            "operation": "installation/floating",
            "project": "TEST",
        },
        "environment": {
            "water": {"depth": 10, "density": 1.025},
            "seabed": {"slope": 0, "stiffness": {"normal": 1000, "shear": 100}},
        },
        "pipeline": {
            "name": "Test Line",
            "material": "X65",
            "dimensions": {"outer_diameter": 0.3, "wall_thickness": 0.01},
            "coatings": {"corrosion": {"thickness": 0.004, "density": 0.95}},
            "segments": [{"type": "X65", "length": 1000, "segment_length": 5}],
        },
        "equipment": {
            "tugs": {
                "count": 1,
                "spacing": 500,
                "first_position": [500, -20, 0],
                "properties": {"mass": 30, "volume": 100},
            },
            "buoyancy_modules": {
                "spacing": 4,
                "properties": {"mass": mass, "volume": volume, "height": height},
            },
        },
        "simulation": {"time_step": 0.1, "stages": [8]},
    }
    return ProjectInputSpec(**data)


def _make_spec_no_bm():
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    data = {
        "metadata": {
            "name": "test",
            "description": "test",
            "structure": "pipeline",
            "operation": "installation/floating",
            "project": "TEST",
        },
        "environment": {
            "water": {"depth": 10, "density": 1.025},
            "seabed": {"slope": 0, "stiffness": {"normal": 100, "shear": 10}},
        },
        "pipeline": {
            "name": "L1",
            "material": "X65",
            "dimensions": {"outer_diameter": 0.3, "wall_thickness": 0.01},
            "coatings": {"corrosion": {"thickness": 0.003, "density": 1.0}},
            "segments": [{"type": "X65", "length": 500, "segment_length": 5}],
        },
        "equipment": {
            "tugs": {
                "count": 1,
                "spacing": 200,
                "first_position": [200, 0, 0],
                "properties": {"mass": 10, "volume": 50},
            }
        },
        "simulation": {"time_step": 0.1, "stages": [8]},
    }
    return ProjectInputSpec(**data)


BM_VERTICES = [
    [0.75, 0.75, 1.75],
    [-0.75, 0.75, 1.75],
    [-0.75, -0.75, 1.75],
    [0.75, -0.75, 1.75],
    [0.75, 0.75, -1.75],
    [-0.75, 0.75, -1.75],
    [-0.75, -0.75, -1.75],
    [0.75, -0.75, -1.75],
]


class TestBuoyancyBuilderBuild:
    def test_returns_none_when_no_bm(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoyancy_builder import BuoyancyBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        result = BuoyancyBuilder(_make_spec_no_bm(), BuilderContext()).build()
        assert result["buoy"] is None
        assert result["name"] is None

    def test_bm_connection_is_pipeline_name(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoyancy_builder import BuoyancyBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        spec = _make_spec_with_bm()
        result = BuoyancyBuilder(spec, BuilderContext()).build()
        assert result["buoy"]["Connection"] == "Test Line"

    def test_bm_has_bulk_modulus_infinity(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoyancy_builder import BuoyancyBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        result = BuoyancyBuilder(_make_spec_with_bm(), BuilderContext()).build()
        assert result["buoy"]["BulkModulus"] == "Infinity"

    def test_bm_name_is_bm(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoyancy_builder import BuoyancyBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        result = BuoyancyBuilder(_make_spec_with_bm(), BuilderContext()).build()
        assert result["buoy"]["Name"] == "BM"
        assert result["name"] == "BM"

    def test_bm_has_unique_smaller_wireframe_vertices(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoyancy_builder import BuoyancyBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
        from digitalmodel.solvers.orcaflex.modular_generator.builders._buoy_geometry import DEFAULT_WIREFRAME_VERTICES

        result = BuoyancyBuilder(_make_spec_with_bm(), BuilderContext()).build()
        bm_verts = result["buoy"]["VertexX, VertexY, VertexZ"]
        assert bm_verts != DEFAULT_WIREFRAME_VERTICES
        assert bm_verts == BM_VERTICES

    def test_bm_connection_z_relative_to_end_a(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoyancy_builder import BuoyancyBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        result = BuoyancyBuilder(_make_spec_with_bm(), BuilderContext()).build()
        assert result["buoy"]["ConnectionzRelativeTo"] == "End A"
