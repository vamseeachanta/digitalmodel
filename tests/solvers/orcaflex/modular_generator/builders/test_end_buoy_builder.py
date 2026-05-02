"""Tests for EndBuoyBuilder SRP sub-builder."""
from __future__ import annotations

import pytest


def _make_spec(length=1000):
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
            "segments": [{"type": "X65", "length": length, "segment_length": 5}],
        },
        "equipment": {
            "tugs": {
                "count": 1,
                "spacing": 500,
                "first_position": [500, -20, 0],
                "properties": {"mass": 30, "volume": 100},
            }
        },
        "simulation": {"time_step": 0.1, "stages": [8]},
    }
    return ProjectInputSpec(**data)


class TestEndBuoyBuilderBuild:
    def test_end_buoy_name_is_6d_buoy1(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.end_buoy_builder import EndBuoyBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        result = EndBuoyBuilder(_make_spec(), BuilderContext()).build()
        assert result["six_d_buoy"]["Name"] == "6D buoy1"
        assert result["six_d_name"] == "6D buoy1"

    def test_mid_pipe_name_is_mid_pipe(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.end_buoy_builder import EndBuoyBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        result = EndBuoyBuilder(_make_spec(), BuilderContext()).build()
        assert result["three_d_buoy"]["Name"] == "Mid-pipe"
        assert result["three_d_name"] == "Mid-pipe"

    def test_mid_pipe_is_only_3d_buoy_producer(self):
        """EndBuoyBuilder is the only component that produces a 3DBuoy."""
        from digitalmodel.solvers.orcaflex.modular_generator.builders.end_buoy_builder import EndBuoyBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        result = EndBuoyBuilder(_make_spec(), BuilderContext()).build()
        # Only one 3D buoy: the Mid-pipe marker
        assert result["three_d_buoy"] is not None
        assert result["three_d_buoy"]["Name"] == "Mid-pipe"

    def test_mid_pipe_position_is_total_length_over_2(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.end_buoy_builder import EndBuoyBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        spec = _make_spec(length=800)
        result = EndBuoyBuilder(spec, BuilderContext()).build()
        # total_length=800; mid_position = 800/2 = 400
        assert result["three_d_buoy"]["InitialPosition"][2] == pytest.approx(400.0)

    def test_end_buoy_connection_is_free(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.end_buoy_builder import EndBuoyBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        result = EndBuoyBuilder(_make_spec(), BuilderContext()).build()
        assert result["six_d_buoy"]["Connection"] == "Free"

    def test_mid_pipe_connected_to_pipeline(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.end_buoy_builder import EndBuoyBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        result = EndBuoyBuilder(_make_spec(), BuilderContext()).build()
        assert result["three_d_buoy"]["Connection"] == "Test Line"
