"""Tests for RollerBuilder SRP sub-builder."""
from __future__ import annotations

import math

import pytest


def _make_floating_spec(roller_arrangement=None, rollers=None, with_tugs=True):
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    data = {
        "metadata": {
            "name": "test",
            "description": "Test floating model",
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
            "coatings": {
                "corrosion": {"thickness": 0.004, "density": 0.95},
                "weight": [{"name": "CWC", "thickness": 0.1, "density": 3}],
            },
            "segments": [{"type": "X65+CWC", "length": 1000, "segment_length": 5}],
        },
        "equipment": {},
        "simulation": {"time_step": 0.1, "stages": [8, 16]},
    }
    if with_tugs:
        data["equipment"]["tugs"] = {
            "count": 2,
            "spacing": 500,
            "first_position": [500, -20, 0],
            "properties": {"mass": 30, "volume": 100},
        }
    if rollers:
        data["equipment"]["rollers"] = rollers
    if roller_arrangement:
        data["equipment"]["roller_arrangement"] = roller_arrangement
    return ProjectInputSpec(**data)


def _make_no_roller_spec():
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    data = {
        "metadata": {
            "name": "test",
            "description": "Test",
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


class TestRollerBuilderBuild:
    def test_empty_when_no_rollers(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.roller_builder import RollerBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        result = RollerBuilder(_make_no_roller_spec(), BuilderContext()).build()
        assert result["buoys"] == []
        assert result["names"] == []

    def test_single_station_uses_legacy_name(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.roller_builder import RollerBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        spec = _make_floating_spec(roller_arrangement={
            "type": "v_roller",
            "stations": [{"position": [5, 0, -2], "v_angle": 120, "diameter": 0.5}],
        })
        result = RollerBuilder(spec, BuilderContext()).build()
        assert len(result["buoys"]) == 1
        assert result["names"][0] == "Rollers"

    def test_multi_station_uses_numbered_names(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.roller_builder import RollerBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        spec = _make_floating_spec(roller_arrangement={
            "type": "v_roller",
            "stations": [
                {"position": [5, 0, -2]},
                {"position": [15, 0, -2]},
            ],
        })
        result = RollerBuilder(spec, BuilderContext()).build()
        assert result["names"] == ["Roller_1", "Roller_2"]

    def test_context_receives_roller_buoy_names(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.roller_builder import RollerBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        spec = _make_floating_spec(roller_arrangement={
            "type": "v_roller",
            "stations": [
                {"position": [5, 0, -2]},
                {"position": [15, 0, -2]},
            ],
        })
        ctx = BuilderContext()
        RollerBuilder(spec, ctx).build()
        assert ctx.roller_buoy_names == ["Roller_1", "Roller_2"]

    def test_legacy_rollers_converted_to_arrangement(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.roller_builder import RollerBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        spec = _make_floating_spec(rollers={"position": [5, 0, -2], "supports": 4})
        result = RollerBuilder(spec, BuilderContext()).build()
        assert len(result["buoys"]) == 1
        assert result["names"][0] == "Rollers"


class TestRollerBuilderGetSupportGeometry:
    def test_v_roller_geometry(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.roller_builder import RollerBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        station = RollerStation(position=[5, 0, -2], v_angle=120, diameter=0.5, support_count=2)
        supports = RollerBuilder.get_support_geometry(station, RollerType.V_ROLLER)
        assert len(supports) == 2
        r = 0.25
        expected_lateral = r * math.sin(math.radians(60))
        expected_vertical = -r * math.cos(math.radians(60))
        assert supports[0]["SupportPosition"][1] == pytest.approx(expected_lateral, abs=1e-6)
        assert supports[0]["SupportPosition"][2] == pytest.approx(expected_vertical, abs=1e-6)

    def test_flat_roller_geometry(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.roller_builder import RollerBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        station = RollerStation(position=[5, 0, -2], support_count=4, diameter=0.5)
        supports = RollerBuilder.get_support_geometry(station, RollerType.FLAT)
        assert len(supports) == 4
        for s in supports:
            assert s["SupportPosition"][2] == 0

    def test_cradle_geometry(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.roller_builder import RollerBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        station = RollerStation(position=[5, 0, -2], support_count=3, diameter=0.5)
        supports = RollerBuilder.get_support_geometry(station, RollerType.CRADLE)
        assert len(supports) == 3

    def test_height_offset_applied(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.roller_builder import RollerBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        station = RollerStation(
            position=[5, 0, -2], v_angle=120, diameter=0.5,
            support_count=2, height_offset=-0.5,
        )
        supports = RollerBuilder.get_support_geometry(station, RollerType.V_ROLLER)
        r = 0.25
        expected_vertical = -r * math.cos(math.radians(60)) + (-0.5)
        assert supports[0]["SupportPosition"][2] == pytest.approx(expected_vertical, abs=1e-6)
