"""Tests for BuoysBuilder roller arrangement support."""
from __future__ import annotations

import math

import pytest


def _make_floating_spec(roller_arrangement=None, rollers=None):
    """Create a minimal floating spec for testing."""
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    data = {
        "metadata": {"name": "test", "description": "Test floating model", "structure": "pipeline", "operation": "installation/floating", "project": "TEST"},
        "environment": {
            "water": {"depth": 10, "density": 1.025},
            "seabed": {"slope": 0, "stiffness": {"normal": 1000, "shear": 100}},
        },
        "pipeline": {
            "name": "Test Line",
            "material": "X65",
            "dimensions": {"outer_diameter": 0.3, "wall_thickness": 0.01},
            "coatings": {"corrosion": {"thickness": 0.004, "density": 0.95}, "weight": [{"name": "CWC", "thickness": 0.1, "density": 3}]},
            "segments": [{"type": "X65+CWC", "length": 1000, "segment_length": 5}],
        },
        "equipment": {
            "tugs": {"count": 2, "spacing": 500, "first_position": [500, -20, 0], "properties": {"mass": 30, "volume": 100}},
            "buoyancy_modules": {"spacing": 4, "properties": {"volume": 5}},
        },
        "simulation": {"time_step": 0.1, "stages": [8, 16]},
    }
    if rollers:
        data["equipment"]["rollers"] = rollers
    if roller_arrangement:
        data["equipment"]["roller_arrangement"] = roller_arrangement
    return ProjectInputSpec(**data)


def _make_slay_spec(roller_arrangement=None):
    """Create a minimal S-lay spec for testing."""
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    data = {
        "metadata": {"name": "test_slay", "description": "Test S-lay model", "structure": "pipeline", "operation": "installation/s-lay", "project": "TEST"},
        "environment": {
            "water": {"depth": 10, "density": 1.025},
            "seabed": {"slope": 0, "stiffness": {"normal": 1000, "shear": 100}},
        },
        "pipeline": {
            "name": "Test Line",
            "material": "X65",
            "dimensions": {"outer_diameter": 0.3, "wall_thickness": 0.01},
            "coatings": {"corrosion": {"thickness": 0.004, "density": 0.95}, "weight": [{"name": "CWC", "thickness": 0.1, "density": 3}]},
            "segments": [{"type": "X65+CWC", "length": 1000, "segment_length": 5}],
        },
        "equipment": {
            "vessel": {"name": "Eclipse", "properties": {"loa": 130, "beam": 28, "draft": 4, "gmt": 2.5, "cog": [0, 0, 3], "gyration_radii": [10, 30, 30]}},
            "stinger": {"radius": 150},
            "tensioner": {"capacity_kn": 500, "tension_value": 400},
        },
        "simulation": {"time_step": 0.1, "stages": [8, 16]},
    }
    if roller_arrangement:
        data["equipment"]["roller_arrangement"] = roller_arrangement
    return ProjectInputSpec(**data)


class TestBuoysBuilderShouldGenerate:
    def test_floating_generates(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoys_builder import BuoysBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        spec = _make_floating_spec(rollers={"position": [5, 0, -2], "supports": 4})
        builder = BuoysBuilder(spec, BuilderContext())
        assert builder.should_generate() is True

    def test_slay_without_rollers_skips(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoys_builder import BuoysBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        spec = _make_slay_spec()
        builder = BuoysBuilder(spec, BuilderContext())
        assert builder.should_generate() is False

    def test_slay_with_roller_arrangement_generates(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoys_builder import BuoysBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        spec = _make_slay_spec(roller_arrangement={
            "type": "v_roller",
            "stations": [{"position": [5, 0, -2]}],
        })
        builder = BuoysBuilder(spec, BuilderContext())
        assert builder.should_generate() is True


class TestBuoysBuilderRollerArrangement:
    def test_single_station_generates_one_buoy(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoys_builder import BuoysBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        spec = _make_floating_spec(roller_arrangement={
            "type": "v_roller",
            "stations": [{"position": [5, 0, -2], "v_angle": 120, "diameter": 0.5}],
        })
        ctx = BuilderContext()
        builder = BuoysBuilder(spec, ctx)
        result = builder.build()

        roller_buoys = [b for b in result["6DBuoys"] if b["Name"].startswith("Roller")]
        assert len(roller_buoys) == 1
        assert roller_buoys[0]["Name"] == "Rollers"  # Single station uses legacy name

    def test_multi_station_generates_numbered_buoys(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoys_builder import BuoysBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        spec = _make_floating_spec(roller_arrangement={
            "type": "v_roller",
            "stations": [
                {"position": [5, 0, -2]},
                {"position": [15, 0, -2]},
                {"position": [25, 0, -2]},
            ],
        })
        ctx = BuilderContext()
        builder = BuoysBuilder(spec, ctx)
        result = builder.build()

        roller_buoys = [b for b in result["6DBuoys"] if "Roller" in b["Name"]]
        assert len(roller_buoys) == 3
        assert roller_buoys[0]["Name"] == "Roller_1"
        assert roller_buoys[1]["Name"] == "Roller_2"
        assert roller_buoys[2]["Name"] == "Roller_3"

    def test_roller_buoy_names_in_context(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoys_builder import BuoysBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        spec = _make_floating_spec(roller_arrangement={
            "type": "v_roller",
            "stations": [{"position": [5, 0, -2]}, {"position": [15, 0, -2]}],
        })
        ctx = BuilderContext()
        builder = BuoysBuilder(spec, ctx)
        builder.build()
        entities = builder.get_generated_entities()
        assert "roller_buoy_names" in entities
        assert entities["roller_buoy_names"] == ["Roller_1", "Roller_2"]

    def test_legacy_rollers_still_works(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoys_builder import BuoysBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext

        spec = _make_floating_spec(rollers={"position": [5, 0, -2], "supports": 4})
        ctx = BuilderContext()
        builder = BuoysBuilder(spec, ctx)
        result = builder.build()

        roller_buoys = [b for b in result["6DBuoys"] if b["Name"].startswith("Roller")]
        assert len(roller_buoys) == 1


class TestSupportGeometry:
    def test_v_roller_geometry(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoys_builder import BuoysBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        station = RollerStation(position=[5, 0, -2], v_angle=120, diameter=0.5, support_count=2)
        supports = BuoysBuilder.get_support_geometry(station, RollerType.V_ROLLER)

        assert len(supports) == 2

        r = 0.25
        half_angle = 60
        expected_lateral = r * math.sin(math.radians(half_angle))
        expected_vertical = -r * math.cos(math.radians(half_angle))

        assert supports[0]["SupportPosition"][1] == pytest.approx(expected_lateral, abs=1e-6)
        assert supports[1]["SupportPosition"][1] == pytest.approx(-expected_lateral, abs=1e-6)
        assert supports[0]["SupportPosition"][2] == pytest.approx(expected_vertical, abs=1e-6)

    def test_flat_roller_geometry(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoys_builder import BuoysBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        station = RollerStation(position=[5, 0, -2], support_count=4, diameter=0.5)
        supports = BuoysBuilder.get_support_geometry(station, RollerType.FLAT)

        assert len(supports) == 4
        # All at same z (height_offset = 0)
        for s in supports:
            assert s["SupportPosition"][2] == 0

    def test_cradle_geometry(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoys_builder import BuoysBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        station = RollerStation(position=[5, 0, -2], support_count=3, diameter=0.5)
        supports = BuoysBuilder.get_support_geometry(station, RollerType.CRADLE)

        assert len(supports) == 3

    def test_height_offset_applied(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoys_builder import BuoysBuilder
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        station = RollerStation(
            position=[5, 0, -2], v_angle=120, diameter=0.5,
            support_count=2, height_offset=-0.5,
        )
        supports = BuoysBuilder.get_support_geometry(station, RollerType.V_ROLLER)

        r = 0.25
        expected_vertical = -r * math.cos(math.radians(60)) + (-0.5)
        assert supports[0]["SupportPosition"][2] == pytest.approx(expected_vertical, abs=1e-6)
