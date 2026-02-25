"""Tests for RollerType, RollerStation, and RollerArrangement models."""
from __future__ import annotations

import pytest
from pydantic import ValidationError


class TestRollerType:
    """Tests for RollerType enum."""

    def test_v_roller_value(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType
        assert RollerType.V_ROLLER == "v_roller"

    def test_flat_value(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType
        assert RollerType.FLAT == "flat"

    def test_cradle_value(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType
        assert RollerType.CRADLE == "cradle"

    def test_invalid_type_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType
        with pytest.raises(ValueError):
            RollerType("invalid")


class TestRollerStation:
    """Tests for RollerStation model."""

    def test_valid_station(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        station = RollerStation(position=[5.0, 0.0, -2.0])
        assert station.position == [5.0, 0.0, -2.0]
        assert station.support_count == 4
        assert station.v_angle is None
        assert station.diameter == 0.5
        assert station.friction_coefficient == 0.1

    def test_position_must_have_3_components(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        with pytest.raises(ValidationError):
            RollerStation(position=[5.0, 0.0])

    def test_v_angle_none_allowed(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        station = RollerStation(position=[0, 0, 0], v_angle=None)
        assert station.v_angle is None

    def test_v_angle_in_range(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        station = RollerStation(position=[0, 0, 0], v_angle=120)
        assert station.v_angle == 120

    def test_v_angle_below_minimum_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        with pytest.raises(ValidationError):
            RollerStation(position=[0, 0, 0], v_angle=20)

    def test_v_angle_above_maximum_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        with pytest.raises(ValidationError):
            RollerStation(position=[0, 0, 0], v_angle=200)

    def test_friction_coefficient_range(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        station = RollerStation(position=[0, 0, 0], friction_coefficient=0.5)
        assert station.friction_coefficient == 0.5

    def test_friction_coefficient_above_max_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        with pytest.raises(ValidationError):
            RollerStation(position=[0, 0, 0], friction_coefficient=1.5)

    def test_diameter_range(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        station = RollerStation(position=[0, 0, 0], diameter=1.0)
        assert station.diameter == 1.0

    def test_diameter_too_small_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        with pytest.raises(ValidationError):
            RollerStation(position=[0, 0, 0], diameter=0.01)

    def test_diameter_too_large_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        with pytest.raises(ValidationError):
            RollerStation(position=[0, 0, 0], diameter=6.0)

    def test_support_count_range(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        station = RollerStation(position=[0, 0, 0], support_count=8)
        assert station.support_count == 8

    def test_support_count_below_min_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        with pytest.raises(ValidationError):
            RollerStation(position=[0, 0, 0], support_count=0)

    def test_support_count_above_max_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerStation
        with pytest.raises(ValidationError):
            RollerStation(position=[0, 0, 0], support_count=25)


class TestRollerArrangement:
    """Tests for RollerArrangement model."""

    def test_valid_arrangement(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import (
            RollerArrangement, RollerStation,
        )
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        arr = RollerArrangement(
            type=RollerType.V_ROLLER,
            stations=[RollerStation(position=[5, 0, -2])],
        )
        assert len(arr.stations) == 1
        # v_angle should auto-fill to 120 for V_ROLLER
        assert arr.stations[0].v_angle == 120.0

    def test_empty_stations_raises(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerArrangement
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        with pytest.raises(ValidationError):
            RollerArrangement(type=RollerType.V_ROLLER, stations=[])

    def test_v_roller_auto_fills_v_angle(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import (
            RollerArrangement, RollerStation,
        )
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        arr = RollerArrangement(
            type=RollerType.V_ROLLER,
            stations=[
                RollerStation(position=[5, 0, -2]),  # v_angle=None
                RollerStation(position=[15, 0, -2], v_angle=90),  # explicit
            ],
        )
        assert arr.stations[0].v_angle == 120.0  # auto-filled
        assert arr.stations[1].v_angle == 90  # kept as specified

    def test_flat_type_leaves_v_angle_none(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import (
            RollerArrangement, RollerStation,
        )
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        arr = RollerArrangement(
            type=RollerType.FLAT,
            stations=[RollerStation(position=[5, 0, -2])],
        )
        assert arr.stations[0].v_angle is None

    def test_cradle_type_leaves_v_angle_none(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import (
            RollerArrangement, RollerStation,
        )
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        arr = RollerArrangement(
            type=RollerType.CRADLE,
            stations=[RollerStation(position=[5, 0, -2])],
        )
        assert arr.stations[0].v_angle is None


class TestRollerArrangementUniform:
    """Tests for RollerArrangement.uniform() factory."""

    def test_creates_correct_count(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerArrangement
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        arr = RollerArrangement.uniform(
            type=RollerType.V_ROLLER, count=3, first_position=[5, 0, -2], spacing=10
        )
        assert len(arr.stations) == 3

    def test_creates_correct_positions(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerArrangement
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        arr = RollerArrangement.uniform(
            type=RollerType.V_ROLLER, count=3, first_position=[5, 0, -2], spacing=10
        )
        assert arr.stations[0].position == [5, 0, -2]
        assert arr.stations[1].position == [15, 0, -2]
        assert arr.stations[2].position == [25, 0, -2]

    def test_sets_spacing_pattern(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerArrangement
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        arr = RollerArrangement.uniform(
            type=RollerType.V_ROLLER, count=2, first_position=[0, 0, 0], spacing=5
        )
        assert arr.spacing_pattern == "uniform:5"

    def test_forwards_station_kwargs(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerArrangement
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        arr = RollerArrangement.uniform(
            type=RollerType.V_ROLLER,
            count=2,
            first_position=[0, 0, 0],
            spacing=10,
            support_count=6,
            diameter=0.3,
            friction_coefficient=0.2,
        )
        for station in arr.stations:
            assert station.support_count == 6
            assert station.diameter == 0.3
            assert station.friction_coefficient == 0.2

    def test_single_station(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import RollerArrangement
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import RollerType

        arr = RollerArrangement.uniform(
            type=RollerType.FLAT, count=1, first_position=[10, 5, -3], spacing=0
        )
        assert len(arr.stations) == 1
        assert arr.stations[0].position == [10, 5, -3]
