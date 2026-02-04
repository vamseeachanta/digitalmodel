"""Tests for Equipment backward compatibility with roller_arrangement."""
from __future__ import annotations

from pathlib import Path

import pytest
import yaml


# Spec file paths
FLOATING_SPEC = (
    Path(__file__).parent.parent.parent.parent.parent.parent
    / "docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/spec.yml"
)
SLAY_SPEC = (
    Path(__file__).parent.parent.parent.parent.parent.parent
    / "docs/modules/orcaflex/pipeline/installation/s-lay/SB-SA/spec.yml"
)


class TestEquipmentRollerCompat:
    """Tests for backward compatibility of roller fields."""

    def test_legacy_rollers_field_still_parses(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import (
            Equipment,
            Rollers,
        )

        equip = Equipment(rollers=Rollers(position=[5, 0, -2], supports=4))
        assert equip.rollers is not None
        assert equip.rollers.position == [5, 0, -2]

    def test_new_roller_arrangement_parses(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import (
            RollerType,
        )
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import (
            Equipment,
            RollerArrangement,
            RollerStation,
        )

        equip = Equipment(
            roller_arrangement=RollerArrangement(
                type=RollerType.V_ROLLER,
                stations=[RollerStation(position=[5, 0, -2])],
            )
        )
        assert equip.roller_arrangement is not None
        assert len(equip.roller_arrangement.stations) == 1

    def test_legacy_auto_converts_to_arrangement(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import (
            Equipment,
            Rollers,
        )

        equip = Equipment(rollers=Rollers(position=[5, 0, -2], supports=6))
        assert equip.roller_arrangement is not None
        assert len(equip.roller_arrangement.stations) == 1
        assert equip.roller_arrangement.stations[0].position == [5, 0, -2]
        assert equip.roller_arrangement.stations[0].support_count == 6

    def test_roller_arrangement_takes_precedence(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema._enums import (
            RollerType,
        )
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import (
            Equipment,
            RollerArrangement,
            Rollers,
            RollerStation,
        )

        equip = Equipment(
            rollers=Rollers(position=[5, 0, -2]),
            roller_arrangement=RollerArrangement(
                type=RollerType.FLAT,
                stations=[
                    RollerStation(position=[10, 0, -3]),
                    RollerStation(position=[20, 0, -3]),
                ],
            ),
        )
        eff = equip.get_effective_rollers()
        assert eff is not None
        assert len(eff.stations) == 2
        assert eff.stations[0].position == [10, 0, -3]

    def test_neither_set_returns_none(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import (
            Equipment,
        )

        equip = Equipment()
        assert equip.get_effective_rollers() is None

    def test_floating_spec_loads_without_error(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema import (
            ProjectInputSpec,
        )

        with open(FLOATING_SPEC) as f:
            data = yaml.safe_load(f)
        spec = ProjectInputSpec(**data)
        assert spec.equipment.rollers is not None
        # Auto-converted
        assert spec.equipment.get_effective_rollers() is not None

    def test_slay_spec_loads_without_error(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema import (
            ProjectInputSpec,
        )

        if not SLAY_SPEC.exists():
            pytest.skip("S-lay spec not available")
        with open(SLAY_SPEC) as f:
            data = yaml.safe_load(f)
        spec = ProjectInputSpec(**data)
        assert spec.equipment.vessel is not None


class TestEquipmentExports:
    """Verify new models are exported from schema __init__."""

    def test_roller_type_exported(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema import RollerType

        assert RollerType.V_ROLLER == "v_roller"

    def test_roller_station_exported(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema import (
            RollerStation,
        )

        station = RollerStation(position=[0, 0, 0])
        assert station is not None

    def test_roller_arrangement_exported(self):
        from digitalmodel.solvers.orcaflex.modular_generator.schema import (
            RollerArrangement,
            RollerStation,
            RollerType,
        )

        arr = RollerArrangement(
            type=RollerType.V_ROLLER,
            stations=[RollerStation(position=[0, 0, 0])],
        )
        assert arr is not None
