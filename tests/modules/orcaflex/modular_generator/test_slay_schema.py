"""Tests for S-lay pipeline installation schema extensions."""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from digitalmodel.modules.orcaflex.modular_generator.schema import (
    Equipment,
    ProjectInputSpec,
    Stinger,
    StingerRoller,
    StingerSection,
    Tensioner,
    Vessel,
    VesselMooring,
    VesselProperties,
)


SLAY_SPEC_DIR = Path(__file__).parent.parent.parent.parent.parent / (
    "docs/modules/orcaflex/pipeline/installation/s-lay"
)


@pytest.fixture
def vessel_properties_data():
    """Minimal valid VesselProperties data."""
    return {
        "loa": 130,
        "beam": 28,
        "draft": 4.0,
        "gmt": 14.7,
        "cog": [68.6, 0.0, 9.6],
        "gyration_radii": [10.3, 37.8, 37.8],
    }


@pytest.fixture
def vessel_data(vessel_properties_data):
    """Minimal valid Vessel data."""
    return {
        "name": "Eclipse",
        "properties": vessel_properties_data,
    }


@pytest.fixture
def stinger_data():
    """Minimal valid Stinger data."""
    return {
        "radius": 150,
        "sections": [{"length": 40, "bend_radius": 150}],
    }


@pytest.fixture
def tensioner_data():
    """Minimal valid Tensioner data."""
    return {
        "capacity_kn": 490,
        "tension_value": 585.4,
    }


@pytest.fixture
def slay_spec_data():
    """Load the SB-SA S-lay spec YAML."""
    spec_file = SLAY_SPEC_DIR / "SB-SA" / "spec.yml"
    with open(spec_file) as f:
        return yaml.safe_load(f)


@pytest.fixture
def rs8_spec_data():
    """Load the RS8-ID S-lay spec YAML."""
    spec_file = SLAY_SPEC_DIR / "RS8-ID" / "spec.yml"
    with open(spec_file) as f:
        return yaml.safe_load(f)


class TestVesselProperties:
    """Tests for the VesselProperties model."""

    def test_valid_properties(self, vessel_properties_data):
        props = VesselProperties(**vessel_properties_data)
        assert props.loa == 130
        assert props.beam == 28
        assert props.draft == 4.0
        assert props.gmt == 14.7
        assert props.cog == [68.6, 0.0, 9.6]
        assert props.gyration_radii == [10.3, 37.8, 37.8]

    def test_defaults(self, vessel_properties_data):
        props = VesselProperties(**vessel_properties_data)
        assert props.depth == 9.5
        assert props.displacement is None
        assert props.rao_reference is None

    def test_invalid_cog_length(self, vessel_properties_data):
        vessel_properties_data["cog"] = [1.0, 2.0]
        with pytest.raises(Exception):
            VesselProperties(**vessel_properties_data)

    def test_invalid_loa_negative(self, vessel_properties_data):
        vessel_properties_data["loa"] = -10
        with pytest.raises(Exception):
            VesselProperties(**vessel_properties_data)


class TestVesselMooring:
    """Tests for the VesselMooring model."""

    def test_defaults(self):
        mooring = VesselMooring()
        assert mooring.drums == 8
        assert mooring.winch_capacity == 735.75
        assert mooring.pattern is None

    def test_custom_values(self):
        mooring = VesselMooring(drums=12, winch_capacity=500)
        assert mooring.drums == 12
        assert mooring.winch_capacity == 500


class TestVessel:
    """Tests for the Vessel model."""

    def test_valid_vessel(self, vessel_data):
        vessel = Vessel(**vessel_data)
        assert vessel.name == "Eclipse"
        assert vessel.position == [0, 0, 0]
        assert vessel.orientation == [0, 0, 0]
        assert vessel.mooring is None

    def test_vessel_with_mooring(self, vessel_data):
        vessel_data["mooring"] = {"drums": 8, "winch_capacity": 735.75}
        vessel = Vessel(**vessel_data)
        assert vessel.mooring.drums == 8

    def test_invalid_position(self, vessel_data):
        vessel_data["position"] = [1, 2]
        with pytest.raises(Exception):
            Vessel(**vessel_data)


class TestStinger:
    """Tests for the Stinger and related models."""

    def test_stinger_section(self):
        section = StingerSection(length=40, bend_radius=150)
        assert section.length == 40
        assert section.bend_radius == 150

    def test_stinger_roller(self):
        roller = StingerRoller(arc_length=10.0)
        assert roller.arc_length == 10.0
        assert roller.support_type == "Support type2"
        assert roller.z_offset == 0

    def test_stinger(self, stinger_data):
        stinger = Stinger(**stinger_data)
        assert stinger.radius == 150
        assert len(stinger.sections) == 1
        assert stinger.origin_position == [0, 0, 0]
        assert stinger.origin_orientation == [180, 0, 0]

    def test_stinger_empty_rollers(self, stinger_data):
        stinger = Stinger(**stinger_data)
        assert stinger.rollers == []

    def test_invalid_radius(self):
        with pytest.raises(Exception):
            Stinger(radius=-10, sections=[])


class TestTensioner:
    """Tests for the Tensioner model."""

    def test_valid_tensioner(self, tensioner_data):
        t = Tensioner(**tensioner_data)
        assert t.capacity_kn == 490
        assert t.tension_value == 585.4
        assert t.name == "Tensioner"
        assert t.stiffness is None
        assert t.damping is None

    def test_invalid_capacity(self):
        with pytest.raises(Exception):
            Tensioner(capacity_kn=-1, tension_value=100)

    def test_invalid_tension(self):
        with pytest.raises(Exception):
            Tensioner(capacity_kn=100, tension_value=-1)


class TestEquipmentSlayFields:
    """Tests for Equipment class S-lay extensions."""

    def test_equipment_defaults_backward_compat(self):
        """Existing Equipment with no S-lay fields should still work."""
        eq = Equipment()
        assert eq.vessel is None
        assert eq.stinger is None
        assert eq.tensioner is None
        assert eq.tugs is None
        assert eq.rollers is None

    def test_equipment_with_vessel(self, vessel_data):
        eq = Equipment(vessel=vessel_data)
        assert eq.vessel is not None
        assert eq.vessel.name == "Eclipse"

    def test_equipment_with_all_slay(self, vessel_data, stinger_data, tensioner_data):
        eq = Equipment(
            vessel=vessel_data,
            stinger=stinger_data,
            tensioner=tensioner_data,
        )
        assert eq.vessel is not None
        assert eq.stinger is not None
        assert eq.tensioner is not None


class TestCrossValidation:
    """Tests for S-lay cross-validation rules in ProjectInputSpec."""

    def test_stinger_requires_vessel(self, slay_spec_data):
        """Stinger without vessel should fail validation."""
        slay_spec_data["equipment"] = {
            "stinger": {"radius": 150, "sections": [{"length": 40, "bend_radius": 150}]},
        }
        with pytest.raises(Exception, match="Stinger requires a vessel"):
            ProjectInputSpec(**slay_spec_data)

    def test_tensioner_requires_vessel(self, slay_spec_data):
        """Tensioner without vessel should fail validation."""
        slay_spec_data["equipment"] = {
            "tensioner": {"capacity_kn": 490, "tension_value": 585.4},
        }
        with pytest.raises(Exception, match="Tensioner requires a vessel"):
            ProjectInputSpec(**slay_spec_data)


class TestInstallationTypeDetection:
    """Tests for is_s_lay() and is_floating() methods."""

    def test_slay_spec_is_s_lay(self, slay_spec_data):
        spec = ProjectInputSpec(**slay_spec_data)
        assert spec.is_s_lay() is True

    def test_slay_spec_is_not_floating(self, slay_spec_data):
        spec = ProjectInputSpec(**slay_spec_data)
        assert spec.is_floating() is False

    def test_floating_spec_is_floating(self, spec_data):
        """Uses the existing floating spec from conftest."""
        spec = ProjectInputSpec(**spec_data)
        assert spec.is_floating() is True

    def test_floating_spec_is_not_s_lay(self, spec_data):
        """Uses the existing floating spec from conftest."""
        spec = ProjectInputSpec(**spec_data)
        assert spec.is_s_lay() is False


class TestSlaySpecFiles:
    """Tests for the SB-SA and RS8-ID spec files."""

    def test_sb_sa_loads_and_validates(self, slay_spec_data):
        spec = ProjectInputSpec(**slay_spec_data)
        assert spec.metadata.name == "prpp_sb_sa_10in"
        assert spec.pipeline.material == "X60"
        assert spec.equipment.vessel.name == "Eclipse"
        assert spec.equipment.tensioner.tension_value == 585.4

    def test_rs8_id_loads_and_validates(self, rs8_spec_data):
        spec = ProjectInputSpec(**rs8_spec_data)
        assert spec.metadata.name == "prpp_rs8_id_10in"
        assert spec.equipment.tensioner.tension_value == 566.9

    def test_sb_sa_pipeline_dimensions(self, slay_spec_data):
        spec = ProjectInputSpec(**slay_spec_data)
        assert spec.pipeline.dimensions.outer_diameter == 0.2731
        assert spec.pipeline.dimensions.wall_thickness == 0.0143

    def test_sb_sa_vessel_properties(self, slay_spec_data):
        spec = ProjectInputSpec(**slay_spec_data)
        vessel = spec.equipment.vessel
        assert vessel.properties.loa == 130
        assert vessel.properties.beam == 28
        assert vessel.properties.gmt == 14.7
        assert vessel.properties.cog == [68.6, 0.0, 9.6]
        assert vessel.properties.gyration_radii == [10.3, 37.8, 37.8]

    def test_sb_sa_stinger(self, slay_spec_data):
        spec = ProjectInputSpec(**slay_spec_data)
        stinger = spec.equipment.stinger
        assert stinger.radius == 150
        assert len(stinger.sections) == 1
        assert stinger.sections[0].length == 40

    def test_backward_compat_floating_unchanged(self, spec_data):
        """Existing floating spec still validates without changes."""
        spec = ProjectInputSpec(**spec_data)
        assert spec.metadata.name == "30in_pipeline_installation"
        assert spec.equipment.tugs is not None
        assert spec.equipment.vessel is None
