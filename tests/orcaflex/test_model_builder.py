"""Tests for digitalmodel.orcaflex.model_builder module."""

import math

import pytest

from digitalmodel.orcaflex.model_builder import (
    Buoy6DConfig,
    LineConfig,
    LineSection,
    LineSectionProperties,
    MATERIAL_LIBRARY,
    VesselConfig,
    build_lazy_wave_model,
    build_mooring_line,
    build_scr_model,
)


class TestVesselConfig:
    """Tests for VesselConfig."""

    def test_default_vessel(self):
        """Default vessel should create valid dict."""
        v = VesselConfig(name="TestFPSO")
        d = v.to_orcaflex_dict()
        assert d["Name"] == "TestFPSO"
        assert d["ObjectType"] == "Vessel"
        assert "Draught" in d

    def test_vessel_position(self):
        """Vessel initial position should be reflected in dict."""
        v = VesselConfig(name="V1", initial_position=(100.0, 200.0, -5.0))
        d = v.to_orcaflex_dict()
        assert d["InitialX"] == 100.0
        assert d["InitialY"] == 200.0
        assert d["InitialZ"] == -5.0

    def test_vessel_heading(self):
        """Vessel heading should be exported."""
        v = VesselConfig(initial_heading=45.0)
        d = v.to_orcaflex_dict()
        assert d["InitialHeading"] == 45.0


class TestLineConfig:
    """Tests for LineConfig."""

    def test_total_length(self):
        """Total length should sum sections."""
        line = LineConfig(sections=[
            LineSection(length=100.0),
            LineSection(length=200.0),
            LineSection(length=300.0),
        ])
        assert line.total_length == pytest.approx(600.0)

    def test_orcaflex_dict_sections(self):
        """Exported dict should contain section data."""
        line = LineConfig(sections=[
            LineSection(length=50.0, line_type_name="TypeA"),
        ])
        d = line.to_orcaflex_dict()
        assert d["ObjectType"] == "Line"
        assert len(d["Sections"]) == 1
        assert d["Sections"][0]["LineType"] == "TypeA"

    def test_end_connections(self):
        """End connections should be in exported dict."""
        line = LineConfig(
            end_a_object="Vessel1",
            end_b_position=(500.0, 0.0, 0.0),
        )
        d = line.to_orcaflex_dict()
        assert d["EndAConnectedObject"] == "Vessel1"
        assert d["EndBX"] == 500.0


class TestBuoy6DConfig:
    """Tests for 6DOF buoy configuration."""

    def test_net_buoyancy_positive(self):
        """Buoy with large volume should have positive net buoyancy."""
        b = Buoy6DConfig(mass=1000.0, volume=5.0)
        assert b.net_buoyancy > 0  # 1025*5 - 1000 = ~4125 kg => >0

    def test_net_buoyancy_negative(self):
        """Heavy clump weight should have negative net buoyancy."""
        b = Buoy6DConfig(mass=50000.0, volume=1.0)
        assert b.net_buoyancy < 0

    def test_orcaflex_dict(self):
        """Buoy dict should contain required fields."""
        b = Buoy6DConfig(name="ClumpWeight")
        d = b.to_orcaflex_dict()
        assert d["ObjectType"] == "6D Buoy"
        assert d["Name"] == "ClumpWeight"


class TestLineSectionProperties:
    """Tests for line section properties."""

    def test_weight_in_water_positive_for_steel(self):
        """Steel pipe should sink in water."""
        props = LineSectionProperties(
            outer_diameter=0.273,
            inner_diameter=0.203,
            mass_per_unit_length=120.0,
        )
        # Moderate OD steel pipe should have positive submerged weight
        assert props.weight_in_water_per_m > 0

    def test_material_library_populated(self):
        """Material library should contain standard entries."""
        assert "R4_84mm_chain" in MATERIAL_LIBRARY
        assert "96mm_wire_rope" in MATERIAL_LIBRARY
        assert "160mm_polyester" in MATERIAL_LIBRARY


class TestBuildModels:
    """Tests for model building factory functions."""

    def test_build_scr_model(self):
        """SCR model should contain vessel and line."""
        model = build_scr_model(water_depth=1500.0)
        assert model["model_type"] == "SCR"
        assert "vessel" in model["objects"]
        assert "line" in model["objects"]

    def test_build_lazy_wave_model(self):
        """Lazy-wave model should have 3 sections."""
        model = build_lazy_wave_model(water_depth=1500.0)
        sections = model["objects"]["line"]["Sections"]
        assert len(sections) == 3  # upper, buoyancy, lower

    def test_build_mooring_line(self):
        """Mooring line should have chain-polyester-chain segments."""
        model = build_mooring_line(water_depth=1000.0, line_length=2500.0, azimuth=45.0)
        assert model["model_type"] == "MooringLine"
        sections = model["objects"]["line"]["Sections"]
        assert len(sections) == 3
