"""Tests for digitalmodel.orcaflex.riser_config module."""

import math

import pytest

from digitalmodel.orcaflex.riser_config import (
    LazyWaveDesignInput,
    RiserPipeProperties,
    SCRDesignInput,
    TTRDesignInput,
    calculate_weight_in_water,
    estimate_scr_hang_off_angle,
)


class TestRiserPipeProperties:
    """Tests for riser pipe cross-section properties."""

    def test_inner_diameter(self):
        """Inner diameter should equal OD - 2*WT."""
        pipe = RiserPipeProperties(outer_diameter=0.3, wall_thickness=0.025)
        assert pipe.inner_diameter == pytest.approx(0.25, abs=1e-6)

    def test_steel_mass_positive(self):
        """Steel mass per unit length should be positive."""
        pipe = RiserPipeProperties()
        assert pipe.steel_mass_per_m > 0

    def test_submerged_weight_positive_for_steel(self):
        """Steel pipe with thin coating should sink."""
        pipe = RiserPipeProperties(
            outer_diameter=0.273,
            wall_thickness=0.025,
            coating_thickness=0.005,
            contents_density=0.0,
        )
        assert pipe.submerged_weight_per_m > 0

    def test_bending_stiffness_positive(self):
        """EI should be positive."""
        pipe = RiserPipeProperties()
        assert pipe.bending_stiffness > 0

    def test_total_od_includes_coating(self):
        """Total OD should include coating."""
        pipe = RiserPipeProperties(outer_diameter=0.3, coating_thickness=0.05)
        assert pipe.total_od == pytest.approx(0.4, abs=1e-6)


class TestSCRDesign:
    """Tests for SCR design calculations."""

    def test_scr_geometry_calculation(self):
        """SCR geometry should produce valid results."""
        scr = SCRDesignInput(water_depth=1500.0, hang_off_angle_from_vertical=12.0)
        geom = scr.calculate_geometry()
        assert geom["horizontal_tension_kN"] > 0
        assert geom["top_tension_kN"] > geom["horizontal_tension_kN"]
        assert geom["suspended_length_m"] > 0
        assert geom["horizontal_offset_m"] > 0

    def test_scr_length_exceeds_depth(self):
        """Suspended length should exceed water depth."""
        scr = SCRDesignInput(water_depth=1000.0, hang_off_angle_from_vertical=15.0)
        geom = scr.calculate_geometry()
        assert geom["suspended_length_m"] > 1000.0

    def test_scr_tdp_tension_less_than_top(self):
        """TDP tension should be less than top tension."""
        scr = SCRDesignInput(water_depth=1500.0)
        geom = scr.calculate_geometry()
        assert geom["tdp_tension_kN"] < geom["top_tension_kN"]


class TestLazyWaveDesign:
    """Tests for Lazy-Wave riser design."""

    def test_buoyancy_section_sizing(self):
        """Buoyancy section length should be positive."""
        lw = LazyWaveDesignInput(
            water_depth=1500.0,
            hog_bend_depth=800.0,
            sag_bend_clearance=50.0,
        )
        result = lw.size_buoyancy_section()
        assert result["buoyancy_section_length_m"] > 0
        assert result["total_riser_length_m"] > 0

    def test_net_uplift_positive(self):
        """Buoyancy section should have net uplift."""
        lw = LazyWaveDesignInput(
            buoyancy_od=0.65,
            buoyancy_density=450.0,
        )
        assert lw.net_uplift_per_m > 0

    def test_upper_lower_lengths_positive(self):
        """Upper and lower catenary lengths should be positive."""
        lw = LazyWaveDesignInput(water_depth=1500.0)
        result = lw.size_buoyancy_section()
        assert result["upper_catenary_length_m"] > 0
        assert result["lower_catenary_length_m"] > 0


class TestTTRDesign:
    """Tests for TTR design calculations."""

    def test_stroke_calculation(self):
        """TTR stroke should have valid components."""
        ttr = TTRDesignInput(water_depth=1500.0, vessel_heave_amplitude=3.0)
        stroke = ttr.calculate_stroke()
        assert stroke["stroke_heave_m"] == 3.0
        assert stroke["design_stroke_m"] > stroke["total_stroke_m"]

    def test_top_tension(self):
        """Top tension should include overpull."""
        ttr = TTRDesignInput(overpull_pct=50.0)
        tension = ttr.calculate_top_tension()
        assert tension["overpull_factor"] == pytest.approx(1.5)
        assert tension["required_top_tension_kN"] > tension["riser_submerged_weight_kN"]

    def test_thermal_stroke_component(self):
        """Deeper water should give larger thermal stroke."""
        ttr_deep = TTRDesignInput(water_depth=3000.0)
        ttr_shallow = TTRDesignInput(water_depth=500.0)
        deep_stroke = ttr_deep.calculate_stroke()["stroke_thermal_m"]
        shallow_stroke = ttr_shallow.calculate_stroke()["stroke_thermal_m"]
        assert deep_stroke > shallow_stroke


class TestUtilityFunctions:
    """Tests for utility functions."""

    def test_weight_in_water_steel_pipe(self):
        """Steel pipe should have positive submerged weight."""
        w = calculate_weight_in_water(
            outer_diameter=0.3,
            wall_thickness=0.025,
            steel_density=7850.0,
        )
        assert w > 0

    def test_weight_in_water_buoyant(self):
        """Pipe with large low-density coating should be buoyant."""
        w = calculate_weight_in_water(
            outer_diameter=0.2,
            wall_thickness=0.01,
            coating_thickness=0.3,
            coating_density=400.0,  # very light coating
        )
        assert w < 0  # buoyant

    def test_hang_off_angle_zero_offset(self):
        """Zero offset should give zero hang-off angle."""
        angle = estimate_scr_hang_off_angle(water_depth=1000.0, horizontal_offset=0.0)
        assert angle == 0.0

    def test_hang_off_angle_positive(self):
        """Positive offset should give positive angle."""
        angle = estimate_scr_hang_off_angle(water_depth=1000.0, horizontal_offset=500.0)
        assert angle > 0
        assert angle < 90
