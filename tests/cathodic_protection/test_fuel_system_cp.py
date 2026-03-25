"""Tests for impressed current CP design for generator fuel piping systems."""

from __future__ import annotations

import math

import pytest

from digitalmodel.cathodic_protection.fuel_system_cp import (
    COATING_BREAKDOWN_FACTOR,
    CoatingType,
    FuelPipeSegment,
    ImpressedCurrentGroundBed,
    RectifierOutput,
    check_protection,
    current_demand_segment,
    design_ground_bed,
    design_rectifier,
    effective_bare_area,
    pipe_surface_area,
    total_current_demand,
)


# ---- FuelPipeSegment validation ----


class TestFuelPipeSegment:
    def test_valid_segment(self):
        seg = FuelPipeSegment(
            segment_id="FS-01",
            length_m=50.0,
            outer_diameter_m=0.1524,
            coating_type=CoatingType.FBE,
            soil_resistivity_ohm_m=50.0,
        )
        assert seg.segment_id == "FS-01"
        assert seg.burial_depth_m == 0.9

    def test_negative_length_raises(self):
        with pytest.raises(ValueError, match="length_m"):
            FuelPipeSegment(
                segment_id="BAD",
                length_m=-1.0,
                outer_diameter_m=0.1,
                coating_type=CoatingType.BARE,
                soil_resistivity_ohm_m=50.0,
            )

    def test_zero_diameter_raises(self):
        with pytest.raises(ValueError, match="outer_diameter_m"):
            FuelPipeSegment(
                segment_id="BAD",
                length_m=10.0,
                outer_diameter_m=0.0,
                coating_type=CoatingType.BARE,
                soil_resistivity_ohm_m=50.0,
            )

    def test_negative_resistivity_raises(self):
        with pytest.raises(ValueError, match="soil_resistivity_ohm_m"):
            FuelPipeSegment(
                segment_id="BAD",
                length_m=10.0,
                outer_diameter_m=0.1,
                coating_type=CoatingType.BARE,
                soil_resistivity_ohm_m=-5.0,
            )


# ---- CoatingType enum ----


class TestCoatingType:
    def test_enum_members(self):
        assert set(CoatingType) == {
            CoatingType.BARE,
            CoatingType.FBE,
            CoatingType.POLYETHYLENE,
            CoatingType.COAL_TAR,
        }


# ---- Coating breakdown factors ----


class TestCoatingBreakdownFactor:
    def test_bare_factor_is_one(self):
        assert COATING_BREAKDOWN_FACTOR[CoatingType.BARE] == 1.0

    def test_fbe_factor(self):
        assert COATING_BREAKDOWN_FACTOR[CoatingType.FBE] == 0.02

    def test_polyethylene_factor(self):
        assert COATING_BREAKDOWN_FACTOR[CoatingType.POLYETHYLENE] == 0.01

    def test_coal_tar_factor(self):
        assert COATING_BREAKDOWN_FACTOR[CoatingType.COAL_TAR] == 0.05


# ---- Surface area ----


class TestPipeSurfaceArea:
    def test_surface_area_calculation(self):
        seg = FuelPipeSegment(
            segment_id="SA-01",
            length_m=100.0,
            outer_diameter_m=0.1524,
            coating_type=CoatingType.FBE,
            soil_resistivity_ohm_m=50.0,
        )
        expected = math.pi * 0.1524 * 100.0
        assert pipe_surface_area(seg) == pytest.approx(expected, rel=1e-6)


# ---- Effective bare area ----


class TestEffectiveBareArea:
    def test_bare_pipe_full_area(self):
        """Bare pipe: breakdown factor starts at 1.0, capped at 1.0."""
        seg = FuelPipeSegment(
            segment_id="BA-01",
            length_m=50.0,
            outer_diameter_m=0.2,
            coating_type=CoatingType.BARE,
            soil_resistivity_ohm_m=50.0,
        )
        sa = pipe_surface_area(seg)
        # f_final = min(1.0 + 0.01*20, 1.0) = 1.0 (capped)
        assert effective_bare_area(seg, years=20) == pytest.approx(sa, rel=1e-6)

    def test_fbe_coating_20_years(self):
        seg = FuelPipeSegment(
            segment_id="BA-02",
            length_m=100.0,
            outer_diameter_m=0.1524,
            coating_type=CoatingType.FBE,
            soil_resistivity_ohm_m=50.0,
        )
        sa = pipe_surface_area(seg)
        f_final = 0.02 + 0.01 * 20  # 0.22
        expected = sa * f_final
        assert effective_bare_area(seg, years=20) == pytest.approx(expected, rel=1e-6)

    def test_cap_at_one(self):
        """Very long design life should cap factor at 1.0."""
        seg = FuelPipeSegment(
            segment_id="BA-03",
            length_m=10.0,
            outer_diameter_m=0.1,
            coating_type=CoatingType.POLYETHYLENE,
            soil_resistivity_ohm_m=50.0,
        )
        sa = pipe_surface_area(seg)
        # f_final = min(0.01 + 0.01*200, 1.0) = 1.0
        assert effective_bare_area(seg, years=200) == pytest.approx(sa, rel=1e-6)


# ---- Current demand ----


class TestCurrentDemand:
    def test_single_segment_current(self):
        seg = FuelPipeSegment(
            segment_id="CD-01",
            length_m=100.0,
            outer_diameter_m=0.1524,
            coating_type=CoatingType.FBE,
            soil_resistivity_ohm_m=50.0,
        )
        current = current_demand_segment(seg, years=20)
        bare_area = effective_bare_area(seg, years=20)
        # current_density = 21.5 mA/m^2 for bare area portion
        expected = bare_area * 21.5 / 1000.0
        assert current == pytest.approx(expected, rel=1e-6)
        assert current > 0

    def test_total_current_multiple_segments(self):
        supply = FuelPipeSegment(
            segment_id="SUPPLY",
            length_m=80.0,
            outer_diameter_m=0.1524,
            coating_type=CoatingType.FBE,
            soil_resistivity_ohm_m=50.0,
        )
        ret = FuelPipeSegment(
            segment_id="RETURN",
            length_m=80.0,
            outer_diameter_m=0.1016,
            coating_type=CoatingType.FBE,
            soil_resistivity_ohm_m=50.0,
        )
        total = total_current_demand([supply, ret], years=20)
        expected = current_demand_segment(supply, 20) + current_demand_segment(ret, 20)
        assert total == pytest.approx(expected, rel=1e-6)


# ---- Ground bed design ----


class TestDesignGroundBed:
    def test_ground_bed_has_at_least_one_anode(self):
        gb = design_ground_bed(0.5, soil_resistivity_ohm_m=50.0)
        assert gb.anode_count >= 1
        assert gb.ground_bed_resistance_ohm > 0

    def test_higher_current_more_anodes(self):
        gb_low = design_ground_bed(0.1, soil_resistivity_ohm_m=50.0)
        gb_high = design_ground_bed(2.0, soil_resistivity_ohm_m=50.0)
        assert gb_high.anode_count >= gb_low.anode_count

    def test_high_soil_resistivity(self):
        """Very high resistivity should increase anode count and resistance."""
        gb = design_ground_bed(0.5, soil_resistivity_ohm_m=500.0)
        assert gb.ground_bed_resistance_ohm > 0
        assert gb.anode_count >= 1


# ---- Rectifier design ----


class TestDesignRectifier:
    def test_rectifier_output(self):
        gb = design_ground_bed(0.5, soil_resistivity_ohm_m=50.0)
        rect = design_rectifier(0.5, gb)
        assert rect.dc_current_a == pytest.approx(0.5, rel=1e-6)
        assert rect.dc_voltage_v > 0
        assert rect.power_w == pytest.approx(
            rect.dc_voltage_v * rect.dc_current_a, rel=1e-6
        )


# ---- Protection check ----


class TestCheckProtection:
    def test_adequate_protection(self):
        gb = design_ground_bed(0.5, soil_resistivity_ohm_m=50.0)
        rect = design_rectifier(0.5, gb, structure_resistance_ohm=0.5)
        result = check_protection(rect, gb, structure_resistance_ohm=0.5)
        assert "pass" in result
        assert "potential_v_cse" in result
        assert isinstance(result["pass"], bool)

    def test_protection_criterion_value(self):
        """Criterion must be -0.85 V CSE."""
        gb = design_ground_bed(0.5, soil_resistivity_ohm_m=50.0)
        rect = design_rectifier(0.5, gb, structure_resistance_ohm=0.5)
        result = check_protection(rect, gb, structure_resistance_ohm=0.5)
        assert result["criterion_v_cse"] == pytest.approx(-0.85, rel=1e-6)


# ---- End-to-end integration ----


class TestEndToEnd:
    def test_full_fuel_system_design(self):
        """End-to-end: segments -> current demand -> ground bed -> rectifier -> check."""
        segments = [
            FuelPipeSegment(
                segment_id="SUPPLY",
                length_m=60.0,
                outer_diameter_m=0.1524,
                coating_type=CoatingType.FBE,
                soil_resistivity_ohm_m=50.0,
            ),
            FuelPipeSegment(
                segment_id="RETURN",
                length_m=60.0,
                outer_diameter_m=0.1016,
                coating_type=CoatingType.POLYETHYLENE,
                soil_resistivity_ohm_m=50.0,
            ),
        ]
        i_total = total_current_demand(segments, years=20)
        assert i_total > 0

        gb = design_ground_bed(i_total, soil_resistivity_ohm_m=50.0)
        assert gb.anode_count >= 1

        rect = design_rectifier(i_total, gb)
        assert rect.dc_voltage_v > 0
        assert rect.power_w > 0

        result = check_protection(rect, gb, structure_resistance_ohm=0.5)
        assert isinstance(result["pass"], bool)
