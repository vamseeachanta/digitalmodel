# ABOUTME: Tests for anchor holding capacity — DNV-RP-E302 / API RP 2SK.
# ABOUTME: Covers drag anchors (soft clay, sand, stiff clay) and suction anchors (clay).
"""Tests for anchor holding capacity — DNV-RP-E302 / API RP 2SK."""
import pytest
import math


class TestDragAnchorCapacity:
    def test_holding_capacity_soft_clay(self):
        """Drag anchor in soft clay — capacity depends on anchor weight and soil."""
        from digitalmodel.geotechnical.anchors import drag_anchor_capacity
        result = drag_anchor_capacity(
            anchor_weight_kn=120.0,
            soil_type="soft_clay",
            anchor_type="stevpris",
        )
        assert result.holding_capacity_kn > 0
        assert result.holding_capacity_kn > result.anchor_weight_kn
        assert result.efficiency > 1.0

    def test_holding_capacity_sand(self):
        """Drag anchor in sand — higher efficiency than clay."""
        from digitalmodel.geotechnical.anchors import drag_anchor_capacity
        result_sand = drag_anchor_capacity(
            anchor_weight_kn=120.0,
            soil_type="sand",
            anchor_type="stevpris",
        )
        result_clay = drag_anchor_capacity(
            anchor_weight_kn=120.0,
            soil_type="soft_clay",
            anchor_type="stevpris",
        )
        assert result_sand.efficiency > result_clay.efficiency

    def test_heavier_anchor_more_capacity(self):
        from digitalmodel.geotechnical.anchors import drag_anchor_capacity
        light = drag_anchor_capacity(
            anchor_weight_kn=50.0,
            soil_type="sand",
            anchor_type="stevpris",
        )
        heavy = drag_anchor_capacity(
            anchor_weight_kn=200.0,
            soil_type="sand",
            anchor_type="stevpris",
        )
        assert heavy.holding_capacity_kn > light.holding_capacity_kn

    def test_unknown_soil_raises(self):
        from digitalmodel.geotechnical.anchors import drag_anchor_capacity
        with pytest.raises(ValueError, match="soil"):
            drag_anchor_capacity(
                anchor_weight_kn=100.0,
                soil_type="rock",
                anchor_type="stevpris",
            )

    def test_zero_weight_raises(self):
        from digitalmodel.geotechnical.anchors import drag_anchor_capacity
        with pytest.raises(ValueError):
            drag_anchor_capacity(
                anchor_weight_kn=0.0,
                soil_type="sand",
                anchor_type="stevpris",
            )


class TestSuctionAnchorCapacity:
    def test_suction_anchor_clay(self):
        """Suction anchor in clay — capacity from skin friction + reverse end bearing."""
        from digitalmodel.geotechnical.anchors import suction_anchor_capacity
        result = suction_anchor_capacity(
            diameter_m=5.0,
            length_m=15.0,
            su_kpa=30.0,
            alpha=0.65,
            nc=9.0,
        )
        assert result.total_capacity_kn > 0
        assert result.skin_friction_kn > 0
        assert result.reverse_end_bearing_kn > 0
        assert result.standard == "DNV-RP-E303"

    def test_larger_diameter_more_capacity(self):
        from digitalmodel.geotechnical.anchors import suction_anchor_capacity
        small = suction_anchor_capacity(
            diameter_m=3.0,
            length_m=15.0,
            su_kpa=30.0,
            alpha=0.65,
            nc=9.0,
        )
        large = suction_anchor_capacity(
            diameter_m=6.0,
            length_m=15.0,
            su_kpa=30.0,
            alpha=0.65,
            nc=9.0,
        )
        assert large.total_capacity_kn > small.total_capacity_kn

    def test_zero_length_raises(self):
        from digitalmodel.geotechnical.anchors import suction_anchor_capacity
        with pytest.raises(ValueError):
            suction_anchor_capacity(
                diameter_m=5.0,
                length_m=0.0,
                su_kpa=30.0,
                alpha=0.65,
                nc=9.0,
            )
