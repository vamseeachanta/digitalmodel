# ABOUTME: Tests for anchor holding capacity — DNV-RP-E302 / API RP 2SK.
# ABOUTME: Covers drag anchors (soft clay, sand, stiff clay) and suction anchors (clay).
"""Tests for anchor holding capacity — DNV-RP-E302 / API RP 2SK."""
import pytest
import math


@pytest.fixture
def suction_inputs():
    """Synthetic inputs from test_suction_anchor_components_hand_calculation."""
    return dict(
        diameter_m=4.0,
        length_m=6.0,
        su_kpa=10.0,
        alpha=0.5,
        nc=3.0,
        wall_thickness_m=0.5,
    )


@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf])
@pytest.mark.parametrize(
    "parameter",
    ["anchor_weight_kn", "diameter_m", "length_m", "su_kpa", "alpha", "nc",
     "wall_thickness_m"],
)
def test_anchor_nonfinite_inputs_rejected(parameter, value, suction_inputs):
    """Every physical scalar must be finite, including both capacity factors."""
    from digitalmodel.geotechnical.anchors import (
        drag_anchor_capacity,
        suction_anchor_capacity,
    )

    with pytest.raises(ValueError, match=parameter):
        if parameter == "anchor_weight_kn":
            drag_anchor_capacity(value, "sand", "stevpris")
        else:
            suction_inputs[parameter] = value
            suction_anchor_capacity(**suction_inputs)


@pytest.mark.parametrize("value", [0.0, -1.0])
@pytest.mark.parametrize(
    "parameter", ["anchor_weight_kn", "diameter_m", "length_m", "su_kpa", "alpha", "nc"],
)
def test_anchor_nonpositive_inputs_rejected(parameter, value, suction_inputs):
    """Zero and a synthetic negative unit value bound strictly positive inputs."""
    from digitalmodel.geotechnical.anchors import (
        drag_anchor_capacity,
        suction_anchor_capacity,
    )

    with pytest.raises(ValueError, match=parameter):
        if parameter == "anchor_weight_kn":
            drag_anchor_capacity(value, "sand", "stevpris")
        else:
            suction_inputs[parameter] = value
            suction_anchor_capacity(**suction_inputs)


@pytest.mark.parametrize("wall_thickness_m", [-0.5, 2.0, 2.5])
def test_suction_anchor_geometry_domain(wall_thickness_m, suction_inputs):
    """For synthetic D=4 m, t=-0.5 is negative; t=2 and 2.5 give Di=0 and -1 m."""
    from digitalmodel.geotechnical.anchors import suction_anchor_capacity

    suction_inputs["wall_thickness_m"] = wall_thickness_m
    with pytest.raises(ValueError):
        suction_anchor_capacity(**suction_inputs)


def test_suction_anchor_zero_thickness_limit(suction_inputs):
    """Accept the zero-wall limit, without qualifying a constructible caisson.

    The synthetic hand case with t=0 has Di=D=4 m, outer and inner areas
    each 24*pi m^2 and base area 4*pi m^2. Skin = 0.5*10*48*pi = 240*pi kN;
    end bearing = 3*10*4*pi = 120*pi kN; total = 360*pi kN.
    """
    from digitalmodel.geotechnical.anchors import suction_anchor_capacity

    suction_inputs["wall_thickness_m"] = 0.0
    result = suction_anchor_capacity(**suction_inputs)
    assert result.skin_friction_kn == pytest.approx(240.0 * math.pi)
    assert result.reverse_end_bearing_kn == pytest.approx(120.0 * math.pi)
    assert result.total_capacity_kn == pytest.approx(360.0 * math.pi)
    assert result.standard == "DNV-RP-E303"


def test_suction_anchor_components_hand_calculation(suction_inputs):
    """Independent hand calculation using assumed synthetic inputs only.

    Choose D=4 m, L=6 m, Su=10 kPa, alpha=0.5, Nc=3 and t=0.5 m.
    These are arithmetic test inputs, not qualified empirical design factors.
    Di = 4 - 2*0.5 = 3 m.
    Outer skin area = pi*4*6 = 24*pi m^2.
    Inner skin area = pi*3*6 = 18*pi m^2.
    Base area = pi*4^2/4 = 4*pi m^2.
    Outer skin = 0.5*10*24*pi = 120*pi kN.
    Inner skin = 0.5*10*18*pi = 90*pi kN.
    Skin friction = 210*pi kN; end bearing = 3*10*4*pi = 120*pi kN.
    Total = (210 + 120)*pi = 330*pi kN.
    """
    from digitalmodel.geotechnical.anchors import suction_anchor_capacity

    result = suction_anchor_capacity(**suction_inputs)
    assert result.skin_friction_kn == pytest.approx(210.0 * math.pi)
    assert result.reverse_end_bearing_kn == pytest.approx(120.0 * math.pi)
    assert result.total_capacity_kn == pytest.approx(330.0 * math.pi)
    assert result.total_capacity_kn == pytest.approx(
        result.skin_friction_kn + result.reverse_end_bearing_kn
    )
    assert result.standard == "DNV-RP-E303"


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
