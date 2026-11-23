# ABOUTME: Unit tests for WellDesign — casing programs, cost estimation, production potential,
# ABOUTME: risk assessment, and decision matrix for slim/standard/big hole comparisons.

"""
Tests for WellDesign in digitalmodel.well.drilling.well_bore_design.

Casing data from issue #1958:
  - Big Hole:  30"@30m -> 20"@400m -> 13-5/8"@1000m -> 10-3/4"@1800m
  - Standard:  30"@30m -> 20"@150m -> 13-3/4"@400m -> 9-5/8"@1000m -> 7"@1800m
  - Slim:      9-5/8"@12m -> 7"@100m -> 5-1/2"@600m -> 3-1/2"@1200m -> 2.9"@2000m
"""

import pytest

from digitalmodel.well.drilling.well_bore_design import (
    CasingString,
    CostBreakdown,
    WellDesign,
)


# ---------------------------------------------------------------------------
# Helpers / fixtures
# ---------------------------------------------------------------------------

def _big_hole() -> WellDesign:
    """Big hole design — large-bore conductor and surface casing."""
    return WellDesign(hole_type="big_hole", water_depth_m=500.0)


def _standard() -> WellDesign:
    """Standard hole design — conventional offshore well."""
    return WellDesign(hole_type="standard_hole", water_depth_m=500.0)


def _slim() -> WellDesign:
    """Slim hole design — reduced diameter exploration well."""
    return WellDesign(hole_type="slim_hole", water_depth_m=500.0)


# ---------------------------------------------------------------------------
# Construction and validation
# ---------------------------------------------------------------------------

class TestWellDesignConstruction:
    def test_valid_hole_types(self):
        for ht in ("big_hole", "standard_hole", "slim_hole"):
            wd = WellDesign(hole_type=ht, water_depth_m=100.0)
            assert wd.hole_type == ht

    def test_invalid_hole_type_raises(self):
        with pytest.raises(ValueError, match="hole_type"):
            WellDesign(hole_type="micro_hole", water_depth_m=100.0)

    def test_negative_water_depth_raises(self):
        with pytest.raises(ValueError, match="water_depth"):
            WellDesign(hole_type="slim_hole", water_depth_m=-10.0)

    def test_zero_water_depth_raises(self):
        with pytest.raises(ValueError, match="water_depth"):
            WellDesign(hole_type="slim_hole", water_depth_m=0.0)

    def test_hole_types_constant(self):
        assert WellDesign.HOLE_TYPES == {"big_hole", "standard_hole", "slim_hole"}


# ---------------------------------------------------------------------------
# Casing program
# ---------------------------------------------------------------------------

class TestCasingProgram:
    def test_big_hole_casing_count(self):
        program = _big_hole().casing_program()
        assert len(program) == 4

    def test_standard_casing_count(self):
        program = _standard().casing_program()
        assert len(program) == 5

    def test_slim_hole_casing_count(self):
        program = _slim().casing_program()
        assert len(program) == 5

    def test_casing_string_is_dataclass(self):
        program = _standard().casing_program()
        cs = program[0]
        assert isinstance(cs, CasingString)
        assert hasattr(cs, "od_inch")
        assert hasattr(cs, "id_inch")
        assert hasattr(cs, "grade")
        assert hasattr(cs, "set_depth_m")
        assert hasattr(cs, "name")

    def test_big_hole_first_casing_is_conductor(self):
        program = _big_hole().casing_program()
        assert program[0].od_inch == pytest.approx(30.0)
        assert program[0].set_depth_m == pytest.approx(30.0)

    def test_slim_hole_largest_casing(self):
        """Slim hole starts with 9-5/8 not 30 inches."""
        program = _slim().casing_program()
        assert program[0].od_inch == pytest.approx(9.625)

    def test_casing_od_decreasing(self):
        """Casing OD must decrease with depth for every hole type."""
        for ht in WellDesign.HOLE_TYPES:
            wd = WellDesign(hole_type=ht, water_depth_m=300.0)
            program = wd.casing_program()
            ods = [cs.od_inch for cs in program]
            for i in range(len(ods) - 1):
                assert ods[i] > ods[i + 1], f"{ht}: OD must decrease with depth"

    def test_casing_depth_increasing(self):
        """Casing set depth must increase for every hole type."""
        for ht in WellDesign.HOLE_TYPES:
            wd = WellDesign(hole_type=ht, water_depth_m=300.0)
            program = wd.casing_program()
            depths = [cs.set_depth_m for cs in program]
            for i in range(len(depths) - 1):
                assert depths[i] < depths[i + 1], f"{ht}: depth must increase"

    def test_standard_last_casing(self):
        program = _standard().casing_program()
        last = program[-1]
        assert last.od_inch == pytest.approx(7.0)
        assert last.set_depth_m == pytest.approx(1800.0)


# ---------------------------------------------------------------------------
# Cost estimation
# ---------------------------------------------------------------------------

class TestCostEstimation:
    def test_cost_breakdown_fields(self):
        cost = _standard().estimate_cost()
        assert isinstance(cost, CostBreakdown)
        assert hasattr(cost, "drilling_usd")
        assert hasattr(cost, "casing_usd")
        assert hasattr(cost, "completion_usd")
        assert hasattr(cost, "total_usd")

    def test_total_is_sum_of_components(self):
        cost = _standard().estimate_cost()
        expected = cost.drilling_usd + cost.casing_usd + cost.completion_usd
        assert cost.total_usd == pytest.approx(expected)

    def test_slim_cheaper_than_standard(self):
        slim_cost = _slim().estimate_cost().total_usd
        std_cost = _standard().estimate_cost().total_usd
        assert slim_cost < std_cost

    def test_standard_cheaper_than_big_hole(self):
        std_cost = _standard().estimate_cost().total_usd
        big_cost = _big_hole().estimate_cost().total_usd
        assert std_cost < big_cost

    def test_slim_savings_30_to_50_percent(self):
        """Slim hole should save 30-50% vs standard (per issue spec)."""
        slim_cost = _slim().estimate_cost().total_usd
        std_cost = _standard().estimate_cost().total_usd
        ratio = slim_cost / std_cost
        assert 0.5 <= ratio <= 0.7, f"Slim/Standard ratio {ratio:.2f} not in 0.5-0.7"

    def test_all_costs_positive(self):
        for ht in WellDesign.HOLE_TYPES:
            wd = WellDesign(hole_type=ht, water_depth_m=300.0)
            cost = wd.estimate_cost()
            assert cost.drilling_usd > 0
            assert cost.casing_usd > 0
            assert cost.completion_usd > 0
            assert cost.total_usd > 0


# ---------------------------------------------------------------------------
# Production potential
# ---------------------------------------------------------------------------

class TestProductionPotential:
    def test_returns_float(self):
        rate = _standard().production_potential()
        assert isinstance(rate, float)

    def test_big_hole_highest_production(self):
        big = _big_hole().production_potential()
        std = _standard().production_potential()
        slim = _slim().production_potential()
        assert big > std > slim

    def test_production_positive(self):
        for ht in WellDesign.HOLE_TYPES:
            wd = WellDesign(hole_type=ht, water_depth_m=300.0)
            assert wd.production_potential() > 0

    def test_slim_production_limited(self):
        """Slim hole production should be notably less than standard."""
        slim = _slim().production_potential()
        std = _standard().production_potential()
        assert slim / std < 0.8, "Slim should produce < 80% of standard"


# ---------------------------------------------------------------------------
# Risk assessment
# ---------------------------------------------------------------------------

class TestRiskAssessment:
    def test_returns_dict(self):
        risk = _standard().risk_assessment()
        assert isinstance(risk, dict)

    def test_required_risk_factors(self):
        risk = _standard().risk_assessment()
        required = {"stuck_pipe", "lost_circulation", "wellbore_instability", "overall_score"}
        assert required.issubset(risk.keys())

    def test_risk_scores_0_to_10(self):
        for ht in WellDesign.HOLE_TYPES:
            wd = WellDesign(hole_type=ht, water_depth_m=300.0)
            risk = wd.risk_assessment()
            for key, val in risk.items():
                assert 0 <= val <= 10, f"{ht}.{key} = {val} out of [0,10]"

    def test_slim_higher_stuck_pipe_risk(self):
        """Slim hole has tighter annulus → higher stuck pipe risk."""
        slim_risk = _slim().risk_assessment()["stuck_pipe"]
        std_risk = _standard().risk_assessment()["stuck_pipe"]
        assert slim_risk > std_risk

    def test_big_hole_higher_lost_circ_risk(self):
        """Big hole has higher lost circulation risk due to larger hole volume."""
        big_risk = _big_hole().risk_assessment()["lost_circulation"]
        std_risk = _standard().risk_assessment()["lost_circulation"]
        assert big_risk > std_risk


# ---------------------------------------------------------------------------
# Decision matrix
# ---------------------------------------------------------------------------

class TestDecisionMatrix:
    def test_exploration_shallow_recommends_slim(self):
        wd = WellDesign(hole_type="standard_hole", water_depth_m=200.0)
        rec = wd.recommend_hole_type(water_depth_m=200.0, objective="exploration")
        assert rec == "slim_hole"

    def test_development_recommends_standard(self):
        wd = WellDesign(hole_type="standard_hole", water_depth_m=500.0)
        rec = wd.recommend_hole_type(water_depth_m=500.0, objective="development")
        assert rec == "standard_hole"

    def test_high_rate_development_recommends_big(self):
        wd = WellDesign(hole_type="standard_hole", water_depth_m=500.0)
        rec = wd.recommend_hole_type(water_depth_m=500.0, objective="high_rate_development")
        assert rec == "big_hole"

    def test_appraisal_recommends_slim(self):
        wd = WellDesign(hole_type="standard_hole", water_depth_m=300.0)
        rec = wd.recommend_hole_type(water_depth_m=300.0, objective="appraisal")
        assert rec == "slim_hole"

    def test_deep_water_exploration_recommends_standard(self):
        """Deep water (>1000m) exploration should upgrade from slim to standard."""
        wd = WellDesign(hole_type="standard_hole", water_depth_m=1500.0)
        rec = wd.recommend_hole_type(water_depth_m=1500.0, objective="exploration")
        assert rec == "standard_hole"

    def test_invalid_objective_raises(self):
        wd = WellDesign(hole_type="standard_hole", water_depth_m=500.0)
        with pytest.raises(ValueError, match="objective"):
            wd.recommend_hole_type(water_depth_m=500.0, objective="unknown_purpose")
