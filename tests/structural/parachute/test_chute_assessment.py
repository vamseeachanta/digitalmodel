"""
ABOUTME: Tests for WRK-1362 — single and dual chute drag assessment
ABOUTME: Stroud sizing logic, dual chute drag, aero lift, tire traction

TDD: RED phase — these tests define the required behavior.

Hand-calc references (from parachute-aerodynamics.md and WRK-5082):
  Vehicle: GT1R R35, 3600 lbs
  Single chute: Stroud 430 Std. 32, 12 ft diameter, Cd=1.4, Cx=1.5
  Dual chute: two chutes at 250 MPH per Stroud sizing chart
  rho = 0.002378 slug/ft^3 (sea level)
"""

import math
import pytest

from digitalmodel.structural.parachute.parachute_drag import (
    calculate_drag_force,
    mph_to_fps,
    chute_area,
)
from digitalmodel.structural.parachute.chute_assessment import (
    DualChuteResult,
    LoadCaseResult,
    StroudRecommendation,
    TractionAssessment,
    calculate_dual_chute_drag,
    recommend_stroud_chute,
    calculate_aero_lift,
    calculate_tire_traction,
    assess_all_load_cases,
)


# -- Constants for hand calcs --
RHO = 0.002378
CD = 1.4
CX = 1.5
DIAMETER = 12.0  # ft
VEHICLE_WEIGHT_LBS = 3600.0


class TestStroudRecommendation:
    """Stroud sizing chart logic: single vs dual threshold."""

    def test_3600lb_200mph_single(self):
        """3600 lb at 200 MPH: single chute adequate (430 Std. 32)."""
        rec = recommend_stroud_chute(
            vehicle_weight_lbs=3600.0, speed_mph=200.0
        )
        assert rec.config == "single"
        assert "430" in rec.model

    def test_3600lb_250mph_dual(self):
        """3600 lb at 250 MPH: dual chutes required per NHRA/Stroud."""
        rec = recommend_stroud_chute(
            vehicle_weight_lbs=3600.0, speed_mph=250.0
        )
        assert rec.config == "dual"

    def test_light_car_200mph_single(self):
        """2000 lb at 200 MPH: single chute (model 400-410 range)."""
        rec = recommend_stroud_chute(
            vehicle_weight_lbs=2000.0, speed_mph=200.0
        )
        assert rec.config == "single"

    def test_heavy_car_300mph_dual(self):
        """4000 lb at 300 MPH: dual chutes required."""
        rec = recommend_stroud_chute(
            vehicle_weight_lbs=4000.0, speed_mph=300.0
        )
        assert rec.config == "dual"

    def test_returns_recommendation_dataclass(self):
        rec = recommend_stroud_chute(3600.0, 200.0)
        assert isinstance(rec, StroudRecommendation)
        assert rec.vehicle_weight_lbs == 3600.0
        assert rec.speed_mph == 200.0

    def test_threshold_at_200mph(self):
        """At exactly 200 MPH, single chute is still adequate."""
        rec = recommend_stroud_chute(3600.0, 200.0)
        assert rec.config == "single"

    def test_threshold_above_200mph(self):
        """At 201 MPH, dual chutes required."""
        rec = recommend_stroud_chute(3600.0, 201.0)
        assert rec.config == "dual"


class TestDualChuteDrag:
    """Dual chute drag force: two chutes with independent parameters."""

    def test_dual_is_sum_of_two_singles(self):
        """Dual chute drag = sum of two individual chute drags."""
        single_1 = calculate_drag_force(250.0, 10.0, CD, CX, RHO)
        single_2 = calculate_drag_force(250.0, 10.0, CD, CX, RHO)
        dual = calculate_dual_chute_drag(
            speed_mph=250.0,
            chute1_diameter_ft=10.0,
            chute2_diameter_ft=10.0,
            cd1=CD,
            cd2=CD,
            cx=CX,
            rho=RHO,
        )
        expected = single_1.force_lbs + single_2.force_lbs
        assert abs(dual.total_force_lbs - expected) < 1.0

    def test_dual_with_different_diameters(self):
        """Dual chutes with different sizes — force is additive."""
        dual = calculate_dual_chute_drag(
            speed_mph=250.0,
            chute1_diameter_ft=10.0,
            chute2_diameter_ft=12.0,
            cd1=CD,
            cd2=CD,
            cx=CX,
            rho=RHO,
        )
        # Hand calc: each chute force = 0.5 * rho * V^2 * Cd * A * Cx
        v = mph_to_fps(250.0)
        a1 = chute_area(10.0)
        a2 = chute_area(12.0)
        f1 = 0.5 * RHO * v**2 * CD * a1 * CX
        f2 = 0.5 * RHO * v**2 * CD * a2 * CX
        assert abs(dual.total_force_lbs - (f1 + f2)) < 1.0

    def test_dual_result_has_individual_forces(self):
        """DualChuteResult includes force from each chute."""
        dual = calculate_dual_chute_drag(
            speed_mph=250.0,
            chute1_diameter_ft=10.0,
            chute2_diameter_ft=12.0,
            cd1=CD,
            cd2=CD,
            cx=CX,
            rho=RHO,
        )
        assert isinstance(dual, DualChuteResult)
        assert dual.chute1_force_lbs > 0
        assert dual.chute2_force_lbs > 0
        assert abs(
            dual.total_force_lbs - dual.chute1_force_lbs - dual.chute2_force_lbs
        ) < 0.01

    def test_dual_has_newtons(self):
        dual = calculate_dual_chute_drag(
            250.0, 10.0, 10.0, CD, CD, CX, RHO
        )
        expected_n = dual.total_force_lbs * 4.44822
        assert abs(dual.total_force_n - expected_n) < 1.0


class TestAeroLift:
    """Aerodynamic lift on R35 at deployment speeds."""

    # R35 approximate values
    CL = 0.35  # lift coefficient (sedan body, no significant downforce)
    A_FRONTAL_FT2 = 25.0  # ~2.32 m^2 frontal area

    def test_lift_at_200mph(self):
        """Lift force at 200 MPH."""
        v = mph_to_fps(200.0)
        expected = 0.5 * RHO * v**2 * self.CL * self.A_FRONTAL_FT2
        result = calculate_aero_lift(
            speed_mph=200.0,
            cl=self.CL,
            frontal_area_ft2=self.A_FRONTAL_FT2,
            rho=RHO,
        )
        assert abs(result - expected) < 1.0

    def test_lift_at_250mph(self):
        """Lift force at 250 MPH — should be 56% higher than 200 MPH."""
        lift_200 = calculate_aero_lift(200.0, self.CL, self.A_FRONTAL_FT2, RHO)
        lift_250 = calculate_aero_lift(250.0, self.CL, self.A_FRONTAL_FT2, RHO)
        ratio = lift_250 / lift_200
        expected_ratio = (250.0 / 200.0) ** 2
        assert abs(ratio - expected_ratio) < 0.01

    def test_lift_positive(self):
        result = calculate_aero_lift(200.0, self.CL, self.A_FRONTAL_FT2, RHO)
        assert result > 0

    def test_zero_speed_zero_lift(self):
        result = calculate_aero_lift(0.0, self.CL, self.A_FRONTAL_FT2, RHO)
        assert result == 0.0


class TestTireTraction:
    """Tire traction assessment at deployment speeds."""

    def test_traction_basic(self):
        """Effective traction = mu * (W - F_lift)."""
        result = calculate_tire_traction(
            vehicle_weight_lbs=3600.0,
            aero_lift_lbs=500.0,
            downforce_lbs=0.0,
            mu=0.8,
        )
        assert isinstance(result, TractionAssessment)
        expected = 0.8 * (3600.0 - 500.0)
        assert abs(result.friction_capacity_lbs - expected) < 0.1

    def test_traction_with_downforce(self):
        """Downforce increases effective normal force."""
        result = calculate_tire_traction(
            vehicle_weight_lbs=3600.0,
            aero_lift_lbs=500.0,
            downforce_lbs=200.0,
            mu=0.8,
        )
        expected = 0.8 * (3600.0 - 500.0 + 200.0)
        assert abs(result.friction_capacity_lbs - expected) < 0.1

    def test_liftoff_flag_when_lift_exceeds_weight(self):
        """Flag when aero lift exceeds weight (tire liftoff risk)."""
        result = calculate_tire_traction(
            vehicle_weight_lbs=3600.0,
            aero_lift_lbs=4000.0,
            downforce_lbs=0.0,
            mu=0.8,
        )
        assert result.liftoff_risk is True
        assert result.effective_normal_lbs < 0

    def test_no_liftoff_normal_conditions(self):
        result = calculate_tire_traction(3600.0, 500.0, 0.0, 0.8)
        assert result.liftoff_risk is False


class TestAssessAllLoadCases:
    """Orchestrator: assess all load cases per WRK-1362 spec."""

    def test_returns_three_load_cases(self):
        """Must return results for cases 1, 2, 3 from WRK-1362."""
        results = assess_all_load_cases(
            vehicle_weight_lbs=3600.0,
            single_chute_diameter_ft=12.0,
            cd_single=1.4,
            cx=1.5,
            rho=RHO,
        )
        assert len(results) >= 3

    def test_case1_200mph_single(self):
        results = assess_all_load_cases(
            vehicle_weight_lbs=3600.0,
            single_chute_diameter_ft=12.0,
            cd_single=1.4,
            cx=1.5,
            rho=RHO,
        )
        case1 = results[0]
        assert case1.speed_mph == 200.0
        assert case1.config == "single"
        assert case1.drag_force_lbs > 0

    def test_case2_250mph_single(self):
        results = assess_all_load_cases(
            vehicle_weight_lbs=3600.0,
            single_chute_diameter_ft=12.0,
            cd_single=1.4,
            cx=1.5,
            rho=RHO,
        )
        case2 = results[1]
        assert case2.speed_mph == 250.0
        assert case2.config == "single"

    def test_case3_250mph_dual(self):
        results = assess_all_load_cases(
            vehicle_weight_lbs=3600.0,
            single_chute_diameter_ft=12.0,
            cd_single=1.4,
            cx=1.5,
            rho=RHO,
        )
        case3 = results[2]
        assert case3.speed_mph == 250.0
        assert case3.config == "dual"

    def test_governing_case_identified(self):
        """The governing case has the highest total drag force."""
        results = assess_all_load_cases(
            vehicle_weight_lbs=3600.0,
            single_chute_diameter_ft=12.0,
            cd_single=1.4,
            cx=1.5,
            rho=RHO,
        )
        forces = [r.drag_force_lbs for r in results]
        governing = [r for r in results if r.is_governing]
        assert len(governing) == 1
        assert governing[0].drag_force_lbs == max(forces)

    def test_each_case_has_traction_assessment(self):
        results = assess_all_load_cases(
            vehicle_weight_lbs=3600.0,
            single_chute_diameter_ft=12.0,
            cd_single=1.4,
            cx=1.5,
            rho=RHO,
        )
        for r in results:
            assert r.traction is not None
            assert isinstance(r.traction, TractionAssessment)
