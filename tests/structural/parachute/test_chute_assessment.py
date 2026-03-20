"""
ABOUTME: Tests for WRK-1362 — dual chute drag, aero lift, tire traction
ABOUTME: Load case orchestrator and YAML export for downstream WRKs

Hand-calc references (from parachute-aerodynamics.md and WRK-5082):
  Vehicle: GT1R R35, 3600 lbs
  Single chute: Stroud 430 Std. 32, 12 ft diameter, Cd=1.4, Cx=1.5
  rho = 0.002378 slug/ft^3 (sea level)
"""

import yaml

from digitalmodel.structural.parachute.parachute_drag import (
    calculate_drag_force, chute_area, mph_to_fps,
)
from digitalmodel.structural.parachute.chute_assessment import (
    DualChuteResult, TractionAssessment,
    calculate_aero_lift, calculate_dual_chute_drag,
    calculate_tire_traction, assess_all_load_cases, export_results_yaml,
)

RHO = 0.002378
CD = 1.4
CX = 1.5
VEHICLE_WEIGHT_LBS = 3600.0


def _r35_load_cases():
    return assess_all_load_cases(
        vehicle_weight_lbs=3600.0, single_chute_diameter_ft=12.0,
        cd_single=1.4, cx=1.5, rho=RHO,
    )


class TestDualChuteDrag:
    """Dual chute drag force: two chutes with independent parameters."""

    def test_dual_is_sum_of_two_singles(self):
        s1 = calculate_drag_force(250.0, 10.0, CD, CX, RHO)
        s2 = calculate_drag_force(250.0, 10.0, CD, CX, RHO)
        dual = calculate_dual_chute_drag(250.0, 10.0, 10.0, CD, CD, CX, RHO)
        assert abs(dual.total_force_lbs - (s1.force_lbs + s2.force_lbs)) < 1.0

    def test_dual_with_different_diameters(self):
        dual = calculate_dual_chute_drag(250.0, 10.0, 12.0, CD, CD, CX, RHO)
        v = mph_to_fps(250.0)
        f1 = 0.5 * RHO * v**2 * CD * chute_area(10.0) * CX
        f2 = 0.5 * RHO * v**2 * CD * chute_area(12.0) * CX
        assert abs(dual.total_force_lbs - (f1 + f2)) < 1.0

    def test_dual_result_has_individual_forces(self):
        dual = calculate_dual_chute_drag(250.0, 10.0, 12.0, CD, CD, CX, RHO)
        assert isinstance(dual, DualChuteResult)
        assert dual.chute1_force_lbs > 0 and dual.chute2_force_lbs > 0
        assert abs(dual.total_force_lbs - dual.chute1_force_lbs - dual.chute2_force_lbs) < 0.01

    def test_dual_has_newtons(self):
        dual = calculate_dual_chute_drag(250.0, 10.0, 10.0, CD, CD, CX, RHO)
        assert abs(dual.total_force_n - dual.total_force_lbs * 4.44822) < 1.0


class TestAeroLift:
    """Aerodynamic lift on R35 at deployment speeds."""

    CL = 0.35
    A = 25.0  # frontal area ft^2

    def test_lift_at_200mph(self):
        v = mph_to_fps(200.0)
        expected = 0.5 * RHO * v**2 * self.CL * self.A
        assert abs(calculate_aero_lift(200.0, self.CL, self.A, RHO) - expected) < 1.0

    def test_lift_scales_with_v_squared(self):
        lift_200 = calculate_aero_lift(200.0, self.CL, self.A, RHO)
        lift_250 = calculate_aero_lift(250.0, self.CL, self.A, RHO)
        assert abs(lift_250 / lift_200 - (250.0 / 200.0) ** 2) < 0.01

    def test_lift_positive(self):
        assert calculate_aero_lift(200.0, self.CL, self.A, RHO) > 0

    def test_zero_speed_zero_lift(self):
        assert calculate_aero_lift(0.0, self.CL, self.A, RHO) == 0.0


class TestTireTraction:
    """Tire traction assessment at deployment speeds."""

    def test_traction_basic(self):
        result = calculate_tire_traction(3600.0, 500.0, 0.0, 0.8)
        assert isinstance(result, TractionAssessment)
        assert abs(result.friction_capacity_lbs - 0.8 * 3100.0) < 0.1

    def test_traction_with_downforce(self):
        result = calculate_tire_traction(3600.0, 500.0, 200.0, 0.8)
        assert abs(result.friction_capacity_lbs - 0.8 * 3300.0) < 0.1

    def test_liftoff_flag_when_lift_exceeds_weight(self):
        result = calculate_tire_traction(3600.0, 4000.0, 0.0, 0.8)
        assert result.liftoff_risk is True
        assert result.effective_normal_lbs < 0

    def test_no_liftoff_normal_conditions(self):
        assert calculate_tire_traction(3600.0, 500.0, 0.0, 0.8).liftoff_risk is False


class TestAssessAllLoadCases:
    """Orchestrator: assess all load cases per WRK-1362 spec."""

    def test_returns_three_load_cases(self):
        assert len(_r35_load_cases()) >= 3

    def test_case1_200mph_single(self):
        c = _r35_load_cases()[0]
        assert c.speed_mph == 200.0 and c.config == "single" and c.drag_force_lbs > 0

    def test_case2_250mph_single(self):
        c = _r35_load_cases()[1]
        assert c.speed_mph == 250.0 and c.config == "single"

    def test_case3_250mph_dual(self):
        c = _r35_load_cases()[2]
        assert c.speed_mph == 250.0 and c.config == "dual"

    def test_governing_case_identified(self):
        results = _r35_load_cases()
        governing = [r for r in results if r.is_governing]
        assert len(governing) == 1
        assert governing[0].drag_force_lbs == max(r.drag_force_lbs for r in results)

    def test_each_case_has_traction_assessment(self):
        for r in _r35_load_cases():
            assert isinstance(r.traction, TractionAssessment)

    def test_governing_force_within_1_percent_of_hand_calc(self):
        """AC6: governing drag within 1% of hand calculation.

        Dual 250 MPH, two 10ft chutes, Cd=1.4, Cx=1.5.
        """
        governing = next(c for c in _r35_load_cases() if c.is_governing)
        v_fps = mph_to_fps(250.0)
        single_force = 0.5 * RHO * v_fps**2 * 1.4 * chute_area(10.0) * 1.5
        expected = 2 * single_force
        rel_err = abs(governing.drag_force_lbs - expected) / expected
        assert rel_err < 0.01, (
            f"{governing.drag_force_lbs:.1f} vs {expected:.1f} lbs ({rel_err:.4%})"
        )


class TestExportResultsYaml:
    """YAML export for downstream WRKs (1363/1364/1365)."""

    def test_yaml_has_required_keys(self):
        data = yaml.safe_load(export_results_yaml(3600.0, _r35_load_cases()))
        assert data["wrk_ref"] == "WRK-1362"
        assert data["vehicle_weight_lbs"] == 3600.0
        assert len(data["load_cases"]) == 3
        assert "governing_case_id" in data and "governing_force_lbs" in data

    def test_yaml_governing_matches_results(self):
        results = _r35_load_cases()
        data = yaml.safe_load(export_results_yaml(3600.0, results))
        governing = next(c for c in results if c.is_governing)
        assert data["governing_case_id"] == governing.case_id
        assert abs(data["governing_force_lbs"] - governing.drag_force_lbs) < 1.0
