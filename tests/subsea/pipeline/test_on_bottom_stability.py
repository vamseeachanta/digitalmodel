"""
Tests for DNV RP F109 on-bottom stability module.

Reference scenarios:
  Scenario 1: 12" bare pipe — D=0.3239 m, t_steel=0.0127 m, no coating/concrete
               U_c=0.5 m/s, U_w=1.0 m/s, T_w=12 s, mu=0.5 (sand)
               Expect: FAIL (pipe too light)

  Scenario 2: Same pipe + 75 mm concrete coating (rho=3040 kg/m3)
               D_hydro=0.4739 m
               Expect: PASS (SF ~ 2.07)
"""
import math

import pytest

from digitalmodel.subsea.pipeline.on_bottom_stability import (
    C_D_DEFAULT,
    C_L_DEFAULT,
    C_M_DEFAULT,
    MU_SAND,
    MU_CLAY,
    peak_combined_velocity,
    drag_force,
    lift_force,
    inertia_force,
    submerged_weight,
    keulegan_carpenter,
    current_velocity_ratio,
    generalized_stability_number,
    required_stability_number,
    stability_check,
    full_stability_assessment,
)


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Scenario 1 — bare 12" pipe
D_BARE = 0.3239       # m (12.75 in OD)
T_STEEL = 0.0127      # m (0.5 in WT)
U_C = 0.5             # m/s
U_W = 1.0             # m/s
T_W = 12.0            # s
RHO_W = 1025.0        # kg/m3
G = 9.81              # m/s2

# Scenario 2 — add 75mm concrete coating
T_CONCRETE = 0.075    # m
RHO_CONCRETE = 3040.0 # kg/m3
D_HYDRO = D_BARE + 2 * T_CONCRETE  # 0.4739 m


# ---------------------------------------------------------------------------
# Individual function tests
# ---------------------------------------------------------------------------

class TestPeakCombinedVelocity:
    def test_basic(self):
        assert peak_combined_velocity(0.5, 1.0) == pytest.approx(1.5)

    def test_zero_current(self):
        assert peak_combined_velocity(0.0, 1.0) == pytest.approx(1.0)

    def test_zero_wave(self):
        assert peak_combined_velocity(0.5, 0.0) == pytest.approx(0.5)


class TestDragForce:
    def test_scenario_1(self):
        """F_D = 0.5 * 0.7 * 1025 * 0.3239 * 1.5^2 ~ 261.2 N/m."""
        U_peak = 1.5
        expected = 0.5 * C_D_DEFAULT * RHO_W * D_BARE * U_peak ** 2
        result = drag_force(D_BARE, U_peak)
        assert result == pytest.approx(expected, rel=1e-6)
        assert result == pytest.approx(261.2, rel=0.01)


class TestLiftForce:
    def test_scenario_1(self):
        """F_L = 0.5 * 0.9 * 1025 * 0.3239 * 1.5^2 ~ 335.6 N/m."""
        U_peak = 1.5
        expected = 0.5 * C_L_DEFAULT * RHO_W * D_BARE * U_peak ** 2
        result = lift_force(D_BARE, U_peak)
        assert result == pytest.approx(expected, rel=1e-6)
        assert result == pytest.approx(335.6, rel=0.01)


class TestInertiaForce:
    def test_scenario_1(self):
        """F_I = C_M * rho_w * pi/4 * D^2 * (2*pi*U_w/T_w)."""
        expected = (
            C_M_DEFAULT * RHO_W * (math.pi / 4) * D_BARE ** 2
            * (2 * math.pi * U_W / T_W)
        )
        result = inertia_force(D_BARE, U_W, T_W)
        assert result == pytest.approx(expected, rel=1e-6)
        assert result == pytest.approx(145.4, rel=0.02)


class TestSubmergedWeight:
    def test_bare_pipe(self):
        """12" bare pipe, empty — W_s ~ 217 N/m."""
        result = submerged_weight(D_BARE, T_STEEL)
        # Steel weight = rho_s * g * pi/4 * (Do^2 - Di^2)
        D_inner = D_BARE - 2 * T_STEEL
        steel_area = math.pi / 4 * (D_BARE ** 2 - D_inner ** 2)
        w_steel = 7850.0 * G * steel_area
        w_buoy = RHO_W * G * math.pi / 4 * D_BARE ** 2
        expected = w_steel - w_buoy
        assert result == pytest.approx(expected, rel=1e-6)
        assert result == pytest.approx(217.0, rel=0.02)

    def test_with_concrete_coating(self):
        """12" pipe + 75mm concrete — W_s ~ 2074 N/m."""
        result = submerged_weight(
            D_BARE, T_STEEL, t_concrete=T_CONCRETE,
            rho_concrete=RHO_CONCRETE,
        )
        assert result == pytest.approx(2074.0, rel=0.02)


class TestKeuleganCarpenter:
    def test_scenario_1(self):
        """KC = U_w * T_w / D = 1.0 * 12 / 0.3239 ~ 37.05."""
        result = keulegan_carpenter(U_W, T_W, D_BARE)
        assert result == pytest.approx(37.05, rel=0.01)


class TestCurrentVelocityRatio:
    def test_scenario_1(self):
        """M = U_c / U_w = 0.5 / 1.0 = 0.5."""
        assert current_velocity_ratio(U_C, U_W) == pytest.approx(0.5)


class TestGeneralizedStabilityNumber:
    def test_bare_pipe(self):
        """Gs = W_s / (rho_w * g * D^2) ~ 0.206 for bare pipe."""
        W_s = 217.0  # approximate from Scenario 1
        result = generalized_stability_number(W_s, D_BARE)
        expected = W_s / (RHO_W * G * D_BARE ** 2)
        assert result == pytest.approx(expected, rel=1e-6)
        assert result == pytest.approx(0.206, rel=0.02)


class TestRequiredStabilityNumber:
    def test_kc_37_m_05(self):
        """Gs_req = (0.7 + 0.4*0.5) * (37.1/10)^0.3 ~ 1.333."""
        result = required_stability_number(37.1, 0.5)
        expected = (0.7 + 0.4 * 0.5) * (37.1 / 10) ** 0.3
        assert result == pytest.approx(expected, rel=1e-6)
        assert result == pytest.approx(1.333, rel=0.01)


class TestStabilityCheck:
    def test_fail_scenario_1(self):
        """Bare pipe: mu*(W_s - F_L) < F_H => FAIL."""
        W_s = 217.0
        F_H = 261.2
        F_L = 335.6
        result = stability_check(W_s, F_H, F_L, mu=MU_SAND)
        assert result["pass"] is False
        assert result["stability_factor"] < 0.0
        expected_sf = MU_SAND * (W_s - F_L) / F_H
        assert result["stability_factor"] == pytest.approx(expected_sf, rel=0.01)

    def test_pass_scenario_2(self):
        """Concrete-coated pipe: SF ~ 2.07 => PASS."""
        W_s = 2074.0
        F_H = 382.0
        F_L = 491.1
        result = stability_check(W_s, F_H, F_L, mu=MU_SAND)
        assert result["pass"] is True
        expected_sf = MU_SAND * (W_s - F_L) / F_H
        assert result["stability_factor"] == pytest.approx(expected_sf, rel=0.01)
        assert result["stability_factor"] == pytest.approx(2.07, rel=0.02)


class TestFullStabilityAssessment:
    def test_pass_with_concrete(self):
        """Full assessment: 12" pipe + 75mm concrete coating => PASS."""
        result = full_stability_assessment(
            D=D_BARE,
            t_steel=T_STEEL,
            U_c=U_C,
            U_w=U_W,
            T_w=T_W,
            mu=MU_SAND,
            t_concrete=T_CONCRETE,
            rho_concrete=RHO_CONCRETE,
        )
        assert result["pass"] is True
        assert result["stability_factor"] > 1.0
        assert result["stability_factor"] == pytest.approx(2.07, rel=0.05)
        assert result["D_hydro"] == pytest.approx(D_HYDRO, rel=1e-6)
        assert result["W_s"] == pytest.approx(2074.0, rel=0.02)

    def test_fail_bare_pipe(self):
        """Full assessment: 12" bare pipe => FAIL."""
        result = full_stability_assessment(
            D=D_BARE,
            t_steel=T_STEEL,
            U_c=U_C,
            U_w=U_W,
            T_w=T_W,
            mu=MU_SAND,
        )
        assert result["pass"] is False
        assert result["stability_factor"] < 0.0
        assert "KC" in result
        assert "M" in result
        assert result["KC"] == pytest.approx(37.05, rel=0.01)
        assert result["M"] == pytest.approx(0.5)
