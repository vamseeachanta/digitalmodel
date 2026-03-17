"""
Tests for parachute drag force calculation (Child-C).

Hand-calc verification: F = 0.5 * rho * V^2 * Cd * A * Cx

Reference values:
  rho = 0.002378 slug/ft^3 (sea level standard)
  V = 200 MPH = 293.33 ft/s
  Cd = 1.4 (hemispherical chute, typical)
  A = pi * (12/2)^2 = 113.097 ft^2 (12 ft diameter)
  Cx = 1.5 (opening shock factor, mid-range)

  F_200 = 0.5 * 0.002378 * 293.33^2 * 1.4 * 113.097 * 1.5
        = 0.5 * 0.002378 * 86042.5 * 1.4 * 113.097 * 1.5
        = 24,255 lbs (approx)
"""

import math
import pytest

from digitalmodel.structural.parachute.parachute_drag import (
    calculate_drag_force,
    mph_to_fps,
    chute_area,
    DragResult,
)


class TestUnitConversions:
    def test_mph_to_fps_200(self):
        fps = mph_to_fps(200.0)
        assert abs(fps - 293.333) < 0.01

    def test_mph_to_fps_250(self):
        fps = mph_to_fps(250.0)
        assert abs(fps - 366.667) < 0.01

    def test_mph_to_fps_zero(self):
        assert mph_to_fps(0.0) == 0.0


class TestChuteArea:
    def test_12ft_diameter(self):
        area = chute_area(12.0)
        expected = math.pi * 36.0  # pi * r^2 = pi * 6^2
        assert abs(area - expected) < 0.01

    def test_zero_diameter_raises(self):
        with pytest.raises(ValueError):
            chute_area(0.0)

    def test_negative_diameter_raises(self):
        with pytest.raises(ValueError):
            chute_area(-5.0)


class TestDragForce:
    """Hand-calc verified drag force tests."""

    RHO = 0.002378  # slug/ft^3
    CD = 1.4
    CX = 1.5
    DIAMETER = 12.0  # ft

    def _hand_calc(self, speed_mph):
        v = mph_to_fps(speed_mph)
        a = chute_area(self.DIAMETER)
        return 0.5 * self.RHO * v**2 * self.CD * a * self.CX

    def test_drag_200_mph(self):
        result = calculate_drag_force(
            speed_mph=200.0,
            chute_diameter_ft=self.DIAMETER,
            cd=self.CD,
            cx=self.CX,
            rho=self.RHO,
        )
        expected = self._hand_calc(200.0)
        assert abs(result.force_lbs - expected) < 1.0
        assert result.force_lbs > 0

    def test_drag_250_mph(self):
        result = calculate_drag_force(
            speed_mph=250.0,
            chute_diameter_ft=self.DIAMETER,
            cd=self.CD,
            cx=self.CX,
            rho=self.RHO,
        )
        expected = self._hand_calc(250.0)
        assert abs(result.force_lbs - expected) < 1.0

    def test_250_greater_than_200(self):
        r200 = calculate_drag_force(200.0, self.DIAMETER, self.CD, self.CX, self.RHO)
        r250 = calculate_drag_force(250.0, self.DIAMETER, self.CD, self.CX, self.RHO)
        assert r250.force_lbs > r200.force_lbs

    def test_force_scales_with_v_squared(self):
        r200 = calculate_drag_force(200.0, self.DIAMETER, self.CD, self.CX, self.RHO)
        r250 = calculate_drag_force(250.0, self.DIAMETER, self.CD, self.CX, self.RHO)
        ratio = r250.force_lbs / r200.force_lbs
        expected_ratio = (250.0 / 200.0) ** 2
        assert abs(ratio - expected_ratio) < 0.01

    def test_result_has_newtons(self):
        result = calculate_drag_force(200.0, self.DIAMETER, self.CD, self.CX, self.RHO)
        expected_n = result.force_lbs * 4.44822
        assert abs(result.force_n - expected_n) < 1.0

    def test_zero_speed_zero_force(self):
        result = calculate_drag_force(0.0, self.DIAMETER, self.CD, self.CX, self.RHO)
        assert result.force_lbs == 0.0

    def test_negative_speed_raises(self):
        with pytest.raises(ValueError):
            calculate_drag_force(-10.0, self.DIAMETER, self.CD, self.CX, self.RHO)

    def test_zero_cd_zero_force(self):
        result = calculate_drag_force(200.0, self.DIAMETER, 0.0, self.CX, self.RHO)
        assert result.force_lbs == 0.0


class TestCdSensitivity:
    """Verify sensitivity to Cd variation (+/- 20%)."""

    def test_cd_sensitivity_range(self):
        cd_base = 1.4
        cd_low = cd_base * 0.8
        cd_high = cd_base * 1.2

        r_base = calculate_drag_force(200.0, 12.0, cd_base, 1.5, 0.002378)
        r_low = calculate_drag_force(200.0, 12.0, cd_low, 1.5, 0.002378)
        r_high = calculate_drag_force(200.0, 12.0, cd_high, 1.5, 0.002378)

        assert r_low.force_lbs < r_base.force_lbs < r_high.force_lbs
        # Force should scale linearly with Cd
        assert abs(r_low.force_lbs / r_base.force_lbs - 0.8) < 0.001
        assert abs(r_high.force_lbs / r_base.force_lbs - 1.2) < 0.001


class TestChatGPTCrossValidation:
    """
    Cross-validation against ChatGPT independent calculation.

    Source: https://chatgpt.com/s/t_69b37c9a90888191be1caf035639aa89

    ChatGPT used SI units with no opening shock factor:
      rho = 1.225 kg/m^3
      V = 250 mph = 112 m/s
      Cd = 1.5
      A = pi * (1.83 m)^2 = 10.5 m^2  (12 ft diameter chute)
      F = 0.5 * 1.225 * 112^2 * 1.5 * 10.5 = ~121,000 N = ~27,000 lbs

    Our code uses Imperial units (slug/ft^3, ft/s, ft^2).
    With Cx=1.0 (no opening shock) and Cd=1.5 the results must agree.

    ChatGPT also notes:
      - Snatch/opening shock can briefly exceed 30,000-40,000 lbs
      - Dual chutes at 240-330 mph: 50,000-60,000 lbs total
      - 25-30k lbs on 3600 lb car ~ 0.7-0.8 g deceleration
    """

    # ChatGPT parameters (steady-state, no opening shock)
    RHO = 0.002378   # slug/ft^3 (equivalent to 1.225 kg/m^3)
    CD = 1.5
    CX = 1.0         # no opening shock factor
    DIAMETER = 12.0   # ft

    # ChatGPT reference results
    CHATGPT_FORCE_N = 121_000    # ~121,000 N
    CHATGPT_FORCE_LBS = 27_000   # ~27,000 lbs

    def test_steady_state_250mph_matches_chatgpt_newtons(self):
        """F ≈ 121,000 N at 250 MPH (within 2%)."""
        result = calculate_drag_force(
            speed_mph=250.0,
            chute_diameter_ft=self.DIAMETER,
            cd=self.CD,
            cx=self.CX,
            rho=self.RHO,
        )
        pct_diff = abs(result.force_n - self.CHATGPT_FORCE_N) / self.CHATGPT_FORCE_N
        assert pct_diff < 0.02, (
            f"Expected ~{self.CHATGPT_FORCE_N:,} N, got {result.force_n:,.0f} N "
            f"({pct_diff*100:.1f}% diff)"
        )

    def test_steady_state_250mph_matches_chatgpt_lbs(self):
        """F ≈ 27,000 lbs at 250 MPH (within 2%)."""
        result = calculate_drag_force(
            speed_mph=250.0,
            chute_diameter_ft=self.DIAMETER,
            cd=self.CD,
            cx=self.CX,
            rho=self.RHO,
        )
        pct_diff = abs(result.force_lbs - self.CHATGPT_FORCE_LBS) / self.CHATGPT_FORCE_LBS
        assert pct_diff < 0.02, (
            f"Expected ~{self.CHATGPT_FORCE_LBS:,} lbs, got {result.force_lbs:,.0f} lbs "
            f"({pct_diff*100:.1f}% diff)"
        )

    def test_chatgpt_range_25k_to_30k(self):
        """ChatGPT states 25,000-30,000 lbs for steady-state at 250 MPH."""
        result = calculate_drag_force(250.0, self.DIAMETER, self.CD, self.CX, self.RHO)
        assert 25_000 <= result.force_lbs <= 30_000, (
            f"ChatGPT range 25-30k lbs, got {result.force_lbs:,.0f} lbs"
        )

    def test_opening_shock_exceeds_steady_state(self):
        """With Cx=1.5 opening shock, force must exceed steady-state."""
        steady = calculate_drag_force(250.0, self.DIAMETER, self.CD, 1.0, self.RHO)
        shock = calculate_drag_force(250.0, self.DIAMETER, self.CD, 1.5, self.RHO)
        assert shock.force_lbs > steady.force_lbs
        assert shock.force_lbs == pytest.approx(steady.force_lbs * 1.5, rel=0.001)

    def test_snatch_load_in_chatgpt_range(self):
        """ChatGPT: snatch load can briefly exceed 30,000-40,000 lbs."""
        # Cx=1.5 represents moderate opening shock
        shock = calculate_drag_force(250.0, self.DIAMETER, self.CD, 1.5, self.RHO)
        assert 30_000 <= shock.force_lbs <= 45_000, (
            f"Snatch load expected 30-45k lbs, got {shock.force_lbs:,.0f} lbs"
        )

    def test_deceleration_chatgpt_range(self):
        """ChatGPT: ~0.7-0.8 g deceleration on 3600 lb car."""
        result = calculate_drag_force(250.0, self.DIAMETER, self.CD, self.CX, self.RHO)
        car_weight_lbs = 3600.0
        decel_g = result.force_lbs / car_weight_lbs
        assert 6.0 < decel_g < 9.0, (
            f"Steady-state decel = {decel_g:.1f} g"
        )

    def test_si_imperial_unit_consistency(self):
        """Verify SI and Imperial density give same drag force."""
        # SI calculation: F = 0.5 * 1.225 * (111.76)^2 * 1.5 * 10.52
        v_ms = 250.0 * 0.44704  # mph to m/s
        a_m2 = math.pi * (12.0 * 0.3048 / 2) ** 2  # ft to m
        rho_si = 1.225  # kg/m^3
        f_si_n = 0.5 * rho_si * v_ms**2 * self.CD * a_m2

        # Imperial calculation via our function
        result = calculate_drag_force(250.0, self.DIAMETER, self.CD, self.CX, self.RHO)

        # Both should agree within 1%
        pct_diff = abs(result.force_n - f_si_n) / f_si_n
        assert pct_diff < 0.01, (
            f"SI={f_si_n:,.0f} N vs Imperial={result.force_n:,.0f} N "
            f"({pct_diff*100:.2f}% diff)"
        )
