# ABOUTME: TDD tests for maneuverability module — rudder forces and turning circle
# ABOUTME: Test data from USNA EN400 Chapter 9 and standard naval architecture refs
"""
Maneuverability module tests.

Covers: rudder normal force, Nomoto turning model, directional stability,
speed loss in turn, drift angle.

Source: USNA EN400 Chapter 9 + standard references.
"""

import math
import pytest

from digitalmodel.naval_architecture.maneuverability import (
    rudder_normal_force,
    rudder_lift_coefficient,
    nomoto_steady_yaw_rate,
    steady_turning_radius,
    directional_stability_criterion,
    is_directionally_stable,
    speed_in_turn,
    drift_angle,
)


class TestRudderLiftCoefficient:
    """Lift coefficient for rectangular rudder."""

    def test_behind_hull_ar_3_6_at_15_deg(self):
        """AR_eff=3.6, delta=15 deg.

        C_N = (6.13*3.6)/(3.6+2.25) * sin(15) = 3.772 * 0.2588 = 0.976
        """
        cn = rudder_lift_coefficient(
            rudder_area_m2=20.0,
            rudder_span_m=6.0,
            rudder_angle_deg=15.0,
            behind_hull=True,
        )
        assert cn == pytest.approx(0.976, rel=0.05)

    def test_free_standing_ar_1_8_at_15_deg(self):
        """Free-standing rudder (no hull mirror), AR=1.8."""
        cn = rudder_lift_coefficient(
            rudder_area_m2=20.0,
            rudder_span_m=6.0,
            rudder_angle_deg=15.0,
            behind_hull=False,
        )
        # AR=1.8 -> (6.13*1.8)/(1.8+2.25)*sin(15) = 2.72 * 0.2588 = 0.704
        # Lower AR -> lower Cn/(sin delta) but still valid
        cn_behind = rudder_lift_coefficient(
            rudder_area_m2=20.0,
            rudder_span_m=6.0,
            rudder_angle_deg=15.0,
            behind_hull=True,
        )
        assert cn < cn_behind

    def test_zero_angle_gives_zero_force(self):
        """Zero rudder angle -> zero lift coefficient."""
        cn = rudder_lift_coefficient(
            rudder_area_m2=20.0,
            rudder_span_m=6.0,
            rudder_angle_deg=0.0,
            behind_hull=True,
        )
        assert cn == pytest.approx(0.0, abs=1e-10)

    def test_negative_area_raises(self):
        """Negative rudder area is invalid."""
        with pytest.raises(ValueError):
            rudder_lift_coefficient(
                rudder_area_m2=-1.0,
                rudder_span_m=6.0,
                rudder_angle_deg=15.0,
            )

    def test_negative_span_raises(self):
        """Negative rudder span is invalid."""
        with pytest.raises(ValueError):
            rudder_lift_coefficient(
                rudder_area_m2=20.0,
                rudder_span_m=-1.0,
                rudder_angle_deg=15.0,
            )


class TestRudderNormalForce:
    """Normal force on rudder from Whicker & Fehlner model."""

    def test_ddg_rudder_force(self):
        """V=10 m/s, A=20 m2, span=6 m, delta=15 deg, behind hull.

        F_N = 0.5 * 1025 * 100 * 20 * 0.976 = 1,000,400 N
        """
        fn = rudder_normal_force(
            velocity_m_s=10.0,
            rho_kg_m3=1025.0,
            rudder_area_m2=20.0,
            rudder_span_m=6.0,
            rudder_angle_deg=15.0,
            behind_hull=True,
        )
        assert fn == pytest.approx(1_000_750.0, rel=0.05)

    def test_zero_velocity_gives_zero_force(self):
        """No flow -> no force."""
        fn = rudder_normal_force(
            velocity_m_s=0.0,
            rho_kg_m3=1025.0,
            rudder_area_m2=20.0,
            rudder_span_m=6.0,
            rudder_angle_deg=15.0,
        )
        assert fn == pytest.approx(0.0, abs=1e-10)

    def test_negative_velocity_raises(self):
        """Negative velocity is invalid."""
        with pytest.raises(ValueError):
            rudder_normal_force(
                velocity_m_s=-1.0,
                rho_kg_m3=1025.0,
                rudder_area_m2=20.0,
                rudder_span_m=6.0,
                rudder_angle_deg=15.0,
            )


class TestNomotoTurning:
    """Nomoto 1st-order turning model."""

    def test_steady_yaw_rate_ddg51(self):
        """K=0.30, delta=35 deg -> r_ss ~ 0.183 rad/s."""
        r_ss = nomoto_steady_yaw_rate(
            K_per_s=0.30,
            rudder_angle_deg=35.0,
        )
        assert r_ss == pytest.approx(0.183, abs=0.01)

    def test_zero_rudder_zero_yaw(self):
        """Zero rudder angle -> zero yaw rate."""
        r_ss = nomoto_steady_yaw_rate(K_per_s=0.30, rudder_angle_deg=0.0)
        assert r_ss == pytest.approx(0.0, abs=1e-10)

    def test_turning_radius_ddg51(self):
        """V=15.4 m/s, r_ss=0.183 rad/s -> R ~ 84.2 m."""
        R = steady_turning_radius(
            velocity_m_s=15.4,
            yaw_rate_rad_s=0.183,
        )
        assert R == pytest.approx(84.2, abs=5.0)

    def test_zero_yaw_rate_raises(self):
        """Zero yaw rate -> infinite radius, raise ValueError."""
        with pytest.raises(ValueError):
            steady_turning_radius(velocity_m_s=15.0, yaw_rate_rad_s=0.0)

    def test_negative_K_raises(self):
        """Negative gain constant is invalid."""
        with pytest.raises(ValueError):
            nomoto_steady_yaw_rate(K_per_s=-0.1, rudder_angle_deg=15.0)


class TestDirectionalStability:
    """Linear directional stability criterion."""

    def test_stable_ship(self):
        """C = Yv'*Nr' - Nv'*Yr' > 0 -> stable."""
        C = directional_stability_criterion(
            Yv_prime=-0.015,
            Nr_prime=-0.005,
            Nv_prime=-0.008,
            Yr_prime=0.003,
        )
        assert C == pytest.approx(0.000099, abs=0.000005)

    def test_stable_flag(self):
        """is_directionally_stable returns True for stable derivatives."""
        assert is_directionally_stable(
            Yv_prime=-0.015,
            Nr_prime=-0.005,
            Nv_prime=-0.008,
            Yr_prime=0.003,
        )

    def test_unstable_ship(self):
        """Manipulated derivatives to give C < 0.

        C = (-0.015)(-0.005) - (0.010)(0.003) = 0.000075 - 0.000030
        Flip Nv sign to positive -> C = 0.000075 - 0.030*0.003 = -0.000015
        """
        assert not is_directionally_stable(
            Yv_prime=-0.015,
            Nr_prime=-0.005,
            Nv_prime=0.010,
            Yr_prime=0.010,
        )


class TestSpeedLoss:
    """Speed reduction during steady turn."""

    def test_speed_loss_35_deg(self):
        """V_0=15, delta=35, factor=0.4 -> V_turn=9.0."""
        v = speed_in_turn(
            velocity_m_s=15.0,
            rudder_angle_deg=35.0,
            loss_factor=0.4,
        )
        assert v == pytest.approx(9.0, abs=0.1)

    def test_zero_rudder_no_loss(self):
        """Zero rudder angle -> no speed loss."""
        v = speed_in_turn(
            velocity_m_s=15.0,
            rudder_angle_deg=0.0,
            loss_factor=0.4,
        )
        assert v == pytest.approx(15.0, abs=0.01)

    def test_loss_factor_out_of_range_raises(self):
        """Loss factor must be in [0, 1]."""
        with pytest.raises(ValueError):
            speed_in_turn(
                velocity_m_s=15.0,
                rudder_angle_deg=35.0,
                loss_factor=1.5,
            )


class TestDriftAngle:
    """Approximate drift angle in steady turn."""

    def test_drift_angle_ddg51(self):
        """L=142, R=84.2 -> beta ~ 40.15 deg."""
        beta = drift_angle(lwl_m=142.0, turning_radius_m=84.2)
        assert beta == pytest.approx(40.15, abs=1.0)

    def test_large_radius_small_drift(self):
        """Very large radius -> small drift angle."""
        beta = drift_angle(lwl_m=142.0, turning_radius_m=10000.0)
        assert beta < 1.0

    def test_zero_radius_raises(self):
        """Zero turning radius is invalid."""
        with pytest.raises(ValueError):
            drift_angle(lwl_m=142.0, turning_radius_m=0.0)
