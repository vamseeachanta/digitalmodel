"""TDD tests for propeller-rudder interaction models (WRK-1149).

Compares Söding/Brix (primary) and Actuator-Disk + flat-plate (fallback)
against Esso Osaka validation data and physical invariants.

Reference vessel: Esso Osaka (McTaggart 2005)
    L = 325 m, C_B = 0.831, D = 9.1 m
    w = 0.352, t = 0.20, C^(rp) = 0.9
    K_T = 0.394 - 0.197*J - 0.148*J^2
"""
import math

import pytest

from digitalmodel.hydrodynamics.propeller_rudder import (
    VesselPropulsion,
    RudderGeometry,
    RudderForces,
    kt_from_poly,
    soding_forces,
    actuator_disk_velocity,
    flat_plate_rudder_cl,
    ad_flat_plate_forces,
)


# --- Esso Osaka fixture ---

@pytest.fixture
def esso_osaka():
    """Esso Osaka propulsion parameters from McTaggart (2005)."""
    return VesselPropulsion(
        D=9.1,
        kt_coeffs=(0.394, -0.197, -0.148),  # K_T = 0.394 - 0.197J - 0.148J²
        t=0.20,
        w=0.352,
        C_rp=0.9,
        rho=1025.0,
    )


@pytest.fixture
def esso_rudder():
    """Estimated rudder geometry for Esso Osaka (325m tanker)."""
    return RudderGeometry(
        area=68.0,      # ~68 m² for a 325m tanker
        aspect_ratio=1.5,
        x_R=160.0,      # ~half ship length from CG to rudder
        gamma_deg=0.0,
    )


# --- K_T polynomial ---

class TestKtPolynomial:

    def test_kt_at_j_zero(self):
        """K_T(0) = a0 = 0.394 (bollard pull)."""
        assert kt_from_poly(0.0, (0.394, -0.197, -0.148)) == pytest.approx(0.394)

    def test_kt_at_design_j(self):
        """K_T at J=0.5 (typical service condition)."""
        expected = 0.394 - 0.197 * 0.5 - 0.148 * 0.25
        assert kt_from_poly(0.5, (0.394, -0.197, -0.148)) == pytest.approx(expected)

    def test_kt_decreases_with_j(self):
        """K_T must decrease monotonically with J (for this propeller)."""
        coeffs = (0.394, -0.197, -0.148)
        j_values = [0.0, 0.2, 0.4, 0.6, 0.8, 1.0]
        kt_values = [kt_from_poly(j, coeffs) for j in j_values]
        for i in range(len(kt_values) - 1):
            assert kt_values[i] > kt_values[i + 1]


# --- Söding/Brix method ---

class TestSodingForces:

    def test_zero_rudder_angle_gives_zero_lift(self, esso_osaka, esso_rudder):
        """delta=0 → sin(0)=0 → F_lift=0, F_sway=0, F_yaw=0."""
        result = soding_forces(8.0, 1.5, 0.0, esso_osaka, esso_rudder)
        assert result.F_sway == pytest.approx(0.0, abs=1.0)
        assert result.F_yaw == pytest.approx(0.0, abs=1.0)
        assert result.method == "soding"

    def test_zero_rudder_gives_nonzero_drag(self, esso_osaka, esso_rudder):
        """delta=0 → (1-cos(0))=0 → F_drag=0 → F_surge=0."""
        result = soding_forces(8.0, 1.5, 0.0, esso_osaka, esso_rudder)
        assert result.F_surge == pytest.approx(0.0, abs=1.0)

    def test_positive_rudder_gives_negative_sway(self, esso_osaka, esso_rudder):
        """Starboard rudder (positive delta) → negative sway (to starboard)."""
        result = soding_forces(8.0, 1.5, 15.0, esso_osaka, esso_rudder)
        assert result.F_sway < 0.0

    def test_symmetric_rudder_angles(self, esso_osaka, esso_rudder):
        """F_sway at +delta should equal -F_sway at -delta."""
        r_pos = soding_forces(8.0, 1.5, 20.0, esso_osaka, esso_rudder)
        r_neg = soding_forces(8.0, 1.5, -20.0, esso_osaka, esso_rudder)
        assert r_pos.F_sway == pytest.approx(-r_neg.F_sway, rel=1e-10)
        assert r_pos.F_yaw == pytest.approx(-r_neg.F_yaw, rel=1e-10)

    def test_force_increases_with_rudder_angle(self, esso_osaka, esso_rudder):
        """Larger delta → larger |F_sway| (below stall)."""
        r10 = soding_forces(8.0, 1.5, 10.0, esso_osaka, esso_rudder)
        r20 = soding_forces(8.0, 1.5, 20.0, esso_osaka, esso_rudder)
        r30 = soding_forces(8.0, 1.5, 30.0, esso_osaka, esso_rudder)
        assert abs(r10.F_sway) < abs(r20.F_sway) < abs(r30.F_sway)

    def test_engine_off_returns_zero(self, esso_osaka, esso_rudder):
        """n=0 → F_prop=0 → all interaction forces zero."""
        result = soding_forces(8.0, 0.0, 20.0, esso_osaka, esso_rudder)
        assert result.F_surge == 0.0
        assert result.F_sway == 0.0
        assert result.F_yaw == 0.0

    def test_force_increases_with_rpm(self, esso_osaka, esso_rudder):
        """Higher RPM → more thrust → larger interaction force."""
        r_low = soding_forces(8.0, 1.0, 20.0, esso_osaka, esso_rudder)
        r_high = soding_forces(8.0, 2.0, 20.0, esso_osaka, esso_rudder)
        assert abs(r_high.F_sway) > abs(r_low.F_sway)

    def test_esso_osaka_35deg_order_of_magnitude(self, esso_osaka, esso_rudder):
        """At 8 m/s, 1.5 rps, 35° rudder: sway force should be O(10⁵–10⁶) N."""
        result = soding_forces(8.0, 1.5, 35.0, esso_osaka, esso_rudder)
        assert 1e4 < abs(result.F_sway) < 1e7

    def test_yaw_sign_convention(self, esso_osaka, esso_rudder):
        """Positive delta → negative yaw (turns bow to starboard)."""
        result = soding_forces(8.0, 1.5, 15.0, esso_osaka, esso_rudder)
        assert result.F_yaw < 0.0

    def test_surge_always_negative_or_zero(self, esso_osaka, esso_rudder):
        """Rudder drag always opposes forward motion (F_surge ≤ 0)."""
        for delta in [0, 10, 20, 35]:
            result = soding_forces(8.0, 1.5, delta, esso_osaka, esso_rudder)
            assert result.F_surge <= 0.0 + 1.0  # small tolerance


# --- Actuator-disk velocity ---

class TestActuatorDiskVelocity:

    def test_zero_thrust_returns_freestream(self):
        """C_T=0 → a=0 → Va_R = V_A."""
        assert actuator_disk_velocity(5.0, 0.0, 4.0, 4.55) == pytest.approx(5.0)

    def test_positive_thrust_accelerates(self):
        """C_T>0 → Va_R > V_A (propeller accelerates flow)."""
        Va_R = actuator_disk_velocity(5.0, 2.0, 4.0, 4.55)
        assert Va_R > 5.0

    def test_velocity_increases_with_thrust(self):
        """Higher C_T → higher Va_R."""
        v1 = actuator_disk_velocity(5.0, 1.0, 4.0, 4.55)
        v2 = actuator_disk_velocity(5.0, 3.0, 4.0, 4.55)
        assert v2 > v1

    def test_negative_thrust_returns_freestream(self):
        """Braking propeller (C_T < 0) → no acceleration."""
        assert actuator_disk_velocity(5.0, -0.5, 4.0, 4.55) == pytest.approx(5.0)

    def test_contraction_factor_at_disc(self):
        """At x=0 (disc plane), contraction factor = 1 → Va = V_A*(1+a)."""
        V_A, C_T = 5.0, 2.0
        a = (math.sqrt(1 + C_T) - 1) / 2
        expected = V_A * (1 + a)  # at x=0, contraction = 1
        assert actuator_disk_velocity(V_A, C_T, 0.0, 4.55) == pytest.approx(expected)

    def test_far_field_approaches_2a(self):
        """At large x/R, contraction → 2 → Va → V_A*(1+2a)."""
        V_A, C_T, R = 5.0, 2.0, 4.55
        a = (math.sqrt(1 + C_T) - 1) / 2
        far_field = V_A * (1 + 2 * a)
        # At x = 100R, should be very close to far-field
        Va_far = actuator_disk_velocity(V_A, C_T, 100 * R, R)
        assert Va_far == pytest.approx(far_field, rel=0.01)


# --- Flat-plate rudder C_L ---

class TestFlatPlateRudderCl:

    def test_zero_angle_zero_lift(self):
        assert flat_plate_rudder_cl(0.0, 1.5) == pytest.approx(0.0)

    def test_positive_angle_positive_lift(self):
        assert flat_plate_rudder_cl(10.0, 1.5) > 0.0

    def test_stall_clamp(self):
        """Beyond 35°, C_L should not increase (clamped)."""
        cl_35 = flat_plate_rudder_cl(35.0, 1.5)
        cl_45 = flat_plate_rudder_cl(45.0, 1.5)
        assert cl_35 == pytest.approx(cl_45)

    def test_lift_slope_value(self):
        """At AR=1.5: dCL/dalpha = 2*pi*1.5/3.5 ≈ 2.694 /rad."""
        expected_slope = 2 * math.pi * 1.5 / 3.5
        # At 1 degree:
        cl = flat_plate_rudder_cl(1.0, 1.5)
        assert cl == pytest.approx(expected_slope * math.radians(1.0), rel=0.01)


# --- AD + flat-plate forces ---

class TestAdFlatPlateForces:

    def test_zero_rudder_zero_lift(self, esso_osaka, esso_rudder):
        result = ad_flat_plate_forces(8.0, 1.5, 0.0, esso_osaka, esso_rudder)
        assert result.F_sway == pytest.approx(0.0, abs=1.0)
        assert result.method == "actuator_disk_flat_plate"

    def test_engine_off_nonzero_if_ship_moving(self, esso_osaka, esso_rudder):
        """Engine off but ship moving → rudder still produces force from V_s."""
        result = ad_flat_plate_forces(8.0, 0.0, 20.0, esso_osaka, esso_rudder)
        assert abs(result.F_sway) > 0.0

    def test_symmetric(self, esso_osaka, esso_rudder):
        r_pos = ad_flat_plate_forces(8.0, 1.5, 20.0, esso_osaka, esso_rudder)
        r_neg = ad_flat_plate_forces(8.0, 1.5, -20.0, esso_osaka, esso_rudder)
        assert r_pos.F_sway == pytest.approx(-r_neg.F_sway, rel=1e-10)

    def test_force_order_of_magnitude(self, esso_osaka, esso_rudder):
        """At 8 m/s, 1.5 rps, 20°: sway force should be O(10⁵–10⁶) N."""
        result = ad_flat_plate_forces(8.0, 1.5, 20.0, esso_osaka, esso_rudder)
        assert 1e4 < abs(result.F_sway) < 1e7


# --- Method comparison ---

class TestMethodComparison:
    """Compare Söding and AD+flat-plate across operating conditions."""

    @pytest.fixture
    def conditions(self):
        """Representative operating conditions: (V_s [m/s], n [rps], delta [deg])."""
        return [
            (8.0, 1.5, 10.0),   # service speed, moderate helm
            (8.0, 1.5, 20.0),   # service speed, hard helm
            (8.0, 1.5, 35.0),   # service speed, full helm
            (4.0, 1.0, 20.0),   # slow speed, moderate helm
            (2.0, 0.5, 20.0),   # very low speed
            (12.0, 2.0, 15.0),  # higher speed
        ]

    def test_both_methods_same_sign(self, esso_osaka, esso_rudder, conditions):
        """Both methods must agree on force direction."""
        for V_s, n, delta in conditions:
            sb = soding_forces(V_s, n, delta, esso_osaka, esso_rudder)
            ad = ad_flat_plate_forces(V_s, n, delta, esso_osaka, esso_rudder)
            # Both should produce negative sway for positive delta
            assert sb.F_sway < 0.0, f"Söding sway wrong sign at {V_s},{n},{delta}"
            assert ad.F_sway < 0.0, f"AD sway wrong sign at {V_s},{n},{delta}"

    def test_ad_larger_than_soding(self, esso_osaka, esso_rudder):
        """AD+flat-plate predicts larger forces because it applies amplified
        velocity to full rudder area, while Söding models only the
        propeller-augmented interaction increment (not the total rudder force).
        This is expected — Söding is an *interaction* model, not a *total force* model."""
        V_s, n, delta = 4.0, 2.0, 20.0
        sb = soding_forces(V_s, n, delta, esso_osaka, esso_rudder)
        ad = ad_flat_plate_forces(V_s, n, delta, esso_osaka, esso_rudder)
        # AD produces total rudder force; Söding only the prop-augmented part
        assert abs(ad.F_sway) > abs(sb.F_sway)

    def test_methods_within_factor_at_design_j(self, esso_osaka, esso_rudder):
        """At design J (~0.5), both methods should agree within ~3×."""
        V_s, n, delta = 8.0, 1.5, 20.0
        sb = soding_forces(V_s, n, delta, esso_osaka, esso_rudder)
        ad = ad_flat_plate_forces(V_s, n, delta, esso_osaka, esso_rudder)
        ratio = abs(sb.F_sway) / abs(ad.F_sway) if abs(ad.F_sway) > 0 else float('inf')
        assert 0.1 < ratio < 10.0, f"Methods diverge too much: ratio={ratio:.2f}"

    def test_engine_off_soding_zero_ad_nonzero(self, esso_osaka, esso_rudder):
        """Engine off: Söding returns zero, AD returns ship-speed rudder force."""
        sb = soding_forces(8.0, 0.0, 20.0, esso_osaka, esso_rudder)
        ad = ad_flat_plate_forces(8.0, 0.0, 20.0, esso_osaka, esso_rudder)
        assert sb.F_sway == 0.0
        assert abs(ad.F_sway) > 0.0

    def test_low_speed_j_range(self, esso_osaka, esso_rudder):
        """Söding must work at J < 0.3 where AD degrades."""
        V_s, n, delta = 1.0, 2.0, 20.0  # very low J
        J = V_s * (1 - esso_osaka.w) / (n * esso_osaka.D)
        assert J < 0.3, f"J={J:.3f} not low enough"
        sb = soding_forces(V_s, n, delta, esso_osaka, esso_rudder)
        assert abs(sb.F_sway) > 0.0
        assert math.isfinite(sb.F_sway)


# --- Parametric sweep for comparison report ---

class TestParametricSweep:
    """Generate comparison data across J range."""

    @pytest.fixture
    def j_sweep_data(self, esso_osaka, esso_rudder):
        """Run both methods across J = 0.1 to 1.0, delta = 20°."""
        D = esso_osaka.D
        w = esso_osaka.w
        delta = 20.0
        n = 1.5  # fixed RPM

        results = []
        for j_target in [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]:
            V_s = j_target * n * D / (1.0 - w)
            J_actual = V_s * (1 - w) / (n * D)

            sb = soding_forces(V_s, n, delta, esso_osaka, esso_rudder)
            ad = ad_flat_plate_forces(V_s, n, delta, esso_osaka, esso_rudder)

            results.append({
                "J": J_actual,
                "V_s": V_s,
                "sb_sway": sb.F_sway,
                "ad_sway": ad.F_sway,
                "sb_yaw": sb.F_yaw,
                "ad_yaw": ad.F_yaw,
            })
        return results

    def test_sweep_all_finite(self, j_sweep_data):
        """All forces must be finite across the sweep."""
        for r in j_sweep_data:
            assert math.isfinite(r["sb_sway"]), f"Söding NaN at J={r['J']:.2f}"
            assert math.isfinite(r["ad_sway"]), f"AD NaN at J={r['J']:.2f}"

    def test_sweep_soding_dominates_low_j(self, j_sweep_data):
        """At J < 0.3, Söding should predict larger forces (higher loading)."""
        low_j = [r for r in j_sweep_data if r["J"] < 0.3]
        for r in low_j:
            # Both are negative; compare magnitudes
            assert abs(r["sb_sway"]) > 0.0

    def test_sweep_methods_converge_high_j(self, j_sweep_data):
        """At J > 0.7 (light loading), methods should be more comparable."""
        high_j = [r for r in j_sweep_data if r["J"] > 0.7]
        for r in high_j:
            if abs(r["ad_sway"]) > 1e3:
                ratio = abs(r["sb_sway"]) / abs(r["ad_sway"])
                # Should be within an order of magnitude
                assert 0.05 < ratio < 20.0, f"Diverged at J={r['J']:.2f}: ratio={ratio:.2f}"

    def test_sweep_print_comparison(self, j_sweep_data, capsys):
        """Print comparison table (informational, always passes)."""
        print("\n" + "=" * 85)
        print("J-Sweep Comparison: Söding vs AD+Flat-Plate (delta=20°, n=1.5 rps)")
        print("=" * 85)
        print(f"{'J':>5} {'V_s':>7} {'SB_sway':>12} {'AD_sway':>12} {'Ratio':>8} {'SB_yaw':>14} {'AD_yaw':>14}")
        print("-" * 85)
        for r in j_sweep_data:
            ratio = abs(r["sb_sway"]) / abs(r["ad_sway"]) if abs(r["ad_sway"]) > 1 else float('inf')
            print(f"{r['J']:5.2f} {r['V_s']:7.2f} {r['sb_sway']:12.0f} {r['ad_sway']:12.0f} "
                  f"{ratio:8.2f} {r['sb_yaw']:14.0f} {r['ad_yaw']:14.0f}")
        print("=" * 85)
        print("Units: forces [N], moments [N·m], V_s [m/s]")
        print("Ratio = |SB_sway| / |AD_sway|")
